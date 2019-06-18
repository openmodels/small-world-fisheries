datapath <- "../../data/"
resultpath <- "../../results/"

library(xtable)
source("../lib/saufuncs.R")

yeargroup.suffix <- "-cl" #"-ln" #"-en"

regions <- read.csv(paste0(datapath, "weights/combined-regions-new.csv"))
weights <- read.csv(paste0(datapath, "weights/combined-country.csv"))
risk.value <- read.csv(paste0(resultpath, "value", yeargroup.suffix, ".csv"))
risk.portion <- read.csv(paste0(resultpath, "portion-split", yeargroup.suffix, ".csv"))
risk.portion <- risk.portion[risk.portion$resilience == 'total',]

## Combine EEZs to SAU level, as done for atrisk/value

tbl.value <- risk.value
for (ii in 1:nrow(risk.value)) {
    insau <- regions$name %in% eez2sau(risk.value$sink[ii])
    regionids <- regions$country[insau]

    if (length(regionids) == 0) {
        print(c("Problem finding eez", eez))
        next
    }

    if (length(unique(regions$sovereign[insau])) > 1) {
        print(c("Multiple sovereigns within", eez))
    }

    tbl.value$avgecon[ii] <- sum(regions$avgecon[insau]) / 1e6
    tbl.value$avgcatch[ii] <- sum(regions$avgcatch[insau]) / 1e3
}

tbl.value$value <- tbl.value$value / 1e6
tbl.value$catch <- tbl.value$catch / 1e3

tbl.value <- tbl.value[order(tbl.value$avgcatch, decreasing=T), c('sink', 'sovereign', 'avgcatch', 'avgecon', 'catch', 'value')]
tbl.value$sink <- as.character(tbl.value$sink)
tbl.value$sovereign <- as.character(tbl.value$sovereign)

tbl.value$sink <- gsub("Saint ", "St. ", tbl.value$sink)
tbl.value$sink <- gsub(" and ", " & ", tbl.value$sink)
tbl.value$sink[tbl.value$sink == "South Georgia & the South Sandwich Islands"] <- "SGSSI"
tbl.value$sink[tbl.value$sink == "Amsterdam Island & St. Paul Island"] <- "Amsterdam & St. Paul Islands"
tbl.value$sink[tbl.value$sink == "Northern Mariana Islands & Guam"] <- "Northern Mariana & Guam"
tbl.value$sink[tbl.value$sink == "Virgin Islands of the United States"] <- "US Virgin Islands"
tbl.value$sink[tbl.value$sink == "Democratic Republic of the Congo"] <- "Congo, Dem. Rep."
tbl.value$sink[tbl.value$sink == "République du Congo"] <- "Congo, Rep."
tbl.value$sink[tbl.value$sink == "St. Vincent & the Grenadines"] <- "St. Vincent"

tbl.value$sovereign <- gsub(" and ", " & ", tbl.value$sovereign)
tbl.value$sovereign[tbl.value$sovereign == "St. Vincent & the Grenadines"] <- "St. Vincent"

Encoding(tbl.value$sink) <- 'latin1'

names(tbl.value) <- c("EEZ", "Sovereign", "Avg. MTe3", "Avg. \\$M", "Risk MTe3", "Risk \\$M")

print(xtable(tbl.value, digits=1, label='tbl:risk-value'), file="risk-value.tex", sanitize.colnames.function=function(x) x, include.rownames=F, tabular.environment="longtable", floating=F)

## Weights
library(dplyr)

tbl.portion <- risk.portion %>% left_join(weights)
tbl.portion <- tbl.portion[order(tbl.portion$avgcatch, decreasing=T), c('sovereign', 'avgcatch', 'avgecon', 'gdp', 'population', 'workers', 'protein.pc', 'protein.ff', 'byecon', 'bygdp', 'byjobs', 'byfood')]
tbl.portion$gdp <- tbl.portion$gdp / 1e6
tbl.portion$population <- tbl.portion$population / 1e6
tbl.portion$workers <- tbl.portion$workers / 1e3
tbl.portion$avgecon <- tbl.portion$avgecon / 1e6
tbl.portion$avgcatch <- tbl.portion$avgcatch / 1e3

tbl.portion$byecon <- tbl.portion$byecon * 100
tbl.portion$bygdp <- tbl.portion$bygdp * 100
tbl.portion$byjobs <- tbl.portion$byjobs * 100
tbl.portion$byfood <- tbl.portion$byfood * 100

tbl.portion$sovereign <- gsub(" and ", " & ", tbl.portion$sovereign)
tbl.portion$sovereign[tbl.portion$sovereign == "St. Vincent & the Grenadines"] <- "St. Vincent"

names(tbl.portion) <- c("Sovereign", "Avg. MTe3", "Avg. \\$M", "GDP \\$M", "Pop. (1e6s)", "Workers (1e3s)", "Protein P.C.", "Fish Protein", "R. value (\\%)", "R. GDP (\\%)", "R. jobs (\\%)", "R. protein (\\%)")

print(xtable(tbl.portion, digits=1, label='tbl:risk-portion'), file="risk-portion.tex", sanitize.colnames.function=function(x) x, include.rownames=F, tabular.environment="longtable", floating=F)

risk.value$valueorder[order(risk.value$value, decreasing=T)] <- 1:nrow(risk.value)
risk.value$catchorder[order(risk.value$catch, decreasing=T)] <- 1:nrow(risk.value)

risk.value$logvalue <- log(risk.value$value)
risk.value$logcatch <- log(risk.value$catch)

risk.value$valueincluded <- risk.value$valueorder > 8 & risk.value$valueorder < 170

mod <- lm(logvalue ~ valueorder, data=risk.value[risk.value$valueincluded,])
summary(mod)

library(ggplot2)
library(scales)

base_breaks <- function(n = 10){
    function(x) {
        axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
    }
}

ggplot(risk.value, aes(valueorder, value / 1e6, colour=valueincluded)) +
    geom_point() + geom_abline(intercept=mod$coeff[1] - log(1e6), slope=mod$coeff[2]) +
    theme_bw() + scale_y_continuous(trans=log_trans(), breaks = base_breaks()) +
    xlab("Decreasing order") + ylab("Value from other countries ($M)") +
    theme(legend.justification=c(0,0), legend.position=c(.1, .1)) +
    scale_colour_manual(name="", breaks=c(T, F), labels=c("Included", "Excluded"), values=c("#a6cee3", "#33a02c")) +
    scale_x_continuous(expand=c(0, 0)) + ggtitle("Exponential decline in value from other countries")
ggsave("logplot-value.pdf", width=5, height=5)

risk.value$catchincluded <- risk.value$catchorder > 8 & risk.value$catchorder < 170

mod <- lm(logcatch ~ catchorder, data=risk.value[risk.value$catchincluded,])

ggplot(risk.value, aes(catchorder, catch / 1e3, colour=catchincluded)) +
    geom_point() + geom_abline(intercept=mod$coeff[1] - log(1e3), slope=mod$coeff[2]) +
    theme_bw() + scale_y_continuous(trans=log_trans(), breaks = base_breaks()) +
    xlab("Decreasing order") + ylab("Catch from other countries (1e3 MT)") +
    theme(legend.justification=c(0,0), legend.position=c(.1, .1)) +
    scale_colour_manual(name="", breaks=c(T, F), labels=c("Included", "Excluded"), values=c("#a6cee3", "#33a02c")) +
    scale_x_continuous(expand=c(0, 0)) + ggtitle("Exponential decline in catch from other countries")
ggsave("logplot-catch.pdf", width=5, height=5)
