## setwd("~/Dropbox/High_Seas/codeR")
## datapath <- "~/research/highseas"

source("names.R")

comtrade <- read.csv(file.path(datapath, "dataSources/comtrade.csv"))

comtradedf <- data.frame(country=c(), iso=c(), year=c(), import.weight=c(), export.weight=c(), import.value=c(), export.value=c())
for (year in min(comtrade$Year):max(comtrade$Year)) {
    print(year)
    for (country in unique(comtrade$Reporter)) {
        tbl <- subset(comtrade, Reporter == country & Year == year)
        if (nrow(tbl) == 0)
            next
        import.weight <- sum(as.numeric(tbl$Netweight..kg.[tbl$Trade.Flow == "Import"], na.rm=T) / 1000) # MT
        export.weight <- sum(as.numeric(tbl$Netweight..kg.[tbl$Trade.Flow == "Export"], na.rm=T) / 1000) # MT
        import.value <- sum(as.numeric(tbl$Trade.Value..US..[tbl$Trade.Flow == "Import"], na.rm=T))
        export.value <- sum(as.numeric(tbl$Trade.Value..US..[tbl$Trade.Flow == "Export"], na.rm=T))
        reimport.weight <- sum(as.numeric(tbl$Netweight..kg.[tbl$Trade.Flow == "Re-Import"], na.rm=T) / 1000) # MT
        reexport.weight <- sum(as.numeric(tbl$Netweight..kg.[tbl$Trade.Flow == "Re-Export"], na.rm=T) / 1000) # MT
        reimport.value <- sum(as.numeric(tbl$Trade.Value..US..[tbl$Trade.Flow == "Re-Import"], na.rm=T))
        reexport.value <- sum(as.numeric(tbl$Trade.Value..US..[tbl$Trade.Flow == "Re-Export"], na.rm=T))
        comtradedf <- rbind(comtradedf, data.frame(country=canonical2worldbank(country), iso=tbl$Reporter.ISO[1], year, import.weight, export.weight, import.value, export.value, reimport.weight, reexport.weight, reimport.value, reexport.value))
    }
}

## Add on production
## countryinfo <- read.csv(file.path(datapath, "dataCSV/weights/combined-country.csv"))

## atrisk <- read.csv("../results/atrisk/portion-shelf-cl-new.csv")
calc.food.table <- function(sovereign, year0, year1) {
    ## Missing from countryinfo:
    ## print(unique(comtradedf$country)[!(unique(comtradedf$country) %in% unique(countryinfo$sovereign))])
    ## ## Missing from comtrade:
    ## print(unique(countryinfo$sovereign)[!(unique(countryinfo$sovereign) %in% unique(comtradedf$country))])

    ## master <- countryinfo[!is.na(countryinfo$sovereign), c('sovereign', 'avgecon', 'avgcatch', 'population', 'protein.ff', 'protein.pc', 'protein.an')]
    ## master$import.weight <- NA
    ## master$export.weight <- NA
    ## master$import.value <- NA
    ## master$export.value <- NA
    ## master$atrisk <- atrisk$catch[!is.na(atrisk$sovereign)] / master$avgcatch
    ## master$protein.req <- 48
    ## ##master$atrisk.value <- atrisk$value[!is.na(atrisk$sovereign)] / master$avgecon <- same!
    ## for (ii in 1:nrow(master)) {

    rows <- subset(comtradedf, country == as.character(sovereign) & year >= year0 & year <= year1)
    import.weight <- mean(rows$import.weight, na.rm=T)
    export.weight <- mean(rows$export.weight, na.rm=T)
    import.value <- mean(rows$import.value, na.rm=T)
    export.value <- mean(rows$export.value, na.rm=T)
    reimport.weight <- mean(rows$reimport.weight, na.rm=T)
    reexport.weight <- mean(rows$reexport.weight, na.rm=T)
    reimport.value <- mean(rows$reimport.value, na.rm=T)
    reexport.value <- mean(rows$reexport.value, na.rm=T)

    c(import.weight, export.weight, import.value, export.value, reimport.weight, reexport.weight, reimport.value, reexport.value)
}

##write.csv(master, "../results/master-food.csv", row.names=F)
