setwd("~/Dropbox/High_Seas/codeR")
datapath <- "~/research/highseas/"

library(dplyr)
library(xtable)

print("SAU")
sau <- read.csv(file.path(datapath, "dataTEXT/saudata-new/species.csv"))

df <- data.frame(scientific=unique(sau$scientific), sau.catch=T)

if (F) {
    sau <- read.csv(file.path(datapath, "dataCSV/groups.csv"))
    df <- df %>% full_join(data.frame(scientific=unique(sau$scientific), sau.groups=T))

    print("Fishbase Summaries")
    fishbase <- data.frame(scientific=c(), fishbase.summary=c())
    for (filename in list.files(paste0(datapath, "dataTEXT/xmls"))) {
        species <- gsub("-", " ", substr(filename, 1, nchar(filename) - 4))
        fishbase <- rbind(fishbase, data.frame(scientific=species, fishbase.summary=T))
    }

    df <- df %>% full_join(fishbase)
}

print("Fishbase Spawning")
fishbase <- data.frame(scientific=c(), fishbase.spawning=c())
for (filename in list.files(paste0(datapath, "dataTEXT/spawning"))) {
    if (filename == "anadromous")
        next

    species <- gsub("-", " ", substr(filename, 1, nchar(filename) - 4))
    fishbase <- rbind(fishbase, data.frame(scientific=species, fishbase.spawning=T))
}

df <- df %>% full_join(fishbase)

print("SCRFA")
tbl <- read.csv(paste0(datapath, "dataTEXT/scrfa/data.csv"))
tbl$fullspecies <- paste(tbl$Genus, tbl$Species)

scrfa <- data.frame(scientific=unique(paste(tbl$Genus, tbl$Species)), scrfa.spawning=T)

df <- df %>% full_join(scrfa)

## Count the total groups we have available
dfcnt <- df # XXY Hard-coded!
dfcnt$fishbase.spawning[dfcnt$scientific %in% c("Arius", "Etmopterus", "Leiognathus", "Nemipterus", "Siganus", "Gerres")] <- T

sum(!is.na(dfcnt$sau.catch) & dfcnt$sau.catch & ((!is.na(dfcnt$fishbase.spawning) & dfcnt$fishbase.spawning | !is.na(dfcnt$scrfa.spawning) & dfcnt$scrfa.spawning)))

print("Aquamaps")
aquamaps <- data.frame(scientific=c(), aquamaps=c())
for (filename in list.files(paste0(datapath, "dataTEXT/fishdata"))) {
    if (filename == "anadromous")
        next

    species <- gsub("-", " ", substr(filename, 1, nchar(filename) - 4))
    aquamaps <- rbind(aquamaps, data.frame(scientific=species, aquamaps=T))
}

df <- df %>% left_join(aquamaps) # left join, because only use maps if have species

print("Fishbase Larval Dynamics")

source("transitions/larvdyn.R")

df <- df[df$scientific != "",]

df <- df %>% full_join(data.frame(scientific=larvdyn$scientific, larvdyn.level='species'))
df$larvdyn.level <- as.character(df$larvdyn.level)
for (ii in which(is.na(df$larvdyn.level)))
    df$larvdyn.level[ii] <- get.species.durations(df$scientific[ii])$level

dropped <- c(list.files(paste0(datapath, "dataTEXT/spawning/anadromous")),
             list.files(paste0(datapath, "dataTEXT/fishdata/anadromous")))
dropped <- gsub("-", " ", gsub("\\.csv", "", unique(dropped)))

df <- subset(df, !(scientific %in% dropped))

df[is.na(df)] = F

## Fix up genus names
for (genus in df$scientific[grep("sp\\.", df$scientific)]) {
    shortgenus <- strsplit(genus, " ")[[1]][1]
    df$scientific[df$scientific == genus] <- shortgenus
    df[which(df$scientific == shortgenus)[1], -c(1, 6)] <- colMeans(df[df$scientific == shortgenus, -c(1, 6)]) > 0
}

df <- df[!duplicated(df$scientific),]

df$genus <- sapply(strsplit(df$scientific, " "), function(x) x[1])
df <- subset(df, !is.na(genus))

for (genus in unique(df$genus)) {
    if (sum(df$scientific == genus) == 1)
        df[which(df$scientific == genus), -c(1, 6, ncol(df))] <- colMeans(df[df$genus == genus, -c(1, 6, ncol(df))]) > 0
    else
        df <- rbind(df, cbind(data.frame(scientific=genus, genus=genus, larvdyn.level=get.species.durations(genus)$level), t(colMeans(df[df$genus == genus, -c(1, 6, ncol(df))]) > 0)))
}

df$is.genus <- df$scientific == df$genus

## Diagnostic
c <- df$sau.catch & (df$fishbase.spawning | df$scrfa.spawning)
table(sapply(unique(df$scientific[df$is.genus & use.subset]),
             function(genus) length(df$scientific[use.subset][grep(genus, df$scientific[use.subset])])))
sapply(unique(df$scientific[!df$is.genus & use.subset]), function(biword) strsplit(biword, " ")[[1]][1]) %in% unique(df$scientific[df$is.genus & use.subset])

df$larvdyn.level <- factor(df$larvdyn.level, levels=rev(c('species', 'genus', 'family', 'order', 'class', 'phylum', 'kingdom')))
df$combo <- 2^2*8 * df$sau.catch + 2*8 * (df$fishbase.spawning | df$scrfa.spawning) + 8 * df$aquamaps + as.numeric(df$larvdyn.level)

tbl <- data.frame(combo=c(), num.species=c(), num.genus=c())
for (combo in unique(df$combo)) {
    columns <- intToBits(combo)[1:6] == 1
    larvdyn.level <- rev(c('species', 'genus', 'family', 'order', 'class', 'phylum', 'kingdom'))[columns[1] + 2*columns[2] + 4*columns[3]]
    if (larvdyn.level %in% c('phylum', 'kingdom'))
        larvdyn.level <- 'other'
    tbl <- rbind(tbl, data.frame(combo, sau.catch=columns[6], spawning=columns[5], aquamaps=columns[4], larvdyn.level,
                 num.species=sum(df$combo == combo & !df$is.genus), num.genus=sum(df$combo == combo & df$is.genus)))
}

tbl <- tbl[tbl$sau.catch | tbl$spawning,] # must have one of these
extra1 <- data.frame(combo=NA, sau.catch=T, spawning=F, aquamaps=NA, larvdyn.level=NA, num.species=sum(tbl$num.species[tbl$sau.catch & !tbl$spawning]), num.genus=sum(tbl$num.genus[tbl$sau.catch & !tbl$spawning]))
extra2 <- data.frame(combo=NA, sau.catch=F, spawning=T, aquamaps=NA, larvdyn.level=NA, num.species=sum(tbl$num.species[!tbl$sau.catch & tbl$spawning]), num.genus=sum(tbl$num.genus[!tbl$sau.catch & tbl$spawning]))
tbl <- tbl[tbl$sau.catch & tbl$spawning,]

tbl <- tbl[order(tbl$combo, decreasing=T),]

disptbl <- tbl[, -1]

summrow1 <- data.frame(sau.catch=c(sum(tbl$num.species[tbl$sau.catch]), sum(tbl$num.genus[tbl$sau.catch])),
                      spawning=c(sum(tbl$num.species[tbl$spawning]), sum(tbl$num.genus[tbl$spawning])),
                      aquamaps=c(sum(tbl$num.species[tbl$aquamaps], na.rm=T), sum(tbl$num.genus[tbl$aquamaps], na.rm=T)),
                      larvdyn.level=c(nrow(larvdyn) - length(grep("sp\\.|NA", larvdyn$scientific)), length(grep("sp\\.|NA", larvdyn$scientific))),
                      num.species=c(sum(tbl$num.species), NA), num.genus=c(NA, sum(tbl$num.genus)))

tbl <- rbind(tbl, extra1, extra2)

summrow2 <- data.frame(sau.catch=c(sum(tbl$num.species[tbl$sau.catch]), sum(tbl$num.genus[tbl$sau.catch])),
                      spawning=c(sum(tbl$num.species[tbl$spawning]), sum(tbl$num.genus[tbl$spawning])),
                      aquamaps=c(sum(tbl$num.species[tbl$aquamaps], na.rm=T), sum(tbl$num.genus[tbl$aquamaps], na.rm=T)),
                      larvdyn.level=c(nrow(larvdyn) - length(grep("sp\\.|NA", larvdyn$scientific)), length(grep("sp\\.|NA", larvdyn$scientific))),
                      num.species=c(sum(tbl$num.species), NA), num.genus=c(NA, sum(tbl$num.genus)))

names(disptbl) <- c("SAU", "Spawning", "AquaMaps", "Larvae Level", "\\# Species", "\\# Genuses")
print(xtable(disptbl), file="../paper/tables/sumdata2.tex", include.rownames=F, sanitize.colnames.function=function(x) x, sanitize.text.function=function(x) ifelse(x == T, "\\checkmark", ifelse(x == F, "", x)), NA.string="$\\cdot$", floating=F)

names(summrow1) <- c("SAU", "Spawning", "AquaMaps", "Larvae Level", "\\# Species", "\\# Genuses")
print(xtable(summrow1), include.rownames=F, sanitize.colnames.function=function(x) x)

print(xtable(rbind(extra1, extra2)[, -1]), include.rownames=F, sanitize.colnames.function=function(x) x, sanitize.text.function=function(x) ifelse(x == T, "\\checkmark", ifelse(x == F, "", x)), NA.string="$\\cdot$", floating=F)

names(summrow2) <- c("SAU", "Spawning", "AquaMaps", "Larvae Level", "\\# Species", "\\# Genuses")
print(xtable(summrow2), include.rownames=F, sanitize.colnames.function=function(x) x)

## Determine known shares

values <- read.delim(paste0(datapath, "dataTEXT/saudata-new/values.csv"), sep=",", header=F)
catches <- read.delim(paste0(datapath, "dataTEXT/saudata-new/catches.csv"), sep=",", header=F)
groups <- read.csv(paste0(datapath, "dataTEXT/saudata-new/species.csv"))

vv = data.frame(key=values[,1], value=rowMeans(values[,(ncol(values)-9):ncol(values)], na.rm=T)) %>% group_by(key) %>% summarize(value=sum(value, na.rm=T))
cc <- data.frame(key=catches[,1], catch=rowMeans(catches[,(ncol(catches)-9):ncol(catches)], na.rm=T)) %>% group_by(key) %>% summarize(catch=sum(catch, na.rm=T))

unigroups <- groups[!duplicated(groups$key),]
avgs <- unigroups %>% left_join(vv) %>% left_join(cc)

sum(avgs$value[avgs$scientific == ""]) / sum(avgs$value) # 10.6%
sum(avgs$catch[avgs$scientific == ""]) / sum(avgs$catch) # 13.9%

avgs[avgs$scientific != "",][which.max(avgs$value[avgs$scientific != ""]),]

sum(avgs$catch[avgs$scientific == "Scyphozoa"]) / sum(avgs$catch) # 1.6%
sum(avgs$value[avgs$scientific == "Scyphozoa"]) / sum(avgs$value) # 4.4%

sum(avgs$value[avgs$scientific %in% df$scientific[use.subset]]) / sum(avgs$value) # 38.4%
sum(avgs$catch[avgs$scientific %in% df$scientific[use.subset]]) / sum(avgs$catch) # 51.1%
