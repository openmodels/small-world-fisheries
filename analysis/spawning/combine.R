setwd("~/Dropbox/High_Seas/codeR/spawning")
datapath <- "~/research/highseas/"

library(PBSmapping) # GIS mapping library

## Load the country definitions
regions <- importShapefile(paste0(datapath, "dataShapes/World_EEZ_v8_20140228_LR/World_EEZ_v8_2014"))
polydata <- attr(regions, 'PolyData')

source("tools_spawning.R")

specieseez <- read.csv("../fao2eez/specieseez.csv")

unrecogs <- data.frame(species=c(), country=c(), localities=c(), source=c())
df <- data.frame(species=c(), eez=c(), localities=c(), source=c())

## Add on all FishBase data
for (filename in list.files(paste0(datapath, "dataTEXT/spawning"))) {
    if (filename == "anadromous")
        next

    print(filename)
    species <- gsub("-", " ", substr(filename, 1, nchar(filename) - 4))

    ## Read this spawning data
    tbl <- read.csv(paste0(datapath, "dataTEXT/spawning/", filename))
    tbl$Locality <- as.character(tbl$Locality)

    ## Add on each EEZ
    for (country in unique(tbl$Country)) {
        if (is.na(country))
            subtbl <- subset(tbl, is.na(Country))
        else
            subtbl <- subset(tbl, Country == country)

        bymonth <- combinespawning(subtbl)
        if (sum(bymonth) == 0)
            bymonth <- rep(NA, 12)

        localities <- paste(subtbl$Locality, collapse=", ")

        index <- country2indexes(country, polydata)

        if (length(index) == 0) { # Can't identify
            subtbl$Locality[subtbl$Locality == "California Current region"] <- "California Current Region"
            for (locality in unique(subtbl$Locality)) {
                if (locality %in% c("Not specified", "not specified", "not given", "Not given", "to be filled", "Not given.", "Not stated", "Not stated.", "to be checked", "Not seen", "not specified, 1976", "Not specified."))
                    next
                indexes = specieseez$eez[specieseez$specie == species & specieseez$region == as.character(locality)]
                if (length(indexes) == 0) {
                    print(paste("         Cannot find", species, locality))
                    unrecogs <- rbind(unrecogs, data.frame(t(bymonth), species, country, localities, source="FishBase"))
                } else
                    df <- rbind(df, data.frame(t(bymonth), species, index=indexes, localities=locality, source="FishBase Matched"))
            }
        } else
            df <- rbind(df, data.frame(t(bymonth), species, index, localities, source="FishBase"))
    }
}

## Add on SCRFA records

monthnames <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

tbl <- read.csv(paste0(datapath, "dataTEXT/scrfa/data.csv"))

dropped <- c(list.files(paste0(datapath, "dataTEXT/spawning/anadromous")),
             list.files(paste0(datapath, "dataTEXT/fishdata/anadromous")))
dropped <- gsub("-", " ", gsub("\\.csv", "", unique(dropped)))

tbl$fullspecies <- paste(tbl$Genus, tbl$Species)

for (ii in 1:nrow(tbl)) {
    if (tbl$fullspecies[ii] %in% dropped)
        next

    if (ii %% 100 == 0)
        print(ii)
    months <- strsplit(as.character(tbl$SpawningMonths[ii]), " ")[[1]]
    bymonth <- rep(0, 12)
    for (month in months)
        bymonth[which(monthnames == month)] <- 100

    if (sum(bymonth) == 0)
        bymonth <- rep(NA, 12)

    species <- paste(tbl$Genus[ii], tbl$Species[ii])

    index <- country2indexes(tbl$CountryName[ii], polydata)

    if (length(index) == 0) # Can't identify
        unrecogs <- rbind(unrecogs, data.frame(t(bymonth), species, country=tbl$CountryName[ii], localities=NA, source="SCRFA"))
    else
        df <- rbind(df, data.frame(t(bymonth), species, index, localities=NA, source="SCRFA"))
}

unrecogs$country[unrecogs$country == ""] <- NA

write.csv(unrecogs, paste0(datapath, "dataCSV/spawning/unrecogs.csv"), row.names=F)
write.csv(df, paste0(datapath, "dataCSV/spawning/combined.csv"), row.names=F)

## Diagnostics

df <- read.csv(paste0(datapath, "dataCSV/spawning/combined.csv"))

length(unique(polydata$Country[df$index]))
length(unique(polydata$Country))

polydata$Country[!(polydata$Country %in% unique(polydata$Country[df$index]))]
