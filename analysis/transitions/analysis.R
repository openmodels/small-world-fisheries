do.windows <- .Platform$OS.type != "unix"
if (do.windows){
  setwd("C:/Users/Nandini/Dropbox/High_Seas/codeR/transitions")
  datapath <- "C:/Users/Nandini/OneDrive/Columbia/Research/Conflict hotspots/highseas/"
} else {
  setwd("~/Dropbox/High_Seas/codeR/transitions")
  datapath <- "~/Documents/highseas/" #"~/research/highseas/"
}

#Install library if needed:
#install.packages("PBSmapping")
#install.packages("maptools")
library(PBSmapping) # GIS mapping library

source("../spawning/tools_spawning.R")
source("larvdyn.R")

## The files are expected to be found at
## <datapath>dataCSV/<yeargroup.dir>transition_<yeargroup.infix>_<yeargroup.file>_<month>_<duration>.csv
## The output files will be named
## <datapath>dataCSV/spawn-transits<yeargroup.suffix>.csv
## <datapath>dataCSV/spawn-transits-rescaled<yeargroup.suffix>.csv

transfiles <- list("shelf-shelf"=list("cl"="shelf_shelf/transition_shelf_shelf_lo_res_cl",
                                      "no"="shelf_shelf/transition_shelf_shelf_lo_res",
                                      "ln"="shelf_shelf/transition_shelf_shelf_lo_res_ln",
                                      "en"="shelf_shelf/transition_shelf_shelf_lo_res_en"),
                   "eez-eez"=list("cl"="eez_eez/transition_all_lo_res_cl",
                                  "no"="eez_eez/transition_all_lo_res_neutral",
                                  "ln"="eez_eez/transition_all_lo_res_ln",
                                  "en"="eez_eez/transition_all_lo_res_en"))

## This contains 3 columns: (1) source.index, (2) number of particles that originate there, (3) 5/(2)
thresholds <- list("shelf-shelf"=read.csv(paste0(datapath, "dataCSV/shelf-shelf-particle-threshold-5.csv"), header=F),
                   "eez-eez"=read.csv(paste0(datapath, "dataCSV/eez-eez-particle-threshold-5.csv"), header=F))

cases <- c('', '30pshorter', 'halfreturn')
scenarios <- c('cl', 'aa')

do.30pshorter <- F
do.halfreturn <- F

## Load the country definitions
regions <- importShapefile(paste0(datapath, "dataShapes/World_EEZ_v8_20140228_LR/World_EEZ_v8_2014"))
polydata <- attr(regions, 'PolyData')

spawning <- read.csv(paste0(datapath, "dataCSV/spawning/combined.csv"))

## Load transitions dynamically
transitions <- list() # [eez-eez or shelf-shelf] => [cl or no or el or ln] => [month] => [duration]

## Return a list of the probabilities by EEZ of a fish spawned in
## country index `source.index` in month `month`.
get.probabilities <- function(source.index, srcdst, scenario, month, duration) {
    if (scenario == 'aa')
        return((get.probabilities(source.index, srcdst, 'no', month, duration) +
                get.probabilities(source.index, srcdst, 'ln', month, duration) +
                get.probabilities(source.index, srcdst, 'en', month, duration)) / 3)

    if (!(srcdst %in% names(transitions)))
        transitions[[srcdst]] <<- list()
    if (!(scenario %in% names(transitions[[srcdst]]))) {
        transitions[[srcdst]][[scenario]] <<- list()
        for (mm in 1:12) {
            transitions[[srcdst]][[scenario]][[mm]] <<- list()
            for (dd in 1:6)
                transitions[[srcdst]][[scenario]][[mm]][[dd]] <<- matrix(NA, 0, 0)
        }
    }
    if (dim(transitions[[srcdst]][[scenario]][[month]][[duration]])[1] == 0) {
        filename <- paste0(datapath, "dataCSV/transitions/", transfiles[[srcdst]][[scenario]], "_", month, "_", duration, ".csv")
        transitions[[srcdst]][[scenario]][[month]][[duration]] <<- read.csv(filename, header=F)
    }

    alltrans <- as.numeric(transitions[[srcdst]][[scenario]][[month]][[duration]][source.index,])
    ## Check if transition is less than threshold; if so, replace with 0
    alltrans <- alltrans * (alltrans >= thresholds[[srcdst]][source.index, 3])
    alltrans[250] <- alltrans[250] + 1 - sum(alltrans) # ensure that sum to 1
    alltrans
}

## get.probabilities(2, 'shelf-shelf', 'cl', 2, 1)

index2country <- function(index) {
    if (index == 250)
        "Open Sea"
    else
        as.character(polydata$Country[index])
}

## index2country(198)

## Determine the transition matrix for a single country, given spawning data
one.country.spawning <- function(tbl, country, duration, index, srcdst, scenario) {
    ## Determine spawning months
    countryspawn <- subset(tbl, Country == country)
    bymonth <- combinespawning(countryspawn)

    if (sum(bymonth) == 0)
        return(0) # no spawning data

    ## Collect weighted transitions
    weightedtrans <- rep(0, 250)
    for (month in which(bymonth > 0))
        weightedtrans <- weightedtrans + get.probabilities(index, srcdst, scenario, month, duration) * bymonth[month]

    ## Divide by total bymonth to get average
    weightedtrans / sum(bymonth)
}

## one.country.spawning(read.csv(paste0(datapath, "dataTEXT/spawning/Engraulis-ringens.csv")), "Peru", 2, country2indexes("Peru", polydata), 'shelf-shelf', 'cl')

## Like one.country.spawning, but applies to data in combined.csv
one.country.spawning.index <- function(tbl, index, duration, srcdst, scenario) {
    ## Determine spawning months
    countryspawn <- tbl[tbl$index == index,]
    bymonth <- combinespawning(countryspawn)

    if (sum(bymonth) == 0)
        return(0) # no spawning data

    ## Collect weighted transitions
    weightedtrans <- rep(0, 250)
    for (month in which(bymonth > 0))
        weightedtrans <- weightedtrans + get.probabilities(index, srcdst, scenario, month, duration) * bymonth[month]

    ## Divide by total bymonth to get average
    weightedtrans / sum(bymonth)
}

## Determine the transition probabilities of every fish in every country
## If rescale, Weight by prevelance in that country, so portions sum to 1
all.spawning.species <- function(filename, scenario, rescale=F) {
    tbl <- read.csv(paste0(datapath, "dataTEXT/spawning/", filename))
    if (file.exists(paste0(datapath, "dataCSV/bycountry/", filename)))
        bycountry <- read.csv(paste0(datapath, "dataCSV/bycountry/", filename))
    else
        bycountry <- NULL

    if (rescale) {
        ## Get PIDs for all spawning grounds
        total <- 0
        for (country in unique(tbl$Country)) {
            index <- country2indexes(country, polydata)
            if (length(index) == 0) { # Can't identify
                if (!is.na(country) && country != "" && country != "NA")
                    print(paste("Unrecognized country:", country))
                next
            }

            if (is.null(bycountry))
                total <- total + 1
            else if (sum(index %in% bycountry$PID) > 0) {
                total <- total + sum(bycountry$total[bycountry$PID %in% index])
            } else {
                bycountry <- rbind(bycountry, data.frame(PID=index, total=0))
            }
        }
    }

    result <- data.frame(source=c(), sink=c(), portion=c())
    for (country in unique(tbl$Country)) {
        index <- country2indexes(country, polydata)
        if (length(index) == 0) { # Can't identify
            if (!is.na(country) && country != "" && country != "NA")
                print(paste("Unrecognized country:", country))
            next
        }

        if (rescale) {
            if (is.null(bycountry))
                probweight <- 1 / total
            else
                probweight <- sum(bycountry$total[bycountry$PID %in% index]) / total
        } else
            probweight <- 1

        species <- gsub("-", " ", substr(filename, 1, nchar(filename) - 4))
        srcdst <- get.species.srcdst(species)
        durations <- round(ifelse(do.30pshorter, .7, 1) * get.species.durations(species)$durations / (365.25/12))
        durations[durations > 6] <- 6

        sumtransits <- rep(0, 250)
        for (duration in unique(durations)) {
            if (duration == 0)
                transits <- (1:250) == country2indexes(country, polydata)[1]
            else if (length(index) == 1)
                transits <- one.country.spawning(tbl, country, duration, index, srcdst, scenario)
            else {
                transits <- one.country.spawning(tbl, country, duration, index[1], srcdst, scenario)
                if (is.null(bycountry)) {
                    transits <- transits
                    for (oneindex in index[-1])
                        transits <- one.country.spawning(tbl, country, duration, oneindex, srcdst, scenario)
                    transits <- transits / length(index)
                } else {
                    transits <- transits * bycountry$total[bycountry$PID == index[1]]
                    for (oneindex in index[-1])
                        transits <- one.country.spawning(tbl, country, duration, oneindex, srcdst, scenario) * bycountry$total[bycountry$PID == oneindex]
                    transits <- transits / sum(bycountry$total[bycountry$PID %in% index])
                }
            }

            sumtransits <- sumtransits + transits * sum(durations == duration)
        }
        transits <- sumtransits / length(durations)

        knowncountry <- index2country(country2indexes(country, polydata)[1])

        if (do.halfreturn)
            transits <- (transits + ((1:length(transits)) == country2indexes(country, polydata)[1])) / 2

        for (sink in which(transits > 0))
            result <- rbind(result, data.frame(source=knowncountry, sink=index2country(sink), portion=transits[sink] * probweight))
    }

    result
}

## Like all.spawning.species, but applies to data in combined
all.spawning.species.combined <- function(species, scenario, rescale=F) {
    tbl <- spawning[spawning$species == species,]
    if (file.exists(paste0(datapath, "dataCSV/bycountry/", gsub(' ', '-', species), ".csv")))
        bycountry <- read.csv(paste0(datapath, "dataCSV/bycountry/", gsub(' ', '-', species), ".csv"))
    else
        bycountry <- NULL

    if (rescale) {
        ## Get PIDs for all spawning grounds
        total <- 0
        for (index in unique(tbl$index)) {
            if (is.null(bycountry))
                total <- total + 1
            else if (sum(index %in% bycountry$PID) > 0) {
                total <- total + sum(bycountry$total[bycountry$PID %in% index])
            } else {
                bycountry <- rbind(bycountry, data.frame(PID=index, total=0))
            }
        }
    }

    result <- data.frame(source=c(), sink=c(), portion=c())
    for (index in unique(tbl$index)) {
        if (rescale) {
            if (is.null(bycountry))
                probweight <- 1 / total
            else
                probweight <- sum(bycountry$total[bycountry$PID %in% index]) / total
        } else
            probweight <- 1

        srcdst <- get.species.srcdst(species)
        durations <- round(ifelse(do.30pshorter, .7, 1) * get.species.durations(species)$durations / (365.25 / 12))
        durations[durations > 6] <- 6

        sumtransits <- rep(0, 250)
        for (duration in unique(durations)) {
            if (duration == 0)
                transits <- (1:250) == index
            else if (length(index) == 1)
                transits <- one.country.spawning.index(tbl, index, duration, srcdst, scenario)
            else {
                transits <- one.country.spawning.index(tbl, index[1], duration, srcdst, scenario)
                if (is.null(bycountry)) {
                    transits <- transits
                    for (oneindex in index[-1])
                        transits <- one.country.spawning.index(tbl, oneindex, duration, srcdst, scenario)
                    transits <- transits / length(index)
                } else {
                    transits <- transits * bycountry$total[bycountry$PID == index[1]]
                    for (oneindex in index[-1])
                        transits <- one.country.spawning.index(tbl, oneindex, duration, srcdst, scenario) * bycountry$total[bycountry$PID == oneindex]
                    transits <- transits / sum(bycountry$total[bycountry$PID %in% index])
                }
            }

            sumtransits <- sumtransits + transits * sum(durations == duration)
        }
        transits <- sumtransits / length(durations)

        if (do.halfreturn)
            transits <- (transits + ((1:length(transits)) == index)) / 2

        knowncountry <- index2country(index)

        for (sink in which(transits > 0))
            result <- rbind(result, data.frame(source=knowncountry, sink=index2country(sink), portion=transits[sink] * probweight))
    }

    result
}

## all.spawning.species("Engraulis-ringens.csv", 'cl')

all.spawning <- function(scenario, rescale=F) {
    result <- data.frame(species=c(), source=c(), sink=c(), portion=c())
    for (filename in list.files(paste0(datapath, "dataTEXT/spawning"))) {
        ## if (!file.exists(paste0(datapath, "dataCSV/bycountry/", filename)))
        ##     next

        result.species <- all.spawning.species(filename, scenario, rescale=rescale)
        if (nrow(result.species) == 0)
            next

        species <- gsub("-", " ", substr(filename, 1, nchar(filename) - 4))
        result.species$species <- species
        result <- rbind(result, result.species)
    }

    result
}

## Like all.spawning, but uses combined
all.spawning.combined <- function(scenario, rescale=F) {
    result <- data.frame(species=c(), source=c(), sink=c(), portion=c())
    for (species in unique(spawning$species)) {
        ## if (!file.exists(paste0(datapath, "dataCSV/bycountry/", gsub(' ', '-', species), ".csv")))
        ##     next

        result.species <- all.spawning.species.combined(species, scenario, rescale=rescale)
        if (nrow(result.species) == 0)
            next

        result.species$species <- species
        result <- rbind(result, result.species)
    }

    result
}


for (case in cases) {
    do.30pshorter <- case == '30pshorter'
    do.halfreturn <- case == 'halfreturn'

    for (scenario in scenarios) {
        print(c(case, scenario))
        result <- all.spawning.combined(scenario)
        if (case != "")
            write.csv(result, file=paste0(datapath, "dataCSV/spawn-transits/spawn-transits-", scenario, "-", case, ".csv"), row.names=F)
        else
            write.csv(result, file=paste0(datapath, "dataCSV/spawn-transits/spawn-transits-", scenario, ".csv"), row.names=F)

        result <- all.spawning.combined(scenario, rescale=T)
        if (case != "")
            write.csv(result, file=paste0(datapath, "dataCSV/spawn-transits/spawn-transits-rescaled-", scenario, "-", case, ".csv"), row.names=F)
        else
            write.csv(result, file=paste0(datapath, "dataCSV/spawn-transits/spawn-transits-rescaled-", scenario, ".csv"), row.names=F)
    }
}
