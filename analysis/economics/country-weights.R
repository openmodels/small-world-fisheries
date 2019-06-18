## This file produces the "weights/combined-temp-nr.csv" file which
## describes regions and countries

## All paths are relative after this point.  Change here as needed.
datapath <- "../../../data/"
resultpath <- "../../../results/"

filesuffix <- "" #"-1995-2004" #"-temp-nr"

if (filesuffix == "-1995-2004") {
    fromyear <- -19
    toyear <- -10
} else if (filesuffix == "") {
    fromyear <- -9
    toyear <- 0
}

## Plots and maps are produced at the bottom if these are T
do.generate <- T
do.plots <- F
do.maps <- F

if (do.generate) {

## `countries` provides a name for each SAU region
## Sea Around Us data has code numbers for each country, countries.txt has the mapping for these codes
## SAU data - catch & landed values
countries <- read.delim(paste0(datapath, "saudata/countries.txt"), sep="\t", header=F)

## Extract the "Totals" row for SAU files
## Returns a matrix with rows of regions and columns of years (1950 - 2006)
name.to.extract <- "Total" #"Others"
skip.after <- 1 #2
sau.extract.totals <- function(values) {
    regionids <- unique(countries[,1])

    ## Construct totals
    totals <- as.data.frame(matrix(NA, ncol(values)-1, length(regionids)))
    names(totals) <- regionids
    lastkey <- values[1,1]
    for (ii in 1:nrow(values)) {
        if (values[ii,1] == name.to.extract) {
            ## At this point, we have everything
            if (ii == 1943) {
                break
            }
            totals[,as.character(lastkey)] <- as.numeric(values[ii, -1])
            lastkey <- values[ii+skip.after,1]
        }
    }

    totals
}

## Load all landed catch values
values <- read.delim(paste0(datapath, "saudata/values.csv"), sep=",", header=F)

totals <- sau.extract.totals(values)

## Load all catch MTs
values <- read.delim(paste0(datapath, "saudata/catches.csv"), sep=",", header=F)

totals2 <- sau.extract.totals(values)

regionids <- unique(countries[,1])

## Construct protein totals
speciesinfo <- read.csv(paste0(datapath, "saudata/species.csv"))
source("../food/protein.R")

regionids <- unique(countries[,1])
proteintotals2 <- as.data.frame(matrix(NA, ncol(values)-1, length(regionids)))
names(proteintotals2) <- regionids
lastkey <- values[1,1]
keyrow <- 1
for (ii in 1:nrow(values)) {
    if (values[ii,1] == name.to.extract) {
        ## At this point, we have everything
        if (ii == 1943)
            break

        eeznum <- as.numeric(substring(lastkey, 7, nchar(as.character(lastkey)) - 5))
        mytotals <- as.numeric(values[ii, -1])
        mysplits <- as.numeric(colSums(values[(keyrow+1):(ii-1), -1], na.rm=T))
        proteintotals2[,as.character(lastkey)] <- 0
        for (kk in (keyrow+1):(ii-1)) {
            scientific <- subset(speciesinfo, eez == eeznum & key == as.character(values[kk, 1]))$scientific
            if (length(scientific) == 0 || scientific == "")
                protein <- 17.8
            else
                protein <- get.protein(as.character(scientific))$protein
            kkvalues <- as.numeric(values[kk, -1]) * protein
            kkvalues[is.na(kkvalues)] <- 0
            proteintotals2[,as.character(lastkey)] <- proteintotals2[,as.character(lastkey)] + kkvalues
        }
        proteintotals2[,as.character(lastkey)] <- proteintotals2[,as.character(lastkey)] * mytotals / mysplits

        lastkey <- values[ii+skip.after,1]
        keyrow <- ii + skip.after
    }
}

## Construct by-country information
regioninfo <- read.delim(paste0(datapath, "saudata/countryinfo.csv"), sep=",", header=T)
regioninfo <- regioninfo[!duplicated(regioninfo),] # drop duplicates

## Load naming tools
source("../lib/names.R") # this is in http://github.com/jrising/research-common

sau2canonical <- function(country) {
    if (country %in% c("China (Hong Kong)", "Hong Kong (China)"))
        return("Hong Kong")
    if (country == "Comoros Isl.")
      return("Comoros")
    if (country == "CÃ´te d'Ivoire")
      return("Cote d'Ivoire")
    if (country %in% c("Denmark (Greenland)", "Greenland (Denmark)"))
      return("Greenland")
    if (country == "India (mainland)")
      return("India")
    if (country == "Viet Nam")
        return("Vietnam")

    return(gsub("&amp;", "&", as.character(country)))
}

## Fill in the name, and recent averages of landed total and catch total for each region
regioninfo$name <- NA
regioninfo$sovereign <- NA #sorts out which sovereign nation each EEZ is part of
regioninfo$eez <- NA
regioninfo$avgecon <- NA
regioninfo$avgcatch <- NA
regioninfo$avgproteincatch <- NA
for (ii in 1:ncol(totals)) {
  if (is.na(totals[1,ii]))
      next

  row <- regioninfo$country == names(totals)[ii]

  chosen <- countries[,1] == names(totals)[ii]
  if (sum(chosen) > 1) {
      ## Handle some special cases
      if (names(totals)[ii] == "~/eez/16.aspx")
          name <- "American Samoa (USA)"
      else if (names(totals)[ii] == "~/eez/344.aspx")
          name <- "Hong Kong (China)"
      else if (names(totals)[ii] == "~/eez/304.aspx")
          name <- "Greenland (Denmark)"
      else if (names(totals)[ii] == "~/eez/254.aspx")
          name <- "French Guiana (France)"
      else if (names(totals)[ii] == "~/eez/258.aspx")
          name <- "French Polynesia (France)"
      else if (names(totals)[ii] == "~/eez/850.aspx")
          name <- "US Virgin Isl. (USA)"
      else {
          ## Many regions are given two names: Foo (Country) and Country (Foo)
          ## Make it so we consistently use Foo (Country)
          parencountries <- c("\\(USA\\)", "\\(France\\)", "\\(Morocco\\)", "\\(UK\\)", "\\(Brazil\\)", "\\(New Zealand\\)",
                              "\\(Norway\\)", "\\(Windward Netherlands Antilles\\)", "\\(Chile\\)", "\\(India\\)",
                              "\\(Leeward Netherlands Antilles\\)", "\\(Australia\\)", "\\(Portugal\\)", "\\(Spain\\)",
                              "\\(Denmark\\)", "\\(Ecuador\\)", "\\(Haiti\\)", "\\(South Africa\\)")
          chosen <- which(chosen)
          for (pattern in parencountries) {
              founds <- grep(pattern, as.character(countries[chosen,2]))
              if (length(founds) == 1) {
                  chosen <- chosen[founds]
                  break
              } else if (length(founds) > 1) {
                  ## Two options for "Amsterdam &amp; St Paul Isl. (France)", so go with first
                  chosen <- chosen[founds][1]
                  break
              }
          }

          if (length(chosen) > 1) {
              print(paste("Cannot find sovereign for", ii, as.character(countries[chosen,2])))
              chosen <- chosen[1]
          }

          name <- sau2canonical(as.character(countries[chosen,2])[1])
      }
  } else
      name <- sau2canonical(as.character(countries[chosen,2])[1])

  if (sum(row) > 0) {
      regioninfo$name[row] <- name
      regioninfo$sovereign[row] <- canonical2worldbank(name)

      ## Add the average of the last decade of values
      regioninfo$avgecon[row] <- mean(totals[(dim(totals)[1]+fromyear):(dim(totals)[1]+toyear),ii])
      regioninfo$avgcatch[row] <- mean(totals2[(dim(totals2)[1]+fromyear):(dim(totals2)[1]+toyear),ii])
      regioninfo$avgproteincatch[row] <- mean(proteintotals2[(dim(proteintotals2)[1]+fromyear):(dim(proteintotals2)[1]+toyear),ii])
  } else {
      regioninfo <- rbind(regioninfo, data.frame(country=names(totals)[ii], eez.km2.=NA, shelf.km2.=NA, inshore.km2.=NA, reefs.pow.=NA, mounts.pow.=NA, primeprod=NA, name, sovereign=canonical2worldbank(name), avgecon=mean(totals[(dim(totals)[1]+fromyear):(dim(totals)[1]+toyear),ii]), avgcatch=mean(totals2[(dim(totals2)[1]+fromyear):(dim(totals2)[1]+toyear),ii]), avgproteincatch=mean(proteintotals2[(dim(proteintotals2)[1]+fromyear):(dim(proteintotals2)[1]+toyear),ii]), eez=NA))
  }
}

## XXX: use sovereign for EEZ for now
regioninfo$eez <- regioninfo$sovereign

## Write out region weights
write.csv(regioninfo, paste0(datapath, "weights/combined-regions-new", filesuffix, ".csv"), row.names=F)

## Aggregate to the country (sovereign) level
countryinfo <- data.frame(sovereign=c(), eez.km2.=c(), shelf.km2.=c(), inshore.km2.=c(), reefs.pow.=c(), mounts.pow.=c(), primeprod=c(), avgecon=c(), avgcatch=c(), avgproteincatch=c())
for (sovereign in unique(regioninfo$sovereign)) {
    rows <- regioninfo$sovereign == sovereign
    countryinfo <- rbind(countryinfo, data.frame(sovereign, eez.km2.=sum(regioninfo$eez.km2.[rows], na.rm=T),
                                                 shelf.km2.=sum(regioninfo$shelf.km2.[rows], na.rm=T),
                                                 inshore.km2.=sum(regioninfo$inshore.km2.[rows], na.rm=T),
                                                 reefs.pow.=sum(regioninfo$reefs.pow.[rows] * regioninfo$eez.km2.[rows], na.rm=T) / sum(regioninfo$eez.km2.[rows], na.rm=T),
                                                 mounts.pow.=sum(regioninfo$mounts.pow.[rows] * regioninfo$eez.km2.[rows], na.rm=T) / sum(regioninfo$eez.km2.[rows], na.rm=T),
                                                 primeprod=sum(as.numeric(regioninfo$primeprod[rows]) * regioninfo$eez.km2.[rows], na.rm=T) / sum(regioninfo$eez.km2.[rows], na.rm=T),
                                                 avgecon=sum(regioninfo$avgecon[rows], na.rm=T),
                                                 avgcatch=sum(regioninfo$avgcatch[rows], na.rm=T),
                                                 avgproteincatch=sum(regioninfo$avgproteincatch[rows], na.rm=T)))
}

## Add the number of fisheries workers
countryinfo$workers <- NA

## These are translations from workers.csv
translations <- list("Antigua and Barbuda "="Antigua &amp; Barbuda",
                     "Brunei Darsm"="Brunei Darussalam",
                     "Comoros"="Comoros Isl.",
                     "Congo (DemRep)"="Congo (ex-Zaire)",
                     "Congo (Rep.)"="Congo, R. of",
                     "Cote d'Ivoire"="Côte d'Ivoire",
                     "Dominican Rp"="Dominican Republic",
                     "Eq Guinea"="Equatorial Guinea",
                     "GuineaBissau"="Guinea-Bissau",
                     "India"="India (mainland)", # prob. includes others
                                        # Indonesia="Indonesia (Eastern)","Indonesia (Western)"
                     "Japan"="Japan (main islands)", # prob. includes others
                     "Korea South"="Korea (South)",
                                        # Malaysia=4 pieces
                     "Marshall Islands"="Marshall Isl.",
                                        # Russia=6 pieces
                     "Saint Kitts and Nevis"="Saint Kitts &amp; Nevis",
                     "Sao Tome Prn"="Sao Tome &amp; Principe",
                                        # Saudi Arabia="Saudi Arabia (Persian Gulf)","Saudi Arabia (Red Sea)"
                     "Solomon Islands"="Solomon Isl.",
                     "St Vincent"="Saint Vincent &amp; the Grenadines",
                     "St. Lucia"="Saint Lucia",
                     "Trinidad and Tobago"="Trinidad &amp; Tobago",
                                        # Turkey="Turkey (Black Sea)","Turkey (Mediterranean Sea)"
                     "Untd Arab Em"="United Arab Emirates",
                                        # USA=3 pieces + Hawaii + Alaska
                     "Vietnam"="Viet Nam")

## Load the worker data
workers <- read.csv(paste0(datapath, "weights/workers.csv"))
for (ii in 1:nrow(workers)) {
    ptval <- as.numeric(strsplit(as.character(workers$Total[ii]), "±")[[1]][1])
    countryname <- as.character(workers$Country[ii])

    ## Use WB to make sovereign
    ## Not including these in translations, because they're for pieces
    if (countryname == "Russian Fed")
        countryname <- "Russian Federation"
    if (countryname == "Saint Kitts and Nevis")
        countryname <- "Saint Kitts & Nevis"
    if (countryname == "Sao Tome Prn")
        countryname <- "Sao Tome & Principe"
    if (countryname == "St Vincent")
        countryname <- "Saint Vincent & the Grenadines"
    if (countryname == "USA")
        countryname <- "United States"

    found <- c(which(countryinfo$sovereign == countryname))
    for (jj in 1:nrow(countryinfo)) {
        if (!is.na(countryinfo$sovereign[jj])) {
            if (canonical2worldbank(countryinfo$sovereign[jj]) == canonical2worldbank(countryname))
                found <- c(found, jj) # duplicated, so list
        }
    }
    if (!is.null(translations[[countryname]]))
        found <- c(found, which(countryinfo$sovereign == translations[[countryname]]))

    if (length(found) > 0)
        countryinfo$workers[found] <- ptval * 1000 # reported in thousands
    else
        print(paste(ii, workers$Country[ii], length(found)))
}

## Add some World Bank data
countryinfo$gdp <- NA # GDP
countryinfo$population <- NA # Population
countryinfo$laborforce <- NA # Labor force
countryinfo$surface.area <- NA # Surface area

gdps <- read.csv(paste0(datapath, "weights/ny.gdp.mktp.kd_Indicator_en_csv_v2.csv"))
pops <- read.csv(paste0(datapath, "weights/sp.pop.totl_Indicator_en_csv_v2.csv"))
labs <- read.csv(paste0(datapath, "weights/API_SL.TLF.TOTL.IN_DS2_en_csv_v2_10516832.csv"))
surs <- read.csv(paste0(datapath, "weights/ag.srf.totl.k2_Indicator_en_csv_v2.csv"))
country.column <- "Country.Name"

## Averaging values over the same years as for SAU data (97-05)
for (countryname in unique(gdps[, country.column])) {
    ## names(gdps)[(59+fromyear):(59+toyear)]
    gdp <- mean(as.numeric(gdps[gdps[, country.column] == countryname,(59+fromyear):(59+toyear)]), na.rm=T)
    pop <- mean(as.numeric(pops[pops[, country.column] == countryname,(59+fromyear):(59+toyear)]), na.rm=T)
    lab <- mean(as.numeric(labs[labs[, country.column] == countryname,(59+fromyear):(59+toyear)]), na.rm=T)
    sur <- mean(as.numeric(surs[surs[, country.column] == countryname,(59+fromyear):(59+toyear)]), na.rm=T)

    ## Try to find this, using canonical2worldbank
    found <- c()
    for (jj in 1:nrow(countryinfo)) {
        if (!is.na(countryinfo$sovereign[jj])) {
            if (canonical2worldbank(countryinfo$sovereign[jj]) == countryname)
                found <- c(found, jj) # duplicated, so list
        }
    }
    ##found <- countryinfo$sovereign == countryname
    if (length(found) > 0) {
        countryinfo$gdp[found] <- gdp
        countryinfo$population[found] <- pop
        countryinfo$laborforce[found] <- lab
        countryinfo$surface.area[found] <- sur
    } else
        print(paste(countryname, sum(found)))
}

## # FAO data
## # Add protein supply per capita from fish, protein supply per capita, food supply from fish, food supply per capita
countryinfo$protein.ff <- NA #protein from fish  g per day per capita
countryinfo$protein.pc <- NA #total protein g per day per per capita
countryinfo$protein.an <- NA #total animal protein g per day per capita
## countryinfo$food.ff <- NA #food from fish kcal per day per capita
## countryinfo$food.pc <- NA #total food kcal per day per capita
## countryinfo$food.weight <- NA #food security weighting that takes into account protein availability etc

food.all <- read.csv(paste0(datapath, "weights/Food_data_2011.csv"))

count <- 0
for (countryname in unique(food.all$Country)) {
#The following 2 lines were used when considering calories from fish (in Food_data_2011.csv)
#  food.pc <- food.all[, 3]
#  food.ff <- food.all[, 2]
  protein.pc <- food.all$Total.Animal.Protein.Supply..g.capita.day.
  protein.ff <- food.all$Protein.Supply.From.Fish..g.capita.day.
  protein.an <- food.all$Total.Protein.Supply..g.capita.day.
    ##food.weight <- food.all[, 18]

  ## Counter
  count <- count+1

  ## Match the country from the food data to the country in the world bank data
  found <- c()
    for (jj in 1:nrow(countryinfo)) {
        if (!is.na(countryinfo$sovereign[jj])) {
            if (countryinfo$sovereign[jj] == countryname)
                found <- c(found, jj) # duplicated, so list
        }
    }
  ##found <- countryinfo$sovereign == countryname
  if (length(found) > 0) {
    #countryinfo$food.ff[found] <- food.ff[count]
    #countryinfo$food.pc[found] <- food.pc[count]
    countryinfo$protein.ff[found] <- protein.ff[count]
    countryinfo$protein.pc[found] <- protein.pc[count]
    countryinfo$protein.an[found] <- protein.an[count]
      ##countryinfo$food.weight[found] <- food.weight[count]
  } else
    print(paste(countryname, sum(found)))
}

countryinfo$worker.ratio <- countryinfo$workers / countryinfo$population
countryinfo$gdp.ratio <- countryinfo$avgecon / countryinfo$gdp
countryinfo$area.ratio <- countryinfo$eez.km2. / (countryinfo$eez.km2. + countryinfo$surface.area)
#countryinfo$food.ratio <- countryinfo$food.ff / countryinfo$food.pc
#countryinfo$protein.ratio <- countryinfo$protein.ff / countryinfo$protein.pc
#countryinfo$protein.an.ratio <- countryinfo$protein.ff / countryinfo$protein.an
write.csv(countryinfo, paste0(datapath, "weights/combined-country", filesuffix, ".csv"), row.names=F)

} else {
    regioninfo <- read.csv(paste0(datapath, "weights/combined-regions-new", filesuffix, ".csv"))
    countryinfo <- read.csv(paste0(datapath, "weights/combined-country", filesuffix, ".csv"))
}

if (do.plots) {
    plot(countryinfo$worker.ratio, countryinfo$gdp.ratio, log="xy", main="Comparison between Employment and Economics", xlab="Population Portion", ylab="GDP Portion")

    library(ggplot2)

    ggplot(countryinfo) +
        geom_histogram(aes(x=countryinfo$worker.ratio, y=..density..), binwidth=.25) +
            geom_density(aes(x=countryinfo$worker.ratio), colour=2) +
                scale_x_log10() + ggtitle("Portion of fishery workers by country") +
                    xlab('')

    ggplot(countryinfo) +
        geom_histogram(aes(x=countryinfo$gdp.ratio, y=..density..), binwidth=.25) +
            geom_density(aes(x=countryinfo$gdp.ratio), colour=2) +
                scale_x_log10() + ggtitle("Portion of fishery GDP by country") +
                    xlab('')

    ggplot(countryinfo) +
        geom_histogram(aes(x = (countryinfo$worker.ratio) / (countryinfo$gdp.ratio), y=..density..), binwidth=.25) +
            geom_density(aes(x = (countryinfo$worker.ratio) / (countryinfo$gdp.ratio)), col=2) +
                scale_x_log10() + ggtitle("Ratio of Worker and GDP ratios") +
                    xlab("Worker ratio divided by GDP ratio")

    ggplot(countryinfo) +
      geom_histogram(aes(x=countryinfo$food.ratio, y=..density..), binwidth=.25) +
      geom_density(aes(x=countryinfo$food.ratio), colour=2) +
      scale_x_log10() + ggtitle("Portion of food supply by country") +
      xlab('')

    ggplot(countryinfo) +
      geom_histogram(aes(x=countryinfo$protein.ratio, y=..density..), binwidth=.25) +
      geom_density(aes(x=countryinfo$protein.ratio), colour=2) +
      scale_x_log10() + ggtitle("Portion of protein supply by country") +
      xlab('')

    ggplot(countryinfo) +
      geom_histogram(aes(x=countryinfo$protein.an.ratio, y=..density..), binwidth=.25) +
      geom_density(aes(x=countryinfo$protein.an.ratio), colour=2) +
      scale_x_log10() + ggtitle("Portion of animal protein by country") +
      xlab('')
}
