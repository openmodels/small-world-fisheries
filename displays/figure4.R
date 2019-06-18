datapath <- "../../data/"
resultpath <- "../../results/"

#socioeconomic variables
totalrisk.country <- read.csv(paste0(datapath, "atrisk/portion-split-cl.csv"))
totalrisk.country <- subset(totalrisk.country, resilience == "total")

#can manually change to move points around
centroids <- read.csv(paste0(datapath, "shapefiles/centroids.csv"))

#top N countries
topN <- 20
library(maps)
library(png)
library(RColorBrewer)
library(classInt)
##gpclibPermit()
library(maptools)
library(mapproj)

source("../analysis/lib/names.R")

## Load all icons
#each icon 128x128 pixels and the last dimension is RGBalpha alpha is level of transparency
icon.catch <- readPNG("icons/catch.png")
icon.food <- readPNG("icons/food.png")
icon.gdp <- readPNG("icons/dollar.png")
icon.jobs <- readPNG("icons/jobs.png")

# catch marine blue
icon.catch[,,2] <- 126 / 255
icon.catch[,,3] <- 165 / 255

#food orange
icon.food[,,1] <- 255/255
icon.food[,,2] <- 102/255
icon.food[,,3] <- 0/255

#GDP green
icon.gdp[,,1] <- 102/255
icon.gdp[,,2] <- 153/255
icon.gdp[,,3] <- 0/255

#labor red
# icon.jobs[,,1] <- 102/255
# icon.jobs[,,2] <- 0/255
# icon.jobs[,,3] <- 51/255

#labor navy
icon.jobs[,,1] <- 0/255
icon.jobs[,,2] <- 0/255
icon.jobs[,,3] <- 102/255

#GDP all red
#icon.gdp[,,1] <- 1
#food all green
#icon.food[,,2] <- 1

#catch grey
colors<-c("grey90", "grey82", "grey74", "grey66", "grey58", "grey50")
brks<-classIntervals(totalrisk.country$catch, n=6, style="quantile")
brks<- brks$brks
valueCol=character(nrow(totalrisk.country))
for (ii in 1:nrow(totalrisk.country)) {valueCol[ii]=colors[findInterval(totalrisk.country$catch[ii], brks, all.inside=T)]}

## Identify top topN countries with each icon
#defines which countries get an icon (top N by variable)
totalrisk.country$icon.catch <- (totalrisk.country$byecon > .3)
totalrisk.country$icon.gdp <- (totalrisk.country$bygdp > 0.008)
totalrisk.country$icon.jobs <- (totalrisk.country$byjobs > 0.015)
totalrisk.country$icon.food <- (totalrisk.country$byfood > 0.018)


# totalrisk.country$icon.catch <- totalrisk.country$catch > quantile(totalrisk.country$catch, 1 - topN / nrow(totalrisk.country), na.rm=T)
# totalrisk.country$icon.gdp <- totalrisk.country$bygdp > quantile(totalrisk.country$bygdp, 1 - topN / nrow(totalrisk.country), na.rm=T)
# totalrisk.country$icon.jobs <- totalrisk.country$byjobs > quantile(totalrisk.country$byjobs, 1 - topN / nrow(totalrisk.country), na.rm=T)
# totalrisk.country$icon.food <- totalrisk.country$byfood > quantile(totalrisk.country$byfood, 1 - topN / nrow(totalrisk.country), na.rm=T)

## Don’t have weight, don’t have icon
totalrisk.country$icon.catch[is.na(totalrisk.country$icon.catch)] <- F
totalrisk.country$icon.gdp[is.na(totalrisk.country$icon.gdp)] <- F
totalrisk.country$icon.jobs[is.na(totalrisk.country$icon.jobs)] <- F
totalrisk.country$icon.food[is.na(totalrisk.country$icon.food)] <- F

name2map <- function(name) {
    if (name == "United States")
        return(c("USA", "Puerto Rico", "Virgin Islands, US"))
    if (name == "United Kingdom")
        return(c("UK", "Anguilla", "Virgin Islands, British", "Cayman Islands", "Montserrat", "Turks and Caicos Islands"))
    if (name == "France")
        return(c("France", "Saint Barthelemy", "Saint Martin", "Guadeloupe", "Martinique"))
    if (name == "Netherlands")
        return(c("Netherlands", "Sint Maarten", "Curacao", "Aruba"))
    if (name == "Congo, Dem. Rep.")
        return("Democratic Republic of the Congo")
    if (name == "Congo, Rep.")
        return("Republic of Congo")
    if (name == "Russian Federation")
        return("Russia")
    if (name == "Korea, Dem. Rep.")
        return("North Korea")
    if (name == "Korea, Rep.")
        return("South Korea")
    if (name == "Antigua and Barbuda")
        return(c("Antigua", "Barbuda"))
    if (name == "Bahamas, The")
        return("Bahamas")
    if (name == "Cabo Verde")
        return("Cape Verde")
    if (name == "Hong Kong SAR, China")
        return("China:Hong Kong")
    if (name == "China")
        return(c("China", "Taiwan"))
    if (name == "Denmark")
        return(c("Denmark", "Greenland"))
    if (name == "St. Kitts and Nevis")
        return(c("Saint Kitts", "Saint Nevis"))
    if (name == "St. Vincent and the Grenadines")
        return(c("Saint Vincent", "Grenadines"))
    if (name == "Egypt, Arab Rep.")
        return("Egypt")
    if (name == "Iran, Islamic Rep.")
        return("Iran")
    if (name == "Yemen, Rep.")
        return("Yemen")
    if (name == "Trinidad and Tobago")
        return(c("Trinidad", "Tobago"))
    if (name == "Micronesia, Fed. Sts.")
        return("Micronesia")
    if (name == "Gambia, The")
        return("Gambia")
    if (name == "Cote d'Ivoire")
        return("Ivory Coast")
    if (name == "Brunei Darussalam")
        return("Brunei")
    if (name == "Gaza Strip")
        return("Palestine:Gaza Strip")
    if (name == "Syrian Arab Republic")
        return("Syria")
    if (name == "Venezuela, RB")
        return("Venezuela")
    name <- gsub("St.", "Saint", name)

    return(name)
}

#ask james how to match our country names in our value spreadsheet to the country names in shapefiles for R
#label countries that have an icon in the call outs
#box the icons in the call outs
#if just 1 icon, put it on the centroid.
plotmap <- function(totalrisk.country, fileout, xlim=c(-180, 180), ylim=c(-90, 90), scalemult=1, yscale=2, bcolor="#00000080") {

    scale <- scalemult * (ylim[2] - ylim[1]) * 10 / 180 # based on y, so row of maps have same scale

    pdf(fileout, width=400 * (xlim[2] - xlim[1]) / (ylim[2] - ylim[1]), height=400)
    par(mar=rep(0, 4))

    #names.R
    map("world", xlim=xlim, ylim=ylim, mar=rep(0,4), fill=T, col="antiquewhite1", boundary=T, border="white", lwd=20)
    for (ii in 1:nrow(totalrisk.country)) {
        tryCatch({
            map("world", name2map(totalrisk.country$sovereign[ii]), xlim=xlim, ylim=ylim, mar=rep(0,4), fill=T, col=valueCol[ii], boundary=T, border="white", add=T, lwd=20)
        }, error=function(err) {
            print(paste("Cannot map", totalrisk.country$sovereign[ii]))
        })
    }
   # map.text("world", region=c("UK:Great Britain", "The Bahamas", "Belgium$", "Norway$", "Spain$", "Cape Verde", "China", "Comoros", "Denmark$", "Ecuador", "Fiji", "Finland", "Gambia", "Guinea Bissau", "Guyana", "Indonesia", "Japan", "North Korea", "Latvia", "Lithuania", "Maldives", "Malta", "Marshall Islands", "Mauritania", "Mauritius", "Netherlands", "Pakistan", "Palau", "Papua New Guinea", "Russia", "Saint Vincent", "Solomon Islands", "Suriname", "Turkey", "Vietnam", "Mexico", "Morocco", "Thailand"), labels=c("United Kingdom", "Bahamas", "Belgium", "Norway", "Spain", "Cape Verde", "China", "Comoros", "Denmark", "Ecuador", "Fiji", "Finland", "Gambia", "Guinea Bissau", "Guyana", "Indonesia", "Japan", "North Korea", "Latvia", "Lithuania", "Maldives", "Malta", "Marshall Islands", "Mauritania", "Mauritius", "Netherlands", "Pakistan", "Palau", "Papua New Guinea", "Russia", "Saint Vincent and the Grenadines", "Solomon Islands", "Suriname", "Turkey", "Vietnam", "Mexico", "Morocco", "Thailand"))
    #Barbados, Dominica, Grenada, Micronesia, Fed. Sts., Sao Tome and Principe, Tuvalu, Kiribati
    legend(x=-150, y=-50, legend=leglabs(round(brks)), fill=colors, bty="n",x.intersp = .5, y.intersp = .5)

    ## Plot each icon
    for (ii in 1:nrow(totalrisk.country)) {
        if (totalrisk.country$icon.catch[ii] || totalrisk.country$icon.gdp[ii] || totalrisk.country$icon.jobs[ii] || totalrisk.country$icon.food[ii]) {
            country <- as.character(totalrisk.country$sovereign[ii])
            if (country == "Cabo Verde") {
                xcoord <- -23.616667
                ycoord <- 15.111111
            } else if (country == "Maldives") {
                xcoord <- 73.22
                ycoord <- 3.2
            } else if (country == "Barbados") {
                xcoord <- -59.55
                ycoord <- 13.17
            } else if (country == "Mauritius") {
                xcoord <- 57.59
                ycoord <- -20.26
            } else if (country == "Malta") {
                xcoord <- 14.4315
                ycoord <- 35.902
            } else if (country == "Micronesia, Fed. Sts.") {
                xcoord <- 158.25
                ycoord <- 6.916667
            } else if (country == "Palau") {
                xcoord <- 134.5
                ycoord <- 7.5
            } else if (country == "French Polynesia") {
                xcoord <- 149.5
                ycoord <- -17.5
            } else if (country == "Tuvalu") {
                xcoord <- 179.2
                ycoord <- -7.478548
            } else if (country == "Bahrain") {
                xcoord <- 50.555
                ycoord <- 26.077
            } else if (country == "Antigua and Barbuda") {
                xcoord <- -61.793
                ycoord <- 17.089
            } else if (country == "Dominica") {
                xcoord <- -61.352
                ycoord <- 15.447
             } else if (country == "St. Kitts and Nevis") {
                xcoord <- -62.77
                ycoord <- 17.35
            } else if (country == "St. Lucia") {
                xcoord <- -60.98
                ycoord <- 13.895
            } else if (country == "St. Vincent and the Grenadines") {
                xcoord <- -61.18
                ycoord <- 13.24
            } else if (country == "Grenada") {
                xcoord <- -61.69
                ycoord <- 12.17
            } else if (country == "Sao Tome and Principe") {
                xcoord <- 6.62
                ycoord <- 0.28
            } else if (country == "Comoros") {
                xcoord <- 43.35
                ycoord <- -11.67
            } else if (country == "Kiribati") {
                xcoord <- 173.2
                ycoord <- 1.3
             } else if (country == "Micronesia, Fed. Sts.") {
                xcoord <- 158.23
                ycoord <- 6.886
            } else if (country == "Marshall Islands") {
                xcoord <- 166.79
                ycoord <- 11.34
            } else if (country == "American Samoa") {
                xcoord <- -171.2281608
                ycoord <- -14.0611192
            } else if (country == "Samoa") {
                xcoord <- -172.6647966
                ycoord <- -13.7498526
            } else if (country == "Tonga") {
                xcoord <- -176.855562
                ycoord <- -18.6024366
            } else if (country == "Nauru") {
                xcoord <- 166.9000475
                ycoord <- -0.527894
            } else {
                if (country == "Bahamas, The")
                    country <- "The Bahamas"
                if (country == "Gambia, The")
                    country <- "Gambia"
                if (country == "Russian Federation")
                    country <- "Russia"
                if (country == "Guinea-Bissau")
                    country <- "Guinea Bissau"
                if (country == "Korea, Dem. Rep.")
                    country <- "North Korea"
                if (country == "Korea, Rep.")
                    country <- "South Korea"
                if (country == "CÃ´te d'Ivoire" || country == "Côte d'Ivoire")
                    country <- "Ivory Coast"
                if (country == "Congo, Rep.")
                    country <- "Republic of Congo"
                if (country == "Timor-Leste")
                    country <- "East Timor"
                if (country == "Yemen, Rep.")
                    country <- "Yemen"
                jj <- which(centroids$sovereignt == country)
                if (length(jj) != 1) {
                    if (totalrisk.country$sovereign[ii] == "Denmark")
                        jj <- 44
                    else if (totalrisk.country$sovereign[ii] == "France")
                        jj <- 56
                    else if (totalrisk.country$sovereign[ii] == "United Kingdom")
                        jj <- 58
                    else if (totalrisk.country$sovereign[ii] == "United States")
                        jj <- 169
                    else {
                        print(c(as.character(totalrisk.country$sovereign[ii]), jj))
                        next
                    }
                }

                xcoord <- centroids$XCOORD[jj]
                ycoord <- centroids$YCOORD[jj]
            }
		#if the centroid is inside the inset
            if (xcoord < xlim[1] || xcoord > xlim[2] || ycoord < ylim[1] || ycoord > ylim[2])
                next

		if(totalrisk.country$icon.catch[ii]+totalrisk.country$icon.gdp[ii]+totalrisk.country$icon.jobs[ii]		+totalrisk.country$icon.food[ii]==1)	{
			if (totalrisk.country$icon.catch[ii]) {
#                rect(xcoord- (scale/2), ycoord-((yscale*scale/2)/2), xcoord+(scale/2), ycoord+((yscale*scale/2)/2), border=bcolor, col=NA)
                rasterImage(icon.catch, xcoord- (scale/2), ycoord- ((yscale*scale/2)/2), xcoord + (scale/2), ycoord + ((yscale*scale/2)/2))
            	}
            	if (totalrisk.country$icon.gdp[ii]) {
#                rect(xcoord- (scale/2), ycoord-((yscale*scale/2)/2), xcoord+(scale/2), ycoord+((yscale*scale/2)/2), border=bcolor, col=NA)
                rasterImage(icon.gdp, xcoord- (scale/2), ycoord- ((yscale*scale/2)/2), xcoord + (scale/2), ycoord + ((yscale*scale/2)/2))
            	}
            	if (totalrisk.country$icon.jobs[ii]) {
#                rect(xcoord- (scale/2), ycoord-((yscale*scale/2)/2), xcoord+(scale/2), ycoord+((yscale*scale/2)/2), border=bcolor, col=NA)
                rasterImage(icon.jobs, xcoord- (scale/2), ycoord- ((yscale*scale/2)/2), xcoord + (scale/2), ycoord + ((yscale*scale/2)/2))
            	}
            	if (totalrisk.country$icon.food[ii]) {
#                rect(xcoord- (scale/2), ycoord-((yscale*scale/2)/2), xcoord+(scale/2), ycoord+((yscale*scale/2)/2), border=bcolor, col=NA)
                rasterImage(icon.food, xcoord- (scale/2), ycoord- ((yscale*scale/2)/2), xcoord + (scale/2), ycoord + ((yscale*scale/2)/2))
			}
		}
		else	{
			 rect(xcoord-scale, ycoord-(yscale*scale/2), xcoord+scale, ycoord+(yscale*scale/2), border=bcolor, lwd=50, col=NA)

			 if (totalrisk.country$icon.catch[ii]) {
		#plotting grey rectangle around the icon and white background slightly transparent
		#rect takes two diagonal coord left to right
		#rerun with ycoord +/- (scale/2) for North Sea

#                rect(xcoord, ycoord, xcoord + scale, ycoord + (yscale*scale/2), border=NA, col=NA)
                rasterImage(icon.catch, xcoord, ycoord, xcoord + scale, ycoord + (yscale*scale/2))
            	}
            	if (totalrisk.country$icon.gdp[ii]) {
#                rect(xcoord, ycoord - (scale/2), xcoord + scale, ycoord, border=NA, col=NA)
                rasterImage(icon.gdp, xcoord, ycoord - (yscale*scale/2), xcoord + scale, ycoord)
            	}
            	if (totalrisk.country$icon.jobs[ii]) {
#                rect(xcoord - scale, ycoord - (scale/2), xcoord, ycoord, border=NA, col=NA)
                rasterImage(icon.jobs, xcoord - scale, ycoord - (yscale*scale/2), xcoord, ycoord)
            	}
            	if (totalrisk.country$icon.food[ii]) {
#                rect(xcoord - scale, ycoord, xcoord, ycoord + (yscale*scale/2), border=NA, col=NA)
                rasterImage(icon.food, xcoord - scale, ycoord, xcoord, ycoord + (yscale*scale/2))
                }
		}
        }
    }

    dev.off()
}

plotmap(totalrisk.country, paste0(resultpath, "bigmap-globe.pdf"), xlim=c(-165, 195), ylim=c(-60, 90), bcolor="#00000080")

##Europe
#plotmap(totalrisk.country, paste0(resultpath, "bigmap-europe-test.png"), xlim=c(-15, 30), ylim=c(35, 75), scalemult=2, yscale=1, bcolor="#00000080")

##Caribbean
plotmap(totalrisk.country, paste0(resultpath, "bigmap-caribbean.pdf"), xlim=c(-65, -53), ylim=c(0, 15), scalemult=2, yscale=2, bcolor="#00000080")

##Southeast Asia and Pacific Islands
plotmap(totalrisk.country, paste0(resultpath, "bigmap-southpacific.pdf"), xlim=c(110, 185), ylim=c(-15, 20), scalemult=2, yscale=2, bcolor="#00000080")
## map.axes()

## North Sea
#x is lat limits and y is lon limits
#play with scalemult to see if that fixes the problem

plotmap(totalrisk.country, paste0(resultpath, "bigmap-northsea.pdf"), xlim=c(-7, 31), ylim=c(49, 71), scalemult=4, yscale=1, bcolor="#00000080")
## West Africa
#plotmap(totalrisk.country, paste0(resultpath, "bigmap-westafrica.png"), xlim=c(-20, -5), ylim=c(0, 20), scalemult=2, yscale=2)

## Indonesia
#plotmap(totalrisk.country, paste0(resultpath, "bigmap-indonesia.png"), xlim=c(95, 130), ylim=c(-15, 20), scalemult=2, yscale=2)


