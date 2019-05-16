##### Calculate total summed prevalence by EEZ

setwd("~/Dropbox/High_Seas/codeR")

library(PBSmapping) # GIS mapping library
library(maps)
source("tools.R")

## EEZ shapefile
eezs <- importShapefile("../dataShapes/World_EEZ_v8_20140228_LR/World_EEZ_v8_2014")

## Collect all the events for valid spawnings
for (filename in list.files("../dataTEXT/spawning")) {
    ## Check that we have both kinds of files
    if (!file.exists(paste("../dataTEXT/fishdata", filename, sep="/")))
        next

    print(filename)

    ## Read in the locations for this fish
    events <- get.table(paste("../dataTEXT/fishdata", filename, sep="/"))
    events$EID = 1:nrow(events)

    findevents <- data.frame(EID=events$EID[events$Overall.Probability > 0], X=events$Center.Long[events$Overall.Probability > 0], Y=events$Center.Lat[events$Overall.Probability > 0])

    ## Only include EEZ-included probabilities
    findevents <- findevents[!duplicated(findevents) & !is.na(findevents$X),]
    findevents <- as.EventData(findevents)

    found <- findPolys(findevents, eezs)

    ## Sum probability over all found
    result <- data.frame(PID=c(), total=c())
    for (pid in unique(found$PID)) {
        total <- sum(events$Overall.Probability[found$EID[found$PID == pid]])
        result <- rbind(result, data.frame(PID=pid, total))
    }

    write.csv(result, paste0("../dataCSV/bycountry/", filename), row.names=F)
}
