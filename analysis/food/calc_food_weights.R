#Calculate food weights from master-food.csv

## do.windows <- .Platform$OS.type != "unix"
## if (do.windows){
##   setwd("C:/Users/Nandini/Dropbox/High_Seas/codeR/food")
##   datapath <- "C:/Users/Nandini/Dropbox/High_Seas/results/"
##   outputpath <- "C:/Users/Nandini/OneDrive/Columbia/Research/Conflict hotspots/highseas/dataCSV/weights/"
## } else {
##   setwd("~/Dropbox/High_Seas/codeR/food")
##   datapath <- "~/research/highseas/"
##   outputpath <- "~/research/highseas/dataCSV/weights/"
## }

## masterfood <- read.csv(paste0(datapath,"master-food.csv"))

calc.food.metric <- function(food.table, production, protein.req, protein.tot, protein.fish, risks) {
    ## food.table = c(import.weight, export.weight, import.value, export.value, reimport.weight, reexport.weight, reimport.value, reexport.value)
    import <- food.table[1] #as.numeric(masterfood$import.weight)
    export <- food.table[2] #as.numeric(masterfood$export.weight)
    reimport <- food.table[5] #as.numeric(masterfood$reimport.weight)
    reexport <- food.table[6] #as.numeric(masterfood$reexport.weight)

    Aminus <- production-export+reimport+import-reexport #local catch for local consumption
    Aplus <- ((production-export+reimport)*risks) #Aminus at risk

    Aplus*protein.fish*protein.req/(Aminus*(protein.tot^2))
}

## write.csv(masterfood,paste0(outputpath,"master-food-weights.csv"),row.names=F)

