df.resilience <- read.csv(file.path(datapath, "dataCSV/resilience.csv"))
df.resilience$genus <- sapply(strsplit(as.character(df.resilience$scientific), " "), function(x) x[[1]][1])

get.species.resilience <- function(scientific) {
    scientific <- as.character(scientific)
    has.species <- which(df.resilience$scientific == scientific)
    if (length(has.species) == 1 && !is.na(df.resilience$threshold[has.species]))
        return(df.resilience$threshold[has.species])

    has.genus <- which(df.resilience$genus == strsplit(scientific, " ")[[1]][1])
    if (length(has.genus) > 0 && sum(!is.na(df.resilience$threshold[has.genus])) > 0)
        return((mean(df.resilience$threshold[has.genus], na.rm=T) + mean(df.resilience$threshold, na.rm=T)) / 2) # Average of genus and global average

    return(mean(df.resilience$threshold, na.rm=T))
}

get.species.resilience.level <- function(scientific) {
    threshold <- get.species.resilience(scientific)
    if (threshold >= .99)
        return("high")
    if (threshold >= .95)
        return("medium")
    if (threshold >= .85)
        return("low")
    if (threshold >= .7)
        return("very low")
    return("unknown")
}

## ## How much does resilience vary within genus?
## tbl <- as.matrix(sapply(unique(df.resilience$genus), function(genus) sd(df.resilience$threshold[df.resilience$genus == genus], na.rm=T)))
## tbl <- as.data.frame(tbl)
## tbl$genus <- row.names(tbl)
## quantile(tbl$V1, na.rm=T)
