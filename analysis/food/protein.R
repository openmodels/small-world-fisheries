protein.portions <- read.csv(paste0(datapath, "dataCSV/protein-portions.csv"))
taxonomy1 <- read.csv(paste0(datapath, "dataCSV/taxonomy-fixed.csv"))

genuses <- sapply(protein.portions$Scientific.name, function(species) { strsplit(as.character(species), ' ')[[1]][1] })
genuses[!(genuses %in% taxonomy1$genus)]

get.protein <- function(scientific) {
    if (length(grep(" ", scientific)) == 0)
        scientific <- paste(scientific, "spp.")

    taxrow <- taxonomy1[taxonomy1$scientific == scientific,]
    if (nrow(taxrow) >= 1)
        taxrow <- taxrow[1, -c(1:2)] # Drop title
    else {
        genus <- strsplit(scientific, " ")[[1]][1]
        taxrow <- taxonomy1[!is.na(taxonomy1$genus) & taxonomy1$genus == genus,]
        if (nrow(taxrow) >= 1)
            taxrow <- taxrow[1, -c(1:2)] # Drop title
        else
            taxrow <- data.frame(kingdom="Animalia", genus)
    }
    columns <- c('scientific', rev(names(taxrow)))
    entries <- c(scientific, rev(as.character(t(taxrow))))

    for (cc in 1:length(columns)) {
        specwithin <- taxonomy1$scientific[taxonomy1[, columns[cc]] == entries[cc]]
        pprows <- which(protein.portions$Scientific.name %in% specwithin)
        if (length(pprows)) {
            protein <- mean(protein.portions$protein.avg[pprows]) / 100
            return(list(protein=protein, level=columns[cc]))
        }
    }

    stopifnot(F) # Should never get here
}

## get.protein("Merluccius capensis")
## get.protein("Merluccius othernis")

## Diagnostics:

## groups <- read.csv(paste0(datapath, "dataTEXT/saudata-new/species.csv"))

## results <- data.frame(scientific=c(), level=c(), protein=c())
## for (ii in which(!duplicated(groups$scientific))) {
##     result <- get.protein(as.character(groups$scientific[ii]))
##     results <- rbind(results, data.frame(scientific=groups$scientific[ii],
##                                          level=result$level, protein=result$protein))
## }

## library(ggplot2)
## ggplot(protein.portions, aes(protein.avg)) +
##     geom_histogram() + theme_bw() +
##     xlab("Percent protein by species")

## library(dplyr)
## results$level <- factor(results$level, rev(c(names(taxonomy1)[3:8], "scientific")))
## toshow <- results %>% group_by(level) %>% summarize(count=length(protein), min=min(protein), max=max(protein))
## library(xtable)
## xtable(toshow)

## protein.portions$genus = genuses
## portions2 <- protein.portions %>% inner_join(taxonomy1, by=c('genus'='genus'))
## portions2 <- portions2[!duplicated(portions2$Scientific.name),]

## sum(portions2$phylum == "Arthropoda")
