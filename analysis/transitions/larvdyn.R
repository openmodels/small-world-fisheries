## do.windows <- .Platform$OS.type != "unix"
## if (do.windows){
##   setwd("C:/Users/Nandini/Dropbox/High_Seas/codeR/transitions")
##   datapath <- "C:/Users/Nandini/OneDrive/Columbia/Research/Conflict hotspots/highseas/"
## } else {
##   setwd("~/Dropbox/High_Seas/codeR/transitions")
##   datapath <- "~/research/highseas/"
## }

larvdyn1 <- read.csv(paste0(datapath, "dataCSV/larvdyn/summary2.csv"))
larvdyn2 <- read.csv(paste0(datapath, "dataCSV/larvdyncustom.csv"))

larvdyn <- data.frame(scientific=c(as.character(larvdyn1$Species), paste(larvdyn2$genus, larvdyn2$species)),
                      LarvaeDuration=c(larvdyn1$LarvaeDuration, larvdyn2$duration),
                      EggDuration=c(larvdyn1$EggDuration, rep(0, nrow(larvdyn2))),
                      EggFloating=c(as.character(larvdyn1$EggFloating), rep("", nrow(larvdyn2))))

## write.csv(larvdyn, paste0(datapath, "generated/dataCSV/larvdyn.csv"), row.names=F)

taxonomy1 <- read.csv(paste0(datapath, "dataCSV/taxonomy-fixed.csv"))

taxonomy <- rbind(taxonomy1, data.frame(scientific=paste(larvdyn2$genus, larvdyn2$species),
                                        alphaid=NA, kingdom="Animalia", phylum=larvdyn2$phylum,
                                        class=larvdyn2$class, order=NA, family=NA, genus=larvdyn2$genus))

## Returns list(durations=c(...), level=...)
get.species.durations <- function(scientific) {
    if (length(grep(" ", scientific)) == 0)
        scientific <- paste(scientific, "spp.")

    taxrow <- taxonomy[taxonomy$scientific == scientific,]
    if (nrow(taxrow) >= 1)
        taxrow <- taxrow[1, -c(1:2)] # Drop title
    else {
        genus <- strsplit(scientific, " ")[[1]][1]
        taxrow <- taxonomy[!is.na(taxonomy$genus) & taxonomy$genus == genus,]
        if (nrow(taxrow) >= 1)
            taxrow <- taxrow[1, -c(1:2)] # Drop title
        else
            taxrow <- data.frame(kingdom="Animalia", genus)
    }
    columns <- c('scientific', rev(names(taxrow)))
    entries <- c(scientific, rev(as.character(t(taxrow))))

    for (cc in 1:length(columns)) {
        specwithin <- taxonomy$scientific[taxonomy[, columns[cc]] == entries[cc]]
        ldrows <- which(larvdyn$scientific %in% specwithin)

        durations <- larvdyn$LarvaeDuration[ldrows] + ifelse(larvdyn$EggFloating[ldrows] == "buoyant (pelagic)", ifelse(is.na(larvdyn$EggDuration[ldrows]), 4, larvdyn$EggDuration[ldrows]), 0)
        durations <- durations[!is.na(durations)]
        if (length(durations) > 0)
            return(list(durations=durations, level=columns[cc]))
    }

    stopifnot(F) # Should never get here
}

## get.species.durations("Ablennes hians")
## get.species.durations("Abudefduf abdominalis")

depths <- read.csv(paste0(datapath, "dataTEXT/depths.csv"))
depths <- depths[!duplicated(depths$scientific),]

get.species.srcdst <- function(scientific) {
    p90depth <- depths$p90depth[depths$scientific == scientific]
    if (length(p90depth) > 0 && p90depth > 200)
        return("eez-eez")
    return("shelf-shelf")
}

## get.species.srcdst("Ablennes hians")
## get.species.srcdst("Abudefduf abdominalis")

## Diagnostics:

## results <- data.frame(scientific=c(), level=c(), count=c(), mean=c())
## allres <- data.frame(scientific=c(), level=c(), duration=c())
## for (ii in 1:nrow(taxonomy)) {
##     result <- get.species.durations(as.character(taxonomy$scientific[ii]))
##     results <- rbind(results, data.frame(scientific=taxonomy$scientific[ii], level=result$level,
##                                          count=length(result$durations), mean=mean(result$durations)))
##     allres <- rbind(allres, data.frame(scientific=taxonomy$scientific[ii], level=result$level,
##                                        duration=result$durations))
## }

## library(ggplot2)
## results$level <- factor(results$level, c(names(taxonomy)[3:8], "scientific"))
## ggplot(results, aes(level)) +
##     geom_bar() + theme_bw() + xlab("Taxonomic level for durations") + ylab("Count")

## allknown <- get.species.durations("A B")$durations

## ggplot(data.frame(duration=c(allknown, results$mean), assump=c(rep("Known", length(allknown)), rep("Inferred", nrow(results)))), aes(duration, colour=assump)) +
##     geom_density(bw=5) + theme_bw() + xlab("Larval Floating Duration") +
##     scale_colour_discrete(name=NULL)

## ggplot(data.frame(duration=c(allknown, allres$duration), assump=c(rep("Known", length(allknown)), rep("Inferred", nrow(allres)))), aes(duration, colour=assump)) +
##     geom_density(bw=5) + theme_bw() + xlab("Larval Floating Duration") +
##     scale_colour_discrete(name=NULL)

## library(dplyr)
## bymonth <- data.frame(duration=30 * pmin(6, round(allres$duration / 30))) %>% group_by(duration) %>% summarize(portion=length(duration) / nrow(allres))
## ggplot(data.frame(duration=allknown), aes(duration)) +
##     geom_density(bw=5) + theme_bw() + xlab("Floating Egg + Larval Floating Duration") +
##     geom_bar(data=bymonth, aes(y=portion / 30), stat="identity", fill="#00000040", color="black") +
## ylab("Density (over days)")
## ggsave("../../paper/hsfigs/floatinghist.pdf", width=5, height=3)

## ## Count number of available species
## larvdyn$knowndur <- larvdyn$LarvaeDuration + ifelse(larvdyn$EggFloating == "buoyant (pelagic)", ifelse(is.na(larvdyn$EggDuration), 4, larvdyn$EggDuration), 0)
## sum(!is.na(larvdyn$knowndur[c(grep("sp\\.", larvdyn$scientific), grep("NA", larvdyn$scientific))]))
## sum(!is.na(larvdyn$knowndur))

## Benefits possible

## library(jsonlite)
## allsau <- sapply(read_json("../../codepy/allsau.json")$data, function(entry) ifelse(length(grep(" ", entry$scientific_name)) > 0, entry$scientific_name, paste(entry$scientific_name, "spp.")))
## spawning <- read.csv(paste0(datapath, "dataCSV/spawning/combined.csv"))
## touse <- unique(c(as.character(spawning$species), sapply(spawning$species, function(species) paste(strsplit(as.character(species), " ")[[1]][1], "spp."))))
## touse <- touse[(touse %in% taxonomy$scientific) & (touse %in% allsau)]

## benefits <- list() # name => #cumulative levels removed
## for (species in touse) {
##     ii <- which(taxonomy$scientific == species)[1]
##     result <- get.species.durations(as.character(taxonomy$scientific[ii]))
##     if (result$level %in% c("scientific", "genus"))
##         next
##     levelcc <- which(names(taxonomy) == result$level)
##     for (cc in (levelcc + 1):ncol(taxonomy)) {
##         if (is.na(taxonomy[ii, cc]) || nchar(as.character(taxonomy[ii, cc])) < 3)
##             next
##         name <- paste(names(taxonomy)[cc], as.character(taxonomy[ii, cc]))
##         benefits[[name]] <- c(benefits[[name]], rep(taxonomy$scientific[ii], cc - levelcc))
##     }
## }

## bendf <- data.frame(name=c(), benefit=c())
## for (name in names(benefits))
##     bendf <- rbind(bendf, data.frame(name, benefit=length(benefits[[name]])))
## write.csv(bendf[order(bendf$benefit, decreasing=T),], "searchlist2.csv", row.names=F)
