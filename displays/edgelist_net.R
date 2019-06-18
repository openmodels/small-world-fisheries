## All paths are relative after this point.  Change here as needed.

resultpath <- "../../results/"

edgeweights <- as.data.frame(read.csv(paste0(resultpath, "edgeweights-cl.csv")))
ctr <-0
edge.net <- vector(mode = "numeric",length = length(as.character(edgeweights$source)))
source.net <- vector(mode = "character",length = length(as.character(edgeweights$source)))
sink.net <- vector(mode = "character",length = length(as.character(edgeweights$source)))

for (i in 1:length(as.character(edgeweights$source))) {
  source1 <- as.character(edgeweights$source[i])
  sink1 <- as.character(edgeweights$sink[i])
  flag <- 0
  for (j in 1:length(as.character(edgeweights$source))) {
    source2 <- as.character(edgeweights$source[j])
    sink2 <- as.character(edgeweights$sink[j])
    if (identical(source1,sink2)&&identical(sink1,source2)) {
      flag <- 1
      ctr <- ctr+1
      weighti <- as.numeric(edgeweights$catchavg[i])
      weighti[is.na(weighti)] <- 0
      weightj <- as.numeric(edgeweights$catchavg[j])
      weightj[is.na(weightj)] <- 0
      if (weighti>weightj){
        edge.net[ctr] <- weighti-weightj
        source.net[ctr] <- source1
        sink.net[ctr] <- sink1
      } else {
        edge.net[ctr] <- weightj-weighti
        source.net[ctr] <- source2
        sink.net[ctr] <- sink2
      }
    }
  }
  if (identical(flag,0)){
    edge.net[ctr] <- as.numeric(edgeweights$catchavg[i])
    source.net[ctr] <- source1
    sink.net[ctr] <- sink1
    ctr <- ctr+1
  }
}

edge.net[edge.net==0] <- NA
edgeweights.net.temp <- data.frame(source=source.net, target=sink.net, weight=edge.net)
edgeweights.net <- na.omit(edgeweights.net.temp)

write.csv(edgeweights.net, file=paste0(resultpath, "edgelist_net_cl_gephi.csv"), row.names=FALSE)
