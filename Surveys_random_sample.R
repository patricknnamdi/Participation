# setting the path
setwd("/Users/ricardocasney/Documents/Work/Chagas/Participation/Randomization")

### Reading the data

# All houses in ASA
mz <- read.csv("ASAEEDGPS_con_MZ.csv")

# Localities that will be sprayed in the second round
brig <- read.csv("CRONOGRAMA DE INTERVENCION II CICLO DE ROCIADO worked.csv")
brig <- brig[, 1:5]
names(brig) <- c("L", "loc_name", "comp_foc", "locsize", "brigade")

#subsetting the localities we'll work on
brig.comp <- brig[brig$comp_foc == "C",]
loc.comp <- brig$L[brig$comp_foc == "C"]
mz2 <- mz[mz$L %in% loc.comp,]
locsize <- brig.comp[brig.comp$comp_foc == "C", c("L", "locsize")]

# generating a unique code for blocks
mz2 <- mz2[, c("L", "V", "MZ")]
mz2$uniMZ <- as.factor(mz2$L*100 + mz2$MZ)

clusters <- read.csv("final_cluster_data2_corrected_for_heraud.csv")

dat <- merge(mz2, clusters, by.x = "uniMZ", by.y = "uni_block", all.x = T)
dat2 <- dat[, c(1, 2, 3, 4, 7, 10, 11)]

dat3 <- na.omit(dat2)

clustersize <- aggregate(V ~ clusterID, data = dat2, FUN = length)
names(clustersize) <- c("clusterID", "clustersize")
clustersize$clustersample <- round(0.3*clustersize$clustersize)
#disable this line and rerun again if don't want extra houses in San Luis
clustersize$clustersample[clustersize$clusterID %in% c(34, 35)] <- 
    round(0.3 * clustersize$clustersize[clustersize$clusterID %in% c(34, 35)]) + 10

dat4 <- merge(dat3, clustersize, by = "clusterID", all = T)

#library(plyr)

sampby <- function(df){
    ddply(df, .(clusterID), function(df) df[sample(nrow(df), size= df$clustersample),])
}

set.seed(1977)
houses_survey <- ddply(dat4, .(clusterID), sampby)

write.csv(houses_survey, file = "Houses_survey.csv")
