#!/usr/bin/env R
# author: chenxm
source("commons.R")

#---------------------------------------
MAXPROC=24
MAXCORE=(detectCores()-1)
SMPMAX=20000
SAMPLE=200
MODEL_DIST_NUM=150 # ~2^10 observations
#---------------------------------------

## Load data
en.mob <- readRDS("rdata/engage.filtered.mob.rds")
en.mob$TOD <- as.numeric(en.mob$TOD)
en.mob$service.en <- as.numeric(en.mob$service.en)
en.mob$BRank <- as.numeric(en.mob$BRank)

## Generate a experimental sample
en.mob.s <- if ( SAMPLE == SMPMAX ) {
  en.mob
} else {
  sample.trajs(en.mob, SAMPLE)
}
saveRDS(en.mob.s, paste("rdata/engage.hmm.traj.s", SAMPLE ,".rds", sep=""))

## All users modeling
en.mob.s2 <- split(en.mob.s, en.mob.s$UID)
models <- mclapply(en.mob.s2, function(user) hmm.learn(user), mc.cores=MAXPROC)
na.selector <- sapply(models, is.na)
models[na.selector] <- NULL # remove NA
attr(models, "split_labels") <- attr(models, "split_labels")[!na.selector,]
saveRDS(models, paste("rdata/engage.hmm.models.s", SAMPLE ,".rds", sep=""))

## Calculate distance matrix parallelly
cl = makeCluster(MAXPROC, outfile="hmm_dist.log", type="FORK")
registerDoParallel(cl, cores=MAXCORE)
dm <- as.dist(hmm.dm(models, en.mob, MODEL_DIST_NUM))
saveRDS(dm, paste("rdata/engage.hmm.dmat.s", SAMPLE ,".rds", sep=""))

stopCluster(cl)
# EOF