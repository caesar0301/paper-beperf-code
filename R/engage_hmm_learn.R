#!/usr/bin/env R
# author: chenxm
source("commons.R")

#---------------------------------------
SMPMAX=20000
SAMPLE=5
MAXCORE=(detectCores()-1)
MAXPROC=MAXCORE
MODEL_DIST_NUM=150 # ~2^10 observations
#---------------------------------------

## Load data
en.mob <- readRDS("rdata/engage.filtered.mob.rds")
en.mob$TOD <- as.numeric(en.mob$TOD)
en.mob$service.en <- as.numeric(en.mob$service.en)
en.mob$BRank <- as.numeric(en.mob$BRank)

## Generate a experimental sample
en.mob.s <- if(SAMPLE == SMPMAX) { en.mob
  } else { sample.trajs(en.mob, SAMPLE) }
saveRDS(en.mob.s, paste("rdata/engage.hmm.traj.s", SAMPLE ,".rds", sep=""))

## All users modeling parallelly
en.mob.s <- split(en.mob.s, en.mob.s$UID)
models <- mclapply(en.mob.s, function(user) hmm.learn(user), mc.cores=MAXPROC)
models <- models[! sapply(models, is.null)] # remove NULL models
saveRDS(models, paste("rdata/engage.hmm.models.s", SAMPLE ,".rds", sep=""))

## Calculate distance matrix parallelly
cl = makeCluster(MAXPROC, outfile="hmm_dist.log", type="FORK")
registerDoParallel(cl)
dm <- as.dist(hmm.dm(models, en.mob, MODEL_DIST_NUM))
saveRDS(dm, paste("rdata/engage.hmm.dmat.s", SAMPLE ,".rds", sep=""))

stopCluster(cl)

# EOF