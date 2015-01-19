#!/usr/bin/env R
# author: chenxm
## import scar.* funcs
source("engage_common.R")

## serialize data as R objects
saveRDS(serialize.mydata("../data/engage/micro_engage_perc.mob"),
        file="rdata/engage.raw.mob.rds")
saveRDS(serialize.mydata("../data/engage/micro_engage_perc.nmob"),
        file="rdata/engage.raw.nmob.rds")

## filter fields, covering more than 95\%
ff <- function(raw.mob){
    en.mob.macro <- subset(raw.mob, volume/SDur>=0.8 & volume/SDur <= 10
                           & ADur <= 60 & SDur <= 300 & MWTime <= 10
                           & PABw <= 512 & VF <= 0.2)
    scar.filter.by.freq(en.mob.macro, c("UID"), min=20)}
saveRDS(ff(readRDS("rdata/engage.raw.mob.rds")),
        "rdata/engage.filtered.mob.rds")
saveRDS(ff(readRDS("rdata/engage.raw.nmob.rds")),
        "rdata/engage.filtered.nmob.rds")
