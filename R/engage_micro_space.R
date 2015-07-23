#!/usr/bin/env R
# author: chenxm
library(ggplot2)
library(plyr)
source("commons.R")

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load data and do some preprocessing
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
read2 <- function(engage.rds) {
  engage <- readRDS(engage.rds)
  engage$IOD <- cut(engage$TOD,
                    breaks=c(-1,2,6,11,13,18,21,24),
                    labels=c("Mid","Dwn","Mor","Noon","Afn","Ngt","Mid"))
  engage$IOD <- factor(engage$IOD)
  engage
}

engage.mob <- read2("rdata/engage.filtered.mob.rds")
engage.nmob <- read2("rdata/engage.filtered.nmob.rds")

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plot spatial chracteristics of engagement sessions
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Return probability of users in the same place (also with semantics)
pu.m <- ddply(engage.mob, c("BRank", "UID"), function(x){
  data.frame(dur=median(x$SDur),
             vf=median(x$IR),
             IR=mean(x$IR),
             vol=median(x$volume),
             ent=entropy(table(x$service.en),method="ML"))
})

p <- ggplot(subset(pu.m, BRank %in% c(1,2,5,10,20)),
            aes(dur,
                group=as.factor(BRank),
                linetype=as.factor(BRank))) +
  theme_bw() + stat_ecdf() +
  coord_cartesian(xlim=c(1,200)) +
  ylab("CDF of users") +
  scale_x_log10(name="Eng. duration (mins)") +
  scale_linetype_discrete(name="PlaceRank") +
  theme(legend.position=c(0.8,0.4),
        axis.title.x=element_text(size=17),
        axis.title.y=element_text(size=17))
ggsave("figures/engage-micro-space-sdur-mob.pdf", p, width=4, height=3.5)

p <- ggplot(subset(pu.m, BRank %in% c(1,2,5,10,20)),
            aes(IR,
                group=as.factor(BRank),
                linetype=as.factor(BRank))) +
  theme_bw() + stat_ecdf() +
  coord_cartesian(xlim=c(0.0001, 0.1)) +
  ylab("CDF of users") +
  scale_x_log10(name="Int. ratio") +
  scale_linetype_discrete(name="PlaceRank")+
  theme(legend.position=c(0.85,0.35),
        axis.title.x=element_text(size=17),
        axis.title.y=element_text(size=17))
ggsave("figures/engage-micro-space-ir-mob.pdf", p, width=4, height=3.5)

p <- ggplot(subset(pu.m, BRank %in% c(1,2,5,10,20)),
            aes(vol,
                group=as.factor(BRank),
                linetype=as.factor(BRank))) +
  theme_bw() + stat_ecdf() +
  coord_cartesian(xlim=c(10, 1000)) + ylab("CDF of users") +
  scale_x_log10(name="Activities per session") +
  scale_linetype_discrete(name="PlaceRank") +
  theme(legend.position=c(0.85,0.35),
        axis.title.x=element_text(size=17),
        axis.title.y=element_text(size=17))
ggsave("figures/engage-micro-space-vol-mob.pdf", p, width=4, height=3.5)

p <- ggplot(subset(pu.m, BRank %in% c(1,2,5,10,20)),
            aes(ent,
                group=as.factor(BRank),
                linetype=as.factor(BRank))) +
  theme_bw() + stat_ecdf() +
  coord_cartesian(xlim=c(0, 1.5)) +
  ylab("CDF of users") +
  scale_x_continuous(name="Entropy of app.") +
  scale_linetype_discrete(name="PlaceRank") +
  theme(legend.position=c(0.85,0.3),
        axis.title.x=element_text(size=17),
        axis.title.y=element_text(size=17))
ggsave("figures/engage-micro-space-ent-mob.pdf", p, width=4, height=3.5)
