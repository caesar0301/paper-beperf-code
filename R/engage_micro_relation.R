#!/usr/bin/env R
# author: chenxm
library(ggplot2)
library(scales)
library(reshape2)
library(grid)
library(gplots)
library(plyr)
source("engage_common.R")

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

engage.mob.ori <- readRDS("rdata/engage.raw.mob.rds")
engage.mob <- read2("rdata/engage.filtered.mob.rds")
engage.nmob.ori <- readRDS("rdata/engage.raw.nmob.rds")
engage.nmob <- read2("rdata/engage.filtered.nmob.rds")

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plot the impact of IR on engagement duration
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# method 1
irs.mob <- engage.mob[which(engage.mob$IR>0),]
irs.nmob <- engage.nmob[which(engage.nmob$IR>0),]
## Plot average relationship of engagement duration and IR
irs.mob.sm <- irs.mob[sample(1000:nrow(irs.mob), 10000, replace=F),]
irs.mob.sm.lir <- irs.mob.sm[which(irs.mob.sm$IR<=0.15),]
attach(irs.mob.sm.lir)
pdf("figures/engage-micro-impact-sdur-ir2.pdf",width=8,height=6)
par(mar=c(5,5,0.5,0.5))
bandplot(IR, SDur, ylim=c(0.1,150), xlim=c(0,0.15), cex=0.5, pch=20,
         xlab="Interruption ratio", ylab="Engagement duration (mins)",
         cex.lab=2, cex.axis=1.5)
dev.off()
wm <- wapply(IR, SDur)
reg <- lm(log(wm$y) ~ wm$x)
summary(reg)
detach(irs.mob.sm.lir)

# method 2
irs.mob.sm <- irs.mob[sample(1000:nrow(irs.mob),2000,replace=F),]
irs.nmob.sm <- irs.nmob[sample(1000:nrow(irs.nmob),2000, replace=F),]
irs <- rbind(cbind(irs.mob.sm,type="0"), cbind(irs.nmob.sm,type="1"))
p <- ggplot(irs, aes(x=SDur, y=IR, group=type, color=type)) +
  theme_bw() +
  geom_point(size=1, alpha=0.5) +
  geom_smooth(method=lm,se=F) +
  scale_x_continuous(limits=c(0,180), name="Eng. duration (mins)") +
  scale_y_continuous(limits=c(0,0.1), name="Interruption ratio") +
  theme(legend.position=c(0.8,0.8)) +
  scale_color_discrete(name="Dev. Types",
                       breaks=c("0","1"),
                       labels=c("Mob","NMob")) +
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15))
Lmfit.mob <- lm(SDur ~ IR, data=irs.mob.sm)
lmfit.nmob <- lm(SDur ~ IR, data=irs.nmob.sm)
ggsave("figures/engage-micro-impact-sdur-ir.pdf",p,width=4,height=3)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plot the impacts of waiting time on IR for different platforms
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

plot.irs.dens <- function(irsbw, filename){
  irsbw$bins <- ave(irsbw$MWTime, cut(irsbw$MWTime, 800), FUN=median)
  irsbw.bins <- aggregate(irsbw$IR, by=list(bins=irsbw$bins), FUN=mean)
  ## plot a long range
  p <- ggplot(irsbw.bins, aes(x=bins, y=x)) +
    geom_line()+
    theme_bw() +
    scale_x_continuous(limits=c(0, 13)) +
    scale_y_continuous(limits=c(0,0.2)) +
    stat_smooth(method="loess") +
    labs(x="Expected waiting time (s)", y="Interruption ratio") +
    theme(axis.title.x=element_text(size=15),
          axis.title.y=element_text(size=15))
  ggsave(filename, p,width=4, height=3)
}
plot.irs.dens(irs.mob, "figures/engage-micro-impact-ir-wt-mob.eps")
plot.irs.dens(irs.nmob, "figures/engage-micro-impact-ir-wt-nmob.eps")

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plot the impact of activity duration on IR
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

plot.iradur.dens <- function(irsbw, filename){
  irsbw$bins <- ave(irsbw$ADur, cut(irsbw$ADur, 1000), FUN=median)
  irsbw.bins <- aggregate(irsbw$IR, by=list(bins=irsbw$bins), FUN=mean)
  ## plot a long range
  p <- ggplot(irsbw.bins, aes(x=bins, y=x)) +
    geom_line()+
    theme_bw() +
    scale_x_continuous(limits=c(0, 60)) +
    scale_y_continuous(limits=c(0,0.5)) +
    stat_smooth(method="loess") +
    labs(x="Activity duration (s)", y="Interruption ratio") +
    theme(axis.title.x=element_text(size=15),
          axis.title.y=element_text(size=15))
  ggsave(filename, p,width=4, height=3)}
plot.iradur.dens(irs.mob, "figures/engage-micro-impact-ir-adur-mob.eps")
plot.iradur.dens(irs.nmob, "figures/engage-micro-impact-ir-adur-nmob.eps")
