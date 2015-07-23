#!/usr/bin/env R
# author: chenxm
library(ggplot2)
library(scales)
library(reshape2)
library(grid)
library(gplots)
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

engage.mob.ori <- readRDS("rdata/engage.raw.mob.rds")
engage.mob <- read2("rdata/engage.filtered.mob.rds")
engage.nmob.ori <- readRDS("rdata/engage.raw.nmob.rds")
engage.nmob <- read2("rdata/engage.filtered.nmob.rds")

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plot unique userstraffic/duration/users in each hour
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ucnt.m <- ddply(engage.mob, c("TOD"), function(x){
  data.frame(uu=length(unique(x$UID)), dur=median(x$SDur), t="m",
             vol=sum(x$volume), size=sum(x$size))})
ucnt.n <- ddply(engage.nmob, c("TOD"), function(x){
  data.frame(uu=length(unique(x$UID)), dur=median(x$SDur), t="n",
             vol=sum(x$volume), size=sum(x$size))})
ucnt.m$dur <- ucnt.m$dur/max(ucnt.m$dur)
ucnt.n$dur <- ucnt.n$dur/max(ucnt.n$dur)
ucnt.m$vol <- ucnt.m$vol/max(ucnt.m$vol)
ucnt.n$vol <- ucnt.n$vol/max(ucnt.n$vol)
ucnt.m$size <- ucnt.m$size/max(ucnt.m$size)
ucnt.n$size <- ucnt.n$size/max(ucnt.n$size)
ucnt.all <- rbind(ucnt.m, ucnt.n)
ucnt.all$uu <- ucnt.all$uu/max(ucnt.all$uu)

## total traffic
p <- ggplot(ucnt.all, aes(TOD, size, linetype=t)) +
  theme_bw() +
  geom_line() +
  geom_point(shape=5) +
  xlab("Hours of Day") +
  ylab("Normalized traffic bytes") +
  scale_linetype_discrete(name="Type",breaks=c("m","n"),labels=c("Mob","NMob"))+
  theme(legend.position=c(0.15,0.8),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15))
ggsave("figures/engage-micro-hour-traffic.pdf", p, width=4, height=3.5)

## unique users
p <- ggplot(ucnt.all, aes(TOD, uu, linetype=t)) +
  theme_bw() +
  geom_line() +
  geom_point(shape=5) +
  xlab("Hours of Day") +
  ylab("Normalized unique users") +
  scale_linetype_discrete(name="Type",breaks=c("m","n"),labels=c("Mob","NMob"))+
  theme(legend.position=c(0.15,0.8),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15))
ggsave("figures/engage-micro-hour-users.pdf", p, width=4, height=3.5)

## session duration
p <- ggplot(ucnt.all, aes(TOD, dur, linetype=t)) +
  theme_bw() +
  geom_line() +
  geom_point(shape=5) +
  xlab("Hours of Day") +
  ylab("Normalized eng. duration") +
  scale_linetype_discrete(name="Type",
                          breaks=c("m","n"),
                          labels=c("Mob","NMob")) +
#   geom_vline(xintercept=7, colour="red", linetype = "longdash") +
#   annotate("text", x=8, y=0.5, label="t=7", color="red") +
#   geom_vline(xintercept=9, colour="green", linetype = "longdash") +
#   annotate("text", x=10, y=0.25, label="t=9", color="green") +
  theme(legend.position=c(0.15,0.85),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15))
ggsave("figures/engage-micro-hour-sdur.pdf", p, width=4, height=3.5)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plot hourly application activity/traffic distribution
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ucnt <- ddply(engage.mob, c("TOD","service.en"), function(x){
  data.frame(vol=sum(x$volume), size=sum(x$size)) })
ucnt$vol <- ucnt$vol/sum(ucnt$vol)
ucnt$size <- ucnt$size/sum(ucnt$size)
srv.ttl <- aggregate(ucnt[, c("vol", "size")], list(ucnt$service.en), sum)
colnames(srv.ttl) <- c("service.en", "vol.ttl", "size.ttl")
ucnt <- join(ucnt, srv.ttl, by="service.en")

## plot activity count
level.ord <- srv.ttl$service.en[order(srv.ttl$vol.ttl, decreasing=T)]
ucnt$service.en <- factor(ucnt$service.en, levels=level.ord)
p <- ggplot(ucnt, aes(TOD, vol, fill=service.en, color=service.en, order=vol.ttl)) +
  geom_area(size=.3, alpha=.4) +
  theme_bw() +
  xlab("Hours of Day") +
  ylab("Normalized total activities") +
  scale_color_discrete(name="Apps") +
  scale_fill_discrete(name="Apps") +
  theme(legend.text=element_text(size=11),
        legend.key.size=unit(0.3, "cm"),
        legend.position=c(0.16, 0.6),
        axis.title.x=element_text(size=17),
        axis.title.y=element_text(size=17))
ggsave("figures/engage-micro-hour-appact-mob.pdf",p, width=4, height=3.5)

## plot traffic volume
level.ord <- srv.ttl$service.en[order(srv.ttl$size.ttl, decreasing=T)]
ucnt$service.en <- factor(ucnt$service.en, levels=level.ord)
p <- ggplot(ucnt, aes(TOD, size,
                      fill=service.en,
                      color=service.en,
                      order=size.ttl)) +
  geom_area(size=.3, alpha=.4) +
  theme_bw() +
  xlab("Hours of Day") +
  ylab("Normalized traffic bytes") +
  scale_color_discrete(name="Apps") +
  scale_fill_discrete(name="Apps") +
  theme(legend.text=element_text(size=9),
        legend.key.size=unit(0.27, "cm"),
        legend.position=c(0.15, 0.65),
        axis.title.x=element_text(size=17),
        axis.title.y=element_text(size=17))
ggsave("figures/engage-micro-hour-apptraffic-mob.pdf",p, width=4, height=3.5)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plot weekly traffic/duration/users in each day
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ucnt.mob <- ddply(engage.mob, c("TOW","TOD"), function(x){
  data.frame(uu=length(unique(x$UID)), dur=median(x$SDur),
             vol=sum(x$volume)/(10^6), size=sum(x$size)/(10^8))})
ucnt.mob$t <- "m"
ucnt.nmob <- ddply(engage.nmob, c("TOW","TOD"), function(x){
  data.frame(uu=length(unique(x$UID)), dur=median(x$SDur),
             vol=sum(x$volume)/(10^6), size=sum(x$size)/(10^8))})
ucnt.nmob$t <- "n"
ucnt.mob$dur <- ucnt.mob$dur/max(ucnt.mob$dur)
ucnt.mob$uu <- ucnt.mob$uu/max(ucnt.mob$uu)
ucnt.mob$vol <- ucnt.mob$vol/max(ucnt.mob$vol)
ucnt.mob$size <- ucnt.mob$size/max(ucnt.mob$size)
ucnt.nmob$dur <- ucnt.nmob$dur/max(ucnt.nmob$dur)
ucnt.nmob$uu <- ucnt.nmob$uu/max(ucnt.nmob$uu)
ucnt.nmob$vol <- ucnt.nmob$vol/max(ucnt.nmob$vol)
ucnt.nmob$size <- ucnt.nmob$size/max(ucnt.nmob$size)

pdf("figures/engage-micro-week-acts.pdf", width=4, height=3.5)
par(mar=c(2,3,1,1), mgp=c(1.5,0.5,0))
plot(ucnt.mob$vol, pch=NA, xlab="", type="l", col="red", cex.lab=1.2,
     xaxt="n", ylab=("Normalized total activities"))
lines(ucnt.nmob$vol, col="blue", lty=3)
legend(5, 1, legend=c("Mob","NMob"), lty=c(1,3), col=c("red", "blue"))
axis(1, at=seq(12, 168, by=24), labels=c("Sn","Mn","Tu","Wd","Th","Fr","Sa"))
dev.off()

pdf("figures/engage-micro-week-sdur.pdf", width=4, height=3.5)
par(mar=c(2,3,1,1), mgp=c(1.5,0.5,0))
plot(ucnt.mob$dur, pch=NA, xlab="", type="l", col="red", cex.lab=1.2,
     xaxt="n", ylab=("Normalized eng. duration"))
lines(ucnt.nmob$dur, col="blue", lty=3)
legend(90, 1, legend=c("Mob","NMob"), lty=c(1,3), col=c("red", "blue"))
axis(1, at=seq(12, 168, by=24), labels=c("Sn","Mn","Tu","Wd","Th","Fr","Sa"))
dev.off()

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plot traffic/duration/users/vf for different intervals of day
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

iday.m <- ddply(subset(engage.mob, IOD!="Dwn"), c("IOD","service.en"), function(x){
  data.frame(uu=length(unique(x$UID)),dur.med=median(x$SDur),
             vf.med=median(x$VF),vol=sum(x$volume)/(10^6), size=sum(x$size)/(10^8))})
iday.m$uu <- iday.m$uu/max(iday.m$uu)
p <- ggplot(iday.m, aes(service.en, uu,  fill=service.en)) +
  geom_bar(stat="identity") + facet_wrap(~IOD, nrow=1) + xlab("") +
  scale_x_discrete(breaks="") + scale_fill_discrete(name="Apps") +
  ylab("Normalized unique users")
p <- p+theme(legend.text=element_text(size=10),
             legend.key.size=unit(0.13,"in"))
ggsave("figures/engage-micro-intday-uniuser-mob.pdf", p, width=6, height=3)