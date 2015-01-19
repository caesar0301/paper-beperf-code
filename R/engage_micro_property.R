#!/usr/bin/env R
# author: chenxm
# http://www.hsiamin.com
library(ggplot2)
library(scales)
library(reshape2)
library(grid)
library(gplots)
library(poweRlaw)
library(plyr)
source("engage_common.R")

## Load data and do some preprocessing
engage.mob.ori <- readRDS("data/engage.raw.mob.rds")
engage.mob <- readRDS("data/engage.filtered.mob.rds")
engage.mob$IOD <- cut(engage.mob$TOD, breaks=c(-1,2,6,11,13,18,21,24),
                      labels=c("Mid","Dwn","Mor","Noon","Afn","Ngt","Mid"))
engage.mob$IOD <- factor(engage.mob$IOD)
engage.nmob.ori <- readRDS("data/engage.raw.nmob.rds")
engage.nmob <- readRDS("data/engage.filtered.mob.rds")
engage.nmob$IOD <- cut(engage.nmob$TOD, breaks=c(-1,2,6,11,13,18,21,24),
                      labels=c("Mid","Dwn","Mor","Noon","Afn","Ngt","Mid"))
engage.nmob$IOD <- factor(engage.nmob$IOD)

## Merge mobile and non-mobile datasets into a unified one
merge.mob.nmob <- function(engage.mob, engage.nmob){
    ## aggregate data by app category
    med.mob <- aggregate(engage.mob[,seq(4,11)],
                         by=list(service.en=engage.mob$service.en),
                         FUN=median)
    med.nmob <- aggregate(engage.nmob[,seq(4,11)],
                          by=list(service.en=engage.nmob$service.en),
                          FUN=median)
    ## merge mobile and non-mobile data
    merge(med.mob, med.nmob, by=c("service.en"))}
merged <- merge.mob.nmob(engage.mob, engage.nmob)


## Plot engagement duration distribution for different
## application categories.
merged$service.en <- factor(merged$service.en,
                            levels=merged$service.en[order(merged$SDur.x)])
melted <- melt(merged[,c("service.en","SDur.x","SDur.y")],
               id.vars=c("service.en"))
p <- ggplot(melted, aes(service.en, y=value, group=variable,
                        fill=variable, width=0.6))
p <- p + geom_bar(position="dodge",stat="identity")
p <- p + theme_bw() + coord_flip() + scale_x_discrete(name="")
p <- p + scale_y_continuous(name="Eng. duration (mins)",
                     limits=c(0,100), oob=squish) + coord_flip()
p <- p + scale_fill_discrete(name="Device types", breaks=c("SDur.x","SDur.y"),
                      labels=c("Mob","NMob"))
p <- p + theme(legend.position=c("none"),
               plot.margin=unit(c(.2,.2,0,-0.3), 'cm'),
               axis.text.y=element_text(size=15),
               axis.title.x=element_text(size=15))
ggsave("figures/engage-micro-property-sdur.eps", p, width=4, height=5)


## Plot session volume (number of activities) distribution
## for different device platforms
merged$service.en <- factor(merged$service.en,
                            levels=merged$service.en[order(merged$volume.x)])
melted <- melt(merged[,c("service.en","volume.x","volume.y")],
               id.vars=c("service.en"))
p <- ggplot(melted, aes(x=service.en, y=value, fill=variable,
                        group=variable, width=0.6))
p <- p + geom_bar(position="dodge",stat="identity")
p <- p + theme_bw() + coord_flip() +scale_x_discrete(name="")
p <- p + scale_y_continuous(name="# Activities/session",
                            limits=c(0,700), oob=squish)
p <- p + scale_fill_discrete(name="Device types", breaks=c("volume.x","volume.y"),
                             labels=c("Mob","NMob"))
p <- p + theme(legend.position=c(0.8,0.4),
               plot.margin=unit(c(0.2,0.2,0,-0.3), 'cm'),
               axis.text.y=element_text(size=15),
               axis.title.x=element_text(size=15))
ggsave("figures/engage-micro-property-vol.eps", p, width=4, height=5)


## Plot visit frequency distribution of platforms
merged$service.en <- factor(merged$service.en, levels=
                                merged$service.en[order(merged$VF.x)])
melted <- melt(merged[,c("service.en","VF.x","VF.y")],
               id.vars=c("service.en"))
melted$value <- melted$value * 100
p <- ggplot(melted, aes(x=service.en, y=value, fill=variable,
                        group=variable, width=.6))
p <- p + geom_bar(position="dodge",stat="identity")
p <- p + theme_bw() + coord_flip() + scale_x_discrete(name="")
p <- p + scale_y_continuous(name="Visit frequency (1/100s)",
                            limits=c(0,20), oob=squish)
p <- p + scale_fill_discrete(name="Device types", breaks=c("VF.x","VF.y"),
                             labels=c("Mob","NMob"))
p <- p + theme(legend.position=c(0.8,0.4),
               plot.margin=unit(c(0.2,0.2,0,-0.3), 'cm'),
               axis.text.y=element_text(size=15),
               axis.title.x=element_text(size=15))
ggsave("figures/engage-micro-property-vf.eps", p, width=4, height=5)


## Select data with positive IR
irs.mob <- engage.mob[which(engage.mob$IR>0),]
irs.nmob <- engage.nmob[which(engage.nmob$IR>0),]
## Plot average relationship of engagement duration and IR
irs.mob.sm <- irs.mob[sample(1000:nrow(irs.mob),10000,replace=F),]
irs.mob.sm.lir <- irs.mob.sm[which(irs.mob.sm$IR<=0.15),]
attach(irs.mob.sm.lir)
postscript("figures/engage-micro-property-ir2.eps",width=8,height=6)
par(mar=c(5,5,0.5,0.5))
bandplot(IR, SDur, ylim=c(0.1,150), xlim=c(0,0.15), cex=0.5, pch=20,
         xlab="Interruption ratio", ylab="Engagement duration (mins)",
         cex.lab=2, cex.axis=1.5)
dev.off()
wm <- wapply(IR, SDur)
reg <- lm(log(wm$y) ~ wm$x)
summary(reg)
detach(irs.mob.sm.lir)


## Plot the impact of IR on engagement duration (similar to formmer)
irs.mob.sm <- irs.mob[sample(1000:nrow(irs.mob),2000,replace=F),]
irs.nmob.sm <- irs.nmob[sample(1000:nrow(irs.nmob),2000, replace=F),]
irs <- rbind(cbind(irs.mob.sm,type="0"), cbind(irs.nmob.sm,type="1"))
p <- ggplot(irs, aes(x=SDur, y=IR, group=type, color=type)) + theme_bw()
p <- p + geom_point(size=1, alpha=0.5) + geom_smooth(method=lm,se=F)
p <- p + scale_x_continuous(limits=c(0,180), name="Eng. duration (mins)")
p <- p + scale_y_continuous(limits=c(0,0.1), name="Interruption ratio")
p <- p + theme(legend.position=c(0.8,0.8))
p <- p + scale_color_discrete(name="Dev. Types", breaks=c("0","1"),
                       labels=c("Mob","NMob"))
p <- p + theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15))
Lmfit.mob <- lm(SDur ~ IR, data=irs.mob.sm)
lmfit.nmob <- lm(SDur ~ IR, data=irs.nmob.sm)
ggsave("figures/engage-micro-property-ir.pdf",p,width=4,height=3)


## Plot the impacts of waiting time on IR for different platforms
plot.irs.dens <- function(irsbw, filename){
    irsbw$bins <- ave(irsbw$MWTime, cut(irsbw$MWTime, 800), FUN=median)
    irsbw.bins <- aggregate(irsbw$IR, by=list(bins=irsbw$bins), FUN=mean)
    ## plot a long range
    p <- ggplot(irsbw.bins, aes(x=bins, y=x)) + geom_line()+ theme_bw()
    + scale_x_continuous(limits=c(0, 13))
    + scale_y_continuous(limits=c(0,0.2)) + stat_smooth(method="loess")
    + labs(x="Expected waiting time (s)", y="Interruption ratio")
    + theme(axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=15))
    ggsave(filename, p,width=4, height=3)}
plot.irs.dens(irs.mob, "figures/engage-micro-property-ir-vs-wt-mob.eps")
plot.irs.dens(irs.nmob, "figures/engage-micro-property-ir-vs-wt-nmob.eps")


## Plot the impact of activity duration on IR
plot.iradur.dens <- function(irsbw, filename){
    irsbw$bins <- ave(irsbw$ADur, cut(irsbw$ADur, 1000), FUN=median)
    irsbw.bins <- aggregate(irsbw$IR, by=list(bins=irsbw$bins), FUN=mean)
    ## plot a long range
    p <- ggplot(irsbw.bins, aes(x=bins, y=x)) + geom_line()+ theme_bw()
    + scale_x_continuous(limits=c(0, 300)) + scale_y_continuous(limits=c(0,0.5))
    + stat_smooth(method="loess")
    + labs(x="Activity duration (s)", y="Interruption ratio")
    + theme(axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=15))
    ggsave(filename, p,width=4, height=3)}
plot.iradur.dens(irs.mob, "figures/engage-micro-property-ir-vs-adur-mob.eps")
plot.iradur.dens(irs.nmob, "figures/engage-micro-property-ir-vs-adur-nmob.eps")


## Plot the distribution of engagement number of users
estimate.pl <- function(V){
    mm <- conpl$new(V)
    mm$setXmin(estimate_xmin(mm))
    mm$setPars(estimate_pars(mm))
    return(mm)}
estimate.ln <- function(V){
    mm <- conlnorm$new(V)
    mm$setPars(estimate_pars(mm))
    mm$setXmin(estimate_xmin(mm))
    return(mm)}
pdf("figures/engage-micro-session-num.pdf", width=8, height=5)
par(mar=c(5,5,2,2), cex.lab=1.8, cex.axis=1.2)
scnt <- ddply(engage.mob.ori, c("UID"), function(x) nrow(x))
pl <- estimate.ln(scnt$V1[1:20000]) # to show fast
plot(pl, pch=1, col="red", xlab="Eng. sessions per user",
     ylab="p.d.f.", xlim=c(1,1000))
lines(pl, col="black", lwd=2)
text(350, 0.3, cex=1.7, substitute(paste(x[0], "=", xmin, ", ",
    sigma, "=", par), list(xmin=pl$getXmin(), par=pl$getPars())), col="red")
par(new=TRUE)
scnt <- ddply(engage.nmob.ori, c("UID"), function(x) nrow(x))
pl <- estimate.ln(scnt$V1[1:20000])
plot(pl, pch=4, axes=FALSE, col="green", xlab="", ylab="", xlim=c(1, 1000))
lines(pl, col="black", lwd=2)
text(50, 0.01, cex=1.7, substitute(paste(x[0], "=", xmin, ", ",
    sigma, "=", par), list(xmin=pl$getXmin(), par=pl$getPars())), col="green")
legend(1.5, 5e-03, legend=c("Mob","NMob"), pch=c(1,4), col=c("red", "green"), cex=2)
dev.off()


## Plot average engagement duration per user on different platforms
slen <- ddply(engage.mob, c("UID"), function(x) median(x$SDur))
slen$t <- "m"
slen.all <- slen
slen <- ddply(engage.nmob, c("UID"), function(x) median(x$SDur))
slen$t <- "n"
slen.all <- rbind(slen.all, slen)
p <- ggplot(slen.all, aes(V1, group=t, linetype=t)) + theme_bw() +
    xlab("Eng. duration (mins)") + ylab("Density of users")
p <- p + geom_density() + scale_x_log10() +
    scale_linetype_discrete(name="Type", breaks=c("m","n"),
                            labels=c("Mob","Nmob"))
p <- p + geom_vline(xintercept=10, colour="red", linetype = "longdash")
p <- p + geom_vline(xintercept=80, colour="green", linetype = "longdash")
p <- p + annotate("text", x = 300, y = 0.9, label="d=80", color="green")
p <- p + theme(legend.position=c(0.2,0.8),
               axis.title.x=element_text(size=15),
               axis.title.y=element_text(size=15))
ggsave("figures/engage-micro-session-dur.pdf", p, width=5, height=3)


## Plot global engagement duration distribution on different platforms
slen.all <- rbind(data.frame(sdur=engage.mob$SDur, t="m"),
                  data.frame(sdur=engage.nmob$SDur, t="n"))
p <- ggplot(slen.all, aes(sdur, linetype=t)) + theme_bw() + stat_ecdf()
p <- p + scale_x_log10(name="Eng. duration (mins)", limit=c(0.1,1000))
p <- p + scale_linetype_discrete(name="Type",breaks=c("m","n"),
                                 labels=c("Mob","NMob"))
p <- p + theme(legend.position=c(0.2,0.7),
               axis.title.x=element_text(size=15),
               axis.title.y=element_text(size=15))
ggsave("figures/engage-micro-session-sdur-all.pdf", p, width=5, height=3)


## Plot average visit frequency per user on different platforms
slen <- ddply(engage.mob, c("UID"), function(x) median(x$VF))
slen$t <- "m"
slen.all <- slen
slen <- ddply(engage.nmob, c("UID"), function(x) median(x$VF))
slen$t <- "n"
slen.all <- rbind(slen.all, slen)
p <- ggplot(slen.all, aes(V1, linetype=t)) + theme_bw() +geom_density()
p <- p + scale_x_log10(name="Visit frequency", limit=c(0.01,1))
p <- p + ylab("Density of users") + scale_linetype_discrete(
    name="Type", breaks=c("m","n"), labels=c("Mob","Nmob"))
p <- p + geom_vline(xintercept=0.05, colour="green", linetype = "longdash")
p <- p + annotate("text", x=0.1, y=3, label="f=0.05", color="green")
p <- p + theme(legend.position=c(0.9,0.8),
               axis.title.x=element_text(size=15),
               axis.title.y=element_text(size=15))
ggsave("figures/engage-micro-visit-freq.pdf", p, width=5, height=3)


## Plot average session activities per user on different platforms
slen <- ddply(engage.mob, c("UID"), function(x) median(x$volume))
slen$t <- "m"
slen.all <- slen
slen <- ddply(engage.nmob, c("UID"), function(x) median(x$volume))
slen$t <- "n"
slen.all <- rbind(slen.all, slen)
p <- ggplot(slen.all, aes(V1, linetype=t)) + theme_bw()+stat_ecdf()
p <- p + xlab("Eng. session volume") + ylab("CDF")
p <- p + coord_cartesian(xlim=c(1, 1000)) +
    scale_linetype_discrete(name="Type",breaks=c("m","n"),
                            labels=c("Mob","Nmob"))
p <- p + geom_vline(xintercept=45, colour="red", linetype = "longdash")
p <- p + annotate("text", x=120, y=0.5, label="n=45", color="red")
p <- p + geom_vline(xintercept=225, colour="green", linetype = "longdash")
p <- p + annotate("text", x=320, y=0.5, label="n=225", color="green")
p <- p + theme(legend.position=c(0.8,0.4),
               axis.title.x=element_text(size=15),
               axis.title.y=element_text(size=15))
ggsave("figures/engage-micro-session-vol.pdf", p, width=5, height=3)


## Plot unique userstraffic/duration/users in each hour
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
p <- ggplot(ucnt.all, aes(TOD, size, linetype=t)) + theme_bw() + geom_line()
p <- p + geom_point(shape=5) + xlab("Hours of Day") +
    ylab("Normalized traffic volume")
p <- p + scale_linetype_discrete(name="Type",breaks=c("m","n"),labels=c("Mob","NMob"))
p <- p + theme(legend.position=c(0.15,0.8),
               axis.title.x=element_text(size=15),
               axis.title.y=element_text(size=15))
ggsave("figures/engage-micro-hour-size.pdf", p, width=5, height=3)
p <- ggplot(ucnt.all, aes(TOD, dur, linetype=t)) + theme_bw() + geom_line()
p <- p + geom_point(shape=5) + xlab("Hours of Day") +
    ylab("Normalized eng. duration")
p <- p + scale_linetype_discrete(name="Type",breaks=c("m","n"),
                                 labels=c("Mob","NMob"))
p <- p + geom_vline(xintercept=7, colour="red", linetype = "longdash")
p <- p + annotate("text", x=8, y=0.5, label="t=7", color="red")
p <- p + geom_vline(xintercept=9, colour="green", linetype = "longdash")
p <- p + annotate("text", x=10, y=0.25, label="t=9", color="green")
p <- p + theme(legend.position=c(0.9,0.8),
               axis.title.x=element_text(size=15),
               axis.title.y=element_text(size=15))
ggsave("figures/engage-micro-hour-sdur.pdf", p, width=5, height=3)


## Plot application distribution in each hour

ucnt.m <- ddply(engage.mob, c("TOD","service.en"), function(x){
    data.frame(vol=sum(x$volume), size=sum(x$size))})
ucnt.m$vol <- ucnt.m$vol/sum(ucnt.m$vol)
ucnt.m$size <- ucnt.m$size/sum(ucnt.m$size)
p <- ggplot(ucnt.m, aes(TOD, vol, fill=service.en, color=service.en))
p <- p + geom_area(size=.3, alpha=.4) + theme_bw()
p <- p + xlab("Hours of Day") + ylab("Normalized total activities")
p <- p + scale_color_discrete(name="Apps")
p <- p + scale_fill_discrete(name="Apps")
p <- p+theme(legend.text=element_text(size=12),legend.key.size=unit(0.4,"cm"),
             legend.position=c(0.17,0.55),
             axis.title.x=element_text(size=17),
             axis.title.y=element_text(size=17))
ggsave("figures/engage-micro-hour-appvol-mob.pdf",p, width=6, height=4)
p <- ggplot(ucnt.m, aes(TOD, size, fill=service.en, color=service.en))
p <- p + geom_area(size=.3, alpha=.4) + theme_bw()
p <- p + xlab("Hours of Day") + ylab("Normalized traffic volume")
p <- p + scale_color_discrete(name="Apps")
p <- p + scale_fill_discrete(name="Apps")
p <- p+theme(legend.text=element_text(size=12),legend.key.size=unit(0.4,"cm"),
             legend.position=c(0.17,0.55),
             axis.title.x=element_text(size=17),
             axis.title.y=element_text(size=17))
ggsave("figures/engage-micro-hour-appsize-mob.pdf",p, width=6, height=4)


## Plot traffic/duration/users in each day of a week
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
pdf("figures/engage-micro-week-vol.pdf", width=5, height=3)
par(mar=c(2,3,1,1), mgp=c(1.5,0.5,0))
plot(ucnt.mob$vol, pch=NA, xlab="", type="l", col="red", cex.lab=1.2,
     xaxt="n", ylab=("Normalized total activities"))
lines(ucnt.nmob$vol, col="blue", lty=3)
legend(5, 1, legend=c("Mob","NMob"), lty=c(1,3), col=c("red", "blue"))
axis(1, at=seq(12, 168, by=24), labels=c("Sn","M","Tu","W","Th","F","Sa"))
dev.off()
pdf("figures/engage-micro-week-sdur.pdf", width=5, height=3)
par(mar=c(2,3,1,1), mgp=c(1.5,0.5,0))
plot(ucnt.mob$dur, pch=NA, xlab="", type="l", col="red", cex.lab=1.2,
     xaxt="n", ylab=("Normalized eng. duration"))
lines(ucnt.nmob$dur, col="blue", lty=3)
legend(90, 1, legend=c("Mob","NMob"), lty=c(1,3), col=c("red", "blue"))
axis(1, at=seq(12, 168, by=24), labels=c("Sn","M","Tu","W","Th","F","Sa"))
dev.off()


## Plot traffic/duration/users/vf for different intervals of day
iday.m <- ddply(subset(engage.nmob, IOD!="Dwn"), c("IOD","service.en"), function(x){
    data.frame(uu=length(unique(x$UID)),dur.med=median(x$SDur),
               vf.med=median(x$VF),vol=sum(x$volume)/(10^6), size=sum(x$size)/(10^8))})
iday.m$uu <- iday.m$uu/max(iday.m$uu)
p <- ggplot(iday.m, aes(service.en, uu,  fill=service.en)) +
    geom_bar(stat="identity") + facet_wrap(~IOD, nrow=1) + xlab("") +
        scale_x_discrete(breaks="") + scale_fill_discrete(name="Apps") +
            ylab("Normalized unique users")
p <- p+theme(legend.text=element_text(size=10),
             legend.key.size=unit(0.13,"in"))
ggsave("figures/engage-micro-intday-uniuser-nmob.pdf", p, width=6, height=3)


## Return probability of users in the same place (also with semantics)
pu.m <- ddply(engage.nmob, c("BRank", "UID"), function(x){
    data.frame(dur=median(x$SDur), vf=median(x$IR), IR=mean(x$IR),
               vol=median(x$volume),
               ent=entropy(table(x$service.en),method="ML"))})
p <- ggplot(subset(pu.m, BRank %in% c(1,2,5,10,20)),
            aes(dur, group=as.factor(BRank), linetype=as.factor(BRank))) +
                theme_bw() + stat_ecdf()
p <- p + coord_cartesian(xlim=c(1,200)) + ylab("CDF of users") +
    scale_x_log10(name="Eng. duration (mins)") +
        scale_linetype_discrete(name="PlaceRank")
p <- p + theme(legend.position=c(0.8,0.4),
             axis.title.x=element_text(size=17),
             axis.title.y=element_text(size=17))
ggsave("figures/engage-micro-plc-sdur-nmob.pdf", p, width=5, height=3)
p <- ggplot(subset(pu.m, BRank %in% c(1,2,5,10,20)),
            aes(IR, group=as.factor(BRank), linetype=as.factor(BRank))) +
                theme_bw() + stat_ecdf()
p <- p + coord_cartesian(xlim=c(0.0001, 0.1)) + ylab("CDF of users") +
    scale_x_log10(name="Int. ratio") +
        scale_linetype_discrete(name="PlaceRank")
p <- p + theme(legend.position=c(0.85,0.35),
             axis.title.x=element_text(size=17),
             axis.title.y=element_text(size=17))
ggsave("figures/engage-micro-plc-ir-nmob.pdf", p, width=5, height=3)
p <- ggplot(subset(pu.m, BRank %in% c(1,2,5,10,20)),
            aes(vol, group=as.factor(BRank), linetype=as.factor(BRank))) +
                theme_bw() + stat_ecdf()
p <- p + coord_cartesian(xlim=c(10, 1000)) + ylab("CDF of users") +
    scale_x_log10(name="Eng. session volume") +
        scale_linetype_discrete(name="PlaceRank")
p <- p + theme(legend.position=c(0.85,0.35),
             axis.title.x=element_text(size=17),
             axis.title.y=element_text(size=17))
ggsave("figures/engage-micro-plc-vol-nmob.pdf", p, width=5, height=3)
p <- ggplot(subset(pu.m, BRank %in% c(1,2,5,10,20)),
            aes(ent, group=as.factor(BRank), linetype=as.factor(BRank))) +
                theme_bw() + stat_ecdf()
p <- p + coord_cartesian(xlim=c(0, 1.5)) + ylab("CDF of users") +
    scale_x_continuous(name="Service entropy") +
        scale_linetype_discrete(name="PlaceRank")
p <- p + theme(legend.position=c(0.8,0.4),
             axis.title.x=element_text(size=17),
             axis.title.y=element_text(size=17))
ggsave("figures/engage-micro-plc-ent-nmob.pdf", p, width=5, height=3)
