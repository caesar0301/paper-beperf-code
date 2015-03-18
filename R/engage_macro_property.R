#!/usr/bin/env R
# author: chenxm
library(plyr)
library(depmixS4)
library(ggplot2)
library(gridExtra)
library(reshape2)
source("engage_common.R")

en.mob.macro <- readRDS("rdata/engage.filtered.mob.rds")
en.nmob.macro <- readRDS("rdata/engage.filtered.nmob.rds")

## Plot engagement time series
pdf("figures/engage-macro-ts.pdf")
plot(ts(en.mob.macro[1:200,c("UID","BRank","TOD","TOW",
                             "service.en","SDur","IR","VF")]),
     main="Macro Enagement")
dev.off()

## Plot session distribution
session.cnt <- ddply(en.mob.macro, c("UID"), function(x)(nrow(x)))
colnames(session.cnt) <- c("UID","sessions")
p <- ggplot(session.cnt, aes(sessions)) + stat_ecdf() +
  coord_cartesian(xlim=c(0,400))
ggsave("figures/engage-macro-obs-cnt.pdf", p)
pdf("figures/engage-macro-obs-cnt2.pdf")
plot(session.cnt$UID, session.cnt$sessions, xlim=c(0,1000))
dev.off()

## Plot entropy of engagement dims
entropies.mob <- cal.entropies(en.mob.macro)
saveRDS(entropies.mob, "rdata/engage.entropies.mob.rds")
entropies.nmob <- cal.entropies(en.nmob.macro)
saveRDS(entropies.nmob, "rdata/engage.entropies.nmob.rds")

## Plot entropies of engagment measures.
postscript("figures/engage-macro-entropy.eps", width=10, height=4)
layout(matrix(c(1,3,2,3), nrow=2, byrow=T))
par(mar=c(3,3,2,1), cex.lab=3, cex.axis=1.5, cex.main=1.5)
random.user <- function(en.mob, entropies.mob){
  user <- en.mob[which(en.mob$UID==sample(entropies.mob$UID, 1)),]
  return(user)}
users <- rbind(random.user(en.mob.macro, entropies.mob),
               random.user(en.mob.macro, entropies.mob))

## Cal sdur prob
sdur.prob <- function(users, group){
  stat <- ddply(users, c("UID"), function(x){
    stat.app <- ddply(x, group, function(y){sum(y$SDur)})
    colnames(stat.app) <- c(group,"SDur")
    stat.app$prob <- stat.app$SDur / sum(stat.app$SDur)
    return(stat.app)})}

## Plot service dist
stat <- acast(sdur.prob(users, c("SrvRank")), UID~SrvRank, value.var="prob")
stat[is.na(stat)] <- 0
barplot(stat, col=heat.colors(length(rownames(stat))),
        width=2, beside=TRUE, main="Distribution of applications")
legend("topright", fill = heat.colors(length(rownames(stat))),
       legend = c("User.A", "User.B"))

## Plot location dist
stat <- acast(sdur.prob(users, c("BRank")), UID~BRank, value.var="prob")
stat[is.na(stat)] <- 0
barplot(stat, col=heat.colors(length(rownames(stat))),
        width=2, beside=TRUE, main="Distribution of positions")

## Plot entropy density
par(mar=c(4.5,4.5,2,2), cex.lab=2, lwd=2)
plot(density(ents$service.en), col=2, xlab="Entropy",
     ylab="p.d.f.", main="", xlim=c(0, 6), ylim=c(0, 2))
for( i in c(3:5))
  lines(density(ents[,i-1]), col=i)

## Groundtruth in random
ents <- entropies.mob[,c("service.en","building","TOD","SDur")]
gt <- entropy(discretize(runif(1000), 100), method="ML")
abline(v=gt, col="black", lty=4)
legend("topright", col=c(2:5,1), lty=c(rep(1,4),4), bg="white", cex=1.2,
       legend=c("App.","Pos.","TOD","Eng.Dur.","Random"))
dev.off()
