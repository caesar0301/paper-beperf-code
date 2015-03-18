#!/usr/bin/env R
# author: chenxm
library(depmixS4)
library(ggplot2)
library(GGally)
library(plyr)
library(cluster)
library(reshape2)
library(scales)
library(car)
library(grid)
source("engage_common.R")

## Load data
en.mob.ori <- readRDS("rdata/engage.filtered.mob.rds")
en.mob <- en.mob.ori
en.mob$TOD <- as.numeric(en.mob$TOD)
en.mob$service.en <- as.numeric(en.mob$service.en)
en.mob$BRank <- as.numeric(en.mob$BRank)

## Generate a experimental sample
en.mob.s <- sample.trajs(en.mob, 100)
saveRDS(en.mob.s, "rdata/engage.macro.s100.rds")

## All users modeling
models <- dlply(en.mob.s, c("UID"), function(user) hmm.learn(user))
na.selector <- sapply(models, is.na)
models[na.selector] <- NULL# remove NA
attr(models, "split_labels") <- attr(models, "split_labels")[!na.selector,]
saveRDS(models, "rdata/engage.hmm.models.s100.rds")

## Calculate distance matrix
models <- readRDS("rdata/engage.hmm.models.s100.rds")
en.mob.s <- sample.trajs(en.mob, 200) # calculate converge threshold
dm <- as.dist(hmm.dm(models, en.mob.s))
saveRDS(dm, "rdata/engage.hmm.dmat.rds")

## Plot dissimilarity matrix
en.mob.s <- readRDS("rdata/engage.macro.s100.rds")
models <- readRDS("rdata/engage.hmm.models.s100.rds")
dm <- readRDS("rdata/engage.hmm.dmat.s100.rds")
postscript("figures/hmm-model-cluster-distance.eps", width=12, height=3)
ck <- plot.dm.hc(dm, 2)
table(ck)
users.cls <- data.frame(UID=attr(models, "split_labels"), class=ck)
en.mob.cls <- merge(en.mob.s, users.cls, by=c("UID"))
dev.off()

## observations of principle states
ps <- do.call(rbind, lapply(models, function(m){
  states <- posterior(m)$state
  pca <- order(table(states), decreasing=T)[1:2]
  response <- summary(m, which="response")
  prp <- response[pca, c(1,3,5,7,9)]
  prp <- data.frame(prp)
  colnames(prp) <- c("SDur","VF","ADur","MWTime","PABw")
  prp <- prp[order(prp$SDur),]
  prp$state <- c("PS1","PS2")
  return(prp)
}))
ps$UID <- sapply(rownames(ps), function(s) strsplit(s, "\\.")[[1]][1])
ps <- merge(ps, users.cls, by=c("UID"))

## Different cluster
pdf("figures/hmm-pstates-cls1.pdf", width=8, height=6)
scatterplotMatrix(
  ~ SDur + VF + ADur + MWTime + PABw|state, smoother=FALSE,
  data=ps[ps$class=="1",], by.groups=TRUE, reg.line=rlm,
  var.labels = c("Eng.Dur","VisitFreq.","Act.Dur","WaitTime","Perc.BW"))
dev.off()
pdf("figures/hmm-pstates-cls2.pdf", width=8, height=6)
scatterplotMatrix(
  ~ SDur + VF + ADur + MWTime + PABw|state, smoother=FALSE,
  data=ps[ps$class=="2",], by.groups=TRUE, reg.line=rlm,
  var.labels = c("Eng.Dur","VisitFreq.","Act.Dur","WaitTime","Perc.BW"))
dev.off()

## Different states
pdf("figures/hmm-clusters-ps1.pdf", width=8, height=6)
scatterplotMatrix(
  ~ SDur + VF + ADur + MWTime + PABw|class, smoother=FALSE,
  data=ps[ps$state=="PS1",], by.groups=TRUE, reg.line=rlm,
  var.labels = c("Eng.Dur","VisitFreq.","Act.Dur","WaitTime","Perc.BW"))
dev.off()
pdf("figures/hmm-clusters-ps2.pdf", width=8, height=6)
scatterplotMatrix(
  ~ SDur + VF + ADur + MWTime + PABw|class, smoother=FALSE,
  data=ps[ps$state=="PS2",], by.groups=TRUE, reg.line=rlm,
  var.labels = c("Eng.Dur","VisitFreq.","Act.Dur","WaitTime","Perc.BW"))
dev.off()


## Cluster states
model.ns <- data.frame(sapply(models, function(x) nstates(x)))
model.ns$UID <- rownames(model.ns)
colnames(model.ns) <- c("ns","UID")
model.ns <- merge(model.ns, users.cls, by=c("UID"))
model.ns$class <- as.factor(model.ns$class)
p <- ggplot(model.ns, aes(ns, group=class, fill=class)) + theme_bw() +
  geom_bar()
plot(p)

## cluster time series
pdf("figures/engage-ts-cls-1.pdf")
plot(ts(en.mob.cls[en.mob.cls$class=="1",
                   c("UID","service.en","TOD","TOW","SDur","ADur","PABw","MWTime")]))
dev.off()
pdf("figures/engage-ts-cls-2.pdf")
plot(ts(en.mob.cls[en.mob.cls$class=="2",
                   c("UID","service.en","TOD","TOW","SDur","ADur","PABw","MWTime")]))
dev.off()


p <- ggplot(en.mob.cls, aes(SDur, group=class)) + theme_bw() +
  stat_ecdf()
plot(p)

## One user 39856 for example
unique.uid <- unique(en.mob$UID)
macro.uid <- 39856 # unique.uid[12101]
macro.user <- subset(en.mob, UID==macro.uid)
saveRDS(macro.user, paste("rdata/hmm-model-user-", macro.uid, ".rds", sep=""))

## Learn HMM model and plot optimal states
opt.model <- hmm.learn(macro.user, plot=T)

## Coordinate plot of observation metrics
macro.user$state <- factor(posterior(opt.model)$state)
p <- ggparcoord(macro.user, scale="center",  order="skewness",
                columns=c("VF","SDur","IR","ADur","PABw","MWTime"),
                groupColumn=c("state")) + geom_line() + theme_bw()
p <- p + xlab("") + ylab("Scaled value") +
  scale_x_discrete(breaks=c("SDur","IR","VF","ADur","MWTime","PABw"),
                   labels=c("EngDur","IntRatio","VisitFreq",
                            "ActDur","WatiTime","PercBW"))
p <- p + ggtitle(paste("User ", macro.uid))
p <- p + theme(legend.position=c(0.95,0.5),
               axis.text.x=element_text(size=13),
               axis.title.y=element_text(size=13),
               plot.margin=unit(c(2,2,0,2), "mm"))
ggsave("figures/hmm-model-optimal-features.pdf", p, width=6, height=3)

## Plot hidden state series
pdf("figures/hmm-model-optimal-states-ts.pdf", width=4, height=3.5)
par(mar=c(3,3,1,1), mgp=c(1.7,0.5,0), cex.lab=1.2)
plot(ts(posterior(opt.model)$state),
     xlab="Observations over time", ylab="Latent state")
dev.off()
