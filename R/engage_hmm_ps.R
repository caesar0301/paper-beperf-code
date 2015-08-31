#!/usr/bin/env R
# author: chenxm
source("commons.R")
library(ggplot2)
library(car)

en.mob.s <- readRDS("rdata/engage.traj.s100.rds")
models <- readRDS("rdata/engage.hmm.models.s100.rds")
dm <- readRDS("rdata/engage.hmm.dmat.s100.rds")

## Plot dissimilarity matrix
k = 2
s = 100
clust <- hclust(dm, method="average")
ck <- cutree(clust, k = k)
ord <- order(ck)
coph <- cophenetic(clust)
# hmm model distance
postscript(paste("figures/hmm-model-cluster-distance-s", s, "-k", k, "-1.eps", sep=""), width=4, height=4.5)
image(as.matrix(dm)[ord, ord], main = "Model dist.")
dev.off()
# cophenetic distance
postscript(paste("figures/hmm-model-cluster-distance-s", s, "-k", k, "-2.eps", sep=""), width=4, height=4.5)
image(as.matrix(coph)[ord, ord], main = "Cophenetic dist.")
dev.off()
# Dendrogram
postscript(paste("figures/hmm-model-cluster-distance-s", s, "-k", k, "-3.eps", sep=""), width=4, height=4.5)
par(mar=c(4,4,3,2), mgp=c(1.5,0.5,0), cex.lab=1.5)
plot(as.dendrogram(clust), leaflab="none", main="Dendrogram",
     xlab="User trajectories", ylab="Height")
dev.off()
# Shepard diagram
postscript(paste("figures/hmm-model-cluster-distance-s", s, "-k", k, "-4.eps", sep=""), width=4, height=4.5)
par(mar=c(4,4,3,2), mgp=c(2,0.5,0), cex.lab=1.5)
plot(coph ~ dm, ylab = "Cophenetic dist.", xlab = "Model dist.",main = "Shepard diagram")
rs.lab <- sprintf("%.3f", cor(coph, dm, method="spearman"))
text(0.8*max(dm), 0.5*max(coph), cex=1.5, substitute(paste(r[s], " = ", rs), list(rs=rs.lab)))
abline(0,1, col = "red")
box()
dev.off()

users.cls <- data.frame(UID=attr(models, "split_labels"), class=ck)
en.mob.cls <- merge(en.mob.s, users.cls, by=c("UID"))

## extract principle states
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

## analyze clusters regarding PS
ggplot(ps, aes(SDur, VF, color=state)) + geom_point(shape=2, size=2) + theme_bw() +
  xlab("Eng. session duration (s)") + ylab("Visit frequency") +
  theme(legend.position=c(.8, 0.8)) +
  scale_color_discrete(name="PriState", breaks=c("PS1", "PS2"))
ggsave("figures/hmm-cluster-ps-vf-sdur.pdf", width=4, height=3)

## analyze clusters regarding PS distance
ps.diff <- ps %>% group_by(class) %>% do(data.frame(
  SDur.diff=diff(.$SDur),
  VF.diff=diff(.$VF),
  ADur.diff=diff(.$ADur),
  MWTime.diff=diff(.$MWTime),
  PABw.diff=diff(.$PABw)))
ggplot(ps.diff, aes(PABw.diff, group=class, linetype=as.factor(class))) + stat_ecdf()

## analyze clusters regarding PS interactions
ggplot(filter(ps, class==1), aes(ADur, VF, group=state, color=state)) +
  geom_point(shape=1, size=2) + geom_smooth(method=lm, se=F, fullrange=F) + theme_bw() +
  theme(legend.position="none")
ggsave("figures/hmm-cluster-c1-vf-adur.pdf", width=3.5, height=2.5)

ggplot(filter(ps, class==1), aes(MWTime, VF, group=state, color=state)) +
  geom_point(shape=1, size=2) + geom_smooth(method=lm, se=F, fullrange=F) + theme_bw() +
  theme(legend.position="none")
ggsave("figures/hmm-cluster-c1-vf-mwtime.pdf", width=4, height=3)

ggplot(filter(ps, class==1), aes(PABw, VF, group=state, color=state)) +
  geom_point(shape=1, size=2) + geom_smooth(method=lm, se=F, fullrange=F) + theme_bw() +
  theme(legend.position="none")
ggsave("figures/hmm-cluster-c1-vf-pabw.pdf", width=4, height=3)

ggplot(filter(ps, class==2), aes(ADur, VF, group=state, color=state)) +
  geom_point(shape=1, size=2) + geom_smooth(method=lm, se=F, fullrange=F) + theme_bw() +
  theme(legend.position="none")
ggsave("figures/hmm-cluster-c2-vf-adur.pdf", width=4, height=3)

ggplot(filter(ps, class==2), aes(MWTime, VF, group=state, color=state)) +
  geom_point(shape=1, size=2) + geom_smooth(method=lm, se=F, fullrange=F) + theme_bw() +
  theme(legend.position="none")
ggsave("figures/hmm-cluster-c2-vf-mwtime.pdf", width=4, height=3)

ggplot(filter(ps, class==2), aes(PABw, VF, group=state, color=state)) +
  geom_point(shape=1, size=2) + geom_smooth(method=lm, se=F, fullrange=F) + theme_bw() +
  theme(legend.position="none")
ggsave("figures/hmm-cluster-c2-vf-pabw.pdf", width=4, height=3)


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
