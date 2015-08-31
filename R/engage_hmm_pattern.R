#!/usr/bin/env R
# author: chenxm
library(dplyr)
library(ggplot2)

## load data 200
models <- readRDS("rdata/engage.hmm.models.s20000.rds")[1:200]
en.mob.s <- readRDS("rdata/engage.hmm.traj.s20000.rds") %>%
  filter(UID %in% names(models))
dm <- as.dist(t(readRDS("rdata/engage.hmm.dmat.s200.rds")))

## Plot dissimilarity matrix
k = 2
s = 200
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

## add class lable to engaging trajectories
users.cls <- data.frame(UID=names(models), cluster=ck)
en.mob.cls <- merge(en.mob.s, users.cls, by=c("UID")) %>% mutate(cluster=as.factor(cluster))

## analyze cluster basic dimensions
en.mob.cls.user <- en.mob.cls %>% group_by(UID) %>% summarise(
  STime=mean(STime),
  SDur=mean(SDur),
  IR=mean(IR),
  VF=mean(VF),
  ADur=mean(ADur),
  MWTime=mean(MWTime),
  PABw=mean(PABw),
  cluster=unique(cluster))
ggplot(en.mob.cls.user, aes(VF, group=cluster, linetype=cluster, color=cluster)) +
  stat_ecdf() + theme_bw() + xlab("Visit Frequency") + ylab("User CDF") +
  theme(legend.position=c(0.8, 0.3)) +
  scale_linetype_discrete(name="Cluster", breaks=c("1", "2")) +
  scale_color_discrete(name="Cluster", breaks=c("1", "2"))
ggsave("figures/hmm-cluster-vf-ecdf.pdf", width=4, height=3)

## analyze cluster correlations
scar.deps <- c("SDur","IR","VF")
scar.inds <- c("ADur","MWTime","PABw")
struct <- c("UID")
target <- c(scar.deps, scar.inds)
cor.c1 <- en.mob.cls %>% filter(cluster==1) %>% scar.filter.by.freq(struct) %>% scar.cor(struct, target)
cor.c2 <- en.mob.cls %>% filter(cluster==2) %>% scar.filter.by.freq(struct) %>% scar.cor(struct, target)
cor.all <- rbind(cor.c1 %>% mutate(cluster=1), cor.c2 %>% mutate(cluster=2))
cor.all.s <- cor.all %>% filter(dep=="VF", ind=="ADur")
ggplot(cor.all.s, aes(cor, group=cluster, linetype=as.factor(cluster), color=as.factor(cluster))) +
  stat_ecdf() + theme_bw() + xlab("Spearman corr. (VisitFreq vs PercAD)") + ylab("User CDF") +
  theme(legend.position=c(0.8, 0.3)) +
  scale_linetype_discrete(name="Cluster", breaks=c("1", "2")) +
  scale_color_discrete(name="Cluster", breaks=c("1", "2"))
ggsave("figures/hmm-cluster-cor-vf-adur.pdf", width=4, height=3)


