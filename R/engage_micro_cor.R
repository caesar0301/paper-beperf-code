#!/usr/bin/env R
## run engage-micro-serialize.R first to generate RDS object
## @author: chenxm, hsiamin@gmx.com
library(dplyr)
library(ggplot2)
library(reshape2)
library(gplots)
library(corrgram)
library(grid)
library(plyr)
source("commons.R")

## read raw engagement data
en.mob.ori <- readRDS("rdata/engage.raw.mob.rds")
en.mob <- readRDS("rdata/engage.filtered.mob.rds")
en.nmob.ori <- readRDS("rdata/engage.raw.nmob.rds")
en.nmob <- readRDS("rdata/engage.filtered.nmob.rds")

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# General correlation analysis
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

en.data <- en.mob
CL <- function(x,sd) mean(x)+sd*sqrt(var(x))

attach(en.data)
## plot SDur ~ ADur
pdf("figures/engage-perc-sdur-adur.pdf", width=4, height=3.5)
par(mar=c(4,4,1,1), cex.lab=1.5, mgp=c(2,0.5,0))
plot(wapply(ADur, SDur, mean,  n=100), xlim=c(0, 60),ylim=c(10, 100),
     type="l", lwd=3, pch=NA,
     xlab="Act. duration (s)", ylab="Eng. duration (m)")
lines(wapply(ADur, SDur, CL, sd=0.25), col="blue", lty=2)
lines(wapply(ADur, SDur, CL, sd=-0.25), col="blue", lty=2)
grid()
dev.off()

## plot SDur ~ WT
pdf("figures/engage-perc-sdur-wt.pdf", width=4, height=3.5)
mpar <- par(mar=c(4,4,1,1), cex.lab=1.5, mgp=c(2,0.5,0))
plot(wapply(MWTime, SDur, mean, n=100), xlim=c(0, 10), ylim=c(30, 100),
     type="l", lwd=4, pch=NA,
     xlab="Wait time (s)", ylab="Eng. duration (m)")
grid()
lines(wapply(MWTime, SDur, CL, sd=0.25), col="blue", lty=2)
lines(wapply(MWTime, SDur, CL, sd=-0.25), col="blue", lty=2)
dev.off()

## plot SDur ~ PABW
pdf("figures/engage-perc-sdur-pbw.pdf", width=4, height=3.5)
mpar <- par(mar=c(4,4,1,1), cex.lab=1.5, mgp=c(2,0.5,0))
plot(wapply(PABw, SDur, mean, n=100), xlim=c(0, 500), ylim=c(20, 60),
     type="l", lwd=4, pch=NA,
     xlab="Perc. bandwidth (KB/s)", ylab="Eng. duration (m)")
grid()
lines(wapply(PABw, SDur, CL, sd=0.25), col="blue", lty=2)
lines(wapply(PABw, SDur, CL, sd=-0.25), col="blue", lty=2)
dev.off()

## plot IR ~ ADur
pdf("figures/engage-perc-ir-adur.pdf", width=4, height=3.5)
mpar <- par(mar=c(4,4,1,1), cex.lab=1.5, mgp=c(2,0.5,0))
plot(wapply(ADur, IR, mean, n=100), xlim=c(0, 60),ylim=c(0, 0.1),
     type="l", lwd=3, pch=NA,
     xlab="Act. duration (s)", ylab="Int. ratio")
grid()
lines(wapply(ADur, IR, CL, sd=0.25), col="blue", lty=2)
lines(wapply(ADur, IR, CL, sd=-0.25), col="blue", lty=2)
dev.off()

## plot IR ~ WT
pdf("figures/engage-perc-ir-wt.pdf", width=4, height=3.5)
mpar <- par(mar=c(4,4,1,1), cex.lab=1.5, mgp=c(2,0.5,0))
plot(wapply(MWTime, IR, mean, n=100), xlim=c(0, 10), ylim=c(0, 0.06),
     type="l", lwd=4, pch=NA,
     xlab="Wait time (s)", ylab="Int. ratio")
grid()
lines(wapply(MWTime, IR, CL, sd=0.25), col="blue", lty=2)
lines(wapply(MWTime, IR, CL, sd=-0.25), col="blue", lty=2)
dev.off()

## plot IR ~ PABW
pdf("figures/engage-perc-ir-pbw.pdf", width=4, height=3.5)
mpar <- par(mar=c(4,4,1,1), cex.lab=1.5, mgp=c(2,0.5,0))
plot(wapply(PABw, IR, mean, n=100), xlim=c(0, 500), ylim=c(0, 0.07),
     type="l", lwd=4, pch=NA,
     xlab="Perc. bandwidth (KB/s)", ylab="Int. ratio")
grid()
lines(wapply(PABw, IR, CL, sd=0.25), col="blue", lty=2)
lines(wapply(PABw, IR, CL, sd=-0.25), col="blue", lty=2)
dev.off()

## plot VF ~ ADur
pdf("figures/engage-perc-vf-adur.pdf", width=4, height=3.5)
mpar <- par(mar=c(4,4,1,1), cex.lab=1.5, mgp=c(2,0.5,0))
plot(wapply(ADur, VF, mean, n=100), xlim=c(0, 60),ylim=c(0, 0.1),
     type="l", lwd=4, pch=NA,
     xlab="Act. duration (s)", ylab="Visit freq.")
grid()
lines(wapply(ADur, VF, CL, sd=0.25), col="blue", lty=2)
lines(wapply(ADur, VF, CL, sd=-0.25), col="blue", lty=2)
dev.off()

## plot VF ~ WT
pdf("figures/engage-perc-vf-wt.pdf", width=4, height=3.5)
mpar <- par(mar=c(4,4,1,1), cex.lab=1.5, mgp=c(2,0.5,0))
plot(wapply(MWTime, VF, mean, n=100), xlim=c(0, 10), ylim=c(0, 0.1),
     type="l", lwd=4, pch=NA,
     xlab="Wait time (s)", ylab="Visit freq.")
grid()
lines(wapply(MWTime, VF, CL, sd=0.25), col="blue", lty=2)
lines(wapply(MWTime, VF, CL, sd=-0.25), col="blue", lty=2)
dev.off()

## plot VF ~ PABW
pdf("figures/engage-perc-vf-pbw.pdf", width=4, height=3.5)
mpar <- par(mar=c(4,4,1,1), cex.lab=1.5, mgp=c(2,0.5,0))
plot(wapply(PABw, VF, mean, n=100), xlim=c(0, 500), ylim=c(0.02, 0.08),
     type="l", lwd=4, pch=NA,
     xlab="Perc. bandwidth (KB/s)", ylab="Visit freq.")
grid()
lines(wapply(PABw, VF, CL, sd=0.25), col="blue", lty=2)
lines(wapply(PABw, VF, CL, sd=-0.25), col="blue", lty=2)
dev.off()

detach(en.data)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Global pair-wise correlation matrix
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cm <- cor(en.mob[,c("SDur","IR","VF","ADur","MWTime","PABw")],
          method="spearman")
svg("figures/engage-micro-cor-pairwise.svg", width=4, height=3.8)
corrgram(cm, order=F, lower.panel=panel.shade,
         upper.panel=panel.pie,
         cex.labels=1,
         labels=c("Eng.\nduration","Int.\nratio","Visit\nfreqency",
                  "Activity\nduration","Expected\nwait time","Perceived\nact. bw."))
rm(cm)
dev.off()

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Calculate correlations with different structured features
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

output.rds <- c("rdata/engage.user.cor.mob.rds",
                "rdata/engage.app.cor.mob.rds",
                "rdata/engage.ua.cor.mob.rds")
input.rds <- rep("rdata/engage.filtered.mob.rds", times=length(output.rds))
structs <- list(c("UID"), c("service.en"), c("UID","service.en"))
mapply(function(ords, irds, struct){
  en.mob <- readRDS(irds)
  en.flt.mob <- scar.filter.by.freq(en.mob, struct)
  cor.mob <- scar.cor(en.flt.mob, struct, scar.target)
  saveRDS(cor.mob, ords)
}, output.rds, input.rds, structs)

## location-specific? Top3 location
en.mob.tl <- readRDS("rdata/engage.filtered.mob.rds")
en.mob.tl <- subset(en.mob.tl, BRank < 4)
struct <- c("UID","service.en","BRank")
en.mob.tl.flt <- scar.filter.by.freq(en.mob.tl, struct, min=10)
en.mob.tl.cor <- scar.cor(en.mob.tl.flt, struct, scar.target)
saveRDS(en.mob.tl.cor, "rdata/engage.ul.cor.mob.rds")

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Impacts of user context factor
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## R({user}|{}): plot significant users (mobile)
usr.cor.mob <- readRDS("rdata/engage.user.cor.mob.rds")
uc.sig.m <- usr.cor.mob[which(usr.cor.mob$prob<0.05), c("UID","dep","ind","cor")]
colnames(uc.sig.m) <- c("UID","eng","perc","cor")

## plot user-specific correlation
plot.user.cor <- function(df){
  levels(df$eng) <- c("Engagement duration",
                      "Interruption ratio",
                      "Visit frequency")
  levels(df$perc) <- c(paste("Act.", "\n", "duration"),
                       paste("Time", "\n", "waiting"),
                       paste("Perc.", "\n", "bandwidth"))
  p <- ggplot(df, aes(factor(perc), cor, group=perc)) +
    geom_violin() + theme_bw() + facet_grid(. ~ eng) +
    labs(x="", y="Spearman corr. (p < 0.05)") +
    theme(axis.text.x=element_text(size=13),
          axis.title.x=element_text(size=13),
          plot.margin=unit(c(3,3,-1,2), "mm"))
  ggsave("figures/engage-micro-cor-user.pdf", width=8, height=3.5)
}
plot.user.cor(uc.sig.m)

## plot application distribution for visit frequency groups
# radical users
uc.sig.m.gr <- subset(uc.sig.m, perc=="ADur" & cor >0 |
                        perc=="MWTime" & cor > 0 |
                        perc=="PABw" & cor < 0)
# conservative users
uc.sig.m.gc <- subset(uc.sig.m, perc=="ADur" & cor <0 |
                        perc=="MWTime" & cor < 0 |
                        perc=="PABw" & cor > 0)

en.mob <- readRDS("rdata/engage.filtered.mob.rds")
en.mob.app <- en.mob %>%
  mutate(UID=UID, service.en=service.en, value=SDur) ## SDur default
en.mob.app.gr <- subset(en.mob.app, UID %in% unique(uc.sig.m.gr$UID))
en.mob.app.gc <- subset(en.mob.app, UID %in% unique(uc.sig.m.gc$UID))

# application visit probability (by time)
cal.vp <- function(app.gr) {
  app.gr %>%
    group_by(UID) %>%
    group_by(UID, service.en) %>%
    dplyr::summarise(app.ttl=sum(value)) %>%
    group_by(UID) %>%
    dplyr::do(
      data.frame(service.en = .$service.en,
                 app.prob = .$app.ttl / sum(.$app.ttl))) %>%
    group_by(service.en) %>%
    dplyr::summarise(
      app.prob.avg = mean(app.prob),
      q25 = quantile(app.prob)[2],
      q75 = quantile(app.prob)[4]
      )
}
app.vf.gr <- cal.vp(en.mob.app.gr)
app.vf.gr$group <- "r"
app.vf.gc <- cal.vp(en.mob.app.gc)
app.vf.gc$group <- "c"
app.vf <- rbind(app.vf.gr, app.vf.gc)

# plot visit probability vs applications of two groups of users
app.vf$group <- factor(app.vf$group, levels=c("r", "c"))
p <- ggplot(app.vf, aes(service.en, y=app.prob.avg,
                        group=group, fill=group, width=0.6)) +
  geom_bar(position="dodge",stat="identity") + theme_bw() +
  xlab("") + ylab("Visit probability") +
  scale_fill_discrete(name="Groups",
                      breaks=c("c", "r"),
                      labels=c("Conservative", "Radical")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = c(0.3, 0.7))
ggsave("figures/engage-micro-cor-user-vf-groups.pdf", p, width=4, height=3)

##*****************************************##
##  impacts of application factor (mobile) ##
##*****************************************##
## R({}|{app})
app.cor.mob <- readRDS("rdata/engage.app.cor.mob.rds")
ac.sig.m <- app.cor.mob[which(app.cor.mob$prob < 0.05),
                        c(".id","dep","ind","cor")]
colnames(ac.sig.m) <- c("app","eng","perc","cor")
ac.sig.m.tb <- xtabs(cor ~ eng + perc + app, ac.sig.m)

##**************************************##
##  impacts of user-app factor (mobile) ##
##**************************************##
uac.all.m <- readRDS("engage.ua.cor.mob.rds")
uac.sig.m <- uac.all.m[which(uac.all.m$prob<0.05),]

## R({user}|{app})
uac.odds.m.apps <- scar.sig.odds(uac.all.m, c("service.en","dep","ind"),
                                 median)
colnames(uac.odds.m.apps) <- c("service.en","eng","perc","cor","odds")
## compare uac.odds.m.apps with ac.sig.m

## R({app}|{user})
## aggregate by eng~perc pairs
uac.odds.m.users <- scar.sig.odds(uac.all.m, c("UID","dep","ind"), mean)
colnames(uac.odds.m.users) <- c("UID","eng","perc","cor","odds")

## R({user}|{}) vs. R({app}|{user})
plot.user.app.corr <- function(uc, uac){
  merged <- merge(uc, uac, by=c("UID","eng","perc"))
  melted <- melt(subset(merged, select=c(UID, eng, perc, cor.x, cor.y)),
                 id.vars=c("UID","eng","perc"),
                 measure.vars=c("cor.x","cor.y"))
  melted <- aggregate(value ~ eng + perc + variable, melted, median)
  print(head(melted))
  p <- ggplot(melted, aes(paste(eng, perc), value, fill=variable)) +
    geom_bar(position="dodge", stat="identity") + theme_bw()
  p <- p + theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))
  p <- p + xlab("") +
    scale_x_discrete(breaks=c("IR ADur", "IR MWTime", "IR PABw",
                              "SDur ADur", "SDur MWTime", "SDur PABw",
                              "VF ADur", "VF MWTime", "VF PABw"),
                     labels=c("IR:AD", "IR:WT", "IR:PB",
                              "ED:AD", "ED:WT", "ED:PB",
                              "VF:AD", "VF:WT", "VF:PB"))
  ggsave("figures/engage-micro-cor-user-vs-userapp.pdf", p,width=4, height=3)
}
plot.user.app.corr(uc.sig.m, uac.odds.m.users)

# plot user vs. app-user odds
## R({user}|{}) vs. R({user}|{app})
plot.user.app.odds <- function(uc, uac, service=1){
  merged <- merge(uc, uac[which(uac$service.en==service),],
                  by = c("eng","perc"))
  melted <- melt(merged,
                 id.vars=c("eng","perc"),
                 measure.vars=c("odds.x","odds.y"))
  p <- ggplot(melted, aes(paste(eng, perc), value, fill=variable)) +
    geom_bar(position="dodge", stat="identity")
  p <- p+ theme_bw() + xlab("") +
    scale_fill_discrete(name="Distribution",
                        breaks=c("odds.x", "odds.y"),
                        labels=c("{App.}", "{User,App.}"))
  p <- p + theme(legend.position=c(0.8,0.8),
                 axis.text.x=element_text(angle=90))
  ggsave("figures/engage-micro-odds-user-vs-userapp.pdf",
         p, width=4, height=3)
  
}
uc.odds.m <- scar.sig.odds(usr.cor.mob, c("dep","ind"), median)
colnames(uc.odds.m) <- c("eng","perc","cor","odds")
plot.user.app.odds(uc.odds.m, uac.odds.m.apps, service=3)

library(doBy)
summaryBy(cor ~ eng + perc, uc.sig.m, FUN=summary)

##**************************************##
##   impacts of place factor (mobile)   ##
##**************************************##
ulc.mob <- readRDS("rdata/engage.ul.cor.mob.rds")
colnames(ulc.mob) <- c("UID","service.en","BRank","eng",
                       "perc","cor","prob","n")

plot.ulc.brank <- function(selector, filename){
  ulc.mob.selected <- ulc.mob[
    which(ulc.mob$eng == selector[1] & ulc.mob$perc == selector[2] & ulc.mob$prob < 0.05),
    c("UID","BRank","eng","perc","cor")]
  uc.sig.m.selected <- uc.sig.m[
    which(uc.sig.m$eng == selector[1] & uc.sig.m$perc == selector[2]),]
  uc.sig.m.selected["BRank"] <- -1
  ulc.rbind <- rbind(ulc.mob.selected, uc.sig.m.selected)
  ulc.rbind$BRank <- factor(ulc.rbind$BRank)

  # calculate ks value
  user.specific <- subset(ulc.rbind, BRank == -1)$cor
  pr1 <- subset(ulc.rbind, BRank == 1)$cor
  pr2 <- subset(ulc.rbind, BRank == 2)$cor
  pr3 <- subset(ulc.rbind, BRank == 3)$cor
  print(ks.test(pr1, user.specific)$statistic)
  print(ks.test(pr2, user.specific)$statistic)
  print(ks.test(pr3, user.specific)$statistic)
    
  # plot
  p <- ggplot(ulc.rbind, aes(cor, group=BRank, color=BRank)) +
    stat_ecdf() + theme_bw() +
    scale_x_continuous(name="Spearman corr.") +
    scale_y_continuous(name="User CDF") +
    scale_color_discrete(breaks=c("1","2","3","-1"),
                         name="Position Rank",
                         labels=c("Rank-1","Rank-2","Rank-3", "User-specific"))
  p <- p + theme(legend.position=c(0.3, 0.7))
  ggsave(filename, plot=p, width=4, height=3.5)
}
plot.ulc.brank(c("IR","ADur"), "figures/engage-micro-cor-ul-ir-adur.eps")
plot.ulc.brank(c("VF","PABw"), "figures/engage-micro-cor-ul-vf-pabw.eps")

plot.ulc.apps <- function(selector){
  p <- ggplot(ulc.mob[which(ulc.mob$BRank==1 & ulc.mob$prob < 0.05 &
                              ulc.mob$eng == selector[1] &
                              ulc.mob$perc == selector[2]),],
              aes(cor, group=service.en, color=service.en)) +
    stat_ecdf() + theme_bw()
  plot(p)
}
plot.ulc.apps(c("IR","ADur"))
