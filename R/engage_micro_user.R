#!/usr/bin/env R
# author: chenxm
library(ggplot2)
library(reshape2)
library(plyr)
library(poweRlaw)

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
# Plot the distribution of engagement session number of users
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## estimate power law
estimate.pl <- function(V){
  mm <- conpl$new(V)
  mm$setXmin(estimate_xmin(mm))
  mm$setPars(estimate_pars(mm))
  return(mm)
}

## estimate log-normal
estimate.ln <- function(V){
  mm <- conlnorm$new(V)
  mm$setPars(estimate_pars(mm))
  mm$setXmin(estimate_xmin(mm))
  return(mm)
}

pdf("figures/engage-micro-user-session-cnt.pdf", width=4, height=3.5)
par(mar=c(3,3,1,1), mgp=c(1.8 ,0.6 ,0), cex.lab = 1.2)
## estimate mobile
scnt <- ddply(engage.mob.ori, c("UID"), nrow)
pl <- estimate.ln(scnt$V1[1:5000]) # to show fast
plot(pl, pch=1, col="red", xlab="Eng. sessions per user",
     ylab="p.d.f.", xlim=c(1,1000))
lines(pl, col="black", lwd=2)
text(7, 0.05, cex=1.2, col="red",
     substitute(paste(x[0], "=", xmin, ", ", sigma, "=", par),
                list(xmin=pl$getXmin(),par=pl$getPars())))
par(new=TRUE)
## estimate non-mobile
scnt <- ddply(engage.nmob.ori, c("UID"), nrow)
pl <- estimate.ln(scnt$V1[1:5000])
plot(pl, pch=4, axes=FALSE, col="green", xlab="", ylab="", xlim=c(1, 1000))
lines(pl, col="black", lwd=2)
text(200, 0.4, cex=1.2, col="green",
     substitute(paste(x[0], "=", xmin, ", ", sigma, "=", par),
                list(xmin=pl$getXmin(), par=pl$getPars())))
legend(1.5, 5e-03, legend=c("Mob","NMob"), pch=c(1,4), col=c("red", "green"))
dev.off()

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plot average engagement session duration on different platforms
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

slen <- ddply(engage.mob, c("UID"), function(x) median(x$SDur))
slen$t <- "m"
slen.all <- slen
slen <- ddply(engage.nmob, c("UID"), function(x) median(x$SDur))
slen$t <- "n"
slen.all <- rbind(slen.all, slen)
ks <- ks.test(slen.all$V1[slen.all$t=="m"], slen.all$V1[slen.all$t=="n"])
p <- ggplot(slen.all, aes(V1, group=t, linetype=t)) +
  theme_bw() +
  xlab("Eng. duration (mins)") +
  ylab("Density of users") +
  geom_density() +
  scale_x_log10() +
  scale_linetype_discrete(name="Type", breaks=c("m","n"),
                          labels=c("Mob","Nmob")) +
  theme(legend.position=c(0.2,0.8),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15)) +
  annotate("text", x=50, y=0.3,
           label=paste("ks =", format(ks$statistic, digits=3), ", p < 2.2e-16"))
ggsave("figures/engage-micro-user-sdur.pdf", p, width=4, height=3.5)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plot average visit frequency per user on different platforms
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

slen <- ddply(engage.mob, c("UID"), function(x) median(x$VF))
slen$t <- "m"
slen.all <- slen
slen <- ddply(engage.nmob, c("UID"), function(x) median(x$VF))
slen$t <- "n"
slen.all <- rbind(slen.all, slen)
ks <- ks.test(slen.all$V1[slen.all$t=="m"], slen.all$V1[slen.all$t=="n"])
p <- ggplot(slen.all, aes(V1, linetype=t)) +
  theme_bw() + geom_density() +
  scale_x_log10(name="Visit frequency", limit=c(0.01,0.5)) +
  ylab("Density of users") +
  scale_linetype_discrete(name="Type",
                          breaks=c("m","n"),
                          labels=c("Mob","Nmob")) +
  theme(legend.position=c(0.7,0.8),
               axis.title.x=element_text(size=15),
               axis.title.y=element_text(size=15)) +
  annotate("text", x=0.2, y=1.5, label=paste("ks =", format(ks$statistic, digits=3))) +
  annotate("text", x=0.25, y=1.2, label="p < 2.2e-16")
ggsave("figures/engage-micro-user-vf.pdf", p, width=4, height=3.5)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plot average session activities per user on different platforms
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

slen <- ddply(engage.mob, c("UID"), function(x) median(x$volume))
slen$t <- "m"
slen.all <- slen
slen <- ddply(engage.nmob, c("UID"), function(x) median(x$volume))
slen$t <- "n"
slen.all <- rbind(slen.all, slen)
ks <- ks.test(slen.all$V1[slen.all$t=="m"], slen.all$V1[slen.all$t=="n"])
med.m <- median(slen.all[slen.all$t=="m",]$V1)
med.nm <- median(slen.all[slen.all$t=="n",]$V1)
p <- ggplot(slen.all, aes(V1, linetype=t)) + theme_bw()+stat_ecdf() +
  xlab("Activities per session") + ylab("CDF") +
  coord_cartesian(xlim=c(1, 500)) +
  scale_linetype_discrete(name="Type",breaks=c("m","n"),
                          labels=c("Mob","Nmob")) +
  geom_vline(xintercept=med.m, colour="red", linetype = "longdash") +
  annotate("text", x=med.m+60, y=0.3, label=paste("n=", med.m), color="red") +
  geom_vline(xintercept=med.nm, colour="green", linetype = "longdash") +
  annotate("text", x=med.nm+60, y=0.5, label=paste("n=", med.nm), color="green") +
  theme(legend.position=c(0.8,0.4),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15)) +
  annotate("text", x=300, y=0.1,
          label=paste("ks =", format(ks$statistic, digits=3), ", p < 2.2e-16"))
ggsave("figures/engage-micro-user-session-vol.pdf", p, width=4, height=3.5)