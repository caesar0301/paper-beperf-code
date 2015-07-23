#!/usr/bin/env R
# author: chenxm
# http://www.hsiamin.com
library(ggplot2)
library(scales)
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

merge2 <- function(engage.mob, engage.nmob){
  ## aggregate data by app category
  fields <- c("volume", "size", "SDur", "IR", "VF",
              "ADur", "MWTime", "PABw")
  med.mob <- aggregate(engage.mob[, fields],
                       list(service.en=engage.mob$service.en),
                       median)
  med.nmob <- aggregate(engage.nmob[, fields],
                        list(service.en=engage.nmob$service.en),
                        median)
  merge(med.mob, med.nmob, by=c("service.en"))
}
merged <- merge2(engage.mob, engage.nmob)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plot engagement duration distribution for different application categories.
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

merged$service.en <-
  factor(merged$service.en, levels=merged$service.en[order(merged$SDur.x)])
melted <- melt(merged[,c("service.en","SDur.x","SDur.y")], id.vars=c("service.en"))

p <- ggplot(melted, aes(service.en, y=value, group=variable,
                        fill=variable, width=0.6)) +
  geom_bar(position="dodge",stat="identity") +
  theme_bw() + 
  coord_flip() + 
  scale_x_discrete(name="") +
  scale_y_continuous(name="Eng. duration (mins)",
                     limits=c(0,100), oob=squish) +
  scale_fill_discrete(name="Device types", breaks=c("SDur.x","SDur.y"),
                      labels=c("Mob","NMob")) +
  theme(legend.position=c("none"),
        plot.margin=unit(c(.2,.2,0,-0.3), 'cm'),
        axis.text.y=element_text(size=15),
        axis.title.x=element_text(size=15))
ggsave("figures/engage-micro-app-sdur.eps", p, width=4, height=5)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plot session volume (number of activities) distribution
# for different device platforms
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

merged$service.en <-
  factor(merged$service.en, levels=merged$service.en[order(merged$volume.x)])
melted <- melt(merged[,c("service.en","volume.x","volume.y")], id.vars=c("service.en"))

p <- ggplot(melted, aes(x=service.en,
                        y=value,
                        fill=variable,
                        group=variable, width=0.6)) +
  geom_bar(position="dodge", stat="identity") +
  theme_bw() +
  coord_flip() +
  scale_x_discrete(name="") +
  scale_y_continuous(name="# Activities/session",
                     limits=c(0,700),
                     oob=squish) +
  scale_fill_discrete(name="Device types",
                      breaks=c("volume.x","volume.y"),
                      labels=c("Mob","NMob")) +
  theme(legend.position=c(0.8,0.4),
        plot.margin=unit(c(0.2,0.2,0,-0.3), 'cm'),
        axis.text.y=element_text(size=15),
        axis.title.x=element_text(size=15))
ggsave("figures/engage-micro-app-vol.eps", p, width=4, height=5)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plot visit frequency distribution of platforms
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

merged$service.en <-
  factor(merged$service.en, levels= merged$service.en[order(merged$VF.x)])
melted <- melt(merged[,c("service.en","VF.x","VF.y")], id.vars=c("service.en"))
melted$value <- melted$value * 100

p <- ggplot(melted, aes(x=service.en,
                        y=value,
                        fill=variable,
                        group=variable,
                        width=.6)) +
  geom_bar(position="dodge",stat="identity") +
  theme_bw() +
  coord_flip() +
  scale_x_discrete(name="") +
  scale_y_continuous(name="Visit frequency (1/100s)",
                     limits=c(0,20),
                     oob=squish) +
  scale_fill_discrete(name="Device types",
                      breaks=c("VF.x","VF.y"),
                      labels=c("Mob","NMob")) +
  theme(legend.position=c(0.8,0.4),
        plot.margin=unit(c(0.2,0.2,0,-0.3), 'cm'),
        axis.text.y=element_text(size=15),
        axis.title.x=element_text(size=15))
ggsave("figures/engage-micro-app-vf.eps", p, width=4, height=5)