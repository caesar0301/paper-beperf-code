## Script to analyze user (activity) perception
library(ggplot2)
library(dplyr)
source('commons.R')

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ETL measurements of user activities from network traffic
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

serialize.perc.data <- function(file){
  ifile = read.csv(file, sep='\t', head=F)
  colnames(ifile) <- c('category.cn', 'volume', 'size',
                       'ADur', 'MWTime', 'MEdur',
                       'VMR', 'CI', 'PABw')
  ifile$PABw <- ifile$PABw/1024 # KiloByte/s
  ## add english service names, totally 32 categories
  category <- data.frame(category.cn = service.cn,
                         category = service.en)
  ifile <- merge(ifile, category, by=c("category.cn"))
  ## filter category with small amount of sessions
  ifile <- ifile[which(ifile$category != "Others"),]
  ## update factors
  ifile$category.cn <- factor(ifile$category.cn)
  ifile$category <- factor(ifile$category)
  ## add volume bins
  ifile <- transform(ifile,
                     class=cut(volume, c(0,1,5,10,20,max(volume)),
                               labels=c('tiny','small','median','large','huge')))
  ifile
}

act.mob <- serialize.perc.data("../data/activity/activity_perc.mob")
saveRDS(act.mob, "rdata/act.perc.mob.rds")
act.nmob <- serialize.perc.data("../data/activity/activity_perc.nmob")
saveRDS(act.nmob, "rdata/act.perc.nmob.rds")

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Visualize the distribution of activity size
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

act.mob <- readRDS("rdata/act.perc.mob.rds")
act.nmob <- readRDS("rdata/act.perc.nmob.rds")

plot.app.vol <- function(act, ofile){
  (p <- ggplot(act, aes(category, fill=class)) +
    geom_bar(position="fill") +
    coord_flip() + theme_bw() +
    scale_fill_brewer(palette=1) + xlab("")+
    scale_y_continuous(name="Activity proportion",
                       limit=c(0,1)) +
    theme(legend.position=c(0.2,0.7),
          axis.text.x=element_text(size=14),
          axis.text.y=element_text(size=14),
          axis.title.x=element_text(size=16)))
  ggsave(ofile, width=4, height=3.7)
}
plot.app.vol(act.mob, "figures/apps-volume-m.eps")
plot.app.vol(act.nmob, "figures/apps-volume-nm.eps")

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Visualize perceptual metrics for applications
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

apps <- c("Email","Microblog","Music")
act.flt <- (act.mob %>% filter(category %in% apps) %>% mutate(dev="m") ) %>%
  rbind( (act.nmob %>% filter(category %in% apps) %>% mutate(dev="nm")) )

## Activity duration
options("scipen"=100, "digits"=4) # disable scientific notation
(p <- ggplot(act.flt, aes(ADur, color=dev, linetype=category)) +
  stat_ecdf(geom='smooth') + theme_bw() + scale_x_log10() +
  coord_cartesian(xlim=c(0.01, 1000), ylim=c(0, 1.0)) +
  xlab("Activity duration (s)") + ylab("Activity proportion") +
  theme(legend.position=c(.85, .4),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15)) +
  scale_linetype_discrete(name="Applications", labels=apps) +
  scale_color_discrete(name="Devices", labels=c("Mobile", "NonMobile")))
ggsave("figures/apps-adur.eps", p, width=4.5, height=3.5)

## Expected waiting time
(p <- ggplot(act.flt, aes(MWTime, color=dev, linetype=category)) +
  stat_ecdf(geom='smooth') + theme_bw() + scale_x_log10() +
  coord_cartesian(xlim=c(0.01, 60), ylim=c(0, 1.0)) +
  xlab("Expected waiting time (s)") +
  ylab("Activity proportion") +
  theme(legend.position="none",
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15)) +
  scale_linetype_discrete(labels=apps))
ggsave("figures/apps-mwtime.eps", p, width=4.5, height=3.5)

## Perceived bandwidth
(p <- ggplot(act.flt, aes(PABw,color=dev, linetype=category)) +
  stat_ecdf(geom='smooth') + theme_bw() +
  coord_cartesian(xlim=c(0, 300), ylim=c(0, 1.0)) +
  xlab("Perceived activity bandwidth (KB/s)") +
  ylab("Activity proportion") +
  theme(legend.position="none",
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15)) +
  scale_linetype_discrete(labels=apps))
ggsave("figures/apps-pabw.eps", p, width=4.5, height=3.5)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plot perception metrics vs. activity properties
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

median.quartile <- function(x){
  out <- quantile(x, probs = c(0.25,0.5,0.75))
  names(out) <- c("ymin","y","ymax")
  return(out)
}

# duration vs volume
vol.adur <- act.mob %>% select(volume, ADur) %>%
  filter(volume<=50, ADur<=8) %>% group_by(volume) %>%
  summarise(ADur=median(ADur))
lmfit <- lm(ADur ~ log(volume), data=vol.adur)
summary(lmfit)

(p <- ggplot(act.mob, aes(volume, ADur)) +
  stat_summary(fun.y=mean, geom='point') + theme_bw() +
  scale_y_continuous(name='Activity duration (s)', limits=c(0,8)) +
  scale_x_continuous(name='Entities/Activity', limits=c(0,50)) +
  stat_smooth(method='lm',formula = y ~ log(x), size = 1) +
  scale_colour_hue(l=80, c=150) +
  annotate("text", x = 20, y = 7, label = "y=1.37lnx-0.53, p<0.001", size=5) +
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15)))
ggsave("figures/act-dur-vol.eps", p, width=4, height=3.5)

## data rate vs. volume
(p <- ggplot(act.mob, aes(x=volume, y=PABw)) + theme_bw() +
  stat_summary(fun.y=mean, geom='point') +
  scale_x_continuous(name='Entities/Activity', limits=c(0,50)) +
  scale_y_continuous(name='Perceived throughput (KB/s)',
                     limits=c(0,50)) +
  scale_colour_hue(l=80, c=150) +
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15)))
ggsave("figures/act-pbw-vol.eps", p, width=4, height=3.5)

## duration vs. perceived bw
(p <- ggplot(act.mob, aes(PABw, ADur))+
  geom_point(size=0.6) + theme_bw() + facet_wrap(~class) +
  scale_x_log10(name='Perceived throughput (KB/s)',
                limits=c(0.1,1000)) +
  scale_y_log10(name='Activity duration (s)', limits=c(0.1,100))+
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15)))
ggsave("figures/act-dur-pabw.eps", p, width=4, height=3.5)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plot relationship of CI and VMR
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ifile <- transform(
  readRDS("rdata/act.perc.mob.rds"),
  class.dur=cut(ADur, c(0,0.2,0.6,1.4,4.5,20),
                labels=c('(0.0,0.2]','(0.2,0.6]','(0.6,1.4]',
                         '(1.4,4.5]','(4.5,20]')),
  class.vol=cut(volume, c(0,1,5,10,20,max(volume)),
                labels=c('1','(1, 5]','(5, 10]','(10, 20]', '>20')))
sample5k <- ifile[sample(nrow(ifile),size=5000,replace=F),]

## CI/VMR with activity size
(p <- ggplot(sample5k, aes(VMR, CI, color=class.dur)) +
  geom_point() + theme_bw() +
  geom_vline(xintercept = .5, color='darkgrey') +
  geom_hline(yintercept = .5, color='darkgrey') +
  scale_x_continuous(limit=c(0,1)) +
  scale_y_continuous(limit=c(0,1)) +
  theme(legend.position=c(.8,.7)) +
  scale_colour_discrete(name="Duration (s)") +
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15)))
ggsave("figures/vmr-ci-adur.eps", p, width=4, height=3.5)

## CI/VMR with activity duration
(p <- ggplot(sample5k, aes(VMR, CI, color=class.vol)) +
  geom_point() + theme_bw() +
  geom_vline(xintercept = .5, color='darkgrey') +
  geom_hline(yintercept = .5, color='darkgrey') +
  scale_x_continuous(limit=c(0,1)) +
  scale_y_continuous(limit=c(0,1)) +
  theme(legend.position=c(.8,.7),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15)) +
  scale_colour_discrete(name="Entities/Activity"))
ggsave("figures/vmr-ci-volume.eps", p, width=4, height=3.5)
