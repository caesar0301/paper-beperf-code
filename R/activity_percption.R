## Script to analyze activity perception
library(ggplot2)
act.mob <- readRDS("rdata/act.perc.mob.rds")
act.nmob <- readRDS("rdata/act.perc.nmob.rds")

plot.app.vol <- function(act, ofile){
  p <- ggplot(act, aes(category, fill=class)) +
    geom_bar(position="fill") +
    coord_flip() + theme_bw() +
    scale_fill_brewer(palette=1) + xlab("")+
    scale_y_continuous(name="Activity proportion",
                       limit=c(0,1)) +
    theme(legend.position=c(0.2,0.7),
          axis.text.x=element_text(size=14),
          axis.text.y=element_text(size=14),
          axis.title.x=element_text(size=16))
  ggsave(ofile, width=4, height=3.7)
  
}
plot.app.vol(act.mob, "figures/apps-volume.m.eps")
plot.app.vol(act.nmob, "figures/apps-volume.nm.eps")


##########################################
## Visualize perceptual metrics for selected applications
##########################################

selector <- c("Email","Microblog","Music")
linetypes <- c("solid","dotted","dotdash")
act.mob.flt <- act.mob[which(act.mob$category %in% selector), ]
act.mob.flt$dev <- "m"
act.nmob.flt <- act.nmob[which(act.nmob$category %in% selector), ]
act.nmob.flt$dev <- "nm"
act.flt <- rbind(act.mob.flt, act.nmob.flt)

## ADur
options("scipen"=100, "digits"=4) # disable scientific notation
p <- ggplot(act.flt, aes(ADur, color=dev, linetype=category)) +
  stat_ecdf(geom='smooth') + theme_bw() +
  scale_x_log10() +
  coord_cartesian(xlim=c(0.01, 1000), ylim=c(0, 1.0)) +
  xlab("Activity duration (s)") + ylab("Activity proportion")
p <- p + theme(legend.position=c(.85, .4),
               axis.title.x=element_text(size=15),
               axis.title.y=element_text(size=15))
p <- p + scale_linetype_manual(name="Applications",
                               breaks=selector, values=linetypes) +
  scale_color_discrete(name="Devices", labels=c("Mobile", "NonMob"))
ggsave("figures/apps-adur.eps", p, width=4.5, height=3.5)

## MWTime
p <- ggplot(act.flt, aes(MWTime, color=dev, linetype=category)) +
  stat_ecdf(geom='smooth') + theme_bw() + scale_x_log10() +
  coord_cartesian(xlim=c(0.01, 60), ylim=c(0, 1.0)) +
  xlab("Expected waiting time (s)") +
  ylab("Activity proportion")
p <- p + theme(legend.position="none",
               axis.title.x=element_text(size=15),
               axis.title.y=element_text(size=15))
## Remove legend to make plot clearer
## Place it beside next plot in paper
ggsave("figures/apps-mwtime.eps", p, width=4.5, height=3.5)

## PABW
p <- ggplot(act.flt, aes(PABw,color=dev, linetype=category)) +
  stat_ecdf(geom='smooth') + theme_bw() +
  coord_cartesian(xlim=c(0, 300), ylim=c(0, 1.0)) +
  xlab("Perceived activity bandwidth (KB/s)") +
  ylab("Activity proportion")
p <- p + theme(legend.position="none",
               axis.title.x=element_text(size=15),
               axis.title.y=element_text(size=15))
ggsave("figures/apps-pabw.eps", p, width=4.5, height=3.5)


## ECDFs
p <- ggplot(act.mob.flt, aes(size,colour=category)) +
  stat_ecdf(geom='smooth') + theme_bw() +
  coord_cartesian(xlim=c(1, 100000), ylim=c(0, 1.0)) +
  scale_x_log10()
plot(p)

p <- ggplot(act.mob.flt, aes(CI, colour=category)) +
  stat_ecdf(geom='smooth') + theme_bw() +
  coord_cartesian(xlim=c(0, 1), ylim=c(0, 1.0))
plot(p)

p <- ggplot(act.mob.flt, aes(VMR,colour=category)) +
  stat_ecdf(geom='smooth') + theme_bw() +
  coord_cartesian(xlim=c(0, 1), ylim=c(0, 1.0))
plot(p)


########################################
## Plot activity properties vs. perception metrics
########################################

median.quartile <- function(x){
  out <- quantile(x, probs = c(0.25,0.5,0.75))
  names(out) <- c("ymin","y","ymax")
  return(out)
}

# duration vs volume
vol <- act.mob[,c("volume","ADur")]
vol.adur <- aggregate(ADur ~ volume,data=subset(vol, volume<=50 &
                                                  ADur<=8), FUN=median)
lmfit <- lm(ADur ~ log(volume), data=vol.adur)
summary(lmfit)

p <- ggplot(act.mob, aes(volume, ADur)) +
  stat_summary(fun.y=mean, geom='point') + theme_bw() +
  scale_y_continuous(name='Activity duration (s)', limits=c(0,8)) +
  scale_x_continuous(name='Entities/Activity', limits=c(0,50))
p <- p + stat_smooth(method='lm',formula = y ~ log(x), size = 1) +
  scale_colour_hue(l=80, c=150) +
  annotate("text", x = 18, y = 6, label = "y=1.37lnx-0.53, p<0.001", size=6) +
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15))
ggsave("figures/act-dur-vol.eps", p, width=4.5, height=3.5)

## data rate vs. volume
p <- ggplot(act.mob, aes(x=volume, y=PABw)) + theme_bw() +
  stat_summary(fun.y=mean, geom='point')
p <- p +  scale_x_continuous(name='Entities/Activity', limits=c(0,50)) +
  scale_y_continuous(name='Perceived bandwidth (KB/s)',
                     limits=c(0,50))
p <- p + scale_colour_hue(l=80, c=150) +
  theme(axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15))
ggsave("figures/act-pbw-vol.eps", p, width=4.5, height=3.5)

## duration vs. perceived bw
p <- ggplot(act.mob, aes(PABw, ADur))+ geom_point(size=0.6) + theme_bw() +
  scale_x_log10(name='Perceived bandwidth (KB/s)',
                limits=c(0.1,1000)) +
  scale_y_log10(name='Activity duration (s)', limits=c(0.1,100))+
  facet_wrap(~class)
p <- p + theme(axis.title.x=element_text(size=15),
               axis.title.y=element_text(size=15))
ggsave("figures/act-dur-pabw.eps", p, width=5, height=5)

## CI vs. VMR
ifile = readRDS("act.perc.mob.rds")
ifile <- transform(
  ifile,
  class.dur=cut(ADur, c(0,0.2,0.6,1.4,4.5,20),
  labels=c('(0.0,0.2]','(0.2,0.6]','(0.6,1.4]',
           '(1.4,4.5','(4.5,20]')),
  class.vol=cut(volume, c(0,1,5,10,20,max(volume)),
                labels=c('tiny','small','median','large','huge')))

sample <- ifile[sample(nrow(ifile),size=5000,replace=F),]
p <- ggplot(sample, aes(VMR, CI, color=class.dur)) +
  geom_point() + theme_bw() +
  geom_vline(xintercept = .5, color='darkgrey') +
  geom_hline(yintercept = .5, color='darkgrey') +
  scale_x_continuous(limit=c(0,1)) +
  scale_y_continuous(limit=c(0,1)) +
  theme(legend.position=c(.75,.75)) +
  scale_colour_discrete(name="Duration")
p <- p + theme(axis.title.x=element_text(size=15),
               axis.title.y=element_text(size=15))
ggsave("figures/vmr-ci-adur.eps", p, width=4.5, height=3.5)

p <- ggplot(sample, aes(VMR, CI, color=class.vol)) +
  geom_point() + theme_bw() +
  geom_vline(xintercept = .5, color='darkgrey') +
  geom_hline(yintercept = .5, color='darkgrey') +
  scale_x_continuous(limit=c(0,1)) +
  scale_y_continuous(limit=c(0,1)) +
  theme(legend.position=c(.75,.75)) +
  scale_colour_discrete(name="Entities/Activity")
p <- p + theme(axis.title.x=element_text(size=15),
               axis.title.y=element_text(size=15))
ggsave("figures/vmr-ci-volume.eps", p, width=4.5, height=3.5)
