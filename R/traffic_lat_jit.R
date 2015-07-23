# Explore the distribution of activity latencies and jitters.
library(ggplot2)
library(reshape2)

sampledata <- function(file, type, sep='\t', head=F, size=10000, replace=F){
  data <- read.csv(file, sep=sep, head=head, nrows=size*3)
  data <- data * 1000 # convert to ms
  s <- data[sample(nrow(data),size=size,replace=replace),]
  colnames(s) <- c('sLat', 'dLat', 'sJit', 'dJit')
  s$type <- type
  return(s)
}

bindtwo <- function(mobile, nonmobile){
  m <- sampledata(mobile, 'm')
  nm <- sampledata(nonmobile, 'n')
  all <- rbind(m, nm)
  return(all)
}

all <- bindtwo('../../data/activity/lat-jit-m.out',
               '../../data/activity/lat-jit-nm.out')

lj.ml <- melt(all, measure.vars=c('sLat', 'dLat'))
# Latency
(p <- ggplot(lj.ml, aes(value, color=variable, linetype=type)) +
  stat_ecdf() + theme_bw() +
  scale_x_log10(name='Network latency (ms)', limits=c(0.1, 1000)) +
  scale_y_continuous(name='Cumulative portion of activities') +
  scale_colour_discrete(name='Direction', labels=c('Client', 'Server')) +
  scale_linetype_discrete(name='Devices', labels=c('Mob', 'NMob')) +
  theme(legend.position=c(.2, .7)))
ggsave("figures/net-latency.eps", p, height=3.5,width=3.5)

lj.ml2 <- melt(all, measure.vars=c('sJit', 'dJit'))
# Jitter
(p <- ggplot(lj.ml2, aes(value, color=variable, linetype=type)) +
  stat_ecdf() + theme_bw() +
  scale_x_log10(name='Network jitter (ms)', limits=c(0.01, 1000)) +
  scale_y_continuous(name='Cumulative portion of activities') +
  scale_colour_discrete(name='Direction', labels=c('Client', 'Server')) +
  scale_linetype_discrete(name='Devices', labels=c('Mob', 'NMob')) +
  theme(legend.position='none'))
ggsave("figures/net-jitter.eps", p, height=3.5,width=3.5)
