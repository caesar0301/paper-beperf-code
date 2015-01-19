# Evalulate the performance of AEM algorithms
# By Xiaming Chen
library(ggplot2)

aem.act <- data.frame()
for(i in seq(1,8)){
  fname <- paste(c("../data/aem-evl/activity-level/perf-aem-c", i, ".out"), collapse='')
  print(fname)
  tmp <- read.csv(fname, header=F)
  aem.act <- rbind(aem.act, data.frame(gap=i, tmp))
}

aem.trc <- data.frame()
for(i in seq(1,8)){
  fname <- paste(c("../data/aem-evl/trace-level/perf-aem-c", i, ".out"), collapse='')
  print(fname)
  tmp <- read.csv(fname, header=F)
  aem.trc <- rbind(aem.trc, data.frame(gap=i, tmp))
}
aem <- rbind(data.frame(type=1, aem.act), data.frame(type=2, aem.trc))
aem$type <- as.factor(aem$type)
aem$gap <- as.factor(aem$gap)

# p <- ggplot(aem, aes(x=gap, y=V1, group=type, colour=type, shape=type)) + 
#   stat_summary(fun.y=median, geom="line",size=1)  + 
#   stat_summary(fun.y=median, geom="point",size=4) +
#   stat_summary(fun.y=mean, geom="line",size=1)  + 
#   stat_summary(fun.y=mean, geom="point",size=4) +
#   expand_limits(y=c(0.5,0.9))

postscript("eval.eps",width=4.5,height=5) # fix graph size
text.tick <- element_text(size = 14)
text.label <- element_text(colour="#990000", size=20)
p <- ggplot(aem.trc, aes(x=gap, y=V1)) + 
  stat_summary(fun.y=median, geom="line",size=1, colour="#009966")  + 
  stat_summary(fun.y=median, geom="point",size=4, colour="#009966", shape=17) +
  expand_limits(y=c(0.7,0.9)) +
  labs(x=expression(tau[L]), y="Detection Accuracy") +
  theme(axis.text.x = text.tick,
        axis.text.y = text.tick,
        axis.title.x = text.label,
        axis.title.y = text.label)
print(p)
dev.off()

ss <- read.csv("../data/aem-evl/perf_streamstructure.out", header=F)
cmp <- rbind(data.frame(type=1, val=ss$V1),
             data.frame(type=2, val=aem.trc$V1[which(aem.trc$gap==2)]))

postscript("aem-vs-ss.eps", width=4.5,height=5)
p1 <- ggplot(cmp, aes(x=type, y=val, group=type)) +
  geom_boxplot() + 
  geom_jitter(shape=1, position = position_jitter(width = .05)) +
  scale_x_discrete(limit = c("StreamStructure", "AID")) +
  labs(x="", y="Detection Accuracy") +
  theme(axis.text.x = text.label,
        axis.text.y = text.tick,
        axis.title.y = text.label)
print(p1)
dev.off()

print(p)
print(p1)
