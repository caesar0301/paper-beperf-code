#!/usr/bin/R
library(ggplot2)

# Read and clean data
AppCat <- read.csv("../data/app-mtc/AppCat.csv", sep='\t', head=T)
input='../data/app-mtc/appProtoStat.day.mob.txt'

# Prepare data
ns <- c('time','proto','flw','pkt_K','byt_M', 'flw_r','pkt_r','byt_r')
app.dm <- read.csv(input, sep='\t',head=F)
colnames(app.dm) <- ns
app.dm$time <- as.POSIXlt(app.dm$time)
app.dm <- merge(app.dm, AppCat, by.x='proto', by.y='ID')
app.dm$strdate <- strftime(app.dm$time, format="%y/%m/%d")
# filter out days with lost data
app.dm <- app.dm[-which(app.dm$strdate %in% c("13/09/29","13/07/16")),]
app.dm<- app.dm[order(app.dm$strdate), ]

# Aggregate by data+cat
ta <- aggregate(byt_r ~ strdate+Category, data=app.dm, FUN=sum)
ts <- ta[ta$Category %in% c("HTTP","InstantMessaging","Streaming","NetMan"),]

(p <- ggplot(ts, aes(x=strdate, y=byt_r, group=Category)) +
  labs(title="", x="Time", y="Volumn ratio", colour="Category") +
  geom_line(aes(colour=ts$Category)) +
  theme(legend.position=c(0.8, 0.5)) +
  scale_x_discrete(breaks = ts$strdate[seq(1, length(ts$strdate), length.out=8)]) +
  theme(
        axis.title.x = element_text(face="bold",colour="#990000", size=20),
        axis.title.y = element_text(face="bold",colour="#990000", size=20),
        legend.title = element_text(size=15)))
ggsave('figures/app-day-stat-mob.eps', p, width=6,height=4)