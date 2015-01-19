#!/usr/bin/R
# Read and clean data
AppCat <- read.csv("../data/app_mtc/AppCat.csv", sep='\t', head=T)
input='../data/app_mtc/appProtoStat.day.nmob.txt'

# Prepare data
ns <- c('time','proto','flw','pkt_K','byt_M', 'flw_r','pkt_r','byt_r')
app.dm <- read.csv(input, sep='\t',head=F)
colnames(app.dm) <- ns
app.dm$time <- as.POSIXlt(app.dm$time)
app.dm <- merge(app.dm, AppCat, by.x='proto', by.y='ID')
app.dm$strdate <- strftime(app.dm$time, format="%Y/%m/%d")
# filter out days with lost data
app.dm <- app.dm[-which(app.dm$strdate %in% c("2013/09/29","2013/07/16")),]
app.dm<- app.dm[order(app.dm$strdate), ]

# Aggregate by data+cat
ta <- aggregate(byt_r ~ strdate+Category, data=app.dm, FUN=sum)
ts <- ta[ta$Category %in% c("HTTP","InstantMessaging","Streaming","NetMan"),]

postscript('appDayStat-nmob.eps',width=8,height=4)
library(ggplot2)
p <- ggplot(data = ts, aes(x=strdate, y=byt_r, group=Category)) +
  # labels and title
  labs(title="", x="Time", y="Volumn ratio", colour="Category") +
  # draw lines
  geom_line(aes(colour=ts$Category)) +
  # ajust x scale ticks
  scale_x_discrete(breaks = ts$strdate[seq(1, length(ts$strdate), length.out=8)]) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0,size = 10),
        axis.title.x = element_text(face="bold",colour="#990000", size=20),
        axis.title.y = element_text(face="bold",colour="#990000", size=20),
        legend.title = element_text(size=15))
print(p)
dev.off()