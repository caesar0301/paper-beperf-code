#!/usr/bin/R
library(plotrix)

# Read and clean data
AppCat <- read.csv('../data/app_mtc/AppCat.csv', sep='\t')
app.am <- read.csv("../data/app_mtc/appProtoStat.all.nmob.txt", sep='\t', head=F)
colnames(app.am) <- c('proto','flw','pkt_K','byt_M','flw_r','pkt_r','byt_r')
app.am <- merge(app.am, AppCat, by.x='proto', by.y='ID')
# Shorten names
levels(app.am$Category) <- c(levels(app.am$Category), "IM") # increase level
app.am$Category[app.am$Category=="InstantMessaging"] <- "IM"

# flows
postscript("figures/app-proto-flow-nmob.eps",height=6,width=8) # set graph size
ta <- aggregate(flw_r ~ Category, data=app.am, FUN=sum)
ta[which(ta$flw_r<0.01),]$Category <- "Other"
ta <- aggregate(flw_r ~ Category, data=ta, FUN=sum)
ta <- ta[order(ta$flw_r,decreasing=T),]

slices <- ta$flw_r
lbls <- ta$Category
pct <- round(slices*1000)/10.0
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels 
#pie(slices,label=lbls, main="Application Breakdown (flows)")
pd <- pie3D(slices,labels=lbls,
      radius=0.8,theta=pi/4,shade=0.5,
      mar=c(0,2,0,4),labelcex=1.5)
dev.off()


# bytes
postscript("figures/app-protol-byte-nmob.eps",height=6,width=8)
ta <- aggregate(byt_r ~ Category, data=app.am, FUN=sum)
ta[which(ta$byt_r<0.01),]$Category = "Other"
ta <- aggregate(byt_r ~ Category, data=ta, FUN=sum)
ta <- ta[order(ta$byt_r,decreasing=T),]

slices <- ta$byt_r
lbls <- ta$Category
pct <- round(slices*1000)/10.0
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels 
#pie(slices,label=lbls, main="Application Breakdown (bytes)")
pie3D(slices,labels=lbls,
      radius=0.8,theta=pi/4,shade=0.5,
      mar=c(0,0,0,4),labelcex=1.5)
dev.off()