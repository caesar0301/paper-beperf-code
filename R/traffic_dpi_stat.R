dpi <- read.table("../data/dpi/dpi.statistics.txt", sep='\t', 
                  col.names=c("time", "proto", "flw", "pkt","byt"))
dpi$flw <- dpi$flw/(10^3) # K Flow
dpi$pkt <- dpi$pkt/(10^6) # M pkt
dpi$byt <- dpi$byt/(10^6) # M byt

parts <- strsplit(as.character(dpi$proto), ':')
dpi$proto <- NULL
dpi$l4 <- as.character(lapply(parts, FUN=function(x) x[1]))
dpi$l7 <- as.character(lapply(parts, FUN=function(x) x[2]))

ts.l4 <- aggregate(cbind(flw, pkt, byt)~l4+time, data=dpi, sum)
stat.l4 <- aggregate(cbind(flw, pkt, byt)~l4, data=ts.l4, sum)

# sort data
stat.l4.ord <- stat.l4[order(stat.l4$byt,decreasing=T),]
stat.l4.ord[which(stat.l4.ord$l4=='TCP'),]$l4 <- 6
stat.l4.ord[which(stat.l4.ord$l4=='UDP'),]$l4 <- 17
stat.l4.ord[which(stat.l4.ord$l4=='VRRP'),]$l4 <- 112
stat.l4.ord[which(stat.l4.ord$l4=='ICMP'),]$l4 <- 1
stat.l4.ord$l4 <- as.numeric(stat.l4.ord$l4)

# clear uninterested data
stat.l4.ord <- stat.l4.ord[-which(stat.l4.ord$flw<1), ]
stat.l4.ord <- stat.l4.ord[-which(stat.l4.ord$l4>=143), ]
# transport layer
# 33 DCCP,
# 40 IL,
# 27 RDP,
# 132 SCTP,
# 6 TCP,
# 17 UDP,
# 136 UDP Lite
stat.l4.ord <- stat.l4.ord[which(stat.l4.ord$l4 
                                 %in% c(33,40,27,132,6,17,136)), ]
rownames(stat.l4.ord) <- NULL
tm <- t(as.matrix(stat.l4.ord))
barplot(tm[4:dim(tm)[1],], beside=T)
barplot(tm[4,], beside=T)
