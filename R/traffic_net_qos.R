#!/usr/bin/R
# Plot the distributions of network qos metrics of mobile/nonmobile flows
library(Hmisc)

qos.mob <- read.delim("../data/net-qos/qos_met.mob.txt",sep='\t')
qos.nmob <- read.delim("../data/net-qos/qos_met.nmob.txt", sep='\t')
fname <- c('flw_dur', 'flw_vdur','rx_byt','rx_pkt','oos_pkt',
           'src_byt','dst_byt','src_pkt','dst_pkt',
           'src_rtt_var','dst_rtt_var','src_rtt_avg','dst_rtt_avg')
colnames(qos.mob) <- fname
colnames(qos.nmob) <- fname

sample.size = 10000
options("scipen"=100)

# sec
# A4 plot template with justified params
pdf("figures/qos-flow-dur.pdf",height=6,width=8) # figure size in inches
par(mar=c(5,5,1,1), mgp=c(3,0.8,0)) # change device params
Ecdf(sample(qos.mob$flw_dur, sample.size),col=1,lty=1,lwd=2,log='x',subtitles=F, 
     xlab="",ylab="",xaxt='n', yaxt='n') # disable axies and lables
Ecdf(sample(qos.nmob$flw_dur, sample.size),col=1,lty=2,lwd=2,add=T,subtitles=F)
Ecdf(sample(qos.mob$flw_vdur, sample.size),col=2,lty=1,lwd=2,add=T,subtitles=F)
Ecdf(sample(qos.nmob$flw_vdur, sample.size),col=2,lty=2,lwd=2,add=T,subtitles=F)
# make lables
title(xlab="Flow duration (sec)",ylab="Distribution of flows",cex.lab=1.5)
# make axies
axis(1,tcl=-.2,cex.axis=1.2)
axis(2,tcl=-.2,cex.axis=1.2,las=2)
# make legend
legend('bottomright',lty=c(1,2,1,2),col=c(1,1,2,2),pt.cex = 1,cex=1.5,
       legend=c("Overall (mob)","Overall (nmob)","Data (mob)","Data (nmob)"))
grid()
dev.off()


# KB
pdf("figures/qos-flow-size.pdf",height=6,width=8)
par(mar=c(5,5,1,1), mgp=c(3,0.8,0)) # change device params
Ecdf(sample(qos.mob$src_byt/1024, sample.size),col=1,lty=1,lwd=2,log='x',subtitles=F, 
    xlim=c(0.01, 5000),xlab="",ylab="",xaxt='n', yaxt='n')
Ecdf(sample(qos.nmob$src_byt/1024, sample.size),col=1,lty=2,lwd=2,add=T,subtitles=F)
Ecdf(sample(qos.mob$dst_byt/1024, sample.size),col=2,lty=1,lwd=2,add=T,subtitles=F)
Ecdf(sample(qos.nmob$dst_byt/1024, sample.size),col=2,lty=2,lwd=2,add=T,subtitles=F)
# make lables
title(xlab="Flow size (KB)",ylab="Flow CDF",cex.lab=1.5)
# make axies
axis(1,tcl=-.2,cex.axis=1.2)
axis(2,tcl=-.2,cex.axis=1.2,las=2)
legend('bottomright',lty=c(1,2,1,2),col=c(1,1,2,2),pt.cex = 1,cex=1.5,
      legend=c("Uplink (mob)","Uplink (nmob)","Downlink (mob)","Downlink (nmob)"));
grid()
dev.off()


# packet
pdf("figures/qos-flow-pkt.pdf",height=6,width=8)
par(mar=c(5,5,1,1), mgp=c(3,0.8,0)) # change device params
Ecdf(sample(qos.mob$src_pkt, sample.size), col=1, lty=1,lwd=2,log='x',xlim=c(1, 1500),
     xlab="",ylab="",xaxt='n', yaxt='n',subtitles=F)
Ecdf(sample(qos.nmob$src_pkt, sample.size), col=1, lty=2, lwd=2,add=T,subtitles=F)
Ecdf(sample(qos.mob$dst_pkt, sample.size), col=2, lty=1, lwd=2,add=T,subtitles=F)
Ecdf(sample(qos.nmob$dst_pkt, sample.size), col=2, lty=2, lwd=2,add=T,subtitles=F)
# make lables
title(xlab="#Packets/flow",ylab="Flow CDF",cex.lab=1.5)
# make axies
axis(1,tcl=-.2,cex.axis=1.2)
axis(2,tcl=-.2,cex.axis=1.2,las=2)
legend('bottomright',pt.cex = 1,cex=1.5,lty=c(1,2,1,2),col=c(1,1,2,2),
       legend=c("Uplink (mob)","Uplink (nmob)","Downlink (mob)","Downlink (nmob)"));
grid()
dev.off()


# byte
pdf("figures/qos-flow-brate.pdf",height=6,width=8)
par(mar=c(5,5,1,1), mgp=c(3,0.8,0)) # change device params
sbw <- qos.mob$src_byt/1024/qos.mob$flw_dur # source bw
vsbw <- qos.mob$src_byt/1024/qos.mob$flw_vdur # valid source bw
dbw <- qos.mob$dst_byt/1024/qos.mob$flw_dur # dest bw
vdbw <- qos.mob$dst_byt/1024/qos.mob$flw_vdur # valid dest bw
Ecdf(sample(sbw, sample.size),col=1,lty=1,lwd=2,log='x',xlim=c(0.001, 1000),subtitles=F,xlab="",ylab="",xaxt='n', yaxt='n')
Ecdf(sample(vsbw, sample.size),col=1,lty=2,lwd=2,add=T,subtitles=F)
Ecdf(sample(dbw, sample.size),col=2,lty=1,lwd=2,add=T,subtitles=F)
Ecdf(sample(vdbw, sample.size),col=2,lty=2,lwd=2,add=T,subtitles=F)
# make lables
title(xlab="Flow byte rate (KB/s)",ylab="flow CDF",cex.lab=1.5)
# make axies
axis(1,tcl=-.2,cex.axis=1.2)
axis(2,tcl=-.2,cex.axis=1.2,las=2)
legend('topleft',pt.cex = 1,cex=1.5,lty=c(1,2,1,2),col=c(1,1,2,2),
       legend=c("Up-overall","Up-efficient","Down-overall","Down-efficient"));
grid()
dev.off()


#packet
pdf("figures/qos-flow-prate.pdf",height=6,width=8)
par(mar=c(5,5,1,1), mgp=c(3,0.8,0)) # change device params
Ecdf(sample(qos.mob$src_pkt/qos.mob$flw_dur, sample.size),col=1,lty=1,lwd=2,subtitles=F,xlab="",ylab="",xaxt='n', yaxt='n')
Ecdf(sample(qos.mob$src_pkt/qos.mob$flw_vdur, sample.size), col=1, lty=2,lwd=2,add=T,subtitles=F)
Ecdf(sample(qos.mob$dst_pkt/qos.mob$flw_dur, sample.size), col=2, lty=1,lwd=2,add=T,subtitles=F)
Ecdf(sample(qos.mob$dst_pkt/qos.mob$flw_vdur, sample.size), col=2, lty=2,lwd=2,add=T,subtitles=F)
# make lables
title(xlab="Transfered #pkts/sec)",ylab="Flow CDF",cex.lab=1.5)
# make axies
axis(1,tcl=-.2,cex.axis=1.2)
axis(2,tcl=-.2,cex.axis=1.2,las=2)
legend('bottomright',pt.cex = 1,cex=1.5,lty=c(1,2,1,2),col=c(1,1,2,2),
       legend=c("Up-overall","Up-efficient","Down-overall","Down-efficient"));
grid()
dev.off()


# ms
pdf("figures/qos-rtt-avg.pdf",height=6,width=8)
par(mar=c(5,5,1,1), mgp=c(3,0.8,0)) # change device params
Ecdf(sample(qos.mob$src_rtt_avg*1000, sample.size),col=1,lty=1,lwd=2,subtitles=F,
     log='x',xlim=c(1, 10e3),xlab="",ylab="",xaxt='n', yaxt='n')
Ecdf(sample(qos.nmob$src_rtt_avg*1000, sample.size), col=1, lty=2, lwd=2,add=T,subtitles=F)
Ecdf(sample(qos.mob$dst_rtt_avg*1000, sample.size), col=2, lty=1, lwd=2,add=T,subtitles=F)
Ecdf(sample(qos.nmob$dst_rtt_avg*1000, sample.size), col=2, lty=2, lwd=2,add=T,subtitles=F)
# make lables
title(xlab="Latency (ms)",ylab="Flow CDF",cex.lab=1.5)
# make axies
axis(1,tcl=-.2,cex.axis=1.2)
axis(2,tcl=-.2,cex.axis=1.2,las=2)
legend('bottomright',pt.cex = 1,cex=1.5,lty=c(1,2,1,2),col=c(1,1,2,2),
       legend=c('Client (mob)','Client (nmob)','Server (mob)','Server (nmob)'));
grid()
dev.off()


# ms
pdf("figures/qos-rtt-var.pdf",height=6,width=8)
par(mar=c(5,5,1,1), mgp=c(3,0.8,0)) # change device params
Ecdf(sample(qos.mob$src_rtt_var*1000, sample.size),col=1,lty=1,lwd=2,subtitles=F,
     log='x',xlim=c(1e-3, 10e3),xlab="",ylab="",xaxt='n', yaxt='n')
Ecdf(sample(qos.nmob$src_rtt_var*1000, sample.size), col=1, lty=2, lwd=2,add=T,subtitles=F)
Ecdf(sample(qos.mob$dst_rtt_var*1000, sample.size), col=2, lty=1, lwd=2,add=T,subtitles=F)
Ecdf(sample(qos.nmob$dst_rtt_var*1000, sample.size), col=2, lty=2, lwd=2,add=T,subtitles=F)
# make lables
title(xlab="Jitter (ms)",ylab="Flow CDF",cex.lab=1.5)
# make axies
axis(1,tcl=-.2,cex.axis=1.2)
axis(2,tcl=-.2,cex.axis=1.2,las=2)
legend('bottomright',pt.cex = 1,cex=1.5,lty=c(1,2,1,2),col=c(1,1,2,2),
       legend=c('Client (mob)','Client (nmob)','Server (mob)','Server (nmob)'));
grid()
dev.off()

# rx 
mob.rx.pkt <- qos.mob$rx_pkt[which(qos.mob$rx_pkt>0)]
cat("Mobile rx flow % ", length(mob.rx.pkt)/length(qos.mob$rx_pkt), '\n')
cat('Mobile mean rx pkts ', mean(mob.rx.pkt), '\n')
mob.rx.byt <- qos.mob$rx_byt[which(qos.mob$rx_byt>0)]
cat('Mobile mean rx bytes ', mean(mob.rx.byt), '\n')

nmob.rx.pkt <- qos.nmob$rx_pkt[which(qos.nmob$rx_pkt>0)]
cat("NonMobile rx flow % ", length(nmob.rx.pkt)/length(qos.nmob$rx_pkt), '\n')
cat('NonMobile mean rx pkts ', mean(nmob.rx.pkt), '\n')
nmob.rx.byt <- qos.nmob$rx_byt[which(qos.nmob$rx_byt>0)]
cat('NonMobile mean rx bytes ', mean(nmob.rx.byt), '\n')

# oos
mob.oos.pkt <- qos.mob$oos_pkt[which(qos.mob$oos_pkt>0)]
cat("Mobile oos flow % ", length(mob.oos.pkt)/length(qos.mob$oos_pkt), '\n')
cat("Mobile mean oos pkts ", mean(mob.oos.pkt), '\n')

nmob.oos.pkt <- qos.nmob$oos_pkt[which(qos.nmob$oos_pkt>0)]
cat("NonMobile oos flow % ", length(nmob.oos.pkt)/length(qos.nmob$oos_pkt), '\n')
cat("NonMobile mean oos pkts ", mean(nmob.oos.pkt), '\n')