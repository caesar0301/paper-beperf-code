# Read data
input_folder <- "../data/omniperf/qsw-2013-10-25-09-27-13/"
clicks <- read.csv(paste(input_folder, "userclicks2.out", sep=''),sep='\t')
# Note: time unit is second for userclick2.out.
#clicks$time <- clicks$time/1000
pkts <- read.csv(paste(input_folder, "packet_times.out", sep=''), sep='\t')
flws <- read.csv(paste(input_folder, "flow_times.out", sep=''), sep='\t')

timeMod <- function(t){
  t1 <- floor(t*10)/10.0
  return(t1)
} 

# Adjust times
mtime <- min(c(clicks$time, flws$time, pkts$time))
print(mtime)
clicks$time <- timeMod(clicks$time-mtime)
flws$time <- timeMod(flws$time-mtime)
pkts$time <- timeMod(pkts$time-mtime)

#layout(matrix(c(1, 2, 3, 4), 2, byrow=TRUE), c(1, 1), c(1, 1))

# plot user interaction with phone
#readline("User interaction with phone screen:\n")
# plot(clicks$screen_x, clicks$screen_y, xlim=c(0,480), ylim=c(0,800), 
#      xlab="Screen X", ylab="Screen Y", main="Screen Interaction Trace")
# lines(x=clicks$screen_x, y=clicks$screen_y)
# library(MASS)
# den3d <- kde2d(clicks$screen_x, clicks$screen_y)
# persp(den3d)


# Plot user interaction and network traffic
postscript("InteractionTrace.eps",height=6,width=8)
par(mar=c(5,1,1,1), mgp=c(3,0.8,0)) # change device params
xmx <- min(c(max(clicks$time), max(pkts$time), max(flws$time)))
ymx <- 8
plot(c(0, xmx), c(0,ymx),ann=F,bty='n',type='n',xaxt='n',yaxt='n')
title(main="", xlab="Elapsed Time (seconds)",cex.lab=1.5)
axis(1,tcl=-.2,cex.axis=1.2)
# packet bursts
text(0,0.3, adj=0, col="black","Red: packet bursts",cex=1.5)
for(i in 1:length(pkts$time))
{
  t <- pkts$time[i]
  if(pkts$is_burst_500[i] == 1)
    rect(t, 0.5, t+0.2, 1.5, col = "red", border = NA)
  else
    rect(t, 0.5, t+0.2, 1.5, col = "grey", border = NA)   
}
# flow bursts
text(0,2.3,adj=0,col="black","Blue: flow gen",cex=1.5)
for(i in 1:length(flws$time))
{
  t <- flws$time[i]
  rect(t,2.5,t+2,3.5, col = "blue", border = NA) 
}
# user clicks
text(0,4.3,adj=0,col="black","Green: user clicks",cex=1.5)
for(i in 1:length(clicks$time))
{
  t <- clicks$time[i]
  rect(t,4.5,t+0.5,5.5, col = "green", border = NA) 
}
# user valid clicks generating network traffic
val_clicks <- numeric(0)
for ( i in 1:length(flws$time)){
  t1 <- flws$time[i]
  delta <- t1 - clicks$time
  match <- which(delta >= 0 & delta <= 1)
  if ( length(match) > 0 ){
    nr <- match[length(match)]
    val_clicks <- c(val_clicks, clicks$time[nr])
  }
}
text(0,6.3,adj=0,col="black","Black: user clicks generating data",cex=1.5)
for(i in 1:length(val_clicks))
{
  t <- val_clicks[i]
  rect(t,6.5,t+1,7.5, col = "black", border = NA) 
}
dev.off()

#delta <- val_clicks - c(0, val_clicks)
#plot(ecdf(delta[1:length(delta)-1]))


# Plot user interaction and network footprint density
postscript("InteractionDesity.eps",height=6,width=8)
par(mar=c(5,5,1,1), mgp=c(3,0.8,0)) # change device params
plot.multi.dens <- function(s)
{
  junk.x = NULL
  junk.y = NULL
  for(i in 1:length(s))
  {
    junk.x = c(junk.x, density(s[[i]])$x)
    junk.y = c(junk.y, density(s[[i]])$y)
  }
  xr <- range(junk.x)
  yr <- range(junk.y)
  plot(density(s[[1]]), xlim = xr, ylim = yr, main = "", xlab='', ylab='',xaxt='n', yaxt='n')
  for(i in 1:length(s))
  {
    lines(density(s[[i]]), xlim = xr, ylim = yr, col = i)
  }
  grid()
}
plot.multi.dens(list(flws$time, pkts$time, clicks$time))
title(main="", xlab="Elapsed Time (seconds)", ylab="Footprint Density",cex.lab=1.5)
axis(1,tcl=-.2,cex.axis=1.2)
axis(2,tcl=-.2,cex.axis=1.2)
legend("topleft", legend=c("Flow","Packet","User click"), col=(1:3), lwd=2, lty = 1)
dev.off()


# Plot flow and packet emission time
#readline("Flow and packet emission:\n")
postscript("FlowEmission.eps",height=6,width=8)
plot(flws$time, xlab="Flow Index", ylab="Flow Emission Time (seconds)", pch=20, ylim=c(0, xmx))
grid()
dev.off()


postscript("PacketEmission.eps",height=6,width=8)
plot(pkts$time, xlab="Packet Index", ylab="Packet Emission Time (seconds)", pch=20, ylim=c(0,xmx))
grid()
dev.off()
