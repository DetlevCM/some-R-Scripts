

source("./LogScaling.R")

### Parameters to set up the plot
xmin <- -5
xmax <- 5
ymin <- -5
ymax <- 5
xdiv <- 10
ydiv <- 10

pdf("sample.pdf",6,6)
par(mar=c(5,5,3,2))
plot(10^10,
type="l",col=2,lwd="0.5",lty=1,
tck=-0.02,lab=c(10,10,4),las=1,
xaxs="i",xlab='',xaxt='n',
xlim=c(xmin,xmax),
yaxs="i",ylab='',yaxt='n',
ylim=c(ymin,ymax) # it is a fraction, so hard limits
)

### Example Data, a simple cubic function divided by 1000 so that the range of values is -1,0 and 0,1
testdata <- ScaleUsingLog(ymin,-ymax,seq(-5,5,by = 0.1),(seq(-5,5,by = 0.1))^(3)/(10^3))
lines(testdata$X,testdata$Y,lty=1,lwd=1)


### Time to draw the axes and label them correctly
axisscale <- 0.75 # just making things a bit smaller

# some leftovers...
xaxmax <- xmax
xaxmin <- xmin

### create the y axis labels - this one writes exponentials
ylabelsP <- NULL
ylabelsN <- NULL
ylabelsN <- c(ylabelsN,expression(10^0))
for(i in 1:(ydiv-1))
{
  tmp <- (ymin/ydiv)*(i)
  ylabelsN<- c(ylabelsN,(bquote(10^ .(tmp))))
}
ylabelsN <- c(ylabelsN," ")
ylabelsP <- c(ylabelsP,expression(" "))
for(i in 1:ydiv)
{
  tmp <- (ymin/ydiv)*(ydiv-i)
  ylabelsP <- c(ylabelsP,(bquote(10^ .(tmp))))
}

xlabels <- NULL
xlabels <- c(xlabels," ")
for(i in 1:(xdiv))
{
  xlabels <- c(xlabels,(xaxmin+i*(xaxmax-xaxmin)/xdiv))
}

### y-axis
mtext("y-value", side=2, line=3)
axis(2, at=seq(from = 0, to = ymax, by = ((ymax-0)/(ydiv))), 
labels=ylabelsP, las=2, tck=-0.02, col.axis=3)  
axis(2, at=seq(from = ymin, to = 0, by = ((0-ymin)/(ydiv))), 
labels=ylabelsN, las=2, tck=-0.02, col.axis=2)  

### x-axis
mtext("x-values", side=1, line=2.25)
axis(1, at=seq(from = xaxmin, to = xaxmax, by = ((xaxmax-xaxmin)/xdiv)), 
labels=FALSE, pos=0, las=0, tck=-0.01) 
axis(3, at=seq(from = xaxmin, to = xaxmax, by = ((xaxmax-xaxmin)/xdiv)), 
labels=FALSE, pos=0, las=0, tck=-0.01) 
axis(1, at=seq(from = xaxmin, to = xaxmax, by = ((xaxmax-xaxmin)/xdiv)), 
labels=xlabels, pos=ymin, las=0, tck=-0.02, mgp=c(3, .5, 0), cex.axis=axisscale) 
axis(3, at=seq(from = xaxmin, to = xaxmax, by = ((xaxmax-xaxmin)/xdiv)), 
labels=xlabels, pos=ymax, las=0, tck=-0.02, mgp=c(3, .5, 0), cex.axis=axisscale) 

### title can be added here

