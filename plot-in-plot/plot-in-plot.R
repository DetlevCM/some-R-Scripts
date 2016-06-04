
### Reading in the data to be plotted
PlotData <- read.table("./PlotData.csv", header=TRUE, sep=",", dec=".")

# Convert Angström to nm:  x 0.1
DistConv=0.1
# Convert Energy from Hartree to kJ/mol: x 2625.5
EnerConv=2625.5

### Setting plot limits
xmin <- 0.1
xmax <- 0.8
ymin <- -600
ymax <- 0
xdiv <- 7
ydiv <- 6

### plot to a pdf - even preffered if the final output should be a PNG
### as the scaling is consistent across different sizes
### the pdf can be converted to other formats or used as is (e.g. with LaTeX)
pdf("Plot.pdf", height=4, width=5)

### plots are arranged bottom, left, top, right
par(mar=c(1,5,4,5))
plot(0, # starting with a random "off-plot" point to have a clean layout and an empty plot
xaxs="i",xlab="",xaxt='n',
xlim=c(xmin,xmax),
yaxs="i",ylab="",yaxt='n',
ylim=c(ymin,ymax+25), # moved ymax to +25 to improve points visibiloty
)

### Some old references that I read, no idea if still current...
# http://stackoverflow.com/questions/12302366/moving-axes-labels-in-r
# http://stackoverflow.com/questions/11775692/how-to-specify-the-actual-x-axis-values-to-plot-as-x-axis-ticks-in-r


### x-axis 
xsubdiv <- 2*xdiv # just to make it clearer how its set
mtext("x-axis", side=3, line=2)
axis(3, at=seq(from=xmin, to=xmax, by=((xmax-xmin)/xdiv)), las=0)
axis(3, at=seq(from=xmin, to=xmax, by=((xmax-xmin)/xsubdiv)), labels = FALSE, tcl = -0.2) 

### y-axis
ysubdiv <- 4*ydiv # just to make it clearer how its set
mtext("y-axis", side=2, line=3, las=0)
axis(2, at=seq(from=ymin, to=ymax, by=((ymax-ymin)/ydiv)), las=2)
axis(2, at=seq(from=ymin, to=ymax, by=((ymax-ymin)/ysubdiv)), labels = FALSE, tcl = -0.2) 


### to plot points a little bit smaller
PointsScale=0.75

points(PlotData$r1*DistConv,(PlotData$E1-max(PlotData$E1, na.rm=TRUE))*EnerConv,pch=1,
 cex=PointsScale, col=1)
points(PlotData$r2*DistConv,(PlotData$E2-max(PlotData$E2, na.rm=TRUE))*EnerConv,pch=2,
 cex=PointsScale, col=2)

### The legend
 par(xpd=TRUE)
legend(0.82,50,
title="legend",
bty='n',
c("data 1","data 2"),
pch=c(1,2),
col=c(1,2)
)

### Creating a second plot in a subsection of the plot area
### plots are arranged bottom, left, top, right
### in this case, bottom and right line up - i.e.
### mar=c(1,x,y,5)
par(xpd=FALSE)
par(new=TRUE,mar=c(1,12,11,5))
plot(
0,
type="l",col=2,lwd="0.05",lty=1,
tck=-0.02,lab=c(10,10,4),las=1,
xlim=c(0.149,0.185),xlab='',xaxt='n',
ylim=c(-575,-500),ylab='',yaxt='n'
#,ann=FALSE # annotation false works as well as an empty label
)

### Some more old references
# http://www.statmethods.net/advgraphs/axes.html
# Some more help: http://stackoverflow.com/questions/3785089/r-change-the-spacing-of-tick-marks-on-the-axis-of-a-plot
# https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/axis.html
# x-axis, set up manually
axis(3, at=c(0.15,0.16,0.17,0.18,0.19), labels=c(0.15,0.16,0.17,0.18,0.19), las=0)
# y-axis, set up manually
axis(2, at=c(-550,-525,-500,-475,-450), las=2)

### and just plotting the data again.
### because we set par(xpd=FALSE) after the legend, the line stops at the border
points(PlotData$r1*DistConv,(PlotData$E1-max(PlotData$E1, na.rm=TRUE))*EnerConv,pch=1, 
cex=PointsScale, col=1)
points(PlotData$r2*DistConv,(PlotData$E2-max(PlotData$E2, na.rm=TRUE))*EnerConv,pch=2, 
cex=PointsScale, col=2)

### being tidy, closing the output device (essential when drawing to files)
dev.off()




