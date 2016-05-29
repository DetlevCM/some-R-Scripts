

### Because comprable chromatograms are plotted under similar conditions, 
### define a function for the actual plot
### The plotting function is built around the csv output from OpenChrom
### https://www.openchrom.net/

PlotChromatogram <- function(filename,plot_height,plot_width,xmin,xmax,xdiv,PlotTitle){
xsubdiv <- 10 # subticks, don't want to overload the function too much

infilename = paste0("./",paste(filename,"csv",sep="."))
outfilename = paste(filename,"pdf",sep=".")

### Read Data -> File is called "something.csv"
undiluted <- read.table(infilename, sep=";", header=TRUE)
### Work out Total Peak (want the Chromatogram, not the mass spec)
undiluted$peaks <- rowSums( undiluted[,3:404] )

### For drawing filled plots :)
### http://earlh.com/blog/2009/07/28/filled-line-plots-graphs-in-r-part-10-in-a-series/
GCxx <- c(undiluted$RT.milliseconds./1000/60, rev(undiluted$RT.milliseconds./1000/60))
GCyy <- c(rep(0, nrow(undiluted)), rev(undiluted$peaks)/max(undiluted$peaks))


### Sizes for pdf plots in R are in inches
### A4 is 8.27 × 11.7 
pdf(outfilename, height=plot_height, width=plot_width)


par(mar=c(5,5,3,2))
plot(undiluted$RT.milliseconds./1000/60,undiluted$peaks/max(undiluted$peaks),
type="l",col=2,lwd="0.5",lty=1,
tck=-0.02,lab=c(10,10,4),las=1,
xaxs="i",xlab='',xaxt='n',
xlim=c(xmin,xmax),
yaxs="i",ylab='',yaxt='n',
ylim=c(0,1.1) # it is a fraction, so hard limits
)

polygon(GCxx, GCyy, col= rgb(1,0,0,0.5), border=NA)



### x-axis
mtext("Time (min)", side=1, line=2)
axis(1, at=seq(xmin, xmax, by = ((xmax-xmin)/xdiv)),tck=-0.02, las=0)
axis(1, at=seq(xmin, xmax, by = ((xmax-xmin)/(xsubdiv*xdiv))), labels=FALSE, tck=-0.01, las=0)

### y-axis
mtext("Detector Count Fraction", side=2, line=3, las=0)
axis(2, at=seq(0, 1, by = 0.1), tck=-0.02, las=2)
axis(side = 2, at = seq(0, 1, by = 0.02), labels = FALSE, tck = -0.01) 

### Title of Plot
title(PlotTitle)


dev.off()
}


PlotChromatogram("Dodecane-Neat",5,6,2,12,5,"Undiluted Dodecane GC Data")

