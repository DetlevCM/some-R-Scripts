###
### A dual plot script for overlays, orginally first presented
### on my personal website
### https://www.detlevcm.eu/content/plotting-chromatograms-r
###

filename = "Dodecane-Neat"
filename2 = "Dodecane-Stressed"
PlotTitle = "Dodecane Neat Chromatogram - Pre and Post Test"

infilename = paste0("./",paste(filename,"csv",sep="."))
infilename2 = paste0("./",paste(filename2,"csv",sep="."))
outfilename = paste(filename,"pdf",sep="-rect.")

## Read Data -> File is called 14mielczarek11.csv
undiluted <- read.table(infilename, sep=";", header=TRUE, nrows=10000)
undiluted2 <- read.table(infilename2, sep=";", header=TRUE, nrows=10000)
## Work out Total Peak (want the Chromatogram, not the mass spec)
undiluted$peaks <- rowSums( undiluted[,3:404] )
undiluted2$peaks <- rowSums( undiluted2[,3:404] )

GCxx <- c(undiluted$RT.milliseconds./1000/60, rev(undiluted$RT.milliseconds./1000/60))
GCyy <- c(rep(0, nrow(undiluted)), rev(undiluted$peaks)/max(undiluted$peaks))
GCxx2 <- c(undiluted2$RT.milliseconds./1000/60, rev(undiluted2$RT.milliseconds./1000/60))
GCyy2 <- c(rep(0, nrow(undiluted2)), rev(undiluted2$peaks)/max(undiluted2$peaks))

library(survival)
library(Hmisc)

# A4 is 8.27 × 11.7 
pdf(outfilename, height=5, width=8)
par(mar=c(5,5,3,2))

plot("",
tck=-0.02,lab=c(10,10,4),las=1,
xaxs="i",xlab="Time (min)",
xlim=c(0,15),
yaxs="i",ylab="Detector Count Fraction",
ylim=c(0,0.15)
)

polygon(GCxx2+0.017, GCyy2, col= rgb(0,0,1,0.5), border=NA) #Blue - Shift the blue ever so slightly to properly overlap
polygon(GCxx, GCyy, col= rgb(1,1,1,1), border=NA) # White (so we don't get violet)
polygon(GCxx, GCyy, col= rgb(1,0,0,0.6), border=NA) # Red

lines(undiluted2$RT.milliseconds./1000/60+0.017,undiluted2$peaks/max(undiluted2$peaks),
type="l",col=4,lwd="0.05",lty=1) #Blue
lines(undiluted$RT.milliseconds./1000/60,undiluted$peaks/max(undiluted$peaks),
type="l",col=2,lwd="0.05",lty=1) # Red

title(PlotTitle)
minor.tick(nx=5, ny=5, tick.ratio=0.3)

dev.off()
