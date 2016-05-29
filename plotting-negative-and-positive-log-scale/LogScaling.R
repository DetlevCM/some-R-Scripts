 
### Log10max and Log10min define the size of the plot
### RatesOfProduction is the vector of Production Rates

ScaleUsingLog <- function(PosMin,NegMin,X,Values)
{
  logpos <- NULL
  logneg <- NULL

  ### zero is used to signify an empty space as log(n) is never zero
  for (i in 1:length(Values)){
	 if(Values[i] > 0)# && !is.na(Values[i])) ### is positive
	 {
		tmp <- log10(Values[i])
		if(tmp < PosMin)
		{
		tmp <- PosMin
		}

		logpos <- c(logpos,tmp)
		logneg <- c(logneg,NegMin)
	 }
	 if(Values[i] < 0)# && !is.na(Values[i])) ### is negative
	 {
		tmp <- log10(-Values[i])
		if(tmp < NegMin)
		{
		tmp <- NegMin
		}

		logneg <- c(logneg,tmp)
		logpos <- c(logpos,PosMin)
	 }
	 if(Values[i] == 0)# || is.na(Values[i])) ### neither positive nor negtaive or when na
	 ### so we set it to the minimum value for either
	 {
		logneg <- c(logneg,NegMin)
		logpos <- c(logpos,PosMin)
	 }
  }

  ### Scale to Plot Range - values range from ProdMax to PosMin, ConsMax to NegMin
  scaledpos <- logpos - PosMin
  scaledneg <- - (logneg - NegMin)

  ### now merge the two:
  Y <- NULL
  for (i in 1:length(scaledpos)){
	 Y <- c(Y,scaledpos[i]+scaledneg[i])
  }

  OutFrame <- data.frame(X,Y)
  return(OutFrame)
}
