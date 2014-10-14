# tools for slopes
#

#' slopeGraph
#' 
#' @param mp is the microplate
#' @param wellNr the wellNr you want the analysis on
#' @param nrOfTimePoints the width  default=10
#' 
#' @export
slopeGraph=function(mp,wellNr,nrOfTimePoints=10){
  index=getWellsMeasurementIndex(mp,wellNr)
  time=mp[index,"time"][,1]
  value=mp[index,"value"][,1]
  
  slopes=data.frame(matrix(0, (length(time)-nrOfTimePoints),3) )
  names(slopes)=c("m","b","r^2")
  selection=1:(length(time)-nrOfTimePoints)
  for(i in selection){
    start=i
    stop=i+nrOfTimePoints
    slopes[i,]=getSlope(mp,wellNr,start,stop,logY=T,plot=F)
  }
  # 2 plots in 1 graph
  # http://stackoverflow.com/questions/6142944/how-can-i-plot-with-2-different-y-axes-in-r
  # or: http://robjhyndman.com/hyndsight/r-graph-with-two-y-axes/
  # 
  plot(slopes[[1]],type="l",ylab="slope",ylim = c(min(slopes[[1]]),max(slopes[[1]])))
  abline(h=0)  
  par(new=TRUE)
  plot(slopes[[3]],type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="",ylim = c(min(slopes[[3]]),max(slopes[[3]])))
  axis(4)
  par(new=TRUE)
  plot(time,log(value), type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
  mtext("y2",side=4,line=3,col="red")
  legend("topright",col=c("blue","black","red"),lty=1,legend=c("graph","slope","r^2"))
  return(slopes)
}

#' getSlope
#' 
#' returns: c(m,b,r^2)
#' 
#' 
#' @param mp is the microplate
#' @param wellNr the wellNr you want the analysis on
#' @param start ...
#' @param stop ...
#' @param logY use the natural logarithm on the Y axis, default is true
#' @param plot a graph to show you the result default is true
#' 
#' @export
getSlope=function(mp,wellNr,start,stop,logY=TRUE,plot=TRUE){
  index=getWellsMeasurementIndex(mp,wellNr)
  time=mp[index,"time"][,1]
  value=mp[index,"value"][,1]
  
  if(logY){
    value=log(value) # note log=ln in R
  }
  
  selection=start:stop
  n=length(selection)
  
  x=time[selection]
  y=value[selection]
  
  
  # http://stats.stackexchange.com/questions/29525/slope-of-a-line-given-multiple-points
  # the math is described here... no idea how but it works :P
  
  # http://www.clemson.edu/ces/phoenix/tutorials/excel/regression.html
  
  
  xy=x*y 
  # y=mx+b
  m=(n*sum(xy)-sum(x)*sum(y)) / (n*sum(x^2)-sum(x)^2)
  b=(sum(y)-m*sum(x))/n
  r=(n*sum(xy)-sum(x)*sum(y))/sqrt((n*sum(x^2)-sum(x)^2)*(n*sum(y^2)-sum(y)^2))
  #   print(m)
  #   print(b)
  #   print(r^2)
  
  if(plot){
    startPointSlopeX=time[start]
    startPointSlopeY=value[start]
    
    plot(time,value)
    
    lines(time+startPointSlopeX,((time*m)+startPointSlopeY)) #slope
    
    abline(v=time[start])
    abline(v=time[stop])
  }
  
  return(c(m,b,r^2))
}

