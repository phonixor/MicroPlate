# tools for slopes
#
#


#' getGrowthRate
#' 
#' @param values - y
#' @param time - x
#' @param logValues - boolean - takes the log of the values, default = TRUE... uses original values to determine slope location!!!
#' @param nrOfTimePointsForSlope - nr or fraction or string ending with the percent sybol (not allowed in docs???? so cant give example :P)
#' @param plot - boolean if it needs to plot the results default = TRUE
#' @param plotTitle - title of the plot... useful to identify
#' 
#' todo: decide if i do anything with r^2
#' 
#' 
#' @export
getGrowthRate=function(values,time,logValues=T,nrOfTimePointsForSlope="10%",plot=T,plotTitle="growthRate"){
  if(length(time)!=length(values))stop("lenght time and values do not match")
  originalValues=values
  originalNrOfTimePointsForSlope=nrOfTimePointsForSlope
  if(logValues){values=log(values)}  
  if(any(is.infinite(values)))stop("Inf, not allowed, maybe due to log(0)?")
  if(any(is.nan(values)))stop("NaN, not allowed")
  if(any(is.na(values)))stop("NA, not allowed")
  nrOFTimePoints=length(time)
  
  
  # nrOfTimePointsForSlope
  # either whole thing is a number or % based
  # xxx%, xxx, xx,x
  if(is.character(nrOfTimePointsForSlope)){
    # string
    if(substring(nrOfTimePointsForSlope,first=nchar(nrOfTimePointsForSlope),last=nchar(nrOfTimePointsForSlope))=="%"){
      # xxx%
      nrOfTimePointsForSlope=substring(nrOfTimePointsForSlope,1,nchar(nrOfTimePointsForSlope)-1)
      if(givesWarning(as.double(nrOfTimePointsForSlope))) stop("not a valid number")
      nrOfTimePointsForSlope=as.double(nrOfTimePointsForSlope)
      nrOfTimePointsForSlope=ceiling(nrOFTimePoints*(nrOfTimePointsForSlope/100))
      if(nrOfTimePointsForSlope<5){
        message(paste("nrOfTimePointsForSlope:",originalNrOfTimePointsForSlope," would result in less then 5 points, set to 5 points."))
        nrOfTimePointsForSlope=5
      }
    }else{
      if(givesWarning(as.double(nrOfTimePointsForSlope))) stop("not a valid number")
      nrOfTimePointsForSlope=as.double(nrOfTimePointsForSlope)
      if(nrOfTimePointsForSlope<1){nrOfTimePointsForSlope=ceiling(nrOFTimePoints*(nrOfTimePointsForSlope/100))}
    }
  }else if(is.numeric(nrOfTimePointsForSlope)){
    # number
    # if smaller then one... it is assumee t
    if(nrOfTimePointsForSlope<1){
      if(nrOfTimePointsForSlope<1){
        nrOfTimePointsForSlope=ceiling(nrOFTimePoints*(nrOfTimePointsForSlope/100))
        if(nrOfTimePointsForSlope<5){
          message(paste("nrOfTimePointsForSlope:",originalNrOfTimePointsForSlope," would result in less then 5 points, set to 5 points."))
          nrOfTimePointsForSlope=5
        }
      }
    }
  }else stop("nrOfTimePointsForSlope needs to be either string or number")
  # do some checks.
  if(as.integer(nrOfTimePointsForSlope)!=nrOfTimePointsForSlope) stop("nrOfTimePointsForSlope needs to be a whole number, or < 1")
  if(nrOfTimePointsForSlope>nrOFTimePoints)stop("more timepoint used then ")
  if(nrOfTimePointsForSlope<5)stop("less then 5 time points selected") # might add a smart modus... cause now the defaults needs 50 time points...
  
#   print(paste("nrOfTimePointsForSlope=",nrOfTimePointsForSlope))
  
  
  
  # get the slopes
  # always uses the originalValues to determine slope location...
  slopes=matrix(data = NA,nrow =(nrOFTimePoints-nrOfTimePointsForSlope),ncol=3 )
  colnames(slopes)=c("slope","base","r2")
  for(i in 1:(nrOFTimePoints-nrOfTimePointsForSlope)){
    selection=i:(i+nrOfTimePointsForSlope)
    slope=getSlope(x=time[selection],y=originalValues[selection])
#     slope=getSlope(x=time[selection],y=values[selection])
    slopes[i,]=slope
  }
  

  sortedIndex=sort(slopes[,1],decreasing=T,index.return=T)
  bestIndex=sortedIndex$ix[1]
  best=slopes[bestIndex,]


#   plot(time,values,col="blue")
#   abline(v=time[bestIndex],col="blue")
#   abline(v=time[(bestIndex+nrOfTimePointsForSlope)],col="blue")
#   lines(time,((best[1]*time)+best[2]),col="blue")
# 
#   return(best)
  
  origenalPar=par(no.readonly=T) # backup plotting pars
  if(logValues){
    par(mar=c(5,4,4,5)+.1)
  }
  
  plot(time,originalValues,col="blue")
  title(plotTitle)
  # plot range
  abline(v=time[bestIndex],col="blue")
  abline(v=time[(bestIndex+nrOfTimePointsForSlope)],col="blue")
  # plot slope
  lines(time,((best[1]*time)+best[2]),col="blue")
  
  if(logValues){
    # original values where used to determine location
    # now use log for the true values...
    selection=bestIndex:(bestIndex+nrOfTimePointsForSlope)
    logSlope=getSlope(x=time[selection],y=values[selection])

    
    # http://www2.nau.edu/~gaud/bio326/class/popul/lesson2-2-1.htm
    # N(t)=N(0)e^rt
    N0=originalValues[1]
#     print(paste("N0:",N0,"slope:",logSlope[1]))
    lines(time, (N0*(exp(1)^(logSlope[1]*time))),lwd=2)
#     print(N0*(exp(1)^(logSlope[1]*time)))

    timeDif=time[1]-time[bestIndex]
    logSlope=c(logSlope,timeZeroCorrection=timeDif)
#     lines(time[selection], (originalValues[bestIndex]*(exp(1)^(logSlope[1]*(time[selection]+timeDif) ))),lwd=2)
    lines(time, (originalValues[bestIndex]*(exp(1)^(logSlope[1]*(time+timeDif) ))),lwd=2)
#     print(best)
#     print(logSlope)
#     lines(time, ((originalValues[bestIndex])*(exp(1)^(logSlope[1]*(time) ))),lwd=2)
#     print(  ((originalValues[bestIndex])*(exp(1)^(logSlope[1]*(time) )))  )

#     print(paste("N0:",originalValues[bestIndex],"slope:",logSlope[1]))
#      print((originalValues[bestIndex]*(exp(1)^(logSlope[1]*(time[selection]+timeDif) ))))
    
    # 2 plots in 1 graph
    # http://stackoverflow.com/questions/6142944/how-can-i-plot-with-2-different-y-axes-in-r
    # or: http://robjhyndman.com/hyndsight/r-graph-with-two-y-axes/
    par(new=TRUE)
    plot(time,values,col="red",xaxt="n",yaxt="n",xlab="",ylab="")
    lines(time,((logSlope[1]*time)+logSlope[2]),col="red")
    axis(4)
    mtext("logValues",side=4,line=3)
    legend("bottomright",col=c("blue","red","black"),lty=1,legend=c("originalValues","logValues","N(t)=N(0)e^rt"))
    
    
    
    suppressWarnings(par(origenalPar)) # restore pars... this can give warnings for some reason..
    return(logSlope)
  } else{
    # no log values...
    
    legend("bottomright",col=c("blue"),lty=1,legend=c("originalValues"))
    
    suppressWarnings(par(origenalPar)) # restore pars... this can give warnings for some reason..
    return(best)
  }
  stop("coding error :P")
}


#'getSlope
#' 
#' math is described here:
#' http://stats.stackexchange.com/questions/29525/slope-of-a-line-given-multiple-points
#' http://www.clemson.edu/ces/phoenix/tutorials/excel/regression.html
#' 
#' @param x the x values
#' @param y the y values
#' 
#' no idea how it works... but verified...
#' 
#' @export 
getSlope=function(x,y){
    n=length(x)
    xy=x*y
    sumx=sum(x)
    sumy=sum(y)
    sumxy=sum(xy)
    # y=mx+b
    m=(n*sumxy-sumx*sumy) / (n*sum(x^2)-sumx^2)
    b=(sumy-m*sumx)/n
    r=(n*sumxy-sumx*sumy)/sqrt((n*sum(x^2)-sumx^2)*(n*sum(y^2)-sumy^2))
    #   print(m)
    #   print(b)
    #   print(r^2)
    #
    # m=slope ... 
    # b=it does not start at 0 height but at b height
    # r^2= would be 1 if all points are on the line... 
    return(c(slope=m,base=b,r2=r^2))
}


#' slopeGraph
#' 
#' TODO: DOES NOT WORK ANYMORE!!!
#' 
#' @param mp is the MicroPlate
#' @param wellNr the wellNr you want the analysis on
#' @param nrOfTimePoints the width  default=10
#' 
#' @export
slopeGraph=function(mp,wellNr,nrOfTimePoints=10){
#   index=getWellsMeasurementIndex(mp,wellNr)
#   time=mp[index,"time"][,1]
#   value=mp[index,"value"][,1]
#   
#   slopes=data.frame(matrix(0, (length(time)-nrOfTimePoints),3) )
#   names(slopes)=c("m","b","r^2")
#   selection=1:(length(time)-nrOfTimePoints)
#   for(i in selection){
#     start=i
#     stop=i+nrOfTimePoints
#     selectiostart:stop
#     slopes[i,]=getSlope(mp,wellNr,start,stop,logY=T,plot=F)
#   }
  # 2 plots in 1 graph
  # http://stackoverflow.com/questions/6142944/how-can-i-plot-with-2-different-y-axes-in-r
  # or: http://robjhyndman.com/hyndsight/r-graph-with-two-y-axes/
  # 
#   plot(slopes[[1]],type="l",ylab="slope",ylim = c(min(slopes[[1]]),max(slopes[[1]])))
#   abline(h=0)  
#   par(new=TRUE)
#   plot(slopes[[3]],type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="",ylim = c(min(slopes[[3]]),max(slopes[[3]])))
#   axis(4)
#   par(new=TRUE)
#   plot(time,log(value), type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
#   mtext("y2",side=4,line=3,col="red")
#   legend("topright",col=c("blue","black","red"),lty=1,legend=c("graph","slope","r^2"))
#   return(slopes)
}


#' getGrowthRates
#' @rdname getGrowthRates
#' @description
#' 
#' TODO: what if time is not on measurment level?
#' TODO: add gcID support
#' TODO: what if multiple wavelengths?
#' TODO: check if my score is actually a score...
#' 
#' default it uses time, value
#' and it separetes it based on wavelength 
#' 
#' 
#' 
#' @param self the MicroPlate object
#' @param wellNrs logical/numerical selection or well numbers - missing is all
#' @param timeColumn column with time, the time point in all selected wells need to be equeal
#' @param valueColumn column with the OD values
#' @param nrOfTimePointsForSlope nr of points used to calculate slopes
#' 
#' 
#' @export
setGeneric("getGrowthRates", function(self, wellNrs=NULL, timeColumn="time", valueColumn="value", nrOfTimePointsForSlope="10%") standardGeneric("getGrowthRates"))
#' @rdname getGrowthRates
setMethod("getGrowthRates", signature(self = "MicroPlate"), function(self, wellNrs=NULL, timeColumn="time", valueColumn="value", nrOfTimePointsForSlope="10%"){
  ### check input
  # wellNrs
  if(is.null(wellNrs)) wellNrs=1:self@.data$levelSize[2] # if not specified get it for everything
  if(is.logical(wellNrs)){
    if(length(wellNrs)==self@.data$levelSize[2]){
      wellNrs=(1:self@.data$levelSize[2])[wellNrs]
    }else{
      stop(paste("nr of wells: ",self@.data$levelSize[2] ," your selection: ",length(wellNrs), sep=""))
    }
  }
  # columnSelectors
  #   # todo: maybe remove additional columns... and do other grofit matches
  #   columns=c(timeColumn,valueColumn,experimentIdentifierColumn,additionalInformationColumn,concentrationOfSubstrateColumn)
  #   if(is.character(columns)){
  #     if(!all(self@.data$colnames %in% self@.data$colNames)){
  #       stop("not all columns exist")
  #     }
  #   }
  nrOfWells=length(wellNrs)
  
  results=vector("list", nrOfWells)
  index=0
  
  # names
  growthRateName="growthRate"
  growthRateR2="growthRateR2"
  growthRateBase="growthRateBase"
  growthRateTimeZeroCorrection="growthRateN0Corr"

  
  # reserve space
  if(is.null(self@.data$well[[growthRateName]])){
    self@.data$well[[growthRateName]]=rep(NA,self@.data$levelSize[2])
  }
  if(is.null(self@.data$well[[growthRateBase]])){
    self@.data$well[[growthRateBase]]=rep(NA,self@.data$levelSize[2])
  }
  if(is.null(self@.data$well[[growthRateR2]])){
    self@.data$well[[growthRateR2]]=rep(NA,self@.data$levelSize[2])
  } 
  if(is.null(self@.data$well[[growthRateTimeZeroCorrection]])){
    self@.data$well[[growthRateTimeZeroCorrection]]=rep(NA,self@.data$levelSize[2])
  }

  for(i in wellNrs){
    index=index+1
    selection=getWellsMeasurementIndex(self,i)
    time=self[timeColumn][selection]
    data=self[valueColumn][selection]
    result=getGrowthRate(time=time, values=data,nrOfTimePointsForSlope=nrOfTimePointsForSlope,plotTitle=paste("wellnr =",i))
    results[[index]]=result
    
#     score=result$spline$crit # is this the score??? need crappier data to test!!
    # $spline$cv.crit might be score... else need to do the model fit for score...
#     print(score)
    
    self@.data$well[[growthRateName]][i]=result[1]
    self@.data$well[[growthRateBase]][i]=result[2]
    self@.data$well[[growthRateR2]][i]=result[3]
    self@.data$well[[growthRateTimeZeroCorrection]][i]=result[4]

    #     lines(c(0,60),c(0,1))
    #     print(paste("",lambda,A,mu,integral))
    #     xcor=c(lambda,lambda+5)
    #     xpoint=(integral*A)
    #     print(xpoint)
    #     lines(c(lambda,xpoint),c(0,A))
    #     ycor=c(0,A*5)
    #     print(paste(paste(xcor),paste(ycor)))
    #     print(result$parametersLowess$lambda)
    #     lines(xcor,ycor)
    #     xcor=c(7.13,12.13)
    #     ycor=c(0,3.045)
    #     lines(xcor, ycor)
    #     lines(c(7.13,12.3),c(0,3.045))
    #     lines(c(7.13,12.3),c(0,0.6))
    #     lines(c(0,60),c(0,0.6))
    #     lines(c(lambda,(lambda+50)),c(0,(A*50)))
    #     lines(c(7.13,12.3),c(0,3.045))
    #     lines(x=c(0,50),y=c(0,1))
    
#     lines( c(lambda,(lambda+(A/mu))) , c(0,A) )
    
#     plot(time,data)
    
    results[index]
  }
  
  updateMetaData(self)
  
  
  
  # todo plot things?
  returnResults=F
  if(returnResults){return(results)}
})


