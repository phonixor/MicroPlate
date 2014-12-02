# tools for slopes
#
# TODO: consider including things like:  smooth.spline
# 


#' getGrowthRateBetween
#' 
#' this function calculates the slope between 2 time points within a larger time series
#' 
#' 
#' @param values - y
#' @param time - x
#' @param start - the start point of the slope window, its a coordinate in the time vector (not an actual time point)
#' @param end - the end point of the slope window, its a coordinate in the time vector (not an actual time point)
#' @param logYValues - boolean - takes the log of the values, default = TRUE... uses original values to determine slope location!!!
#' @param plotTitle - title of the plot... useful to identify
#' 
#' TODO: add non logYValue path
#' 
#'@export
getGrowthRateBetween=function(values,time, start, end, logYValues=T, plotTitle="growthRate"){
  if(length(time)!=length(values))stop("lenght time and values do not match")
  if(any(is.infinite(values)))stop("Inf, not allowed")
  if(any(is.nan(values)))stop("NaN, not allowed")
  if(any(is.na(values)))stop("NA, not allowed")
  nrOFTimePoints=length(time)
  logValues=NULL
  if(logYValues){
    logValues=log(values)
    if(any(is.infinite(values)))stop("Inf, probably due to log(0)?")
    if(any(is.nan(values)))stop("NaN, not allowed")
    if(any(is.na(values)))stop("NA, not allowed")
  } 
  
  slope=getSlope(x=time[start:end],y=values[start:end])


  if(logYValues){
    
    logSlope=getSlope(x=time[start:end],y=logValues[start:end])
    logSlope=c(logSlope, doublingTime=(log(2)/logSlope[1]), timeZero=time[start])
    
    origenalPar=par(no.readonly=T) # backup plotting pars
    par(mar=c(5,4,4,5)+.1)
    plot(time,values,col="blue")
    title(plotTitle)
    # plot range
    abline(v=time[start],col="blue",lwd=1)
    abline(v=time[end],col="blue",lwd=1)
    # plot slope
    lines(time,(slope[1]*time)+slope[2],col="blue")

    # 
    lines(time, (values[start]*(exp(1)^(logSlope[1]*(time-logSlope[5]) ))),lwd=2)
    #
    
    
    # 2 plots in 1 graph
    # http://stackoverflow.com/questions/6142944/how-can-i-plot-with-2-different-y-axes-in-r
    # or: http://robjhyndman.com/hyndsight/r-graph-with-two-y-axes/
    par(new=TRUE)
    # plot log
    plot(time,logValues,col="red",xaxt="n",yaxt="n",xlab="",ylab="")
    lines(time,((logSlope[1]*time)+logSlope[2]),col="red")
    axis(4)
    mtext("logValues",side=4,line=3)
    legend("bottomright",col=c("blue","red","black"),lty=1,legend=c("originalValues","logValues","N(t)=N(0)e^rt"))
    par(origenalPar)
    
    return(logSlope)
  }
  
  
}


#' getGrowthRate
#' 
#' This function takes a time series and finds the exponential fase, and calculates the growthrate.
#' It work by moving a window (nrOfTimePointsForSlope) over the entire time series and calculates the slope within.
#' Where the slope is highest it calculates the slope of the same data in the natural logarithm (e log/ln).
#' Since this window is most likely at the end of the exponential growth fase, it then start moving back to the left (back in time)
#' As the log(value) slope is most likely higher there, it continues going left as long as the slope continous to increase.
#' But only if the error does not increase.
#' 
#' @param values - y
#' @param time - x
#' @param logYValues - boolean - takes the log of the values, default = TRUE... uses original values to determine slope location!!!
#' @param nrOfTimePointsForSlope - the window size, a nr or fraction or string ending with the percent sybol (not allowed in docs???? so cant give example :P)
#' @param plot - boolean if it needs to plot the results default = TRUE
#' @param plotTitle - title of the plot... useful to identify
#' 
#' todo: decide if i do anything with r^2
#' todo add logYValues=F path..
#' 
#' 
#' @export
getGrowthRate=function(values,time,logYValues=T,nrOfTimePointsForSlope="10%",plot=T,plotTitle="growthRate"){
  originalNrOfTimePointsForSlope=nrOfTimePointsForSlope # backup
  if(length(time)!=length(values))stop("lenght time and values do not match")
  if(any(is.infinite(values)))stop("Inf, not allowed")
  if(any(is.nan(values)))stop("NaN, not allowed")
  if(any(is.na(values)))stop("NA, not allowed")
  nrOFTimePoints=length(time)
  logValues=NULL
  if(logYValues){
    logValues=log(values)
    if(any(is.infinite(values)))stop("Inf, probably due to log(0)?")
    if(any(is.nan(values)))stop("NaN, not allowed")
    if(any(is.na(values)))stop("NA, not allowed")
  } 
  
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
    slope=getSlope(x=time[selection],y=values[selection],suppressWarnings=T)
    slopes[i,]=slope
  }
  
  # get the max slope
  sortedIndex=sort(slopes[,1],decreasing=T,index.return=T) 
  bestIndex=sortedIndex$ix[1] 
  best=slopes[bestIndex,]

  if(logYValues){
    # the theory is, that the max slope with normal values determines the location
    # however the last few points of the expenential growth are no longer exponential...
    # in any case the better values are on the left side of where the normal slope was max
#     print(paste("bestIndex",bestIndex,"nrOfTimePointsForSlope",nrOfTimePointsForSlope))
    logSlopes=matrix(data = NA,nrow=bestIndex,ncol=3 )
    colnames(logSlopes)=c("slope","base","r2")
    index=1
    for(i in bestIndex:1){
      selection=i:(i+nrOfTimePointsForSlope)
#       print(selection)
      slope=getSlope(x=time[selection],y=logValues[selection],suppressWarnings=T)
      logSlopes[index,]=slope
      index=index+1
    }
    #
    logBest=logSlopes[1,]
    logBestIndex=bestIndex
    for(i in 1:bestIndex){
      if(logBest[1]<=logSlopes[i,1]){ # if slope gets bigger
        if(logBest[3]<=logSlopes[i,3]){ # only if error get smaller (closer to 1, so bigger :P)
          logBest=logSlopes[i,]
          logBestIndex=bestIndex-i+1
#           print("better slope found")
        }else {
          break  
#           if(logSlopes[i,3]>0.95){
#             logBest=logSlopes[i,]
#           }
        }
      }
    }
  
#     print(logBest)
      
    logBest=c(logBest, doublingTime=(log(2)/logBest[1]),timeZero=time[logBestIndex])
    if(plot){
      origenalPar=par(no.readonly=T) # backup plotting pars
      par(mar=c(5,4,4,5)+.1)
      plot(time,values,col="blue")
      title(plotTitle)
      # plot range
      abline(v=time[bestIndex],col="blue",lwd=2)
      abline(v=time[(bestIndex+nrOfTimePointsForSlope)],col="blue",lwd=2)
      # plot slope
      lines(time,((best[1]*time)+best[2]),col="blue")
  
  
#       N0=values[1]
  #     print(paste("N0:",N0,"slope:",logBest[1]))
#       lines(time, (N0*(exp(1)^(logBest[1]*time))),lwd=2) # not very interesting 
  #     print(N0*(exp(1)^(logBest[1]*time)))
  
      
      
  #     lines(time[selection], (originalValues[bestIndex]*(exp(1)^(logBest[1]*(time[selection]+timeDif) ))),lwd=2)
      lines(time, (values[logBestIndex]*(exp(1)^(logBest[1]*(time-logBest[5]) ))),lwd=2)
      
      # 2 plots in 1 graph
      # http://stackoverflow.com/questions/6142944/how-can-i-plot-with-2-different-y-axes-in-r
      # or: http://robjhyndman.com/hyndsight/r-graph-with-two-y-axes/
      par(new=TRUE)
      # plot log
      plot(time,logValues,col="red",xaxt="n",yaxt="n",xlab="",ylab="")
      # plot log range
      abline(v=time[logBestIndex],col="red")
      abline(v=time[(logBestIndex+nrOfTimePointsForSlope)],col="red")
      lines(time,((logBest[1]*time)+logBest[2]),col="red")
      axis(4)
      mtext("logValues",side=4,line=3)
      legend("bottomright",col=c("blue","red","black"),lty=1,legend=c("originalValues","logValues","N(t)=N(0)e^rt"))
      par(origenalPar)
    }
    return(logBest)
  }
}


#' getGrowthRateBasedOnLogOnly
#' 
#' This function takes a time series and calculates slopes over the time series
#' and returns the max...
#'  
#' @param values - y
#' @param time - x
#' @param nrOfTimePointsForSlope - the window size, a nr or fraction or string ending with the percent sybol (not allowed in docs???? so cant give example :P)
#' @param minR2 - minimum r^2 for the slope, basically what error do you allow...
#' @param plot - boolean if it needs to plot the results default = TRUE
#' @param plotTitle - title of the plot... useful to identify
#' 
#' todo: decide if i do anything with r^2
#' todo add logYValues=F path..
#' todo deside what todo if minr2 gives no results.... (no clue what happends now :p)
#' 
#' 
#' @export
getGrowthRateBasedOnLogOnly=function(values,time,nrOfTimePointsForSlope="10%",minR2=0.95,plot=T,plotTitle="growthRate"){
  originalNrOfTimePointsForSlope=nrOfTimePointsForSlope # backup
  if(length(time)!=length(values))stop("lenght time and values do not match")
  if(any(is.infinite(values)))stop("Inf, not allowed")
  if(any(is.nan(values)))stop("NaN, not allowed")
  if(any(is.na(values)))stop("NA, not allowed")
  nrOFTimePoints=length(time)
  logValues=log(values)
  if(any(is.infinite(values)))stop("Inf, probably due to log(0)?")
  if(any(is.nan(values)))stop("NaN, not allowed")
  if(any(is.na(values)))stop("NA, not allowed")
  
  
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
  slopes=matrix(data = NA,nrow =(nrOFTimePoints-nrOfTimePointsForSlope),ncol=4 )
  colnames(slopes)=c("slope","base","r2","index")
  slopes[,4]=1:dim(slopes)[1]
  for(i in 1:(nrOFTimePoints-nrOfTimePointsForSlope)){
    selection=i:(i+nrOfTimePointsForSlope)
    slope=getSlope(x=time[selection],y=logValues[selection],suppressWarnings=T)
    slopes[i,1:3]=slope
  }
  
#   print(slopes)
  
  # get rid of crappy slopes
  slopes=slopes[slopes[,3]>=minR2,]
  
  # get the max slope
  sortedIndex=sort(slopes[,1],decreasing=T,index.return=T) 
#   print(sortedIndex)
  logBestIndex=slopes[[sortedIndex$ix[1],4]]
#   print(dim(slopes))
#   print(logBestIndex)
  logBest=slopes[sortedIndex$ix[1],1:3]
  logBest=c(logBest, doublingTime=(log(2)/logBest[1]),timeZero=time[logBestIndex])
  
  if(plot){
    origenalPar=par(no.readonly=T) # backup plotting pars
    par(mar=c(5,4,4,5)+.1)
    plot(time,values,col="blue")
    title(plotTitle)
    
    # plot growthrate
    lines(time, (values[logBestIndex]*(exp(1)^(logBest[1]*(time-logBest[5]) ))),lwd=2)
    
    # 2 plots in 1 graph
    # http://stackoverflow.com/questions/6142944/how-can-i-plot-with-2-different-y-axes-in-r
    # or: http://robjhyndman.com/hyndsight/r-graph-with-two-y-axes/
    par(new=TRUE)
    # plot log
    plot(time,logValues,col="red",xaxt="n",yaxt="n",xlab="",ylab="")
    # plot log range
    abline(v=time[logBestIndex],col="red")
    abline(v=time[(logBestIndex+nrOfTimePointsForSlope)],col="red")
    lines(time,((logBest[1]*time)+logBest[2]),col="red")
    axis(4)
    mtext("logValues",side=4,line=3)
    legend("bottomright",col=c("blue","red","black"),lty=1,legend=c("originalValues","logValues","N(t)=N(0)e^rt"))
    par(origenalPar)
  }
  return(logBest)
}





#' getSlope
#' 
#' calculates the slope in a time series, using the same formula as excel uses
#' 
#' math is described here:
#' http://stats.stackexchange.com/questions/29525/slope-of-a-line-given-multiple-points
#' http://www.clemson.edu/ces/phoenix/tutorials/excel/regression.html
#' 
#' @param x the x values
#' @param y the y values
#' @param suppressWarnings useful for growth rates...
#' 
#' no idea how it works... but verified...
#' 
#' @export 
getSlope=function(x,y,suppressWarnings=F){
    n=length(x)
    xy=x*y
    sumx=sum(x)
    sumy=sum(y)
    sumxy=sum(xy)
    # y=mx+b
    m=(n*sumxy-sumx*sumy) / (n*sum(x^2)-sumx^2)
    b=(sumy-m*sumx)/n
    if(suppressWarnings){
      r=suppressWarnings((
        (n*sumxy-sumx*sumy)/sqrt((n*sum(x^2)-sumx^2)*(n*sum(y^2)-sumy^2)) 
      ))
    }else{
      r=(n*sumxy-sumx*sumy)/sqrt((n*sum(x^2)-sumx^2)*(n*sum(y^2)-sumy^2))
    }
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
  stop("not supported at this time")
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
#' calculate growth rates for the selected wells.
#' N(t)=N(0)*e^(slope*t)
#' #' also calculates doubling time
#' doublingTime=ln(2)/slope
#' 
#' TODO: what if time is not on measurment level?
#' TODO: what if multiple wavelengths?
#' TODO: make column names changable...
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
  growthRateBaseName="growthRateBase"
  growthRateR2Name="growthRateR2"
  doublingTimeName="doublingTime"
  timeZeroName="timeZero"
 
  
  # reserve space
  if(is.null(self@.data$well[[growthRateName]])){
    self@.data$well[[growthRateName]]=rep(NA,self@.data$levelSize[2])
  }
  if(is.null(self@.data$well[[growthRateBaseName]])){
    self@.data$well[[growthRateBaseName]]=rep(NA,self@.data$levelSize[2])
  }
  if(is.null(self@.data$well[[growthRateR2Name]])){
    self@.data$well[[growthRateR2Name]]=rep(NA,self@.data$levelSize[2])
  } 
  if(is.null(self@.data$well[[doublingTimeName]])){
    self@.data$well[[doublingTimeName]]=rep(NA,self@.data$levelSize[2])
  }
  if(is.null(self@.data$well[[timeZeroName]])){
    self@.data$well[[timeZeroName]]=rep(NA,self@.data$levelSize[2])
  }


  for(i in wellNrs){
    index=index+1
    selection=getWellsMeasurementIndex(self,i)
    time=self[timeColumn][selection]
    data=self[valueColumn][selection]
    result=getGrowthRate(time=time, values=data,nrOfTimePointsForSlope=nrOfTimePointsForSlope,plotTitle=paste("wellnr =",i))
    results[[index]]=result
    
    self@.data$well[[growthRateName]][i]=result[1]
    self@.data$well[[growthRateBaseName]][i]=result[2]
    self@.data$well[[growthRateR2Name]][i]=result[3]
    self@.data$well[[doublingTimeName]][i]=result[4]
    self@.data$well[[timeZeroName]][i]=result[5]
    
  }
  
  updateMetaData(self)
  
  # todo plot things?
  returnResults=F
  if(returnResults){return(results)}
})


