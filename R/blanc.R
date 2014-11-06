## BLANC
# operations to remove blancos




#' removeAvarageBlancPerTime
#' @rdname removeAvarageBlancPerTime
#' @description
#' 
#' calculate the avarage blanc
#' @param mp is the microplate object
#' @param column the column in which you define what blancs are
#' @param indicator the indicator in the column, usualy a string like "blanc" 
#' @param newVariable if you dont want the original values column to be overwritten, put a new column name here
#' 
#' TODO: check if start at the same time...
#' 
#' @export
setGeneric("removeAvarageBlancPerTime", function(mp, column, indicator, newVariable=NULL) standardGeneric("removeAvarageBlancPerTime")) 
#' @rdname removeAvarageBlancPerTime
setMethod("removeAvarageBlancPerTime", signature(mp = "MicroPlate"), function( mp, column, indicator, newVariable=NULL ){
  for(i in 1:length(mp@.data$plate$plateName)){
    
    averageBlanc=aggregate(value~time, data=mp[mp[column,level="measurement"]==indicator,] , mean)
    sdBlanc=aggregate(value~time, data=mp[mp[column,level="measurement"]==indicator,] , sd)
    
    
    averageBlanc[[1]]
    averageBlanc[[2]]
    sdBlanc[[2]]
    
    averageBlanc[[2]]+sdBlanc[[2]]
    
    plot(aggregate(value~row, data=mp[mp[column,level="measurement"]==indicator,] , mean))
    plot(aggregate(value~column, data=mp[mp[column,level="measurement"]==indicator,] , mean))
    
    mp$value[mp$time==mp$time[[1]]]
    

    plotWithErrorBars(averageBlanc[[1]],averageBlanc[[2]],sdBlanc[[2]])
    
#     library(scatterplot3d)
    
    firstTimePointSelection=mp$time==mp$time[1]
    
#     scatterplot3d(mp[["row",level=1]][firstTimePointSelection],mp[["column",level=1]][firstTimePointSelection],mp$value[firstTimePointSelection],type = "l")
#     scatterplot3d(mp[["row",level=1]][firstTimePointSelection],mp[["column",level=1]][firstTimePointSelection],mp$value[firstTimePointSelection],type = "l")
    row=mp["row",level=1][firstTimePointSelection]
    column=mp["column",level=1][firstTimePointSelection]
    value=mp$value[firstTimePointSelection]
    persp(row,column,value)
    
#     plot(averageBlanc)
#     
#     arrows(x,CI.dn,x,CI.up,code=3,length=0.2,angle=90,col='red')
#     
    
    #   segments(x,y-sd,x,y+sd)
    #   epsilon <- 0.02
    #   segments(x-epsilon,y-sd,x+epsilon,y-sd)
    #   segments(x-epsilon,y+sd,x+epsilon,y+sd)
    
    
    
    #   plot(averageBlanc)
    # remove blanc from data
#     mp$corValue=mp$value-averageBlanc[match(mp$time, averageBlanc$time),2] # remove blanc
    
  }
})


lalala=function(mp){
  install.package("scatterplot3d")
  selection=mp$time==min(mp$time)
  scatterplot3d(x=mp[selection,"row",level=1],y=mp[selection,"column",level=1],z=mp[selection,"value"])
                
#                 highlight.3d=TRUE,
#                 col.axis="blue", col.grid="lightblue",
#                 main="scatterplot3d - 2", pch=20)
  
}


#' plotWithErrorBars
#' 
#' creates a plot with error bars
#' 
#' @param x a list with the x coordinates
#' @param y a list with the y coordinates
#' @param sd a list with the standard deviation
#' 
#' @export
plotWithErrorBars=function(x,y,sd){
 
  sdUp=y+sd
  sdDown=y-sd
  
#   plot(x,y,type="n",ylim=c(min(sdDown),max(sdUp)))
  plot(x,y,type="n",ylim=c(0,max(sdUp)),xlab="time in minutes", ylab="OD")
  arrows(x,sdDown,x,sdUp,code=3,length=0,angle=90,col='red')
  points(x,y,pch=20)
  
}







