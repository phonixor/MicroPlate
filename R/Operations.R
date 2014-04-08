#Operations
# for now just a set of fuctions to apply on data or views
# NOT USED!!!








#     row column temp time  value content
# 1     1     01 30.0    0 1.6139       B
# 2     1     01 29.9   10 1.6112       B
# 3     1     01 29.9   20 1.6122       B
# 4     1     01 29.9   30 1.6051       B





#' calculateAvarage()
#'
#' calculate the avarage and the sd 
#'
#'  @export
setGeneric("calculateAvarage", function(self,over="values", weq="content") standardGeneric("calculateAvarage")) 
setMethod("calculateAvarage", signature(self = "Data"), function(self,over="values", weq="content"){
  # calculate the average and the sd
  #
  df=self
  dfmean=aggregate(value~content,df,mean)
  dfmean$sd=aggregate(value~content,df,sd)$value
  dfmean
  
  plot(df)
  
  # 
  #
  return(self)
})
