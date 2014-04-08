#' MicroplateExperiment
#' 
#' This class is the root of the experiment...
#' it contains the classes that contain the data...
#' 
#' linked lists may be more appropriate... as you want to clean the data while not redoing everything else...
#' so an insert method instead of an add method would be handy...
#' 
#' the operations follow a command pattern:
#' http://en.wikipedia.org/wiki/Command_pattern
#' 
#' TODO: maybe turn this into a singleton??
#' 
#' @export
Data=setClass(
  Class = "MicroplateExperiment", 
  representation = representation( 
    .mainData = "Data",
    .views = "list", # with more Data :)
    .operations="list" # with operations??
  )
)


#' setData
#' 
#' the set method for the maindata... to keep data abstract.
#' (cause R knows little privacy)
#' 
#' 
#' 
#' @export
setGeneric("setData", function(self,data=NULL) standardGeneric("setData")) 
setMethod("setData", signature(self = "MicroplateExperiment"), function(self,data=NULL){
  self@.mainData=data 
  return(self)
})


#' getData
#' 
#' the get method for the maindata... to keep data abstract.
#' (cause R knows little privacy)
#' 
#' @export
setGeneric("getData", function(self) standardGeneric("getData")) 
setMethod("getData", signature(self = "MicroplateExperiment"), function(self){
  return(self@.mainData)
})



#' addOperation
#' 
#' adds an operation to the operation list... for now on the back of it...
#' 
#' @export
setGeneric("addOperation", function(self,operation=NULL) standardGeneric("addOperation")) 
setMethod("addOperation", signature(self = "MicroplateExperiment"), function(self, operation=NULL){
  self@.operations=self@.operations+operation
  
  return(self)
})




