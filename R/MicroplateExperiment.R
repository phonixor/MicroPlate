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
    .operations="list" # with operations?? how to enforce?
  )
)


#' setData
#' 
#' the set method for the maindata... to keep data abstract.
#' (cause R knows little privacy)
#' 
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
#' TODO GENERATE VIEW!
#' 
#' @export
setGeneric("addOperation", function(self,operation=NULL) standardGeneric("addOperation")) 
setMethod("addOperation", signature(self = "MicroplateExperiment"), function(self, operation=NULL){
  self@.operations=append(self@.operations,operation)
  #TODO GENERATE VIEW!
  
  return(self)
})


#' runAll
#' 
#' @export
setGeneric("runAll", function(self) standardGeneric("runAll")) 
setMethod("runAll", signature(self = "MicroplateExperiment"), function(self){
  # always start with original data as source
  data=getData(self)
  #
  # then execute all data 
  for(i in 1:length(self@.operations)){
    operation=self@.operations[[i]]
#     print(class(operation))
#     print(typeof(operation))
#     print(operation@.blankIdentifierName) # should only work for RemoveBlank operation...
    data=execute(operation,data)
  }
  # and cookies!!!
  return(self)
})
