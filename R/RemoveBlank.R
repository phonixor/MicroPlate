#     row column temp time  value content
# 1     1     01 30.0    0 1.6139       B
# 2     1     01 29.9   10 1.6112       B
# 3     1     01 29.9   20 1.6122       B
# 4     1     01 29.9   30 1.6051       B

## TODO NEED TO USE getData() instead of assuming its a data.frame!!!!!

#' Operation
#' 
#' inheriteds/implements Operation
#' 
#' what if blank on different samples???
#' and other more complex behaviour???
#' 
#' values always in value??? for now they are!
#' 
#' 
#' @export
Data=setClass(
  Class = "RemoveBlank",
  contains = "Operation",
  representation = representation( 
    .blankIdentifierColumn= "character",
    .blankIdentifierName  = "character"
  ),
  prototype = prototype(
    .blankIdentifierColumn = "basic",
    .blankIdentifierName = "blank"
  )
)


#' execute
#'
#' implementation of Operation's execute()
setGeneric("execute", function(self) standardGeneric("execute")) 
setMethod("execute", signature(self = "Operation"), function(self){
  self=removeBlank(self,data=self@.data,column=self@.blankIdentifierColumn,identifier=self@.blankIdentifierName)
  
  return(self)
})


#' removeBlank
#'
#' the thing that does the actual calculations...
#' maybe move this to execute?
#' @export
setGeneric("removeBlank", function(self,data=NULL ,column=NULL, identifier=NULL) standardGeneric("removeBlank")) 
setMethod("removeBlank", signature(self = "Operation"), function(self,data=NULL, column=NULL, identifier=NULL){
  # for now keep it simple
  # what todo with sd?
  
  # here or someplace else??
  self@.data=data # mmmmh this can be problametic
  self@.blankIdentifierColumn = column
  self@.blankIdentifierName = identifier
  
  print(self@.data)
  print(self@.blankIdentifierColumn)
  print(self@.blankIdentifierName)
  
  meanBlank=mean(self@.data["value"][self@.data[self@.blankIdentifierColumn]==self@.blankIdentifierName])
#   meanBlank=aggregate(value~content,df,mean)
#   meanBlank=aggregate(value~content,df,mean)
#     mean(self@.data[self@.]
  print(meanBlank)
  
  return(self)
})



