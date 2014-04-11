#
#' CustomOperation()
#' 
#' the idea is that this will automatically turn a function into an Operation
#' allowing them to be added to the experiment without having to write an entire new class
#' 
#' the parameters used are stored within
#' 
#' TODO add library/CRAN support
#' TODO think about non CRAN packages
#' TODO even more stuff
#' 
#' @include Operation.R
#' @export
CustomOperation=setClass(
  Class = "CustomOperation",
  contains = "Operation",
  representation = representation( 
    .function="function",
    .parameters="list"
  )
)

setMethod("initialize", "CustomOperation", function(.Object, FUN=NULL, parameters , ...){
  .Object@.function=match.fun(FUN)
  .Object@.parameters=parameters
  print("---match.call---")
  print(match.call())
#   print("---match.arg---")
#   print(match.arg())

  return(.Object)
})

setGeneric("execute", function(self,data) standardGeneric("execute")) 
setMethod("execute", signature(self = "CustomOperation"), function(self,data){
#   data=removeBlank(self,data,column=self@.blankIdentifierColumn,identifier=self@.blankIdentifierName)
  
  
  
  data=self@.function(self@.parameters)
  
  
  return(data)
})


