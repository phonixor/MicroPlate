# Operation Interface Class
#
# http://en.wikipedia.org/wiki/Command_pattern
# http://stackoverflow.com/questions/104918/command-pattern-how-to-pass-parameters-to-a-command
# 
# all operations should ge able to run on their own..
# so all given parameters should be stored within the instance of the class...
#
# i am not sure how to inherent a method in R, as they are not really bound to the class...
# well they are... but you still need to overwrite them anyways...
#



#' Operation
#' 
#' interface for operations
#' needs to be inherited
#' 
#' 
#' 
#' easy way to add new methods without having to make the classes???
#' like a constructor/factory thingy???
#' 
#' 
#' 
#' @export
Data=setClass(
  Class = "Operation",
#   representation = representation( 
#     .data="Data"
#   )
)



#' execute
#'
#' interface for operations
#' needs to be implemented...  don't really know how to do that in R... 
#' so we will try this :)
#' 
#' should start with a view, and return a view
#' 
#' 
#' @export
setGeneric("execute", function(self, data) standardGeneric("execute")) 
setMethod("execute", signature(self = "Operation"), function(self, data){
  print("execute() in Operation, YOU SHOULD NEVER SEE ME!, I SHOULD BE OVERWRITTEN!!!")
  return(self)
})

