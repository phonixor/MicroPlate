# 
# 
# #' Operation
# #' 
# #' inheriteds/implements Operation
# #' 
# #' what if blank on different samples???
# #' and other more complex behaviour???
# #' 
# #' values always in value??? for now they are!
# #' 
# #' 
# #' @export
# RemoveBlank=setClass(
#   Class = "RemoveBlank",
#   contains = "Operation",
#   representation = representation( 
#     .blankIdentifierColumn= "character",
#     .blankIdentifierName  = "character"
#   ),
#   prototype = prototype(
#     .blankIdentifierColumn = "basic",
#     .blankIdentifierName = "blank"
#   )
# )
# 
# #setMethod("initialize", signature(self= "RemoveBlank"), function(self, column=NULL, identifier=NULL){
# setMethod("initialize", "RemoveBlank", function(.Object, column=NULL, identifier=NULL){
#   #   test <- callNextMethod(self, ...)
#   if(!is.null(column))
#     .Object@.blankIdentifierColumn = column
#   if(!is.null(identifier))
#     .Object@.blankIdentifierName = identifier
#   
#   return(.Object)
# })
# 
# 
# 
# #' execute
# #'
# #' implementation of Operation's execute()
# setGeneric("execute", function(self,data) standardGeneric("execute")) 
# setMethod("execute", signature(self = "RemoveBlank"), function(self,data){
#   data=removeBlank(self,data,column=self@.blankIdentifierColumn,identifier=self@.blankIdentifierName)
#   
#   return(data)
# })
# 
# 
# #' removeBlank
# #'
# #' the thing that does the actual calculations...
# #' maybe move this to execute?
# #' @export
# setGeneric("removeBlank", function(self,data=NULL ,column=NULL, identifier=NULL) standardGeneric("removeBlank")) 
# setMethod("removeBlank", signature(self = "RemoveBlank"), function(self, data=NULL, column=NULL, identifier=NULL){
#   # for now keep it simple
#   
#   # for now try:
#   df=getDataAsDF(data)
#   self@.blankIdentifierColumn = column
#   self@.blankIdentifierName = identifier
#   
#   #   print(self@.data)
#   #   typeof(self@.data)
#   #   print(self@.blankIdentifierColumn)
#   #   print(self@.blankIdentifierName)
#   
#   # i should make it easy to do it over time and stuff...
#   # but for now:
#   #
#   # 
#   meanBlank=mean(df["value"][df[self@.blankIdentifierColumn]==self@.blankIdentifierName])
#   sdBlank=sd(df["value"][df[self@.blankIdentifierColumn]==self@.blankIdentifierName])
#   print(meanBlank)
#   print(sdBlank)
#   print("what todo with the SD here?")
#   
#   
#   #   print(df["value"])
#   
#   df["value"]=df["value"]-meanBlank
#   # print(df["value"])
#   #   print(df)
#   
#   # does this need a set method??? why no pointers??? 
#   # Q: does R suck y/y?
#   #   print(typeof(df))
#   #   print(typeof(data))
#   #   print(typeof(data@.data))
#   data@.data=df
#   
#   
#   return(data)
# })
# 






# #
# #' CustomOperation()
# #' 
# #' the idea is that this will automatically turn a function into an Operation
# #' allowing them to be added to the experiment without having to write an entire new class
# #' 
# #' the parameters used are stored within
# #' 
# #' TODO add library/CRAN support
# #' TODO think about non CRAN packages
# #' TODO even more stuff
# #' 
# #' 
# #' @export
# CustomOperation=setClass(
#   Class = "CustomOperation",
#   contains = "Operation",
#   representation = representation( 
#     .function="function",
#     .parameters="list"
#   )
# )
# 
# setMethod("initialize", "CustomOperation", function(.Object, FUN=NULL, ...){
#   .function=FUN
#   print("---match.call---")
#   print(match.call())
# #   print("---match.arg---")
# #   print(match.arg())
# 
#   return(.Object)
# })
# 
# setGeneric("execute", function(self,data) standardGeneric("execute")) 
# setMethod("execute", signature(self = "CustomOperation"), function(self,data){
# #   data=removeBlank(self,data,column=self@.blankIdentifierColumn,identifier=self@.blankIdentifierName)
#   
#   return(data)
# })
# 

