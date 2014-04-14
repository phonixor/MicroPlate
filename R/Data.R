library(gtools)
#
# Data stores everything!
# hopefully on different levels...
#
#
# TODO:
# dim, length, +-*/ == !=
# Arith() = +-*/  ???
# Compate = == != < > etc... ???
# $ [ [[
# nrow, colnames, head, names etc
# 
# may also need the assignment variables like "$<-"... but i don't know if i want to give users that much access.
#
#
# overwritting/defining +/-*operators
# https://stat.ethz.ch/pipermail/r-help/2011-March/273554.html
# http://stackoverflow.com/questions/4730551/making-a-string-concatenation-operator-in-r -- nice example
# http://stackoverflow.com/questions/14035506/how-to-see-the-source-code-of-r-internal-or-primitive-function -- needed for variable names!!!
#
# memory managment:
# http://adv-r.had.co.nz/memory.html
#
# maybe useful:
# http://web.njit.edu/all_topics/Prog_Lang_Docs/html/library/methods/html/GenericFunctions.html
#
#
#' Data
#' 
#' This class stores data
#' 
#' the addData should keep it abstract -- while still allowing data.frames for import
#' however for now the data.frame is also the only thing that is added...
#' 
#' 
#' 
#' @export
Data=setClass(
  Class = "Data", 
  representation = representation(
#     .data = "data.frame"
    .data="list"
  )
)


#' addData
#' 
#' stores data in the class instance
#' if no data excist the data imported becomes the data
#' else smartbind is used to add the data
#' 
#' 
#' @export
setGeneric("addData", function(self,newData=NULL) standardGeneric("addData")) 
setMethod("addData", signature(self = "Data"), function(self,newData=NULL){
  # check newData names and compare them with the names currently in use
  # add new columns to existing data
  # fill those with NA for existing data

  #
  # adding stuff to and empty data.frame with smartbind creates a row with NA's....
  # ...so it's not actually that smart :P
  if(nrow(self@.data)==0){
#     print("empty .data, .data=newData")
#     print(newData)
    self@.data=newData
  }else{
    smartbind(self@.data, newData)
  }
  return(self)
})


# #' addData
# #' 
# #' stores data in the class instance
# #' if no data excist the data imported becomes the data
# #' else smartbind is used to add the data
# #' 
# #' 
# #' @export
# setGeneric("addData", function(self,newData=NULL) standardGeneric("addData")) 
# setMethod("addData", signature(self = "Data"), function(self,newData=NULL){
#   # check newData names and compare them with the names currently in use
#   # add new columns to existing data
#   # fill those with NA for existing data
#   # add the new row
# #   print(self@.data)
#   #
#   #
#   # adding stuff to and empty data.frame with smartbind creates a row with NA's....
#   # ...so it's not actually that smart :P
#   if(nrow(self@.data)==0){
# #     print("empty .data, .data=newData")
# #     print(newData)
#     self@.data=newData
#   }else{
#     smartbind(self@.data, newData)
#   }
#   return(self)
# })



#' getDataAsDF
#' 
#' 
#' 
#' @export
setGeneric("getDataAsDF", function(self) standardGeneric("getDataAsDF")) 
setMethod("getDataAsDF", signature(self = "Data"), function(self){
  return(self@.data)
})


# # overwrite the [] function..
# setMethod("[", signature(self = "Data", i = "ANY", j = "missing"), function(self, i) {
#   self@.data[i]
# })

# overwrite the $ function..
#
# the variable names are important! they need to be the same as in the R source... else i get:
### Error in match.call(definition, call, expand.dots) : 
###  unused arguments (self = c("Data", ""), name = c("ANY", ""))
# getGeneric("$")
# it says: 
## standardGeneric for "$" defined from package "base"
## 
## function (x, name) 
##   standardGeneric("$", .Primitive("$"))
## <bytecode: 0x0ad56620>
##   <environment: 0x0914d0c0>
##   Methods may be defined for arguments: x
## Use  showMethods("$")  for currently available ones.
#
# only x may be defined, so "name" may not be in the signature...
# but may still be in function.... right... like a unique key thingy...
setMethod("$", signature(x = "Data"), function(x, name) {
#   print("if i could get a $ each time this was used... i would have made this into a recursive algorithm!")
#   x$.data[name] ## lol!!! this actually does exactly that... eeuhm... ... damn still not rich..
  x@.data[name] # works!
})





# setMethod("+",
#           signature(e1 = "character", e2 = "character"),
#           function (e1, e2) {
#             paste(e1, e2, sep = "")
#           })


##
## stuff from the book: 
## Chambers, "Software for data analysis", Springer, 2008.
## chapter 10
# setMethod("==", c("track", "track"),
#           function(e1, e2) {
#             e1@x == e2@x &
#               e1@y == e2@y
#           })
# setMethod("!=", c("track", "track"),
#           function(e1, e2) {
#             e1@x != e2@x |
#               e1@y != e2@y
#           })
# setMethod("Compare", c("track", "track"),
#           function(e1, e2) {
#             cmpx <- callGeneric(e1@x, e2@x)
#             cmpy <- callGeneric(e1@y, e2@y)
#             ifelse(cmpx & cmpy, TRUE,
#                    ifelse(cmpx | cmpy, NA, FALSE))
#           })
# setMethod("[",
#           signature(x = "trackNumeric", i = "ANY", j = "missing"),
#           function(x, i) {
#             x@.Data[i]
#           })


####
