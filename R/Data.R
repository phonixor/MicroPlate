library(gtools)


# setGeneric("foo", function(a, b) standardGeneric("foo")) 
# setMethod("foo", signature("A1", "A2"), function(a, b) "1-2")

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
    .data = "data.frame"
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
  # add the new row
#   print(self@.data)
  #
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



#' getDataAsDF
#' 
#' 
#' 
#' @export
setGeneric("getDataAsDF", function(self) standardGeneric("getDataAsDF")) 
setMethod("getDataAsDF", signature(self = "Data"), function(self){
  return(self@.data)
})



