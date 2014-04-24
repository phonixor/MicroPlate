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
# copy <----!!!!!
# stringsAsFactors,  check.names... if i want to inherent data.frame -- make.names...
# http://stackoverflow.com/questions/8691812/get-object-methods-r
#
# may also need the assignment variables like "$<-"... but i don't know if i want to give users that much access.
# 
# ok... maybe make the data lockable???
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
#' for memory reasons everything is to be stored in an enviroment .data
#' all behaviour to acces the .data is overwritten to work with it...
#' this means that once you have created an instance of a class you can copy it 
#' and the data is still sotred at only 1 location
#' 
#' columns 
#' columns with list are recorded in their own lists
#' 
#' 
#' @export
Data=setClass(
  Class = "Data", 
  contains = "data.frame", # S3 s4 conflicts??? it kinda doesnt work :P
  representation = representation(
    .data="environment" # only i may touch me!
  )
#   ,
#   prototype = prototype(
#     .data=new.env() # make sure it has its own little space
#   )
)  
#' initialize
setMethod("initialize", "Data", function(.Object){
  # initialize the love!
  .Object@.data=new.env() # make sure it has its own little space
  .Object@.data$data=NULL # stores all data!
  # per column
  .Object@.data$colNames=NULL # stores the colnames!
  .Object@.data$colType=NULL
  .Object@.data$colLevel=NULL # contains the name of the level above NA if at top level
  # per level
  .Object@.data$level=NULL # contains the name of the level which corresponds to a column name of the level above
  # need to get a way to get to the right level.... and back???
  # rownames are ignored...
  
  return(.Object)
})





#' createFromDataFrame
#' 
#' @export
setGeneric("createFromDataFrame", function(self,df=NULL) standardGeneric("createFromDataFrame")) 
setMethod("createFromDataFrame", signature(self = "Data"), function(self,df=NULL){
  # check if the object is a data.frame
  if(class(df)!="data.frame") stop("not a data frame")
  
  
  colNames=colnames(df)
  uniquesPerCol=data.frame()
  for(col in lenght(colNames)){
    uniqesPerCol[colNames[col]]=length(table(df[colNames[col]]))
  }
  print(uniqesPerCol)
    
  
  
  
  return(self)
})


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
  if(is.null(self@.data$data)){ # is there already any data?
    # no! use data as new data!
    self@.data$data=newData
    
  }else{
    # yes! add data to excisting data!
    print("adding extra data not implemented yet!")
  }
  #
  # update colnames
  updateColnames(self)
  
  
  
  #
  # adding stuff to and empty data.frame with smartbind creates a row with NA's....
  # ...so it's not actually that smart :P 
#   
#   if(nrow(self@.data)==0){
# #     print("empty .data, .data=newData")
# #     print(newData)
#     self@.data=newData
#   }else{
#     smartbind(self@.data, newData)
#   }
  return(self)
})

# 
# #' updateColnames()
# #'
# #' update colnames based on the data available
# #' 
# #' @export
# setGeneric("updateColnames", function(self) standardGeneric("updateColnames")) 
# setMethod("updateColnames", signature(self = "Data"), function(self){
#   # validate data first?
#   #
#   # TODO what if multiple list per level..
#   #
#   # reset
#   self@.data$colnames=NULL
#   self@.data$collevel=NULL
#   self@.data$levels=NULL
#   self@.data$coltype=NULL
#     
#   currentLevel=1
#   currentLevelNames=names(self@.data$data)
#   currentListNr=1
#   currentPath="self@.data$data"
#   nextPath=""
#   
#   # use eval so that i dont have to unlist!!!
#   # http://stackoverflow.com/questions/9449542/access-list-element-in-r-using-get
#   # eval(parse(text="testData@.data$data[['measurement']][[1]]"))
#   continue=T # keep looping while true
#   
#   
#   
#   while(continue){
#     continue=F
# 
#     
#     for(i in 1:length(currentLevelNames)){
#       # use eval here to get into list within list without using unlist
#       dataType=eval(parse(text=paste("typeof(",currentPath,"[['",currentLevelNames[i],"']][",i,"])",sep="")))
#       if(dataType=="list"){
#         print("list!")
#         
#         nextPath=currentPath
#         
#         
#       } 
#       else {
#         print("not a list!")
#         self@.data$coltype=append(coltype,dataType)
#       }
#       print("wwwwwwwwwwwwwwwwwww")
#     } 
#   }
#   
# })

# ' updateColnames
# ' 
# ' recursive loop to update colnames and other column/meta data
# ' @export
setGeneric("updateColnames", function(self, path=NULL, level=NULL) standardGeneric("updateColnames")) 
setMethod("updateColnames", signature(self = "Data"), function(self, path=NULL, level=NULL){
#   print("recursive curse!")
#   print(path)
  
  currentPath=path
  currentLevel=level
  
  if(is.null(path)){
    # first time this method is called (root/top/main level)
#     print("first time!")
    #
    # update 
    currentPath="self@.data$data"
    currentLevel=NA
    #
    # reset mete data
    self@.data$colNames=NULL
    self@.data$colLevel=NULL
    self@.data$colType=NULL
    self@.data$level=NULL
    
  } 
  
  currentLevelNames=names(eval(parse(text=currentPath)))
  
  for(i in 1:length(currentLevelNames)){
      
    dataType=eval(parse(text=paste("typeof(",currentPath,"[['",currentLevelNames[i],"']][",i,"])",sep="")))
    
#     print(currentLevelNames[i])
    
    if(dataType=="list"){
#       print("list!")
      nextPath=paste(currentPath,"$",currentLevelNames[i],"[[1]]",sep="") # the firest nested list
      
      # add to level
      self@.data$level=append(self@.data$level,currentLevelNames[i])
        
      # recursively call this
      updateColnames(self,nextPath, currentLevelNames[i])
    } 
    else {
#       print("not a list!")
      # add to metadata
      self@.data$colNames=append(self@.data$colNames,currentLevelNames[i])
      self@.data$colType=append(self@.data$colType,dataType)
      self@.data$colLevel=append(self@.data$colLevel,currentLevel)
    }
    
  }  
  
  return(self)
})


#   # Semi recursive loop...
#   # how to acces lists in lists??? wihtout unlist
#   # gonna ask douwe...
#   while continue
#     continue=F
#     for(i in 1:length(currentLevelNames)){
#       
#       if(typeof(self@.data$data[[currentLevelNames[i]]])!="list"){
#         # column has data
#         self@.data$colnames=append(self@.data$colnames,currentLevelNames[i])
#         self@.data$collevel=append(self@.data$collevel,1)
#       } else {
#         # column is a list
#         continue=T # make sure you also check that level.
#         self@.data$levels=append(self@.data$levels,currentLevelNames[i])
#         
#         # how to acces lists in lists???
#         # gonna ask douwe...
#         
#         unlist(testData@.data$data[["measurement"]][1])
#         
#       }
#     }
#   






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

#' $
#' overwrite the $ function..
#' 
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
#'
#'
#' @export
setMethod("$", signature(x = "Data"), function(x, name) {
  
  # 
  # x@.data$colLevel[x@.data$colNames==name]
  
  level=x@.data$colLevel[x@.data$colNames==name]
  if (is.null(level)){
    print("ok this shouldnt happen... but it did!") # change in a warning later...
    return(x@.data$data[[name]])
  }
  
  if (is.na(level)){
    # data at top level
    return(x@.data$data[[name]])
  } else{
    # data is hidden in lists in lists in ...
    returnValue=NULL
    returnValue=recursiveFetch(self=x, returnValue=returnValue, goalName=name)
    return(returnValue)
  }
})

#' recursiveFetch
#' 
#' @export
setGeneric("recursiveFetch", function(self,returnValue=NULL,path=NULL,goalName=NULL,level=NULL) standardGeneric("recursiveFetch")) 
setMethod("recursiveFetch", signature(self = "Data"), function(self,returnValue=NULL,path=NULL,goalName=NULL,level=NULL){
#   print("recursive curse!")
#   print(path)
  
  currentPath=path
  currentLevel=level
  
  if(is.null(path)){
    # first time this method is called (root/top/main level)
    #     print("first time!")
    #
    # update 
    currentPath="self@.data$data"
    currentLevel=x@.data$colLevel[x@.data$colNames==goalName]
    returnValue=NULL
  } 
  #   self@.data$data$measurement
  goalLevel=x@.data$colLevel[x@.data$colNames==goalName]
  
  #self@.data$data[currentLevel]
  text=paste(currentPath,"[['",currentLevel,"']]",sep="")
#   print(text)
  len=length(eval(parse(text=text)))
#   print(len)
  for(i in 1:len){
    if (currentLevel==goalLevel){
      # at this level there is acces to the data!
      #
      #self@.data$data$measurement[[i]]$value
      text=paste(currentPath,"$",goalLevel,"[[",i,"]]","$",goalName,sep="")
      newValue=eval(parse(text=text))
      returnValue=append(returnValue,newValue)
                         
    }else{
      print("ok no real recursive stuff yet :P")
    }
  }
  
 
  
  
  return(returnValue)
})





#' $<-
#' overwrite the $<- function
#' @export
setMethod("$<-", signature(x = "Data"), function(x, name, value) {
  # TODO test if its valid data???
  
  # test if its new
  if(is.null(x@.data$data[[name]]) || is.na(x@.data$data[[name]])){
    x@.data$colNames=append(x@.data$colNames,name)
  }
  x@.data$data[name]=value
  return(x)
})


#' overwrite colnames()
#' @export
setMethod("colnames", signature(x = "Data"), function(x) {
  return(x@.data$colNames)
})
#' overwrite colnames<-
#' @export
setMethod("colnames<-", signature(x = "Data"), function(x, value) {
  # TODO add checks! if its the same size as data... and you probably dont want to change this anyways...
  warning("you are adviced not to do this!... but you already did...")
  x@.data$colNames=value
  names(x@.data$data)=value
  
  #   return(x@.data$colnames)
  return(x) # for some reason i have to do this, else the instance of the class transforms into the value passed...
})


#' overwrite print()
#' @export
setMethod("print", signature(x = "Data"), function(x) {
#   print("oooh you want to know my secrets???... well they are secret!!!")
#   x@.data
  print(x@.data$data)
  return(x)
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
