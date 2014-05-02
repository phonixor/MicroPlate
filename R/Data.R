library(gtools)
library(plyr)
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
# data.frame source:
# https://github.com/SurajGupta/r-source/blob/91aaa43312da6b0e6906ef221fd7756dc0459843/src/library/base/R/dataframe.R
#
#
#
#
# adding contains data.frame changes:
# > testData
# An object of class "Data"
# Slot ".data":
#   <environment: 0x5f248f0>
# to:
# > testData
# Object of class "Data"
# data frame with 0 columns and 0 rows
# Slot ".data":
#   <environment: 0x537d728>
#
#
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
#     x="list", # data.frame has this???
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
  .Object@.data$data=NULL # stores all well and measurement data!
  .Object@.data$plate=NULL # stores all plate data!
  # per column
  .Object@.data$colNames=NULL # stores the colnames!
  .Object@.data$colLevel=NULL # contains the name of the level
  .Object@.data$colLevelNr=NULL
  # per level
  .Object@.data$level=NULL # contains the name of the level which corresponds to a column name of the level above
  .Object@.data$levelNr=NULL # contains the level 1 = measurement, 2 = well etc...
  .Object@.data$levelSize=NULL # number of rows per level
  # need to get a way to get to the right level.... and back???
  # rownames are ignored...
  .Object@.data$reservedNames=c("plate","well","measurement")
  
  return(.Object)
})




#' createFromDataFrame
#' 
#' doesnt work!
#' 
#' 
#' @export
setGeneric("createFromDataFrame", function(self,df=NULL) standardGeneric("createFromDataFrame")) 
setMethod("createFromDataFrame", signature(self = "Data"), function(self,df=NULL){
  # check if the object is a data.frame
  if(class(df)!="data.frame") stop("not a data frame")
  
  
  colNames=colnames(df)
  uniquesPerCol=data.frame()
  for(col in length(colNames)){
    uniqesPerCol[colNames[col]]=length(table(df[colNames[col]]))
  }
  print(uniqesPerCol)
  
  return(self)
})


#' addData
#' 
#' stores data in the class instance
#' if no data exist the data imported becomes the data
#' else smartbind is used to add the data
#' 
#' TODO add more plate data...
#' 
#' @export
setGeneric("addData", function(self,newData=NULL, plateName=NULL, ...) standardGeneric("addData")) 
setMethod("addData", signature(self = "Data"), function(self,newData=NULL, plateName=NULL, ...){
  # check newData names and compare them with the names currently in use
  # add new columns to existing data
  # fill those with NA for existing data
  if(is.null(self@.data$data)){ # is there already any data?
    # no! use data as new data!
    
    self@.data$data=newData
    self@.data$data$plate=rep(1,length(newData[[1]])) # create foreign keys
    
    if( is.null(plateName) ){
      # generate platename  --> plate number=row number
      self@.data$plate=data.frame(plateName=1)
    } else {
      # use plateName.... however plate still has
      self@.data$plate=data.frame(plateName=plateName)
    }
  }else{
    # yes! add data to excisting data!
    print("adding extra data not implemented yet!")
    if( is.null(plateName) ){
      # generate platename
      
    } else {
      # 
    }
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

#' updateColnames
#' 
#' recursive loop to update colnames and other column/meta data
#' 
#' TODO: add reset to last level name if list not at end of eeuh the columns...
#' TODO: remove recursives to more standard "well" model
#' TODO: add plate... and increase levelSize of well for each plate... and recalculate measurements...
#' 
#' 
#' @export
setGeneric("updateColnamesOld", function(self, path=NULL, level=NULL) standardGeneric("updateColnamesOld")) 
setMethod("updateColnamesOld", signature(self = "Data"), function(self, path=NULL, level=NULL){
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
    currentLevel="well" # default top level!
    #
    # reset meta data
    self@.data$colNames=NULL
    self@.data$colLevel=NULL
    self@.data$colType=NULL
    self@.data$level="well" # default top level is well
    self@.data$levelNr=2
    self@.data$levelSize=length(self@.data$data[[1]])
    
  } 
  
  currentLevelNames=names(eval(parse(text=currentPath)))
  
  for(i in 1:length(currentLevelNames)){
      
    dataType=eval(parse(text=paste("typeof(",currentPath,"[['",currentLevelNames[i],"']][",i,"])",sep="")))
    
#     print(currentLevelNames[i])
    
    if(dataType=="list"){
#       print("list!")
      nextPath=paste(currentPath,"$",currentLevelNames[i],"[[1]]",sep="") # the firest nested list
      
      # calculate that levels size
#       len=eval(parse(text=paste("length(",currentPath,"[['",currentLevelNames[i],"']][",i,"][[1]][[1]])",sep="")))      
      len=0
      for(j in 1:self@.data$levelSize[1]){
        len=len+eval(parse(text=paste("length(",currentPath,"[['",currentLevelNames[i],"']][[",j,"]][[1]])",sep="")))
      }  
      # add to level
      self@.data$level=append(self@.data$level,currentLevelNames[i])
      self@.data$levelNr=append(self@.data$levelNr,currentLevelNr[i])
      self@.data$levelSize=append(self@.data$levelSize,len)
        
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


#' updateColnames
#' 
#' this method is responsible for updating colnames and meta data
#' to keep the Data from working properly
#'
#' @export
setGeneric("updateColnames", function(self) standardGeneric("updateColnames")) 
setMethod("updateColnames", signature(self = "Data"), function(self){
  # TODO maybe add custom levels???
  # 
  # plate
  self@.data$level="plate"
  self@.data$levelNr=3 # measurement=1, well=2, plate=3
  self@.data$levelSize=length(self@.data$plate[[1]])
  self@.data$colNames=colnames(self@.data$plate)
  self@.data$colLevel=rep("plate",length(self@.data$plate))
  self@.data$colLevelNr=rep(3,length(self@.data$plate))
  #
  # well
  self@.data$level=append(self@.data$level,"well")
  self@.data$levelNr=append(self@.data$levelNr,2)
  self@.data$levelSize=append(self@.data$levelSize,length(self@.data$data[[1]]))
  self@.data$colNames=append(self@.data$colNames,names(self@.data$data)[!is.element(names(self@.data$data),c("measurement","plate"))])  
  self@.data$colLevel=append(self@.data$colLevel,rep("well",length(self@.data$data)-2)) # ignore col plate and measurement
  self@.data$colLevelNr=append(self@.data$colLevelNr,rep(2,length(self@.data$data)-2))
  #
  # measurement
  self@.data$level=append(self@.data$level,"measurement")
  self@.data$levelNr=append(self@.data$levelNr,1)
  size=0
  for(i in 1:length(self@.data$data$measurement)){#check each well for measurements
    size=size+length(self@.data$data$measurement[[i]][[1]])
  }
  self@.data$levelSize=append(self@.data$levelSize,size)
  self@.data$colNames=append(self@.data$colNames,names(self@.data$data$measurement[[1]]))
  self@.data$colLevel=append(self@.data$colLevel,rep("measurement",length(self@.data$data$measurement[[1]])))
  self@.data$colLevelNr=append(self@.data$colLevelNr,rep(1,length(self@.data$data$measurement[[1]])))
})  


#' as.data.frame()
#' 
#' 
#' 
#' @export
setGeneric("as.data.frame", function(self) standardGeneric("as.data.frame")) 
setMethod("as.data.frame", signature(self = "Data"), function(self){
  return(self[])
})


#' []
#' overwrite the [] function..
#' 
#' 
#' 
#' data.frame also has a DUMP slot... no clue what this does... or how to call it...
#' its probably not called... but instead filled when called... don't know its function though...
#' 
#' TODO if no measurement level??!?!?
#' 
#' Returns data as if it was a data.frame (so in many cases it returns a data.frame)
#' Unlike a date.frame this function wont repeat cols and rows, if the same row/col is requested multiple times
#' 
#' this function gives the data at the appropiate level "plate","well" or "measurement"
#' collumns of a higher level will be repeated
#' 
#' @slot x - Data
#' @slot i - row - number only
#' @slot j - column - use column number or column name
#' @slot level - use this to force the data to be repeated for the appropiate level: "plate","well","measurement" 
#'  
#' 
#' @export 
setMethod("[", signature(x = "Data", i = "ANY", j = "ANY"), function(x, i , j, ...) {
  # without "..." nargs() does not work! 
  # even if df[] you still get 2 args...  
  args <- list(...)
  level=NULL
  col=NULL
  row=NULL
  
  # check for level in input
  if(!length(args)==0){
    if(length(args)==1 & !is.null(args$level)){
      level=args$level
    } else {
      stop("invalid args given, only accepts i,j,level")
    } 
  }

  #
  # data.frame has some special behaviour
  nrOfCol=length(x@.data$colNames) 
  if(missing(i) & missing(j)){
    # df[] and df[,]
#     print("df[] or df[,]")
    # return everything
    row=NULL
    col=1:nrOfCol
#     might want to return it in its original form...
#     return(x)
  } else if(missing(i)){
    # df[,1]
#     print("df[,1]")
    row=NULL
    col=j
  } else if( (missing(j) & nargs()==2) | (missing(j) & nargs()==3 & !is.null(level)) ){
    # df[1], df[1,level=...]
#     print("df[1] or df[1,level=...]")
    # data.frame special case
    # should return column instead of row!
    row=NULL
    col=i
  } else if(missing(j)) {
    # df[1,]
#     print("df[1,]")
    row=i
    col=1:nrOfCol
  } else {
    # df[1,2]
#     print("df[1,2]")
    row=i
    col=j
  }
  

  # check col
  if(!(class(col)=="numeric" | class(col)=="integer" | class(col)=="character")){
    stop(paste("col index should be a number or char, not a: ",class(col)))
  }
  if(class(col)=="character" & length(wcol<-unique(col[!is.element(col,x@.data$colNames)]))>0 ) {
    stop(paste("columns given that do not exist:", paste(wcol, collapse=", "), "\n valid colnames are:",paste(x@.data$colNames,collapse=", "), sep=""))
  }
  if((class(col)=="numeric" | class(col)=="integer") & !all(is.element(col,1:nrOfCol))  ) {
    stop(paste("column number(s) given that does not exist!\n number(s) given:",paste(col,collapse=", "),"\n max col number in data:",nrOfCol, sep=""))
  }
  # todo add col number check..

  # make sure you don't fetch dubplicates
  col=unique(col)
  # also change to names if numbers
  if(class(col)!="character"){ 
    col=x@.data$colNames[col]
  }
  # check lowest level
  requestedColLevels=x@.data$colLevel[is.element(x@.data$colNames,col)]
  lowestLevel=min(x@.data$levelNr[is.element(x@.data$level,requestedColLevels)])

  if (!is.null(level)){
    
    highestLevel=max(x@.data$levelNr[is.element(x@.data$level,requestedColLevels)])
    
    if(class(level)=="character"){
      # check if its a valid level name
      if(any(is.element(x@.data$level,level)))
        lowestLevel=x@.data$levelNr[x@.data$level==level]
      else{
        stop(paste("level given not in: ",paste(x@.data$level,collapse=" ")," given level: ",level,sep=""))
      }
    } else if(class(level)=="numeric"| class(level)=="integer"){
      # check if its a valid level
      if(any(is.element(x@.data$levelNr,level)))
        lowestLevel=level
      else{
        stop(paste("level given not in: ",paste(x@.data$levelNr,collapse=" ")," given level: ",level,sep=""))
      }
    } else {
      stop(paste("level is of invalid class expected level name: ",paste(x@.data$level, collapse=" "),"\n or level number: ",paste(x@.data$levelNr,collapse=" "),"\n but got data of class: ",class(level),sep=""))
    }    
    
    if(missing(i)){
      # df[level=...]
      # return all data at that level anyways
      lowestLevel=level
      # make sure to only return the columns that are higher then then lowest level
      col=x@.data$colNames[x@.data$colLevelNr>=lowestLevel]
      print(col)
      print("HERE!!!")
    }else if (highestLevel<level){
      stop(paste("data requested at a level higher then the colums allow"))
    } else if (T){
      
    }
  }


  # check row
  nrOfRows=x@.data$levelSize[x@.data$levelNr==lowestLevel]
  if(!is.null(row)){
    if(!(class(row)=="numeric"|class(row)=="integer")){
      stop(paste("row index should be a number, not a: ",class(row)))
    }
    
    if(nrOfRows<row){
      stop(paste("Data only has ",nrOfRows," rows, you asked for row(s):",row))
    }
  }
  row=unique(row)
  
  print(paste("returning data at min column level:",x@.data$level[x@.data$levelNr==lowestLevel]))
    
  if(lowestLevel==3){ # plate
    if(is.null(row)){
      # whole column
      # TODO return as true data frame...
      return(x@.data$plate[,col])
    } else {
      # specific rows
      return(x@.data$plate[row,col])
    }
    
  }else if (lowestLevel==2){ # well
    # repeat plate for each well
    #
    # reserve space
    returnValue=data.frame(matrix(nrow=if(!is.null(row)){length(row)}else{x@.data$levelSize[x@.data$levelNr==lowestLevel]},ncol=length(col)))
    colnames(returnValue)=col
    for(colnr in 1:length(col)){ # for each column
      tempData=NULL
      if(requestedColLevels[colnr]=="plate"){
        # repeat for each well
        for(i in 1:length(x@.data$plate[[1]])){
          tempData=append(tempData,rep(x@.data$plate[col[colnr]],count(x@.data$data$plate,i)[[2]]))
        }
#         tempData=lapply(x@.data$data, function(x)returnValue=append(returnValue,x[[name]]))
        tempData=c(tempData,recursive=T)        
      }else if(requestedColLevels[colnr]=="well"){
        tempData=x@.data$data[[col[colnr]]]
      }else{
        stop("WEIRD ERROR !@#!")
      }
      
      if(is.null(row)){
        # whole column
        returnValue[,colnr]=tempData
      } else {
        # specific rows
        returnValue[,colnr]=tempData[row]
      }
      
    }
    return(returnValue)
    
  }else if(lowestLevel==1){ # measurement

  #   print(proc.time())
    #
    # fetch the requested data
    returnValue=data.frame(matrix(nrow=if(!is.null(row)){length(row)}else{nrOfRows},ncol=length(col)))
    colnames(returnValue)=col
    for(colnr in 1:length(col)){ # for each column
      # always first fill tempdata with the whole column (at measurement level)
      # then do the row select
      level=x@.data$colLevel[x@.data$colNames==col[colnr]]
      tempData=NULL
      if (level=="well"){
        # data at top level
        #
        # data has to be repeated for each measurement
        for (i in 1:length(x@.data$data$measurement)){ # for each measurement
          tempData=append(tempData,rep(x@.data$data[[col[colnr]]][[i]],length(x@.data$data$measurement[[i]][[1]])))
        }
      } else if(level=="measurement"){
        # get whole column
        for(i in 1:length(x@.data$data$measurement)){
          tempData=append(tempData, x@.data$data$measurement[[i]][[col[colnr]]])
        }
      } else if(level=="plate"){
        # repeat for each well*each measurement
        for(i in 1:length(x@.data$data[[1]])){ # for each well
          # get the corresponding plate values
          data=x@.data$plate[x@.data$data$plate[[i]],col[colnr]]
          tempData=append(tempData,rep(data,length(x@.data$data$measurement[[i]][[1]]))) # for each measurement
        }
        #         tempData=lapply(x@.data$data, function(x)returnValue=append(returnValue,x[[name]]))
        tempData=c(tempData,recursive=T)
      } else {
        stop("data at unknown level... this error means a coding error as it should have been cought above!")
      }
  
      if(is.null(row)){
        # whole column
        returnValue[,colnr]=tempData
      } else {
        # specific rows
        returnValue[,colnr]=tempData[row]
      }
    }
  return(returnValue)
  }

})


#' [[]]
#' overwrite the [[]] function..
#' 
#' data.frame also has a DUMP slot... no clue what this does... or how to call it...
#' its probably not called... but instead filled when called... don't know its function though...
#' 
#' STILL VERY BUGGY!!!
#' 
#' @export 
setMethod("[[", signature(x = "Data", i = "ANY", j = "ANY"), function(x, i , j, ...) {
  if(missing(i) & missing(j)){
    # df[] and df[,]
    stop("df[[]] or df[[,]] CRASH!")
  } else if(missing(i)){
    # df[,1]
    stop("df[,1] CRASH!")
  } else if(missing(j) & nargs()==2) {
    # df[1]
    temp=x[i]
    return(temp[[dim(temp)[2]]])
  } else if(missing(j)) {
    # df[1,]
    stop("df[1,] CRASH!")
  } else {
    # df[1,2]
    temp=x[i,j]
#     print(temp)
    return(temp[[dim(temp)]])
  }
  stop("I should never get here. CRASH!")
})


#' [<-
#' overwrite the []<- function..
#' 
#' 
#' todo add idea of adding data also on well level.. only if no rows are given? (so all rows)
#' 
#' 
#' @export 
setMethod("[<-", signature(x = "Data", i = "ANY", j = "ANY",value="ANY"), function(x, i, j, ..., value) {
  col=NULL
  row=NULL
  data=value
  nrOfRows=x@.data$levelSize[x@.data$level=="measurement"]
  nrOfCol=length(x@.data$colNames)
  #
  # data.frame has some special behaviour
  if(missing(i) & missing(j)){
    # df[]<- and df[,]<-
    #     print("df[] or df[,]")
    # return everything
    row=NULL
    col=1:nrOfCol
  } else if(missing(i)){
    # df[,1]<-
    #     print("df[,1]")
    row=NULL
    col=j
  } else if(missing(j) & nargs()==2) {
    # df[1]<-
    #     print("df[1]")
    # data.frame special case
    # should return column instead of row!
    row=NULL
    col=i
  } else if(missing(j)) {
    # df[1,]<-
    #     print("df[1,]")
    row=i
    col=1:nrOfCol
  } else {
    # df[1,2]<-
    #     print("df[1,2]")
    row=i
    col=j
  }
  
  

  # validate row and col...
  
  
  if (any(is.element(col,x@.data$reservedNames))){
    stop(paste("The following names are reserved for other purposes!: ",paste(x@.data$reservedNames,sep=", "), sep=""))
  }
  
  
  # analyse new input
  # the way data.frame seems to handle data that 
  # does not match the size of the rows and columns selected
  # is by if its smaller then copy it ... but only if it can be devided without rest
  # if its more... ignore the more...
  # data is filled by column, so first all rows of a column are added, then the next column...
  #
  #
  
  # adding data.frames (and matrices??) need the right amount of rows and cols
  # what about lists???
  # if you use df[] and you add something way bigger, 
  # it will keep the df the same size, and throw a bunch of warnings
  if(class(value)=="data.frame"){
    if( is.null(row) && (nrOfRows%%dim(value)[1]>0) ){
      stop(paste("Data only has: ",nrOfRows," but new data has: ", dim(value)[1]," rows.", sep=""))
    }
    
    if((length(row)%%dim(value)[1])>0 ){
      stop(paste("incorrect nr of rows, asked for ",length(row),"rows, while the size of data is: ",dim(value)[1],"",,sep=""))
    }
    if((length(col)%%dim(value)[2])>0 ){
      stop(paste("incorrect nr of columns, asked for ",length(row),"cols, while the size of data is: ",dim(value)[1],"",,sep=""))
    }
    
    # change data into a big as vector to handle it uniformly down below
    data=c(value, recursive=TRUE)
  }
  
  
#   also change to names if numbers
#   do this above!!! when checking... also add col>collen+1 stop col==collen+1 add
#   if(class(col)!="character"){ 
#     temp=x@.data$colNames[col]
#     
#   } #todo add 1 more...
#   
  
  
  currentDataIndex=1
  for(colnr in 1:length(col)){ # for each column
    # check if new
    new=is.element(col[colnr],x@.data$colNames)
    if (!new){
      level=x@.data$colLevel[x@.data$colNames==col[colnr]]    
    } else {
      # new data column!!!
      # TODO add smarter selection here!!!
      level="measurement"
    }
    
#     print(level)
#     print(col[colnr])
    tempData=NULL
    if (level=="well"){
      # data at top level
      #
      # check if data is similar for each measurement in a well...
#       for (i in 1:length(x@.data$data$measurement)){ # for each measurement
#         tempData=append(tempData,rep(x@.data$data[[col[colnr]]][[i]],length(x@.data$data$measurement[[i]][[1]])))
#       }
    } else if(level=="measurement"){
      # set whole column
      for(i in 1:length(x@.data$data$measurement)){
        tempData=append(tempData, x@.data$data$measurement[[i]][[col[colnr]]])
      }
    } else {
      stop("data at unknown level... this error means a coding error as it should have been cought above!")
    }
    
    if(is.null(row)){
      # whole column
      returnValue[,colnr]=tempData
    } else {
      # specific rows
      returnValue[,colnr]=tempData[row]
    }
    
    
    
  }

 
  return(x) 
})


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
#' this function automatically determines the data level based on row number...
#' as a consequence data is not automatically repeated if its not of the right length for new colums
#'
#'
#' @export
setMethod("$", signature(x = "Data"), function(x, name) {
  # 
  # check if the col name is valid
  if(!any(x@.data$colNames==name)){
    warning(paste("not a valid colname given, valid colnames are: ",paste(x@.data$colNames,collapse=", ")))
    return(NULL)
  }
  level=x@.data$colLevel[x@.data$colNames==name]
  if (is.null(level)){
    # remove this once i implemented $= properly
    print("ok this shouldn't happen... but it did!") # change in a warning later...
    return(x@.data$data[[name]])
  }
  
  
  if (level=="well"){
    return(x@.data$data[[name]])
  } else if(level=="measurement"){
    # data is hidden in lists in the column measurement
    returnValue=NULL
    returnValue=lapply(x@.data$data$measurement, function(x)returnValue=append(returnValue,x[[name]]))
    returnValue=c(returnValue,recursive=T)
     
#     returnValue=NULL
#     for(i in 1:length(x@.data$data$measurement)){
#       returnValue=append(returnValue,x@.data$data$measurement[[i]][[name]])
#     }  
    return(returnValue)
  } else if(level=="plate") {
    return(x@.data$plate[[name]])
  } else {
    warning("data at unknown level")
  }
  
})


#' $<-
#' overwrite the $<- function
#' 
#' if given a new column name, the data will use row number to determine the level
#' 
#' 
#' @export
setMethod("$<-", signature(x = "Data"), function(x, name, value) {
  # check if its a valid colname
  if (any(x@.data$reservedNames==name)){
    stop(paste("The following names are reserved for other purposes!: ",paste(x@.data$reservedNames,sep=", "), sep=""))
  }
    
  if(!any(x@.data$levelSize==length(value))){
    stop(paste("given rows do not match any of the levels!"))
  }
  level=x@.data$level[x@.data$levelSize==length(value)]
  
  if (level=="plate") {
    x@.data$plate[name]=value
  } else if (level=="well") {
    x@.data$data[[name]]=value
  } else if (level=="measurement") {
    index=1
    for(i in 1:length(x@.data$data$measurement)){#for each well
      len=length(x@.data$data$measurement[[i]][[name]])
      print(len)
      x@.data$data$measurement[[i]][[name]]=value[index:(index+len-1)]
      index=index+len
    }
  } else {
    stop("unknown level!!!")
  }
  
  # check if was an existing colname
  if(!any(x@.data$colNames==name)){
    updateColnames(x)
    print(paste("new column:",name," added at level:",level,sep=""))
  }

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

#' overwrite show()
#' 
#' slot needs to be named "object"
#'
#' still gives error:
#' > testData
#' Object of class "Data"
#' Error in S3Part(object, strictS3 = TRUE) : 
#'  S3Part() is only defined for classes set up by setOldCLass(), basic classes or subclasses of these:  not true of class “Data”
#' 
#' 
#' @export
setMethod("show", signature(object = "Data"), function(object) {
  print("steal the show!!!")
  print(object@.data$data)
  return(object)
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
