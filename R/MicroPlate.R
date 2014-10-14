library(gtools)
library(grofit)
library(plyr)

#
# Data stores everything!
# hopefully on different levels...
# 
# TODO: createFromDataFrame
#
# TODO:alanced endgame, all new missions, secure online play, links to Dungeon Defenders II and more!
# dim, length, +-*/ == !=
# Arith() = +-*/  ???
# Compate = == != < > etc... ???
# $ [ [[
# nrow, colnames, head, names etc
# copy <----!!!!!
# stringsAsFactors,  check.names... if i want to inherent data.frame -- make.names...
# also factors may safe memory!!!
# http://stackoverflow.com/questions/8691812/get-object-methods-r
# may also need the assignment variables like "$<-"... but i don't know if i want to give users that much access.
# 
# TODO allow plate deletion!
#
# ok... maybe make the data lockable???
# 
# more TODO:
# tab integration... microplate$ should display the list of colnames in rstudio if possible...
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
# An object of class "MicroPlate"
# Slot ".data":
#   <environment: 0x5f248f0>
# to:
# > testData
# Object of class "MicroPlate"
# data frame with 0 columns and 0 rows
# Slot ".data":
#   <environment: 0x537d728>
#
#
# DONE: change data$levelNr from 3,2,1 to 1,2,3... 
# TODO:this would allow me to remove code... ~~ partially done
#


#
#' MicroPlate
#' 
#' This class stores microplate data
#' 
#' for memory reasons everything is to be stored in an enviroment .data
#' all behaviour to acces the .data is overwritten to work with it...
#' this means that once you have created an instance of a class you can copy it 
#' and the data is still stored at only 1 location
#' 
#' columns 
#' columns with list are recorded in their own lists
#' 
#' 
#' @export
#' @include generalFunctions.R setup.R
#' @import methods gtools plyr grofit 
MicroPlate=setClass(
  Class = "MicroPlate", 
#   contains = "data.frame", # S3 S4 conflicts??? it kinda doesnt work :P
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
setMethod("initialize", "MicroPlate", function(.Object){
  ## initialize the love!
  # the core objects that store all the user data
  .Object@.data=new.env() # make sure it has its own little space
  .Object@.data$measurement=NULL # stores all measurement data!
  .Object@.data$well=NULL # stores all well data!
  .Object@.data$plate=NULL # stores all plate data!
  
  # the rest is meta data stored in a way for fast access
  # and is used by the rest of the program for checks and data access
  # the consistency of these values is enforced by the function updateMetaData
  # which should be called after each change
  #
  ## per column
  .Object@.data$colNames=NULL # stores the colnames!
  .Object@.data$colLevel=NULL # contains the name of the level
  .Object@.data$colLevelNr=NULL # contains the level number
  ## per level
  .Object@.data$level=NULL # contains the name of the level which corresponds to a column name of the level above
  .Object@.data$levelNr=NULL # contains the level 1 = measurement, 2 = well etc...
  .Object@.data$levelSize=NULL # number of rows per level
  ## per plate
  .Object@.data$measurementsPerPlate=NULL # contains the number of measurements per plate
  .Object@.data$wellsPerPlate=NULL # contains the number of wells per plate 
  #
  #
  #
  # need to get a way to get to the right level.... and back???
  # rownames are ignored...
  .Object@.data$reservedNames=c("plate","well","measurement")
  
  return(.Object)
})


#' merge
#' @rdname merge
#' @details
#' merge two MicroPlates
#' 
#' default it removes the 2nd (other)
#' 
#' @param self the MicroPlate object
#' @param other the other MicroPlate object
#' @param removeOther default behaviour is to remove the other/2nd microplate given, put FALSE to stop this.
#' 
#' @export
setGeneric("merge", function(self,other,removeOther=TRUE) standardGeneric("merge"))
#' @rdname merge
setMethod("merge", signature(self = "MicroPlate", other="MicroPlate"), function(self,other,removeOther=TRUE){
  # 
#   self@.data$colNames
#   other@.data$colNames
  
  # new means that other has them and self does not
  # missing means that other does not have them but self does
  nrOfPlates=self@.data$levelSize[3]
  nrOfWells=self@.data$levelSize[2]
  nrOfMeasurements=self@.data$levelSize[1]
  nrOfNewPlates=other@.data$levelSize[3]
  nrOfNewWells=other@.data$levelSize[2]
  nrOfNewMeasurements=other@.data$levelSize[1]
#   plateNumber=self@.data$levelSize[self@.data$level=="plate"]+1 #this needs change
  
  colsPlate=names(other@.data$plate)
  colsWell=names(other@.data$well)
  colsMeasurement=names(other@.data$measurement)
  newColsPlate=colsPlate[!(colsPlate %in% self@.data$colNames[self@.data$colLevel=="plate"])]
  newColsWell=colsWell[!(colsWell %in% self@.data$colNames[self@.data$colLevel=="well"])]
  newColsMeasurement=colsMeasurement[!(colsMeasurement %in% self@.data$colNames[self@.data$colLevel=="measurement"])]
  missingColsPlate=self@.data$colNames[self@.data$colLevel=="plate"][!(self@.data$colNames[self@.data$colLevel=="plate"] %in% colsPlate)]
  missingColsWell=self@.data$colNames[self@.data$colLevel=="well"][!(self@.data$colNames[self@.data$colLevel=="well"] %in% colsWell)]
  missingColsMeasurement=self@.data$colNames[self@.data$colLevel=="measurement"][!(self@.data$colNames[self@.data$colLevel=="measurement"] %in% colsMeasurement)]
  existingColsPlate=colsPlate[(colsPlate %in% self@.data$colNames[self@.data$colLevel=="plate"])]
  existingColsWell=colsWell[(colsWell %in% self@.data$colNames[self@.data$colLevel=="well"])]
  existingColsMeasurement=colsMeasurement[(colsMeasurement %in% self@.data$colNames[self@.data$colLevel=="measurement"])]


  # plate
  if(length(newColsPlate)>0){
    for(i in 1:length(newColsPlate)){
      # create new column
      # fill existing plate columns with NA and add the new data
      self@.data$plate[[newColsPlate[i]]]=append(rep(x=NA,nrOfPlates),other@.data$plate[[newColsPlate[i]]])
    }
  }
  if(length(missingColsPlate)>0){
    for(i in 1:length(missingColsPlate)){
      # fill the existing columns for which the newData has no data with NA
      self@.data$plate[[missingColsPlate[i]]]=append(self@.data$plate[[missingColsPlate[i]]],rep(x=NA,nrOfPlates))
    }
  }
  if(length(existingColsPlate)>0){ # this should always be the case as row and column are mandatory...
    for(i in 1:length(existingColsPlate)){
      # add newData to existing columns
      self@.data$plate[[existingColsPlate[i]]]=append(self@.data$plate[[existingColsPlate[i]]],other@.data$plate[[existingColsPlate[i]]])
    }
  }


  # Well
  for(i in 1:length(newColsWell)){ # measurement is seen as a new column
    # create new column
    # fill the existing wells with NA and add the new data
    
    # note that: newColsWell contains measurement!
    if(newColsWell[i]=="measurement"){
      # starting positions of other are increased by nrOfMeasurements of self
      self@.data$well$measurement=append(self@.data$well$measurement,(other@.data$well$measurement+self@.data$levelSize[1]))
    } else if(newColsWell[i]=="plate"){
      # plate number reference
      # plate numbers need to match row numbers, so you cannot use the row numbers from other.
      # the row numbers need to continue from the last self plate row number
      for(i in 1:nrOfNewPlates){
        # get number of wells per plate
        #     nrOfWellsPerPlate=sum(other@.data$data$plate==i) 
        self@.data$well$plate=append(self@.data$well$plate, rep(x=nrOfPlates+i,other@.data$wellsPerPlate[[i]]))
      }
    } else {
      self@.data$well[[newColsWell[i]]]=append(rep(x=NA,nrOfWells),other@.data$well[[newColsWell[i]]])
    }
  }
#   print(paste("missingColsWell",missingColsWell))
  if(length(missingColsWell)>0){
    for(i in 1:length(missingColsWell)){
      # fill the existing columns for which the newData has no data with NA
      self@.data$well[[missingColsWell[i]]]=append(self@.data$well[[missingColsWell[i]]],rep(x=NA,nrOfNewWells))
    }
  }
#   print(paste("existingColsWell",existingColsWell))
  if(length(existingColsWell)>0){ # this should always be the case as row and column are mandatory...
    for(i in 1:length(existingColsWell)){
      # add newData to existing columns
      self@.data$well[[existingColsWell[i]]]=append(self@.data$well[[existingColsWell[i]]],other@.data$well[[existingColsWell[i]]])
    }
  }

  # measurement
  if(length(newColsMeasurement)>0){
    for(i in 1:length(newColsMeasurement)){
      # create new column
      # fill existing columns with NA and add the new data
      self@.data$measurement[[newColsMeasurement[i]]]=append(rep(x=NA,nrOfMeasurements),other@.data$measurement[[newColsMeasurement[i]]])
    }
  }
  if(length(missingColsMeasurement)>0){
    for(i in 1:length(missingColsMeasurement)){
      # fill the existing columns for which the newData has no data with NA
      self@.data$measurement[[missingColsMeasurement[i]]]=append(self@.data$measurement[[missingColsMeasurement[i]]],rep(x=NA,nrOfNewMeasurements))
    }
  }
  if(length(existingColsMeasurement)>0){
    for(i in 1:length(existingColsMeasurement)){
      # add newData to existing columns
      self@.data$measurement[[existingColsMeasurement[i]]]=append(self@.data$measurement[[existingColsMeasurement[i]]],other@.data$measurement[[existingColsMeasurement[i]]])
    }
  }
  
  
  # remove other
  if(removeOther){
    rm(list=deparse(substitute(other)),envir=sys.frame(-2))
  }
  
  updateMetaData(self)
  
  return(self)
})


#' updateMetaData
#' @rdname updateMetaData
#' @keywords internal 
#' @description
#' this method is responsible for updating colnames and meta data
#' to keep the Data from working properly
#' 
#' TODO make it so that the level meta data is sorted! --DONE!!!
#' this allowes the rest of the code to be optimized a bit more...
#' self@@.data$levelSize[self@@.data$level=="plate"] would become self@@.data$levelSize[3] -- not done yet!
#' measurement=1, well=2, plate=3
#' 
#' 
#' @param self the microplate object
#' 
#' 
#' @export
#' @import plyr
setGeneric("updateMetaData", function(self) standardGeneric("updateMetaData"))
#' @rdname updateMetaData
setMethod("updateMetaData", signature(self = "MicroPlate"), function(self){
  # measurement
  self@.data$level="measurement"
  self@.data$levelNr=1 # measurement=1, well=2, plate=3
  self@.data$levelSize=length(self@.data$measurement[[1]])
  self@.data$colNames=names(self@.data$measurement)
  self@.data$colLevel=rep("measurement",length(self@.data$measurement))
  self@.data$colLevelNr=rep(1,length(self@.data$measurement))
  #
  # well
  self@.data$level=append(self@.data$level,"well")
  self@.data$levelNr=append(self@.data$levelNr,2)
  self@.data$levelSize=append(self@.data$levelSize,length(self@.data$well[[1]]))
  self@.data$colNames=append(self@.data$colNames,names(self@.data$well)[!is.element(names(self@.data$well),c("measurement","plate"))])  
  self@.data$colLevel=append(self@.data$colLevel,rep("well",length(self@.data$well)-2)) # ignore col plate and measurement
  self@.data$colLevelNr=append(self@.data$colLevelNr,rep(2,length(self@.data$well)-2))
  #
  # plate
  self@.data$level=append(self@.data$level,"plate")
  self@.data$levelNr=append(self@.data$levelNr,3) # measurement=1, well=2, plate=3
  self@.data$levelSize=append(self@.data$levelSize,length(self@.data$plate[[1]]))
  self@.data$colNames=append(self@.data$colNames,names(self@.data$plate))
  self@.data$colLevel=append(self@.data$colLevel,rep("plate",length(self@.data$plate)))
  self@.data$colLevelNr=append(self@.data$colLevelNr,rep(3,length(self@.data$plate)))
  # wellsPerPlate
  self@.data$wellsPerPlate=plyr::count(self@.data$well$plate)[[2]]
  # measurementsPerPlate
  
  currentWellNr=1
  nextWellNr=1
  self@.data$measurementsPerPlate=NULL
  for(i in 1:length(self@.data$plate[[1]])){# for each plate
    currentWellNr=nextWellNr
    nextWellNr=currentWellNr+self@.data$wellsPerPlate[i]# nrOfWells
    nrOfMeasurement=0
    if(!is.na(self@.data$well$measurement[nextWellNr])){
      nrOfMeasurement=self@.data$well$measurement[nextWellNr]-self@.data$well$measurement[currentWellNr]
    }else{
      # last well
      nrOfMeasurement=length(self@.data$measurement[[1]])-self@.data$well$measurement[currentWellNr]+1
    }
    self@.data$measurementsPerPlate=append(self@.data$measurementsPerPlate,nrOfMeasurement)
  }
})


#' []
#' overwrite the [] function..
#'
#' 
#' @description
#' Returns data as if it was a data.frame (so in many cases it returns a data.frame)
#' Unlike a date.frame this function wont repeat cols and rows, if the same row/col is requested multiple times
#' 
#' this function gives the data at the appropiate level "plate","well" or "measurement"
#' collumns of a higher level will be repeated
#' 
#' @note
#' data.frame also has a DUMP slot... no clue what this does... or how to call it...
#' its probably not called... but instead filled when called... don't know its function though...
#' 
#' 
#' @param x MicroPlate
#' @param i row - number only
#' @param j column - use column number or column name
#' @param ... you can use the argument "level" to force the data to be repeated for the appropiate level: "plate","well","measurement"  other uses of ... will throw errors.
#'
#' 
#' @export 
setMethod("[", signature(x = "MicroPlate", i = "ANY", j = "ANY"), function(x, i , j, ...) {
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
  } else if( ( missing(j) & nargs()==2 ) | ( missing(j) & nargs()==3 & !is.null(level) ) ){
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
  
  # row logical to number conversion
  if(is.logical(row)){
    if(length(row)%in%x@.data$levelSize){
      if(missing(j)){ # only select column of the right level
        # only select cols that belong to that level
        lowestLevel=which(length(row)==x@.data$levelSize)
        col=col[x@.data$colLevelNr>=lowestLevel] # note that col will contain all cols, so i dont have to match
      }
      row=(1:length(row))[row]
    } else {
      stop(paste("row arugment was logical and not of the same length as any of the levels:", paste(x@.data$levelSize)))
    }
  }


  # check col
  if(!(class(col)=="numeric" | class(col)=="integer" | class(col)=="character") ){
    stop(paste("col index should be a number or char, not a: ",class(col)))
  }

  if(class(col)=="character" & length(wcol<-unique(col[!is.element(col,x@.data$colNames)]))>0 ) {
    stop(paste("columns given that do not exist:", paste(wcol, collapse=", "), "\n valid colnames are:",paste(x@.data$colNames,collapse=", "), sep=""))
  }
  if((class(col)=="numeric" | class(col)=="integer") & !all(is.element(col,1:nrOfCol))  ) {
    stop(paste("column number(s) given that does not exist!\n number(s) given:",paste(col,collapse=", "),"\n max col number in data:",nrOfCol, sep=""))
  }

  # todo add col number check..

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
      if(any(is.element(x@.data$level,level))){
        lowestLevel=x@.data$levelNr[x@.data$level==level]
      } else {
        stop(paste("level given not in: ",paste(x@.data$level,collapse=" ")," given level: ",level,sep=""))
      }
    } else if(class(level)=="numeric"| class(level)=="integer"){
      # check if its a valid level
      if(any(is.element(x@.data$levelNr,level))){
        lowestLevel=level
      } else {
        stop(paste("level given not in: ",paste(x@.data$levelNr,collapse=" ")," given level: ",level,sep=""))
      }
    } else {
      stop(paste("level is of invalid class expected level name: ",paste(x@.data$level, collapse=" "),"\n or level number: ",paste(x@.data$levelNr,collapse=" "),"\n but got data of class: ",class(level),sep=""))
    }    
    
#     print(highestLevel)
#     print(level)
    
    if(missing(i)){
      # df[level=...]
#       # return all data at that level anyways
#       lowestLevel=level
      # make sure to only return the columns that are higher then then lowest level
      col=x@.data$colNames[x@.data$colLevelNr>=lowestLevel]
#       print(col)
#       print("HERE!!!")
    }else if (highestLevel<lowestLevel){
      stop(paste("data requested at a level higher then the colums allow"))
    } else if (T){
      
    }
  }
  
  
  # check row
  nrOfRows=x@.data$levelSize[x@.data$levelNr==lowestLevel]
  if(!is.null(row)){
    if(!(class(row)=="numeric" | class(row)=="integer" | is.logical(row) )){
      stop(paste("row index should be a number, not a: ",class(row)))
    }  
#     if(nrOfRows<max(row)){
#       stop(paste("Data only has ",paste(nrOfRows, sep="",collapse=" ")," rows, you asked for row(s):",paste(row, sep="",collapse=" ")))
#     }
#     print(row)
    if(max(row)>nrOfRows){
      stop(paste("Asked for row number ",max(row)," while the level only has ",x@.data$levelSize[x@.data$levelNr==lowestLevel]," rows",sep=""))
    }
    if(length(row)==0){
      warning("length of row was 0, select atleast 1 or more rows")
      return(NULL)
    }
    
  }
#   row=unique(row)


#   print(paste("returning data at min column level:",x@.data$level[x@.data$levelNr==lowestLevel]))
#   print(col)
  if(lowestLevel==3){ # plate
    if(is.null(row)){
      # whole column
      return(as.data.frame(x@.data$plate, stringsAsFactors=F)[col])
    } else {
      # specific rows
      return(as.data.frame(x@.data$plate, stringsAsFactors=F)[row,col])
    }
    
  }else if (lowestLevel==2){ # well
    # repeat plate for each well
    #
    # reserve space
    returnValue=data.frame(matrix(nrow=if(!is.null(row)){length(row)}else{x@.data$levelSize[x@.data$levelNr==lowestLevel]},ncol=length(col)), stringsAsFactors=F)
    colnames(returnValue)=col
    for(colnr in 1:length(col)){ # for each column
      level=x@.data$colLevel[x@.data$colNames==col[colnr]]
      tempData=NULL
      if(level=="plate"){
        # repeat for each well
        for(i in 1:length(x@.data$plate[[1]])){
          tempData=append(tempData,rep(x@.data$plate[[col[colnr]]][i],count(x@.data$well$plate)[[2]][i]))
        }
#         tempData=lapply(x@.data$data, function(x)returnValue=append(returnValue,x[[name]]))
        tempData=c(tempData,recursive=T)        
      }else if(level=="well"){
        tempData=x@.data$well[[col[colnr]]]
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
#     print("here?")
  #   print(proc.time())
    #
    # fetch the requested data
    returnValue=data.frame(matrix(nrow=if(!is.null(row)){length(row)}else{nrOfRows},ncol=length(col)),stringsAsFactors = FALSE)
    colnames(returnValue)=col
    for(colnr in 1:length(col)){ # for each column
      # always first fill tempdata with the whole column (at measurement level)
      # then do the row select
      level=x@.data$colLevel[x@.data$colNames==col[colnr]]
      tempData=NULL
      if(level=="measurement"){
        # get whole column
        tempData=x@.data$measurement[[col[colnr]]]
      } else if(level=="well"){
        # data at top level
        #
        # data has to be repeated for each measurement
        for (i in 1:length(x@.data$well$measurement)){ # for each well
          nrOfMeasurement=0
          if(!is.na(x@.data$well$measurement[i+1])){
            nrOfMeasurement=x@.data$well$measurement[[i+1]]-x@.data$well$measurement[[i]]
          }else{
            # last well
            nrOfMeasurement=length(x@.data$measurement[[1]])-x@.data$well$measurement[[i]]+1
          }
          tempData=append(tempData,rep(x@.data$well[[col[colnr]]][[i]],nrOfMeasurement))
        }
      } else if(level=="plate"){
        # repeat for eachWell*eachMeasurement
        for(i in 1:x@.data$levelSize[3]){ # for each plate
          # get the corresponding plate values
          tempData=append(tempData,rep(x@.data$plate[[col[colnr]]][[i]],x@.data$measurementsPerPlate[[i]])) # for each measurement
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
  return(returnValue)
  }

})


#' [[]]
#' overwrite the [[]] function..
#' @description
#' data.frame also has a DUMP slot... no clue what this does... or how to call it...
#' its probably not called... but instead filled when called... don't know its function though...
#' 
#' STILL VERY BUGGY!!!
#' 
#' @param x the MicroPlate object
#' @param i row selection
#' @param j column selection
#' @param ... you can use the argument "level" to force the data to be repeated for the appropiate level: "plate","well","measurement"  other uses of ... will throw errors.
#' 
#' @export 
setMethod("[[", signature(x = "MicroPlate", i = "ANY", j = "ANY"), function(x, i , j, ...) {
  args <- list(...)
  level=NULL
      
  # check for level in input
  if(!length(args)==0){
    if(length(args)==1 & !is.null(args$level)){
      level=args$level
    } else {
      stop("invalid args given, only accepts i,j,level")
    }
  }

  if(missing(i) & missing(j)){
    # df[] and df[,]
    stop("df[[]] or df[[,]] CRASH!")
  } else if(missing(i)){
    # df[,1]
    stop("df[,1] CRASH!")
  } else if( ( missing(j) & nargs()==2 ) | ( missing(j) & nargs()==3 & !is.null(level) ) ){
    # df[1], df[1,level=...]
    if(is.null(level)){
      temp=x[i]
    } else {
      temp=x[i,level=level]
    }
    return(temp[[dim(temp)[2]]])
  }else if(missing(j)) {
    # df[1,]
    stop("df[1,] CRASH!")
  } else {
    # df[1,2]
    if(is.null(level)){
      temp=x[i,j]
    }else{
      temp=x[i,j,level=level]
    }
    return(temp[[dim(temp)]])
  }
  stop("I should never get here. CRASH!")
})


#' removeColumn
#' @rdname removeColumn
#' @description
#' remove the column with the given colname
#' 
#' @param self the MicroPlate object
#' @param colNames the names of the columns you want to delete
#' 
#' @export
setGeneric("removeColumn", function(self, colNames) standardGeneric("removeColumn"))
#' @rdname removeColumn
setMethod("removeColumn", signature(self = "MicroPlate" ), function(self, colNames) {
  col=colNames
  
  # check col
  if(!(class(col)=="numeric" | class(col)=="integer" | class(col)=="character")){
    stop(paste("col index should be a number or char, not a: ",class(col)))
  }  
  
  # also change to names if numbers
  if(class(col)!="character"){
    # check if its a valid column number
    if(max(col)>length(self@.data$colNames)){
      stop(paste("microplate only has ", length(self@.data$colNames) ," columns, asked for column ", max(col), sep="",collapse=""))
    }
    col=self@.data$colNames[col]
  }
  
  # check valid column names
  if(class(col)=="character" & length(wcol<-unique(col[!is.element(col,self@.data$colNames)]))>0 ) {
    stop(paste("columns given that do not exist:", paste(wcol, collapse=", "), "\n valid colnames are:",paste(self@.data$colNames,collapse=", "), sep=""))
  }
  
  # check for reserved names
  if (any(is.element(col,self@.data$reservedNames))){
    stop(paste("The following names are reserved for other purposes!: ",paste(self@.data$reservedNames,sep=", "), sep=""))
  }
  
  
  
  for(i in 1:length(col)) {
    # determine level
    level=self@.data$colLevel[self@.data$colNames==col[i]]
    
    if (level=="plate") {
      self@.data$plate[[col[i]]]=NULL
    } else if(level=="well") {
      self@.data$well[[col[i]]]=NULL
    } else if (level=="measurement") {
      self@.data$measurement[[col[i]]]=NULL
    }
  }
  # restore the balance
  updateMetaData(self)
  return(self)
})


#' [<-
#' overwrite the []<- function..
#' 
#' @description
#' NOTES:
#' only allows data of the same level to be added (so no adding well and measurement level data in one go)
#' this means no new rows can be created!!!! for now...
#' maybe an exception will be made if all variables of the row are given (need at least platenr/name)
#' 
#' differences with data.frame:
#' - data given has to be of the correct size!
#'   this function will not repeat your data!
#' - this function does allow you to create new rows!
#' - multiple collumns are only allowed if given a matrix or data.frame as input
#' - ignores all names in a data.frame
#' - it is possible to add a new column even if you do not give a value for all rows
#'   the remaining rows will be filled with NA
#' - new columns have to be named within the brackets!!
#'   all names in the value slot are ignored!   
#' - this function does not allow you to change the level of a colname
#'   if you want this done you would need to first delete that column using df[1]=NULL
#' - no negative indexing (matlab version is way better anyways)
#'  
#' todo df[1]=NULL
#' todo also allow rows to be deleted?
#' 
#' todo: testData["content"]=1:12 -- content remains character instead of number/int
#' 
#' todo: better list vector support on plate level?
#' 
#' todo better row check... it is now pretty much assumed that user gives proper row numbers..
#' which is a silly thing to assume... use unique / max / min / interger
#' 
#' @param x the MicroPlate object
#' @param i row selection
#' @param j column selection
#' @param value the value to replace or add, with =NULL you can remove a column
#' @param ... you can use the argument "level" to force the data to be repeated for the appropiate level: "plate","well","measurement"  other uses of ... will throw errors.
#' 
#' @export 
setMethod("[<-", signature(x = "MicroPlate", i = "ANY", j = "ANY",value="ANY"), function(x, i, j, ..., value) {
  args <- list(...)
  col=NULL
  row=NULL
#   data=value
  dataLength=NULL
  level=NULL
  
  # check for level in input
  if(!length(args)==0){
    if(length(args)==1 & !is.null(args$level)){
      level=args$level
      if(length(level)>1){
        stop(paste("level may only contain 1 of the following values: ", x@.data$level ,sep="",collapse=" "))
      }
    } else {
      stop("invalid args given, only accepts i,j,level,value")
    }
  }
  
  # check if the level exist and convert to number levels if they were string levels
  if(!is.null(level)){
    if(class(level)=="character"){
      # check if its a valid level name
      if(any(is.element(x@.data$level,level)))
         level=x@.data$levelNr[x@.data$level==level]
      else{
        stop(paste("level given not in: ",paste(x@.data$level,collapse=" ")," given level: ",level,sep=""))
      }
    } else if(class(level)=="numeric"| class(level)=="integer"){
      # check if its a valid level
      if(any(is.element(x@.data$levelNr,level))){
         # level=level
      } else {
        stop(paste("level given not in: ",paste(x@.data$levelNr,collapse=" ")," given level: ",level,sep=""))
      }
    } else {
      stop(paste("level is of invalid class expected level name: ",paste(x@.data$level, collapse=" "),"\n or level number: ",paste(x@.data$levelNr,collapse=" "),"\n but got data of class: ",class(level),sep=""))
    }
  }
  
  
#   nrOfRows=x@.data$levelSize[x@.data$level=="measurement"]
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
  } else if( ( missing(j) & nargs()==3 ) | ( missing(j) & nargs()==4 & !is.null(level) ) ){
#   } else if(missing(j) & nargs()==2) {
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
  


  # check col
  if(!(class(col)=="numeric" | class(col)=="integer" | class(col)=="character")){
    stop(paste("col index should be a number a char, not a: ",class(col)))
  }
  if(length(col)!=length(unique(col))){
    stop("duplicate columns selected")
  }
  
  # check if its a column remove df[names]=NULL
  if(is.null(value)){
    if(is.null(row)){
      return(removeColumn(x,col)) 
    }
    else{
      stop("you cannot delete rows or individual values")
    }
  }
  
  # check row
  if(!is.null(row)){
    if(!(class(row)=="numeric" | class(row)=="integer" | class(row)=="logical")){
      stop(paste("row index should be a number or a logical, not a: ",class(row)))
    }
    if(is.logical(row)){
      row=(1:length(row))[row] # convert it for now determine if it is valid later after level has been determined
    }else{
      if(length(row)!=length(unique(row))){
        stop("duplicate rows selected")
      }
    }
  }
  # also change to names if numbers
  if(class(col)!="character"){
    col=x@.data$colNames[col]
  }
  
  if (any(is.element(col,x@.data$reservedNames))){
    stop(paste("The following names are reserved for other purposes!: ",paste(x@.data$reservedNames,sep=", ",collapse  = " "), sep=""))
  }
  


#   print(col)

  
  # TODO check row
  
  
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
  # it will keep the df the same size, and throw a bunch of warning
  if(class(value)=="matrix"){
    value=data.frame(value, stringsAsFactors=F) # dont want to deal with this crap seperatly!
  }
  if(class(value)=="data.frame"){
#     if( is.null(row) && (nrOfRows%%dim(value)[1]>0) ){
#       stop(paste("Data only has: ",nrOfRows," but new data has: ", dim(value)[1]," rows.", sep=""))
#     }
#     
#     if((length(row)%%dim(value)[1])>0 ){
#       stop(paste("incorrect nr of rows, asked for ",length(row),"rows, while the size of data is: ",dim(value)[1],"",,sep=""))
#     }
#     if((length(col)%%dim(value)[2])>0 ){
#       stop(paste("incorrect nr of columns, asked for ",length(row),"cols, while the size of data is: ",dim(value)[1],"",,sep=""))
#     }
    
    if(!is.null(row)){
      if((length(row)!=dim(value)[1])){
        stop(paste("incorrect nr of rows, selected ",length(row)," rows, while the size of data is: ",dim(value)[1],"",sep=""))
      }
    }
    if((length(col)!=dim(value)[2])){
      stop(paste("incorrect nr of columns, selected ",length(col)," cols, while the size of data is: ",dim(value)[2],"",sep=""))
    }
    dataLength=dim(value)[1]
        
#     # change data into a big as vector to handle it uniformly down below
#     data=c(value, recursive=TRUE) # this converts everything into chars?
  } else if (any(class(value) %in% c("character","numeric","integer"))) {
    
    if (length(col)!=1) {
      if(length(col)==length(value)){
        # a vector with 1 value for each column
#         value=data.frame......
        dataLength=length(value)
        value=data.frame(value,stringsAsFactors = F)
        value=data.frame(t(value),stringsAsFactors = F) 
        # transposing a data.frame, produces a matrix!!! FUCK YOU R!
        # also it should be a 90degree flip, but apperently R doesnt have that by default?
        #TODO CHECK NAMES
#         # ignore vector names... and make sure thing can get copied
#         colnames(value)=col
      }else{
        stop("multiple columns given, while only a single dimensional data")
      }
    }
    
    # todo []
    if(!is.null(row)){
      if ( (length(row)*length(col))!=length(value) ){
        if(length(value)!=1){
          stop(paste("invalid number of rows given:",length(value),"rows selected:",length(row)))
        }
        else{
          # make sure the selection size = value size
          value=rep(value,length(row))
        }
      }
    }
    
    # vector data
    dataLength=length(value)
    
    
  } else if(is.null(class(value))) {
    # delete columns!
    stop("mmmmmh why is this even here, removal of coluns is done above.... please delete this elseif thingy...")
  } else {
    stop(paste("data type of class: ",class(value)," not supported", sep="",collapse=""))
  }
  
  
  #
  #  
  # determine level
  # check if all columns are either new or of the same level...
  colLevel=unique(x@.data$colLevelNr[x@.data$colNames %in% col])
#   print(colLevel)
  if(length(colLevel)>1){
    stop("the changing of different levels of data is not allowed! ")
  } else if (length(colLevel)==1) { # specific columns selected
    # df["wellColumn1"]=value # min
    # df[1:5,c("wellColumn1","wellColumn2", "newColumn"),level="well"]=value # max
    # also with rows and stuff
    # or its variants with or without rows/plate level and row info like:
    # df[c("wellColumn1","wellColumn2")]=value 
    # df[c("wellColumn1","wellColumn2"),level="well"]
    # df[c("wellColumn1","wellColumn2", "newColumn")]
    # df[c("wellColumn1","wellColumn2", "newColumn"),level="well"]
    #
    # the only way to enter multiple column data is by using data.frames / matrices
    #
    # if the rows were not specified, the row dim could not have been checked before
    if (is.null(row) & (dataLength!=x@.data$levelSize[x@.data$levelNr==colLevel]) ) {
      if(dataLength==1){
        # single value repeat is allowed!
        value=rep(value,x@.data$levelSize[x@.data$levelNr==colLevel])
        dataLength=length(value)
      }else if(class(value)=="data.frame"){
        # a vector/data.frame with cols equal to the colsize
      }else{
        stop(paste("no rows given and so rows expected to be equal to level size: ",x@.data$levelSize[x@.data$levelNr==colLevel]," rows supplied: ",dataLength,sep=""))
      }
    }
#     #
#     if( dataLength>x@.data$levelSize[x@.data$levelNr==colLevel] ){
#       stop("the data does not have the correct amount of rows for the excisting column")
#     }
    
    #
    # check if there are any newColumns
    newColumns=col[!(col %in% x@.data$colNames)]
    if(length(newColumns)>0){
      # there are new columns selected!
      # this means there are atleast 2 columns
      # df[c("wellColumn1","newColumn")]=value # min
      # df[1:5,c("wellColumn1","wellColumn2","newColumn"),level="well"]=value # max
      
      #TODO ADD LEVEL STUFF HERE!!
      level=colLevel
      
    } else {
      # there are no new columns selected!
      # df["wellColumn1"]=value # min
      # df[1:5,c("wellColumn1","wellColumn2"),level="well"]=value # max
      #
      # check if the level argument was given
      if(!is.null(level)){
        # level argument was given
        # df["wellColumn1",level="well"]=value # min
        #
        # check if level argument=wellColumn
        if(colLevel!=level) {
          stop(paste("selected and argument level do not match, selected: ",colLevel," level argument: ", x@.data$level[x@.data$levelNr==level] ,collapse=" ",sep=""))
        }
        # rows where already checked
      } else {
        # level argument was not given
        # df["wellColumn1"]=value # min
        # df[1:5,c("wellColumn1","wellColumn2")]=value # max
        # rows where already checked
#         level=x@.data$levelNr[x@.data$level==colLevel]
        level=colLevel
      }
    } # new columns? 
  } else { # no known col names so all colnames are new
    # df["newColumn"]=value # min
    # df[1:5,c("newColumn","newColumn"),level="well"]=value # max
    # note that #df[]=value would have been converted to:
    # .. df[allColumnNamesHere] ... which would have made it a multi level operation
    # and those would have triggered an error message above
    # 
    #
    #
    # check if a level argument is given
    if(!is.null(level)){ # level argument given
      # df["newColumn",level="well"]=value # min
      # df[1:5,c("newColumn","newColumn"),level="well"]=value # max
      #
      #
      # check if rows match the level
      if( dataLength>x@.data$levelSize[x@.data$levelNr==level] ){
        if(dataLength==length(col)){
          #...
        }else{
          stop("you want to insert more rows into a level then there are rows in that level")
        }
      }
      # TODO test more exceptions
      
      # else the level is already defined... so thats good...
    } else { # no level argument given...
      # df["newColumn"]=value # min
      # df[1:5,c("newColumn","newColumn")]=value # max
      #
      # only interested in case rows were not given
      if(is.null(row)){
        # check if the data rows match a level and if so assign that level    
        if(length(x@.data$level[x@.data$levelSize==dataLength])>0){
          # check if multiple levels have the same size???? -- take the biggest
          level=x@.data$levelNr[x@.data$levelNr==max(x@.data$levelNr[x@.data$levelSize==dataLength])]
        } else if(length(col)==dataLength){
          # new single row with multiple columns...
          # TODO
          # check logic: this is allowed if level is given... else not...????
          # why would level be important here?? only if levels have the same size could that be important..
          # dim(value) should be the decider here...       
          # still the no level is better then the one directly bellow
        } else {
          stop (paste("the amount of rows given: ", dataLength ,"  does not match any of the data level sizes: ", paste(x@.data$levelSize,collapse=" ",sep=" ") ,collapse=" ",sep=""))
        }
      } else {
        # no way to determine at what level the new data has to be!
        # df[1:5,"newColumn"]=value
        stop("data level not specified, use syntax: df[1:5,'newColname',level='well']=data")
      } 
    }
    # check if level is given as an argument
    if(is.null(level)){
      stop("all new columns, and no level given... please use the level argument df['newColumn', level='well']")
    }
  }

  
  allRowsSelected=F
  if (is.null(row)){
    row=1:x@.data$levelSize[x@.data$levelNr==level]
    allRowsSelected=T
  }
  notSelectedRows=(1:x@.data$levelSize[x@.data$levelNr==level])[!((1:x@.data$levelSize[x@.data$levelNr==level]) %in% row )]

  # check if rows are within bounds
  if(max(row)>x@.data$levelSize[x@.data$levelNr==level]){
    stop(paste("Asked for row number ",max(row)," while the level only has ",x@.data$levelSize[x@.data$levelNr==level]," rows",sep=""))
  }


  # TODO check if col cannot be resorted ... i do use unique..
  # mmmh maybe add a check and stop there...
  # add/change the data
  for(colnr in 1:length(col)){ # for each column
    # check if new column
    newColumn=!is.element(col[colnr],x@.data$colNames)
#     print(row)
#     print(col)
#     print(level)
    
    if (level==3){ # plate 
      if(class(value)=="data.frame"){
#         x@.data$plate[[col[colnr]]][row]=value[[col[colnr]]] 
        x@.data$plate[[col[colnr]]][row]=value[[colnr]]
      }else{
        x@.data$plate[[col]][row]=value
      }
      if(!allRowsSelected & newColumn){
        x@.data$plate[[col[colnr]]][notSelectedRows]=NA #this is repeated if needed
      }
    } else if (level==2){ # well
      if(class(value)=="data.frame"){
        x@.data$well[[col[colnr]]][row]=value[[colnr]]
      }else{
        x@.data$well[[col]][row]=value
      }
      if(!allRowsSelected & newColumn){
        x@.data$well[[col[colnr]]][notSelectedRows]=NA #this is repeated if needed
      }
    } else if(level==1){ # measurement
      
      if(class(value)=="data.frame"){
        x@.data$measurement[[col[colnr]]][row]=value[[colnr]]
      }else{
        x@.data$measurement[[col]][row]=value
      }
      if(!allRowsSelected & newColumn){
        x@.data$measurement[[col[colnr]]][notSelectedRows]=NA #this is repeated if needed
      }
    } else {
      stop("data at unknown level... this error means a coding error as it should have been cought above!")
    }
#     
#     if(is.null(row)){
#       # whole column
#       returnValue[,colnr]=tempData
#     } else {
#       # specific rows
#       returnValue[,colnr]=tempData[row]
#     }
#     
#     
    
  }

  updateMetaData(x) # TODO maybe only if new cols?
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
#' @description
#' this function automatically determines the data level based on row number...
#' as a consequence data is not automatically repeated if its not of the right length for new colums
#'
#' @param x the MicroPlate object
#' @param name the name of the column you want returned
#' 
#' @export
setMethod("$", signature(x = "MicroPlate"), function(x, name) {
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
    return(x@.data$well[[name]])
  }
  
  # return the column
  if (level=="well"){
    return(x@.data$well[[name]])
  } else if(level=="measurement"){
    return(x@.data$measurement[[name]])
  } else if(level=="plate") {
    return(x@.data$plate[[name]])
  } else {
    warning("data at unknown level")
  }
  
})


#' $<-
#' overwrite the $<- function
#' 
#' @description
#' if given a new column name, the data will use row number to determine the level
#' if the row number does not equal the size of any of the data levels, an error is thrown
#' 
#' for known columns a single value can be given that will be replicated
#' =NULL will remove the column
#' 
#' 
#' @param x the MicroPlate object
#' @param name the name of the column you want to create/add to
#' @param value the new value, this should be of the correct length
#' 
#' @export
setMethod("$<-", signature(x = "MicroPlate"), function(x, name, value) {
  # check if its a valid colname
  if (any(x@.data$reservedNames==name)){
    stop(paste("The following names are reserved for other purposes!: ",paste(x@.data$reservedNames,sep=", "), sep=""))
  }
  # check if mp$name=NULL
  if(is.null(value)){
    return(removeColumn(x,name))
  }
  
  # determine level
  level=NULL
  if(any(x@.data$colNames==name)){ # is it an existing variable?
    level=x@.data$colLevel[x@.data$colNames==name]
    #
    # overwrite is more flexible in that a single value can be repeated
    levelSize=x@.data$levelSize[x@.data$levelNr==x@.data$colLevelNr[x@.data$colNames==name]]
    if(levelSize!=length(value)){
      if(length(value)==1){
        value=rep(value,levelSize)
      }else{
        stop("levelsize does not match valuesize")
      }
    }
    
  }else if(!any(x@.data$levelSize==length(value))){ # does the data size match any of the level sizes
    stop(paste("given rows do not match any of the levels!"))
  }else{
    level=x@.data$level[x@.data$levelSize==length(value)]
  }
  
  if(length(level)>1){
    # multiple levels had the same sizes...
    # add it to the highest #TODO or add it to the lowest??? that has its advantages...
    level=x@.data$level[x@.data$levelNr==max(x@.data$levelNr[x@.data$level %in% level])]
  }
    
  # check if the name matches the level size...


  
  if (level=="plate") {
    x@.data$plate[name]=value
  } else if (level=="well") {
    x@.data$well[[name]]=value
  } else if (level=="measurement") {
    x@.data$measurement[[name]]=value
  } else {
    stop("unknown level!!!")
  }
  
  # check if was an existing colname
  if(!any(x@.data$colNames==name)){
    updateMetaData(x)
    print(paste("new column:",name," added at level:",level,sep=""))
  }

  return(x)
})


#setGeneric("colnames", function(x) standardGeneric("colnames")) 
#rdname colnames
#' overwrite colnames()
#' @rdname colnames
#' @description
#' returns the column names (hiddes internal names)
#' 
#' TODO:make sure this does not overwrite data.frame/base colname function
#' 
#' @param x the MicroPlate object you want the column names from
#' 
#' @export
setMethod("colnames", signature(x = "MicroPlate"), function(x) {
  return(x@.data$colNames)
})


#' overwrite colnames<-
#' 
#' TODO: BROKEN!
#' TODO: decide if i want this funtion
#' 
#' @rdname colnames
#' @description
#' overwrite the column names 
#' 
#' 
#' @param x the MicroPlate object
#' @param value the new column names
#' 
#' @export
setMethod("colnames<-", signature(x = "MicroPlate"), function(x, value) {
  # TODO add checks! if its the same size as data... and you probably dont want to change this anyways...
  stop("no longer supported for now!")
  
  if(length(value)!=length(x@.data$colNames)){
    stop("invalid number of column names, please don't try again!")
  }
  
    
  
  warning("you are adviced not to do this!... but you already did...")
  x@.data$colNames=value
#   length(x@.data$colLevel!=2)
  names(x@.data$plate)=value[1:length(x@.data$colLevel==3)]
  names(x@.data$data)=value[length(x@.data$colLevel==3)+1:length(value)]
  # all measurements...
  
  
  #   return(x@.data$colnames)
  return(x) # for some reason i have to do this, else the instance of the class transforms into the value passed...
})


#' overwrite show()
#' 
#' @description
#' prints the data of the object in it's primative form
#' 
#' 
#' slot needs to be named "object"
#'  
#' still gives error:
#' > testData
#' Object of class "MicroPlate"
#' Error in S3Part(object, strictS3 = TRUE) : 
#'  S3Part() is only defined for classes set up by setOldCLass(), basic classes or subclasses of these:  not true of class "MicroPlate"
#' 
#' @param object the Microplate object
#' 
#' 
#' @export
setMethod("show", signature(object = "MicroPlate"), function(object) {
  print("measurement data:")
  print(object@.data$measurement)
  print("well data:")
  print(object@.data$well)
  print("plate data:")
  print(object@.data$plate)
  return(object)
})


#' overwrite print()
#' @export
#' @rdname print
#' @description
#' just calls show(x)
#' @param x the MicroPlate object
#' @param ... not used but roxygen complains without this
#' 
setGeneric("print", function(x) standardGeneric("print"))
#' @rdname print
setMethod("print", signature(x = "MicroPlate"), function(x) {
#   print("oooh you want to know my secrets???... well they are secret!!!")
#   x@.data
  return(show(x))
})


#' plotPerWell
#' @rdname plotPerWell
#' @description
#' TODO: add huge amounts of checks and stuff
#' 
#' @param self the MicroPlate object
#' 
#' @export
setGeneric("plotPerWell", function(self) standardGeneric("plotPerWell")) 
#' @rdname plotPerWell
setMethod("plotPerWell", signature(self = "MicroPlate"), function(self){
#   origenalPar=par() # backup plotting pars
  nrOfWells=self@.data$levelSize[2]
    
  for(i in 1:nrOfWells){
    
    index=getWellsMeasurementIndex(self,i)
#     print(index)
    start=index[[1]]
    end=index[[2]]
    print(start)
    print(end)
    
    plot(x=self@.data$measurement$time[start:end],y=self@.data$measurement$value[start:end],main=self@.data$well$content[[i]]) 
#     data=self@.data$measurement[selection,]
#     plot(x=data$time,y=data$value,main=self@.data$data$content[[i]]) 
  }



#   
#   par(oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))#test
#   for(i in wells){
#     data=self@.data$data$measurement[[i]]
#     plot(x=data$time,y=data$value,main=self@.data$data$content[[i]]) 
#   }
# 
#   par(origenalPar) # restore pars...



  return(self)
})


#' plotPerPlate
#' @rdname plotPerPlate
#' @description
#' TODO: add huge amounts of checks and stuff
#' todo: what if not on measurement level?
#' todo: other column selection
#' 
#' @param self the MicroPlate object
#' 
#' @export
setGeneric("plotPerPlate", function(self) standardGeneric("plotPerPlate")) 
#' @rdname plotPerPlate
setMethod("plotPerPlate", signature(self = "MicroPlate"), function(self){
#   dev.new()#dont use rstudio window
  origenalPar=par() # backup plotting pars
  nrOfPlates=self@.data$levelSize[3]
  nrOfWells=self@.data$levelSize[2]
  
  firstMeasurementNr=1
  lastMeasurementNr=0
  
  for(plateNr in 1:nrOfPlates){
    wells=(1:nrOfWells)[self@.data$well$plate==plateNr]
#     print(wells)
    #get amount of row/columns
    nrRows=max(self@.data$well$row[wells])-min(self@.data$well$row[wells])+1
    nrColumns=max(self@.data$well$column[wells])-min(self@.data$well$row[wells])+1
    
    
    # create a NA grid
#     par(mfcol=c(nrRows,nrColumns))
#     layoutMatrix=matrix(data=0,nrow=nrRows,ncol=nrColumns)
#     for(i in 1:length(wells)){
#       layoutMatrix[self@.data$well$row[wells[i]],self@.data$well$column[wells[i]]]=i
#     }
#     print(layoutMatrix)
#     print(nrRows)
#     print(nrColumns)
#     layout(mat=layoutMatrix,nrRows,nrColumns)
  
#     layout.show(length(wells)) # this takes a while
    
#     par(oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))#test

    lastMeasurementNr=lastMeasurementNr+self@.data$measurementsPerPlate[plateNr]
    index=firstMeasurementNr:lastMeasurementNr
    xlim=c(min(self@.data$measurement$time[index]),max(self@.data$measurement$time[index]))
    ylim=c(min(self@.data$measurement$value[index]),max(self@.data$measurement$value[index]))

#     print(paste("index: ",firstMeasurementNr,":",lastMeasurementNr,sep=""))
#     print(paste("xlim:",xlim,"ylim",ylim,sep=" "))
#     print("---")

    par(mfrow = c(nrRows,nrColumns), mai=c(0,0,0,0), oma=c(1,1,1,1), ann=FALSE, xaxt="n",yaxt="n" )
    for(i in wells){
      selection=getWellsMeasurementIndex(self,i)
#       print(selection)
      time=self@.data$measurement$time[selection]
      value=self@.data$measurement$value[selection]
      plot(x=time,y=value,xlim=xlim,ylim=ylim, type="l")
#       plot(x=time,y=value,main=self@.data$well$content[[i]]) 
    }#well
    firstMeasurementNr=lastMeasurementNr+1 # not so much a +1 increase as its a first=last
    
  }# plate
  suppressWarnings(par(origenalPar)) # restore pars... this can give warnings for some reason..
#   return(self)
#   return()
})


# #' microplate apply
# #' MPApply
# #' @@rdname MPApply
# #' @@description 
# #' 
# #' 
# #' TODO make it accept more complex things!
# #' maybe make it an interface to an existing apply like function
# #' TODO support formulas!!!
# #' 
# #' @@param self the MicroPlate object
# #' @@param fun the function 
# #' @@param ... ...
# #' 
# #' @@export
# setGeneric("MPApply", function(self, fun, what="value", wellNrs=NULL, forEach="time", ...) standardGeneric("MPApply"))
# #' @@rdname MPApply
# setMethod("MPApply", signature(self = "MicroPlate"), function(self, fun, what="value", wellNrs=NULL, forEach="time", ...){
# #   funcall=substitute(fun(...))
# #   x="time"
# #   y="value"
# #   
# #   # for now no input... need to studie formula first....
# #   results=list()
# #   # for each well
# #   for(i in 1:self@.data$levelSize[2]){
# #     x=self@.data$data$measurement[[i]][["time"]]
# #     y=self@.data$data$measurement[[i]][["value"]]
# #     results[i]=list(do.call(what=fun,args=list(x=x,y=y,unlist(list(...)))))
# #   }
#   
#   ### check input
#   # wellNrs
# #   if(is.null(wellNrs)) wellNrs=1:self@.data$levelSize[2] # if not specified get it for everything
# #   if(is.logical(wellNrs)){
# #     if(length(wellNrs)==self@.data$levelSize[2]){ #well
# #       wellNrs=(1:self@.data$levelSize[2])[wellNrs]
# #     }else if(length(wellNrs)==self@.data$levelSize[1]){#measurement
# #       wellNrs=(1:self@.data$levelSize[1])[wellNrs]
# #     }else{# else... maybe add plate..
# #       stop(paste("nr of wells: ",self@.data$levelSize[2] ," your selection: ",length(wellNrs), sep=""))
# #     }
# #   }
#   
# #   level=
# #   if(level==3){ # plate
# #     
# #   }else if(level=2){ # well
# #     
# #   }else if(level=1){# measurement
# #     
# #   }else{
# #     stop("unknown level")
# #   }
#   rows=wellNrs
#     
#   
#     
#   
#   
# #   what="value"
# #   forEach="time"
#   
#   uniques=unique(self[rows,forEach])
#   
#   #TODO what if not the same level??
#   #always get lowest?
#   
#   results=list()
#   for(i in uniques){
# #     print(self[rows,forEach])
#     x=unlist(self[self[forEach]==i,what])
# #     print(x)
# #     print(list(do.call(what=fun,args=list(x))))
# #     print("---------------")
#     results[i]=list(do.call(what=fun,args=list(x)))
#     
# #     results[i]=list(do.call(what=fun,args=list(x=x,y=y,unlist(list(...)))))
#   }
#   
#   
#   
#   return(results)
# 
# })


#' copy
#' @rdname copy
#' @description
#' make a copy of the microplate data instance
#' this function is used to get around the default behaviour
#' 
#' @param self the MicroPlate object
#' 
#' @export
setGeneric("copy", function(self) standardGeneric("copy"))
#' @rdname copy
setMethod("copy", signature(self = "MicroPlate"), function(self){
  copy=new("MicroPlate")
  listOldVars=ls(envir=self@.data, all.names=T)
  for(i in listOldVars){
    copy@.data[[i]]=self@.data[[i]]
  }
  # this could even be an lapply?  
  return(copy)
})


#' dim
#' @rdname dim
#' @description
#' return the diminsions of the microplate
#' 
#' @param x the MicroPlate object
#' 
#' TODO figure out if i can add level to this beauty
#' TODO figure out about non primative columns
#' 
#' @export
setMethod("dim", signature(x = "MicroPlate"), function(x){
  level="measurement"
  return(dim(x[level=level]))
#   return(c( self@.data$levelSize[self@.data$level=="measurement"], length(self@.data$colNames) ) )
})


#' getGrowthRateOLD
#' @rdname getGrowthRateOLD
#' @description
#' uses the grofit package to determine growth rate / doubling time???
#' currently just returns the grofit results
#' 
#' TODO:
#' default it uses time, value
#' and it separetes it based on wavelength 
#' 
#' what if multiple wavelengths?
#' 
#' @param self the MicroPlate object
#' @param wellNrs logical selection or well numbers - missing is all
#' @param timeColumn column with time, the time point in all selected wells need to be equeal
#' @param valueColumn column with the OD values
#' @param experimentIdentifierColumn a column name of the microplate, gcFit needs this 
#' @param additionalInformationColumn a column name of the microplate, gcFit needs this 
#' @param concentrationOfSubstrateColumn a column name of the microplate, gcFit needs this 
#' @param settings parameters passed to the grofit package see \code{\link[grofit]{grofit.control}}:
#' 
#' 
#' @export
#' @import grofit
setGeneric("getGrowthRateOLD", function(self, wellNrs, timeColumn="time", valueColumn="value", experimentIdentifierColumn=NULL, additionalInformationColumn=NULL, concentrationOfSubstrateColumn=NULL, settings=NULL) standardGeneric("getGrowthRateOLD"))
#' @rdname getGrowthRateOLD
setMethod("getGrowthRateOLD", signature(self = "MicroPlate"), function(self, wellNrs, timeColumn="time", valueColumn="value", experimentIdentifierColumn=NULL, additionalInformationColumn=NULL, concentrationOfSubstrateColumn=NULL, settings=NULL){
  # gcFit wants
  # time
  # data=data.frame with
  # 1. column, character as an experiment identifier
  # 2. column: character, additional information about respecting experiment
  # 3. column: concentration of substrate of a compound under which the experiment is obtained
  # 4.-(n+3). column: growth data corresponding to the time points in time.
  
  # $model.type
  # [1] "logistic"     "richards"     "gompertz"     "gompertz.exp"

  # maybe just do a gcSplineFit thingy for each well... that way i dont require as much stuff
  if(is.null(settings)){
    message("no settings provided using defaults: nboot.gc=100,interactive=F,suppress.messages=T,model.type=c('gompertz')")
    settings=grofit.control(nboot.gc=100,interactive=F,suppress.messages=T,model.type=c("gompertz"))
  }
  
  
  ### check input
  # wellNrs
  if(is.logical(wellNrs)){
    if(length(wellNrs)==self@.data$levelSize[2]){
      wellNrs=(1:self@.data$levelSize[2])[wellNrs]
    }else{
      stop(paste("nr of wells: ",self@.data$levelSize[2] ," your selection: ",length(wellNrs), sep=""))
    }
  }
  # columnSelectors
  # todo: maybe remove additional columns... and do other grofit matches
  columns=c(timeColumn,valueColumn,experimentIdentifierColumn,additionalInformationColumn,concentrationOfSubstrateColumn)
  if(is.character(columns)){
    if(!all(self@.data$colnames %in% self@.data$colNames)){
      stop("not all columns exist")
    }
  }
  # todo: add column level checks
  # TODO: error not all wells have same time points... 
  # todo: support for multiple wavelengths
  
  print(wellNrs)
  nrOfWells=length(wellNrs)
  nrOfTimePoints=length(getWellsMeasurementIndex(self,wellNrs[1]))# assume all wells have the same number of time points
  
  
  
  time=matrix(0,nrow = nrOfWells, ncol = nrOfTimePoints) 
  data=data.frame(matrix(0,nrow = nrOfWells, ncol = nrOfTimePoints+3))
  data[,1]=self@.data$well[[experimentIdentifierColumn]][wellNrs]
  data[,2]=self@.data$well[[additionalInformationColumn]][wellNrs]
  data[,3]=self@.data$well[[concentrationOfSubstrateColumn]][wellNrs]
  index=0
  for(i in wellNrs){# for each well
    index=index+1
    data[index,(4:(nrOfTimePoints+3))]=self@.data$measurement[[valueColumn]][getWellsMeasurementIndex(mp,i)]
    time[index,1:nrOfTimePoints]=self@.data$measurement[[timeColumn]][getWellsMeasurementIndex(mp,i)]
  }
  # head(data)
  
  
  result=gcFit(time=time, data=data, control=settings)
  
  # todo plot things?
  
  
  return(result)
})


#' getGrowthRate
#' @rdname getGrowthRate
#' @description
#' uses the grofit package to determine growth rate???
#' currently just returns the grofit results
#' 
#' TODO: what if time is not on measurment level?
#' TODO: add gcID support
#' TODO: what if multiple wavelengths?
#' TODO: log the data?!?!?! or ask user to???
#' TODO: check if my score is actually a score...
#' 
#' default it uses time, value
#' and it separetes it based on wavelength 
#' 
#' 
#' 
#' @param self the MicroPlate object
#' @param wellNrs logical/numerical selection or well numbers - missing is all
#' @param timeColumn column with time, the time point in all selected wells need to be equeal
#' @param valueColumn column with the OD values
#' @param settings parameters passed to the grofit package see \code{\link[grofit]{grofit.control}}:
#' 
#' 
#' @export
#' @import grofit
setGeneric("getGrowthRate", function(self, wellNrs=NULL, timeColumn="time", valueColumn="value", settings=NULL) standardGeneric("getGrowthRate"))
#' @rdname getGrowthRate
setMethod("getGrowthRate", signature(self = "MicroPlate"), function(self, wellNrs=NULL, timeColumn="time", valueColumn="value", settings=NULL){
  # gcFitSpline wants:
  # time    - Numeric vector containing the data for x-axes.
  # data    - Numeric vector giving the growth values belonging to each element of time.
  # gcID    - Vector (of any length) identifying the growth curve data.
  # control - Object of class grofit.control containing a list of options generated by the function grofit.control.#   
  
  
  # maybe just do a gcSplineFit thingy for each well... that way i dont require as much stuff
  if(is.null(settings)){
    print("no settings provided")
    settings=grofit.control(log.y.gc=T,interactive=F)
#     message("no settings provided using defaults: nboot.gc=100,interactive=F,suppress.messages=T,model.type=c('gompertz')")
#     settings=grofit.control(nboot.gc=100,interactive=F,suppress.messages=T,model.type=c("gompertz"))
  }
  
  # 
  
  ### check input
  # wellNrs
  if(is.null(wellNrs)) wellNrs=1:self@.data$levelSize[2] # if not specified get it for everything
  if(is.logical(wellNrs)){
    if(length(wellNrs)==self@.data$levelSize[2]){
      wellNrs=(1:self@.data$levelSize[2])[wellNrs]
    }else{
      stop(paste("nr of wells: ",self@.data$levelSize[2] ," your selection: ",length(wellNrs), sep=""))
    }
  }
  # columnSelectors
#   # todo: maybe remove additional columns... and do other grofit matches
#   columns=c(timeColumn,valueColumn,experimentIdentifierColumn,additionalInformationColumn,concentrationOfSubstrateColumn)
#   if(is.character(columns)){
#     if(!all(self@.data$colnames %in% self@.data$colNames)){
#       stop("not all columns exist")
#     }
#   }
  nrOfWells=length(wellNrs)

  results=vector("list", nrOfWells)
  index=0

  yieldName="grofit.yield"
  growthRateName="grofit.growthRate"
  lagPhaseTimeName="grofit.lagPhaseTime"
  grofitFitScroreName="grofit.fitScrore"
  
  # reserve space
  if(is.null(self@.data$well[[yieldName]])){
    self@.data$well[[yieldName]]=rep(NA,self@.data$levelSize[2])
  }
  if(is.null(self@.data$well[[growthRateName]])){
    self@.data$well[[growthRateName]]=rep(NA,self@.data$levelSize[2])
  }
  if(is.null(self@.data$well[[lagPhaseTimeName]])){
    self@.data$well[[lagPhaseTimeName]]=rep(NA,self@.data$levelSize[2])
  } 
  if(is.null(self@.data$well[[grofitFitScroreName]])){
    self@.data$well[[grofitFitScroreName]]=rep(NA,self@.data$levelSize[2])
  }

  for(i in wellNrs){
    index=index+1
    selection=getWellsMeasurementIndex(self,i)
    time=self[[timeColumn]][selection]
    data=self[[valueColumn]][selection]
    result=gcFitSpline(time=time, data=data, control=settings)
    results[[index]]=result
    
    plot(result)
    title(main = i)
    lambda=result$parameters$lambda
    A=result$parameters$A
    mu=result$parameters$mu
    integral=result$parameters$integral
    plot(result$raw.time,result$raw.data)
    
    
    score=result$spline$crit # is this the score??? need crappier data to test!!
    # $spline$cv.crit might be score... else need to do the model fit for score...
    print(score)
    
    self@.data$well[[yieldName]][i]=A
    self@.data$well[[growthRateName]][i]=mu
    self@.data$well[[lagPhaseTimeName]][i]=lambda
    self@.data$well[[grofitFitScroreName]][i]=score
#     lines(c(0,60),c(0,1))
#     print(paste("",lambda,A,mu,integral))
#     xcor=c(lambda,lambda+5)
#     xpoint=(integral*A)
#     print(xpoint)
#     lines(c(lambda,xpoint),c(0,A))
#     ycor=c(0,A*5)
#     print(paste(paste(xcor),paste(ycor)))
#     print(result$parametersLowess$lambda)
#     lines(xcor,ycor)
#     xcor=c(7.13,12.13)
#     ycor=c(0,3.045)
#     lines(xcor, ycor)
#     lines(c(7.13,12.3),c(0,3.045))
#     lines(c(7.13,12.3),c(0,0.6))
#     lines(c(0,60),c(0,0.6))
#     lines(c(lambda,(lambda+50)),c(0,(A*50)))
#     lines(c(7.13,12.3),c(0,3.045))
#     lines(x=c(0,50),y=c(0,1))
    
    lines( c(lambda,(lambda+(A/mu))) , c(0,A) )
    
    plot(time,data)
    
    results[index]
  }
  
  updateMetaData(self)
  
  
  
  # todo plot things?
  returnResults=F
  if(returnResults){return(results)}
})


#' getWellsMeasurementIndex
#' @rdname getWellsMeasurementIndex
#' @description
#' get start and end coordinates of the requested well measurements in the measurement 'table'
#' 
#' NEEDS A BETTER NAME
#' 
#' @param self the microplate object
#' @param wellNr the well you want the measurement row numbers from
#'  
#' @export
setGeneric("getWellsMeasurementIndex", function(self,wellNr) standardGeneric("getWellsMeasurementIndex"))
#' @rdname getWellsMeasurementIndex
setMethod("getWellsMeasurementIndex", signature(self = "MicroPlate"), function(self, wellNr){
  nrOfMeasurement=0
  i=wellNr
  
  # check if the well given was the last well
  if(!is.na(self@.data$well$measurement[i+1])){
    nrOfMeasurement=self@.data$well$measurement[[i+1]]-self@.data$well$measurement[[i]]
  }else{
    # last well
    nrOfMeasurement=length(self@.data$measurement[[1]])-self@.data$well$measurement[[i]]+1
  }
  end=self@.data$well$measurement[i]+nrOfMeasurement-1 # the -1 is cause the start is also included
  return(self@.data$well$measurement[i]:end)
#   return(data.frame(start=self@.data$well$measurement[i],end=end,stringsAsFactors = F))
})




#' showWellNrs
#' @param mp the MicroPlate object
#' @export
#' @import shape
showWellNrs=function(mp){
  firstWellNumber=0
  lastWellNumber=0
 
  
  # well numbers continue over different plates
  # so it should plot all plates
  for(plateNumber in 1:mp@.data$levelSize[3]){
    # set variables for next
    firstWellNumber=lastWellNumber+1
    lastWellNumber=lastWellNumber+mp@.data$wellsPerPlate[plateNumber]
    selection=firstWellNumber:lastWellNumber
        
    nrOfRows=max(mp@.data$well$row[selection])
    nrOfColumns=max(mp@.data$well$column[selection])
    
    plot.new()
    plot.window(xlim=c(1,nrOfColumns),ylim=c(-1,nrOfRows)) 
#     roundrect(mid = c(nrOfColumns/2,nrOfRows/2),radx=nrOfColumns/2,rady=nrOfRows/2)
    
    for(wellNr in selection){
      filledcylinder(mid=c(mp@.data$well$column[wellNr],nrOfRows-mp@.data$well$row[wellNr]),rx=0.4, ry=0.4,len=0.2, angle = 90, col = "white", lcol = "black", lcolint = "grey")  
      text(mp@.data$well$column[wellNr],nrOfRows-mp@.data$well$row[wellNr],wellNr)
    }
    
  }

}


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
