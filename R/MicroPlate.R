library(gtools)
library(grofit)
library(plyr)

#
# Data stores everything!
# hopefully on different levels...
# 
# TODO:
# - createFromDataFrame
# - dim, length, +-*/ == !=
# - consider: Arith() = +-*/  ???
# - consider: Compate = == != < > etc... ???
# - consider: nrow, colnames, head, names etc
# - stringsAsFactors,  check.names... if i want to inherent data.frame -- make.names...
#     also factors may safe memory!!! ... but it may make everything even more slow...
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
  self@.data$firstMeasurmentRowNrPerPlate=NULL
  firstMeasurmentRowNrPerPlate=1
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
    self@.data$firstMeasurmentRowNrPerPlate=append(self@.data$firstMeasurmentRowNrPerPlate,firstMeasurmentRowNrPerPlate)
    firstMeasurmentRowNrPerPlate=firstMeasurmentRowNrPerPlate+nrOfMeasurement
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
#' TODO: merge modes more AKA recode
#' TODO: document all params... though they are hard to read ...
#' TODO: consider formula mode
#' TODO: consider adding a single measurement point to well level if specifically requested...
#' 
#' 
#' @export 
setMethod("[", signature(x = "MicroPlate", i = "ANY", j = "ANY"), function(x, i , j, ...) {
  args <- list(...)
  argsNames=names(args)
  col=NULL
  row=NULL
  

#     print("nr of parameters..")
#     print(nargs())
#     print(length(args))
#     print(nargs()-length(args))
  #
  # data.frame has some special behaviour
  if(missing(i) & missing(j)){
    # mp[]<- and mp[,]<-
    # print("mp[] or mp[,]")
    # return everything
    row=NULL
    col=NULL
  } else if(missing(i)){
    # mp[,1]<-
    #     print("mp[,1]")
    row=NULL
    col=j
  } else if(missing(j) & (nargs()-length(args))==2  )  {
    # mp[1]<-
    # print("mp[1]")
    # the 2 are: the microplate,the column
    # data.frame special case
    # should return column instead of row!
    row=NULL
    col=i
  } else if(missing(j)) {
    # mp[1,]<-
    # print("mp[1,]")
    row=i
    col=NULL
  } else {
    # mp[1,2]<-
    # print("mp[1,2]")
    row=i
    col=j
  }
  
  
  # if present convert level to number instead string
  if(!is.null(args$level)){
    # mp[...level=..]=value
    # check if args level is 1,2,3 or "plate","well","measurement"
    if(class(args$level)=="character"){
      # check if its a valid level name
      if(any(is.element(x@.data$level,args$level))) {
        args$level=x@.data$levelNr[x@.data$level==args$level]
      }else {
        stop(paste("level given not in: ",paste(x@.data$level,collapse=" ")," given level: ",level,sep=""))
      }
    } else if(class(args$level)=="numeric"| class(args$level)=="integer"){
      if(!any(is.element(x@.data$levelNr,args$level))){
        stop(paste("level given not in: ",paste(x@.data$levelNr,collapse=" ")," given level: ",args$level,sep=""))
        #         stop(paste("level is of invalid class expected level name: ",paste(x@.data$level, collapse=" "),"\n or level number: ",paste(x@.data$levelNr,collapse=" "),"\n but got data of class: ",class(args$level),sep=""))
      }
    }
  }
  
  # 2nd check level
  level=NULL
  # get col to colnames if possible...
  if(!is.null(col)){
    # check col
    if(!(class(col)=="numeric" | class(col)=="integer" | class(col)=="character") ){
      stop(paste("col index should be a number or char, not a: ",class(col)))
    }
    if((class(col)=="numeric" | class(col)=="integer") & !all(is.element(col,1:length(x@.data$colNames)))  ) {
      stop(paste("column number(s) given that does not exist!\n number(s) given:",paste(col,collapse=", "),"\n max col number in data:",length(x@.data$colNames), sep=""))
    }
    if(length(col)!=length(unique(col))){
      stop("duplicate columns selected")
    }
    if (any(is.element(col,x@.data$reservedNames))){
      stop(paste("The following names are reserved for other purposes!: ",paste(x@.data$reservedNames,sep=", ",collapse  = " "), sep=""))
    }
    
    # also change to names if numbers
    if(class(col)!="character"){ 
      col=x@.data$colNames[col]
    }
    
    # check if all columns exist
    if(!all(col%in%x@.data$colNames)) stop(paste("columns given that do not exist:", paste(col, collapse=", "), "\n valid colnames are:",paste(x@.data$colNames,collapse=", "), sep=""))
    
    if(suppressWarnings(min(x@.data$colLevelNr[x@.data$colNames%in%col]))!=Inf){
      level=suppressWarnings(min(x@.data$colLevelNr[x@.data$colNames%in%col]))
      #       print(paste("min excisting level=",level))
    }
  } else { 
    # col==NULL
  }
  
  
  if(!is.null(args$level)){
    # mp[...level=..]=value
    # check if level matches...
    if(!is.null(level)){
      if(args$level>level) {
        stop("level parameter greater than column selection")
      }else{
        level=args$level
      }
    }else{
      level=args$level
    }
  }
  
  
  # check row and make it a logical
  if(!is.null(row)){
    if(!(class(row)=="numeric" | class(row)=="integer" | class(row)=="logical")){
      stop(paste("row index should be a number or a logical, not a: ",class(row)))
    }
    
    # todo: check length?
    # todo: check selction length?
    if(is.null(level))stop("can this happen??????????????")
    
    if(!is.logical(row)){
      if(max(row)>x@.data$levelSize[level])stop("level and row numbers do not match")
      row=(1:x@.data$levelSize[level])%in%row
    }else{
      if(length(row)>x@.data$levelSize[level])stop("level and row numbers do not match")
    }
    # row should be logical now
  } else {
    # rows were not provided
    # mp[,col,...]=value
    # mp[] mp[,]
    if(is.null(level)){ # level was not yet set
      #df[]???
      level=1
    }
    #     print(level)
    row=rep(T,x@.data$levelSize[level])
  }



  # check args for 
  if(!length(args)==0){
    # check if names
    if(any(argsNames%in%"")) stop("unspecified argument provided")
    if(length(argsNames)!=length(unique(argsNames))) stop("you are only allowed to use arguments once")
    if(!all(argsNames%in%append(x@.data$colNames,c("well","plate","level")))) stop(paste("only allowed: ",paste(x@.data$colNames,sep=", "),", well and level",sep=""))
  
  
  }
  
  if(is.null(col)){# if col is still NULL set it to everything at that level
    col=x@.data$colNames[x@.data$colLevelNr>=level]
  }
  
#   print(level)
#   print(row)
#   print(col)
  bothCol=union(x@.data$colNames[x@.data$colNames%in%argsNames],col)
#   print(bothCol)
  
  returnValue=NULL
  
  
  # return the requested data.
  if(level==3){ # plate
    returnValue=as.data.frame(as.data.frame(x@.data$plate, stringsAsFactors=F)[,bothCol],stringsAsFactors=F)
    colnames(returnValue)=bothCol
  }else if (level==2){ # well
    # repeat plate for each well
    #
    # reserve space
    returnValue=data.frame(matrix(nrow=x@.data$levelSize[level],ncol=length(bothCol)), stringsAsFactors=F)
    colnames(returnValue)=bothCol
    for(colnr in 1:length(bothCol)){ # for each column
      tempData=NULL
      if(x@.data$colLevel[x@.data$colNames==bothCol[colnr]]=="plate"){
        # repeat for each well
        for(i in 1:length(x@.data$plate[[1]])){ # an append??? slow!!!
          tempData=append(tempData,rep(x@.data$plate[[bothCol[colnr]]][i],count(x@.data$well$plate)[[2]][i]))
        }
#         tempData=lapply(x@.data$data, function(x)returnValue=append(returnValue,x[[name]]))
        tempData=c(tempData,recursive=T)        
      }else if(x@.data$colLevel[x@.data$colNames==bothCol[colnr]]=="well"){
        tempData=x@.data$well[[bothCol[colnr]]]
      }else{
        stop("WEIRD ERROR !@#!")
      }
      returnValue[,colnr]=tempData
    }
  }else if(level==1){ # measurement
    #
    # fetch the requested data
    returnValue=data.frame(matrix(nrow=x@.data$levelSize[level],ncol=length(bothCol)),stringsAsFactors = FALSE)
    colnames(returnValue)=bothCol
    for(colnr in 1:length(bothCol)){ # for each column
      # always first fill tempdata with the whole column (at measurement level)
      # then do the row select
      tempData=NULL
      if(x@.data$colLevel[x@.data$colNames==bothCol[colnr]]=="measurement"){
        # get whole column
        tempData=x@.data$measurement[[bothCol[colnr]]]
      } else if(x@.data$colLevel[x@.data$colNames==bothCol[colnr]]=="well"){
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
          tempData=append(tempData,rep(x@.data$well[[bothCol[colnr]]][[i]],nrOfMeasurement))
        }
      } else if(x@.data$colLevel[x@.data$colNames==bothCol[colnr]]=="plate"){
        # repeat for eachWell*eachMeasurement
        for(i in 1:x@.data$levelSize[3]){ # for each plate
          # get the corresponding plate values
          tempData=append(tempData,rep(x@.data$plate[[bothCol[colnr]]][[i]],x@.data$measurementsPerPlate[[i]])) # for each measurement
        }
      } else {
        stop("data at unknown level... this error means a coding error as it should have been cought above!")
      }
      returnValue[,colnr]=tempData
    }
  }
  
  
  
  # check selection
  selection=NULL# make sure it is available on the right level only..
  if(!length(args)==0){ # 2nd part is to prevent an endless loop
    # mp[... something=something]=value
        
#     mp=x[level=level] # make sure this part is never called from itself
    size=x@.data$levelSize[level]
    true=rep(T,size)
    false=logical(size)
    selection=true
    
    for(i in 1:length(args)){
      if(argsNames[i]=="level"){
        # already handled above
      }else if(argsNames[i]=="well"){
        #           print(class(args[[i]]))
        #           print(args[[i]])
        if(class(args[[i]])=="character"){ # A11
          coordinates=extractPlateCoordinates(args[[i]]) # A11 ->  A=1 , 11=11 ....
          selection=selection&(returnValue$row%in%coordinates["row"])
          selection=selection&(returnValue$col%in%coordinates["column"])
        } else if((class(args[[i]])=="numeric")||(class(args[[i]])=="integer")){ # well numbers
          if(level==1){ # measurement level
            # todo for is very slow! so change this later!
            wellNrs=double(size)
            for(j in 1:x@.data$levelSize[2]){# for each well
              wellNrs[getWellsMeasurementIndex(x,j)]=j
            }
            selection=selection&(wellNrs%in%args[[i]])
          } else if(level==2){# well level
            selection=selection&((1:size)%in%args[[i]])
          }else stop("can't use well selection at plate level")
        } else stop("weird well selection")
      }else if(argsNames[i]=="plate"){
        if(level==1){# measurement level
          plateNrs=double(size)
          for(j in 1:x@.data$levelSize[3]){# for each plate
            plateNrs[getPlatesMeasurementIndex(returnValue,j)]=j
          }
          selection=selection&(plateNrs%in%args[[i]])
        }else if (level==2){# well level
          plateNrs=double(size)
          for(j in 1:x@.data$levelSize[3]){# for each plate
            plateNrs[getPlatesWellIndex(mp,j)]=j
          }
          selection=selection&(plateNrs%in%args[[i]])
        }else {# plate level
          selection=selection&((1:size)%in%args[[i]])
        }
        
      }else{ # its a col name
        selection=selection&(returnValue[[argsNames[i]]]%in%args[[i]])
      }
      #         print(sum(selection))
    }# for each args/...
  }# if each args/...
  
  if(!is.null(selection)){
    selection=selection&row
  }else{
    selection=row
  }
  
#   print(paste("selection length: ",length(selection),"nrOfSelctions", sum(selection)))
#   print(paste("col:",col))
  return(returnValue[selection,col])
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
#'  
#' todo: clean up code
#' todo mp[1]=NULL
#' todo also allow rows to be deleted?
#' 
#' todo: testData["content"]=1:12 -- content remains character instead of number/int
#' todo: list support? as in none string/number values
#' 
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
 
  dataRows=NULL
  dataCols=NULL
  
#   print("nr of parameters..")
#   print(nargs())
#   print(length(args))
#   print(nargs()-length(args))

  nrOfCol=length(x@.data$colNames)

  #
  # data.frame has some special behaviour
  if(missing(i) & missing(j)){
    # mp[]<- and mp[,]<-
    # print("mp[] or mp[,]")
    # return everything
    row=NULL
    col=NULL
  } else if(missing(i)){
    # mp[,1]<-
    #     print("mp[,1]")
    row=NULL
    col=j
  } else if(missing(j) & (nargs()-length(args))==3  )  {
    # mp[1]<-
    # print("mp[1]")
    # the 3 are: the microplate,the column,the value
    # data.frame special case
    # should return column instead of row!
    row=NULL
    col=i
  } else if(missing(j)) {
    # mp[1,]<-
    # print("mp[1,]")
    row=i
    col=NULL
  } else {
    # mp[1,2]<-
    # print("mp[1,2]")
    row=i
    col=j
  }
    
  # first check if it is a remove operation
  if(is.null(value)){
    # check if its a column remove mp[names]=NULL
    # todo add checks to make sure only col is filled...
    if(!is.null(row)) stop("only allowed to delete columns, use: mp[1],mp['colname']")
    if(length(args)!=0) stop("only allowed to delete columns, use: mp[1],mp['colname']")
    return(removeColumn(x,col)) 
  }

  # if present convert level to number instead string
  if(!is.null(args$level)){
    # mp[...level=..]=value
    # check if args level is 1,2,3 or "plate","well","measurement"
    if(class(args$level)=="character"){
      # check if its a valid level name
      if(any(is.element(x@.data$level,args$level))) {
        args$level=x@.data$levelNr[x@.data$level==args$level]
      }else {
        stop(paste("level given not in: ",paste(x@.data$level,collapse=" ")," given level: ",level,sep=""))
      }
    } else if(class(args$level)=="numeric"| class(args$level)=="integer"){
      if(!any(is.element(x@.data$levelNr,args$level))){
        stop(paste("level given not in: ",paste(x@.data$levelNr,collapse=" ")," given level: ",args$level,sep=""))
        #         stop(paste("level is of invalid class expected level name: ",paste(x@.data$level, collapse=" "),"\n or level number: ",paste(x@.data$levelNr,collapse=" "),"\n but got data of class: ",class(args$level),sep=""))
      }
    }
  }

  # analyse new input
  # the way data.frame seems to handle data that 
  # does not match the size of the rows and columns selected
  # is by if its smaller then copy it ... but only if it can be devided without rest
  # if its more... ignore the more...
  # data is filled by column, so first all rows of a column are added, then the next column...
  # 
  # adding data.frames (and matrices??) need the right amount of rows and cols
  # what about lists???
  # if you use mp[] and you add something way bigger, 
  # it will keep the mp the same size, and throw a bunch of warning
  if(class(value)=="matrix"){
    value=data.frame(value, stringsAsFactors=F) # dont want to deal with this crap seperatly!
  }
  if(class(value)=="data.frame"){
    dataRows=dim(value)[1]
    dataCols=dim(value)[2]
  } else if (any(class(value) %in% c("character","numeric","integer","logical"))) {
    dataRows=length(value)[1]
    dataCols=1
  } else {
    stop(paste("data type of class: ",class(value)," not supported", sep="",collapse=""))
  }
  
  # 2nd check level
  level=NULL
  # get col to colnames if possible...
  if(!is.null(col)){
    # check col
    if(!(class(col)=="numeric" | class(col)=="integer" | class(col)=="character") ){
      stop(paste("col index should be a number or char, not a: ",class(col)))
    }
    if((class(col)=="numeric" | class(col)=="integer") & !all(is.element(col,1:nrOfCol))  ) {
      stop(paste("column number(s) given that does not exist!\n number(s) given:",paste(col,collapse=", "),"\n max col number in data:",nrOfCol, sep=""))
    }
    if(length(col)!=length(unique(col))){
      stop("duplicate columns selected")
    }
    if (any(is.element(col,x@.data$reservedNames))){
      stop(paste("The following names are reserved for other purposes!: ",paste(x@.data$reservedNames,sep=", ",collapse  = " "), sep=""))
    }

    # also change to names if numbers
    if(class(col)!="character"){ 
      col=x@.data$colNames[col]
    }
    
    # check if all levels are the same
    if(length(unique(x@.data$colLevelNr[x@.data$colNames%in%col]))>1) stop("you can change data only at 1 level at a time")
    # get the level
#     level=suppressWarnings(min(x@.data$colLevelNr[x@.data$colNames%in%col])) # not so much min, as any :P
    
    if(suppressWarnings(min(x@.data$colLevelNr[x@.data$colNames%in%col]))!=Inf){
      level=suppressWarnings(min(x@.data$colLevelNr[x@.data$colNames%in%col]))
#       print(paste("min excisting level=",level))
    }
    if(!is.null(level)){
      if(!is.null(args$level)){if(level!=args$level)stop("level parameter does not match column selection")}
    }
  } else { 
    # col==NULL
    # check if the data length matches a level
    if(max(x@.data$levelNr[x@.data$levelSize==dataRows])!=Inf){ # if data size=anyof the levels
      level=max(x@.data$levelNr[x@.data$levelSize==dataRows])
    }
  }


  if(!is.null(args$level)){
    # mp[...level=..]=value
    # check if level matches...
    if(!is.null(level)){
      if(args$level<level) {
        stop("level parameter does not match column selection")
      }else{
        level=args$level
      }
    }else{
      level=args$level
    }
  }

  # check selection
  selection=NULL# make sure it is available on the right level only..
  if(!length(args)==0){
    # mp[... something=something]=value
    names=names(args)
  
    # check if names
    if(any(names%in%"")) stop("unspecified argument provided")
    if(length(names)!=length(unique(names))) stop("you are only allowed to use arguments once")
    if(!all(names%in%append(x@.data$colNames,c("well","plate","level")))) stop(paste("only allowed: ",paste(x@.data$colNames,sep=", "),", well and level",sep=""))
    
    # check if data selection is not < then level
    if(!is.null(level)){
      if(sum(names%in%c("well","plate","level"))!=length(names)){
        if(min(x@.data$colLevel[x@.data$colNames%in%names])<level){
          stop("..........................................")
        }
      }else{
        # not sure.... yet...
      }
    } else {
      # can still determine on level size?
       stop("!!!!!!!!!!!!!!TODO!!!!!!!!!!!!1")
    }
    
   
    
    
    
    
    mp=x[level=level]
    size=x@.data$levelSize[level]
    true=rep(T,size)
    false=logical(size)
    selection=true
    
    for(i in 1:length(args)){
      if(names[i]=="level"){
        # already handled above
      }else if(names[i]=="well"){
        #           print(class(args[[i]]))
        #           print(args[[i]])
        if(class(args[[i]])=="character"){ # A11
          coordinates=extractPlateCoordinates(args[[i]]) # A11 ->  A=1 , 11=11 ....
          selection=selection&(mp$row%in%coordinates["row"])
          selection=selection&(mp$col%in%coordinates["column"])
        } else if((class(args[[i]])=="numeric")||(class(args[[i]])=="integer")){ # well numbers
          if(level==1){ # measurement level
            # todo for is very slow! so change this later!
            wellNrs=double(size)
            for(j in 1:x@.data$levelSize[2]){# for each well
              wellNrs[getWellsMeasurementIndex(mp,j)]=j
            }
            selection=selection&(wellNrs%in%args[[i]])
          } else if(level==2){# well level
            selection=selection&((1:size)%in%args[[i]])
          }else stop("can't use well selection at plate level")
        } else stop("weird well selection")
      }else if(names[i]=="plate"){
        if(level==1){# measurement level
          plateNrs=double(size)
          for(j in 1:x@.data$levelSize[3]){# for each plate
            plateNrs[getPlatesMeasurementIndex(mp,j)]=j
          }
          selection=selection&(plateNrs%in%args[[i]])
        }else if (level==2){# well level
          plateNrs=double(size)
          for(j in 1:x@.data$levelSize[3]){# for each plate
            plateNrs[getPlatesWellIndex(mp,j)]=j
          }
          selection=selection&(plateNrs%in%args[[i]])
        }else {# plate level
          selection=selection&((1:size)%in%args[[i]])
        }
        
      }else{ # its a col name
        selection=selection&(mp[[names[i]]]%in%args[[i]])
      }
      #         print(sum(selection))
    }# for each args/...
  }# if each args/...



  # check row and make it a logical
  if(!is.null(row)){
    if(!(class(row)=="numeric" | class(row)=="integer" | class(row)=="logical")){
      stop(paste("row index should be a number or a logical, not a: ",class(row)))
    }
    
#     if(is.logical(row)){
#       row=(1:length(row))[row] # convert it for now determine if it is valid later after level has been determined
#     }else{
#       if(length(row)!=length(unique(row))){
#         stop("duplicate rows selected")
#       }
#     }

    # todo: check length?
    # todo: check selction length?
    if(is.null(level))stop("can this happen??????????????")

    if(!is.logical(row)){
      if(max(row)>x@.data$levelSize[level])stop("level and row numbers do not match")
      row=(1:x@.data$levelSize[level])%in%row
    }else{
      if(length(row)>x@.data$levelSize[level])stop("level and row numbers do not match")
    }
    # row should be logical now
  } else {
    # rows were not provided
    # mp[,col,...]=value
    if(is.null(level)){ # level was not yet set
      if(dataRows%in%x@.data$levelSize){ # check data size
        level=max(x@.data$levelNr[x@.data$levelSize==dataRows])
      }else{ 
        stop("data not specified, and not the size of any of the levels...")
      }
    }
#     print(level)
    row=rep(T,x@.data$levelSize[level])
  }

  if(!is.null(selection)){
    selection=selection&row
  }else{
    selection=row
  }

  # check if row matches data, and data repeat
  if(sum(selection)!=dataRows){
    if(dataRows==1){
      # single value repeat is allowed!
      value=rep(value,sum(selection))
      dataRows=sum(selection)
    }else if(sum(selection)==1&&dataRows==length(col)){
      # rows and column switch!
      value=data.frame(matrix(value,1,dataRows),stringsAsFactors = F)# make columns columns instead of rows...
      dataCols=dataRows
      dataRows=1
    }else{
      print(paste("datarows=",dataRows," datacols=",dataCols," selection cols=",length(col)," selection rows=",sum(selection),sep=""))
      stop(paste("data length: ",dataRows," does not match selection length: ",sum(selection),sep=""))
    }
  }
  if(length(col)!=dataCols)stop(paste("nr of columns: ",dataCols," does not match selection length: ",length(col),sep=""))
  
  # change data
#   print("change data")
#   print(sum(selection))
#   print(col)
#   print(level)
  for(colnr in 1:length(col)){ # for each column
    # check if new column
    newColumn=!is.element(col[colnr],x@.data$colNames)
#     print(paste("newColumn: ",newColumn))
    #     print(row)
    #     print(col)
    #     print(level)
    
    if (level==3){ # plate 
      if(class(value)=="data.frame"){
        #         x@.data$plate[[col[colnr]]][row]=value[[col[colnr]]] 
        x@.data$plate[[col[colnr]]][selection]=value[[colnr]]
      }else{
        x@.data$plate[[col]][selection]=value
      }
      if(newColumn){
        x@.data$plate[[col[colnr]]][!selection]=NA #this is repeated if needed
      }
    } else if (level==2){ # well
      if(class(value)=="data.frame"){
        x@.data$well[[col[colnr]]][selection]=value[[colnr]]
      }else{
        x@.data$well[[col]][selection]=value
      }
      if(newColumn){
        x@.data$well[[col[colnr]]][!selection]=NA #this is repeated if needed
      }
    } else if(level==1){ # measurement
      
      if(class(value)=="data.frame"){
        x@.data$measurement[[col[colnr]]][selection]=value[[colnr]]
      }else{
        x@.data$measurement[[col]][selection]=value
      }
      if(newColumn){
        x@.data$measurement[[col[colnr]]][!selection]=NA #this is repeated if needed
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
  return(x) # without this... the whole thing is kinda deleted... weird stuff!
})


#
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



#' overwrtie colnames
#' 
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


#' getPlatesMeasurementIndex
#' @rdname getPlatesMeasurementIndex
#' @description
#' get start and end coordinates of the requested well measurements in the measurement 'table'
#' 
#' NEEDS A BETTER NAME
#' 
#' @param mp the microplate object
#' @param plateNr the plate you want the measurement row numbers from
#'  
#' @export
setGeneric("getPlatesMeasurementIndex", function(mp,plateNr) standardGeneric("getPlatesMeasurementIndex"))
#' @rdname getPlatesMeasurementIndex
setMethod("getPlatesMeasurementIndex", signature(mp = "MicroPlate"), function(mp, plateNr){
  first=mp@.data$firstMeasurmentRowNrPerPlate[plateNr]
  return(first:(first+mp@.data$measurementsPerPlate[plateNr]-1))
})


#' getPlatesWellIndex
#' @rdname getPlatesWellIndex
#' @description
#' get start and end coordinates of the requested well measurements in the measurement 'table'
#' 
#' NEEDS A BETTER NAME
#' 
#' @param mp the microplate object
#' @param plateNr the plate you want the measurement row numbers from
#'  
#' @export
setGeneric("getPlatesWellIndex", function(mp,plateNr) standardGeneric("getPlatesWellIndex"))
#' @rdname getPlatesWellIndex
setMethod("getPlatesWellIndex", signature(mp = "MicroPlate"), function(mp, plateNr){
  first=1
  for(i in 1:mp@.data$levelSize[3]){ # for each plate
    if(i==plateNr) return(first:(first+mp@.data$wellsPerPlate[i]-1))
    first=first+mp@.data$wellsPerPlate[i]
  }
  stop("invalid plateNr????")
})


#' showWellNrs
#' 
#' @param mp the MicroPlate object
#' 
#' plots the plates and shows the well nr...
#' color is based on OD if present else its green
#' 
#' todo: change to white to bg
#' todo: check wavelength properly
#' todo: decide if multiple wavelengths (multiple plots)
#' todo: decide if i need to change maxOD per plate(as it is now) or global... 
#' todo: test for bigger other than 96 well plates. 
#' 
#' @export
#' @import shape
showWellNrs=function(mp){
  firstWellNumber=0
  lastWellNumber=0
  
  # is there a wavelength column
#   wellColor="#FFFFFF" # default color=white
  plateCol="#00FF00" # green is default
  if(!is.null(suppressWarnings(mp$waveLength[1]))){
    plateCol=waveLengthToRGBString(mp$waveLength[1])
  }
  
  colFunc=colorRampPalette(c("white", plateCol))
  colGradient=colFunc(100)
  
  # well numbers continue over different plates
  # so it should plot all plates
  for(plateNumber in 1:mp@.data$levelSize[3]){
    # set variables for next
    firstWellNumber=lastWellNumber+1
    lastWellNumber=lastWellNumber+mp@.data$wellsPerPlate[plateNumber]
    selection=firstWellNumber:lastWellNumber
        
    nrOfRows=max(mp@.data$well$row[selection])
    nrOfColumns=max(mp@.data$well$column[selection])
    
    maxODOfPlate=max(mp$value[getPlatesMeasurementIndex(mp,plateNumber)])
    minODOfPlate=min(mp$value[getPlatesMeasurementIndex(mp,plateNumber)])
    
    
#    wellColor=c(255,255,255) # white
    
    plot.new()
    plot.window(xlim=c(1,nrOfColumns),ylim=c(-1,nrOfRows))
    if(!is.null(suppressWarnings(mp$waveLength[1]))){
      title(paste("WellNrs plate: ",plateNumber," at wavelength: " , mp$waveLength[1], sep=""))
    }else{
      title(paste("WellNrs plate: ",plateNumber, sep=""))
    }
#     filledmultigonal(mid=c(nrOfColumns/2,nrOfRows/2),rx=nrOfColumns/2,ry=nrOfRows/2,nr=4,angle=45,col="lightblue1")
#     roundrect(mid = c(nrOfColumns/2,nrOfRows/2),radx=nrOfColumns/2,rady=nrOfRows/2)
#     rect(xleft=1,ybottom=-1,xright=nrOfColumns,ytop=nrOfRows,col="lightblue1")
#     rect(xleft=-2,ybottom=-1,xright=nrOfColumns+2,ytop=nrOfRows,col="white")
    
    for(wellNr in selection){
      maxOD=max(mp@.data$measurement$value[getWellsMeasurementIndex(mp,wellNr)])
      
      wellColor=colGradient[ceiling(((maxOD-minODOfPlate)/(maxODOfPlate-minODOfPlate))*100)]
      
      filledcylinder(mid=c(mp@.data$well$column[wellNr],nrOfRows-mp@.data$well$row[wellNr]),rx=0.4, ry=0.4,len=0.2, angle = 90, col=wellColor,topcol=wellColor,botcol=wellColor, lcol = "black", lcolint = "grey")  
      text(mp@.data$well$column[wellNr],nrOfRows-mp@.data$well$row[wellNr],wellNr,cex=0.7)
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
