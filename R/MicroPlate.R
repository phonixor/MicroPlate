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
# also factors may safe memory!!!
# http://stackoverflow.com/questions/8691812/get-object-methods-r
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
# TODO change data$levelNr from 3,2,1 to 1,2,3... this would allow me to remove code...


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
  # initialize the love!
  .Object@.data=new.env() # make sure it has its own little space
  .Object@.data$data=NULL # stores all well and measurement data!
  .Object@.data$plate=NULL # stores all plate data!
  # per column
  .Object@.data$colNames=NULL # stores the colnames!
  .Object@.data$colLevel=NULL # contains the name of the level
  .Object@.data$colLevelNr=NULL # contains the level number
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
setMethod("createFromDataFrame", signature(self = "MicroPlate"), function(self,df=NULL){
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


#' addPlate
#' 
#' stores data in the class instance
#' if no data exist the data imported becomes the data
#' else smartbind is used to add the data
#' 
#' TODO add more plate data...
#' @export
setGeneric("addPlate", function(self,newData=NULL, layoutData=NULL,  plateName=NULL, ...) standardGeneric("addPlate")) 
setMethod("addPlate", signature(self = "MicroPlate"), function(self,newData=NULL, layoutData=NULL, plateName=NULL, ...){
  # check newData names and compare them with the names currently in use
  # add new columns to existing data
  # fill those with NA for existing data
  if(is.null(self@.data$data)){ # is there already any data?
    # no! use data as new data!
    print("new plate")
    
    self@.data$data=newData
    self@.data$data$plate=rep(1,length(newData[[1]])) # create foreign keys
    
    if( is.null(plateName) ){
      # generate platename  --> plate number=row number
      self@.data$plate=list(plateName="1")
    } else {
      # use plateName.... however plate still has
      self@.data$plate=list(plateName=plateName)
    }
  }else{
    # yes! add data to excisting data!
    print("adding to excisting plate data")
    if( is.null(plateName) ){
      # generate platename
      # why does this one take my own smartbind.... i dont give it a Data, i give it a data.frame....
      self@.data$plate=smartbind(self@.data$plate, data.frame(plateName=(self@.data$levelSize[self@.data$level=="plate"]+1),stringsAsFactors=F))
    } else {
      # use plateName....
      # todo check if plateName is unique..
      self@.data$plate=smartbind(self@.data$plate, data.frame(plateName=plateName,stringsAsFactors=F))
    }
    bindParsedData(self,newData)
  }
  # update colnames
  updateColnames(self)
  
  
  # add experimental layout file with more information about each well
  if(!is.null(layoutData)){
    # check length
    if(length(newData[[1]])==dim(layoutData)[1]){
      # files are equal in length, nice!
    }else if( length(newData[[1]])==length(which(layoutData$basic!="empty"))[1] ){
      # files are not equal in length, but they are if you remove the empty stuff
      # print("removing empty elements")
      layoutData=layoutData[which(layoutData$basic!="empty"),]
#       print(layoutData)
    } else {
      stop("layout data does not match the reader data")
    }
    #
    # find data columns
    colNames=colnames(layoutData)[!colnames(layoutData) %in% c("row","column")]
#     print(layoutData$row)
#     print(layoutData$column)
    #
    # add experimental data to well...
    # rewrite to check all first and then insert in one go...
    for(i in 1:dim(layoutData)[1]){ # for each well/row
      # check if the data is for the same wells
#       print("----")
#       print(which(self@.data$data$column==layoutData$column[i]))
#       print(self@.data$data$column)
#       print(layoutData$row[i])
#        print(layoutData$column[i])
#       print(which(self@.data$data$row==layoutData$row[i]))
      index=which(self@.data$data$column==layoutData$column[i] & self@.data$data$row==layoutData$row[i] & self@.data$data$plate==self@.data$levelSize[self@.data$level=="plate"])
#       print(index)
      if(length(index)==1){
        # add the data to the correct row...
        self[index,colNames,level="well"]=layoutData[i,colNames]
      } else {
        stop("row and columns of layout file and microplate data did not match")
      }
    }
    #
    # update colnames
    updateColnames(self)
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
setMethod("updateColnames", signature(self = "MicroPlate"), function(self){
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


#' bindParsedData
#'
#' similar as plyr's smart bind, only then for the thing that the parsers provide...
#' typing this kinda makes me realize that this might not be very functional... mmmmh...
#'
#' needs a new name as i don't bind 2 MicroPlate objects...
#'
#' TODO TEST!!!
#'
setGeneric("bindParsedData", function(self,newData=NULL) standardGeneric("bindParsedData"))
setMethod("bindParsedData", signature(self = "MicroPlate"), function(self, newData=NULL){
  # TODO what about other plate info?
  # add a lot of checks!
  # what if one deletes column and the new column only has NA as length? instead of the true vector length?
  #
  # TODO check what if columns at different level? move to smallest?
  
  nrOfNewWells=length(newData[[1]])
  nrOfWells=self@.data$levelSize[self@.data$level=="well"]
  plateNumber=self@.data$levelSize[self@.data$level=="plate"]+1 #this needs change
  
  colsWell=names(newData)
  colsMeasurement=names(newData$measurement[[1]])
  newColsWell=colsWell[!(colsWell %in% self@.data$colNames[self@.data$colLevel=="well"])]
  newColsMeasurement=colsMeasurement[!(colsMeasurement %in% self@.data$colNames[self@.data$colLevel=="measurement"])]
  missingColsWell=self@.data$colNames[self@.data$colLevel=="well"][!(self@.data$colNames[self@.data$colLevel=="well"] %in% colsWell)]
  missingColsMeasurement=self@.data$colNames[self@.data$colLevel=="measurement"][!(self@.data$colNames[self@.data$colLevel=="measurement"] %in% colsMeasurement)]
  existingColsWell=colsWell[(colsWell %in% self@.data$colNames[self@.data$colLevel=="well"])]
#   existingColsMeasurement=colsMeasurement[is.element(colsMeasurement,self@.data$colNames)] # this one is kinda useless

#   print(self@.data$colNames[self@.data$colLevel=="well"])
#   print(self@.data$colNames[self@.data$colLevel=="measurement"])  
#   print(self@.data$colNames)
#   print(self@.data$colLevel)

#   print("colsWell")
#   print(colsWell)
#   print("colsMeasurement")
#   print(colsMeasurement)
#   print("")
#   print("newColsWell")
#   print(newColsWell)
#   print("newColsMeasurement")
#   print(newColsMeasurement)
#   print("")
#   print("missingColsWell")
#   print(missingColsWell)
#   print("missingColsMeasurement")
#   print(missingColsMeasurement)
#   print("")
#   print("existingColsWell")
#   print(existingColsWell)


  # Well
  self@.data$data$plate=append(self@.data$data$plate, rep(x=plateNumber,nrOfNewWells)) # generate foreign key
  for(i in 1:length(newColsWell)){
    # create new column
    # fill the existing wells with NA and add the new data
    
    # note that: newColsWell contains measurement!
    if(newColsWell[i]=="measurement"){
      # combine the measurement columns in at the well level
      self@.data$data$measurement=append(self@.data$data$measurement,newData$measurement)
    } else {
      self@.data$data[[newColsWell[i]]]=append(rep(x=NA,nrOfWells),newData[[newColsWell[i]]])
    }
  }
  if(length(missingColsWell)>0){
    for(i in 1:length(missingColsWell)){
      # fill the existing columns for which the newData has no data with NA
      self@.data$data[[missingColsWell[i]]]=append(self@.data$data[[missingColsWell[i]]],rep(x=NA,nrOfNewWells))
    }
  }
  if(length(existingColsWell)>0){ # this should always be the case as row and column are mandatory...
    for(i in 1:length(existingColsWell)){
      # add newData to existing columns
      self@.data$data[[existingColsWell[i]]]=append(self@.data$data[[existingColsWell[i]]],newData[[existingColsWell[i]]])
    }
  }
  # adding NA to empty spaces
  # update existing wells measurements with NA for new columns
  if(length(newColsMeasurement)>0){
    for(i in 1:nrOfWells){ # for each well
      nrOfMeasurements=length(self@.data$data$measurement[[i]][[1]])
      for(j in 1:length(newColsMeasurement)){
        # give it a name, but give it no values... as that would just be a waste of memory
        # TODO change $ and [ ... to deal with this!
        # TODO do i really want this???
        self@.data$data$measurement[[i]][[newColsMeasurement[j]]]=NA
      }
    }
  }
  # add NA's to the newData
  if(length(missingColsMeasurement)>0){
    for(i in nrOfWells:(nrOfWells+nrOfNewWells)){
      nrOfMeasurements=length(self@.data$data$measurement[[i]][[1]])
      for(j in 1:length(missingColsMeasurement)){
        self@.data$data$measurement[[i]][[missingColsMeasurement[j]]]=NA
      }
    }
  }
  updateColnames(self)
  
})


#' as.data.frame()
#' 
#' 
#' 
#' @export
setGeneric("as.data.frame", function(self) standardGeneric("as.data.frame")) 
setMethod("as.data.frame", signature(self = "MicroPlate"), function(self){
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
#' @slot x - MicroPlate
#' @slot i - row - number only
#' @slot j - column - use column number or column name
#' @slot level - use this to force the data to be repeated for the appropiate level: "plate","well","measurement" 
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
  col=unique(col)
  row=unique(row)
  

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
    if(!(class(row)=="numeric"|class(row)=="integer")){
      stop(paste("row index should be a number, not a: ",class(row)))
    }
    if(nrOfRows<length(row)){
      stop(paste("Data only has ",paste(nrOfRows, sep="",collapse=" ")," rows, you asked for row(s):",paste(row, sep="",collapse=" ")))
    }
    if(max(row)>x@.data$levelSize[x@.data$levelNr==lowestLevel]){
      stop(paste("Asked for row number ",max(row)," while the level only has ",x@.data$levelSize[x@.data$levelNr==lowestLevel]," rows",sep=""))
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
          tempData=append(tempData,rep(x@.data$plate[[col[colnr]]][i],count(x@.data$data$plate)[[2]][i]))
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
setMethod("[[", signature(x = "MicroPlate", i = "ANY", j = "ANY"), function(x, i , j, ...) {
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


#' removeColumn
#' 
#' todo Colnumber
#' todo add export 
#' todo add multiple row delete...
#' 
#' @export
setGeneric("removeColumn", function(self, colNames) standardGeneric("removeColumn")) 
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
      self@.data$data[[col[i]]]=NULL
    } else if (level=="measurement") {
      for(j in 1:length(self@.data$data$measurement)){
        self@.data$data$measurement[[j]][[col[i]]]=NULL
      }
    }
  }
  # restore the balance
  updateColnames(self)
  return(self)
})


#' [<-
#' overwrite the []<- function..
#' 
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
#' todo better row check... it is now pretty much assumed that user gives proper row numbers..
#' which is a silly thing to assume... use unique / max / min / interger
#' 
#' 
#' @export 
setMethod("[<-", signature(x = "MicroPlate", i = "ANY", j = "ANY",value="ANY"), function(x, i, j, ..., value) {
  args <- list(...)
  col=NULL
  row=NULL
  data=value
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
      stop("invalid args given, only accepts i,j,level")
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
  row=unique(row) # maybe throw error if this does anything...
  col=unique(col)

  # check col
  if(!(class(col)=="numeric" | class(col)=="integer" | class(col)=="character")){
    stop(paste("col index should be a number or char, not a: ",class(col)))
  }
  
  # also change to names if numbers
  if(class(col)!="character"){
    col=x@.data$colNames[col]
  }
  
  if (any(is.element(col,x@.data$reservedNames))){
    stop(paste("The following names are reserved for other purposes!: ",paste(x@.data$reservedNames,sep=", "), sep=""))
  }
  
  # check if its a column remove df[names]=NULL
  if(is.null(value)) return(removeColumn(x,col))

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
    value=data.frame(value) # dont want to deal with this crap seperatly!
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
        stop(paste("incorrect nr of rows, asked for ",length(row)," rows, while the size of data is: ",dim(value)[1],"",sep=""))
      }
    }
    if((length(col)!=dim(value)[2])){
      stop(paste("incorrect nr of columns, asked for ",length(col)," cols, while the size of data is: ",dim(value)[2],"",sep=""))
    }
    dataLength=dim(value)[1]
        
#     # change data into a big as vector to handle it uniformly down below
#     data=c(value, recursive=TRUE) # this converts everything into chars?
  } else if (any(class(value) %in% c("character","numeric","integer"))) {
    
    if (length(col)!=1) {
      stop("multiple columns given, while only a single dimensional data")
    }
    
    # todo []
    if(!is.null(row)){
      if (length(row)!=length(value)){
        stop("invalid number of rows")
      }
    }
    
    # vector data
    dataLength=length(value)
    
    
  } else if(is.null(class(value))) {
    # delete columns!
    
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
      stop(paste("no rows given and so rows expected to be equal to level size: ",x@.data$levelSize[x@.data$levelNr==colLevel]," rows supplied: ",dataLength,sep=""))
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
        stop("you want to insert more rows into a level then there are rows in that level")
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
        } else {
          stop (paste("the amount of rows given: ", dataLength ,"  does not match any of the data level sizes: ", paste(x@.data$levelSize,collapse="",sep="") ,collapse=" ",sep=""))
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
#     # check if new
    new=!is.element(col[colnr],x@.data$colNames)
    
    if (level==3){ # plate 
      if(class(data)=="data.frame"){
        x@.data$plate[[col[colnr]]][row]=data[[col[colnr]]]
      }else{
        x@.data$plate[[col]][row]=data
      }
      if(!allRowsSelected & new){
        x@.data$plate[[col[colnr]]][notSelectedRows]=NA #this is repeated if needed
      }
    } else if (level==2){ # well
      if(class(data)=="data.frame"){
        x@.data$data[[col[colnr]]][row]=data[[col[colnr]]]
      }else{
        x@.data$data[[col]][row]=data
      }
      if(!allRowsSelected & new){
#         print(notSelectedRows)
        x@.data$data[[col[colnr]]][notSelectedRows]=NA #this is repeated if needed
      }
    } else if(level==1){ # measurement
      # this is slow and annoying... 
      # but shame on you for changing measurements anyways!
      index=0
      dataIndex=0
      for(i in 1:length(x@.data$data$measurement)){ # for each well
        for(j in 1:length(x@.data$data$measurement[[i]])){ # for each measurement
          index=index+1
          if(is.element(index,row)){
            dataIndex=dataIndex+1
            if(class(data)=="data.frame"){
              x@.data$data$measurement[[i]][[j,col[colnr]]]=data[dataIndex,colnr]
            } else {   
              x@.data$data$measurement[[i]][[j,col[colnr]]]=data[dataIndex]
            }
          } else {
            if (!new){
              x@.data$data$measurement[[i]][[j,col[colnr]]]=NA
            }
          }
        } # for each measurement
      } # for each well
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

  updateColnames(x) # TODO maybe only if new cols?
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
setMethod("$<-", signature(x = "MicroPlate"), function(x, name, value) {
  # check if its a valid colname
  if (any(x@.data$reservedNames==name)){
    stop(paste("The following names are reserved for other purposes!: ",paste(x@.data$reservedNames,sep=", "), sep=""))
  }
    
  if(!any(x@.data$levelSize==length(value))){
    stop(paste("given rows do not match any of the levels!"))
  }
  level=x@.data$level[x@.data$levelSize==length(value)]
  if(length(level)>1){
    # multiple levels had the same sizes...
    # add it to the highest
    level=x@.data$level[x@.data$levelNr==max(x@.data$levelNr[x@.data$level %in% level])]
  }
  
  
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
setMethod("colnames", signature(x = "MicroPlate"), function(x) {
  return(x@.data$colNames)
})


#' overwrite colnames<-
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
#' slot needs to be named "object"
#'
#' still gives error:
#' > testData
#' Object of class "MicroPlate"
#' Error in S3Part(object, strictS3 = TRUE) : 
#'  S3Part() is only defined for classes set up by setOldCLass(), basic classes or subclasses of these:  not true of class “MicroPlate”
#' 
#' 
#' @export
setMethod("show", signature(object = "MicroPlate"), function(object) {
  print("steal the show!!!")
  print(object@.data$data)
  print(object@.data$plate)
  return(object)
})


#' overwrite print()
#' @export
setMethod("print", signature(x = "MicroPlate"), function(x) {
#   print("oooh you want to know my secrets???... well they are secret!!!")
#   x@.data
  print(x@.data$data)
  return(x)
})

#' plotPerWell
#' 
#' TODO: add huge amounts of checks and stuff
#' @export
setGeneric("plotPerWell", function(self) standardGeneric("plotPerWell")) 
setMethod("plotPerWell", signature(self = "MicroPlate"), function(self){
  nrOfWells=self@.data$levelSize[self@.data$level=="well"]
  
  for(i in 1:nrOfWells){
    data=self@.data$data$measurement[[i]]
    plot(x=data$time,y=data$value,main=self@.data$data$content[[i]]) 
  }
  return(self)
})

#' plotPerPlate
#' 
#' TODO: add huge amounts of checks and stuff
#' @export
setGeneric("plotPerPlate", function(self) standardGeneric("plotPerPlate")) 
setMethod("plotPerPlate", signature(self = "MicroPlate"), function(self){
  origenalPar=par() # backup plotting pars
  nrOfPlates=self@.data$levelSize[self@.data$level=="plate"]
  nrOfWells=self@.data$levelSize[self@.data$level=="well"]
  for(i in 1:nrOfPlates){
    wells=(1:nrOfWells)[self@.data$data$plate==i]
#     print(wells)
    #get amount of row/columns
    nrRows=max(self@.data$data$row[wells])
    nrColumns=max(self@.data$data$column[wells])
    
    # create a NA grid
#     par(mfcol=c(nrRows,nrColumns))
    layoutMatrix=matrix(data=0,nrow=nrRows,ncol=nrColumns)
    for(i in 1:length(wells)){
      layoutMatrix[self@.data$data$row[wells[i]],self@.data$data$column[wells[i]]]=i
    }
    print(layoutMatrix)
    print(nrRows)
    print(nrColumns)
    layout(mat=layoutMatrix,nrRows,nrColumns)
  
    layout.show(length(wells)) # this takes a while
    
    par(oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))#test
    for(i in wells){
      data=self@.data$data$measurement[[i]]
      plot(x=data$time,y=data$value,main=self@.data$data$content[[i]]) 
    }
    
    par(origenalPar) # restore pars...
  }
  return(self)
})


#' microplate apply
#' MPApply
#' 
#' @export
setGeneric("MPApply", function(self, fun, ...) standardGeneric("MPApply")) 
setMethod("MPApply", signature(self = "MicroPlate"), function(self, fun, ...){
#   funcall=substitute(fun(...))
  x="time"
  y="value"
  
  # for now no input... need to studie formula first....
  
  results=list()
  # for each well
  for(i in 1:self@.data$levelSize[self@.data$level=="well"]){
    x=self@.data$data$measurement[[i]][["time"]]
    y=self@.data$data$measurement[[i]][["value"]]
    results[i]=list(do.call(what=fun,args=list(x=x,y=y,unlist(list(...)))))
  }
  
  
  return(results)

})


#' copy
#'
#' make a copy of the microplate data instance
#' this function is used to get around the default behaviour
#' 
#' @export
setGeneric("copy", function(self) standardGeneric("copy")) 
setMethod("copy", signature(self = "MicroPlate"), function(self){
  copy=new("MicroPlate")
  listOldVars=ls(envir=self@.data, all.names=T)
  for(i in listOldVars){
    copy@.data[[i]]=self@.data[[i]]
  }
  # this could even be an lapply?  
  return(copy)
})


library("grofit")
#' getGrowthRate
#' 
#' what if multiple wavelengths?
#'
#' @export
setGeneric("getGrowthRate", function(self,...) standardGeneric("getGrowthRate")) 
setMethod("getGrowthRate", signature(self = "MicroPlate"), function(self,...){
#   $model.type
#   [1] "logistic"     "richards"     "gompertz"     "gompertz.exp"
  for (i in 1:length(self@.data$data$measurement)){ # for each well
    
  }
  grofit()
  results=gcFit()
  
  return(self)
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
