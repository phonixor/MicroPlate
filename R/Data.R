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
  .Object@.data$levelSize=NULL # number of rows per level
  # need to get a way to get to the right level.... and back???
  # rownames are ignored...
  
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
    currentLevel="well" # default top level!
    #
    # reset meta data
    self@.data$colNames=NULL
    self@.data$colLevel=NULL
    self@.data$colType=NULL
    self@.data$level="well" # default top level is well
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
#' data.frame also has a DUMP slot... no clue what this does... or how to call it...
#' its probably not called... but instead filled when called... don't know its function though...
#' 
#' TODO if no measurement level??!?!?
#' 
#' 
#' @export 
setMethod("[", signature(x = "Data", i = "ANY", j = "ANY"), function(x, i , j, ...) {
  # without "..." nargs() does not work!
  # even if df[] you still get 2 args...
#   print(nargs())
#   print(proc.time())
  col=NULL
  row=NULL
  nrOfRows=x@.data$levelSize[x@.data$level=="measurement"]
  nrOfCol=length(x@.data$colNames)
  #
  # data.frame has some special behaviour
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
  } else if(missing(j) & nargs()==2) {
    # df[1]
#     print("df[1]")
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
  
  # validate input
  if(!is.null(row)){
    if(!(class(row)=="numeric"|class(row)=="integer")){
      stop(paste("row index should be a number, not a: ",class(row)))
    } 
    if(nrOfRows<row){
      stop(paste("Data only has ",nrOfRows," rows, you asked for row(s):",row))
    }
  }
  if(!is.null(col)){
    if(!(class(col)=="numeric" | class(col)=="integer" | class(col)=="character")){
      stop(paste("col index should be a number or char, not a: ",class(col)))
    }
    if(class(col)=="character" & length(wcol<-unique(col[!is.element(col,x@.data$colNames)]))>0 ) {
      stop(paste("columns given that do not exist:", paste(wcol, collapse=", ")))
    }
  }
  # todo add col number check..
  
  
  # make sure you don't fetch dubplicates
  col=unique(col)
  row=unique(row)
  # also change to names if numbers
  if(class(col)!="character"){ 
    col=x@.data$colNames[col]
  }
  
#   print(proc.time())
  #
  # fetch the requested data
  returnValue=data.frame(matrix(nrow=if(!is.null(row)){length(row)}else{nrOfRows},ncol=length(col)))
  colnames(returnValue)=col
  for(colnr in 1:length(col)){ # for each column
    
    level=x@.data$colLevel[x@.data$colNames==col[colnr]]    
#     print(level)
#     print(col[colnr])
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
#   print(proc.time())
  return(returnValue)
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
#   
#   
#   
#   ###########old
#   level=x@.data$colLevel[x@.data$colNames==name]
#   if (is.null(level)){
#     # remove this once i implemented $= properly
#     print("ok this shouldn't happen... but it did!") # change in a warning later...
#     return(x@.data$data[[name]])
#   }
#   
#   
#   if (level=="well"){
#     # data at top level
#     # assume well data for now
#     #     return(x@.data$data[[name]])
#     #
#     # data has to be repeated for each measurement
#     returnValue=NULL
#     #     index=1
#     #     for (i in 1:length(x@.data$data$measurement)){ # for each measurement
#     #       # check the ammount of measurements
#     #       numberOfMeasurement=length(x@.data$data$measurement[[i]][[1]])
#     #       returnValue[index:(index+numberOfMeasurement-1)]=x@.data$data[[name]][[i]]
#     #       index=index+numberOfMeasurement
#     #     }
#     for (i in 1:length(x@.data$data$measurement)){ # for each measurement
#       returnValue=append(returnValue,rep(x@.data$data[[name]][[i]],length(x@.data$data$measurement[[i]][[1]])))
#     }    
#     return(returnValue)
#   } else if(level=="measurement"){
#     # data is hidden in lists in the column measurement
#     returnValue=NULL
#     for(i in 1:length(x@.data$data$measurement)){
#       returnValue=append(returnValue,x@.data$data$measurement[[i]][[name]])
#     }        
#     return(returnValue)
#   } else {
#     warning("data at unknown level")
#   }
#   
#  
#   




# 
# `[.data.frame` <-
#   function(x, i, j, drop = if(missing(i)) TRUE else length(cols) == 1)
#   {
#     mdrop <- missing(drop)
#     Narg <- nargs() - !mdrop  # number of arg from x,i,j that were specified
#     has.j <- !missing(j)
#     if(!all(names(sys.call()) %in% c("", "drop"))
#        && !isS4(x)) # at least don't warn for callNextMethod!
#       warning("named arguments other than 'drop' are discouraged")
#     
#     if(Narg < 3L) {  # list-like indexing or matrix indexing
#       if(!mdrop) warning("'drop' argument will be ignored")
#       if(missing(i)) return(x)
#       if(is.matrix(i))
#         return(as.matrix(x)[i])  # desperate measures
#       ## zero-column data frames prior to 2.4.0 had no names.
#       nm <- names(x); if(is.null(nm)) nm <- character()
#       ## if we have NA names, character indexing should always fail
#       ## (for positive index length)
#       if(!is.character(i) && anyNA(nm)) { # less efficient version
#         names(nm) <- names(x) <- seq_along(x)
#         y <- NextMethod("[")
#         cols <- names(y)
#         if(anyNA(cols)) stop("undefined columns selected")
#         cols <- names(y) <- nm[cols]
#       } else {
#         y <- NextMethod("[")
#         cols <- names(y)
#         if(!is.null(cols) && anyNA(cols))
#           stop("undefined columns selected")
#       }
#       ## added in 1.8.0
#       if(anyDuplicated(cols)) names(y) <- make.unique(cols)
#       ## since we have not touched the rows, copy over the raw row.names
#       ## Claimed at one time at least one fewer copies: PR#15274
#       attr(y, "row.names") <- .row_names_info(x, 0L)
#       attr(y, "class") <- oldClass(x)
#       return(y)
#     }
#     
#     if(missing(i)) { # df[, j] or df[ , ]
#       ## not quite the same as the 1/2-arg case, as 'drop' is used.
#       if(drop && !has.j && length(x) == 1L) return(.subset2(x, 1L))
#       nm <- names(x); if(is.null(nm)) nm <- character()
#       if(has.j && !is.character(j) && anyNA(nm)) {
#         ## less efficient version
#         names(nm) <- names(x) <- seq_along(x)
#         y <- .subset(x, j)
#         cols <- names(y)
#         if(anyNA(cols)) stop("undefined columns selected")
#         cols <- names(y) <- nm[cols]
#       } else {
#         y <- if(has.j) .subset(x, j) else x
#         cols <- names(y)
#         if(anyNA(cols)) stop("undefined columns selected")
#       }
#       if(drop && length(y) == 1L) return(.subset2(y, 1L))
#       if(anyDuplicated(cols)) names(y) <- make.unique(cols)
#       nrow <- .row_names_info(x, 2L)
#       if(drop && !mdrop && nrow == 1L)
#         return(structure(y, class = NULL, row.names = NULL))
#       else {
#         ## Claimed at one time at least one fewer copies: PR#15274
#         attr(y, "class") <- oldClass(x)
#         attr(y, "row.names") <- .row_names_info(x, 0L)
#         return(y)
#       }
#     }
#     
#     ### df[i, j] or df[i , ]
#     ## rewritten for R 2.5.0 to avoid duplicating x.
#     xx <- x
#     cols <- names(xx)  # needed for computation of 'drop' arg
#     ## make a shallow copy
#     x <- vector("list", length(x))
#     ## attributes(x) <- attributes(xx) expands row names
#     x <- .Internal(copyDFattr(xx, x))
#     oldClass(x) <- attr(x, "row.names") <- NULL
#     
#     if(has.j) { # df[i, j]
#       nm <- names(x); if(is.null(nm)) nm <- character()
#       if(!is.character(j) && anyNA(nm))
#         names(nm) <- names(x) <- seq_along(x)
#       x <- x[j]
#       cols <- names(x)  # needed for 'drop'
#       if(drop && length(x) == 1L) {
#         ## for consistency with [, <length-1>]
#         if(is.character(i)) {
#           rows <- attr(xx, "row.names")
#           i <- pmatch(i, rows, duplicates.ok = TRUE)
#         }
#         ## need to figure which col was selected:
#         ## cannot use .subset2 directly as that may
#         ## use recursive selection for a logical index.
#         xj <- .subset2(.subset(xx, j), 1L)
#         return(if(length(dim(xj)) != 2L) xj[i] else xj[i, , drop = FALSE])
#       }
#       if(anyNA(cols)) stop("undefined columns selected")
#       ## fix up names if we altered them.
#       if(!is.null(names(nm))) cols <- names(x) <- nm[cols]
#       ## sxx <- match(cols, names(xx)) fails with duplicate names
#       nxx <- structure(seq_along(xx), names=names(xx))
#       sxx <- match(nxx[j], seq_along(xx))
#     } else sxx <- seq_along(x)
#     
#     rows <- NULL # placeholder: only create row names when needed
#     # as this can be expensive.
#     if(is.character(i)) {
#       rows <- attr(xx, "row.names")
#       i <- pmatch(i, rows, duplicates.ok = TRUE)
#     }
#     for(j in seq_along(x)) {
#       xj <- xx[[ sxx[j] ]]
#       ## had drop = drop prior to 1.8.0
#       x[[j]] <- if(length(dim(xj)) != 2L) xj[i] else xj[i, , drop = FALSE]
#     }
#     
#     if(drop) {
#       n <- length(x)
#       if(n == 1L) return(x[[1L]]) # drops attributes
#       if(n > 1L) {
#         xj <- x[[1L]]
#         nrow <- if(length(dim(xj)) == 2L) dim(xj)[1L] else length(xj)
#         ## for consistency with S: don't drop (to a list)
#         ## if only one row, unless explicitly asked for
#         drop <- !mdrop && nrow == 1L
#       } else drop <- FALSE ## for n == 0
#     }
#     
#     if(!drop) { # not else as previous section might reset drop
#       ## row names might have NAs.
#       if(is.null(rows)) rows <- attr(xx, "row.names")
#       rows <- rows[i]
#       if((ina <- anyNA(rows)) | (dup <- anyDuplicated(rows))) {
#         ## both will coerce integer 'rows' to character:
#         if (!dup && is.character(rows)) dup <- "NA" %in% rows
#         if(ina)
#           rows[is.na(rows)] <- "NA"
#         if(dup)
#           rows <- make.unique(as.character(rows))
#       }
#       ## new in 1.8.0  -- might have duplicate columns
#       if(has.j && anyDuplicated(nm <- names(x)))
#         names(x) <- make.unique(nm)
#       if(is.null(rows)) rows <- attr(xx, "row.names")[i]
#       attr(x, "row.names") <- rows
#       oldClass(x) <- oldClass(xx)
#     }
#     x
#   }

# 
# `[[.data.frame` <- function(x, ..., exact=TRUE)
# {
#   ## use in-line functions to refer to the 1st and 2nd ... arguments
#   ## explicitly. Also will check for wrong number or empty args
#   na <- nargs() - !missing(exact)
#   if(!all(names(sys.call()) %in% c("", "exact")))
#     warning("named arguments other than 'exact' are discouraged")
#   
#   if(na < 3L)
#     (function(x, i, exact)
#       if(is.matrix(i)) as.matrix(x)[[i]]
#      else .subset2(x, i, exact=exact))(x, ..., exact=exact)
#   else {
#     col <- .subset2(x, ..2, exact=exact)
#     i <- if(is.character(..1))
#       pmatch(..1, row.names(x), duplicates.ok = TRUE)
#     else ..1
#     ## we do want to dispatch on methods for a column.
#     ## .subset2(col, i, exact=exact)
#     col[[i, exact = exact]]
#   }
# }
# 
# 
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
#'
#'
#' @export
setMethod("$", signature(x = "Data"), function(x, name) {
  # 
  # x@.data$colLevel[x@.data$colNames==name]
  
  level=x@.data$colLevel[x@.data$colNames==name]
  if (is.null(level)){
    # remove this once i implemented $= properly
    print("ok this shouldn't happen... but it did!") # change in a warning later...
    return(x@.data$data[[name]])
  }
  
  
  if (level=="well"){
    # data at top level
    # assume well data for now
#     return(x@.data$data[[name]])
    #
    # data has to be repeated for each measurement
    returnValue=NULL
#     index=1
#     for (i in 1:length(x@.data$data$measurement)){ # for each measurement
#       # check the ammount of measurements
#       numberOfMeasurement=length(x@.data$data$measurement[[i]][[1]])
#       returnValue[index:(index+numberOfMeasurement-1)]=x@.data$data[[name]][[i]]
#       index=index+numberOfMeasurement
#     }
    for (i in 1:length(x@.data$data$measurement)){ # for each measurement
      returnValue=append(returnValue,rep(x@.data$data[[name]][[i]],length(x@.data$data$measurement[[i]][[1]])))
    }    
    return(returnValue)
  } else if(level=="measurement"){
    # data is hidden in lists in the column measurement
    returnValue=NULL
    for(i in 1:length(x@.data$data$measurement)){
      returnValue=append(returnValue,x@.data$data$measurement[[i]][[name]])
    }        
    return(returnValue)
  } else {
    warning("data at unknown level")
  }
  
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
