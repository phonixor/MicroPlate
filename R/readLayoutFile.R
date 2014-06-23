# # Layout for 2777.dbf  											
# >  basic	
# #	1	2	3	4	5	6	7	8	9	10	11	12
# A	sample	sample	sample	sample	sample	sample	sample	sample	sample	sample	sample	sample
# B	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty
# C	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty
# D	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty
# E	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty
# F	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty
# G	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty
# H	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty
# >  sample	
# #	1	2	3	4	5	6	7	8	9	10	11	12				
# A	A	A	B	B	C	C	D	D	E	E	F	F
# B	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA
# C	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA
# D	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA
# E	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA
# F	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA
# G	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA
# H	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA	NA
# 
# 
#
library(gdata)
library(readODS)

#' Read Experiment File
#'
#' @export
setGeneric("readLayoutFile", function(file=NULL) standardGeneric("readLayoutFile")) 
setMethod("readLayoutFile", signature(), function( file=NULL){
  # deteremine filetype
  splitedFile=unlist(strsplit(file,split = ".",fixed=TRUE))
  extention=casefold(splitedFile[length(splitedFile)], upper = FALSE)
#   print(extention)
  
  if(extention=="xls" || extention=="xlsx"){
    # uses gdata
    data=list()
    for(i in 1:sheetCount(file)){
      data[[i]]=read.xls(file, sheet=i, stringsAsFactors=FALSE, header=FALSE, blank.lines.skip=FALSE)
    }
    
    
  }else if(extention=="ods"){
    # uses readODS
    data=read.ods(file)
    
  }else if(extention=="csv" || extention=="txt"){
    # 
    
  }else{
    stop("Unknown file type, please make sure the file has a proper extention.\n supported extentions are sheet formats: xls,xlsx & ods. comma seperated files: csv & txt")
  }
  
  print(data)
  
  
  
    
})
  



#' Read Experiment File
#' 
#' @export
setGeneric("readLayoutFile2", function(file=NULL) standardGeneric("readLayoutFile2")) 
setMethod("readLayoutFile2", signature(), function( file=NULL){
  # maybe change it to get restricted names from Data...
  # though row and column arent their...
  reservedVariables=c("plate","well","measurement","row","column")
  lines <- readLines(file, warn=FALSE) # do not need warning if missing final EOL
  #
  # storage
  data=list()
  dataNames=NULL
  #
  # 
  rows=NULL
  rowLock=FALSE
  nrOfRows=0
  cols=NULL
  colLock=FALSE
  errorList=NULL
  #
  #
  index=1
  line=lines[index]
  continue=TRUE
  #
  while (continue){
#     print(line)
#     print(substring(line,1,1))




    if(trim(line)==""){ # empty line ignore
#       print("ignore empty line")
    } else if(substring(line,1,1)=="#"){ # comment line ignore
#       print("comment line ignore")
    } else if(substring(line,1,1)==">"){ # variable
      # add variable name to list
#       print(line)
      line=listFromLine(line)
#       print(typeof(line))
#       print(class(line))
#       print(line)
#       print(line[2])
#       print(line[[2]])
      dataNames=append(dataNames,line[2])
      currentName=line[2]
#       data[[currentName]]=NULL
      print(paste("parsing:", currentName))
      # assume next line is the column list
      index=index+1
      line=listFromLine(lines[index])
      if(substring(line[1],1,1)=="#"){
        if(!colLock){
          # set cols
          cols=line[2:length(line)]
          colLock=TRUE
          print(paste("nr of cols =",length(cols)))
        } else {
          # check cols
          if(any(cols!=line[2:length(line)])){
            stop("columns are not consistent")
          }
        }
      } else {
        stop("column line expected after a > line")
      }
      
      # get data
      while(TRUE){
        index=index+1
        line=lines[index]
        #
        # check if done
        if(is.na(line) || substring(line,1,1)==">" || trim(line)=="" || substring(line,1,1)=="#"){ # end of data
          index=index-1
          #
          # do some checks
          if(!rowLock){
            print(paste("nr of rows =",length(rows)))
            rowLock=TRUE
            nrOfRows=length(rows)
          } else {
            if(length(rows)%%nrOfRows!=0){
              stop("row numbers are not consistent")
            }
            
          }
          
          # TODO check row
          break
        }
        # assume its data
        line=listFromLine(line)
        
        if(!rowLock){
          # set row
          rows=append(rows,line[1])
        }
        
        if((length(line)-1)!=length(cols)){
          # would be a weird error as excel would enforce this
          stop("not enough data")
        }
        
        for (i in 2:length(line)){
          data[[currentName]]=append(data[[currentName]],line[i])
        }
      }
    } else {
      warning("the end or dont know what todo...")
    }

    # prepare for next line
    index=index+1
    line=lines[index]

    if(is.na(line)){# end of file
      continue=FALSE
      # finish up...
    } 
  } # main while loop

  # convert to numbers
  if(all(suppressWarnings(!is.na(as.numeric(cols))))){
    cols=as.numeric(cols)
  } else {
    # asume that its in the A=1, B=2 etc format
    cols=lettersToNumber(cols)
    if(any(is.na(cols))){
      error("unsupported column names, use letters or numbers")
    }
  }

  # convert to numbers
  if(all(suppressWarnings(!is.na(as.numeric(rows))))){
    rows=as.numeric(rows)
  } else {
    # asume that its in the A=1, B=2 etc format
    rows=lettersToNumber(rows)
    if(any(is.na(rows))){
      error("unsupported row names, use letters or numbers")
    }
  }  

  #
  #
  # finishing convert data to proper table
  for (i in 1:length(rows))
  {
    for(j in 1:length(cols)){
      data[["column"]]=append(data[["column"]],cols[j])
      data[["row"]]=append(data[["row"]],rows[i])
    }
  }
#   data[["col"]]=rep(length(rows),cols)
#   data[["row"]]=rep(length(cols),rows)
  
  # convert to data.frame
  data=data.frame(data,stringsAsFactors=FALSE)

  return(data)
})

  
#' listFromLine
#' 
#' @export
setGeneric("listFromLine", function(line=NULL) standardGeneric("listFromLine")) 
setMethod("listFromLine", signature(), function( line=NULL){
  list=strsplit(line,"\t")
  return(list[[1]])
})






# 
# ## Constructor for the MicroplateFrame class.
# ## Reads a layout from a layout file. A plateFormat must be provided. It is
# ## used to read and check for consistency. A platename must be provided if the 
# ## layout file does not define one. If the platename argument is provided, it 
# ## will override the plate name provided in the layout file
# "read.Layout" <- function (file, formatID, platename=NULL) {
#   format <- plateFormat(formatID)
#   rsvVar <- getOption("MicroP")$rsvVar
#   rsvVarProtected <- unlist(lapply(rsvVar,"[[","protected"))
#   rsvVarDescription <- unlist(lapply(rsvVar,"[[","description"))
#   lines <- readLines(file, warn=FALSE) # do not need warning if missing final EOL
#   # CHECK THE STRUCTURE OF THE FILE
#   # ms0 will collect error messages of fatal errors for a layout file 
#   # that can still be parsed fairly well
#   ms0 <- c()
#   # Detect comment, variable, plate name, and data lines. Plate name lines 
#   # are the first non-comment lines before the variable and comment lines.
#   # Data lines are lines that do not belong to the other three categories.
#   cmtlines <- grepl("^#.*$|^[[:space:]]*$", lines)
#   varlines <- grepl("^>", lines)
#   namlines <- !(cmtlines | varlines)
#   namlines[min(which(cmtlines | varlines)):length(lines)] <- FALSE
#   datlines <- !(cmtlines | varlines | namlines)
#   dnrs <- which(datlines)
#   vnrs <- which(varlines)
#   nnrs <- which(namlines)
#   # Stop if there are no variable lines: no further parsing possible
#   if (!any(varlines)) 
#     stop(paste("No variable lines detected in the layout file,",
#                "i.e. lines starting with \">\"")
#     )
#   # Check variable names
#   varnames <- make.names(.colFromLines(lines[varlines],2), unique=TRUE)
#   ## Translate reserved variables to canonical lower case name
#   basenames <- which(tolower(varnames) %in% names(rsvVar))
#   varnames[basenames] <- tolower(varnames[basenames])
#   ## Are there protected variables in the layout file?
#   if (any(rsvVarProtected[varnames[varnames %in% names(rsvVarProtected)]])) {
#     protname <- 
#       names(rsvVarProtected[varnames[varnames %in% names(rsvVarProtected)]])[
#         which(rsvVarProtected[varnames[varnames %in% names(rsvVarProtected)]])]
#     ms0 <- c(ms0, paste("Protected variable names not allowed in layout:",
#                         paste("\"", protname, "\"", collapse=", ", sep=""))
#     )
#   }
#   # Determine the plate name provided by the file
#   platenameFromFile <- NULL
#   if (length(nnrs) > 0) {
#     if (length(nnrs) > 1) {
#       warning("Multiple plate name lines (",
#               paste(nnrs, collapse=", "),
#               "). Only first line will be used.")
#     }
#     platenameFromFile <- unlist(strsplit(lines[nnrs[1]],"\t"))[1]
#   }
#   # If both platename and platenameFromFile are provided, issue a warning
#   # that platenameFromFile will be used
#   if (!is.null(platenameFromFile) && !is.null(platename)) {
#     message("Both layout file and \"platename\" argument provide a plate name.",
#             "\n  The plate name from the \"platename\" argument will be used.")
#   }
#   # Set plate name to the one from file if platename argument was NULL
#   if (is.null(platename) && !is.null(platenameFromFile))
#     platename <- platenameFromFile
#   # Test for wrong plate name
#   platename <- make.names(platename)
#   # If no platename is given in file or argument then issue error
#   if(is.null(platename)) {
#     ms0 <- c(ms0, paste("No platename defined. Platename must be given",
#                         "either in layout file or in \"platename\" argument")
#     )
#   }
#   # Determine data lines of sub-layouts
#   names(vnrs) <- varnames
#   sublayouts <- list()
#   for (i in seq(along=vnrs)) {
#     if (i < length(vnrs)) {
#       sublayouts[[varnames[i]]] <- dnrs[dnrs > vnrs[i] & dnrs < vnrs[i+1]]
#     } else {
#       sublayouts[[varnames[i]]] <- dnrs[dnrs > vnrs[i]]
#     }
#   }
#   # Check number of well columns and rows
#   wrongrows <- 
#     names(which(sapply(lapply(sublayouts, length),"!=",rows(format))))
#   if (length(wrongrows) > 0) {
#     ms0 <- c(ms0, paste("Number of rows does not match with plate format", 
#                         "in sublayouts", paste("\"",wrongrows,"\"", sep="", collapse=", "))
#     )
#   }
#   wrongcols <- which(datlines & 
#                        sapply(strsplit(sub("\t$","\t|",lines),"\t"), length) < columns(format)+1)
#   if (length(wrongcols) > 0) {
#     ms0 <- c(ms0, paste("Number of columns does not match with plate format", 
#                         "in lines", paste(wrongcols, collapse=","))
#     )
#   } else {
#     warncols <- which(datlines & 
#                         sapply(strsplit(lines,"\t"), length) > columns(format)+1)
#     if (length(warncols) > 0) {
#       warning("Number of columns in lines ",paste(warncols, collapse=","), 
#               " larger than plate format.\n  Ignoring excess columns.")
#     }
#   }
#   # Finished analyzing the file structure. Start building the output, or 
#   # stop and issue error messages
#   if (length(ms0) > 0) {
#     stop(.niceError(ms0))
#   } else {
#     wells <- wells(format)
#     data <- list(
#       'formatID'  = as.factor(rep(formatID, wells)),
#       'plate'     = as.factor(rep(platename, wells)),
#       'row'       = as.factor(rep(rownames(format), each=columns(format))),
#       'column'    = as.factor(rep(colnames(format), rows(format))),
#       'well'      = as.factor(wellnames(format)),
#       'basic'     = as.factor(rep("sample", wells)),
#       'experiment'= as.factor(rep("###", wells))
#     )
#     wellLabels = paste(platename, wellnames(format, byrow=TRUE), sep=".")
#     ## construct the SampleFrame@x list
#     colrange <- 2:(columns(format) + 1)
#     for (vname in varnames) {
#       values <- .colFromLines(lines[sublayouts[[vname]]], 
#                               colrange, na.regexp="^[[:space:]]*$|^NA$", ignore.case=TRUE)
#       ## Convert to factor or numerical, if possible
#       ## TODO: make class of layout variable controllable by user
#       if (any(is.na(suppressWarnings(as.numeric(values))) & !is.na(values))) {
#         values <- as.factor(values)
#       } else {
#         values <- as.numeric(values)
#       }
#       ## Check the reserved variables, translate standard values if necessary
#       if (vname %in% names(rsvVar)) {
#         matchlist <- .matchRsvValues(vname, values)
#         values <- matchlist$values
#         ms0 <- c(ms0, matchlist$errors)
#       }
#       data[[vname]] <- values
#     }
#     ## Construct the varMetadata data.frame
#     descriptions <- rsvVarDescription[names(data)]
#     names(descriptions) <- names(data)
#     descriptions[is.na(descriptions)] <- 
#       .colFromLines(lines[vnrs[names(data)]], 3, 
#                     na.regexp="^[[:space:]]*$|^NA$", ignore.case=TRUE
#       )[is.na(descriptions)]
#     descriptions[is.na(descriptions)] <- 
#       .firstToUpper(names(descriptions)[is.na(descriptions)])
#     units <- .colFromLines(lines[vnrs[names(data)]], 4, 
#                            na.regexp="^[[:space:]]*$|^NA$", ignore.case=TRUE)
#     recognizedUnits <- units %in% character(0L) # TODO: make a Unit class
#     metadata <- data.frame(
#       labelDescription = descriptions,
#       unit             = units,
#       recognizedUnit   = recognizedUnits,
#       stringsAsFactors = FALSE,
#       row.names        = names(data)
#     )
#     if (length(ms0) > 0) {
#       stop(.niceError(ms0))
#     } else {
#       output <- (new("MicroplateFrame", x=data, varMetadata=metadata, 
#                      sampleLabels=wellLabels))
#       if (any(output$basic=='empty'))
#         output <- output[,-which(output$basic=='empty')]
#       return(output)
#     }
#   }
#   
# })