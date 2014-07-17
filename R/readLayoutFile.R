# # comment line
#
# /  basic	
# /	1	2	3	4	5	6	7	8	9	10	11	12
# A	sample	sample	sample	sample	sample	sample	sample	sample	sample	sample	sample	sample
# B	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty
# C	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty
# D	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty
# E	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty
# F	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty
# G	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty
# H	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty	empty
# /  sample	
# /	1	2	3	4	5	6	7	8	9	10	11	12				
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
# todo check restricted names
# todo fix .ods
# todo fix .csv



library(gdata)
library(readODS)
#' readLayoutFile
#' 
#' @description
#' Read Experiment File and generates/returns a MicroPlate object
#' 
#' the layout file can be .xls .xlsx .ods
#' TODO add link to true documentation...
#' 
#' @param file the path to the layout file
#' @@param existingMicroPlate if you want to add it to excisting data
#'  
#' @export
#' @include MicroPlate.R
#' @import gdata readODS
setGeneric("readLayoutFile", function(file=NULL) standardGeneric("readLayoutFile")) 
setMethod("readLayoutFile", signature(), function(file=NULL){
#   if(missing(existingMicroPlate)) existingMicroPlate=NULL
  # deteremine filetype
  returnValue=NULL
  splitedFile=unlist(strsplit(file,split = ".",fixed=TRUE))
  extention=casefold(splitedFile[length(splitedFile)], upper = FALSE)
#   print(extention)
  spreadsheet=NULL
  if(extention=="xls" || extention=="xlsx"){
    # uses gdata
    spreadsheet=list()
    for(i in 1:sheetCount(file)){
      spreadsheet[[i]]=read.xls(file, sheet=i, stringsAsFactors=FALSE, header=FALSE, blank.lines.skip=FALSE)
      #does blank.lines.skip actually work?
    }
    
  }else if(extention=="ods"){
    # uses readODS
    spreadsheet=read.ods(file)
    
  }else if(extention=="csv" || extention=="txt"){
    # 
#     print("csv / txt not implemented atm...")
    # need to test this :)
    spreadsheet=read.csv(file, stringsAsFactors=FALSE, header=FALSE)
  }else{
    stop("Unknown file type, please make sure the file has a proper extention.\n supported extentions are sheet formats: xls,xlsx & ods. comma seperated files: csv & txt")
  }
  
  #TODO remove empty sheets
  
#   print(spreadsheet)
  
  nrOfPlates=length(spreadsheet)
  

  for(sheet in spreadsheet){
    print("next sheet!")
    
    nrOfRows=dim(sheet)[1] # rows of the sheet, not of the plate
    nrOfColumns=dim(sheet)[2] # cols of the sheet = cols of the plate
    
    data=NULL
    plateData=NULL
    dataNames=NULL
    
    cols=NULL
    rowLock=TRUE
    rows=NULL
    
    index=0
    continue=TRUE
    while(continue){ # for each row in sheeta=1:10 
      index=index+1
      # 
      # 
      if(trim(sheet[index,1])==""){ # empty line ignore
        #       print("ignore empty line")
      } else if(substr(sheet[index,1],1,1)=="#"){ # comment line ignore
        #       print("comment line ignore")
      } else if(sheet[index,1]=="/"){ # well data
        # add variable name to list
        dataNames=append(dataNames,sheet[index,2])
        currentName=sheet[index,2]
        #       data[[currentName]]=NULL
        print(paste("parsing:", currentName))
        #
        #
        # assume next line is the column list
        index=index+1      
        # use the first column list to check if it is consistent for all future variables.
        # col names/number should start with a "#" 
        if(sheet[index,1]=="/"){
          if(is.null(cols)){
            # set cols 
            cols=sheet[index,2:nrOfColumns]
          } else {
            # check cols
            if(any(cols!=sheet[index,2:nrOfColumns]))stop("columns are not consistent")
          }
        } else stop("wrong format!")
        #
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
        #
        # get data
        while(TRUE){
          index=index+1
          #
          # check if done
          if(index>=nrOfRows){
            rowLock=TRUE
            continue=FALSE
            break
          }
          if(sheet[index,1]=="/" || sheet[index,1]=="" || substr(sheet[index,1],1,1)=="#"){ # end of data
            # end of data section 
            rowLock=TRUE
            index=index-1# row back!
            break # stop!
          }else{
            # do some checks
            if(!rowLock){
              # add stuff to row
              rows=append(rows,sheet[index,1])
            }else{
              # check if row names/numbers are the same 
#               if(rows[...]==sheet[index,1]){
#                 stop("row names inconsistent")
#               }
            }
            # add the data!
#             print("--------------------")
#             print(as.character(sheet[index,2:nrOfColumns]))
            print(length(data[[currentName]]))
            data[[currentName]]=append(data[[currentName]],as.character(sheet[index,2:nrOfColumns]))
            data[[currentName]][data[[currentName]]=="NA"]=NA
            print(length(data[[currentName]]))
          }
        
        }# data section loop
      # data section including >
      } else {
        # asume new plate variable
        variableName=sheet[index,1]
        value=sheet[index,2]
        plateData[[variableName]]=value
        
        
        
        
      }  
      #stop condition
      if(index>=nrOfRows) continue=FALSE
      
      
      
    }#sheet
    
#     
#     # convert to numbers
#     if(all(suppressWarnings(!is.na(as.numeric(cols))))){
#       cols=as.numeric(cols)
#     } else {
#       # asume that its in the A=1, B=2 etc format
#       cols=lettersToNumber(cols)
#       if(any(is.na(cols))){
#         error("unsupported column names, use letters or numbers")
#       }
#     }
#     
#     # convert to numbers
#     if(all(suppressWarnings(!is.na(as.numeric(rows))))){
#       rows=as.numeric(rows)
#     } else {
#       # asume that its in the A=1, B=2 etc format
#       rows=lettersToNumber(rows)
#       if(any(is.na(rows))){
#         error("unsupported row names, use letters or numbers")
#       }
#     }  
#     
#     #
#     #
#     # finishing convert data to proper table
#     for (i in 1:length(rows))
#     {
#       for(j in 1:length(cols)){
#         data[["column"]]=append(data[["column"]],cols[j])
#         data[["row"]]=append(data[["row"]],rows[i])
#       }
#     }


#     print(data)
#     print("_________________")
    print("data parsed...")
    # smartbind data or something...
    

#       print(existingMicroPlate)
#       print(data$plate)
    print(plateData[["dataFile"]])
    print(plateData[["parser"]])
#       print(data$plate$dataFile)
    dataFile=plateData[["dataFile"]]
    parser=plateData[["parser"]]
    plateName=plateData[["plateName"]]
    
#     layoutData=data[names(data)!="plate"]# don't include plate numbers
#     print(layoutData)
  
    measuredData=eval(parse(text=paste(parser,"('",dirname(file),"/",dataFile,"')",sep="")))
    # add plate data
    print(names(plateData))
    for(i in 1:length(plateData)){
#       print(names(plateData)[i])
#       print(plateData[[names(plateData)[i]]])
      measuredData[names(plateData)[i]]=plateData[[names(plateData)[i]]]
    }
    # add layout data at well level
    
    # TODO BETTER CHECKS!!!
#     selector=1:length(data$basic)[(data$basic!="empty")]

    for(i in 1:length(data)){
      # TODO NEEDS CHECKS!!!!
      print(names(data)[i])
      print(data[[names(data)[i]]])
      
      measuredData[names(data)[i]]=data[[names(data)[i]]]
      
    }
    



    updateColnames(measuredData)
    measuredData
    returnValue=measuredData
#     addPlate(existingMicroPlate, newData=measuredData, layoutData=layoutData, plateName=plateName )
    print("___________________________________")
#       print(measuredData)
      

#       existingMicroPlate@.data$data=measuredData

      
#       existingMicroPlate@.data$plate=data$plate
#       print(existingMicroPlate)
#       print(data[names(data)])
#       existingMicroPlate@.data$data=data[names(data)!="plate"]
      #       existingMicroPlate=existingMicroPlate@.data$data
#       print("_________________")
#       print(existingMicroPlate)

    
    
    
  }#spreadsheet
  
#   updateColnames(existingMicroPlate)
  return(returnValue)
})


#' Read Experiment File
#' 
#' @param file old dont use
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
#' @description
#' returns a list from a sting separating it with tabs
#' 
#' @keywords internal
#' 
#' @param line the line given
#' 
setGeneric("listFromLine", function(line=NULL) standardGeneric("listFromLine")) 
setMethod("listFromLine", signature(), function( line=NULL){
  list=strsplit(line,"\t")
  return(list[[1]])
})


