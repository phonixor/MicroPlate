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
# todo check restricted names
# todo fix .csv
#
# TODO create a function to give an example file to a user specified location...
# so they dont mess up the syntax and stuff...
# propably just a copy thing... cause dynamically writing one is... eeuh possible.. but package dependand...
# and i can't do that shit yet with my readODS package (ITS READ ODS!! NOT WRITEODS!!)



library(readODS)
#' readLayoutFile
#' 
#' @rdname readLayoutFile
#' @description
#' Read Experiment File and generates/returns a MicroPlate object
#' 
#' the layout file can be .xls .xlsx .ods
#' TODO add link to true documentation...
#' 
#' @param file the path to the layout file
#' @param existingMicroPlate if you want to add it to excisting data
#'  
#' @export
#' @include MicroPlate.R
setGeneric("readLayoutFile", function(file=NULL, existingMicroPlate=NULL) standardGeneric("readLayoutFile")) 
#' @rdname readLayoutFile
setMethod("readLayoutFile", signature(), function(file=NULL, existingMicroPlate=NULL){
  if(missing(existingMicroPlate)) existingMicroPlate=NULL
  
  firstPlate=TRUE
  
  #TODO remove empty sheets
  spreadsheet=read.sheet(file)
#   print(spreadsheet)
  
  nrOfPlates=length(spreadsheet)
#   print(spreadsheet)  

  for(sheet in spreadsheet){
    print("next sheet!")

    nrOfRows=dim(sheet)[1] # rows of the sheet, not of the plate
    nrOfColumns=dim(sheet)[2] # cols of the sheet = cols of the plate
#     print(paste("nrOfRows:",nrOfRows,"nrOfColumns:",nrOfColumns))
    
    
    data=NULL
#     data$plate=list()
    dataNames=NULL
    
    cols=NULL # store the col numbers
    rowLock=TRUE # in the first pass row numbers will be collected
                 # to compare with the next sections, and to construct the row column
    rows=NULL # store the row numbers/letters
    
    index=0
    continue=TRUE
    while(continue){ # for each row in sheet 
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
        # col names/number should start with a "/"
        # this should be 1 2 3 4 5 6 7 8
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
            stop("unsupported column names, use letters or numbers")
          }
        }
        #
        # get data
        localIndex=0
        while(TRUE){ # for each row in a data section
          index=index+1
          localIndex=localIndex+1
          #
          # check if done
          if(index>nrOfRows){
            rowLock=FALSE
            continue=FALSE
            break
          }
          if(sheet[index,1]=="/" || sheet[index,1]=="" || substr(sheet[index,1],1,1)=="#"){ # end of data
            # end of data section 
            rowLock=FALSE
            index=index-1# row back!
            break # stop!
          }
          # do some checks
          if(rowLock){
            # add stuff to row
            rows=append(rows,sheet[index,1])
          }else{
            # check if row names/numbers are the same 
            if(rows[localIndex]!=sheet[index,1]){
              stop("row names inconsistent")
            }
          }
          # add the data!
#             print("--------------------")
#             print(as.character(sheet[index,2:nrOfColumns]))
          data[[currentName]]=append(data[[currentName]],as.character(sheet[index,2:nrOfColumns]))
          data[[currentName]][data[[currentName]]=="NA"]=NA # shoudnt this be lower?        
        }# data section loop
        # test if data was numeric
        if(!givesWarning(as.numeric(data[[currentName]]))){
          data[[currentName]]=as.numeric(data[[currentName]]) # convert to numeric
        }
        
      # data section including >
      } else { # plate data
        # asume new plate variable
        variableName=sheet[index,1]
        value=sheet[index,2]
        data$plate[[variableName]]=value
        
      }  
      #stop condition
      if(index>=nrOfRows) continue=FALSE
      
      
      
    }#row in sheet
    
    # creating 
      
    # convert to numbers
    if(all(suppressWarnings(!is.na(as.numeric(cols))))){
      cols=as.numeric(cols)
    } else {
      # asume that its in the A=1, B=2 etc format
      cols=lettersToNumber(cols)
      if(any(is.na(cols))){
        stop("unsupported column names, use letters or numbers")
      }
    }
   
    # convert to numbers
    if(all(suppressWarnings(!is.na(as.numeric(rows))))){
      rows=as.numeric(rows)
    } else {
      # asume that its in the A=1, B=2 etc format
      rows=lettersToNumber(rows)
      if(any(is.na(rows))){
        stop("unsupported row names, use letters or numbers")
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
    
#     print(data)
#     print(data$plate)
#     print("_________________")
   
    print("data parsed...")
#     print(paste("layout has rows:",length(data$row),"columns:",length(data$column)))
#     print(unique(data$row))
#     print(unique(data$column))
    # smartbind data or something...
    if(firstPlate){
      firstPlate=FALSE
      if(is.null(existingMicroPlate)){
        # new plate
        # layoutfile needs to point to plate data file:
        if(any(is.null(data$plate[["dataFile"]]),is.null(data$plate[["parser"]]))){
          stop("no existingMicroplate given, and no dataFile + parser found in layout file")
        }
        dataFile=data$plate[["dataFile"]]
        parser=data$plate[["parser"]]
        plateName=data$plate[["plateName"]]
        existingMicroPlate=eval(parse(text=paste(parser,"('",dirname(file),"/",dataFile,"')",sep="")))
      } else {
        # add layout to existing plate
        if(existingMicroPlate@.data$levelSize[3]!=1){
          stop("only allowed to add layout data to single plates at a time")
        }
      }
      addLayoutDataToMicroPlate(existingMicroPlate,data)
    }else{
      # only happends if the layoutfile has multiple sheets
      if(any(is.null(data$plate[["dataFile"]]),is.null(data$plate[["parser"]]))){
        stop("no existingMicroplate given, and no dataFile + parser found in layout file")
      }
      dataFile=data$plate[["dataFile"]]
      parser=data$plate[["parser"]]
      plateName=data$plate[["plateName"]]
      newMp=eval(parse(text=paste(parser,"('",dirname(file),"/",dataFile,"')",sep="")))
      addLayoutDataToMicroPlate(newMp,data)
      merge(existingMicroPlate,newMp)
    }
  }#spreadsheet
  
#   updateColnames(existingMicroPlate)
  return(existingMicroPlate)
})





#' addLayoutDataToMicroPlate
#' 
#' @keywords internal
#' @rdname addLayoutDataToMicroPlate
#' @description
#' ...
#' 
#' add layout data to last plate in MicroPlate
#' 
#' TODO add better support for multipleplate
#' 
#' 
#' @param self the MicroPlate you want to add things to
#' @param layoutData the data from a layout file you want to add
#' 
#'  
#' @export
#' @include MicroPlate.R
setGeneric("addLayoutDataToMicroPlate", function(self=NULL, layoutData=NULL) standardGeneric("addLayoutDataToMicroPlate")) 
#' @rdname addLayoutDataToMicroPlate
setMethod("addLayoutDataToMicroPlate", signature(self="MicroPlate"), function(self=NULL, layoutData=NULL){
  print("addLayoutDataToMicroPlate!!")
  wellData=layoutData[names(layoutData)!="plate"]
  plateData=layoutData$plate
  # well + measurement
  index=which(self@.data$well$column==wellData$column & self@.data$well$row==wellData$row & self@.data$well$plate==self@.data$levelSize[3])
#   print(index)
  #
  
  if(length(index)!=self@.data$wellsPerPlate[self@.data$levelSize[3]]){
    stop( paste("Layout and MicroPlate sizes are not the same layout:",length(index),"MicroPlate:",self@.data$wellsPerPlate[self@.data$levelSize[3]]) )
  }
  # add the data
  #data[names(data)!="plate"]
  wellData=data.frame(wellData,stringsAsFactors = F)
  colNames=base::colnames(wellData)[!base::colnames(wellData) %in% c("row","column")] #everything except row, column, plate 
#   print("---------------")
#   print(colNames)
#   print(wellData)
#   print("---------------")
#   print(wellData[colNames])
  self[index,colNames,level="well"]=wellData[colNames]
  
  # plate data
  colNames=names(plateData)
  index=self@.data$levelSize[3]
#   print("___________________________________")
#   print(index)
#   print(colNames)
#   print(plateData[colNames])
#   print("___________________________________")
#   self[index,colNames,level="plate"]=plateData[colNames]
  self[colNames,level="plate"]=plateData[colNames]
})


