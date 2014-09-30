# example novostar dbf file
# lots of columns in the middle removed
#
#    WELLNUM CONTENT CH      M1      M2      M3       M49      M50 PLATZ KONZ_CHAR KONZ_MANT DILUTION TIME TEMP VOLPUMP1 VOLPUMP2 VOLPUMP3 VOLPUMP4
# 1     <NA>    <NA>  t  0.0000 10.0000 20.0000  727.0000 737.0000    NA        NA        NA       NA   NA   NA       NA       NA       NA       NA
# 2     <NA>    <NA>  C 30.0000 29.9000 29.9000   30.2000  30.2000    NA        NA        NA       NA   NA   NA       NA       NA       NA       NA
# 3      A01       B  1  1.6139  1.6112  1.6122    1.4371   1.4281    NA         0         0        1   NA   NA        0        0        0        0
# 4      A02       B  1  1.5900  1.6153  1.5811    1.4196   1.4533    NA         0         0        1   NA   NA        0        0        0        0
# 5      A03      X1  1  1.5801  1.5881  1.5998    0.2138   0.2199    NA         0         0        1   NA   NA        0        0        0        0
# 6      A04      X2  1  1.5842  1.5833  1.5723    0.1919   0.1860    NA         0         0        1   NA   NA        0        0        0        0
# 7      A05      X3  1  1.5808  1.5858  1.5751    0.2373   0.2282    NA         0         0        1   NA   NA        0        0        0        0
# 8      A06      X4  1  1.5770  1.5858  1.5643    0.2212   0.2200    NA         0         0        1   NA   NA        0        0        0        0
# 9      A07      X5  1  1.5714  1.5668  1.5692    0.4137   0.3739    NA         0         0        1   NA   NA        0        0        0        0
# 10     A08      X6  1  1.5754  1.5795  1.5910    0.4898   0.4685    NA         0         0        1   NA   NA        0        0        0        0
# 11     A09      X7  1  1.5916  1.5923  1.5939    0.2753   0.2473    NA         0         0        1   NA   NA        0        0        0        0
# 12     A10      X8  1  1.5757  1.5619  1.5683    0.6907   0.6811    NA         0         0        1   NA   NA        0        0        0        0
# 13     A11      X9  1  1.6034  1.5871  1.5988    1.1699   1.1607    NA         0         0        1   NA   NA        0        0        0        0
# 14     A12     X10  1  1.5871  1.5830  1.6075    1.1403   1.1124    NA         0         0        1   NA   NA        0        0        0        0
#
# novostat appears to work with 96 well plates..
# TODO: we need way more samples of this data! as there are lots of columns with NA / 0 values... 
# TODO: also add time per row instead of column ...
#
# 


library(foreign)
#' a parser for the novostar platereader's dbf files
#' 
#' @description
#' TODO better plate identification
#' 
#' @param path path to the novostar .dbf file.
#' @param name becomes the plateName
#' 
#' @export
#' @import foreign
"novostar.dbf"=function(path=NULL,name=NULL){
  #   print(path)
  # extract data (with function from the foreign package)
  # Suppress lots of warnings: because novostar dbf seem to have a wrong 
  # column type annotation
  # TODO: make tryCatch construct around read.dbf
  buffer <- suppressWarnings(read.dbf(path))
  # TODO: Build in check for valid Novostar dbf structure
  #
  # everything is now in a big data frame  
  #
  # Measurement column names have the format M\d+ 
  measurements <- grep  ("^M[[:digit:]]+$", colnames(buffer))
  
  # CH column indicates data channels(?)
  # Rows with CH=[numeric] represent wells
  # Rows with CH=t represent time (only measurement columns)
  # Rows with CH=C represent temperature (only measurement colums)
  channels <- buffer[,"CH"]
  # WELLNUM column contains well names
  wellNames <- as.character(buffer[channels=="1", "WELLNUM"]) # is the channel always 1 for data???
  content=as.character(buffer[channels=="1", "CONTENT"])
  nofWells <- length(wellNames)
  wellData <- buffer[channels=="1", measurements] # is the channel always 1 for data???
  timePoints <- as.numeric(buffer[channels=="t", measurements])
  temperature <- as.numeric(buffer[channels=="C", measurements])
  #
  #
  l=list()
  m=list()
  nrOfMeasurements=(dim(wellData)[1])*(dim(wellData)[2])
  
  m$value=numeric(nrOfMeasurements)
  m$time=numeric(nrOfMeasurements)
  m$waveLength=numeric(nrOfMeasurements)
  
  measurementIndex=1
  nextMeasurementIndex=1
  nrOfDataRows=dim(wellData)[1]
  row=1:nrOfDataRows

  nrOfDataCols=dim(wellData)[2]
  for (i in 1:nrOfDataCols) { # for each well
    measurementIndex=nextMeasurementIndex
    nextMeasurementIndex=measurementIndex+nrOfDataRows
      
    #well
    coordinates=extractPlateCoordinates(wellNames[i])
    l$row[i]=coordinates[1]
    l$column[i]=coordinates[2]
    l$content[i]=content[i]
    l$plate[i]=1 # foreign key
    l$measurement[i]=measurementIndex # kind of a foreign key
    
    # measurement
    index=measurementIndex:(nextMeasurementIndex-1)
    m$value[index]=as.numeric(wellData[row,i])
    m$time[index]=timePoints[i]
    m$temp[index]=temperature[i]
  }
  
  # transform it into a MicroPlate
  returnValue=new("MicroPlate")
  returnValue@.data$well=l
  returnValue@.data$measurement=m
  
  if(is.null(name)){
    returnValue@.data$plate$plateName=basename(path)
  }else{
    returnValue@.data$plate@plateName=name
  }
  
  updateMetaData(returnValue)
  
  return(returnValue)
}


#' a parser for the novostar platereader's xls files
#' 
#' @description
#' TODO: multiple wavelengths
#' TODO: seconds/days
#' 
#' @param path the path to the novostar excel file
#' @param name become the plateName
#' 
#' @export
"novostar.xls"=function(path=NULL,name=NULL){
  print("novostar.xls!!!")
#   xls = read.xls(path,stringsAsFactors=FALSE) # FUCK FACTORS!!!
  xls = read.sheet(path,sheet = 1)
  # appears to ingnore empty rows
#   print(head(xls)[1:10])
  
#   print(dim(xls))
#   print(xls)
#   print("cookies")
  

#   print(xls[5,1])
  if(xls[5,1]!="Well\nRow"){
    stop("data not found!")
  }
  
  # get headers
  header=xls[5,]
  
#   print(header)
  time=NULL
  waveLength=NULL
  for(i in 4:length(header)){
#     "Raw Data (600)\n1 - 0 h  " 
#     "Raw Data (600)\n2 - 0 h 10 min "	
#     "Raw Data (600)\n3 - 0 h 20 min "
#     print(header[i])
#     print(typeof(header[i]))
  
    temp=gsub("\n"," ",header[i])
    temp=strsplit(temp," ")[[1]]
    temptime=0
    
#     print("-------------")
#     print(temp)
#     print(length(temp))
    for(j in 1:length(temp)){ # for each word in the header
      if(j==3){
        #(waveLength)
#         print("WAVE")
#         print(substr(temp[j],2,nchar(temp[j])-1))
        waveLength=append(waveLength,as.numeric(substr(temp[j],2,nchar(temp[j])-1)))
      }
      
      if(temp[j]=="h"){
#         print(temp[j-1])
        temptime=temptime+as.numeric(temp[j-1])*60
      }
      if(temp[j]=="min"){
        temptime=temptime+as.numeric(temp[j-1])
      }
    }
    time=append(time,temptime)
  }
  

  l=list()
  m=list()
  nrOfMeasurements=(dim(xls)[1]-5)*(dim(xls)[2]-3)
  
  m$value=numeric(nrOfMeasurements)
  m$time=numeric(nrOfMeasurements)
  m$waveLength=numeric(nrOfMeasurements)
  col=4:dim(xls)[2]

  measurementIndex=1
  nextMeasurementIndex=1
  nrOfDataCols=dim(xls)[2]-3
  
  dataRows=6:dim(xls)[1]
  for(row in dataRows){ # for each row
    measurementIndex=nextMeasurementIndex
    nextMeasurementIndex=measurementIndex+nrOfDataCols
    #well
    l$row=append(l$row, xls[row,1])
    l$column=append(l$column,as.numeric(xls[row,2]))
    l$content=append(l$content,xls[row,3])
    l$plate=append(l$plate,1) # foreign key
    l$measurement=append(l$measurement,measurementIndex) # kind of a foreign key
    
    # measurement
    index=measurementIndex:(nextMeasurementIndex-1)
    m$value[index]=as.numeric(xls[row,col])
    m$time[index]=time[col-3]
    m$waveLength[index]=waveLength[col-3]
  }
  l$row=lettersToNumber(l$row)# change letters to numbers
  
  # transform it into a MicroPlate
  returnValue=new("MicroPlate")
  returnValue@.data$well=l
  returnValue@.data$measurement=m
  
  if(is.null(name)){
    returnValue@.data$plate$plateName=basename(path)
  }else{
    returnValue@.data$plate@plateName=name
  }

  updateMetaData(returnValue)

  return(returnValue)
}


