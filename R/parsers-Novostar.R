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
#' TODO better plate identification
#' 
#'  @export
#'  @import foreign
"novostar.dbf"=function(path=NULL,name=Null){
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
  measurements <- grep("^M[[:digit:]]+$", colnames(buffer))
  
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
  #
  #   df=data.frame(row=numeric(0),column=numeric(0),temp=numeric(0),time=numeric(0),value=numeric(0),content=str(0)) # create the df
  #   l=list(row=numeric(0),column=numeric(0),content=str(0),measurement=NULL)
  l=list()
  for (i in 1:length(wellNames)) { # for each well
    coordinates=extractPlateCoordinates(wellNames[i])
    
    l$row[i]=coordinates[1]
    l$column[i]=coordinates[2]
    l$content[i]=content[i]
    #     print(l)
    #     print(i)
    #     l[["measurement"]][1]=list()
    temp=list()
    for (j in 1:length(measurements)){ # for each measurement
      temp$value=append(temp$value,wellData[i,j])
      temp$time=append(temp$time,timePoints[j])
      temp$temp=append(temp$temp,temperature[j])
    } 
    l[["measurement"]][[i]]=temp
  }
  return(l)
}


library(gdata)
#' a parser for the novostar platereader's xls files
#' 
#' TODO: multiple wavelengths
#' TODO: seconds/days
#' 
#' @export
#' @import gdata
"novostar.xls"=function(path=NULL,name=Null){
  print("novostar.xls!!!")
  xls = read.xls(path,stringsAsFactors=FALSE) # FUCK FACTORS!!!
  # appears to ingnore empty rows
  #   print(head(xls)[1:10])
  
#   print(dim(xls))
#   print("cookies")
  
  
  if(xls[4,1]!="Well\nRow"){
    stop("data not found!")
  }
  
  # get headers
  header=xls[4,]
  
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
  
#   print("time....")
#   print(time)
#   print("waveLength....")
#   print(waveLength)
  
  l=list()
  dataRows=5:dim(xls)[1]
  for(row in dataRows){ # for each row
    l$row=append(l$row, xls[row,1])
    l$column=append(l$column,as.numeric(xls[row,2]))
    l$content=append(l$content,xls[row,3])
    
    
    temp=list()
    for(col in 4:dim(xls)[2]){ # for each column
      temp$value=append(temp$value,xls[row,col])
      temp$time=append(temp$time,time[col-3])
      temp$waveLength=append(temp$waveLength,waveLength[col-3])
      # no temperature!
    }
    l[["measurement"]][[row-4]]=temp
  }
  l$row=lettersToNumber(l$row)# change letters to numbers
  return(l)
}


#' a parser for the novostar platereader's dbf files
#' 
#' TODO better plate identification
#' 
#'  @export
"novostar.dbf_gjsdfold"=function(path=NULL,name=Null){
  print(path)
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
  measurements <- grep("^M[[:digit:]]+$", colnames(buffer))
  
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
  #
  df=data.frame(row=numeric(0),column=numeric(0),temp=numeric(0),time=numeric(0),value=numeric(0),content=str(0)) # create the df
  for (i in 1:length(wellNames)) { # for each well
    for (j in 1:length(measurements)){ # for each measurement
      currentRow=dim(df)[1]+1 # get current row
      coordinates=extractPlateCoordinates(wellNames[i])
      df[currentRow,"row"]=coordinates[1]
      df[currentRow,"column"]=coordinates[2]
      df[currentRow,"value"]=wellData[i,j]
      df[currentRow,"time"]=timePoints[j]
      df[currentRow,"temp"]=temperature[j]
      df[currentRow,"content"]=content[i]
      
    } 
  }
  
  return(df)
}

#' extractPlateCoordinates()
#' extracts row and column from something like "A11"
#' 
#' it then transforms A into 1 B into 2 etc...
#' 
#' TODO what if AA? or more then Z??
#' 
#' and puts both in a list
#' return(c(row,column))
#' 
#' @export
extractPlateCoordinates=function(wellName){
  # example B11
  column=regmatches(wellName,regexpr("[[:digit:]]+", wellName)) # extract 11
  column=as.numeric(column)
  row=regmatches(wellName,regexpr("[[:alpha:]]+", wellName)) # ectraxt B
  row=match(casefold(row),letters) # convert B to 2
  return(c(row,column))
}


#' The SampleFrame class
#' @export
"novostar.dbf_douwe" <- 
  function(
    paths=NULL, variables=list(
      time=list(label="time", description="Time", unit="s"), 
      data=list(label="measurements"),
      temperature=list(label="temperature", description="Temperature",
                       unit="degC")
    )
  ) 
  {
    ## "paths" is a character vector with one or more paths to the files to be 
    ## read. "labels" is a list giving the labels for the time and data dimensions
    ## "variables" is a character vector of variables that will be read from the 
    ## file and returned in the output
    ## Note that sampleLabels MUST be set in the output, otherwise combination
    ## with MicroplateFrames is generally impossible, certainly  in case only part 
    ## of the wells is present in the files, or the wells appear in order differnt
    ## from that present in the MicroplateFrames.
    ## FIXME: Check whether requested channels are present at all. If not, Stop or Warn
    ## - - - - - - - - - - - - - - - - - - -
    ## Check validity of the 'variables' list
    require(foreign)
    intVars <- c('time','data','temperature')
    validNames <- c('label','description','unit')
    .validParserVariables(intVars, validNames, variables)
    if (!is.null(paths)) {
      names <- NULL
      data <- NULL
      time <- NULL
      temperature <- NULL
      for (currentPath in paths) {
        ## Suppress lots of warnings: because novostar dbf seem to have a wrong 
        ## column type annotation
        ## TODO: make tryCatch construct around read.dbf
        buffer <- suppressWarnings(read.dbf(currentPath))
        ## TODO: Build in check for valid Novostar dbf structure
        ## Measurement column names have the format M\d+
        measurements <- grep("^M[[:digit:]]+$", colnames(buffer))
        ## CH column indicates data channels(?)
        ## Rows with CH=[numeric] represent wells
        ## Rows with CH=t represent time (only measurement columns)
        ## Rows with CH=C represent temperature (only measurement colums)
        channels <- buffer[,"CH"]
        ## WELLNUM column contains well names
        wellNames <- as.character(buffer[channels=="1", "WELLNUM"])
        nofWells <- length(wellNames)
        names <- c(names, wellNames)
        wellData <- buffer[channels=="1", measurements]
        if (is.null(data)) data <- t(wellData)
        else {
          dat2 <- t(wellData)
          data <- cbind(data, dat2, deparse.level=0)
        }
        timePoints <- as.numeric(buffer[channels=="t", measurements])
        if (is.null(time)) 
          time <- matrix(rep(timePoints, nofWells), ncol=nofWells, byrow=FALSE)
        else {
          tim2 <- matrix(rep(timePoints, nofWells), ncol=nofWells, byrow=FALSE)
          time <- cbind(time, tim2, deparse.level=0)
        }
        tmpPoints <- as.numeric(buffer[channels=="C", measurements])
        if (is.null(temperature)) 
          temperature <- matrix(rep(tmpPoints, nofWells), ncol=nofWells, byrow=FALSE)
        else {
          tmp2 <- matrix(rep(tmpPoints, nofWells), ncol=nofWells, byrow=FALSE)
          temperature <- cbind(temperature, tmp2, deparse.level=0)
        }
      }
      colnames(data) <- NULL
      rownames(data) <- NULL
      colnames(time) <- NULL
      rownames(time) <- NULL
      colnames(temperature) <- NULL
      rownames(temperature) <- NULL
      x <- list()
      for (vlabel in names(variables)) {
        x[[variables[[vlabel]][['label']]]] <- get(vlabel, inherits=FALSE)
      }
      ## Parse out leading '0's in the column identifiers of the well names in
      ## the Novostar output, i.e. 'A01' becomes 'A1'
      names <- sub("([[:alpha:]]+)0+","\\1", names)
      vm <- NULL
      validNames <- c('label','description','unit')
      for (v in names(variables)) {
        vm <- rbind(vm, unlist(variables[[v]])[validNames[-1]])
      }
      vm <- as.matrix(vm)
      colnames(vm) <- c('labelDescription','unit')
      rownames(vm) <- unlist(lapply(variables,"[",'label'))
      vm[is.na(vm[,'labelDescription']),'labelDescription'] <- 
        rownames(vm)[is.na(vm[,'labelDescription'])]
      output <- new("AnnotatedSampleFrame", x=x, sampleLabels=names, 
                    varMetadata=as.data.frame(vm))  
    } else stop("Argument ", sQuote('paths'), " not defined")
    return(output)
  }