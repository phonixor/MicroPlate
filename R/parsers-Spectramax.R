## Functions for reading files created by Spectramax readers


## "time" text file output produced by Spectramax
#' @export
"spectramax.txt" <- function(paths=NULL){
    ## Expecting a tab-delimited file with the following structure:
    ## Time Temperature A1  A2  ...
    ## 0 30  0.1306  0.1264 ...
    ## . .   .       .      ...
    ## - - - - - - - - - - - - - - - - - - -
    ## Check validity of the 'variables' list
    intVars <- c('time','data','temperature')
    validNames <- c('label','description','unit')
    .validParserVariables(intVars, validNames, variables)
    if (!is.null(paths)) {
      names <- NULL
      data <- NULL
      time <- NULL
      temperature <- NULL
      for (currentPath in paths) {
        lines <- readLines(currentPath, warn=FALSE)
        ## Check for valid Spectramax file structure
        header <- unlist(strsplit(lines[1], "\t"))
        if (!(length(header) > 2)) 
          stop("File ", sQuote(currentPath),
               " seems to be an invalid Spectramax time file")
        if (!(header[1]=="Time" && header[2]=="Temperature")) 
          stop("Missing \"Time\" and \"Temperature\" columns in file ", 
               sQuote(currentPath))
        wellColumns <- 2 + grep("^[A-Z][[:digit:]]+$", header[-c(1,2)])
        nofWells <- length(wellColumns)
        if (nofWells < 1) 
          stop("Missing well columns, or well column names do not conform to ",
               "canonical names in file ", sQuote(currentPath))
        wellNames <- header[wellColumns]
        names <- c(names, wellNames)
        lines <- strsplit(lines, "\t")
        if (length(unique(sapply(lines,length))) > 1) 
          stop("The number of columns is not equal in all lines in file ",
               sQuote(currentPath))
        wellData <- t(sapply(lapply(lines[-1],"[",3:(2+nofWells)), as.numeric))
        if (is.null(data)) {
          data <- wellData
        } else {
          data <- cbind(data, wellData, deparse.level=0)
        }
        timePoints <- as.numeric(sapply(lines[-1], "[", 1))
        if (is.null(time)) {
          time <- matrix(rep(timePoints, nofWells), ncol=nofWells, byrow=FALSE)
        } else {
          tim2 <- matrix(rep(timePoints, nofWells), ncol=nofWells, byrow=FALSE)
          time <- cbind(time, tim2, deparse.level=0)
        }
        tmpPoints <- as.numeric(sapply(lines[-1], "[", 2))
        if (is.null(temperature)) {
          temperature <- matrix(rep(tmpPoints, nofWells), ncol=nofWells, byrow=FALSE)
        } else {
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
      vm <- NULL
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






## "time" text file output produced by Spectramax
#' @export
"spectramax.txt_OLD" <- 
  function(
    paths=NULL, variables=list(
      time=list(label="time", description="Time", unit="s"), 
      data=list(label="measurements"),
      temperature=list(label="temperature", description="Temperature",
        unit="degC")
    )
  ) {
  ## Expecting a tab-delimited file with the following structure:
  ## Time Temperature A1  A2  ...
  ## 0 30  0.1306  0.1264 ...
  ## . .   .       .      ...
  ## - - - - - - - - - - - - - - - - - - -
  ## Check validity of the 'variables' list
  intVars <- c('time','data','temperature')
  validNames <- c('label','description','unit')
  .validParserVariables(intVars, validNames, variables)
  if (!is.null(paths)) {
    names <- NULL
    data <- NULL
    time <- NULL
    temperature <- NULL
    for (currentPath in paths) {
      lines <- readLines(currentPath, warn=FALSE)
      ## Check for valid Spectramax file structure
      header <- unlist(strsplit(lines[1], "\t"))
      if (!(length(header) > 2)) 
        stop("File ", sQuote(currentPath),
          " seems to be an invalid Spectramax time file")
      if (!(header[1]=="Time" && header[2]=="Temperature")) 
        stop("Missing \"Time\" and \"Temperature\" columns in file ", 
          sQuote(currentPath))
      wellColumns <- 2 + grep("^[A-Z][[:digit:]]+$", header[-c(1,2)])
      nofWells <- length(wellColumns)
      if (nofWells < 1) 
        stop("Missing well columns, or well column names do not conform to ",
          "canonical names in file ", sQuote(currentPath))
      wellNames <- header[wellColumns]
      names <- c(names, wellNames)
      lines <- strsplit(lines, "\t")
      if (length(unique(sapply(lines,length))) > 1) 
        stop("The number of columns is not equal in all lines in file ",
          sQuote(currentPath))
      wellData <- t(sapply(lapply(lines[-1],"[",3:(2+nofWells)), as.numeric))
      if (is.null(data)) {
        data <- wellData
      } else {
        data <- cbind(data, wellData, deparse.level=0)
      }
      timePoints <- as.numeric(sapply(lines[-1], "[", 1))
      if (is.null(time)) {
        time <- matrix(rep(timePoints, nofWells), ncol=nofWells, byrow=FALSE)
      } else {
        tim2 <- matrix(rep(timePoints, nofWells), ncol=nofWells, byrow=FALSE)
        time <- cbind(time, tim2, deparse.level=0)
      }
      tmpPoints <- as.numeric(sapply(lines[-1], "[", 2))
      if (is.null(temperature)) {
        temperature <- matrix(rep(tmpPoints, nofWells), ncol=nofWells, byrow=FALSE)
      } else {
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
    vm <- NULL
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

#"spectramax.xml" <- function(
#  paths=NULL, variables=list(
#    time=list(label="time", description="Time", unit="s"), 
#    data=list(label="measurements"),
#    temperature=list(label="temperature", description="Temperature",
#      unit="degC"))) {
#  if(!require('XML', quietly=TRUE)) 
#    stop("Function ",sQuote('spectramax.xml')," requires package ",sQuote('XML'))
#}