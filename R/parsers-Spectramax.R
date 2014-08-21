## Functions for reading files created by Spectramax readers


## "time" text file output produced by Spectramax
#' spectramax.txt()
#' @description
#' a parser to read spectramax files
#' 
#' 
#' @param path the path to the spectramax plate reader .txt file. 
#' @param name becomes platename
#' 
#' @export
"spectramax.txt" <- function(path=NULL, name=NULL){
  ## Expecting a tab-delimited file with the following structure:
  ## Time Temperature A1  A2  ...
  ## 0 30  0.1306  0.1264 ...
  ## . .   .       .      ...
  ## - - - - - - - - - - - - - - - - - - -
  ## Check validity of the 'variables' list
  
  table=read.table(path,header = T,sep = "\t") # without "\t" it does not work for empty columns
  
  ## Check for valid Spectramax file structure
  header=colnames(table)
  if (!(header[1]=="Time" && header[2]=="Temperature")){
    stop("Missing \"Time\" and \"Temperature\" columns in file ", sQuote(path))
  }
  plateCoordinates=extractPlateCoordinates(header[3:length(header)])
  time=table[[1]]
  temperature=table[[2]]
  data=table[3:ncol(table)]
  
  # create a proper microplate object
  l=list()
  
  m=list()
  nrOfMeasurements=(dim(data)[1])*(dim(data)[2])
  
  m$value=numeric(nrOfMeasurements)
  m$time=numeric(nrOfMeasurements)
  m$waveLength=numeric(nrOfMeasurements)
  
  measurementIndex=1
  nextMeasurementIndex=1
  nrOfDataRows=dim(data)[1]
  row=1:nrOfDataRows
  
  l$row=plateCoordinates[[1]]
  l$column=plateCoordinates[[2]]
  for(i in 1:ncol(data)){ # for each well
    measurementIndex=nextMeasurementIndex
    nextMeasurementIndex=measurementIndex+nrOfDataRows
    
    # well
    l$plate[i]=1 # foreign key
    l$measurement[i]=measurementIndex # kind of a foreign key
    
    # measurement
    index=measurementIndex:(nextMeasurementIndex-1)
    m$value[index]=as.numeric(data[row,i])
    m$time[index]=time
    m$temp[index]=temperature
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
