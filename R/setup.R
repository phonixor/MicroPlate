# this file contains all the stuff related to 3rd party parser libs and the selection and handling of them


#' onAttach
#' 
#' @rdname onAttach
#' @keywords internal
#' @description 
#' http://stat.ethz.ch/R-manual/R-devel/library/base/html/ns-hooks.html
#' 
#' @param libname no clue ... i aint using it
#' @param pkgname also not used
.onAttach = function(libname, pkgname){
  #   setup()
}



#' setup
#' 
#' @details
#' 
#' this functions test which spreadsheet parsers work on your system, and stores the result in some global variables
#' this function is called when the package is loaded (.onAttach, not .onLoad)
#' you should use read.sheet() to actually get the sheets
#' 
#' 
#' TODO: maybe don't do all tests
#' all test
#' 
#' http://cran.r-project.org/web/packages/openxlsx/index.html
#' http://cran.r-project.org/web/packages/xlsx/index.html
#' 
#'
#' @export
setup = function(){
  # check the current OS/Environment to determine which packages are used to load
  # 
  message("*** microplate setup ***")
  message("- seaching for an ODS parser")
  # ODS
  #
  # readODS
  .readODSWorksForODS<<-tryCatch({
    install.package("readODS")
    
    file=paste(path.package("microplate"),"/extdata/test.ods",sep="")
    read.ods(file) 
    
    message(" + readODS package works for ODS files")
    T # if it got here without errors it works!
  }, error=function(e){
    message(" - readODS package does not work for ODS files on this system")
    return(F)
  })
  
  # check if anything worked
  if(!.readODSWorksForODS){
    warning("no packages work on your system for .ods files, packages tried: readODS")
  }
  
  
  # XLSX
  #
  # gdata
  message("- searching for a XLSX parser")
  .gdataWorksForXLSX<<-tryCatch({
    install.package("gdata")
    
    
    file=paste(path.package("microplate"),"/extdata/test.xlsx",sep="")
    
    # warnings are still thrown even in a try catch... FUCKING R!
    gdata::read.xls(file, stringsAsFactors=FALSE, header=FALSE)
    #     tryCatch(expr={gdata::read.xls(file, stringsAsFactors=FALSE, header=FALSE)},error=function(e)T,warning=function(w)F)
    message(" + gdata works for .xlsx files on this system.")
    T # if it got here without errors it works!
  }, error=function(e){
    message(" - gdata does not work for .xlsx files on this system.")
    return(F)
  },warning=function(w){
    # errors get priority over warning... so i guess if things go wrong it wont get here
    message(" - gdata gave warning...")
    return(F)
  }
  )
  #
  # xlsx
  .xlsxWorksForXLSX<<-tryCatch({
    install.package("xlsx")
    
    file=paste(path.package("microplate"),"/extdata/test.xlsx",sep="")
    xlsx::read.xlsx(file, sheetIndex=1, stringsAsFactors=FALSE, header=FALSE)
    
    message(" + xlsx works for .xlsx files on this system.")
    T # if it got here without errors it works!
  }, error=function(e){
    message(" - xlsx does not work for .xlsx files on this system.")
    return(F) 
  })
  #
  # openxlsx
  .openxlsxWorksForXLSX<<-tryCatch({
    install.package("openxlsx")
    
    file=paste(path.package("microplate"),"/extdata/test.xlsx",sep="")
    openxlsx::read.xlsx(file,colNames=F)
    
    message(" + openxlsx works for .xlsx files on this system.")
    T # if it got here without errors it works!  
  }, error=function(e){
    message(" - openxlsx does not work for .xlsx files on this system.")
    return(F)
  })
  
  # check if anything worked
  if(!any(.gdataWorksForXLSX, .xlsxWorksForXLSX, .openxlsxWorksForXLSX)){
    warning("no packages work on your system for .xlsx files, packages tried: gdata, xlsx, openxlsx")
  }
  
  
  # XLS
  #
  # gdata
  message("- searching for a XLS parser")
  .gdataWorksForXLS<<-tryCatch({
    install.package("gdata")
    
    file=paste(path.package("microplate"),"/extdata/test.xls",sep="")
    gdata::read.xls(file, stringsAsFactors=FALSE, header=FALSE)
    
    message(" + gdata works for .xls files on this system.")
    T # if it got here without errors it works!
  }, error=function(e){
    message(" - gdata does not work for .xls files on this system.")
    return(F)
  })
  #
  # xlsx
  .xlsxWorksForXLS<<-tryCatch({
    install.package("xlsx")
    
    file=paste(path.package("microplate"),"/extdata/test.xls",sep="")
    xlsx::read.xlsx(file, sheetIndex=1, stringsAsFactors=FALSE, header=FALSE)
    
    message(" + xlsx works for .xls files on this system.")
    T # if it got here without errors it works!
  }, error=function(e){
    message(" - xlsx does not work for .xls files on this system.")
    return(F) 
  })
  
  # check if anything worked
  if(!any(.gdataWorksForXLSX, .openxlsxWorksForXLSX, .xlsxWorksForXLSX)){
    warning("no packages work on your system for .xls files, packages tried: gdata, xlsx")
  }
  message("*** finished microplate setup ***")
}


#' read.sheet
#' 
#' @details
#' a function that calls a ods, xls, xlsx parser that works on your system
#' 
#' since not all systems have JAVA or PERL installed. some of the parsers may not work out of the box
#' during the .onAttach fase of loading the package different parsers are tested and installed.
#' the result of these test are stored in global variables like:
#' .xlsxWorksForXLS & .openxlsxWorksForXLSX & .readODSWorksForODS
#' in general "."+PackageName+"WorksFor"+format
#' 
#' TODO: decide if i want to support "..."
#' TODO: support .csv ???
#' TODO: multi sheet... for openxml
#' 
#' @param file the spreadsheet file .ods, .xls or .xlsx
#' @param sheet the sheet or sheets you want from the spreadsheet
#' 
#' @export
read.sheet = function(file=NULL, sheet=NULL){
  if(!exists(".readODSWorksForODS")) {
    message(".readODSWorksForODS undefined, running setup()")
    setup()
  }
  
  splitedFile=unlist(strsplit(file,split = ".",fixed=TRUE))
  extention=casefold(splitedFile[length(splitedFile)], upper = FALSE)
  
  if(extention=="ods"){
    if(.readODSWorksForODS){
      return(readODS::read.ods(file))
    }else{
      stop("no valid ods parser")
    }
    
  }else if(extention=="xls"){
    if(.gdataWorksForXLS){
      return(gdataInterface(file,sheet))
    } else if(.xlsxWorksForXLS){
      return(xlsxInterface(file,sheet))
    } else {
      stop("no valid xls parser")
    }
  }else if(extention=="xlsx"){
    if(.gdataWorksForXLSX){
      return(gdataInterface(file,sheet))
    } else if(.xlsxWorksForXLSX){
      return(xlsxInterface(file,sheet))
    } else if(.openxlsxWorksForXLSX){
      return(openxlsx::read.xlsx(file,colNames=F))
    } else {
      stop("no valid xlsx parser")
    }
  }else{
    stop(paste("extention not supported: ",extention, "supported extentions: ods,xls,xlsx", sep=""))
  }
  stop("how did i get here?")
}


#' gdataInterface
#' 
#' @param file the spreadsheet file .ods, .xls or .xlsx
#' @param sheet the sheet or sheets you want from the spreadsheet
#' 
#' @export
gdataInterface=function(file=NULL,sheet=NULL){
  # if no sheet is given return all sheets
  if(is.null(sheet)){
    sheet=1:sheetCount(file)
  }else if (length(sheet)==1){
    # only 1 sheet, return it as a df not as a list of df
    return(gdata::read.xls(file, sheet=sheet, stringsAsFactors=FALSE, header=FALSE))
  }      
  returnValue=list()
  index=0 # sheet does not have to start with 1, so we need an other counter
  for(i in sheet){
    index=index+1 
    returnValue[[index]]=gdata::read.xls(file, sheet=i, stringsAsFactors=FALSE, header=FALSE)
  }
  return(returnValue)
}


#' xlsxInterface
#' 
#' 
#' TODO figure out how to read stuff out of getSheet...
#'        so i dont parse everything for each sheet...
#' 
#' @param file the spreadsheet file .ods, .xls or .xlsx
#' @param sheet the sheet or sheets you want from the spreadsheet
#' 
#' @export
xlsxInterface=function(file=NULL,sheet=NULL){
  # if no sheet is given return all sheets
  if(is.null(sheet)){
    sheet=1:length(xlsx::getSheets(xlsx::loadWorkbook(file)))#oooh yeah!!!
  }else if (length(sheet)==1){
    # only 1 sheet, return it as a df not as a list of df
    return(xlsx::read.xlsx(file, sheetIndex=sheet, stringsAsFactors=FALSE, header=FALSE))
  }
  returnValue=list()
  index=0 # sheet does not have to start with 1, so we need an other counter
  for(i in sheet){
    index=index+1 
    returnValue[[index]]=xlsx::read.xlsx(file, sheetIndex=i, stringsAsFactors=FALSE, header=FALSE)
  }
  return(returnValue)
}
