
##
# this file contains general functions


#' trim.leading
#' 
#' @description
#' returns string w/o leading whitespace
#' includes spaces, tabs, ...
#' @param x the string you want to remove the white space from
#' @export
trim.leading <- function (x)  sub("^\\s+", "", x)

#' trim.trailing
#' 
#' @description
#' returns string w/o trailing whitespace
#' includes spaces, tabs, ...
#' @param x the string you want to remove the white space from
#' @export
trim.trailing <- function (x) sub("\\s+$", "", x)

#' trim
#' 
#' @description
#' returns string w/o leading or trailing whitespace
#' includes spaces, tabs, ...
#' @param x the string you want to remove the white space from
#' @export
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#' lettersToNumber
#' @rdname lettersToNumber
#' @description
#' A=1, Z=26
#' AA=27, ZZ=... a lot!
#' ZZ=702
#' AAA=703
#' 
#' for each letter:
#' letterNrInAlphabet*26^-totalLetters-letterPositionInString
#' so:
#' ABC=A*26^(3-1) + B*26^(3-2) + C*26^(3-3)
#' ABC=1*26^2 + 2*26^1 + 3*26^0
#' ABC=676 + 52 + 3 = 731
#' 
#' this is basically a base26 to base10 math converter!
#' @param listOfStrings a string or list/vector of strings that need to be converted
#' 
#' @export
setGeneric("lettersToNumber", function(listOfStrings=NULL) standardGeneric("lettersToNumber")) 
#' @rdname lettersToNumber
setMethod("lettersToNumber", signature(listOfStrings="character"), function( listOfStrings=NULL){
  total=NULL
  for(i in 1:length(listOfStrings)){ # for each string
    total[i]=0
    for (j in 1:nchar(listOfStrings[i])){ # for each char in the string
      current=match(casefold(substring(listOfStrings[i],j,j)),letters)
      total[i]=total[i]+(current*26^(nchar(listOfStrings[i])-j))
    }
  }
  return(total)
})

#'
#'
#'
#'@import
#'@include 
.onLoad - function(){
  # check the current OS/Environment to determine which packages are used to load
  # http://stat.ethz.ch/R-manual/R-devel/library/base/html/ns-hooks.html
  
  # TODO: put this in its own function!!
  # http://cran.r-project.org/web/packages/openxlsx/index.html
  # http://cran.r-project.org/web/packages/xlsx/index.html
  
  
  # read.ODS
  tryCatch({
    file=paste(path.package("readODS"),"/readODS/tests/testdata/project/layout.ods",sep="")
    print(file)
    read.ods(file)
    
    
    
  })
  # set function
  .read.ods.function<<-read.ods
  
  .read.ods.function=error("no packages work on your system for .ods files, packages tried: readODS")
  
}


read.Sheet = function(){
  
}


