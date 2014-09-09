
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


#' numberToLetters
#' 
#' @keywords internal
#' @description
#' converts numbers to microplate row names and Excel & ODS column names
#' 
#' @param listOfNumbers the numbers you want to convert to chars
#' @details
#' 1=A
#' 26=Z
#' 27=AA
#' 702=ZZ
#' 703=AAA
#' 
#' supports lists of numbers!
#' 
#' numberToLetters(1:1000)
#' @export
numberToLetters=function(listOfNumbers=NULL){
  returnValue=NULL
  for(i in 1:length(listOfNumbers)){
    remainder=listOfNumbers[[i]]
    returnLetters=""
    while(T){
      if(remainder==0){
        break
      }
      if(remainder%%26!=0){
        returnLetters=paste(LETTERS[remainder%%26],returnLetters,sep = "")
        remainder=remainder%/%26  
      }else{
        returnLetters=paste("Z",returnLetters,sep = "")
        remainder=(remainder%/%26)-1
      }
    }
    returnValue[[i]]=returnLetters
  }
  return(returnValue)
}


#' extractPlateCoordinates()
#' @description
#' extracts row and column from something like "A11"
#' 
#' it then transforms A into 1 B into 2 etc...
#' 
#' and puts both in a list
#' return(c(row,column))
#' if row contains more then 1, return it as a data.frame
#' 
#' @param wellName the name of the well you want to convert
#' 
#' 
#' @export
extractPlateCoordinates=function(wellName){
  column=regmatches(wellName,regexpr("[[:digit:]]+", wellName)) # extract 11
  column=as.numeric(column)
  row=regmatches(wellName,regexpr("[[:alpha:]]+", wellName)) # ectraxt B
  row=lettersToNumber(row) # convert B to 2
  
  if(length(row)>1){
    returnValue=NULL
    returnValue$row=row
    returnValue$column=column
    return(data.frame(returnValue))
  }else{
    return(c(row=row,column=column))
  }
}


#' givesWarning
#' 
#' @param x the thing you wanna test
#' 
#' @description
#' returns true if a warning was given
#' returns false if no warning was given
#' 
#' 
#' @export
givesWarning=function(x){
  return(tryCatch(expr={x;F},warning=function(w)return(T)))
}



#' install.package
#' 
#' load a package or
#' install a package and then load it...
#' 
#' because R defaults R Ridiculous
#' 
#' https://bugs.r-project.org/bugzilla/show_bug.cgi?id=14776
#' 
#' @param packageName the name of the package you want to use
#' 
#' 
#' @export
install.package = function(packageName=NULL){
  gdata=packageName # if i use packageName check will give warnings
  # https://bugs.r-project.org/bugzilla/show_bug.cgi?id=14776
  
  if(gdata %in% rownames(installed.packages()) == FALSE) {
#     message(paste("installing package:",sep=""))
    lib=.libPaths()[[1]]
    install.packages(gdata,lib=lib) 
  }
  library(gdata,character.only = T)
}


