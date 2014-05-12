
#' trim.leading
#' returns string w/o leading whitespace
#' @export
trim.leading <- function (x)  sub("^\\s+", "", x)

#' trim.trailing
#' returns string w/o trailing whitespace
#' @export
trim.trailing <- function (x) sub("\\s+$", "", x)

#' trim
#' returns string w/o leading or trailing whitespace
#' @export
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#' lettersToNumber
#' 
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
#' 
#' @export
setGeneric("lettersToNumber", function(listOfStrings=NULL) standardGeneric("lettersToNumber")) 
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

