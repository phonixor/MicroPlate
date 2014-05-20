# compute the first order derivative from a window of 'wind' points
# where wind mustr be uneven
# output is a dataframe with time and running average of the derivative
#' @export
numericderiv <- function(x, y, window=min(5, length(x)), errcutoff=0.2) {
  window <- trunc(abs(window))
  if (window %% 2 == 0) window <- window + 1
  # now wind is guaranteed to be uneven
  deltax <- x[-1] - x[-length(x)]
  deltay <- y[-1] - y[-length(y)]
  dydx <- deltay/deltax
  derivt <- (x[-1] + x[-length(x)])/2
  wmat <- c(rep(NA, window-1), dydx)
  if (window > 1) {
    for (i in 2:window) {
      wmat <- cbind(wmat, c(rep(NA, window-i), dydx, rep(NA, i-1)))   
    }
    runningavg <- apply(wmat, 1, mean, na.rm=TRUE)
    runningerr <- apply(abs(wmat-runningavg),1,sum)/window
    runningavg <- runningavg[-c(1:(window%/%2),(length(runningavg)-(window%/%2)+1):length(runningavg))]
    runningerr <- runningerr[-c(1:(window%/%2),(length(runningerr)-(window%/%2)+1):length(runningerr))]
    #errcutoff <- quantile(runningerr,(1-errquantile),na.rm=TRUE)
    #runningavg[(runningerr > errcutoff)|is.na(runningerr)] <- NA
  } else {
    runningavg <- dydx
  }
  return(data.frame(x=c(derivt,NA), deriv=c(runningavg, NA)))
}

# creates a spline through x-y data, reurns the spline and higher derivatives
#' @export
smoothfit <- function(x, y, deriv.degree=1, ...) {
  available <- which(is.finite(y))
  xr <- x[available]
  yr <- y[available]
  calcindex <- seq_along(y)[available]
  splfit <- smooth.spline(xr, yr, ...)
  pr.y <- as.numeric(rep(NA, length(x)))
  pr.y[calcindex] <- predict(splfit)$y
  result <- data.frame(y=pr.y)
  for (i in seq(length.out=deriv.degree)) {
    colname <- paste("y",i,sep="")
    pr.y <- as.numeric(rep(NA,length(x)))
    pr.y[calcindex] <- predict(splfit, deriv=i)$y
    result[colname] <- pr.y
  }
  return(result)
}

# calls smoothfit separately for different episodes in a curve. Mainly to avoid 
# distorting effects of discontinuities on the splines.
#' @export
smoothEpisode <- function(x, y, deriv.degree=1, episode=c(min(x), max(x)), ...) {
  if(!length(x) == length(y)) {
    stop("Arguments ",sQuote("x")," and ",sQuote("y")," must have same length")
  }
  index <- which(x >= episode[1] & x <= episode[2])
  result <- smoothfit(x[index], y[index], deriv.degree=deriv.degree, ...)
  result$x <- x[index]
  return(result)
}

##########################################################################
#
# outliers
# function that determines outlier points relative to a fitted curve
#
##########################################################################

.outliers <- function (x, fit, meddev=6) {
  nofpoints <- length(x)
  rdev <- abs(x-fit)/max(abs(x-fit))
  # a flat distribution of rdev indicates no outliers
  # a peaked distribution at low values indicates that 
  # high values are outliers
  med <- meddev*median(rdev)
  return (rdev < med)
}

##########################################################################
#
# Using the SiZer package
#
##########################################################################

sizerfit <- function(x, y, degree) {
  if(!require('SiZer', quietly=TRUE)) 
    stop("Function ",sQuote('sizerfit')," requires package ",sQuote('SiZer'))
}