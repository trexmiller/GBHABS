#' Calculate a mean after removing NA values
#' @param x a vector
#' @keywords stats
#' @export
#' @examples meanNA(x)
#' @returns A single number representing the average
meanNA = function(x){
  mean(x,na.rm=TRUE)
}

#' Calculate the minimum after removing NA values
#' @param x a vector
#' @keywords stats
#' @export
#' @examples minNA(x)
#' @returns A single number representing the minimum
minNA =  function(x){
  min(x,na.rm=TRUE)
}

#' Calculate the maximum after removing NA values
#' @param x a vector
#' @keywords stats
#' @export
#' @examples maxNA(x)
#' @returns A single number representing the maximum
maxNA =  function(x){
  max(x,na.rm=TRUE)
}

#' Calculate the standard deviation after removing NA values
#' @param x a vector
#' @keywords stats
#' @export
#' @examples sdNA(x)
#' @returns A single number representing the standard deviation
sdNA = function(x){
  sd(x,na.rm=TRUE)
}

#' Calculate the standard error of the mean after removing NA values
#' @param x a vector
#' @keywords stats
#' @export
#' @examples maxNA(x)
#' @returns A single number representing the standard error
SE = function(x){
  S = sd(x,na.rm=TRUE)
  S/sqrt(length(x))
}

#' Calculate the percentage of values above a minimum
#' @param x a vector
#' @param l the minimum threshold number, a single number
#' @keywords stats
#' @export
#' @examples freqL(x)
#' @returns A single number representing the frequency above l as a percentage
freqL = function(x,l){
  Freq = length(x[x<l])/length(x) * 100
  return(Freq)
}


#' Calculate the geometric mean
#' @param x a vector
#' @keywords stats
#' @export
#' @examples getGEO(x)
#' @returns A single number representing the geometric mean of x
getGEO = function(x){
  GEO = exp(mean(log(x[x>0]),na.rm=TRUE))
  return(GEO)
}


