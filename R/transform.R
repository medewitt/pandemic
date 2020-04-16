#' Perform A Log Transform on Median
#' 
#' Transforms medians into a log scale
#' @param zMedian the median
#' @export

mu_transform <- function(zMedian){
	mu <- log(zMedian) 
}

#' Transform for Sigma
#' @param zMean the median
#' @param mu the median
#' @export
sigma_transform <- function(zMean, mu){
	sigma <- sqrt(2*(log(zMean) - mu))
}
