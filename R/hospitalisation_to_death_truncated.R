#' Hospitalization to Death Truncated
#' @param x the input value
#' @param mu the mean of the lognormal function
#' @param sigma the standard deviation of the lognomal distribution
#' @importFrom stats plnorm setNames
#' @export

hospitalisation_to_death_truncated <- function(x, mu, sigma) {
	plnorm(x + 1, mu, sigma) - plnorm(x, mu, sigma)
}