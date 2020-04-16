#' Hospitalization to Death Truncated
#' @export

hospitalisation_to_death_truncated <- function(x, mu, sigma) {
	plnorm(x + 1, mu, sigma) - plnorm(x, mu, sigma)
}