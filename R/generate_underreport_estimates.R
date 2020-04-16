#' Generate the Under Reporting Estimates
#' 
#' Generates the under reporting estimates based off of reporting delays
#' and the assumed case fatality rates.
#' 
#' @param dat the case data
#' @param mean_low mean delay days
#' @param median_low median delay days
#' @param mean_mid mean delay days
#' @param median_mid median delay days
#' @param mean_high mean delay days
#' @param median_high median delay days
#' @param ... additional arguments to pass to \code{under_reporting_estimates}
#' 
#' @export
generate_underreporting_estimates <-function(dat,
																						 mean_low = 8.7,
																						 median_low = 6.7,
																						 mean_mid = 13,
																						 median_mid = 9.1,
																						 mean_high = 20.9,
																						 median_high = 13.7,
																						 ...){
	muLow <- mu_transform(median_low)
	sigmaLow <- sigma_transform(mean_low, muLow)


	# middle of the range
	muMid <- mu_transform(median_mid)
	sigmaMid <- sigma_transform(mean_mid, muMid)

	# upper end of the range
	muHigh <- mu_transform(median_high)
	sigmaHigh <- sigma_transform(median_high, muHigh)

	hospitalisation_to_death_truncated_low <- function(x){
		hospitalisation_to_death_truncated(x, muLow, sigmaLow)
	}

	hospitalisation_to_death_truncated_mid <- function(x){
		hospitalisation_to_death_truncated(x, muMid, sigmaMid)
	}

	hospitalisation_to_death_truncated_high <- function(x){
		hospitalisation_to_death_truncated(x, muHigh, sigmaHigh)
	}

	allTogetherLow <- under_reporting_estimates(dat,
																							hospitalisation_to_death_truncated_low, ...)
	allTogetherMid <- under_reporting_estimates(dat,
																						hospitalisation_to_death_truncated_mid, ...)
	allTogetherHigh <- under_reporting_estimates(dat,
																						 hospitalisation_to_death_truncated_high, ...)
	# choosing CIs such that they include all uncertainty from delay distribution
	finalRes <- dplyr::tibble(
		country = allTogetherMid$country,
		total_cases = allTogetherMid$total_cases,
		total_deaths = allTogetherMid$total_deaths,
		underreporting_estimate  = pmin(allTogetherLow$underreporting_estimate, allTogetherMid$underreporting_estimate,
																		allTogetherHigh$underreporting_estimate, na.rm = T),
		lower = pmin(allTogetherLow$lower, allTogetherMid$lower, allTogetherHigh$lower),
		upper = pmax(allTogetherLow$upper, allTogetherMid$upper, allTogetherHigh$upper))


	# putting all of the data together in a readable format for the Rmd file
	reportDataFinal <- finalRes %>%
		dplyr::select(country, total_cases, total_deaths, underreporting_estimate, lower,
									upper) %>%
		dplyr::mutate(underreporting_estimate = ifelse(underreporting_estimate <= 1, underreporting_estimate, 1)) %>%
		dplyr::mutate(upper = ifelse(upper <= 1, upper, 1)) %>%
		dplyr::mutate(underreporting_estimate = signif(underreporting_estimate, 2)) %>%
		dplyr::mutate(lower = signif(lower, 2)) %>%
		dplyr::mutate(upper = signif(upper, 2)) %>%
		#dplyr::ungroup(country, ) %>%
		dplyr::mutate(country = gsub(x = country, pattern = "_",replacement =  " ")) %>%
		dplyr::mutate(underreporting_estimate_clean = paste0(underreporting_estimate*100,
																												 "% (",lower*100,"% - ",upper*100,"%)"))

}
