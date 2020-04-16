#' Retrieve State Data
#' 
#' This function pull state data and formats for case reporting
#' 
#' @param state_of_interest the state desired
#' @param min_reported_deaths minimum reported deaths to keep, 
#'     default of \code{5}
#' @export
#' 

pull_state_cases <- function(state_of_interest = "North Carolina",
														 min_reported_deaths = 5){
	nc_data <- nccovid::get_covid_state(state = state_of_interest)
	
	nc_data <- nc_data %>% 
		dplyr::select(county,date, cases_daily,deaths_daily) %>% 
		setNames(c("country", "date", "new_cases", "new_deaths")) %>% 
		dplyr::mutate(date = lubridate::mdy(date)) %>% 
		dplyr::group_by(country) %>%
		padr::pad() %>%
		dplyr::mutate(new_cases = tidyr::replace_na(new_cases, 0),
									new_deaths = tidyr::replace_na(new_deaths, 0)) %>%
		dplyr::group_by(country) %>%
		dplyr::mutate(cum_deaths = sum(new_deaths)) %>%
		dplyr::filter(cum_deaths > 0) %>%
		dplyr::select(-cum_deaths)
	
	nc_death <- nc_data %>% 
		dplyr::group_by(country) %>% 
		dplyr::summarise(tot = sum(new_deaths)) %>% 
		dplyr::filter(tot >= min_reported_deaths) %>% 
		dplyr::pull(country)
	
	nc_data <- nc_data %>% 
		dplyr::filter(country %in% nc_death)
	
	nc_data
}
