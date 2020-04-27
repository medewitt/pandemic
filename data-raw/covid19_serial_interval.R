## code to prepare `covid19_serial_interval` dataset goes here

covid19_serial_interval <- read.csv(here::here("data-raw", "serial_interval.csv"))


usethis::use_data(covid19_serial_interval, overwrite = TRUE)
