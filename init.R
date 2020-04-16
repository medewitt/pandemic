# Start Packages
usethis::use_description()
usethis::use_build_ignore("init.R")
usethis::use_namespace()
usethis::use_package_doc()
usethis::use_readme_rmd()

# Packages
usethis::use_package("dplyr")
usethis::use_package("ggplot2")
usethis::use_package("padr")
usethis::use_pipe()

# Add Vignettes
usethis::use_vignette("under-reporting", title = "Example Under Reporting Estimates")
