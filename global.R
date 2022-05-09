shhh <- suppressPackageStartupMessages # It's a library, so shhh!

shhh(library(dplyr))
shhh(library(tidyr))
shhh(library(shinycssloaders))
shhh(library(plotly))
shhh(library(DT))
shhh(library(ggthemes))
shhh(library(collapsibleTree))
shhh(library(scales))
shhh(library(meta))
shhh(library(shiny))
shhh(library(shinydashboard))
shhh(library(shinyWidgets))
shhh(library(NCmisc))
shhh(library(utils))
shhh(library(shinyGovstyle))
shhh(library(shinyjs))
shhh(library(readr))
shhh(library(glue))
shhh(library(purrr))

# tidy_code_function -------------------------------------------------------------------------------
#
tidy_code_function <- function() {
  message("----------------------------------------")
  message("App scripts")
  message("----------------------------------------")
  app_scripts <- eval(styler::style_dir(recursive = FALSE)$changed)
  message("R scripts")
  message("----------------------------------------")
  r_scripts <- eval(styler::style_dir("R/")$changed)
  message("Test scripts")
  message("----------------------------------------")
  test_scripts <- eval(styler::style_dir("tests/", filetype = "r")$changed)
  script_changes <- c(app_scripts, r_scripts, test_scripts)
  return(script_changes)
}

#source("R/read_data.R")
