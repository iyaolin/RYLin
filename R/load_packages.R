#' load most frequently used packages
#'
#'
#' @param pkgs packages to be loaded
#' @param install if the package is not installed, install it or not
#'
#' @return vector, if a package is sucessfully loaded
#'
#' @examples
#' load_packages()
#'
#' @export

pkgs_default <- c(
  'tidyverse',
  'ggplot2',
  'ggthemes',
  'viridis',
  'stringi',
  'hrbrthemes',
  'lubridate',
  'DT',
  'scales',
  'magrittr',
  'Hmisc',
  'RPresto',
  'DBI',
  'bit64',
  'pool',
  'devtools',
  'data.table'
)

pkgs_vis <- c("grid", 'gridExtra', 'RColorBrewer', 'corrplot', 'ggforce', 'ggridges')
pkgs_ml <- c('OpenML', 'mlr', 'h2o', 'caret')
pkgs_shiny <- c('shiny', 'shinydashboard', 'shinythemes', 'leaflet', 'highcharter')

loadPackages <- function(pkgs = pkgs_default, install = TRUE){
  if (install) {
    packages_to_install = pkgs[!(pkgs %in% installed.packages()[, 1])]
    if (length(packages_to_install) > 0) {
      install.packages(packages_to_install)
    }
  }
  sapply(pkgs, require, character.only = TRUE)
}
