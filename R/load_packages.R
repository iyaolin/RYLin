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

pkgs_required <- c(
  'tidyverse',
  'ggplot2',
  'viridis',
  'stringi',
  'hrbrthemes',
  'lubridate',
  'DT',
  'scales',
  'magrittr',
  # 'shiny',
  # 'shinydashboard',
  # 'shinythemes',
  # 'leaflet',
  # 'highcharter',
  'Hmisc',
  'RPresto',
  'DBI',
  'bit64',
  'pool',
  'devtools',
  'data.table'
)

load_packages <- function(pkgs = pkgs_required, install = TRUE){

  if (install) {
    packages_to_install = pkgs[!(pkgs %in% installed.packages()[, 1])]
    if (length(packages_to_install) > 0) {
      install.packages(packages_to_install)
    }
  }

  sapply(pkgs, require, character.only = TRUE)
}
