#' showNAInfo: show columns with NA value
#'
#' @param df data.frame
#' @param na.prop numeric, default = 0.0
#'
#' @return data.table, show columns with prop ('= #na_cnt/#total_cnt) equal or larger than na.prop
#'
#' @examples
#' showNAInfo(flights)
#'
#' @export

showNAInfo <- function(dt, na.prop = 0.0){
  # if (!'data.table' %in% loadedNamespaces()) {loadPackages('data.table')}
  t <- data.table(col = names(dt),
                  totalcnt = nrow(dt),
                  nacnt = apply(is.na(dt), MARGIN = 2, sum))
  message(is.data.table(t) == TRUE)
  t[, ':='(prop = round(nacnt / totalcnt, 6))]
  t[prop >= na.prop][order(-prop)]
}


#' fillNAs: fill NAs with the most frequent value(for numeric) or default value(for category)
#'
#' @param dt data.table
#' @param col.name
#'
#' @return NULL
#'
#' @examples
#'
#'
#' @export

fillNAs <- function(dt, col.name, default.cat = 'None', default.num = NA){
  t <- dt[, get(col.name)]
  if (is.numeric(t)) {
    fillvalue <- ifelse(is.na(default.num), median(t, na.rm = TRUE), default.num)
  } else {
    fillvalue <- ifelse(is.na(default.cat), names(which.max(table(t))), default.cat)
  }
  dt[is.na(get(col.name)), (col.name) := fillvalue]
  invisible(NULL)
}
