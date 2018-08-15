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

showNAInfo <- function(df, na.prop = 0.0){
  t <- data.table(col = names(df),
                  totalcnt = nrow(df),
                  nacnt = apply(is.na(df), MARGIN = 2, sum))
  t[, prop := round(nacnt / totalcnt, 6)]
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
}