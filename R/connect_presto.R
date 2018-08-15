####################################################################
#' Connect to server using RPresto
#'
#'
#'
#' @param server_type string
#' @param db_type string
#' @param schema_name string
#' @export


connectPresto <- function(server_type = "nm",
                           db_type = "dbi",
                           schema_name = NA_character_) {
  c <- config::get(config = 'presto', file = '~/config.yml')
  hostname <- sprintf("http://%s",
                      ifelse(server_type == "nm", c$host_nm,
                             c$host_md))
  schemaname <- ifelse(is.na(schema_name), c$schema, schema_name)

  if (db_type == "dbi") {
    DBI::dbConnect(
      drv = RPresto::Presto(),
      host = hostname,
      port = c$port,
      user = c$user,
      password = c$password,
      schema = schemaname,
      catalog = c$catalog
    )
  } else {
    pool::dbPool(
      drv = RPresto::Presto(),
      host = hostname,
      port = c$port,
      user = c$user,
      password = c$password,
      schema = schemaname,
      catalog = c$catalog
    )
  }
}

# con <- connect_presto()
# DBI::dbGetQuery(con, 'select 787')
