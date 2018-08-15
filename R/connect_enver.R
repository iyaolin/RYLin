connectEnver <- function(db_type = "dbi"){
  c <- config::get(config = 'enver', file = '~/config.yml')
  # pool <- pool::dbPool(
  #   drv = RPostgreSQL::PostgreSQL(),
  #   dbname = c$dbname,
  #   host = c$host,
  #   user = c$user,
  #   password = c$password
  # )
  if (db_type == "dbi") {
    DBI::dbConnect(
      drv = RPostgreSQL::PostgreSQL(),
      dbname = c$dbname,
      host = c$host,
      user = c$user,
      password = c$password
    )
  } else {
    pool::dbPool(
      drv = RPostgreSQL::PostgreSQL(),
      dbname = c$dbname,
      host = c$host,
      user = c$user,
      password = c$password
    )
  }

}

# con <- connect_enver()
# DBI::dbGetQuery(con, 'select 787')

