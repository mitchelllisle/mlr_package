#' @exportdev

mlr_getPostgres <- function(host, port, user, password, db, query){
  pg = dbDriver("PostgreSQL")
  con <- dbConnect(pg, user, password, host, db, port)
  query_results <- dbGetQuery(con, query)
  dbDisconnect(con)
  query_results
}

mlr_putPostgres_dataframe <- function(host, port, user, password, db, table, data) {
  pg = dbDriver("PostgreSQL")
  message("WARNING: This will drop table before inserting data. Cancel now if this is not acceptable")
  Sys.sleep(5)
  con <- dbConnect(pg, user, password, host, db, port)
  if (dbExistsTable(con, table)) {
    dbRemoveTable(con, table)
  }
  dbWriteTable(con, table, data, row.names = FALSE)
  message("Data written to Postgres")
  dbDisconnect(con)
}

mlr_getMongo <- function (host, db, collection){
  con <- mongo(collection = collection, db = db, url = host ,verbose = TRUE)
  mongoData <- con$find()
  rm(con)
}