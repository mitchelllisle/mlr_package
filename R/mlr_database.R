#' @exportdev

mlr_getPostgres <- function(host, port, user, password, db, query){
  con <- dbConnect(RPostgres::Postgres(), dbname = db,
                   host = host,
                   port = port, 
                   user = user, 
                   password = password)
  res <- dbSendQuery(con, query)
  dbFetch(res)
}

mlr_putPostgres_dataframe <- function(host, port, user, password, db, table, data) {
  message("WARNING: This will drop table before inserting data. Cancel now if this is not acceptable")
  Sys.sleep(5)
  con <- dbConnect(RPostgres::Postgres(), dbname = db,
                   host = host,
                   port = port, 
                   user = user, 
                   password = password)
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
  mongoData
}
