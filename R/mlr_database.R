#' @exportdev

mlr_getPostgres <- function(query){
  con <- dbConnect(RPostgres::Postgres(), dbname = Sys.getenv("POSTGRES_DB"),
                   host = Sys.getenv("POSTGRES_HOST"),
                   port = Sys.getenv("POSTGRES_PORT"),
                   user = Sys.getenv("POSTGRES_USER"),
                   password = Sys.getenv("POSTGRES_PASS"))
  res <- dbSendQuery(con, query)
  dbFetch(res)
}

mlr_putPostgres_dataframe <- function(table, data) {
  message("WARNING: This will drop table before inserting data. Cancel now if this is not acceptable")
  Sys.sleep(5)
  con <- dbConnect(RPostgres::Postgres(), dbname = Sys.getenv("POSTGRES_DB"),
                   host = Sys.getenv("POSTGRES_HOST"),
                   port = Sys.getenv("POSTGRES_PORT"),
                   user = Sys.getenv("POSTGRES_USER"),
                   password = Sys.getenv("POSTGRES_PASS"))
  if (dbExistsTable(con, table)) {
    dbRemoveTable(con, table)
  }
  dbWriteTable(con, table, data, row.names = FALSE)
  message("Data written to Postgres")
  dbDisconnect(con)
}

mlr_getMongo <- function (host, port, db, collection, query = NULL, fields = NULL){
  con <- mongo(collection = collection, db =  db, url = paste0("mongodb://",host,":", port) ,verbose = TRUE)
  mongoData <- con$find(query = query, fields = fields)
  rm(con)
  mongoData
}

mlr_putMongo <- function (host, db, collection, data){
  con <- mongo(collection = collection, db = db, url = host ,verbose = TRUE)
  mongoData <- con$insert(data)
  rm(con)
}
