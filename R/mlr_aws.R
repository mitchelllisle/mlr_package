#test
mlr_s3_readCSV <- function(s3_file_path){
  read.csv(text = rawToChar(get_object(object = s3_file_path)))
}

mlr_s3_putObject <- function(data, s3_path, col.names = FALSE, row.names = FALSE, quote = FALSE, sep = ",", type = "csv"){
  tmp <- tempfile()
  on.exit(unlink(tmp))
  if(type == "csv"){
    utils::write.table(data, file = tmp, col.names = col.names, row.names = row.names, quote = quote, sep = sep)  
  }
  if(type == "json"){
    write_json(x = data, path = tmp)
  }
  put_object(tmp, object = s3_path)
}  

mlr_getAthena <- function(driver_location, s3_staging, query){
  # Get Athena Driver
  fil <- driver_location
  # Initiate driver
  drv <- JDBC(driverClass="com.amazonaws.athena.jdbc.AthenaDriver", fil, identifier.quote="'")
  # Create connection
  con <- jdbcConnection <- RJDBC::dbConnect(drv, 'jdbc:awsathena://athena.us-east-1.amazonaws.com:443/',
                                     s3_staging_dir = s3_staging,
                                     user = Sys.getenv("AWS_ACCESS_KEY_ID"),
                                     password = Sys.getenv("AWS_SECRET_ACCESS_KEY"))
  
  query <- RJDBC::dbSendQuery(con, query)
  data <- fetch(query, -1, block = 999)
  return(data)
}
