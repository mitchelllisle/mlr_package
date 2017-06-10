mlr_s3_readCSV <- function(s3_file_path){
  read.csv(text = rawToChar(get_object(object = s3_file_path)))
}

mlr_s3_putObject <- function(object, file_name){
  tmp <- tempfile()
  on.exit(unlink(tmp))
  utils::write.csv(object, file = tmp)
  put_object(tmp, object = file_name)
}  

mlr_getAthena <- function(driver, s3_staging, query){
  # Get Athena Driver
  fil <- driver
  
  # Initiate driver
  drv <- JDBC(driverClass="com.amazonaws.athena.jdbc.AthenaDriver", fil, identifier.quote="'")
  
  # Create connection
  con <- jdbcConnection <- dbConnect(drv, 'jdbc:awsathena://athena.us-east-1.amazonaws.com:443/',
                                     s3_staging_dir = s3_staging,
                                     user = Sys.getenv("AWS_ACCESS_KEY_ID"),
                                     password = Sys.getenv("AWS_SECRET_ACCESS_KEY"))
  
  query <- dbSendQuery(con, query)
  data <- fetch(query, -1, block = 999)
  return(data)
}