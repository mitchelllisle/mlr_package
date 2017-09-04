mlr_s3_readCSV <- function(s3_file_path){
  read.csv(text = rawToChar(aws.s3::get_object(object = s3_file_path)))
}

mlr_s3_readCSV_fread <- function(s3_file_path){
  data.table::fread(rawToChar(aws.s3::get_object(object = s3_file_path)))
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
  aws.s3::put_object(tmp, object = s3_path)

  return(paste0("Upload Finished. File: ", s3_path))
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

# .onLoad <- function(libname, pkgname) {
#
#   ## path to the JDBC driver
#   file <- sprintf('AthenaJDBC41-%s.jar', packageVersion(pkgname))
#   path <- file.path(system.file('java', package = pkgname), file)
#
#   ## check if the jar is available and install if needed (on first load)
#   if (!file.exists(path)) {
#
#     url <- paste0('https://s3.amazonaws.com/athena-downloads/drivers/', file)
#
#     ## download the jar file from AWS
#     try(download.file(url = url, destfile = path, mode = 'wb'),
#         silent = TRUE)
#
#   }
#
#   ## add the RJDBC driver and the log4j properties file to classpath
#   rJava::.jpackage(pkgname, lib.loc = libname)
#
# }
#
# .onAttach <- function(libname, pkgname) {
#
#   ## let the user know if the automatic JDBC driver installation failed
#   path <- system.file('java', package = pkgname)
#   if (length(list.files(path, '^AthenaJDBC41-[0-9.]*jar$')) == 0) {
#     packageStartupMessage(
#       'The automatic installation of the Athena JDBC driver seems to have failed.\n',
#       'Please check your Internet connection and if the current user can write to ', path, '\n',
#       'If still having issues, install the jar file manually from:\n',
#       'https://docs.aws.amazon.com/athena/latest/ug/connect-with-jdbc.html\n')
#   }
#
# }
