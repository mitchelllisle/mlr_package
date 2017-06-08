mlr_s3_readCSV <- function(s3_file_path){
  read.csv(text = rawToChar(get_object(object = s3_file_path)))
}

mlr_s3_putObject <- function(object, file_name){
  tmp <- tempfile()
  on.exit(unlink(tmp))
  utils::write.csv(object, file = tmp)
  put_object(tmp, object = file_name)
}  