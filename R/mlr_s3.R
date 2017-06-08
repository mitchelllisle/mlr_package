mlr_s3_readCSV <- function(s3_file_path){
  read.csv(text = rawToChar(get_object(object = s3_file_path)))
}

mlr_s3_putObject <- function(object){
  tmp <- tempfile()
  on.exit(unlink(tmp))
  utils::write.csv(spots_data, file = tmp)
  put_object(tmp, object = object)
}