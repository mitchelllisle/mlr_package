mlr_s3_readCSV <- function(s3_file_path){
  read.csv(text = rawToChar(get_object(object = s3_file_path)))
}