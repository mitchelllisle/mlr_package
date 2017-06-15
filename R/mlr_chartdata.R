mlr_chart_data <- function(row_key1, row_key2, row_value1, row_value2, label_value){
  #' Converts key:value pairs into ChartData specification written by developers at BigDatr
  #' 
  #' @param row_key the key for the key:value pair
  #' @param row_value the value for the key:value pair
  #' @return Outputs a JSON structure with Key:Value pairs in rows and columns. This JSON
  #' structure can then be written to a file, s3, database etc. This function will not perform
  #' this final step.
  #' @examples
  #' no_of_users.json <- mlr_chart_data("date", "count", no_of_users$date, no_of_users$no_of_users, "NoOfUsers")
  rows <- data.frame(list(c(row_value1)),c(row_value2))
  row_names <- c(row_key1, row_key2)
  names(rows) <- c(row_names[1], row_names[2])
  
  # Todo: allow for any number of key:value pairs
  columns <- data.frame(list(c(row_names[1], row_names[2], NA),c(NA,NA,label_value)))
  names(columns) <- c("key", "label")
  
  # There are some specifics that a fromJSON function does in terms of formatting
  # Things like escape characters and quotes will be written to file if you don't perform this step
  json_data <- jsonlite::toJSON(list(rows = rows, columns = columns), pretty = TRUE)
  json_data <- jsonlite::fromJSON(json_data)
  return(json_data)
}
