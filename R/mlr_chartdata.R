mlr_chart_data <- function(dataframe, labels = ...){
  #' Converts key:value pairs into ChartData specification written by developers at BigDatr
  #' 
  #' @param dataframe the data to be used for rows. Must be a column for each key:value pair
  #' with column name as the key and row as the value. 
  #' @param labels a `c()` must be passed for each label you are assigning in columns.
  #' @return Outputs a JSON structure with Key:Value pairs in rows and columns. This JSON
  #' structure can then be written to a file, s3, database etc. This function will not perform
  #' this final step.
  #' @examples
  #' mlr_chart_data(no_of_users, c("Date", "Users"))
  
  columns <- data.frame(names(dataframe),c(labels))
  names(columns) <- c("key", "label")
  
  json_data <- jsonlite::toJSON(list(rows = dataframe, columns = columns), pretty = TRUE)
  json_data <- jsonlite::fromJSON(json_data)
  return(json_data)
}
