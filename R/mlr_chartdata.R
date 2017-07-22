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

mlr_chart_data <- function(dataframe, include_columns = FALSE, labels = ...){
  if(include_columns == FALSE){
    columns <- data.frame(names(dataframe))
    names(columns) <- c("key")
    
    json_data <- jsonlite::toJSON(list(rows = dataframe, columns = columns), pretty = TRUE)
  } else {
    json_data <- jsonlite::toJSON(list(rows = dataframe), pretty = TRUE)
  }
  
  json_data <- jsonlite::fromJSON(json_data)
  return(json_data) 
  
}
