mlr_datetime_aggregation <- function(data, seconds = 60, origin = "1970-01-01") { # defaults to minute rounding
  if(!is(data, "POSIXct")) {
    stop("Please pass in a POSIXct variable")
  } else {
    return(as.POSIXct(floor(as.numeric(data) / (seconds))*(seconds), origin = origin))
  }
}

#'Return a count for the number of business days between two dates.
#'
#'This function uses the package `timeDate` to return a count of the
#'number of business days between two dates.
#'
#'@param start : The start date for counting business days. Must be type
#'character
#'@param end : The end date for counting business days. Must be type
#'character
#'
#'@example
#'mlr_business_days("2017-01-01", "2017-01-02")
#'
mlr_business_days <- function(start, end){
  
  tS <- timeDate::timeSequence(from = paste(start), to = paste(end))
  bizDays <- data.frame(isBizday(tS))
  
  bizcount <- bizDays %>%
    dplyr::filter(isBizday.tS. == "TRUE") %>%
    summarise(cout_of_bizdays = n())
  
  bizcount$cout_of_bizdays
}

count_biz_days("2017-06-26", "2017-07-03")


