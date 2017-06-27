mlr_datetime_aggregation <- function(data, seconds = 60, origin = "1970-01-01") { # defaults to minute rounding
  if(!is(data, "POSIXct")) {
    stop("Please pass in a POSIXct variable")
  } else {
    return(as.POSIXct(floor(as.numeric(data) / (seconds))*(seconds), origin = origin))
  }
}


