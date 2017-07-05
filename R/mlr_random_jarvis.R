#' Generates a random inspirational quote from Jarvis
mlr_random_jarvis <- function(){
  index <- round(runif(1, 1, 22), 0)
  quotes <- data.table::fread("https://raw.githubusercontent.com/mitchelllisle/mlr_package/master/R/data/randomjarvis.csv", showProgress = FALSE, header = FALSE)
  chosen_quote <- quotes[index]
  as.character(chosen_quote)
}


