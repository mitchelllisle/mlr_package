#' Generates a random inspirational quote from Jarvis
mlr_random_jarvis <- function(){
  quotes <- data.table::fread("https://raw.githubusercontent.com/mitchelllisle/mlr_package/master/R/data/randomjarvis.csv", showProgress = FALSE, header = FALSE)
  index <- round(runif(1, 1, as.numeric(count(quotes))))
  chosen_quote <- quotes[index]
  as.character(chosen_quote)
}

