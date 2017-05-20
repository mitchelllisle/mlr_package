mlr_census_reshape <- function(read_file, id){
  x <- read.csv(read_file, check.names = FALSE)
  molten = reshape2::melt(x, id = id, na.rm = FALSE)
  data.frame(molten)
}
