mlr_census_reshape <- function(read_file, id){
  x <- read.csv(read_file, check.names = FALSE)
  molten = reshape2::melt(x, id = id, na.rm = FALSE)
  data.frame(molten)
}

mlr_extract_hashes <- function(vec){
  hash.pattern = "#[[:alpha:]]+"
  have.hash = grep(x = vec, pattern = hash.pattern)
  hash.matches = gregexpr(pattern = hash.pattern, text = vec[have.hash])
  extracted.hash = regmatches(x = vec[have.hash], m = hash.matches)
  df = data.frame(table(tolower(unlist(extracted.hash))))
  colnames(df) = c("tag","freq")
  df = df[order(df$freq,decreasing = TRUE),]
  return(df)
}
