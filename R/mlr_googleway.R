mlr_geocode_address <- function(address, key){
  data.frame(google_geocode(address = address, key = geocode_key, simplify = TRUE))
}

mlr_google_radar <- function(lat, lng, radius, type, key){
  req <- GET(paste("https://maps.googleapis.com/maps/api/place/radarsearch/json?location=",lat,",",lng,"&radius=",radius,"&type=",type,"&key=",key, sep = ""))
  req_df <- fromJSON(content(req, type = "text", encoding = "UTF-8"))
  data.frame(cbind(req_df$results$place_id, req_df$results$geometry$location$lat, req_df$results$geometry$location$lng))
}

mlr_google_place <- function(place){
  request <- GET(paste("https://maps.googleapis.com/maps/api/place/details/json?placeid=",place,"&key=",places_key, sep = ""))
  request_json <- fromJSON(content(request, type = "text", encoding = "UTF-8"))
  place_data <- data.frame(request_json$result$types, request_json$result$id,request_json$result$formatted_address, request_json$result$name, request_json$result$geometry$location$lat, request_json$result$geometry$location$lng)
}

mlr_googleway <- function(geocode_address, geocode_key, radar_radius, radar_type, places_key){
  req <- mlr_geocode_address(geocode_address, geocode_key)
  req2 <- mlr_google_radar(request_json$result$types[1], req$results.geometry$location$lat, req$results.geometry$location$lng, radar_radius, radar_type, places_key)
  places <- as.list(req2$X1)
  data <- lapply(places, FUN = mlr_google_place)
  dataframe <- ldply(data, data.frame)
}