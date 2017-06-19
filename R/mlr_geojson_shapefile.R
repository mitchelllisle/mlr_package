
mlr_geojson_shapefile <- function(input, output, layer, join_file = NULL, join_key = NULL, output_join = NULL) {
  if(!is.null(join_file)) {
    shape_data <- raster::shapefile(input)
    join_data <- join
    merge_data <- raster::merge(shape_data, join_data, by = join_key)
    raster::shapefile(merge_data, output_join)
    shape_final <- rgdal::readOGR(output_join, layer, verbose = FALSE)
    shape_final_json <- geojsonio::geojson_json(shape_final)
    shape_final_json_simplified <- rmapshaper::ms_simplify(shape_final_json)
    geojsonio::geojson_write(shape_final_json_simplified, output)
  } else {
    shape_final <- rgdal::readOGR(input, layer, verbose = FALSE)
    shape_final_json <- geojsonio::geojson_json(shape_final)
    shape_final_json_simplified <- rmapshaper::ms_simplify(shape_final_json)
    geojsonio::geojson_write(input = shape_final_json_simplified, file = output)
  }
}  