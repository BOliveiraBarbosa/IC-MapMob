cria_cenarios <- function(
    diretorio,
    dist,
    caminho_arquivos
){
  
  origem <- open_dataset(diretorio) |>
    summarise(
      lat_min_df = min(LAT),
      long_min_df = min(LONG)
    ) |>
    collect()
  
  df <- open_dataset(diretorio) |>
    mutate(
      class_lat = as.integer((LAT - origem$lat_min_df) / dist),
      class_long = as.integer((LONG - origem$long_min_df) / dist),
      lat_min = as.integer(class_lat) * dist + origem$lat_min_df,
      long_min = as.integer(class_long) * dist + origem$long_min_df,
      lat_max = lat_min + dist,
      long_max = long_min + dist,
      lat_mean = (lat_max + lat_min) / 2,
      long_mean = (long_max + long_min) / 2
    ) |>
    group_by(format(mdy_hms(DATE), "%Y-%m-%d")) |>
    write_dataset(
      path = caminho_arquivos, 
      format = "parquet",
      existing_data_behavior = "overwrite"
    )
  
}
