cria_cenarios <- function(
    df, 
    dist, 
    caminho_banco = "db/busdata_class_id.db", 
    nome_banco = "busdata_class_id"
){
  
  con <- dbConnect(drv = RSQLite::SQLite(), dbname = caminho_banco)
  
  lat_min_rj <- -23.0919
  long_min_rj <- -43.80629
  
  df <- df |>
    filter(
      lat >= -23.0919 & lat <= -22.75768,
      long >= -43.80629 & long <= -43.12748,
    ) |>
    mutate(
      class_lat = formatC((lat - lat_min_rj) / dist, width = 2, format = "d", flag = "0"),
      class_long = formatC((long - long_min_rj) / dist, width = 2, format = "d", flag = "0")
    ) |>
    group_by(class_lat, class_long) |>
    mutate(class_id = cur_group_id()) |>
    ungroup() |>
    mutate(
      lat_min = as.integer(class_lat) * dist + lat_min_rj,
      long_min = as.integer(class_long) * dist + long_min_rj,
      lat_max = lat_min + dist,
      long_max = long_min + dist,
      lat_mean = (lat_max + lat_min) / 2,
      long_mean = (long_max + long_min) / 2,
      # verifc_lat = if_else(lat_mean >= lat_min & lat_mean <= lat_max, "OK", "ERRO"),
      # verifc_long = if_else(long_mean >= long_min & long_mean <= long_max, "OK", "ERRO")
    )

  dbWriteTable(con, nome_banco, df)
  
  dbExecute(
    con,
    stringr::str_glue(
      "CREATE INDEX idx ON {nome_banco}",
      " ({stringr::str_c(c('date', 'lat_mean', 'long_mean', 'class_id'), collapse = ', ')});"
    )
  )
}
