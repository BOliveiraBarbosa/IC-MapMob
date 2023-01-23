cria_cenarios <- function(
    df,
    dist,
    db = "db/bus_parados.db",
    arquivo_dia = 1,
    intervalo = "01:00:00"
  ){
  
  conexao <- dbConnect(drv = RSQLite::SQLite(), db)
  
  df <- df |>
    filter(velocity == 0 & name == arquivo_dia) |>
    mutate(
      date = lubridate::mdy_hms(date),
      date_ymd = as.Date(date),
      date_hms = hms(as_hms(date)),
    )
  
  limites_rj <- tribble(
    ~lat_min,  ~lat_max,  ~long_min, ~long_max,
    -23.06677, -22.78761, -43.7207,  -43.15302,
  )
  
  var_tempo <- hms("00:00:00")
  
  while (var_tempo < hms("23:59:59")) {
    
  var_lat <- limites_rj$lat_min
  var_long <- limites_rj$long_min
  
    df_filter_tempo <- df |>
      filter(date_hms > var_tempo & date_hms <= seconds_to_period(period_to_seconds(var_tempo) + period_to_seconds(hms(intervalo)))) |>
      select(lat, long)
    
    while (var_lat < limites_rj$lat_max) {
      while (var_long < limites_rj$long_max) {
        
        var_busdata_filtrado <- df_filter_tempo |>
          filter(
            lat >= var_lat,
            lat < var_lat + dist,
            long >= var_long,
            long <  var_long + dist,
          ) |>
          summarise(n = n())
        
        info_pontos_regiao <- tribble(
          ~var_temp,                                       ~lat_min,   ~lat_max,         ~long_min,   ~long_max,         ~n,
          paste0(var_tempo$hour, ":", var_tempo$minute),   var_lat,    var_lat + dist,   var_long,    var_long + dist,   var_busdata_filtrado$n
        )
        
        dbWriteTable(conexao, "bus_regiao", info_pontos_regiao, append = TRUE)
        
        var_long <- var_long + dist
      }
      
      var_long <- limites_rj$long_min
      var_lat <- var_lat + dist
    }
    
    var_tempo <- seconds_to_period(period_to_seconds(var_tempo) + period_to_seconds(hms(intervalo)))
  }
  
}
