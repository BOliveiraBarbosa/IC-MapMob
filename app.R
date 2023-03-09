library(shiny) 
library(tidyverse)
library(arrow)
library(plotly)
library(lubridate)

library(shiny)

ui <- fluidPage(
  numericInput(
    inputId = "filtra_qtd_pontos", 
    label = "Filtrar Qtd. Pontos", 
    value = 1, 
    min = 0
  ),
  
  sliderInput(
    inputId = "filtra_hora",
    label = "Filtra Intervalo de Hora",
    min = lubridate::origin,
    max = lubridate::origin + days(1) - seconds(1),
    value = c(lubridate::origin, lubridate::origin + days(1) - seconds(1)),
    step = 5 * 60,
    timeFormat = "%H:%M",
    timezone = "+0000",
    ticks = FALSE
  ),
  plotlyOutput("test")
)

server <- function(input, output) {
  
  output$test <- renderPlotly({
    
    # browser()
    
    df_cenario_group <- open_dataset("busdata_class/500m") |> 
      mutate(
        date_hm = format(mdy_hms(DATE), "%H:%M")
      ) |>
      filter(date_hm >= format(input$filtra_hora[1], "%H:%M") & date_hm <= format(input$filtra_hora[2], "%H:%M")) |>
      group_by(class_lat, class_long, lat_min, long_min, lat_max, long_max, lat_mean, long_mean) |>
      summarise(qtd_pontos = n()) |>
      ungroup() |>
      collect() |>
      group_by(class_lat, class_long) |>
      mutate(class_id = cur_group_id()) |>
      ungroup() |>
      select(-c(class_lat, class_long)) |>
      filter(qtd_pontos > input$filtra_qtd_pontos)
    
    df_cenario_pontos <- df_cenario_group |>
      select(-c(lat_mean, long_mean)) |>
      pivot_longer(cols = !c(class_id, qtd_pontos, lat_min, lat_max), names_to = "long_ref", values_to = "long_loc") |> 
      pivot_longer(cols = !c(class_id, qtd_pontos, long_ref, long_loc), names_to = "lat_ref", values_to = "lat_loc") |>
      mutate(
        position = case_when(
          long_ref == "long_min" & lat_ref == "lat_min" ~ 1,
          long_ref == "long_min" & lat_ref == "lat_max" ~ 2,
          long_ref == "long_max" & lat_ref == "lat_max" ~ 3,
          long_ref == "long_max" & lat_ref == "lat_min" ~ 4,
        )
      )
    
    temp_termina_quad <- df_cenario_pontos |>
      filter(position == 1) |>
      mutate(position = 5)
    
    df_cenario_pontos <- df_cenario_pontos |>
      bind_rows(temp_termina_quad) |> 
      arrange(class_id, position) |>
      group_by(class_id)
    
    fig <- plot_ly(type = "scattermapbox", mode = "markers") |>
      add_trace(
        data = df_cenario_pontos,
        fill = "toself",
        lat = ~lat_loc,
        lon = ~long_loc,
        color = ~qtd_pontos,
        mode = "markers+lines",
        marker = list(colorscale = "RdBu"),
        line = list(color = "black"),
        fillcolor = "color",
        hoverinfo = "text",
        text = ~paste(
          "Lat:", lat_loc,
          "Long:", long_loc
        ),
        name = "Quadrados"
      ) |>
      add_trace(
        data = df_cenario_group,
        lat = ~lat_mean,
        lon = ~long_mean,
        color = ~qtd_pontos,
        size = ~qtd_pontos,
        opacity = 0.5,
        marker = list(colorscale = "RdBu"),
        hoverinfo = "text",
        text = ~paste(
          "Lat:", lat_mean,
          "Long:", long_mean, "<br>",
          "ID Quadrado:", class_id,
          "Qtd. Pontos:", qtd_pontos
        ),
        name = "Informações Quadrados"
      ) |>
      layout(
        mapbox = list(
          style = "open-street-map",
          zoom = 9.2,
          center = list(lat = -22.9, lon = -43.45)
        )
      ) |>
      hide_colorbar() |>
      suppressWarnings() # Remove Warnings
    
    fig
    
  })
}
 
shinyApp(ui = ui, server = server)
