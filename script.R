# Config/Setup -----------------------------------------------------------------

library(tidyverse)
library(unglue)
library(janitor)
library(DBI)
library(arrow)
library(hms)
library(tictoc)
library(lubridate)

diretorio_busdata <- "busdata"

source("R/cria_cenarios.R")

# Read files -------------------------------------------------------------------

df <- list.files(diretorio_busdata) |> 
  enframe(value = "arquivo") |> 
  rowwise() |> 
  mutate(
    conteudo = list(str_glue("{diretorio_busdata}/{arquivo}") |> read_parquet())
  ) |> 
  unnest(conteudo) |>
  clean_names()

# Calc dist --------------------------------------------------------------------

dist <- c(
  c(-22.81653, -43.39437) - c(-22.81653, -43.39887),
  c(-22.81653, -43.39437) - c(-22.82272, -43.39437)
)

dist <- min(dist[dist > 0])

# Call Function ----------------------------------------------------------------

cria_cenarios(df, dist, arquivo_dia = 1)

# Test -------------------------------------------------------------------------

conexao <- dbConnect(drv = RSQLite::SQLite(), "db/bus_parados.db")

test <- tbl(conexao, "bus_regiao") |>
  collect() |>
  group_by(var_temp) |>
  summarise(sum = sum(n))