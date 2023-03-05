# Config/Setup -----------------------------------------------------------------

library(tidyverse)
library(arrow)

source("R/cria_cenarios.R")

# Calc dist --------------------------------------------------------------------

# geosphere::distHaversine(c(-22.81653, -43.39437), c(-22.81653, -43.39887)) # 500m
# geosphere::distHaversine(c(-22.81653, -43.39437), c(-22.82890, -43.39437)) # 1000

dist_500 <- c(
  c(-22.81653, -43.39437) - c(-22.81653, -43.39887),
  c(-22.81653, -43.39437) - c(-22.82272, -43.39437)
)

dist_500 <- min(dist_500[dist_500 > 0])

dist_1000 <- c(
  c(-22.81653, -43.39437) - c(-22.81653, -43.38538),
  c(-22.81653, -43.39437) - c(-22.82890, -43.39437)
)

dist_1000 <- min(dist_1000[dist_1000 > 0])

# Call Function ----------------------------------------------------------------

df_cenario <- cria_cenarios(diretorio = "busdata", dist = dist_500, caminho_arquivos = "busdata_class/500m")
df_cenario <- cria_cenarios(diretorio = "busdata", dist = dist_1000, caminho_arquivos = "busdata_class/1000m")
