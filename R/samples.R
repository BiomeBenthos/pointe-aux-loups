library(sf)
library(dplyr)
library(magrittr)

sous_sites <- st_read('./data/soussites.geojson')
source('./R/fnc_sampling.R')

# Samples 2023
samples <- list()
for(i in 1:nrow(sous_sites)) {
  cat(sprintf("Rangée %i de %i \n",i, nrow(sous_sites)))
  samples[[i]] <- sampling(sous_sites[i, "noms_sous_sites"])
}
samples <- bind_rows(samples)

samples <- st_transform(samples, "EPSG:4326")

st_write(samples, './data/samples_2023.geojson', delete_dsn = TRUE)
