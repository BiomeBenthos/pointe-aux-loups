# library(pao)
# Bathymétrie
# bathy <- st_read("data/bathymetrie.geojson")
# bathymetrie <- st_contour(bathy, contour_lines = TRUE, breaks = c(seq(-15,1, by = 1),35))
# bathymetrie <- st_contour(bathy, contour_lines = TRUE, breaks = c(-15,0,35))
# tmp <- mapedit::editMap()
# tmp <- st_transform(tmp, st_crs(bathymetrie))
# st_write(tmp, 'analysis/data/tmp.geojson', delete_dsn = TRUE)
# tmp <- st_read('analysis/data/tmp.geojson', quiet = TRUE)
# bathymetrie <- st_intersection(bathymetrie, tmp)
# bathymetrie <- st_transform(bathymetrie, st_crs(recharge_crete))
# st_write(bathymetrie, './data/bathymetrie.geojson', delete_dsn = TRUE)


library(stars)
library(sf)

bathy <- sf::st_read("data/bathymetrie.geojson")
test <- smoothr::smooth(bathy, method = "chaikin")

bathy1 <- stars::read_stars("data/bathymetry/GeoTIFF/NONNA10_4750N06160W.tiff")
bathy2 <- stars::read_stars("data/bathymetry/GeoTIFF/NONNA10_4750N06170W.tiff")
bathy3 <- stars::read_stars("data/bathymetry/GeoTIFF/NONNA10_4760N06160W.tiff")

bathy1 <- st_contour(bathy1,
                     contour_lines = TRUE,
                     breaks = units::as_units(c(-20, -10, 0), "m"))
bathy2 <- st_contour(bathy2,
                     contour_lines = TRUE,
                     breaks = units::as_units(c(-20, -10, 0), "m"))
bathy3 <- st_contour(bathy3,
                     contour_lines = TRUE,
                     breaks = units::as_units(c(-20, -10, 0), "m"))

bathy <- rbind(bathy1, bathy2, bathy3)
bathy <- smoothr::smooth(bathy, method = "chaikin")
st_write(bathy, "data/bathymetrie.geojson", append = FALSE)


bathy10 <- st_collection_extract(
  st_split(bathy, 
           intersect),              
  "LINESTRING")
bathy10 <- bathy10[1,]