library(sf)
library(terra)
library(stars)

bathy <- terra::rast("data/bathymetry/GeoTIFF/NONNA10_4750N06170W.tiff")
bathy <- terra::focal(bathy, w=16, fun=mean, na.policy="only", na.rm=T)

bathy_contour <- st_contour(stars::st_as_stars(bathy),
                            contour_lines = TRUE,
                            breaks = units::as_units(0, "m"))

test <- mapedit::editMap(mapview::mapview(bathy_contour))

test <- sf::st_transform(test$finished, sf::st_crs(bathy_contour))
bathy_final <- sf::st_intersection(bathy_contour, test)

st_write(bathy_final, './data/bathymetrie.geojson', delete_dsn = TRUE)


lignes <- sf::st_read("data/lignes_construction/6301-23-0104_20230504_ZoneTransfertAdministrationF0110.shp")

coco <- mapedit::editMap(mapview::mapview(sf::st_as_sf(lignes)))
multilignes <- terra::vect(lignes) |> as.lines() |> sf::st_as_sf()


