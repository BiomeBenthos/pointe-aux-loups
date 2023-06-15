library(sf)
library(lwgeom)
library(dplyr)

construction <- st_read("data/lignes_construction/6301-23-0104_20230504_ZoneTransfertAdministrationF0110.shp")
bathymetrie <- st_read("data/bathymetrie.geojson") |>
  st_transform(x=_, crs = st_crs(construction))

intersections <- st_read("data/intersect.geojson")
intersections <- st_combine(intersections)
intersections <- st_transform(intersections, st_crs(construction))

construction <- st_cast(construction,"POLYGON")[1,]
construction <- st_cast(construction,"LINESTRING")

lignes <- st_collection_extract(st_split(construction, st_combine(intersections)),"LINESTRING")

lignes <- lignes[rownames(lignes) %in% "1.1",]
lignes$nom_site <- c("haut")


# Commence ici pour zones

uid <- lignes$nom_site == "haut"
xy <- st_point_on_surface(lignes[lignes$nom_site == "haut",]) %>%
      st_sf()


distance_to_0 <- xy %>%
                 st_nearest_points(bathymetrie) %>%
                 st_cast("POINT") %>%
                 st_distance() %>%
                 .[2,1] %>%
                 units::drop_units()


# Zones
xy <- st_cast(lignes[lignes$nom_site == "haut",], "POINT") %>%
       .[c(1,nrow(.)),] %>%
       st_sf()

xym <- st_point_on_surface(lignes[lignes$nom_site == "haut",]) %>%
       st_sf()


# Site 3
site3 <- bind_rows(xym, xy[2, ]) %>%
         st_union() %>%
         st_cast("LINESTRING") %>%
         st_buffer(1095, endCapStyle = "SQUARE", singleSide = TRUE) %>%
         st_as_sf(data.frame(nom = 'pal3'))


# Site 4
site4 <- bind_rows(xym, xy[1, ]) %>%
         st_union() %>%
         st_cast("LINESTRING") %>%
         st_buffer(1095, endCapStyle = "SQUARE", singleSide = TRUE) %>%
         st_as_sf(data.frame(nom = 'pal4'))

# Site 1
site1line <- st_read("data/site1line.geojson") |>
  st_transform(x = _, crs = st_crs(bathymetrie))

site1 <- site1line %>%
         st_buffer(-1095, endCapStyle = "SQUARE", singleSide = TRUE) %>%
         st_as_sf(data.frame(nom = 'pal1'))
site1 <- site1[,-c(1,2)]
site1[,"nom"] <- "Site1"

# Site 2
site2line <- st_read("data/site2line.geojson") |>
  st_transform(x = _, crs = st_crs(bathymetrie))

site2 <- site2line %>%
         st_buffer(-1095, endCapStyle = "SQUARE", singleSide = TRUE) %>%
         st_as_sf(data.frame(nom = 'pal2'))
site2 <- site2[,-c(1,2)]
site2[,"nom"] <- "Site2"

# Site 5
site5line <- st_read("data/site5line.geojson") |>
  st_transform(x = _, crs = st_crs(bathymetrie))

site5 <- site5line %>%
         st_buffer(1095, endCapStyle = "SQUARE", singleSide = TRUE) %>%
         st_as_sf(data.frame(nom = 'pal5'))
site5  <- site5[,-c(1,2)]
site5[,"nom"] <- "Site5"
 
# Site 6
site6line <- st_read("data/site6line.geojson") |>
  st_transform(x = _, crs = st_crs(bathymetrie))

site6 <- site6line %>%
         st_buffer(1095, endCapStyle = "SQUARE", singleSide = TRUE) %>%
         st_as_sf(data.frame(nom = 'pal6'))
site6 <- site6[,-c(1,2)]
site6[,"nom"] <- "Site6"


#split12 <- mapedit::editMap(mapview::mapview(list(site1,site2)))$finished |>
#  st_transform(x = _, crs = st_crs(bathymetrie))
#split23 <- mapedit::editMap(mapview::mapview(list(site2,site3)))$finished |>
#  st_transform(x = _, crs = st_crs(bathymetrie))
#split45 <- mapedit::editMap(mapview::mapview(list(site4,site5)))$finished |>
#  st_transform(x = _, crs = st_crs(bathymetrie))
#split56 <- mapedit::editMap(mapview::mapview(list(site5,site6)))$finished |>
#  st_transform(x = _, crs = st_crs(bathymetrie))

#st_write(split12, './data/split12.geojson', delete_dsn = TRUE)
#st_write(split23, './data/split23.geojson', delete_dsn = TRUE)
#st_write(split45, './data/split45.geojson', delete_dsn = TRUE)
#st_write(split56, './data/split56.geojson', delete_dsn = TRUE)

split12 <- st_read('./data/split12.geojson')
split23 <- st_read('./data/split23.geojson')
split45 <- st_read('./data/split45.geojson')
split56 <- st_read('./data/split56.geojson')

#split12 <- qgis::qgis_extendlines(INPUT = split12,
#                               START_DISTANCE = 100,
#                               END_DISTANCE = 100) |> 
#           st_as_sf()
#split23 <- qgis::qgis_extendlines(INPUT = split23,
#                               START_DISTANCE = 100,
#                               END_DISTANCE = 100) |> 
#           st_as_sf()
#split45 <- qgis::qgis_extendlines(INPUT = split45,
#                               START_DISTANCE = 100,
#                               END_DISTANCE = 100) |> 
#           st_as_sf()
#split56 <- qgis::qgis_extendlines(INPUT = split56,
#                               START_DISTANCE = 100,
#                               END_DISTANCE = 100) |> 
#           st_as_sf()



site1spl <- st_collection_extract(
              st_split(site1,
                       split12),
              "POLYGON")
site1spl <- site1spl[1,]
site2spl <- st_collection_extract(
              st_split(site2,
                       split12),
              "POLYGON")
site2spl <- site2spl[1,]
site2spl <- st_collection_extract(
              st_split(site2spl,
                       split23),
              "POLYGON")
site2spl <- site2spl[1,]
site3spl <- st_collection_extract(
              st_split(site3,
                       split23),
              "POLYGON")
site3spl <- site3spl[1,]
site4spl <- st_collection_extract(
              st_split(site4,
                       split45),
              "POLYGON")
site4spl <- site4spl[1,]
site5spl <- st_collection_extract(
              st_split(site5,
                       split45),
              "POLYGON")
site5spl <- site5spl[1,]
site5spl <- st_collection_extract(
              st_split(site5spl,
                       split56),
              "POLYGON")
site5spl <- site5spl[1,]
site6spl <- st_collection_extract(
              st_split(site6,
                       split56),
              "POLYGON")
site6spl <- site6spl[1,]

st_geometry(site3spl) <- "geometry"
st_geometry(site4spl) <- "geometry"

zones <- rbind(site1spl, site2spl, site3spl, site4spl, site5spl, site6spl)

st_write(zones, './data/zones.geojson', delete_dsn = TRUE)
