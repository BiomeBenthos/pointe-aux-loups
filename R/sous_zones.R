library(sf)
library(lwgeom)
library(dplyr)

# Setup
zones <- sf::st_read("data/zones.geojson")
construction <- st_read("data/lignes_construction/6301-23-0104_20230504_ZoneTransfertAdministrationF0110.shp")
bathymetrie <- st_read("data/bathymetrie.geojson") |>
  st_transform(x=_, crs = st_crs(construction))

intersections <- st_read("data/intersect.geojson")
intersections <- st_combine(intersections)
intersections <- st_transform(intersections, st_crs(construction))

# Lignes de départ pour les sites 3 et 4
construction <- st_cast(construction,"POLYGON")[1,]
construction <- st_cast(construction,"LINESTRING")

lignes <- st_collection_extract(st_split(construction, st_combine(intersections)),"LINESTRING")
lignes <- lignes[rownames(lignes) %in% "1.1",]
lignes$nom_site <- c("haut")

uid <- lignes$nom_site == "haut"
xy <- st_point_on_surface(lignes[lignes$nom_site == "haut",]) %>%
      st_sf()

distance_to_0 <- xy %>%
                 st_nearest_points(bathymetrie) %>%
                 st_cast("POINT") %>%
                 st_distance() %>%
                 .[2,1] %>%
                 units::drop_units()

xy <- st_cast(lignes[lignes$nom_site == "haut",], "POINT") %>%
       .[c(1,nrow(.)),] %>%
       st_sf()

xym <- st_point_on_surface(lignes[lignes$nom_site == "haut",]) %>%
       st_sf()


# site3line
site3line <- bind_rows(xym, xy[2, ]) %>%
               st_union() %>%
               st_cast("LINESTRING") 

# site4line
site4line <- bind_rows(xym, xy[1, ]) %>%
               st_union() %>%
               st_cast("LINESTRING") 

# Lignes de départ des autres sites
site1line <- st_read("data/site1line.geojson") |>
  st_transform(x = _, crs = st_crs(bathymetrie))
site2line <- st_read("data/site2line.geojson") |>
  st_transform(x = _, crs = st_crs(bathymetrie))
site5line <- st_read("data/site5line.geojson") |>
  st_transform(x = _, crs = st_crs(bathymetrie))
site6line <- st_read("data/site6line.geojson") |>
  st_transform(x = _, crs = st_crs(bathymetrie))
  

# Séparer segments en 3 (site1)
# Ordre: El, Mo, Pr
pts_site1 <- st_segmentize(site1line, st_length(site1line)/3) |> 
               st_cast(x=_, to="POINT")
dist_rive <- c("1100", "600", "100")
dist_recharge <- c("pr", "mo", "el")

soussite1 <- lapply(1:3, function(x) {
  
  pts_segment <- st_cast(st_union(pts_site1[c(x, x+1),]), "LINESTRING")

  subz1100 <- st_buffer(pts_segment,
                        1100,
                        endCapStyle = "SQUARE",
                        singleSide = TRUE) %>%
                st_as_sf(data.frame(nom = 'subz1100'))
  subz600 <- st_buffer(pts_segment,
                       600,
                       endCapStyle = "SQUARE",
                       singleSide = TRUE) %>%
               st_as_sf(data.frame(nom = 'subz600'))
  subz100 <- st_buffer(pts_segment,
                       100,
                       endCapStyle = "SQUARE",
                       singleSide = TRUE) %>%
               st_as_sf(data.frame(nom = 'subz100'))

  subz1100 <- st_difference(subz1100, subz600) 
  subz600 <- st_difference(subz600, subz100)
  
  subz1100 <- st_cast(subz1100, "POINT")
  pts_to_rm <- st_geometry(subz1100)[st_geometry(subz1100) %in% 
               st_geometry(pts_site1)]
  subz1100 <- subz1100[!subz1100$x %in% pts_to_rm,]
  subz1100 <- st_combine(subz1100) |>
                st_cast(x=_, to = "POLYGON") |>
                  st_as_sf(data.frame(nom = 'subz1100'))

  subz600 <- st_cast(subz600, "POINT")
  pts_to_rm <- st_geometry(subz600)[st_geometry(subz600) %in% 
               st_geometry(pts_site1)]
  subz600 <- subz600[!subz600$x %in% pts_to_rm,]
  subz600 <- st_combine(subz600) |>
               st_cast(x=_, to = "POLYGON") |>
                 st_as_sf(data.frame(nom = 'subz600'))

  subz <- rbind(subz1100, subz600, subz100)
  subz$nom <- paste0("pal1-",dist_recharge[x],"-",dist_rive)
  return(subz)

}) |>
  do.call(what=rbind, args=_)


# Séparer segments en 3 (site2)
# Ordre: Pr, Mo, El
pts_site2 <- st_segmentize(site2line, st_length(site2line)/3) |> 
               st_cast(x=_, to="POINT")
dist_rive <- c("1100", "600", "100")
dist_recharge <- c("pr", "mo", "el")

soussite2 <- lapply(1:3, function(x) {
  
  pts_segment <- st_cast(st_union(pts_site2[c(x, x+1),]), "LINESTRING")

  subz1100 <- st_buffer(pts_segment,
                        1100,
                        endCapStyle = "SQUARE",
                        singleSide = TRUE) %>%
                st_as_sf(data.frame(nom = 'subz1100'))
  subz600 <- st_buffer(pts_segment,
                       600,
                       endCapStyle = "SQUARE",
                       singleSide = TRUE) %>%
               st_as_sf(data.frame(nom = 'subz600'))
  subz100 <- st_buffer(pts_segment,
                       100,
                       endCapStyle = "SQUARE",
                       singleSide = TRUE) %>%
               st_as_sf(data.frame(nom = 'subz100'))

  subz1100 <- st_difference(subz1100, subz600) 
  subz600 <- st_difference(subz600, subz100)
  
  subz1100 <- st_cast(subz1100, "POINT")
  pts_to_rm <- st_geometry(subz1100)[st_geometry(subz1100) %in% 
               st_geometry(pts_site2)]
  subz1100 <- subz1100[!subz1100$x %in% pts_to_rm,]
  subz1100 <- st_combine(subz1100) |>
                st_cast(x=_, to = "POLYGON") |>
                  st_as_sf(data.frame(nom = 'subz1100'))

  subz600 <- st_cast(subz600, "POINT")
  pts_to_rm <- st_geometry(subz600)[st_geometry(subz600) %in% 
               st_geometry(pts_site2)]
  subz600 <- subz600[!subz600$x %in% pts_to_rm,]
  subz600 <- st_combine(subz600) |>
               st_cast(x=_, to = "POLYGON") |>
                 st_as_sf(data.frame(nom = 'subz600'))

  subz <- rbind(subz1100, subz600, subz100)
  subz$nom <- paste0("pal2-",dist_recharge[x],"-",dist_rive)
  return(subz)

}) |>
  do.call(what=rbind, args=_)


# Séparer segments en 3 (site1)
# Ordre: El, Mo, Pr
pts_site3 <- st_segmentize(site3line, st_length(site3line)/3) |> 
               st_cast(x=_, to="POINT")
dist_rive <- c("1100", "600", "100")
dist_recharge <- c("el", "mo", "pr")

soussite3 <- lapply(1:3, function(x) {
  
  pts_segment <- st_cast(st_union(pts_site3[c(x, x+1)]), "LINESTRING")

  subz1100 <- st_buffer(pts_segment,
                        1100,
                        endCapStyle = "SQUARE",
                        singleSide = TRUE) %>%
                st_as_sf(data.frame(nom = 'subz1100'))
  subz600 <- st_buffer(pts_segment,
                       600,
                       endCapStyle = "SQUARE",
                       singleSide = TRUE) %>%
               st_as_sf(data.frame(nom = 'subz600'))
  subz100 <- st_buffer(pts_segment,
                       100,
                       endCapStyle = "SQUARE",
                       singleSide = TRUE) %>%
               st_as_sf(data.frame(nom = 'subz100'))

  subz1100 <- st_difference(subz1100, subz600) 
  subz600 <- st_difference(subz600, subz100)
  
  subz1100 <- st_cast(subz1100, "POINT")
  pts_to_rm <- st_geometry(subz1100)[st_geometry(subz1100) %in% 
               st_geometry(pts_site3)]
  subz1100 <- subz1100[!subz1100$x %in% pts_to_rm,]
  subz1100 <- st_combine(subz1100) |>
                st_cast(x=_, to = "POLYGON") |>
                  st_as_sf(data.frame(nom = 'subz1100'))

  subz600 <- st_cast(subz600, "POINT")
  pts_to_rm <- st_geometry(subz600)[st_geometry(subz600) %in% 
               st_geometry(pts_site3)]
  subz600 <- subz600[!subz600$x %in% pts_to_rm,]
  subz600 <- st_combine(subz600) |>
               st_cast(x=_, to = "POLYGON") |>
                 st_as_sf(data.frame(nom = 'subz600'))

  subz <- rbind(subz1100, subz600, subz100)
  subz$nom <- paste0("pal3-",dist_recharge[x],"-",dist_rive)
  return(subz)

}) |>
  do.call(what=rbind, args=_)


# Séparer segments en 3 (site1)
# Ordre: El, Mo, Pr
pts_site4 <- st_segmentize(site4line, st_length(site4line)/3) |> 
               st_cast(x=_, to="POINT")
dist_rive <- c("1100", "600", "100")
dist_recharge <- c("pr", "mo", "el")

soussite4 <- lapply(1:3, function(x) {
  
  pts_segment <- st_cast(st_union(pts_site4[c(x, x+1)]), "LINESTRING")

  subz1100 <- st_buffer(pts_segment,
                        1100,
                        endCapStyle = "SQUARE",
                        singleSide = TRUE) %>%
                st_as_sf(data.frame(nom = 'subz1100'))
  subz600 <- st_buffer(pts_segment,
                       600,
                       endCapStyle = "SQUARE",
                       singleSide = TRUE) %>%
               st_as_sf(data.frame(nom = 'subz600'))
  subz100 <- st_buffer(pts_segment,
                       100,
                       endCapStyle = "SQUARE",
                       singleSide = TRUE) %>%
               st_as_sf(data.frame(nom = 'subz100'))

  subz1100 <- st_difference(subz1100, subz600) 
  subz600 <- st_difference(subz600, subz100)
  
  subz1100 <- st_cast(subz1100, "POINT")
  pts_to_rm <- st_geometry(subz1100)[st_geometry(subz1100) %in% 
               st_geometry(pts_site4)]
  subz1100 <- subz1100[!subz1100$x %in% pts_to_rm,]
  subz1100 <- st_combine(subz1100) |>
                st_cast(x=_, to = "POLYGON") |>
                  st_as_sf(data.frame(nom = 'subz1100'))

  subz600 <- st_cast(subz600, "POINT")
  pts_to_rm <- st_geometry(subz600)[st_geometry(subz600) %in% 
               st_geometry(pts_site4)]
  subz600 <- subz600[!subz600$x %in% pts_to_rm,]
  subz600 <- st_combine(subz600) |>
               st_cast(x=_, to = "POLYGON") |>
                 st_as_sf(data.frame(nom = 'subz600'))

  subz <- rbind(subz1100, subz600, subz100)
  subz$nom <- paste0("pal4-",dist_recharge[x],"-",dist_rive)
  return(subz)

}) |>
  do.call(what=rbind, args=_)


# Séparer segments en 3 (site1)
# Ordre: El, Mo, Pr
pts_site5 <- st_segmentize(site5line, st_length(site5line)/3) |> 
               st_cast(x=_, to="POINT")
dist_rive <- c("1100", "600", "100")
dist_recharge <- c("pr", "mo", "el")

soussite5 <- lapply(1:3, function(x) {
  
  pts_segment <- st_cast(st_union(pts_site5[c(x, x+1),]), "LINESTRING")

  subz1100 <- st_buffer(pts_segment,
                        1100,
                        endCapStyle = "SQUARE",
                        singleSide = TRUE) %>%
                st_as_sf(data.frame(nom = 'subz1100'))
  subz600 <- st_buffer(pts_segment,
                       600,
                       endCapStyle = "SQUARE",
                       singleSide = TRUE) %>%
               st_as_sf(data.frame(nom = 'subz600'))
  subz100 <- st_buffer(pts_segment,
                       100,
                       endCapStyle = "SQUARE",
                       singleSide = TRUE) %>%
               st_as_sf(data.frame(nom = 'subz100'))

  subz1100 <- st_difference(subz1100, subz600) 
  subz600 <- st_difference(subz600, subz100)
  
  subz1100 <- st_cast(subz1100, "POINT")
  pts_to_rm <- st_geometry(subz1100)[st_geometry(subz1100) %in% 
               st_geometry(pts_site5)]
  subz1100 <- subz1100[!subz1100$x %in% pts_to_rm,]
  subz1100 <- st_combine(subz1100) |>
                st_cast(x=_, to = "POLYGON") |>
                  st_as_sf(data.frame(nom = 'subz1100'))

  subz600 <- st_cast(subz600, "POINT")
  pts_to_rm <- st_geometry(subz600)[st_geometry(subz600) %in% 
               st_geometry(pts_site5)]
  subz600 <- subz600[!subz600$x %in% pts_to_rm,]
  subz600 <- st_combine(subz600) |>
               st_cast(x=_, to = "POLYGON") |>
                 st_as_sf(data.frame(nom = 'subz600'))

  subz <- rbind(subz1100, subz600, subz100)
  subz$nom <- paste0("pal5-",dist_recharge[x],"-",dist_rive)
  return(subz)

}) |>
  do.call(what=rbind, args=_)


# Séparer segments en 3 (site1)
# Ordre: El, Mo, Pr
pts_site6 <- st_segmentize(site6line, st_length(site6line)/3) |> 
               st_cast(x=_, to="POINT")
dist_rive <- c("1100", "600", "100")
dist_recharge <- c("pr", "mo", "el")

soussite6 <- lapply(1:3, function(x) {
  
  pts_segment <- st_cast(st_union(pts_site6[c(x, x+1),]), "LINESTRING")

  subz1100 <- st_buffer(pts_segment,
                        1100,
                        endCapStyle = "SQUARE",
                        singleSide = TRUE) %>%
                st_as_sf(data.frame(nom = 'subz1100'))
  subz600 <- st_buffer(pts_segment,
                       600,
                       endCapStyle = "SQUARE",
                       singleSide = TRUE) %>%
               st_as_sf(data.frame(nom = 'subz600'))
  subz100 <- st_buffer(pts_segment,
                       100,
                       endCapStyle = "SQUARE",
                       singleSide = TRUE) %>%
               st_as_sf(data.frame(nom = 'subz100'))

  subz1100 <- st_difference(subz1100, subz600) 
  subz600 <- st_difference(subz600, subz100)
  
  subz1100 <- st_cast(subz1100, "POINT")
  pts_to_rm <- st_geometry(subz1100)[st_geometry(subz1100) %in% 
               st_geometry(pts_site6)]
  subz1100 <- subz1100[!subz1100$x %in% pts_to_rm,]
  subz1100 <- st_combine(subz1100) |>
                st_cast(x=_, to = "POLYGON") |>
                  st_as_sf(data.frame(nom = 'subz1100'))

  subz600 <- st_cast(subz600, "POINT")
  pts_to_rm <- st_geometry(subz600)[st_geometry(subz600) %in% 
               st_geometry(pts_site6)]
  subz600 <- subz600[!subz600$x %in% pts_to_rm,]
  subz600 <- st_combine(subz600) |>
               st_cast(x=_, to = "POLYGON") |>
                 st_as_sf(data.frame(nom = 'subz600'))

  subz <- rbind(subz1100, subz600, subz100)
  subz$nom <- paste0("pal6-",dist_recharge[x],"-",dist_rive)
  return(subz)

}) |>
  do.call(what=rbind, args=_)


soussite1 <- st_intersection(soussite1, zones[zones$nom == "pal1",])
soussite2 <- st_intersection(soussite2, zones[zones$nom == "pal2",])
soussite3 <- st_intersection(soussite3, zones[zones$nom == "pal3",])
soussite4 <- st_intersection(soussite4, zones[zones$nom == "pal4",])
soussite5 <- st_intersection(soussite5, zones[zones$nom == "pal5",])
soussite6 <- st_intersection(soussite6, zones[zones$nom == "pal6",])


soussite3 <- st_collection_extract(
               st_split(soussite3,
                        lignes),
               "POLYGON")
soussite3 <- soussite3[!rownames(soussite3) %in% c("3", "6", "9"),]

soussite4 <- st_collection_extract(
               st_split(soussite4,
                        lignes),
               "POLYGON")
soussite4 <- soussite4[!rownames(soussite4) %in% c("3.1", "6.1"),]

soussites <- rbind(soussite1,
                   soussite2,
                   soussite3,
                   soussite4,
                   soussite5,
                   soussite6)
                   
names(soussites)[1:2] <- c("noms_sous_sites", "noms_sites")

st_write(st_as_sf(soussites), "data/soussites.geojson")
