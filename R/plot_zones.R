zones <- st_read("data/zones.geojson")
bathy <- st_read("data/bathymetrie.geojson") |>
  st_transform(x=_, crs = st_crs(zones))
site_recharge <- st_read("data/polygones")
site_recharge <- site_recharge[site_recharge$nom_site == "Site recharge",]


library(sf)
library(terra)

zones <- terra::vect(zones)
bathy <- terra::vect(bathy)
recharge <- terra::vect(site_recharge)


qc <- geodata::gadm("can", path = tempdir())
qc <- qc[qc$NAME_1 == "Québec",]
qc <- project(qc, zones)


svg("plan_echantillonnage.svg",width = 11, height = 8)
terra::plot(zones,
            col = "transparent",
            background = "white")
terra::plot(qc, add = TRUE, col = "lightgrey")
terra::plot(zones,
            y = "nom",
            col = viridis::viridis(6),
            type = "classes",,
            add = TRUE,
            alpha = 0.3)
terra::plot(recharge,
            y = "nom_site",
            col = "red",
            add = TRUE,
            colNA = "transparent")
terra::plot(bathy,
            add = TRUE,
            lty = 2,
            lwd = 2,
            colNA = "transparent")
sbar(1000, "bottom", type="bar", divs=4, below = "kilomètre",
     labels = c(0, 0.5, 1))
legend("right", 
       legend=c("Site 1",
                "Site 2",
                "Site 3",
                "Site 4",
                "Site 5",
                "Site 6",
                "Site recharge",
                "Bathymétrie 0m"),
       bty = "n",
       col = c(rep(NA, 7), "black"),
       lty = c(rep(0, 7), 2),
       lwd = 2,
       fill = c(viridis::viridis(6, alpha = 0.3),
                "red",
                NA),
       border = c(rep("black", 7), NA),
       merge = FALSE)
dev.off()
