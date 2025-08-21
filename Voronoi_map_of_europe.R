##########################################
# 
# Voronoi map of europe
#
# Series:
# Little Useless-useful R functions #71
# Created: August 20, 2025
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#        
###########################################

#install.packages(c("sf","lwgeom","dplyr","ggplot2","rnaturalearth","rnaturalearthdata","cartogram"), dependencies = TRUE)

library(sf)
library(lwgeom)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(cartogram)


# EU map
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
eu <- world |>
  dplyr::filter(continent == "Europe" | name_long %in% c("Russia","Turkey","Kazakhstan","Azerbaijan","Cyprus")) |>
  st_make_valid()


eu <- world |>
  dplyr::filter(continent == "Europe" | name_long %in% c("Turkey","Cyprus")) |>
  dplyr::filter(sovereignt != "Russia") |>
  st_make_valid()

eu_ae <- st_transform(eu, 3035)  # ETRS89 / LAEA Europe / check ----->

# some fake data :D
metal <- tibble::tribble(
  ~iso_a2, ~bands,
  "FI", 1650,
  "SE", 1000,
  "NO", 600,
  "IS",  450,
  "DK", 700,
  "EE",  500,
  "LV",  380,
  "LT",  520
)


metal <- tibble::tribble(
  ~iso_a2, ~bands,
  "FI", 1,
  "SE", 1,
  "NO", 1,
  "IS",  1,
  "DK", 1,
  "EE",  1,
  "LV",  1,
  "LT",  1
)

eu_dat <- eu_ae |>
  left_join(metal, by = "iso_a2")

# add random noise weights :)
if (anyNA(eu_dat$bands)) {
  min_pos <- max(1, floor(min(eu_dat$bands, na.rm = TRUE) * 0.05))
  eu_dat$bands[is.na(eu_dat$bands)] <- min_pos
}

# Continuous cartogram  - add weithgt of the bands :D
eu_cart <- cartogram::cartogram_cont(eu_dat, weight = "bands", itermax = 15)

# Voronoi tessellation in the DEFORMED space ----
seeds <- st_point_on_surface(eu_cart)

win <- st_make_valid(st_union(eu_cart))
v_raw <- st_voronoi(st_union(st_geometry(seeds)))                  
vor  <- st_collection_extract(v_raw, "POLYGON") |> st_sf(crs = st_crs(eu_cart))
vor  <- suppressWarnings(st_intersection(vor, win))                


# add seeds for voronoi diagram
cent <- st_centroid(vor)
idx  <- st_nearest_feature(cent, seeds)
vor$iso_a2    <- seeds$iso_a2[idx]
vor$name_long <- seeds$name_long[idx]
vor$bands     <- seeds$bands[idx]



ggplot() +
  geom_sf(data = vor, aes(fill = bands), color = "grey20", linewidth = 0.2) +
  geom_sf(data = win, fill = NA, color = "black", linewidth = 0.4) +
  scale_fill_viridis_c(option = "C", name = "Metal bands") +
  coord_sf(crs = st_crs(3035)) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major = element_blank(), axis.title = element_blank())

