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


#install if needed

install.packages(c("sf","dplyr","ggplot2","rnaturalearth","rnaturalearthdata","readr",
                   "qlcVisualize", "automap"), dependencies=TRUE)

#install.packages("automap")

#install.packages("devtools")
library("devtools")
#devtools::install_github("cysouw/qlcVisualize")

library(sf)
library(dplyr)
library(ggplot2)
library(readr)
library(rnaturalearth)
library(rnaturalearthdata)
library(qlcVisualize)

 
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
eu <- world |>
  filter(continent == "Europe" | name_long %in% c("Russia","Turkey","Kazakhstan","Azerbaijan","Cyprus")) |>
  st_make_valid()

 
eu_ae <- st_transform(eu, 3035)

 
seeds <- st_point_on_surface(eu_ae)
seeds <- st_as_sf(   
  data.frame(
    iso_a2 = seeds$iso_a2,
    name_long = seeds$name_long
  ),
  geometry = st_geometry(seeds),
  crs = st_crs(eu_ae)
)

# Expect a CSV 
# metal <- readr::read_csv("metal_bands_per_country.csv")

# For demo, here’s a tiny inline example (replace with your file above):
metal <- tibble::tribble(
  ~iso_a2, ~bands,
  "FI", 3650,   # Finland > Sweden -> Finland gets larger than Sweden
  "SE", 2900,
  "NO", 2100,
  "IS",  450,
  "DK", 1300,
  "EE",  500,
  "LV",  380,
  "LT",  520
)

seeds <- seeds |>
  left_join(metal, by = "iso_a2")

seeds$bands[is.na(seeds$bands)] <- min(seeds$bands, na.rm = TRUE) * 0.05

eu_window <- st_union(eu_ae)

### This is part with qlcVisualize
# qlcVisualize::weightedMap accepts an sf of points for `x`
# `weights` is your metric; method = "gn" (Gastner-Newman / flow-based) is accurate
# Increase maxit if you want tighter area matching (may take longer on complex maps)
v <- qlcVisualize::weightedMap(
  x        = seeds,                # sf POINTS (already in EPSG:3035)
  window   = eu_window,            # sf MULTIPOLYGON
  crs      = 3035,                 # keep everything in equal-area CRS
  weights  = seeds$bands,          # your weights
  method   = "gn",                 # "gn" or "gsm" (accurate), "dcn" (faster, rougher)
  maxit    = 20,
  verbose  = 0
)

vor <- v$weightedVoronoi
vor$iso_a2     <- seeds$iso_a2
vor$name_long  <- seeds$name_long
vor$bands      <- seeds$bands


ggplot() +
  geom_sf(data = vor, aes(fill = bands), color = "grey20", linewidth = 0.2) +
  geom_sf(data = v$weightedWindow, fill = NA, color = "black", linewidth = 0.4) +
  guides(fill = guide_colorbar(title = "Metal bands")) +
  coord_sf(crs = st_crs(3035)) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_blank(),
    axis.title = element_blank()
  )

# Optional: label countries at their *deformed* positions
# lab_pts <- st_as_sf(v$weightedPoints) # POINTS after deformation
# lab_pts$name_long <- seeds$name_long
# ggplot() +
#   geom_sf(data = vor, aes(fill = bands), color = "grey20", linewidth = 0.2) +
#   geom_sf_text(data = lab_pts, aes(label = name_long), size = 2) +
#   geom_sf(data = v$weightedWindow, fill = NA, color = "black", linewidth = 0.4) +
#   coord_sf(crs = st_crs(3035)) +
#   theme_void()

