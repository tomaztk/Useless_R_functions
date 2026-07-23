library(geosphere)
library(ggplot2)
library(maps)
library(sf)
library(dplyr)
library(httr)
library(jsonlite)

geocode_city <- function(city_name) {
  cat(sprintf("  Geocoding: %s ...", city_name))
  
  url <- modify_url(
    "https://nominatim.openstreetmap.org/search",
    query = list(
      q       = city_name,
      format  = "json",
      limit   = 1
    )
  )
  
  resp <- tryCatch(
    GET(url, user_agent("PointingSignFinder/1.0 (R script)")),
    error = function(e) {
      cat(" FAILED (network error)\n")
      return(NULL)
    }
  )
  
  if (is.null(resp) || http_error(resp)) {
    cat(" FAILED (HTTP error)\n")
    return(NULL)
  }
  
  result <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
  
  if (length(result) == 0) {
    cat(" NOT FOUND\n")
    return(NULL)
  }
  
  lat <- as.numeric(result$lat[1])
  lon <- as.numeric(result$lon[1])
  cat(sprintf(" found: %.4f°, %.4f°\n", lat, lon))
  
  # Nominatim rate limit — be polite, 1 req/sec
  Sys.sleep(1.1)
  
  list(lat = lat, lon = lon, display_name = result$display_name[1])
}


sign_location_finder <- function(cities,
                                 distances,
                                 tolerance = 50,
                                 coarse_res = 0.2,
                                 fine_res   = 0.02,
                                 nearby_radius = 200,
                                 nearby_min_pop = 100000) {
  
  stopifnot(length(cities) == length(distances))
  stopifnot(length(cities) >= 2)
  
  cat("Step 1: Geocoding cities\n")
  coords <- lapply(cities, geocode_city)
  
  failed <- which(sapply(coords, is.null))
  if (length(failed) > 0) {
    stop(sprintf("Could not geocode: %s",
                 paste(cities[failed], collapse = ", ")))
  }
  
  sign_data <- data.frame(
    city    = cities,
    lat     = sapply(coords, `[[`, "lat"),
    lon     = sapply(coords, `[[`, "lon"),
    dist_km = distances
  )
  
  cat("\nResolved sign data:\n")
  print(sign_data[, c("city", "lat", "lon", "dist_km")])
  
 # bound and search
  max_dist_deg <- max(sign_data$dist_km) / 111
  lat_min <- min(sign_data$lat) - max_dist_deg - 5
  lat_max <- max(sign_data$lat) + max_dist_deg + 5
  lon_min <- min(sign_data$lon) - max_dist_deg * 2 - 5
  lon_max <- max(sign_data$lon) + max_dist_deg * 2 + 5
  
  # Clamp to valid lat/lon range
  lat_min <- max(lat_min, -85)
  lat_max <- min(lat_max,  85)
  lon_min <- max(lon_min, -180)
  lon_max <- min(lon_max,  180)
  
  cat(sprintf("\nStep 2: Search bounding box: lat [%.1f, %.1f], lon [%.1f, %.1f]\n",
              lat_min, lat_max, lon_min, lon_max))
  

  score_point <- function(plat, plon) {
    diffs <- sapply(seq_len(nrow(sign_data)), function(i) {
      d <- distHaversine(
        c(plon, plat),
        c(sign_data$lon[i], sign_data$lat[i])
      ) / 1000
      abs(d - sign_data$dist_km[i])
    })
    max(diffs)
  }
  
 
  cat(sprintf("\nStep 3: Coarse grid search (%.2f° resolution)...\n", coarse_res))
  grid <- expand.grid(
    lat = seq(lat_min, lat_max, by = coarse_res),
    lon = seq(lon_min, lon_max, by = coarse_res)
  )
  cat(sprintf("  Evaluating %d grid points...\n", nrow(grid)))
  grid$score <- mapply(score_point, grid$lat, grid$lon)
  
  candidates <- grid[grid$score <= tolerance, ]
  cat(sprintf("  Found %d candidate cells within ±%d km tolerance.\n",
              nrow(candidates), tolerance))
  
  coarse_best <- grid[which.min(grid$score), ]
  
 
  fine_grid <- expand.grid(
    lat = seq(coarse_best$lat - 1, coarse_best$lat + 1, by = fine_res),
    lon = seq(coarse_best$lon - 1, coarse_best$lon + 1, by = fine_res)
  )
  fine_grid$score <- mapply(score_point, fine_grid$lat, fine_grid$lon)
  best <- fine_grid[which.min(fine_grid$score), ]
  
  cat(sprintf("\n>>> Estimated sign location:\n"))
  cat(sprintf("    Latitude  : %.4f°\n", best$lat))
  cat(sprintf("    Longitude : %.4f°\n", best$lon))
  cat(sprintf("    Max error : ±%.1f km\n", best$score))
  cat(sprintf("    Google Maps: https://www.google.com/maps?q=%.4f,%.4f\n",
              best$lat, best$lon))
  
 
  cat(sprintf("\nStep 5: Nearby cities (within %d km, pop > %s)...\n",
              nearby_radius, format(nearby_min_pop, big.mark = ",")))
  
  world_cities <- world.cities
  nearby <- world_cities |>
    filter(pop > nearby_min_pop) |>
    mutate(
      dist_to_sign = distHaversine(
        cbind(long, lat),
        c(best$lon, best$lat)
      ) / 1000
    ) |>
    filter(dist_to_sign <= nearby_radius) |>
    arrange(dist_to_sign) |>
    select(name, country.etc, lat, long, pop, dist_to_sign) |>
    head(10)
  
  if (nrow(nearby) > 0) {
    cat("\n  Cities found:\n")
    print(nearby, digits = 4)
    cat(sprintf("\n  Nearest: %s, %s (%.1f km away)\n",
                nearby$name[1], nearby$country.etc[1], nearby$dist_to_sign[1]))
  } else {
    cat("  No major cities nearby — possibly a remote location.\n")
  }
  
### ne 
  colors <- c("#1d7aed", "#e8541a", "#16a34a",
              "#9333ea", "#dc2626", "#ca8a04", "#0891b2")
  col_cycle <- colors[((seq_len(nrow(sign_data)) - 1) %% length(colors)) + 1]
  
  world_map <- map_data("world")
  
  # Helper: build circle points
  circle_pts <- function(city_idx, radius_km) {
    pts <- destPoint(
      p = c(sign_data$lon[city_idx], sign_data$lat[city_idx]),
      b = seq(0, 360, length.out = 720),
      d = radius_km * 1000
    )
    data.frame(lon = pts[, 1], lat = pts[, 2],
               city = sign_data$city[city_idx],
               color = col_cycle[city_idx])
  }
  
  # --- World overview ---
  p_world <- ggplot() +
    geom_polygon(data = world_map,
                 aes(x = long, y = lat, group = group),
                 fill = "#e8e4dc", color = "#b5b1a9", linewidth = 0.2) +
    coord_fixed(1.3,
                xlim = c(lon_min, lon_max),
                ylim = c(lat_min, lat_max)) +
    theme_minimal(base_size = 13) +
    theme(
      panel.background = element_rect(fill = "#c8dff0", color = NA),
      panel.grid.major = element_line(color = "white", linewidth = 0.3),
      plot.title    = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 11, color = "#555")
    ) +
    labs(
      title    = "Pointing Sign Location Finder",
      subtitle = sprintf("Distance circle intersections (±%d km tolerance)", tolerance),
      x = "Longitude", y = "Latitude"
    )
  
  for (i in seq_len(nrow(sign_data))) {
    col <- col_cycle[i]
    p_world <- p_world +
      geom_path(data = circle_pts(i, sign_data$dist_km[i] - tolerance),
                aes(x = lon, y = lat),
                color = col, linetype = "dashed", linewidth = 0.35, alpha = 0.55) +
      geom_path(data = circle_pts(i, sign_data$dist_km[i] + tolerance),
                aes(x = lon, y = lat),
                color = col, linetype = "dashed", linewidth = 0.35, alpha = 0.55) +
      geom_path(data = circle_pts(i, sign_data$dist_km[i]),
                aes(x = lon, y = lat),
                color = col, linewidth = 0.9, alpha = 0.85)
  }
  
  p_world <- p_world + {
      for (i in seq_len(nrow(sign_data))) {
        p_world <- p_world +
          geom_point(
            data   = sign_data[i, ],
            aes(x = lon, y = lat),
            fill   = col_cycle[i],
            color  = "white",
            shape  = 21, size = 4, stroke = 1.2
          )
      }
    } + 
    geom_label(data = sign_data,
               aes(x = lon, y = lat,
                   label = paste0(city, "\n(", dist_km, " km)")),
               nudge_y = (lat_max - lat_min) * 0.04,
               size = 3.2, fontface = "bold",
               fill = "white", color = "#222", label.size = 0.3) +
    geom_point(aes(x = best$lon, y = best$lat),
               fill = "#fbbf24", color = "black",
               shape = 23, size = 5, stroke = 1.5) +
    annotate("label",
             x = best$lon,
             y = best$lat - (lat_max - lat_min) * 0.06,
             label = sprintf("Sign location\n(%.2f°, %.2f°)", best$lat, best$lon),
             size = 3.5, fontface = "bold",
             fill = "#fef9c3", color = "#92400e", label.size = 0.4)
  
  # --- Zoomed map ---
  zoom_pad <- max(tolerance / 80, 2.5)
  
  p_zoom <- ggplot() +
    geom_polygon(data = world_map,
                 aes(x = long, y = lat, group = group),
                 fill = "#e8e4dc", color = "#b5b1a9", linewidth = 0.3) +
    coord_fixed(1.3,
                xlim = c(best$lon - zoom_pad * 3, best$lon + zoom_pad * 3),
                ylim = c(best$lat - zoom_pad * 2, best$lat + zoom_pad * 2)) +
    theme_minimal(base_size = 12) +
    theme(
      panel.background = element_rect(fill = "#c8dff0", color = NA),
      panel.grid.major = element_line(color = "white", linewidth = 0.3),
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, color = "#555")
    ) +
    labs(
      title    = "Zoomed: Estimated Sign Location",
      subtitle = sprintf("Lat %.4f  /  Lon %.4f  |  Deviation ≤ %.1f km",
                         best$lat, best$lon, best$score),
      x = "Longitude", y = "Latitude"
    )
  
  for (i in seq_len(nrow(sign_data))) {
    col <- col_cycle[i]
    for (adj in c(-tolerance, 0, tolerance)) {
      ltype <- if (adj == 0) "solid" else "dotted"
      lwd   <- if (adj == 0) 1.1 else 0.5
      p_zoom <- p_zoom +
        geom_path(data = circle_pts(i, sign_data$dist_km[i] + adj),
                  aes(x = lon, y = lat),
                  color = col, linetype = ltype,
                  linewidth = lwd, alpha = 0.85)
    }
  }
  
  p_zoom <- p_zoom +
    geom_point(aes(x = best$lon, y = best$lat),
               fill = "#fbbf24", color = "black",
               shape = 23, size = 6, stroke = 1.5) +
    annotate("label",
             x = best$lon, y = best$lat + zoom_pad * 0.6,
             label = sprintf("%.4f°N / %.4f°E\n±%.0f km",
                             best$lat, best$lon, best$score),
             size = 3.5, fill = "#fef9c3", color = "#92400e",
             fontface = "bold", label.size = 0.4)
  
  print(p_world)
  print(p_zoom)
  
  # Return results invisibly
  invisible(list(
    sign_data = sign_data,
    location  = list(lat = best$lat, lon = best$lon, error_km = best$score),
    nearby    = nearby
  ))
}

# ======= SAMPLES

# sample 1
result <- sign_location_finder(
  cities    = c("New York", "Sitka", "Chicago", "Toronto", "Paris","London", "Oslo", "Toyko", "Cancun"),
  distances = c(1157, 3480, 1239, 1297, 4636, 4456, 4793, 7498, 473),
  tolerance = 50
)

# sample 2
result <- sign_location_finder(
   cities    = c("London", "New York", "Cairo", "Mumbai"),
   distances = c(1200, 5400, 3800, 6200),
   tolerance = 50
 )

# sample 3
result <- sign_location_finder(
   cities    = c("Paris", "Berlin", "Rome"),
   distances = c(850, 920, 1100),
   tolerance = 20
 )


# sample 3
# with the help of: https://www.distancefromto.net/  "air distance"
result <- sign_location_finder(
  cities    = c("Koper", "Celje", "Maribor", "Kranj"),
  distances = c(83, 61, 104, 24),
  tolerance = 20
)


 # Access results programmatically:
# result$location      → list(lat, lon, error_km)
# result$sign_data     → data frame with geocoded coords
# result$nearby        → data frame of nearby cities