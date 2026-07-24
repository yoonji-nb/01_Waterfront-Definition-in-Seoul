library(tidyverse)
library(sf)
library(dplyr)
library(lwgeom)
library(purrr)
library(units)
project_crs = 5174

##-------------- Buffer parks and exclude hangang area

hangang_boundaries = st_read('input/hangang_boundaries.gpkg', layer = 'lines')
hangang_boundaries = st_cast(hangang_boundaries, "LINESTRING")
hangang_boundaries = hangang_boundaries |>
  mutate(id = row_number())

offseted = st_buffer(hangang_boundaries, 1000, joinStyle = 'MITRE') |>
  mutate(id = hangang_boundaries$id)|>
  rename(geometry = geom)

generate_perp_lines_both <- function(lns, L = 3000, eps = 0.001) {
  # lns : sf object (LINESTRING or MULTILINESTRING)
  # L   : total perpendicular length (CRS units)
  # eps : small fraction of line length to compute tangent direction
  
  # Storage list
  perp_list <- list()
  id_list <- c()
  
  for (i in seq_len(nrow(lns))) {
    
    ln <- st_geometry(lns[i, ])
    
    # ---- START POINT ----
    p_start  <- st_line_sample(ln, sample = 0)
    p_near_s <- st_line_sample(ln, sample = eps)
    
    # ---- END POINT ----
    p_end    <- st_line_sample(ln, sample = 1)
    p_near_e <- st_line_sample(ln, sample = 1 - eps)
    
    # Convert to coordinates
    cs  <- st_coordinates(p_start)
    cs2 <- st_coordinates(p_near_s)
    ce  <- st_coordinates(p_end)
    ce2 <- st_coordinates(p_near_e)
    
    # Tangent vectors
    dx_s <- cs2[1] - cs[1]
    dy_s <- cs2[2] - cs[2]
    
    dx_e <- ce[1] - ce2[1]
    dy_e <- ce[2] - ce2[2]
    
    # Perpendicular vectors
    perp_s <- c(-dy_s, dx_s)
    perp_e <- c(-dy_e, dx_e)
    
    # Normalize
    perp_s <- (perp_s / sqrt(sum(perp_s^2))) * (L / 2)
    perp_e <- (perp_e / sqrt(sum(perp_e^2))) * (L / 2)
    
    # Build perpendicular lines
    start_line <- st_linestring(rbind(
      c(cs[1] + perp_s[1], cs[2] + perp_s[2]),
      c(cs[1] - perp_s[1], cs[2] - perp_s[2])
    ))
    
    end_line <- st_linestring(rbind(
      c(ce[1] + perp_e[1], ce[2] + perp_e[2]),
      c(ce[1] - perp_e[1], ce[2] - perp_e[2])
    ))
    
    perp_list[[length(perp_list) + 1]] <- start_line
    perp_list[[length(perp_list) + 1]] <- end_line
    
    # store IDs (twice because start + end)
    id_list <- c(id_list, i, i)
  }
  
  

st_sf(
  id = id_list,
  geometry = st_sfc(perp_list, crs = st_crs(lns))
)
}

perp_lines = generate_perp_lines_both(hangang_boundaries)

## split buffer polygons according to id

split_list <- list()

for (i in unique(offseted$id)) {
  
  poly <- offseted[offseted$id == i, ]
  p_cutters <- perp_lines[perp_lines$id == i, ]
  
  if (nrow(p_cutters) > 0) {
    
    split_geom <- st_split(poly, st_union(p_cutters))
    
    split_list[[as.character(i)]] <-
      st_collection_extract(split_geom, "POLYGON") |>
      st_as_sf(id = i)
    
  } else {
    
    split_list[[as.character(i)]] <- poly
    
  }
}

split_buffers <- do.call(rbind, split_list)

## extend lines
extend_lines <- function(lines, d = 1000){
  
  geoms <- st_geometry(lines)
  
  new_geoms <- lapply(geoms, function(g){
    
    coords <- st_coordinates(g)
    
    # start and near-start
    p_start <- coords[1, 1:2]
    p_next  <- coords[2, 1:2]
    
    # end and near-end
    p_end   <- coords[nrow(coords), 1:2]
    p_prev  <- coords[nrow(coords)-1, 1:2]
    
    # tangent directions
    dir_start <- p_start - p_next
    dir_end   <- p_end - p_prev
    
    dir_start <- dir_start / sqrt(sum(dir_start^2))
    dir_end   <- dir_end / sqrt(sum(dir_end^2))
    
    new_start <- p_start + dir_start * d
    new_end   <- p_end + dir_end * d
    
    st_linestring(rbind(new_start, coords[,1:2], new_end))
  })
  
  st_sf(
    lines,
    geometry = st_sfc(new_geoms, crs = st_crs(lines))
  )
}

extended_boundaries = extend_lines(hangang_boundaries, d = 500)



# again slit the polygons 

split_list_two <- list()

for (i in unique(split_buffers$id)) {
  
  poly <- split_buffers[split_buffers$id == i, ]
  c_cutters = extended_boundaries[extended_boundaries$id == i, ]
  
  if (nrow(c_cutters) > 0) {
    
    split_geom_two <- st_split(poly, st_union(c_cutters))
    
    split_list_two[[as.character(i)]] <-
      st_collection_extract(split_geom_two, "POLYGON") |>
      st_as_sf(id = i)
    
  } else {
    
    split_list_two[[as.character(i)]] <- poly
    
  }
}

split_buffers_two = do.call(rbind, split_list_two)
split_buffers_two = split_buffers_two |>
  mutate(new_id = paste0('id', row_number()))

## delete those that intersect with the river

hangang = st_read('input/water_centerlines.gpkg', layer = 'hangang')
hangang = hangang |>
  st_transform(crs = project_crs)

intersections = st_intersection(split_buffers_two, hangang)|>
  mutate(area = st_area(intersections)) |>
  group_by(new_id) |>
  summarize(intersect_area = sum(area))

split_buffers_two =  split_buffers_two |>
  left_join(
    intersections |> 
      st_set_geometry(NULL) |>
    select(new_id, intersect_area), 
    by = 'new_id')

filtered = split_buffers_two |>
  filter(intersect_area < set_units(500000, 'm^2'))

st_write(split_buffers_two, 'output/hangang_buffer_1km.gpkg', overwrite = TRUE)

ggplot()+
  geom_sf(data = split_buffers_two, color = 'blue') +
  coord_sf(datum = project_crs)
