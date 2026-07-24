##---------------------Waterfront definition------------------------##

library(here)
library(tidyverse)
library(sf)
library(dplyr)
library(lwgeom)
library(purrr)
here()
project_crs = 5174

##------------------------Define intersection points and Create Perpendicular lines--------------------- ##

# join lines
centerlines = st_read('output/stream_segmentation_centerline.gpkg', layer = 'stream_segmentation_centerline' )
snapped_centerlines = st_snap(centerlines, centerlines, tolerance = 20)
merged_centerlines = snapped_centerlines |> 
  st_union() |> 
  st_line_merge()

st_geometry_type(merged_centerlines)
str(merged_centerlines)

## extract points
singleparts = st_cast(merged_centerlines, 'LINESTRING')

st_write(singleparts, 'output/stream_segmentation_centerline.gpkg', driver = 'GPKG', layer = 'merged', delete_layer = TRUE)
st_geometry_type(singleparts)

    ## --------------the merged layer must be manually modified and ordered ---------------##

ggplot() +
  geom_sf(data = merged_centerlines, color = 'red') +
  geom_sf(data = all_pts, color = 'blue') +
  coord_sf(crs = project_crs)

    # categorize streams
ordered_lines = st_read('output/stream_segmentation_centerline.gpkg', layer = 'merged_ordered')
first_order = ordered_lines |> 
  filter(order == 1) |> 
  rowid_to_column('stream_id')
second_order = ordered_lines |> 
  filter(order ==2) |> 
  rowid_to_column('stream_id')
third_order = ordered_lines |> 
  filter(order ==3) |> 
  rowid_to_column('stream_id')

st_geometry_type(first_order)
st_is_longlat(first_order)
  
##------------function for generating perpendicular lines----------

generate_perp_lines_start <- function(lns, L = 1000, eps = 0.001) {
  # lns : sf object (LINESTRING or MULTILINESTRING)
  # L   : total perpendicular length (CRS units)
  # eps : small fraction of line length to compute tangent direction

  # Storage list
  perp_list <- list()
  
  for (i in seq_len(nrow(lns))) {
    
    ln <- st_geometry(lns[i, ])
    
    # ---- START POINT ----
    p_start  <- st_line_sample(ln, sample = 0)
    p_near_s <- st_line_sample(ln, sample = eps)

    # Convert to coordinates
    cs  <- st_coordinates(p_start)
    cs2 <- st_coordinates(p_near_s)

    # Tangent vectors
    dx_s <- cs2[1] - cs[1]
    dy_s <- cs2[2] - cs[2]

    # Perpendicular vectors
    perp_s <- c(-dy_s, dx_s)

    # Normalize
    perp_s <- (perp_s / sqrt(sum(perp_s^2))) * (L / 2)

    # Build perpendicular lines
    start_line <- st_linestring(rbind(
      c(cs[1] + perp_s[1], cs[2] + perp_s[2]),
      c(cs[1] - perp_s[1], cs[2] - perp_s[2])
    ))
    
    perp_list[[length(perp_list) + 1]] <- start_line
  }
  
  st_sf(
    geometry = st_sfc(perp_list, crs = st_crs(lns))
  )
}

generate_perp_lines_end <- function(lns, L = 1000, eps = 0.001) {
  # lns : sf object (LINESTRING or MULTILINESTRING)
  # L   : total perpendicular length (CRS units)
  # eps : small fraction of line length to compute tangent direction
  
  # Storage list
  perp_list <- list()
  
  for (i in seq_len(nrow(lns))) {
    
    ln <- st_geometry(lns[i, ])
    
    # ---- END POINT ----
    p_end    <- st_line_sample(ln, sample = 1)
    p_near_e <- st_line_sample(ln, sample = 1 - eps)
    
    # Convert to coordinates
    ce  <- st_coordinates(p_end)
    ce2 <- st_coordinates(p_near_e)
    
    # Tangent vectors
    dx_e <- ce[1] - ce2[1]
    dy_e <- ce[2] - ce2[2]
    
    # Perpendicular vectors
    perp_e <- c(-dy_e, dx_e)
    
    # Normalize
    perp_e <- (perp_e / sqrt(sum(perp_e^2))) * (L / 2)
    
    # Build perpendicular lines
    end_line <- st_linestring(rbind(
     c(ce[1] + perp_e[1], ce[2] + perp_e[2]),
     c(ce[1] - perp_e[1], ce[2] - perp_e[2])
    ))
    
    perp_list[[length(perp_list) + 1]] <- end_line
  }
  
  st_sf(
    geometry = st_sfc(perp_list, crs = st_crs(lns))
  )
}

generate_perp_lines_both <- function(lns, L = 1000, eps = 0.001) {
  # lns : sf object (LINESTRING or MULTILINESTRING)
  # L   : total perpendicular length (CRS units)
  # eps : small fraction of line length to compute tangent direction
  
  # Storage list
  perp_list <- list()
  
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
  }
  
  st_sf(
    geometry = st_sfc(perp_list, crs = st_crs(lns))
  )
}

##-----------generate perpendicular lines for each order --------------##
first_perp_lines_bt = generate_perp_lines_start(first_order|>
                                               filter(type %in% c('between', 'upstream', 'downstream'))) |>
  mutate(order = 1)
first_perp_lines_end = generate_perp_lines_end(first_order|>
                                                  filter(type == 'downstream')) |>
  mutate(order = 1)
first_perp_lines_solo = generate_perp_lines_both(first_order|>
                                                  filter(type == 'solo')) |>
  mutate(order = 1)
first_perp_lines = rbind(first_perp_lines_bt, first_perp_lines_end, first_perp_lines_solo)


second_perp_lines_bt = generate_perp_lines_start(second_order |>
                                          filter(type %in% c('between', 'upstream', 'downstream'))) |> 
  mutate(order =2)
second_perp_lines_end = generate_perp_lines_end(second_order |>
                                             filter(type == 'downstream')) |> 
  mutate(order =2)
second_perp_lines_solo = generate_perp_lines_both(second_order |>
                                             filter(type == 'solo')) |> 
  mutate(order =2)
second_perp_lines = rbind(second_perp_lines_bt, second_perp_lines_end, second_perp_lines_solo)


third_perp_lines_bt = generate_perp_lines_start(third_order |>
                                                   filter(type %in% c('between', 'upstream', 'downstream'))) |> 
  mutate(order =2)
third_perp_lines_end = generate_perp_lines_end(third_order |>
                                                  filter(type == 'downstream')) |> 
  mutate(order =2)
third_perp_lines_solo = generate_perp_lines_both(third_order |>
                                                    filter(type == 'solo')) |> 
  mutate(order =2)
third_perp_lines = rbind(third_perp_lines_bt, third_perp_lines_end, third_perp_lines_solo)


st_write(first_perp_lines, 'output/perpendicular_lines.gpkg', layer = 'first_order_1km', overwrite = TRUE)
st_write(second_perp_lines, 'output/perpendicular_lines.gpkg', layer = 'second_order_500', overwrite = TRUE)
st_write(third_perp_lines, 'output/perpendicular_lines.gpkg', layer = 'third_order')

# check
ggplot() +
  geom_sf(data = ordered_lines, color = 'black') +
  geom_sf(data = first_perp_lines, color = 'red') +
  geom_sf(data = second_perp_lines, color = 'blue') +
  geom_sf(data = third_perp_lines, color='green') +
  coord_sf(datum = project_crs)
  
##------------------------------------Extract road boundaries --------------------------##
# Buffer Seoul boundary by 500m
seoul_500m = st_read(here('input/Seoul.gpkg'))
seoul_500m = st_buffer(seoul_500m, 500)
st_crs(seoul_500m)

# Import Seoul and Kyeongki road polygons and reset crs

kyeongki_roads = st_read("directory to road polygon data")
kyeongki_roads = st_transform(kyeongki_roads, crs = project_crs)
seoul_roads = st_read("directory to road polygon data")
seoul_roads = st_transform(seoul_roads, crs = project_crs)
st_crs(seoul_roads)

# Clip Gyeong-gi road with buffered Seoul boundary and join with Seoul

cut_kyeongki_roads = st_intersection(kyeongki_roads, seoul_500m |> select(geom))
str(cut_kyeongki_roads)
roads_total = rbind(seoul_roads, cut_kyeongki_roads)

ggplot() +
  geom_sf(data = seoul_roads, fill = 'cyan', color = 'cyan')+
  geom_sf(data = cut_kyeongki_roads, fill = 'magenta', color = 'magenta')+
  coord_sf(datum = project_crs)
  
# Buffer streams according to order : first -> 250m, second ->60m, third -> don't apply 
# extract roads that intersect with the buffered area

first_buffer = st_buffer(first_order, 250)
second_buffer = st_buffer(second_order, 60)  

first_outline_roads = roads_total |> 
  st_filter(first_buffer, .predicate = st_intersects) |> 
second_outline_roads = roads_total |> 
  st_filter(second_buffer, .predicate = st_intersects)

st_write(first_outline_roads, 'output/first_boundaries.gpkg', overwrite = TRUE)
st_write(second_outline_roads, 'output/second_boundaries.gpkg', overwrite = TRUE)


## ------------------ Create waterfront polygons -------------------------##
f_waterfront_boundary_long = st_read('input/water_boundary_lines.gpkg', layer = 'first')
s_waterfront_boundary_long = st_read('input/water_boundary_lines.gpkg', layer = 'second')

f_waterfront_boundary_long = f_waterfront_boundary_long |>
  rename(geometry = geom)

# Close the long lines to make polygons first and split with perpendicular lines

close_lines <- function(sf_lines, name_col){
  
  sf_lines |>
    group_by({{name_col}}) |>
    summarise(
      geometry = {
        
        lines <- geometry
        
        c1 = st_coordinates(lines[[1]])[,1:2, drop = FALSE]
        c2 = st_coordinates(lines[[2]])[,1:2, drop = FALSE]
        
        c2_rev = c2[rev(seq_len(nrow(c2))), , drop = FALSE]
        
        ring = rbind(c1, c2_rev, c1[1,])
        
        st_sfc(st_polygon(list(ring)), crs = st_crs(sf_lines))
      },
      .groups = "drop"
    )
}

f_lines_all = close_lines(f_waterfront_boundary_long, name_col = name)
f_waterfronts = st_split(f_lines_all, first_perp_lines)
f_waterfronts = st_collection_extract(f_waterfronts)

# give each polygon id
f_waterfronts = st_filter(f_waterfronts, first_order, .predicate = st_intersects)|>
  group_by(name) |>
  mutate(id = paste0(name, '_', row_number()))|>
  ungroup()
  


ggplot()+
  geom_sf(data = f_waterfronts, fill = 'cyan', color = 'red') +
  geom_sf(data = first_perp_lines, color = 'magenta') +
  coord_sf(datum = project_crs)

st_write(f_waterfronts, 'output/first_waterfront_definition.gpkg', layer = 'first', overwrite = TRUE)

















