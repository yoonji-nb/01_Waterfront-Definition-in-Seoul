library(here)
library(tidyverse)
library(sf)
library(dplyr)
library(lwgeom)
library(purrr)
library(polyclip)

project_crs = 5174

ordered_lines = st_read('output/stream_segmentation_centerline.gpkg', layer = 'merged_ordered')
f_stream = ordered_lines |> 
  filter(order == 1) |> 
  rowid_to_column('stream_id')
f_boundaries = st_read('input/water_boundary_lines.gpkg', layer = 'first')
f_boundaries = f_boundaries |>
  rename(geometry = geom)
f_perp_lines = st_read('output/perpendicular_lines.gpkg', layer = 'first_order')
f_waterfronts = st_read('output/first_waterfront_definition.gpkg')
f_waterfronts = f_waterfronts|>
  rename(geometry = geom)

f_polygon = f_waterfronts|>
  group_by(name)|>
  summarize(geometry = st_union(geometry), .groups = 'drop')

f_buffered_1km = st_buffer(f_polygon, 1000, joinStyle = 'MITRE')
f_buffered_600m = st_buffer(f_polygon, 600, joinStyle = 'MITRE')

f_polygon = f_polygon |> arrange(name)
f_buffered_600m   = f_buffered_600m |> arrange(name)
f_buffered_1km   = f_buffered_1km |> arrange(name)

f_org_union = st_union(f_polygon)
f_donut_600 = st_difference(f_buffered_600m, f_org_union)
f_donut_1km = st_difference(f_buffered_1km, f_org_union)

f_buffered_600m_split = st_split(f_donut_600, f_perp_lines)
f_buffered_600m_split = st_collection_extract(f_buffered_600m_split)
f_buffered_600m_split = f_buffered_600m_split |> 
  mutate(id = row_number())

f_buffered_1km_split = st_split(f_donut_1km, f_perp_lines)
f_buffered_1km_split = st_collection_extract(f_buffered_1km_split)
f_buffered_1km_split = f_buffered_1km_split |> 
  mutate(id = row_number())

# keep objects that intersect in lines with the straem

intersections_600m = st_intersection(f_buffered_600m_split, f_stream)
intersections_1km = st_intersection(f_buffered_1km_split, f_stream)

# keep only line-type intersections
line_intersections_600m = intersections_600m |> 
  filter(st_geometry_type(.) %in% c("LINESTRING", "MULTILINESTRING"))
line_intersections_1km = intersections_1km |> 
  filter(st_geometry_type(.) %in% c("LINESTRING", "MULTILINESTRING"))

# get unique polygon IDs
valid_ids_600m <- unique(line_intersections_600m$id)
valid_ids_1km <- unique(line_intersections_1km$id)

# filter original polygons
f_buffers_600m = f_buffered_600m_split %>%
  filter(id %in% valid_ids_600m)
f_buffers_1km = f_buffered_1km_split %>%
  filter(id %in% valid_ids_1km)

st_write(f_buffers_600m, 'output/first_buffers_600m_new.gpkg', overwrite = TRUE)
st_write(f_buffers_1km, 'output/first_buffers_1km_new.gpkg', overwrite = TRUE)


###------------------assign waterfront id to buffered areas

buffers = st_read('output/first_buffers_600m.gpkg')
buffers = buffers |>
  rename(geometry = geom)

# 1. Compute intersection areas matrix (neighbors × originals)
overlaps <- st_intersects(buffers, f_waterfronts)  # list of indices each neighbor touches

# Initialize vector to store assigned names
assigned_names <- vector("character", length = nrow(buffers))

for (i in seq_len(nrow(buffers))) {
  neighbor <- buffers[i, ]
  touching <- overlaps[[i]]  # indices of originals touching this neighbor
  
  if(length(touching) == 0) {
    assigned_names[i] <- NA_character_  # no touching polygon
  } else if(length(touching) == 1) {
    assigned_names[i] <- f_waterfronts$id[touching]  # only one match
  } else {
    # multiple originals touched: pick the one with largest intersection area
    areas <- sapply(touching, function(j) {
      st_length(st_intersection(neighbor, f_waterfronts[j, ]))
    })
    assigned_names[i] <- f_waterfronts$id[touching[which.max(areas)]]
  }
}

buffers$id <- assigned_names

st_write(buffers, 'output/first_buffers_600_named.gpkg', overwrite = TRUE)

ggplot()+
  geom_sf(data= f_donut) +
  geom_sf(data = f_buffers)+
  coord_sf(datum = project_crs)

