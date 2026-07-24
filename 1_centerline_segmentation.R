##------------------Centerline segmentation and generate perpendidcular lines------------##
library(here)
library(tidyverse)
library(sf)

##-----------------project CRS
project_crs = 5174

# Buffer Seoul boundary by 500m
seoul_500m = st_read(here('input/Seoul.gpkg'))
seoul_500m = st_buffer(seoul_500m, 500)
st_crs(seoul_500m)

# import water centerlines(already clipped and reprojected)
water_center = st_read('directory to waterway centerline gpkg data', layer = 'reprojected')
st_crs(water_center)

# import water polygon, clip and buffer by -15m
water_polygon = st_read ("directory to polygon water data")
water_polygon = st_transform(water_polygon, crs = project_crs)
seoul_water_polygon = st_intersection(water_center, seoul_500m)
st_crs(seoul_water_polygon)
seoul_water_polygon_30m = st_buffer(seoul_water_polygon, -10)

# Clip centerline with 30m polygon to extract those that are more than 30m width
seoul_30m_water_center = st_filter(water_center, seoul_water_polygon_30m, .predicate = st_intersects)
seoul_30m_water_center = seoul_30m_water_center |> 
  mutate('total_length' = st_length(geom))

# Exclude those that are in forest areas
landuse = st_read("directory to OSM Land use data")
landuse = st_transform(landuse, crs = project_crs)
landuse_seoul = st_intersection(landuse, seoul_500m)
factor(landuse_seoul$fclass) |> levels()

not_urban = c('forest', 'farmland', 'heath')
forest_seoul = landuse_seoul |> 
  filter(fclass %in% not_urban)
st_crs(forest)

water_inside_forest = st_intersection(seoul_30m_water_center, forest_seoul)
water_inside_forest = water_inside_forest |> 
  mutate('length_intersect' = st_length(geom)) |> 
  mutate('forest_ratio' = as.numeric(length_intersect/total_length))

water_30m_urban = left_join(
  seoul_30m_water_center, 
  st_drop_geometry(water_inside_forest) |> 
    select('NF_ID', 'forest_ratio'), by = 'NF_ID' )

water_30m_urban$forest_ratio[is.na(water_30m_urban$forest_ratio)] = 0
water_30m_urban = water_30m_urban |> 
  filter(forest_ratio<0.5)

# Check 
ggplot() +
  geom_sf(data = seoul_water_polygon, fill = 'cyan', color = 'cyan') +
  # geom_sf(data = seoul_water_polygon_30m, fill = 'blue', color = 'blue') +
  geom_sf(data = forest_seoul, fill = 'green') +
  geom_sf(data = seoul_30m_water_center, size = 2, color = 'yellow') +
  # geom_sf(data = water_30m_urban, size = 1, color = 'red') +
  geom_sf(data = stream_30m_urban, size = 1, color = 'red') +
  coord_sf(datum = st_crs(5174))

# Trim parts that intersect with Hangang

hangang = st_read('directory to waterway centerline data', layer = 'hangang')
hangang = st_transform(hangang, crs = project_crs)
st_crs(hangang)
hangang = st_union(hangang)

stream_30m_urban = st_difference(water_30m_urban, hangang)


## export to file
st_write(stream_30m_urban, 'desired output directory', overwrite = TRUE)










