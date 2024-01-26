# 0. Load libraries and define functions ----
# *******************************************
library(tidyverse)
library(readxl)
library(osrm)
library(sf)
library(parallel)
library(leaflet)

# Set OSRM options
options(osrm.server = 'http://router.project-osrm.org/') 
options(osrm.profile = 'car') 

# Create the route function using osrm
fnCreateRoute <- function(x){
  osrmRoute(src = unname(as.numeric(x[c('src_lng', 'src_lat')])),
            dst = unname(as.numeric(x[c('dst_lng', 'dst_lat')])),
            # Return just distance and duration (not the actual route)
            overview = FALSE)
}

# 1. Load data ----
# *****************

# Load the LSOA 2021 shapefile
sf_lsoa21 <- st_read(dsn = './data/lsoa21', layer = 'lsoa21') %>%
  # Transform to WGS84 coordinate system
  st_transform(crs = 4326) %>% 
  # Filter to just English LSOAs (removing Welsh LSOAs)
  filter(grepl('^E', LSOA21CD))

# Load the LSOA 2021 population weighted centroids
df_lsoa21 <- read.csv('./data/lsoa21/lsoa21_pwc.csv') %>%
  rename_with(.fn = ~c('id','lsoa21cd', 'global_id', 'easting', 'northing')) %>%
  # Drop the unused columns
  select(-c('id', 'global_id')) %>%
  mutate(x = easting, y = northing) %>%
  # Convert to geographical point OSGB36 (easting and northing)...
  st_as_sf(coords = c('x','y'), crs = 27700) %>%
  # ... and then convert to WGS84 (longitude and latitude)
  st_transform(crs = 4326) %>%
  mutate(longitude = unname(st_coordinates(.)[,1]),
         latitude = unname(st_coordinates(.)[,2])) %>%
  # and remove the geometry
  st_drop_geometry()

# Load the Emergency Department site data
df_ed_site <- read.csv('./data/ed_locations.csv')

# Load the LSOA 2021 to NHS England Region lookup
df_lsoa21_nhser21 <- read.csv('./data/OA21_LSOA21_MSOA21_ICB22_LAD22_EW_LU.csv') %>%
  distinct(lsoa21cd, ICB22CDH, RGN22CD, RGN22NM) %>%
  rename_with(.fn = ~c('lsoa21cd', 'icb22cdh', 'rgn22cd', 'rgn22nm'))

# 2. Create source to destination matrix ----
# *******************************************

df_matrix <- df_lsoa21 %>% 
  mutate(src_easting = easting, src_northing = northing,
         src_lng = longitude, src_lat = latitude,
         .keep = 'unused') %>% 
  expand_grid(df_ed_site %>% 
                mutate(dst_easting = easting, dst_northing = northing,
                       dst_lng = longitude, dst_lat = latitude,
                       .keep = 'unused')) %>%
  mutate(line_distance = sqrt(((dst_easting - src_easting)^2) + ((dst_northing - src_northing)^2))) %>%
  mutate(line_distance_km = line_distance / 1000) %>%
  arrange(line_distance) %>%
  select(lsoa21cd, src_lng, src_lat, 
         code, name, postcode, dst_lng, dst_lat,
         line_distance, line_distance_km)
  
# 3. Simplify matrix ----
# ***********************
df_matrix <- df_matrix %>% filter(line_distance_km <= 100)

# 4. Calculate travel distance and time ----
# ******************************************
# Create the clusters for parallelisation
n_cores <- detectCores()
# Leave one free
clust <- makeCluster(n_cores - 1)
# Export the route creation function
clusterExport(clust, c('fnCreateRoute','osrmRoute'))
# Get the routes
dtStart <- Sys.time()
res_travel_time_and_distance <- parApply(clust, X = df_matrix, MARGIN = 1, FUN = fnCreateRoute)
Sys.time() - dtStart
stopCluster(clust)

# Add the dutration and distance to the matrix
df_matrix$duration_min <- res_travel_time_and_distance[1, ]
df_matrix$distance_km <- res_travel_time_and_distance[2, ]

# 5. Summarise data ----
# ***********************

plurality_distance = 30
plurality_duration = 30

df_summary <- df_matrix %>% 
  group_by(lsoa21cd) %>% 
  summarise(
    promixity_dis = min(distance_km, na.rm = TRUE),
    promixity_dur = min(duration_min, na.rm = TRUE),
    plurality_dis = sum(ifelse(distance_km <= plurality_distance, 1, 0)),
    plurality_dur = sum(ifelse(distance_km <= plurality_duration, 1, 0)),
    .groups = 'keep'
  ) %>%
  ungroup()

# 6. Output summary data ----
# ***************************


dir.create('./output', showWarnings = FALSE, recursive = TRUE)
write.csv(df_matrix, './output/matrix.csv', row.names = FALSE)

# 7. Visualise data ----
# **********************




