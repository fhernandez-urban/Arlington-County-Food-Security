library(tidyverse)
library(here)
library(sf)
library(tigris)
library(urbnmapr)
library(rjson)

#https://mcdc.missouri.edu/applications/geocorr2014.html
#run for DC, MD, VA - select tracts as source and target geographies
centroids_all <- read_csv(here("routing/data", "geocorr_centroids.csv")) %>%
  filter(tract != "Tract") %>%
  st_as_sf(coords = c("intptlon", "intptlat"), crs = 4269, remove = FALSE) %>%
  mutate(tract_str = str_replace(tract, "[.]", ""),
    geoid = paste0(county, tract_str)) %>%
  rename(c("lon" = "intptlon", "lat" = "intptlat")) %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon))

# Route to all tracts in arlington, alexandria, and fairfax
centroids_food <- centroids_all %>%
  filter(cntyname %in% c("Arlington VA", "Alexandria city VA", "Fairfax VA"))

# Identify valid starting tracts in arlington county
centroids_arl <- centroids_all %>%
  # exclude two tracts with zero population from analysis and 
  # tract inside Joint Base Myer-Henderson Hall as OTP fails to find any routes 
  # due to lack of public access
  filter(cntyname == "Arlington VA" & !(tract %in% c("9801.00", "9802.00", "1034.01")))

# Create pairs of routes starting in Arlington where end centroid within 15 miles
route_pairs <- st_join(centroids_arl,
                       centroids_food, 
                       join = st_is_within_distance,
                       #15 miles is 24,140 meters
                       dist = 25000,
                       suffix = c("_start", "_end"))

route_out <- route_pairs 
st_geometry(route_out) <- NULL
write_csv(route_out, here("routing/data", "route_pairs.csv"))

#Create json with bounds for clipping osm
end_point <- centroids_all %>% 
  filter(geoid %in% unique(route_pairs$geoid_end))

bbox <- st_bbox(end_point)

bounding <- list(
  n = bbox[["ymax"]] + 0.25,
  s = bbox[["ymin"]] - 0.25,
  e = bbox[["xmin"]] - 0.25,
  w = bbox[["xmax"]] + 0.25
)

json_bounding <- toJSON(bounding)
write(json_bounding, here("routing/data", "osm_bounds.json"))
