library(tidyverse)
library(tidycensus)
library(haven)
library(here)
library(sf)
library(tigris)
library(urbnthemes)
library(Cairo)
library(patchwork)
library(urbnmapr)
library (tigris)
library(censusapi)
library(forcats)
library(gridExtra)
library(haven)
library(scales)
library(extrafont)
source("routing/analysis_functions.R")
set_urbn_defaults(style = "map")

## clarify number of open, year-round, weekly food sites

routes_transit <- read_csv(here("routing/data", "all_routes_transit_final.csv"),
                   col_types = c("geoid_start" = "character",
                                 "geoid_end" = "character",
                                 "date" = "character")) %>%
  mutate(mode = "TRANSIT")

routes_car <- read_csv(here("routing/data", "all_routes_car_final.csv"),
                           col_types = c("geoid_start" = "character",
                                         "geoid_end" = "character")) 

routes_car_rush <- routes_car %>%
  # calculate adjusted duration by multiplying car trips by the 
  # 2019 ratio of INRIX off-peak speed (32) and peak speed (18) for Washington DC
  # https://inrix.com/scorecard-city/?city=Washington%20DC&index=89
  mutate(adj_duration = adj_duration * 1.78,
         date = "2021-09-15") %>%
  select(-departure_time)

routes_car_wknd <- routes_car %>%
  mutate(date = "2021-09-19") %>%
  select(-departure_time)
  

routes <- rbind(routes_car_rush, routes_car_wknd, routes_transit) 

# routes_arl <- routes %>%
#   filter(substr(geoid_end, 1, 5) == "51013")

acs <- read_process_acs()
  

fi_raw <- read_csv("Raw FI/Food Insecurity Rates - Arlington County - MFI.csv")

fi_raw <- fi_raw %>%
  mutate(tract = str_replace(str_extract(geography, "\\d+\\.?\\d+"), "\\.", ""),
         GEOID = str_pad(paste0("51013", tract), side = "right", width = 11, pad = "0"))

 
routes_wide <- routes %>%
  select(geoid_end, geoid_start, adj_duration, mode, date) %>%
  #mutate(mode = factor(mode)) %>%
  pivot_wider(id_cols = c(geoid_start, geoid_end, date),
              names_from = mode, 
              values_from = adj_duration)

routes_self_wkdy <- tibble(
  geoid_start = acs$GEOID,
  geoid_end = acs$GEOID,
  CAR = 0,
  TRANSIT = 0,
  date = "2021-09-15"
  
)

routes_self_wknd <- tibble(
  geoid_start = acs$GEOID,
  geoid_end = acs$GEOID,
  CAR = 0,
  TRANSIT = 0,
  date = "2021-09-19"
  
)

routes_wide <- rbind(routes_wide, routes_self_wkdy, routes_self_wknd)

routes_acs <- routes_wide %>%
  left_join(acs, by = c("geoid_start" = "GEOID")) %>%
  mutate(CAR = as.numeric(CAR),
         TRANSIT = as.numeric(TRANSIT),
         wt_duration = CAR * (1 - pct_no_car/100) + TRANSIT * (pct_no_car/100),
         wt_duration_com = CAR * pct_car_commute/100 + TRANSIT * (1 - pct_car_commute/100))

food_sites <- read_csv(here("Final food data/Food site data", "Food_retailers_TRANSPORT.csv")) %>%
  #Exclude food sites that are available by appointment
  filter(is.na(frequency_visit) | frequency_visit != "Other frequency") %>%
  filter(!is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4269) %>%
  mutate(is_snap = case_when(location_type == "SNAP-retailer" ~ 1,
                             T ~ 0),
         is_charitable = case_when(location_type == "Charitable food-site" ~ 1,
                                   T ~ 0),
         char_open_all = case_when((restrictions == "Open to all" & 
                                      year_round == "Open year-round") ~ 1,
                                   T ~ 0),
         char_open_weekly = case_when((restrictions == "Open to all" & 
                                         frequency_visit == "Weekly or more frequent" &
                                         year_round == "Open year-round") ~ 1,
                                      T ~ 0),
         char_child_all = case_when(restrictions == "Serving children only" ~ 1,
                                    T ~ 0),
         char_child_weekly = case_when((restrictions == "Serving children only" &
                                          frequency_visit == "Weekly or more frequent") ~ 1,
                                       T ~ 0),
         char_sen_all = case_when(restrictions == "Serving elders only"~ 1,
                                  T ~ 0),
         char_sen_weekly = case_when((restrictions == "Serving elders only" &
                                        frequency_visit == "Weekly or more frequent") ~ 1,
                                     T ~ 0)
  )

va_tract <- tracts(state = "51")
# get road shapefle
road <- roads(state = "Virginia", county = "013")
tract_food <- st_join(va_tract, food_sites, join = st_intersects)

tract_food_count <- tract_food %>%
  group_by(GEOID) %>%
  summarise(across(c(starts_with("char"), is_snap, is_charitable),
               sum, na.rm = TRUE)
  )

# Number of tracts in Arlington County with a SNAP retailer in the tract
tract_food_count %>%
  filter(substr(GEOID, 1, 5) == "51013", is_snap > 0) %>%
  nrow()

# Number of tracts in Arlington County with a Charitable retailer in the tract
tract_food_count %>%
  filter(substr(GEOID, 1, 5) == "51013", is_charitable > 0) %>%
  nrow()

# Number of tracts in Arlington County with an Open Charitable retailer in the tract
tract_food_count %>%
  filter(substr(GEOID, 1, 5) == "51013", char_open_all > 0) %>%
  nrow()

routes_all <- routes_acs %>%
  left_join(tract_food_count, by = c("geoid_end" = "GEOID"))

write_csv(routes_all, here("routing/data", "routes_all.csv"))

routes_all <- read_csv(here("routing/data", "routes_all.csv"),
                       col_types = c("geoid_start" = "character",
                                      "geoid_end" = "character",
                                      "date" = "character"))

fi <- read_csv(here("Final food data/ACS and FI-MFI", 
                    "FI_ACS_data.csv"),
               col_types = c("geoid" = "character")) %>%
  select(geoid, percent_food_insecure)


mfi <- read_csv(here("Final food data/ACS and FI-MFI", 
                     "MFI_ACS_data.csv"),
                col_types = c("geoid" = "character")) %>%
  select(geoid, percent_mfi = percent_food_insecure)

all_fi <- fi %>%
  left_join(mfi, by = "geoid") %>%
  # set at in the top quartile of tracts
  mutate(is_high_fi = ifelse(percent_food_insecure > .12, 1, 0),
         is_high_mfi = ifelse(percent_mfi > .12, 1, 0)) 

route_date <- c("2021-09-15", "2021-09-19")
food_type <- c("is_snap", "is_charitable", "char_open_all", "char_open_weekly",
               "char_sen_all", "char_sen_weekly", "char_child_all", "char_child_weekly")
dur_type <- c("wt_duration", "wt_duration_com", "TRANSIT")

ttc_params <- expand_grid(
  route_date = route_date,
  food_type = food_type,
  dur_type = dur_type
)


all_ttc <- pmap_dfr(ttc_params, 
                    travel_time_to_closest, 
                    all_data = routes_all,
                    fi_data = all_fi)


num_no_access_snap_20 <- all_ttc %>%
  filter(food_type == "is_snap", min_duration > 20) %>%
  group_by(dur_type, route_date) %>%
  summarise(count = n())

num_no_access_snap_15 <- all_ttc %>%
  filter(food_type == "is_snap", min_duration > 15) %>%
  group_by(dur_type, route_date) %>%
  summarise(count = n())


high_need_low_access <- all_ttc %>%
  group_by(food_type, dur_type, route_date) %>%
  summarise(high_need_low_access_15 = sum(high_need_low_access_snap_15, na.rm = TRUE),
            high_need_low_access_20 = sum(high_need_low_access_snap_20, na.rm = TRUE))


low_access <- all_ttc %>%
  mutate(over_15 = ifelse(min_duration > 15, 1, 0),
         over_20 = ifelse(min_duration > 20, 1, 0)) %>%
  group_by(food_type, dur_type, route_date) %>%
  summarise(low_access_15 = sum(over_15, na.rm = TRUE),
            low_access_20 = sum(over_20, na.rm = TRUE))

cwt_params <- expand_grid(
  route_date = route_date,
  food_type = food_type,
  dur_type = dur_type,
  t = c(15, 20)
)

all_cwt <- pmap_dfr(cwt_params, 
                    count_accessible_within_t, 
                    all_data = routes_all,
                    fi_data = all_fi)


count_snap <- all_cwt %>%
  filter(food_type == "is_snap", 
         time == 20) %>%
  filter(!geoid_start %in% c("51013103401","51013980200", "51013980100")) %>%
  group_by(route_date, dur_type) %>%
  summarise(min_count = min(count, na.rm = TRUE),
            max_count = max(count, na.rm = TRUE))

# look for just transit
# look at number of retailers accessible

map_char_ttc <- map_time_to_closest(
  ttc = all_ttc %>% 
    filter(route_date == "2021-09-15", 
           food_type == "char_open_all",
           dur_type == "wt_duration_com"),
  county_shp = acs,
  opp = "Open Charitable Food Location",
  need_var = "is_high_fi",
  dur_type = "Weighted Travel Time",
  road = road)

map_char_ttc_wkly <- map_time_to_closest(
  ttc = all_ttc %>% 
    filter(route_date == "2021-09-15", 
           food_type == "char_open_weekly",
           dur_type == "wt_duration_com"),
  county_shp = acs,
  opp = "Weekly Open Charitable Food Location",
  need_var = "is_high_fi",
  dur_type = "Weighted Travel Time",
  road = road)

map_char_ttc_transit <- map_time_to_closest(
  ttc = all_ttc %>% 
    filter(route_date == "2021-09-15", 
           food_type == "char_open_all",
           dur_type == "TRANSIT"),
  county_shp = acs,
  opp = "Open Charitable Food Location",
  need_var = "is_high_fi",
  dur_type = "Transit",
  road = road)

map_char_ttc_wkly_transit <- map_time_to_closest(
  ttc = all_ttc %>% 
    filter(route_date == "2021-09-15", 
           food_type == "char_open_weekly",
           dur_type == "TRANSIT"),
  county_shp = acs,
  opp = "Weekly Open Charitable Food Location",
  need_var = "is_high_fi",
  dur_type = "Transit",
  road = road)

map_char_ttc_all_transit <- map_time_to_closest(
  ttc = all_ttc %>% 
    filter(route_date == "2021-09-15", 
           food_type == "is_charitable",
           dur_type == "TRANSIT"),
  county_shp = acs,
  opp = "Charitable Food Location",
  need_var = "is_high_fi",
  dur_type = "Transit",
  road = road)

map_snap_ttc <- map_time_to_closest(
  ttc = all_ttc %>% 
    filter(route_date == "2021-09-15", 
           food_type == "is_snap",
           dur_type == "TRANSIT"),
  county_shp = acs,
  opp = "SNAP Retailer",
  need_var = "is_high_fi",
  dur_type = "Transit",
  road = road)

#map_snap_com <- map_time_to_closest(acs, snap_wkdy_com, "SNAP Retailer")

#map_char <- map_time_to_closest(acs, char_open_wkdy, "Open Charitable Food Site")

map_count_snap <- map_count_within_t(
  count_within_t = all_cwt %>%
    filter(route_date == "2021-09-15", 
           food_type == "is_snap",
           dur_type == "wt_duration_com",
           time == 20),
  county_shp = acs,
  opp = "SNAP Retailers",
  need_var = "is_high_fi",
  dur_type = "Weighted Travel Time")

map_count_snap_transit <- map_count_within_t(
  count_within_t = all_cwt %>%
    filter(route_date == "2021-09-15", 
           food_type == "is_snap",
           dur_type == "TRANSIT",
           time == 20),
  county_shp = acs,
  opp = "SNAP Retailers",
  need_var = "is_high_fi",
  dur_type = "Transit")


map_count_char_open <- map_count_within_t(
  count_within_t = all_cwt %>%
    filter(route_date == "2021-09-15", 
           food_type == "char_open_all",
           dur_type == "wt_duration_com",
           time == 20),
  county_shp = acs,
  opp = "Open Charitable Food Locations",
  need_var = "is_high_fi",
  dur_type = "Weighted Travel Time")

map_count_char_open_transit <- map_count_within_t(
  count_within_t = all_cwt %>%
    filter(route_date == "2021-09-15", 
           food_type == "char_open_all",
           dur_type == "TRANSIT",
           time == 20),
  county_shp = acs,
  opp = "Open Charitable Food Locations",
  need_var = "is_high_fi",
  dur_type = "Transit")