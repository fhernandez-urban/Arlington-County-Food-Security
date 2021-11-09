library(tidyverse)
library(tidycensus)
library(haven)
library(here)
library(sf)
library(tigris)
library(urbnthemes)
source("routing/analysis_functions.R")

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

food_sites <- read_csv(here("Final food data", "Food_retailers_TRANSPORT.csv")) %>%
  #Exclude food sites that are available by appointment
  filter(frequency_visits != "By appointment/upon request or other")
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4269) 

va_tract <- tracts(state = "51")
tract_food <- st_join(va_tract, food_sites, join = st_intersects)

tract_food <- tract_food %>%
  mutate(is_snap = case_when(location_type == "SNAP-retailer" ~ 1,
                             T ~ 0),
         is_charitable = case_when(location_type == "Charitable food-site" ~ 1,
                                   T ~ 0),
         char_open_all = case_when(elig_type == "No age restriction" ~ 1,
                               T ~ 0),
         char_open_weekly = case_when((elig_type == "No age restriction" & 
                                         frequency_visits %in% c("More than once a week", 
                                                                 "Weekly")) ~ 1,
                                   T ~ 0),
         char_open_mult_per_month = case_when((elig_type == "No age restriction" & 
                                         frequency_visits %in% c("More than once a month, less than weekly",
                                                                 "More than once a week", 
                                                                 "Weekly")) ~ 1,
                                      T ~ 0),
         char_child_all = case_when(elig_type == "Open to children only" ~ 1,
                                T ~ 0),
         char_child_weekly = case_when((elig_type == "Open to children only" &
                                          frequency_visits %in% c("More than once a week", 
                                                                  "Weekly")) ~ 1,
                                    T ~ 0),
         char_child_mult_per_month = case_when((elig_type == "Open to children only" & 
                                                  frequency_visits %in% c("More than once a month, less than weekly",
                                                                          "More than once a week", 
                                                                          "Weekly")) ~ 1,
                                    T ~ 0),
         char_sen_all = case_when(elig_type == "Open to seniors only" ~ 1,
                              T ~ 0),
         char_sen_weekly = case_when((elig_type == "Open to seniors only" &
                                        frequency_visits %in% c("More than once a week", 
                                                                "Weekly")) ~ 1,
                                  T ~ 0),
         char_sen_mult_per_month = case_when((elig_type == "Open to seniors only" & 
                                                frequency_visits %in% c("More than once a month, less than weekly",
                                                                        "More than once a week", 
                                                                        "Weekly")) ~ 1,
                                  T ~ 0)
  )

tract_food_count <- tract_food %>%
  group_by(GEOID) %>%
  summarise(vars(starts_with("char"), is_snap, is_charitable),
               sum, na.rm = TRUE
  )


# tract_food_count <- tract_food %>%
#   group_by(GEOID) %>%
#   summarise_at(
#     count_snap = sum(is_snap, na.rm = TRUE),
#     
#     count_char_open_all = sum(char_open_all, na.rm = TRUE),
#     count_char_child_all = sum(char_child_all, na.rm = TRUE),
#     count_char_sen_all = sum(char_sen_all, na.rm = TRUE),
#   )

routes_all <- routes_acs %>%
  left_join(tract_food_count, by = c("geoid_end" = "GEOID"))

write_csv(routes_all, here("routing/data", "routes_all.csv"))

routes_all <- read_csv(here("routing/data", "routes_all.csv"),
                       col_types = c("geoid_start" = "character",
                                      "geoid_end" = "character",
                                      "date" = "character"))

fi <- read_csv(here("Food insecurity rates", 
                    "Food Insecurity Rates - Arlington County.csv"),
               col_types = c("GEOID" = "character")) 


mfi <- read_csv(here("Food insecurity rates", 
                     "Food Insecurity Rates - Arlington County - MFI.csv"),
                col_types = c("GEOID" = "character"))

all_fi <- fi %>%
  select(GEOID, percent_food_insecure) %>%
  left_join(mfi %>% select(GEOID, percent_mfi = percent_food_insecure), 
            by = "GEOID") %>%
  # set at in the top quartile of tracts
  mutate(is_high_fi = ifelse(percent_food_insecure > .12, 1, 0),
         is_high_mfi = ifelse(percent_mfi > .12, 1, 0)) 

snap_wkdy <- travel_time_to_closest(all_data = routes_all, 
                                       food_type = count_snap,
                                       dur_type = wt_duration,
                                       route_date = "2021-09-15") %>%
  left_join(all_fi, by = c("geoid_start" = "GEOID")) %>%
  mutate(high_need_low_access_snap = ifelse((is_high_fi == 1) & (min_duration > 15), 1, 0))

char_open_wkdy <- travel_time_to_closest(all_data = routes_all, 
                                    food_type = count_char_open,
                                    dur_type = wt_duration,
                                    route_date = "2021-09-15")

char_child_wkdy <- travel_time_to_closest(all_data = routes_all, 
                                    food_type = count_char_child,
                                    dur_type = wt_duration,
                                    route_date = "2021-09-15")

char_senior_wkdy <- travel_time_to_closest(all_data = routes_all, 
                                          food_type = count_char_sen,
                                          dur_type = wt_duration,
                                          route_date = "2021-09-15")

snap_wkdy_com <- travel_time_to_closest(all_data = routes_all, 
                                    food_type = count_snap,
                                    dur_type = wt_duration_com,
                                    route_date = "2021-09-15") %>%
  left_join(all_fi, by = c("geoid_start" = "GEOID")) %>%
  mutate(high_need_low_access_snap = ifelse((is_high_fi == 1) & (min_duration > 15), 1, 0))

snap_wkdy_count <- count_accessible_within_t(all_data = routes_all, 
                                             food_type = count_snap, 
                                             dur_type = wt_duration_com, 
                                             t = 15, 
                                             route_date = "2021-09-15")
# look for just transit
# look at number of retailers accessible

map_snap <- map_time_to_closest(acs, snap_wkdy, "SNAP Retailer")

map_snap_com <- map_time_to_closest(acs, snap_wkdy_com, "SNAP Retailer")

map_char <- map_time_to_closest(acs, char_open_wkdy, "Open Charitable Food Site")



