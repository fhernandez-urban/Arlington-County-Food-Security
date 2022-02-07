read_process_acs <- function() {
  ## Uses tidycensus::get_acs function to query API and obtain ACS estimates
  ## for defined variables. Reshapes data frame to wide.
  
  acs = get_acs(state = "51", county = "013", geography = "tract", 
                variables = c("B02001_003", "B02001_002", "B01003_001",
                              "DP04_0001", "B03003_003", "B02001_005", "C17002_001",
                              "C17002_002", "C17002_003", "DP04_0058P",
                              "B08201_001", "B08201_002", "B23025_005",
                              "S0801_C01_001", "S0801_C01_002",
                              "B01001_003", "B01001_004", "B01001_005",
                              "B01001_006", "B01001_020", "B01001_021",
                              "B01001_022", "B01001_023", "B01001_024",
                              "B01001_025", "B01001_027", "B01001_028",
                              "B01001_029", "B01001_030", "B01001_044",
                              "B01001_045", "B01001_046", "B01001_047",
                              "B01001_048", "B01001_049", "S1701_C02_013",
                              "S1701_C02_014", "S1701_C02_015", "S1701_C02_016",
                              "S1701_C02_017", "S1701_C02_018", "S1701_C02_019",
                              "S1701_C02_020", "S1701_C02_002", "S1701_C02_010",
                              "DP05_0019", "DP05_0024"),
                geometry = TRUE)
  
  wide_acs <- acs %>% select(-moe) %>% 
    spread(variable, estimate) %>%
    rename(black = B02001_003, 
           white = B02001_002, 
           total_pop = B01003_001,
           total_hh = DP04_0001, 
           hispanic = B03003_003, 
           asian = B02001_005, 
           total_hh_poverty = C17002_001,
           hh_under_0.5_poverty = C17002_002, 
           hh_0.5_to_0.99_poverty = C17002_003,
           total_hh_car = B08201_001,
           no_cars = B08201_002,
           pct_no_car = DP04_0058P,
           pct_car_commute = S0801_C01_002,
           num_workers = S0801_C01_001,
           unemployed = B23025_005,
           pov_white = S1701_C02_013,
           pov_black = S1701_C02_014,
           pov_asian = S1701_C02_016,
           pov_anai = S1701_C02_015,
           pov_nhpi = S1701_C02_017,
           pov_other = S1701_C02_018,
           pov_two_more = S1701_C02_019,
           pov_hisp = S1701_C02_020,
           pov_children_total = S1701_C02_002,
           pov_seniors_total =  S1701_C02_010,
           children_total = DP05_0019,
           seniors_total = DP05_0024) %>%
    mutate(pct_black = black / total_pop,
           pct_white = white / total_pop,
           pct_hispanic = hispanic / total_pop,
           pct_asian = asian / total_pop,
           pct_own_car = (total_hh_car - no_cars) / total_hh_car,
           num_children = B01001_003 + B01001_004 + B01001_005 + B01001_006 +
             B01001_027 + B01001_028 + B01001_029 + B01001_030,
           num_senior = B01001_020 + B01001_021 + B01001_022 + B01001_023 +
             B01001_024 + B01001_025 + B01001_044 + B01001_045 + B01001_046 +
             B01001_047 + B01001_048 + B01001_049,
           pct_pov_senior = if_else(seniors_total > 0, pov_seniors_total/seniors_total, 0),
           pct_pov_child = if_else(children_total > 0, pov_children_total/children_total, 0),
           is_high_num_pov_senior = ifelse(pov_seniors_total >= quantile(pov_seniors_total, 0.9), 1, 0),
           is_high_num_pov_child = ifelse(pov_children_total >= quantile(pov_children_total, 0.9), 1, 0),
           is_high_pov_senior = ifelse(pct_pov_senior >= quantile(pct_pov_senior, 0.9), 1, 0),
           is_high_pov_child = ifelse(pct_pov_child>= quantile(pct_pov_child, 0.9), 1, 0),
           pct_pov_black = pov_black/sum(pov_black, na.rm = TRUE),
           pct_pov_white = pov_white/sum(pov_white, na.rm = TRUE),
           pct_pov_asian = pov_asian/sum(pov_asian, na.rm = TRUE),
           pct_pov_hisp = pov_hisp/sum(pov_hisp, na.rm = TRUE)) %>%
    select(-starts_with("B01001"))
  
  return(wide_acs)
}


travel_time_to_closest <- function(all_data, 
                                   fi_data,
                                   food_type, 
                                   dur_type,
                                   route_date) {
  
  time_to_closest <- all_data %>%
    # need to update based on data stucture
    filter(.data[[food_type]] > 0, date == route_date) %>%
    group_by(geoid_start) %>%
    summarise(min_duration = min(.data[[dur_type]], na.rm = TRUE)) %>%
    right_join(fi_data, by = c("geoid_start" = "geoid")) %>%
    mutate(high_need_low_access_snap_15 = ifelse((is_high_fi == 1) & (min_duration > 15), 1, 0),
           high_need_low_access_snap_20 = ifelse((is_high_fi == 1) & (min_duration > 20), 1, 0),
           route_date = route_date,
           dur_type = dur_type,
           food_type = food_type)
  
  
  return(time_to_closest)
}


map_time_to_closest <- function(county_shp, ttc, opp, need_var, dur_type, road){

  ttc_shp <- left_join(county_shp, 
                           ttc, 
                           by = c("GEOID" = "geoid_start")) %>%
    mutate({{ need_var }} := as.factor(.data[[need_var]]),
           min_duration = ifelse(min_duration > 30, 30, min_duration))
  
  opp_formatted <- gsub("\ ", "_", tolower(opp))
  dur_type_formatted <- gsub("\ ", "_", tolower(dur_type))
  
  set_urbn_defaults(style = "map")
  urban_colors <- c("#cfe8f3", "#a2d4ec", "#73bfe2", "#46abdb", "#1696d2", "#12719e", "#0a4c6a", "#062635")
  
  time_to_closest <- ggplot() +
    geom_sf(data = ttc_shp, 
            mapping = aes(fill = min_duration, color = .data[[need_var]]), size = .6) +
    #add roads to map
    geom_sf(data = road,
            color="grey", fill="white", size=0.25, alpha =.5) +
    scale_fill_gradientn(colours = urban_colors, 
                         name = "Time (minutes)", 
                         limits = c(0, 30),
                         breaks=c(0, 10, 20, 30)) +
    scale_color_manual(values = c("grey", palette_urbn_main[["magenta"]]), 
                       guide = 'none') + 
    #guides(fill = guide_colourbar(barheight = 8)) +
    theme(legend.position = "right", 
        legend.box = "vertical", 
        legend.key.size = unit(1, "cm"), 
        legend.title = element_text(size=16), #change legend title font size
        legend.text = element_text(size=16))
  
    ggsave(plot = time_to_closest,
           filename = here("routing/images", 
                           str_glue("time_to_closest_{opp_formatted}_{dur_type_formatted}.pdf")),
           height = 6, width = 10, units = "in", dpi = 500, 
           device = cairo_pdf)
    
  return(time_to_closest)
}


count_accessible_within_t <- function(all_data, 
                                      fi_data,
                                      food_type, 
                                      dur_type, 
                                      t, 
                                      route_date) {
  count_within_t <- all_data %>%
    filter(.data[[dur_type]] <= t, date == route_date) %>%
    group_by(geoid_start) %>%
    summarise(count = sum(.data[[food_type]], na.rm = TRUE)) %>%
    right_join(fi_data, by = c("geoid_start" = "geoid")) %>%
    mutate(route_date = route_date,
           dur_type = dur_type,
           food_type = food_type,
           time = t)
  
  

  return(count_within_t)
}


map_count_within_t <- function(count_within_t, 
                               county_shp, 
                               opp, 
                               need_var,
                               dur_type,
                               road,
                               limits,
                               breaks){
  count_within_t <- left_join(county_shp, 
                                count_within_t, 
                                by = c("GEOID" = "geoid_start")) %>%
    mutate({{ need_var }} := as.factor(.data[[need_var]]),
           count = ifelse(GEOID %in% c("51013980200", "51013980100"), NA_real_, count))
  
  time <- count_within_t %>% pull(time) %>% unique()
  set_urbn_defaults(style = "map")
  urban_colors <- c("#cfe8f3", "#a2d4ec", "#73bfe2", "#46abdb", "#1696d2", "#12719e", "#0a4c6a", "#062635")
  
  opp_formatted <- gsub("\ ", "_", tolower(opp))
  dur_type_formatted <- gsub("\ ", "_", tolower(dur_type))
  
  count_t <- ggplot() +
    geom_sf(data = count_within_t, mapping = aes(fill = count, color = .data[[need_var]]),
            size = .6) +
    #add roads to map
    geom_sf(data = road,
            color="grey", fill="white", size=0.25, alpha =.5) +
    scale_fill_gradientn(colours = rev(urban_colors),
                         name = str_glue("Number {opp}"),
                         limits = c(0, 30),
                         breaks=c(0, 10, 20, 30)) +
    scale_color_manual(values = c("grey", palette_urbn_main[["magenta"]]),
                       guide = 'none') +
    #labs(title = str_glue("Number of {opp} accessible within {time} minute\n via {dur_type}"), 
    #     fill = str_glue("Number {opp}")) +
    #guides(fill = guide_colourbar(barheight = 8)) +
    theme(legend.position = "right", 
          legend.box = "vertical", 
          legend.key.size = unit(1, "cm"), 
          legend.title = element_text(size=16), #change legend title font size
          legend.text = element_text(size=16))
    ggsave(
      plot = count_t,
      filename = here("routing/images", 
                      paste0(opp_formatted, "_number_within_", time, dur_type_formatted, ".pdf")),
      height = 6, width = 10, units = "in", dpi = 500, 
      device = cairo_pdf)
  
  return(count_t)
}  
  
map_access_within_t <- function(ttc, 
                               county_shp, 
                               opp, 
                               need_var,
                               dur_type,
                               road,
                               t_limit){
  ttc <- left_join(county_shp,
                   ttc, 
                   by = c("GEOID" = "geoid_start")) %>%
    mutate({{ need_var }} := as.factor(.data[[need_var]]),
           access_in_limit = factor(ifelse(min_duration <= t_limit, "Yes", "No")))
  
  set_urbn_defaults(style = "map")
  
  opp_formatted <- gsub("\ ", "_", tolower(opp))
  dur_type_formatted <- gsub("\ ", "_", tolower(dur_type))
  
  access_in_t <- ggplot() +
    geom_sf(data = ttc, mapping = aes(fill = access_in_limit, color = .data[[need_var]]),
            size = .6) +
    #add roads to map
    geom_sf(data = road,
            color="white", fill="white", size=0.25, alpha =.5) +
    scale_fill_manual(values = c("grey", palette_urbn_main[["yellow"]]),
                         name = str_glue("Access to any location\n within {t_limit} minutes")
                         ) +
    scale_color_manual(values = c("white", palette_urbn_main[["magenta"]]),
                       guide = 'none') +
    theme(legend.position = "right", 
          legend.box = "vertical", 
          legend.key.size = unit(1, "cm"), 
          legend.title = element_text(size=16), #change legend title font size
          legend.text = element_text(size=16))
  ggsave(
    plot = access_in_t,
    filename = here("routing/images", 
                    str_glue("access_to_{opp_formatted}_in_{t_limit}_{dur_type_formatted}.pdf")),
    height = 6, width = 10, units = "in", dpi = 500, 
    device = cairo_pdf)
  
  return(access_in_t)
}  

make_bar_plot_race <- function(county_shp, ttc, opp, dur_type) {
  opp_formatted <- gsub("\ ", "_", tolower(opp))
  dur_type_formatted <- gsub("\ ", "_", tolower(dur_type))
  
  county_shp$geometry <- NULL
  
  wt_avg_race <- left_join(county_shp,
                   ttc,
                   by = c("GEOID" = "geoid_start")) %>%
    mutate(min_duration = ifelse(min_duration > 30 , 30, min_duration),
      across(starts_with("pct_pov"), ~.x * min_duration)) %>%
    select(starts_with("pct_pov")) %>%
    colSums(na.rm = TRUE)
  
  df <- tribble(
    ~race, ~wt_avg,
    "Black", wt_avg_race[["pct_pov_black"]],
    "White", wt_avg_race[["pct_pov_white"]],
    "Hispanic", wt_avg_race[["pct_pov_hisp"]],
    "Asian", wt_avg_race[["pct_pov_asian"]]
  )
  
  set_urbn_defaults(style = "print")
  race_bar_plot <- df %>% mutate(wt_avg = round(wt_avg, 2), race = as.factor(race)) %>%
    ggplot(aes(x = race, y = wt_avg, fill = race)) +
    geom_bar(stat="identity") +
    labs(y = "Weighted Average Time (minutes)",
         x = "Race/Ethnicity Group") +
    geom_text(aes(label=wt_avg), vjust=-0.3, size=3.5) +
    scale_fill_manual(values = c("#1696d2", "#fdbf11", "#000000", "#d2d2d2"),
                       guide = 'none') 
  
  ggsave(
    plot = race_bar_plot,
    filename = here("routing/images", 
                    str_glue("access_to_{opp_formatted}_{dur_type_formatted}_race.pdf")),
    height = 6, width = 10, units = "in", dpi = 500, 
    device = cairo_pdf)
  
  return(race_bar_plot)
}

make_scatter_plot_race <- function(county_shp, ttc, opp, dur_type) {
  opp_formatted <- gsub("\ ", "_", tolower(opp))
  dur_type_formatted <- gsub("\ ", "_", tolower(dur_type))
  
  county_shp$geometry <- NULL
  
  dur_pov_race <- left_join(county_shp,
                           ttc,
                           by = c("GEOID" = "geoid_start")) %>%
    #mutate(across(starts_with("pct_pov"), ~.x * min_duration)) %>%
    select("pov_asian", "pov_black", "pov_hisp", "pov_white", "min_duration", "GEOID") %>%
    mutate(min_duration = ifelse(min_duration > 30 , 30, min_duration)) %>%
    pivot_longer(-c(min_duration, GEOID), 
                 names_to = "race", 
                 names_prefix = "pov_", 
                 values_to = "num_pov")
  set_urbn_defaults(style = "print")
  scatter_race <- ggplot(dur_pov_race, mapping = aes(x = num_pov, y = min_duration, color = race)) +
    geom_point() +
    facet_wrap(~race, ncol = 2) +
    labs(x = "Population Under Federal Poverty Line",
         y = "Time (minutes)")+
    scatter_grid()
                           
  
  ggsave(
    plot = scatter_race,
    filename = here("routing/images", 
                    str_glue("scatter_wt_dur_{opp_formatted}_{dur_type_formatted}_race.pdf")),
    height = 8, width = 8, units = "in", dpi = 500, 
    device = cairo_pdf)
  
  return(scatter_race)
  
  
}


make_facet_map_race_avg <- function(county_shp, ttc, opp, dur_type, road) {
  opp_formatted <- gsub("\ ", "_", tolower(opp))
  dur_type_formatted <- gsub("\ ", "_", tolower(dur_type))
  
  wt_avg_race <- left_join(county_shp,
                           ttc,
                           by = c("GEOID" = "geoid_start")) %>%
    mutate(min_duration = ifelse(min_duration > 30 , 30, min_duration),
           across(starts_with("pct_pov"), ~.x * min_duration)) %>%
    select(starts_with("pct_pov"), "GEOID")
  
  wt_avg_race$geometry <- NULL
  
  wt_avg_race <- wt_avg_race %>%
    pivot_longer(-c(GEOID),
                 names_to = "race", 
                 names_prefix = "pct_pov_", 
                 values_to = "wt_min_dur") 
  
  all_data <- county_shp %>%
    select(GEOID) %>%
    left_join(wt_avg_race, by = "GEOID") 
    
  
  set_urbn_defaults(style = "map")
  urban_colors <- c("#cfe8f3", "#a2d4ec", "#73bfe2", "#46abdb", "#1696d2", "#12719e", "#0a4c6a", "#062635")
  
  
  map_facet_race <- ggplot() +
    geom_sf(data = all_data, mapping = aes(fill = wt_min_dur)) +
    #scale_fill_gradientn(colours = rev(urban_colors)) %>%
    facet_wrap(~race, ncol = 2)
  
  ggsave(
    plot = map_facet_race,
    filename = here("routing/images", 
                    str_glue("map_wt_dur_{opp_formatted}_{dur_type_formatted}_race.pdf")),
    height = 8, width = 8, units = "in", dpi = 500, 
    device = cairo_pdf)
  
  return(map_facet_race)
  
}
 
make_dot_density_race <- function(county_shp){
  pop_pov_race <- county_shp %>%
    select("pov_asian", "pov_black", "pov_hisp", "pov_white", "GEOID")
  
  pop_pov_race$geometry <- NULL
  
  pop_pov_race <-pop_pov_race %>%
    pivot_longer(-c(GEOID),
                 names_to = "race", 
                 names_prefix = "pov_", 
                 values_to = "pop_pov") 
  
  all_data <- county_shp %>%
    select(GEOID) %>%
    left_join(pop_pov_race, by = "GEOID") 
  
  groups <- unique(all_data$race)
  
  race_dots <- map_dfr(groups, ~ {
    all_data %>%
      filter(race == .x) %>%
      st_transform(crs = "EPSG:6487") %>%
      # Have every dot represent 10 people
      mutate(est50 = as.integer(pop_pov / 10)) %>%
      st_sample(size = .$est50, exact = TRUE) %>%
      st_sf() %>%
      # Add group (ie race) as a column so we can use it when plotting
      mutate(group = .x)
  })
  
  dot_map <- ggplot() +
    # Plot tracts, then dots on top of tracts
    geom_sf(
      data = county_shp,
      # Make interior of tracts transparent and boundaries black
      fill = "transparent",
      color = "black"
    ) +
    geom_sf(
      data = race_dots,
      # Color in dots by racial group
      aes(color = group),
      # Adjust transparency and size to be more readable
      alpha = 0.5,
      size = 1.5,
      stroke = FALSE,
      shape = 19
    ) 
  
  ggsave(
    plot = dot_map,
    filename = here("routing/images", 
                    str_glue("dot_density_race.pdf")),
    height = 6, width = 10, units = "in", dpi = 500, 
    device = cairo_pdf)
  
  return(dot_map)
}
