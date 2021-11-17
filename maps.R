# Date: November 24, 2020 
# Project: ArCo Food insecurity
# Author: Fernando Hernandez
# Task: Map out race, poverty, and food insecurity
# Skip the following steps if urbnmapr and associated packages already installed:
#install.packages("tidyverse")
#**ggplot2 included within tidyverse**
#install.packages("devtools")
#install.packages("tidycensus")                           
#install.packages("patchwork")
#devtools::
remotes::install_github("UrbanInstitute/urbnthemes")
remotes::install_github("UrbanInstitute/urbnmapr")

library(tidyverse)
library(ggplot2)
library(patchwork)
library(urbnthemes)
library(urbnmapr)
library (tigris)
library(sf)
library(dplyr) 
library(censusapi)
library(tidycensus)  
library(forcats)
library(gridExtra)
library(haven)
library(scales)

set_urbn_defaults(style = "map")
urbnthemes :: lato_test()
urbnthemes :: lato_install()
extrafont::font_import(paths =c("C:/Users/FHernandez/Downloads/"), pattern = "Lato-Regular")

## Uses tidycensus::get_acs function to query API and obtain ACS estimates
## for defined variables. Reshapes data frame to wide.
census_api_key('3a2ea8d29baee033d48931425c57035f6ed1f2f7', overwrite = TRUE, install = TRUE)

acs = get_acs(state = "51", county = "013", geography = "tract", 
              variables = c("B03002_001", "B03002_002", "B03002_003",
                            "B03002_004", "B03002_006", "B03002_012",
                            "B03002_013", "B03002_014", "B03002_016",
                            "DP04_0001", "B19013_001", "C17002_001", 
                            "C17002_002", "C17002_003", "C17002_004", 
                            "C17002_005", "C17002_006", "B25064_001", 
                            "B25070_001", "B25070_008", "B25070_009", 
                            "B25070_010", "B28002_001", "B28002_002",
                            "B05001_001", "B05001_005", "B05001_006"),
              geometry = T)

wide_acs <- acs %>% select(-moe) %>% 
  spread(variable, estimate) %>%
  rename(total_pop = B03002_001,
         nonlatine = B03002_002,
         nlwhite = B03002_003,
         nlblack = B03002_004, 
         nlasian = B03002_006, 
         latine = B03002_012,
         lwhite = B03002_013,
         lblack = B03002_014, 
         lasian = B03002_016, 
         total_hh = DP04_0001, 
         medhhinc = B19013_001,
         total_hh_poverty = C17002_001,
         hh_under_0.5_poverty = C17002_002, 
         hh_0.5_to_0.99_poverty = C17002_003,
         hh_1_to_1.24_poverty = C17002_004,
         hh_1.25_to_1.49_poverty = C17002_005,
         hh_1.5_to_1.84_poverty = C17002_006,
         medrent = B25064_001,
         total_pctincrent = B25070_001,
         pctincrent35_to_39.9 = B25070_008,
         pctincrent40_to_49.9 = B25070_009,
         pctincrent50_ormore = B25070_010,
         total_ipop = B28002_001,
         hh_inet = B28002_002,
         tpop_nat = B05001_001,
         nat_nat = B05001_005,
         nat_noncit = B05001_006) %>%
  mutate(pct_nlblack = nlblack / total_pop,
         pct_nlwhite = nlwhite / total_pop,
         pct_nlasian = nlasian / total_pop,
         pct_latine = latine / total_pop,
         pct_lblack = lblack / total_pop,
         pct_lwhite = lwhite / total_pop,
         pct_lasian = lasian / total_pop,
         pct_185pov = (hh_under_0.5_poverty + hh_1_to_1.24_poverty + hh_1.25_to_1.49_poverty  + hh_1.5_to_1.84_poverty)/total_hh_poverty,
         pct_incrent35 = (pctincrent35_to_39.9 + pctincrent40_to_49.9 + pctincrent50_ormore)/total_pctincrent,
         pct_inetaccess = hh_inet/total_ipop,
         comcolor <- ifelse(pct_nlwhite <0.40, 1, 0),
         pct_nonnat = (nat_nat + nat_noncit)/tpop_nat,
         pct_noncit = (nat_noncit)/tpop_nat)

write.csv(wide_acs, "wide_acs.csv", row.names = F)

arco_tracts <- tigris::tracts(state = "VA",
                              cb = TRUE,
                              class = "sf") 
arco_tracts <- subset(arco_tracts, COUNTYFP == "013")

#FI/MFI data
combined_FI_MFI <- read_excel("Raw FI/Combined FI-MFI.xlsx")%>%
  mutate(tract = str_replace(str_extract(geography, "\\d+\\.?\\d+"), "\\.", ""),
         GEOID = str_pad(paste0("51013", tract), side = "right", width = 11, pad = "0")) %>%
  select(-tract)

fs <- read.csv("Raw FI/Food Insecurity Rates - Arlington County.csv") %>%
  mutate(tract = str_replace(str_extract(geography, "\\d+\\.?\\d+"), "\\.", ""),
         GEOID = str_pad(paste0("51013", tract), side = "right", width = 11, pad = "0")) %>%
  select(-tract)

fs_mfi <- read.csv("Raw FI/Food Insecurity Rates - Arlington County - MFI.csv") %>%
  mutate(tract = str_replace(str_extract(geography, "\\d+\\.?\\d+"), "\\.", ""),
         GEOID = str_pad(paste0("51013", tract), side = "right", width = 11, pad = "0")) %>%
  select(-tract)

##Merging on ACS and FI/MFI data
acs_ficombo <- wide_acs %>% left_join(combined_FI_MFI, by = "GEOID")
#acs_fs <- wide_acs %>% left_join(fs, by = "GEOID")
#acs_fsmfi <- wide_acs %>% left_join(fs_mfi, by = "GEOID")

#Retailer data
##SNAP Retailers
snap_fs <- read_csv("Food site data/Food_retailers_MAPPING.csv")
snap_fs<-snap_fs[!(snap_fs$zip_code==22306 | snap_fs$zip_code==22044),]

##Charitable food sites
cfs_all <- read.csv("Food site data/Food_retailers_cfs_o2a.csv")
cfs_kids <- read.csv("Food site data/Food_retailers_cfs_child.csv")
cfs_elder <- read.csv("Food site data/Food_retailers_cfs_elder.csv")

##Non-SNAP retailers
non_snap <-read.csv("https://raw.githubusercontent.com/fhernandez-urban/Arlington-County-Food-Security/main/Non_SNAP_Retailers.csv")

##Setting geo
fsite_all <- snap_fs %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4269) %>% 
  st_transform(crs = 6487)

fsite_snap <- snap_fs %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4269) %>% 
  st_transform(crs = 6487)%>% 
  filter(!location_type %in% c("Charitable food-site"))

fs_cfsall <- cfs_all %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4269) %>% 
  st_transform(crs = 6487)%>% 
  filter(!zip_code %in% c("22306"))%>% 
  filter(!objectid %in% c("75"))%>% 
  filter(!objectid %in% c("48"))

fs_cfskids <- cfs_kids %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4269) %>% 
  st_transform(crs = 6487)%>% 
  filter(!zip_code %in% c("22306"))%>% 
  filter(!objectid %in% c("75"))%>% 
  filter(!objectid %in% c("48"))

fs_cfselders <- cfs_elder %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4269) %>% 
  st_transform(crs = 6487)%>% 
  filter(!zip_code %in% c("22306"))%>% 
  filter(!objectid %in% c("75"))%>% 
  filter(!objectid %in% c("48"))

##NEEDS TO BE FIXED
fsite_nonsnap <- non_snap %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4269) %>% 
  st_transform(crs = 6487)


#MISC
set_urbn_defaults(style = "map")
urban_colors <- c("#cfe8f3", "#a2d4ec", "#73bfe2", "#46abdb", "#1696d2", "#12719e", "#0a4c6a", "#062635")
fsite_colors <- c("#ec008b", "#fdbf11")

# Function to produce maps ------------------------------------------------

# get road shapefle
road <- roads(state = "Virginia", county = "013")

#function to make demographic map
##All retailers (except nonSNAP)
map_all <-  function (data1 = acs_ficombo,data2=fsite_all, percent_variable = "pct_latine", title = "Percent Latine Population"){
  percent_variable <- rlang::sym(percent_variable)
  plot <- ggplot() +
    geom_sf(data=acs_ficombo, aes(fill = FI), color = "grey")+
    geom_sf(data = road,
            color="grey", fill="white", size=0.25, alpha =.5)+
    scale_fill_gradientn(name = "Share of food insecure households", colours = urban_colors, labels = percent, 
                         limits = c(0,.15) ,breaks=c(0, .05, .10, .15)) +
    geom_sf(data = fsite_all, mapping = aes(color = elig_type),color = "#e54096",size = 1.5, show.legend = "point", inherit.aes = F) +
    scale_color_discrete(name = "Eligibility type")+
    theme(legend.position = "left")
}

##SNAP retailers
map_snap <-  function (data1 = acs_ficombo,data2=fsite_snap, percent_variable = "pct_latine", title = "Percent Latine Population"){
  percent_variable <- rlang::sym(percent_variable)
  plot <- ggplot() +
    geom_sf(data=acs_ficombo, aes(fill = FI), color = "grey")+
    geom_sf(data = road,
            color="grey", fill="white", size=0.25, alpha =.5)+
    scale_fill_gradientn(name = "Share of food insecure households", colours = urban_colors, labels = percent, 
                         limits = c(0,.15) ,breaks=c(0, .05, .10, .15)) +
    geom_sf(data = fsite_all, mapping = aes(color = elig_type),color = "#e54096",size = 1.5, show.legend = "point", inherit.aes = F) +
    scale_color_discrete(name = "Eligibility type")+
    theme(legend.position = "left")
  return(plot)
}

##Non-SNAP retailers
map_nonsnap <-  function (data1 = acs_ficombo,data2=non_snap, percent_variable = "pct_latine", title = "Percent Latine Population"){
  percent_variable <- rlang::sym(percent_variable)
  plot <- ggplot() +
    geom_sf(data=acs_ficombo, aes(fill = FI), color = "grey")+
    geom_sf(data = road,
            color="grey", fill="white", size=0.25, alpha =.5)+
    scale_fill_gradientn(name = "Share of food insecure households", colours = urban_colors, labels = percent, 
                         limits = c(0,.15) ,breaks=c(0, .05, .10, .15)) +
    geom_sf(data = fsite_all, mapping = aes(color = elig_type),color = "#e54096",size = 1.5, show.legend = "point", inherit.aes = F) +
    scale_color_discrete(name = "Eligibility type")+
    theme(legend.position = "left")
  return(plot)
}

##Charitable food sites - all
map_cfsall <-  function (data1 = acs_ficombo,data2=fs_cfsall, percent_variable = "pct_latine", title = "Percent Latine Population"){
  percent_variable <- rlang::sym(percent_variable)
  plot <- ggplot() +
    geom_sf(data=acs_ficombo, aes(fill = FI), color = "grey")+
    geom_sf(data = road,
            color="grey", fill="white", size=0.25, alpha =.5)+
    scale_fill_gradientn(name = "Share of food insecure households", colours = urban_colors, labels = percent, 
                         limits = c(0,.15) ,breaks=c(0, .05, .10, .15)) +
    geom_sf(data = fsite_all, mapping = aes(color = elig_type),color = "#e54096",size = 1.5, show.legend = "point", inherit.aes = F) +
    scale_color_discrete(name = "Eligibility type")+
    theme(legend.position = "left")
  return(plot)
}

##Charitable food sites - Children only
map_cfskids <-  function (data1 = acs_ficombo,data2=fs_cfskids, percent_variable = "pct_latine", title = "Percent Latine Population"){
  percent_variable <- rlang::sym(percent_variable)
  plot <- ggplot() +
    geom_sf(data=acs_ficombo, aes(fill = FI), color = "grey")+
    geom_sf(data = road,
            color="grey", fill="white", size=0.25, alpha =.5)+
    scale_fill_gradientn(name = "Share of food insecure households", colours = urban_colors, labels = percent, 
                         limits = c(0,.15) ,breaks=c(0, .05, .10, .15)) +
    geom_sf(data = fsite_all, mapping = aes(color = elig_type),color = "#e54096",size = 1.5, show.legend = "point", inherit.aes = F) +
    scale_color_discrete(name = "Eligibility type")+
    theme(legend.position = "left")
  return(plot)
}

##Charitable food sites - Elders only
map_cfselders <-  function (data1 = acs_ficombo,data2=fs_cfselders, percent_variable = "pct_latine", title = "Percent Latine Population"){
  percent_variable <- rlang::sym(percent_variable)
  plot <- ggplot() +
    geom_sf(data=acs_ficombo, aes(fill = FI), color = "grey")+
    geom_sf(data = road,
            color="grey", fill="white", size=0.25, alpha =.5)+
    scale_fill_gradientn(name = "Share of food insecure households", colours = urban_colors, labels = percent, 
                         limits = c(0,.15) ,breaks=c(0, .05, .10, .15)) +
    geom_sf(data = fsite_all, mapping = aes(color = elig_type),color = "#e54096",size = 1.5, show.legend = "point", inherit.aes = F) +
    scale_color_discrete(name = "Eligibility type")+
    theme(legend.position = "left")
  return(plot)
}


# MAPS
#Arlington county
ggplot(acs_ficombo, aes(fill = FI)) +
  geom_sf() +
  geom_sf(data = road,
          color="grey", fill="white", size=0.25, alpha =.5)+
  scale_fill_gradientn(colours = urban_colors, name = "Food insecurity rate", labels = percent, 
                       limits = c(0,.15) ,breaks=c(0, .05, .10, .15))+
  theme(legend.position = "left")
ggsave("Final Maps/arco_fi.png", height = 6, width = 12, units = "in", dpi = 500)

#FI and all food sites
ggplot(acs_ficombo, aes(fill = FI)) +
  geom_sf() +
  geom_sf(data = road,
          color="grey", fill="white", size=0.25, alpha =.5)+
  scale_fill_gradientn(colours = urban_colors, name = "Food insecurity rate", labels = percent, 
                       limits = c(0,.15) ,breaks=c(0, .05, .10, .15))+
  theme(legend.position = "left")+
  geom_sf(data = fsite_all, mapping = aes(color = location_type, ),size = 1.5, show.legend = "point", inherit.aes = F) +
  scale_color_manual(name = "Type of food site", values = fsite_colors)+
  theme(legend.position = "right")
ggsave("Final Maps/fsites_fi.png", height = 6, width = 12, units = "in", dpi = 500)

#Food insecurity and access to retail foods
map_all(percent_variable = "FI", title ="Share of food insecure households")
  ggsave("Final Maps/allretailers_fi.png", height = 6, width = 12, units = "in", dpi = 500)

  map_snap(percent_variable = "FI", title ="Share of food insecure households")
  ggsave("Final Maps/snap_fi.png", height = 6, width = 12, units = "in", dpi = 500)

#Food security and access to CFS
  map_cfsall(percent_variable = "FI", title ="Share of food insecure households")
  ggsave("Final Maps/cfsall_fi.png", height = 6, width = 12, units = "in", dpi = 500)
    
    map_cfskids(percent_variable = "FI", title ="Share of food insecure households")
    ggsave("Final Maps/cfskids_fi.png", height = 6, width = 12, units = "in", dpi = 500)
  
    map_cfselders(percent_variable = "FI", title ="Share of food insecure households")
    ggsave("Final Maps/cfselders_fi.png", height = 6, width = 12, units = "in", dpi = 500)

#SNAP/CFS and race
    map_snap(percent_variable = "pct_nlasian", title ="Share of Non-Latine Asian population")
    ggsave("Final Maps/snap_nlasian.png", height = 6, width = 12, units = "in", dpi = 500)
    
    map_snap(percent_variable = "pct_nlblack", title ="Share of Non-Latine Black population")
    ggsave("Final Maps/snap_nlblack.png", height = 6, width = 12, units = "in", dpi = 500)
    
    map_snap(percent_variable = "pct_latine", title ="Share of Latine population")
    ggsave("Final Maps/snap_latine.png", height = 6, width = 12, units = "in", dpi = 500)
    
    map_cfsall(percent_variable = "pct_nlasian", title ="Share of Non-Latine Asian population")
    ggsave("Final Maps/cfsall_nlasian.png", height = 6, width = 12, units = "in", dpi = 500)
    
    map_cfsall(percent_variable = "pct_nlblack", title ="Share of Non-Latine Black population")
    ggsave("Final Maps/cfsall_nlblack.png", height = 6, width = 12, units = "in", dpi = 500)
    
    map_cfsall(percent_variable = "pct_latine", title ="Share of Latine population")
    ggsave("Final Maps/cfsall_latine.png", height = 6, width = 12, units = "in", dpi = 500)

#Income to rent and SNAP/CFS
    map_snap(percent_variable = "pct_incrent35", title ="Share of households whose rent is 35% or more of their income")
    ggsave("Final Maps/snap_rentinc.png", height = 6, width = 12, units = "in", dpi = 500)
    
    map_cfsall(percent_variable = "pct_incrent35", title ="Share of households whose rent is 35% or more of their income")
    ggsave("Final Maps/cfsall_rentinc.png", height = 6, width = 12, units = "in", dpi = 500)

#Non-native population and SNAP/CFS
    map_snap(percent_variable = "pct_noncit", title ="Share of households who are non-US natives")
    ggsave("Final Maps/snap_noncit.png", height = 6, width = 12, units = "in", dpi = 500)
    
    map_cfsall(percent_variable = "pct_noncit", title ="ShareShare of households who are non-US natives")
    ggsave("Final Maps/cfsall_noncit.png", height = 6, width = 12, units = "in", dpi = 500)
