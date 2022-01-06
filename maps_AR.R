# Date: November 24, 2020 
# Project: ArCo Food insecurity
# Author: Fernando Hernandez
# Task: Map out race, poverty, and food insecurity
# Skip the following steps if urbnmapr and associated packages already installed:
#install.packages("tidyverse")
#*ggplot2 included within tidyverse**
#install.packages("devtools")
#install.packages("tidycensus")                           
#install.packages("patchwork")
#devtools::
remotes::install_github("UrbanInstitute/urbnthemes")
remotes::install_github("UrbanInstitute/urbnmapr")
library(Cairo)
library(tidyverse)
library(patchwork)
library(urbnthemes)
library(urbnmapr)
library (tigris)
library(sf)
library(censusapi)
library(tidycensus)  
library(forcats)
library(haven)
library(scales)
library(readxl)
library(janitor)
library(ggnewscale)
library(GISTools)

set_urbn_defaults(style = "map")
#urbnthemes :: lato_import()
#urbnthemes :: lato_test()

#extrafont::font_import(paths =c("C:/Users/Arogin/Downloads/Lato"), pattern = "Lato-Regular", prompt = FALSE)

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
                            "B05001_001", "B05001_005", "B05001_006",
                            "B01001_001", "B01001_003", "B01001_004",
                            "B01001_005", "B01001_006", "B01001_020",
                            "B01001_021", "B01001_022", "B01001_023",
                            "B01001_024", "B01001_025", "B01001_027", 
                            "B01001_028", "B01001_029", "B01001_030",
                            "B01001_044", "B01001_045", "B01001_046",
                            "B01001_047", "B01001_048", "B01001_049",
                            "B17001_004", "B17001_004", "B17001_005",
                            "B17001_006", "B17001_007", "B17001_008", 
                            "B17001_009", "B17001_015", "B17001_016", 
                            "B17001_018", "B17001_019", "B17001_020", 
                            "B17001_021", "B17001_022", "B17001_023", 
                            "B17001_029", "B17001_030"),
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
         nat_noncit = B05001_006,
         tp_age = B01001_001, 
         child1 = B01001_003, 
         child2 = B01001_004,
         child3 = B01001_005,
         child4 = B01001_006,
         senior1 = B01001_020,
         senior2 = B01001_021, 
         senior3 = B01001_022, 
         senior4 = B01001_023,
         senior5 = B01001_024,
         senior6 = B01001_025,
         child5 = B01001_027,
         child6 = B01001_028,
         child7 = B01001_029,
         child8 = B01001_030,
         senior7 = B01001_044,
         senior8 = B01001_045,
         senior9 = B01001_046,
         senior10 = B01001_047,
         senior11 = B01001_048,
         senior12 = B01001_049,
         povchild1 = B17001_004,
         povchild2 = B17001_004,
         povchild3 = B17001_005,
         povchild4 = B17001_006,
         povchild5 = B17001_007,
         povchild6 = B17001_008,
         povchild10 = B17001_009,
         povsenior1 = B17001_015,
         povsenior2 = B17001_016,
         povchild11 = B17001_018,
         povchild12 = B17001_019,
         povchild13 = B17001_020,
         povchild14 = B17001_021,
         povchild15 = B17001_022,
         povchild16 = B17001_023,
         povsenior3 = B17001_029,
         povsenior4 = B17001_030) %>%
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
         pct_noncit = (nat_noncit)/tpop_nat,
         pct_povchildu18 = (povchild1+povchild2+povchild3+povchild4+povchild5+povchild6+povchild7+povchild8+povchild9+povchild10+povchild11+povchild12+povchild13+povchild14+povchild15+povchild16)/(child1+child2+child3+child4+child5+child6+child7+child8),
         pct_povseniors = (povsenior1+povsenior2+povsenior3+povsenior4)/(senior1+senior2+senior3+senior4+senior5+senior6+senior7+senior8+senior9+senior10+senior11+senior12))


arco_tracts <- tigris::tracts(state = "VA",
                              cb = TRUE,
                              class = "sf") 
arco_tracts <- subset(arco_tracts, COUNTYFP == "013")

#FI/MFI data
combined_FI_MFI <- read_excel("Raw FI/Combined FI-MFI.xlsx")%>%
  mutate(tract = str_replace(str_extract(geography, "\\d+\\.?\\d+"), "\\.", ""),
         GEOID = str_pad(paste0("51013", tract), side = "right", width = 11, pad = "0")) %>%
  select(-tract)

##Merging on ACS and FI/MFI data
acs_ficombo <- wide_acs %>% left_join(combined_FI_MFI, by = "GEOID") %>% 
  mutate(is_high_fi = as.factor(ifelse(FI > .12, 1, 0)))
acs_ficombo$FI[58:59] <- NA

acs_ficombo <- acs_ficombo %>% 
  st_transform(crs = 6487)

#Retailer data
##SNAP Retailers
snap_fs <- read_csv("Final food data/Food site data/Food_retailers_MAPPING.csv") 
#snap_fs<-snap_fs[!(snap_fs$zip_code==22306 | snap_fs$zip_code==22044),]

##Non-SNAP retailers
non_snap <-read.csv("non_snap-geocoded.csv") %>% 
  filter(!zip %in% c(22302, 22041, 22044, 22305)) %>% 
  select(c("location_name","location_type", "Longitude", 
           "Latitude"))  %>% 
  rename(type = location_type, 
         longitude = Longitude, 
         latitude = Latitude) %>% 
  mutate(location_type = "Non-SNAP retailer") %>% 
  relocate(location_type, .before = type)


# snap <- rbind(snap_fs, non_snap) %>% 
#   st_as_sf(coords = c("longitude", "latitude"),
#            crs = 4269) %>% 
#   st_transform(crs = 6487) 

##Setting geo

# all food sites
fsite_all <- snap_fs %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4269) %>% 
  st_transform(crs = 6487) %>% 
  filter(!zip_code %in% c(22306,22044), 
         !location_address %in% c("3159 Row St.","3305 Glen Carlyn Rd"))

# Just snap food sites
fsite_snap <- snap_fs %>%
  filter(!location_type %in% c("Charitable food-site"), 
         !zip_code %in% c(22306,22044), 
         !location_address %in% c("3159 Row St.","3305 Glen Carlyn Rd"))

# Just charitable food sites
fs_cfsall <- fsite_all %>% 
  filter(!location_type %in% c("SNAP-retailer"))
  

char_fullaccess <- fs_cfsall %>%
  mutate(accessible = as.factor(case_when((year_round == "Open year-round" &
         restrictions == "Open to all") ~ "Open all year without restrictions", TRUE ~ "Restrictions")))

char_frequent <- char_fullaccess %>% 
  filter(frequency_visit == "Weekly or more frequent", 
         accessible == "Open all year without restrictions")

char_flexible <- char_frequent %>% 
  filter(weekends == "Yes"| open_afterhrs == "Open at or after 5:00 PM")


# SNAP Sites by Census Tract ----------------------------------------------

snap_tracts <- acs_ficombo %>% 
  mutate(counts = lengths(st_intersects(., fsite_snap))) %>% 
  dplyr::select(NAME, counts)

write.csv(snap_tracts, "snap_tracts.csv")

access <- char_fullaccess %>% 
  filter(restrictions == "Open to all")

cf_tracts <- acs_ficombo %>% 
  mutate(counts = lengths(st_intersects(., char_frequent))) %>% 
  dplyr::select(NAME, counts)

# Just charitable food sites
fs_cfsall <- fsite_all %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4269) %>% 
  st_transform(crs = 6487) %>% 
  filter(!objectid %in% c("75", "48"))%>% 
  filter(!location_type %in% c("SNAP-retailer"))
  


#MISC
urban_colors <- c("#cfe8f3", "#a2d4ec", "#73bfe2", "#46abdb", "#1696d2", "#12719e", "#0a4c6a", "#062635")
fsite_colors <- c("#ec008b", "#fdbf11", "#000000")
cfs1_colors <- c("#db2b27", "#fdbf11")
two_color <- c("#ec008b", "#fdbf11")
three_color <- c("#ec008b", "#fdbf11", "#000000")
four_color <- c("#55b748", "#db2b27", "#696969", "#fdbf11")
two_color2 <- c("#55b748", "#fdbf11")

# Function to produce maps ------------------------------------------------

# get road shapefle
road <- roads(state = "Virginia", county = "013")

#Arlington county

ggplot() +
  geom_sf(acs_ficombo, mapping = aes(fill = FI, color = is_high_fi), size = 0.9) +
  geom_sf(data = road,
          color="grey", fill="white", size=0.25, alpha =.5)+
  scale_fill_gradientn(colours = urban_colors, name = "Food insecurity rate", labels = percent, 
                       limits = c(0,.15) ,breaks=c(0, .05, .10, .15))+
  scale_color_manual(values = c("grey", palette_urbn_main[["magenta"]]), 
                     guide = 'none') + 
  theme(legend.position = "right", 
        legend.box = "vertical", 
        legend.key.size = unit(1, "cm"), 
        legend.title = element_text(size=16), #change legend title font size
        legend.text = element_text(size=16))
# ggsave("Final Maps/arco_fi.pdf", height = 6, width = 10, units = "in", dpi = 500, 
#        device = cairo_pdf)

# SNAP Retailers
ggplot() +
  geom_sf(acs_ficombo, mapping = aes(fill = FI, color = is_high_fi), size = 0.9) +
  geom_sf(data = road,
          color="grey", fill="white", size=0.25, alpha =.5)+
  scale_fill_gradientn(colours = urban_colors, name = "Food insecurity rate", labels = percent, 
                       limits = c(0,.15) ,breaks=c(0, .05, .10, .15))+
  scale_color_manual(values = c("grey", palette_urbn_main[["magenta"]]), 
                     guide = 'none') + 
  new_scale_color()+
  geom_sf(data = fsite_snap, mapping = aes(color = location_type),size = 2.5, 
          show.legend = "point", inherit.aes = F) +
  scale_color_manual(values = "#fdbf11", 
                     name = NULL,
                     labels = "SNAP retailers")+
  theme(legend.position = "right", 
        legend.box = "vertical", 
        legend.key.size = unit(1, "cm"), 
        legend.title = element_text(size=16), #change legend title font size
        legend.text = element_text(size=16))

# ggsave("Final Maps/snap_retailers.pdf", height = 6, width = 10, units = "in", dpi = 500, 
#        device = cairo_pdf)


#CFS OPEN YR AND NO ELIGIBILITY REQ
ggplot() +
  # map the census tracts and make food insecurity the fill, and  
  geom_sf(acs_ficombo,mapping = aes(fill = FI, color = is_high_fi), size = 0.9) +
  scale_fill_gradientn(colours = urban_colors, name = "Food insecurity rate", labels = percent, 
                       limits = c(0,.15) ,breaks=c(0, .05, .10, .15))+
  scale_color_manual(values = c("grey", palette_urbn_main[["magenta"]]), 
                      guide = 'none')+
  geom_sf(data = road,
          color="grey", fill="white", size=0.25, alpha =.5)+
  new_scale_color()+
  geom_sf(data = char_fullaccess, mapping = aes(color = accessible),size = 2.5, 
          show.legend = "point", inherit.aes = F) +
  scale_color_manual(values = c("#fdbf11","#000000"), 
                     name = "Charitable Food Sites")+
  theme(legend.position = "right", 
        legend.box = "vertical", 
        legend.key.size = unit(1, "cm"), 
        legend.title = element_text(size=16), #change legend title font size
        legend.text = element_text(size=16)) #change legend text font size)

# ggsave("Final Maps/fsites_cfs_fullaccess.pdf", height = 6, width = 10, units = "in", dpi = 300,
#        device = cairo_pdf)

#CFS OPEN YR AND NO ELIG AND WEEKLY
ggplot() +
  geom_sf(acs_ficombo,mapping = aes(fill = FI, color = is_high_fi), size = 0.9) +
  scale_fill_gradientn(colours = urban_colors, name = "Food insecurity rate", labels = percent, 
                       limits = c(0,.15) ,breaks=c(0, .05, .10, .15))+
  geom_sf(data = road,
          color="grey", fill="white", size=0.25, alpha =.5)+
  scale_color_manual(values = c("grey", palette_urbn_main[["magenta"]]), 
                     guide = 'none') +
  new_scale_color()+
  geom_sf(data = char_frequent,mapping = aes(color = location_type),size = 2.5, 
          show.legend = "point", inherit.aes = F) +
  scale_color_manual(values = "#fdbf11", 
                     labels = "Charitable food sites",
                     name = NULL)+
ggsave("Final Maps/fsites_cfs_fullaccess.pdf", height = 6, width = 10, units = "in", dpi = 300,
       device = cairo_pdf)

# ggsave("Final Maps/cfs_flexibleaccess.pdf", height = 6, width = 10, units = "in", dpi = 500, 
#        device = cairo_pdf)


#CFS OPEN YR AND DURING WEEKENDS AND NTH
ggplot() +
  geom_sf(acs_ficombo,mapping = aes(fill = FI, color = is_high_fi), size = 0.6) +
  scale_fill_gradientn(colours = urban_colors, name = "Food insecurity rate", labels = percent, 
                       limits = c(0,.15) ,breaks=c(0, .05, .10, .15))+
  geom_sf(data = road,
          color="grey", fill="white", size=0.25, alpha =.5)+
  scale_color_manual(values = c("grey", palette_urbn_main[["magenta"]], "#fdbf11"), 
                     guide = 'none') +
  new_scale_color()+
  geom_sf(data = char_flexible,mapping = aes(color = location_type),size = 2.5, 
          show.legend = "point", inherit.aes = F) +
  scale_color_manual(labels = "Chartiable food sites", values = "#fdbf11", 
                     name = NULL)+
  theme(legend.position = "right", 
        legend.box = "vertical", 
        legend.key.size = unit(1, "cm"), 
        legend.title = element_text(size=16), #change legend title font size
        legend.text = element_text(size=16)) #change legend text font size)  
# ggsave("Final Maps/cfs_flexibleaccess.pdf", height = 6, width = 10, units = "in", dpi = 500, 
#        device = cairo_pdf)

#FH: SHARE OF CHILDREN UNDER 18 Y/O AND CFS THAT SERVE CHILDREN
ggplot() +
  geom_sf(acs_ficombo,mapping = aes(fill = pct_childu18, color = is_high_fi), size = 0.6) +
  scale_fill_gradientn(colours = urban_colors, name = "Children under 18 y/o", labels = percent, 
                       limits = c(0,.15) ,breaks=c(0, .05, .10, .15))+
  geom_sf(data = road,
          color="grey", fill="white", size=0.25, alpha =.5)+
  scale_color_manual(values = c("grey", palette_urbn_main[["magenta"]], "#fdbf11"), 
                     guide = 'none') +
  new_scale_color()+
  geom_sf(data = char_flexible,mapping = aes(color = location_type),size = 2.5, 
          show.legend = "point", inherit.aes = F) +
  scale_color_manual(labels = "Chartiable food sites", values = "#fdbf11", 
                     name = NULL)+
  theme(legend.position = "right", 
        legend.box = "vertical", 
        legend.key.size = unit(1, "cm"), 
        legend.title = element_text(size=16), #change legend title font size
        legend.text = element_text(size=16)) #change legend text font size)  
# ggsave("Final Maps/cfs_childrenu18.pdf", height = 6, width = 10, units = "in", dpi = 500, 
#        device = cairo_pdf)


#FH: SHARE OF SENIORS 65+ Y/O AND CFS THAT SERVE SENIORS
ggplot() +
  geom_sf(acs_ficombo,mapping = aes(fill = pct_povseniors, color = is_high_fi), size = 0.6) +
  scale_fill_gradientn(colours = urban_colors, name = "Food insecurity rate", labels = percent, 
                       limits = c(0,.15) ,breaks=c(0, .05, .10, .15))+
  geom_sf(data = road,
          color="grey", fill="white", size=0.25, alpha =.5)+
  scale_color_manual(values = c("grey", palette_urbn_main[["magenta"]], "#fdbf11"), 
                     guide = 'none') +
  new_scale_color()+
  geom_sf(data = char_flexible,mapping = aes(color = location_type),size = 2.5, 
          show.legend = "point", inherit.aes = F) +
  scale_color_manual(labels = "Chartiable food sites", values = "#fdbf11", 
                     name = NULL)+
  theme(legend.position = "right", 
        legend.box = "vertical", 
        legend.key.size = unit(1, "cm"), 
        legend.title = element_text(size=16), #change legend title font size
        legend.text = element_text(size=16)) #change legend text font size)  
# ggsave("Final Maps/cfs_seniors.pdf", height = 6, width = 10, units = "in", dpi = 500, 
#        device = cairo_pdf)


# OLD MAPS ----------------------------------------------------------------

# 
# #function to make demographic map
# ##All retailers (except nonSNAP)
# map_all <-  function (data1 = acs_ficombo,data2=fsite_all, percent_variable = "pct_latine", title = "Percent Latine Population"){
#   percent_variable <- rlang::sym(percent_variable)
#   plot <- ggplot() +
#     geom_sf(data=acs_ficombo, aes(fill = FI), color = "grey")+
#     geom_sf(data = road,
#             color="grey", fill="white", size=0.25, alpha =.5)+
#     scale_fill_gradientn(name = "Share of food insecure households", colours = urban_colors, labels = percent, 
#                          limits = c(0,.15) ,breaks=c(0, .05, .10, .15)) +
#     geom_sf(data = fsite_all, mapping = aes(color = elig_type),color = "#e54096",size = 2.5, show.legend = "point", inherit.aes = F) +
#     scale_color_discrete(name = "Eligibility type")+
#     theme(legend.position = "left")
# }
# 
# ##SNAP retailers
# map_snap <-  function (data1 = acs_ficombo,data2=fsite_snap, percent_variable = "pct_latine", title = "Percent Latine Population"){
#   percent_variable <- rlang::sym(percent_variable)
#   plot <- ggplot() +
#     geom_sf(data=acs_ficombo, aes(fill = FI), color = "grey")+
#     geom_sf(data = road,
#             color="grey", fill="white", size=0.25, alpha =.5)+
#     scale_fill_gradientn(name = "Food Insecurity Rate", colours = urban_colors, labels = percent, 
#                          limits = c(0,.15) ,breaks=c(0, .05, .10, .15)) +
#     geom_sf(data = fsite_all, mapping = aes(color = elig_type),color = "#fdbf11",
#             size = 2.5, show.legend = "point", inherit.aes = F) +
#     scale_color_discrete(name = "Eligibility type")+
#     theme(legend.position = "left")
#   return(plot)
# }
# 
# # MAPS
# 
# 
# #FI and all food sites
# ggplot(acs_ficombo, aes(fill = FI)) +
#   geom_sf() +
#   geom_sf(data = road,
#           color="grey", fill="white", size=0.25, alpha =.5)+
#   scale_fill_gradientn(colours = urban_colors, name = "Food insecurity rate", labels = percent,
#                        limits = c(0,.15) ,breaks=c(0, .05, .10, .15))+
#   theme(legend.position = "left")+
#   geom_sf(data = fsite_all, mapping = aes(color = location_type, ),size = 2.5,
#           show.legend = "point", inherit.aes = F) +
#   scale_color_manual(name = "Type of food site", values = fsite_colors)+
#   theme(legend.position = "right")
# ggsave("Final Maps/fsites_fi.png", height = 6, width = 12, units = "in", dpi = 500)

# #Food insecurity and SNAP retailers
# map_snap(percent_variable = "FI", title ="Food insecurity rate")
# ggsave("Final Maps/snap_fi.png", height = 6, width = 12, units = "in", dpi = 500)
# 
# ###########
# 
# #Food security and access to CFS
# 
# 
# #ALL CFS 
# ggplot(acs_ficombo, aes(fill = FI)) +
#   geom_sf() +
#   geom_sf(data = road,
#           color="grey", fill="white", size=0.25, alpha =.5)+
#   scale_fill_gradientn(colours = urban_colors, name = "Food insecurity rate", labels = percent, 
#                        limits = c(0,.15) ,breaks=c(0, .05, .10, .15))+
#   theme(legend.position = "left")+
#   geom_sf(data = fs_cfsall, mapping = aes(color = charitablefs, ),size = 2.5, 
#           show.legend = "point", inherit.aes = F) +
#   scale_color_manual(name = "Charitable food sites", values = "#db2b27")+
#   theme(legend.position = "right")
# ggsave("Final Maps/fsites_cfs_only.png", height = 6, width = 12, units = "in", dpi = 500)
# 
# 
# #ALL CFS BY WHO HAS ACCESS
# ggplot(acs_ficombo, aes(fill = FI)) +
#   geom_sf() +
#   geom_sf(data = road,
#           color="grey", fill="white", size=0.25, alpha =.5)+
#   scale_fill_gradientn(colours = urban_colors, name = "Food insecurity rate", labels = percent, 
#                        limits = c(0,.15) ,breaks=c(0, .05, .10, .15))+
#   theme(legend.position = "left")+
#   geom_sf(data = fs_cfsall, mapping = aes(color = access, ),size = 2.5, 
#           show.legend = "point", inherit.aes = F) +
#   scale_color_manual(name = "Open year-round, during weekends and NTH", values = four_color)+
#   theme(legend.position = "right")
# ggsave("Final Maps/fsites_cfs_access_conditions.png", height = 6, width = 12, units = "in", dpi = 500)
# 
# #Food sites open yearly
# ggplot(acs_ficombo, aes(fill = FI)) +
#   geom_sf() +
#   geom_sf(data = road,
#           color="grey", fill="white", size=0.25, alpha =.5)+
#   scale_fill_gradientn(colours = urban_colors, name = "Food insecurity rate", labels = percent, 
#                        limits = c(0,.15) ,breaks=c(0, .05, .10, .15))+
#   theme(legend.position = "left")+
#   geom_sf(data = fs_cfsyround, mapping = aes(color = year_round, ),size = 2.5, 
#           show.legend = "point", inherit.aes = F) +
#   scale_color_manual(name = "Open year-round", values = two_color)+
#   theme(legend.position = "right")
# ggsave("Final Maps/fsites_cfs_yearround.png", height = 6, width = 12, units = "in", dpi = 500)  
# 
# #Food sites by frequency
# ggplot(acs_ficombo, aes(fill = FI)) +
#   geom_sf() +
#   geom_sf(data = road,
#           color="grey", fill="white", size=0.25, alpha =.5)+
#   scale_fill_gradientn(colours = urban_colors, name = "Food insecurity rate", labels = percent, 
#                        limits = c(0,.15) ,breaks=c(0, .05, .10, .15))+
#   theme(legend.position = "left")+
#   geom_sf(data = fs_cfsfreq, mapping = aes(color = frequency_visit, ),size = 2.5, 
#           show.legend = "point", inherit.aes = F) +
#   scale_color_manual(name = "Frequency of visit", values = two_color)+
#   theme(legend.position = "right")
# ggsave("Final Maps/fsites_cfs_frequency.png", height = 6, width = 12, units = "in", dpi = 500)
# 
# #Food sites open NTH
# ggplot(acs_ficombo, aes(fill = FI)) +
#   geom_sf() +
#   geom_sf(data = road,
#           color="grey", fill="white", size=0.25, alpha =.5)+
#   scale_fill_gradientn(colours = urban_colors, name = "Food insecurity rate", labels = percent, 
#                        limits = c(0,.15) ,breaks=c(0, .05, .10, .15))+
#   theme(legend.position = "left")+
#   geom_sf(data = fs_cfsnth, mapping = aes(color = open_nth, ),size = 2.5, 
#           show.legend = "point", inherit.aes = F) +
#   scale_color_manual(name = "Open during NTH", values = two_color2)+
#   theme(legend.position = "right")
# ggsave("Final Maps/fsites_cfs_nth.png", height = 6, width = 12, units = "in", dpi = 500)
