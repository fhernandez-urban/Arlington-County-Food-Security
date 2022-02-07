library(tidyverse)

all_sites <- read_csv("Final food data/Food_retailers_MAPPING.csv") %>% 
  filter(!location_address %in% c("3159 Row St.","3305 Glen Carlyn Rd"))

levels(as.factor(all_sites$frequency))

weekly_count <- all_sites %>% 
  mutate(weekly = case_when(frequency %in% c("3 times weekly. Participants can come once a week.",
                                                   "Daily", "Open Mon-Sat. Participants can visit once per week.", 
                                                   "Twice weekly", "Weekly") ~ "Weekly or more frequent", 
                            TRUE ~ "Less than weekly")) %>% 
  count(weekly == "Weekly or more frequent")


all_sites %>% 
  count(frequency_visit == "Weekly or more frequent")

nth_count <- all_sites %>% 
  mutate(weekend_ar = case_when(day %in% c("Saturday", "Sunday", "4th Saturday of the month", 
                                           "3rd Saturday of the month", "Wed, Thurs, Sat") ~ "Yes", TRUE ~ "No"), 
         nth = case_when(times %in% c("Mon - Fri 9:30 AM - 1 PM, Thurs 6-7PM, Sat 9-11 AM",
                                      "6:00 PM", "6:30 PM", "3 - 7 PM") ~ "Yes", TRUE ~ "No")) %>% 
  count(nth == "Yes")

all_sites %>% 
  count(open_afterhrs == "Open at or after 5:00 PM")
         