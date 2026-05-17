

############################################################
############################################################
#################### ATAP Workshop #########################
############### 3. Exploratory figures #####################
############################################################
############################################################


# Question? What are the first results that generally appear in the 
# telemetry literature? 


pacman::p_load(tidyverse, lubridate, janitor, stringr, viridis, flextable, officer) 

rm(list = ls()) # start fresh 

# load your detections biometrics receivers and release sites

detections <- read_csv("data/detections_clean.csv")

lubridate::tz(detections$timestamp)

sprintf(
  "There are %s detections in the dataset.",
  nrow(detections)
)

# but not retained in csv so force it do not reconvert 

detections <- detections %>% 
  mutate(
    timestamp = force_tz(
      timestamp,
      tzone = "Africa/Johannesburg"
    ), # remember we converted to SAST in the cleaning so needs forcing
    transmitter = as.character(transmitter)
  ) %>% 
  mutate(detectday = as.Date(timestamp)) %>% ## remove time to only include > 1 detection on any receiver per day
  group_by(detectday, transmitter) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  select(-detectday)

lubridate::tz(detections$timestamp)

sprintf(
  "There are %s detections in the dataset.",
  nrow(detections)
)

# false detection filtering can be much more refined, especially for more resident
# species on smaller arrays. For example, a common rule is to require at least
# two detections within a defined time window, such as one hour, either at the
# same receiver or within the same local receiver group.

# At the scale of this regional array, and for the purposes of this workshop,
# we use a simple conservative filter: remove transmitter-days with only one
# detection anywhere on the array.

# another method of removing false detections is by using a speed filter 
# many packages can do this for you but we can do a quick and dirty 
# to highlight potentially unreasonable detection intervals or speeds between consecutive detections 

# define a conservative speed threshold for flagging unusual movements
max_speed_kmh <- 3

# similar to how we've worked with the receivers we use lags 
# to understand the time difference between each detection relative to each 
# transmitter 

detections_speed <- detections %>% 
  arrange(transmitter, timestamp) %>% 
  group_by(transmitter) %>% 
  mutate(
    previous_timestamp = lag(timestamp),
    previous_station = lag(station_name),
    previous_latitude = lag(latitude),
    previous_longitude = lag(longitude),
    
    time_diff_hours = as.numeric(
      difftime(timestamp, previous_timestamp, units = "hours")
    ),
# using the geosphere package we can conservatively estimate the shortest path between 
# two receivers in km
    distance_km = geosphere::distHaversine(
      cbind(previous_longitude, previous_latitude),
      cbind(longitude, latitude)
    ) / 1000,
# simple speed = dist / time calc    
    speed_kmh = distance_km / time_diff_hours,
# use logical statements to see whether there are wierd things going on    
    possible_false_detection =
      is.finite(speed_kmh) &
      speed_kmh > max_speed_kmh &
      distance_km > 5 # filter to remove overlapping receiver ranges this can also be tweaked
  ) %>% 
  ungroup() %>% 
  filter(possible_false_detection) %>% 
  arrange(desc(speed_kmh))

detections_speed %>% select(timestamp,transmitter,station_name,previous_station, time_diff_hours,distance_km,speed_kmh) %>% 
  arrange(desc(speed_kmh)) %>% 
  print(n=20)

# if we go into the df we can sort by speed distance and time intervals to determine if 
# detections look off. In theory we'd be looking for unrealistic speeds in excess of 10km 
# to really be concerned 



biometrics <- read_csv("data/biometrics_clean.csv")

biometrics <- biometrics %>% 
  mutate(transmitter = as.character(transmitter))

receivers <- read_csv("data/receiver_effort_clean.csv")

# force the same for receivers

receivers <- receivers %>% 
  mutate(
    effort_start = lubridate::force_tz(
      effort_start,
      tzone = "Africa/Johannesburg"
    ),
    effort_end = lubridate::force_tz(
      effort_end,
      tzone = "Africa/Johannesburg"
    )
  )

lubridate::tz(receivers$effort_start)
lubridate::tz(receivers$effort_end)

release <- read_csv("data/release_clean.csv")


# lets plot a few standard telemetry plots the go to is usually an abacus plot of 
# your detections 

ggplot()+
  geom_point(data = detections, aes(x = timestamp, y = transmitter))

# cool first abacus plotted, but it is a bit boring, and this is the power of 
# ggplot r and all the customisation you can do to get the perfect plot 
# for your thesis or manuscript 

# we can build in some aesthetics to make the plot pop a bit 


ggplot()+
  geom_point(data = detections, aes(x = timestamp, y = transmitter))+
theme_bw() +
  theme(
    panel.background = element_blank(),
     panel.grid.major.y = element_line(colour = "grey", linewidth = 0.3),
    # panel.grid.minor.y = element_blank(),
    # panel.grid.major.x = element_blank(),
     panel.grid.minor.x = element_blank(),
    #axis.line = element_line(colour = "black"),
    strip.background = element_rect(fill = "grey"),
    text = element_text(size = 12)
  )+
  labs(x = "Date",
       y = "Shark ID") ## customise your labels


# still kinda boring though 

ggplot()+
  geom_point(data = detections, aes(x = timestamp, y = transmitter, colour = longitude))+
  scale_colour_viridis_c(name = "Longitude") +
  theme_bw() +
  theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "grey", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    strip.background = element_rect(fill = "grey"),
    text = element_text(size = 12)
  )+
  labs(x = "Date",
       y = "Shark ID")


# noice, looking a bit more interesting 
# lets try and add some more information to the plot 
# remember in the biometrics we had a shark that was recaptured  
# so lets try and plot the start and end dates for each tag so we get an effective study 
# window 


ggplot()+
  geom_point(data = detections, aes(x = timestamp, y = transmitter, colour = longitude))+
  scale_colour_viridis_c(name = "Longitude") +
  geom_point(data = biometrics, aes(tag_end, transmitter))+
  theme_bw() +
  theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "grey", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    strip.background = element_rect(fill = "grey"),
    text = element_text(size = 12)
  )+
  labs(x = "Date",
       y = "Shark ID")

# so the tag.end here is way outside our study period and we therefore need 
# to add some study bounds to contain this 
# in this case let's call study start when the sharks were tagged 
# and the study end we can call 2026-12-31 therefore we'll only want to retain the 
# recaputre sharks metadata


study_start <- ymd("2021-07-05", tz = "Africa/Johannesburg") # where you have multiple transmitters deployed at different times 
# use your first tag as the study start but it you can also use this to plot on the abacus if you wanted

study_end <- ymd("2026-12-31", tz = "Africa/Johannesburg")

## this will be more useful for the summary tabel calculations later on


# we can filter data within ggplot 

ggplot()+
  geom_point(data = detections, aes(x = timestamp, y = transmitter, colour = longitude))+
  scale_colour_viridis_c(name = "Longitude") +
  geom_point(data = biometrics %>% 
               filter(recaptured == "Y"), aes(tag_end, transmitter),
             colour = "red", shape = 4, stroke = 1, size = 2)+
  geom_vline(xintercept = study_start, linetype = "dashed", colour = "black")+
  # or plot x at the start of each transmitter
  geom_point(
    data = biometrics,
    aes(x = release_date, y = transmitter), shape = 4, stroke = 1, size = 1.5)+
  theme_bw() +
  theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "grey", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    strip.background = element_rect(fill = "grey"),
    text = element_text(size = 12)
  )+
  labs(x = "Date",
       y = "Shark ID")

# we also might not want to publish raw ID codes so we can do some creative shark ID 
# creation based on what we already have 


biometrics <- biometrics %>% 
  arrange(release, release_date) %>% 
  group_by(release) %>% 
  mutate(
    shark = paste0(release, row_number())
  ) %>% 
  ungroup()

# we can add this to our detections as well if we want to use this code instead of ID 
# add some bio info and turn sex to factor

detections <- detections %>% 
  left_join(biometrics %>% select(transmitter,shark, sex, group)) %>% 
  mutate(sex = factor(sex, levels = c("F", "M")))

# and now lets replot 


ggplot()+
  geom_point(data = detections, aes(x = timestamp, y = shark, colour = longitude))+
  scale_colour_viridis_c(name = "Longitude") +
  geom_point(data = biometrics %>% 
               filter(recaptured == "Y"), aes(tag_end, shark),
             colour = "red", shape = 4, stroke = 1, size = 2)+
  geom_point(data = biometrics,
    aes(x = release_date, y = shark),, shape = 4, stroke = 1, size = 1.5)+
  theme_bw() +
  theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "grey", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    strip.background = element_rect(fill = "grey"),
    text = element_text(size = 12)
  )+
  labs(x = "Date",
       y = "Shark ID")


# cool so the ylab now tells us the provenence of each shark and it's tagging order as an 
# id code 
# on e last tweak to really squeeze the juice 

abacus_sex <- ggplot()+
  geom_point(data = detections, aes(x = timestamp, y = shark, colour = longitude))+
  scale_colour_viridis_c(name = "Longitude") +
  geom_point(data = biometrics %>% 
               filter(recaptured == "Y"), aes(tag_end, shark),
             colour = "red", shape = 4, stroke = 1, size = 2)+
  geom_point(data = biometrics,
             aes(x = release_date, y = shark), shape = 4, stroke = 1, size = 1.5)+
  facet_wrap(~sex, scales = "free_y", ncol = 2, labeller = labeller(sex = c("F" = "Female", "M" = "Male")))+
  theme_bw() +
  theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "grey", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    strip.background = element_rect(fill = "grey"),
    text = element_text(size = 12)
  )+
  labs(x = NULL,
    y = "Shark ID")


abacus_sex

# for 5 individuals there are ~30k detections that ggplot is trying to render
# when you have lots of tags and tens of thousands of detections, plotting can become slow
# one useful option is to thin the data for visualisation by keeping only the first
# detection per shark, receiver and day
# this keeps a daily record of receiver use while removing repeated intraday detections
# and reduces the computational burden of rendering the abacus plot

# looks pretty good lets export to our plot folder and tweak the image dimensions 

ggsave("plots/Abacus_plot_sex.png",
       plot = abacus_sex,
       dpi = 300,
       height = 6,
       width = 12)

# at the array scale plotting points by receiver can get pretty messy, but it might 
# be something of interest to a finer scale study 

# alternatively we can build in another grouping level to give the study region some 
# more biogeographic context 

# broadly from mozambique to st lucia is tropical 
# then subtropical to kei mouth and warm temperate after kei mouth 

stlucia_lat <- -28.17
kei_lat <- -32.68

receivers$biogeozone <- dplyr::case_when(
  receivers$latitude <= kei_lat ~ "Warm temperate",
  receivers$latitude > kei_lat &
  receivers$latitude < stlucia_lat ~ "Subtropical",
  receivers$latitude >= stlucia_lat ~ "Tropical"
)

# and we can add country in a similar way if we're dealing with transboundary 
# movement 

sa_moz_lat <- -26.86

receivers$region <- case_when(
  receivers$latitude < sa_moz_lat ~ "South Africa",
  receivers$latitude >= sa_moz_lat ~ "Mozambique"
)

# last abacus this time by biozone

detections <- detections %>% 
  left_join(receivers %>% 
              select(station_name,biogeozone,region) %>% 
              distinct(),
              by = "station_name")

ggplot()+
  geom_point(data = detections, aes(x = timestamp, y = shark, colour = biogeozone))+
 
  geom_point(data = biometrics %>% 
               filter(recaptured == "Y"), aes(tag_end, shark),
             colour = "red", shape = 4, stroke = 1, size = 2)+
  geom_point(data = biometrics,
             aes(x = release_date, y = shark), shape = 4, stroke = 1, size = 1.5)+
  facet_wrap(~sex, scales = "free_y", ncol = 2, labeller = labeller(sex = c("F" = "Female", "M" = "Male")))+
  theme_bw() +
  theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "grey", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    strip.background = element_rect(fill = "grey"),
    text = element_text(size = 12)
  )+
  labs(x = NULL,
       y = "Shark ID")

# you'll notice more gremlins appearing 

# let's have a look at which receivers are giving trouble and if we can fix it 

detections %>% 
  filter(is.na(biogeozone)) %>% 
  distinct(station_name) 

# a step further, cases often creep in and become problematic
# typically you can lowercase everything at the start of your workflow 
# but I've left these in to illustrate how it can cause headaches


detections %>% 
  filter(is.na(biogeozone)) %>% 
  distinct(station_name) %>% 
  mutate(
    station_match = tolower(station_name)
  ) %>% 
  left_join(
    receivers %>% 
      distinct(station_name) %>% 
      mutate(
        receiver_station_name = station_name,
        station_match = tolower(station_name)
      ) %>% 
      select(receiver_station_name, station_match),
    by = "station_match"
  )

# so we can see that the issues in the detection data is Black Rocks Inside is capitalised 
# and we find a match when we do a temp lower case. 
# in the case of the KLM receiver it appears that there is no receiver 
# metadata so we have to go hassle Taryn for upto date metadata

# so we can do a manual change 

detections <- detections %>% 
  mutate(
    station_name = case_when(
      station_name == "Black Rocks Inside" ~ "Black Rocks inside",
      TRUE ~ station_name
    )
  ) %>% 
  filter(
    !station_name %in% c(
      "KLM010",
      "KLM011",
      "KLM012",
      "KLM013",
      "KLM014",
      "KLM015",
      "KLM016"
    )##### and then remove detections for which we have no metadata for
  )


#rerun 
detections <- detections %>% select(-biogeozone,region) %>% 
  left_join(receivers %>% 
              select(station_name,biogeozone,region) %>% 
              distinct(),
            by = "station_name")

# as we checked with our lost receivers lets take another qc step to now check that all detection data have 
# receiver metadata 



detections %>% 
  anti_join(receivers, by = "station_name")


# replot and save based on updated detections 

abacus_sex <- ggplot()+
  geom_point(data = detections, aes(x = timestamp, y = shark, colour = longitude))+
  scale_colour_viridis_c(name = "Longitude") +
  geom_point(data = biometrics %>% 
               filter(recaptured == "Y"), aes(tag_end, shark),
             colour = "red", shape = 4, stroke = 1, size = 2)+
  geom_point(data = biometrics,
             aes(x = release_date, y = shark), shape = 4, stroke = 1, size = 1.5)+
  facet_wrap(~sex, scales = "free_y", ncol = 2, labeller = labeller(sex = c("F" = "Female", "M" = "Male")))+
  theme_bw() +
  theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "grey", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    strip.background = element_rect(fill = "grey"),
    text = element_text(size = 12)
  )+
  labs(x = NULL,
       y = "Shark ID")


abacus_sex

ggsave("plots/Abacus_plot_sex.png",
       plot = abacus_sex,
       dpi = 300,
       height = 6,
       width = 12)


ggplot()+
  geom_point(data = detections, aes(x = timestamp, y = shark, colour = biogeozone))+
  
  geom_point(data = biometrics %>% 
               filter(recaptured == "Y"), aes(tag_end, shark),
             colour = "red", shape = 4, stroke = 1, size = 2)+
  geom_point(data = biometrics,
             aes(x = release_date, y = shark), shape = 4, stroke = 1, size = 1.5)+
  facet_wrap(~sex, scales = "free_y", ncol = 2, labeller = labeller(sex = c("F" = "Female", "M" = "Male")))+
  theme_bw() +
  theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "grey", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    strip.background = element_rect(fill = "grey"),
    text = element_text(size = 12)
  )+
  labs(x = NULL,
       y = "Shark ID",
       colour = "Zone")

##################################################################
##################################################################
###################### END ABACUS ################################
##################################################################
##################################################################




tablesum <- biometrics %>% 
  left_join(
    detections %>% 
      mutate(
        detect_date = as.Date(timestamp)
      ) %>% 
      group_by(transmitter) %>% 
      summarise(
        detections = n(),
        receivers = n_distinct(station_name),
        days_detected = n_distinct(detect_date),
        first_detect = min(detect_date),
        last_detect = max(detect_date),
        .groups = "drop"
      ),
    by = "transmitter"
  ) %>% 
  mutate(
    detections = coalesce(detections, 0L), ### this is a nice safety net when reporting tags with no detections as you've drawn from your overall sample instead of detected
    receivers = coalesce(receivers, 0L),
    days_detected = coalesce(days_detected, 0L),
    # adjust the end date of the taf based on known fate dates
    effective_tag_end = case_when(
      recaptured == "Y" & tag_end < study_end ~ tag_end,
      TRUE ~ study_end
    ),
    
    days_at_liberty = as.numeric(
      difftime(last_detect, release_date, units = "days")
    ),
    
    total_days = as.numeric(
      difftime(effective_tag_end, release_date, units = "days")
    ),
    
    detections_per_day_detected = if_else(
      days_detected > 0,
      detections / days_detected,
      NA_real_
    ),
    ###### residency can be calualted in a number of meaningful ways in this case it is residency/occurence in the entire array
    ##### for a wide ranging sepcies. At finer scales using a different RI could also be useful but see Appert et al (2023)
    residency_index = if_else(
      total_days > 0,
      days_detected / total_days,
      NA_real_
    )
  )

print(tablesum)

publication_table <- tablesum %>% 
 
  mutate(effective_tag_end = as.Date(effective_tag_end)) %>% 
  select(
    shark,
    
    release_date,
    effective_tag_end,
    # first_detect,
    # last_detect,
    group,
    sex,
    recaptured,
    detections,
    receivers,
    days_detected,
    residency_index
  ) %>% 
  mutate(
    residency_index = round(residency_index, 3)
  ) %>% 
  rename_with(
    ~ stringr::str_to_sentence(gsub("_", " ", .x))
  ) 


publication_flex <- publication_table %>% 
  flextable() %>% 
  autofit() %>% 
  theme_booktabs() %>% 
  bold(part = "header") %>% 
  align(
    align = "center",
    part = "all"
  ) %>% 
  fontsize(
    size = 17,
    part = "all"
  ) %>% 
  width(
    width = 0.55
  ) %>% 
  colformat_num(
    big.mark = "",
    digits = 0
  )

publication_flex

save_as_image(
  publication_flex,
  path = "tables/publication_summary_table.png"
)

### As with any table there's a million ways to skin it and style it 
### to present the core results that you want to in the most effective form 


