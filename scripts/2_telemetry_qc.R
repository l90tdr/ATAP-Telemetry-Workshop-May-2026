
############################################################
############################################################
#################### ATAP Workshop #########################
################### 2. Telemetry QC ########################
############################################################
############################################################


### The first take home is that data are generally messy and need cleaning!

### One of the most time consuming aspects of any analysis will be cleaning 
### datasets and running some quality controls to ensure your following data
### analysis are robust and repeatable

### Some of these are some fairly standard data science concepts 
### and then others will be more applicable to telemetry data 

### Fundamentally, acoustic telemetry datasets consist of 3 or 4 core 
### datasets including a detections, biometrics, receiver metadata and 
### in some cases a release file depending on how you chop your 
### own metadata up 

### The following workflow is based broadly on the formats that ATAP
### data reports are currently distributed to the telemetry community
### in southern Africa. 

### If you are using data that have been uploaded to and then downloaded from
### fathom then you will maybe need to slightly the column names or 
### file structures a bit. 

### But this workshop should get you familiar with some of the core coding 
### contexts so that you can apply these to different formats of data. 

# Let's go!

# Start by loading the relevant packages that we'll need to dig into data 
# wrangling and plotting 

# since we've loaded pacman in the previous script we can use that to load 
# or install the required packages 

pacman::p_load(tidyverse, lubridate, janitor, stringr) 


rm(list = ls()) # clear your env to start fresh 

# tidyverse loads all the relevant tidy packages and lubridate is very useful 
# for wrangling dates in a clean way 

# Starting with our tagging metadata. This is often the most user specific 
# dataset as we each have our own idea of how best to record all the information 
# when we are tagging fish sharks or rays. 

biometrics <- read.csv("data/biometrics.csv")

print(biometrics)

# converting to a tibble the modern tidy version of a df 
# gives you a little more context on the underlying column structures

biometrics <- as_tibble(biometrics)

print(biometrics)

# one of the first things to notice is the column names are a mix of 
# caps lower case and . separators so lets tidy those up
# which will make linking dfs easier later on using janitor 

biometrics <- janitor::clean_names(biometrics)

print(biometrics)

# now we can go about converting columns from character values to dates etc
# dates are probably some of the trickiest concepts to get right 
# if you've imported from csv as 2025-12-12 then you could use 
# as date or lubridate::ymd as YYYY-MM-DD is what base r expects.  
# But if your format is for example 12/12/2025 then r won't decode
# it using as.Date or ymd 
# you can then instead use lubridate::dmy 

biometrics <- biometrics %>% 
  mutate(
    release_date = dmy(release_date), # convert release dates
    tag_end = dmy(tag_end), # convert end dates
    
    transmitter = as.character(transmitter), # transmitter to character for joins
    
    # this is useful where you have a mix of M F f m so all one format
    # as R treats capital strings as different objects
    species = str_to_title(species), 
    release = str_to_upper(release),
    sex = str_to_upper(sex),
    recaptured = str_to_upper(recaptured)
    )


# quickly look at your data structure 

glimpse(biometrics)

print(biometrics)

# lets query some of the columns to see what we have 

biometrics %>% count(species)

# see how r treats Bronze whaler as 3 unique elements because of 
# spelling and a trailing space 

biometrics <- biometrics %>% 
  mutate(
    species = stringr::str_trim(species),
    species = recode(
      species,
      "Brinze Whaler" = "Bronze Whaler"
    )
  )


# lets take a look at the size data

biometrics %>% 
  arrange(desc(size)) %>% # arrange data in descending order
  select(transmitter, size) # lets just remove all the other data 

### NOTE: some packages share function names. For example, select() can be
### masked by other packages. If select() behaves strangely, use dplyr::select()
### so R knows which function you mean


# so lets just do some simple maths and an if_else vectorised statement 
# so if the thing you want to locate == true then apply the change else
# otherwise leave the remaining values unchanged 

biometrics <- biometrics %>% 
  mutate(
    size = if_else(
      transmitter == "5212",
      size / 10,
      size
    )
  )

### one final check and that should be us happy with the biometrics 

print(biometrics)

# export your clean biometrics file for later

#write.csv(biometrics, 'data/biometrics_clean.csv', row.names = FALSE)

##################################################################
##################################################################
###################### END BIO ###################################
##################################################################
##################################################################


### Load the detections and intro readr straight to tibble
# readr is useful to detect certain column types and format them
# on import

detections <- readr::read_csv("data/detections.csv")

# useful to check the top and bottom of the dataset
head(detections)

tail(detections)

# lets janitor the col names 

detections <- janitor::clean_names(detections)

head(detections)

### TIME! It is in UTC which is what the receivers natively offload in
### so you need to be hugely careful to check what time format your data 
### are in and what timezone your are working in so that detections then align
### to local time - learning what UTC coordinated universal time is and why 
### it is used in the telemetry world is a vital concept

# convert detections from UTC to South African local time
# South Africa operates on SAST (UTC +2) and does not use daylight savings

detections <- detections %>% 
  mutate(
    date_and_time_utc = with_tz(
      date_and_time_utc,
      tzone = "Africa/Johannesburg"
    )
  ) %>% 
  rename(timestamp = date_and_time_utc) # this can be ranmed timestamp_sast etc. 

# check the conversion

head(detections)

# there are some naming irregularities which need fixing and determine what columns we want 
# to keep and drop the vr2w and vr2ar encoding as its not overly useful downstream 

detections <- detections %>% 
  select(-transmitter, -sensor_value, -sensor_unit, -transmitter_serial) %>% 
  rename(transmitter = transmitter_name) %>% 
  mutate(
    receiver = sub(".*-", "", receiver) # use regex to remove everything before the receiver number 549803 i.e. (VR2AR-) and replace with "" nothing
    )
  

# lets interrogate each transmitter first 

detections %>% distinct(transmitter)

# first hitch - we've got 9 transmitters but only 5 in our biometrics 

detections %>% group_by(transmitter) %>% summarise(detections = n())

### lets compare which transmitters are in the biometrics df
### alternatives using the %in% operator can also be used but
### tidy anti and semi join operators are a little weasier on the eyes 

# look at the transmitters that are not in the biometrics file 

detections %>% 
  anti_join(
    biometrics,
    by = "transmitter"
  ) %>% 
  count(transmitter, sort = TRUE)

# look at the transmitter summaries that are in the biometrics file

detections %>% 
  semi_join(
    biometrics,
    by = "transmitter"
  ) %>% 
  count(transmitter, sort = TRUE)


# what is tag 5211a though? 

detections %>% 
  filter(transmitter %in% c("5211", "5211a")) %>% 
  count(transmitter)


detections %>% 
  filter(transmitter %in% c("5211", "5211a")) %>% 
  arrange(timestamp)


# if we look carefully it appears that the 5211a detections are duplicates of 5211 

# inspect the duplicated rows directly

detections %>% 
  filter(transmitter %in% c("5211", "5211a")) %>% 
  group_by(timestamp, station_name) %>% 
  filter(n() > 1) %>% 
  arrange(timestamp)

# so lets remove 5211a and the transmitters that we dont have metadata for 

detections <- detections %>% 
  filter(transmitter %in% biometrics$transmitter)

# or 

# detections <- detections %>% 
#   semi_join(
#     biometrics,
#     by = "transmitter"
#   )

detections %>% distinct(transmitter)

# are there any other gremlins in there? 

detections %>% 
  group_by(across(everything())) %>% 
  filter(n() > 1)

# in the output transmitter [2,319] suggests that there are this many duplicates
# if we * 2 we see that 5211 has exact duplicate number of rows in there 

### Duplicate detections are common when looking at telemetry datasets especially if 
### there has been manual movement between csv files previously or accidental import of 
### the same receiver into your main database 
### receiver distance can also play a role in duplicate detections where neighbouring 
### receivers within the same detection range can detect a transmitter at the same time 
### hence why we group across everything so the call checks the receiver name and id 
### Duplicates can also occur as a result of the time resolution if seconds are missing 
### and a detection occurs during the same minute 


# that should do it for detections qc for now so lets export 

#write.csv(detections, 'data/detections_clean.csv', row.names = FALSE)

##################################################################
##################################################################
###################### END DETECTION  ############################
##################################################################
##################################################################

# load the receiver metadata 

receiver <- readr::read_csv("data/receivers.csv")

# receiver metadata are often one of the most challenging parts of telemetry workflows.
# each row usually represents a physical receiver deployment, but these rows do not
# always translate cleanly into continuous station listening effort.

# receivers may be lost, recovered late, replaced before recovery, or redeployed
# with missing/approximate times. Because downstream analyses often rely on effort,
# we need to reconstruct when each station was actually listening.

# time ---> atap metadata is reported in SAST

head(receiver)

receiver <- receiver %>% 
  janitor::clean_names() %>% 
  mutate(
    receiver = sub(".*-", "", receiver_name),
    station_name = stringr::str_squish(station_name),
    installation_name = stringr::str_squish(installation_name),
    status = stringr::str_to_lower(status)
  ) %>% 
  select(-project_name, -receiver_name) %>% 
  rename(longitude = station_longitude,
         latitude = station_latitude)



# a note for receiver deployment times for downstream use in packages like actel and rsp
# when researchers haven't reported the deployment and recovery times, you will need to 
# create reasonable time windows where rollovers could have occurred as 
# these packages expect accurate times.

# lets have a look at the number of deployment rows for each receiver station

receiver %>% 
  count(station_name, sort = TRUE) %>% 
  print(n = 30)

# and check for any duplicate records

receiver %>% 
  duplicated() %>% 
  sum()

# check whether receivers have inconsistent coordinates across deployments
# this could happen when your marginal redeplpoyment coordinates have been 
# used so this checks to see if each station_name has the same coordinates

###### Remember that receivers and their ID numbers move around a study region ###### ###### ###### 
###### station_names stay constant so filter and check this using station name ###### ###### ###### 

receiver %>% 
  group_by(station_name) %>% 
  summarise(
    n_lat = n_distinct(latitude),
    n_lon = n_distinct(longitude)
  ) %>% 
  filter(
    n_lat > 1 | n_lon > 1
  )


### because some receivers may have been lost we might want to remove them 
### from later effort calculations


lost_receiver_periods <- receiver %>% 
  filter(status == "lost") %>% 
  select(
    station_name,
    receiver,
    deploymentdatetime_timestamp,
    recoverydatetime_timestamp,
    status
  )



print(lost_receiver_periods)

# this can look deceptive because lost receivers have an end date 
# but this typically takes the form of the next deployment date closing 
# the deployment window 

# but lets also check if there are there detections on receivers that were lost 

detections_on_lost_receivers <- detections %>% 
  inner_join(
    lost_receiver_periods,
    by = "station_name",
    relationship = "many-to-many"
  ) %>% 
  filter(
    timestamp >= deploymentdatetime_timestamp,
    timestamp <= recoverydatetime_timestamp
  )


print(detections_on_lost_receivers)

# would we keep detections from a receiver that is marked as lost but has somehow 
# been found and added to the dataset - bearing in mind that the actual recovery date could then
# be ambiguous? 

# lets remove 

receiver <- receiver %>% 
  filter(status != "lost")


# lets take a look at the time gaps between deployments 
# typically we'd expect hours to days, in some cases a week or so if you don't have spare units
# to roll immediately and have to wait for another weather window 

receiver <- receiver %>% 
  arrange(station_name, deploymentdatetime_timestamp) %>% 
  group_by(station_name) %>% 
  mutate(
    previous_recovery = lag(recoverydatetime_timestamp), # use the lag command to identify the consecutive time gaps
    deployment_gap_days = as.numeric(
      difftime(
        deploymentdatetime_timestamp,
        previous_recovery,
        units = "days"
      )
    )
  ) %>% 
  arrange(desc(deployment_gap_days))


receiver_gaps <- receiver %>% 
  ungroup() %>% 
  arrange(station_name, deploymentdatetime_timestamp) %>% 
  group_by(station_name) %>% 
  mutate(
    receiver_a = receiver,
    deploymentdatetime_timestamp_a = deploymentdatetime_timestamp,
    recoverydatetime_timestamp_a = recoverydatetime_timestamp,
    
    receiver_b = lead(receiver),
    deploymentdatetime_timestamp_b = lead(deploymentdatetime_timestamp),
    recoverydatetime_timestamp_b = lead(recoverydatetime_timestamp),
    
    gap_days = as.numeric(
      difftime(
        deploymentdatetime_timestamp_b,
        recoverydatetime_timestamp_a,
        units = "days"
      )
    )
  ) %>% 
  filter(
    !is.na(gap_days),
    gap_days > 21
  ) %>% 
  select(
    station_name,
    receiver_a,
    deploymentdatetime_timestamp_a,
    recoverydatetime_timestamp_a,
    receiver_b,
    deploymentdatetime_timestamp_b,
    recoverydatetime_timestamp_b,
    gap_days
  ) %>% 
  arrange(desc(gap_days))



# this is another example of challenging receiver data and the need to dig into it 
# we can see that indeed there are receviers that fall into the criteia above, but in some 
# cases receivers were recovered 100s of days later. Now this could've been for a number of reasons 
# an ar that didnt respond and was picked up on another rollover. a receiver that was missed on 
# a dive and a new unit redeployed on a new mooring 

# the challenge now is how do we deal with these strange deployment periods 
# where possible we want to have a single continous deployment window for a station
# but overlaps, where a receiver was maybe recovered later or gaps in these time series 
# may occur because units were never recovered 

# lets start with overlapping deployments. We want to compare stations were deployment 
# windows coincide and for how long. in some cases this can be pretty short and we can 
# assume a pretty continuous period, In others, it might be 100s of days where a receiver 
# was recovered way later, but data were uploaded to the database and therefore we can 
# also treat these periods as continuous and collapse the row by row deployment 
# records later 


receiver_overlaps <- receiver %>% 
  ungroup() %>% 
  mutate(row_id = row_number()) %>% 
  inner_join(
    receiver %>% 
      ungroup() %>% 
      mutate(row_id_compare = row_number()),
    by = "station_name",
    suffix = c("_a", "_b"),
    relationship = "many-to-many"
  ) %>% 
  filter(
    row_id < row_id_compare,
    deploymentdatetime_timestamp_a < recoverydatetime_timestamp_b,
    recoverydatetime_timestamp_a > deploymentdatetime_timestamp_b
  ) %>% 
  mutate(
    overlap_start = pmax(
      deploymentdatetime_timestamp_a,
      deploymentdatetime_timestamp_b
    ),
    
    overlap_end = pmin(
      recoverydatetime_timestamp_a,
      recoverydatetime_timestamp_b
    ),
    
    overlap_days = as.numeric(
      difftime(
        overlap_end,
        overlap_start,
        units = "days"
      )
    )
  ) %>% 
  filter(overlap_days > 1) %>% 
  select(
    station_name,
    
    receiver_a,
    deploymentdatetime_timestamp_a,
    recoverydatetime_timestamp_a,
    
    receiver_b,
    deploymentdatetime_timestamp_b,
    recoverydatetime_timestamp_b,
    
    overlap_start,
    overlap_end,
    overlap_days
  ) %>% 
  arrange(desc(overlap_days))


# and now lets have a look at some of the stations that have significant gaps
# which would then benefit from having multiple deployment records in the df 
# so that we can more accurately account for listening effort later on 


gap_tolerance_days <- 21

receiver_effort <- receiver %>% 
  ungroup() %>% 
  filter(status != "lost") %>% 
  arrange(station_name, deploymentdatetime_timestamp) %>% 
  group_by(station_name) %>% 
  mutate(
    recovery_for_effort = coalesce(
      recoverydatetime_timestamp,
      deploymentdatetime_timestamp
    ),
    
    recovery_num = as.numeric(recovery_for_effort),
    
    running_effort_end_num = cummax(recovery_num),
    
    previous_effort_end = as.POSIXct(
      lag(running_effort_end_num),
      origin = "1970-01-01",
      tz = "Africa/Johannesburg"
    ),
    
    gap_from_previous = as.numeric(
      difftime(
        deploymentdatetime_timestamp,
        previous_effort_end,
        units = "days"
      )
    ),
    
    new_effort_window = case_when(
      is.na(gap_from_previous) ~ 1,
      gap_from_previous > gap_tolerance_days ~ 1,
      TRUE ~ 0
    ),
    
    effort_window = cumsum(new_effort_window)
  ) %>% 
  group_by(station_name, effort_window) %>% 
  summarise(
    effort_start = min(deploymentdatetime_timestamp, na.rm = TRUE),
    effort_end = max(recoverydatetime_timestamp, na.rm = TRUE),
    n_receiver_deployments = n(),
    receivers_used = paste(unique(receiver), collapse = ", "),
    .groups = "drop"
  ) %>% 
  arrange(station_name, effort_start)


# often times a researcher may assum the min and max deployemnts 
# are suffucinet to calcualte listening effort so lets look at the 
# assumed receiver effort and then compare against our
# effort corrected deployment history


assumed_station_effort <- receiver %>% 
  filter(status != "lost") %>% 
  group_by(station_name) %>% 
  summarise(
    assumed_start = min(deploymentdatetime_timestamp, na.rm = TRUE),
    assumed_end = max(recoverydatetime_timestamp, na.rm = TRUE),
    
    assumed_effort_days = as.numeric(
      difftime(
        assumed_end,
        assumed_start,
        units = "days"
      )
    ),
    .groups = "drop"
  )

# based on our cleaned receiver effort lets calcualte the same as assumed

cleaned_station_effort <- receiver_effort %>% 
  mutate(
    effort_days = as.numeric(
      difftime(
        effort_end,
        effort_start,
        units = "days"
      )
    )
  ) %>% 
  group_by(station_name) %>% 
  summarise(
    cleaned_effort_days = sum(effort_days, na.rm = TRUE),
    n_effort_windows = n(),
    .groups = "drop"
  )

# and now lets compare to see whether it was worth all this effort 

effort_comparison <- assumed_station_effort %>% 
  left_join(
    cleaned_station_effort,
    by = "station_name"
  ) %>% 
  mutate(
    overestimation_days =
      assumed_effort_days - cleaned_effort_days,
    
    overestimation_percent =
      (
        overestimation_days /
          cleaned_effort_days
      ) * 100
  ) %>% 
  arrange(desc(overestimation_days))


overall_effort_summary <- effort_comparison %>% 
  summarise(
    total_assumed_days = sum(assumed_effort_days, na.rm = TRUE),
    total_cleaned_days = sum(cleaned_effort_days, na.rm = TRUE),
    
    total_overestimate_days =
      total_assumed_days - total_cleaned_days,
    
    overall_overestimate_percent =
      (
        total_overestimate_days /
          total_cleaned_days
      ) * 100
  )

overall_effort_summary


### a quick plot to illustrate the gremlins

effort_comparison %>% 
  slice_max(overestimation_days, n = 10) %>% 
  select(station_name, assumed_effort_days, cleaned_effort_days) %>% 
  pivot_longer(
    -station_name,
    names_to = "effort_type",
    values_to = "effort_days"
  ) %>% 
  mutate(
    effort_type = recode(
      effort_type,
      assumed_effort_days = "Assumed",
      cleaned_effort_days = "Cleaned"
    )
  ) %>% 
  ggplot(aes(
    x = reorder(station_name, effort_days),
    y = effort_days,
    fill = effort_type
  )) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    x = "Station",
    y = "Effort days",
    fill = NULL
  ) +
  theme_minimal()

# how many uniqie receivers stations have been deployed in the ATAP network over time

receiver_effort %>% distinct(station_name) %>% count()

# lets export our receiver effort to use for later analysis 

write.csv(receiver_effort, 'data/receiver_effort_clean.csv', row.names = FALSE)
