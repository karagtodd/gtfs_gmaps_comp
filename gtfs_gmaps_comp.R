# USING GTFS TO CALCULATE TRAVEL TIME SAVINGS POTENTIAL OF BUS PREFERENTIAL TREATMENTS 
# Spring 2020
# Daniel Arias, Kara Todd, Jennifer Krieger, Spencer Maddox, & Pearse Haley
# Advised by Dr. Kari Watkins & Dr. Simon Berrebi

# Comparing GTFS time savings estimates with Google Maps Distance Matrix API time savings estimates
### Uses GTFS time savings estimates calculated by Daniel
### Calculates time savings based on Google Distance Matrix API estimates
### Compares the time savings estimates on each segment to validate our methodology
### Author: Kara Todd
### April 19, 2020


setwd("~/Documents/Georgia Tech/Spring 2020/Bus Lanes")

library(dplyr)
library(sf)
library(leaflet)
library(chron)
library(ggplot2)
library(markdown)
library(gmapsdistance)


##### IMPORT SAVED DATA ON GTFS TIME SAVINGS AND RIDERSHIP #####
seg_data <- read.csv("[GTFS TIME AND RIDERSHIP DATA.csv", stringsAsFactors = FALSE)

# calc min time elapsed, join
sec_minimum <- seg_data %>% group_by(start_id, end_id) %>% 
  summarize(sec_minimum = min(sec_elapsed), # identify the shortest time allowed in schedule between each stop pair
            sec_maximum = max(sec_elapsed)) # identify the longest time allowed in schedule between each stop pair
sec_minimum$id <- paste0(sec_minimum$start_id, sec_minimum$end_id)
sec_minimum <- sec_minimum[,c("id", "sec_minimum", "sec_maximum")]
seg_data$id <- paste0(seg_data$start_id, seg_data$end_id)

summary(sec_minimum$sec_maximum - sec_minimum$sec_minimum)

seg_data_save <- left_join(seg_data, sec_minimum, by = "id")


# create sec savings vars 
seg_data_save$sec_savings <- seg_data_save$sec_elapsed - seg_data_save$sec_minimum
seg_data_save$person_sec_savings <- seg_data_save$sec_savings * seg_data_save$wkday_riders

# shorten direction values
seg_data_save$direction <- ifelse(seg_data_save$direction == "Northbound", "NB",
                                  ifelse(seg_data_save$direction == "Southbound", "SB",
                                         ifelse(seg_data_save$direction == "Eastbound", "EB",
                                                ifelse(seg_data_save$direction == "Westbound", "WB",
                                                       "Clockwise"))))

# aggregate from trips to unique stop pair
seg_data_agg <- seg_data_save %>% group_by(start_id, end_id) %>% 
  mutate(route = paste0(as.character(unique(route)), collapse = ","),
         direction = paste0(unique(direction), collapse = ",")) %>%
  summarize(route = max(route),
            direction = max(direction),
            start_name = max(start_name),
            end_name = max(end_name),
            wkday_riders = round(sum(wkday_riders)),
            sec_savings = round(sum(sec_savings)),
            person_sec_savings = round(sum(person_sec_savings)))

seg_data_agg$id <- paste0(seg_data_agg$start_id, seg_data_agg$end_id)

# join GTFS max and min TT's
seg_data_agg <- left_join(seg_data_agg, sec_minimum, by = "id")
gtfs <- seg_data_agg # rename for clarity when comparing with Google Maps time savings below



##### USE GOOGLE DISTANCE MATRIX API TO CALCULATE ALTERNATIVE TRAVEL TIME SAVINGS ESTIMATES #####
# read in saved data or continue with script below
gmaps <- read.csv("[GOOGLE API TIME SAVINGS.csv")

# returns distance in meters and time in seconds
set.api.key("[ENTER KEY HERE]")

df <- st_read("[SEGMENT GEOMETRY].shp") # read in spatial data

# pull O & D coordinates for trip between each stop pair
origin = c()
dest = c()
for(i in 1:nrow(df)){
  start_lat = as.numeric(st_bbox(df[i,]$geom)$ymin)
  start_long = as.numeric(st_bbox(df[i,]$geom)$xmin)
  end_lat = as.numeric(st_bbox(df[i,]$geom)$ymax)
  end_long = as.numeric(st_bbox(df[i,]$geom)$xmax)
  
  origin = c(origin, paste0(start_lat, "+", start_long))
  dest = c(dest, paste0(end_lat, "+", end_long))
}

# Google requires future date to estimate travel times
# set date as random Wednesday in Sept 2020
# time is in UTC
# rush hour TT at 5 PM EDT (21:00 UTC)
# free flow TT at 3 AM EDT (07:00 UTC)
tt_rh = gmapsdistance(origin = origin,
                      destination = dest,
                      mode = "driving",
                      key = get.api.key(),
                      dep_date = "2020-09-14",
                      dep_time = "21:00:00",
                      combinations = "pairwise",
                      traffic_model = "best_guess")

tt_ff = gmapsdistance(origin = origin,
                      destination = dest,
                      mode = "driving",
                      key = get.api.key(),
                      dep_date = "2020-09-14",
                      dep_time = "07:00:00",
                      combinations = "pairwise",
                      traffic_model = "best_guess")

# add rush hour and free flow travel time columns to df
df$tt_rh <- tt_rh$Time[,3]
df$tt_ff <- tt_ff$Time[,3]

# add tt savings column to df
df$tt_savings = df$tt_rh - df$tt_ff

# rename for clarity when comparing with GTFS time savings
gmaps <- df


##### JOIN AND COMPARE GTFS AND GMAPS DATA #####
# create id in gmaps to join
gmaps$id <- paste0(gmaps$start_id, gmaps$end_id)
gtfs$id <- as.character(gtfs$id)

length(unique(gtfs$id))-length(unique(gmaps$id)) # check for missing estimtates from gmaps

# figure out which are missing from gmaps
comp <- left_join(gtfs, gmaps, by = "id")
missing <- which(is.na(comp$tt_rh)) #284 rows from GTFS are missing a gmaps comparison

no_comp <- comp[missing,]
no_comp <- no_comp[, 1:11]
colnames(no_comp) <- colnames(gtfs)

# eliminate rows without comparisons
comp <- na.omit(comp)
comp <- comp %>% select(-c("X", "start_id.y", "end_id.y", "route.y", "direction.y", "start_name.y", "end_name.y"))
names(comp)[1:7] <- names(gtfs)[1:7]

# compare gmaps rush hour estimate to GTFS max 
comp$rh_comp <- comp$tt_rh - comp$sec_maximum # assume GTFS max padding reflects most congested travel time
summary(comp$rh_comp)

nrow(comp[comp$rh_comp > 0,]) # count of gmaps > GTFS
per_gtfs_under <- round(100*nrow(comp[comp$rh_comp > 0,])/nrow(comp), 2)

nrow(comp[comp$rh_comp < 0,]) # count of gmaps < GTFS
per_gtfs_over <- round(100*nrow(comp[comp$rh_comp < 0,])/nrow(comp), 2)

cat("GTFS underestimates ", paste0(per_gtfs_under, "% of segments, according to Google Maps",
                                   "\n", "GTFS overestimates ", paste0(per_gtfs_over, "% of segments, according to Google Maps")))

# summarize segments where gmaps > GTFS
summary(comp[comp$rh_comp > 0,]$rh_comp)
plot(density(comp[comp$rh_comp > 0,]$rh_comp))

# summarize segments where gmaps  < GTFS
summary(abs(comp[comp$rh_comp < 0,]$rh_comp))
plot(density(comp[comp$rh_comp < 0,]$rh_comp))

# create comparison plots
plot(density(comp$rh_comp), main = "Comparison of Peak Travel Time Estimates \n (Google Maps - GTFS)",
     xlab  = "Travel Time Difference (seconds)", lwd = 2, xlim = c(-500,500))

plot(density(comp$sec_maximum), main = "Peak Travel Time Estimates",
     col = "red", lwd = 2,
     xlab = "Estimated Peak Travel Time (seconds)",
     xlim = c(0,750))
lines(density(comp$tt_rh), col = "blue", lwd = 2)
legend('topright', legend = c("GTFS", "Google Maps"),
       col = c("red", "blue"),
       lwd = 3)


# percentiles of person-time savings
comp$savings_pctile <- floor(rank(comp$person_sec_savings) * 100 / length(comp$person_sec_savings))

summary(comp$savings_pctile)
summary(comp$sec_savings)
plot(density(comp$sec_savings))

summary(seg_data_agg$person_sec_savings)
plot(density(log(seg_data_agg$person_sec_savings))) # log transformed to check 0's

###### CLOSER LOOK AT TOP 10% OF SEGMENTS (in terms of GTFS savings) #####
top10 <- comp[comp$savings_pctile >= 90,]

# create comparison plots
plot(density(top10$rh_comp), main = "Comparison of Peak Travel Time Estimates: Priority Segments \n (Google Maps - GTFS)",
     xlab  = "Travel Time Difference (seconds)", lwd = 2, xlim = c(-500,500))

plot(density(top10$sec_maximum), main = "Peak Travel Time Estimates: Priority Segments",
     col = "red", lwd = 2,
     xlab = "Estimated Peak Travel Time (seconds)",
     xlim = c(0,750))
lines(density(top10$tt_rh), col = "blue", lwd = 2)
legend('topright', legend = c("GTFS", "Google Maps"),
       col = c("red", "blue"),
       lwd = 3)

# summarize segments where gmaps > GTFS
summary(top10[top10$rh_comp > 0,]$rh_comp)

# summarize segments where gmaps < GTFS
summary(abs(top10[top10$rh_comp < 0,]$rh_comp))

nrow(top10[top10$rh_comp > 0,]) # count of gmaps > GTFS
top10_per_gtfs_under <- round(100*nrow(top10[top10$rh_comp > 0,])/nrow(top10), 2)

nrow(top10[top10$rh_comp < 0,]) # count of gmaps < GTFS
top10_per_gtfs_over <- round(100*nrow(top10[top10$rh_comp < 0,])/nrow(top10), 2)

cat("GTFS underestimates ", paste0(top10_per_gtfs_under, "% of segments, according to Google Maps",
                                   "\n", "GTFS overestimates ", paste0(top10_per_gtfs_over, "% of segments, according to Google Maps")))


##### compare peak hour savings estimates #####
comp$gmaps_save <- comp$tt_rh - comp$sec_minimum
comp$sec_savings_ph <- comp$sec_maximum - comp$sec_minimum

comp$gmaps_pctile <- floor(rank(comp$gmaps_save) * 100 / length(comp$gmaps_save))
comp$ph_pctile <- floor(rank(comp$sec_savings_ph) * 100 / length(comp$sec_savings_ph))

length(which(comp$gmaps_pctile >= 90 & comp$ph_pctile >= 90))
length(which(comp$gmaps_pctile >= 90))
length(which(comp$ph_pctile >= 90))



##### MAP PEAK HOUR SAVINGS ####
seg_data_geo <- st_read("[SEGMENT GEOMETRY].gpkg")

seg_data_geo$id <- paste0(seg_data_geo$start_id, seg_data_geo$end_id)
comp_geo <- right_join(seg_data_geo, comp, by = "id")
comp_geo <- comp_geo[, -c(17:23, 26:31)]
names(comp_geo)[2:8] <- names(comp)[1:7]
names(comp_geo)[11:14] <- names(comp)[13:16]

comp_geo_less1 <- comp_geo[comp_geo$seg_dist<1,] # looking only at segments less than 1 mile long
comp_geo_less1$gmaps_pctile <- floor(rank(comp_geo_less1$gmaps_save) * 100 / length(comp_geo_less1$gmaps_save))
comp_geo_less1$ph_pctile <- floor(rank(comp_geo_less1$sec_savings_ph) * 100 / length(comp_geo_less1$sec_savings_ph))

# create dataframe of segments <1 mi that are in top 10% by both metrics
comp_geo_less1_overlap <- comp_geo_less1[comp_geo_less1$gmaps_pctile >=90 & comp_geo_less1$ph_pctile >= 90,]
comp_geo_less1_overlap$avg_pctile <- (comp_geo_less1_overlap$gmaps_pctile + comp_geo_less1_overlap$ph_pctile)/2


leaflet_map_comp <- function(gtfs, gmaps, bins) {
  # define symbology
  cscale_blue <- colorBin("Blues", reverse = FALSE, domain = gmaps$gmaps_pctile, bins = bins)
  cscale_red <- colorBin("Reds", reverse = FALSE, domain = gtfs$ph_pctile, bins = bins)
  
  # map
  leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% 
    # add lines for GTFS savings
    addPolylines(data = gtfs,
                 color = cscale_red(gtfs$ph_pctile), opacity = 0.7, weight = 2,
                 popup = paste0("routes: ", gtfs$route, "<br>",
                                "from: " , gtfs$start_name, "<br>",
                                "to: " , gtfs$end_name, "<br>",
                                "Direction: ", gtfs$direction, "<br>",
                                "Euclidean Distance (mi): ", gtfs$euclid_mi, "<br>",
                                "ridership: ", gtfs$wkday_riders, "<br>",
                                "peak hour seconds saved: ", gtfs$sec_savings_ph),
                 group = "GTFS")  %>%
    
    # add lines for gmaps savings               
    addPolylines(data = gmaps,
                 color = cscale_blue(gmaps$gmaps_pctile), opacity = 0.7, weight = 2,
                 popup = paste0("routes: ", gmaps$route, "<br>",
                                "from: " , gmaps$start_name, "<br>",
                                "to: " , gmaps$end_name, "<br>",
                                "Direction: ", gmaps$direction, "<br>",
                                "Euclidean Distance (mi): ", gmaps$euclid_mi, "<br>",
                                "ridership: ", gmaps$wkday_riders, "<br>",
                                "peak hour seconds saved: ", gmaps$gmaps_save),
                 group = "Google Maps")  %>%

    addLayersControl(overlayGroups = c("GTFS", "Google Maps")) %>% 
    
    addLegend(pal = cscale_red,
              values = gtfs$ph_pctile,
              opacity = 0.7,
              title = "Percentile of Peak Hour Savings: </br> GTFS",
              position = "bottomleft",
              group = "GTFS") %>% 
    
    addLegend(pal = cscale_blue,
              values = gmaps$gmaps_pctile,
              opacity = 0.7,
              title = "Percentile of Peak Hour Savings: </br> Google Maps",
              position = "bottomleft",
              group = "Google Maps")
}


leaflet_map_comp2 <- function(gtfs, gmaps, overlap, bins) {
  # define symbology
  cscale_blue <- colorBin("Blues", reverse = FALSE, domain = gmaps$gmaps_pctile, bins = bins)
  cscale_red <- colorBin("Reds", reverse = FALSE, domain = gtfs$ph_pctile, bins = bins)
  cscale_purple <- colorBin("Purples", reverse = FALSE, domain = overlap$avg_pctile, bins = bins)
  
  # map
  leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% 
    # add lines for GTFS savings
    addPolylines(data = gtfs,
                 color = cscale_red(gtfs$ph_pctile), opacity = 0.7, weight = 2,
                 popup = paste0("routes: ", gtfs$route, "<br>",
                                "from: " , gtfs$start_name, "<br>",
                                "to: " , gtfs$end_name, "<br>",
                                "Direction: ", gtfs$direction, "<br>",
                                "Euclidean Distance (mi): ", gtfs$euclid_mi, "<br>",
                                "ridership: ", gtfs$wkday_riders, "<br>",
                                "peak hour seconds saved: ", gtfs$sec_savings_ph),
                 group = "GTFS")  %>%
   
     # add lines for gmaps savings               
    addPolylines(data = gmaps,
                 color = cscale_blue(gmaps$gmaps_pctile), opacity = 0.7, weight = 2,
                 popup = paste0("routes: ", gmaps$route, "<br>",
                                "from: " , gmaps$start_name, "<br>",
                                "to: " , gmaps$end_name, "<br>",
                                "Direction: ", gmaps$direction, "<br>",
                                "Euclidean Distance (mi): ", gmaps$euclid_mi, "<br>",
                                "ridership: ", gmaps$wkday_riders, "<br>",
                                "peak hour seconds saved: ", gmaps$gmaps_save),
                 group = "Google Maps")  %>%
    
    # add lines for segments that are in both sets
    addPolylines(data = overlap,
                 color = cscale_purple(overlap$avg_pctile), opacity = 1, weight = 2,
                 popup = paste0("routes: ", overlap$route, "<br>",
                                "from: " , overlap$start_name, "<br>",
                                "to: " , overlap$end_name, "<br>",
                                "Direction: ", overlap$direction, "<br>",
                                "Euclidean Distance (mi): ", overlap$euclid_mi, "<br>",
                                "ridership: ", overlap$wkday_riders, "<br>",
                                "peak hour seconds saved: ", overlap$gmaps_save),
                 group = "Overlap")  %>%
    
   
    addLayersControl(overlayGroups = c("GTFS", "Google Maps", "Overlap")) %>% 
    
    addLegend(pal = cscale_red,
              values = gtfs$ph_pctile,
              opacity = 0.7,
              title = "Percentile of Peak Hour Savings: </br> GTFS",
              position = "bottomleft",
              group = "GTFS") %>% 
    
    addLegend(pal = cscale_blue,
            values = gmaps$gmaps_pctile,
            opacity = 0.7,
            title = "Percentile of Peak Hour Savings: </br> Google Maps",
            position = "bottomleft",
            group = "Google Maps") %>% 
    
    addLegend(pal = cscale_purple,
              values = overlap$avg_pctile,
              opacity = 0.7,
              title = "Avg. Percentile of Peak Hour Savings: </br> Google Maps & GTFS",
              position = "bottomleft",
              group = "Overlap")
}


bins <- c(90,93,95,97,99,100)
gtfs_top10 <- comp_geo %>% filter(ph_pctile >= min(bins))
gmaps_top10 <- comp_geo %>% filter(gmaps_pctile >= min(bins))
comp_map <- leaflet_map_comp(gtfs_top10, gmaps_top10, bins)
comp_map

gtfs_top10_less1 <- comp_geo_less1 %>% filter(ph_pctile >= min(bins))
gmaps_top10_less1 <- comp_geo_less1 %>% filter(gmaps_pctile >= min(bins))
comp_map_less1 <- leaflet_map_comp(gtfs_top10_less1, gmaps_top10_less1, bins)
comp_map_less1

# map overlapping segments less than 1 mile
comp_map_overlap <- leaflet_map_comp2(gtfs_top10_less1, gmaps_top10_less1, comp_geo_less1_overlap, bins)
comp_map_overlap


htmlwidgets::saveWidget(comp_map, file="GTFS_gmaps_comparison.html", selfcontained = TRUE)
rpubsUpload("Comparison of GTFS and Google Maps Time Savings (Top 10% of Segments)", "GTFS_gmaps_comparison.html")


