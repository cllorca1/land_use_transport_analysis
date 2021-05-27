pacman::p_load(readr, dplyr, ggplot2, tidyr)

mito_folder = "c:/models/mito/muc/mitoMunich/"

scenario = "test"

year = 2011


trip_filename = paste(mito_folder, "scenOutput/", scenario , "/", year, "/microData/trips.csv", sep = "" )
trips = read_csv(trip_filename)


#extract return trips

purposes_hb = c("HBW", "HBO", "HBS", "HBE")
trips_with_return = trips %>% filter(purpose %in% purposes_hb)
trips = trips %>% mutate(departure_time_return = 0)
trips_with_return = trips_with_return %>% mutate(departure_time = departure_time_return)
trips_with_return = trips_with_return %>% mutate(departure_time_return = 1)

trips = trips %>% bind_rows(trips_with_return)

trips = trips %>% mutate(beeline_distance = sqrt((originX - destinationX)^2 + (originY - destinationY)^2))


##spatial filter

##zones in Munich origin and destination: 

zone_attributes = read_csv(paste(mito_folder, "input/zoneSystem.csv", sep = ""))

zones_muc = zone_attributes %>% filter(ID_city == 9162000)


trips = trips %>% filter(origin %in% zones_muc$Zone,destination %in% zones_muc$Zone)



summary(trips$departure_time)

time_interval = 15

time_bins = seq(from = 0, to = 24*60, by = time_interval)

trips = trips %>% mutate(departure_time_bin = cut(departure_time, time_bins, labels = FALSE))
trips = trips %>% mutate(departure_time_bin_h = (departure_time_bin - 1) * time_interval / 60)

trips_by_tod = trips %>% group_by(mode, departure_time_bin_h) %>% summarise(count = n())

ggplot(trips_by_tod, aes(x=departure_time_bin_h, y = count, color = mode)) +
  geom_line() + geom_point()


pt = c("bus", "train", "tramOrMetro")

pt_trips_by_tod = trips_by_tod %>% filter(mode %in% pt)

ggplot(pt_trips_by_tod, aes(x=departure_time_bin_h, y = count, color = mode)) + geom_line() + geom_point()


pt_trips_by_tod_wide = pt_trips_by_tod %>% spread(mode, count)
pt_trips_by_tod_wide = pt_trips_by_tod_wide %>% mutate(total_pt = bus + tramOrMetro + train) 

write.table(pt_trips_by_tod_wide, "clipboard", row.names = F, sep = "\t")


trips  %>% group_by(mode) %>% summarize(count = n())


##obtain average speeds by mode

trips = trips %>%
  mutate(speed_car = distance/time_auto * 60) %>% 
  mutate(speed_train = distance/time_train * 60) %>% 
  mutate(speed_tram_metro = distance/time_tram_metro * 60) %>% 
  mutate(speed_bus = distance/time_bus * 60) %>%
  mutate(beeline_speed_car = beeline_distance/time_auto * 60 / 1000) %>% 
  mutate(beeline_speed_train = beeline_distance/time_train * 60 / 1000) %>% 
  mutate(beeline_speed_tram_metro = beeline_distance/time_tram_metro * 60/ 1000) %>% 
  mutate(beeline_speed_bus = beeline_distance/time_bus * 60 / 1000) 


speeds = trips %>% group_by(mode) %>% summarize(mean(speed_car),
                                       mean(speed_train), 
                                       mean(speed_tram_metro), 
                                       mean(speed_bus),
                                       mean(beeline_speed_car),
                                       mean(beeline_speed_train), 
                                       mean(beeline_speed_tram_metro), 
                                       mean(beeline_speed_bus))

summary(trips$beeline_distance)
summary(trips$distance)

write.table(speeds, "clipboard", row.names = F, sep = "\t")
