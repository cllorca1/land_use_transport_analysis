pacman::p_load(readr, dplyr, ggplot2, tidyr)

mito_folder = "c:/models/mito/muc/mitoMunich/"

scenario = "new_tod_20210421_test"

year = 2011


trip_filename = paste(mito_folder, "scenOutput/", scenario , "/", year, "/microData/trips.csv", sep = "" )
trips = read_csv(trip_filename)

speed_walk = 5
speed_bike = 13

trips = trips %>%
  mutate(time = if_else(mode == "autoDriver" | mode == "autoPassenger", time_auto, 
                        if_else(mode == "train", time_train, 
                                if_else(mode == "bus", time_bus, 
                                        if_else(mode== "tramOrMetro", time_tram_metro, 
                                                if_else(mode== "walk", distance/speed_walk * 60, 
                                                       distance/speed_bike * 60))))))


interval_min = 15
intervals = 24 * 60 / 15

trip_times = trips %>% select(id, purpose, person, departure_time, departure_time_return, time)

trip_times = trip_times %>% 
  mutate(end_time = if_else(!is.na(departure_time_return) ,departure_time_return + time, departure_time + time))


# 
# 
# trip_times = trip_times %>% group_by(person) %>% mutate(n = n())
# 
# trip_times_multiple = trip_times %>% filter(n > 2)

sample_person = sample(trip_times$person, size = 50000)
trip_times_copy = trip_times %>% filter(person  %in% sample_person)
trip_times_copy$my_id = 1:nrow(trip_times_copy)


time_usage_matrix = matrix(nrow = nrow(trip_times_copy), ncol = intervals)

for (i in 1:nrow(trip_times_copy)){
  start = trip_times_copy$departure_time[i]
  end = trip_times_copy$end_time[i]
  my_id = trip_times_copy$my_id[i]
  for (interval in 1:intervals){
    time = interval*interval_min
    #still just an approximation
    if (start < (time - interval_min) & end >= time){
      time_usage_matrix[my_id,interval] = 1
    } else {
      time_usage_matrix[my_id,interval] = 0
    }
  }
  if (i %% 10000 == 0){
    print(i)
  }
}

#cuidado!
time_usage_matrix = data.frame(time_usage_matrix, my_id = 1:nrow(time_usage_matrix))

trip_times_copy = trip_times_copy %>% left_join(time_usage_matrix, by = "my_id")

persons = trip_times_copy %>%
  group_by(person) %>%
  select(-purpose, -departure_time, -time, -end_time, -departure_time_return, -id, -my_id) %>%
  summarize_all(sum)

write.table(persons, "clipboard-100000k", sep = "\t", row.names = F)

persons_2 = trip_times_copy %>%
  filter(purpose != "NHBW", purpose != "NHBO") %>%
  group_by(person) %>%
  select(-purpose, -departure_time, -time, -end_time, -departure_time_return, -id, -my_id) %>%
  summarize_all(sum)

write.table(persons_2, "clipboard-100000k", sep = "\t", row.names = F)


purposes = trip_times_copy %>%
  group_by(purpose) %>%
  select(-person, -departure_time, -time, -end_time, -departure_time_return, -id, -my_id) %>%
  summarize_all(sum)

purposes$purpose = factor(purposes$purpose, levels = c("HBW", "HBE", "HBS", "HBO", "NHBW", "NHBO"))

write.table(purposes, "clipboard-100000k", sep = "\t", row.names = F)


ggplot(persons , aes(x=time/60)) + geom_histogram(binwidth = 1) + xlim(-24,24)


summary(as.factor(trips$mode))
