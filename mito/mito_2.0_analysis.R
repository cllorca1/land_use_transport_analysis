pacman::p_load(readr, dplyr, ggplot2, tidyr, scales)

mito_folder = "c:/models/mito/muc/mitoMunich/"

scenarios = c("base_1_0_v2",
              #"telework_1_0_v2",
              "base_2.1_calibrated"#, 
              #"telework_2_1"
              )
scenario_names = c("base 1.0",
                   #"telework",
                   "base 2.0"#,
                   #"telework"
                   )

#scenarios = c("base_1_0_v2","supercongestion_1_0_v2", "base_2.1_calibrated", "supercongestion_2_1")
# scenario_names = c("base","congestion", "base", "congestion")
purposes_ordered = c("HBW", "HBE", "HBO", "HBS", "NHBW", "NHBO")

versions = c(1,1,2,2)

classification = data.frame(scenario = scenarios, scen_name = scenario_names, version = versions)


year = 2011

trips = data.frame()
persons = data.frame()

pp = read_csv(paste(mito_folder, "microData/pp_2011.csv", sep  =""))
worker_ids = pp %>% filter(occupation == 1) %>% select(id)

for (scenario in scenarios){
  trip_filename = paste(mito_folder, "scenOutput/", scenario , "/", year, "/microData/trips.csv", sep = "" )
  this_trips = read_csv(trip_filename)
  this_trips = this_trips %>% mutate(time = if_else(mode == "autoDriver" | mode == "autoPassenger", time_auto,
                                                    if_else(mode == "train", time_train, 
                                                            if_else(mode == "tramOrMetro", time_tram_metro, 
                                                                    if_else(mode == "bus", time_bus, 
                                                                            if_else(mode == "walk", distance/5 * 60, 
                                                                                    if_else(mode == "bicycle" , distance / 12.5 * 60, -1)))))))
  
  #check = this_trips %>% group_by(mode) %>% summarize(mean(distance), mean(time))
  #print(check)
  this_trips = this_trips %>%
    mutate(purpose2 = if_else(purpose == "HBW" | purpose == "HBE", "mandatory", purpose)) 
  this_persons = this_trips %>%  group_by(person, purpose2) %>% summarize(trips = n(), time = sum(time))
  this_persons$scenario = scenario
  persons = persons %>% bind_rows(this_persons)
  
  this_trips$scenario = scenario
  trips = trips %>% bind_rows(this_trips)
  
  print(scenario)
}


trips =  trips %>% mutate(is_worker = if_else(person %in% worker_ids$id, "employed", "non-employed"))

summary = trips %>% group_by(purpose, scenario, is_worker) %>%
  summarize(count = n(),
            distance_mean = mean(distance, na.rm = T),
            sum_distance = sum(distance),
            time_mean  =mean(time, na.rm = T),
            sum_time = sum(time))


summary$purpose = factor(summary$purpose, levels = purposes_ordered)





summary = summary %>% left_join(classification)
summary$scenario = factor(summary$scenario, levels = scenarios)

ggplot(summary, aes(group = scenario, x= purpose, fill = scenario, y = count/1e6) ) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_brewer(palette = "Paired") + 
  theme_bw() + 
  xlab("Purpose") + ylab("Number of trips (Million)") + 
  facet_grid(is_worker~.)

ggplot(summary, aes(group = scenario, x= purpose, fill = scenario, y = time_mean) ) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_brewer(palette = "Paired") + 
  theme_bw() + 
  xlab("Purpose") + ylab("Average travel time (min)") + 
  facet_grid(is_worker~.)


ggplot(summary, aes(group = scenario, x= purpose, fill = scenario, y = distance_mean) ) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_brewer(palette = "Paired") + 
  theme_bw() + 
  xlab("Purpose") + ylab("Average trip distance (km)") + 
  facet_grid(is_worker~.)

ggplot(summary, aes(group = scenario, x= purpose, fill = scenario, y = sum_distance) ) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_brewer(palette = "Paired") + 
  theme_bw() + 
  xlab("Purpose") + ylab("Total distance travelled (km)") + 
  facet_grid(is_worker~.)

ggplot(summary, aes(group = scenario, x= purpose, fill = scenario, y = sum_time) ) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_brewer(palette = "Paired") + 
  theme_bw() + 
  xlab("Purpose") + ylab("Total time travelled (min)") + 
  facet_grid(is_worker~.)


##difference-only

reference_scenarios = summary %>% filter(scen_name == "base")

summary_differences = summary %>% filter(scen_name != "base") %>% left_join(reference_scenarios , 
                      by = c("purpose", "is_worker", "version"), suffix = c("","_ref"))



ggplot(summary_differences, aes(group = scenario, x= purpose, fill = as.factor(version), y = (count - count_ref)/count_ref * 100)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("#A6CEE3","#B2DF8A")) + 
  theme_bw() + 
  xlab("Purpose") + ylab("Difference in number of trips (%)") + 
  facet_grid(is_worker~.) + 
  geom_hline(yintercept = 0)


ggplot(summary_differences, aes(group = scenario, x= purpose, fill = as.factor(version), y = (count - count_ref))) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("#A6CEE3","#B2DF8A")) + 
  theme_bw() + 
  xlab("Purpose") + ylab("Difference in number of trips") + 
  facet_grid(is_worker~.) + 
  geom_hline(yintercept = 0)


ggplot(summary_differences, aes(group = scenario, x= purpose, fill = as.factor(version), y = (distance_mean - distance_mean_ref)/ distance_mean_ref * 100)
        ) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("#A6CEE3","#B2DF8A")) + 
  theme_bw() + 
  xlab("Purpose") + ylab("Average distance difference (%)") + 
  facet_grid(is_worker~.) + 
  geom_hline(yintercept = 0)


ggplot(summary_differences, aes(group = scenario, x= purpose, fill = as.factor(version), y = (time_mean - time_mean_ref)/time_mean_ref * 100)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("#A6CEE3","#B2DF8A")) + 
  theme_bw() + 
  xlab("Purpose") + ylab("Average time difference (%)") + 
  facet_grid(is_worker~.) + 
  geom_hline(yintercept = 0)


#### modal share
summary_by_mode = trips %>% group_by(scenario, mode, purpose) %>%
  summarize(count = n(),
            distance_mean = mean(distance, na.rm = T),
            sum_distance = sum(distance),
            time_mean  =mean(time, na.rm = T),
            sum_time = sum(time))
summary_by_mode$scenario = factor(summary_by_mode$scenario, levels = scenarios)
summary_by_mode$purpose = factor(summary_by_mode$purpose, levels = purposes_ordered)

summary_by_mode = summary_by_mode %>% left_join(classification)


ggplot(summary_by_mode, aes(x= scen_name, fill = mode, y = count)) +
  geom_bar(stat = "identity", position = "fill") + 
  theme_bw() + 
  scale_fill_brewer(palette = "Spectral") + 
  xlab("Purpose") + ylab("Modal share") + 
  facet_grid(version~purpose) + theme(axis.text.x = element_text(angle = 90))







persons$scenario = factor(persons$scenario, levels = scenarios)

persons_wide = persons %>%
  pivot_wider(id_cols = c(person, scenario), names_from = purpose2,
              values_from = c(trips, time), names_sep = "_", values_fill = 0)

summary(persons_wide$time_mandatory)

persons_wide$mandatory_time_bin = cut(persons_wide$time_mandatory,
                                          breaks = c(0,0.1,10,20,30,40,50,1600),
                                          include.lowest = T)
levels(persons_wide$mandatory_time_bin)
persons_wide %>% filter(mandatory_time_bin == "(50,1.6e+03]") %>% summarize(mean(time_mandatory))
persons_wide$mandatory_time_bin_name = factor(persons_wide$mandatory_time_bin,
                                                  levels = levels(persons_wide$mandatory_time_bin),
                                                  labels = c(0,5,15,25,35,45,116))

results_total_counts = persons_wide %>%
  group_by(scenario, mandatory_time_bin_name) %>%
  summarize(trips_HBO = sum(trips_HBO), trips_HBS= sum(trips_HBS),
            trips_NHBO = sum(trips_NHBO), trips_NHBW = sum(trips_NHBW))

results_total_counts = results_total_counts %>% pivot_longer(cols = c(trips_HBO, trips_HBS, trips_NHBW, trips_NHBO),
                                                             names_to = "trips")

ggplot(results_total_counts, aes(x=as.numeric(as.character(mandatory_time_bin_name)),
                                 y = value, group = trips, color = trips)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  facet_wrap(.~scenario) +
  coord_flip() + 
  scale_color_brewer(palette = "Paired") +
  xlab("Time spent in mandatory trips (HBE, HBW) in 10 min bins") +
  ylab("Number of trips") + xlim(1,120) + ylim(0,500000)


##plot number of trips by purpose as a function of mandatory distance bin

trips_per_purpose = persons_wide %>%
  group_by(scenario, mandatory_time_bin_name) %>%
  summarize(HBO = mean(trips_HBO), HBS= mean(trips_HBS),
            NHBO = mean(trips_NHBO), NHBW = mean(trips_NHBW))


trips_per_purpose = trips_per_purpose %>% pivot_longer(cols = c(HBO, HBS, NHBW, NHBO),
                                                 names_to = "trips")

colors_corin =  c("HBS" = "#F8766D", "HBO" = "#00BF7D", "NHBW" = "#00B0F6", "NHBO" = "#E76BF3")


ggplot(trips_per_purpose, aes(x=as.numeric(as.character(mandatory_time_bin_name)),
                           y = value, group = trips, color = trips)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  facet_wrap(.~scenario, scales = "free_x") +
  scale_color_manual(values = colors_corin) + 
  xlab("Time spent in mandatory trips (HBE, HBW) in 10 min bins") +
  ylab("Average number of trips per person") + coord_flip()


  
avg_time_per_trip = persons_wide %>%
  group_by(scenario, mandatory_time_bin_name) %>%
  summarize(HBO = sum(time_HBO, na.rm = T) / sum(trips_HBO, na.rm = T),
            HBS = sum(time_HBS, na.rm = T) / sum(trips_HBS, na.rm = T),
            NHBO = sum(time_NHBO, na.rm = T) / sum(trips_NHBO, na.rm = T),
            NHBW = sum(time_NHBW, na.rm = T) / sum(trips_NHBW, na.rm = T))




avg_time_per_trip = avg_time_per_trip %>% pivot_longer(cols = c(HBO, HBS, NHBO, NHBW),
                                                     names_to = "distance")

ggplot(avg_time_per_trip, aes(x=as.numeric(as.character(mandatory_time_bin_name)),
                             y = value, group = distance, color = distance)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  scale_color_manual(values = colors_corin) + 
  facet_wrap(.~scenario)  +
  coord_flip() + 
  xlab("Time spent in mandatory trips (HBE, HBW) in 10 min bins") +
  ylab("Average time per trip (min)")


avg_time_per_person = persons_wide %>%
  group_by(scenario, mandatory_time_bin_name) %>%
  summarize(HBO = sum(time_HBO, na.rm = T) / n(),
            HBS = sum(time_HBS, na.rm = T) / n(),
            NHBO = sum(time_NHBO, na.rm = T) / n(),
            NHBW = sum(time_NHBW, na.rm = T) / n())



avg_time_per_person = avg_time_per_person %>% pivot_longer(cols = c(HBO, HBS, NHBO, NHBW),
                                                                           names_to = "distance")

ggplot(avg_time_per_person, aes(x=as.numeric(as.character(mandatory_time_bin_name)),
                                        y = value, group = distance, color = distance)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  scale_color_manual(values = colors_corin) + 
  facet_wrap(.~scenario, scales = "free")  + ylim(0,35) + 
  coord_flip()+
  xlab("Distance in mandatory trips (HBE, HBW) in 10 km bins") +
  ylab("Average time per person (min)") 






####only for workers
workers = persons %>% filter(person %in% worker_ids$id)
persons_wide = workers %>%
  pivot_wider(id_cols = c(person, scenario), names_from = purpose2,
              values_from = c(trips, time), names_sep = "_", values_fill = 0)

summary(persons_wide$time_mandatory)

persons_wide$mandatory_time_bin = cut(persons_wide$time_mandatory,
                                      breaks = c(0,0.1,10,20,30,40,50,1600),
                                      include.lowest = T)
levels(persons_wide$mandatory_time_bin)
persons_wide %>% filter(mandatory_time_bin == "(50,1.6e+03]") %>% summarize(mean(time_mandatory))
persons_wide$mandatory_time_bin_name = factor(persons_wide$mandatory_time_bin,
                                              levels = levels(persons_wide$mandatory_time_bin),
                                              labels = c(0,5,15,25,35,45,116))

results_total_counts = persons_wide %>%
  group_by(scenario, mandatory_time_bin_name) %>%
  summarize(trips_HBO = sum(trips_HBO), trips_HBS= sum(trips_HBS),
            trips_NHBO = sum(trips_NHBO), trips_NHBW = sum(trips_NHBW))

results_total_counts = results_total_counts %>% pivot_longer(cols = c(trips_HBO, trips_HBS, trips_NHBW, trips_NHBO),
                                                             names_to = "trips")




ggplot(results_total_counts, aes(x=as.numeric(as.character(mandatory_time_bin_name)),
                                 y = value, group = scenario, color = scenario)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  facet_wrap(.~trips) +
  theme_bw() +
  scale_color_brewer(palette = "Paired") +
  xlab("Time spent in mandatory trips (HBE, HBW) in 10 min bins") +
  ylab("Number of trips (made by workers)")


##plot number of trips by purpose as a function of mandatory distance bin

results_counts = persons_wide %>%
  group_by(scenario, mandatory_time_bin_name) %>%
  summarize(trips_HBO = mean(trips_HBO), trips_HBS= mean(trips_HBS),
            trips_NHBO = mean(trips_NHBO), trips_NHBW = mean(trips_NHBW))


results_counts = results_counts %>% pivot_longer(cols = c(trips_HBO, trips_HBS, trips_NHBW, trips_NHBO),
                                                 names_to = "trips")


ggplot(results_counts, aes(x=as.numeric(as.character(mandatory_time_bin_name)),
                           y = value, group = scenario, color = scenario)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  facet_wrap(.~trips) +
  theme_bw() +
  scale_color_brewer(palette = "Paired") +
  xlab("Time spent in mandatory trips (HBE, HBW) in 10 min bins") +
  ylab("Average number of trips per person (made by workers)")


#error
# persons_wide = persons_wide %>% mutate(
#   avg_HBO = distance_HBO / trips_HBO, avg_HBS = distance_HBS / trips_HBS,
#   avg_NHBO = distance_NHBO / trips_NHBO, avg_NHBW = distance_NHBW / trips_NHBW)

#change!
results_distance = persons_wide %>%
  group_by(scenario, mandatory_time_bin_name) %>%
  summarize(avg_HBO = sum(time_HBO, na.rm = T) / sum(trips_HBO, na.rm = T),
            avg_HBS = sum(time_HBS, na.rm = T) / sum(trips_HBS, na.rm = T),
            avg_NHBO = sum(time_NHBO, na.rm = T) / sum(trips_NHBO, na.rm = T),
            avg_NHBW = sum(time_NHBW, na.rm = T) / sum(trips_NHBW, na.rm = T))



results_distance = results_distance %>% pivot_longer(cols = c(avg_HBO, avg_HBS, avg_NHBO, avg_NHBW),
                                                     names_to = "distance")

ggplot(results_distance, aes(x=as.numeric(as.character(mandatory_time_bin_name)),
                             y = value, group = scenario, color = scenario)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Paired") +
  facet_wrap(.~distance)  +
  theme_bw() +
  xlab("Time spent in mandatory trips (HBE, HBW) in 10 min bins (only by workers)") +
  ylab("Average time per trip (min)")


results_distance_per_person = persons_wide %>%
  group_by(scenario, mandatory_time_bin_name) %>%
  summarize(avg_HBO = sum(time_HBO, na.rm = T) / n(),
            avg_HBS = sum(time_HBS, na.rm = T) / n(),
            avg_NHBO = sum(time_NHBO, na.rm = T) / n(),
            avg_NHBW = sum(time_NHBW, na.rm = T) / n())



results_distance_per_person = results_distance_per_person %>% pivot_longer(cols = c(avg_HBO, avg_HBS, avg_NHBO, avg_NHBW),
                                                                           names_to = "distance")

ggplot(results_distance_per_person, aes(x=as.numeric(as.character(mandatory_time_bin_name)),
                                        y = value, group = scenario, color = scenario)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Paired") +
  facet_wrap(.~distance, scales = "free")  +
  theme_bw() +
  xlab("Time in mandatory trips (HBE, HBW) in 10 min bins (only by workers)") +
  ylab("Average distance per person (km)")



workers = persons %>% filter(person %in% worker_ids$id)






