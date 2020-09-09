#+ message = FALSE
#+ warning = FALSE

pacman::p_load(readr, dplyr, ggplot2)

scenario_names = c("test_100", "test_10", "test_10_sp", "test_1", "test_1_sp")
scales = c(100,10,10,1,1)

distance_bins = seq(0,100,10)

trips_by_mode_purpose = data.frame()
trips_by_mode_distance = data.frame()

for (i in 1:length(scenario_names)){
  path = "c:/models/mito/muc/mitoMunich/scenOutput/"
  path = paste(path, scenario_names[i], "/", sep = "")
  year = 2011
  fileEnding = "/microData/trips.csv"
  
  trips = read_csv(paste(path, year, fileEnding, sep = ""))
  summary1 = trips %>% group_by(mode, purpose) %>% summarize(count = n(), distance = mean(distance))
  summary1$scenario = scenario_names[i]
  summary1$scale = scales[i]
  trips_by_mode_purpose = trips_by_mode_purpose %>% bind_rows(summary1)
  
  trips = trips %>% mutate(distance_bin = cut(distance, distance_bins))
  summary2 = trips %>% group_by(mode, distance_bin) %>% summarize(count = n())
  summary2$scenario = scenario_names[i]
  summary2$scale = scales[i]
  trips_by_mode_distance = trips_by_mode_distance %>% bind_rows(summary2)
}


trips_by_mode_purpose$scenario = factor(trips_by_mode_purpose$scenario, levels = scenario_names)
trips_by_mode_distance$scenario = factor(trips_by_mode_distance$scenario, levels = scenario_names)

aux = trips_by_mode_purpose %>% filter(scenario == "test_100")
total = sum(aux$count)
rm(aux)


scenario_colors = c("test_100" = "grey20", "test_10" = "red", "test_10_sp"= "red4",
                    "test_1"= "plum", "test_1_sp" = "plum4")

ggplot(trips_by_mode_purpose %>% filter(purpose!= "AIRPORT", mode != "null"), aes(fill = scenario, y = count * 100 / scale / total,
                                                                                  x = purpose)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_fill_manual(values = scenario_colors)

ggplot(trips_by_mode_purpose %>% filter(purpose!= "AIRPORT", mode != "null"), aes(fill = scenario, y = count * 100 / scale / total,
                                                                                  x = mode)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_fill_manual(values = scenario_colors)

ggplot(trips_by_mode_purpose %>% filter(purpose!= "AIRPORT", mode != "null"), aes(fill = scenario, y = distance,
                                                                                  x = purpose)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_fill_manual(values = scenario_colors)

ggplot(trips_by_mode_purpose %>% filter(purpose!= "AIRPORT", mode != "null"), aes(fill = scenario, y = distance,
                                                                                  x = mode)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_fill_manual(values = scenario_colors)


ggplot(trips_by_mode_purpose %>% filter(purpose!= "AIRPORT"), aes(fill = scenario, y = count * 100 / scale / total,
                                                                  x = mode)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(.~purpose) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_fill_manual(values = scenario_colors)

ggplot(trips_by_mode_purpose %>% filter(purpose!= "AIRPORT", mode != "null"), aes(fill = scenario, y = distance,
                                                                                  x = mode)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(.~purpose) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_fill_manual(values = scenario_colors)

ggplot(trips_by_mode_distance, aes(fill = mode, y = count * 100 / scale, x = scenario)) + 
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(.~distance_bin) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90))


