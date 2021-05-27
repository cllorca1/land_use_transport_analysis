pacman::p_load(readr, dplyr, ggplot2, tidyr)

mito_folder = "c:/models/mito/muc/mitoMunich/"

scenarios = c("new_tod_20210421_test", "reference_tod_20210421")
scales = c(1.0, 1.0)

year = 2011

trips = data.frame()

for (i in 1:length(scenarios)){
  scenario = scenarios[i]
  scale = scales[i]
  this_trip_filename = paste(mito_folder, "scenOutput/", scenario , "/", year, "/microData/trips.csv", sep = "" )
  this_trips = read_csv(this_trip_filename)
  this_trips = this_trips %>% mutate(scenario = scenario) %>% mutate(scale = scale)
  trips = trips %>% bind_rows(this_trips)
}

interval = 5
my_breaks = interval * 0:289



trips  = trips %>%
  mutate(departure_time = if_else(departure_time>1440, departure_time - 1440, departure_time)) %>% 
  mutate(departure_time_return = if_else(departure_time_return>1440, departure_time_return - 1440, departure_time_return))


trips  = trips %>%
  mutate(dt_bin = cut(departure_time, breaks = my_breaks, labels = F)) %>%
  mutate(dt_return_bin = cut(departure_time_return, breaks = my_breaks, labels = F))

summary = trips %>% group_by(purpose, scenario, scale, dt_bin) %>% summarise(n = n()) %>%
  mutate(n = n/scale) %>% mutate(dt_bin = dt_bin * interval)

summary %>% ggplot(aes(x = dt_bin/60, y = n, linetype = scenario, color = purpose)) +
  geom_line(size = 1) +
  facet_wrap(.~purpose)



summary = trips %>% group_by(purpose, scenario, scale, dt_return_bin) %>%
  summarise(n = n()) %>%
  filter(!is.na(dt_return_bin)) %>%
  mutate(n = n/scale) %>%
  mutate(dt_return_bin = dt_return_bin * interval)

summary %>% ggplot(aes(x = dt_return_bin/60, y = n, linetype = scenario, color = purpose)) +
  geom_line(size = 1) +
  facet_wrap(.~purpose)

