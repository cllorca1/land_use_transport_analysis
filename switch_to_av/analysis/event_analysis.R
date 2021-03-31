pacman::p_load(data.table, dplyr, ggplot2, tidyr, readr, plotly, sf, tmap, rgeos, RColorBrewer)

scenarios = c("base_5", "AV0_parking_2", "AVA_parking_2")

base_folder = "C:/models/silo/muc/scenOutput/"


events = data.frame()

for (scenario in scenarios){
  this_events = read_csv(paste(base_folder, scenario, "/siloResults/eventCounts.csv", sep ="")) %>%
    mutate(scenario  =scenario)
  events = events %>% bind_rows(this_events)
  rm(this_events)
}



events = events %>% pivot_wider(names_from = event,values_from = count )

ggplot(events, aes(x = year, y = MoveEvent, group = scenario, color = scenario)) + 
  geom_line(stat = "identity")
