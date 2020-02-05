pacman::p_load(readr, dplyr, data.table, ggplot2, tidyr, rhdf5, sf, tmap, plotly)


upper_folder = "c:/models/silo/muc/scenOutput/"


##read silo results

years = c(2011, 2020, 2030, 2040, 2050)

scenarios = c("0_no_parking", "E_no_parking", "D_no_parking", "C_no_parking", "B_no_parking", "A_no_parking")

modeChoice = data.frame()

for (scenario in scenarios){
   
  scenario_name = paste("AV", scenario, sep = "")
  
  for (year in years){
    this_year_data = read_csv(paste(upper_folder, scenario_name, "/siloResults/avOwnershipByHh_", year, ".csv", sep  = ""))
    this_year_data$year = year
    this_year_data$scenario = scenario
    modeChoice = modeChoice %>% bind_rows(this_year_data)
    rm(this_year_data)
  }
  
  
}


modeChoice$hh_type = if_else(modeChoice$autos == 0, "zero-autos", if_else(modeChoice$avs == 0 ,  "hh-with-cvs", "hh-with-avs"))


zoneTypes = read_csv("c:/models/silo/muc/input/zoneSystem.csv")

zoneTypes = zoneTypes %>% select(id = Zone, area = Area, type = BBSR_Type)


modeChoice = modeChoice %>% left_join(zoneTypes, by = c("homeZone" = "id"))
modeChoice = modeChoice %>% left_join(zoneTypes, by = c("jobZone" = "id"), suffix = c("home", "job"))


summary_hhs = modeChoice %>% group_by(year, typehome, typejob, scenario) %>% summarise(workers = n())

summary_hhs$typehome = factor(x = summary_hhs$typehome, levels = c(10,20,30,40), labels = c("from Core", "from Medium city", "from Town", "from Rural"))
summary_hhs$typejob = factor(x = summary_hhs$typejob, levels = c(10,20,30,40), labels = c("to Core", "to Medium city", "to Town", "to Rural"))


summary_hhs$scenario = factor(summary_hhs$scenario, levels = scenarios)


scenario_colors = c("#000000","#DE5959","#D98282","#C99797", "#C7B3B3","#FF0000")

#scenario_colors = c("#FF0000", "#DE5959", "#D98282", "#C99797", "#C7B3B3","#000000")

ggplot(summary_hhs, aes(x=year, y=workers*20/1000, color = scenario)) +
  geom_line(size  =1) +
  facet_grid(typejob~typehome, scales = "free") +
  scale_color_manual(values = scenario_colors) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  xlab("Year") + ylab("Number of workers (thousands)") + labs(color = "Scenario") 

ggplotly()

#ggsave("C:/projects/Papers/2020_cities/figs/workers_from_to_type.pdf", width = 15, units = "cm", height = 10, scale = 1.5)



summary_hhs_by_home = modeChoice %>% group_by(year, typehome, scenario) %>% summarise(workers = n())
summary_hhs_by_home$typehome = factor(x = summary_hhs_by_home$typehome, levels = c(10,20,30,40), labels = c("from Core", "from Medium city", "from Town", "from Rural"))

summary_hhs_by_home$scenario = factor(summary_hhs_by_home$scenario, levels = scenarios)


ggplot(summary_hhs_by_home, aes(x=year, y=workers*20/1000, color = scenario)) +
  geom_line(size  =1) +
  facet_grid(.~typehome, scales = "free") +
  scale_color_manual(values = scenario_colors) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90)) + 
  xlab("Year") + ylab("Number of workers (thousands)") + labs(color = "Scenario")

#ggsave("C:/projects/Papers/2020_cities/figs/workers_from_type.pdf", width = 15, units = "cm", height = 5, scale = 1.5)

summary_hhs_by_job = modeChoice %>% group_by(year, typejob, scenario) %>% summarise(workers = n())
summary_hhs_by_job$typejob = factor(x = summary_hhs_by_job$typejob, levels = c(10,20,30,40), labels = c("from Core", "from Medium city", "from Town", "from Rural"))

summary_hhs_by_job$scenario = factor(summary_hhs_by_job$scenario, levels = scenarios)


ggplot(summary_hhs_by_job, aes(x=year, y=workers*20, color = scenario)) +
  geom_line(size  =1) +
  facet_grid(.~typejob, scales = "free") +
  scale_color_manual(values = scenario_colors) + 
  theme_bw()


