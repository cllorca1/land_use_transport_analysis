pacman::p_load(readr, dplyr, data.table, ggplot2, tidyr, rhdf5, sf, tmap, plotly, ggalluvial)


upper_folder = "c:/models/silo/muc/scenOutput/"


##read silo results

years = c(2011, 2020, 2030, 2040, 2050)


scenarios = c( "0_none", "A_none")
scenario_labels = c( "no-AV", "AV")
scenario_colors = c("#000000", "#FF0000")

t1 = data.frame(scenarios)
t1$case = "AV"

scenarios = c("0_vot", "A_vot")
t2 = data.frame(scenarios)
t2$case = "AV+VOT"
t1 = t1 %>% bind_rows(t2)

# scenarios = c("0_parking", "A_parking")
# t2 = data.frame(scenarios)
# t2$case = "AV+VOT"
# t1 = t1 %>% bind_rows(t2)

scenarios = c("0_parking_2", "A_parking_2")
t2 = data.frame(scenarios)
t2$case = "AV+Parking"
t1 = t1 %>% bind_rows(t2)

scenarios = c("0_only_transport", "A_only_transport")
t2 = data.frame(scenarios)
t2$case = "AV+Transport congestion"
t1 = t1 %>% bind_rows(t2)

scenarios = c("0_all", "A_all")
t2 = data.frame(scenarios)
t2$case = "AV+All"
t1 = t1 %>% bind_rows(t2)

cases = unique(t1$case)

rm(t2)

modeChoice = data.frame()

for (this_case in cases){
  scenarios = t1 %>% filter(case == this_case)
  scenarios = scenarios$scenarios
  
  for (i in 1:length(scenarios)){
    
    scenario_name = paste("AV", scenarios[i], sep = "")
    
    for (year in years){
      this_year_data = read_csv(paste(upper_folder, scenario_name, "/siloResults/avOwnershipByHh_", year, ".csv", sep  = ""))
      this_year_data$year = year
      this_year_data$scenario = scenario_labels[i]
      this_year_data$case = this_case
      modeChoice = modeChoice %>% bind_rows(this_year_data)
      rm(this_year_data)
    }
    
    
  }
  
}

modeChoice$hh_type = if_else(modeChoice$autos == 0, "zero-autos", if_else(modeChoice$avs == 0 ,  "hh-with-cvs", "hh-with-avs"))


zoneTypes = read_csv("c:/models/silo/muc/input/zoneSystem.csv")

zoneTypes = zoneTypes %>% select(id = Zone, area = Area, type = BBSR_Type)


modeChoice = modeChoice %>% left_join(zoneTypes, by = c("homeZone" = "id"))
modeChoice = modeChoice %>% left_join(zoneTypes, by = c("jobZone" = "id"), suffix = c("home", "job"))


summary_hhs = modeChoice %>% group_by(case, year, typehome, typejob, scenario) %>% summarise(workers = n())

summary_hhs$typehome = factor(x = summary_hhs$typehome, levels = c(10,20,30,40), labels = c("home in Core city", "home in Medium city", "home in Town", "home in Rural area"))
summary_hhs$typejob = factor(x = summary_hhs$typejob, levels = c(10,20,30,40), labels = c("work in Core city", "work in Medium city", "work in Town", "work in Rural area"))


summary_hhs$scenario = factor(summary_hhs$scenario, levels = scenario_labels)

# 
# ggplot(summary_hhs, aes(x=year, y=workers*20/1000, color = scenario)) +
#   geom_line(size  =1) +
#   facet_grid(typejob~typehome, scales = "free") +
#   scale_color_manual(values = scenario_colors) + 
#   theme_bw() + 
#   theme(axis.text.x = element_text(angle = 90)) + 
#   xlab("Year") + ylab("Number of workers (thousands)") + labs(color = "Scenario") 
# 
# ggplotly()


color_areas = c("#a3a3a3", "#d07f63", "#ffd688", "#a4c1a1")



ggplot(summary_hhs %>% filter(year == 2050), aes(x= scenario, y=workers*20/1000, fill = typehome)) +
  geom_bar(position = "fill", stat = "identity", color = "black") +
  facet_grid(case~typejob, scales = "free") +
  scale_fill_manual(values= color_areas) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  xlab("scenario") + ylab("Share of workers") + labs(fill = "Home location")



summary_hhs2050 = summary_hhs %>% filter(year == 2050) %>% spread(scenario, workers)
summary_hhs2050$case = factor(summary_hhs2050$case, levels = cases)


ggplot(summary_hhs2050, aes(x= typehome, y=(AV - `no-AV`)*20/1000, fill = typehome)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  facet_grid(case~typejob) +
  scale_fill_manual(values= color_areas) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom") + 
  xlab("Year") + ylab("Difference in number of workers respect to no-AV scenario (thousands)") +
  labs(fill = "Home location") + ylim (-30,30)


ggsave("C:/projects/Papers/2020_cities/figs/workers_from_to_type.pdf", width = 15, units = "cm", height = 20, scale = 1.3)















# 
# summary_hhs_by_home = modeChoice %>% group_by(year, typehome, scenario) %>% summarise(workers = n())
# summary_hhs_by_home$typehome = factor(x = summary_hhs_by_home$typehome, levels = c(10,20,30,40), labels = c("from Core", "from Medium city", "from Town", "from Rural"))
# 
# summary_hhs_by_home$scenario = factor(summary_hhs_by_home$scenario, levels = scenarios)
# 
# 
# ggplot(summary_hhs_by_home, aes(x=year, y=workers*20/1000, color = scenario)) +
#   geom_line(size  =1) +
#   facet_grid(.~typehome, scales = "free") +
#   scale_color_manual(values = scenario_colors) + 
#   theme_bw() + theme(axis.text.x = element_text(angle = 90)) + 
#   xlab("Year") + ylab("Number of workers (thousands)") + labs(color = "Scenario")
# 
# 
# ggplot(summary_hhs_by_home, aes(x = year,y=workers*20/1000, fill = typehome )) + 
#   geom_area(position = "fill") + 
#   facet_wrap(.~scenario)
# 
# 
# 
# #ggsave("C:/projects/Papers/2020_cities/figs/workers_from_type.pdf", width = 15, units = "cm", height = 5, scale = 1.5)
# 
# summary_hhs_by_job = modeChoice %>% group_by(year, typejob, scenario) %>% summarise(workers = n())
# summary_hhs_by_job$typejob = factor(x = summary_hhs_by_job$typejob, levels = c(10,20,30,40), labels = c("from Core", "from Medium city", "from Town", "from Rural"))
# 
# summary_hhs_by_job$scenario = factor(summary_hhs_by_job$scenario, levels = scenarios)
# 
# 
# ggplot(summary_hhs_by_job, aes(x=year, y=workers*20, color = scenario)) +
#   geom_line(size  =1) +
#   facet_grid(.~typejob, scales = "free") +
#   scale_color_manual(values = scenario_colors) + 
#   theme_bw()
# 
# 
