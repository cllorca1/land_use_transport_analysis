pacman::p_load(readr, dplyr, data.table, ggplot2, tidyr, plotly)

upper_folder = "c:/models/silo/muc/scenOutput/"

years = c(2011, 2020, 2030, 2040, 2050)

commuteDistance = data.frame()
satisfaction = data.frame()

#compatibility_older_data
# scenarios = c("0_none", "A_none", 
#               "0_vot", "A_vot",
#               "0_parking_2","A_parking_2",
#               "0_only_transport", "A_only_transport",
#               "0_all", "A_all")
# scenario_labels = c("no-AV", "AV",
#                     "no-AV", "AV",
#                     "no-AV", "AV",
#                     "no-AV", "AV",
#                     "no-AV", "AV")
# 
# cases = c("AV", "AV",
#           "AV+VOT" ,"AV+VOT",
#           "AV+Parking", "AV+Parking",
#           "AV+Transport congestion", "AV+Transport congestion",
#           "AV-All", "AV-All")


scenarios = c("base_5", "AV0_parking_2","AVA_parking_2")
scenario_labels = c("base", "no-AV", "AV")
cases = scenario_labels



for (i in 1:length(scenarios)){

  
  scenario_label = scenario_labels[i]
  scenario = scenarios[i]
  scenario_name =scenario
  
  #this_commuteDistance = read_csv(paste(upper_folder, scenario_name, "/siloResults/commutingDistance.csv", sep = ""))
  this_commuteDistance = read_csv(paste(upper_folder, scenario_name, "/siloResults/regionAvCommutingTime.csv", sep = ""))
  
  
  this_satisfaction = read_csv(paste(upper_folder, scenario_name, "/siloResults/hhSatisfactionByRegion.csv", sep = ""))
  thisHhs = this_satisfaction %>% group_by(year, region) %>% summarize(hh = sum(hh_count))
  this_commuteDistance = this_commuteDistance %>% left_join(thisHhs, by=c("year", "aveCommuteDistByRegion" = "region"))
  
  
  this_commuteDistance$scenario = scenario_label
  this_satisfaction$scenario = scenario_label
  
  this_commuteDistance$case = cases[i]
  this_satisfaction$case = cases[i]
  
  commuteDistance = commuteDistance %>% bind_rows(this_commuteDistance)
  satisfaction = satisfaction %>% bind_rows(this_satisfaction)
  
  
}


commuteDistance = commuteDistance %>% group_by(scenario, year, case) %>% summarize(time = weighted.mean(minutes, hh, na.rm = T))
#commuteDistance$case = factor(commuteDistance$case,  c("AV", "AV+VOT", "AV+Parking", "AV+Transport congestion", "AV-All"))

ggplot(commuteDistance, aes(x=year, y= time, color = scenario)) +
  geom_line(size = 2)  + theme_bw() +
  xlab("Year") + ylab("Average commute time (measured as time by car) (min)") + labs(color = "Scenario") +
    facet_wrap(.~case, ncol = 3) + 
  theme(legend.position = "bottom") + 
  theme(axis.text.x = element_text(angle = 90))




#ggsave("C:/projects/Papers/2020_cities/figs/commuteTime.pdf", width = 15, units = "cm", height = 10, scale = 1.3)




##add the zone type to the satisfaction data

zoneTypes = read_csv("c:/models/silo/muc/input/zoneSystem.csv")

zoneTypes = zoneTypes %>% select(id = Zone, area = Area, type = BBSR_Type)


satisfaction = satisfaction %>% left_join(zoneTypes, by = c("zone" = "id"))



satisfaction$type = factor(x = satisfaction$type, levels = c(10,20,30,40), labels = c("Core", "Medium city", "Town", "Rural"))


satisfaction_by_type = satisfaction %>% group_by(scenario, year, type) %>% summarise(satisfaction = weighted.mean(hh_ave_satisfaction, hh_count))


ggplot(satisfaction_by_type, aes(x=year, y = satisfaction, color = scenario)) +
  geom_line(size = 1) + facet_wrap(.~type)


ggplot(satisfaction_by_type, aes(x=year, y = satisfaction, color = type)) +
  geom_line(size = 1) + facet_wrap(.~scenario) 

