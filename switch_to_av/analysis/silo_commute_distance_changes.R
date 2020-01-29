pacman::p_load(readr, dplyr, data.table, ggplot2, tidyr)

upper_folder = "c:/models/silo/muc/scenOutput/"

years = c(2011, 2020, 2030, 2040, 2050)

commuteDistance = data.frame()
satisfaction = data.frame()

scenarios = c("A", "B", "C", "D", "E", "0")


for (scenario in scenarios){

  scenario_name = paste("AV", scenario, sep = "")
  
  this_commuteDistance = read_csv(paste(upper_folder, scenario_name, "/siloResults/commutingDistance.csv", sep = ""))
  this_satisfaction = read_csv(paste(upper_folder, scenario_name, "/siloResults/hhSatisfactionByRegion.csv", sep = ""))
  thisHhs = this_satisfaction %>% group_by(year, region) %>% summarize(hh = sum(hh_count))
  this_commuteDistance = this_commuteDistance %>% left_join(thisHhs, by=c("year", "region"))
  
  
  this_commuteDistance$scenario = scenario
  this_satisfaction$scenario = scenario
  
  commuteDistance = commuteDistance %>% bind_rows(this_commuteDistance)
  satisfaction = satisfaction %>% bind_rows(this_satisfaction)
  
  
}


commuteDistance = commuteDistance %>% group_by(scenario, year) %>% summarize(time = weighted.mean(time, hh, na.rm = T))

commuteDistance$scenario = factor(commuteDistance$scenario, levels = scenarios)
scenario_colors = c("#FF0000", "#DE5959", "#D98282", "#C99797", "#C7B3B3","#000000")


ggplot(commuteDistance, aes(x=year, y= time, color = scenario)) +
  geom_line(size = 1) + ylim(0,40) + scale_color_manual(values= scenario_colors) + theme_bw() +
  xlab("Year") + ylab("Average commute time (measured as time by car) (min)") + labs(color = "Scenario")

#ggsave("C:/projects/Papers/2020_cities/figs/commuteTime.pdf", width = 15, units = "cm", height = 10, scale = 1.5)


##add the zone type to the satisfaction data

zoneTypes = read_csv("c:/models/silo/muc/input/zoneSystem.csv")

zoneTypes = zoneTypes %>% select(id = Zone, area = Area, type = BBSR_Type)


satisfaction = satisfaction %>% left_join(zoneTypes, by = c("zone" = "id"))



satisfaction$type = factor(x = satisfaction$type, levels = c(10,20,30,40), labels = c("Core", "Medium city", "Town", "Rural"))


satisfaction_by_type = satisfaction %>% group_by(scenario, year, type) %>% summarise(satisfaction = weighted.mean(hh_ave_satisfaction, hh_count))
satisfaction_by_type$scenario = factor(satisfaction_by_type$scenario, levels = scenarios)

ggplot(satisfaction_by_type, aes(x=year, y = satisfaction, color = scenario)) +
  geom_line(size = 1) + facet_wrap(.~type) + scale_color_manual(values = scenario_colors)


ggplot(satisfaction_by_type, aes(x=year, y = satisfaction, color = type)) +
  geom_line(size = 1) + facet_wrap(.~scenario) 

