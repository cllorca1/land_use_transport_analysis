pacman::p_load(readr, dplyr, data.table, ggplot2, tidyr)


upper_folder = "c:/models/silo/muc/scenOutput/"



years = c(2011, 2020, 2030, 2040, 2050)

summary = data.frame()

scenarios = c("0_t", "A", "B", "C", "D", "E")

for (scenario in scenarios){

  scenario_name = paste("AV", scenario, sep = "")
  
  for (year in years){
    workers_micro_data = read_csv(paste(upper_folder, scenario_name, "/siloResults/avOwnershipByHh_", year, ".csv", sep  =""))
    workers_micro_data$isAvHh = if_else(workers_micro_data$avs > 0, "AV", if_else(workers_micro_data$autos> 0, "CV-only", "no-car"))
    # p = ggplot(workers_micro_data, aes(x= timeCar, group = isAvHh, color = as.factor(isAvHh), ..density..)) + 
    #   geom_freqpoly(size = 1) + ggtitle(year)
    # print(p)
    # 
    workers = nrow(workers_micro_data)
    hh_with_workers = length(unique(workers_micro_data$household))
    
    hh = workers_micro_data %>% select(household, isAvHh, timeCar)
    hh = hh[!duplicated(hh$household),]
    hh_with_avs = nrow(hh %>% filter(isAvHh == "AV"))
    hh_with_cvs = nrow(hh %>% filter(isAvHh == "CV-only"))
    time_hh_with_cvs = mean((hh %>% filter(isAvHh == "CV-only"))$timeCar)
    time_hh_with_avs = mean((hh %>% filter(isAvHh == "AV"))$timeCar)
    time_hh_without_cars = mean((hh %>% filter(isAvHh == "no-car"))$timeCar)
    
    row = data.frame(year, scenario, workers, hh_with_workers, hh_with_avs,hh_with_cvs,time_hh_with_cvs,time_hh_with_avs,time_hh_without_cars)
    summary = summary %>% bind_rows(row)
  }
}

summary$hh_without_cars = summary$hh_with_workers - summary$hh_with_avs - summary$hh_with_cvs 
summary2 = summary %>% gather(c(hh_with_avs, hh_with_cvs, hh_without_cars), key  ="variable", value = "value")

ggplot(summary2, aes(x= year, y = value, color = variable)) + geom_line(size =1) + geom_point(size = 2) + facet_wrap(.~scenario)
ggplot(summary2 %>% filter(variable != "hh_with_workers", variable != "workers"), 
       aes(x= year, y = value, fill = variable)) + geom_bar(stat = "identity", position = "fill") + facet_wrap(.~scenario)


summary2_only_av = summary2 %>% filter(variable == "hh_with_avs")
ggplot(summary2_only_av, aes(x= year, y = value/hh_with_workers * 100, color = scenario)) + geom_line(size =1) + geom_point(size = 2)


summary3 = summary %>% gather(c(time_hh_with_avs, time_hh_with_cvs, time_hh_without_cars), key  ="variable", value = "value")
ggplot(summary3, aes(x= year, y = value, color = variable)) + geom_line(size =1) + geom_point(size = 2) + facet_wrap(.~scenario)

color_hh_types = c("indianRed1", "indianRed4", "royalblue3")

ggplot(summary3 %>% filter(scenario == "0_t" | scenario == "A"), aes(x= year, y = value, color = variable)) +
  geom_line(size =1) + geom_point(size = 2) + facet_wrap(.~scenario) + scale_color_manual(values = color_hh_types) + 
  ylab("average commute time")







