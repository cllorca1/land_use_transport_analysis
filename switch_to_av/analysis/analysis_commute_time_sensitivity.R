pacman::p_load(readr, dplyr, data.table, ggplot2, tidyr)


upper_folder = "c:/models/silo/muc/scenOutput/"

scenario = "baseAV2"

years = c(2011, 2020, 2030, 2040, 2050)

summary = data.frame()

for (year in years){
  workers_micro_data = read_csv(paste(upper_folder, scenario, "/siloResults/avOwnershipByHh_", year, ".csv", sep  =""))
  workers_micro_data$isAvHh = if_else(workers_micro_data$avs > 0, "AV", if_else(workers_micro_data$autos>1, "CV-only", "no-car"))
  p = ggplot(workers_micro_data, aes(x= timeCar, group = isAvHh, color = as.factor(isAvHh), ..density..)) + 
    geom_freqpoly(size = 1) + ggtitle(year)
  print(p)
  
  workers = nrow(workers_micro_data)
  hh_with_workers = length(unique(workers_micro_data$household))
  
  hh = workers_micro_data %>% select(household, isAvHh)
  hh = hh[!duplicated(hh$household),]
  hh_with_avs = nrow(hh %>% filter(isAvHh == "AV"))
  hh_with_cvs = nrow(hh %>% filter(isAvHh == "CV-only"))

  row = data.frame(year, workers, hh_with_workers, hh_with_avs,hh_with_cvs)
  summary = summary %>% bind_rows(row)


}

summary$hh_without_cars = summary$hh_with_workers - summary$hh_with_avs - summary$hh_with_cvs 

summary2 = summary %>% gather(2:6, key  ="variable", value = "value")

ggplot(summary2, aes(x= year, y = value, color = variable)) + geom_line(size =1) + geom_point(size = 2)


ggplot(summary2 %>% filter(variable != "hh_with_workers", variable != "workers"), 
       aes(x= year, y = value, fill = variable)) + geom_bar(stat = "identity", position = "fill")
