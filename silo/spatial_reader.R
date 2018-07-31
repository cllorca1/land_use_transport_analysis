pacman::p_load(ggplot2, dplyr)

read_spatial = function(output_path, subpath, scenario_name, regions){

path = "c:/models/silo/muc/scenOutput"

file_name = "resultFileSpatial1.csv"

raw = read.csv(paste(path,subpath,file_name,sep = "/"),
               fill = T,
               skip = 1,as.is = 1:21)

raw = raw %>% filter(autoAccessibility != "", autoAccessibility != "autoAccessibility")
zones  = unique(as.numeric(raw$Year2011))
years = 2011:2050

firstColumns = data.frame()
for(year in years){
  thisYear = data.frame(zone = zones, year = year)
  firstColumns = rbind(firstColumns, thisYear)
}

raw = cbind(firstColumns, raw)

raw = raw %>% mutate (validation = if_else(zone == as.numeric(Year2011), T, F))
summary(raw$validation)

data = raw %>% select(year, zone, autoAccessibility,
                      transitAccessibility , 
                      population, 
                      households, 
                      jobs) %>% 
  mutate(autoAccessibility = as.numeric(autoAccessibility)) %>%
  mutate(transitAccessibility = as.numeric(transitAccessibility)) %>%
  mutate(population = as.numeric(population)) %>%
  mutate(households = as.numeric(households)) %>%
  mutate(jobs = as.numeric(jobs))

data = merge(x= data, y = zonesWithRegionName, by = "zone")

data_region = data %>% group_by(year, region_name = work_region_name) %>% summarize(autoAccessibility = mean(autoAccessibility),
                                                                         transitAccessibility = mean(transitAccessibility),
                                                                         population = sum(population),
                                                                         households = sum(households), 
                                                                         jobs = sum(jobs))


data_region = data_region %>%
  filter(as.character(region_name) %in% regions)

plot5 = ggplot(data_region, aes(x=as.integer(year),
                        y = population, 
                        color = region_name,
                        group = region_name)) + 
  ylab("Population") + 
  geom_line(size = 1) + 
  theme_light() + 
  theme(legend.position = "bottom") + 
  labs(color = "Region") + 
  xlab("Year")

print(plot5)

ggsave(file = paste(output_path,"pp_regional_",scenario_name,".png", sep = ""),
       plot =  plot5, height = 15, width = 10, units = "cm" )

plot6 = ggplot(data_region, aes(x=as.integer(year),
                        y = autoAccessibility, 
                        color = region_name,
                        group = region_name)) + 
  ylab("Auto accessibility") + 
  geom_line(size = 1) + 
  theme_light() + 
  theme(legend.position = "bottom") + 
  labs(color = "Region") + 
  xlab("Year")

#print(plot6)

ggsave(file = paste(output_path,"auto_access_regional_",scenario_name,".png", sep = ""),
       plot =  plot6, height = 15, width = 10, units = "cm" )

plot7= ggplot(data_region, aes(x=as.integer(year),
                                y = jobs, 
                                color = region_name,
                                group = region_name)) + 
  ylab("Jobs") + 
  geom_line(size = 1) + 
  theme_light() + 
  theme(legend.position = "bottom") + 
  labs(color = "Region") + 
  xlab("Year")

#print(plot7)

ggsave(file = paste(output_path,"jj_regional_",scenario_name,".png", sep = ""),
       plot =  plot7, height = 15, width = 10, units = "cm" )
}
