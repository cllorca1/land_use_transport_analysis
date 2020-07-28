pacman::p_load(readr, dplyr, ggplot2, reshape, plotly, sf, leaflet, tmap, tidyr, here)

##upper folder of results - one level up of scenarios
folder = "d:/simulations/silo/fabilutSiloResultsSpring2020/"

##upper folder of inputs (zone system and zone shapefile)
folder_input = "c:/code/msm-visualizer/"
shapefile_name = paste(folder_input,"/map/muc/zones_31468.shp", sep ="")
zones_regions = read_csv(paste(folder_input,"/examples/muc/zoneSystem.csv", sep =""))


source(paste(here(), "/silo/data_reader_v2_muc.R", sep  =""))

##name of the base scenario
name_of_the_base_scenario = "baseCase"

##choose the way you want to clasifiy the results spatially (core-not core, muc-not muc, area type, core city name, etc.)
spatial$classifier = spatial$core

spatial_base = spatial %>% filter(scenario == name_of_the_base_scenario)
spatial = spatial %>% left_join(spatial_base, by = c("year", "zone", "region", "lkr", "areaType", "municipality", "is_core", "is_muc", "core"), 
                                suffix = c("","_base"))
rm(spatial_base)

all_zones_summary = spatial %>% group_by(scenario,year) %>%
  summarize(pp = sum(population),
            jj = sum(jobs),
            price = weighted.mean(avePrice,dd),
            dd=sum(dd), 
            hh = sum(households),
            acc = weighted.mean(autoAccessibility, population),
            acc_transit = weighted.mean(transitAccessibility, population))

all_zones_summary_relative = spatial %>% group_by(scenario,year) %>%
  summarize(pp = sum(population - population_base),
            jj = sum(jobs - jobs_base),
            price = weighted.mean(avePrice - avePrice_base,dd),
            dd=sum(dd - dd_base), 
            hh = sum(households - households_base),
            acc = weighted.mean(autoAccessibility - autoAccessibility_base, population),
            acc_transit = weighted.mean(transitAccessibility -  transitAccessibility_base, population))

all_dataset = all_zones_summary_relative

p = ggplot(all_dataset, aes(x=year, y = pp, color = as.factor(scenario))) +
  geom_path(size = 1) + geom_point(size = 2) + 
  ggtitle("Population") + 
  xlab("Year") + ylab("Population") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90)) + 
  scale_x_continuous(expand = c(0,0)) +  scale_y_continuous(expand = c(0,0))

p

ggplotly(p, width = 1200)

p = ggplot(all_dataset , aes(x=year, y = dd, color = as.factor(scenario))) +
  geom_path(size = 1) + 
  ggtitle("Dwellings") +  theme_bw() + theme(axis.text.x = element_text(angle = 90))
ggplotly(p, width = 1200)

summary_by_zone_type = spatial %>% group_by(scenario,year,classifier) %>%
  summarize(pp = sum(population),
            jj = sum(jobs),
            price = weighted.mean(avePrice,dd),
            dd= sum(dd), 
            hh = sum(households),
            acc = weighted.mean(autoAccessibility, population),
            acc_transit = weighted.mean(transitAccessibility, population))

summary_by_zone_type_relative = spatial %>% group_by(scenario,year,classifier) %>%
  summarize(pp = sum(population - population_base),
            jj = sum(jobs - jobs_base),
            price = weighted.mean(avePrice - avePrice_base,dd),
            dd=sum(dd - dd_base), 
            hh = sum(households - households_base),
            acc = weighted.mean(autoAccessibility - autoAccessibility_base, population),
            acc_transit = weighted.mean(transitAccessibility - transitAccessibility_base, population))


dataset = summary_by_zone_type_relative
##you are seeing results with respect of the base scenario!!

dataset$scenario = factor(dataset$scenario, levels = c("baseCase", "baseCase_MagLev", "MucCoreCityDev_MagLev", "MucCoreCityDev", "MucDev"), 
                          labels = c("baseCase", "baseCase_MdfgfdagLev", "dfgddf", "MucCoreCityDev", "MucDev"))

p = ggplot(dataset , aes(x=year, y = pp, color = scenario)) +
  geom_path(size = 1) + 
  facet_grid(.~classifier) +
  ggtitle("Population") + theme(axis.text.x = element_text(angle = 90))

p


ggplotly(p, width = 1200)
p = ggplot(dataset, aes(x=year, y = hh, color = as.factor(scenario))) +
  geom_path(size = 1) + 
  facet_grid(.~classifier)  +
  ggtitle("Households")+ theme(axis.text.x = element_text(angle = 90))
ggplotly(p, width = 1200)
p = ggplot(dataset, aes(x=year, y = dd, color = as.factor(scenario))) +
  geom_path(size = 1) + 
  facet_grid(.~classifier) +
  ggtitle("Number of dwellings")+ theme(axis.text.x = element_text(angle = 90))
ggplotly(p, width = 1200)
p = ggplot(dataset, aes(x=year, y = dd-hh, color = as.factor(scenario))) +
  geom_path(size = 1) + 
  facet_grid(.~classifier, scales = "free") +
  ggtitle("Number of vacant dwellings")+ theme(axis.text.x = element_text(angle = 90))
ggplotly(p, width = 1200)
p = ggplot(dataset, aes(x=year, y = price, color = as.factor(scenario))) +
  geom_path(size = 1) + 
  facet_grid(.~classifier) +
  ggtitle("Average dwelling price")+ theme(axis.text.x = element_text(angle = 90))
ggplotly(p, width = 1200)
p = ggplot(dataset, aes(x=year, y = 1 - hh/dd, color = as.factor(scenario))) +
  geom_path(size = 1) + 
  facet_grid(.~classifier) +
  ggtitle("Global vacancy rate")+ theme(axis.text.x = element_text(angle = 90))
ggplotly(p, width = 1200)
p = ggplot(dataset, aes(x=year, y = acc, color = as.factor(scenario))) +
  geom_path(size = 1) + 
  facet_grid(.~classifier) +
  ggtitle("Average auto accessibility (employment-calculation and population-averaging)")+ theme(axis.text.x = element_text(angle = 90))
ggplotly(p, width = 1200)
p = ggplot(dataset, aes(x=year, y = acc_transit, color = as.factor(scenario))) +
  geom_path(size = 1) + 
  facet_grid(.~classifier) +
  ggtitle("Average transit accessibility (employment-calculation and population-averaging)")+ theme(axis.text.x = element_text(angle = 90))
ggplotly(p, width = 1200)




regional_summary = spatial %>% group_by(scenario,year,region) %>%
  summarize(pp = sum(population),
            jj = sum(jobs),
            price = weighted.mean(avePrice,dd),
            dd = sum(dd), hh = sum(households),
            acc = weighted.mean(autoAccessibility, jobs))


##shose here absolute or relative
commuting_distance_base = commuting_distance %>% filter(scenario == name_of_the_base_scenario)
commuting_distance = commuting_distance %>% left_join(commuting_distance_base, by = c("year", "region"), suffix = c("", "_base"))
rm(commuting_distance_base)

regional_summary = commuting_distance %>% left_join(regional_summary, by = c("region", "year", "scenario"))

region_types = spatial %>% filter(year == 2011) %>% select(region, classifier) %>% unique()

regional_summary = regional_summary %>% left_join(region_types, by = "region")  

commute_time_by_zone_type = regional_summary %>% group_by(year, scenario, classifier) %>%
  summarize(avgTime = weighted.mean(time, pp), avgTime_base =  weighted.mean(time_base, pp))

p = ggplot(commute_time_by_zone_type, aes(x=year, y = avgTime, color = scenario)) +
  geom_path() +
  facet_wrap(.~classifier) + 
  ggtitle("Global average commuting distance by origin region")
ggplotly(p, width = 1200)

p = ggplot(commute_time_by_zone_type, aes(x=year, y = avgTime - avgTime_base, color = scenario)) +
  geom_path() +
  facet_wrap(.~classifier) + 
  ggtitle("Global average commuting distance by origin region with respect of base scenario") 
ggplotly(p, width = 1200)


event_base = events %>% filter(scenario == name_of_the_base_scenario)
events = events %>% left_join(event_base, by= c("year", "event"), suffix = c("", "_base"))
rm(event_base)

p = ggplot(events %>% filter(event == "ConstructionEvent" |
                           event == "MoveEvent"|
                           event == "RenovationEvent"), aes(x=year, y=count, color = scenario)) + geom_line() + 
  facet_wrap(.~event, ncol = 3, scales = "free") + 
  theme(legend.position = "bottom")
ggplotly(p, width = 1200)
p = ggplot(events %>% filter(event == "ConstructionEvent" |
                           event == "MoveEvent"|
                           event == "RenovationEvent"), aes(x=year, y=count - count_base, color = scenario)) + geom_line() + 
  facet_wrap(.~event, ncol = 3, scales = "free") + 
  theme(legend.position = "bottom")
ggplotly(p, width = 1200)



p = ggplot(events %>% filter(event == "MigrationEvent"), aes(x=year, y=count, color = scenario)) + geom_line() + 
  facet_wrap(.~event, ncol = 3, scales = "free") + 
  theme(legend.position = "bottom")
ggplotly(p, width = 1200)





