pacman::p_load(data.table, dplyr, ggplot2, tidyr, readr, plotly, sf, tmap, rgeos, RColorBrewer)


silo_folder = "C:/models/silo/muc/"

scenarios = c("base_5", "AV0_parking_2","AVA_parking_2")
scenario_names = c("base", "no-AV", "AV")
scales = c(0.05, 0.05,0.05)


my_scale_linetype = c("dashed", "solid")
my_scale_linetype = c("solid") # for only one scenario
maxYear = 2050


regional_data = data.frame()
for (i in 1:length(scenarios)){

  scenario = scenarios[i]
  scale = scales[i]
  scenario_name = scenario_names[i]

  this_commute_times =
    read_csv(paste(silo_folder,"scenOutput/",
                   scenario, "/siloResults/regionAvCommutingTime.csv", sep =""))


  this_hh_satisfaction =
    read_csv(paste(silo_folder,"scenOutput/",
                   scenario, "/siloResults/hhSatisfactionByRegion.csv", sep =""))

  this_land = read_csv(paste(silo_folder,"scenOutput/",
                             scenario, "/siloResults/regionAvailableLand.csv", sep =""))

  this_hh_satisfaction = this_hh_satisfaction %>%
    group_by(year, region) %>%
    summarise(hh = sum(hh_count), satisfaction = weighted.mean(hh_ave_satisfaction, hh_count))



  this_commute_times = this_hh_satisfaction %>% left_join(this_commute_times, by = c("year", "region" = "aveCommuteDistByRegion"))

  this_commute_times = this_land %>% left_join(this_commute_times, by = c("year", "region"))


  this_commute_times = this_commute_times %>%
    mutate(scenario = scenario) %>%
    mutate(scenario_name = scenario_name) %>%
    mutate(scale = scale)

  regional_data = regional_data %>% bind_rows(this_commute_times)

}
# 
# regional_data_summary = regional_data %>%
#   group_by(scenario, scenario_name, scale, year) %>% 
#   summarize(commute_time = weighted.mean(minutes, hh, na.rm = T), land = sum(land/scale))
# 
# ggplot(regional_data_summary %>% filter(year < maxYear), aes(x = year, y =  commute_time, 
#                                   color = scenario_name, group = scenario, 
#                                   linetype = as.factor(scale))) +
#   geom_line() 
# 
# ggplot(regional_data_summary %>% filter(year < maxYear), aes(x = year, y =  land, 
#                                   color = scenario_name, group = scenario, linetype = as.factor(scale))) + geom_line()


#####define zone classification based on population density

spatial_results = read_csv(paste(silo_folder, "scenOutput/", scenarios[1], "/siloResults/resultFileSpatial.csv", sep =""))

zones_data = spatial_results %>% filter(year == 2015) %>% select(zone, population) 

zone_properties = read_csv(paste(silo_folder,"input/zoneSystem.csv", sep =""))

zone_properties = zone_properties %>% select(Zone, Name, Area, Region)

zones_data = zones_data %>% left_join(zone_properties, by = c("zone" = "Zone"))

subzones = zones_data %>%
  group_by(Region) %>%
  summarise(population = sum(population), area_m2 = sum(Area))


subzones = subzones %>% mutate(pop_density_km2 = population / area_m2 * 1e6)
ggplot(subzones
       , aes(x = pop_density_km2)) + geom_histogram()

q = quantile(subzones$pop_density_km2, probs = c(-0,0.2, 0.4,0.6,0.8,1.00))

subzones = subzones %>% mutate(density_bin = cut(pop_density_km2, q, include.lowest = T))

summary(subzones
       $density_bin)

####assign a density bin to each zone based on its district
zones_data = zones_data %>%
  group_by(Region) %>%
  mutate(population_quantile = sum(population), area_quantile = sum(Area)) %>%
  mutate(pop_density_km2 = population_quantile / area_quantile * 1e6) %>%
  mutate(density_bin = cut(pop_density_km2, q, include.lowest = T))

summary(zones_data$density_bin)

write.table(zones_data, sep = "\t", row.names = F, "clipboard-2000k" )


#####repeat the spatial analysis by grouping by "section density"

summary_by_group = data.frame()

for (i in 1:length(scenarios)){
  
  scenario = scenarios[i]
  scale = scales[i]
  scenario_name = scenario_names[i]
  
  spatial_results = read_csv(paste(silo_folder, "scenOutput/", scenario, "/siloResults/resultFileSpatial.csv", sep =""))
  
  spatial_results = spatial_results %>%
    mutate(dwellings = dd_MF5plus + dd_MF234 + dd_SFA + dd_SFD)
  
  spatial_results = spatial_results %>%
    left_join(zones_data %>% ungroup() %>% select(zone, density_bin), by = "zone")
  
  
  this_summary = spatial_results %>%
    group_by(year, density_bin) %>%
    summarize(pp = sum(population)/scale,
              hh = sum(households)/scale,
              jj = sum (jobs)/scale,
              dd = sum(dwellings)/scale, 
              dd_1_MF5 = sum(dd_MF5plus)/scale,
              dd_2_MF234 = sum(dd_MF234)/scale,
              dd_3_SFA = sum(dd_SFA)/scale,
              dd_4_SFD = sum(dd_SFD)/scale,
              land = sum(availLand)/scale, 
              price = weighted.mean(avePrice, dwellings)) %>%
              mutate(vacant_dd = (dd - hh)/dd * 100) %>%
    pivot_longer(c(pp, hh, jj, dd, vacant_dd, land, price, dd_1_MF5, dd_2_MF234, 
                   dd_3_SFA, dd_4_SFD))
  
  this_summary = this_summary %>%
    mutate(scenario = scenario) %>%
    mutate(scenario_name = scenario_name) %>%
    mutate(scale = scale)
  
  summary_by_group = summary_by_group %>% bind_rows(this_summary)
  
  rm(spatial_results, this_summary)
}

summary_by_group$name = factor(x = summary_by_group$name, 
                               levels = c("pp", "hh", "jj", "dd", "land", "price", "vacant_dd",
                                          "dd_1_MF5", "dd_2_MF234", "dd_3_SFA", 
                                          "dd_4_SFD"),
                               labels = c("Popualtion", "Households", "Jobs", "Dwellings", "Land", "Price", "Vacancy rate",
                                          "MF5", "MF234", "SFA", "SFD"))
                                 

summary_by_group$density_bin = factor(x = summary_by_group$density_bin, 
                                      labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"))

ggplot(summary_by_group %>% filter(name %in% c("Dwellings", "Land"), year < maxYear), 
       aes(x = year, y = value, color = scenario_name, group = scenario, linetype = as.factor(scale))) +
  geom_line(size = 1) +
  facet_grid(name~density_bin, scales = "free") + 
  xlab("Year") + ylab("Value") + theme_bw() + theme(legend.position = "bottom") +
  scale_linetype_manual(values = my_scale_linetype)

#ggsave("analysis/tmp/dd_land_group.png", device = "png", width = 15, height = 10, units = "cm", scale = 2)

ggplot(summary_by_group %>% filter( name %in% c("Dwellings", "Price", "Vacancy rate"), year < maxYear), 
       aes(x = year, y = value, color = scenario_name, group = scenario, linetype = as.factor(scale))) +
  geom_line(size = 1) +
  facet_grid(name~density_bin, scales = "free") + 
  xlab("Year") + ylab("Value") + theme_bw() + theme(legend.position = "bottom") +
  scale_linetype_manual(values = my_scale_linetype)

#ggsave("analysis/tmp/real_estate_group.png", device = "png", width = 15, height = 10, units = "cm", scale = 2)

ggplot(summary_by_group %>% filter(name %in% c("Popualtion", "Dwellings"), year < maxYear), 
       aes(x = year, y = value, color = scenario_name, group = scenario, linetype = as.factor(scale))) +
  geom_line(size = 1) +
  facet_grid(name~density_bin, scales = "free") + 
  xlab("Year") + ylab("Value") + theme_bw() + theme(legend.position = "bottom") +
  scale_linetype_manual(values = my_scale_linetype)

#ggsave("analysis/tmp/overview_group.png", device = "png", width = 15, height = 10, units = "cm", scale = 2)

ggplot(summary_by_group %>% filter(name %in% c("MF5", "MF234", "SFA", 
                                               "SFD"), year < maxYear), 
       aes(x = year, y = value, color = scenario_name, group = scenario, linetype = as.factor(scale))) +
  geom_line(size = 1) +
  facet_grid(name~density_bin, scales = "free") + 
  xlab("Year") + ylab("Value") + theme_bw() + theme(legend.position = "bottom")

#ggsave("analysis/tmp/dd_types.png", device = "png", width = 15, height = 10, units = "cm", scale = 2)

##repeat the regional analysis by region group region == zone!


region_properties = zones_data %>% group_by(Region) %>%
  summarize(region_population = sum(population), region_area = sum(Area)) %>% 
  rowwise() %>%
  mutate(pop_density = region_population/region_area)

q = quantile(region_properties$pop_density, probs = c(-0,0.2, 0.4,0.6,0.8,1.00))

region_properties = region_properties %>% mutate(density_bin = cut(pop_density, q, include.lowest = T))


regional_data = regional_data %>% left_join(region_properties, by = c("region" = "Region"))

regional_data_summary_by_group = regional_data %>%
  group_by(scenario, scenario_name, scale, year, density_bin) %>% 
  summarize(commute_time = weighted.mean(minutes, hh, na.rm = T), land = sum(land/scale))

regional_data_summary_by_group$density_bin = factor(x = regional_data_summary_by_group$density_bin, 
                                          labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"))


ggplot(regional_data_summary_by_group %>% filter(year < maxYear), aes(x = year, y =  commute_time, 
                                  color = scenario_name, group = scenario, 
                                  linetype = as.factor(scale))) +
  geom_line(size = 1) + 
  facet_wrap(.~density_bin, ncol = 5)  + 
  xlab("Year") + ylab("Average commute time (min)") + theme_bw() + theme(legend.position = "bottom") + 
  scale_linetype_manual(values = my_scale_linetype)

#ggsave("analysis/tmp/commute_dist.png", device = "png", width = 15, height = 10, units = "cm", scale = 2)

# ggplot(regional_data_summary_by_group  %>% filter(year < maxYear), aes(x = year, y =  land, 
#                                   color = scenario_name,
#                                   group = scenario, linetype = as.factor(scale))) + 
#   geom_line(size = 1) +
#   facet_wrap(.~density_bin)


###modal shares and car-ownership###


modal_share_by_group = data.frame()

for (i in 1:length(scenarios)){
  
  scenario = scenarios[i]
  scale = scales[i]
  scenario_name = scenario_names[i]
  
  this_modal_share_by_zone = read_csv(paste(silo_folder, "scenOutput/",scenario, "/siloResults/modalShares.csv", sep = ""))
 
  
  this_modal_share_by_zone = this_modal_share_by_zone %>%
    left_join(zones_data %>% ungroup() %>% select(zone, density_bin), by = "zone")
  
  
  this_modal_share_by_zone = this_modal_share_by_zone %>%
    group_by(year, density_bin) %>%
    summarize(car = sum(tripsCar), 
              pt = sum(tripsPt),
              other = sum(tripsOther),
              car_time = sum(totalTimeCar),
              pt_time = sum(totalTimePt),
              other_time = sum(totalTimeOther),
              no_travel = sum(doNotTravel)) %>%
    mutate(car_time = car_time / car) %>%
    mutate(pt_time = pt_time / pt) %>%
    mutate(other_time = other_time / other) %>%
    mutate(car_share = car / (car + pt + other)) %>%
    mutate(car_share = pt / (car + pt + other)) %>%
    mutate(other_share = other / (car + pt + other)) %>%
    pivot_longer(c(car, pt, other, car_time, pt_time, other_time, no_travel))
    
  
    this_modal_share_by_zone = this_modal_share_by_zone %>%
    mutate(scenario = scenario) %>%
    mutate(scenario_name = scenario_name) %>%
    mutate(scale = scale)
  
  modal_share_by_group = modal_share_by_group %>% bind_rows(this_modal_share_by_zone)
  
  
}
  

modal_share_by_group$density_bin = factor(x = modal_share_by_group$density_bin, 
                                      labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"))

ggplot(modal_share_by_group %>% filter(name %in% c("car", "pt", "other")), aes(x=year, y = value/scale, fill = name, color = name)) +
  geom_bar(stat = "identity", position = "fill") + facet_grid(scenario~density_bin) + 
  scale_fill_manual(values = c("car" = "#3b5998", "pt" = "#c4cde0", "other" = "red")) + 
  scale_color_manual(values = c("car" = "#3b5998", "pt" = "#c4cde0", "other" = "red")) + 
  theme_bw() + theme(legend.position = "bottom") + 
  scale_y_continuous(expand = c(0,0)) + xlab("Year") + ylab("Modal share")

#ggsave("analysis/tmp/modal_share.png", device = "png", width = 15, height = 10, units = "cm", scale = 2)

ggplot(modal_share_by_group %>% filter(name %in% c("car", "pt", "other", "no_travel")), aes(x=year, y = value/scale, fill = name, color = name)) +
  geom_bar(stat = "identity", position = "stack") + facet_wrap(scenario~density_bin, ncol = 5) + 
  ggtitle("Trips by region (from low density to high density - left to right)") 

#ggsave("analysis/tmp/trips_mode_region.png", device = "png", width = 15, height = 10, units = "cm", scale = 2)


ggplot(modal_share_by_group %>%
         filter(name %in% c("car_time", "pt_time", "other_time")), aes(x=year, y = value, color = scenario_name, linetype = as.factor(scale))) +
  geom_line(stat = "identity", size = 1) + facet_grid(name~density_bin) + 
 theme_bw() + 
  xlab("Year") + ylab("Average travel time by mode (min)")

#ggsave("analysis/tmp/time_by_mode.png", device = "png", width = 15, height = 10, units = "cm", scale = 2)

  
modal_share_by_group_wide = modal_share_by_group %>% pivot_wider(names_from = name, values_from  = value)
  
modal_share_by_group_wide[is.na(modal_share_by_group_wide)] = 0

modal_share_by_group_wide = modal_share_by_group_wide %>%
  mutate(w_ave_commute_time = (car * car_time + pt * pt_time + other*other_time)/(car + pt + other))


ggplot(modal_share_by_group_wide, aes(x=year, y = w_ave_commute_time, color = scenario_name, linetype = as.factor(scale))) +
  geom_line(stat = "identity", size = 1) + facet_wrap(.~density_bin, ncol = 5) + 
  theme_bw() + theme(legend.position = "bottom") + 
  xlab("Year") + ylab("Average commute time (min) (selected mode)") 

#ggsave("analysis/tmp/average_time_wighted.png", device = "png", width = 15, height = 10, units = "cm", scale = 2)


modal_share_by_group_base_year = modal_share_by_group %>% filter(year == 2015)

ggplot(modal_share_by_group_base_year %>% filter(name %in% c("car", "pt", "other")), aes(x=density_bin, y = value/scale, fill = name, color = name)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("car" = "#3b5998", "pt" = "#c4cde0", "other" = "red")) + 
  scale_color_manual(values = c("car" = "#3b5998", "pt" = "#c4cde0", "other" = "red")) + 
  theme_bw() + theme(legend.position = "bottom") + 
  scale_y_continuous(expand = c(0,0)) + xlab("Year") + ylab("Modal share") + facet_wrap(.~scenario)

#ggsave("analysis/tmp/modal_share_base.png", device = "png", width = 15, height = 10, units = "cm", scale = 2)


##only for one scenario
# ggplot(modal_share_by_group_base_year %>% filter(name %in% c("car", "pt"), scenario == "base_1_without_transport_v2"), aes(x=density_bin, y = value/scale, fill = name, color = name)) +
#   geom_bar(stat = "identity", position = "fill") +
#   scale_fill_manual(values = c("car" = "#3b5998", "pt" = "#c4cde0")) + 
#   scale_color_manual(values = c("car" = "#3b5998", "pt" = "#c4cde0")) + 
#   theme_bw() + theme(legend.position = "bottom") + 
#   scale_y_continuous(expand = c(0,0)) + xlab("Year") + ylab("Modal share") + facet_wrap(.~scenario)



# ###car-ownership### global
# scenarios = c("base_10_test_test", "base_10_test_with_plans")
#   scenario = "base_10_test_with_plans"
#   scale = .1
#   scenario_name = "base"
#   
#   this_car_ownership = read_csv(paste(silo_folder, "scenOutput/",scenario, "/siloResults/carOwnership.csv", sep = ""))
#   
#   this_car_ownership = this_car_ownership %>% pivot_wider(id_cols = year, names_from = carOwnershipLevel, values_from = households, names_prefix = "cars" )
# 
#   this_car_ownership = this_car_ownership %>% mutate(avg_number_of_cars = (cars1 + 2*cars2 + 3 * cars3) / (cars0 + cars1 + cars2 + cars3))
#   ggplot(this_car_ownership, aes(x = year, y = avg_number_of_cars)) + geom_path() + geom_point() + ylim(0,1)
#   
#   