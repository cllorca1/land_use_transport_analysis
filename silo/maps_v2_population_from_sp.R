#sp analyzer

pacman::p_load(readr, dplyr, ggplot2, reshape, plotly, sf, leaflet, tmap, tidyr, here)

##upper folder of results - one level up of scenarios
folder = "c:/models/fabilut/muc/svn/29052020_"
##upper folder of inputs
folder_input = "c:/code/msm-visualizer/"

##name of the base scenario
name_of_the_base_scenario = "baseCase"

scenarios = c("baseCase", "baseCaseMagLev", "MucCoreCityDev_MagLev", "MucCoreCityDev", "MucDev")
scenario_names = scenarios

file_name_pp = "/microData/pp_2050.csv"
file_name_hh = "/microData/hh_2050.csv"
file_name_dd = "/microData/dd_2050.csv"

shapefile_name = paste(folder_input,"/map/muc/zones_31468.shp", sep ="")

shp = st_read(shapefile_name)

zones_regions = read_csv(paste(folder_input,"/examples/muc/zoneSystem.csv", sep =""))
zones_regions = zones_regions %>% select(zone = Zone, region = Region, lkr = Landkreis_ID, areaType = BBSR_Type, municipality = Gemeinde_ID)
shp = shp %>% left_join(zones_regions, by=c("shp_id" = "zone"))
shp_regions = shp %>% group_by(region) %>% summarise()

##define several spatial classifications
lkrs_core = c(9162, 9761, 9161, 9163, 9261)
zones_urban = zones_regions %>%
  mutate (is_core = if_else(lkr %in% lkrs_core, "core", "no-core")) %>% 
  mutate (is_muc = if_else(lkr == 9162, "muc", "no-muc")) %>% 
  mutate (core = if_else(lkr  %in% lkrs_core, lkr , 9999))

zones_urban$core = factor(zones_urban$core, levels =  c(9162, 9761, 9161, 9163, 9261, 9999),
                          labels = c("Munich", "Augsburg", "Ingolstadt","Rosenheim", "Landshut","Not a core city"))

all_summary = data.frame()

for (selected_scenario in scenarios){
  this_pp = read_csv(paste(folder, selected_scenario, file_name_pp, sep  =""))
  this_dd = read_csv(paste(folder, selected_scenario, file_name_dd, sep  =""))
  this_hh = read_csv(paste(folder, selected_scenario, file_name_hh, sep  =""))
   
  this_pp = this_pp %>%
    mutate(is_worker = if_else(workplace >0, 1,0)) %>%
    mutate(is_adult = if_else(age >18, 1,0))
  this_pp = this_pp %>% group_by(hhid) %>% summarize(people = n(), adults = sum(is_adult), workers = sum(is_worker))
  this_hh = this_hh %>% left_join(this_dd, by = c("dwelling" = "id"))
  this_hh = this_hh %>% select(id, zone, autos)
  
  this_pp = this_pp %>% left_join(this_hh, by = c( "hhid" = "id"))
  this_pp = this_pp%>% mutate(is_transit_captive = if_else(autos == 0 & workers > 0, 1, 0))
  this_summary = this_pp  %>% group_by(zone, is_transit_captive) %>% summarize(hh = n(), pp = sum(people))
  this_summary$scenario = selected_scenario
  
  all_summary = all_summary %>% bind_rows(this_summary)
  
  rm(this_pp, this_hh, this_dd, this_summary)
}


all_summary = all_summary %>% left_join(zones_urban, by = c("zone"))

##rename transit captive variable
all_summary$is_transit_captive  = factor(all_summary$is_transit_captive, levels = c(0,1), labels = c("with some autos or without workers", "no autos and workers"))

summary_by_core = all_summary %>% group_by(core,is_transit_captive, scenario) %>% summarize(hh = sum(hh), pp = sum(pp))


ggplot(summary_by_core, aes(x = scenario, y  = hh, fill = scenario)) +
  geom_bar(stat = "identity", color = "black") +
  facet_grid(is_transit_captive~core, scales = "free") + 
  theme(axis.text.x = element_text(angle = 90))

summary_by_core_base = summary_by_core %>% filter(scenario == name_of_the_base_scenario)
summary_by_core = summary_by_core %>% left_join(summary_by_core_base, by = c("is_transit_captive", "core"), suffix = c("", "_base"))


ggplot(summary_by_core, aes(x = scenario, y  = (hh - hh_base), fill = scenario)) +
  geom_bar(stat = "identity", color = "black") +
  facet_grid(is_transit_captive~core) + 
  theme(axis.text.x = element_text(angle = 90))
#ggplotly()


p = ggplot(summary_by_core, aes(x = scenario, y  = (hh - hh_base)/hh_base * 100, fill = scenario)) +
  geom_bar(stat = "identity", color = "black") +
  facet_grid(is_transit_captive~core) + 
  theme(axis.text.x = element_text(angle = 90))
ggplotly(p)


