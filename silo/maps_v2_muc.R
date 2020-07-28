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

spatial_base = spatial %>% filter(scenario == name_of_the_base_scenario, year == 2050)

map  = tm_basemap(leaflet::providers$CartoDB)
map_muni  = tm_basemap(leaflet::providers$CartoDB)

for (selected_scenario in scenarios){
  if (selected_scenario != name_of_the_base_scenario){
    this_scenario_spatial = spatial %>% filter(scenario == selected_scenario, year == 2050)
    this_scenario_spatial = this_scenario_spatial %>% left_join(spatial_base, by = c("year", "zone", "region", "lkr", "areaType", "municipality", "is_core", "is_muc", "core"), 
                                                                                        suffix = c("","_base"))
    this_scenario_spatial =  this_scenario_spatial %>%
    select(zone,transitAccessibility, transitAccessibility_base, region) %>% 
      mutate(difference = transitAccessibility - transitAccessibility_base)
    
    this_shp = shp %>% left_join(this_scenario_spatial, by = c("shp_id"= "zone", "region"))
    
    
    this_shp_muni = this_shp %>% group_by(municipality) %>%
      summarize(transitAccessibility = sum(transitAccessibility), transitAccessibility_base = sum (transitAccessibility_base)) %>% 
      mutate(difference = transitAccessibility - transitAccessibility_base)
    
    map =  map + tm_shape(this_shp, name = selected_scenario) +
      tm_fill(col ="difference", sep = "", palette="-RdBu")
    
    map_muni =  map_muni + tm_shape(this_shp_muni, name = selected_scenario) +
      tm_fill(col ="difference", sep = "", palette="-RdBu")
  }
}
tmap_leaflet(map)
tmap_leaflet(map_muni)

