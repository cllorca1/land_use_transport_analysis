
readRelocation = F

scenarios = c("baseCase", "baseCase_MagLev", "MucCoreCityDev_MagLev", "MucCoreCityDev", "MucDev")
#scenarios = c("baseCase", "MucDev")
scenario_names = scenarios


names = data.frame(code = scenarios, name = scenario_names)
file_name = "/resultFileSpatial_2.csv"

file_name_2 = "/siloResults/commutingDistance.csv"



shp = st_read(shapefile_name)


zones_regions = zones_regions %>% select(zone = Zone, region = Region, lkr = Landkreis_ID, areaType = BBSR_Type, municipality = Gemeinde_ID)
spatial = data.frame()

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


commuting_distance = data.frame()
events = data.frame()
relocation_to = data.frame()
relocation_from = data.frame()

for (scenario in scenarios){
  scenario_name = (names %>% filter(code == scenario))$name
  this_spatial = read_csv(paste(folder,scenario,file_name,sep=""))
  this_spatial$scenario = scenario_name
  spatial = spatial %>% bind_rows(this_spatial)
  this_commuting_distance = read_csv(paste(folder,scenario,file_name_2,sep = ""))
  this_commuting_distance$scenario = scenario_name
  commuting_distance = commuting_distance %>% bind_rows(this_commuting_distance)
  this_events = read_csv(paste(folder,scenario,"/siloResults/eventCounts.csv",sep = ""))
  this_events$scenario = scenario_name
  events = events %>% bind_rows(this_events)
  
  if (readRelocation){
    this_scenario_relocation = data.frame()
    for (year in 2011:2049){
      this_year_relocation = read_csv(gzfile(paste(folder,scenario,"/siloResults/relocation/relocation",year,".csv.gz",sep = "")))
      this_scenario_relocation = this_scenario_relocation %>% bind_rows(this_year_relocation)
      print(year)
    }
    
    this_relocation_to = this_scenario_relocation %>% group_by(autos,workers,newZone) %>%
      summarize(count = n()) %>%
      mutate(scenario  = scenario)
    this_relocation_from = this_scenario_relocation %>% group_by(autos,workers,oldZone) %>% 
      summarize(count = n())%>%
      mutate(scenario  = scenario)
    relocation_to = relocation_to %>% bind_rows(this_relocation_to)
    relocation_from = relocation_from %>% bind_rows(this_relocation_from)
  }
}


spatial = spatial %>% left_join(zones_urban, by = "zone")
spatial$dd = spatial$dd_SFD + spatial$dd_SFA + spatial$dd_MF234 + spatial$dd_MF5plus + spatial$dd_MH


rm(names, this_spatial, this_events, this_commuting_distance, file_name, file_name_2, scenario,
   shapefile_name, scenario_name, this_relocation_to, this_relocation_from, this_year_relocation, this_scenario_relocation)
