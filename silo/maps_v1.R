library(here)

source(paste(here(), "/silo/data_reader_v2_muc.R", sep  =""))


relocation_to_2 = relocation_to %>%
  #filter(autos == 0, workers > 0) %>%
  group_by(scenario, newZone) %>% summarize(count_to = sum(count))

relocation_from_2 = relocation_from %>%
  #filter(autos == 0, workers > 0) %>%
  group_by(scenario, oldZone) %>% summarize(count_from = sum(count))

relocation_to_2 = relocation_to_2 %>% inner_join(relocation_from_2, by = c("scenario", "newZone"="oldZone"))

relocation_to_2$balance = relocation_to_2$count_to - relocation_to_2$count_from

relocation_to_2 = relocation_to_2 %>% left_join(zones_regions, by=c("newZone"= "zone"))
relocation_to_region = relocation_to_2 %>% group_by(scenario, region) %>% summarize(balance = sum(balance))

relocation_to_by_area_type = relocation_to_2 %>% group_by(scenario, areaType) %>% summarize(balance = sum(balance))



relocation_to_region_base = relocation_to_region %>% filter(scenario == "baseCase") %>% ungroup() %>%  select(region, balance_base = balance)
relocation_to_region_comparison = relocation_to_region %>% filter(scenario != "baseCase") %>% ungroup() %>%select(region, balance = balance)

relocation_to_region_comparison = relocation_to_region_comparison %>% left_join(relocation_to_region_base)
relocation_to_region_comparison = relocation_to_region_comparison %>%
  mutate(balance_dif = balance - balance_base) %>%
  mutate(balance_rel_dif = balance_dif / balance_base * 100)


shp_regions = shp %>% group_by(region) %>% summarise()



shp_regions = shp_regions %>% left_join(relocation_to_region_comparison, by = "region")

map  = tm_basemap(leaflet::providers$CartoDB)
map =  map + tm_shape(shp_regions, name = "move_to") +
  tm_fill(col ="balance", sep = "") +
   tm_borders()
tmap_leaflet(map)

map
