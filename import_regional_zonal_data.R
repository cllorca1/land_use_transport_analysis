pacman::p_load(data.table, ggplot2, dplyr)

#collect regional data
source("c:/code/matsimScaling/siloAnalysis/readZonesAndRegions.R")

#population_by_region
path_sp = "C:/models/silo/mucSmall/microData/"
file_pp = "pp_2011.csv"

pp = fread(paste(path_sp,file_pp, sep = ""))


pp = merge(x = pp, y = zonesWithRegion, by.x = "homeZone", by.y = "zone")
pp = merge(x = pp, y = zonesWithRegionName, by.x = "homeZone", by.y = "zone")

pp_by_region = pp %>% group_by(region = region_id, region_name = work_region_name) %>% summarize(population = n()) 
