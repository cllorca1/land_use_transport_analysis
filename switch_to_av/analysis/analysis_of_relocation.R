#analysis of the relocation of workers in SILO


pacman::p_load(readr, dplyr, data.table, ggplot2, tidyr, rhdf5, sf, tmap, plotly, ggalluvial)


upper_folder = "c:/models/silo/muc/scenOutput/"


##read silo results

scenarios = c("A_vot","0_vot")
scenario_colors = c("#FF0000","#000000")


modeChoice = data.frame()

for (scenario in scenarios){
  scenario_name = paste("AV", scenario, sep = "")
  this_modeChoice = read_csv(paste(upper_folder, scenario_name, "/siloResults/modeChoiceMicroData.csv", sep  = ""))
  this_modeChoice$scenario = scenario
  modeChoice = modeChoice %>% bind_rows(this_modeChoice)
  rm(this_modeChoice)
  
}

relocation = data.frame()

for (scenario in scenarios){
  scenario_name = paste("AV", scenario, sep = "")
  for (year in 2011:2049){
    this_relocation = read_csv(paste(upper_folder, scenario_name, "/siloResults/relocation/relocation", year, ".csv", sep  = ""))
    this_relocation$year = year
    this_relocation$scenario = scenario
    relocation = relocation %>% bind_rows(this_relocation)
    rm(this_relocation)
  }
}
  
relocation = relocation %>% left_join(modeChoice, by = c("scenario", "year", "hh"))


zoneTypes = read_csv("c:/models/silo/muc/input/zoneSystem.csv")
zoneTypes = zoneTypes %>% select(id = Zone, area = Area, type = BBSR_Type)


relocation = relocation %>% left_join(zoneTypes, by = c("oldZone" = "id"))
relocation = relocation %>% left_join(zoneTypes, by = c("newZone" = "id"), suffix = c("old", "new"))

relocation$typenew = factor(x = relocation$typenew, levels = c(10,20,30,40), labels = c("Core", "Medium city", "Town", "Rural"))
relocation$typeold = factor(x = relocation$typeold, levels = c(10,20,30,40), labels = c("Core", "Medium city", "Town", "Rural"))

relocation$has_av = if_else(relocation$avs == 0, "no", "yes")


flows = relocation %>% group_by(scenario, typeold, typenew, has_av) %>% summarize(count = n())

flows = flows %>% filter(!is.na(has_av))

ggplot(flows, aes(axis1 = typeold, axis2 = typenew, y = count)) +
geom_alluvium() + geom_stratum() + 
  facet_wrap(has_av~scenario, scales = "free")



write.table(relocation %>% group_by(scenario, typeold, typenew) %>% summarize(count = n())%>% spread(scenario, count), "clipboard", sep =  "\t", row.names= F)




