pacman::p_load(readr, dplyr, data.table, ggplot2, tidyr, rhdf5, sf, tmap)


upper_folder = "c:/models/silo/muc/scenOutput/"

years = c(2011, 2020, 2030, 2040, 2050)

scenarios = c("AV0_parking_2","AVA_parking_2")
scenario_labels = c("no-AV", "AV")


##read silo results

modeChoice = data.frame()

for (i in 1:length(scenarios)){
  scenario_label = scenario_labels[i]
  scenario = scenarios[i]
  scenario_name = paste(scenario, sep = "")
  this_modeChoice = read_csv(paste(upper_folder, scenario_name, "/siloResults/modeChoiceMicroData.csv", sep  = ""))
  this_modeChoice$scenario = scenario_label
  modeChoice = modeChoice %>% bind_rows(this_modeChoice)
  rm(this_modeChoice)
  
}



modeChoice$zeroAutoHh = if_else(modeChoice$autos == 0, "zero-autos", if_else(modeChoice$avs == 0 ,  "hh-with-cvs", "hh-with-avs"))

color_modes = c("#d43838", "#6f6f6f","#2563a9")


modeChoice_summary = modeChoice %>% group_by(scenario, year, mode) %>% summarize(trips = n())

modeChoice_summary$mode = factor(modeChoice_summary$mode, levels = c("av", "car", "pt"), labels = c("AV", "CV", "PT"))

modeChoice_summary$scenario = factor(modeChoice_summary$scenario, levels = scenario_labels)

ggplot(modeChoice_summary, aes(x=year, fill = mode, y = trips,  color = as.factor(mode))) + geom_bar(stat = "identity", position = "fill", size = 2) +
  scale_fill_manual(values= color_modes) + 
  scale_color_manual(values= color_modes) + facet_wrap(.~scenario, ncol = 3) + 
  xlab("Year") + ylab("Share") + labs(fill = "Mode", color = "Mode") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90))


ggplot(modeChoice_summary %>% filter(scenario %in% c("no-AV", "AV")), aes(x=year, fill = mode, y = trips,  color = as.factor(mode))) + geom_bar(stat = "identity", position = "fill", size = 2) +
  scale_fill_manual(values= color_modes) + 
  scale_color_manual(values= color_modes) + facet_wrap(.~scenario, ncol = 3) + 
  xlab("Year") + ylab("Share") + labs(fill = "Mode", color = "Mode") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90), legend.position = "bottom") 


ggsave("C:/projects/Papers/2020_cities/figs/modal_share_relocation_2.pdf", width = 10, units = "cm", height = 7, scale = 1.5)


# modeChoice_scA = modeChoice %>% filter(scenario == "A")
# 
# ggplot(modeChoice_scA, aes(x=year, fill = mode, color = mode)) + geom_bar(stat = "count", position = "fill") +
#   scale_fill_manual(values= color_modes) + scale_color_manual(values= color_modes)
# 
# 
# color_modes = c("indianRed4", "royalblue3")
# 
# modeChoice_sc0 = modeChoice %>% filter(scenario == "0")
# 
# ggplot(modeChoice_sc0, aes(x=year, fill = mode, color = mode)) + 
#   geom_bar(stat = "count", position = "fill") + scale_fill_manual(values= color_modes) + scale_color_manual(values= color_modes)



# ggplot(modeChoice, aes(x=year, fill = mode)) + geom_bar(stat = "count", position = "stack") + 
#   facet_wrap(.~zeroAutoHh)
# 
# ggplot(modeChoice, aes(x=year, fill = mode)) + geom_bar(stat = "count", position = "stack")
# 
# 
# ggplot(modeChoice, aes(x=year, fill = mode)) + geom_bar(stat = "count", position = "fill") + 
#   facet_grid(parkingAtWork~zeroAutoHh)
# 
# 
# ggplot(modeChoice, aes(x=year, fill = mode)) + geom_bar(stat = "count", position = "fill") + 
#   facet_grid(parkingAtWork~.)


#link the distance on the network to the data set
source("c:/code/omx/api/r/omx2.r")

path_to_matrix = "c:/models/silo/muc/skims/skimsAllIntrazonal.omx"

matrix = readMatrixOMX(path_to_matrix, "distanceByTime")

modeChoice = modeChoice %>% rowwise() %>% mutate(distance = matrix[homeZone, workZone])


vkt = modeChoice %>% group_by(scenario, year, mode) %>% summarize(n = n(), vkt = sum(distance), avg_distance = mean(distance))

vkt$scenario = factor(vkt$scenario, levels = scenario_labels)
vkt$mode = factor(vkt$mode, levels = c("av", "car", "pt"), labels = c("AV", "CV", "PT"))

ggplot(vkt %>% filter(mode != "PT"), aes(x = year, y= vkt * 20 *2 / 1e9, fill = mode, color = mode)) + geom_bar(stat = "identity") + 
  facet_wrap(.~scenario, ncol = 3) +
  scale_fill_manual(values = color_modes) + scale_color_manual(values = color_modes) + xlab("Year") +
  ylab("Distance traveled (10E9 km)") + 
  labs(fill = "Mode", color = "Mode") + theme_bw() +
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom")

ggsave("C:/projects/Papers/2020_cities/figs/VKT.pdf", width = 15, units = "cm", height = 10, scale = 1.3)

# 
# modeChoice$scenario = factor(modeChoice$scenario, levels = scenarios)
# 
# scenario_colors = c("#000000", "#FF0000", "#DE5959", "#D98282", "#C99797", "#C7B3B3")
# 
# ggplot(modeChoice %>% filter(year == 2049), aes(x=distance, color = scenario)) + stat_ecdf(size = 1) + scale_color_manual(values = scenario_colors)
# 
# 
# zones = st_read("c:/models/silo/muc/input/zonesShapefile/zones.shp")
# 
# modeChoiceAtJob  =modeChoice %>% group_by(year, workZone, mode) %>% summarize(count = n()) %>% spread(mode, count, fill = 0)
# modeChoiceAtJob$total = modeChoiceAtJob$av + modeChoiceAtJob$car + modeChoiceAtJob$pt
# modeChoiceAtJob$shareAv = modeChoiceAtJob$av / modeChoiceAtJob$total
# modeChoiceAtJob$sharePt = modeChoiceAtJob$pt / modeChoiceAtJob$total
# modeChoiceAtJob$shareCv = modeChoiceAtJob$car / modeChoiceAtJob$total
# 
# zones = zones %>% left_join(modeChoiceAtJob %>% filter(year == 2011), by = c("id" = "workZone"))
# zones [is.na(zones)] = 0 #fills zones without observation of mode choice
# 
# regions = zones %>% group_by(AGS) %>% summarize(shareAv = weighted.mean(shareAv, total), 
#                                                 shareCv = weighted.mean(shareCv, total),
#                                                 sharePt = weighted.mean(sharePt, total))
# 
# p  = tm_basemap(leaflet::providers$CartoDB) +
#   tm_shape(regions, name = "Av") +  tm_polygons(col = "shareAv") + 
#   tm_shape(regions, name = "Pt")  + tm_polygons(col = "sharePt") + 
#   tm_shape(regions, name = "Cv") + tm_polygons(col = "shareCv")
# 
# tmap_leaflet(p)
# 
# 
# zoneTypes = read_csv("c:/models/silo/muc/input/zoneSystem.csv")
# 
# zoneTypes = zoneTypes %>% select(id = Zone, area = Area, type = BBSR_Type)
# 
# shareByJobZoneType = zoneTypes %>% left_join(modeChoiceAtJob , by = c("id" = "workZone"))
# shareByJobZoneType [is.na(shareByJobZoneType)] = 0
# 
# shareByJobZoneType = shareByJobZoneType %>% group_by(type, year) %>% summarize(shareAv = weighted.mean(shareAv, total), 
#                                                          shareCv = weighted.mean(shareCv, total),
#                                                          sharePt = weighted.mean(sharePt, total))
# 
# shareByJobZoneType = shareByJobZoneType %>% gather(3:5, key = "mode", value = "share")
# 
# shareByJobZoneType$typeWord = factor(x = shareByJobZoneType$type, levels = c(10,20,30,40), labels = c("Core", "Medium city", "Town", "Rural"))
# 
# ggplot(shareByJobZoneType %>% filter(year %in% selected_years), aes(x = typeWord, fill = mode, y = share)) +
#   geom_bar(stat = "identity") +
#   facet_wrap(.~year)
# 
# 
# modeChoiceAtHome  =modeChoice %>% group_by(year, homeZone, mode) %>% summarize(count = n()) %>% spread(mode, count, fill = 0)
# modeChoiceAtHome$total = modeChoiceAtHome$av + modeChoiceAtHome$car + modeChoiceAtHome$pt
# modeChoiceAtHome$shareAv = modeChoiceAtHome$av / modeChoiceAtHome$total
# modeChoiceAtHome$sharePt = modeChoiceAtHome$pt / modeChoiceAtHome$total
# modeChoiceAtHome$shareCv = modeChoiceAtHome$car / modeChoiceAtHome$total
# 
# 
# shareByHomeZoneType = zoneTypes %>% left_join(modeChoiceAtHome , by = c("id" = "homeZone"))
# shareByHomeZoneType [is.na(shareByHomeZoneType)] = 0
# 
# shareByHomeZoneType = shareByHomeZoneType %>% group_by(type, year) %>% summarize(shareAv = weighted.mean(shareAv, total), 
#                                                                                shareCv = weighted.mean(shareCv, total),
#                                                                                sharePt = weighted.mean(sharePt, total))
# 
# shareByHomeZoneType = shareByHomeZoneType %>% gather(3:5, key = "mode", value = "share")
# 
# shareByHomeZoneType$typeWord = factor(x = shareByHomeZoneType$type, levels = c(10,20,30,40), labels = c("Core", "Medium city", "Town", "Rural"))
# 
# ggplot(shareByHomeZoneType %>% filter(year %in% selected_years), aes(x = typeWord, fill = mode, y = share)) +
#   geom_bar(stat = "identity") +
#   facet_wrap(.~year)
# 
# 
# modeChoiceByParkingQuality = modeChoice %>% group_by(year, parkingAtHome, parkingAtWork, mode) %>%
#   summarize(count = n()) 
# 
# 
# 
# ggplot(modeChoiceByParkingQuality %>% filter(year %in% selected_years), aes(x=as.factor(parkingAtWork), y = count, fill = mode)) +
#   geom_bar(stat = "identity", position = "fill") + facet_grid(parkingAtHome~year)
