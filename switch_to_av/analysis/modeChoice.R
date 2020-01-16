pacman::p_load(readr, dplyr, data.table, ggplot2, tidyr, rhdf5, sf)


upper_folder = "c:/models/silo/muc/scenOutput/"

scenario = "baseAV3_nt"

##read silo results

modeChoice = read_csv(paste(upper_folder, scenario, "/siloResults/modeChoiceMicroData.csv", sep  = ""))

modeChoice$zeroAutoHh = if_else(modeChoice$autos == 0, "zero-autos", if_else(modeChoice$avs ==0,  "hh-with-cvs", "hh-with-avs"))

ggplot(modeChoice, aes(x=year, fill = mode)) + geom_bar(stat = "count", position = "fill") + 
  facet_wrap(.~zeroAutoHh)

ggplot(modeChoice, aes(x=year, fill = mode)) + geom_bar(stat = "count", position = "stack") + 
  facet_wrap(.~zeroAutoHh)

ggplot(modeChoice, aes(x=year, fill = mode)) + geom_bar(stat = "count", position = "stack")


ggplot(modeChoice, aes(x=year, fill = mode)) + geom_bar(stat = "count", position = "fill") + 
  facet_grid(parkingAtWork~zeroAutoHh)


ggplot(modeChoice, aes(x=year, fill = mode)) + geom_bar(stat = "count", position = "fill") + 
  facet_grid(parkingAtWork~.)


#link the distance on the network to the data set
source("c:/code/omx/api/r/omx2.r")

path_to_matrix = "c:/models/silo/muc/skims/skimsAllIntrazonal.omx"

matrix = readMatrixOMX(path_to_matrix, "distanceByTime")

modeChoice = modeChoice %>% rowwise() %>% mutate(distance = matrix[homeZone, workZone])

selected_years = c(2011,2030,2049)

ggplot(modeChoice %>% filter(year %in% selected_years), aes(x=distance, fill = mode)) +
  geom_area(stat = "bin", position = "stack") + facet_grid(zeroAutoHh~year, scales = "free_y")





zones = st_read("c:/models/silo/muc/input/zonesShapefile/zones.shp")

modeChoiceAtJob  =modeChoice %>% group_by(year, workZone, mode) %>% summarize(count = n()) %>% spread(mode, count, fill = 0)
modeChoiceAtJob$total = modeChoiceAtJob$av + modeChoiceAtJob$car + modeChoiceAtJob$pt
modeChoiceAtJob$shareAv = modeChoiceAtJob$av / modeChoiceAtJob$total
modeChoiceAtJob$sharePt = modeChoiceAtJob$pt / modeChoiceAtJob$total
modeChoiceAtJob$shareCv = modeChoiceAtJob$car / modeChoiceAtJob$total

zones = zones %>% left_join(modeChoiceAtJob %>% filter(year == 2011), by = c("id" = "workZone"))
zones [is.na(zones)] = 0 #fills zones without observation of mode choice

regions = zones %>% group_by(AGS) %>% summarize(shareAv = weighted.mean(shareAv, total), 
                                                shareCv = weighted.mean(shareCv, total),
                                                sharePt = weighted.mean(sharePt, total))

p  = tm_basemap(leaflet::providers$CartoDB) +
  tm_shape(regions, name = "Av") +  tm_polygons(col = "shareAv") + 
  tm_shape(regions, name = "Pt")  + tm_polygons(col = "sharePt") + 
  tm_shape(regions, name = "Cv") + tm_polygons(col = "shareCv")

tmap_leaflet(p)


zoneTypes = read_csv("c:/models/silo/muc/input/zoneSystem.csv")

zoneTypes = zoneTypes %>% select(id = Zone, area = Area, type = BBSR_Type)

shareByZoneType = zoneTypes %>% left_join(modeChoiceAtJob , by = c("id" = "workZone"))
shareByZoneType [is.na(shareByZoneType)] = 0



shareByZoneType = shareByZoneType %>% group_by(type, year) %>% summarize(shareAv = weighted.mean(shareAv, total), 
                                                         shareCv = weighted.mean(shareCv, total),
                                                         sharePt = weighted.mean(sharePt, total))

shareByZoneType = shareByZoneType %>% gather(3:5, key = "mode", value = "share")

shareByZoneType$typeWord = factor(x = shareByZoneType$type, levels = c(10,20,30,40), labels = c("Core", "Medium city", "Town", "Rural"))

ggplot(shareByZoneType %>% filter(year %in% selected_years), aes(x = typeWord, fill = mode, y = share)) +
  geom_bar(stat = "identity") +
  facet_wrap(.~year)
