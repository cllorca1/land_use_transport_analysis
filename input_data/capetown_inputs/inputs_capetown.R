pacman::p_load(dplyr, readr, tidyr, ggplot2, sf, tmap)

folder = "C:/models/silo/capetown/cape_town_fabilut/silo/"


jobStartDistribution = read_csv(paste(folder, "input/jobStartTimeDistributions.csv",sep  =""))

ggplot(data  = jobStartDistribution) + 
  geom_line(inherit.aes = F, aes(x = time/3600, y = Mnft/max(jobStartDistribution$Mnft)), color = "red", size = 1) + 
  geom_line(inherit.aes = F, aes(x = time/3600, y = Admn/max(jobStartDistribution$Admn)), color = "blue", size = 1) + 
  ylab("Rel. frequency (red = manufacturing, blue = services)")


jobDurationDistribution = read_csv(paste(folder, "input/jobDurationDistributions.csv",sep  =""))

ggplot(data  = jobDurationDistribution) + 
  geom_line(inherit.aes = F, aes(x = time/3600, y = Mnft/max(jobDurationDistribution$Mnft)), color = "red", size = 1) + 
  geom_line(inherit.aes = F, aes(x = time/3600, y = Admn/max(jobDurationDistribution$Admn)), color = "blue", size = 1) + 
  ylab("Rel. frequency (red = manufacturing, blue = services)")

#Job start and duration: the data is obtained from Mobilität in Deutshcland, based on trip diaries (trip to work). 


tripFrequencyDistribution = read_csv(paste(folder, "input/hts_work_tripLengthFrequencyDistribution.csv",sep  =""))

ggplot(data  = tripFrequencyDistribution) + 
  geom_line(inherit.aes = F, aes(x = TravelTime, y = utility), color = "black", size = 1) + 
  geom_line(inherit.aes = F, aes(x = TravelTime, y = exp(-0.2 * TravelTime)), color = "orange", size = 1) + 
  ylab("Rel. frequency (red = manufacturing, blue = services)")

#The trip freq. distribution controls the penalty of longer commutes. In the current setting we have the black line, copied from Munich long time ago. The black
# line reproduces the observed travel time distribution. We found afterwards that the black line can lead to too long commute times in the successive years. If this 
# is observed in SILO-Capetown, it could be a good idea to replace this file by an exponential function, such as the orange line = exp(-0.2 * time_minutes).
#This can be simply done by replacing the column utility of this file accordingly. Has to be checked in the changes in commute distance by year.I selected the -0.2 
# value for Munich with a zero-growth scenario, assuming that commute distance has to remain relatively stable. 

#Crime index unfortunately not assigned. All subplaces (region) have same value, equal to 0.5. 
#School quality index unfortunately not assigned. All subplaces (region) have same value, equal to 0.9.

zones = read_csv(paste(folder, "input/zoneSystem.csv",sep  =""))
development = read_csv(paste(folder, "input/development.csv", sep  =""))
#the last two columns define the development capacity. They are expressed in acres (column DevLandUse). Unless specified, the column DevCapacity is not used, and 
# would define the capacity in dwellings. 
#The previos colums (dummy variables per dd type) simply define wether it is allowed or not to develop a dwelling of the type. They are all "1". 

shp = st_read(paste(folder, "input/zonesShapeFile/zones.shp",sep  =""))

shp = shp %>% left_join(development, by = c("ID_cell" = "Zone"))

map  = tm_basemap(leaflet::providers$CartoDB)
map =  map + tm_shape(shp) +
  tm_polygons(col ="DevLandUse")
tmap_leaflet(map)

#The developable land is extracted from the field area_1 (a 1 % of this, as the word doc says).
# Unfortunately, this field seems to be a wrong computation of the area!!
shp$true_area = st_area(shp)
ggplot(shp, aes(x = area_1, y = DevLandUse)) + geom_point()
ggplot(shp, aes(x = as.numeric(true_area), y = DevLandUse)) + geom_point()


sum(shp$DevLandUse)

ggplot(shp, aes(x = DevLandUse/0.15)) + geom_histogram()

pp = read_csv(paste(folder, "microData/pp_2011.csv", sep = ""))
hh = read_csv(paste(folder, "microData/hh_2011.csv", sep = ""))
dd = read_csv(paste(folder, "microData/dd_2011.csv", sep = ""))
jj = read_csv(paste(folder, "microData/jj_2011.csv", sep = ""))


pp %>% group_by(race) %>% summarize(count = n(), ave_income = mean(income))
ggplot(pp, aes(x = income, color = race)) + stat_ecdf() + scale_x_log10() + xlab("Log(income)")
#The income is defined differently by races. 

pp = pp %>% left_join(hh, by = c("hhid" = "id"))
pp = pp %>% left_join(dd, by = c("dwelling" = "id"))

pp_by_zone_and_race = pp %>% group_by(race,zone) %>% summarize(count = n()) %>% pivot_wider(names_from = "race", values_from = "count", values_fill = 0)

shp = st_read(paste(folder, "input/zonesShapeFile/zones.shp",sep  =""))
shp = shp %>% left_join(pp_by_zone_and_race, by = c("ID_cell" = "zone"))
map  = tm_basemap(leaflet::providers$CartoDB)
map =  map + tm_shape(shp, "black") +
  tm_polygons(col ="BLACK", style = "quantile", convert2density = T)
map =  map + tm_shape(shp, "white") +
  tm_polygons(col ="WHITE", style = "quantile", convert2density = T)
tmap_leaflet(map)

#The pop density but race seems ok, at least, different and seggregated. Only two of four races are plotted!

jj = jj %>% mutate(is_vacant = if_else(personId==-1, "vacant", "taken"))

jj %>% group_by(type, is_vacant) %>% summarise(n())
#There is only one job type! The vacant is set to a 4.2%. 

dd = dd %>% mutate(is_vacant = if_else(hhID==-1, "vacant", "taken"))
dd %>% group_by(type, is_vacant) %>% summarise(n(),
                                               ave_cost = mean(monthlyCost),
                                               ave_quality = mean(quality))
#In the version of SP in gitlab there is no vacant dwellings!!
#Costs per type look reasonable


dd_by_zone = dd %>% group_by(type,zone) %>% summarize(count = n()) %>% pivot_wider(names_from = "type", values_from = "count", values_fill = 0)

shp = st_read(paste(folder, "input/zonesShapeFile/zones.shp",sep  =""))
shp = shp %>% left_join(dd_by_zone, by = c("ID_cell" = "zone"))

map  = tm_basemap(leaflet::providers$CartoDB)
map =  map + tm_shape(shp, "multifamily") +
  tm_polygons(col ="MULTIFAMILY", style = "quantile", convert2density = T)
map =  map + tm_shape(shp, "backyard_informal") +
  tm_polygons(col ="BACKYARD_INFORMAL", style = "quantile", convert2density = T)
tmap_leaflet(map)

#Seems to be an spatial distribution of dd by type. I do not know if it is correct. Only two of 5 types are plotted. 

