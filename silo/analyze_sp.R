pacman::p_load(data.table, dplyr, ggplot2)

analyze_sp = function(output_path, subpath, scenario_name){
path = "C:/models/silo/muc/microData/"


#read 2011 files
pp11 = fread(paste(path,"pp_2011.csv",sep = ""))
jj11 = fread(paste(path,"jj_2011.csv", sep = ""))

#read 2050 files
pp50 = fread(paste(path,"futureYears/pp_2050.csv", sep = ""))
jj50 = fread(paste(path,"futureYears/jj_2050.csv", sep = ""))
hh50 = fread(paste(path,"futureYears/hh_2050.csv", sep = ""))


#populationByCounty ----- 


pp11 = merge(pp11, zonesWithRegionName, by.x = "homeZone", by.y = "zone")
aux = hh50 %>% select(id, homeZone = zone)
aux = merge(pp50, aux, by.x = "hhID", by.y = "id")
aux = merge(aux, zonesWithRegionName, by.x = "homeZone", by.y = "zone")

summary11 = pp11 %>% group_by(region = work_region_name) %>% summarize(count = n())
summary50 = aux %>% group_by(region = work_region_name) %>% summarize(count = n())

summary11$year = 2011
summary50$year = 2050

plot1 = ggplot(rbind(summary11, summary50), aes(y=count, x = region, group = as.factor(year)
                                        , fill = as.factor(year))) +
  geom_bar(stat = "identity", position=position_dodge()) + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
  xlab("Home region") + ylab("Persons") + labs(fill = "Year") + 
  theme(legend.position = "bottom")
print(plot1)
ggsave(file = paste(output_path,"pp_",scenario_name,".png", sep = ""),
       plot =  plot1, height = 15, width = 10, units = "cm" )


#workers by county-------

#2011: filter to workers 
workers11 = pp11 %>% filter(workplace>0)

#merge pp and jj to get workzones of persons
workersjj11 = merge(x = workers11, y = jj11, by.x="workplace", by.y = "id")

#store workZone in the right field
workersjj11$workZone = workersjj11$zone

#check if there is only one worker or more than one in the hh
workersByhh11 = workersjj11 %>% group_by (hhid) %>% summarize (hhworkers = n())

#and assign this value to the ppjj11 table
woorkersjj11 = merge(workersjj11, workersByhh11, by="hhid")


#select only what need for comparison
woorkersjj11  = woorkersjj11 %>% select(homeZone, workZone, id, hhworkers) 

#2050: filter to workers
workers50 = pp50 %>% filter(workplace>0)

#merge pp and jj
workersjj50 = merge(x = pp50, jj50, by.x = "workplace", by.y = "id")

#store workZone in the right place
workersjj50$workZone = workersjj50$zone

#merge pp and hh
workersjjhh50 = merge(x = workersjj50, y = hh50, by.x = "hhID", by.y = "id")

#store homeZone in the right place
workersjjhh50$homeZone = workersjjhh50$zone.y

#check if there is only one worker or more than one in the hh
workersByhh50 = workersjjhh50 %>% group_by(hhID) %>% summarize (hhworkers = n())

#and assign this value to the ppjj11 table
workersjjhh50 = merge(workersjjhh50, workersByhh50, by="hhID")


#select the useful columns only
workersjjhh50  = workersjjhh50 %>% select(homeZone, workZone, id, hhworkers) 


#calculate totals of employment by county
#run the script analyze_region_selection.R first

woorkersjj11 = merge(woorkersjj11, zonesWithRegionName, by.x = "workZone", by.y = "zone")
workersjjhh50 = merge(workersjjhh50, zonesWithRegionName, by.x = "workZone", by.y = "zone")

summary11 = woorkersjj11 %>% group_by(work_region_name) %>% summarize(count = n())
summary50 = workersjjhh50 %>% group_by(work_region_name) %>% summarize(count = n())

summary11$year = 2011
summary50$year = 2050

totals11 = sum(summary11$count)
totals50 = sum(summary50$count)

summary11$percent = summary11$count / totals11
summary50$percent = summary50$count / totals11

plot2 = ggplot(rbind(summary11, summary50), aes(y=count, x = work_region_name, group = as.factor(year)
                                        , fill = as.factor(year))) +
  geom_bar(stat = "identity", position=position_dodge()) + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
  theme(legend.position = "bottom") + 
  xlab("Work region") + ylab("Jobs") + labs(fill = "Year")

print(plot2)
ggsave(file = paste(output_path, "workers_",scenario_name,".png", sep = ""),
       plot =  plot2, height = 15, width = 10, units = "cm" )



#read zonal data
fileNameZones = "C:/models/silo/muc/input/zoneSystem.csv"
zones = read.csv(fileNameZones)


# 
# #Munich and Augsburg
# counties = c(9162,9772)
# 
# for (county in counties){
#   
#   #filter to Munich == 9162  (if Ausgburg just change to 9772, otherwise look at zone data frame)
#   zonesMunich = zones %>% filter (ID_county == county)
#   inMunich = zonesMunich$Zone
#   
#   #subset SP files to people working in the selected area
#   sp11Muc = ppjj11 %>% filter(workZone %in% inMunich)
#   sp50Muc = ppjjhh50 %>% filter(workZone %in% inMunich)
#   
#   #merge with zone file
#   zones$homeZone = zones$Zone
#   
#   sp11Muc = merge(sp11Muc, zones, by= "homeZone")
#   sp50Muc = merge(sp50Muc, zones, by= "homeZone")
#   
#   #group by home zone - here jur_name
#   
#   list11 = sp11Muc %>% group_by(JUR_NAME) %>% summarize(workers = n())
#   list50 = sp50Muc %>% group_by(JUR_NAME) %>% summarize(workers = n())
#   
#   #add year column
#   list11$year = 11
#   list50$year = 50
#   
#   #join years
#   list = rbind(list11, list50)
#   
#   #plot
#   print(ggplot(list, aes(x=as.factor(JUR_NAME), y=workers,group = year, fill = as.factor(year))) +
#           geom_bar(stat = "identity", position=position_dodge()) +
#           theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
#           ggtitle(paste("home location of workers of ",county,sep="")) + 
#           scale_y_continuous(labels = scales::comma)
#   )
# }
}