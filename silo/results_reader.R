pacman::p_load(ggplot2, dplyr)

read_results = function(output_path, subpath, scenario_name){

path = "c:/models/silo/muc/scenOutput"

file_name = "resultFile1.csv"

min_y = 0
max_y = 50

raw = readLines(paste(path,subpath,file_name,sep = "/"))
this_variable = "aveCommuteDistByRegion"
this_variable_stops = "carOwnershipLevel"

data = data.frame()
i = 1
while (i <= length(raw)){
  if (length(grep(pattern = "Year", x = raw[i]))==1){
    #found a new year
    year = as.numeric(sub(pattern = "Year ",replace = "",x = raw[i]))
    print(year)
    i = i+1
  } else {
    line_elements =strsplit(x = raw[i], split = ",")[[1]]
    if(line_elements[1] == this_variable){
      i = i+1
      line_elements =strsplit(x = raw[i], split = ",")[[1]]
      while (line_elements[1] != this_variable_stops){
        row = list(year = year, region = as.numeric(line_elements[1]), time =as.numeric(line_elements[2]))
        data = rbind(data, row)
        i = i+1
        line_elements =strsplit(x = raw[i], split = ",")[[1]]
      }
    }
    i = i+1
  }
}



ggplot(data, aes(x=year, y =time, color = as.factor(region))) +
  geom_line() + geom_point() + theme_light() + 
  xlab("Year") + ylab("Average comuting time by region (minutes)") + 
  ylim(min_y, max_y)
 
data_mean = data %>% group_by(year) %>% summarize(time = mean(time, na.rm = T))

plot3 = ggplot(data_mean, aes(x=year, y =time)) +
  geom_line() + geom_point() + theme_light() + 
  xlab("Year") + ylab("Average comuting time by region (minutes)") + 
  ylim(min_y, max_y)
print(plot3)

ggsave(file = paste(output_path,"time_all_",scenario_name,".png", sep = ""),
       plot =  plot3, height = 15, width = 10, units = "cm" )


# Age
# ppByRace
# hhByType
# hhByRace
# hhBySize
# AveHHSize
# AveHHInc
# laborParticipationRateByAge
# carOwnershipLevel
# QualityLevel
# CountOfDD
# AveMonthlyPrice
# AveVacancy
# Available land for construction by region
# Housing costs by income group
# jobByRegion
# InmigrantsPP
# OutmigrantsPP
# Count of simulated events
# DivorceEvent
# LicenseEvent
# MigrationEvent
# ConstructionEvent
# MoveEvent
# LeaveParentsEvent
# BirthEvent
# BirthDayEvent
# RenovationEvent
# EducationEvent
# DemolitionEvent
# EmploymentEvent
# MarriageEvent
# DeathEvent
# AddedCar
# RelinquishedCar


}






