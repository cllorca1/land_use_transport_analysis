pacman::p_load(readr, dplyr, data.table, ggplot2, tidyr, rhdf5)


upper_folder = "c:/models/silo/muc/scenOutput/"

scenario = "baseAV3_nt"

##read silo results

modeChoice = read_csv(paste(upper_folder, scenario, "/siloResults/modeChoiceMicroData.csv", sep  = ""))

modeChoice$zeroAutoHh = if_else(modeChoice$autos == 0, "zero-autos", if_else(modeChoice$avs ==0,  "hh-with-cvs", "hh-with-avs"))

ggplot(modeChoice, aes(x=year, fill = mode)) + geom_bar(stat = "count", position = "fill") + 
  facet_wrap(.~zeroAutoHh)

ggplot(modeChoice, aes(x=year, fill = mode)) + geom_bar(stat = "count", position = "stack") + 
  facet_wrap(.~zeroAutoHh)


ggplot(modeChoice, aes(x=year, fill = mode)) + geom_bar(stat = "count", position = "fill") + 
  facet_grid(parkingAtWork~zeroAutoHh)


ggplot(modeChoice, aes(x=year, fill = mode)) + geom_bar(stat = "count", position = "fill") + 
  facet_grid(parkingAtWork~.)


#link the distance on the network to the data set
source("c:/code/omx/api/r/omx2.r")

path_to_matrix = "c:/models/silo/muc/skims/skimsAllIntrazonal.omx"

matrix = readMatrixOMX(path_to_matrix, "distanceByTime")

modeChoice = modeChoice %>% rowwise() %>% mutate(distance = matrix[homeZone, workZone])

ggplot(modeChoice %>% filter(year == 2011), aes(x=distance, fill = mode)) + geom_area(stat = "bin", position = "fill")
ggplot(modeChoice %>% filter(year == 2049), aes(x=distance, fill = mode)) + geom_area(stat = "bin", position = "fill")
