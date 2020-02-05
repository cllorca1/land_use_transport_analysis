pacman::p_load(readr, dplyr, data.table, ggplot2, tidyr, rhdf5, sf, tmap, expss)


upper_folder = "c:/models/silo/muc/microData/"


hh = read_csv(paste(upper_folder, "hh_2011.csv", sep = ""))


hh$zero = if_else(hh$autos == 0, 1,0)
hh$one = if_else(hh$autos == 1, 1,0)
hh$two = if_else(hh$autos == 2, 1,0)
hh$three = if_else(hh$autos == 3, 1,0)



hh_by_zone = hh %>% group_by(zone) %>% summarize(numberOfCars = sum(autos), zero = sum(zero), one = sum(one), two = sum(two), three = sum(three))

write.table(hh_by_zone, "clipboard-1000k", sep = "\t", row.names = F)
