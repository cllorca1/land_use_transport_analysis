pacman::p_load(readr, dplyr, ggplot2, tidyr)


pp = read_csv("c:/models/mito/germany/microData/pp_1perc_2011.csv")
hh = read_csv("c:/models/mito/germany/microData/hh_1perc_2011.csv")



pp = pp %>% left_join(hh, by = "hhid" )


ss = pp %>% filter(occupation == 3) 


ee = ss %>% group_by(id = zone.y, zone = zone.y) %>% summarize(occupancy = round(n() * 1.2, 0), coordX = mean(coordX), coordY = mean(coordY))


write_csv(ee, "c:/models/mito/germany/microData/ee_2011.csv")
