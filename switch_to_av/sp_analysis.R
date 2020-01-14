
pacman::p_load(data.table, dplyr, ggplot2, readr, tidyr, reshape, shiny, 
               shinydashboard, plotly, processx, leaflet, sf, tmap, rgdal, shinyWidgets)


path_to_pp  = "C:/models/silo/muc/microData/pp_2011.csv"
path_to_hh  = "C:/models/silo/muc/microData/hh_2011.csv"


pp = read_csv(path_to_pp)
hh = read_csv(path_to_hh)
hh = hh %>% mutate(has_car = ifelse(autos>0, T,F))

hh_income = pp %>% group_by(hhid) %>% summarise(income = sum(income))

hh = hh %>% left_join(hh_income, by=c("id" = "hhid"))

ggplot(hh, aes(x=income, color = has_car)) + stat_ecdf() + xlim(0,120000)
