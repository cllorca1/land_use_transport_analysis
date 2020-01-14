pacman::p_load(readr, dplyr, data.table, ggplot2, tidyr)


upper_folder = "c:/models/silo/muc/scenOutput/"

scenario = "baseAV2_nt"

##read silo results

av_ownership = read_csv(paste(upper_folder, scenario, "/siloResults/avOwnership.csv", sep  = ""))

av_ownership2 = av_ownership %>% gather(2:5, key = "variable", value = "value")


ggplot(av_ownership2 %>% filter(variable == "autos" | variable == "avs" | variable == "hhs"), aes(x= year, y = value, color = variable)) + 
  geom_line(size  =1) + geom_point(size = 2)


ggplot(av_ownership, aes(x= year, y = autos/hhs)) + 
  geom_line(size  =1) + geom_point(size = 2)




#read mito results

years = c(2011, 2020, 2030, 2040, 2050)

modal_share = data.frame()

for (year in years){
  this_year_modal_share = read_csv(paste(upper_folder, scenario, "/", year, "/modeChoice/Munich_modalSplit.csv", sep = ""))
  this_year_modal_share$year = year
  modal_share = modal_share %>% bind_rows(this_year_modal_share)
  rm(this_year_modal_share)
}

#remove the purpose airport if not used

modal_share = modal_share %>% filter(purpose != "AIRPORT")

ggplot(modal_share %>% filter(mode == "privateAV" | mode == "sharedAV"), aes(x=year, y = share, color = mode)) + 
  geom_line(size = 1) + geom_point(size  = 2) + 
  facet_wrap(.~purpose)

color_modes = c(walk ="#538d66", bicycle = "#8aae8e", bus = "#8cb7ff", tramOrMetro = "#5364b6", train = "#2c2b5c",
                autoDriver = "#696969", autoPassenger = "#454545" , privateAV = "#b09d81", sharedAV =  "#655936")
modes_ordered = c("walk", "bicycle", "bus", "tramOrMetro", "train",
                  "autoDriver", "autoPassenger", "privateAV", "sharedAV")

modal_share$mode = factor(modal_share$mode, levels = modes_ordered)


ggplot(modal_share, aes(x=year, y = share, fill = mode)) + 
  geom_bar(position = "fill", stat = "identity") + 
  scale_fill_manual(values = color_modes) +
  facet_wrap(.~purpose)
