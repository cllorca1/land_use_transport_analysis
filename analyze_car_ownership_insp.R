pacman::p_load(readr, dplyr, ggplot2, colourpicker, tidyr)

folder_sp = "C:/models/silo/muc/microData/"
file_dd = "dd_2011.csv"
file_hh = "hh_2011.csv"

dd2011 = read_csv(paste(folder_sp, file_dd, sep = ""))
hh2011 = read_csv(paste(folder_sp, file_hh, sep = ""))


hh2011 = hh2011 %>% left_join(dd2011, by = c("id"= "hhID"))


summary = hh2011 %>% group_by(type, autos) %>% summarize ( count = n())


summary = filter(summary, type != "NA")

summary$type = factor(x=summary$type, levels = c("SFD", "SFA", "MF234", "MF5plus"))

summary$autos = factor(x=summary$autos, levels = c("3", "2", "1", "0"))

ggplot(summary, aes(x = type, y = count, fill = as.factor(autos))) + geom_bar(stat = "identity", pos = "fill") + 
  scale_fill_manual(values = c("#D2D7F7", "#8898F2", "#203FA8", "#BABABA"))


spatial_summary = hh2011 %>% group_by(zone = zone.x, autos) %>% summarize (count = n()) %>% spread(autos, count, fill = 0)
write.table(spatial_summary, "clipboard-1000k", sep = "\t", row.names= F)
