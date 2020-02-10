
pacman::p_load(readr, dplyr, data.table, ggplot2, tidyr, rhdf5, sf, tmap, plotly, ggalluvial)

results = read.table("clipboard", sep = "\t", header = T)
write_csv(results,"C:/projects/Papers/2020_cities/figs/dummy.csv") 

results$Household = factor(results$Household, levels = c("no car", "CV", "AV"))

results$Alternative = paste(results$Mode, " from dwelling in ", results$Dwelling, sep = "")




results$Alternative = factor(results$Alternative, levels = c("PT from dwelling in city", 
                                                             "CV from dwelling in city", "AV from dwelling in city", 
                                                             "PT from dwelling in outskirts", "CV from dwelling in outskirts", 
                                                             "AV from dwelling in outskirts"))

alternative_fill_colors = c("#2563a9", "#6f6f6f","#d43838", "#a4d0f9", "#d0c7c7", "#eaa8a8")



ggplot(results, aes(x = Household, y = Probability, fill = Alternative)) +
  geom_bar(position = "fill", stat = "identity", size = 2) + 
  facet_wrap(.~Job) + 
  scale_color_manual(values = alternative_fill_colors) + scale_fill_manual(values = alternative_fill_colors) + 
  theme_bw()

ggsave("C:/projects/Papers/2020_cities/figs/dummy_choices.pdf", width = 15, units = "cm", height = 7, scale = 1.3)


