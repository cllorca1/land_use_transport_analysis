
pacman::p_load(readr, dplyr, data.table, ggplot2, tidyr, rhdf5, sf, tmap, plotly, ggalluvial)

results = read_csv("C:/projects/Papers/2020_cities/figs/dummy_2.csv") 

results$Household = factor(results$Household, levels = c("no car", "CV", "AV"))

results$Job = factor(results$Job, levels = c("Center", "Outskirts"), labels = c("Job in center", "Job in outskirts"))

results$Alternative = paste(results$Mode, " from dwelling in ", results$Dwelling, sep = "")




results$Alternative = factor(results$Alternative, levels = c("PT from dwelling in city", 
                                                             "CV from dwelling in city", "AV from dwelling in city", 
                                                             "PT from dwelling in outskirts", "CV from dwelling in outskirts", 
                                                             "AV from dwelling in outskirts"))

alternative_fill_colors = c("PT from dwelling in city" = "#2563a9", 
                            "CV from dwelling in city" = "#6f6f6f",
                            "AV from dwelling in city" = "#d43838", 
                            "PT from dwelling in outskirts" = "#a4d0f9", 
                            "CV from dwelling in outskirts" = "#d0c7c7",
                            "AV from dwelling in outskirts" = "#eaa8a8")


results$reduction_of_beta = 1 - results$beta_ratio_av_cv


ggplot(results %>% filter(case == "vot",Household == "AV"), aes(group = , x = as.factor(reduction_of_beta), y = Probability, fill = Alternative)) +
  geom_bar(position = "fill", stat = "identity", size = 2) + 
  facet_grid(.~Job) + 
  scale_color_manual(values = alternative_fill_colors) + scale_fill_manual(values = alternative_fill_colors) + 
  theme_bw() + xlab("Reduction of beta from CV to AV") + scale_y_continuous(expand = c(0,0))

ggsave("C:/projects/Papers/2020_cities/figs/dummy_choices_v2_vot.pdf", width = 15, units = "cm", height = 7, scale = 1.3)

results$p_penalty = as.factor(paste(results$home_penalty_a_av, "+", results$home_penalty_b_av, "q"))

ggplot(results %>% filter(case == "parking_av", Household == "AV"), aes(group = , x = p_penalty, y = Probability, fill = Alternative)) +
  geom_bar(position = "fill", stat = "identity", size = 2) + 
  facet_grid(.~Job) + 
  scale_color_manual(values = alternative_fill_colors) + scale_fill_manual(values = alternative_fill_colors) + 
  theme_bw() + xlab("Parking penalty for AV users") + scale_y_continuous(expand = c(0,0)) + theme(axis.text.x = element_text(angle = 90))

ggsave("C:/projects/Papers/2020_cities/figs/dummy_choices_v2_parking.pdf", width = 15, units = "cm", height = 9, scale = 1.3)
