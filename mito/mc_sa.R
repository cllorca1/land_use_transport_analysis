pacman::p_load(readr, dplyr, data.table, ggplot2, tidyr, rhdf5, sf, tmap, plotly)

results = read_csv("C:/code/mito/modeChoiceSensitivity.csv") %>% select(-privateAV, -sharedAV, -pooledTaxi)

results_long = results %>% pivot_longer(cols = c(autoDriver,autoPassenger,bicycle,bus,train,tramOrMetro,walk), names_to  = "mode", values_to = "p")


results_long$p = round(results_long$p,5)
results_long$distance = round(results_long$distance,0)
results_long$factorCar = round(results_long$factorCar,1)
results_long$factorPt = round(results_long$factorPt,1)
results_long$factorCarPrice = round(results_long$factorCarPrice, 1)
results_long$factorPtPrice = round(results_long$factorPtPrice, 1)


mode_order = c("autoDriver","autoPassenger","train","tramOrMetro","bus", "bicycle", "walk")
mode_colors = c("autoDriver" = "#878787",
               "autoPassenger" = "#a9a9a9",
               "train" = "#789ec9",
               "tramOrMetro" = "#5c768d",
               "bus" = "#50546d",
               "bicycle" = "#87c77d",
               "walk" = "#53815b")

results_long$mode = factor(results_long$mode, levels = mode_order)



p = results_long %>% filter(factorPt == 1., factorCar == 1., factorCarPrice == 1., factorPtPrice == 1.) %>% 
  ggplot(aes(x = distance, y = p, fill = mode, color = mode)) +
  geom_bar(stat  ="identity") +
  scale_fill_manual(values = mode_colors) + scale_color_manual(values = mode_colors) + 
  facet_grid(income~as.factor(purpose)) + 
  ylab("Modal share") + xlab("Distance (km)") + 
  ggtitle(paste("Modal share by distance, purpose (columns) and income (rows)"))

this_distances = c(5, 20, 50)

for (distance in this_distances){
  p = p + geom_vline(xintercept = distance,color = "red")
}


ggsave(plot = p, filename = "c:/projects/mito/mc_sa/base_share.png",
       device = png(), scale = 3, width = 15, height = 6, units = "cm")




for (this_distance in this_distances){

  q = results_long %>%
      filter(factorPt == 1, factorPtPrice == 1, factorCarPrice == 1, distance == this_distance) %>%
      ggplot(aes(x = factorCar, y = as.numeric(p), fill = mode, color = mode)) +
      geom_bar(stat  ="identity") +
      scale_fill_manual(values = mode_colors) + scale_color_manual(values = mode_colors) +
    facet_grid(income~as.factor(purpose)) +
      ylab("Modal share") + xlab("Travel time factor (x CAR time)") +
      ggtitle(paste("Trip distance =", this_distance, "km"))

  ggsave(plot = q, filename = paste("c:/projects/mito/mc_sa/",this_distance, "_by_car_time.png", sep = ""),
         device = png(), scale = 3, width = 15, height = 6, units = "cm")


  q = results_long %>%
    filter(factorCar == 1, factorPtPrice == 1., factorCarPrice == 1., distance == this_distance) %>%
    ggplot(aes(x = factorPt, y = p, fill = mode, color = mode)) +
    geom_bar(stat  ="identity") +
    scale_fill_manual(values = mode_colors) + scale_color_manual(values = mode_colors) +
    facet_grid(income~as.factor(purpose)) +
    ylab("Modal share") + xlab("Travel time factor (x PT time)") +
    ggtitle(paste("Trip distance =", this_distance, "km"))

  
  ggsave(plot = q, filename = paste("c:/projects/mito/mc_sa/",this_distance, "_by_pt_time.png", sep = ""),
         device = png(), scale = 3, width = 15, height = 6, units = "cm")

  q = results_long %>%
    filter(factorPt == 1, factorPtPrice == 1, factorCar == 1, distance == this_distance) %>%
    ggplot(aes(x = factorCarPrice, y = as.numeric(p), fill = mode, color = mode)) +
    geom_bar(stat  ="identity") +
    scale_fill_manual(values = mode_colors) + scale_color_manual(values = mode_colors) +
    facet_grid(income~as.factor(purpose)) +
    ylab("Modal share") + xlab("Travel cost factor (x CAR cost)") +
    ggtitle(paste("Trip distance =", this_distance, "km"))

  ggsave(plot = q, filename = paste("c:/projects/mito/mc_sa/",this_distance, "_by_car_cost.png", sep = ""),
         device = png(), scale = 3, width = 15, height = 6, units = "cm")


  q = results_long %>%
    filter(factorCar == 1, factorCarPrice == 1, factorPt == 1, distance == this_distance) %>%
    ggplot(aes(x = factorPtPrice, y = p, fill = mode, color = mode)) +
    geom_bar(stat  ="identity") +
    scale_fill_manual(values = mode_colors) + scale_color_manual(values = mode_colors) +
    facet_grid(income~as.factor(purpose)) +
    ylab("Modal share") + xlab("Travel cost factor (x PT cost)") +
    ggtitle(paste("Trip distance =", this_distance, "km"))

  ggsave(plot = q, filename = paste("c:/projects/mito/mc_sa/",this_distance, "_by_pt_cost.png", sep = ""),
         device = png(), scale = 3, width = 15, height = 6, units = "cm")

}



