pacman::p_load(readr, dplyr, ggplot2, tidyr)

region_utility = read_csv("C:/projects/SILO/relocation/analysis/regionUtility.csv")

id_subset = sample(x = region_utility$id, size = 20000)

color_modes = c(AV = "#d43838", CV =  "#6f6f6f", PT = "#2563a9")

avg_accessibility = 50
avg_price = 1500
avg_distance_to_work = 10

region_utility$income_category = factor(x = region_utility$income_category,
                                        levels = c("LOW", "MEDIUM", "HIGH", "VERY_HIGH"),
                                        labels = c("low income", "medium income", "high income", "high income"))


region_utility$mode = factor(region_utility$mode, levels = c("car", "pt", "av"), labels = c("CV", "PT", "AV"))

ggplot(region_utility %>% filter(price < avg_price+ 200, price > avg_price - 200,
                                 nationality == "GERMAN",
                                 shareForeign < 0.25,
                                 autos == 1,
                                 pt_factor_slower == 1.5,
                                 accessibility < avg_accessibility + 20, accessibility > avg_accessibility - 20)
       ,aes(x = distance_to_work, y = utility, color = mode)) +
  geom_point(alpha = 0.5, size = 1) +
  theme_bw() + 
  xlab("Time to work (min)") + ylab("Region utility") + 
  facet_grid(. ~ income_category) + 
  scale_color_manual(name = "Mode", values = color_modes)


ggsave(filename = "relocation/sensitivity_region_distance.bmp", 
       width = 15, height = 6, units = "cm", device = "bmp", scale = 1.5)
ggsave(filename = "relocation/sensitivity_region_distance.pdf", 
       width = 15, height = 6, units = "cm", device = "pdf", scale = 1.5)



#### dwelling

dwelling_utility = read_csv("C:/projects/SILO/relocation/analysis/dwellingUtility.csv")

id_subset = sample(x = dwelling_utility$id, size = 20000)

dwelling_utility$mode = factor(dwelling_utility$mode, levels = c("car", "pt", "av"), labels = c("CV", "PT", "AV"))

avg_accessibility = 50
avg_price = 1500
avg_time_to_work = 20

hh_types = c("SIZE_1_INC_LOW","SIZE_1_INC_MEDIUM","SIZE_1_INC_HIGH","SIZE_2_INC_LOW","SIZE_2_INC_MEDIUM","SIZE_2_INC_HIGH",
"SIZE_3_INC_LOW","SIZE_3_INC_MEDIUM","SIZE_3_INC_HIGH","SIZE_4_INC_LOW","SIZE_4_INC_MEDIUM","SIZE_4_INC_HIGH")

hh_type_labels = c("SIZE_1_INC_LOW","SIZE_1_INC_MEDIUM","SIZE_1_INC_HIGH","SIZE_2_INC_LOW","SIZE_2_INC_MEDIUM","SIZE_2_INC_HIGH",
             "size 3, low income","size 3, medium income","size 3, high income","SIZE_4_INC_LOW","SIZE_4_INC_MEDIUM","SIZE_4_INC_HIGH")


hh_types_selected = c("size 3, low income","size 3, medium income","size 3, high income")

dwelling_utility$household_type = factor(dwelling_utility$household_type, levels = hh_types, labels = hh_type_labels)

ggplot(dwelling_utility %>% filter(time_to_work < avg_time_to_work + 10, time_to_work > avg_time_to_work - 10,
                                   pt_accessibility < avg_accessibility + 20, pt_accessibility > avg_accessibility - 20, 
                                   car_accessibility < avg_accessibility + 20, car_accessibility > avg_accessibility - 20,
                                   bedrooms == 2, 
                                   quality == 4, 
                                   household_type %in% hh_types_selected,
                                   pt_slow_factor == 1.5,
                                   parking_spaces == 1),
       aes(x = price, y = utility, color = mode)) +
  geom_point(alpha= 0.5, size = 1)  +
  theme_bw() + facet_wrap(.~household_type, ncol = 3) + 
  xlab("Dwelling price (EUR)") + ylab("Dwelling utility") +
  scale_color_manual(name = "Mode", values = color_modes)

ggsave(filename = "relocation/sensitivity_dd_price.bmp", 
       width = 15, height = 6, units = "cm", device = "bmp", scale = 1.5)
ggsave(filename = "relocation/sensitivity_dd_price.pdf", 
       width = 15, height = 6, units = "cm", device = "pdf", scale = 1.5)


ggplot(dwelling_utility %>% filter(price < avg_price + 200, price > avg_price - 200,
                                   pt_accessibility < avg_accessibility + 20, pt_accessibility > avg_accessibility - 20, 
                                   car_accessibility < avg_accessibility + 20, car_accessibility > avg_accessibility - 20,
                                   bedrooms == 2, 
                                   quality == 4, 
                                   household_type %in% hh_types_selected,
                                   pt_slow_factor == 1.5,
                                   parking_spaces == 1),
       aes(x = time_to_work, y = utility, color = mode)) +
  geom_point(alpha= 0.5, size = 1)  +
  theme_bw() + facet_wrap(.~household_type, ncol = 3) + 
  xlab("Time to work (min)") + ylab("Dwelling utility") +
  scale_color_manual(name = "Mode", values = color_modes)



ggsave(filename = "relocation/sensitivity_dd_time_to_work.bmp", 
       width = 15, height = 6, units = "cm", device = "bmp", scale = 1.5)
ggsave(filename = "relocation/sensitivity_dd_time_to_work.pdf", 
       width = 15, height = 6, units = "cm", device = "pdf", scale = 1.5)

