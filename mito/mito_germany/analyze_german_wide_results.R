pacman::p_load(dplyr, readr, ggplot2, tidyr)


trips = read_csv("C:/users/carlloga/LRZ Sync+Share/bast_EntlastungBundesfernstraßen (Rolf Moeckel)/output_trips_file/trips_10percent.csv")


ggplot(trips, aes(x = distance, color = purpose)) + stat_ecdf(size = 1) + xlim(0,1000) + theme_bw()

trips = trips %>% mutate(distance_bin = cut(distance, breaks  = -1:100 * 10))

trip_summary = trips %>% group_by(purpose, mode, distance_bin) %>% summarize(n = n())

ggplot(trip_summary, aes(x = as.numeric(distance_bin) * 10, y = n, fill = mode)) +
  geom_area(stat = "identity", position = "fill") +
  theme_bw() + 
  facet_wrap(.~purpose, scales = "free_x")



trip_summary_2 = trips %>% group_by(purpose, mode) %>% summarize(n = n()) %>% spread(purpose, n)
trip_summary_2
