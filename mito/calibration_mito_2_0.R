pacman::p_load(readr, dplyr, ggplot2, tidyr)


mid_trips = read_csv("c:/projects/MITO/mito2.0/mid_trips.csv")
mito_trips = read_csv("c:/models/mito/muc/mitoMunich/scenOutput/base_2.1_calibrated/2011/microData/trips.csv")

ggplot(mid_trips, aes(x=t.distance, color = t.full_purpose)) + stat_ecdf() + xlim(0,100)


t_purp_mid = mid_trips %>% group_by(t.full_purpose) %>%
  summarize(mean = mean(t.distance), median = median(t.distance))


#write.table(t_purp_mid, "clipboard", sep = "\t", row.names = F)

t_mode_purp_mid = mid_trips %>% group_by(t.full_purpose, t.mode) %>%
  summarize(count = n()) %>% pivot_wider(names_from = t.mode, values_from = count)

#write.table(t_mode_purp, "clipboard", sep = "\t", row.names = F)




t_purp_mito = mito_trips %>% group_by(purpose) %>%
  summarize(mean = mean(distance), median = median(distance))



#write.table(t_purp_mito, "clipboard", sep = "\t", row.names = F)


#compare simple indicators (distance)

purposes = t_purp$purpose

t_purp_mid = t_purp_mid %>% filter(t.full_purpose %in% purposes) 
colnames(t_purp_mid)[colnames(t_purp_mid) == "t.full_purpose"] = "purpose"

t_purp =  t_purp_mito %>% mutate(source = "mito") %>% bind_rows(t_purp_mid %>% mutate(source = "mid"))

ggplot(t_purp, aes(x=purpose, y = mean, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") + xlab("Purpose") + ylab("Mean distance (km)")

ggplot(t_purp, aes(x=purpose, y = median, fill = source)) + geom_bar(stat = "identity", position = "dodge")


##dist

t_purp_bin_mid = mid_trips %>% mutate(dist_bin = cut(t.distance, seq(0,100,2))) %>% 
  group_by(t.full_purpose, dist_bin) %>% summarize(count = n())

t_purp_bin_mito = mito_trips %>% mutate(dist_bin = cut(distance, seq(0,100,2))) %>% 
  group_by(purpose, dist_bin) %>% summarize(count = n())



t_purp_bin_mid = t_purp_bin_mid %>% filter(t.full_purpose %in% purposes) 
colnames(t_purp_bin_mid)[colnames(t_purp_bin_mid) == "t.full_purpose"] = "purpose"

t_purp_bin =  t_purp_bin_mito %>% mutate(source = "mito") %>% bind_rows(t_purp_bin_mid %>% mutate(source = "mid"))

##need to normalize
t_purp_bin = t_purp_bin %>% group_by(source, purpose) %>% mutate(total = sum(count))

t_purp_bin = t_purp_bin %>% group_by(source, purpose) %>% mutate(cum_count = cumsum(count/total))


ggplot(t_purp_bin, aes(x=as.numeric(dist_bin)*2, y = count/total, color = source, group= source)) +
  geom_line(stat = "identity", size = 1) + facet_wrap(.~purpose) + scale_x_sqrt() + 
  xlab("Distance (km) - sqrt scale") + ylab("Relative frequency")

ggplot(t_purp_bin, aes(x=as.numeric(dist_bin)*2, y = cum_count, color = source, group= source)) +
  geom_line(stat = "identity", size = 1) + facet_wrap(.~purpose) + scale_x_log10()+
  xlab("Distance (km)") + ylab("Relative frequency")




