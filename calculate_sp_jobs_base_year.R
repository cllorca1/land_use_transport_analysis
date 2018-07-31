works_base_year = jj11 %>%
  group_by(zone, type) %>%
  summarize(number = n()) %>%
  tidyr::spread(key = type, value = number)

all_zones = 1:4953

all_zones = data.frame(zone = all_zones)

works_base_year = merge(all_zones, works_base_year, by = "zone", all.x = T)
works_base_year[is.na(works_base_year)] = 0

write.csv(x=works_base_year, file = "c:/models/silo/mucSmall/input/assumptions/job_forecast_as_sp.csv", row.names = F)
