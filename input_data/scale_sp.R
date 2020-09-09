pacman::p_load(readr, dplyr, ggplot2)

path = "c:/models/mito/muc/mitoMunich/microData/"


pp100 = read_csv(paste(path, "pp_2011.csv", sep  =""))
dd100 = read_csv(paste(path, "dd_2011.csv", sep  =""))
jj100 = read_csv(paste(path, "jj_2011.csv", sep  =""))
hh100 = read_csv(paste(path, "hh_2011.csv", sep  =""))

scale_factor = 0.01

number_of_households = nrow(hh100)

hhids_scaled = sample(hh100$id, size = round(scale_factor * number_of_households, digits = 0))

hh_scaled = hh100 %>% filter(id %in% hhids_scaled)
pp_scaled = pp100 %>% filter(hhid %in% hhids_scaled)

taken_work_places = pp_scaled %>% filter(workplace != 0) %>% select(workplace)

jj_scaled_taken = jj100 %>% filter(id %in% taken_work_places$workplace)

jj_vacant = jj100 %>% filter(personId < 0)

number_of_vacant_jobs = nrow(jj_vacant)

jjids_scaled_vacant = sample(jj_vacant$id, size = round(scale_factor * number_of_vacant_jobs, digits = 0))

jj_scaled_vacant = jj_vacant %>% filter(id %in% jjids_scaled_vacant)

jj_scaled = jj_scaled_taken %>% bind_rows(jj_scaled_vacant)


taken_dds = hh_scaled %>% select(dwelling)

dd_scaled_taken = dd100 %>% filter(id %in% taken_dds$dwelling)

dd_vacant = dd100 %>% filter(hhID < 0)

number_of_vacant_dd = nrow(dd_vacant)

ddids_scaled_vacant = sample(dd_vacant$id, size = round(scale_factor * number_of_vacant_dd, digits = 0))

dd_scaled_vacant = dd_vacant %>% filter(id %in% ddids_scaled_vacant)

dd_scaled = dd_scaled_taken %>% bind_rows(dd_scaled_vacant)

write_csv(pp_scaled, paste(path, "pp_2011", scale_factor, ".csv", sep  =""))
write_csv(dd_scaled, paste(path, "dd_2011", scale_factor, ".csv", sep  =""))
write_csv(jj_scaled, paste(path, "jj_2011", scale_factor, ".csv", sep  =""))
write_csv(hh_scaled, paste(path, "hh_2011", scale_factor, ".csv", sep  =""))


