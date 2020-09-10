#+ message = FALSE
#+ warning = FALSE

pacman::p_load(readr, dplyr, ggplot2, tidyr)

scenario_names = c("test_hurdle_10", "test_enumeration_10")
scales = c(10,10)

distance_bins = seq(0,100,10)

trips = data.frame()


for (i in 1:length(scenario_names)){
  path = "c:/models/mito/muc/mitoMunich/scenOutput/"
  path = paste(path, scenario_names[i], "/", sep = "")
  year = 2011
  fileEnding = "/microData/trips.csv"
  
  this_trips = read_csv(paste(path, year, fileEnding, sep = ""))
  this_trips$scenario = scenario_names[i]
  this_trips$scale = scales[i]
  trips = trips %>% bind_rows(this_trips)
  rm(this_trips)
}

path = "c:/models/mito/muc/mitoMunich/microData/"
pp100 = read_csv(paste(path, "pp_2011.csv", sep  =""))
pp100 = pp100 %>% group_by(hhid) %>% mutate(hh_size = n())
hh100 = read_csv(paste(path, "hh_2011.csv", sep  =""))


trips = trips %>% left_join(pp100, by = c("person" = "id"))
trips = trips %>% group_by(hhid) %>% mutate(hh_income = sum(income))
trips = trips %>% ungroup()

##trips by zone
trips_by_zone = trips %>% group_by(scenario, origin) %>% summarize(n = n())
trips_by_zone = trips_by_zone %>% arrange(n)
trips_by_zone = trips_by_zone %>% mutate(i = 1) %>% group_by(scenario) %>% mutate(cum_i = cumsum(i))
trips_by_zone = trips_by_zone %>% group_by(scenario) %>% mutate(cumulated = cumsum(n)/sum(n))
ggplot(trips_by_zone, aes(x = cum_i, y = cumulated, group = scenario, color = scenario)) + geom_path()

##trips by age
trips = trips %>% arrange(age)
trips = trips %>% mutate(i = 1) %>% group_by(scenario) %>% mutate(cum_i = cumsum(i)/sum(i))
ggplot(trips, aes(y = cum_i, x = age, group = scenario, color = scenario)) + geom_path()

ggplot(trips, aes(x = age, group = scenario,..density.., color = scenario)) +
  geom_freqpoly() + xlab("Age") + ylab("Trips (relative frequency)")


##trips by hh_income
trips = trips %>% arrange(hh_income)
trips = trips %>% mutate(i = 1) %>% group_by(scenario) %>% mutate(cum_i = cumsum(i)/sum(i))
ggplot(trips, aes(y = cum_i, x = hh_income + 1, group = scenario, color = scenario)) + geom_path() + scale_x_log10() 

ggplot(trips, aes(x = hh_income, group = scenario,..density.., color = scenario)) +
  geom_freqpoly() + xlim(1,150000) + xlab("Income (EUR) exclude 0 and > 150k") + ylab("Trips (relative frequency)")


##trips by occupation
trips = trips %>% arrange(occupation)
trips = trips %>% mutate(i = 1) %>% group_by(scenario) %>% mutate(cum_i = cumsum(i)/sum(i))
ggplot(trips, aes(y = cum_i, x = occupation, group = scenario, color = scenario)) + geom_path() 
#1: employed, 2: unemployed, 3: student

ggplot(trips, aes(x = occupation, group = scenario,..density.., color = scenario)) +
  geom_freqpoly(binwidth = 1) + xlab("1: employed, 2: unemployed, 3: student") + ylab("Trips (relative frequency)")


##trips by occupation
trips = trips %>% arrange(gender)
trips = trips %>% mutate(i = 1) %>% group_by(scenario) %>% mutate(cum_i = cumsum(i)/sum(i))
ggplot(trips, aes(y = cum_i, x = gender, group = scenario, color = scenario)) + geom_path()

ggplot(trips, aes(x = gender, group = scenario,..density.., color = scenario)) +
  geom_freqpoly(binwidth = 1) + xlab("1: female, 2: male?") + ylab("Trips (relative frequency)")


##trips by hhsize
trips = trips %>% arrange(hh_size)
trips = trips %>% mutate(i = 1) %>% group_by(scenario) %>% mutate(cum_i = cumsum(i)/sum(i))
ggplot(trips, aes(y = cum_i, x = hh_size, group = scenario, color = scenario)) + geom_path()

ggplot(trips, aes(x = hh_size, group = scenario,..density.., color = scenario)) +
  geom_freqpoly(binwidth = 1) + xlab("Household size") + ylab("Trips (relative frequency)")

# rmarkdown::render("mito/compare_tg.R")
