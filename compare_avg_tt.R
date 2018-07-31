#reader
pacman::p_load(data.table, dplyr, ggplot2)

path = "C:/projects/SILO/integrationTests/with_and_without_acc_every_year/"
file1 = "result_file_every_year_prepared.csv" 
file2 = "result_file_transport_year_prepared.csv"


raw = fread(paste(path,file1,sep = ""))
unique(raw$V1)
data1 = raw %>% filter(V1 == "aveCommuteDistByRegion") %>% select(year = V2,
                                                                  region = V3,
                                                                  time = V4) %>%
  mutate(year = as.numeric(year)) %>% mutate(time = as.numeric(time))

data1$scenario = "everyYear"

raw = fread(paste(path,file2,sep = ""))
# unique(raw$V1)
data2 = raw %>% filter(V1 == "aveCommuteDistByRegion") %>% select(year = V2,
                                                                  region = V3,
                                                                  time = V4) %>%
  mutate(year = as.numeric(year)) %>% mutate(time = as.numeric(time))
data2$scenario = "transportYear"

data = rbind(data1,data2)

avgTimeByYear = data %>% group_by(year, scenario) %>% summarize(time = mean(time))

ggplot(avgTimeByYear, aes(x=year, y= time, color = as.factor(scenario))) + 
  geom_path() + geom_point() +
  xlab("year") +
  ylab("average commuting time (avg. of region avg.)") + 
  labs(color = "Accessibility calculation periodicity") + 
  theme(legend.position = "bottom")
