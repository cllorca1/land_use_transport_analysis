pacman::p_load(readr, dplyr, ggplot2, tidyr)

folder_sp = "C:/models/silo/muc/microData/"
file_dd = "dd_2011.csv"
dd2011 = read_csv(paste(folder_sp, file_dd, sep = ""))


max(dd2011$monthlyCost)/200
min(dd2011$monthlyCost)

ggplot(dd2011, aes(x = monthlyCost/200)) + geom_histogram()


dd2011 = dd2011 %>% mutate(rentCat = round(monthlyCost/200,0))


shares = dd2011 %>% group_by(rentCat) %>% summarize(n())
write.table(shares, "clipboard")
