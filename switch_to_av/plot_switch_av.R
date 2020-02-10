
years = 2011:2050

initialYears = c(2025, 2050)
scenarios = c("AV", "no-AV")
time_to_equal_price = 15

df = data.frame()

for (i in 1:length(scenarios)){
  initialYear = initialYears[i]
  equalPriceYear = initialYear + 15
  scenario = scenarios[i]
  
  for (year in years){
    if (year < initialYear){
      ratio = 10 #/high ratio to avoid the probability of switching to autonomous before 2020
    } else {
      ratio = max(c(10 + (year - initialYear)/(equalPriceYear - initialYear)*(1-10) , 1))
    }
    
    row = list(year = year, scenario = scenario, ratio = ratio)
    df = df %>% bind_rows(row)
    
    
  }
  
}

df$scenario = factor(df$scenario, levels = scenarios)
scenario_colors = c("#FF0000","#000000")

ggplot(df, aes(x= year, y = ratio, color = scenario)) + geom_line(size = 2) + ylim(0,10) + 
  ylab("Ratio between AV and CV purchase costs") + xlab("Year") + theme_bw() + labs(color = "Scenario") + scale_color_manual(values = scenario_colors)

ggsave("C:/projects/Papers/2020_cities/figs/av_scenarios.pdf", width = 10, units = "cm", height = 7, scale = 1.5)


##VOT changes

scenarios = c("AV", "CV/PT")
minutes = 0:120

d1 = data.frame(minutes)
d1 = d1 %>% mutate(u =exp(-0.2*minutes))
d1$scenario = "CV/PT"

d2 = data.frame(minutes)
d2 = d2 %>% mutate(u =exp(-0.2*0.6*minutes))
d2$scenario = "AV"

d = d1 %>% bind_rows(d2)

scenario_colors = c("#FF0000","#000000")

ggplot(d, aes(x= minutes, y = u, color = scenario)) + geom_line(size = 2) + 
  ylab("Utility of travel time") + xlab("Travel time (min)") + theme_bw() + labs(color = "Mode") + scale_color_manual(values = scenario_colors)

ggsave("C:/projects/Papers/2020_cities/figs/vot_changes.pdf", width = 10, units = "cm", height = 7, scale = 1.5)


