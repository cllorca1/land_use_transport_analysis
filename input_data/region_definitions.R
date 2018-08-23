pacman::p_load(ggplot2, dplyr, data.table)

folder = "c:/projects/SILO/relocation/new region definition/"
file_name = "definition_regions_csv.csv"

data = fread(paste(folder, file_name, sep = ""))


by_region = data%>% 
  group_by(name_count) %>% 
  summarize(pop_emp = sum(Pop_Emp), zones = n())
plotThisRegionDefinition(by_region, 2000000,2000,25)

by_muni = data%>% 
  group_by(name_muni) %>% 
  summarize(pop_emp = sum(Pop_Emp), zones = n())
plotThisRegionDefinition(by_muni, 2000000,2000,25)

by_region_district = data%>% 
  group_by(name_count_dis) %>% 
  summarize(pop_emp = sum(Pop_Emp), zones = n())
plotThisRegionDefinition(by_region_district, 500000,500, 25)

by_muni_district = data%>% 
  group_by(name_muni_dist) %>% 
  summarize(pop_emp = sum(Pop_Emp), zones = n())
plotThisRegionDefinition(by_muni_district, 500000,500, 25)


plotThisRegionDefinition = function(data, maxPop, maxZones, bins){

  bin_w = maxPop/bins
  
  plot_pop = ggplot(data, aes(x=pop_emp)) +
    geom_histogram(fill = "lightblue", color = "blue", binwidth = bin_w) + 
    theme_bw() + 
    scale_x_continuous(limits = c(-bin_w/2,maxPop), expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) + 
    theme(plot.margin= unit(c(2,10,1,1), "mm")) 
  
  print(plot_pop)
  
  bin_w = maxZones/bins
  
  plot_zones = ggplot(data, aes(x=zones)) +
    geom_histogram(fill = "lightblue", color = "blue", binwidth = bin_w) + 
    theme_bw() + 
    scale_x_continuous(limits = c(-bin_w/2,maxZones), expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) + 
    theme(plot.margin= unit(c(2,10,1,1), "mm"))
  
  print(plot_zones)
  
}
