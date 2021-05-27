pacman::p_load(readr,dplyr, ggplot2, tidyr)

diary_raw = read_csv("C:/models/abm/output.csv")

diary = data.frame(t(as.matrix(diary_raw)))

diary = diary %>% tibble::rownames_to_column(var = "time")

diary = diary %>% pivot_longer(2:7)


colors = c("H" = "#CFCFCF","T" =  "#525252", "W" = "#7CCD7C",
           "A"= "#8F4343", "O" = "#E9967A", "S" =  "#DB7093", "R" = "#CDAA7D")


diary$value = factor(diary$value, levels = c("H", "W", "A", "S", "O", "R", "T"))

ggplot(diary, aes(x = as.numeric(time)/3600, y = name, fill = value)) + geom_raster() +
  scale_y_discrete(limits=rev, expand = c(0,0)) + 
  scale_x_continuous(expand = c(0,0), lim=c(6,22), breaks = 6:22) + 
  scale_fill_manual(values = colors) + facet_grid(name~., scales = "free") + 
  theme_bw() + theme(legend.position = "bottom") + 
  xlab("Time of day (h)") + ylab("Scheduling step")
