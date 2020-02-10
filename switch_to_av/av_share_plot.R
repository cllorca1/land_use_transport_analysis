

av_ownership  = read_csv("c:/models/silo/muc/scenOutput/AVA_none/siloResults/avOwnership.csv")


ggplot(av_ownership, aes(x = year, y= avs/autos)) + geom_line(size = 2) + 
  ylab("Share of autonomous vehicles") + xlab("Year") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90))

ggsave("C:/projects/Papers/2020_cities/figs/av_share.pdf", width = 8, units = "cm", height = 7, scale = 1.3)
