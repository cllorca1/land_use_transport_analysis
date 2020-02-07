

av_ownership  = read_csv("c:/models/silo/muc/scenOutput/AVA_none/siloResults/avOwnership.csv")


ggplot(av_ownership, aes(x = year, y= avs/autos)) + geom_line(size = 2) + 
  ylab("Share of autonomous vehicles") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90))

