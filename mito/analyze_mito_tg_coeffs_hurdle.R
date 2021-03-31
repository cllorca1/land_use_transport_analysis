pacman::p_load(readr, dplyr, ggplot2, tidyr, plotly)

mito_folder = "c:/models/mito/muc/mitoMunich/"

zero_binomial_coefficients = read_csv(paste(mito_folder, "input/tripGeneration/CoefficientsZeroHurdlePersonBased.csv", sep =""))

zero_binomial_coefficients = zero_binomial_coefficients %>% pivot_longer(cols = c("HBW", "HBE", "HBS", "HBO", "HBR", "NHBW", "NHBO"))

zero_binomial_coefficients$name = factor(zero_binomial_coefficients$name, levels = c("HBW", "HBE", "HBS", "HBO", "HBR", "NHBW", "NHBO"))

ggplot(zero_binomial_coefficients, aes(variable, y=value, fill = name, group = name))  +
  geom_bar(stat = "identity") + geom_hline(yintercept = 0, color = "black") + coord_flip() + facet_grid(.~name)

ggplotly()

count_coefficients = read_csv(paste(mito_folder, "input/tripGeneration/CoefficientsNegBinCountPersonBased.csv", sep =""))

count_coefficients = count_coefficients %>% pivot_longer(cols = c("HBW", "HBE", "HBS", "HBO", "HBR", "NHBW", "NHBO"))

count_coefficients$name = factor(count_coefficients$name, levels = c("HBW", "HBE", "HBS", "HBO", "HBR", "NHBW", "NHBO"))

ggplot(count_coefficients %>% filter(variable != "theta"), aes(variable, y=value, fill = name, group = name))  +
  geom_bar(stat = "identity") + geom_hline(yintercept = 0, color = "black") +  coord_flip() + facet_grid(.~name)

