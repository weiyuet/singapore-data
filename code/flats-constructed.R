#Load libraries
library(tidyverse)
library(scales)

#Load data
flats_constructed <- read_csv("data/flats-constructed/flats-constructed-by-housing-and-development-board-annual.csv")

#Plot number of flats constructed
flats_constructed %>% 
  ggplot(aes(x = year, y = flats_constructed)) +
  geom_line() +
  scale_x_continuous(limits = c(1975, 2020),
                     breaks = seq(1975, 2020, 5)) +
  scale_y_continuous(limits = c(2000, 70000),
                     breaks = seq(5000, 70000, 10000),
                     labels = label_number(big.mark = ",")) +
  theme_classic() +
  labs(x = "", y = "",
       title = "Flats Constructed By Housing And Development Board, Annual",
       caption = "Source: data.gov.sg / Housing and Development Board\nGraphic: @weiyuet")

#Save png
ggsave("figures/flats-constructed.png", width = 7, height = 5)
