#Load libraries
library(tidyverse)
library(scales)

#Load data
flats_constructed <- read_csv("data/flats-constructed/flats-constructed-by-housing-and-development-board-annual.csv")

#Plot number of flats constructed
p <- flats_constructed %>% 
  ggplot(aes(x = year, y = flats_constructed)) +
  geom_line() +
  scale_x_continuous(limits = c(1975, 2020),
                     breaks = seq(1975, 2020, 5)) +
  scale_y_continuous(limits = c(2000, 75000),
                     breaks = seq(5000, 75000, 10000),
                     labels = label_number(big.mark = ",")) +
  theme_classic() +
  labs(x = "", y = "",
       title = "Flats Constructed By Housing And Development Board, Annual",
       caption = "Source: Housing and Development Board (data.gov.sg)\nGraphic: @weiyuet")

# Annotate
p +
  annotate("text", x = 1984, y = 70000, label = "67,017", size = 3) + #peak
  annotate("text", x = 2006, y = 8000, label = "2,733", size = 3) + #trough
  geom_curve(aes(x = 2005, y = 10000, xend = 6000, yend = 25000),
             colour = 'black', size = 0.5, curvature = -0.2,
             arrow = arrow(length = unit(0.03, 'npc')))

#Save png
ggsave("figures/flats-constructed.png", width = 6, height = 4.5)