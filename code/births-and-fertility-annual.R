#Loading libraries
library(tidyverse)
library(scales)

# Loading data
live_births <- read_csv("data/births-and-fertility-annual/live-births.csv")

# Data wrangling
live_births$value <- as.double(as.character(live_births$value))

# Plotting number of live births
live_births %>% 
  drop_na() %>% 
  ggplot(aes(x = year, y = value, colour = level_1)) +
  geom_line() +
  geom_hline(yintercept = 40000, linetype = 2, size = 0.5) +
  facet_wrap(~level_1, scales = "free") +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "", y = "",
       title = "Resident live-births and total live-births in Singapore",
       caption = "Source: data.gov.sg\nGraphic: @weiyuet")

ggsave("figures/resident-and-total-live-births.png", width = 8, height = 6)
  