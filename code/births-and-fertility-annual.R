# Setup
library(tidyverse)
library(scales)
library(ggsci)

#Load live-births data
live_births <- read_csv("data/births-and-fertility-annual/live-births.csv")

# Wrangle
live_births$value <- as.double(as.character(live_births$value))

# Plot number of live-births
live_births %>% 
  drop_na() %>% 
  ggplot(aes(x = year, y = value, colour = level_1)) +
  geom_line(size = 1.05) +
  geom_hline(yintercept = 40000, linetype = 2, size = 0.5) +
  facet_wrap(~level_1, scales = "free") +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_colour_jco() +
  labs(x = "", y = "",
       title = "Resident live-births and total live-births in Singapore",
       caption = "Data: Ministry of Trade and Industry - Department of Statistics (data.gov.sg) | Graphic: @weiyuet")

# Save image
ggsave("figures/resident-and-total-live-births.png", width = 8, height = 6)

# Load total fertility rate by ethnic group data
total_fertility_rate_by_ethnic_group <- read_csv("data/births-and-fertility-annual/total-fertility-rate-by-ethnic-group.csv")

total_fertility_rate_by_ethnic_group %>% 
  ggplot(aes(x = year, y = value, colour = level_2)) +
  geom_line() +
  geom_point(aes(shape = level_2)) +
  geom_hline(yintercept = 2.1, linetype = 2, size = 0.5) +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.85, 0.55)) +
  scale_x_continuous(breaks = seq(1960, 2020, 5)) +
  scale_y_continuous(breaks = seq(0, 8, 0.5), labels = label_number(decimal.mark = '.', accuracy = 0.1)) +
  scale_colour_jco() +
  labs(x = "", y ="",
       title = "Total Fertility Rate by Ethnic Groups",
       subtitle = "Dashed line at 2.1 represents the population replacement rate",
       caption = "Data: Ministry of Trade and Industry - Department of Statistics (data.gov.sg) | Graphic: @weiyuet")

# Save image
ggsave("figures/fertility-rate-ethnic-groups.png", width = 8, height = 6)