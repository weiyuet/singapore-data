# Load libraries
library(tidyverse)
library(scales)
library(ggsci)

# Load data
life_expectancy <- read_csv('data/life-expectancy-by-sex-annual/life-expectancy-at-birth-and-age-65-years.csv')

# Plot data
life_expectancy %>% 
  ggplot(aes(x = year, y = value, colour = level_1)) +
  geom_line() +
  geom_point() +
  facet_wrap(~level_1, scales = 'free_y') +
  scale_x_continuous(breaks = seq(1960, 2020, 10)) +
  scale_colour_jco() +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = 'none') +
  labs(x = '', y = '',
       title = 'Life Expectancy of Residents in Singapore',
       caption = 'Source: Ministry of Trade and Industry - Department of Statistics (data.gov.sg)\nGraphic: @weiyuet')

# Save png
ggsave('figures/life-expectancy.png', width = 6.5, height = 4.5)