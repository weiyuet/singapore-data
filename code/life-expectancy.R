# Load libraries
library(tidyverse)
library(scales)
library(ggsci)

# Load data
life_expectancy <- read_csv('data/life-expectancy-by-sex-annual/life-expectancy-at-birth-and-age-65-years.csv')

# Plot data
life_expectancy %>% 
  ggplot(aes(x = year, y = value, colour = level_1, shape = level_1)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1960, 2020, 10)) +
  scale_y_continuous(limits = c(5, 85),
                     breaks = seq(5, 85, 5)) +
  scale_colour_jco(labels = c('Life Expectancy at 65 Years', 'Life Expectancy at Birth')) +
  scale_shape_discrete(labels = c('Life Expectancy at 65 Years', 'Life Expectancy at Birth')) +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.5)) +
  labs(x = '', y = '',
       title = 'Life Expectancy at Birth and Age 65 years',
       caption = 'Source: Ministry of Trade and Industry - Department of Statistics (data.gov.sg)\nGraphic: @weiyuet')

# Save png
ggsave('figures/life-expectancy.png', width = 6, height = 4.5)