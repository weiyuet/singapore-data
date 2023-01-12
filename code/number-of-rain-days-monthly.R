########################################
# Rainfall Number of Rain Days Monthly #
########################################

#### Setup ####
library(tidyverse)
library(scales)

#### Load data ####
number_of_rain_days_monthly <- read_csv("data/rainfall-monthly-number-of-rain-days/rainfall-monthly-number-of-rain-days.csv")

#### Wrangle ####
# Separate year and month
number_of_rain_days_monthly <- number_of_rain_days_monthly %>% 
  separate(month, c("year", "month"))

# Convert year and month into numeric
number_of_rain_days_monthly = transform(number_of_rain_days_monthly,
                                        year = as.numeric(year),
                                        month = as.numeric(month))

#### Visualize ####
# How many rain days are there in a month?
number_of_rain_days_monthly %>%
  ggplot(aes(x = month,
             y = no_of_rainy_days)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 15,
             linetype = "dashed",
             colour = "red") +
  facet_wrap(vars(year)) +
  scale_x_continuous(breaks = seq(1, 12, 2),
                     labels = month.abb[seq(1, 12, 2)]) +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  labs(x = "",
       y = "",
       title = "Number of Rain Days per Month - recorded at Changi Climate Station",
       caption = "Data: National Environment Agency (data.gov.sg) | Graphic: @weiyuet") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1))

#### Save image ####
ggsave("figures/number-of-rain-days-monthly.png", width = 8, height = 8)