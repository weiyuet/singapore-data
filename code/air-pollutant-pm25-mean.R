#############################
# Air Pollutants PM2.5 Mean #
#############################

#### Setup ####
library(tidyverse)

#### Load Data ####
air_pollutant_pm25_mean <- read_csv("data/air-pollutant-pm25-mean/air-pollutant-pm25-mean.csv")

#### Explore Data ####
range(air_pollutant_pm25_mean$year) # 2002 to 2022

#### Visualize ####
# What is the yearly trend of PM2.5 particulate matter?
air_pollutant_pm25_mean %>% 
  ggplot(aes(x = year,
             y = pm2.5_mean)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_continuous(breaks = min(air_pollutant_pm25_mean$year):max(air_pollutant_pm25_mean$year),
                     labels = air_pollutant_pm25_mean$year) +
  scale_y_continuous(breaks = min(air_pollutant_pm25_mean$pm2.5_mean):max(air_pollutant_pm25_mean$pm2.5_mean),
                     limits = c(min(air_pollutant_pm25_mean$pm2.5_mean),
                     max(air_pollutant_pm25_mean$pm2.5_mean))) +
  labs(x = NULL,
       y = NULL,
       title = "Concentration of PM2.5 Particulate Matter",
       subtitle = "Unit of measure = micrograms per cubic meter\nBlue line is a smoothed-line showing the trend",
       caption = "Data: National Environment Agency (data.gov.sg) | Graphic: @weiyuet") +
  theme_classic()

#### Save Image ####
ggsave("figures/air-pollutant-pm25-mean.png",
       width = 8,
       height = 5)