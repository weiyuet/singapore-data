##########################
# Rainfall Monthly Total #
##########################

#### Setup ####
library(tidyverse)
library(glue)

#### Load data ####
rainfall_monthly_total <- read_csv("data/rainfall-monthly-total/rainfall-monthly-total.csv")

#### Wrangle ####
# Separate year and month
rainfall_monthly_total <- rainfall_monthly_total %>% 
  separate(month, c("year", "month"))

# Convert year and month into numeric
rainfall_monthly_total <- rainfall_monthly_total %>% 
  mutate(year = as.numeric(year),
         month = as.numeric(month))

#### Visualize ####
# How much rain falls in a month in total?
rainfall_monthly_total %>% 
  ggplot(aes(x = month,
             y = total_rainfall)) +
  geom_col() +
  facet_wrap(vars(year)) +
  scale_x_continuous(breaks = 1:12,
                     labels = month.abb[1:12]) +
  labs(x = "",
       y = "measured in mm",
       title = glue("Monthly Total Rainfall in Singapore ({min(rainfall_monthly_total$year)}-{max(rainfall_monthly_total$year)})"),
       subtitle = "Recorded at Changi Climate Station (1.3667, 103.9833)",
       caption = "Data: National Environment Agency (data.gov.sg) | Graphic: @weiyuet") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1,
                                   size = 7))

#### Save image ####
ggsave("figures/rainfall-monthly-total.png", width = 8, height = 6.5)