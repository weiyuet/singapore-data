# Setup
library(tidyverse)
library(scales)
library(glue)

# Load data
weekly_infectious_disease <- read_csv('data/weekly-infectious-disease-bulletin-cases/weekly-infectious-disease-bulletin-cases.csv')

# Wrangle split year and week column
col <- paste("col", 1:2)

weekly_infectious_disease <- weekly_infectious_disease %>%
  separate(col = epi_week, sep = "-", into = col, remove = TRUE)

weekly_infectious_disease <- weekly_infectious_disease %>% 
  rename(year = "col 1",
         week = "col 2",
         cases = "no._of_cases")

# Visualize weekly case numbers of Dengue Fever from 2012 to 2021
weekly_infectious_disease %>% 
  group_by(year) %>%
  filter(disease == "Dengue Fever") %>%
  ggplot(aes(x = week, y = cases)) +
  geom_col() +
  facet_wrap(~year) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(x = "", y = "",
       title = glue("Weekly Case Numbers of Dengue Fever from {min(weekly_infectious_disease$year)} to {max(weekly_infectious_disease$year)} in Singapore"),
       subtitle = "June seems to be the peak month, with 2020 being a particularly bad year",
       caption = "Data: Ministry of Health (data.gov.sg) | Graphic: @weiyuet")

# Save image
ggsave("figures/weekly-cases-dengue-fever.png", width = 8, height = 5)