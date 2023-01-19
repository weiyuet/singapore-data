##########################################
# Infectious Disease Weekly Case Numbers #
##########################################

#### Setup ####
library(tidyverse)
library(scales)
library(glue)

#### Load data ####
weekly_infectious_disease <- read_csv('data/weekly-infectious-disease-bulletin-cases/weekly-infectious-disease-bulletin-cases.csv')

#### Wrangle ####
col <- paste("col", 1:2)

# Split year and week column
weekly_infectious_disease <- weekly_infectious_disease %>%
  separate(col = epi_week,
           sep = "-",
           into = col,
           remove = TRUE)

weekly_infectious_disease <- weekly_infectious_disease %>% 
  rename(year = "col 1",
         week = "col 2",
         cases = "no._of_cases")

# Extract numbers from week column
weekly_infectious_disease$week <- as.numeric(str_extract(weekly_infectious_disease$week,
                                                         "[0-9]+"))

# Convert year into numeric
weekly_infectious_disease <- weekly_infectious_disease %>% 
  mutate(year = as.numeric(year))

# Clean HFMD and Hand, Foot Mouth Disease names
weekly_infectious_disease <- weekly_infectious_disease %>% 
  mutate(disease = case_when(disease == "HFMD" ~ "Hand, Foot Mouth Disease",
                             TRUE ~ disease))

#### Visualize ####
# Plot weekly case numbers of Dengue Fever
weekly_infectious_disease %>%
  filter(disease == "Dengue Fever") %>%
  ggplot(aes(x = week,
             y = cases)) +
  geom_col() +
  facet_wrap(vars(year)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Week#",
       y = "",
       title = glue("Weekly Case Numbers of Dengue Fever ({min(weekly_infectious_disease$year)}-{max(weekly_infectious_disease$year)}) in Singapore"),
       subtitle = "June seems to be the peak month; 2020 and 2022 being very bad years",
       caption = "Data: Ministry of Health (data.gov.sg) | Graphic: @weiyuet") +
  theme_classic() +
  theme(axis.text.x = element_text(),
        axis.ticks.x = element_blank())

#### Save image ####
ggsave("figures/weekly-cases-dengue-fever.png", width = 8, height = 5)