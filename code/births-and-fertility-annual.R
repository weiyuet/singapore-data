###############################
# Births and Fertility Annual #
###############################

#### Setup ####
library(tidyverse)
library(scales)
library(paletteer)

#### Load data live births ####
live_births <- read_csv("data/births-and-fertility-annual/live-births.csv")

#### Wrangle ####
# Convert value to numeric
live_births$value <- as.numeric(as.character(live_births$value))

# Calculate long term mean
live_births %>%
  group_by(level_1) %>% 
  summarize(across(value,
                   mean,
                   na.rm = TRUE)) %>% 
  ungroup()

#### Visualize ####
# Plot number of live births
live_births %>% 
  drop_na() %>% 
  ggplot(aes(x = year,
             y = value,
             colour = level_1)) +
  geom_line(linewidth = 1.05) +
  geom_hline(yintercept = 45000,
             linetype = 2,
             linewidth = 0.5) +
  facet_wrap(~level_1,
             scales = "free_x") +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_colour_paletteer_d("ggsci::default_jco") +
  labs(x = "",
       y = "",
       title = "Number of Live-births in Singapore",
       subtitle = "Dashed line at 45,000 represents the long term mean",
       caption = "*Note: Resident Live-Births refers to births with at least one parent who is a Singapore citizen or permanent resident\nData: Department of Statistics (data.gov.sg) | Graphic: @weiyuet")

#### Save image ####
ggsave("figures/resident-and-total-live-births.png", width = 8, height = 6)

#### Load data fertility rate by ethnic group ####
total_fertility_rate_by_ethnic_group <- read_csv("data/births-and-fertility-annual/total-fertility-rate-by-ethnic-group.csv")

#### Visualize ####
# Plot fertility rate by ethnic groups
total_fertility_rate_by_ethnic_group %>% 
  ggplot(aes(x = year,
             y = value,
             colour = level_2)) +
  geom_line() +
  geom_point(aes(shape = level_2)) +
  geom_hline(yintercept = 2.1,
             linetype = 2,
             linewidth = 0.5) +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.85, 0.55)) +
  scale_x_continuous(expand = c(0.01, 0),
                     limits = c(1960, 2020),
                     breaks = seq(1960, 2020, 5)) +
  scale_y_continuous(breaks = seq(0, 8, 0.5),
                     labels = label_number(decimal.mark = '.',
                                           accuracy = 0.1)) +
  scale_colour_paletteer_d("ggsci::default_jco") +
  labs(x = "",
       y = "",
       title = "Total Fertility Rate by Ethnic Groups",
       subtitle = "Dashed line at 2.1 represents the population replacement rate",
       caption = "Data: Department of Statistics (data.gov.sg) | Graphic: @weiyuet")

#### Save image ####
ggsave("figures/fertility-rate-ethnic-groups.png", width = 8, height = 6)