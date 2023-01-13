################################
# Container Throughput Monthly #
################################

#### Setup ####
library(tidyverse)
library(scales)
library(glue)

#### Load data ####
container_throughput_monthly <- read_csv("data/container-throughput-monthly-total/container-throughput-monthly.csv")

#### Wrangle ####
# Separate month and year
container_throughput_monthly <- container_throughput_monthly %>% 
  separate(month, c("year", "month"))

# Convert year and month into numeric
container_throughput_monthly <- container_throughput_monthly %>% 
  mutate(across(1:2, as.numeric))

#### Visualize ####
# How many containers go through Singapore each month?
container_throughput_monthly %>% 
  ggplot(aes(x = month,
             y = container_throughput)) +
  geom_col() +
  facet_wrap(vars(year)) +
  scale_x_continuous(breaks = seq(1, 12, 3),
                     labels = month.abb[seq(1, 12, 3)]) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "",
       y = "",
       title = glue("Container Throughput ({min(container_throughput_monthly$year)}-{max(container_throughput_monthly$year)}) '000 Twenty-foot equivalent units"),
       caption = "Data: Maritime and Port Authority of Singapore (data.gov.sg) | Graphic: @weiyuet") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1))

#### Save image ####
ggsave("figures/container-throughput-monthly.png", width = 8, height = 8)