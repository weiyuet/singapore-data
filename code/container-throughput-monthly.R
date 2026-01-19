################################
# Container Throughput Monthly #
################################

#### Setup ####
# Load libraries
library(tidyverse)
library(glue)

#### Load Data ####
container_throughput_raw <- read_csv(
  "data/container-throughput-monthly-total/container-throughput-monthly.csv"
)

#### Prep Data ####
# Separate month and year and convert into numeric
container_throughput_clean <- container_throughput_raw %>%
  separate(month, c("year", "month")) %>%
  mutate(across(1:2, as.numeric))

#### Plot Data ####
# How many containers go through Singapore each month?
container_throughput_clean %>%
  ggplot(aes(x = month, y = container_throughput)) +
  geom_col() +
  facet_wrap(vars(year)) +
  scale_x_continuous(breaks = seq(1:12), labels = month.abb[seq(1:12)]) +
  scale_y_continuous(expand = c(0, 0.05)) +
  labs(
    title = glue(
      "Container Throughput Monthly ({min(container_throughput_clean$year)}-{max(container_throughput_clean$year)})"
    ),
    subtitle = "'000 TEU (Twenty-foot Equivalent Unit)",
    caption = "Data: Maritime and Port Authority of Singapore (data.gov.sg) | Graphic: @weiyuet"
  ) +
  theme_classic() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.caption = element_text(hjust = 0)
  )

#### Save Image ####
ggsave("figures/container-throughput-monthly.png", width = 8, height = 5)

# End
