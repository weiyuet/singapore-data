# Load libraries
library(tidyverse)
library(scales)
library(lubridate)

# Load data
container_throughput_monthly <- read_csv("data/container-throughput-monthly-total/container-throughput-monthly.csv")

# Wrangle data
# Convert date from chr to date, separate month and year, prelim plot
container_throughput_monthly %>% separate(month, c("year", "month")) %>% 
  group_by(year) %>% summarise(container_throughput_yearly = sum(container_throughput)) %>% 
  ggplot(aes(x = year, y = container_throughput_yearly)) +
  geom_point() +
  scale_y_continuous(limits = c(10000, 40000), breaks = seq(10000, 40000, 5000),
                     labels = label_number(big.mark = ",")) +
  theme_classic() +
  labs(x = "", y ="",
       title = "Container Throughput (Yearly Total)",
       subtitle = "'000 Twenty-foot equivalent units",
       caption = "Source: data.gov.sg / Maritime and Port Authority of Singapore\nGraphic: @weiyuet")

# Save png
ggsave("figures/container-throughput-yearly-total.png", width = 10, height = 8)
  