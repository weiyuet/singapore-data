# Setup
library(tidyverse)
library(scales)
library(ggsci)

# Load data
cpi <- read_csv("data/consumer-price-index-cpi-additional-indicators-annual/consumer-price-index-cpi-additional-indicators-2019-as-base-year-index-annual.csv")

# Rename measure column
cpi <- cpi %>% 
  rename(measure = level_1)

# Convert value column to numeric
cpi$value <- as.numeric(as.character(cpi$value))

# Visualize
cpi %>% 
  ggplot(aes(x = year, y = value, colour = measure)) +
  geom_line() +
  geom_hline(yintercept = 100, colour = "red", linetype = "dotted") +
  scale_x_continuous(limits = c(min(cpi$year), max(cpi$year)),
                     breaks = seq(min(cpi$year), max(cpi$year), 5),
                     expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(60, 120),
                     breaks = seq(60, 120, 5)) +
  scale_colour_jco(labels = c("Electricity & Gas Inflation", "MAS Core Inflation", "Retail & Other Goods Inflation", "Services Inflation")) +
  theme_bw() +
  theme(legend.position = c(0.3, 0.85),
        legend.background = element_blank()) +
  labs(x = "", y = "",
       colour = "",
       title = "Consumer Price Index (CPI), 2019 as Base Year",
       caption = "Data: Ministry of Trade and Industry (data.gov.sg) | Graphic: @weiyuet")

# Save image
ggsave("figures/cpi-indicators.png", width = 7, height = 5)
