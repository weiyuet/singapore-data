######################################
# Number of Flats Constructed by HDB #
######################################

#### Setup ####
library(tidyverse)
library(scales)
library(glue)

#### Load data ####
flats_constructed <- read_csv("data/flats-constructed/flats-constructed-by-housing-and-development-board-annual.csv")

#### Visualize ####
# Plot number of flats constructed
p <- flats_constructed %>%
  ggplot(aes(x = year,
             y = flats_constructed)) +
  geom_step() +
  scale_x_continuous(
    expand = c(0.01, 0),
    limits = c(1975, 2020),
    breaks = seq(1975, 2020, 5)
  ) +
  scale_y_continuous(
    limits = c(2000, 75000),
    breaks = seq(5000, 75000, 10000),
    labels = label_number(big.mark = ",")
  ) +
  theme_classic() +
  labs(
    x = "",
    y = "",
    title = glue("Number of Flats Constructed by the Housing and Development Board ({min(flats_constructed$year)}-{max(flats_constructed$year)})"),
    caption = "Data: Housing and Development Board (data.gov.sg) | Graphic: @weiyuet"
  )

# Annotate
p + (annotate("text", x = 1984, y = 70000, label = "67,017", size = 3)) + 
       (annotate("text", x = 2006, y = 8000, label = "2,733", size = 3))

#### Save image ####
ggsave("figures/flats-constructed.png", width = 8, height = 4.5)