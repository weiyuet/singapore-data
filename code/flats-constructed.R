# Load libraries
library(tidyverse)
library(scales)

# Load data
flats_constructed <- read_csv("data/flats-constructed/flats-constructed-by-housing-and-development-board-annual.csv")

# Plot number of flats constructed
p <- flats_constructed %>%
  ggplot(aes(x = year, y = flats_constructed)) +
  geom_line() +
  scale_x_continuous(
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
    x = "", y = "",
    title = "Flats Constructed By Housing And Development Board, Annual",
    caption = "Source: Housing and Development Board (data.gov.sg)\nGraphic: @weiyuet"
  )

# Annotate
p + (annotate("text", x = 1984, y = 70000, label = "67,017", size = 3)) + 
       (annotate("text", x = 2006, y = 8000, label = "2,733", size = 3))

# Save png
ggsave("figures/flats-constructed.png", width = 6, height = 4.5)
