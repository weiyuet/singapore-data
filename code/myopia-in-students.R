# Setup
library(tidyverse)
library(scales)
library(ggsci)

# Load data
myopia <- read_csv("data/common-health-problems-in-students-defective-vision-annual/common-health-problems-of-students-examined-defective-vision-annual.csv")

# Add percentage column
myopia <- myopia %>% 
  mutate(percentage = per_10000_examined / 10000)

# Visualize
myopia %>% 
  ggplot(aes(x = year, y = percentage, colour = gender)) +
  geom_line(size = 1.1) +
  scale_x_continuous(expand = c(0.01, 0),
                     limits = c(min(myopia$year), max(myopia$year)),
                     breaks = seq(min(myopia$year), max(myopia$year), 1)) +
  scale_y_continuous(labels = label_percent()) +
  scale_colour_jco() +
  theme_classic() +
  theme(legend.position = c(0.85, 0.9)) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(x = "", y = "",
       colour = "",
       title = "Defective Vision Rate",
       subtitle = "% of students with visual acuity - VA (Snellen) ≥ 6/12 or VA (LogMAR) ≥ 0.30",
       caption = "Data: Health Promotion Board (data.gov.sg) | Graphic: @weiyuet")

# Save image
ggsave("figures/myopia-in-students.png", width = 7, height = 4.5)