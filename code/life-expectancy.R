###################
# Life Expectancy #
###################

# Setup
library(tidyverse)
library(scales)
library(glue)
library(paletteer)

# Load data
life_expectancy <- read_csv("data/life-expectancy-by-sex-annual/life-expectancy-at-birth-and-age-65-years.csv")

# Plot data
# Line plot of life expectancy
life_expectancy %>%
       ggplot(aes(x = year,
                  y = value,
                  colour = level_1)) +
       geom_line(linewidth = 1) +
       facet_wrap(vars(level_1),
                  scales = "free_y") +
       scale_x_continuous(breaks = seq(1960, 2020, 10)) +
       scale_colour_paletteer_d("ggsci::default_jco") +
       labs(x = "",
            y = "",
            title = "Life Expectancy of Residents in Singapore",
            caption = "Data: Ministry of Trade and Industry - Department of Statistics (data.gov.sg) | Graphic: @weiyuet") +
        theme_classic() +
        theme(legend.title = element_blank(),
              legend.position = "none")

# Save image
ggsave("figures/life-expectancy.png", width = 6.5, height = 4.5)

# Wrangle data and plot
# Change in life expectancy
life_expectancy %>%
       filter(year == 1960 | year == 2018) %>%
       pivot_wider(names_from = year, values_from = value) %>%
       mutate(gap = `2018` - `1960`) %>%
       arrange(desc(gap)) %>%
       ggplot(aes(x = `1960`, xend = `2018`, y = reorder(level_1, gap)),
              group = level_1
       ) +
       geom_dumbbell(
              colour = "#dddddd",
              size = 5,
              colour_x = "#FAAB18",
              colour_xend = "#1380A1"
       ) +
       scale_x_continuous(
              limits = c(0, 85),
              breaks = seq(0, 85, 5)
       ) +
       scale_y_discrete(labels = c("From Age 65", "At Birth")) +
       theme_classic() +
       labs(
              x = "", y = "",
              title = glue("Change in Life Expectancy between {min(life_expectancy$year)} to {max(life_expectancy$year)}"),
              subtitle = "We're living longer",
              caption = "Data: Ministry of Trade and Industry - Department of Statistics (data.gov.sg) | Graphic: @weiyuet"
       )

# Save image
ggsave("figures/life-expectancy-change.png", width = 8, height = 4.5)