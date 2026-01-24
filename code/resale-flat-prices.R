######################
# Resale Flat Prices #
######################

#### Setup ####
# Load libraries
library(tidyverse)
library(zoo)

#### Load Data ####
# Check if updated file exists
existing <- "data/resale-flat-prices/resale-flat-prices.csv"
updated <- "data/resale-flat-prices/ResaleflatpricesbasedonregistrationdatefromJan2017onwards.csv"

if (file.exists(updated)) {
  file.rename(
    from = updated,
    to = existing
  )
}

# Read local file
resale_flat_prices_raw <- read_csv(existing)

#### Prep Data ####
# Convert month from character to a yearmon object
resale_flat_prices_clean <- resale_flat_prices_raw %>%
  mutate(month = as.yearmon(month, format = "%Y-%m"))

#### Explore Data ####
resale_flat_prices_clean %>%
  count(town, sort = TRUE) %>%
  print(n = Inf) # 26 towns

resale_flat_prices_clean %>%
  count(flat_type, sort = TRUE) # 7 types

resale_flat_prices_clean %>%
  count(flat_model, sort = TRUE) %>%
  print(n = Inf) # 21 models

#### Plot Data ####
# By town
plot_1 <- resale_flat_prices_clean %>%
  filter(town != "CENTRAL AREA") %>%
  ggplot(aes(x = month, y = resale_price)) +
  geom_point(color = "gray70", size = 0.5) +
  scale_x_yearmon(
    format = "%Y-%m",
    breaks = seq(
      from = min(resale_flat_prices_clean$month),
      to = max(resale_flat_prices_clean$month),
      by = 4
    )
  ) +
  geom_smooth(se = FALSE) +
  geom_hline(yintercept = 1000000, linetype = "dotted", color = "red") +
  facet_wrap(~town) +
  theme(axis.title = element_blank(), plot.caption = element_text(hjust = 0)) +
  labs(
    title = "Resale flat prices are trending up; SGD1 million flats are not evenly distributed across towns",
    caption = "Data: Housing & Development Board (HDB) | Graphic: weiyuet"
  )

# By type of flat
plot_2 <- resale_flat_prices_clean %>%
  ggplot(aes(x = month, y = resale_price)) +
  geom_point(color = "gray70", size = 0.5) +
  scale_x_yearmon(
    format = "%Y-%m",
    breaks = seq(
      from = min(resale_flat_prices_clean$month),
      to = max(resale_flat_prices_clean$month),
      by = 4
    )
  ) +
  geom_smooth(se = FALSE) +
  geom_hline(yintercept = 1000000, linetype = "dotted", color = "red") +
  facet_wrap(~flat_type) +
  theme(axis.title = element_blank(), plot.caption = element_text(hjust = 0)) +
  labs(
    title = "Impossible to sell 1, 2-room flat for over a million",
    caption = "Data: Housing & Development Board (HDB) | Graphic: weiyuet"
  )

# By storey
plot_3 <- resale_flat_prices_clean %>%
  ggplot(aes(x = month, y = resale_price)) +
  geom_point(color = "gray70", size = 0.5) +
  scale_x_yearmon(
    format = "%Y-%m",
    breaks = seq(
      from = min(resale_flat_prices_clean$month),
      to = max(resale_flat_prices_clean$month),
      by = 4
    )
  ) +
  geom_smooth(se = FALSE) +
  geom_hline(yintercept = 1000000, linetype = "dotted", color = "red") +
  facet_wrap(~storey_range, ncol = 6) +
  theme(axis.title = element_blank(), plot.caption = element_text(hjust = 0)) +
  labs(
    title = "Higher floors generally command better selling prices",
    caption = "Data: Housing & Development Board (HDB) | Graphic: weiyuet"
  )

#### Save Images ####
ggsave(
  "figures/resale-flat-prices-town.png",
  plot_1,
  width = 10,
  height = 8
)

ggsave(
  "figures/resale-flat-prices-flat-type.png",
  plot_2,
  width = 10,
  height = 8
)

ggsave(
  "figures/resale-flat-prices-storey-range.png",
  plot_3,
  width = 10,
  height = 8
)

# End
