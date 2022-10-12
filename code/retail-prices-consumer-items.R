# Setup
library(tidyverse)
library(scales)

raw_prices <- read_csv("data/retail-prices-consumer-items/M212951-table.csv", skip = 10)

# Wrangle
# Remove bottom rows
prices <- raw_prices[-c(70:86), ]

# Rename item column
prices <- prices %>% 
  rename(item = `Data Series`)

# Convert character columns to numeric
prices <- prices %>%
  mutate(across(10:13, as.numeric))

# Change to tidy format
tidy_prices <- prices %>%
  pivot_longer(2:13,
               names_to = "year",
               values_to = "price")

# Change year to numeric
tidy_prices$year <- as.numeric(as.character(tidy_prices$year))

# Visualize
# Price of typical dishes
items_selected <- c("Fishball Noodle (Per Bowl)", "Mee Rebus (Per Bowl)", "Chicken Rice (Per Plate)", "Chicken Nasi Briyani (Per Plate)", "Economical Rice (1 Meat & 2 Vegetables) (Per Plate)", "Roti Prata (Plain) (Per Piece)", "Fried Carrot Cake (Per Plate)", "Ice Kachang (Per Bowl)")

tidy_prices %>% 
  filter(item %in% items_selected) %>% 
  ggplot(aes(x = year, y = price, colour = item)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~item, scales = "free_y") +
  scale_x_continuous(limits = c(2014, 2022)) +
  scale_y_continuous(labels = label_number(prefix = "$",
                                           accuracy = 0.01)) +
  theme_light() +
  labs(x = "", y = "",
       title = "Price Trends of Typical Dishes",
       caption = "Data: Singapore Department of Statistics")

# Save image
ggsave("figures/price-typical-dishes.png", width = 7, height = 5)

# Price of selected household groceries
items_selected <- c("Premium Thai Rice (Per 5 Kilogram)", "Ordinary White Bread (Per 400 Gram)", "Instant Noodles (Per 5 Packets)", "Whole Chicken, Chilled (Per Kilogram)", "Hen Eggs (Per 10)", "Chinese Kale (Kailan) (Per Kilogram)")

tidy_prices %>% 
  filter(item %in% items_selected) %>% 
  ggplot(aes(x = year, y = price, colour = item)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~item, scales = "free_y") +
  scale_y_continuous(labels = label_number(prefix = "$",
                                           accuracy = 0.01)) +
  theme_light() +
  labs(x = "", y = "",
       title = "Price Trends of Selected Groceries",
       caption = "Data: Singapore Department of Statistics")

# Save image
ggsave("figures/price-household-groceries.png", width = 7, height = 5)

# Price of fuel
items_selected <- c("Diesel (Per Litre)", "Petrol, 98 Octane (Per Litre)", "Petrol, 95 Octane (Per Litre)", "Petrol, 92 Octane (Per Litre)", "Liquefied Petroleum Gas (LPG) (Per Kilogram)")

tidy_prices %>% 
  filter(item %in% items_selected) %>% 
  ggplot(aes(x = year, y = price, colour = item)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~item, scales = "free_y") +
  scale_y_continuous(labels = label_number(prefix = "$",
                                           accuracy = 0.1)) +
  theme_light() +
  labs(x = "", y = "",
       title = "Price Trends of Fuel",
       caption = "Data: Singapore Department of Statistics")

# Save image
ggsave("figures/price-fuel.png", width = 7, height = 5)
  