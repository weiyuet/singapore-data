library(tidyverse)

cpi <- read_csv("data/M212951.csv", skip = 10)

# Wrangle
# Remove bottom rows
cpi <- cpi[-c(70:86), ]

# Rename item column
cpi <- cpi %>% 
  rename(item = `Data Series`)

# Convert character columns to numeric
cpi <- cpi %>%
  mutate(across(10:13, as.numeric))

# Change to tidy format
cpi <- cpi %>%
  pivot_longer(2:13,
               names_to = "year",
               values_to = "price")

# Change year to numeric
cpi$year <- as.numeric(as.character(cpi$year))

#Visualize

items_selected <- c("Fishball Noodle (Per Bowl)", "Mee Rebus (Per Bowl)", "Chicken Rice (Per Plate)", "Chicken Nasi Briyani (Per Plate)", "Economical Rice (1 Meat & 2 Vegetables) (Per Plate)")

cpi %>% 
  filter(item %in% items_selected) %>% 
  ggplot(aes(x = year, y = price, colour = item)) +
  geom_line()