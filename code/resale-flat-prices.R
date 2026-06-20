######################
# Resale Flat Prices #
######################

#### 1.0 Setup ####
# 1.1 Load libraries
library(tidyverse)
library(scales)
library(zoo)
library(flextable)

#### 2.0 Load Data ####
# 2.1 Clean up files and rename new downloaded files
list.files(
  "data/resale-flat-prices",
  pattern = "^Resaleflatprices",
  full.names = TRUE
) %>%
  walk(~ file.rename(.x, "data/resale-flat-prices/resale-flat-prices.csv"))

# 2.2 Read local file
resale_flat_prices_raw <- read_csv(
  "data/resale-flat-prices/resale-flat-prices.csv"
)

#### 3.0 Prep Data ####
# 3.1 Convert month from character to a yearmon object #
resale_flat_prices_clean <- resale_flat_prices_raw %>%
  mutate(
    month = as.yearmon(month, format = "%Y-%m"),
    # 3.2 Calculate price per square meter
    price_per_sqm = resale_price / floor_area_sqm,
    # 3.3 Parse remaining lease into a clean decimal number
    lease_years = as.numeric(str_extract(
      remaining_lease,
      "\\d+(?=\\s*years?)"
    )),
    lease_months = str_extract(remaining_lease, "\\d+(?=\\s*months?)"),
    lease_months = if_else(is.na(lease_months), 0, as.numeric(lease_months)),
    remaining_lease_numeric = lease_years + (lease_months / 12)
  ) %>%
  select(-lease_years, -lease_months)

#### 4.0 Shared Plot Configurations ####
# 4.1 Define a standardized plot style
plot_style <- list(
  geom_point(color = "gray75", size = 0.3, alpha = 0.4),
  theme_minimal(base_size = 11),
  theme(
    plot.caption = element_text(hjust = 0),
    panel.grid.minor = element_blank()
  )
)

# 4.2 Date scale configuration for clean year-only label output
min_date <- min(resale_flat_prices_clean$month)
max_date <- max(resale_flat_prices_clean$month)

date_breaks <- as.yearmon(seq(
  from = floor(min_date),
  to = ceiling(max_date),
  by = 2
))

date_scale <- scale_x_yearmon(
  format = "%Y",
  breaks = date_breaks
)

# 4.3 Price scale configuration (Millions)
price_scale_m <- scale_y_continuous(
  labels = label_dollar(scale = 1 / 1000000, suffix = "M")
)

# 4.4 Dynamic update time-stamp
update_time <- format(Sys.time(), "%Y-%m-%d %H:%MH")
shared_caption <- paste0(
  "Data: Housing & Development Board (HDB) | Updated: ",
  update_time,
  " | Graphic: weiyuet"
)

#### 5.0 Plotting Data ####
# 5.1 By Town
plot_1 <- resale_flat_prices_clean %>%
  filter(town != "CENTRAL AREA") %>%
  ggplot(aes(x = month, y = resale_price)) +
  geom_point(aes(color = resale_price >= 1000000), size = 0.3, alpha = 0.4) +
  scale_color_manual(values = c("TRUE" = "coral1", "FALSE" = "gray75")) +
  geom_smooth(
    color = "royalblue",
    se = FALSE,
    linewidth = 0.8,
    method = "gam"
  ) +
  geom_hline(yintercept = 1000000, linetype = "dotted", color = "black") +
  date_scale +
  price_scale_m +
  facet_wrap(~town) +
  theme_minimal(base_size = 11) +
  theme(
    plot.caption = element_text(hjust = 0),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +
  labs(
    title = "Million-dollar flats are not evenly distributed across towns",
    x = NULL,
    y = NULL,
    caption = shared_caption
  )

# 5.2 Multivariate Comparison (Time vs Price/Sqm vs Size vs Town)
plot_2 <- resale_flat_prices_clean %>%
  filter(
    town %in%
      c(
        "BISHAN",
        "BUKIT MERAH",
        "JURONG WEST",
        "MARINE PARADE",
        "TAMPINES",
        "TOA PAYOH"
      )
  ) %>%
  filter(flat_type %in% c("3 ROOM", "4 ROOM", "5 ROOM")) %>%
  ggplot(aes(x = month, y = price_per_sqm, color = flat_type)) +
  theme_minimal(base_size = 11) +
  geom_smooth(se = FALSE, linewidth = 0.8, method = "gam") +
  date_scale +
  scale_y_continuous(labels = label_dollar()) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  facet_wrap(~town, ncol = 3) +
  labs(
    title = "Value Trends by Flat Size and Region",
    subtitle = "Price per sq meter growth across sample towns",
    x = NULL,
    y = "Price per Sqm ($)",
    color = "Flat Type",
    caption = shared_caption
  ) +
  theme(plot.caption = element_text(hjust = 0))

# 5.3 Lease Depreciation Impact
plot_3 <- resale_flat_prices_clean %>%
  ggplot(aes(x = remaining_lease_numeric, y = price_per_sqm)) +
  plot_style +
  geom_smooth(
    color = "royalblue",
    se = FALSE,
    linewidth = 0.8,
    method = "gam"
  ) +
  scale_x_reverse(limits = c(99, 40), breaks = seq(100, 40, -10)) +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "How does an aging HDB lease affect its value?",
    subtitle = "Price per square meter vs. Remaining lease (years)",
    x = "Remaining Lease (Years left)",
    y = "Price per Sqm ($)",
    caption = shared_caption
  )

#### 6.0 Summary Table ####
# Compute total and highest transacted price for million-dollar transactions
million_dollar_flat_summary <- resale_flat_prices_clean %>%
  filter(resale_price >= 1000000) %>%
  group_by(Town = town) %>%
  summarise(
    `Total Million-Dollar Flats` = n(),
    `Highest Record Price` = max(resale_price)
  ) %>%
  arrange(desc(`Total Million-Dollar Flats`))

# Format summary table
table_image <- million_dollar_flat_summary %>%
  flextable() %>%
  theme_vanilla() %>%
  colformat_double(big.mark = ",", digits = 0, prefix = "$") %>%
  add_footer_lines(shared_caption) %>%
  bg(bg = "white", part = "all") %>%
  autofit()

#### 7.0 Export and Save Images ####
# Save plots
all_plots <- list(
  "town-absolute" = plot_1,
  "multivariate-sqm" = plot_2,
  "lease_decay" = plot_3
)

iwalk(
  all_plots,
  ~ ggsave(
    filename = paste0("figures/resale-flat-prices-", .y, ".png"),
    plot = .x,
    width = 10,
    height = 8
  )
)

# Save summary table
save_as_image(
  table_image,
  path = "figures/resale-flats-prices-summary-table.png"
)

# End
