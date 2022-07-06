# Loading libraries
library(tidyverse)
library(scales)

# Loading data
students_and_teachers_primary_schools <- read_csv("data/students-and-teachers-in-schools/students-and-teachers-primary-schools.csv")

# Wrangling data
students_and_teachers_primary_schools %>% 
  filter(sex == "MF", school_type == "Govt") %>% 
  ggplot(aes(x = year, y = students_pri)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  scale_x_continuous(breaks = seq(1980, 2020, 5)) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(x = "", y = "",
       title = "Primary school students (M&F) in Govt schools",
       caption = "Source: data.gov.sg\nGraphic: @weiyuet")
  
ggsave("figures/primary-school-students-in-govt-schools.png", width = 8, height = 6)

students_and_teachers_primary_schools %>% 
  filter(sex == "MF", school_type == "Aided") %>% 
  ggplot(aes(x = year, y = students_pri)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  scale_x_continuous(breaks = seq(1980, 2020, 5)) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(x = "", y = "",
       title = "Primary school students (M&F) in Aided schools",
       caption = "Source: data.gov.sg\nGraphic: @weiyuet")

ggsave("figures/primary-school-students-in-aided-schools.png", width = 8, height = 6)


students_and_teachers_primary_schools %>% 
  ggplot(aes(x = year, y = students_pri,
             colour = school_type)) +
  geom_line() +
  facet_wrap(~sex) +
  theme_classic() +
  theme(legend.position = "right") +
  scale_color_brewer(type = "qual", palette = 6) +
  scale_x_continuous(breaks = seq(1980, 2020, 5)) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(x = "", y = "",
       colour = "School Type",
       caption = "Source: data.gov.sg\nGraphic: @weiyuet")

ggsave("figures/primary-school-students-in-govt-and-aided-schools.png", width = 8, height = 6)
