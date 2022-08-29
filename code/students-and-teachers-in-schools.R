# Load libraries
library(tidyverse)
library(scales)
library(ggsci)

# Load data
students_and_teachers_primary_schools <- read_csv("data/students-and-teachers-in-schools/students-and-teachers-primary-schools.csv")

# Plot number of primary school students in government and aided schools
students_and_teachers_primary_schools %>% 
  ggplot(aes(x = year, y = students_pri,
             colour = school_type)) +
  geom_line(size = 1) +
  facet_wrap(~sex) +
  theme_classic() +
  theme(legend.position = c(0.25, 0.7)) +
  scale_colour_jco() +
  scale_x_continuous(breaks = seq(1980, 2020, 5)) +
  scale_y_continuous(labels = label_number(big.mark = ","), 
                     limits = c(10000, 250000)) +
  labs(x = "", y = "",
       colour = "School Type",
       caption = "Source: Ministry of Education (data.gov.sg)\nGraphic: @weiyuet")

# Save png
ggsave("figures/primary-school-students-in-govt-and-aided-schools.png", width = 8, height = 6)

# Wrangle data
# Calculate student-teacher ratio
students_and_teachers_primary_schools <- students_and_teachers_primary_schools %>% 
  mutate(student_teacher_ratio = students_pri/teachers_pri)

students_and_teachers_primary_schools %>% 
  filter(sex == "MF") %>%
  ggplot(aes( x = year, y = student_teacher_ratio, colour = school_type)) +
  geom_smooth() +
  geom_jitter() +
  facet_wrap(~school_type) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_colour_jco() +
  labs(x = "", y = "",
       title = "Student-Teacher Ratio",
       caption = "Source: Source: Ministry of Education (data.gov.sg)\nGraphic: @weiyuet")

# Save png
ggsave("figures/student-teacher-ratio-primary-schools.png", width = 8, height = 6)