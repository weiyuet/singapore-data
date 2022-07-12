# Load libraries
library(tidyverse)
library(scales)

# Load data
students_and_teachers_primary_schools <- read_csv("data/students-and-teachers-in-schools/students-and-teachers-primary-schools.csv")

# Wrangle data

# Plot number of primary school students in governmentt and aided schools
students_and_teachers_primary_schools %>% 
  ggplot(aes(x = year, y = students_pri,
             colour = school_type)) +
  geom_line(size = 1.1) +
  facet_wrap(~sex) +
  theme_classic() +
  theme(legend.position = "right") +
  scale_color_brewer(type = "qual", palette = 6) +
  scale_x_continuous(breaks = seq(1980, 2020, 5)) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(x = "", y = "",
       colour = "School Type",
       caption = "Source: data.gov.sg\nGraphic: @weiyuet")

# Save png
ggsave("figures/primary-school-students-in-govt-and-aided-schools.png", width = 8, height = 6)

# Wrangle data
# Calculate student-teacher ratio
students_and_teachers_primary_schools %>% 
  select(year, school_type, students_pri, teachers_pri) %>% 
  mutate(student_teacher_ratio = students_pri/teachers_pri) %>% 
  ggplot(aes( x = year, y = student_teacher_ratio, colour = school_type)) +
  geom_smooth() +
  geom_jitter() +
  facet_wrap(~school_type) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_colour_brewer(type = "qual", palette = 6) +
  labs(x = "", y = "",
       title = "Student-Teacher Ratio",
       caption = "Source: data.gov.sg\nGraphic: @weiyuet")

# Save png
ggsave("figures/student-teacher-ratio-primary-schools.png", width = 8, height = 6)
