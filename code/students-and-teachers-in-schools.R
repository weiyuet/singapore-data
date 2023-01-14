#########################
# Students and Teachers #
#########################

#### Setup ####
library(tidyverse)
library(scales)
library(paletteer)

#### Load data primary schools ####
students_and_teachers_primary_schools <- read_csv("data/students-and-teachers-in-schools/students-and-teachers-primary-schools.csv")

#### Wrangle ####
# Calculate student-teacher ratio
students_and_teachers_primary_schools <- students_and_teachers_primary_schools %>% 
  mutate(student_teacher_ratio = students_pri/teachers_pri)

#### Visualize ####
# Plot number of students in primary schools
students_and_teachers_primary_schools %>% 
  ggplot(aes(x = year,
             y = students_pri,
             colour = school_type)) +
  geom_line(linewidth = 1) +
  facet_wrap(~sex) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(colour = guide_legend(nrow = 1)) +
  scale_colour_paletteer_d("ggsci::default_jco") +
  scale_x_continuous(breaks = seq(1980, 2020, 5)) +
  scale_y_continuous(labels = label_number(big.mark = ","), 
                     limits = c(10000, 250000)) +
  labs(x = "",
       y = "",
       title = "Number of Students in Primary Schools",
       colour = "School Type",
       caption = "Data: Ministry of Education (data.gov.sg) | Graphic: @weiyuet")

#### Save image ####
ggsave("figures/primary-school-students.png", width = 8, height = 6)

# Plot student-teacher ratio
students_and_teachers_primary_schools %>% 
  filter(sex == "MF") %>%
  ggplot(aes( x = year, y = student_teacher_ratio, colour = school_type)) +
  geom_smooth() +
  geom_jitter() +
  facet_wrap(~school_type) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_colour_paletteer_d("ggsci::default_jco") +
  labs(x = "", y = "",
       title = "Student-Teacher Ratio in Primary Schools",
       caption = "Data: Ministry of Education (data.gov.sg) | Graphic: @weiyuet")

#### Save image ####
ggsave("figures/student-teacher-ratio-primary-schools.png", width = 8, height = 6)

#### Load data secondary schools ####
students_and_teachers_secondary_schools <- read_csv("data/students-and-teachers-in-schools/students-and-teachers-secondary-schools.csv")

#### Wrangle ####
# Fix duplicate school types "Specialised Independant" and "Specialised Independent" and "Auto"
students_and_teachers_secondary_schools <- students_and_teachers_secondary_schools %>%
  mutate(school_type = case_when(school_type == "Specialised Independant" ~ "Specialised Independent",
                                 school_type == "Auto" ~ "Autonomous",
                                 TRUE ~ school_type))

# Calculate student-teacher ratio
students_and_teachers_secondary_schools <- students_and_teachers_secondary_schools %>% 
  mutate(student_teacher_ratio = student_sec/teacher_sec)

#### Visualize ####
# Plot number of secondary school students
students_and_teachers_secondary_schools %>% 
  ggplot(aes(x = year,
             y = student_sec,
             colour = school_type)) +
  geom_line(linewidth = 1) +
  facet_wrap(~sex) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(colour = guide_legend(nrow = 1)) +
  scale_colour_paletteer_d("ggsci::default_jco") +
  scale_x_continuous(breaks = seq(1980, 2020, 5)) +
  scale_y_continuous(breaks = seq(0, 200000, 25000),
                     labels = label_number(big.mark = ",")) +
  labs(x = "",
       y = "",
       title = "Number of Students in Secondary Schools",
       colour = "School Type",
       caption = "Data: Ministry of Education (data.gov.sg) | Graphic: @weiyuet")

#### Save image ####
ggsave("figures/secondary-school-students.png", width = 8, height = 6)

# Plot student-teacher ratio
students_and_teachers_secondary_schools %>% 
  filter(sex == "MF") %>%
  ggplot(aes(x = year,
             y = student_teacher_ratio,
             colour = school_type)) +
  geom_smooth() +
  geom_jitter() +
  facet_wrap(~school_type, scales = "free") +
  theme_classic() +
  theme(legend.position = "none") +
  scale_colour_paletteer_d("ggsci::default_jco") +
  labs(x = "",
       y = "",
       title = "Student-Teacher Ratio in Secondary Schools",
       caption = "Data: Ministry of Education (data.gov.sg) | Graphic: @weiyuet")

#### Save image ####
ggsave("figures/student-teacher-ratio-secondary-schools.png", width = 8, height = 6)