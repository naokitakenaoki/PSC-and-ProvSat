library(tidyverse)
psc<- (read.csv("C:/Users/Owner/Dropbox (個人)/UgandaPS/06 Assessments/067 Endline report/3Original data (Staff)/(Work0)PSC20260312.csv"))
class(psc)
psc24 <- read_csv("C:/Users/Owner/Dropbox (個人)/UgandaPS/06 Assessments/067 Endline report/3Original data (Staff)/(Work0)PSC20260312.csv") %>%
  filter(HID %in% c(2,4)) %>%
  mutate(across(starts_with("Q"),
                ~as.numeric(trimws(.)))) %>%
  mutate(across(starts_with("Q"),
                ~na_if(., 9)))

provsat24 <- read_csv("C:/Users/Owner/Dropbox (個人)/UgandaPS/06 Assessments/067 Endline report/3Original data (Staff)/(Work0)ProvSat20260312.csv") %>%
  filter(HID %in% c(2,4)) %>%
  mutate(across(starts_with("Q"),
                ~as.numeric(trimws(.)))) %>%
  mutate(across(starts_with("Q"),
                ~na_if(., 9)))

summary(psc24)
summary(provsat24)

library(tidyverse)
library(magrittr)

# Re-categorise cadre (Q1.2)
psc24 <- psc24 %>%
  mutate(
    cadre2 = case_when(
      Q1.2 == 1 ~ "Doctor",
      Q1.2 == 2 ~ "Nurse/Midwife",
      Q1.2 %in% c(3,4,5) ~ "Allied Health",
      TRUE ~ "Other"
    )
  )

# Definition of 10 elements of PSC
elements <- list(
  teamwork = c("Q2.01","Q2.08","Q2.09"),
  staffing = c("Q2.02","Q2.03","Q2.05","Q2.11"),
  org_learning = c("Q2.04","Q2.12","Q2.14"),
  response_error = c("Q2.06","Q2.07","Q2.10","Q2.13"),
  dept_head = c("Q3.1","Q3.2","Q3.3","Q3.4"),
  comm_error = c("Q4.1","Q4.2","Q4.3","Q4.4"),
  comm_open = c("Q4.5","Q4.6","Q4.7","Q4.8"),
  reporting = c("Q5.1","Q5.2"),
  top_mgmt = c("Q7.1","Q7.2","Q7.3","Q7.4"),
  handover = c("Q7.5","Q7.6","Q7.7")
)

# Making "long data"
psc24_long <- psc24 %>%
  pivot_longer(cols = starts_with("Q"),
               names_to = "question",
               values_to = "score")

# Linking 10 elements
psc24_long <- psc24_long %>%
  mutate(
    element = case_when(
      question %in% elements$teamwork ~ "Teamwork",
      question %in% elements$staffing ~ "Staffing",
      question %in% elements$org_learning ~ "Organisational learning",
      question %in% elements$response_error ~ "Response to error",
      question %in% elements$dept_head ~ "Dept head support",
      question %in% elements$comm_error ~ "Communication error",
      question %in% elements$comm_open ~ "Communication openness",
      question %in% elements$reporting ~ "Reporting",
      question %in% elements$top_mgmt ~ "Top management",
      question %in% elements$handover ~ "Handover"
    )
  )

# Mean by element
psc24_element <- psc24_long %>%
  group_by(RID, HID, BE, cadre2, element) %>%
  summarise(score = mean(score, na.rm = TRUE), .groups = "drop")

# Mean: 1 Teamwork
teamwork_data <- psc24_element %>%
  filter(element == "Teamwork")
teamwork_data %>%
  group_by(HID, BE) %>%
  summarise(
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
summary(teamwork_data$score)

# Mean: 2 Staffing and work pace
staffing_data <- psc24_element %>%
  filter(element == "Staffing")
staffing_data %>%
  group_by(HID, BE) %>%
  summarise(
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
summary(staffing_data$score)

# Mean: 3 Organisational learning
org_learning_data <- psc24_element %>%
  filter(element == "Organisational learning")
org_learning_data %>%
  group_by(HID, BE) %>%
  summarise(
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
summary(org_learning_data$score)

# Mean: 4 Response to error
response_error_data <- psc24_element %>%
  filter(element == "Response to error")
response_error_data %>%
  group_by(HID, BE) %>%
  summarise(
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
summary(response_error_data$score)

# Mean: 5 Support from dept/unit head
dept_head_data <- psc24_element %>%
  filter(element == "Dept head support")
dept_head_data %>%
  group_by(HID, BE) %>%
  summarise(
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
summary(dept_head_data$score)

# Mean: 6 Communication about error
comm_error_data <- psc24_element %>%
  filter(element == "Communication error")
comm_error_data %>%
  group_by(HID, BE) %>%
  summarise(
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
summary(comm_error_data$score)

# Mean: 7 Communication openness
comm_open_data <- psc24_element %>%
  filter(element == "Communication openness")
comm_open_data %>%
  group_by(HID, BE) %>%
  summarise(
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
summary(comm_open_data$score)

# Mean: 8 Actual reporting events
reporting_data <- psc24_element %>%
  filter(element == "Reporting")
reporting_data %>%
  group_by(HID, BE) %>%
  summarise(
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
summary(reporting_data$score)

# Mean: 9 Support from top management
top_mgmt_data <- psc24_element %>%
  filter(element == "Top management")
top_mgmt_data %>%
  group_by(HID, BE) %>%
  summarise(
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
summary(top_mgmt_data$score)

# Mean: 10 Handover and info exchange
handover_data <- psc24_element %>%
  filter(element == "Handover")
handover_data %>%
  group_by(HID, BE) %>%
  summarise(
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
summary(handover_data$score)

# Chart before-after
ggplot(psc24_element,
       aes(x = factor(BE), y = score, fill = factor(HID))) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  facet_wrap(~element)

# Chart score by question
ggplot(psc24_long,
       aes(x = factor(BE), y = score, fill = factor(HID))) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  facet_wrap(~question)

# U test
install.packages("rstatix")
library(rstatix)

psc24_element %>%
  group_by(HID, element) %>%
  wilcox_test(score ~ BE)

# Krustal-Wallis test difference by cadre
psc24_element %>%
  group_by(element, BE) %>%
  kruskal_test(score ~ cadre2)

ggplot(psc24_element,
       aes(x = factor(BE), y = score, fill = cadre2)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  facet_wrap(~element)
