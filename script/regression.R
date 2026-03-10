library(MASS)
library(tidyverse)
library(broom)
library(ggplot2)
library(dplyr)

df <- read_csv("data/employee_survey.csv") 


df <- df %>%
  mutate(
    Gender        = as.integer(factor(Gender, levels = c("Male", "Female", "Other"))),
    MaritalStatus = as.integer(factor(MaritalStatus, levels = c("Single", "Married", "Divorced", "Widowed"))),
    JobLevel      = as.integer(factor(JobLevel, levels = c("Intern/Fresher", "Junior", "Mid", "Senior", "Lead"))),
    Dept          = as.integer(factor(Dept, levels = c("IT", "Sales", "Finance", "HR", "Marketing", "Legal", "Operations", "Customer Service"))),
    EmpType       = as.integer(factor(EmpType, levels = c("Contract", "Part-Time", "Full-Time"))),
    CommuteMode   = as.integer(factor(CommuteMode, levels = c("Walk", "Bike", "Motorbike", "Car", "Public Transport"))),
    EduLevel      = as.integer(factor(EduLevel, levels = c("High School", "Bachelor", "Master", "PhD"))),
    haveOT        = as.integer(haveOT),
    
    Age                  = as.integer(cut(Age, breaks = c(0, 25, 35, Inf), labels = 1:3, right = FALSE)),
    Experience           = as.integer(cut(Experience, breaks = c(-1, 3, 10, Inf), labels = 1:3)),
    PhysicalActivityHours = as.integer(cut(PhysicalActivityHours, breaks = c(-0.1, 1, 3, Inf), labels = 1:3)),
    TrainingHoursPerYear = as.integer(cut(TrainingHoursPerYear, breaks = c(-0.1, 10, 30, Inf), labels = 1:3)),
    SleepHours           = as.integer(cut(SleepHours, breaks = c(4, 6, 8, 11), labels = 1:3, right = FALSE)),
    CommuteDistance      = as.integer(cut(CommuteDistance, breaks = c(-1, 5, 15, 30, Inf), labels = 1:4)),
    TeamSize             = as.integer(cut(TeamSize, breaks = c(-1, 5, 15, Inf), labels = 1:3)),
    NumReports           = as.integer(cut(NumReports, breaks = c(-1, 0, 5, Inf), labels = 1:3)),
    NumCompanies         = as.integer(cut(NumCompanies, breaks = c(-1, 1, 3, 10, Inf), labels = 1:4))
  ) %>%
  select(-EmpID)


model1 <- lm(JobSatisfaction ~ ., data = df)

summary(model1)

tidy_model1 <- broom::tidy(model1) %>%
  filter(term != "(Intercept)") %>%
  mutate(significant = p.value < 0.05)


ggplot(tidy_model1, aes(x = reorder(term, estimate), y = estimate, fill = significant)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "firebrick", "FALSE" = "grey70"), 
                    labels = c("Non-Significant", "Significant")) +
  labs(
    title = "Factors Affecting Job Satisfaction",
    subtitle = "Analysis of coefficients with statistical significance (p < 0.05)",
    x = "Variables",
    y = "Regression Coefficient",
    fill = "Significance"
  ) +
  theme_minimal()






reg_selected <- df %>%
  dplyr::select(JobSatisfaction, WLB, WorkEnv, Workload, Stress, SleepHours, haveOT) %>%
  mutate(JobSatisfaction = factor(JobSatisfaction, ordered = TRUE)) 

model2 <- polr(JobSatisfaction ~ ., data = reg_selected, Hess = TRUE)

summary(model2)
exp(coef(model2)) 

tidy_model2 <- broom::tidy(model2) %>%
  filter(!stringr::str_detect(term, "\\|")) %>% 
  mutate(effect = ifelse(estimate > 0, "Positive", "Negative"))


ggplot(tidy_model2, aes(x = reorder(term, estimate), y = estimate, fill = effect)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "darkgreen", "Negative" = "firebrick")) +
  labs(
    title = "Impact of Key Factors on Job Satisfaction Level",
    subtitle = "Ordered Logistic Regression (Log-odds scale)",
    x = "Factors",
    y = "Estimate",
    fill = "Direction of Effect"
  ) +
  theme_minimal()



