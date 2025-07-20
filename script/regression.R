library(MASS)
library(tidyverse)

setwd("~/Desktop/HR-Project/Survey")

df <- read_csv("data/employee_survey.csv")

df$Gender <- as.integer(factor(df$Gender, levels = c("Male", "Female", "Other")))
df$MaritalStatus <- as.integer(factor(df$MaritalStatus, levels = c("Single", "Married", "Divorced","Widowed")))
df$JobLevel <- as.integer(factor(df$JobLevel, levels = c("Intern/Fresher", "Junior", "Mid", "Senior", "Lead")))
df$Dept <- as.integer(factor(df$Dept, levels = c("IT", "Sales", "Finance", "HR", "Marketing", "Legal", "Operations","Customer Service")))
df$EmpType <- as.integer(factor(df$EmpType, levels = c("Contract", "Part-Time", "Full-Time")))
df$CommuteMode <- as.integer(factor(df$CommuteMode, levels = c("Walk", "Bike", "Motorbike", "Car", "Public Transport")))
df$EduLevel <- as.integer(factor(df$EduLevel, levels = c("High School", "Bachelor", "Master", "PhD")))
df$haveOT <- as.integer(df$haveOT)

df$Age <- as.integer(cut(df$Age, breaks = c(0, 25, 35, Inf), labels = c(1, 2, 3), right = FALSE))
df$Experience <- as.integer(cut(df$Experience, breaks = c(-1, 3, 10, Inf), labels = c(1, 2, 3)))
df$PhysicalActivityHours <- as.integer(cut(df$PhysicalActivityHours, breaks = c(-0.1, 1, 3, Inf), labels = c(1, 2, 3)))
df$TrainingHoursPerYear <- as.integer(cut(df$TrainingHoursPerYear, breaks = c(-0.1, 10, 30, Inf), labels = c(1, 2, 3)))
df$SleepHours <- as.integer(cut(df$SleepHours, breaks = c(4, 6, 8, 11), labels = c(1, 2, 3), right = FALSE))
df$CommuteDistance <- as.integer(cut(df$CommuteDistance, breaks = c(-1, 5, 15, 30, Inf), labels = c(1, 2, 3, 4)))
df$TeamSize <- as.integer(cut(df$TeamSize, breaks = c(-1, 5, 15, Inf), labels = c(1, 2, 3)))
df$NumReports <- as.integer(cut(df$NumReports, breaks = c(-1, 0, 5, Inf), labels = c(1, 2, 3)))
df$NumCompanies <- as.integer(cut(df$NumCompanies, breaks = c(-1, 1, 3, 10, Inf), labels = c(1, 2, 3, 4)))

selected <- df |> dplyr::select(-EmpID)

summary(lm(selected$JobSatisfaction~., data = selected))

reg.selected <- df |> dplyr::select(JobSatisfaction, WLB, WorkEnv, Workload, 
                             Stress, SleepHours, haveOT)
summary(lm(reg.selected$JobSatisfaction~., data=reg.selected))

hist(reg.selected$JobSatisfaction)

df$JobSatisfaction <- factor(df$JobSatisfaction, ordered = TRUE)

model <- polr(JobSatisfaction ~ WLB + Stress + Workload + WorkEnv + SleepHours + haveOT, data = df, Hess = TRUE)

summary(model)

exp(coef(model))
