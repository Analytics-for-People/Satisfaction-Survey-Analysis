library(tidyverse)
library(psych)
library(polycor)
library(lavaan)

df <- read_csv("data/employee_survey.csv")

df$Gender <- as.integer(factor(df$Gender, levels = c("Male", "Female", "Other")))
df$MaritalStatus <- as.integer(factor(df$MaritalStatus, levels = c("Single", "Married", "Divorced","Widowed")))
df$JobLevel <- as.integer(factor(df$JobLevel, levels = c("Intern/Fresher", "Junior", "Mid", "Senior", "Lead"), ordered = TRUE))
df$Dept <- as.integer(factor(df$Dept, levels = c("IT", "Sales", "Finance", "HR", "Marketing", "Legal", "Operations","Customer Service")))
df$EmpType <- as.integer(factor(df$EmpType, levels = c("Contract", "Part-Time", "Full-Time")))
df$CommuteMode <- as.integer(factor(df$CommuteMode, levels = c("Walk", "Bike", "Motorbike", "Car", "Public Transport")))
df$EduLevel <- as.integer(factor(df$EduLevel, levels = c("High School", "Bachelor", "Master", "PhD"), ordered = TRUE))
df$haveOT <- as.integer(df$haveOT)

df$Age <- as.integer(cut(df$Age, breaks = c(0, 25, 35, Inf), labels = c(1, 2, 3), right = FALSE, ordered_result = TRUE))
df$Experience <- as.integer(cut(df$Experience, breaks = c(-1, 3, 10, Inf), labels = c(1, 2, 3), ordered_result = TRUE))
df$PhysicalActivityHours <- as.integer(cut(df$PhysicalActivityHours, breaks = c(-0.1, 1, 3, Inf), labels = c(1, 2, 3), ordered_result = TRUE))
df$TrainingHoursPerYear <- as.integer(cut(df$TrainingHoursPerYear, breaks = c(-0.1, 10, 30, Inf), labels = c(1, 2, 3), ordered_result = TRUE))
df$SleepHours <- as.integer(cut(df$SleepHours, breaks = c(4, 6, 8, 11), labels = c(1, 2, 3), right = FALSE, ordered_result = TRUE))
df$CommuteDistance <- as.integer(cut(df$CommuteDistance, breaks = c(-1, 5, 15, 30, Inf), labels = c(1, 2, 3, 4), ordered_result = TRUE))
df$TeamSize <- as.integer(cut(df$TeamSize, breaks = c(-1, 5, 15, Inf), labels = c(1, 2, 3), ordered_result = TRUE))
df$NumReports <- as.integer(cut(df$NumReports, breaks = c(-1, 0, 5, Inf), labels = c(1, 2, 3), ordered_result = TRUE))
df$NumCompanies <- as.integer(cut(df$NumCompanies, breaks = c(-1, 1, 3, 10, Inf), labels = c(1, 2, 3, 4), ordered_result = TRUE))

df$WLB <- as.integer(factor(df$WLB, ordered = TRUE))
df$WorkEnv <- as.integer(factor(df$WorkEnv, ordered = TRUE))
df$Workload <- as.integer(factor(df$Workload, ordered = TRUE))
df$Stress <- as.integer(factor(df$Stress, ordered = TRUE))
df$Stress <- as.integer(factor(df$Stress, ordered = TRUE))
df$NumCompanies <- as.integer(factor(df$NumCompanies, ordered = TRUE))
df$JobSatisfaction <- as.integer(factor(df$JobSatisfaction, ordered = TRUE))

selected <- df |> select(-c(EmpID, Gender, MaritalStatus, Dept, EmpType, CommuteMode))

poly_s <-polychoric(selected)$rho

fa.parallel(poly_s, fa = "fa", show.legend = TRUE, main = "Parallel Analysis")

efa_result <- fa(poly_s, nfactors = 3, rotate = "varimax", fm = "ml")
fa.diagram(efa_result, main = "EFA 3-Factor Solution Diagram")

model_cfa <- '
  CareerDev =~ Experience + EduLevel
  Leadership =~ TeamSize + NumReports + JobLevel
  '
fit_cfa <- cfa(model_cfa, data = selected, estimator = 'MLR', std.lv = TRUE)

summary(fit_cfa, fit.measures = TRUE, standardized = TRUE)
