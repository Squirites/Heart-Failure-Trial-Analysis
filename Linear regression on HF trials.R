library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(boot)

## Description of data set

# This data set contains details regarding 895 heart failure (HF) trials, including: length of the trial in months, number of endpoints, number of trial locations, enrollment number,
# phase of the trial, whether the trial has study results, funder type and age cohort. Please note only trials that were completed and had data
# for all of the independent variables used in the analysis (noted below) were included in this analysis. 

## Description of analysis

# In the following analysis, linear regression is performed on the dependent variable "Months" (length of the trial), with remaining variables as independent variables. Please note
# some variables ('Registry Code', 'SSD', 'PCD' and 'Completion Date') were not included as independent variables. First data is loaded.

hf_trials <- read_excel("C:/Users/adamb/OneDrive/Documents/R Files/HF Trials ML Algorithms/hf_trials.xlsx")

# 'StudyResults', 'Sex', 'Age', 'Phases' & 'FunderType' variables are converted to factors 

hf_trials$StudyResults <- as.factor(hf_trials$StudyResults )
hf_trials$Age <- as.factor(hf_trials$Age )
hf_trials$Phases <- as.factor(hf_trials$Phases )
hf_trials$FunderType <- as.factor(hf_trials$FunderType )

# Linear regression is performed with the following variables: Months (dependent), "EndpointNo", "LocationNo", "FunderType", "Age", "StudyREsults" & "Enrollment"

lm_fit_all <- lm(Months ~ Enrollment + FunderType + Phases + EndpointsNo + LocationNo + Age + StudyResults, data = hf_trials)
summary(lm_fit_all)

# The summary of lm_fit_all shows that the following independent variables are significantly related
# to the dependent variable 'Months': 'Enrollment', 'FunderType', and 'Phases', with an adj R^2 of 0.17

# We now run produce a regression modelling using only those dependent variables significantly 
# related to 'Months' and include a transformation of 'Enrollment', raising it to the power of (1/2)

lm_fit_select <- lm(Months ~ Enrollment + I(Enrollment^(1/2)) + FunderType + Phases, data = hf_trials)
summary(lm_fit_select)

# The inclusion of the tranformed 'Enrollment' variable improves the model accuracy, adj R^2 is now 0.25
# We now run 20 k-fold cross validation (with k = 10) to determine the optimal exponent to use to transform
# the Enrollment variable

set.seed(123)
cv_error <- rep(0,20)
for (i in 1:20) {
  glm_fit = glm(Months ~ I(Enrollment^(1/i)), data = hf_trials)
  cv_error[i] <- cv.glm(hf_trials, glm_fit, K =10)$delta[1]
}

# Plotting and printing the vector of cv_errors (containing the cv_error for each exponent), we can see that the CV error drammatically decreases once an exponent of 1/2 
# or smaller is used, and that the exponent that produced the smallest CV error is (1/14)

print(cv_error)
plot(cv_error)
which.min(cv_error)

# We now update the model following the k-fold cross validation

lm_fit_updated <- lm(Months ~ Enrollment + I(Enrollment^(1/14)) + FunderType + Phases, data = hf_trials)
summary(lm_fit_updated)

# Using the model to predict trial duration in months for a Phase 1 trial with enrollment of 35 people, which funded by industry. THe prediction is 15 months. 

pred <- predict(lm_fit_updated,  data.frame( Enrollment = 35, FunderType = "INDUSTRY", Phases = "PHASE1"), interval = "confidence")
print(pred)

