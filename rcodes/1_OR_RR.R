#========================================================================#
# Title: Odds ratio and relative risk
# Author: Jom Research (https://jomresearch.netlify.app/)
# Date: Feb28, 2024
#========================================================================#

# Packages ----------------------------------------------------------------

library(tidyverse) #data manipulation
library(summarytools) #descriptive
library(epitools) #OR and RR


# Data --------------------------------------------------------------------

pc_data <- read.csv("data/chisquare_smoking.csv")


# Edit variable type ------------------------------------------------------

pc_data <- 
  pc_data %>% 
  mutate(Smoking = as.factor(Smoking),
         Lung_cancer = as.factor(Lung_cancer),
         Smoking = fct_recode(Smoking, 
                              nonsmoker = "0",
                              smoker = "1"),
         Lung_cancer = fct_recode(Lung_cancer, 
                                  noncancer = "0",
                                  cancer = "1"),
         Smoking = relevel(Smoking, ref = "smoker"),
         Lung_cancer = relevel(Lung_cancer, ref = "cancer"))


# Explore -----------------------------------------------------------------

summary(pc_data)
freq(pc_data)
ctable(pc_data$Smoking, pc_data$Lung_cancer)


# Odds ratio --------------------------------------------------------------

# Change data structure
or_data <- xtabs(~ Smoking + Lung_cancer, data = pc_data)
or_data

# OR
oddsratio.wald(or_data)

# Smoker have 3.37 higher odds of getting lung cancer than the non-smokers

# Relative risk -----------------------------------------------------------

# RR
riskratio.wald(or_data, rev = "both")

# Smokers have 1.56 higher risk of getting lung cancer than the non-smokers