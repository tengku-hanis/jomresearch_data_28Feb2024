#========================================================================#
# Title: Pearson's chi-square test
# Author: Jom Research (https://jomresearch.netlify.app/)
# Date: Feb28, 2024
#========================================================================#

# Packages ----------------------------------------------------------------

library(tidyverse) #data manipulation
library(summarytools) #descriptive


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
                                  cancer = "1"))


# Explore -----------------------------------------------------------------

summary(pc_data)
freq(pc_data)
ctable(pc_data$Smoking, pc_data$Lung_cancer)


# Pearson's chi-square test -----------------------------------------------

pc_mod <- chisq.test(x = pc_data$Smoking, y = pc_data$Lung_cancer, correct = FALSE)
pc_mod$expected 
#Assumption - expected count < 5 is 0 or <20%, so Pearson's chi-square is used

pc_mod #result


# More advance ------------------------------------------------------------

# Presentation using gtsummary package
library(gtsummary)

# Independent t test
pc_data %>%
  mutate(
    Smoking = case_when(Smoking == "nonsmoker" ~ "Non-smoker",
                        Smoking == "smoker" ~ "Smoker"),
    Lung_cancer = case_when(Lung_cancer == "noncancer" ~ "Non-cancer",
                            Lung_cancer == "cancer" ~ "Cancer")
  ) %>% 
  select(-id) %>% 
  tbl_summary(
    label = list(Smoking ~ "Smoking status",
                 Lung_cancer ~ "Lung cancer"),
    by = Lung_cancer,
    digits = list(all_categorical() ~ c(0, 1))
  ) %>%
  add_n() %>%
  add_p(test = Smoking ~ "chisq.test.no.correct")