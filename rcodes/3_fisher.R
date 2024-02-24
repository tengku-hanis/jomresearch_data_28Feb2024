#========================================================================#
# Title: Fisher's exact test
# Author: Jom Research (https://jomresearch.netlify.app/)
# Date: Feb28, 2024
#========================================================================#

# Packages ----------------------------------------------------------------

library(tidyverse) #data manipulation
library(summarytools) #descriptive


# Data --------------------------------------------------------------------

fe_data <- read.csv("data/fisher_vaping.csv")


# Edit variable type ------------------------------------------------------

fe_data <- 
  fe_data %>% 
  mutate(Gender = as.factor(Gender),
         Vaping = as.factor(Vaping),
         Gender = fct_recode(Gender, 
                             female = "1",
                             male = "2"),
         Vaping = fct_recode(Vaping, 
                             no = "0",
                             yes = "1"))


# Explore -----------------------------------------------------------------

summary(fe_data)
freq(fe_data)
ctable(fe_data$Gender, fe_data$Vaping)


# Fisher's exact test -----------------------------------------------------

fe_mod <- chisq.test(x = fe_data$Gender, y = fe_data$Vaping, correct = FALSE)
fe_mod$expected 
#Assumption - expected count < 5 is 2 or >20%, so Fisher's exact is used

fisher.test(x = fe_data$Gender, y = fe_data$Vaping) #result


# More advance ------------------------------------------------------------

# Presentation using gtsummary package
library(gtsummary)

# Independent t test
fe_data %>%
  mutate(
    Gender = case_when(Gender == "female" ~ "Female",
                       Gender == "male" ~ "Male"),
    Vaping = case_when(Vaping == "no" ~ "Non-vaper",
                       Vaping == "yes" ~ "Vaper")
  ) %>% 
  select(-id) %>% 
  tbl_summary(
    by = Vaping,
    digits = list(all_categorical() ~ c(0, 1))
  ) %>%
  add_n() %>%
  add_p(test = Gender ~ "fisher.test")