#========================================================================#
# Title: Cochran-Mantel-Haenszel test
# Author: Jom Research (https://jomresearch.netlify.app/)
# Date: Feb28, 2024
#========================================================================#

# Packages ----------------------------------------------------------------

library(tidyverse) #data manipulation
library(summarytools) #descriptive
library(vcd) #woolf-test


# Data --------------------------------------------------------------------

cmh_data <- read.csv("data/mantel_heart_attack.csv")


# Edit variable type ------------------------------------------------------

cmh_data <- 
  cmh_data %>% 
  mutate(Obese = as.factor(Obese),
         Heart_attack = as.factor(Heart_attack),
         Age_group = as.factor(Age_group),
         Obese = fct_recode(Obese, 
                            obese = "1",
                            non_obese = "2"),
         Heart_attack = fct_recode(Heart_attack, 
                                   no = "0",
                                   yes = "1"),
         Age_group = fct_recode(Age_group,
                                below50 = "0",
                                above50 = "1"),
         Heart_attack = relevel(Heart_attack, ref = "yes"))


# Explore -----------------------------------------------------------------

# Descriptive
summary(cmh_data)
freq(cmh_data)

# Plot
cmh_data %>% 
  group_by(Obese, Heart_attack, Age_group) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(Obese, count)) +
  geom_col() +
  facet_grid(rows = vars(Age_group), cols = vars(Heart_attack)) 

# Change data structure
cmh_data2 <- xtabs(~ Obese + Heart_attack + Age_group, data = cmh_data) 
cmh_data2


# Assumptions -------------------------------------------------------------

## Homogenous OR across the strata ----
woolf_test(cmh_data2)


# Cochran-Mantel-Haenszel test --------------------------------------------

mantelhaen.test(cmh_data2)

