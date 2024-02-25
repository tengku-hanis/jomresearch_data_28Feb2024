#========================================================================#
# Title: Cochran's Q test
# Author: Jom Research (https://jomresearch.netlify.app/)
# Date: Feb28, 2024
#========================================================================#

# Packages ----------------------------------------------------------------

library(tidyverse) #data manipulation
library(summarytools) #descriptive
library(rstatix) #cochran's q test


# Data --------------------------------------------------------------------

cq_data <- read.csv("data/cochran_q_exam.csv")


# Edit variable type ------------------------------------------------------

cq_data <- 
  cq_data %>% 
  mutate(Exam_method_default = as.factor(Exam_method_default),
         Exam_method_A = as.factor(Exam_method_A),
         Exam_method_B = as.factor(Exam_method_B),
         Exam_method_default = fct_recode(Exam_method_default, 
                                          fail = "0",
                                          pass = "1"),
         Exam_method_A = fct_recode(Exam_method_A, 
                                    fail = "0",
                                    pass = "1"),
         Exam_method_B = fct_recode(Exam_method_B, 
                                    fail = "0",
                                    pass = "1"))


# Explore -----------------------------------------------------------------

summary(cq_data)
freq(cq_data)

cq_data2 <- 
  cq_data %>% 
  pivot_longer(2:4, names_to = "Exam_method", values_to = "Result") 

ggplot(cq_data2, aes(Result)) +
  geom_bar() +
  facet_grid(rows = vars(Exam_method))


# Cochran's Q test --------------------------------------------------------

cochran_qtest(cq_data2, Result ~ Exam_method | id)

# Pairwise comparisons between groups
pairwise_mcnemar_test(cq_data2, Result ~ Exam_method | id)
