#========================================================================#
# Title: McNemar's test
# Author: Jom Research (https://jomresearch.netlify.app/)
# Date: Feb28, 2024
#========================================================================#

# Packages ----------------------------------------------------------------

library(tidyverse) #data manipulation
library(summarytools) #descriptive


# Data --------------------------------------------------------------------

mn_data <- read.csv("data/mcnemar_self_esteem.csv")


# Edit variable type ------------------------------------------------------

mn_data <- 
  mn_data %>% 
  mutate(Pre = as.factor(Pre),
         Post = as.factor(Post),
         Pre = fct_recode(Pre, 
                          low_self_esteem = "0",
                          high_self_esteem = "1"),
         Post = fct_recode(Post, 
                           low_self_esteem = "0",
                           high_self_esteem = "1"))


# Explore -----------------------------------------------------------------

summary(mn_data)
freq(mn_data)

ctable(mn_data$Pre, mn_data$Post)


# McNemar's test ----------------------------------------------------------

mcnemar.test(x = mn_data$Pre, y = mn_data$Post, correct = FALSE)


# More advance ------------------------------------------------------------

# Presentation using gtsummary package
library(gtsummary)

# McNemar's test
mn_data %>%
  mutate(
    Pre = case_when(Pre == "low_self_esteem" ~ "Low self-esteem",
                    Pre == "high_self_esteem" ~ "High self-esteem"),
    Post = case_when(Post == "low_self_esteem" ~ "Low self-esteem",
                     Post == "high_self_esteem" ~ "High self-esteem")
  ) %>% 
  select(-id) %>% 
  tbl_summary(
    by = Post,
    digits = list(all_categorical() ~ c(0, 1))
  ) %>%
  add_n() %>%
  add_p(test = Pre ~ "mcnemar.test.wide",
        test.args = Pre ~ list(correct = FALSE))