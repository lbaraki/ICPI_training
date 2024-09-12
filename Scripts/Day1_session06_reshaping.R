# Exercise: Day1_session06_reshaping_joining

# SETUP -------------------------------------------------------------------

# Library
library(tidyverse)
library(gagglr)

# Load data
df_path <- list.files("Data", pattern = "final-2024", full.names = T)

df_train <- read_csv(df_path) %>% 
  janitor::clean_names()

# Filter dataset to HTS_TST_POS

df_train <- df_train %>% 
  filter(result_of_test == "Reactive",
         kp_status == "Key Population") %>% 
  summarize(cumulative = n(), 
            .by = c("art_init_period","kp_status", "age_group_art_init","result_of_test")) %>% 
  arrange(art_init_period)



# EXERCISE ----------------------------------------------------------------

# Reshape the dataset wide so you get each quarter as its column

names(df_train)
glimpse(df_train)


df_train %>% 
  pivot_wider(names_from = art_init_period, 
              values_from = cumulative) %>% 
  select(-`Other Years`)






