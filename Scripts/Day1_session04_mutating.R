# Exercise: Day1_session04_mutating

# SETUP -------------------------------------------------------------------

# Library
library(tidyverse)
library(gagglr)

# Load data
df_path <- list.files("Data", pattern = "_final", full.names = T)

df_train <- read_excel(df_path) %>% 
  janitor::clean_names()

# Filter dataset to HTS_TST_POS
df_train <- df_train %>% 
  filter(result_of_test == "Reactive") %>% 
  rename(kp_at_diagnosis = self_reported_kp_at_diagnosis)




# EXERCISE ----------------------------------------------------------------

# Try running the following code and review the results. 
# What is the code trying to do?
# What do you notice is going on? Does it work?


#Create population variable for KP vs General 
df_train %>% distinct(kp_at_diagnosis)

df_train %>% 
  mutate(pop_type = case_when(
    kp_at_diagnosis %in% c("", "", "", "",
                           "", "", "") ~ "Key Population", 
    TRUE ~ "General Population"
  ))


#Create age groups in alingment with target age (15-24, 35-49, etc.)
  

  df_train <- df_train %>% filter(!is.na(date_of_art_initiation) & 
                      !(is.na(date_of_birth)))
  
  df_train %>% mutate(
    age_at_art_init = as.integer(difftime(date_of_art_initiation, date_of_birth, unit = "weeks")/52.25),
    age_group_art_init = case_when(
      age_at_art_init <1 ~ "<01",
      age_at_art_init >=1 & age_at_art_init <10 ~ "01-09",
      age_at_art_init >=10 & age_at_art_init <15 ~ "10-14",
      age_at_art_init >=15 & age_at_art_init <25 ~ "15-24",
      age_at_art_init >=25 & age_at_art_init <35 ~ "25-34",
      age_at_art_init >=35 & age_at_art_init <50 ~ "35-49",
      age_at_art_init >50 ~"50+")
  ) 
  
  
#Categorize period of ART initiation (aligns with FY22Q4 - FY23Q4)
  names(df_train)
  
  
  df_train %>%
    rename(date_art_init = date_of_art_initiation) %>% 
    mutate(
      art_init_period = case_when(
        date_art_init >= "2022-07-01" & date_art_init <= "2022-09-30"  ~ "FY22Q4",
        date_art_init >= "2022-10-01" & date_art_init <= "2022-12-31"  ~ "FY23Q1",
        date_art_init >= "2023-01-01" & date_art_init <= "2023-03-30"  ~ "FY23Q2",
        date_art_init >= "2023-04-01" & date_art_init <= "2023-06-30"  ~ "FY23Q3",
        date_art_init >= "2023-07-01" & date_art_init <= "2023-09-30"  ~ "FY23Q4",
        TRUE ~ "Other Years") )
  
  
  


