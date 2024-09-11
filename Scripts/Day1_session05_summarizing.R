# Exercise: Day1_session05_summarizing

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
  rename(kp_at_diagnosis = self_reported_kp_at_diagnosis) %>% 
  mutate(kp_status = case_when(
    kp_at_diagnosis %in% c("FSW", "TG", "Prisoner", "MSW",
                           "TWG", "PWID", "MSM") ~ "Key Population", 
    TRUE ~ "General Population"
  ))




# EXERCISE ----------------------------------------------------------------

# Determine which population group experienced the most positives in country A
# use filter(), summarize(), and arrange()
names(df_train)

df_train %>% 
  filter(country == "") %>% 
  group_by("", "***") %>% 
  summarise(cumulative = n(), .groups = "drop") %>% 
  arrange(cumulative)



