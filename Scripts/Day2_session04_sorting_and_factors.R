# Exercise: Day2_session04_sorting_and_factors

# SETUP -------------------------------------------------------------------

# Library
library(tidyverse)
library(gagglr)

# Load data
df_path <- list.files("Data", pattern = "final-2024", full.names = T)

df_train <- read_csv(df_path) 

# Set core data frame for exercises and examples

df_tst_psnu <- df_train %>% 
  filter(result_of_test == "Reactive") %>% 
  summarize(cumulative = n(), 
            .by = c("art_init_period","country","province","result_of_test")) %>% 
  rename(fy = art_init_period) %>% 
  filter(country == "A", 
         fy == "FY23Q2")


glimpse(df_tst_psnu)
names(df_tst_psnu)

# EXERCISE ----------------------------------------------------------------

# Instructions: Explore the data frame and review the help for fct_reorder
# Practice creating factors and sorting plots by them

# Review the help for fct_reorder
?fct_reorder
# What package is the function from? 
forcats::fct_reorder()

# Create a factor for the psnus, where the levels are mapped to cumulative results
df_tst_fct <- 
  df_tst_psnu %>% 
  mutate(prov_cmltv = fct_reorder(province,cumulative)) 

# How would you investigate the new data frame df_tst_fct?
glimpse(df_tst_fct)    


# Create a factor for the psnus, where the levels are mapped to cumulative results
# and create a column plot with the psnus plotted in order

df_tst_psnu %>% 
  mutate(prov_cmltv = fct_reorder(province, cumulative)) %>% 
  ggplot(aes(y = prov_cmltv, x = cumulative)) +
  geom_col()


# SORTING A FACETING VARIABLE
# Try creating a faceting variable to sort the provinces in order of cumulative results
# when using small multiples

df_tst_psnu %>% 
  mutate(prov_cmltv  = fct_reorder(province, cumulative, .desc = TRUE)) %>% 
  ggplot() +
  geom_col(aes(x = cumulative, y = result_of_test), width = 0.5, fill = scooter) + #fills by cumulative 
  facet_wrap(~ prov_cmltv, nrow = 5) + #sorts psnu in order of targets
  theme_minimal()
