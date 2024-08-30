# Exercise: Day1_session03_data_inspection_isolation_exercise_2

# Project Setup & Data Prep -----------------------------------------------
library(tidyverse)
library(gagglr)

# Create a path to data
mock_path <- list.files("Data", pattern = "dataset", full.names = T)

# Load the data
df_mock <- read_excel(mock_path) %>% 
  janitor::clean_names()


# RENAMING ----------------------------------------------------------------

# Check the names of the data frame
names(df_mock)

# Rename the "no" column to "number"
df_mock %>% 
  rename("..." = "..." ) %>% 
  names("...")


# FILTERING ---------------------------------------------------------------

# Practice filtering the Data 

# Filter the result_of_test column to only "Reactive"

df_mock %>% 
  filter(result_of_test == "...") %>% 
  count("...")

# Try the same filter, but now filter the province to "Bangkok"
df_mock %>% 
  filter(result_of_test == "...",
         province == "...") %>% 
  count("...")


# ARRANGING ---------------------------------------------------------------

# Count the number of key populations in the dataset and arrange the 
# frequency in descending order  
df_mock %>% 
  count("...") %>% 
  arrange(desc("...")) #NA data category 

# Try arranging the results in ascending order
df_mock %>% 
  count("...") %>% 
  arrange("...")


# SELECTING ---------------------------------------------------------------

# Create a new data frame that only contains (select) the 
# variables province and facility_name
df_mock_facility <- 
  df_mock %>% 
  select(province, facility_name)

# Check the names of the columns in the df_msd_mech data frame
names(df_mock_facility)
