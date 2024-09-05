# Exercise: Day1_session02_data_inspection_isolation_exercise_1

# Project Setup & Data Prep -----------------------------------------------
library(tidyverse)
library(gagglr)
library(readxl)

# Create a path to data
mock_path <- list.files("Data", pattern = "dataset", full.names = T)

# Load the data
df_mock <- read_excel(mock_path) %>% 
  janitor::clean_names()


# INVESTIGATING -----------------------------------------------------------

# Print the data frame to the screen
df_mock

# For the examples below, insert the correct dataframe in the function  

# head() -Returns the first or last parts of a vector, matrix, table, data frame or function.
head("...")
tail("...")


# names() - Functions to get or set the names of an object.
names("...")


# View() - Invoke a spreadsheet-style data viewer on a matrix-like R object.
view("...")


# str() - Compactly display the internal structure of an R object
str("...")

# glimpse() - glimpse() is like a transposed version of print(): columns run down the page, and data runs across. 
glimpse("...")


# Summary - What does this do? How to use it?


# UNIQUENESS --------------------------------------------------------------

# Check for unique observations using distinct, unique or count
# country, province, type_of_testing_modality
unique("...")

df_mock %>% 
  distinct("...")

df_mock %>% 
  count("...") %>% 
  prinf(n = Inf)

#How many unique provinces are there?

#How many unique testing modalities are there?

#What is different about the three functions (unique, distinct, and count)?


