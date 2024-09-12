# Exercise: Day2_session01_exploring_aesthetics

# SETUP -------------------------------------------------------------------

# Library
library(tidyverse)
library(gagglr)

# Load data
df_path <- list.files("Data", pattern = "final-2024", full.names = T)

df_train <- read_csv(df_path) 

# Set core data frame for exercises and examples

df_tst_psnu <- df_train %>% 
  filter(result_of_test == "Reactive",
         art_init_period %ni% c("Other Years")) %>% 
  summarize(cumulative = n(), 
            .by = c("art_init_period","country","province","result_of_test")) %>% 
  rename(fy = art_init_period)


glimpse(df_tst_psnu)
names(df_tst_psnu)

# EXERCISE ----------------------------------------------------------------

# Instructions: Experiment with different aesthetic mappings 
# Using the starter code below, try different mappings for 
# size, shape, color, fill, and alpha 

?aes()

names(df_train)
glimpse(df_train)


# Starter columns
# provide columns for the aesthetic mapping below
df_tst_psnu %>%
  ggplot(mapping = aes(x = fy, y = cumulative, fill = country )) +
  geom_col()


# Starter points
# provide columns for the aesthetic mapping below
df_tst_psnu %>%
  ggplot(aes(x = fy, y = cumulative, 
             size = cumulative, 
             color = country,
             shape = country)) +
  geom_point()






