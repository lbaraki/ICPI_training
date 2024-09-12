# Exercise: Day2_session03_exploring_themes

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
  
  arrange(fy)


glimpse(df_tst_psnu)
names(df_tst_psnu)

# EXERCISE ----------------------------------------------------------------

# Instructions: Experiment with different theme settings 
# Using the starter code below, try adding different themes to plots
# What do you notice about the different themes?
# Try changing some of the theme arguments, what happens?

?theme_dark()

# Theme dark
df_tst_psnu %>% 
  ggplot(aes(x = fy, 
             y = cumulative)) +
  geom_col() + 
  facet_wrap(~country) +
  theme_gray() #gray grid

# Theme bw
df_tst_psnu %>% 
  ggplot(aes(x = fy, 
             y = cumulative)) +
  geom_col() + 
  facet_wrap(~country) +
  theme_bw() #white grid

# Theme SI style
df_tst_psnu %>% 
  ggplot(aes(x = fy, 
             y = cumulative)) +
  geom_col() + 
  facet_wrap(~country) +
  si_style() #clear grid 

# Theme SI ygrid
df_tst_psnu %>% 
  ggplot(aes(x = fy, 
             y = cumulative)) +
  geom_col() + 
  facet_wrap(~country) +
  si_style_ygrid() #y-axis grid lines only

# Theme SI xgrid
df_tst_psnu %>% 
  ggplot(aes(x = fy, 
             y = cumulative)) +
  geom_col() + 
  facet_wrap(~country) +
  si_style_xgrid() #x-axis grid lines only

# What does the help for theme() return?
?theme #showcases all the parameters 
