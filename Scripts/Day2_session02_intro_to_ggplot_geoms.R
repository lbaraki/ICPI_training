# Exercise: Day2_session02_exploring_geoms

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
# Using the starter code below, try applying different geoms 

?geom_line()

# columns
df_tst_psnu %>%
  ggplot(aes(x = fy, y = cumulative, fill = country)) +
  geom_col()



# points (scatterplot)
df_tst_psnu %>%
  ggplot(aes(x = fy, y = cumulative, color = country)) +
  geom_point()



# line (scatterplot)
df_tst_psnu %>%
  ggplot(aes(x = fy, y = cumulative, group = country)) +
  geom_line()



# Area (scatterplot)
df_tst_psnu %>%
  ggplot(aes(x = fy, y = cumulative, 
             group = country, fill = country)) +
  geom_area() +
  facet_wrap(~country)


# Try combining some of the aesthetics
df_tst_psnu %>% 
  ggplot(aes(x = fy, y = cumulative,
             group = country)) +
  geom_area(fill = "#d1d3d4", alpha = 0.75) +
  geom_line(linetype = "dotted", size = 0.5, color = "#c43d4d") + 
  geom_point(aes(size = cumulative)) + 
  facet_wrap(~ country) + si_style_ygrid()
