# Exercise: Day2_session05_applying_color

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
  mutate(target = case_when(
    country == "A" ~ cumulative *2, 
    country == "B" ~ cumulative * 0.5
    ))


glimpse(df_tst_psnu)
names(df_tst_psnu)

# EXERCISE ----------------------------------------------------------------

# Instructions: Practice applying color to different types of plots

df_tst_psnu %>% 
  ggplot(aes(y = country, x = cumulative, 
             fill = country)) +
  geom_col()


# Complete the code below.
# Create a new variable that takes the the following values
# for country B --> lightblue
# for all other countries "grey50"
# Use the new variable to apply a fill to the column graph
df_tst_psnu %>% 
  mutate(country_fill = case_when(
    country %in% c("B") ~ usaid_lightblue,
    TRUE ~ grey50k,
  )) %>% 
  ggplot(aes(y = country, x = cumulative, 
             fill = country_fill)) +
  geom_col() +
  scale_fill_identity()



# Shapes and color
# Using the slide on color (shapes 0-20) or fill (shapes 21-25), add the correct aesthetic value

# color or fill
df_tst_psnu %>% 
  ggplot(aes(y = target, x = cumulative)) +
  geom_point(aes(color = country), shape = 15, size = 5)

# color or fill
df_tst_psnu %>% 
  ggplot(aes(y = target, x = cumulative)) +
  geom_point(aes(color = country), shape = 8, size = 5)

# color or fill
df_tst_psnu %>% 
  ggplot(aes(y = target, x = cumulative)) +
  geom_point(aes(fill = country), shape = 21, size = 5)

# color or fill
df_tst_psnu %>% 
  ggplot(aes(y = target, x = cumulative)) +
  geom_point(aes(fill = country), shape = 25, size = 7,   color = "black", stroke = 1)

# Experiment with your own plots, using color to fill in bars, areas or points


# Examples of continuous mapping of color
df_tst_psnu %>% 
  ggplot(aes(y = target, x = cumulative)) +
  geom_point(aes(fill = cumulative), shape = 25, size = 7, stroke = 1) +
  scale_fill_viridis_c()

df_tst_psnu %>% 
  pivot_longer(cols = c(target, cumulative),
               values_to = "value",
               names_to = "type") %>% 
  ggplot(aes(y = type, x = country)) +
  geom_tile(aes(fill = value), color = "white") +
  scale_fill_viridis_c(option = "C")
