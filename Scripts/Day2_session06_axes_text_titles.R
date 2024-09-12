# Exercise: Day2_session06_axes_text_titles

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
  filter(country == "A", fy == "FY23Q2")

glimpse(df_tst_psnu)
names(df_tst_psnu)

# EXERCISE ----------------------------------------------------------------

# Instructions: Practice manipulating text on ggplot2 graphs

# Base plot
p <- df_tst_psnu %>% 
  mutate(prov_order = fct_reorder(province, cumulative)) %>% 
  ggplot(aes(y = prov_order, x = cumulative)) +
  geom_col()

# Print the base plot to the screen 
p

# Base plot -- add a subtitle
p + labs(subtitle = "subtitle")



# Modify the x and y axes
p + labs(x = "HTS_TST_POS", y = "PROVINCE")
p + scale_x_continuous(breaks = seq(0, 30, 10),
                       limits = c(0,30)) + 
  scale_y_discrete(labels = abbreviate, position = "right")

# Add a caption
p + labs(caption = "caption")


# Using the df_tst_psnu data frame, create a new plot
# Add in a meaningful title, caption, subtitle and axis labels
# Make sure any continuous variable axis texts use commas
df_tst_psnu %>% 
  mutate(prov_order = fct_reorder(province, cumulative)) %>%
  ggplot(aes(x = cumulative, y = prov_order)) +
  geom_col() +
  labs(x = "HTS TST POS", y = "PROVINCE", 
       title = "Chiang Rai Leads in Testing", 
       caption = "Source: Faux Training Data", 
       subtitle = "Followed by Bangkok") + 
  scale_x_continuous(labels = scales::comma, position = "top") + 
  scale_y_discrete(position = "right")
