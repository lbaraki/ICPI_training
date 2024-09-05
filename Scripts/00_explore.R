# PROJECT: ICPI_training
# PURPOSE: Munge and Analysis of Training Data
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  4159a116
# LICENSE: MIT
# DATE:   2024-08-27
# NOTES:   

# LOCALS & SETUP ============================================================================

# Libraries
library(tidyverse)
library(gagglr)
library(glue)
library(scales)
library(systemfonts)
library(tidytext)
library(patchwork)
library(ggtext)
library(readxl)
library(glue)

# SI specific paths/functions  


# Grab metadata
get_metadata(file_path)

# REF ID for plots
ref_id <- "4159a116"

# Functions  


# LOAD DATA ============================================================================  

#Training data available for download from SharePoint
df_test <- read_excel("Data/Mock_dataset_final.xlsx") %>% 
  janitor::clean_names()

#Print data frame to screen
df_test


# INVESTIGATE ============================================================================

#Explore
head(df_test) #returns first parts of data frame 
tail(df_test) #returns last parts of data frame
names(df_test) #function to get set of names of an object
view(df_test) #invoke a spreadhseet-style data viewer on a matrix-like R object
str(df_test) 
glimpse(df_test)
summary(df_test)

#Uniqueness - check for unique observations using distinct, unique or count
unique(df_test$country)
unique(df_test$province)
unique(df_test$facility_name)

unique(df_test$self_reported_kp_at_diagnosis)
unique(df_test$self_reported_sex_at_diagnosis)
unique(df_test$self_reported_educational_level)
unique(df_test$type_of_testing_modality)
unique(df_test$result_of_test)
unique(df_test$art_bottles_prescribed)
unique(df_test$art_bottles_dispensed)

df_test %>% 
  distinct(self_reported_kp_at_diagnosis)

#How many unique provinces are there? 23

#How many unique testing modalities? 3 (VCT for A, vct for B?)


# MUNGE ============================================================================

names(df_test)
glimpse(df_test)

#Convert date format
df_clean <- df_test %>% 
  mutate(across(contains("date"), ~as_date(.))) 

#Rename variables
df_clean <- df_clean %>% 
  rename_with(~.x %>% 
                str_replace("^self_reported_", "") %>% 
                str_replace("^type_of_", "") %>% 
                str_replace("^last_viral_load_", "last_vl_")) %>% 
  rename(id = no, dob = date_of_birth, facility = facility_name,
         date_hiv_diag = date_of_diagnosis, date_art_init = date_of_art_initiation) 

#Review distinct clients and recode variables
df_clean %>% distinct(id) #52,648 distinct clients 
df_clean %>% count(country) #30k clients in country A and 22.6k in country B
df_clean %>% filter(country == "B") %>% count(country)
df_clean %>% distinct(province) %>% prinf() #23 provinces 
df_clean %>% distinct(sex_at_diagnosis) #Male, Female, Uknown, M, F
df_clean <- df_clean %>% 
    mutate(sex_at_diagnosis = case_when(
      sex_at_diagnosis == "M" ~ "Male",
      sex_at_diagnosis == "F" ~ "Female",
      TRUE ~ sex_at_diagnosis)) 

df_clean %>% distinct(result_of_test) #Reactive, No-reactive, Non-reactive, Indeterminate
df_clean <- 
  df_clean %>% 
    mutate(result_of_test = case_when(
      result_of_test == "No-Reactive" ~ "Non-reactive",
      TRUE ~ result_of_test
    )) 

df_clean %>% distinct(testing_modality)
df_clean <- 
  df_clean %>% 
  mutate(testing_modality = case_when(
    testing_modality == "VCT" ~ "vct",
    TRUE ~ testing_modality
  )) 

df_clean %>% distinct(educational_level)
df_clean <- df_clean %>% 
  mutate(educational_level = str_replace(educational_level, "^\\d+_", ""),
         educational_level = case_when(
           educational_level == "High school" ~ "High School",
    TRUE ~ educational_level
  ))

#Create population variable from kp information
  #7 categories - only 5 align w/ MER (MSM, TG, Prisoner, PWID, FSW vs MSW, TGW) 
df_clean %>% distinct(kp_at_diagnosis) 
df_clean <- df_clean %>% mutate(
  kp_status = case_when(
    kp_at_diagnosis %in% c("FSW", "TG", "PWID", "Prisoner",
                           "MSW", "MSM", "TGW") ~ "Key Population",
    TRUE ~ "General Population")) 
  

#Calculate age at diagnosis & ART initiation (align with targets ex. 15-24, 35-49)
df_clean <- df_clean %>% filter(!is.na(date_art_init) & !(is.na(dob))) #30427 of 52658

df_clean <- df_clean %>% mutate(
  age_at_art_init = as.integer(difftime(date_art_init, dob, unit = "weeks")/52.25),
  #age_group_art_init = if_else(age_at_art_init <15, "<15 years", "15+ years")
  age_group_art_init = case_when(
    age_at_art_init <1 ~ "<01",
    age_at_art_init >=1 & age_at_art_init <10 ~ "01-09",
    age_at_art_init >=10 & age_at_art_init <15 ~ "10-14",
    age_at_art_init >=15 & age_at_art_init <25 ~ "15-24",
    age_at_art_init >=25 & age_at_art_init <35 ~ "25-34",
    age_at_art_init >=35 & age_at_art_init <50 ~ "35-49",
    age_at_art_init >50 ~"50+")
  ) 
  

#df_clean <- df_clean %>% filter(!is.na(date_hiv_diag) & !(is.na(dob))) %>% count()

df_clean <- df_clean %>% mutate(
  age_at_hiv_diag = as.integer(difftime(date_hiv_diag, dob, unit = "weeks")/52.25),
  age_group_hiv_diag = case_when(
    age_at_hiv_diag <1 ~ "<01",
    age_at_hiv_diag >=1 & age_at_hiv_diag <10 ~ "01-09",
    age_at_hiv_diag >=10 & age_at_hiv_diag <15 ~ "10-14",
    age_at_hiv_diag >=15 & age_at_hiv_diag <25 ~ "15-24",
    age_at_hiv_diag >=25 & age_at_hiv_diag <35 ~ "25-34",
    age_at_hiv_diag >=35 & age_at_hiv_diag <50 ~ "35-49",
    age_at_hiv_diag >50 ~"50+")
) #%>% select(dob, date_hiv_diag, date_art_init, age_at_hiv_diag, age_group_hiv_diag) %>% View()


df_clean %>%
  write_csv("Data/mock-dataset-clean-2024.csv")

# VIZ ============================================================================
#Subset data to past 12 months 

max_date <- df_clean %>% 
  summarise(max_date = max(date_hiv_diag, na.rm = TRUE)) %>% 
  pull(max_date) 

min_date <- df_subset - months(12)

df_viz <- df_clean %>%
  filter(date_hiv_diag >= min_date & date_hiv_diag <= max_date)

#In last 12 months, proportion of clients diagnosed HIV+ are KP vs Gen Pop? 
  #compare positivity across populations 

  df_viz %>% 
  filter(result_of_test == "Reactive") %>% 
  count(result_of_test, kp_status, sort = TRUE) #KP: 15931 vs GP: 86
  
  df_viz %>% 
    filter(result_of_test == "Reactive") %>% 
    count(kp_status, name = "hts_pos") %>% 
    mutate(proportion = hts_pos/sum(hts_pos),
           benchmark = 1) %>% 
    arrange(desc(proportion)) %>% 
    ggplot(aes(x = proportion, y = kp_status)) + 
    geom_col(aes(x = benchmark), fill = grey20k, alpha = 0.5) + 
    geom_col(aes(fill = kp_status)) + 
    geom_vline(xintercept = 1, color = grey40k, linetype = "dotted") + 
    geom_text(aes(label = percent(proportion, 1)), 
              hjust = -.10) +
    #facet_wrap(~country) + 
    scale_x_continuous(labels = percent) + 
    scale_fill_si(palette = "old_rose", discrete = T) + 
    si_style_xgrid() + 
    theme(legend.position = "none") + 
    labs(x = NULL, y = NULL,
         title = "KEY POPULATIONS LEAD IN POSITIVIE CASES",
         #subtitle = "Proportion of HTS_TST_POS for 12 months period",
         caption = "Source: Mock training 2024") 
  
  #compare positivity across KP groups 
  df_viz %>% 
    filter(result_of_test == "Reactive",
           !is.na(kp_at_diagnosis)) %>% 
    count(kp_at_diagnosis, name = "hts_pos") %>% 
    mutate(proportion = hts_pos/sum(hts_pos),
           benchmark = 1,
           kp_at_diagnosis = fct_reorder(kp_at_diagnosis, proportion)) %>% 
    #arrange(desc(proportion)) %>% 
    ggplot(aes(x = proportion, y = kp_at_diagnosis)) + 
    geom_col(aes(x = benchmark), fill = grey20k, alpha = 0.5) + 
    geom_col(aes(fill = kp_at_diagnosis)) + 
    geom_vline(xintercept = 1, color = grey40k, linetype = "dotted") + 
    geom_text(aes(label = percent(proportion, 1)), 
              hjust = -.10) +
    #facet_wrap(~country) + 
    scale_x_continuous(labels = percent) + 
    scale_fill_si(palette = "old_rose", discrete = T) + 
    si_style_xgrid() + 
    theme(legend.position = "none") + 
    labs(x = NULL, y = NULL,
         title = "MSM LEAD IN POSITIVIE CASES AMONGST KP GROUPS",
         #subtitle = "Proportion of HTS_TST_POS for 12 months period",
         caption = "Source: Mock training 2024") 
  
