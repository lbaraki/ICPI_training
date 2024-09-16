# Exercise: Day2_session07_sf_exercises


# SETUP -------------------------------------------------------------------

# Library
library(tidyverse)
library(gagglr)
library(scales)
library(sf)
library(gisr)


# Data - Make sure to downlaod `MER_Structured_Datasets_PSNU_IM_FY22-25.*South and Southeast Asia Region.zip` file and move it to `Data` folder

# GIS - Make sure the GIS folder has the proper shapefiles
# Load PSNU x IM data

df_msd <- return_latest("Data", "PSNU_IM.FY22-25.*South") %>%
  read_psd() %>% 
  filter(snu1 == "Thailand")

# Load sf data
psnu_sf <- st_read("GIS/VcPepfarPolygons_20230524/VcPepfarPolygons.shp")

# Set core data frame for exercises and examples
# Sequence of operations
# 1) Filter HTS POS indicator and fiscal year
# 2) summarize targets and cumulative results by fiscal year, psnu, snu1 and indicator
# 3) Create a new variable (fy) that transform fiscal year variable as character type
df_tst_psnu <- df_msd %>%
  filter(indicator == "HTS_TST_POS",
         standardizeddisaggregate == "Total Numerator") %>%
  group_by(fiscal_year, snu1, snu1uid, psnu, psnuuid, cop22_psnu, cop22_psnuuid, indicator) %>% 
  summarize(across(c(targets, cumulative), 
                   \(x) sum(x, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(fy = as.character(fiscal_year)) 

# Take a look at the structure of the spatial data
glimpse(psnu_sf)
glimpse(fac_sf)
names(psnu_sf)   


# EXERCISE ----------------------------------------------------------------

#Instruction: Join the sf data frame to the testing (df_tst_psnu) data frame
# Using a left_join with the sf data frame as the base
names(psnu_sf)

uid_df <- psnu_sf%>% 
  rename(cop22_psnuuid = uid) #join on cop22_psnu/province level
  #rename(psnuuid = uid) #join on psnu level 

df_psnu_tst_geo <- uid_df %>% 
  left_join(., df_tst_psnu) %>% 
  filter(psnu == "Thailand")

# Try making map of the testing targets
df_psnu_tst_geo %>% 
  ggplot() +
  geom_sf(aes(fill = targets)) +
  labs(title = "Testing Targets") 

# Try faceting the same map above by fiscal year
df_psnu_tst_geo %>% 
  ggplot() +
  geom_sf(aes(fill = targets)) +
  labs(title = "TBD") +
  facet_wrap(~ fiscal_year)


# Review the help sections for geom_sf() and geom_sf_label()
?geom_sf
?geom_sf_label  


#Practice
  #Call in the sf package via library function 
  library(sf)

  #Load the snu1 shapefile from the GIS folder
  #Filter to the country of interest
  snu1_df <- st_read("GIS/pepfar_countries.shp")  %>% 
    filter(country == "Thailand")
  
  snu1_df %>% 
    ggplot() +
    geom_sf(aes(geometry = geometry, fill = country),
            color = grey90k) + 
    labs(title = "PSNU MAP CREATED BY SF") + 
    si_style_map() + 
    theme(legend.text = element_text(size = 7))
  
  df_psnu_tst_geo %>% 
    arrange(desc(cumulative)) %>% slice(1:15) %>% 
    ggplot() + 
    geom_sf(aes(fill = cumulative), 
            color = "white", size = 1) + 
    geom_sf_label(aes(label = cop22_psnu),
                  size = 7/.pt,
                  nudge_x = 0.3,
                  nudge_y = 0.5) + 
    scale_fill_viridis_c(direction = -1) + 
    si_style_map() + 
    si_legend_fill() +
    labs(x = NULL, y = NULL, 
         title = "BANGKOK WITH THE LARGEST TESTING RESULTS")


