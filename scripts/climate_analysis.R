## This script contains the climate analysis
## Andrew Cunliffe <andrewmcunliffe@gmail.com>
## Started 2021-04-15

# ------ 0 Setup Environment ----------

# Install packages
library(tidyverse) # For dplyr and ggplot2
library(viridis) # friendly colour palette
library(patchwork) # for multi-panel plots


#-------------- 1. Extract Data --------------

# NB. differences in these files prevent batch reading and merging. 

df_1988_1995 <- read_csv("data/meteorological_data/sev1_meteorology_1988-1995.txt")
df_1996_2000 <- read_csv("data/meteorological_data/sev1_meteorology_1996-2000.txt")

# NB. The header is missing from the 2001-2005 file, so column names were copied from the first file
col_names <- names(df_1988_1995)
df_2001_2005 <- read_csv("data/meteorological_data/sev1_meteorology_2001-2005.txt", col_names = col_names)

df_2006_2010 <- read_csv("data/meteorological_data/sev1_meteorology_2006-2010.txt")

# NB. This file contains no data in the 'Bar_Pressure column'?!
df_2011_2015 <- read_csv("data/meteorological_data/sev1_meteorology_2011-2015.txt")


# Remove the 'RecordID' column from the 2011-2015 file to facilitate merging with the other files.
df_2011_2015 <- df_2011_2015 %>% 
  select(-RecordID)

# Combine data
df <- rbind(df_1988_1995, df_1996_2000, df_2001_2005, df_2006_2010, df_2011_2015)

# 
rm(df_1988_1995, df_1996_2000, df_2001_2005, df_2006_2010, df_2011_2015)


# -------------- 2. Tidy Data --------------

# Replace error codes with NA values (where '-999' = missing values and '-888' = missing sensor)
df <- df %>%
  dplyr::na_if(-999) %>% 
  dplyr::na_if(-888)



# Select variables of interest
df2 <- df %>% 
  select(StationID,
         Year,
         Julian_Day,
         Hour,
         Temp_C, 
         Precipitation)



# -------------- 3. Analyze Data --------------


# The closest station is Five Points Meteorological Station (StationID = 49), but this only started in 1999.

AnnualPrecip_Station40 <- df2 %>% 
  filter(StationID == 40) %>% 
  group_by(Year) %>% 
  summarise(Precipitation = sum(Precipitation, na.rm = TRUE))

AnnualTemp_Station40 <- df2 %>% 
  filter(StationID == 40) %>%
  group_by(Year) %>% 
  summarise(Temp_C = mean(Temp_C, na.rm = TRUE))


plot(AnnualPrecip_Station40)
plot(AnnualTemp_Station40)




# Visualize with mean



# calculate IQR


# Add most recent data missing from 2016-2020