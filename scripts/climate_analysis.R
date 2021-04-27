## This script contains the climate analysis
## Andrew Cunliffe <andrewmcunliffe@gmail.com>
## Started 2021-04-15
## Using data sourced from https://sevlter.unm.edu/content/meteorology-data-sevilleta-national-wildlife-refuge-new-mexico-1988-present


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

# The longest running station is StationID = 40, which has been running since mid 1987.

AnnualPrecip_Station40 <- df2 %>% 
  filter(StationID == 40) %>% 
  group_by(Year) %>% 
  summarise(Precipitation = sum(Precipitation, na.rm = TRUE))

AnnualTemp_Station40 <- df2 %>% 
  filter(StationID == 40) %>%
  group_by(Year) %>% 
  summarise(Temp_C = mean(Temp_C, na.rm = TRUE))


# The closest station is StationID = 49 (Five Points Meteorological Station but this only started in 1999.
AnnualPrecip_Station49 <- df2 %>% 
  filter(StationID == 49) %>% 
  group_by(Year) %>% 
  summarise(Precipitation = sum(Precipitation, na.rm = TRUE))

AnnualTemp_Station49 <- df2 %>% 
  filter(StationID == 49) %>%
  group_by(Year) %>% 
  summarise(Temp_C = mean(Temp_C, na.rm = TRUE))


### summarise Mean Annual Precipitation
(mean_precip_ID40 <- mean(AnnualPrecip_Station40$Precipitation))
(mean_precip_ID49 <- mean(AnnualPrecip_Station49$Precipitation))

### summarise Mean Annual Temperature 
(mean_temp_ID40 <- mean(AnnualTemp_Station40$Temp_C))
(mean_temp_ID49 <- mean(AnnualTemp_Station49$Temp_C))

### Station 49 is ~0.8 degrees warmer and receives ~15 mm more precipitation than Station 40... (over different periods though!)


# calculate IQR
(iqr_ID40 <- IQR(AnnualPrecip_Station40$Precipitation))
(iqr_ID49 <- IQR(AnnualPrecip_Station49$Precipitation))




# -------------- 4. Visualize Data --------------

(precip_ID40 <- ggplot(AnnualPrecip_Station40, aes(x=Year, y=Precipitation)) +
    labs(x = "Year", y = "Annual Precipitation (mm)", title = "Station 40") +
    geom_point(aes(x=Year, y=Precipitation)) +
    geom_line(aes(y = mean_precip_ID40)) +
    geom_line(aes(y = (mean_precip_ID40-(iqr_ID40/2))), linetype = "dashed") +
    geom_line(aes(y = (mean_precip_ID40+(iqr_ID40/2))), linetype = "dashed") +
    coord_cartesian(ylim=c(0, 550)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  )
  

(precip_ID49 <- ggplot(AnnualPrecip_Station49, aes(x=Year, y=Precipitation)) +
    labs(x = "Year", y = "Annual Precipitation (mm)", title = "Station 49") +
    geom_point(aes(x=Year, y=Precipitation)) +
    geom_line(aes(y = mean_precip_ID49)) +
    geom_line(aes(y = (mean_precip_ID49-(iqr_ID49/2))), linetype = "dashed") +
    geom_line(aes(y = (mean_precip_ID49+(iqr_ID49/2))), linetype = "dashed") +
    coord_cartesian(ylim=c(0, 550)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  )

#### Outstanding Questions ####
# missing years of data? I should have 2010-2015-

# Where exactly is station 40


# Possible to align years with our years?


# Add most recent data missing from 2016-2020 - ask Kris Hall Information Manager - and also share about missing header on other met file.