## This script contains the climate analysis
## Andrew Cunliffe <andrewmcunliffe@gmail.com>
## Started 2021-04-15



# ------ 0 Setup Environment ----------

## Install packages
library(tidyverse) # For dplyr and ggplot2
library(viridis) # friendly colour palette
library(patchwork) # for multi-panel plots


#-------------- 1. Extract Data --------------
## Using data sourced from Moore, D.I. 2021. Meteorology Data from the Sevilleta 
## National Wildlife Refuge, New Mexico ver 14. Environmental Data Initiative. 
## https://doi.org/10.6073/pasta/1cbc37ae4d40b3844b5e4be9f6f18073 
## (Accessed 2021-04-28).

## For more information see: https://unmsevilletafieldstation.wordpress.com/met-station-network/

## Read data
df_1988_1994 <- read_csv("data/meteorological_data/Sevilleta_LTER_Hourly_Meteorological_Data_1988_1994.csv",
                         col_types = cols(Evaporation = col_double(),
                                          Evap_Pan_Temperature = col_double(),
                                          Bar_Pressure = col_double()))

df_1995_1999 <- read_csv("data/meteorological_data/Sevilleta_LTER_Hourly_Meteorological_Data_1995_1999.csv",
                         col_types = cols(Moisture_10_cm = col_double(),
                                          Moisture_30_cm  = col_double(),
                                          Evaporation = col_double(),
                                          Evap_Pan_Temperature  = col_double()))

df_2000_2004 <- read_csv("data/meteorological_data/Sevilleta_LTER_Hourly_Meteorological_Data_2000_2004.csv",
                         col_types = cols(Moisture_10_cm = col_double(),
                                          Moisture_30_cm  = col_double(),
                                          Evaporation = col_double(),
                                          Evap_Pan_Temperature  = col_double()))

df_2005_2009 <- read_csv("data/meteorological_data/Sevilleta_LTER_Hourly_Meteorological_Data_2005_2009.csv",
                         col_types = cols(Moisture_10_cm = col_double(),
                                          Moisture_30_cm  = col_double(),
                                          Evaporation = col_double(),
                                          Evap_Pan_Temperature  = col_double()))

df_2010_2014 <- read_csv("data/meteorological_data/Sevilleta_LTER_Hourly_Meteorological_Data_2010_2014.csv",
                         col_types = cols(Moisture_10_cm = col_double(),
                                          Moisture_30_cm  = col_double(),
                                          Evaporation = col_double(),
                                          Evap_Pan_Temperature  = col_double()))


df_2015_2019 <- read_csv("data/meteorological_data/Sevilleta_LTER_Hourly_Meteorological_Data_2015_2019.csv",
                         col_types = cols(Moisture_10_cm = col_double(),
                                          Moisture_30_cm  = col_double(),
                                          Evaporation = col_double(),
                                          Evap_Pan_Temperature  = col_double()))

df_2020 <- read_csv("data/meteorological_data/Sevilleta_LTER_Hourly_Meteorological_Data_2020.csv",
                    col_types = cols(Moisture_10_cm = col_double(),
                                     Moisture_30_cm  = col_double(),
                                     Evaporation = col_double(),
                                     Evap_Pan_Temperature  = col_double()))

# Annual summaries extracted from Fluxnet files (as part of JULES prep. script)
AnnualPrecip_Seg <- read_csv("data/meteorological_data/annual_precip_Seg.csv")
AnnualPrecip_Sen <- read_csv("data/meteorological_data/annual_precip_Sen.csv")
AnnualPrecip_Ses <- read_csv("data/meteorological_data/annual_precip_Ses.csv")


## Combine data
df <- rbind(df_1988_1994, df_1995_1999, df_2000_2004, df_2005_2009, df_2010_2014, df_2015_2019, df_2020)

rm(df_1988_1994, df_1995_1999, df_2000_2004, df_2005_2009, df_2010_2014, df_2015_2019, df_2020)  # Remove unnecessary objects 



# -------------- 2. Tidy Data --------------
## Select variables of interest
df2 <- df %>% 
  select(StationID,
         Date_Time,
         Year,
         Julian_Day,
         Hour,
         Temp_C, 
         Precipitation
         )



# -------------- 3. Analyze Data --------------

## StationID = 40 (Deep Well) is the longest running station, active since mid 1987.
## Station 40 is within a few km of our study sites and we consider it representative for annual totals. 


### Extract annual statistics ###
AnnualPrecip_Station40 <- df2 %>% 
  filter(StationID == 40) %>% 
  group_by(Year) %>% 
  summarise(Precipitation = sum(Precipitation, na.rm = TRUE))

AnnualTemp_Station40 <- df2 %>% 
  filter(StationID == 40) %>%
  group_by(Year) %>% 
  summarise(Temp_C = mean(Temp_C, na.rm = TRUE))


# StationID = 49 (Five Points Meteorological Station) is closer to the Ses site but only started in 1999.
AnnualPrecip_Station49 <- df2 %>% 
  filter(StationID == 49) %>% 
  group_by(Year) %>% 
  summarise(Precipitation = sum(Precipitation, na.rm = TRUE))

AnnualTemp_Station49 <- df2 %>% 
  filter(StationID == 49) %>%
  group_by(Year) %>% 
  summarise(Temp_C = mean(Temp_C, na.rm = TRUE))


### Add Station ID to precipitation records
AnnualPrecip_Station40 <- AnnualPrecip_Station40 %>% 
  mutate(Station = "Station40")

AnnualPrecip_Station49 <- AnnualPrecip_Station49 %>% 
  mutate(Station = "Station49")

AnnualPrecip_Seg <- AnnualPrecip_Seg %>% 
  mutate(Station = "Seg") %>% 
  rename(Year = year)

AnnualPrecip_Sen <- AnnualPrecip_Sen %>% 
  mutate(Station = "Sen") %>% 
  rename(Year = year)

AnnualPrecip_Ses <- AnnualPrecip_Ses %>% 
  mutate(Station = "Ses") %>% 
  rename(Year = year)


### Extract mean annual statistics ###

## Mean Annual Precipitation
(mean_precip_ID40 <- mean(AnnualPrecip_Station40$Precipitation))
(mean_precip_ID49 <- mean(AnnualPrecip_Station49$Precipitation))

## Mean Annual Temperature 
(mean_temp_ID40 <- mean(AnnualTemp_Station40$Temp_C))
(mean_temp_ID49 <- mean(AnnualTemp_Station49$Temp_C))

## Calculate IQR
(iqr_ID40 <- IQR(AnnualPrecip_Station40$Precipitation))
(iqr_ID49 <- IQR(AnnualPrecip_Station49$Precipitation))

## Calculate Coefficient of Variation
(CoV_ID40 <- sd(AnnualPrecip_Station40$Precipitation)/mean_precip_ID40)
(CoV_ID49 <- sd(AnnualPrecip_Station49$Precipitation)/mean_precip_ID49)


# -------------- 4. Visualize Data --------------


# (precip_ID40 <- ggplot(AnnualPrecip_Station40, aes(x=Year, y=Precipitation)) +
#    labs(x = "Year", y = "Annual Precipitation (mm)", title = "Precipitation at Station 40") +
#    geom_point(aes(x=Year, y=Precipitation)) +
#    geom_line(aes(y = mean_precip_ID40)) +
#    geom_line(aes(y = (mean_precip_ID40-(iqr_ID40/2))), linetype = "dashed") +
#    geom_line(aes(y = (mean_precip_ID40+(iqr_ID40/2))), linetype = "dashed") +
#    geom_point(aes(x=2019, y=517, colour="Red")) +
#    geom_point(aes(x=2019, y=458, colour="blue")) +
#    coord_cartesian(xlim=c(1988,2020), ylim=c(0, 550)) +
#    theme_bw() +
#    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# )

# (precip_ID40 <- ggplot(AnnualPrecip_Station40, aes(x=Year, y=Precipitation)) +
#    labs(title = "Precipitation at Station 40") +
#    geom_point(aes(x=Year, y=Precipitation)) +
#     geom_line(aes(y = mean_precip_ID40)) +
#     geom_line(aes(y = (mean_precip_ID40-(iqr_ID40/2))), linetype = "dashed") +
#     geom_line(aes(y = (mean_precip_ID40+(iqr_ID40/2))), linetype = "dashed") +
#    geom_point(aes(x=2019, y=517, colour="Red")) +
#    geom_point(aes(x=2019, y=458, colour="blue")) +
#    scale_x_continuous(name="Year", breaks=seq(1990,2020,10), limits=c(1988,2020)) +
#    scale_y_continuous(name="Annual Precipitation (mm)", breaks=seq(0,550,50), limits=c(0, 550)) +
#    theme_bw() +
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#   )
#   
# ggsave("plots/Annual precipitation at Station 40.png", 
#        precip_ID40,
#        width=12,
#        height=12,
#        units="cm")


# (precip_ID49 <- ggplot(AnnualPrecip_Station49, aes(x=Year, y=Precipitation)) +
#     labs(title = "Precipitation at Station 49") +
#     geom_point(aes(x=Year, y=Precipitation)) +
#     geom_line(aes(y = mean_precip_ID49)) +
#     geom_line(aes(y = (mean_precip_ID49-(iqr_ID40/2))), linetype = "dashed") +
#     geom_line(aes(y = (mean_precip_ID49+(iqr_ID40/2))), linetype = "dashed") +
#     geom_point(aes(x=2019, y=517, colour="Red")) +
#     geom_point(aes(x=2019, y=458, colour="blue")) +
#     scale_x_continuous(name="Year", breaks=seq(1990,2020,10), limits=c(1988,2020)) +
#     scale_y_continuous(name="Annual Precipitation (mm)", breaks=seq(0,550,50), limits=c(0, 550)) +
#     theme_bw() +
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# )
# 
# ggsave("plots/Annual precipitation at Station 49.png", 
#        precip_ID49,
#        width=12,
#        height=12,
#        units="cm")


## Manually add our stated precipitation totals
# Stated_Seg <- data.frame(Year=2019, Precipitation=179, Station="Seg_Study_Period")
# Stated_Ses <- data.frame(Year=2019, Precipitation=171, Station="Ses_Study_Period")


### Combine annual precipitation records from all stations
AnnualPrecip <- rbind(AnnualPrecip_Station40,
                      AnnualPrecip_Station49,
                      AnnualPrecip_Seg,
                      AnnualPrecip_Sen,
                      AnnualPrecip_Ses
                      )


## Plot annual precipitation from all stations
(AnnualPrecip_plots <- ggplot(AnnualPrecip, aes(x=Year, y=Precipitation, group=Station)) +
    labs(title = "Annual precipitation at different stations") +
    geom_point(aes(x=Year, y=Precipitation, colour=Station)) +
    geom_hline(yintercept = mean_precip_ID40) +
    geom_hline(yintercept = (mean_precip_ID40+(iqr_ID40/2)), linetype = "dotted") +
    geom_hline(yintercept = (mean_precip_ID40-(iqr_ID40/2)), linetype = "dotted") +
    scale_colour_viridis_d(option = "plasma") +
    scale_x_continuous(name="Year", breaks=seq(1990,2020,10), limits=c(1988,2020)) +
    scale_y_continuous(name="Annual Precipitation (mm)", breaks=seq(0,350,50), limits=c(0, 350)) +
    # Add annotation for study periods
    geom_text(x=2024.2, y=179, label="Seg Study Period", color="Red", size=2.1, fontface="bold") +
    geom_text(x=2024.2, y=171, label="Ses Study Period", color="Red", size=2.1, fontface="bold") +
    theme_bw() +
    theme(plot.margin = unit(c(1, 4, 1, 1), "lines")) +
    coord_cartesian(clip = "off") +
    theme(legend.position = "bottom") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)

ggsave("plots/Annual precipitation.png", 
       AnnualPrecip_plots,
       width=16,
       height=12,
       units="cm")


# -------------- 5. Analysis of Potential Evapotranspiration --------------

