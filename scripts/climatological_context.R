## This script contains the climate analysis
## Andrew Cunliffe <andrewmcunliffe@gmail.com>
## Started 2021-04-15



# ------ 0 Setup Environment ----------

## Install packages
library(tidyverse) # For dplyr and ggplot2
library(viridis) # friendly colour palette
library(patchwork) # for multi-panel plots
library(scales) # for colour review


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
df2 <- df %>% 
  mutate(month = lubridate::month(Date)) %>%
  rename(year = Year) %>% 
  select(StationID,
         Date_Time,
         year,
         month,
         Julian_Day,
         Hour,
         Temp_C, 
         Precipitation,
         Relative_Humidity
  )



# -------------- 3. Analyze Data --------------

## StationID = 40 (Deep Well) is the longest running station, active since mid 1987.
## Station 40 is within a few km of our study sites and we consider it representative for annual totals. 


### Extract annual statistics ###
AnnualPrecip_Station40 <- df2 %>% 
  filter(StationID == 40) %>% 
  group_by(year) %>% 
  summarise(Precipitation = sum(Precipitation, na.rm = TRUE))

AnnualTemp_Station40 <- df2 %>% 
  filter(StationID == 40) %>%
  group_by(year) %>% 
  summarise(Temp_C = mean(Temp_C, na.rm = TRUE))


# StationID = 49 (Five Points Meteorological Station) is closer to the Ses site but only started in 1999.
AnnualPrecip_Station49 <- df2 %>% 
  filter(StationID == 49) %>% 
  group_by(year) %>% 
  summarise(Precipitation = sum(Precipitation, na.rm = TRUE))

AnnualTemp_Station49 <- df2 %>% 
  filter(StationID == 49) %>%
  group_by(year) %>% 
  summarise(Temp_C = mean(Temp_C, na.rm = TRUE))


### Add Station ID to precipitation records
AnnualPrecip_Station40 <- AnnualPrecip_Station40 %>% 
  mutate(Station = "Station40")

AnnualPrecip_Station49 <- AnnualPrecip_Station49 %>% 
  mutate(Station = "Station49")

AnnualPrecip_Seg <- AnnualPrecip_Seg %>% 
  mutate(Station = "Seg") %>% 
  rename(year = year)

AnnualPrecip_Sen <- AnnualPrecip_Sen %>% 
  mutate(Station = "Sen") %>% 
  rename(year = year)

AnnualPrecip_Ses <- AnnualPrecip_Ses %>% 
  mutate(Station = "Ses") %>% 
  rename(year = year)


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

### Comparison of total annual precipitation ###

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
scales::show_col(viridis_pal(option = "viridis")(5)) # review colour options

(AnnualPrecip_plots <- ggplot(AnnualPrecip, aes(x=year, y=Precipitation, group=Station)) +
    labs(title = "Annual precipitation at different stations") +
    geom_point(aes(x=year, y=Precipitation, colour=Station)) +
    geom_hline(yintercept = mean_precip_ID40, colour="#5DC863FF") +
    geom_hline(yintercept = (mean_precip_ID40+(iqr_ID40/2)), linetype = "dotted", colour="#5DC863FF") +
    geom_hline(yintercept = (mean_precip_ID40-(iqr_ID40/2)), linetype = "dotted", colour="#5DC863FF") +
    scale_colour_viridis_d(option = "viridis") +
    scale_x_continuous(name="Year", breaks=seq(1990,2020,10), limits=c(1988,2020)) +
    scale_y_continuous(name="Annual Precipitation (mm)", breaks=seq(0,350,50), limits=c(0, 350)) +
    # Add annotation for study periods
    geom_text(x=2024.2, y=197, label="Seg Study Period", colour="#440154FF", size=2.1, fontface="bold") +
    geom_text(x=2024.2, y=171, label="Ses Study Period", colour="#21908CFF", size=2.1, fontface="bold") +
    theme_bw() +
    theme(plot.margin = unit(c(1, 4, 1, 1), "lines")) +
    coord_cartesian(clip = "off") +
    theme(legend.position = "bottom") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)

ggsave("plots/Climatological Context/Annual precipitation.png", 
       AnnualPrecip_plots,
       width=16,
       height=12,
       units="cm")


### 4.2 -  precipitation seasonality comparison ####

# Calculate mean monthly precipitation
MonthlyPrecip_Station40 <- df2 %>% 
  filter(StationID == 40) %>%  # Filter to Station 40
  group_by(year, month) %>% # Group by year and month
  summarise(Precipitation = sum(Precipitation, na.rm = TRUE)) %>%  # Return total precipitation for every month
  group_by(month) %>% # Group by year
  summarise(Precipitation = mean(Precipitation, na.rm = TRUE))  # return mean precipitation for each month

# Calculate mean monthly precipitation
MonthlyPrecip_Station49 <- df2 %>% 
  filter(StationID == 49) %>%  # Filter to Station 40
  group_by(year, month) %>% # Group by year and month
  summarise(Precipitation = sum(Precipitation, na.rm = TRUE)) %>%  # Return total precipitation for every month
  group_by(month) %>% # Group by year
  summarise(Precipitation = mean(Precipitation, na.rm = TRUE))  # return mean precipitation for each month

# Calculate monthly precipitation in study year
MonthlyPrecip_Station40_study <- df2 %>% 
  dplyr::filter(StationID == 40) %>%  # Filter to Station 40
  dplyr::filter(Date_Time >= "2018-11-01",
                Date_Time >= "2019-10-31") %>% 
  group_by(month) %>%
  summarise(Precipitation = sum(Precipitation, na.rm = TRUE))  # Return total precipitation for every month

# Calculate monthly precipitation in study year
MonthlyPrecip_Station49_study <- df2 %>% 
  dplyr::filter(StationID == 49) %>%  # Filter to Station 40
  dplyr::filter(Date_Time >= "2018-11-01",
                Date_Time >= "2019-10-31") %>% 
  group_by(month) %>%
  summarise(Precipitation = sum(Precipitation, na.rm = TRUE))  # Return total precipitation for every month

## plot for station 40
(Precip_seasonal_plots <- ggplot(MonthlyPrecip_Station40, aes(x=month, y=Precipitation)) +
    labs(title = "Monthly precipitation") +
    geom_col(aes(x=month, y=Precipitation), fill="light blue") +
    geom_point(data=MonthlyPrecip_Station40_study, aes(x=month, y=Precipitation)) +
    geom_vline(xintercept = 10.5, linetype="dotted") +
    scale_x_discrete(limits=month.abb)    +
    scale_y_continuous(name="Precipitation (mm)", breaks=seq(0,100,20), limits=c(0, 100)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)

ggsave("plots/Climatological Context/precipitation seasonality40.png", 
       Precip_seasonal_plots,
       width=16,
       height=12,
       units="cm")

## plot for station 49
(Precip_seasonal_plots <- ggplot(MonthlyPrecip_Station49, aes(x=month, y=Precipitation)) +
    labs(title = "Monthly precipitation") +
    geom_col(aes(x=month, y=Precipitation), fill="light blue") +
    geom_point(data=MonthlyPrecip_Station49_study, aes(x=month, y=Precipitation)) +
    geom_vline(xintercept = 10.5, linetype="dotted") +
    scale_x_discrete(limits=month.abb)    +
    scale_y_continuous(name="Precipitation (mm)", breaks=seq(0,100,20), limits=c(0, 100)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)

ggsave("plots/Climatological Context/precipitation seasonality49.png", 
       Precip_seasonal_plots,
       width=16,
       height=12,
       units="cm")


### Averaging seasonality across two stations
MonthlyPrecip_average <- (MonthlyPrecip_Station40 + MonthlyPrecip_Station49) /2
MonthlyPrecip_average_study <- (MonthlyPrecip_Station40_study + MonthlyPrecip_Station49_study)/2

## plot for two station average
(Precip_seasonal_plots <- ggplot(MonthlyPrecip_average, aes(x=month, y=Precipitation)) +
    labs(title = "Monthly precipitation") +
    geom_col(aes(x=month, y=Precipitation), fill="light blue") +
    geom_point(data=MonthlyPrecip_average_study, aes(x=month, y=Precipitation)) +
    geom_vline(xintercept = 10.5, linetype="dotted") +
    scale_x_discrete(limits=month.abb)    +
    scale_y_continuous(name="Precipitation (mm)", breaks=seq(0,100,20), limits=c(0, 100)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)

ggsave("plots/Climatological Context/precipitation seasonality_average.png", 
       Precip_seasonal_plots,
       width=16,
       height=12,
       units="cm")



# -------------- 5. Analysis of Potential Evapotranspiration --------------


# 6. Relative humidity ----
# Summarize relative humidity
RH_Station40 <- df2 %>% 
  filter(StationID == 40) %>%
  select(Date_Time,
         Relative_Humidity)

(mean_RH_ID40 <- mean(RH_Station40$Relative_Humidity, na.rm = T))
(median_RH_ID40 <- median(RH_Station40$Relative_Humidity, na.rm = T))
(iqr_RH_ID40 <- IQR(RH_Station40$Relative_Humidity, na.rm = T))

(RH_plots <- ggplot(RH_Station40, aes(x=Date_Time, y=Relative_Humidity)) +
    labs(title = "Relative Humidity at station 40") +
    # geom_point(aes(x=Date_Time, y=Relative_Humidity, shape =1)) +
    # geom_point(aes(x=Date_Time, y=Relative_Humidity, alpha=0.4, na.rm=T)) +
    # geom_point(aes(alpha=0.1, na.rm=T) +
    geom_point(alpha=0.1, na.rm=T) +
    geom_hline(yintercept = mean_RH_ID40, colour="red") +
    geom_hline(yintercept = median_RH_ID40, colour="blue") +
    geom_hline(yintercept = (mean_RH_ID40+(iqr_RH_ID40/2)), linetype = "dotted", colour="#5DC863FF") +
    geom_hline(yintercept = (mean_RH_ID40-(iqr_RH_ID40/2)), linetype = "dotted", colour="#5DC863FF") +
    # scale_x_continuous(name="Year", breaks=seq(1990,2020,10), limits=c(1988,2021)) +
    scale_y_continuous(name="Relative Humidity (%)", breaks=seq(0,100,25), limits=c(0, 120)) +
    theme_bw() +
    theme(plot.margin = unit(c(1, 4, 1, 1), "lines")) +
    coord_cartesian(clip = "off") +
    theme(legend.position = "bottom") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)

