## This script contains the climate analysis

# code from workstation

# ------ 0 Setup Environment ----------


## Install packages
library(scales) # for colour review
library(tidyverse) # For dplyr and ggplot2
library(viridis) # friendly colour palette
library(patchwork) # for multi-panel plots


## Plotting theme
theme_fancy <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 8, color = "black"),
      axis.title = element_text(size = 10, color = "black"),
      axis.line.x = element_line(size = 0.3, color = "black"),
      axis.line.y = element_line(size = 0.3, color = "black"),
      axis.ticks = element_line(size = 0.3, color = "black"),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
      plot.title = element_text(
        size = 10,
        vjust = 1,
        hjust = 0.5,
        color = "black"
      ),
      legend.text = element_text(size = 9, color = "black"),
      legend.title = element_text(size = 9, color = "black"),
      legend.position = c(0.9, 0.9),
      legend.key.size = unit(0.7, "line"),
      legend.background = element_rect(
        color = "black",
        fill = "transparent",
        size = 2,
        linetype = "blank"
      )
    )
}
windowsFonts("Helvetica" = windowsFont("Helvetica"))  # Ensure font is mapped correctly


#-------------- 1. Read Data --------------
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


df_recent <- read_csv("data/meteorological_data/sev1_meteorology_2011-current.csv")  # File of recent climate supplied by Tomer


# Annual summaries extracted from Fluxnet files (as part of JULES prep. script)
AnnualPrecip_Seg <- read_csv("data/meteorological_data/annual_precip_Seg.csv")
AnnualPrecip_Sen <- read_csv("data/meteorological_data/annual_precip_Sen.csv")
AnnualPrecip_Ses <- read_csv("data/meteorological_data/annual_precip_Ses.csv")



# # Data from 2021 supplied by Renee are in a different (terrible!) format. User Tomer's instead.
# 
# # Station 40 (all years)
# df_2021Met40 <- read_csv("data/meteorological_data/Sev Met Data 20220126/Met40.csv",
#                          col_names = FALSE,
#                          na = c("","NA","-999")
# )
# 
# # Station 49 (all years)
# df_2021Met49 <- read_csv("data/meteorological_data/Sev Met Data 20220126/Met49.csv",
#                          col_names = FALSE,
#                          na = c("","NA","-999"),
#                          skip = 1
# )
# # Unique code for each station
# df_2021Met40 <- df_2021Met40 %>%
#   rename(
#     Station = X1,
#     Year = X2,
#     DOY = X3,
#     time = X4,
#     precipitation_mm = X14
#   ) %>% 
#   mutate(
#     across(everything(), ~replace(., . %in%  c("", "NA", -888, -999), NA))  # Recode NA's
#   ) %>% 
#   mutate(
#     hours = time/100,
#     datetime = lubridate::make_datetime(year = Year, hour = hours) + lubridate::days(DOY-1)  # Create datetime
#   ) %>% 
#   filter(
#     Year !="-6999"  # Remove erroneous row with year value of -6999
#   )
# 
# 
# df_2021Met49 <- df_2021Met49 %>%
#   filter(row_number() > 90510) %>% # Not that variable order changes in 2020, so this discards the first part of the record
#   rename(
#     Station = X1,
#     Year = X8,
#     DOY = X9,
#     time = X10,
#     precipitation_mm = X24
#   ) %>% 
#   mutate(
#     Station = 49,
#     across(everything(), ~replace(., . %in%  c("", "NA", -888, -999), NA))  # Recode NA's
#   ) %>% 
#   mutate(
#     hours = time/100,
#     datetime = lubridate::make_datetime(year = Year, hour = hours) + lubridate::days(DOY-1)  # Create datetime
#   ) 
# 
# 
# ## Filter to retain hourly data (discarding short lines of embedded 1-minute precipitation data)
# ## All values are in mm's
# ## Skip/exclude row/lines where multiple columns are NA (filter(!is.na...)))
# df_2021Met40_hourly <- df_2021Met40 %>%
#   filter(!(is.na(X6) &
#              is.na(X7) &
#              is.na(X8) &
#              is.na(X9) &
#              is.na(X10) &
#              is.na(X11) &
#              is.na(X12) &
#              is.na(X13) &
#              is.na(X15) &
#              is.na(X16) &
#              is.na(X17) &
#              is.na(X18) &
#              is.na(X19) &
#              is.na(X20) &
#              is.na(X21) &
#              is.na(X22) &
#              is.na(X23) &
#              is.na(X24) &
#              is.na(X25) &
#              is.na(X26) &
#              is.na(X27) &
#              is.na(X28))
#   )
# 
# df_2021Met49_hourly <- df_2021Met49 %>%
#   filter(!(is.na(X5) &
#              is.na(X6) &
#              is.na(X12) &
#              is.na(X13) &
#              is.na(X14) &
#              is.na(X15) &
#              is.na(X16) &
#              is.na(X17) &
#              is.na(X18) &
#              is.na(X19) &
#              is.na(X20) &
#              is.na(X21) &
#              is.na(X22) &
#              is.na(X23) &
#              is.na(X25) &
#              is.na(X26) &
#              is.na(X27) &
#              is.na(X28))
#   )
# 
# 
# ## Subset time series to required year
# subset_datetime <- function(df) {
#   df %>%
#     filter(between(datetime,
#                    as.POSIXct("2021-01-01 00:00:00"),
#                    as.POSIXct("2021-12-31 23:30:00")))
# }
# 
# df_2021Met40_hourly_clip <- subset_datetime(df_2021Met40_hourly)
# df_2021Met49_hourly_clip <- subset_datetime(df_2021Met49_hourly)
# 
# 
# names(df_2021Met40_hourly_clip)
# 
# 
# # create plot
# (plot <- ggplot(df_2021Met40_hourly_clip, aes(x=datetime, y=precipitation_mm)) +
#     labs(x = expression("Datetime"),
#          y = expression("Hourly Precipitation (mm)"),
#          title = "Precipitation at Station 40") +
#     geom_point() +
#     theme_fancy() 
# )
# 
# # create plot
# (plot <- ggplot(df_2021Met49_hourly_clip, aes(x=datetime, y=precipitation_mm)) +
#     labs(x = expression("Datetime"),
#          y = expression("Hourly Precipitation (mm)"),
#          title = "Precipitation at Station 49") +
#     geom_point() +
#     theme_fancy() 
# )
# 
# # This is incomplete, something isn't right here! implasuble annual precip for station 40 in 2021!
# ## annual precip
# (S40_precip_in_2021 <- sum(df_2021Met40_hourly_clip$precipitation_mm))
# (S49_precip_in_2021 <- sum(df_2021Met49_hourly_clip$precipitation_mm))
# 



#-------------- 2. Tidy Data --------------

## Combine data
df <- rbind(df_1988_1994, df_1995_1999, df_2000_2004, df_2005_2009, df_2010_2014, df_2015_2019, df_2020)

rm(df_1988_1994, df_1995_1999, df_2000_2004, df_2005_2009, df_2010_2014, df_2015_2019, df_2020)  # Remove unnecessary objects 


df <- df %>% 
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


## reformat and subset recent met data
df2 <- df_recent %>% 
  rename(year = Year) %>%
  mutate(
    across(everything(), ~replace(., . %in%  c("", "NA", -888, -999), NA))  # Recode NA's
    ) %>%
  mutate(
    Date_Time = lubridate::make_datetime(year = year, hour = Hour) + lubridate::days(Julian_Day-1),  # Create datetime
    month = lubridate::month(Date_Time)
    ) %>% 
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

## subset for missing recent time only
# Subset time series to study period dates
subset_datetime <- function(df) {
  df %>%
    filter(between(Date_Time,
                   as.POSIXct("2021-01-01 00:00:00"),
                   as.POSIXct("2021-12-31 23:59:00")))
}

df3 <- subset_datetime(df2)

df <- rbind(df, df3)

rm(df2, df3, df_recent)


# -------------- 3. Analyze Data --------------

## StationID = 40 (Deep Well) is the longest running station, active since mid 1987.
## Station 40 is within a few km of our study sites and we consider it representative for annual totals. 


### Extract annual statistics ###
AnnualPrecip_Station40 <- df %>% 
  filter(StationID == 40) %>% 
  group_by(year) %>% 
  summarise(Precipitation = sum(Precipitation, na.rm = TRUE))

AnnualTemp_Station40 <- df %>% 
  filter(StationID == 40) %>%
  group_by(year) %>% 
  summarise(Temp_C = mean(Temp_C, na.rm = TRUE))


# StationID = 49 (Five Points Meteorological Station) is closer to the Ses site but only started in 1999.
AnnualPrecip_Station49 <- df %>% 
  filter(StationID == 49) %>% 
  group_by(year) %>% 
  summarise(Precipitation = sum(Precipitation, na.rm = TRUE))

AnnualTemp_Station49 <- df %>% 
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
    scale_x_continuous(name="Year", breaks=seq(1990,2021,10), limits=c(1988,2021)) +
    scale_y_continuous(name="Annual Precipitation (mm)", breaks=seq(0,350,50), limits=c(0, 350)) +
    ## Add annotation for study periods. Tower data recorded 393 mm precipitation at SEG and 374 mm precipitation at SES over the 2-year study period.
    # Ca. SEG 197 mm pa and SES 187 mm pa.
    geom_text(x=2025.4, y=197, label="Seg Study Period", colour="#440154FF", size=2.1, fontface="bold") +
    geom_text(x=2025.4, y=187, label="Ses Study Period", colour="#21908CFF", size=2.1, fontface="bold") +
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
MonthlyPrecip_Station40 <- df %>%
  filter(StationID == 40) %>%  # Filter to Station 40
  group_by(year, month) %>% # Group by year and month
  summarise(Precipitation = sum(Precipitation, na.rm = TRUE)) %>%  # Return total precipitation for every month
  group_by(month) %>% # Group by year
  summarise(Precipitation = mean(Precipitation, na.rm = TRUE))  # return mean precipitation for each month

# # Calculate mean monthly precipitation
# MonthlyPrecip_Station49 <- df %>% 
#   filter(StationID == 49) %>%  # Filter to Station 40
#   group_by(year, month) %>% # Group by year and month
#   summarise(Precipitation = sum(Precipitation, na.rm = TRUE)) %>%  # Return total precipitation for every month
#   group_by(month) %>% # Group by year
#   summarise(Precipitation = mean(Precipitation, na.rm = TRUE))  # return mean precipitation for each month


## Calculate observed monthly precipitation from the EC towers # calculated in main script
# MonthlyPrecip_SEG_study
# MonthlyPrecip_SES_study 

# 
# 
# 
# OLD CODE UNUSED Calculate monthly precipitation in study period
# MonthlyPrecip_Station49_study <- df %>% 
#   dplyr::filter(StationID == 49) %>%  # Filter to Station 40
#   dplyr::filter(Date_Time >= "2018-11-01",
#                 Date_Time >= "2020-10-31") %>% 
#   group_by(year, month) %>%
#   summarise(Precipitation = sum(Precipitation, na.rm = TRUE))  # Return total precipitation for every month
# 




# Manually putting together average precipitation
MonthlyPrecip_Station40 # review monthly averages

year <- c(2018, 2018,
            2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019,
            2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020)
month  <- c(11, 12,
            1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
            1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
Precipitation <- c(10.6, 12.2,
                    6.96, 7.10, 11.2, 10.8, 9.66, 16.8, 44.6, 40.2, 32.9, 22.8, 10.6, 12.2,
                    6.96, 7.10, 11.2, 10.8, 9.66, 16.8, 44.6, 40.2, 32.9, 22.8) 

average_monthly_precip <- data.frame(year, month, Precipitation)



# Combine monthly site data for plotting
MonthlyPrecip_SEG_study2 <- MonthlyPrecip_SEG_study %>% 
  mutate(
    StationID = "SEG",
    date = lubridate::make_datetime(year = year, month = month)
  ) 

MonthlyPrecip_SES_study2 <- MonthlyPrecip_SES_study %>% 
  mutate(
    StationID = "SES",
    date = lubridate::make_datetime(year = year, month = month)
  ) 

average_monthly_precip <- average_monthly_precip %>% 
  mutate(
    StationID = "Station_40",
    date = lubridate::make_datetime(year = year, month = month)
  ) 

MonthlyPrecip <- rbind(MonthlyPrecip_SEG_study2, MonthlyPrecip_SES_study2)

## plot for climate context
(Precip_seasonal_plots <- ggplot(MonthlyPrecip, aes(x=date, y=Precipitation, group=StationID)) +
    # labs(title = "Monthly precipitation", StationID = "Station ID") +
    labs(title = "Monthly precipitation", colour = "Station ID") +
    geom_col(data=average_monthly_precip, aes(x=date, y=Precipitation), fill="light blue") +
    geom_point(aes(colour = factor(StationID))) +
    scale_colour_manual(values = c("#440154FF", "#21908CFF")) +
    scale_y_continuous(name="Precipitation (mm)", breaks=seq(0,80,20), limits=c(0, 80)) +
    theme_fancy() +
    theme(legend.position = c(0.1, 0.9))
)

ggsave("plots/Climatological Context/precipitation seasonality.png", 
       Precip_seasonal_plots,
       width=16,
       height=12,
       units="cm")


# ## plot for station 40
# (Precip_seasonal_plots <- ggplot(MonthlyPrecip_Station40, aes(x=month, y=Precipitation)) +
#     labs(title = "Monthly precipitation") +
#     geom_col(aes(x=month, y=Precipitation), fill="light blue") +
#     # geom_point(data=MonthlyPrecip_Station40_study, aes(x=month, y=Precipitation)) +
#     geom_vline(xintercept = 10.5, linetype="dotted") +
#     scale_x_discrete(limits=month.abb)    +
#     scale_y_continuous(name="Precipitation (mm)", breaks=seq(0,100,20), limits=c(0, 100)) +
#     theme_bw() +
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# )



# ## plot for station 49
# (Precip_seasonal_plots <- ggplot(MonthlyPrecip_Station49, aes(x=month, y=Precipitation)) +
#     labs(title = "Monthly precipitation") +
#     geom_col(aes(x=month, y=Precipitation), fill="light blue") +
#     geom_point(data=MonthlyPrecip_Station49_study, aes(x=month, y=Precipitation)) +
#     geom_vline(xintercept = 10.5, linetype="dotted") +
#     scale_x_discrete(limits=month.abb)    +
#     scale_y_continuous(name="Precipitation (mm)", breaks=seq(0,100,20), limits=c(0, 100)) +
#     theme_bw() +
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# )
# 
# ggsave("plots/Climatological Context/precipitation seasonality49.png", 
#        Precip_seasonal_plots,
#        width=16,
#        height=12,
#        units="cm")

# 
# ### Averaging seasonality across two stations
# MonthlyPrecip_average <- (MonthlyPrecip_Station40 + MonthlyPrecip_Station49) /2
# MonthlyPrecip_average_study <- (MonthlyPrecip_Station40_study + MonthlyPrecip_Station49_study)/2
# 
# ## plot for two station average
# (Precip_seasonal_plots <- ggplot(MonthlyPrecip_average, aes(x=month, y=Precipitation)) +
#     labs(title = "Monthly precipitation") +
#     geom_col(aes(x=month, y=Precipitation), fill="light blue") +
#     geom_point(data=MonthlyPrecip_average_study, aes(x=month, y=Precipitation)) +
#     geom_vline(xintercept = 10.5, linetype="dotted") +
#     scale_x_discrete(limits=month.abb)    +
#     scale_y_continuous(name="Precipitation (mm)", breaks=seq(0,100,20), limits=c(0, 100)) +
#     theme_bw() +
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# )
# 
# ggsave("plots/Climatological Context/precipitation seasonality_average.png", 
#        Precip_seasonal_plots,
#        width=16,
#        height=12,
#        units="cm")
# 


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



# ## This script contains the climate analysis
# 
# 
# # ------ 0 Setup Environment ----------
# # 
# # install.packages("tidyverse") # For dplyr and ggplot2
# # # install.packages("vctrs") # For dplyr and ggplot2
# # install.packages("ellipsis") # For dplyr and ggplot2
# # install.packages("rlang") # For dplyr and ggplot2
# # install.packages("dplyr") # For dplyr and ggplot2
# # install.packages("viridis") # For dplyr and ggplot2
# # install.packages("readr") # For dplyr and ggplot2
# # install.packages("lubridate") # For dplyr and ggplot2
# 
# ## Install packages
# library(scales) # for colour review
# library(tidyverse) # For dplyr and ggplot2
# library(viridis) # friendly colour palette
# library(patchwork) # for multi-panel plots
# 
# 
# #-------------- 1. Extract Data --------------
# ## Using data sourced from Moore, D.I. 2021. Meteorology Data from the Sevilleta 
# ## National Wildlife Refuge, New Mexico ver 14. Environmental Data Initiative. 
# ## https://doi.org/10.6073/pasta/1cbc37ae4d40b3844b5e4be9f6f18073 
# ## (Accessed 2021-04-28).
# 
# ## For more information see: https://unmsevilletafieldstation.wordpress.com/met-station-network/
# 
# ## Read data
# df_1988_1994 <- read_csv("data/meteorological_data/Sevilleta_LTER_Hourly_Meteorological_Data_1988_1994.csv",
#                          col_types = cols(Evaporation = col_double(),
#                                           Evap_Pan_Temperature = col_double(),
#                                           Bar_Pressure = col_double()))
# 
# df_1995_1999 <- read_csv("data/meteorological_data/Sevilleta_LTER_Hourly_Meteorological_Data_1995_1999.csv",
#                          col_types = cols(Moisture_10_cm = col_double(),
#                                           Moisture_30_cm  = col_double(),
#                                           Evaporation = col_double(),
#                                           Evap_Pan_Temperature  = col_double()))
# 
# df_2000_2004 <- read_csv("data/meteorological_data/Sevilleta_LTER_Hourly_Meteorological_Data_2000_2004.csv",
#                          col_types = cols(Moisture_10_cm = col_double(),
#                                           Moisture_30_cm  = col_double(),
#                                           Evaporation = col_double(),
#                                           Evap_Pan_Temperature  = col_double()))
# 
# df_2005_2009 <- read_csv("data/meteorological_data/Sevilleta_LTER_Hourly_Meteorological_Data_2005_2009.csv",
#                          col_types = cols(Moisture_10_cm = col_double(),
#                                           Moisture_30_cm  = col_double(),
#                                           Evaporation = col_double(),
#                                           Evap_Pan_Temperature  = col_double()))
# 
# df_2010_2014 <- read_csv("data/meteorological_data/Sevilleta_LTER_Hourly_Meteorological_Data_2010_2014.csv",
#                          col_types = cols(Moisture_10_cm = col_double(),
#                                           Moisture_30_cm  = col_double(),
#                                           Evaporation = col_double(),
#                                           Evap_Pan_Temperature  = col_double()))
# 
# df_2015_2019 <- read_csv("data/meteorological_data/Sevilleta_LTER_Hourly_Meteorological_Data_2015_2019.csv",
#                          col_types = cols(Moisture_10_cm = col_double(),
#                                           Moisture_30_cm  = col_double(),
#                                           Evaporation = col_double(),
#                                           Evap_Pan_Temperature  = col_double()))
# 
# df_2020 <- read_csv("data/meteorological_data/Sevilleta_LTER_Hourly_Meteorological_Data_2020.csv",
#                     col_types = cols(Moisture_10_cm = col_double(),
#                                      Moisture_30_cm  = col_double(),
#                                      Evaporation = col_double(),
#                                      Evap_Pan_Temperature  = col_double()))
# 
# 
# 
# 
# # Data from 2021 in different (terrible!) format
# ## attempting read with read_csv doesn't work, obscure error
# # df_2021Met40 <- read_csv("data/meteorological_data/Sev Met Data 20220126/Met40.csv",
# #                          col_names = FALSE,
# #                          na = c("","NA","-999")
# # )
# # 
# # Error in app$vspace(new_style$`margin-top` %||% 0) :                                                                                               
# #   attempt to apply non-function
# # In addition: Warning message:
# #   One or more parsing issues, see `problems()` for details 
# 
# 
# 
# df_2021Met40 <- read.csv("data/meteorological_data/Sev Met Data 20220126/Met40.csv", header=FALSE)
# 
# 
# temp1 <- read.csv("data/meteorological_data/Sev Met Data 20220126/Met40.csv", header=FALSE) %>%
#   mutate(across(everything(), ~replace(., . %in%  c("", "NA", -999), NA)))
# 
# temp2 <- read.csv("data/meteorological_data/Sev Met Data 20220126/Met40.csv", header=FALSE) %>%
#   mutate(across(everything(), ~replace(., . %in%  c("", "NA", -999), NA))) %>% 
#   rename(
#     Station = V1,
#     Year = V2,
#     DOY = V3,
#     time = V4
#   )
# 
# # temp3 <- read.csv("data/meteorological_data/Sev Met Data 20220126/Met40.csv", header=FALSE) %>%
# #   mutate(across(everything(), ~replace(., . %in%  c("", "NA", -999), NA))) %>% 
# #   rename(
# #     Station = V1,
# #     Year = V2,
# #     DOY = V3,
# #     time = V4
# #   ) %>% 
# #   mutate(
# #     hours = time/100,
# #     datetime = lubridate::make_datetime(year = Year, hour = hours) + lubridate::days(DOY-1)
# #   )
# # # Encounter error
# # # Error in app$vspace(new_style$`margin-top` %||% 0) : 
# #  # attempt to apply non-function
# 
# 
# str(temp2)
# 
# names(temp2)
# 
# # very strange issues (Hugh couldn't replicate)
# # system specific issue'?
# # update everything in R 
# # use R console with admin
# 
# 
# # None of my stuff is working!
# 
# temp4 <- as_tibble(temp2)
# 
# temp5 <- temp4 %>%
#   filter(if_any(all_of(c(6:28), is.na)))
# 
# 
# temp5 <- temp4 %>% 
#   mutate(
#     hours = time/100,
#     datetime = lubridate::make_datetime(year = Year, hour = hours) + lubridate::days(DOY-1)
#   )
# 
# 
# 
# temp5 <- temp4 %>%
#   filter(!is.na(X6) &
#            !is.na(X7) &
#            !is.na(X8) &
#            !is.na(X9) &
#            !is.na(X10) &
#            !is.na(X11) &
#            !is.na(X12) &
#            !is.na(X13) &
#            !is.na(X14) &
#            !is.na(X15) &
#            !is.na(X16) &
#            !is.na(X17) &
#            !is.na(X18) &
#            !is.na(X19) &
#            !is.na(X20) &
#            !is.na(X21) &
#            !is.na(X22) &
#            !is.na(X23) &
#            !is.na(X24) &
#            !is.na(X25) &
#            !is.na(X26) &
#            !is.na(X27) &
#            !is.na(X28)
#   )
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# df_2021Met40_dev <- df_2021Met40 %>%
#   rename(
#     Station = X1,
#     Year = X2,
#     DOY = X3,
#     time = X4
#   ) %>% 
#   mutate(
#     across(everything(), ~replace(., . %in%  c("", "NA", -999), NA))
#   ) %>% 
#   mutate(
#     hours = time/100,
#     datetime = lubridate::make_datetime(year = Year, hour = hours) + lubridate::days(DOY-1)
#   )
# 
# 
# 
# # str(df_2021Met40)
# # 
# 
# # 
# # 
# # 
# # 
# # # Data from 2021 in different (terrible!) format
# # df_2021Met40 <- read_csv2("data/meteorological_data/Sev Met Data 20220126/Met40.csv",
# #                          col_names = FALSE,
# #                          na = c("NA","-999")
# # )
# # 
# # 
# # 
# # # Data from 2021 in different (terrible!) format
# # df_2021Met40 <- read_csv(I("./data/meteorological_data/Sev Met Data 20220126/Met40.csv"),
# #                          col_names = FALSE,
# #                          na = c("NA","-999")
# # )
# 
# 
#      
# 
# df_2021Met40 <- df_2021Met40 %>% 
#   rename(
#     Station = X1,
#     Year = X2,
#     DOY = X3,
#     time = X4
#   ) %>% 
#   mutate(
#     hours = time/100,
#     datetime = lubridate::make_datetime(year = Year, hour = hours) + lubridate::days(DOY-1)
#   )
# 
# ## Note that there is also 1-minute precip. data imbedded within the hourly; these are the short lines of data.
# ## All values are in mm's
# ## Skip/exclude row/lines where multiple columns are NA (filter(!is.na...)))
# # temp <- df_2021Met40 %>%
# #   filter(!is.na(column_name))
# # 
# # 
# # temp <- df_2021Met40 %>%
# #   filter(!is.na(X6))
# 
# str(df_2021Met40)
# 
# temp4 <- df_2021Met40 %>%
#   filter(if_any(all_of(c(6:28), is.na)))
#          
# ?if_any
# ?tidyverse
# packageVersion('dplyr')
#          
# 
# temp3 <- df_2021Met40 %>%
#   filter(!is.na(X6) &
#            !is.na(X7) &
#            !is.na(X8) &
#            !is.na(X9) &
#            !is.na(X10) &
#            !is.na(X11) &
#            !is.na(X12) &
#            !is.na(X13) &
#            !is.na(X14) &
#            !is.na(X15) &
#            !is.na(X16) &
#            !is.na(X17) &
#            !is.na(X18) &
#            !is.na(X19) &
#            !is.na(X20) &
#            !is.na(X21) &
#            !is.na(X22) &
#            !is.na(X23) &
#            !is.na(X24) &
#            !is.na(X25) &
#            !is.na(X26) &
#            !is.na(X27) &
#            !is.na(X28)
#          )
# 
# 
# 
# 
# How to write and?
# 
# "X6" "X7"  "X8"  "X9"  "X10" "X11" "X12" "X13" "X14" "X15" "X16" "X17" "X18"
# "X19" "X20" "X21" "X22" "X23" "X24" "X25" "X26" "X27" "X28"
# 
# 
# # Logical operator - test for all conditions
# 
# 
# 
# 
# str(df_2021Met40)
# summarise(df_2021Met40$X14)
# plot(df_2021Met40$X14)
# plot(df_2021Met40$X18)
# 
# 
# 
# # Data from 2021 in different format
# df_2021Met49 <- read_csv("data/meteorological_data/Sev Met Data 20220126/Met49.csv",
#                          col_names = "X6" )
# 
# 
# # TO DO ####
# # Waiting for Doug Moor to confirm precipitation column and units in the raw data files
#                    
# 
# 
# # Annual summaries extracted from Fluxnet files (as part of JULES prep. script)
# AnnualPrecip_Seg <- read_csv("data/meteorological_data/annual_precip_Seg.csv")
# AnnualPrecip_Sen <- read_csv("data/meteorological_data/annual_precip_Sen.csv")
# AnnualPrecip_Ses <- read_csv("data/meteorological_data/annual_precip_Ses.csv")
# 
# 
# ## Combine data
# df <- rbind(df_1988_1994, df_1995_1999, df_2000_2004, df_2005_2009, df_2010_2014, df_2015_2019, df_2020)
# 
# rm(df_1988_1994, df_1995_1999, df_2000_2004, df_2005_2009, df_2010_2014, df_2015_2019, df_2020)  # Remove unnecessary objects 
# 
# 
# 
# # -------------- 2. Tidy Data --------------
# df2 <- df %>% 
#   mutate(month = lubridate::month(Date)) %>%
#   rename(year = Year) %>% 
#   select(StationID,
#          Date_Time,
#          year,
#          month,
#          Julian_Day,
#          Hour,
#          Temp_C, 
#          Precipitation,
#          Relative_Humidity
#   )
# 
# 
# 
# # -------------- 3. Analyze Data --------------
# 
# ## StationID = 40 (Deep Well) is the longest running station, active since mid 1987.
# ## Station 40 is within a few km of our study sites and we consider it representative for annual totals. 
# 
# 
# ### Extract annual statistics ###
# AnnualPrecip_Station40 <- df2 %>% 
#   filter(StationID == 40) %>% 
#   group_by(year) %>% 
#   summarise(Precipitation = sum(Precipitation, na.rm = TRUE))
# 
# AnnualTemp_Station40 <- df2 %>% 
#   filter(StationID == 40) %>%
#   group_by(year) %>% 
#   summarise(Temp_C = mean(Temp_C, na.rm = TRUE))
# 
# 
# # StationID = 49 (Five Points Meteorological Station) is closer to the Ses site but only started in 1999.
# AnnualPrecip_Station49 <- df2 %>% 
#   filter(StationID == 49) %>% 
#   group_by(year) %>% 
#   summarise(Precipitation = sum(Precipitation, na.rm = TRUE))
# 
# AnnualTemp_Station49 <- df2 %>% 
#   filter(StationID == 49) %>%
#   group_by(year) %>% 
#   summarise(Temp_C = mean(Temp_C, na.rm = TRUE))
# 
# 
# ### Add Station ID to precipitation records
# AnnualPrecip_Station40 <- AnnualPrecip_Station40 %>% 
#   mutate(Station = "Station40")
# 
# AnnualPrecip_Station49 <- AnnualPrecip_Station49 %>% 
#   mutate(Station = "Station49")
# 
# AnnualPrecip_Seg <- AnnualPrecip_Seg %>% 
#   mutate(Station = "Seg") %>% 
#   rename(year = year)
# 
# AnnualPrecip_Sen <- AnnualPrecip_Sen %>% 
#   mutate(Station = "Sen") %>% 
#   rename(year = year)
# 
# AnnualPrecip_Ses <- AnnualPrecip_Ses %>% 
#   mutate(Station = "Ses") %>% 
#   rename(year = year)
# 
# 
# ### Extract mean annual statistics ###
# 
# ## Mean Annual Precipitation
# (mean_precip_ID40 <- mean(AnnualPrecip_Station40$Precipitation))
# (mean_precip_ID49 <- mean(AnnualPrecip_Station49$Precipitation))
# 
# ## Mean Annual Temperature 
# (mean_temp_ID40 <- mean(AnnualTemp_Station40$Temp_C))
# (mean_temp_ID49 <- mean(AnnualTemp_Station49$Temp_C))
# 
# ## Calculate IQR
# (iqr_ID40 <- IQR(AnnualPrecip_Station40$Precipitation))
# (iqr_ID49 <- IQR(AnnualPrecip_Station49$Precipitation))
# 
# ## Calculate Coefficient of Variation
# (CoV_ID40 <- sd(AnnualPrecip_Station40$Precipitation)/mean_precip_ID40)
# (CoV_ID49 <- sd(AnnualPrecip_Station49$Precipitation)/mean_precip_ID49)
# 
# 
# # -------------- 4. Visualize Data --------------
# 
# ### Comparison of total annual precipitation ###
# 
# ## Manually add our stated precipitation totals
# # Stated_Seg <- data.frame(Year=2019, Precipitation=179, Station="Seg_Study_Period")
# # Stated_Ses <- data.frame(Year=2019, Precipitation=171, Station="Ses_Study_Period")
# 
# 
# ### Combine annual precipitation records from all stations
# AnnualPrecip <- rbind(AnnualPrecip_Station40,
#                       AnnualPrecip_Station49,
#                       AnnualPrecip_Seg,
#                       AnnualPrecip_Sen,
#                       AnnualPrecip_Ses
#                       )
# 
# 
# ## Plot annual precipitation from all stations
# scales::show_col(viridis_pal(option = "viridis")(5)) # review colour options
# 
# (AnnualPrecip_plots <- ggplot(AnnualPrecip, aes(x=year, y=Precipitation, group=Station)) +
#     labs(title = "Annual precipitation at different stations") +
#     geom_point(aes(x=year, y=Precipitation, colour=Station)) +
#     geom_hline(yintercept = mean_precip_ID40, colour="#5DC863FF") +
#     geom_hline(yintercept = (mean_precip_ID40+(iqr_ID40/2)), linetype = "dotted", colour="#5DC863FF") +
#     geom_hline(yintercept = (mean_precip_ID40-(iqr_ID40/2)), linetype = "dotted", colour="#5DC863FF") +
#     scale_colour_viridis_d(option = "viridis") +
#     scale_x_continuous(name="Year", breaks=seq(1990,2020,10), limits=c(1988,2020)) +
#     scale_y_continuous(name="Annual Precipitation (mm)", breaks=seq(0,350,50), limits=c(0, 350)) +
#     # Add annotation for study periods
#     geom_text(x=2024.2, y=197, label="Seg Study Period", colour="#440154FF", size=2.1, fontface="bold") +
#     geom_text(x=2024.2, y=171, label="Ses Study Period", colour="#21908CFF", size=2.1, fontface="bold") +
#     theme_bw() +
#     theme(plot.margin = unit(c(1, 4, 1, 1), "lines")) +
#     coord_cartesian(clip = "off") +
#     theme(legend.position = "bottom") +
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# )
# 
# ggsave("plots/Climatological Context/Annual precipitation.png", 
#        AnnualPrecip_plots,
#        width=16,
#        height=12,
#        units="cm")
# 
# 
# ### 4.2 -  precipitation seasonality comparison ####
# 
# # Calculate mean monthly precipitation
# MonthlyPrecip_Station40 <- df2 %>% 
#   filter(StationID == 40) %>%  # Filter to Station 40
#   group_by(year, month) %>% # Group by year and month
#   summarise(Precipitation = sum(Precipitation, na.rm = TRUE)) %>%  # Return total precipitation for every month
#   group_by(month) %>% # Group by year
#   summarise(Precipitation = mean(Precipitation, na.rm = TRUE))  # return mean precipitation for each month
# 
# # Calculate mean monthly precipitation
# MonthlyPrecip_Station49 <- df2 %>% 
#   filter(StationID == 49) %>%  # Filter to Station 40
#   group_by(year, month) %>% # Group by year and month
#   summarise(Precipitation = sum(Precipitation, na.rm = TRUE)) %>%  # Return total precipitation for every month
#   group_by(month) %>% # Group by year
#   summarise(Precipitation = mean(Precipitation, na.rm = TRUE))  # return mean precipitation for each month
# 
# # Calculate monthly precipitation in study year
# MonthlyPrecip_Station40_study <- df2 %>% 
#   dplyr::filter(StationID == 40) %>%  # Filter to Station 40
#   dplyr::filter(Date_Time >= "2018-11-01",
#                 Date_Time >= "2019-10-31") %>% 
#   group_by(month) %>%
#   summarise(Precipitation = sum(Precipitation, na.rm = TRUE))  # Return total precipitation for every month
# 
# # Calculate monthly precipitation in study year
# MonthlyPrecip_Station49_study <- df2 %>% 
#   dplyr::filter(StationID == 49) %>%  # Filter to Station 40
#   dplyr::filter(Date_Time >= "2018-11-01",
#                 Date_Time >= "2019-10-31") %>% 
#   group_by(month) %>%
#   summarise(Precipitation = sum(Precipitation, na.rm = TRUE))  # Return total precipitation for every month
# 
# ## plot for station 40
# (Precip_seasonal_plots <- ggplot(MonthlyPrecip_Station40, aes(x=month, y=Precipitation)) +
#     labs(title = "Monthly precipitation") +
#     geom_col(aes(x=month, y=Precipitation), fill="light blue") +
#     geom_point(data=MonthlyPrecip_Station40_study, aes(x=month, y=Precipitation)) +
#     geom_vline(xintercept = 10.5, linetype="dotted") +
#     scale_x_discrete(limits=month.abb)    +
#     scale_y_continuous(name="Precipitation (mm)", breaks=seq(0,100,20), limits=c(0, 100)) +
#     theme_bw() +
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# )
# 
# ggsave("plots/Climatological Context/precipitation seasonality40.png", 
#        Precip_seasonal_plots,
#        width=16,
#        height=12,
#        units="cm")
# 
# ## plot for station 49
# (Precip_seasonal_plots <- ggplot(MonthlyPrecip_Station49, aes(x=month, y=Precipitation)) +
#     labs(title = "Monthly precipitation") +
#     geom_col(aes(x=month, y=Precipitation), fill="light blue") +
#     geom_point(data=MonthlyPrecip_Station49_study, aes(x=month, y=Precipitation)) +
#     geom_vline(xintercept = 10.5, linetype="dotted") +
#     scale_x_discrete(limits=month.abb)    +
#     scale_y_continuous(name="Precipitation (mm)", breaks=seq(0,100,20), limits=c(0, 100)) +
#     theme_bw() +
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# )
# 
# ggsave("plots/Climatological Context/precipitation seasonality49.png", 
#        Precip_seasonal_plots,
#        width=16,
#        height=12,
#        units="cm")
# 
# 
# ### Averaging seasonality across two stations
# MonthlyPrecip_average <- (MonthlyPrecip_Station40 + MonthlyPrecip_Station49) /2
# MonthlyPrecip_average_study <- (MonthlyPrecip_Station40_study + MonthlyPrecip_Station49_study)/2
# 
# ## plot for two station average
# (Precip_seasonal_plots <- ggplot(MonthlyPrecip_average, aes(x=month, y=Precipitation)) +
#     labs(title = "Monthly precipitation") +
#     geom_col(aes(x=month, y=Precipitation), fill="light blue") +
#     geom_point(data=MonthlyPrecip_average_study, aes(x=month, y=Precipitation)) +
#     geom_vline(xintercept = 10.5, linetype="dotted") +
#     scale_x_discrete(limits=month.abb)    +
#     scale_y_continuous(name="Precipitation (mm)", breaks=seq(0,100,20), limits=c(0, 100)) +
#     theme_bw() +
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# )
# 
# ggsave("plots/Climatological Context/precipitation seasonality_average.png", 
#        Precip_seasonal_plots,
#        width=16,
#        height=12,
#        units="cm")
# 
# 
# 
# # -------------- 5. Analysis of Potential Evapotranspiration --------------
# 
# 
# # 6. Relative humidity ----
# # Summarize relative humidity
# RH_Station40 <- df2 %>% 
#   filter(StationID == 40) %>%
#   select(Date_Time,
#          Relative_Humidity)
# 
# (mean_RH_ID40 <- mean(RH_Station40$Relative_Humidity, na.rm = T))
# (median_RH_ID40 <- median(RH_Station40$Relative_Humidity, na.rm = T))
# (iqr_RH_ID40 <- IQR(RH_Station40$Relative_Humidity, na.rm = T))
# 
# (RH_plots <- ggplot(RH_Station40, aes(x=Date_Time, y=Relative_Humidity)) +
#     labs(title = "Relative Humidity at station 40") +
#     # geom_point(aes(x=Date_Time, y=Relative_Humidity, shape =1)) +
#     # geom_point(aes(x=Date_Time, y=Relative_Humidity, alpha=0.4, na.rm=T)) +
#     # geom_point(aes(alpha=0.1, na.rm=T) +
#     geom_point(alpha=0.1, na.rm=T) +
#     geom_hline(yintercept = mean_RH_ID40, colour="red") +
#     geom_hline(yintercept = median_RH_ID40, colour="blue") +
#     geom_hline(yintercept = (mean_RH_ID40+(iqr_RH_ID40/2)), linetype = "dotted", colour="#5DC863FF") +
#     geom_hline(yintercept = (mean_RH_ID40-(iqr_RH_ID40/2)), linetype = "dotted", colour="#5DC863FF") +
#     # scale_x_continuous(name="Year", breaks=seq(1990,2020,10), limits=c(1988,2021)) +
#     scale_y_continuous(name="Relative Humidity (%)", breaks=seq(0,100,25), limits=c(0, 120)) +
#     theme_bw() +
#     theme(plot.margin = unit(c(1, 4, 1, 1), "lines")) +
#     coord_cartesian(clip = "off") +
#     theme(legend.position = "bottom") +
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# )
# 
