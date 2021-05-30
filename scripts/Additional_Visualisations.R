## This script sources data output from EdiRe and AmeriFlux,
## merges them into a single file
## performs visualization and data analysis



# ------ 0.0 Setup Environment ----------
## Load packages
library(tidyverse)
library(patchwork)   


## Define paths
## NB. these data are ca. 110 GB

## Paths Andy's machine
path  <-  "C:/workspace/REC_7_Data/8_datasets/"
mpath  <-  "C:/workspace/REC_7_Data/12_Marcys_data/"


#-------------- 1. Read data --------------
{
# Note datetime  format differs (yyyy vs yy) between data files.

DF_SEG_EC0a  <- read_csv(file=paste(mpath, "US-Seg_HH_201801010000_201901010000.csv", sep=""),
                      col_types = cols(TIMESTAMP_START = col_datetime("%Y %m %d %H %M"),
                                       TIMESTAMP_END = col_datetime("%Y %m %d %H %M")))

DF_SEG_EC0b  <- read_csv(file=paste(mpath, "US-Seg_HH_201901010000_202001010000.csv", sep=""),
                      col_types = cols(TIMESTAMP_START = col_datetime("%Y %m %d %H %M"),
                                       TIMESTAMP_END = col_datetime("%Y %m %d %H %M")))

DF_SES_EC0a  <- read_csv(file=paste(mpath, "US-Ses_HH_201801010000_201901010000.csv", sep=""),
                      col_types = cols(TIMESTAMP_START = col_datetime("%Y %m %d %H %M"),
                                       TIMESTAMP_END = col_datetime("%Y %m %d %H %M")))

DF_SES_EC0b  <- read_csv(file=paste(mpath, "US-Ses_HH_201901010000_202001010000.csv", sep=""),
                      col_types = cols(TIMESTAMP_START = col_datetime("%Y %m %d %H %M"),
                                       TIMESTAMP_END = col_datetime("%Y %m %d %H %M")))

DF_SEG_EC1  <- read_csv(file=paste(path, "2020_01_01_from_flash_Txcor_SEG_REC1_flux.csv", sep=""),
                     col_types = cols('Date/Time' = col_datetime("%d/%m/%Y %H:%M")))

DF_SEG_EC2  <- read_csv(file=paste(path, "2020_01_01_from_flash_Txcor_SEG_REC2_flux.csv", sep=""),
                     col_types = cols('Date/Time' = col_datetime("%d/%m/%y %H:%M:%S")))

DF_SEG_EC3  <- read_csv(file=paste(path, "2020_01_01_from_flash_Txcor_SEG_REC3_flux.csv", sep=""),
                     col_types = cols('Date/Time' = col_datetime("%d/%m/%y %H:%M:%S")))

DF_SEG_EC4  <- read_csv(file=paste(path, "2020_01_01_from_flash_Txcor_SEG_REC4_flux.csv", sep=""),
                     col_types = cols('Date/Time' = col_datetime("%d/%m/%y %H:%M:%S")))

DF_SES_EC1  <- read_csv(file=paste(path, "2020_01_01_from_flash_Txcor_SES_REC1_flux.csv", sep=""),
                     col_types = cols('Date/Time' = col_datetime("%d/%m/%y %H:%M:%S")))

DF_SES_EC2  <- read_csv(file=paste(path, "2020_01_01_from_flash_Txcor_SES_REC2_flux.csv", sep=""),
                     col_types = cols('Date/Time' = col_datetime("%d/%m/%Y %H:%M")))

DF_SES_EC3  <- read_csv(file=paste(path, "2020_01_01_from_flash_Txcor_SES_REC3_flux.csv", sep=""),
                     col_types = cols('Date/Time' = col_datetime("%d/%m/%y %H:%M:%S")))

DF_SES_EC4  <- read_csv(file=paste(path, "2020_01_01_from_flash_Txcor_SES_REC4_flux.csv", sep=""),
                     col_types = cols('Date/Time' = col_datetime("%d/%m/%y %H:%M:%S")))
}  # Read data



#-------------- 2. Tidy data --------------

## Compile Marcy's files
DF_SEG_EC0 <- dplyr::bind_rows(DF_SEG_EC0a, DF_SEG_EC0b); rm(DF_SEG_EC0a, DF_SEG_EC0b)
DF_SES_EC0 <- dplyr::bind_rows(DF_SES_EC0a, DF_SES_EC0b); rm(DF_SES_EC0a, DF_SES_EC0b)


## create lists for processing
list1a <- list(DF_SEG_EC0, DF_SES_EC0) 
list1b <- c("SEG_EC0", "SES_EC0") 

list2a <- list(DF_SEG_EC1, DF_SEG_EC2, DF_SEG_EC3, DF_SEG_EC4, DF_SES_EC1, DF_SES_EC2, DF_SES_EC3, DF_SES_EC4) 
list2b <- c("SEG_EC1", "SEG_EC2", "SEG_EC3", "SEG_EC4", "SES_EC1", "SES_EC2", "SES_EC3", "SES_EC4") 


## Create custom function to select variables of interest and rename variables
## Marcy's data
tidy_marcy <- function(df, station) {
  df %>%
    rename(Precipitation = P_F,
           Datetime_Start = TIMESTAMP_START) %>% 
    select(Datetime_Start,
           FC,
           LE,
           H,
           Precipitation, 
           SW_IN
    ) %>% 
    mutate(Station = station)
  }

## REC data
tidy_RECs <- function(df, station) {
  df %>%
    rename(Datetime = 'Date/Time',
           FC = Fc,
           LE = rLE) %>% 
    mutate(Station = station,
           Datetime_Start = Datetime - lubridate::minutes(15)) %>%  # Align time stamps
    select(Datetime_Start,
           FC,
           LE,
           H,
           Station)
  }

# TO DO  Need to confirm the correct LE!!!!!!! ----
# TO DO  Need to confirm the correct FC!!!!!!! ----


## Use purrr::map2 to process multiple tibbles through pipe and return a single merged dataframe
DF_EC_Marcy <- list1a %>%
  purrr::map2(.x =., .y=list1b, ~tidy_marcy(.x, .y)) %>%
  bind_rows()

DF_EC_REC <- list2a %>%
  purrr::map2(.x =., .y=list2b, ~tidy_RECs(.x, .y)) %>%
  bind_rows()

## NB. If we need to handle more than two lists, use purrr::pmap
# my_list <- list(df_list, name_list, other_varlist)
# purrr::pmap(my_list, ~myfunction(..1, ..2, ..3))

# merge data from all station
DF_EC <- dplyr::bind_rows(DF_EC_Marcy, DF_EC_REC) 

# remove all unnecessary objects
rm(list1a, list1b, list2a, list2b,
   DF_SEG_EC0, DF_SES_EC0,
   DF_SEG_EC1, DF_SEG_EC2, DF_SEG_EC3, DF_SEG_EC4, DF_SES_EC1, DF_SES_EC2, DF_SES_EC3, DF_SES_EC4,
   DF_EC_Marcy, DF_EC_REC)


# Subset time series to study period dates

names(DF_EC)

str(DF_EC)

DF_EC_study <-DF_EC %>% 
  Filter(Datetime_Start >= X,
         Datetime_Start <= X)

### 

# TO DO - confirm whether I should apply any filtering to the flux data (U* etc.) ----



#-------------- 2. Analyze data --------------


#-------------- 2.1 Co-location comparison data --------------
# analyze EC0 versus EC1
# - inc. group by day



# Reproduce hourly plots by month












