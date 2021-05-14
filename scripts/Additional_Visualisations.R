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
# Including parsing datetimes. 
# Note that the year format differs (yyyy vs yy) between REC systems.

SEG_EC0a  <- read_csv(file=paste(mpath, "US-Seg_HH_201801010000_201901010000.csv", sep=""),
                      col_types = cols(TIMESTAMP_START = col_datetime("%Y %m %d %H %M"),
                                       TIMESTAMP_END = col_datetime("%Y %m %d %H %M")))

SEG_EC0b  <- read_csv(file=paste(mpath, "US-Seg_HH_201901010000_202001010000.csv", sep=""),
                      col_types = cols(TIMESTAMP_START = col_datetime("%Y %m %d %H %M"),
                                       TIMESTAMP_END = col_datetime("%Y %m %d %H %M")))

SES_EC0a  <- read_csv(file=paste(mpath, "US-Ses_HH_201801010000_201901010000.csv", sep=""),
                      col_types = cols(TIMESTAMP_START = col_datetime("%Y %m %d %H %M"),
                                       TIMESTAMP_END = col_datetime("%Y %m %d %H %M")))

SES_EC0b  <- read_csv(file=paste(mpath, "US-Ses_HH_201901010000_202001010000.csv", sep=""),
                      col_types = cols(TIMESTAMP_START = col_datetime("%Y %m %d %H %M"),
                                       TIMESTAMP_END = col_datetime("%Y %m %d %H %M")))




# SEG_EC1  <- read_csv(file=paste(path, "2020_01_01_from_flash_Txcor_SEG_REC1_flux.csv", sep=""))
# head(SEG_EC1$'Date/Time')

SEG_EC1  <- read_csv(file=paste(path, "2020_01_01_from_flash_Txcor_SEG_REC1_flux.csv", sep=""),
                     col_types = cols('Date/Time' = col_datetime("%d/%m/%Y %H:%M")))
# head(SEG_EC1$'Date/Time')

#### ISSUE ####
# This is really strange - the date parsing seems intermittent! not sure why.
# different REC fields have different year formats

SEG_EC2  <- read_csv(file=paste(path, "2020_01_01_from_flash_Txcor_SEG_REC2_flux.csv", sep=""))
head(SEG_EC2$'Date/Time')

SEG_EC2  <- read_csv(file=paste(path, "2020_01_01_from_flash_Txcor_SEG_REC2_flux.csv", sep=""),
                     col_types = cols('Date/Time' = col_datetime("%d/%m/%y %H:%M")))
head(SEG_EC2$'Date/Time')



SEG_EC3  <- read_csv(file=paste(path, "2020_01_01_from_flash_Txcor_SEG_REC3_flux.csv", sep=""),
                     col_types = cols('Date/Time' = col_datetime("%d/%m/%Y %H:%M")))

SEG_EC4  <- read_csv(file=paste(path, "2020_01_01_from_flash_Txcor_SEG_REC4_flux.csv", sep=""),
                     col_types = cols('Date/Time' = col_datetime("%d/%m/%Y %H:%M")))

SES_EC1  <- read_csv(file=paste(path, "2020_01_01_from_flash_Txcor_SES_REC1_flux.csv", sep=""),
                     col_types = cols('Date/Time' = col_datetime("%d/%m/%Y %H:%M")))

SES_EC2  <- read_csv(file=paste(path, "2020_01_01_from_flash_Txcor_SES_REC2_flux.csv", sep=""),
                     col_types = cols('Date/Time' = col_datetime("%d/%m/%Y %H:%M")))

SES_EC3  <- read_csv(file=paste(path, "2020_01_01_from_flash_Txcor_SES_REC3_flux.csv", sep=""),
                     col_types = cols('Date/Time' = col_datetime("%d/%m/%Y %H:%M")))

SES_EC4  <- read_csv(file=paste(path, "2020_01_01_from_flash_Txcor_SES_REC4_flux.csv", sep=""),
                     col_types = cols('Date/Time' = col_datetime("%d/%m/%Y %H:%M")))
}  # Read data




#-------------- 2. Tidy data --------------

## Compile Marcy's files
SEG_EC0 <- dplyr::bind_rows(SEG_EC0a, SEG_EC0b); rm(SEG_EC0a, SEG_EC0b)
SES_EC0 <- dplyr::bind_rows(SES_EC0a, SES_EC0b); rm(SES_EC0a, SES_EC0b)



## Select variables of interest
## Marcy's data

# basic pipe. Works, but needs tuning in three places for each case, so would be better as a function
SEG_EC0_clean <- SEG_EC0 %>%
    select(TIMESTAMP_START,
           FC,
           LE,
           H,
           P_F, 
           SW_IN
           ) %>% 
  mutate(Station = "SEG_EC0") %>% 
  rename(Precipitation = P_F,
         Datetime_Start = TIMESTAMP_START)


# TO DO ----
# Functional programming
# In order to implement this as a function, I would like to return the df name, to use this to:
# (i) name the station, and (ii) name the output object
tidy_marcy <- function(df) {
  df %>%
    select(TIMESTAMP_START,
           FC,
           LE,
           H,
           P_F, 
           SW_IN
    ) %>% 
    mutate(Station = "SEG_EC0") %>%
    # mutate(Station = df) %>% 
    rename(Precipitation = P_F,
           Datetime_Start = TIMESTAMP_START)
  }

SEG_EC0_temp <- tidy_marcy(SEG_EC0)

# perhaps using assign?
# https://stackoverflow.com/questions/57593514/dynamic-naming-for-the-output-of-pipe-with-group-by-mutate-select?rq=1





 



# Align datetime with Lubridat::
# format(SEG_EC4$Date/Time[1], scientific = FALSE)
SEG_EC0_temp$Datetime_Start[3]
SEG_EC0_temp$Datetime_Start[4]



## Tidy REC data
SEG_EC1_clean <- SEG_EC1 %>%
  select("Date/Time",
         Fc,
         # Need to identify the correct LE!
         H
  ) %>% 
  mutate(Station = "SEG_EC1") %>% 
  rename(FC = Fc,
         Datetime_Start = "Date/Time") %>% 
  transmute(Datetime_Start = Datetime_Start - minutes(15))

head(SEG_EC1)
SEG_EC1$'Date/Time'[3]
SEG_EC1_clean$Datetime_Start[4]

SEG_EC1_clean$Datetime_Start[3]
SEG_EC1_clean$Datetime_Start[4]




# REC data
# Need to confirm whether I've selected the correct variables=
# rLE or cLR?
# what is 'cLEc'?

names(SEG_EC1)

str(SEG_EC0)
str(SEG_EC1)




# Need to subset time series to study period dates



# analyze EC0 versus EC1
# - inc. group by day


# combine data frames for plotting dplyr::bind_rows




## remove objects to clean environment
rm(SES_EC0, SEG_EC0a, SEG_EC0b,
   SEG_EC1, SEG_EC2, SEG_EC3, SEG_EC4,
   SES_EC0, SES_EC0a, SES_EC0b, 
   SES_EC1, SES_EC2, SES_EC3, SES_EC4
   )  


