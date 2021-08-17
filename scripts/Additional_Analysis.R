## This script sources gap filled data output from ReddyProc and AmeriFlux,
## merges them into a single file and performs visualization and data analysis



# ------ 0.0 Setup Environment ----------
## Load packages
library(tidyverse)
library(patchwork)   
library(tls)   


## Define paths
## NB. these data are ca. 110 GB

## Paths Andy's machine
# path  <-  "C:/workspace/REC_7_Data/8_datasets/"  # Unfilled EdiRe output
fpath  <-  "C:/workspace/REC_7_Data/11_ReddyProc/"  # Gap filled ReddyProc output
mpath  <-  "C:/workspace/REC_7_Data/12_Marcys_data/"


# Plotting theme
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
      legend.text = element_text(size = 10, color = "black"),
      legend.title = element_text(size = 10, color = "black"),
      legend.position = c(0.9, 0.9),
      legend.key.size = unit(0.9, "line"),
      legend.background = element_rect(
        color = "black",
        fill = "transparent",
        size = 2,
        linetype = "blank"
      )
    )
}



#-------------- 1. Read data --------------
{
# Note datetime  format differs (yyyy vs yy) between data files.
  
## Read data from Marcy's systems
DF_SEG_EC0a  <- read_csv(file=paste(mpath, "US-Seg_HH_201801010000_201901010000.csv", sep=""),
                      col_types = cols(TIMESTAMP_START = col_datetime("%Y %m %d %H %M"),
                                       TIMESTAMP_END = col_datetime("%Y %m %d %H %M")),
                      na = c("", "NA",-9999))

DF_SEG_EC0b  <- read_csv(file=paste(mpath, "US-Seg_HH_201901010000_202001010000.csv", sep=""),
                      col_types = cols(TIMESTAMP_START = col_datetime("%Y %m %d %H %M"),
                                       TIMESTAMP_END = col_datetime("%Y %m %d %H %M")),
                      na = c("", "NA",-9999))

DF_SES_EC0a  <- read_csv(file=paste(mpath, "US-Ses_HH_201801010000_201901010000.csv", sep=""),
                      col_types = cols(TIMESTAMP_START = col_datetime("%Y %m %d %H %M"),
                                       TIMESTAMP_END = col_datetime("%Y %m %d %H %M")),
                      na = c("", "NA",-9999))

DF_SES_EC0b  <- read_csv(file=paste(mpath, "US-Ses_HH_201901010000_202001010000.csv", sep=""),
                      col_types = cols(TIMESTAMP_START = col_datetime("%Y %m %d %H %M"),
                                       TIMESTAMP_END = col_datetime("%Y %m %d %H %M")),
                      na = c("", "NA",-9999))

## Unused Read unfilled EdiRe output
# DF_SEG_EC1  <- read_csv(file=paste(path, "2020_01_01_from_flash_Txcor_SEG_REC1_flux.csv", sep=""),
#                         col_types = cols('Date/Time' = col_datetime("%d/%m/%Y %H:%M")))
# 
# DF_SEG_EC2  <- read_csv(file=paste(path, "2020_01_01_from_flash_Txcor_SEG_REC2_flux.csv", sep=""),
#                         col_types = cols('Date/Time' = col_datetime("%d/%m/%y %H:%M:%S")))
# 
# DF_SEG_EC3  <- read_csv(file=paste(path, "2020_01_01_from_flash_Txcor_SEG_REC3_flux.csv", sep=""),
#                         col_types = cols('Date/Time' = col_datetime("%d/%m/%y %H:%M:%S")))
# 
# DF_SEG_EC4  <- read_csv(file=paste(path, "2020_01_01_from_flash_Txcor_SEG_REC4_flux.csv", sep=""),
#                         col_types = cols('Date/Time' = col_datetime("%d/%m/%y %H:%M:%S")))
# 
# DF_SES_EC1  <- read_csv(file=paste(path, "2020_01_01_from_flash_Txcor_SES_REC1_flux.csv", sep=""),
#                         col_types = cols('Date/Time' = col_datetime("%d/%m/%y %H:%M:%S")))
# 
# DF_SES_EC2  <- read_csv(file=paste(path, "2020_01_01_from_flash_Txcor_SES_REC2_flux.csv", sep=""),
#                         col_types = cols('Date/Time' = col_datetime("%d/%m/%Y %H:%M")))
# 
# DF_SES_EC3  <- read_csv(file=paste(path, "2020_01_01_from_flash_Txcor_SES_REC3_flux.csv", sep=""),
#                         col_types = cols('Date/Time' = col_datetime("%d/%m/%y %H:%M:%S")))
# 
# DF_SES_EC4  <- read_csv(file=paste(path, "2020_01_01_from_flash_Txcor_SES_REC4_flux.csv", sep=""),
#                         col_types = cols('Date/Time' = col_datetime("%d/%m/%y %H:%M:%S")))


## Read gap filled Reddy Proc output (tab delimited, read twice because the second row contains units)
## '_f' in variable names indicates gap filled.
df_names <- read_tsv(file=paste(fpath, "SEG_REC1_2020_50_filled_Txcor.txt", sep=""), n_max = 1) %>% names()
DF_SEG_EC1  <- read_tsv(file=paste(fpath, "SEG_REC1_2020_50_filled_Txcor.txt", sep=""), col_names = df_names, na = c("-9999"), skip=2, guess_max = min(10000))  # Needed to parse the first 10,000 records to ensure col types were correctl specified.

df_names <- read_tsv(file=paste(fpath, "SEG_REC2_2020_50_filled_Txcor.txt", sep=""), n_max = 1) %>% names()
DF_SEG_EC2  <- read_tsv(file=paste(fpath, "SEG_REC2_2020_50_filled_Txcor.txt", sep=""), col_names = df_names, na = c("-9999"), skip=2, guess_max = min(10000))  # Needed to parse the first 10,000 records to ensure col types were correctl specified.

df_names <- read_tsv(file=paste(fpath, "SEG_REC2_2020_50_filled_Txcor.txt", sep=""), n_max = 1) %>% names()
DF_SEG_EC2  <- read_tsv(file=paste(fpath, "SEG_REC2_2020_50_filled_Txcor.txt", sep=""), col_names = df_names, na = c("-9999"), skip=2, guess_max = min(10000))  # Needed to parse the first 10,000 records to ensure col types were correctl specified.

df_names <- read_tsv(file=paste(fpath, "SEG_REC3_2020_50_filled_Txcor.txt", sep=""), n_max = 1) %>% names()
DF_SEG_EC3  <- read_tsv(file=paste(fpath, "SEG_REC3_2020_50_filled_Txcor.txt", sep=""), col_names = df_names, na = c("-9999"), skip=2, guess_max = min(10000))  # Needed to parse the first 10,000 records to ensure col types were correctl specified.

df_names <- read_tsv(file=paste(fpath, "SEG_REC4_2020_50_filled_Txcor.txt", sep=""), n_max = 1) %>% names()
DF_SEG_EC4  <- read_tsv(file=paste(fpath, "SEG_REC4_2020_50_filled_Txcor.txt", sep=""), col_names = df_names, na = c("-9999"), skip=2, guess_max = min(10000))  # Needed to parse the first 10,000 records to ensure col types were correctl specified.

df_names <- read_tsv(file=paste(fpath, "SES_REC1_2020_50_filled_Txcor.txt", sep=""), n_max = 1) %>% names()
DF_SES_EC1  <- read_tsv(file=paste(fpath, "SES_REC1_2020_50_filled_Txcor.txt", sep=""), col_names = df_names, na = c("-9999"), skip=2, guess_max = min(10000))  # Needed to parse the first 10,000 records to ensure col types were correctl specified.

df_names <- read_tsv(file=paste(fpath, "SES_REC2_2020_50_filled_Txcor.txt", sep=""), n_max = 1) %>% names()
DF_SES_EC2  <- read_tsv(file=paste(fpath, "SES_REC2_2020_50_filled_Txcor.txt", sep=""), col_names = df_names, na = c("-9999"), skip=2, guess_max = min(10000))  # Needed to parse the first 10,000 records to ensure col types were correctl specified.

df_names <- read_tsv(file=paste(fpath, "SES_REC3_2020_50_filled_Txcor.txt", sep=""), n_max = 1) %>% names()
DF_SES_EC3  <- read_tsv(file=paste(fpath, "SES_REC3_2020_50_filled_Txcor.txt", sep=""), col_names = df_names, na = c("-9999"), skip=2, guess_max = min(10000))  # Needed to parse the first 10,000 records to ensure col types were correctl specified.

df_names <- read_tsv(file=paste(fpath, "SES_REC4_2020_50_filled_Txcor.txt", sep=""), n_max = 1) %>% names()
DF_SES_EC4  <- read_tsv(file=paste(fpath, "SES_REC4_2020_50_filled_Txcor.txt", sep=""), col_names = df_names, na = c("-9999"), skip=2, guess_max = min(10000))  # Needed to parse the first 10,000 records to ensure col types were correctl specified.

rm(df_names)
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


## Create functions to rename and select variables of interest 
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
  }  # Tidy Marcy's data

## REC data
tidy_RECs <- function(df, station) {
  df %>%
    mutate(
      Station = station,
      Datetime_Start = lubridate::make_datetime(year = Year, min = round(Hour*60)) + lubridate::days(DoY-1)
      ) %>%
    select(
      Datetime_Start,
      NEE_uStar_f,
      LE_f,
      H_f,
      Station) %>% 
    rename(
      FC = NEE_uStar_f,
      LE = LE_f,
      H = H_f
      )
  }  # Tidy REC data




## Use purrr::map2 to process multiple tibbles through pipe and return a single merged dataframe
DF_EC_Marcy <- list1a %>%
  purrr::map2(.x =., .y=list1b, ~tidy_marcy(.x, .y)) %>%
  bind_rows()

DF_EC_REC <- list2a %>%
  purrr::map2(.x =., .y=list2b, ~tidy_RECs(.x, .y)) %>%
  bind_rows()

## NB. If we need to handle more than two lists, use purrr::pmap
## Example:
# my_list <- list(df_list, name_list, other_varlist)
# purrr::pmap(my_list, ~myfunction(..1, ..2, ..3))

# merge data from all stations
DF_EC <- dplyr::bind_rows(DF_EC_Marcy, DF_EC_REC) 

# remove unnecessary objects
rm(list1a, list1b, list2a, list2b,
   DF_SEG_EC0, DF_SES_EC0,
   DF_SEG_EC1, DF_SEG_EC2, DF_SEG_EC3, DF_SEG_EC4, DF_SES_EC1, DF_SES_EC2, DF_SES_EC3, DF_SES_EC4,
   DF_EC_Marcy, DF_EC_REC)









#-------------- 2. Analyze data --------------
#-------------- 2.1 Co-location comparison --------------


# Create function to facilitate versatile comparisons of EC0 with EC1
comparison_EC1_vs_EC0 <- function(df,
                                  beginning,
                                  ending,
                                  resolution) {
  df %>%
    # Subset time series to study period dates
    filter(between(Datetime_Start,
                   as.POSIXct(beginning),
                   as.POSIXct(ending))) %>% 
    
    # Resample time series to specified temporal resolution
    
    ### WHERE TO PASS IN THE TIME REOSLUTION VARIABLE? 
    ### COULD THIS BE AS THE FORMAT?? 
    ### "YYYY-M", or "Y-m-d", or "y-m-d h" I think this would work
    ### WHAT RANGE OF ARGUMENTS ARE allowed FOR RESOLUTION?
    ### NOTE THIS ISN'T RIGHT! 
    ### THIS SIMPLY RESAMPLES TO DAY. should use As.datetime!:(
    
    # create new datetime variable at the specified temproal resolution for resampling
    mutate(Datetime_Start_res = as.Date(Datetime_Start, format = "%Y-%m-%d")) %>% 
    
    # group by the resolution column
    group_by(Datetime_Start_res) %>% 
    # calculate the SUM of each flux per unit time
    summarise(FC_res = sum(FC),
              LE_res = sum(LE),
              H_res = sum(H),
              Precipitation_res = sum(Precipitation),
              SW_IN_res = sum(SW_IN)
              # Station = Station
              )  
  ### BUT VITAL TO NEED TO KEEP STATION SOMEHOW... 
  


}  # comparison_EC1_vs_EC0 function

names(DF_EC)
head(testing)

#  filters data creates plots and saves output. 
# Not yet implemented

testing <- comparison_EC1_vs_EC0(DF_EC,
                         "2018-11-01 00:00:00",
                         "2019-11-01 23:30:00",
                         1)

plot(testing$FC_res)

#   
#   
#   # pivot data from long to wide form
# create function pivoting data from long to wide form for analysis
# shape_wider <- function(df, variable) {
#   df %>%
#     select(Datetime_Start, variable, Station) %>%
#     pivot_wider(names_from = Station,
#                 values_from = variable)
# }  #
# 
# # pivot data to wide form
# DF_EC_study_H <- shape_wider(DF_EC_study, "H")
# DF_EC_study_LE <- shape_wider(DF_EC_study, "LE")
# DF_EC_study_FC <- shape_wider(DF_EC_study, "FC")





### Generate half-hourly comparative plots (EC0 versus EC1)

{
  
  # Subset time series to study period dates
  DF_EC_study <- DF_EC %>%
    filter(between(Datetime_Start,
                   as.POSIXct("2018-11-01 00:00:00"),
                   as.POSIXct("2019-10-31 23:30:00")))

  
  
  
# create function pivoting data from long to wide form for analysis
shape_wider <- function(df, variable) {
  df %>%
    select(Datetime_Start, variable, Station) %>%
    pivot_wider(names_from = Station,
                values_from = variable)
  }  #

# pivot data to wide form
DF_EC_study_H <- shape_wider(DF_EC_study, "H")
DF_EC_study_LE <- shape_wider(DF_EC_study, "LE")
DF_EC_study_FC <- shape_wider(DF_EC_study, "FC")

# Determine axis limits
lims_h  <- range(DF_EC_study_H[, c("SEG_EC0",  "SEG_EC1",  "SES_EC0",  "SES_EC1")],  na.rm=T)
lims_le  <- range(DF_EC_study_LE[, c("SEG_EC0",  "SEG_EC1",  "SES_EC0",  "SES_EC1")],  na.rm=T)
lims_fc  <- range(DF_EC_study_FC[, c("SEG_EC0",  "SEG_EC1",  "SES_EC0",  "SES_EC1")],  na.rm=T)

# H / US-Seg
{

# Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
pca <- prcomp(~SEG_EC0+SEG_EC1, DF_EC_study_H)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept

equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute Pearson correlation coefficient
r <- cor(DF_EC_study_H$SEG_EC0, DF_EC_study_H$SEG_EC1, method="pearson", use = "complete.obs")
r <- paste("r: ", round(r,2))

# create plot
seg_h <- ggplot(DF_EC_study_H, aes(x=SEG_EC0, y=SEG_EC1)) +
    labs(x = expression("Seg EC0 - H (W m"^"-2"*")"),
         y = expression("Seg EC1 - H (W m"^"-2"*")"),
         title = "Sensible Heat Flux - Seg") +
    geom_bin2d(bins = 150, show.legend=T) +   # Bin size control
    scale_fill_continuous(type = "viridis") +     # colour palette
    xlim(lims_h) +
    ylim(lims_h) +
    theme_fancy() +
    theme(plot.title = element_text(size = 14, face = "bold")) +
    theme(legend.position = c(0.85, 0.25)) + # legend position
    geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
  geom_abline(intercept = tls_int, slope = tls_slp) +
  annotate("text", x = 65, y = 500, label = equation, size=4) +
  annotate("text", x = -100, y = 420, label = r, size=4)

} # H / US-Seg


# H / US-Ses
{
  # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
  pca <- prcomp(~SES_EC0+SES_EC1, DF_EC_study_H)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute Pearson correlation coefficient
  r <- cor(DF_EC_study_H$SES_EC0, DF_EC_study_H$SES_EC1, method="pearson", use = "complete.obs")
  r <- paste("r: ", round(r,2))
  
  # create plot
  ses_h <- ggplot(DF_EC_study_H, aes(x=SES_EC0, y=SES_EC1)) +
    labs(x = expression("Ses EC0 - H (W m"^"-2"*")"),
         y = expression("Ses EC1 - H (W m"^"-2"*")"),
         title = "Sensible Heat Flux - Ses") +
    geom_bin2d(bins = 150, show.legend=T) +   # Bin size control
    scale_fill_continuous(type = "viridis") +     # colour palette
    xlim(lims_h) +
    ylim(lims_h) +
    theme_fancy() +
    theme(plot.title = element_text(size = 14, face = "bold")) +
    theme(legend.position = c(0.85, 0.25)) + # legend position
    geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
    geom_abline(intercept = tls_int, slope = tls_slp) +
    annotate("text", x = 65, y = 500, label = equation, size=4) +
    annotate("text", x = -100, y = 420, label = r, size=4)
  
} # H / US-Ses


# LE / US-Seg
{
  # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
  pca <- prcomp(~SEG_EC0+SEG_EC1, DF_EC_study_LE)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute Pearson correlation coefficient
  r <- cor(DF_EC_study_LE$SEG_EC0, DF_EC_study_LE$SEG_EC1, method="pearson", use = "complete.obs")
  r <- paste("r: ", round(r,2))
  
  # create plot
  seg_le <- ggplot(DF_EC_study_LE, aes(x=SEG_EC0, y=SEG_EC1)) +
    labs(x = expression("Seg EC0 - LE (W m"^"-2"*")"),
         y = expression("Seg EC1 - LE (W m"^"-2"*")"),
         title = "Latent Heat Flux - Seg") +
    geom_bin2d(bins = 150, show.legend=T) +   # Bin size control
    scale_fill_continuous(type = "viridis") +     # colour palette
    xlim(lims_le) +
    ylim(lims_le) +
    theme_fancy() +
    theme(plot.title = element_text(size = 14, face = "bold")) +
    theme(legend.position = c(0.85, 0.25)) + # legend position
    geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
    geom_abline(intercept = tls_int, slope = tls_slp) +
    annotate("text", x = 100, y = 430, label = equation, size=4) +
    annotate("text", x = 0, y = 380, label = r, size=4)
  
} # LE / US-Seg


# LE / US-Ses
{
  # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
  pca <- prcomp(~SES_EC0+SES_EC1, DF_EC_study_LE)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute Pearson correlation coefficient
  r <- cor(DF_EC_study_LE$SES_EC0, DF_EC_study_LE$SES_EC1, method="pearson", use = "complete.obs")
  r <- paste("r: ", round(r,2))
  
  # create plot
  ses_le <- ggplot(DF_EC_study_LE, aes(x=SES_EC0, y=SES_EC1)) +
    labs(x = expression("Ses EC0 - LE (W m"^"-2"*")"),
         y = expression("Ses EC1 - LE (W m"^"-2"*")"),
         title = "Latent Heat Flux - Ses") +
    geom_bin2d(bins = 150, show.legend=T) +   # Bin size control
    scale_fill_continuous(type = "viridis") +     # colour palette
    xlim(lims_le) +
    ylim(lims_le) +
    theme_fancy() +
    theme(plot.title = element_text(size = 14, face = "bold")) +
    theme(legend.position = c(0.85, 0.25)) + # legend position
    geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
    geom_abline(intercept = tls_int, slope = tls_slp) +
    annotate("text", x = 100, y = 430, label = equation, size=4) +
    annotate("text", x = 0, y = 380, label = r, size=4)
  
} # LE / US-Ses


# NEE / US-Seg
{
  # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
  pca <- prcomp(~SEG_EC0+SEG_EC1, DF_EC_study_FC)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute Pearson correlation coefficient
  r <- cor(DF_EC_study_FC$SEG_EC0, DF_EC_study_FC$SEG_EC1, method="pearson", use = "complete.obs")
  r <- paste("r: ", round(r,2))
  
  # create plot
  seg_fc <- ggplot(DF_EC_study_FC, aes(x=SEG_EC0, y=SEG_EC1)) +
    labs(x = expression("Seg EC0 - NEE (umol CO"[2]*" m"^"-2"*"s"^"-1"*")"),
         y = expression("Seg EC1 - NEE (umol CO"[2]*" m"^"-2"*"s"^"-1"*")"),
         title = "Net Ecosys. Exchange - Ses") +
    geom_bin2d(bins = 150, show.legend=T) +   # Bin size control
    scale_fill_continuous(type = "viridis") +     # colour palette
    xlim(lims_fc) +
    ylim(lims_fc) +
    theme_fancy() +
    theme(plot.title = element_text(size = 14, face = "bold")) +
    theme(legend.position = c(0.85, 0.25)) + # legend position
    geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
    geom_abline(intercept = tls_int, slope = tls_slp) +
    annotate("text", x = -5, y = 7.5, label = equation, size=4) +
    annotate("text", x = -8.5, y = 5.5, label = r, size=4)
  
} # NEE / US-Seg


# NEE / US-Ses
{
  # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
  pca <- prcomp(~SES_EC0+SES_EC1, DF_EC_study_FC)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute Pearson correlation coefficient
  r <- cor(DF_EC_study_FC$SES_EC0, DF_EC_study_FC$SES_EC1, method="pearson", use = "complete.obs")
  r <- paste("r: ", round(r,2))
  
  # create plot
  ses_fc <- ggplot(DF_EC_study_FC, aes(x=SES_EC0, y=SES_EC1)) +
    labs(x = expression("Ses EC0 - NEE (umol CO"[2]*" m"^"-2"*"s"^"-1"*")"),
         y = expression("Ses EC1 - NEE (umol CO"[2]*" m"^"-2"*"s"^"-1"*")"),
         title = "Net Ecosys. Exchange - Ses") +
    geom_bin2d(bins = 150, show.legend=T) +   # Bin size control
    scale_fill_continuous(type = "viridis") +     # colour palette
    xlim(lims_fc) +
    ylim(lims_fc) +
    theme_fancy() +
    theme(plot.title = element_text(size = 14, face = "bold")) +
    theme(legend.position = c(0.85, 0.25)) + # legend position
    geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
    geom_abline(intercept = tls_int, slope = tls_slp) +
    annotate("text", x = -5, y = 7.5, label = equation, size=4) +
    annotate("text", x = -9, y = 5.5, label = r, size=4)
  
} # NEE / US-Ses



### combine plot objects with Patchwork
pall <- (seg_h + ses_h) / (seg_le + ses_le) / (seg_fc + ses_fc) +
  plot_annotation(tag_levels = 'a') & theme(plot.tag.position = c(0.0, 0.97))

filename = "plots/EC0 vs EC1 comparison - half hourly.png"

ggsave(
  pall,
  filename = filename,
  width = 17,
  height = 26,
  units = "cm"
  )
} # Generate half-hourly comparative plots (EC0 versus EC1)


 

# - inc. group by day and also by month


# 
# 
# # 
# # FC, LE, 
# # filter(Station %in% c("SEG_EC0", "SEG_EC1")) %>% 
# # group_by(Station) %>%
# # group_split() 
# 
# # I need to remember how to use the 'Station' vector 
# # with 'SEG_EC0' and 'SEG_EC1', to split the comparison!
# # isn't there a more efficient way than filtering 
# # to create new objects!?!
# 
# # Think about how to do this better
# # maybe with a function (calling col names and site code)
# EC0_vs_EC1_SEG <- DF_EC_study %>%
#   select(Datetime_Start, FC, LE, H, Station) %>%
#   filter(Station %in% c("SEG_EC0", "SEG_EC1")) %>% 
#   group_by(Station) %>%
#   group_split() 
# 
# test3 <- right_join(EC0_vs_EC1_SEG[[1]], EC0_vs_EC1_SEG[[2]], by="Datetime_Start")

# 
