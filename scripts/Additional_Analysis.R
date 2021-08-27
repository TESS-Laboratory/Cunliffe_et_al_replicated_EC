## This script sources gap filled data output from ReddyProc and AmeriFlux,
## merges them into a single file and performs visualization and data analysis



# ------ 0.0 Setup Environment ----------
## Load packages
library(tidyverse)
library(patchwork)   
library(tls)   


## Define paths
## NB. these data are ca. 150 GB

## Paths Andy's machine
# path  <-  "C:/workspace/REC_7_Data/8_datasets/"  # Unfilled EdiRe output
fpath  <-  "C:/workspace/REC_7_Data/11_ReddyProc/"  # Gap filled ReddyProc output
mpath  <-  "C:/workspace/REC_7_Data/12_Marcys_data/"


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
windowsFonts("Helvetica" = windowsFont("Helvetica"))  # Ensure font is mapped correctly


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

DF_SEG_EC0c  <- read_csv(file=paste(mpath, "US-Seg_HH_202001010000_202101010000.csv", sep=""),
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

DF_SES_EC0c  <- read_csv(file=paste(mpath, "US-Ses_HH_202001010000_202101010000.csv", sep=""),
                         col_types = cols(TIMESTAMP_START = col_datetime("%Y %m %d %H %M"),
                                          TIMESTAMP_END = col_datetime("%Y %m %d %H %M")),
                         na = c("", "NA",-9999))


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
DF_SEG_EC0 <- dplyr::bind_rows(DF_SEG_EC0a, DF_SEG_EC0b, DF_SEG_EC0c); rm(DF_SEG_EC0a, DF_SEG_EC0b, DF_SEG_EC0c)
DF_SES_EC0 <- dplyr::bind_rows(DF_SES_EC0a, DF_SES_EC0b, DF_SES_EC0c); rm(DF_SES_EC0a, DF_SES_EC0b, DF_SES_EC0c)


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
    rename(NEE = FC) %>% 
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
      NEE,
      NEE_uStar_f,
      LE,
      LE_f,
      H,
      H_f,
      Station)
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


# Subset time series to study period dates
DF_EC_study <- DF_EC %>%
  filter(between(Datetime_Start,
                 as.POSIXct("2018-11-01 00:00:00"),
                 as.POSIXct("2020-10-31 23:30:00")))


# create function pivoting data from long to wide form for analysis
shape_wider <- function(df, variable) {
  df %>%
    select(Datetime_Start, variable, Station) %>%
    pivot_wider(names_from = Station,
                values_from = variable)
}  #

# pivot data to wide form
{
  DF_EC_study_H <- shape_wider(DF_EC_study, "H")
  DF_EC_study_LE <- shape_wider(DF_EC_study, "LE")
  DF_EC_study_NEE <- shape_wider(DF_EC_study, "NEE")
}



#-------------- 3. Analyze data --------------

#-------------- 3.1 Co-location comparison --------------


### Half-hourly comparison (EC0 versus EC1)----
{
## Determine axis limits
lims_h  <- range(DF_EC_study_H[, c("SEG_EC0",  "SEG_EC1",  "SES_EC0",  "SES_EC1")],  na.rm=T)
lims_le  <- range(DF_EC_study_LE[, c("SEG_EC0",  "SEG_EC1",  "SES_EC0",  "SES_EC1")],  na.rm=T)
lims_nee  <- range(DF_EC_study_NEE[, c("SEG_EC0",  "SEG_EC1",  "SES_EC0",  "SES_EC1")],  na.rm=T)

### H
{
## H / US-Seg
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
  geom_hex(bins = 100, show.legend=T) +
  # scale_fill_continuous(type = "viridis")) +     # colour palette
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


## H / US-Ses
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
    geom_hex(bins = 100, show.legend=T) +
    # scale_fill_continuous(type = "viridis") +     # colour palette
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

# Calculate the range of values in both plots for consistent scalars
count.range = range(lapply(list(seg_h, ses_h), function(p){ ggplot_build(p)$data[[1]]$count}))
seg_h <- seg_h +   scale_fill_continuous(type = "viridis", limits=c(count.range))    # colour palette
ses_h <- ses_h +   scale_fill_continuous(type = "viridis", limits=c(count.range))    # colour palette

}

### LE
{
## LE / US-Seg
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
    geom_hex(bins = 100, show.legend=T) +
    # scale_fill_continuous(type = "viridis") +     # colour palette
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
    geom_hex(bins = 100, show.legend=T) +
    # scale_fill_continuous(type = "viridis") +     # colour palette
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

# Calculate the range of values in both plots for consistent scalars
count.range = range(lapply(list(seg_le, ses_le), function(p){ ggplot_build(p)$data[[1]]$count}))
seg_le <- seg_le +   scale_fill_continuous(type = "viridis", limits=c(count.range))    # colour palette
ses_le <- ses_le +   scale_fill_continuous(type = "viridis", limits=c(count.range))    # colour palette
  
}

  
### NEE
{
## NEE / US-Seg
{
  # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
  pca <- prcomp(~SEG_EC0+SEG_EC1, DF_EC_study_NEE)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute Pearson correlation coefficient
  r <- cor(DF_EC_study_NEE$SEG_EC0, DF_EC_study_NEE$SEG_EC1, method="pearson", use = "complete.obs")
  r <- paste("r: ", round(r,2))
  
  # create plot
  seg_nee <- ggplot(DF_EC_study_NEE, aes(x=SEG_EC0, y=SEG_EC1)) +
    labs(
      x = expression(paste("Ses EC0 - NEE (", mu, "mol CO"[2]*" m"^"-2"*"s"^"-1"*")")),
      y = expression(paste("Ses EC1 - NEE (", mu, "mol CO"[2]*" m"^"-2"*"s"^"-1"*")")),
      title = "Net Ecosys. Exchange - Ses") +
    geom_hex(bins = 100, show.legend=T) +
    # scale_fill_continuous(type = "viridis") +     # colour palette
    xlim(lims_nee) +
    ylim(lims_nee) +
    theme_fancy() +
    theme(plot.title = element_text(size = 14, face = "bold")) +
    theme(legend.position = c(0.85, 0.25)) + # legend position
    geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
    geom_abline(intercept = tls_int, slope = tls_slp) +
    annotate("text", x = -5, y = 7.5, label = equation, size=4) +
    annotate("text", x = -8.5, y = 5.5, label = r, size=4)
  
} # NEE / US-Seg


## NEE / US-Ses
{
  # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
  pca <- prcomp(~SES_EC0+SES_EC1, DF_EC_study_NEE)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute Pearson correlation coefficient
  r <- cor(DF_EC_study_NEE$SES_EC0, DF_EC_study_NEE$SES_EC1, method="pearson", use = "complete.obs")
  r <- paste("r: ", round(r,2))
  
  # create plot
  ses_nee <- ggplot(DF_EC_study_NEE, aes(x=SES_EC0, y=SES_EC1)) +
    labs(
      x = expression(paste("Ses EC0 - NEE (", mu, "mol CO"[2]*" m"^"-2"*"s"^"-1"*")")),
      y = expression(paste("Ses EC1 - NEE (", mu, "mol CO"[2]*" m"^"-2"*"s"^"-1"*")")),
      title = "Net Ecosys. Exchange - Ses") +
    geom_hex(bins = 100, show.legend=T) +
    # scale_fill_continuous(type = "viridis") +     # colour palette
    xlim(lims_nee) +
    ylim(lims_nee) +
    theme_fancy() +
    theme(plot.title = element_text(size = 14, face = "bold")) +
    theme(legend.position = c(0.85, 0.25)) + # legend position
    geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
    geom_abline(intercept = tls_int, slope = tls_slp) +
    annotate("text", x = -5, y = 7.5, label = equation, size=4) +
    annotate("text", x = -9, y = 5.5, label = r, size=4)
  
} # NEE / US-Ses

# Calculate the range of values in both plots for consistent scalars
count.range = range(lapply(list(seg_nee, ses_nee), function(p){ ggplot_build(p)$data[[1]]$count}))
seg_nee <- seg_nee +   scale_fill_continuous(type = "viridis", limits=c(count.range))    # colour palette
ses_nee <- ses_nee +   scale_fill_continuous(type = "viridis", limits=c(count.range))    # colour palette
  
}



### combine plot objects with Patchwork
pall <- (seg_h + ses_h) / (seg_le + ses_le) / (seg_nee + ses_nee) +
  plot_annotation(tag_levels = 'a') & theme(plot.tag.position = c(0.0, 0.97))

filename = "plots/EC0 vs EC1 comparison - half hourly"

ggsave(
  pall,
  filename = paste0(filename, ".png"),
  width = 17,
  height = 26,
  units = "cm"
)

ggsave(
  pall,
  filename = paste0(filename, ".pdf"),
  width = 17,
  height = 26,
  units = "cm"
)

} # Generate half-hourly comparative plots (EC0 versus EC1)



### Daily comparison (EC0 versus EC1) ----
{
  ## Resample time series to desired temporal resolution
    ## Resample H
     DF_EC_study_H_SEG_daily <- DF_EC_study_H %>%
       mutate(Datetime_Start_res = as.Date(Datetime_Start, format = "%Y-%m-%d")) %>% 
       group_by(Datetime_Start_res) %>%
       select(SEG_EC0,
              SEG_EC1) %>% 
       na.omit %>%  # drop incomplete half hours
       summarise(
         SEG_EC0 = mean(SEG_EC0),
         SEG_EC1 = mean(SEG_EC1)
         )

     DF_EC_study_H_SES_daily <- DF_EC_study_H %>%
       mutate(Datetime_Start_res = as.Date(Datetime_Start, format = "%Y-%m-%d")) %>% 
       group_by(Datetime_Start_res) %>%
       select(SES_EC0,
              SES_EC1) %>% 
       na.omit %>%  # drop incomplete half hours
       summarise(
         SES_EC0 = mean(SES_EC0),
         SES_EC1 = mean(SES_EC1)
       )
     
     
     ## Resample LE
     DF_EC_study_LE_SEG_daily <- DF_EC_study_LE %>%
       mutate(Datetime_Start_res = as.Date(Datetime_Start, format = "%Y-%m-%d")) %>% 
       group_by(Datetime_Start_res) %>%
       select(SEG_EC0,
              SEG_EC1) %>% 
       na.omit %>%  # drop incomplete half hours
       summarise(
         SEG_EC0 = mean(SEG_EC0),
         SEG_EC1 = mean(SEG_EC1)
       )   
   
     DF_EC_study_LE_SES_daily <- DF_EC_study_LE %>%
       mutate(Datetime_Start_res = as.Date(Datetime_Start, format = "%Y-%m-%d")) %>% 
       group_by(Datetime_Start_res) %>%
       select(SES_EC0,
              SES_EC1) %>% 
       na.omit %>%  # drop incomplete half hours
       summarise(
         SES_EC0 = mean(SES_EC0),
         SES_EC1 = mean(SES_EC1)
       )   
     
     
     ## Resample NEE 
     DF_EC_study_NEE_SEG_daily <- DF_EC_study_NEE %>%
       mutate(Datetime_Start_res = as.Date(Datetime_Start, format = "%Y-%m-%d")) %>% 
       group_by(Datetime_Start_res) %>%
       select(SEG_EC0,
              SEG_EC1) %>% 
       na.omit %>%  # drop incomplete half hours
       summarise(
         SEG_EC0 = mean(SEG_EC0),
         SEG_EC1 = mean(SEG_EC1)
       )
    
     DF_EC_study_NEE_SES_daily <- DF_EC_study_NEE %>%
       mutate(Datetime_Start_res = as.Date(Datetime_Start, format = "%Y-%m-%d")) %>% 
       group_by(Datetime_Start_res) %>%
       select(SES_EC0,
              SES_EC1) %>% 
       na.omit %>%  # drop incomplete half hours
       summarise(
         SES_EC0 = mean(SES_EC0),
         SES_EC1 = mean(SES_EC1)
       )
     
   
  ## Determine axis limits
  lims_h  <- range(range(DF_EC_study_H_SEG_daily[, c("SEG_EC0",  "SEG_EC1")]), range(DF_EC_study_H_SES_daily[, c("SES_EC0",  "SES_EC1")]))
  lims_le  <- range(range(DF_EC_study_LE_SEG_daily[, c("SEG_EC0",  "SEG_EC1")]), range(DF_EC_study_LE_SES_daily[, c("SES_EC0",  "SES_EC1")]))
  lims_nee  <- range(range(DF_EC_study_NEE_SEG_daily[, c("SEG_EC0",  "SEG_EC1")]), range(DF_EC_study_NEE_SES_daily[, c("SES_EC0",  "SES_EC1")]))

  ### H
  {
  ## H / US-Seg
  {
    # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
    pca <- prcomp(~SEG_EC0+SEG_EC1, DF_EC_study_H_SEG_daily)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # Compute Pearson correlation coefficient
    r <- cor(DF_EC_study_H_SEG_daily$SEG_EC0, DF_EC_study_H_SEG_daily$SEG_EC1, method="pearson", use = "complete.obs")
    r <- paste("r: ", round(r,2))
    
    # create plot
    seg_h <- ggplot(DF_EC_study_H_SEG_daily, aes(x=SEG_EC0, y=SEG_EC1)) +
      labs(x = expression("Seg EC0 - H (W m"^"-2"*")"),
           y = expression("Seg EC1 - H (W m"^"-2"*")"),
           title = "Sensible Heat Flux - Seg") +
      # geom_point(shape=1) +
      geom_hex(bins = 50, show.legend=T) +
      # scale_fill_continuous(type = "viridis") +     # colour palette
      xlim(lims_h) +
      ylim(lims_h) +
      theme_fancy() +
      theme(plot.title = element_text(size = 14, face = "bold")) +
      theme(legend.position = c(0.85, 0.25)) + # legend position
      geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
      geom_abline(intercept = tls_int, slope = tls_slp) +
      annotate("text", x = 77, y = (lims_h[1] + 0.99 * abs(lims_h[1] - lims_h[2])), label = equation, size=4) +
      annotate("text", x = -14, y = (lims_h[1] + 0.90 * abs(lims_h[1] - lims_h[2])), label = r, size=4)

  } # H / US-Seg
  
  
  # H / US-Ses
  {
    # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
    pca <- prcomp(~SES_EC0+SES_EC1, DF_EC_study_H_SES_daily)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # Compute Pearson correlation coefficient
    r <- cor(DF_EC_study_H_SES_daily$SES_EC0, DF_EC_study_H_SES_daily$SES_EC1, method="pearson", use = "complete.obs")
    r <- paste("r: ", round(r,2))
    
    # create plot
    ses_h <- ggplot(DF_EC_study_H_SES_daily, aes(x=SES_EC0, y=SES_EC1)) +
      labs(x = expression("Ses EC0 - H (W m"^"-2"*")"),
           y = expression("Ses EC1 - H (W m"^"-2"*")"),
           title = "Sensible Heat Flux - Ses") +
      # geom_point(shape=1) +
      geom_hex(bins = 50, show.legend=T) +
      # scale_fill_continuous(type = "viridis") +     # colour palette
      xlim(lims_h) +
      ylim(lims_h) +
      theme_fancy() +
      theme(plot.title = element_text(size = 14, face = "bold")) +
      theme(legend.position = c(0.85, 0.25)) + # legend position
      geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
      geom_abline(intercept = tls_int, slope = tls_slp) +
      annotate("text", x = 77, y = (lims_h[1] + 0.99 * abs(lims_h[1] - lims_h[2])), label = equation, size=4) +
      annotate("text", x = -14, y = (lims_h[1] + 0.90 * abs(lims_h[1] - lims_h[2])), label = r, size=4)
    
  } # H / US-Ses
  
    # Calculate the range of values in both plots for consistent scalars
    count.range = range(lapply(list(seg_h, ses_h), function(p){ ggplot_build(p)$data[[1]]$count}))
    seg_h <- seg_h +   scale_fill_continuous(type = "viridis", limits=c(count.range))    # colour palette
    ses_h <- ses_h +   scale_fill_continuous(type = "viridis", limits=c(count.range))    # colour palette
    
  }
  
 
 ### LE
  {
  
  # LE / US-Seg
  {
    # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
    pca <- prcomp(~SEG_EC0+SEG_EC1, DF_EC_study_LE_SEG_daily)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # Compute Pearson correlation coefficient
    r <- cor(DF_EC_study_LE_SEG_daily$SEG_EC0, DF_EC_study_LE_SEG_daily$SEG_EC1, method="pearson", use = "complete.obs")
    r <- paste("r: ", round(r,2))
    
    # create plot
    seg_le <- ggplot(DF_EC_study_LE_SEG_daily, aes(x=SEG_EC0, y=SEG_EC1)) +
      labs(x = expression("Seg EC0 - LE (W m"^"-2"*")"),
           y = expression("Seg EC1 - LE (W m"^"-2"*")"),
           title = "Latent Heat Flux - Seg") +
      # geom_point(shape=1) +
      geom_hex(bins = 50, show.legend=T) +
      # scale_fill_continuous(type = "viridis") +     # colour palette
      xlim(lims_le) +
      ylim(lims_le) +
      theme_fancy() +
      theme(plot.title = element_text(size = 14, face = "bold")) +
      theme(legend.position = c(0.85, 0.25)) + # legend position
      geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
      geom_abline(intercept = tls_int, slope = tls_slp) +
      annotate("text", x = 36, y = (lims_le[1] + 0.99 * abs(lims_le[1] - lims_le[2])), label = equation, size=4) +
      annotate("text", x = 10, y = (lims_le[1] + 0.90 * abs(lims_le[1] - lims_le[2])), label = r, size=4)
    
  } # LE / US-Seg
  
  
  # LE / US-Ses
  {
    # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
    pca <- prcomp(~SES_EC0+SES_EC1, DF_EC_study_LE_SES_daily)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # Compute Pearson correlation coefficient
    r <- cor(DF_EC_study_LE_SES_daily$SES_EC0, DF_EC_study_LE_SES_daily$SES_EC1, method="pearson", use = "complete.obs")
    r <- paste("r: ", round(r,2))
    
    # create plot
    ses_le <- ggplot(DF_EC_study_LE_SES_daily, aes(x=SES_EC0, y=SES_EC1)) +
      labs(x = expression("Ses EC0 - LE (W m"^"-2"*")"),
           y = expression("Ses EC1 - LE (W m"^"-2"*")"),
           title = "Latent Heat Flux - Ses") +
      # geom_point(shape=1) +
      geom_hex(bins = 50, show.legend=T) +
      # scale_fill_continuous(type = "viridis") +     # colour palette
      xlim(lims_le) +
      ylim(lims_le) +
      theme_fancy() +
      theme(plot.title = element_text(size = 14, face = "bold")) +
      theme(legend.position = c(0.85, 0.25)) + # legend position
      geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
      geom_abline(intercept = tls_int, slope = tls_slp) +
      annotate("text", x = 36, y = (lims_le[1] + 0.99 * abs(lims_le[1] - lims_le[2])), label = equation, size=4) +
      annotate("text", x = 10, y = (lims_le[1] + 0.90 * abs(lims_le[1] - lims_le[2])), label = r, size=4)
    
  } # LE / US-Ses
  
    # Calculate the range of values in both plots for consistent scalars
    count.range = range(lapply(list(seg_le, ses_le), function(p){ ggplot_build(p)$data[[1]]$count}))
    seg_le <- seg_le +   scale_fill_continuous(type = "viridis", limits=c(count.range))    # colour palette
    ses_le <- ses_le +   scale_fill_continuous(type = "viridis", limits=c(count.range))    # colour palette
    
  }
  
 
    
    ### NEE
  {
  # NEE / US-Seg
  {
    # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
    pca <- prcomp(~SEG_EC0+SEG_EC1, DF_EC_study_NEE_SEG_daily)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # Compute Pearson correlation coefficient
    r <- cor(DF_EC_study_NEE_SEG_daily$SEG_EC0, DF_EC_study_NEE_SEG_daily$SEG_EC1, method="pearson", use = "complete.obs")
    r <- paste("r: ", round(r,2))
    
    # create plot
    seg_nee <- ggplot(DF_EC_study_NEE_SEG_daily, aes(x=SEG_EC0, y=SEG_EC1)) +
      labs(
        x = expression(paste("Ses EC0 - NEE (", mu, "mol CO"[2]*" m"^"-2"*"s"^"-1"*")")),
        y = expression(paste("Ses EC1 - NEE (", mu, "mol CO"[2]*" m"^"-2"*"s"^"-1"*")")),
        title = "Net Ecosys. Exchange - Ses") +
      # geom_point(shape=1) +
      geom_hex(bins = 50, show.legend=T) +
      # scale_fill_continuous(type = "viridis") +     # colour palette
      xlim(lims_nee) +
      ylim(lims_nee) +
      theme_fancy() +
      theme(plot.title = element_text(size = 14, face = "bold")) +
      theme(legend.position = c(0.85, 0.25)) + # legend position
      geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
      geom_abline(intercept = tls_int, slope = tls_slp) +
      annotate("text", x = 0, y = (lims_nee[1] + 0.99 * abs(lims_nee[1] - lims_nee[2])), label = equation, size=4) +
      annotate("text", x = -1.05, y = (lims_nee[1] + 0.90 * abs(lims_nee[1] - lims_nee[2])), label = r, size=4)
    
  } # NEE / US-Seg
  
  
  # NEE / US-Ses
  {
    # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
    pca <- prcomp(~SES_EC0+SES_EC1, DF_EC_study_NEE_SES_daily)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # Compute Pearson correlation coefficient
    r <- cor(DF_EC_study_NEE_SES_daily$SES_EC0, DF_EC_study_NEE_SES_daily$SES_EC1, method="pearson", use = "complete.obs")
    r <- paste("r: ", round(r,2))
    
    # create plot
    ses_nee <- ggplot(DF_EC_study_NEE_SES_daily, aes(x=SES_EC0, y=SES_EC1)) +
      labs(
        x = expression(paste("Ses EC0 - NEE (", mu, "mol CO"[2]*" m"^"-2"*"s"^"-1"*")")),
        y = expression(paste("Ses EC1 - NEE (", mu, "mol CO"[2]*" m"^"-2"*"s"^"-1"*")")),
        title = "Net Ecosys. Exchange - Ses") +
      # geom_point(shape=1) +
      geom_hex(bins = 50, show.legend=T) +
      # scale_fill_continuous(type = "viridis") +     # colour palette
      xlim(lims_nee) +
      ylim(lims_nee) +
      theme_fancy() +
      theme(plot.title = element_text(size = 14, face = "bold")) +
      theme(legend.position = c(0.85, 0.25)) + # legend position
      geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
      geom_abline(intercept = tls_int, slope = tls_slp) +
      annotate("text", x = 0, y = (lims_nee[1] + 0.99 * abs(lims_nee[1] - lims_nee[2])), label = equation, size=4) +
      annotate("text", x = -1.05, y = (lims_nee[1] + 0.90 * abs(lims_nee[1] - lims_nee[2])), label = r, size=4)
    
  } # NEE / US-Ses
  
    # Calculate the range of values in both plots for consistent scalars
    count.range = range(lapply(list(seg_nee, ses_nee), function(p){ ggplot_build(p)$data[[1]]$count}))
    seg_nee <- seg_nee +   scale_fill_continuous(type = "viridis", limits=c(count.range))    # colour palette
    ses_nee <- ses_nee +   scale_fill_continuous(type = "viridis", limits=c(count.range))    # colour palette
    
  }
  
  ### combine plot objects with Patchwork
  pall <- (seg_h + ses_h) / (seg_le + ses_le) / (seg_nee + ses_nee) +
    plot_annotation(tag_levels = 'a') & theme(plot.tag.position = c(0.0, 0.97))
  
  filename = "plots/EC0 vs EC1 comparison - daily"
  
  ggsave(
    pall,
    filename = paste0(filename, ".png"),
    width = 17,
    height = 26,
    units = "cm"
  )
  
  ggsave(
    pall,
    filename = paste0(filename, ".pdf"),
    width = 17,
    height = 26,
    units = "cm"
  )
  
} # Generate daily comparative plots (EC0 versus EC1)



### Monthly comparison (EC0 versus EC1) ----
# NB. monthly comparison doesn't work because every month contains gaps in the EC0
{
  ## Resample time series to desired temporal resolution
  ## Resample H
  DF_EC_study_H_SEG_monthly <- DF_EC_study_H %>%
    mutate(year = format(Datetime_Start, format = "%Y"),
           month = format(Datetime_Start, format = "%m"),
           day = format(Datetime_Start, format = "%d")
           ) %>% 
    group_by(year, month) %>% 
    select(SEG_EC0,
           SEG_EC1) %>% 
    na.omit %>%  # drop incomplete half hours
    summarize(
      SEG_EC0 = mean(SEG_EC0),
      SEG_EC1 = mean(SEG_EC1)
    )

  DF_EC_study_H_SES_monthly <- DF_EC_study_H %>%
    mutate(year = format(Datetime_Start, format = "%Y"),
           month = format(Datetime_Start, format = "%m"),
           day = format(Datetime_Start, format = "%d")
    ) %>% 
    group_by(year, month) %>% 
    select(SES_EC0,
           SES_EC1) %>% 
    na.omit %>%  # drop incomplete half hours
    summarize(
      SES_EC0 = mean(SES_EC0),
      SES_EC1 = mean(SES_EC1)
      )
  
  
  ## Resample LE
  DF_EC_study_LE_SEG_monthly <- DF_EC_study_LE %>%
    mutate(year = format(Datetime_Start, format = "%Y"),
           month = format(Datetime_Start, format = "%m"),
           day = format(Datetime_Start, format = "%d")
    ) %>% 
    group_by(year, month) %>% 
    select(SEG_EC0,
           SEG_EC1) %>% 
    na.omit %>%  # drop incomplete half hours
    summarize(
      SEG_EC0 = mean(SEG_EC0),
      SEG_EC1 = mean(SEG_EC1)
    )
  
  DF_EC_study_LE_SES_monthly <- DF_EC_study_LE %>%
    mutate(year = format(Datetime_Start, format = "%Y"),
           month = format(Datetime_Start, format = "%m"),
           day = format(Datetime_Start, format = "%d")
    ) %>% 
    group_by(year, month) %>% 
    select(SES_EC0,
           SES_EC1) %>% 
    na.omit %>%  # drop incomplete half hours
    summarize(
      SES_EC0 = mean(SES_EC0),
      SES_EC1 = mean(SES_EC1)
    )

  
  ## Resample NEE
  DF_EC_study_NEE_SEG_monthly <- DF_EC_study_NEE %>%
    mutate(year = format(Datetime_Start, format = "%Y"),
           month = format(Datetime_Start, format = "%m"),
           day = format(Datetime_Start, format = "%d")
    ) %>% 
    group_by(year, month) %>% 
    select(SEG_EC0,
           SEG_EC1) %>% 
    na.omit %>%  # drop incomplete half hours
    summarize(
      SEG_EC0 = mean(SEG_EC0),
      SEG_EC1 = mean(SEG_EC1)
    )
  
  DF_EC_study_NEE_SES_monthly <- DF_EC_study_NEE %>%
    mutate(year = format(Datetime_Start, format = "%Y"),
           month = format(Datetime_Start, format = "%m"),
           day = format(Datetime_Start, format = "%d")
    ) %>% 
    group_by(year, month) %>% 
    select(SES_EC0,
           SES_EC1) %>% 
    na.omit %>%  # drop incomplete half hours
    summarize(
      SES_EC0 = mean(SES_EC0),
      SES_EC1 = mean(SES_EC1)
    )
 
  ## Determine axis limits
  lims_h  <- range(range(DF_EC_study_H_SEG_monthly[, c("SEG_EC0",  "SEG_EC1")]), range(DF_EC_study_H_SES_monthly[, c("SES_EC0",  "SES_EC1")]))
  lims_le  <- range(range(DF_EC_study_LE_SEG_monthly[, c("SEG_EC0",  "SEG_EC1")]), range(DF_EC_study_LE_SES_monthly[, c("SES_EC0",  "SES_EC1")]))
  lims_nee  <- range(range(DF_EC_study_NEE_SEG_monthly[, c("SEG_EC0",  "SEG_EC1")]), range(DF_EC_study_NEE_SES_monthly[, c("SES_EC0",  "SES_EC1")]))
  
  
  
  ## H / US-Seg
  {
    # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
    pca <- prcomp(~SEG_EC0+SEG_EC1, DF_EC_study_H_SEG_monthly)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # Compute Pearson correlation coefficient
    r <- cor(DF_EC_study_H_SEG_monthly$SEG_EC0, DF_EC_study_H_SEG_monthly$SEG_EC1, method="pearson", use = "complete.obs")
    r <- paste("r: ", round(r,2))
    
    # create plot
    seg_h <- ggplot(DF_EC_study_H_SEG_monthly, aes(x=SEG_EC0, y=SEG_EC1)) +
      labs(x = expression("Seg EC0 - H (W m"^"-2"*")"),
           y = expression("Seg EC1 - H (W m"^"-2"*")"),
           title = "Sensible Heat Flux - Seg") +
      geom_point(shape=1) +
      # geom_bin2d(bins = 150, show.legend=T) +   # Bin size control
      # scale_fill_continuous(type = "viridis") +     # colour palette
      xlim(lims_h) +
      ylim(lims_h) +
      theme_fancy() +
      theme(plot.title = element_text(size = 14, face = "bold")) +
      theme(legend.position = c(0.85, 0.25)) + # legend position
      geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
      geom_abline(intercept = tls_int, slope = tls_slp) +
      annotate("text", x = 45, y = (lims_h[1] + 0.99 * abs(lims_h[1] - lims_h[2])), label = equation, size=4) +
      annotate("text", x = 25, y = (lims_h[1] + 0.90 * abs(lims_h[1] - lims_h[2])), label = r, size=4)
    
  } # H / US-Seg
  
  
  # H / US-Ses
  {
    # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
    pca <- prcomp(~SES_EC0+SES_EC1, DF_EC_study_H_SES_monthly)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # Compute Pearson correlation coefficient
    r <- cor(DF_EC_study_H_SES_monthly$SES_EC0, DF_EC_study_H_SES_monthly$SES_EC1, method="pearson", use = "complete.obs")
    r <- paste("r: ", round(r,2))
    
    # create plot
    ses_h <- ggplot(DF_EC_study_H_SES_monthly, aes(x=SES_EC0, y=SES_EC1)) +
      labs(x = expression("Ses EC0 - H (W m"^"-2"*")"),
           y = expression("Ses EC1 - H (W m"^"-2"*")"),
           title = "Sensible Heat Flux - Ses") +
      geom_point(shape=1) +
      # geom_bin2d(bins = 150, show.legend=T) +   # Bin size control
      # scale_fill_continuous(type = "viridis") +     # colour palette
      xlim(lims_h) +
      ylim(lims_h) +
      theme_fancy() +
      theme(plot.title = element_text(size = 14, face = "bold")) +
      theme(legend.position = c(0.85, 0.25)) + # legend position
      geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
      geom_abline(intercept = tls_int, slope = tls_slp) +
      annotate("text", x =45, y = (lims_h[1] + 0.99 * abs(lims_h[1] - lims_h[2])), label = equation, size=4) +
      annotate("text", x = 25, y = (lims_h[1] + 0.90 * abs(lims_h[1] - lims_h[2])), label = r, size=4)

  } # H / US-Ses
  
  
  # LE / US-Seg
  {
    # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
    pca <- prcomp(~SEG_EC0+SEG_EC1, DF_EC_study_LE_SEG_monthly)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # Compute Pearson correlation coefficient
    r <- cor(DF_EC_study_LE_SEG_monthly$SEG_EC0, DF_EC_study_LE_SEG_monthly$SEG_EC1, method="pearson", use = "complete.obs")
    r <- paste("r: ", round(r,2))
    
    # create plot
    seg_le <- ggplot(DF_EC_study_LE_SEG_monthly, aes(x=SEG_EC0, y=SEG_EC1)) +
      labs(x = expression("Seg EC0 - LE (W m"^"-2"*")"),
           y = expression("Seg EC1 - LE (W m"^"-2"*")"),
           title = "Latent Heat Flux - Seg") +
      geom_point(shape=1) +
      # geom_bin2d(bins = 150, show.legend=T) +   # Bin size control
      # scale_fill_continuous(type = "viridis") +     # colour palette
      xlim(lims_le) +
      ylim(lims_le) +
      theme_fancy() +
      theme(plot.title = element_text(size = 14, face = "bold")) +
      theme(legend.position = c(0.85, 0.25)) + # legend position
      geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
      geom_abline(intercept = tls_int, slope = tls_slp) +
      annotate("text", x = 15, y = (lims_le[1] + 0.99 * abs(lims_le[1] - lims_le[2])), label = equation, size=4) +
      annotate("text", x = 9, y = (lims_le[1] + 0.90 * abs(lims_le[1] - lims_le[2])), label = r, size=4)
    
  } # LE / US-Seg
  
  
  # LE / US-Ses
  {
    # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
    pca <- prcomp(~SES_EC0+SES_EC1, DF_EC_study_LE_SES_monthly)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # Compute Pearson correlation coefficient
    r <- cor(DF_EC_study_LE_SES_monthly$SES_EC0, DF_EC_study_LE_SES_monthly$SES_EC1, method="pearson", use = "complete.obs")
    r <- paste("r: ", round(r,2))
    
    # create plot
    ses_le <- ggplot(DF_EC_study_LE_SES_monthly, aes(x=SES_EC0, y=SES_EC1)) +
      labs(x = expression("Ses EC0 - LE (W m"^"-2"*")"),
           y = expression("Ses EC1 - LE (W m"^"-2"*")"),
           title = "Latent Heat Flux - Ses") +
      geom_point(shape=1) +
      # geom_bin2d(bins = 150, show.legend=T) +   # Bin size control
      # scale_fill_continuous(type = "viridis") +     # colour palette
      xlim(lims_le) +
      ylim(lims_le) +
      theme_fancy() +
      theme(plot.title = element_text(size = 14, face = "bold")) +
      theme(legend.position = c(0.85, 0.25)) + # legend position
      geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
      geom_abline(intercept = tls_int, slope = tls_slp) +
      annotate("text", x = 15, y = (lims_le[1] + 0.99 * abs(lims_le[1] - lims_le[2])), label = equation, size=4) +
      annotate("text", x = 9, y = (lims_le[1] + 0.90 * abs(lims_le[1] - lims_le[2])), label = r, size=4)
  } # LE / US-Ses
  
  
  # NEE / US-Seg
  {
    # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
    pca <- prcomp(~SEG_EC0+SEG_EC1, DF_EC_study_NEE_SEG_monthly)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # Compute Pearson correlation coefficient
    r <- cor(DF_EC_study_NEE_SEG_monthly$SEG_EC0, DF_EC_study_NEE_SEG_monthly$SEG_EC1, method="pearson", use = "complete.obs")
    r <- paste("r: ", round(r,2))
    
    # create plot
    seg_nee <- ggplot(DF_EC_study_NEE_SEG_monthly, aes(x=SEG_EC0, y=SEG_EC1)) +
      labs(
        x = expression(paste("Ses EC0 - NEE (", mu, "mol CO"[2]*" m"^"-2"*"s"^"-1"*")")),
        y = expression(paste("Ses EC1 - NEE (", mu, "mol CO"[2]*" m"^"-2"*"s"^"-1"*")")),
        title = "Net Ecosys. Exchange - Ses") +
      geom_point(shape=1) +
      # geom_bin2d(bins = 150, show.legend=T) +   # Bin size control
      # scale_fill_continuous(type = "viridis") +     # colour palette
      xlim(lims_nee) +
      ylim(lims_nee) +
      theme_fancy() +
      theme(plot.title = element_text(size = 14, face = "bold")) +
      theme(legend.position = c(0.85, 0.25)) + # legend position
      geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
      geom_abline(intercept = tls_int, slope = tls_slp) +
      annotate("text", x = -0.4, y = (lims_nee[1] + 0.99 * abs(lims_nee[1] - lims_nee[2])), label = equation, size=4) +
      annotate("text", x = -0.6, y = (lims_nee[1] + 0.90 * abs(lims_nee[1] - lims_nee[2])), label = r, size=4)
    
  } # NEE / US-Seg
  
  
  # NEE / US-Ses
  {
    # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
    pca <- prcomp(~SES_EC0+SES_EC1, DF_EC_study_NEE_SES_monthly)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # Compute Pearson correlation coefficient
    r <- cor(DF_EC_study_NEE_SES_monthly$SES_EC0, DF_EC_study_NEE_SES_monthly$SES_EC1, method="pearson", use = "complete.obs")
    r <- paste("r: ", round(r,2))
    
    # create plot
    ses_nee <- ggplot(DF_EC_study_NEE_SES_monthly, aes(x=SES_EC0, y=SES_EC1)) +
      labs(
        x = expression(paste("Ses EC0 - NEE (", mu, "mol CO"[2]*" m"^"-2"*"s"^"-1"*")")),
        y = expression(paste("Ses EC1 - NEE (", mu, "mol CO"[2]*" m"^"-2"*"s"^"-1"*")")),
        title = "Net Ecosys. Exchange - Ses") +
      geom_point(shape=1) +
      # geom_bin2d(bins = 150, show.legend=T) +   # Bin size control
      # scale_fill_continuous(type = "viridis") +     # colour palette
      xlim(lims_nee) +
      ylim(lims_nee) +
      theme_fancy() +
      theme(plot.title = element_text(size = 14, face = "bold")) +
      theme(legend.position = c(0.85, 0.25)) + # legend position
      geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
      geom_abline(intercept = tls_int, slope = tls_slp) +
      annotate("text", x = -0.4, y = (lims_nee[1] + 0.99 * abs(lims_nee[1] - lims_nee[2])), label = equation, size=4) +
      annotate("text", x = -0.6, y = (lims_nee[1] + 0.90 * abs(lims_nee[1] - lims_nee[2])), label = r, size=4)
    
  } # NEE / US-Ses
  
  
  
  ### combine plot objects with Patchwork
  pall <- (seg_h + ses_h) / (seg_le + ses_le) / (seg_nee + ses_nee) +
    plot_annotation(tag_levels = 'a') & theme(plot.tag.position = c(0.0, 0.97))
  
  filename = "plots/EC0 vs EC1 comparison - monthly"
  
  ggsave(
    pall,
    filename = paste0(filename, ".png"),
    width = 17,
    height = 26,
    units = "cm"
  )
  
  ggsave(
    pall,
    filename = paste0(filename, ".pdf"),
    width = 17,
    height = 26,
    units = "cm"
  )
} # Generate monthly comparative plots (EC0 versus EC1)



#-------------- 3.2 cumulative sum of LE and NEE --------------


# Make GGplot version of SEG LE (using gapfilled and U*filtered records)
# NB, will need gapfilled version of Marcy's (new) data to enable this!


# subset to site and compute cumulative fluxes
DF_EC_study_SEG <- DF_EC_study %>% 
  filter(Station == c("SEG_EC0", "SEG_EC1", "SEG_EC2", "SEG_EC3", "SEG_EC4")) %>% 
  group_by(Station) %>% 
  mutate(
    NEE_uStar_f_cum = cumsum(NEE_uStar_f) *(12.0107/10^6) * 1800,  # Convert umol m-2 s-1 to g C m-2 30 min-1. Where 12.0107 g C/mole * 1 gram /10^6 ugrams * time (1800 s)
    LE_f_cum = cumsum(LE_f),
    LE_f_cum_mm = cumsum(LE_f) * 1800 / 2260 / 1000,  # Calculate mm of cumulative evaporation. Where 1800 = seconds in 30 mins, 2260 =  specific latent heat = energy required to evaporate 1 g of water in J g-1, and '/1000' converts from g m-2 to mm.
    H_f_cum = cumsum(H_f)
    )


DF_EC_study_SES <- DF_EC_study %>% 
  filter(Station == c("SES_EC0", "SES_EC1", "SES_EC2", "SES_EC3", "SES_EC4")) %>% 
  group_by(Station) %>% 
  mutate(
    NEE_uStar_f_cum = cumsum(NEE_uStar_f) *(12.0107/10^6) * 1800,  # Convert umol m-2 s-1 to g C m-2 30 min-1. Where 12.0107 g C/mole * 1 gram /10^6 ugrams * time (1800 s)
    LE_f_cum = cumsum(LE_f),
    LE_f_cum_mm = cumsum(LE_f) * 1800 / 2260 / 1000,  # Calculate mm of cumulative evaporation. Where 1800 = seconds in 30 mins, 2260 =  specific latent heat = energy required to evaporate 1 g of water in J g-1, and '/1000' converts from g m-2 to mm.
    H_f_cum = cumsum(H_f)
    )

# colour mappings
selected_colours <- c("orange",
          "purple",
          "green",
          "darkgreen",
          "brown",
          "cyan")
# labs <- c("EC0", "EC1",    "EC2",    "EC3",  "EC4", "Precipit")

# TO DO ----
### NB. review conversion from LE (W m-2) to evaporation (mm)
# Our initial analysis suggested annual LE MW m-2 was ca. 400 MW

# 400 MW = 400 000 000 j s-1

# 400,000,000 / 2257 = 177,226 g of water per m-2 
# 177,226 g of water per m-2 = 177.226 mm evap 
# This estimate seems highly plausible

# https://www.researchgate.net/post/How-to-calculate-evapotranspiration-from-latent-heat-flux

# t <- 1800  # seconds in 30 mins
# e <- 2257  # The amount of energy required to evaporate 1 g of water in J g-1.

# (xj <- xw * t) # convert to jules per half hour
# (evap1 <- xj / e) # convert to g m2 of evaporation 
# (evap2 <- evap1 / 1000) # convert to mm evaporation
# 
# evap <- 65 * 1800 / 2257 / 1000

# You need to divide the LE, which should be in an energy unit such as W/m-2 by
# the latent heat of evaporation (i.e. the amount of energy required to evaporate
# 1g or 1ml of water) which is 2257 J/g.

# For example, if you have a total LE of 500 W/m-2 for one hour this would 
# be 1800 000 J of energy (with watts equal to jouls per second). 
# Enough to evaporate 798 g of water per m-2 (1800000/2257). 
# This is equal to 0.798 mm of evaporation (1 kg H20 per m-2 = 1 mm). 


## SEG Cumulative LE
{
  (cum_seg_le <- ggplot(DF_EC_study_SEG,
                       aes(y=LE_f_cum_mm, 
                           x=Datetime_Start,
                           color=Station
                       )) +
     labs(x = "",
          y = expression("Cumulative evapotranspiration (mm)", sep=""),
          title = "LE Grassland") +
     geom_line() +
     scale_color_manual(values=selected_colours) +
     theme_fancy()
  )
} # SEG Cumulative LE


## SES Cumulative LE
{
  (cum_ses_le <- ggplot(DF_EC_study_SES,
                       aes(y=LE_f_cum_mm, 
                           x=Datetime_Start,
                           color=Station
                       )) +
     labs(x = "",
          y = expression("Cumulative evapotranspiration (mm)", sep=""),
          title = "LE Shrubland") +
     geom_line() +
     scale_color_manual(values=selected_colours) +
     theme_fancy()
  )
} # SES Cumulative LE





mean(DF_EC_study_SEG$NEE, na.rm=t)

# Need to review (and correct) NEE units, 

# ## 
# ccm <- grep("Fc", colnames(dmat))
# 
# dmat[,ccm] <- dmat[ccm]*  (12 / 10^6) * 1800     # 12 g C/mole * 1 gram /10^6 ugrams * time (1800 s)


## SEG Cumulative NEE
{
  (cum_seg_nee <- ggplot(DF_EC_study_SEG,
                        aes(y=NEE_uStar_f_cum, 
                            x=Datetime_Start,
                            color=Station
                        )) +
     labs(x = "",
          y = expression("Cumulative NEE (g C m"^"-2"*")", sep=""),
          title = "LE Grassland") +
     geom_line() +
     scale_color_manual(values=selected_colours) +
     theme_fancy()
  )
}





# add precipitation
    # geom_line(aes(y = P_plot_le_gm, colour = "prec")) +
    # scale_y_continuous(sec.axis = sec_axis(~.*-ylim_p[2]/diff(ylim_l) + (ylim_p[2]), name = ya2)) +
    # theme(axis.text.x = element_text(angle = 45, vjust=0.3)) +
    # theme(legend.title = element_blank(), legend.text = element_text(size = 8), legend.position = c(0.18, 0.52)) +    # legend position
    # theme(axis.title.y.right = element_text( angle = 90)) +   # Rotate secondary axis



# if(T){
#   
#   cols <- rep(c("purple", "green", "darkgreen", "brown"), 2)
#   add_AmeriFlux <- T
#   add_AmeriFlux_edire <- F
#   add_AmeriFlux_edire_no_WPL <- F
#   add_prec <- T
#   use_reddy <- T
#   no_seg_nee <- F
#   
#   
#   day_only <- F
#   night_only <- F    
#   pre_prec <- F
#   post_prec <- F     
#   
#   #   
#   plot_nm <- last_date; tp1 <- 1:4; tp2 <- 5:8; attr <- ""
#   les <- paste("cLEc", datasets, sep="_")
#   nees <- paste("Fcc", datasets, sep="_")
#   
#   legend_g <- c("EC1", "EC2", "EC3", "EC4")
#   # u* filtered (NEE only) and gapfilled
#   
#   
#   if(add_AmeriFlux){
#     cols <- c(cols, "orange", "orange")
#     les <- c(les, "cLE_gm", "cLE_sm")
#     nees <- c(nees, "Fc_gm", "Fc_sm")
#     legend_g <- c(legend_g, "EC0")
#     plot_nm <- paste(plot_nm, "_plusm", sep=""); tp1 <- c(tp1, 9); tp2 <- c(tp2, 10)
#   }
#   
#   if(add_prec){    plot_nm <- paste(plot_nm, "_prec", sep="")  }
#   
#   if(use_reddy){
#     les <- paste("LE_f", adatasets[1:10], sep="_")
#     nees <- paste("NEE_uStar_f", adatasets[1:10], sep="_")
#     attr <- "_reddy"
#   }
#   
#   if(add_AmeriFlux_edire){
#     cols <- c(cols, "red", "red")
#     les <- c(les, "LEcw_gp", "LEcw_sp")
#     nees <- c(nees, "Fccw_gp", "Fccw_sp")
#     legend_g <- c(legend_g, "US-SEG EdiRe")
#     #legend_s <- c(legend_s, "US-SES EdiRe")
#     plot_nm <- paste(plot_nm, "_m_edire", sep=""); tp1 <- c(tp1, 11); tp2 <- c(tp2, 12)
#     attr <- "_m_edire"
#   }
#   
#   
#   if(add_AmeriFlux_edire_no_WPL){
#     cols <- c(cols, "blue4", "blue4")
#     les <- c(les, "LEc_gp", "LEc_sp")
#     nees <- c(nees, "Fcc_gp", "Fcc_sp")
#     legend_g <- c(legend_g, "SEG Edi no WPL")
#     #legend_s <- c(legend_s, "SES Edi no WPL")
#     plot_nm <- paste(plot_nm, "_no_WPL", sep=""); tp1 <- c(tp1, 13); tp2 <- c(tp2, 14)
#     attr <- "_m_edire_no_WPL"
#   }
#   
#   
#   
#   
#   datp <- datdd   # temp datasets
#   
#   gcs <- c(les[grepl("_g", les)], nees[grepl("_g", nees)])
#   scs <- c(les[grepl("_s", les)], nees[grepl("_s", nees)])
#   
#   # filter day/night (gm/sm specific filter for H)
#   selh_gm <- datp[,"H_gm"]>0;    selh_gm[is.na(selh_gm)] <- T     # data.frame does not allow NAs in sel
#   selh_sm <- datp[,"H_sm"]>0;    selh_sm[is.na(selh_sm)] <- T
#   
#   if(day_only){  datp[!selh_gm, gcs] <- NA; datp[!selh_sm, scs] <- NA; attr <- paste(attr, "_day_only", sep="")}
#   if(night_only){datp[selh_gm, gcs] <- NA;  datp[selh_sm, scs] <- NA;  attr <- paste(attr, "_night_only", sep="")}
#   
#   
#   
#   
#   
#   #### filter before/after precipitation
#   
#   selp_gm <- selp_sm <- rep(F, nrow(datp))
#   selp_gm[unique( unlist(   lapply( which( datp[,"P_hh_gm"]>0 ) , day3_fun)    ) )] <- T
#   selp_sm[unique( unlist(   lapply( which( datp[,"P_hh_sm"]>0 ) , day3_fun)    ) )] <- T
#   
#   if(pre_prec){  datp[!selp_gm, gcs] <- NA; datp[!selp_sm, scs] <- NA; attr <- paste(attr, "_pre_prec", sep="")}
#   if(post_prec){ datp[selp_gm, gcs] <- NA;  datp[selp_sm, scs] <- NA;  attr <- paste(attr, "_post_prec", sep="")}
#   
#   
#   
#   
#   ## cumsum
#   dmat <- datp[, c(nees, les)]; dmat[is.na(dmat)] <- 0; 
#   dmat$dt <- datp[,"dt"]; dmat$P_gm <- datp[,"P_hh_gm"]; dmat$P_sm <- datp[,"P_hh_sm"]
#   dmat <- dmat[dmat[,"dt"]>="20/10/18",]
#   for(i in 1:(ncol(dmat)-3)){dmat[,i] <- cumsum(dmat[,i])}
#   
#   
#   
#   ## convert umol m-2 s-1 to gC m-2 30min-1
#   ccm <- grep("Fc", colnames(dmat))
#   if(use_reddy){ccm <- grep("NEE", colnames(dmat))}
#   if(add_AmeriFlux_edire)ccm <- c(ccm, grep("Fc", colnames(dmat)))
#   
#   dmat[,ccm] <- dmat[ccm]*  (12 / 10^6) * 1800     # 12 g C/mole * 1 gram /10^6 ugrams * time (1800 s)
#   
#   ## convert W m-2 into MW m-2 (30 min sum)
#   lcm <- grep("LE", colnames(dmat))
#   dmat[,lcm] <- dmat[lcm]*  (1 / 10^6) * 1800
#   
#   
#   # exclude NEE of US-SEG
#   if(no_seg_nee){ 
#     ec <- grepl("_gm", colnames(dmat)) & 
#       (grepl("NEE", colnames(dmat)) | grepl("Fcc", colnames(dmat)))
#     dmat[, ec] <- NA
#     attr <- paste("_no_seg_nee", attr, sep="")  
#   } 
#   
#   
#   
#   
#   
#   xlim <- range(dmat[,"dt"])
#   ylim_c <- range(dmat[,ccm], na.rm=T)*c(1, 1.5)
#   if(ylim_c[1]<0)ylim_c <- ylim_c*c(1.05, 1)
#   #ylim_c[1] <- (-40)
#   if(add_AmeriFlux_edire_no_WPL)ylim_c[1] <- (-1100)
#   ylim_l <- range(dmat[,lcm], na.rm=T)*c(1, 1.5)
#   ylim_p <- range(datdd[,c("P_hh_gm", "P_hh_sm")], na.rm=T)*c(1, 2.2)
#   
#   if(pre_prec | post_prec){ylim_l <- c(0, 450); ylim_c <- c(-30, 70)}
#   if(day_only | night_only){ylim_c <- c(-60, 80)}
#   
#   
#   
#   # time stamp
#   mthetimes <-  as.POSIXct(dmat[,"dt"], format="%d/%m/%y %H:%M:%S") 
#   dmat$dt_2 <- mthetimes
#   
#   ylim_l <- c(0, 800)
#   
#   ya2 <- expression("Precipitation (mm h"^"-1"*")",sep="")
#   
#   
#   # ylim correction factor for precipitation
#   
#   # diff(ylim_l)/ylim_p[2]   = 20.41817
#   # diff(ylim_l)/20.41817    = 36.32201    # desired ylim
#   
#   # diff(ylim_c)/ylim_p[2]   =  1.629766
#   # diff(ylim_c)/ 1.629766   =  36.322     # desired ylim
#   
#   
#   
#   # add modified precipitation
#   dmat$P_plot_le_gm <- (dmat$P_gm * (-diff(ylim_l)/ylim_p[2])) + ylim_l[2]
#   dmat$P_plot_le_sm <- (dmat$P_sm * (-diff(ylim_l)/ylim_p[2])) + ylim_l[2]
#   dmat$P_plot_nee_gm <- (dmat$P_gm * (-diff(ylim_c)/ylim_p[2])) + ylim_c[2]
#   dmat$P_plot_nee_sm <- (dmat$P_sm * (-diff(ylim_c)/ylim_p[2])) + ylim_c[2]
#   
#   
#   
#   p1 <- ggplot(dmat, aes(x=dt_2)) +
#     labs(x = "", y = expression("Cumulative LE (MW m"^"-2"*")",sep=""), title = "LE Grassland") +
#     geom_line(aes(y = LE_f_g1, colour = "EC1")) + 
#     geom_line(aes(y = LE_f_g2, colour = "EC2")) + 
#     geom_line(aes(y = LE_f_g3, colour = "EC3")) + 
#     geom_line(aes(y = LE_f_g4, colour = "EC4")) +
#     geom_line(aes(y = LE_f_gm, colour = "EC0")) +
#     geom_line(aes(y = P_plot_le_gm, colour = "prec")) +
#     scale_y_continuous(sec.axis = sec_axis(~.*-ylim_p[2]/diff(ylim_l) + (ylim_p[2]), name = ya2)) +
#     theme_bw() +
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + # remove grid 
#     theme(axis.text.x = element_text(angle = 45, vjust=0.3)) +
#     theme(legend.title = element_blank(), legend.text = element_text(size = 8), legend.position = c(0.18, 0.52)) +    # legend position
#     theme(axis.title.y.right = element_text( angle = 90)) +   # Rotate secondary axis 
#     scale_colour_manual(values = colours)
#   
#   
#   p2 <- ggplot(dmat, aes(x=dt_2)) +
#     labs(x = "", y = expression("Cumulative LE (MW m"^"-2"*")",sep=""), title = "LE Shrubland") +
#     geom_line(aes(y = LE_f_s1, colour = "EC1")) + 
#     geom_line(aes(y = LE_f_s2, colour = "EC2")) + 
#     geom_line(aes(y = LE_f_s3, colour = "EC3")) + 
#     geom_line(aes(y = LE_f_s4, colour = "EC4")) +
#     geom_line(aes(y = LE_f_sm, colour = "EC0")) +
#     geom_line(aes(y = P_plot_le_sm, colour = "prec")) +
#     scale_y_continuous(sec.axis = sec_axis(~.*-ylim_p[2]/diff(ylim_l) + (ylim_p[2]), name = ya2)) +
#     theme_bw() + 
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + # remove grid 
#     theme(axis.text.x = element_text(angle = 45, vjust=0.3)) +
#     theme(legend.position="none") +  
#     theme(axis.title.y.right = element_text( angle = 90)) +   # Rotate secondary axis 
#     scale_colour_manual(values = colours)
#   
#   
#   p3 <- ggplot(dmat, aes(x=dt_2)) +
#     labs(x = "", y = expression("Cumulative NEE (g C m"^"-2"*")",sep=""), title = "NEE Grassland") +
#     geom_line(aes(y = NEE_uStar_f_g1, colour = "EC1")) + 
#     geom_line(aes(y = NEE_uStar_f_g2, colour = "EC2")) + 
#     geom_line(aes(y = NEE_uStar_f_g3, colour = "EC3")) + 
#     geom_line(aes(y = NEE_uStar_f_g4, colour = "EC4")) +
#     geom_line(aes(y = NEE_uStar_f_gm, colour = "EC0")) +
#     geom_line(aes(y = P_plot_nee_gm, colour = "prec")) +
#     geom_point(aes(x=dt_2[1], y=ylim_c[1]), colour="white") + # only to specify lower end
#     scale_y_continuous(sec.axis = sec_axis(~.*-ylim_p[2]/diff(ylim_c) - min(ylim_c*(-ylim_p[2]/diff(ylim_c))), name = ya2)) +
#     theme_bw() + 
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + # remove grid 
#     theme(axis.text.x = element_text(angle = 45, vjust=0.3)) +
#     theme(legend.position="none") +  
#     theme(axis.title.y.right = element_text( angle = 90)) +   # Rotate secondary axis 
#     scale_colour_manual(values = colours)
#   
#   
#   p4 <- ggplot(dmat, aes(x=dt_2)) +
#     labs(x = "", y = expression("Cumulative NEE (g C m"^"-2"*")",sep=""), title = "NEE Shrubland") +
#     geom_line(aes(y = NEE_uStar_f_s1, colour = "EC1")) + 
#     geom_line(aes(y = NEE_uStar_f_s2, colour = "EC2")) + 
#     geom_line(aes(y = NEE_uStar_f_s3, colour = "EC3")) + 
#     geom_line(aes(y = NEE_uStar_f_s4, colour = "EC4")) +
#     geom_line(aes(y = NEE_uStar_f_sm, colour = "EC0")) +
#     geom_line(aes(y = P_plot_nee_sm, colour = "prec")) + 
#     geom_point(aes(x=dt_2[1], y=ylim_c[1]), colour="white") + # only to specify lower end
#     scale_y_continuous(sec.axis = sec_axis(~.*-ylim_p[2]/diff(ylim_c) - min(ylim_c*(-ylim_p[2]/diff(ylim_c))), name = ya2)) +
#     theme_bw() + 
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + # remove grid 
#     theme(axis.text.x = element_text(angle = 45, vjust=0.3)) +
#     theme(legend.position="none") +
#     theme(axis.title.y.right = element_text( angle = 90)) +   # Rotate secondary axis 
#     scale_colour_manual(values = colours)
#   
#   
#   ## combine plots using Patchwork
#   pall <- (p1 + p2) / (p3 + p4) +
#     plot_annotation(tag_levels = 'a') & theme(plot.tag.position = c(0.0, 0.97)) 
#   
#   
#   # Save raster
#   gpath <- "E:/REC_7_Data/10_Plots/7_cumsum/"
#   outfile_r <- paste(gpath, plot_nm, "_cumsum", attr, "_h", xch, "_ggplot.png", sep="")
#   
#   ggsave(pall,
#          filename = outfile_r,
#          width = 17,
#          height = 17,
#          units = "cm")
#   
#   # Save vector
#   gpath <- "E:/REC_7_Data/10_Plots/7_cumsum/"
#   outfile_v <- paste(gpath, plot_nm, "_cumsum", attr, "_h", xch, "_ggplot_test.pdf", sep="")
#   
#   ggsave(pall,
#          filename = outfile_v,
#          width = 17,
#          height = 17,
#          units = "cm")
#   
# }  # cumulative LE and NEE
# 


