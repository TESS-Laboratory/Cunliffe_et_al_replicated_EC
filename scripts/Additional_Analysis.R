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
# fpath  <-  "C:/workspace/REC_7_Data/11_ReddyProc/"  # Gap filled ReddyProc output
mpath  <-  "C:/workspace/REC_7_Data/12_Marcys_data/"
npath <- "data/gapfilled_fluxes/"

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
# Note datetime  format differs (yyyy vs yy) between some data files.
  
## Read data from Licor systems (for met data)
SEG0a  <- read_csv(file=paste(mpath, "US-Seg_HH_201801010000_201901010000.csv", sep=""),
                      col_types = cols(TIMESTAMP_START = col_datetime("%Y %m %d %H %M"),
                                       TIMESTAMP_END = col_datetime("%Y %m %d %H %M")),
                      na = c("", "NA",-9999))

SEG0b  <- read_csv(file=paste(mpath, "US-Seg_HH_201901010000_202001010000.csv", sep=""),
                      col_types = cols(TIMESTAMP_START = col_datetime("%Y %m %d %H %M"),
                                       TIMESTAMP_END = col_datetime("%Y %m %d %H %M")),
                      na = c("", "NA",-9999))

SEG0c  <- read_csv(file=paste(mpath, "US-Seg_HH_202001010000_202101010000.csv", sep=""),
                         col_types = cols(TIMESTAMP_START = col_datetime("%Y %m %d %H %M"),
                                          TIMESTAMP_END = col_datetime("%Y %m %d %H %M")),
                         na = c("", "NA",-9999))


SES0a  <- read_csv(file=paste(mpath, "US-Ses_HH_201801010000_201901010000.csv", sep=""),
                      col_types = cols(TIMESTAMP_START = col_datetime("%Y %m %d %H %M"),
                                       TIMESTAMP_END = col_datetime("%Y %m %d %H %M")),
                      na = c("", "NA",-9999))

SES0b  <- read_csv(file=paste(mpath, "US-Ses_HH_201901010000_202001010000.csv", sep=""),
                      col_types = cols(TIMESTAMP_START = col_datetime("%Y %m %d %H %M"),
                                       TIMESTAMP_END = col_datetime("%Y %m %d %H %M")),
                      na = c("", "NA",-9999))

SES0c  <- read_csv(file=paste(mpath, "US-Ses_HH_202001010000_202101010000.csv", sep=""),
                         col_types = cols(TIMESTAMP_START = col_datetime("%Y %m %d %H %M"),
                                          TIMESTAMP_END = col_datetime("%Y %m %d %H %M")),
                         na = c("", "NA",-9999))


## Compile time series from Licor systems
SEG0_all <- dplyr::bind_rows(SEG0a, SEG0b, SEG0c); rm(SEG0a, SEG0b, SEG0c)
SES0_all <- dplyr::bind_rows(SES0a, SES0b, SES0c); rm(SES0a, SES0b, SES0c)


## Create lists for processing Licor systems ## not currently used
# list_licor_a <- list(SEG0_all, SES0_all) 
# list_licor_b <- c("SEG0", "SES0") 

## Create function to rename and select meteorological variables of interest 
tidy_licor <- function(df, station) {
  df %>%
    rename(Precipitation = P_F,
           datetime = TIMESTAMP_START,
           AirTemperature = TA_F) %>% 
    select(datetime,
           Precipitation, 
           SW_IN,
           AirTemperature,
           VPD_F,
           PPFD_IN
    ) %>%
    mutate(Station = station,
           datetime = datetime + 15*60) # adding 15 minutes (in seconds) to change datetime to center of half hour
}  # Tidy Licor data


SEG_met <- tidy_licor(SEG0_all, "SEG0"); rm(SEG0_all)
SES_met <- tidy_licor(SES0_all, "SES0"); rm(SES0_all)




#### Read fluxes gap filled with Random Forest Regression workflow. 
## Note that timestamps are the middle of the half hour rather than start and end.

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#### WORK IN PROGRESS ####

# ultimately aiming for a single data frame, containing P, SW_in, NEE, H, LE, Station, Site
# merge met obbs (code above) with gapfilled fluxes (code below)...


## Want to automate reading in 30 files, with list of station codes and list of variables
## Then use these station and variable codes to put the data together

## Think I want a purrr solution?

# list_station <- c("SEG_EC1", "SEG_EC2", "SEG_EC3", "SEG_EC4", "SES_EC1", "SES_EC2", "SES_EC3", "SES_EC4")
# list_variable <- c("H", "LE", "NEE")



# X  <- read_csv(file=paste(npath, "XXX.csv", sep=""))
# SEG_EC0 <- "cbind"(SEG_EC0_H, SEG_EC0_LE, SEG_EC0_NEE) #  cbind generates with duplicate datetime columns!!!

# ## Use purrr::map2 to process multiple tibbles through pipe and return a single merged dataframe of Licor data
# DF_EC_licor <- list1a %>%
# purrr::map2(.x =., .y=list1b, ~tidy_licor(.x, .y)) %>%
# # bind_rows()

# ## Use purrr::map2 to process multiple tibbles through pipe and return a single merged dataframe
# DF_EC_test <- list1a %>%
#   purrr::map2(.x =.) %>%
#   bind_rows()
# 



# the ugly way!!!

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


# Read and combine gap filled flux files for SEG0
SEG0_H  <- read_csv("data/gapfilled_fluxes/SEG0 H.csv") 
SEG0_LE  <- read_csv("data/gapfilled_fluxes/SEG0 LE.csv") 
SEG0_NEE <- read_csv("data/gapfilled_fluxes/SEG0 NEE.csv") 
SEG0_flux <- merge(SEG0_H, SEG0_LE, by = "datetime")  # Merge happens visa two step because it can only work with two inputs
SEG0_flux <- merge(SEG0_flux, SEG0_NEE, by = "datetime")
rm(SEG0_H, SEG0_LE, SEG0_NEE)  # Tidy up

# Read and combine gap filled flux files for SES0
SES0_H  <- read_csv("data/gapfilled_fluxes/SES0 H.csv") 
SES0_LE  <- read_csv("data/gapfilled_fluxes/SES0 LE.csv") 
SES0_NEE <- read_csv("data/gapfilled_fluxes/SES0 NEE.csv") 
SES0_flux <- merge(SES0_H, SES0_LE, by = "datetime")  # Merge happens visa two step because it can only work with two inputs
SES0_flux <- merge(SES0_flux, SES0_NEE, by = "datetime")
rm(SES0_H, SES0_LE, SES0_NEE)  # Tidy up

# Read gap filled flux files for low frequency systems
SEG1_H  <- read_csv("data/gapfilled_fluxes/SEG1 H.csv") 
SEG1_LE  <- read_csv("data/gapfilled_fluxes/SEG1 LE.csv") 
SEG1_NEE <- read_csv("data/gapfilled_fluxes/SEG1 NEE.csv") 
SEG1_flux <- merge(SEG1_H, SEG1_LE, by = "datetime")  # Merge happens visa two step because it can only work with two inputs
SEG1_flux <- merge(SEG1_flux, SEG1_NEE, by = "datetime"); rm(SEG1_H, SEG1_LE, SEG1_NEE)  # Tidy up

SEG2_H  <- read_csv("data/gapfilled_fluxes/SEG2 H.csv") 
SEG2_LE  <- read_csv("data/gapfilled_fluxes/SEG2 LE.csv") 
SEG2_NEE <- read_csv("data/gapfilled_fluxes/SEG2 NEE.csv") 
SEG2_flux <- merge(SEG2_H, SEG2_LE, by = "datetime")  # Merge happens visa two step because it can only work with two inputs
SEG2_flux <- merge(SEG2_flux, SEG2_NEE, by = "datetime"); rm(SEG2_H, SEG2_LE, SEG2_NEE)  # Tidy up

SEG3_H  <- read_csv("data/gapfilled_fluxes/SEG3 H.csv") 
SEG3_LE  <- read_csv("data/gapfilled_fluxes/SEG3 LE.csv") 
SEG3_NEE <- read_csv("data/gapfilled_fluxes/SEG3 NEE.csv")
SEG3_flux <- merge(SEG3_H, SEG3_LE, by = "datetime")  # Merge happens visa two step because it can only work with two inputs
SEG3_flux <- merge(SEG3_flux, SEG3_NEE, by = "datetime"); rm(SEG3_H, SEG3_LE, SEG3_NEE)  # Tidy up

SEG4_H  <- read_csv("data/gapfilled_fluxes/SEG4 H.csv") 
SEG4_LE  <- read_csv("data/gapfilled_fluxes/SEG4 LE.csv") 
SEG4_NEE <- read_csv("data/gapfilled_fluxes/SEG4 NEE.csv") 
SEG4_flux <- merge(SEG4_H, SEG4_LE, by = "datetime")  # Merge happens visa two step because it can only work with two inputs
SEG4_flux <- merge(SEG4_flux, SEG4_NEE, by = "datetime"); rm(SEG4_H, SEG4_LE, SEG4_NEE)  # Tidy up

SES1_H  <- read_csv("data/gapfilled_fluxes/SES1 H.csv") 
SES1_LE  <- read_csv("data/gapfilled_fluxes/SES1 LE.csv") 
SES1_NEE <- read_csv("data/gapfilled_fluxes/SES1 NEE.csv") 
SES1_flux <- merge(SES1_H, SES1_LE, by = "datetime")  # Merge happens visa two step because it can only work with two inputs
SES1_flux <- merge(SES1_flux, SES1_NEE, by = "datetime"); rm(SES1_H, SES1_LE, SES1_NEE)  # Tidy up

SES2_H  <- read_csv("data/gapfilled_fluxes/SES2 H.csv") 
SES2_LE  <- read_csv("data/gapfilled_fluxes/SES2 LE.csv") 
SES2_NEE <- read_csv("data/gapfilled_fluxes/SES2 NEE.csv") 
SES2_flux <- merge(SES2_H, SES2_LE, by = "datetime")  # Merge happens visa two step because it can only work with two inputs
SES2_flux <- merge(SES2_flux, SES2_NEE, by = "datetime"); rm(SES2_H, SES2_LE, SES2_NEE)  # Tidy up

SES3_H  <- read_csv("data/gapfilled_fluxes/SES3 H.csv") 
SES3_LE  <- read_csv("data/gapfilled_fluxes/SES3 LE.csv") 
SES3_NEE <- read_csv("data/gapfilled_fluxes/SES3 NEE.csv") 
SES3_flux <- merge(SES3_H, SES3_LE, by = "datetime")  # Merge happens visa two step because it can only work with two inputs
SES3_flux <- merge(SES3_flux, SES3_NEE, by = "datetime"); rm(SES3_H, SES3_LE, SES3_NEE)  # Tidy up

SES4_H  <- read_csv("data/gapfilled_fluxes/SES4 H.csv") 
SES4_LE  <- read_csv("data/gapfilled_fluxes/SES4 LE.csv") 
SES4_NEE <- read_csv("data/gapfilled_fluxes/SES4 NEE.csv") 
SES4_flux <- merge(SES4_H, SES4_LE, by = "datetime")  # Merge happens visa two step because it can only work with two inputs
SES4_flux <- merge(SES4_flux, SES4_NEE, by = "datetime"); rm(SES4_H, SES4_LE, SES4_NEE)  # Tidy up


### add station codes to flux tibbles
add_stations <- function(df, station) {
  df %>%
    mutate(Station = station)
  }  # Tidy flux data

SEG0 <- add_stations(SEG0_flux, "SEG0"); rm(SEG0_flux)
SEG1 <- add_stations(SEG1_flux, "SEG1"); rm(SEG1_flux)
SEG2 <- add_stations(SEG2_flux, "SEG2"); rm(SEG2_flux)
SEG3 <- add_stations(SEG3_flux, "SEG3"); rm(SEG3_flux)
SEG4 <- add_stations(SEG4_flux, "SEG4"); rm(SEG4_flux)

SES0 <- add_stations(SES0_flux, "SES0"); rm(SES0_flux)
SES1 <- add_stations(SES1_flux, "SES1"); rm(SES1_flux)
SES2 <- add_stations(SES2_flux, "SES2"); rm(SES2_flux)
SES3 <- add_stations(SES3_flux, "SES3"); rm(SES3_flux)
SES4 <- add_stations(SES4_flux, "SES4"); rm(SES4_flux)


# combine all systems into long form
SEG_fluxes_long <- rbind(SEG0, SEG1, SEG2, SEG3, SEG4)
SES_fluxes_long <- rbind(SES0, SES1, SES2, SES3, SES4)
rm(SEG0, SEG1, SEG2, SEG3, SEG4)
rm(SES0, SES1, SES2, SES3, SES4)
}  # Read data



#-------------- 2. Tidy data --------------

# Subset time series to study period dates
subset_datetime <- function(df) {
  df %>%
    filter(between(datetime,
                   as.POSIXct("2018-11-01 00:00:00"),
                   as.POSIXct("2020-10-31 23:30:00")))
}

SEG_fluxes <- subset_datetime(SEG_fluxes_long); rm(SEG_fluxes_long)
SES_fluxes <- subset_datetime(SES_fluxes_long); rm(SES_fluxes_long)
SEG_met <- subset_datetime(SEG_met)
SES_met <- subset_datetime(SES_met)

fluxes <- rbind(SEG_fluxes, SES_fluxes)


# create function pivoting data from long to wide form for analysis
shape_wider <- function(df, variable) {
  df %>%
    select(datetime, variable, Station) %>%
    pivot_wider(names_from = Station,
                values_from = variable)
}  #

# pivot data to wide form
{
  fluxes_H <- shape_wider(fluxes, "H_filled")
  fluxes_LE <- shape_wider(fluxes, "LE_filled")
  fluxes_NEE <- shape_wider(fluxes, "NEE_filled")
}



#-------------- 3. Analyze data --------------

#-------------- 3.1 Co-location comparison --------------


### Half-hourly comparison (EC0 versus EC1)----
{
## Determine axis limits
lims_h  <- range(fluxes_H[, c("SEG0",  "SEG1",  "SES0",  "SES1")],  na.rm=T)
lims_le  <- range(fluxes_LE[, c("SEG0",  "SEG1",  "SES0",  "SES1")],  na.rm=T)
lims_nee  <- range(fluxes_NEE[, c("SEG0",  "SEG1",  "SES0",  "SES1")],  na.rm=T)

### H
{
## H / US-Seg
{

# Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
pca <- prcomp(~SEG0+SEG1, fluxes_H)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept

equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute Pearson correlation coefficient
r <- cor(fluxes_H$SEG0, fluxes_H$SEG1, method="pearson", use = "complete.obs")
r <- paste("r: ", round(r,2))

# create plot
seg_h <- ggplot(fluxes_H, aes(x=SEG0, y=SEG1)) +
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
  annotate("text", x = -250, y = 650, label = equation, size=4) +
  annotate("text", x = -550, y = 540, label = r, size=4)

} # H / US-Seg


## H / US-Ses
{
  # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
  pca <- prcomp(~SES0+SES1, fluxes_H)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute Pearson correlation coefficient
  r <- cor(fluxes_H$SES0, fluxes_H$SES1, method="pearson", use = "complete.obs")
  r <- paste("r: ", round(r,2))
  
  # create plot
  ses_h <- ggplot(fluxes_H, aes(x=SES0, y=SES1)) +
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
    annotate("text", x = -250, y = 650, label = equation, size=4) +
    annotate("text", x = -550, y = 540, label = r, size=4)
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
  pca <- prcomp(~SEG0+SEG1, fluxes_LE)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute Pearson correlation coefficient
  r <- cor(fluxes_LE$SEG0, fluxes_LE$SEG1, method="pearson", use = "complete.obs")
  r <- paste("r: ", round(r,2))
  
  # create plot
  seg_le <- ggplot(fluxes_LE, aes(x=SEG0, y=SEG1)) +
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
    annotate("text", x = 10, y = 430, label = equation, size=4) +
    annotate("text", x = -110, y = 380, label = r, size=4)
  
} # LE / US-Seg


# LE / US-Ses
{
  # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
  pca <- prcomp(~SES0+SES1, fluxes_LE)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute Pearson correlation coefficient
  r <- cor(fluxes_LE$SES0, fluxes_LE$SES1, method="pearson", use = "complete.obs")
  r <- paste("r: ", round(r,2))
  
  # create plot
  ses_le <- ggplot(fluxes_LE, aes(x=SES0, y=SES1)) +
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
    annotate("text", x = 25, y = 430, label = equation, size=4) +
    annotate("text", x = -120, y = 380, label = r, size=4)
  
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
  pca <- prcomp(~SEG0+SEG1, fluxes_NEE)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute Pearson correlation coefficient
  r <- cor(fluxes_NEE$SEG0, fluxes_NEE$SEG1, method="pearson", use = "complete.obs")
  r <- paste("r: ", round(r,2))
  
  # create plot
  seg_nee <- ggplot(fluxes_NEE, aes(x=SEG0, y=SEG1)) +
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
    annotate("text", x = -4.4, y = 10.6, label = equation, size=4) +
    annotate("text", x = -8.7, y = 8.6, label = r, size=4)
  
} # NEE / US-Seg


## NEE / US-Ses
{
  # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
  pca <- prcomp(~SES0+SES1, fluxes_NEE)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # Compute Pearson correlation coefficient
  r <- cor(fluxes_NEE$SES0, fluxes_NEE$SES1, method="pearson", use = "complete.obs")
  r <- paste("r: ", round(r,2))
  
  # create plot
  ses_nee <- ggplot(fluxes_NEE, aes(x=SES0, y=SES1)) +
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
    annotate("text", x = -4.5, y = 10.6, label = equation, size=4) +
    annotate("text", x = -8.6, y = 8.6, label = r, size=4)
  
} # NEE / US-Ses

# Calculate the range of values in both plots for consistent scalars
count.range = range(lapply(list(seg_nee, ses_nee), function(p){ ggplot_build(p)$data[[1]]$count}))
seg_nee <- seg_nee +   scale_fill_continuous(type = "viridis", limits=c(count.range))    # colour palette
ses_nee <- ses_nee +   scale_fill_continuous(type = "viridis", limits=c(count.range))    # colour palette
  
}



### combine plot objects with Patchwork
pall <- (seg_h + ses_h) / (seg_le + ses_le) / (seg_nee + ses_nee) +
  plot_annotation(tag_levels = 'a') & theme(plot.tag.position = c(0.0, 0.97))

filename = "plots/EC0 vs EC1 comparison A - half hourly"

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
  fluxes_H_SEG_daily <- fluxes_H %>%
       mutate(Datetime_Start_res = as.Date(datetime, format = "%Y-%m-%d")) %>% 
       group_by(Datetime_Start_res) %>%
       select(SEG0,
              SEG1) %>% 
       summarise(
         SEG0 = mean(SEG0),
         SEG1 = mean(SEG1)
         )

  fluxes_H_SES_daily <- fluxes_H %>%
       mutate(Datetime_Start_res = as.Date(datetime, format = "%Y-%m-%d")) %>% 
       group_by(Datetime_Start_res) %>%
       select(SES0,
              SES1) %>% 
       summarise(
         SES0 = mean(SES0),
         SES1 = mean(SES1)
       )
     
     
     ## Resample LE
     fluxes_LE_SEG_daily <- fluxes_LE %>%
       mutate(Datetime_Start_res = as.Date(datetime, format = "%Y-%m-%d")) %>% 
       group_by(Datetime_Start_res) %>%
       select(SEG0,
              SEG1) %>% 
       summarise(
         SEG0 = mean(SEG0),
         SEG1 = mean(SEG1)
       )   
   
     fluxes_LE_SES_daily <- fluxes_LE %>%
       mutate(Datetime_Start_res = as.Date(datetime, format = "%Y-%m-%d")) %>% 
       group_by(Datetime_Start_res) %>%
       select(SES0,
              SES1) %>% 
       summarise(
         SES0 = mean(SES0),
         SES1 = mean(SES1)
       )   
     
     
     ## Resample NEE 
     fluxes_NEE_SEG_daily <- fluxes_NEE %>%
       mutate(Datetime_Start_res = as.Date(datetime, format = "%Y-%m-%d")) %>% 
       group_by(Datetime_Start_res) %>%
       select(SEG0,
              SEG1) %>% 
       summarise(
         SEG0 = mean(SEG0),
         SEG1 = mean(SEG1)
       )
    
     fluxes_NEE_SES_daily <- fluxes_NEE %>%
       mutate(Datetime_Start_res = as.Date(datetime, format = "%Y-%m-%d")) %>% 
       group_by(Datetime_Start_res) %>%
       select(SES0,
              SES1) %>% 
       summarise(
         SES0 = mean(SES0),
         SES1 = mean(SES1)
       )
     
   
  ## Determine axis limits
  lims_h  <- range(range(fluxes_H_SEG_daily[, c("SEG0",  "SEG1")]), range(fluxes_H_SES_daily[, c("SES0",  "SES1")]))
  lims_le  <- range(range(fluxes_LE_SEG_daily[, c("SEG0",  "SEG1")]), range(fluxes_LE_SES_daily[, c("SES0",  "SES1")]))
  lims_nee  <- range(range(fluxes_NEE_SEG_daily[, c("SEG0",  "SEG1")]), range(fluxes_NEE_SES_daily[, c("SES0",  "SES1")]))

  ### H
  {
  ## H / US-Seg
  {
    # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
    pca <- prcomp(~SEG0+SEG1, fluxes_H_SEG_daily)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # Compute Pearson correlation coefficient
    r <- cor(fluxes_H_SEG_daily$SEG0, fluxes_H_SEG_daily$SEG1, method="pearson", use = "complete.obs")
    r <- paste("r: ", round(r,2))
    
    # create plot
    seg_h <- ggplot(fluxes_H_SEG_daily, aes(x=SEG0, y=SEG1)) +
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
    pca <- prcomp(~SES0+SES1, fluxes_H_SES_daily)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # Compute Pearson correlation coefficient
    r <- cor(fluxes_H_SES_daily$SES0, fluxes_H_SES_daily$SES1, method="pearson", use = "complete.obs")
    r <- paste("r: ", round(r,2))
    
    # create plot
    ses_h <- ggplot(fluxes_H_SES_daily, aes(x=SES0, y=SES1)) +
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
    pca <- prcomp(~SEG0+SEG1, fluxes_LE_SEG_daily)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # Compute Pearson correlation coefficient
    r <- cor(fluxes_LE_SEG_daily$SEG0, fluxes_LE_SEG_daily$SEG1, method="pearson", use = "complete.obs")
    r <- paste("r: ", round(r,2))
    
    # create plot
    seg_le <- ggplot(fluxes_LE_SEG_daily, aes(x=SEG0, y=SEG1)) +
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
    pca <- prcomp(~SES0+SES1, fluxes_LE_SES_daily)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # Compute Pearson correlation coefficient
    r <- cor(fluxes_LE_SES_daily$SES0, fluxes_LE_SES_daily$SES1, method="pearson", use = "complete.obs")
    r <- paste("r: ", round(r,2))
    
    # create plot
    ses_le <- ggplot(fluxes_LE_SES_daily, aes(x=SES0, y=SES1)) +
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
    pca <- prcomp(~SEG0+SEG1, fluxes_NEE_SEG_daily)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # Compute Pearson correlation coefficient
    r <- cor(fluxes_NEE_SEG_daily$SEG0, fluxes_NEE_SEG_daily$SEG1, method="pearson", use = "complete.obs")
    r <- paste("r: ", round(r,2))
    
    # create plot
    seg_nee <- ggplot(fluxes_NEE_SEG_daily, aes(x=SEG0, y=SEG1)) +
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
    pca <- prcomp(~SES0+SES1, fluxes_NEE_SES_daily)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # Compute Pearson correlation coefficient
    r <- cor(fluxes_NEE_SES_daily$SES0, fluxes_NEE_SES_daily$SES1, method="pearson", use = "complete.obs")
    r <- paste("r: ", round(r,2))
    
    # create plot
    ses_nee <- ggplot(fluxes_NEE_SES_daily, aes(x=SES0, y=SES1)) +
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
  
  filename = "plots/EC0 vs EC1 comparison B - daily"
  
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
{
  ## Resample time series to desired temporal resolution
  ## Resample H
  fluxes_H_SEG_monthly <- fluxes_H %>%
    mutate(year = format(datetime, format = "%Y"),
           month = format(datetime, format = "%m"),
           day = format(datetime, format = "%d")
           ) %>% 
    group_by(year, month) %>% 
    select(SEG0,
           SEG1) %>% 
    summarize(
      SEG0 = mean(SEG0),
      SEG1 = mean(SEG1)
    )

  fluxes_H_SES_monthly <- fluxes_H %>%
    mutate(year = format(datetime, format = "%Y"),
           month = format(datetime, format = "%m"),
           day = format(datetime, format = "%d")
    ) %>% 
    group_by(year, month) %>% 
    select(SES0,
           SES1) %>% 
    na.omit %>%  # drop incomplete half hours
    summarize(
      SES0 = mean(SES0),
      SES1 = mean(SES1)
      )
  
  
  ## Resample LE
  fluxes_LE_SEG_monthly <- fluxes_LE %>%
    mutate(year = format(datetime, format = "%Y"),
           month = format(datetime, format = "%m"),
           day = format(datetime, format = "%d")
    ) %>% 
    group_by(year, month) %>% 
    select(SEG0,
           SEG1) %>% 
    na.omit %>%  # drop incomplete half hours
    summarize(
      SEG0 = mean(SEG0),
      SEG1 = mean(SEG1)
    )
  
  fluxes_LE_SES_monthly <- fluxes_LE %>%
    mutate(year = format(datetime, format = "%Y"),
           month = format(datetime, format = "%m"),
           day = format(datetime, format = "%d")
    ) %>% 
    group_by(year, month) %>% 
    select(SES0,
           SES1) %>% 
    na.omit %>%  # drop incomplete half hours
    summarize(
      SES0 = mean(SES0),
      SES1 = mean(SES1)
    )

  
  ## Resample NEE
  fluxes_NEE_SEG_monthly <- fluxes_NEE %>%
    mutate(year = format(datetime, format = "%Y"),
           month = format(datetime, format = "%m"),
           day = format(datetime, format = "%d")
    ) %>% 
    group_by(year, month) %>% 
    select(SEG0,
           SEG1) %>% 
    na.omit %>%  # drop incomplete half hours
    summarize(
      SEG0 = mean(SEG0),
      SEG1 = mean(SEG1)
    )
  
  fluxes_NEE_SES_monthly <- fluxes_NEE %>%
    mutate(year = format(datetime, format = "%Y"),
           month = format(datetime, format = "%m"),
           day = format(datetime, format = "%d")
    ) %>% 
    group_by(year, month) %>% 
    select(SES0,
           SES1) %>% 
    na.omit %>%  # drop incomplete half hours
    summarize(
      SES0 = mean(SES0),
      SES1 = mean(SES1)
    )
 
  ## Determine axis limits
  lims_h  <- range(range(fluxes_H_SEG_monthly[, c("SEG0",  "SEG1")]), range(fluxes_H_SES_monthly[, c("SES0",  "SES1")]))
  lims_le  <- range(range(fluxes_LE_SEG_monthly[, c("SEG0",  "SEG1")]), range(fluxes_LE_SES_monthly[, c("SES0",  "SES1")]))
  lims_nee  <- range(range(fluxes_NEE_SEG_monthly[, c("SEG0",  "SEG1")]), range(fluxes_NEE_SES_monthly[, c("SES0",  "SES1")]))
  
  
  
  ## H / US-Seg
  {
    # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
    pca <- prcomp(~SEG0+SEG1, fluxes_H_SEG_monthly)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # Compute Pearson correlation coefficient
    r <- cor(fluxes_H_SEG_monthly$SEG0, fluxes_H_SEG_monthly$SEG1, method="pearson", use = "complete.obs")
    r <- paste("r: ", round(r,2))
    
    # create plot
    seg_h <- ggplot(fluxes_H_SEG_monthly, aes(x=SEG0, y=SEG1)) +
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
    pca <- prcomp(~SES0+SES1, fluxes_H_SES_monthly)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # Compute Pearson correlation coefficient
    r <- cor(fluxes_H_SES_monthly$SES0, fluxes_H_SES_monthly$SES1, method="pearson", use = "complete.obs")
    r <- paste("r: ", round(r,2))
    
    # create plot
    ses_h <- ggplot(fluxes_H_SES_monthly, aes(x=SES0, y=SES1)) +
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
    pca <- prcomp(~SEG0+SEG1, fluxes_LE_SEG_monthly)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # Compute Pearson correlation coefficient
    r <- cor(fluxes_LE_SEG_monthly$SEG0, fluxes_LE_SEG_monthly$SEG1, method="pearson", use = "complete.obs")
    r <- paste("r: ", round(r,2))
    
    # create plot
    seg_le <- ggplot(fluxes_LE_SEG_monthly, aes(x=SEG0, y=SEG1)) +
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
    pca <- prcomp(~SES0+SES1, fluxes_LE_SES_monthly)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # Compute Pearson correlation coefficient
    r <- cor(fluxes_LE_SES_monthly$SES0, fluxes_LE_SES_monthly$SES1, method="pearson", use = "complete.obs")
    r <- paste("r: ", round(r,2))
    
    # create plot
    ses_le <- ggplot(fluxes_LE_SES_monthly, aes(x=SES0, y=SES1)) +
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
    pca <- prcomp(~SEG0+SEG1, fluxes_NEE_SEG_monthly)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # Compute Pearson correlation coefficient
    r <- cor(fluxes_NEE_SEG_monthly$SEG0, fluxes_NEE_SEG_monthly$SEG1, method="pearson", use = "complete.obs")
    r <- paste("r: ", round(r,2))
    
    # create plot
    seg_nee <- ggplot(fluxes_NEE_SEG_monthly, aes(x=SEG0, y=SEG1)) +
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
    pca <- prcomp(~SES0+SES1, fluxes_NEE_SES_monthly)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # Compute Pearson correlation coefficient
    r <- cor(fluxes_NEE_SES_monthly$SES0, fluxes_NEE_SES_monthly$SES1, method="pearson", use = "complete.obs")
    r <- paste("r: ", round(r,2))
    
    # create plot
    ses_nee <- ggplot(fluxes_NEE_SES_monthly, aes(x=SES0, y=SES1)) +
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
  
  filename = "plots/EC0 vs EC1 comparison C - monthly"
  
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
    LE_f_cum_mm = cumsum(LE_f) * 1800 / 2260 / 1000,  # Calculate mm of cumulative evaporation. Where cumsum(LE_f) is the cumulative MW m-2, 1800 = seconds in 30 mins, 2260 =  specific latent heat = energy required to evaporate 1 g of water in J g-1, and '/1000' converts from g m-2 to mm.
    H_f_cum = cumsum(H_f)
    )


DF_EC_study_SES <- DF_EC_study %>% 
  filter(Station == c("SES_EC0", "SES_EC1", "SES_EC2", "SES_EC3", "SES_EC4")) %>% 
  group_by(Station) %>% 
  mutate(
    NEE_uStar_f_cum = cumsum(NEE_uStar_f) *(12.0107/10^6) * 1800,  # Convert umol m-2 s-1 to g C m-2 30 min-1. Where 12.0107 g C/mole * 1 gram /10^6 ugrams * time (1800 s)
    LE_f_cum = cumsum(LE_f),
    LE_f_cum_mm = cumsum(LE_f) * 1800 / 2260 / 1000,  # Calculate mm of cumulative evaporation. Where cumsum(LE_f) is the cumulative MW m-2, 1800 = seconds in 30 mins, 2260 =  specific latent heat = energy required to evaporate 1 g of water in J g-1, and '/1000' converts from g m-2 to mm.
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


