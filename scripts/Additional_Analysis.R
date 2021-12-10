## This script sources gap filled data output from ReddyProc and AmeriFlux,
## merges them into a single file and performs visualization and data analysis



# ------ 0.0 Setup Environment ----------
## Load packages
library(tidyverse)
library(patchwork)   
library(tls)   
library(DescTools)


## Define paths
## NB. these data are ca. 150 GB

## Paths Andy's machine
# path  <-  "C:/workspace/REC_7_Data/8_datasets/"  # Unfilled EdiRe output
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



# NB. SEG2 instrument failed on 2020-02-19, so LE_filled and NEE_filled are 
# invalid after this date and should be excluded from analysis.

fluxes <- fluxes %>% 
  mutate(LE_filled = ifelse(datetime > as.Date("2020-03-29") & Station == "SEG2",
         NA,
         LE_filled),
         NEE_filled = ifelse(datetime > as.Date("2020-03-29") & Station == "SEG2",
                            NA,
                            NEE_filled))


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

# # Compute Pearson correlation coefficient
# r <- cor(fluxes_H$SEG0, fluxes_H$SEG1, method="pearson", use = "complete.obs")
# r <- paste("r: ", round(r,2))

# Compute  Lin's  correlation concordance coefficient
ccc_result <- DescTools::CCC(fluxes_H$SEG0, fluxes_H$SEG1, ci = "z-transform", conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 2))

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
  annotate("text", x = -250, y = 650, label = equation) +
  annotate("text", x = -440, y = 530, label = ccc)

} # H / US-Seg


## H / US-Ses
{
  # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
  pca <- prcomp(~SES0+SES1, fluxes_H)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # # Compute Pearson correlation coefficient
  # r <- cor(fluxes_H$SES0, fluxes_H$SES1, method="pearson", use = "complete.obs")
  # r <- paste("r: ", round(r,2))
  
  # Compute  Lin's  correlation concordance coefficient
  ccc_result <- DescTools::CCC(fluxes_H$SES0, fluxes_H$SES1, ci = "z-transform", conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 2))
  
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
    annotate("text", x = -250, y = 650, label = equation) +
    annotate("text", x = -420, y = 530, label = ccc)
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
  
  # # Compute Pearson correlation coefficient
  # r <- cor(fluxes_LE$SEG0, fluxes_LE$SEG1, method="pearson", use = "complete.obs")
  # r <- paste("r: ", round(r,2))
  
  # Compute  Lin's  correlation concordance coefficient
  ccc_result <- DescTools::CCC(fluxes_LE$SEG0, fluxes_LE$SEG1, ci = "z-transform", conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 2))
  
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
    annotate("text", x = 10, y = 430, label = equation) +
    annotate("text", x = -70, y = 380, label = ccc)
  
} # LE / US-Seg


# LE / US-Ses
{
  # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
  pca <- prcomp(~SES0+SES1, fluxes_LE)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # # Compute Pearson correlation coefficient
  # r <- cor(fluxes_LE$SES0, fluxes_LE$SES1, method="pearson", use = "complete.obs")
  # r <- paste("r: ", round(r,2))
  
  # Compute  Lin's  correlation concordance coefficient
  ccc_result <- DescTools::CCC(fluxes_LE$SES0, fluxes_LE$SES1, ci = "z-transform", conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 2))
  
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
    annotate("text", x = 25, y = 430, label = equation) +
    annotate("text", x = -80, y = 380, label = ccc)
  
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
  
  # # Compute Pearson correlation coefficient
  # r <- cor(fluxes_NEE$SEG0, fluxes_NEE$SEG1, method="pearson", use = "complete.obs")
  # r <- paste("r: ", round(r,2))
  
  # Compute  Lin's  correlation concordance coefficient
  ccc_result <- DescTools::CCC(fluxes_NEE$SEG0, fluxes_NEE$SEG1, ci = "z-transform", conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 2))
  
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
    annotate("text", x = -4.4, y = 10.6, label = equation) +
    annotate("text", x = -7, y = 8.7, label = ccc)
  
} # NEE / US-Seg


## NEE / US-Ses
{
  # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
  pca <- prcomp(~SES0+SES1, fluxes_NEE)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
  
  # # Compute Pearson correlation coefficient
  # r <- cor(fluxes_NEE$SES0, fluxes_NEE$SES1, method="pearson", use = "complete.obs")
  # r <- paste("r: ", round(r,2))
  
  # Compute  Lin's  correlation concordance coefficient
  ccc_result <- DescTools::CCC(fluxes_NEE$SES0, fluxes_NEE$SES1, ci = "z-transform", conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 2))
  
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
    annotate("text", x = -4.5, y = 10.6, label = equation) +
    annotate("text", x = -6.6, y = 8.6, label = ccc)
  
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
    
    # # Compute Pearson correlation coefficient
    # r <- cor(fluxes_H_SEG_daily$SEG0, fluxes_H_SEG_daily$SEG1, method="pearson", use = "complete.obs")
    # r <- paste("r: ", round(r,2))
    # 
        # Compute  Lin's  correlation concordance coefficient
    ccc_result <- DescTools::CCC(fluxes_H_SEG_daily$SEG0, fluxes_H_SEG_daily$SEG1, ci = "z-transform", conf.level = 0.95)
    ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 2))
    
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
      annotate("text", x = 77, y = (lims_h[1] + 0.99 * abs(lims_h[1] - lims_h[2])), label = equation) +
      annotate("text", x = 25, y = (lims_h[1] + 0.90 * abs(lims_h[1] - lims_h[2])), label = ccc)
  } # H / US-Seg
  
  
  # H / US-Ses
  {
    # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
    pca <- prcomp(~SES0+SES1, fluxes_H_SES_daily)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # # Compute Pearson correlation coefficient
    # r <- cor(fluxes_H_SES_daily$SES0, fluxes_H_SES_daily$SES1, method="pearson", use = "complete.obs")
    # r <- paste("r: ", round(r,2))
    
    # Compute  Lin's  correlation concordance coefficient
    ccc_result <- DescTools::CCC(fluxes_H_SES_daily$SES0, fluxes_H_SES_daily$SES1, ci = "z-transform", conf.level = 0.95)
    ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 2))
    
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
      annotate("text", x = 77, y = (lims_h[1] + 0.99 * abs(lims_h[1] - lims_h[2])), label = equation) +
      annotate("text", x = 25, y = (lims_h[1] + 0.90 * abs(lims_h[1] - lims_h[2])), label = ccc)
    
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
    
    # # Compute Pearson correlation coefficient
    # r <- cor(fluxes_LE_SEG_daily$SEG0, fluxes_LE_SEG_daily$SEG1, method="pearson", use = "complete.obs")
    # r <- paste("r: ", round(r,2))
    
    # Compute  Lin's  correlation concordance coefficient
    ccc_result <- DescTools::CCC(fluxes_LE_SEG_daily$SEG0, fluxes_LE_SEG_daily$SEG1, ci = "z-transform", conf.level = 0.95)
    ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 2))
        
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
      annotate("text", x = 26, y = (lims_le[1] + 0.99 * abs(lims_le[1] - lims_le[2])), label = equation) +
      annotate("text", x = 14, y = (lims_le[1] + 0.90 * abs(lims_le[1] - lims_le[2])), label = ccc)
    
  } # LE / US-Seg
  
  
  # LE / US-Ses
  {
    # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
    pca <- prcomp(~SES0+SES1, fluxes_LE_SES_daily)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # # Compute Pearson correlation coefficient
    # r <- cor(fluxes_LE_SES_daily$SES0, fluxes_LE_SES_daily$SES1, method="pearson", use = "complete.obs")
    # r <- paste("r: ", round(r,2))
    
    # Compute  Lin's  correlation concordance coefficient
    ccc_result <- DescTools::CCC(fluxes_LE_SES_daily$SES0, fluxes_LE_SES_daily$SES1, ci = "z-transform", conf.level = 0.95)
    ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 2))
    
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
      annotate("text", x = 26, y = (lims_le[1] + 0.99 * abs(lims_le[1] - lims_le[2])), label = equation) +
      annotate("text", x = 14, y = (lims_le[1] + 0.90 * abs(lims_le[1] - lims_le[2])), label = ccc)
    
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
    
    # # Compute Pearson correlation coefficient
    # r <- cor(fluxes_NEE_SEG_daily$SEG0, fluxes_NEE_SEG_daily$SEG1, method="pearson", use = "complete.obs")
    # r <- paste("r: ", round(r,2))
    
    # Compute  Lin's  correlation concordance coefficient
    ccc_result <- DescTools::CCC(fluxes_NEE_SEG_daily$SEG0, fluxes_NEE_SEG_daily$SEG1, ci = "z-transform", conf.level = 0.95)
    ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 2))
    
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
      annotate("text", x = -0.3, y = (lims_nee[1] + 0.99 * abs(lims_nee[1] - lims_nee[2])), label = equation) +
      annotate("text", x = -0.92, y = (lims_nee[1] + 0.90 * abs(lims_nee[1] - lims_nee[2])), label = ccc)
    
  } # NEE / US-Seg
  
  
  # NEE / US-Ses
  {
    # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
    pca <- prcomp(~SES0+SES1, fluxes_NEE_SES_daily)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # # Compute Pearson correlation coefficient
    # r <- cor(fluxes_NEE_SES_daily$SES0, fluxes_NEE_SES_daily$SES1, method="pearson", use = "complete.obs")
    # r <- paste("r: ", round(r,2))
    
    # Compute  Lin's  correlation concordance coefficient
    ccc_result <- DescTools::CCC(fluxes_NEE_SES_daily$SES0, fluxes_NEE_SES_daily$SES1, ci = "z-transform", conf.level = 0.95)
    ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 2))
    
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
      annotate("text", x = -0.35, y = (lims_nee[1] + 0.99 * abs(lims_nee[1] - lims_nee[2])), label = equation) +
      annotate("text", x = -0.92, y = (lims_nee[1] + 0.90 * abs(lims_nee[1] - lims_nee[2])), label = ccc)
    
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
    
    # # Compute Pearson correlation coefficient
    # r <- cor(fluxes_H_SEG_monthly$SEG0, fluxes_H_SEG_monthly$SEG1, method="pearson", use = "complete.obs")
    # r <- paste("r: ", round(r,2))
    
    # Compute  Lin's  correlation concordance coefficient
    ccc_result <- DescTools::CCC(fluxes_H_SEG_monthly$SEG0, fluxes_H_SEG_monthly$SEG1, ci = "z-transform", conf.level = 0.95)
    ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 2))
    
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
      annotate("text", x = 46, y = (lims_h[1] + 0.99 * abs(lims_h[1] - lims_h[2])), label = equation) +
      annotate("text", x = 36, y = (lims_h[1] + 0.90 * abs(lims_h[1] - lims_h[2])), label = ccc)
    
  } # H / US-Seg
  
  
  # H / US-Ses
  {
    # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
    pca <- prcomp(~SES0+SES1, fluxes_H_SES_monthly)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # # Compute Pearson correlation coefficient
    # r <- cor(fluxes_H_SES_monthly$SES0, fluxes_H_SES_monthly$SES1, method="pearson", use = "complete.obs")
    # r <- paste("r: ", round(r,2))
    
    # Compute  Lin's  correlation concordance coefficient
    ccc_result <- DescTools::CCC(fluxes_H_SES_monthly$SES0, fluxes_H_SES_monthly$SES1, ci = "z-transform", conf.level = 0.95)
    ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 2))
    
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
      annotate("text", x = 46, y = (lims_h[1] + 0.99 * abs(lims_h[1] - lims_h[2])), label = equation) +
      annotate("text", x = 36, y = (lims_h[1] + 0.90 * abs(lims_h[1] - lims_h[2])), label = ccc)

  } # H / US-Ses
  
  
  # LE / US-Seg
  {
    # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
    pca <- prcomp(~SEG0+SEG1, fluxes_LE_SEG_monthly)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # # Compute Pearson correlation coefficient
    # r <- cor(fluxes_LE_SEG_monthly$SEG0, fluxes_LE_SEG_monthly$SEG1, method="pearson", use = "complete.obs")
    # r <- paste("r: ", round(r,2))
    
    # Compute  Lin's  correlation concordance coefficient
    ccc_result <- DescTools::CCC(fluxes_LE_SEG_monthly$SEG0, fluxes_LE_SEG_monthly$SEG1, ci = "z-transform", conf.level = 0.95)
    ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 2))
    
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
      annotate("text", x = 13.3, y = (lims_le[1] + 0.99 * abs(lims_le[1] - lims_le[2])), label = equation) +
      annotate("text", x = 10, y = (lims_le[1] + 0.90 * abs(lims_le[1] - lims_le[2])), label = ccc)
    
  } # LE / US-Seg
  
  
  # LE / US-Ses
  {
    # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
    pca <- prcomp(~SES0+SES1, fluxes_LE_SES_monthly)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # # Compute Pearson correlation coefficient
    # r <- cor(fluxes_LE_SES_monthly$SES0, fluxes_LE_SES_monthly$SES1, method="pearson", use = "complete.obs")
    # r <- paste("r: ", round(r,2))
    
    # Compute  Lin's  correlation concordance coefficient
    ccc_result <- DescTools::CCC(fluxes_LE_SES_monthly$SES0, fluxes_LE_SES_monthly$SES1, ci = "z-transform", conf.level = 0.95)
    ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 2))
    
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
      annotate("text", x = 13.3, y = (lims_le[1] + 0.99 * abs(lims_le[1] - lims_le[2])), label = equation) +
      annotate("text", x = 10, y = (lims_le[1] + 0.90 * abs(lims_le[1] - lims_le[2])), label = ccc)
  } # LE / US-Ses
  
  
  # NEE / US-Seg
  {
    # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
    pca <- prcomp(~SEG0+SEG1, fluxes_NEE_SEG_monthly)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # # Compute Pearson correlation coefficient
    # r <- cor(fluxes_NEE_SEG_monthly$SEG0, fluxes_NEE_SEG_monthly$SEG1, method="pearson", use = "complete.obs")
    # r <- paste("r: ", round(r,2))
    
    # Compute  Lin's  correlation concordance coefficient
    ccc_result <- DescTools::CCC(fluxes_NEE_SEG_monthly$SEG0, fluxes_NEE_SEG_monthly$SEG1, ci = "z-transform", conf.level = 0.95)
    ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 2))
    
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
      annotate("text", x = -0.25, y = (lims_nee[1] + 0.99 * abs(lims_nee[1] - lims_nee[2])), label = equation) +
      annotate("text", x = -0.35, y = (lims_nee[1] + 0.90 * abs(lims_nee[1] - lims_nee[2])), label = ccc)
    
  } # NEE / US-Seg
  
  
  # NEE / US-Ses
  {
    # Fit linear model with Total Least Squares regression (extracted from base-R PCA function)
    pca <- prcomp(~SES0+SES1, fluxes_NEE_SES_monthly)
    tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
    tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
    
    equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")
    
    # # Compute Pearson correlation coefficient
    # r <- cor(fluxes_NEE_SES_monthly$SES0, fluxes_NEE_SES_monthly$SES1, method="pearson", use = "complete.obs")
    # r <- paste("r: ", round(r,2))
    
    # Compute  Lin's  correlation concordance coefficient
    ccc_result <- DescTools::CCC(fluxes_NEE_SES_monthly$SES0, fluxes_NEE_SES_monthly$SES1, ci = "z-transform", conf.level = 0.95)
    ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 2))
    
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
      annotate("text", x = -0.25, y = (lims_nee[1] + 0.99 * abs(lims_nee[1] - lims_nee[2])), label = equation) +
      annotate("text", x = -0.35, y = (lims_nee[1] + 0.90 * abs(lims_nee[1] - lims_nee[2])), label = ccc)
    
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

# subset to site and compute cumulative fluxes
fluxes_SEG_cum <- fluxes %>% 
  filter(Station == c("SEG0", "SEG1", "SEG2", "SEG3", "SEG4")) %>% 
  group_by(Station) %>% 
  mutate(
    NEE_cum = cumsum(NEE_filled) *(12.0107/10^6) * 1800,  # Convert umol m^2 s-1 to g C m-2 30 min-1. Where 12.0107 g C/mole * 1 gram /10^6 ugrams * time (1800 s)
    LE_cum = cumsum(LE_filled),
    LE_cum_mm = cumsum(LE_filled) * 1800 / 2260 / 1000,  # Convert W m^2 to mm of cumulative evapotranspiration. Where cumsum(LE_f) is the cumulative W m-2, 1800 = seconds in 30 mins, 2260 =  specific latent heat = energy required to evaporate 1 g of water in J g-1, and '/1000' converts from g m-2 to mm.
    H_cum = cumsum(H_filled)
  ) %>% 
  select(datetime,
         Station,
         NEE_cum,
         LE_cum,
         LE_cum_mm,
         H_cum)

fluxes_SES_cum <- fluxes %>% 
  filter(Station == c("SES0", "SES1", "SES2", "SES3", "SES4")) %>% 
  group_by(Station) %>% 
  mutate(
    NEE_cum = cumsum(NEE_filled) *(12.0107/10^6) * 1800,  # Convert umol m-2 s-1 to g C m-2 30 min-1. Where 12.0107 g C/mole * 1 gram /10^6 ugrams * time (1800 s)
    LE_cum = cumsum(LE_filled),
    LE_cum_mm = cumsum(LE_filled) * 1800 / 2260 / 1000,  # Convert W m^2 to mm of cumulative evapotranspiration. Where cumsum(LE_f) is the cumulative W m-2, 1800 = seconds in 30 mins, 2260 =  specific latent heat = energy required to evaporate 1 g of water in J g-1, and '/1000' converts from g m-2 to mm.
    H_cum = cumsum(H_filled)
  ) %>% 
  select(datetime,
         Station,
         NEE_cum,
         LE_cum,
         LE_cum_mm,
         H_cum)



# TO DO ----
### NB. review conversion from LE (MW m-2) to evaporation (mm)

# 400 MW = 400,000,000 W
# 400,000,000 W = 400,000,000 j s-1
# 400,000,000 W j s-1 * 1800 second in 30 mins = 720,000 j
# 720,000 / 2260 = 318.58 g of water per m-2
# 318.58 g of water per m-2 = 0.318 mm evapotranspiration
# This estimate seems implausibly small

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



## Determine axis limits
lims_le <- range(range(fluxes_SEG_cum$LE_cum, na.rm=T),range(fluxes_SES_cum$LE_cum, na.rm=T))
lims_le_mm <- range(range(fluxes_SEG_cum$LE_cum_mm, na.rm=T),range(fluxes_SES_cum$LE_cum_mm, na.rm=T))
lims_nee <- range(range(fluxes_SEG_cum$NEE_cum, na.rm=T),range(fluxes_SES_cum$NEE_cum, na.rm=T))

# colour mappings
selected_colours <- c("orange",
                      "purple",
                      "green",
                      "darkgreen",
                      "brown",
                      "cyan")

# labs <- c("EC0", "EC1",    "EC2",    "EC3",  "EC4", "Precipit")




{  # Create Cumulative LE and NEE plots
 #set params for legend theme
  leg_pos <- 0.14
  leg_tx <- 8
  
  
## SEG Cumulative LE
{
  (cum_seg_le <- ggplot(fluxes_SEG_cum,
                        aes(y=LE_cum, 
                            x=datetime,
                            color=Station
                        )) +
     labs(x = "",
          y = expression("Cumulative evapotranspiration (W m"^-2*")", sep=""),
          title = "LE Grassland") +
     geom_line(na.rm=T) +
     scale_color_manual(values=selected_colours) +
     ylim(lims_le) +
     theme_fancy() +
     theme(legend.position = c(leg_pos, 0.8),
           legend.text = element_text(size=leg_tx))
  )
} # SEG Cumulative LE


## SEG Cumulative LE mm
{
  (cum_seg_le_mm <- ggplot(fluxes_SEG_cum,
                        aes(y=LE_cum_mm, 
                            x=datetime,
                            color=Station
                        )) +
     labs(x = "",
          y = expression("Cumulative evapotranspiration (mm)", sep=""),
          title = "LE Grassland") +
     geom_line(na.rm=T) +
     scale_color_manual(values=selected_colours) +
     ylim(lims_le_mm) +
     theme_fancy() +
     theme(legend.position = c(leg_pos, 0.8),
           legend.text = element_text(size=leg_tx)) # legend position
  )
} # SEG Cumulative LE



## SES Cumulative LE
{
  (cum_ses_le <- ggplot(fluxes_SES_cum,
                       aes(y=LE_cum, 
                           x=datetime,
                           color=Station
                       )) +
     labs(x = "",
          y = expression("Cumulative evapotranspiration (W m"^-2*")", sep=""),
          title = "LE Shrubland") +
     geom_line(na.rm=T) +
     scale_color_manual(values=selected_colours) +
     ylim(lims_le) +
     theme_fancy() +
     theme(legend.position = c(leg_pos, 0.8),
           legend.text = element_text(size=leg_tx)) # legend position
   )
} # SES Cumulative LE


## SES Cumulative LE mm
{
  (cum_ses_le_mm <- ggplot(fluxes_SES_cum,
                       aes(y=LE_cum_mm, 
                           x=datetime,
                           color=Station
                       )) +
     labs(x = "",
          y = expression("Cumulative evapotranspiration (mm)", sep=""),
          title = "LE Shrubland") +
     geom_line(na.rm=T) +
     scale_color_manual(values=selected_colours) +
     ylim(lims_le_mm) +
     theme_fancy() +
     theme(legend.position = c(leg_pos, 0.8),
           legend.text = element_text(size=leg_tx)) # legend position
  )
} # SES Cumulative LE mm





## SEG Cumulative NEE
{
  (cum_seg_nee <- ggplot(fluxes_SEG_cum,
                        aes(y=NEE_cum, 
                            x=datetime,
                            color=Station
                        )) +
     labs(x = "",
          y = expression("Cumulative NEE (g C m"^"-2"*")", sep=""),
          title = "NEE Grassland") +
     geom_line(na.rm=T) +
     scale_color_manual(values=selected_colours) +
     ylim(lims_nee) +
     theme_fancy() +
     theme(legend.position = c(leg_pos, 0.23),
           legend.text = element_text(size=leg_tx)) # legend position
  )
}



## SES Cumulative NEE
{
  (cum_ses_nee <- ggplot(fluxes_SES_cum,
                         aes(y=NEE_cum, 
                             x=datetime,
                             color=Station
                         )) +
     labs(x = "",
          y = expression("Cumulative NEE (g C m"^"-2"*")", sep=""),
          title = "NEE Shrubland") +
     geom_line(na.rm=T) +
     scale_color_manual(values=selected_colours) +
     ylim(lims_nee) +
     theme_fancy() +
     theme(legend.position = c(leg_pos, 0.23),
           legend.text = element_text(size=leg_tx)) # legend position
  )
}



## combine plots using Patchwork
pall <- (cum_seg_le_mm + cum_ses_le_mm) / (cum_seg_nee + cum_ses_nee) +
  plot_annotation(tag_levels = 'a') & theme(plot.tag.position = c(0.0, 0.97))

filename = "plots/Cumulative LE and NEE"

ggsave(
  pall,
  filename = paste0(filename, ".png"),
  width = 18,
  height = 18,
  units = "cm"
)

ggsave(
  pall,
  filename = paste0(filename, ".pdf"),
  width = 18,
  height = 18,
  units = "cm"
)
}  # Create Cumulative LE and NEE plots






# add precipitation
    # geom_line(aes(y = P_plot_le_gm, colour = "prec")) +
    # scale_y_continuous(sec.axis = sec_axis(~.*-ylim_p[2]/diff(ylim_l) + (ylim_p[2]), name = ya2)) +
    # theme(axis.text.x = element_text(angle = 45, vjust=0.3)) +
    # theme(legend.title = element_blank(), legend.text = element_text(size = 8), legend.position = c(0.18, 0.52)) +    # legend position
    # theme(axis.title.y.right = element_text( angle = 90)) +   # Rotate secondary axis


#   # ylim correction factor for precipitation
#   
#   # diff(ylim_l)/ylim_p[2]   = 20.41817
#   # diff(ylim_l)/20.41817    = 36.32201    # desired ylim
#   
#   # diff(ylim_c)/ylim_p[2]   =  1.629766
#   # diff(ylim_c)/ 1.629766   =  36.322     # desired ylim
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
#  
# }  # cumulative LE and NEE
# 


