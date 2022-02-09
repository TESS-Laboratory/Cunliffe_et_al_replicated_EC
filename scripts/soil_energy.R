# Utility script for preparing soil energy flux data to calculate soil storage terms


# ------------- 0. Setup Environment ----------
## Load packages
library(tidyverse)

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



## Define paths
## specify input file path
inpath <- "C:/workspace/JULES/flux_and_soil_input_data"  # Quality Controlled soil met files from UNM


## Define land cover at each site
## Based on classification of fine spatial resoltuion drone data 
# Site	Bare Ground	C4 grasses	Evergreen shrubs
# Seg	  0.68	      0.31	      0.01
# Ses	  0.66	      0.1	        0.24
SEG_bare <- 0.68
SES_bare <- 0.66



## Create list of files for each site
seg.soil.file.ls <- paste(inpath, list.files(path=inpath, pattern="^GLand_"), sep="/")
ses.soil.file.ls <- paste(inpath, list.files(path=inpath, pattern="^SLand"), sep="/")

## Read in data
seg.soil <- seg.soil.file.ls %>%
  purrr::map(.x=., ~read_csv(.x,
                             na = c("", "NA", -9999),
                             col_types = cols(TIMESTAMP_START = col_datetime("%d-%b-%Y %H:%M:%S"),
                                              TIMESTAMP_END = col_datetime("%d-%b-%Y %H:%M:%S")))) %>%
  bind_rows()


ses.soil <- ses.soil.file.ls %>%
  purrr::map(.x=., ~read_csv(.x,
                             na = c("", "NA", -9999),
                             col_types = cols(TIMESTAMP_START = col_datetime("%d-%b-%Y %H:%M:%S"),
                                              TIMESTAMP_END = col_datetime("%d-%b-%Y %H:%M:%S")))) %>%
  bind_rows()







## Tidy data
SEG_SHF <- seg.soil %>%
  mutate(
    datetime = TIMESTAMP_START,
    datetime = datetime + 15*60,  # adding 15 minutes (in seconds) to change datetime to center of half hour
    SHF_G_AVG = ifelse(is.nan(SHF_G_AVG), NA, SHF_G_AVG), # Replace NaN with NA
    SHF_O_AVG = ifelse(is.nan(SHF_O_AVG), NA, SHF_O_AVG) # Replace NaN with NA
    ) %>% 
  select(
    datetime,
    SHF_G_AVG,
    SHF_O_AVG,
    SHF_ALL_AVG
  )


SES_SHF <- ses.soil %>%
  mutate(
    datetime = TIMESTAMP_START,
    datetime = datetime + 15*60,  # adding 15 minutes (in seconds) to change datetime to center of half hour
    SHF_S_AVG = ifelse(is.nan(SHF_S_AVG), NA, SHF_S_AVG), # Replace NaN with NA
    SHF_O_AVG = ifelse(is.nan(SHF_O_AVG), NA, SHF_O_AVG) # Replace NaN with NA
  ) %>% 
  select(
    datetime,
    SHF_S_AVG,
    SHF_O_AVG,
    SHF_ALL_AVG
  )



## Subset time series to study period dates
subset_datetime <- function(df) {
  df %>%
    filter(between(datetime,
                   as.POSIXct("2018-11-01 00:00:00"),
                   as.POSIXct("2020-10-31 23:30:00")))
}

SEG_SHF <- subset_datetime(SEG_SHF)
SES_SHF <- subset_datetime(SES_SHF)


## Calculate area weighted average of SHF_G2_AVG and SHF_O1_AVG for each site
SEG_SHF <- SEG_SHF %>%
  mutate(
    SHF_weighted_mean = ((SHF_O_AVG * SEG_bare)+(SHF_G_AVG * (1-SEG_bare)))
  ) %>% 
  select(
    datetime,
    SHF_G_AVG,
    SHF_O_AVG,
    SHF_weighted_mean,
    SHF_ALL_AVG
  )

SES_SHF <- SES_SHF %>%
  mutate(
    SHF_weighted_mean = ((SHF_O_AVG * SES_bare)+(SHF_S_AVG * (1-SES_bare)))
  ) %>% 
  select(
    datetime,
    SHF_S_AVG,
    SHF_O_AVG,
    SHF_weighted_mean,
    SHF_ALL_AVG
  )


## Compare Average with weighted average
pca <- prcomp(~SHF_weighted_mean+SHF_ALL_AVG, SEG_SHF)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute  Lin's  correlation concordance coefficient
ccc_result <- DescTools::CCC(SEG_SHF$SHF_weighted_mean, SEG_SHF$SHF_ALL_AVG, ci = "z-transform", conf.level = 0.95, na.rm = T)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 2))

seg_plot <- ggplot(SEG_SHF, aes(x=SHF_weighted_mean, y=SHF_ALL_AVG)) +
  labs(x = expression("SHF_weighted_mean"),
       y = expression("SHF_ALL_AVG"),
       title = "SEG") +
  geom_hex(bins = 100, na.rm=T, show.legend=T) +
  scale_fill_continuous(type = "viridis") +     # colour palette
  xlim(-500,500) +
  ylim(-500,500) +
  theme_fancy() +
  theme(legend.position = c(0.9, 0.2))  +
  geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
  geom_abline(intercept = tls_int, slope = tls_slp) +
  annotate("text", x = -330, y = 500, label = equation) +
  annotate("text", x = -360, y = 400, label = ccc)



pca <- prcomp(~SHF_weighted_mean+SHF_ALL_AVG, SES_SHF)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute  Lin's  correlation concordance coefficient
ccc_result <- DescTools::CCC(SES_SHF$SHF_weighted_mean, SES_SHF$SHF_ALL_AVG, ci = "z-transform", conf.level = 0.95, na.rm = T)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 2))

ses_plot <- ggplot(SES_SHF, aes(x=SHF_weighted_mean, y=SHF_ALL_AVG)) +
  labs(x = expression("SHF_weighted_mean"),
       y = expression("SHF_ALL_AVG"),
       title = "SES") +
  geom_hex(bins = 100, na.rm=T, show.legend=T) +
  scale_fill_continuous(type = "viridis") +     # colour palette
  xlim(-500,500) +
  ylim(-500,500) +
  theme_fancy() +
  theme(legend.position = c(0.9, 0.2))  +
  geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
  geom_abline(intercept = tls_int, slope = tls_slp) +
  annotate("text", x = -330, y = 500, label = equation) +
  annotate("text", x = -360, y = 400, label = ccc)



## Review gaps
# Gaps in SEG 
sum(is.na(SEG_SHF$SHF_weighted_mean))  # 604 observations missing, 
sum(is.na(SEG_SHF$SHF_weighted_mean)) / 30587 # 1.97% of total

sum(is.na(SES_SHF$SHF_weighted_mean))  # 2578 observations missing, 
sum(is.na(SES_SHF$SHF_weighted_mean)) / 30587 # 8.43% of total



## Save data 
# Save outputs in data in this repo here!
write.csv(SEG_SHF,"data/soil_energy/soil_heat_flux_SEG.csv", row.names = FALSE)
write.csv(SES_SHF,"data/soil_energy/soil_heat_flux_SES.csv", row.names = FALSE)

ggsave(
  seg_plot,
  filename = "data/soil_energy/seg comparison of average soil heat flux.png",
  width = 16,
  height = 16,
  units = "cm"
)

ggsave(
  ses_plot,
  filename = "data/soil_energy/ses comparison of average soil heat flux.png",
  width = 16,
  height = 16,
  units = "cm"
)



