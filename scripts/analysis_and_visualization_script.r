## This script sources data output from EdiRe and AmeriFlux,
## merges them into a single file
## performs visualization and data analysis

# Tested and working with R version 4.0.3


#### Load packages
library(chron)
library(oce)
library(lubridate)                                                              # only for 'month()' function
library(scales)                                                                 # for alpha in base r plots
library(plotrix)                                                                # for std.error() function
library(ggplot2)
library(patchwork)                                                              # to arrange plots
library(fields)                                                                 # to plot footprint
library("BiocManager")
BiocManager::install("EBImage")
library(spatialfil)
library(viridis)                                                                # colour palette
library(SPEI)




###
### Paths for sourcing data ####
###
## Local version on Fabio's laptop
# path  <-  "E:/REC_7_Data/8_datasets/"
# rpath  <-  "E:/REC_7_Data/11_ReddyProc/"
# mpath  <-  "E:/REC_7_Data/12_Marcys_data/"

## P drive
path  <-  "P:/REC_7_Data/8_datasets/"
rpath  <-  "P:/REC_7_Data/11_ReddyProc/"
mpath  <-  "P:/REC_7_Data/12_Marcys_data/"


###
### Initialize lists ####
###
sites  <-  c("SEG", "SES");
recs  <-  1:4
towers  <-  paste(rep(sites, each=4), rep(recs,2), sep="_REC")
systems  <-  c(towers, sites)
mlabs  <-  c("Seg", "Ses")
soildatasets  <-  c("soilg", "soils")
asystems  <-  c(systems, 
                "SEG_AVG4",
                "SES_AVG4",
                "SEG_AVG3",
                "SES_AVG3",
                "SEG_AVG_23",
                "SEG_AVG_34",
                "SEG_AVG_42",
                "SES_AVG_23",
                "SES_AVG_34",
                "SES_AVG_42"); 
fluxes  <-  c("H", "cLE", "rLE", "Fc", "Hc", "cLEc", "rLEc", "Fcc") 
fluxes_reddy  <-  c("NEE", "LE", "H_r", "NEE_uStar_f", "LE_f", "H_f", "Reco_DT_uStar", "GPP_DT_uStar")
all_fluxes  <-  c(fluxes, fluxes_reddy)
pfluxes  <-  c("H", "Fc", "LE", "Hc", "LEc", "Fcc", "WPL_LE", "WPL_Fc", "LEcw", "Fccw")

colours <- c("orange", "purple", "green", "darkgreen", "brown", "cyan")         # Define colour scheme for visualization.


last_date    <-  "2020_01_01_from_flash_Txcor_"
last_date_2  <-  "2020_02_19_from_flash_Txcor_"

last_date_reddy  <-  "_2019_365"          # used for files ID    ----- from point 2.2 !!! 
last_date_use  <-  "01/11/2019"           # used for chron filtering

date_start  <-  "01/11/2018"              # default: "01/11/2018"
date_end  <-    "01/11/2019"              # default: "01/11/2019"



xch  <-  xct  <-  ""; if(grepl("Txcor", last_date)){xch  <-  "_Txcor"; xct  <-  "Txcor_"}

mdatasets  <-  c("gm", "sm") 
pdatasets  <-  c("gp", "sp")
datasets  <-  c("g1", "g2", "g3", "g4", "s1", "s2", "s3", "s4")
adatasets  <-  c(datasets, mdatasets, "ga4", "sa4", "ga3", "sa3", 
             "ga23", "ga34", "ga42", "sa23", "sa34", "sa42");


datasets_f  <-  paste(datasets, "f", sep="_")
datasets_fa  <-  c(datasets_f, "gm_f", "sm_f", "ga4_f", "sa4_f", "ga3_f", "sa3_f",
               "ga23_f", "ga34_f", "ga42_f", "sa23_f", "sa34_f", "sa42_f")


make_txt_for_reddy  <-  F        # prepare datasets for gapfilling
run_reddy_proc  <-  F            # run gapfilling code




### 
### 0.0 customized functions   =====================
### 
if(T){
  
  ### flux-specific uncertainty from Hollinger 2005 
  # https://stats.stackexchange.com/questions/281682/how-to-fit-a-data-against-a-laplace-double-exponential-distribution-and-check
  laplace_err <- function(x){sqrt(2) * mean(abs(x - median(x, na.rm=T)), na.rm=T) / sqrt(sum(!is.na(x)))}
  
  ### find how many empty rows/cols there are here
  sum_na <- function(x){sum(is.na(x))}
  
  # consider data up to X days before/after a certain time
  day3_fun <- function(x){x <- x:(x+144)}   # consider data up to 3 days (144 h)after a certain time
  pm_days_fun <- function(x, pm_days){x <- (x-(pm_days*48)):(x+(pm_days*48))}   # consider data up to ndays before and after a certain time
  pm_days_be_fun <- function(x, pm_days){x <- (x[[1]]-(pm_days*48)):(x[[2]]+(pm_days*48))}   # consider data up to ndays before and after a time span
  
  
  # calculate the sd of cumsum
  cumsum_unc <- function(x){x <- sqrt( cumsum( x^2 ) )}    
  
  
  # remove last element from an entire vector
  sub_last <- function(x){x <- x-x[length(x)]}
  
  
  # create custom ggplot2 theme
  theme_fancy  <-  function() {
    theme_bw() +
    theme(
    text = element_text(family = "Helvetica"),
    axis.text = element_text(size = 8, colour = "black"),
    axis.title = element_text(size =  10, colour = "black"),
    axis.line.x = element_line(size = 0.3, colour = "black"),
    axis.line.y = element_line(size = 0.3, colour = "black"),
    axis.ticks = element_line(size = 0.3, colour = "black"),
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
          colour = "black"
          ),
    legend.text = element_text(size = 10, colour = "black"),
    legend.title = element_text(size = 10, colour = "black"),
    legend.position = c(0.9, 0.9),
    legend.key.size = unit(0.9, "line"),
    legend.background = element_rect(
          colour = "black",
          fill = "transparent",
          size = 2,
          linetype = "blank"
          )
     )
  }

}






### 
### 1.0 get REC csv datasets   =====================
### 
for(i in 1:8){
  
  
  dat  <- read.csv(file=paste(path, last_date, towers[i], "_flux.csv", sep=""), header=TRUE, sep=",")
  dat2 <- read.csv(file=paste(path, last_date_2, towers[i], "_flux.csv", sep=""), header=TRUE, sep=",")
  fluxesc <- fluxes
  
  
  # check for double dt raws
  # a
  # 1 2 3 4 5 6 7 8 9 
  # 3 1 2 1 1 1 1 1 1 
  # > which(table(a)==2)
  
  
  if(T){  # add dt to dat
    
    ## sometimes, empty factor columns that are not visible in Excel are added
    dat <- dat[, !grepl("X", colnames(dat))     ]
    
    dtimes <- as.character(dat[,1])
    
    dtparts  <-  t(as.data.frame(strsplit(dtimes,' '))); row.names(dtparts) = NULL
    if(unique(nchar(dtparts[,2]))==5)dtparts[,2] <- paste(dtparts[,2], ":00", sep="")
    thetimes0  <-  chron(dates=dtparts[,1],times=dtparts[,2],format=c("d/m/y","h:m:s"))
    #thetimes <-  as.POSIXct(paste(as.Date(dates(thetimes0)),times(thetimes0)%%1), tz="GMT") # Mountain Standard Time (New Mexico)
    
    
    # remove 1 hour (timestamp issue)
    #thetimes0[thetimes0 < "24/02/19"] <- thetimes0[thetimes0 < "24/02/19"]-1/24
    if(i %in% c(1:8))thetimes0 <- thetimes0-1/24     
    
    
    # This solves a problem with rounding of chrom date-times after subtracting  1/24---------
    dt_char <- as.character(thetimes0)                                 # without, dat and datm will not merge and lines with same name will be created
    dt_char_c <- substring(dt_char,2,nchar(dt_char)-1)
    
    dtparts2  <-  t(as.data.frame(strsplit(dt_char_c,' '))); row.names(dtparts2) = NULL
    if(unique(nchar(dtparts2[,2]))==5)dtparts2[,2] <- paste(dtparts2[,2], ":00", sep="")
    thetimes0  <-  chron(dates=dtparts2[,1],times=dtparts2[,2],format=c("d/m/y","h:m:s"))  
    dat$dt <- thetimes0
    ###
  }  # end - add dt to dat
  
  # dat1 and dat2 has different time format (only SEG1)!  
  # dat2: 1/1/20 00:15:00
  # dat1: 01/11/2018 00:15
  
  
  if(i %in% c(1:8)){
    if(T){  # add dt to dat2
      
      ## sometimes, empty fator columns that are not visible in excel are added
      dat2 <- dat2[, !grepl("X", colnames(dat2))     ]
      
      dtimes2 <- as.character(dat2[,1])
      
      dtparts2  <-  t(as.data.frame(strsplit(dtimes2,' '))); row.names(dtparts2) = NULL
      if(unique(nchar(dtparts2[,2]))==5)dtparts2[,2] <- paste(dtparts2[,2], ":00", sep="")
      thetimes0_2  <-  chron(dates=dtparts2[,1],times=dtparts2[,2],format=c("d/m/y","h:m:s"))
      #thetimes <-  as.POSIXct(paste(as.Date(dates(thetimes0)),times(thetimes0)%%1), tz="GMT") # Mountain Standard Time (New Mexico)
      
      dat2$dt <- thetimes0_2
      ###
    }  # end - add dt to dat2
    
    dat <- rbind(dat, dat2)     # join dat and dat2
  }
  
  ## need a time series with no gaps 

  load(file="E:/REC_7_Data/9_R/Rdata/DC_time_series.RData") # time series up to 2020-12-31
  
  dat <- merge(dat, DC_time_series, by="dt", all=T)  

  dat <- dat[dat[,"dt"] >= thetimes0[1], ]
  dat <- dat[dat[,"dt"] <= thetimes0_2[length(thetimes0_2)], ]
  
  
  
  # be sure that there is no factor in the datasets
  #for(j in 2:ncol(dat)){
  #  dat[,j] <- as.numeric(as.character(dat[,j]))
  #}
  
  
  
  ####### NAs introduced by coercion!
  
  
  
  
  
  
  # remove outliers/despike before ustar/gapfilling as in FLUXNET_2015 data processing protocol
  
  # remove outliers according to Tukey's Fence: 
  # 1.5 times the interquartile difference (3 times iqd, it is "far out") 
  
  for(fx in fluxesc){ # iqd*30 is 10 times what is considered "far out"
    #fx <- "Fcc"
    vec <- as.numeric(as.character(dat[,fx]))
    #vec <- dat[,fx]
    med_p <- median(vec[vec>=0], na.rm=T); top_p <- med_p*30 
    med_n <- median(vec[vec<0], na.rm=T);  top_n <- med_n*30
    vec[vec>top_p | vec<top_n] <- NA; dat[,fx] <- vec
  }
  
   dat[, fluxesc] <- despike(dat[, fluxesc])    # despike from oce package
  
  
  
  
  if(i %in% c(1:8)){
    # rename all columns but "dt
    colnames(dat)[colnames(dat)!="dt"] <- paste(colnames(dat)[colnames(dat)!="dt"], datasets[i], sep="_")
    assign(datasets[i], dat)
    assign(paste("dt_", datasets[i], sep=""), thetimes0)
  } else {
    colnames(dat)[colnames(dat)!="dt"] <- paste(colnames(dat)[colnames(dat)!="dt"], pdatasets[i-8], sep="_")
    assign(pdatasets[i-8], dat)
    
  }
  
  
}   # 9 & 10 are AmeriFlux's Data (uncorrected) with EdiRe processing




#### 
#### 2.0 get AmeriFlux's csv datasets ==========
#### 
for(im in 1:2){
    
   
    d18 <- read.csv(file=paste(mpath, "US-", mlabs[im], 
           "_HH_201801010000_201901010000.csv", sep=""), header=TRUE, sep=",")
    d19 <- read.csv(file=paste(mpath, "US-", mlabs[im], 
           "_HH_201901010000_202001010000.csv", sep=""), header=TRUE, sep=",")
                              #"_HH_201901010000_202001010000_xtr2.csv", sep=""), header=TRUE, sep=",")
    
    #d18 <- rbind(d18, c(201812312330, 201901010000, rep(-9999, ncol(d18)-2)))   # last line is missing
    datm <- rbind(d18, d19)
    
    
    
    # invert AmeriFlux wind directions (CSAT vs Windmaster)
    datm[,"WD"] <- datm[,"WD"]+180; datm[ datm[,"WD"]>360 ,"WD"] <-  datm[datm[,"WD"]>360  ,"WD"]-360
    
    
    datm[datm==-9999] <- NA
    
    date_string <- datm[,"TIMESTAMP_START"]
    
    yy <- substring(date_string, 3,4)
    mm <- substring(date_string, 5,6)
    dd <- substring(date_string, 7,8)
    
    hh <- substring(date_string, 9,10)
    min <- as.numeric(substring(date_string, 11,12))+15
    
    mdates <- paste(dd,mm,yy, sep="/")
    mtimes <- paste(hh, min, "00", sep=":")
    
    mthetimes0  <-  chron(dates=mdates,times=mtimes,format=c("d/m/y","h:m:s"))
    mthetimes <-  as.POSIXct(paste(as.Date(dates(mthetimes0)),times(mthetimes0)%%1), tz="GMT") # MST = Mountain Standard Time (New Mexico)
    
    
    # rm outliers/despike before ustar/gapfilling as in FLUXNET_2015 data processing protocol
    # AmeriFlux data includes big spikes
    
    # remove outliers according to Tukey's Fence: 
    # 1.5 times the interquartile difference (3 times iqd, it is "far out")
    
    if(F){
    for(fx in c("H", "LE", "FC")){ # iqd*30 is 10 times what is considered "far out"
      #dc <- "Fcc_g1"
      #vec <- as.numeric(datf[,dc])
      vec <- datm[,fx]
      med_p <- median(vec[vec>=0], na.rm=T); top_p <- med_p*30 
      med_n <- median(vec[vec<0], na.rm=T);  top_n <- med_n*30
      vec[vec>top_p | vec<top_n] <- NA; datm[,fx] <- vec
    }
    
    datm[,c("H", "LE", "FC")] <- despike(datm[,c("H", "LE", "FC")])    # despike from oce package
    }
    
    colnames(datm)[3:4] <- c("Fc", "cLE") # same colnames as REC datasets
    colnames(datm) <- paste(colnames(datm), mdatasets[im], sep="_")
    datm$dt <- mthetimes0
    
    assign(mdatasets[im], datm)
    
    
    #if(im==1)save(gm, file=paste("E:/REC_7_Data/9_R/Rdata/gm.RData", sep=""))
    #if(im==2)save(sm, file=paste("E:/REC_7_Data/9_R/Rdata/sm.RData", sep=""))
  }





####
#### 2.0.1 get soil data from AmeriFlux ============
####
for(is in 1:2){
  
  g18 <- read.table(file=paste(mpath, "soil_data/Previous_received_20201125/", substring(sites[is], 3), "Land_2018_soilmet_qc.txt",sep=""), header=TRUE, sep=",")
  g19 <- read.table(file=paste(mpath, "soil_data/Previous_received_20201125/", substring(sites[is], 3), "Land_2019_soilmet_qc.txt",sep=""), header=TRUE, sep=",")
  
  gdat <- rbind(g18, g19)
  
  mdatesg <- paste(gdat[,"day"],  gdat[,"month"], gdat[,"year"], sep="/")
  mtimesg <- paste(gdat[,"hour"], gdat[,"min"],   "00", sep=":")
  gdt0 <-  chron(dates=mdatesg, times=mtimesg, format=c("d/m/y","h:m:s"))
  gdt <- gdt0+1/(24*4)   # move dt forward by 15 min to match dt of the other datasets
  
  
  
  # --------------------------------- this ugly piece of code is to solve a problem with rounding of chrom date-times after subtracting  1/24
  dt_char <- as.character(gdt)                                 # without, dat and datm will not merge and lines with same name will be created
  dt_char_c <- substring(dt_char,2,nchar(dt_char)-1)
  
  dtparts2  <-  t(as.data.frame(strsplit(dt_char_c,' '))); row.names(dtparts2) = NULL
  if(unique(nchar(dtparts2[,2]))==5)dtparts2[,2] <- paste(dtparts2[,2], ":00", sep="")
  thetimes0  <-  chron(dates=dtparts2[,1],times=dtparts2[,2],format=c("d/m/y","h:m:s"))  
  ####
  
  
  colnames(gdat) <- paste(colnames(gdat), soildatasets[is], sep="_")
  
  gdat$dt <- thetimes0

  
  assign(soildatasets[is], gdat)
  
  
}





####
#### 2.1  produce unfilled txt files for Reddy Proc (14) ======
#### 
if(make_txt_for_reddy){
  
  ## loop for merging REC datasets with AmeriFlux's data
  mdata_vec <- rep(mdatasets, each=4)
  for(i in 1:8){
    
    dtir <- get(datasets[i])
    dtim <- get(mdata_vec[i])
    
    dti <- merge(dtir, dtim, by="dt", all=T)
    
    #dti <- dti[dti[,"dt"] >= dtir[1,"dt"], ]
    dti <- dti[dti[,"dt"] >= "01/10/2018", ]
    
    dti <- dti[dti[,"dt"] <= "30/12/2021", ]
    
    #dti <- dti[dti[,"dt"] <= last_date_use, ]   
    #dti <- dti[dti[,"dt"] <= dtir[nrow(dtir),"dt"], ]
    
    #dti <- dti[!is.na(dti[,paste("Date.Time_", datasets[i], sep="")]),] # filter Jan-Oct 2018
    #dti <- dti[!is.na(dti[,"TIMESTAMP_START"]),]
    
    Year <- as.integer(format(as.Date(dates(dti[,"dt"])), "%Y"))
    DoY <- yday(as.Date(dates(dti[,"dt"]))) # lubridate
    Hour <- chron::hours(dti[,"dt"])+(chron::minutes(dti[,"dt"])-15)/60
    
    NEE <- dti[,paste("Fcc", datasets[i], sep="_")]
    LE <- dti[,paste("cLEc", datasets[i], sep="_")]
    H <- dti[,paste("Hc", datasets[i], sep="_")]
    
    Ustar <- dti[,paste("friction_velocity", datasets[i], sep="_")]
    rH <- dti[,paste("RH_F", mdata_vec[i], sep="_")]    # these are filled with Reddy altrady (see 3.0)
    
    
    # from AmeriFlux's data
    Tair <- dti[,paste("TA_F", mdata_vec[i], sep="_") ] 
    Rg <- dti[,paste("SW_IN", mdata_vec[i], sep="_")]     # Rg = global radiation = total short wave rediation from the sun
    VPD <- dti[,paste("VPD_F", mdata_vec[i], sep="_")]    # VPD = vapor pressure deficit  
    
    Tsoil <-  -9999
    
    
    
    # prepare txt file
    txt <- data.frame("Year"=Year, "DoY"=DoY, "Hour"=Hour, "NEE"=NEE, "LE"=LE, "H"=H,
                    "Rg"=Rg, "Tair"=Tair, "Tsoil"=Tsoil, "rH"=rH, "VPD"=VPD, "Ustar"=Ustar)
    

    
    
    # -------- turn NAs into -9999
    txt[is.na(txt)]  <-  -9999
    
    colnm <- colnames(txt)
    units <- c("--","--","--","umolm-2s-1", "Wm-2", "Wm-2", "Wm-2", "degC", "DegC", "%", 
             "hPa", "ms-1")
    
    

    fd <- paste("_", txt[nrow(txt),1], "_", txt[nrow(txt),2], sep="")  

    #filet <- "E:/REC_7_Data/11_ReddyProc/SEG_REC1.txt"
    filet <- paste("E:/REC_7_Data/11_ReddyProc/", towers[i], fd, "_unfilled", xch, ".txt", sep="")
    cat(c(colnm,"\n"), file=filet, sep="\t")
    cat(c(units, "\n"), file=filet, append=TRUE, sep="\t")
    write.table(txt, file=filet, append=TRUE, row.names=F, col.names=F, sep="\t")
    
  }     # 1:8
  
  
  ## rearrange AmeriFlux's data to get Reddy's input file
  for (im in 1:2){
    
    dm_tr <- get(mdatasets[im])
    
    
    Year <- as.integer(format(as.Date(dates(dm_tr[,"dt"])), "%Y"))
    DoY <- yday(as.Date(dates(dm_tr[,"dt"]))) # lubridate
    Hour <- chron::hours(dm_tr[,"dt"])+(chron::minutes(dm_tr[,"dt"])-15)/60
    
    NEE <- dm_tr[, paste("Fc",  mdatasets[im], sep="_")]
    LE  <- dm_tr[, paste("cLE", mdatasets[im], sep="_")]
    H   <- dm_tr[, paste("H",   mdatasets[im], sep="_")]
    
    Ustar <- dm_tr[, paste("USTAR", mdatasets[im], sep="_")]
    rH <- dm_tr[, paste("RH_F", mdatasets[im], sep="_")]             
    
    
    # from AmeriFlux's data
    Tair <- dm_tr[,paste("TA_F",  mdatasets[im], sep="_")]   # filled with reddy (see 3.0)
    Rg   <- dm_tr[,paste("SW_IN", mdatasets[im], sep="_")]   # Rg = global radiation = total short wave rediation from the sun
    VPD  <- dm_tr[,paste("VPD_F", mdatasets[im], sep="_")]   # VPD = vapor pressure deficit  
    
    Tsoil <-  -9999
    
    
    
    # prepare txt file
    txt <- data.frame("Year"=Year, "DoY"=DoY, "Hour"=Hour, "NEE"=NEE, "LE"=LE, "H"=H,
                    "Rg"=Rg, "Tair"=Tair, "Tsoil"=Tsoil, "rH"=rH, "VPD"=VPD, "Ustar"=Ustar)
    
    
    # -------- turn NAs into -9999
    txt[is.na(txt)]  <-  -9999
    
    colnm <- colnames(txt)
    units <- c("--","--","--","umolm-2s-1", "Wm-2", "Wm-2", "Wm-2", "degC", "DegC", "%", 
             "hPa", "ms-1")
    
    
    
    fd <- paste("_", txt[nrow(txt),1], "_", txt[nrow(txt),2], sep="")
    
    
    #filet <- "E:/REC_7_Data/11_ReddyProc/SEG_REC1.txt"
    filet <- paste("E:/REC_7_Data/11_ReddyProc/", sites[im], fd, "_unfilled", xch, ".txt", sep="")
    cat(c(colnm,"\n"), file=filet, sep="\t")
    cat(c(units, "\n"), file=filet, append=TRUE, sep="\t")
    write.table(txt, file=filet, append=TRUE, row.names=F, col.names=F, sep="\t")
    
  }  

  
  ## make Reddy's input file for mean fluxes from multiple towers (4, 3 and 2x3 RECs)
  for(ia in 1:10){   # 1:10
    
    # there are 2 ways of doing the average of reddy_fluxes!
    # 1) reddy the avg_REC  <-  preferred; reddy input is less noisy so output has better quality
    # 2) reddy individual RECs and avg
    
    
    
    #### get 5 datasets, and merge them all by dt
    
    dat5 <- Reduce(function(x,y) merge(x=x, y=y, by="dt", all=T), list(g1, g2, g3, g4, gm))
    sts <- rep("_g", each=4);
    
    if(ia %in% c(2, 4, 6, 8, 10)){
      dat5 <- Reduce(function(x,y) merge(x=x, y=y, by="dt", all=T), list(s1, s2, s3, s4, sm))
      sts <- rep("_s", each=4); 
    }
    
    dat5 <- dat5[ dat5[,"dt"]>="01/10/18" & dat5[,"dt"]<=last_date_use  ,]
    
  
    ## make avg fluxes
    wrecs <- recs; iam <- ia; avg_lab <- "_AVG4"
    if(ia %in% c(3, 4)){wrecs <- recs[2:4]; iam <- ia-2; avg_lab <- "_AVG3"}
    if(ia %in% c(5, 6)){wrecs <- recs[c(2, 3)]; iam <- ia-4; avg_lab <- "_AVG_23"}
    if(ia %in% c(7, 8)){wrecs <- recs[c(3, 4)]; iam <- ia-6; avg_lab <- "_AVG_34"}
    if(ia %in% c(9,10)){wrecs <- recs[c(4, 2)]; iam <- ia-8; avg_lab <- "_AVG_42"}
    
    mat <- matrix(NA, ncol=4, nrow=nrow(dat5)); 
    colnames(mat) <- c("Hc", "cLEc", "Fcc", "friction_velocity")
    
    for(i in 1:ncol(mat)){
      acols <- paste(colnames(mat)[i],sts[i], wrecs, sep="")
      mat[,i]  <- apply( dat5[,acols], 1, mean)   # make average only when data from all towers are available
    }
    
    
    
    
    Year <- as.integer(format(as.Date(dates(dat5[,"dt"])), "%Y"))
    DoY <- yday(as.Date(dates(dat5[,"dt"]))) # lubridate
    Hour <- chron::hours(dat5 [,"dt"])+(chron::minutes(dat5[,"dt"])-15)/60
    
    NEE <- mat[,"Fcc"]
    LE <- mat[,"cLEc"]
    H <- mat[,"Hc"]
    
    Ustar <- mat[,"friction_velocity"]
    #Ustar <- dat5[,paste("USTAR", mdatasets[ia], sep="_")]  # used until 30/08/2019 
    rH <- dat5[,paste("RH_F", mdatasets[iam], sep="_")]    # use AmeriFlux's because there is a lot of attenuation
                                                        # in the REC. LE has freq-response-corr; RH does not
    
    # from AmeriFlux's data
    Tair <- dat5[,paste("TA_F", mdatasets[iam], sep="_") ]   # filled with reddy (see 3.0)
    Rg <- dat5[,paste("SW_IN", mdatasets[iam], sep="_")]     # Rg = global radiation = total short wave rediation from the sun
    VPD <- dat5[,paste("VPD_F", mdatasets[iam], sep="_")]    # VPD = vapor pressure deficit  
    
    Tsoil <-  -9999
    
    
    
    # prepare txt file
    txt <- data.frame("Year"=Year, "DoY"=DoY, "Hour"=Hour, "NEE"=NEE, "LE"=LE, "H"=H,
                    "Rg"=Rg, "Tair"=Tair, "Tsoil"=Tsoil, "rH"=rH, "VPD"=VPD, "Ustar"=Ustar)
    
    
    
    
    # -------- turn NAs into -9999
    txt[is.na(txt)]  <-  -9999
    
    colnm <- colnames(txt)
    units <- c("--","--","--","umolm-2s-1", "Wm-2", "Wm-2", "Wm-2", "degC", "DegC", "%", 
             "hPa", "ms-1")
    
    
    
    fd <- paste("_", txt[nrow(txt),1], "_", txt[nrow(txt),2], sep="")
    
    
    #filet <- "E:/REC_7_Data/11_ReddyProc/SEG_REC1.txt"
    filet <- paste("E:/REC_7_Data/11_ReddyProc/", sites[iam], avg_lab, fd, "_unfilled", xch, ".txt", sep="")
    cat(c(colnm,"\n"), file=filet, sep="\t")
    cat(c(units, "\n"), file=filet, append=TRUE, sep="\t")
    write.table(txt, file=filet, append=TRUE, row.names=F, col.names=F, sep="\t")
    
  }

    
}  




#### 
#### 2.2  produce gap-filled txt files from Reddy Proc ======
#### 
if(run_reddy_proc){
  
  #TO INSTALL:
  #install.packages("REddyProc", repos="http://R-Forge.R-project.org", type="source")
  
  #HELP PAGES:
  #https://www.bgc-jena.mpg.de/bgi/index.php/Services/REddyProcWebRPackage
  # help('REddyProc-package')
  #  help('sEddyProc.example')
  
  
  library(REddyProc)  # Only loading here because this large package isn't often needed.
  library(minpack.lm)

  setwd("E:/REC_7_Data/11_ReddyProc/")

  #systems <- c("SEG_AVG", "SES_AVG")   # to do only avg fluxes
  for(i in c(11:20)){    # 1:20
    
    
    EddyData.F  <-  fLoadTXTIntoDataframe(paste("E:/REC_7_Data/11_ReddyProc/",
                                 asystems[i], last_date_reddy, "_unfilled", xch, ".txt", sep=""))
    
    
    
    
    #+++ Add time stamp in POSIX time format
    #EddyDataWithPosix.F  <-  fConvertTimeToPosix(EddyData.F, 'YMDHM', Year.s='Year', Month.s ='Month', Day.s='Day', Hour.s='Hour',Min.s = 'Minute')
    EddyDataWithPosix.F  <-  fConvertTimeToPosix(EddyData.F, 'YDH', Year='Year', Day='DoY', Hour='Hour')
    
    #+++ Initalize R5 reference class sEddyProc for processing of eddy data
    #+++ with all variables needed for processing later
    EddyProc.C  <-  sEddyProc$new(asystems[i], EddyDataWithPosix.F, c('NEE','LE','H','Rg','Tair','VPD', 'Tsoil', 'Ustar'))
    
    
    LatDeg <- 34.3623; LongDeg <- (-106.7020)
    
    if(asystems[i] %in% c("SES_REC1", "SES_REC2", "SES_REC3", "SES_REC4", "SES", "SES_AVG4", "SES_AVG3")){
      LatDeg <- 34.3350; LongDeg <- (-106.7448)
    }
    
    
    EddyProc.C$sSetLocationInfo(LatDeg=LatDeg, LongDeg=LongDeg, TimeZoneHour=-6)
    
    
    
    ### ATTENTION !!!!
    ### the US-SES datasets returns a weird error when trying to do some of the plots
    ### (error: ep function not found). If you skip those plots, the rest of the script
    ### and the produced dataset have nothing wrong
    
    if(F){
      #+++ Generate plots of all data in directory \plots (of current R working dir)
      if(i!=10){
        EddyProc.C$sPlotHHFluxes('NEE')
        EddyProc.C$sPlotHHFluxes('LE')
        EddyProc.C$sPlotHHFluxes('H')
        EddyProc.C$sPlotHHFluxes('Ustar')
      }
      EddyProc.C$sPlotFingerprint('NEE')
      EddyProc.C$sPlotFingerprint('LE')
      EddyProc.C$sPlotFingerprint('H')
      EddyProc.C$sPlotFingerprint('Rg')
      EddyProc.C$sPlotFingerprint('Tair')
      EddyProc.C$sPlotFingerprint('VPD')
      EddyProc.C$sPlotFingerprint('Ustar')
      EddyProc.C$sPlotDiurnalCycle('Tair')
      EddyProc.C$sPlotDiurnalCycle('NEE')
    }
    #+++ Plot individual months/years to screen (of current R graphics device)
    #EddyProc.C$sPlotHHFluxesY('NEE', Year=2018)
    #EddyProc.C$sPlotFingerprintY('NEE', Year=2018)
    #EddyProc.C$sPlotDiurnalCycleM('NEE', Month=1)
    
    
    #### Ustar filtering 
    #(uStarTh  <-  EddyProc.C$sEstUstarThreshold()$uStarTh)
    # this only calculates thresholds; filtering uses annual values as default
    uStarTh <- EddyProc.C$sEstUstarThold(UstarColName = "Ustar")  
    
    
    write.csv(uStarTh, paste("UstarThresholds_", xct, asystems[i], ".csv", sep=""), row.names=F)
    
    
    
    ##### use seasonal u* values
    EddyProc.C$useSeaonsalUStarThresholds()
    
    
    
    #EddyProc.C$sPlotNEEVersusUStarForSeason( levels(uStarTh$season)[3] ) 
    EddyProc.C$sPlotNEEVersusUStarForSeason( levels(uStarTh$season[4:8]) ) 
    
    
    
    
    #+++ Fill gaps in variables with MDS gap filling algorithm (without prior ustar filtering)
    EddyProc.C$sMDSGapFillAfterUstar('NEE',FillAll=TRUE) #Fill all values to estimate flux uncertainties
    #EddyProc.C$sMDSGapFill('NEE', FillAll=TRUE) #Fill all values to estimate flux uncertainties
    EddyProc.C$sMDSGapFill('LE', FillAll=TRUE) #Fill all values to estimate flux uncertainties
    EddyProc.C$sMDSGapFill('H', FillAll=TRUE) #Fill all values to estimate flux uncertainties
    
    EddyProc.C$sMDSGapFill('Rg', FillAll=FALSE) #Fill only the gaps for the meteo condition, e.g. 'Rg'
    EddyProc.C$sMDSGapFill('Tair', FillAll=FALSE) #Fill only the gaps for the meteo condition, e.g. 'Rg'
    EddyProc.C$sMDSGapFill('Tsoil', FillAll=FALSE) #Fill only the gaps for the meteo condition, e.g. 'Rg'
    EddyProc.C$sMDSGapFill('VPD', FillAll=FALSE) #Fill only the gaps for the meteo condition, e.g. 'Rg'
    ####
    
    
    #EddyProc.C$sGLFluxPartition(suffix='WithUstar')
    EddyProc.C$sGLFluxPartition(suffix='uStar')
    ####
    
    
    
    #+++ Export gap filled and partitioned data to standard data frame
    FilledEddyData.F  <-  EddyProc.C$sExportResults()
    
    
    
    
    
    
    #+++ Save results into (tab-delimited) text file in directory \out
    CombinedData.F  <-  cbind(EddyData.F, FilledEddyData.F)
    #fWriteDataframeToFile(CombinedData.F, 'REC_SEG1_filled_edited.txt')
    
    fWriteDataframeToFile(CombinedData.F, paste(asystems[i], last_date_reddy, '_filled', xch, '.txt', sep=""))
    
    
    
    ## explicitly set NAs to -9999. Was done automatically in fWriteDataframetoFile above
    CombinedData.F[is.na(CombinedData.F)]  <-  -9999
    
    ## write file without unit line (again added automatically above) and comma separated
    file_nm <- paste(asystems[i], last_date_reddy, "_filled_edited", xch, ".txt", sep="")
    write.table(CombinedData.F, file_nm , sep=",", row.names=F, quote=F)
    
    
  }  
  setwd("\\\\isad.isadroot.ex.ac.uk/UOE/User/")   # back to usual working directory

}





#### 
#### 3.0 get gap-filled datasets from ReddyProc =====
#### 
for(ir in c(1:20)){
  
  file_nm <- paste(rpath, asystems[ir], last_date_reddy, "_filled", xch, ".txt", sep="")
  header  <-  read.table(file_nm, nrows = 1, header = FALSE, sep ='\t', stringsAsFactors = FALSE)
  datr     <-  read.table(file_nm, skip = 2, header = FALSE, sep ='\t')
  colnames( datr )  <-  unlist(header)
  
  datr[datr==-9999] <- NA

  ## change format for dates and times
  dt_dates0 <- NULL
  ys <- unique(datr[,"Year"])
  
  for(yi in ys){
    sel <- datr[,"Year"]==yi
    dt_dates0[sel] <- as.character(as.Date(datr[sel,"DoY"], origin=paste(yi-1, "-12-31", sep="")))
  }
  
  dt_dates <- paste(substring(dt_dates0, 9, 10), substring(dt_dates0, 6, 7), substring(dt_dates0, 3, 4), sep="/")
  
  hh <- substring(as.character(trunc(1000+datr[,"Hour"])), 3, 4)
  min <- rep("15", nrow(datr)); min[datr[,"Hour"]%%1==0.5] <- "45"   # fractional part: %%1
  dt_times <- paste(hh, min, "00", sep=":")
  
  thetimes_r  <-  chron(dates=dt_dates, times=dt_times, format=c("d/m/y","h:m:s"))
  
  
  
  colnames(datr)[colnames(datr)=="H"] <- "H_r"   # H is also in the REC.csv
  colnames(datr) <- paste(colnames(datr), adatasets[ir], sep="_")
  datr$dt <- thetimes_r   # thetimes_r
  
  assign(datasets_fa[ir], datr)
}





#### 
#### 4.0 merge datasets / add avg fluxes / add YMD ======
#### 
if(T){
  
  # merge all data ("_f" are filled datasets)
  data <- Reduce(function(x,y) merge(x=x, y=y, by="dt", all=T), 
               list(g1, g1_f, g2, g2_f, g3, g3_f, g4, g4_f, 
                    s1, s1_f, s2, s2_f, s3, s3_f, s4, s4_f, 
                    gm, gm_f, sm, sm_f, 
                    ga4_f, sa4_f, ga3_f, sa3_f,
                    ga23_f, ga34_f, ga42_f, sa23_f, sa34_f, sa42_f,  
                    soilg, soils))      

  
  # convert everything in numeric (sometimes there are issues from the .csv files)
  dcols <- paste(rep(fluxes,  length(datasets)),  rep(datasets,  each=length(fluxes)),  sep="_")

  
  for(dc in dcols){
    if(is.factor(data[,dc])){data[,dc] <- as.numeric(as.character(data[,dc]))}
  }

  
  
  
  # remove data preceding DRIVING-C
  data <- data[data[,"dt"]>=date_start,]   
  data <- data[data[,"dt"]<=date_end,]   
  
  
  
  # turn into NAs all the dcols if there is at least one NA in the row
  #datf[!complete.cases(datf[,dcols]),dcols] <- NA  
  
  

  
  
  
  # add REC_AVG4, REC_AVG3 and 3 AVG2 data (un-filled)
  
  mat4 <- mat3 <- mat23 <- mat34 <- mat42 <- matrix(NA, ncol=length(fluxes)*2, nrow=nrow(data)); 
  colnames(mat4) <- paste(rep(fluxes, 2), rep(c("ga4", "sa4"), each=length(fluxes)), sep="_" )
  colnames(mat3) <- paste(rep(fluxes, 2), rep(c("ga3", "sa3"), each=length(fluxes)), sep="_" )
  
  colnames(mat23) <- paste(rep(fluxes, 2), rep(c("ga23", "sa23"), each=length(fluxes)), sep="_" )
  colnames(mat34) <- paste(rep(fluxes, 2), rep(c("ga34", "sa34"), each=length(fluxes)), sep="_" )
  colnames(mat42) <- paste(rep(fluxes, 2), rep(c("ga42", "sa42"), each=length(fluxes)), sep="_" )
  
  sts <- rep(c("_g", "_s"), each=length(fluxes))
  f2a <- rep(fluxes, 2)
  
  
  for(i in 1:ncol(mat4)){
    acols4 <- paste(f2a[i],sts[i], recs[1:4], sep="")
    mat4[,i]  <- apply( data[,acols4], 1, mean) # mean only when fluxes from all RECs are present 
    #print(c(i, acols4))
    acols3 <- paste(f2a[i],sts[i], recs[1:3], sep="")
    mat3[,i]  <- apply( data[,acols3], 1, mean) # mean only when fluxes from all RECs are present 
    # LEC couples
    acols23 <- paste(f2a[i],sts[i], recs[c(2, 3)], sep="")
    mat23[,i]  <- apply( data[,acols23], 1, mean) 
    acols34 <- paste(f2a[i],sts[i], recs[c(3, 4)], sep="")
    mat34[,i]  <- apply( data[,acols34], 1, mean) 
    acols42 <- paste(f2a[i],sts[i], recs[c(4, 2)], sep="")
    mat42[,i]  <- apply( data[,acols42], 1, mean) 
  }
  
  
  
  
  
  
  data <- cbind(data, mat4, mat3, mat23, mat34, mat42)
  
  
  
  # define fluxes columns
  dcols <- paste(rep(all_fluxes, length(adatasets[adatasets != c("gm", "sm") ])) , 
               rep(adatasets[adatasets != c("gm", "sm") ], each=length(all_fluxes)), sep="_")
  # add AmeriFlux's
  dcols <- c(dcols, "H_gm", "cLE_gm", "Fc_gm", "H_sm", "cLE_sm", "Fc_sm",
         paste(rep(fluxes_reddy, 2), rep(c("gm", "sm"), each=length(fluxes_reddy)), sep="_" )) 
  
  
  # add month, year and hour columns
  yyyy <- year(data[,"dt"]); mon <- month(data[,"dt"]); dd <- day(data[,"dt"]); 
  hh <- chron::hours(data[,"dt"]); min <- minute(data[,"dt"])
  
  seas <- rep(1, nrow(data));      seas[mon %in% c(5, 6, 7)] <- 2
  seas[mon %in% c(8, 9, 10)] <- 3; seas[mon %in% c(11,12,1)] <- 4
  
  
  # add growing season columns (see 4.1 for dates)
  g_seas_gm <- g_seas_sm <- rep(F, nrow(data))
  g_seas_gm[data[,"dt"]>"30/03/2019" & data[,"dt"]<"11/06/2019"] <- T
  g_seas_sm[data[,"dt"]>"23/03/2019" & data[,"dt"]<"05/07/2019"] <- T
  g_seas_gm[data[,"dt"]>"13/09/2019" & data[,"dt"]<"08/10/2019"] <- T
  g_seas_sm[data[,"dt"]>"29/07/2019" & data[,"dt"]<"10/10/2019"] <- T
  
  
  datdd <- cbind(data, g_seas_gm, g_seas_sm, min, hh, dd, mon, seas, yyyy)
  
  
  
  
  
  #save(datdd, file = paste("P:/6_EC_Work_Package/datdd.RData", sep=""))
  #save(datdd, file = paste("E:/REC_7_Data/9_R/Rdata/datdd.RData", sep=""))
  
}


## compare NEE noise 
#std.error(datdd[,"Fcc_g1"])/std.error(datdd[,"Fc_gm"])  # 2.381398        ## all data
#std.error(datdd[,"Fcc_s1"])/std.error(datdd[,"Fc_sm"])  # 1.923183








#### 
#### 4.0.1 Add uncertainty to fluxes from Hollinger et al. (2005)  ======
#### 
if(T){
  
  ### uncertainty following a Laplace distribution rather than guassian distribution
  
  
  dat48 <- datdd[49:nrow(datdd),]
  datc <- matrix(NA, nrow=48, ncol=ncol(datdd)); colnames(datc) <- colnames(datdd)
  dat48 <- rbind(dat48, datc)
  
  mat_res <- matrix(NA, ncol=length(dcols), nrow=nrow(datdd))
  colnames(mat_res) <- dcols
  
  
  # loop over all 20 "towers" (10 are mean fluxes)
  for(i in adatasets){
    SEG <- T; if(substring(i, 1, 1)=="s")SEG <- F
    if(SEG){mi <- "gm"} else {mi <- "sm"}
    
    fli <- dcols[grep(i, dcols)]

    if(i %in% datasets){
      wds <- abs(datdd[,paste("mean_Wind_Spd", i, sep="_")]-dat48[,paste("mean_Wind_Spd", i, sep="_")])<1
      tss <- abs(datdd[,paste("mean_gT", i, sep="_")]      -dat48[,paste("mean_gT", i, sep="_")])<3
    } else {
      wds <- abs(datdd[,paste("WS", mi, sep="_")] - dat48[,paste("WS", mi, sep="_")])<1
      tss <- abs(datdd[,paste("TA", mi, sep="_")] - dat48[,paste("TA", mi, sep="_")])<3
    }  
      
    pfs <- abs(datdd[,paste("PPFD_IN", mi, sep="_")] - dat48[,paste("PPFD_IN", mi, sep="_")])<3
    
    
    # fill residuals matrix
    # x+dx -> dx = 1/sqrt(2) * d(x1-x2)
    sel <- wds & tss & pfs; sel[is.na(sel)] <- F
    for(j in fli){ mat_res[sel, j] <-  1/sqrt(2) * (datdd[sel, j] - dat48[sel, j]) }
    

  
  }
  
  
  
  
  colnames(mat_res) <- paste("res_", dcols, sep="") # change colnames to "res"
  datdd <- cbind(datdd, mat_res)
  

  
}








#### 
#### 4.1 find dates of growing / senescent season =======
#### 
if(F){
  
  
  #### add season filter according to Petrie (2015)
  #### 10 gg consecutive (first/last); threshold = 0.25 gC d-1 
  #### use AmeriFlux's tower as reference   
  
  thres <- 0.25    # 0.25
  wlen <- 10       # 11
  
  if(T){
    
    # find beginning and end of growing season
    
    
    ### make daily sums (new datasets)
    dtimes <- as.character(datdd[,"dt"])
    dtparts  <-  t(as.data.frame(strsplit(dtimes,' '))); row.names(dtparts) = NULL
    ddays <- substring(dtparts[,1], 2, 9)
    
    
    #ddata <- data.frame(ddays, datdd[,"GPP_DT_uStar_gm"], datdd[,"GPP_DT_uStar_sm"])
    ddata <- data.frame(datdd[,"dt"], datdd[,"GPP_DT_uStar_gm"], datdd[,"GPP_DT_uStar_sm"])
    colnames(ddata) <- c("dt", "GPP_DT_uStar_gm", "GPP_DT_uStar_sm")
    dc <- ddata
    
    
    ### convert to g C d-1
    
    ## convert umol m-2 s-1 to gC m-2 30min-1
    ccm <- grep("GPP", colnames(ddata))
    dc[,ccm] <- dc[ccm]*  (12 / 10^6) * 1800     # 12 g C/mole * 1 gram /10^6 ugrams * time (1800 s)
    
    
    # aggregate to gC m-2 d-1
    dggp <- aggregate(dc[,c(2,3)],by=list(date(dc[,"dt"])), sum, na.rm=T); colnames(dggp)[1] <- "Date"
    dggp0 <- dggp  
    
    
    #iris %>% head() is equivalent to head(iris).
    #iris %>% head() %>% summary() is equivalent to summary(head(iris))
    # moving average:
    #ma  <-  function(x, n = 5){filter(x, rep(1 / n, n), sides = 2)}
    # aa <- stats::filter(dggp[,2], 1, sides=1) applies coeff to slot of values
    #If you use dplyr, be careful to specify stats::filter in the function above.
    
    
    # count consecutive days (cd) that satisfy conditions
    # start of growing season
    dggp <- dggp %>%
      dplyr::group_by(return_rleid = {return_rleid = rle(GPP_DT_uStar_gm > thres); rep(seq_along(return_rleid$lengths), return_rleid$lengths)}) %>%
      dplyr::mutate(cd_gm_beg = ifelse(GPP_DT_uStar_gm <= thres, 0, seq_along(return_rleid))) %>%
      dplyr::ungroup() %>% 
      dplyr::select(-return_rleid)
    
    dggp <- dggp %>%
      dplyr::group_by(return_rleid = {return_rleid = rle(GPP_DT_uStar_sm > thres); rep(seq_along(return_rleid$lengths), return_rleid$lengths)}) %>%
      dplyr::mutate(cd_sm_beg = ifelse(GPP_DT_uStar_sm <= thres, 0, seq_along(return_rleid))) %>%
      dplyr::ungroup() %>% 
      dplyr::select(-return_rleid)
    
    
    
    # end of growing season
    dggp <- dggp %>%
      dplyr::group_by(return_rleid = {return_rleid = rle(GPP_DT_uStar_gm < thres); rep(seq_along(return_rleid$lengths), return_rleid$lengths)}) %>%
      dplyr::mutate(cd_gm_end = ifelse(GPP_DT_uStar_gm >= thres, 0, seq_along(return_rleid))) %>%
      dplyr::ungroup() %>% 
      dplyr::select(-return_rleid)
    
    dggp <- dggp %>%
      dplyr::group_by(return_rleid = {return_rleid = rle(GPP_DT_uStar_sm < thres); rep(seq_along(return_rleid$lengths), return_rleid$lengths)}) %>%
      dplyr::mutate(cd_sm_end = ifelse(GPP_DT_uStar_sm >= thres, 0, seq_along(return_rleid))) %>%
      dplyr::ungroup() %>% 
      dplyr::select(-return_rleid)
    
    
    
    
    
    # beginning / end of growing season candidates
    
    beg_gm <- dggp[ dggp[,"cd_gm_beg"]==wlen ,"Date"]-wlen+1
    beg_sm <- dggp[ dggp[,"cd_sm_beg"]==wlen ,"Date"]-wlen+1
    
    end_gm <- dggp[ dggp[,"cd_gm_end"]==wlen ,"Date"]
    end_sm <- dggp[ dggp[,"cd_sm_end"]==wlen ,"Date"]
    
    
    # thres <-  0.25; wlen <- 10
    #beg_gm      
    # 2019-03-30  *
    # 2019-09-13  **
    #end_gm    
    # 2018-11-14
    # 2019-06-11  *
    # 2019-07-22
    # 2019-08-21
    # 2019-10-08 **
    
    
    #beg_sm    
    # 2018-11-01
    # 2019-01-02
    # 2019-03-23  *
    # 2019-07-29  **
    # 2019-08-25
    # 2019-09-16
    #end_sm    
    # 2018-11-25
    # 2019-02-03
    # 2019-07-05  *
    # 2019-10-10  **
  }   # find beginning and end of growing season
  
  
  
  ### length of growing season in days
  gdg <- (as.chron("2019-06-11")-as.chron("2019-03-30"))+(as.chron("2019-10-08")-as.chron("2019-09-13")) # 98
  gds <- (as.chron("2019-07-05")-as.chron("2019-03-23"))+(as.chron("2019-10-10")-as.chron("2019-07-29")) # 177
  
  
  
  
  ### try make a plot GPP vs DOY and compare with Petrie et al (2015), fig 4
  ptest <- dggp0
  ptest$doy <- yday(dggp0[,1])
  ptest_o <- ptest[order(ptest[,"doy"]),]

  plot(ptest_o[,"GPP_DT_uStar_sm"]~ptest_o[,"doy"], type="l")
  lines(ptest_o[,"GPP_DT_uStar_gm"]~ptest_o[,"doy"], col="grey")
  legend("topright", c("SEG", "SES"), pch=16, col=c("grey", "black"))
  
}




#### 
#### 4.3 add hourly precipitation columns ==================
#### 
if(T){
  
  matp <- matrix(NA, ncol=8, nrow=nrow(datdd)); 
  matp <- datdd[, c("yyyy", "mon", "dd", "hh", "P_gm", "P_F_gm", "P_sm", "P_F_sm")]
  colnames(matp)[5:8] <- c("P_hh_gm", "P_F_hh_gm", "P_hh_sm", "P_F_hh_sm")
  
  
  
  # order: date, hour (all 0s, all 1s ect...)
  mm <- aggregate(matp[,5:8],by=list(matp[,"yyyy"], matp[,"mon"], matp[,"dd"], matp[,"hh"]), sum, na.rm=T)
  colnames(mm)[1:4] <- c("yyyy", "mon", "dd", "hh")
  
  
  datdd <- merge(datdd, mm, by=c("yyyy", "mon", "dd", "hh"))
  datdd[ datdd[,"min"]==45, c("P_hh_gm", "P_F_hh_gm", "P_hh_sm", "P_F_hh_sm")] <- 0
  
  ### check if values are always double. If so, force all min==45 to zero
  ### kk <- datdd[, "P_F_hh_gm"]; table(kk)
  
  
  
  # re-order chronologically
  datdd <- datdd[order(datdd[,"dt"]),]
  
  
}






#### 
#### 5.0 process fluxes into monthly and seasonal chunks (for avg days) =======
#### 
if(T){
  ## Mostly useful for common ylims (at the end)
  
  
  ###### total AVG day
  avd_mn <- aggregate(data[,dcols],by=list(chron::hours(data[,"dt"])), mean, na.rm=T)
  avd_sd <- aggregate(data[,dcols],by=list(chron::hours(data[,"dt"])), laplace_err)
  colnames(avd_mn) <- c("HOD", paste(colnames(avd_mn[-1]), "mn",  sep="_"))
  colnames(avd_sd) <- c("HOD", paste(colnames(avd_sd[-1]), "sde", sep="_"))
  avd <- merge(avd_mn, avd_sd, by="HOD")
  
  
  
  ###### dataset for seasonal avg days
  savd_mn <- aggregate(datdd[,dcols],by=list(datdd[,"yyyy"], datdd[,"seas"], chron::hours(datdd[,"dt"])), mean, na.rm=T)
  savd_sd <- aggregate(datdd[,dcols],by=list(datdd[,"yyyy"], datdd[,"seas"], chron::hours(datdd[,"dt"])), laplace_err)
  colnames(savd_mn) <- c("yyyy", "seas", "HOD", paste(colnames(savd_mn)[-c(1:3)], "mn",  sep="_"))
  colnames(savd_sd) <- c("yyyy", "seas", "HOD", paste(colnames(savd_sd)[-c(1:3)], "sde", sep="_"))
  savd_s <- merge(savd_mn, savd_sd, by=c("yyyy", "seas", "HOD"))
  
  
  
  ###### dataset for monthly avg days
  tavd_mn <- aggregate(datdd[,dcols],by=list(datdd[,"yyyy"], datdd[,"mon"], chron::hours(datdd[,"dt"])), mean, na.rm=T)
  tavd_sd <- aggregate(datdd[,dcols],by=list(datdd[,"yyyy"], datdd[,"mon"], chron::hours(datdd[,"dt"])), laplace_err)
  colnames(tavd_mn) <- c("yyyy", "mon", "HOD", paste(colnames(tavd_mn)[-c(1:3)], "mn",  sep="_"))
  colnames(tavd_sd) <- c("yyyy", "mon", "HOD", paste(colnames(tavd_sd)[-c(1:3)], "sde", sep="_"))
  tavd_m <- merge(tavd_mn, tavd_sd, by=c("yyyy", "mon", "HOD"))
  
  
  
  
  hcol  <- c(paste("Hc",   datasets, sep="_"), "H_gm", "H_sm")
  lecol <- c(paste("cLEc", datasets, sep="_"), "cLE_gm", "cLE_sm")
  fcol  <- c(paste("Fcc",  datasets, sep="_"), "Fc_gm", "Fc_sm")
  
  ylim_H <- range(tavd_m[,paste(hcol, "mn", sep="_")]+tavd_m[,paste(hcol, "sde", sep="_")],
                tavd_m[,paste(hcol, "mn", sep="_")]-tavd_m[,paste(hcol, "sde", sep="_")], na.rm=T)
  ylim_LE <- range(tavd_m[,paste(lecol, "mn", sep="_")]+tavd_m[,paste(lecol, "sde", sep="_")],
                 tavd_m[,paste(lecol, "mn", sep="_")]-tavd_m[,paste(lecol, "sde", sep="_")], na.rm=T)
  ylim_F <- range(tavd_m[,paste(fcol, "mn", sep="_")]+tavd_m[,paste(fcol, "sde", sep="_")],
                tavd_m[,paste(fcol, "mn", sep="_")]-tavd_m[,paste(fcol, "sde", sep="_")], na.rm=T)
  
  ylims <- list(ylim_H, ylim_LE, ylim_F)
  
  
  
  
  ylim_H_11  <- range(datdd[, c("H_f_g1",  "H_f_s1",  "H_f_gm",  "H_f_sm")],  na.rm=T)
  ylim_LE_11 <- range(datdd[, c("LE_f_g1", "LE_f_s1", "LE_f_gm", "LE_f_sm")], na.rm=T)
  ylim_F_11  <- range(datdd[, c("NEE_uStar_f_g1",  "NEE_uStar_f_s1",  
                              "NEE_uStar_f_gm",  "NEE_uStar_f_sm")], na.rm=T)
  ylims_11   <- list(ylim_H_11, ylim_LE_11, ylim_F_11)
  
  
  
  
  
  # mean monthly precipitation, air T, SW_IN (Rg), NETRAD
  wea_par <- c("TA", "SW_IN", "NETRAD")
  wea_cols <- paste(rep(wea_par, length(mdatasets)), rep(mdatasets, each=length(wea_par)), sep="_")
  wea_mn <- aggregate(datdd[,wea_cols],by=list(datdd[,"yyyy"], datdd[,"mon"], chron::hours(datdd[,"dt"])), mean, na.rm=T)

  
  
  p_mn <- aggregate(datdd[,c("P_gm", "P_sm")], 
                 by=list(datdd[,"yyyy"], datdd[,"mon"], chron::hours(datdd[,"dt"])), sum, na.rm=T)
  colnames(p_mn) <- c("yyyy", "mon", "HOD", "P_gm_mn", "P_sm_mn")
  
  
  
  # monthly precipitation
  pm <- aggregate(datdd[,c("P_gm", "P_sm")], by=list(datdd[,"yyyy"], datdd[,"mon"]), sum, na.rm=T)
  colnames(pm)[1:2] <- c("yyyy", "mon")
  
}







#### 
#### 6.0 Footprint analysis ======
#### 

if(F){
  ## Natascha kljun 
  
  source("E:/REC_7_Data/9_R/FFP_R/calc_footprint_FFP.R")
  source("E:/REC_7_Data/9_R/FFP_R/calc_footprint_FFP_climatology.R")
  
  ### packages needed for footprint climatology
  #source("https://bioconductor.org/biocLite.R")
  #biocLite("EBImage")
  #library(RcolourBrewer)   # max n:9; need combine with colourRampPalette
  
  
  ### AmeriFlux's footprint are currently done with some data from REC1
  
  datfoot0 <- datdd
  lb <- ""
  time_f <- ""    # can be "day_time", "night_time", ""
  AmeriFlux_f <- T
  xl <- 440      # default
  
  if(time_f=="day_time")  {lb <- "_day";   xl <- 250}
  if(time_f=="night_time"){lb <- "_night"; xl <- 350}
  
  
  wcols <- paste(rep( c("MO_stability", "sigma_V", "friction_velocity", "Wind_Dir"), length(datasets)),
               rep(datasets, each=4), sep="_") 
  
  # last AmeriFlux's corrected fluxes do not have the extra columns
  if(AmeriFlux_f)wcols <- c(wcols, paste( rep( c("zoL", "V_SIGMA", "USTAR", "WD", "SW_IN" ), 2), 
                                    rep(mdatasets, each=5), sep="_"))
  
  
  # remove all rows with some NA-flux
  datfoot0 <- datfoot0[complete.cases(datfoot0[,wcols]),]  # from 7699 to 6945 (90.2 %) for 8 RECs
  
  
  
  # ## land cover map (synthetic)
  # lcm <- matrix(1, ncol=(2*xl)+1, nrow=(2*xl)+1)
  # lcm[1:xl, (xl+1):((2*xl)+1)] <- 2
  # lcm[(xl+1):((2*xl)+1), (xl+1):((2*xl)+1)] <- 3
  # lcm <- apply(lcm, 2, rev); lcm <- t(lcm) # image.plot plots rotated images (FFP is right, so adjust lcm)
  # sel1 <- lcm==1; sel2 <- lcm==2; sel3 <- lcm==3   
  

  
  # ## land cover map
  library(raster)
  library(rgdal)

  
  
  if(T){ ## SEG - cover map
    
    # 0 = barren
    # 1 = shrubland
    # 2 = herbaceous
    # order of the categories in ArcGIS
    
    #seg_tiff <- "U:/ArcGIS/Projects/SEV_land_cover/SEG_cent_circ_extract_tif.tif"
    seg_tiff <- "U:/ArcGIS/Projects/SEV_land_cover/SEG_Extract_SEG_1.tif"
    segr <- raster(seg_tiff); segm0 <- as.matrix(segr, nrow=955, ncol=937)
    segm <- t(apply(segm0, 2, rev))
    # re-orient because climatology's output is set for image.plot (starts filling from ll corner)
    #image.plot(segm)   
    
    
    
    
    rr <- apply(segm, 2, sum_na)
    #sum(rr!=937)               # number of rows not entirely NA - 880
    #table(rr)                  # find row with min amount of data
    which(rr == 906)
    rrna0 <- which(rr == 937); rrna <- rrna0[rrna0 != 25]      # rows to remove
    selr <- !(1:length(rr) %in% rrna)
    
    cc <- apply(segm, 1, sum_na)
    #sum(cc!=955)               # number of cols not entirely NA - 880
    #table(cc)                  # find row with min amount of data
    #which(cc == 921)
    ccna0 <- which(cc == 955); ccna <- ccna0[ccna0 != 14]     # cols to remove
    selc <- !(1:length(cc) %in% ccna)
    
    segmap <- segm[selc, selr]
    
    
    ### cover map partitioning
    round( sum(segmap==0, na.rm=T)/sum(!is.na(segmap)) , 3)   # 0.779    / bare        
    round( sum(segmap==1, na.rm=T)/sum(!is.na(segmap)) , 3)   # 0.004    / shrubs      
    round( sum(segmap==2, na.rm=T)/sum(!is.na(segmap)) , 3)   # 0.217    / herbaceous  
    
  }  ## SEG - cover map
  
  
  if(T){ ## SES - cover map
    
    # 0 = barren
    # 1 = shrubland
    # 2 = herbaceous
    # order of the categories in ArcGIS
    
    
    ses_tiff <- "U:/ArcGIS/Projects/SEV_land_cover/SES_Extract_Bloc1.tif"
    sesr <- raster(ses_tiff); sesm0 <- as.matrix(sesr, nrow=994, ncol=929)
    sesm <- t(apply(sesm0, 2, rev))
    # re-orient because climatology's output is set for image.plot (starts filling from ll corner)
    #image.plot(sesm)   
    
    
    
    
    
    rr <- apply(sesm, 2, sum_na)  # find how many empty rows/cols there are here
    #sum(rr!=929)               # number of rows not entirely NA - 880
    #table(rr)                  # find row with min amount of data
    #which(rr == 892)
    rrna0 <- which(rr == 929); rrna <- rrna0[rrna0 != 941]      # rows to remove
    selr <- !(1:length(rr) %in% rrna)
    
    cc <- apply(sesm, 1, sum_na)
    #sum(cc!=994)               # number of cols not entirely NA - 880
    #table(cc)                  # find row with min amount of data
    #which(cc == 984)
    ccna0 <- which(cc == 994); ccna <- ccna0[ccna0 != 14]     # cols to remove
    selc <- !(1:length(cc) %in% ccna)
    
    sesmap <- sesm[selc, selr]
    
    
    ### cover map partitioning
    round( sum(sesmap==0, na.rm=T)/sum(!is.na(sesmap)) , 3)   # 0.667    / bare
    round( sum(sesmap==1, na.rm=T)/sum(!is.na(sesmap)) , 3)   # 0.234    / shrubs
    round( sum(sesmap==2, na.rm=T)/sum(!is.na(sesmap)) , 3)   # 0.099    / herbaceous
    
    
    
    
  }  ## SES - cover map
  
  
}  # 6.1 load packages, specify areas/times etc... =====
  
### examples 1 - basics 

if(F){
  
  source("E:/REC_7_Data/9_R/FFP_R/calc_footprint_FFP.R")
  source("E:/REC_7_Data/9_R/FFP_R/calc_footprint_FFP_climatology.R")
  
  
  # example
  FFP  <-  calc_footprint_FFP(zm=20,z0=0.01,h=2000,ol=-100,sigmav=0.6,ustar=0.4,
                            wind_dir=30,
                            r=seq(10,80,10))
  
  names(FFP)  # "x_ci_max" "x_ci"     "f_ci"     "x_2d"     "y_2d"     "f_2d"     
  # "flag_err" "r"        "fr"       "xr"       "yr"
  
  
  # FFP output
  # FFP = Structure array with footprint data for measurement at [0 0 zm] m
  # 1) x_ci_max = x location of footprint peak (distance from measurement) [m]
  # 2) x_ci = x array of crosswind integrated footprint [m]
  # 3) f_ci = Footprint function values of crosswind integrated footprint [m-1]
  # 4) x_2d = x-grid of 2-dimensional footprint [m], rotated if wind_dir is provided
  # 5) y_2d = y-grid of 2-dimensional footprint [m], rotated if wind_dir is provided
  # 6) f_2d = footprint function values of 2-dimensional footprint [m-2]
  # 7) flag_err = 1 in case of error, 0 otherwise
  # 8) r = percentage of footprint as in input, if provided
  # 9) fr = footprint value at r, if r is provided
  # 10)xr = x-array for contour line of r, if r is provided
  # 11)yr = y-array for contour line of r, if r is provided
  
  
  length(FFP[["x_ci_max"]])  # 1
  length(FFP[["x_ci"]])      # 1001
  length(FFP[["f_ci"]])      # 1001
  
  dim(FFP[["x_2d"]])     # 1501 x 1001
  dim(FFP[["y_2d"]])     # 1501 x 1001
  dim(FFP[["f_2d"]])     # 1501 x 1001
  
  length(FFP[["flag_err"]])  # 1
  length(FFP[["r"]])         # 8
  length(FFP[["fr"]])        # 8
  length(FFP[["xr"]])        # 8
  length(FFP[["xr"]][[1]])      # 61
  length(FFP[["xr"]][[2]])      # 93
  length(FFP[["xr"]][[3]])      # 127
  length(FFP[["xr"]][[4]])      # 169
  length(FFP[["xr"]][[5]])      # 219
  length(FFP[["xr"]][[6]])      # 297
  length(FFP[["xr"]][[7]])      # 425
  length(FFP[["xr"]][[8]])      # 653
  length(FFP[["yr"]])        # 8
  length(FFP[["yr"]][[1]])      # 61
  length(FFP[["yr"]][[2]])      # 93
  length(FFP[["yr"]][[3]])      # 127
  length(FFP[["yr"]][[4]])      # 169
  length(FFP[["yr"]][[5]])      # 219
  length(FFP[["yr"]][[6]])      # 297
  length(FFP[["yr"]][[7]])      # 425
  length(FFP[["yr"]][[8]])      # 653  
  
  
  
  #Crosswind-integrated footprint
  plot(FFP$x_ci,FFP$f_ci, type="l")
  
  
  
  #Two-dimensional view of single footprint
  ffp_x  <-  c(FFP$x_2d)
  ffp_y  <-  c(FFP$y_2d)
  ffp_f  <-  c(FFP$f_2d)
  quilt.plot(ffp_x,ffp_y,ffp_f,nx=1000,ny=1000, xlim=c(-100,1000),ylim=c(-100,1000))
  for (i in 1:8) lines(FFP$xr[[i]],FFP$yr[[i]], type="l", col="red")
  
}    # kljun footprint example

if(F){
  
  i <- 200    # row
  dt <- 1   # datasets
  if(dt %in% c(1:4,9)) {canopy_h <- 0.5}
  if(dt %in% c(5:8,10)){canopy_h <- 1.0}
  
  
  
  
  #for(i in 200:250){
  
  FFP  <-  calc_footprint_FFP(zm=   6 - (0.67 * canopy_h),    # Meas h. above displacement h.  
                            z0=        0.15 * canopy_h,     # Roughness length
                            h=    1000,                     # PBL depth (set always at 1000)
                            ol=   6 / data[i,paste("MO_stability", adatasets[dt], sep="_")],
                            sigmav=   data[i,paste("sigma_V", adatasets[dt], sep="_")],
                            ustar=    data[i,paste("friction_velocity", adatasets[dt], sep="_")],
                            wind_dir= data[i,paste("Wind_Dir", adatasets[dt], sep="_")],
                            r=seq(10,80,10))
  
  print(i)
  
  #}
  
  
  #Crosswind-integrated footprint
  plot(FFP$x_ci,FFP$f_ci, type="l")
  
  
  
  #Two-dimensional view of single footprint
  ffp_x  <-  c(FFP$x_2d)
  ffp_y  <-  c(FFP$y_2d)
  ffp_f  <-  c(FFP$f_2d)
  #quilt.plot(ffp_x,ffp_y,ffp_f,nx=1000,ny=1000, xlim=c(-100,500),ylim=c(0,600))
  quilt.plot(ffp_x,ffp_y,ffp_f,nx=1000,ny=1000, xlim=c(-100,500),ylim=c(0,600))
  for (i in 1:8) lines(FFP$xr[[i]],FFP$yr[[i]], type="l", col="red")
  
  
  # limits of the plotted area: ~600 m along the wind direction and  ~500 m across in both directions 
  # quilt.plot(ffp_x,ffp_y,ffp_f,nx=1000,ny=1000, xlim=c(-1000,1000),ylim=c(-1000,1000))
  
  
  # try to plot single foot with the function of climatology
  image.plot(FFP$x_2d[1,], FFP$y_2d[,1], FFP$f_2d)
  for (i in 1:8) lines(FFP$xr[[i]], FFP$yr[[i]], type="l", col="red")
  
  
  
  
  
  
}    # g1 footprint example

if(F){
  FFP  <-  calc_footprint_FFP_climatology(
    zm=20, z0=0.01, umean=NA, h=c(2000,1800,1500),
    ol=c(-10,-100,-500), sigmav=c(0.9,0.7,0.3), ustar=c(0.5,0.3,0.4), wind_dir=c(30,50,70),
    domain=c(-100,1000,-100,1000), nx=1100, r=seq(10,80,10), smooth_data=1)
  
  length(FFP)       # 9
  names(FFP)        # "x_2d"     "y_2d"     "fclim_2d" "flag_err" 
  # "r"        "fr"       "xr"       "yr"       "n"
  
  
  image.plot(FFP$x_2d[1,], FFP$y_2d[,1], FFP$fclim_2d)
  for (i in 1:8) lines(FFP$xr[[i]], FFP$yr[[i]], type="l", col="red")
}    # kljun climatology example

### examples 2 - getting serious

if(F){ 
  for(dt in c(1:10)){
    
    #dt <- 10   # datasets
    
    time_f <- "day_time"
    
    datfoot <- datfoot0
    if(dt %in% c(1:4,9)) {
      canopy_h <- 0.5
      if(time_f=="day_time")  datfoot <- datfoot0[ datfoot0[,"SW_IN_gm"]!=0 ,]
      if(time_f=="night_time")datfoot <- datfoot0[ datfoot0[,"SW_IN_gm"]==0 ,]
    }
    if(dt %in% c(5:8,10)){
      canopy_h <- 1.0
      if(time_f=="day_time")  datfoot <- datfoot0[ datfoot0[,"SW_IN_sm"]!=0 ,]
      if(time_f=="night_time")datfoot <- datfoot0[ datfoot0[,"SW_IN_sm"]==0 ,]
    }
    
    
    
    #browser()
    
    #datfoot <- datfoot[4000:4500,]
    is <- 1:nrow(datfoot)
    
    if(dt==10){ # this code is ugly, but actually needed. Do not change it
      ct <- "(20/12/18 14:45:00)" %in% as.character(as.chron(datfoot[,"dt"])) |
        "(25/04/19 08:15:00)" %in% as.character(as.chron(datfoot[,"dt"]))
      if(ct){   
        tbr <- which(as.character(as.chron(datfoot[,"dt"]))=="(20/12/18 14:45:00)" | 
                     as.character(as.chron(datfoot[,"dt"]))=="(25/04/19 08:15:00)"  )
        is <- is[-tbr]
      }
    }
    
    
    
    
    
    
    
    
    if(!(dt %in% c(9,10))){
      FFP <-  calc_footprint_FFP_climatology(
        zm=   rep(6 - (0.67 * canopy_h), length(is)),    # Meas h. above displacement h.  
        z0=   rep(     0.15 * canopy_h,  length(is)),    # Roughness length
        umean=NA,
        h=    rep(1000, length(is)),             # PBL depth (set always at 1000)
        ol=   6 / datfoot[is,paste("MO_stability", adatasets[dt], sep="_")],
        sigmav=   datfoot[is,paste("sigma_V", adatasets[dt], sep="_")],
        ustar=    datfoot[is,paste("friction_velocity", adatasets[dt], sep="_")],
        wind_dir= datfoot[is,paste("Wind_Dir", adatasets[dt], sep="_")],
        domain=c(-xl,xl,-xl,xl),  # x and y cormers in m
        nx=2*xl,                      # 2x2 m each as default
        r=seq(10,80,10), smooth_data=1)
    }
    
    
    
    #is <- 3086:3090
    
    
    if(dt %in% c(9,10)){
      FFP <-  calc_footprint_FFP_climatology(
        zm=   rep(3 - (0.67 * canopy_h), length(is)),    # Meas h. above displacement h.  
        z0=   rep(     0.15 * canopy_h,  length(is)),    # Roughness length
        umean=NA,
        h=    rep(1000, length(is)),             # PBL depth (set always at 1000)
        ol=     3/datfoot[is,paste("zoL", adatasets[dt], sep="_")],
        sigmav=   datfoot[is,paste("V_SIGMA", adatasets[dt], sep="_")],
        ustar=    datfoot[is,paste("USTAR", adatasets[dt], sep="_")],
        wind_dir= datfoot[is,paste("WD", adatasets[dt], sep="_")],
        domain=c(-xl,xl,-xl,xl),  # x and y cormers in m
        nx=2*xl,                      # 2x2 m each as default
        r=seq(10,80,10), smooth_data=1)
    }
    
    
    
    
    
    ######## try to loop foot 3000:3500 SES to find the one with the bug (not useful)
    if(F){for(i in 3000:3500){
      FFP  <-  calc_footprint_FFP(
        zm=   6 - (0.67 * canopy_h),    # Meas h. above displacement h.  
        z0=        0.15 * canopy_h,     # Roughness length
        h=    1000,                     # PBL depth (set always at 1000)
        #ol=   6 / data[i,paste("MO_stability", adatasets[dtp], sep="_")],
        #sigmav=   data[i,paste("sigma_V", adatasets[dtp], sep="_")],
        ol=     3/data[i,paste("zoL", adatasets[dt], sep="_")],
        sigmav=   data[i,paste("V_SIGMA", adatasets[dt], sep="_")],
        ustar=    data[i,paste("USTAR", adatasets[dt], sep="_")],
        wind_dir= data[i,paste("WD", adatasets[dt], sep="_")],
        r=seq(10,80,10))
      print(i)
    }    }
    
    
    
    ## save plots with png ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    mt3 <- towers[dt]
    if(dt %in% c(9,10)){mt3 <- sites[dt-8]}
    
    if(F){
      ppath <- "E:/REC_7_Data/10_Plots/footprints/"
      filenm <- paste(ppath, mt3, "_", last_date, "footprint", 
                    ifelse(AmeriFlux_f, "_mdata",""), lb, "_all.png", sep="")
      
      png(filenm, width=600, height=600)
      
      
      par(mar=c(5,5,4,4), oma=c(1,1,1,2) )
      
      image.plot(FFP$x_2d[1,], FFP$y_2d[,1], FFP$fclim_2d)
      #for (i in 1:8) lines(FFP$xr[[i]], FFP$yr[[i]], type="l", lty=3, col="red")
      lines(FFP$xr[[5]], FFP$yr[[5]], type="l", col="red")
      lines(FFP$xr[[8]], FFP$yr[[8]], type="l", col="orange")
      mtext(paste("Foot 80%  -  ", mt3, " - ",
                  substring(lb, 2, nchar(lb)), sep=""), 3, line=1, font=2, cex=1.5)
      
      
      dev.off()
    }
    
    
    
    ### filter NAs
    x50 <- FFP$xr[[5]]; x50 <- x50[!is.na(x50)]
    y50 <- FFP$yr[[5]]; y50 <- y50[!is.na(y50)]
    x80 <- FFP$xr[[8]]; x80 <- x80[!is.na(x80)]
    y80 <- FFP$yr[[8]]; y80 <- y80[!is.na(y80)]
    
    
    ### add NAD83 coordinates of the towers
    if(dt %in% c(1:4,9)) {xc <- 343643; yc <- 3803412}
    if(dt %in% c(5:8,10)){xc <- 339497; yc <- 3800677}
      
    xt <- yt <- 0
    
    if(mt3=="SEG_REC1"){xt <- (-15); yt <-   15 }
    if(mt3=="SEG_REC2"){xt <-  (-6); yt <-   81 }
    if(mt3=="SEG_REC3"){xt <-  (72); yt <- (-38)}
    if(mt3=="SEG_REC4"){xt <- (-76); yt <- (-32)}
    
    if(mt3=="SES_REC1"){xt <-  (15); yt <-  (-5)}
    if(mt3=="SES_REC2"){xt <-   (4); yt <-   83 }
    if(mt3=="SES_REC3"){xt <-  (70); yt <- (-44)}
    if(mt3=="SES_REC4"){xt <- (-72); yt <- (-38)}
    
    x50 <- x50+xc+xt; y50 <- y50+yc+yt;
    x80 <- x80+xc+xt; y80 <- y80+yc+yt;
    
    
    
    
    ### save obj with lines
    mat50 <- matrix(NA, ncol=2, nrow=length(x50)); colnames(mat50) <- c("x", "y")
    mat80 <- matrix(NA, ncol=2, nrow=length(x80)); colnames(mat80) <- c("x", "y")
    
    mat50[,"x"] <- x50; mat50[,"y"] <- y50
    mat80[,"x"] <- x80; mat80[,"y"] <- y80
    
    spath <- "E:/REC_7_Data/9_R/footdata/"
    write.csv(mat50, paste(spath, mt3, '_foot_50.csv', sep=""), row.names=F)
    write.csv(mat80, paste(spath, mt3, '_foot_80.csv', sep=""), row.names=F)
    
    
  } 
} # Kljun climatology for 10 towers (sel day/night) ***

if(F){  
  do_plot <- F
  mon_ys <- unique(datfoot0[,c("yyyy", "mon")])
  
  pmat <- matrix(NA, ncol=8, nrow=nrow(mon_ys) * 24); 
  colnames(pmat) <- c("yyyy", "mon", "hh", "pt", "p1", "p2", "p3", "ps")
  
  #logmat <- matrix(NA, ncol=13, nrow=nrow(mon_ys) * 24)
  #colnames(logmat) <- c("yyyy", "mon", "hh", adatasets[1:10])   # make log of comulative footprints  
  
  
  for(i in 1:nrow(mon_ys)){
    
    datfootm <- datfoot0[  datfoot0[,"yyyy"]==mon_ys[i,"yyyy"] & datfoot0[,"mon"]==mon_ys[i,"mon"] , ]
    print(paste(mon_ys[i,"yyyy"], mon_ys[i,"mon"]))
    
    
    for(hi in 0:23){
      
      datfoot <- datfootm[  datfootm[,"hh"]==hi , ]
      print(paste( "Hour of day:", hi ) )
      #logmat[(i-1)*24+(hi+1), 1:3] <- c(mon_ys[i,"yyyy"], mon_ys[i,"mon"], hi)
      pmat[(i-1)*24+(hi+1), 1:3] <- c(mon_ys[i,"yyyy"], mon_ys[i,"mon"], hi)
      
      
      for(dt in c(1:10)){
        
        #dt <- 10   # datasets
        if(dt %in% c(1:4,9)) { canopy_h <- 0.5 }
        if(dt %in% c(5:8,10)){ canopy_h <- 1.0 }
        
        
        #browser()
        
        is <- 1:nrow(datfoot)
        
        if(dt==10){ # this code is ugly, but actually needed. Do not change it
          ct <- "(20/12/18 14:45:00)" %in% as.character(as.chron(datfoot[,"dt"])) |
            "(25/04/19 08:15:00)" %in% as.character(as.chron(datfoot[,"dt"]))
          if(ct){   
            tbr <- which(as.character(as.chron(datfoot[,"dt"]))=="(20/12/18 14:45:00)" | 
                         as.character(as.chron(datfoot[,"dt"]))=="(25/04/19 08:15:00)"  )
            is <- is[-tbr]
          }
        }
        
        
        
        
        
        
        
        if(!(dt %in% c(9,10))){
          FFP <-  calc_footprint_FFP_climatology(
            zm=   rep(6 - (0.67 * canopy_h), length(is)),    # Meas h. above displacement h.  
            z0=   rep(     0.15 * canopy_h,  length(is)),    # Roughness length
            umean=NA,
            h=    rep(1000, length(is)),             # PBL depth (set always at 1000)
            ol=   6 / datfoot[is,paste("MO_stability", adatasets[dt], sep="_")],
            sigmav=   datfoot[is,paste("sigma_V", adatasets[dt], sep="_")],
            ustar=    datfoot[is,paste("friction_velocity", adatasets[dt], sep="_")],
            wind_dir= datfoot[is,paste("Wind_Dir", adatasets[dt], sep="_")],
            domain=c(-xl,xl,-xl,xl),  # x and y cormers in m
            nx=2*xl,                      # 2x2 m each as default
            r=seq(10,80,10), smooth_data=1)
        }
        
        
        
        #is <- 3086:3090
        
        
        if(dt %in% c(9,10)){
          FFP <-  calc_footprint_FFP_climatology(
            zm=   rep(3 - (0.67 * canopy_h), length(is)),    # Meas h. above displacement h.  
            z0=   rep(     0.15 * canopy_h,  length(is)),    # Roughness length
            umean=NA,
            h=    rep(1000, length(is)),             # PBL depth (set always at 1000)
            ol=   3 / datfoot[is,paste("zoL", adatasets[dt], sep="_")],
            sigmav=   datfoot[is,paste("V_SIGMA", adatasets[dt], sep="_")],
            ustar=    datfoot[is,paste("USTAR", adatasets[dt], sep="_")],
            wind_dir= datfoot[is,paste("WD", adatasets[dt], sep="_")],
            domain=c(-xl,xl,-xl,xl),  # x and y cormers in m
            nx=2*xl,                      # 2x2 m each as default
            r=seq(10,80,10), smooth_data=1)
        }
        
        
        #FFP$fclim_2d[is.na(FFP$fclim_2d)] <- 0
        
        #logmat[(i-1)*24+(hi+1), 3+dt] <- sum(FFP$fclim_2d)
        
        
        
        
        ## save plots as png ===============================================================
        if(do_plot){
          mt3 <- towers[dt]
          if(dt %in% c(9,10)){mt3 <- sites[dt-8]}
          
          ppath <- paste("E:/REC_7_Data/10_Plots/footprints/monthly_avg_days/", mt3, "/", sep="")
          filenm <- paste(ppath, mt3, "_", last_date, "footprint", ifelse(AmeriFlux_f, "_mdata",""), 
                        "_", mon_ys[i,1], "_", mon_ys[i,2], "_", hi, ".png", sep="")
          
          png(filenm, width=600, height=600)
          par(mar=c(5,5,4,4), oma=c(1,1,1,2) )
          
          zmax <- 0.0021; nlevs <- 1000
          image.plot(FFP$x_2d[1,], FFP$y_2d[,1], FFP$fclim_2d, zlim=c(0,zmax), 
                     col=magma(nlevs), breaks=(0:nlevs)^5/(nlevs+1)^5*zmax)
          lines(FFP$xr[[5]], FFP$yr[[5]], type="l", col="black")
          lines(FFP$xr[[8]], FFP$yr[[8]], type="l", col="black", lty=3)
          mtext(paste("Foot 50-80%  -  ", mt3, " - ", mon_ys[i,1], "/", mon_ys[i,2], 
                      " H:", hi, sep=""), 3, line=1, font=2, cex=1.5)
          
          
          dev.off()
        }
        
        
        
        
        
        ## land cover probability ===============================================================
        ## need to lock the dt loop on a single dataset
        
        
        sel1 <- lcm==1
        sel2 <- lcm==2
        sel3 <- lcm==3
        
        pt <- sum(FFP$fclim_2d)
        p1 <- sum(FFP$fclim_2d[sel1])
        p2 <- sum(FFP$fclim_2d[sel2])
        p3 <- sum(FFP$fclim_2d[sel3])
        ps <- sum(p1+p2+p3)
        
        pmat[(i-1)*24+(hi+1), c("pt", "p1", "p2", "p3", "ps")] <- c(pt, p1, p2, p3, ps)
        
        
        
        ### this currently produces a plot EVERY HOUR !!!
        if(F){  
          mt3 <- towers[dt]; if(dt %in% c(9,10)){mt3 <- sites[dt-8]}
          
          ppath <- paste("E:/REC_7_Data/10_Plots/footprints/monthly_avg_days/", mt3, "/", sep="")
          filenm <- paste(ppath, mt3, "_prob_plot_", mon_ys[i,1], "_", mon_ys[i,2], "_", hi, ".png", sep="")
          
          png(filenm, width=600, height=600)
          
          plot(pmat[1:24,"pt"], type="n", ylim=c(0,1), axes=T, xlab="", ylab=""); box()
          #axis(1, at=1:24, labels=paste("2018_10", 0:23), las=3); axis(2); 
          mtext("Probability", 2, line=2.5); mtext("Hour of Day", 1, line=2.5)
          mtext(paste(mon_ys[i,"yyyy"], "/", mon_ys[i,"mon"], "-", towers[dt]), line=0.5, cex=2)
          lines(pmat[1:24,"pt"])
          lines(pmat[1:24,"p1"], col="blue")
          lines(pmat[1:24,"p2"], col="green")
          lines(pmat[1:24,"p3"], col="brown")
          lines(pmat[1:24,"ps"], col="red", lty=3)
          
          dev.off()
        }
        
      }
      
    }
    
  }
  
  
  # save(logmat, file = paste("E:/REC_7_Data/9_R/Rdata/", "logmat.Rdata", sep=""))
  # range(logmat[,4:13])   # 0.8082753 0.9737382
  # mean(logmat[,4:13])    # 0.8898167
  
  
} # Kljun climatology for 10 towers (monthly avg days / sum(FFP))

if(F){
  mon_ys <- unique(datfoot0[,c("yyyy", "mon")])
  
  logmat <- matrix(NA, ncol=13, nrow=nrow(mon_ys) * 24)
  colnames(logmat) <- c("yyyy", "mon", "hh", adatasets[1:10])   # make log of comulative footprints  
  
  xl <- 400
  ## issue with the plotting (it's messy and difficult to distinguish the curves)
  
  for(i in 1:nrow(mon_ys)){
    
    #i <- 1
    datfootm <- datfoot0[  datfoot0[,"yyyy"]==mon_ys[i,"yyyy"] & datfoot0[,"mon"]==mon_ys[i,"mon"] , ]
    print(paste(mon_ys[i,"yyyy"], mon_ys[i,"mon"]))
    
    
    for(hi in 0:23){
      
      #hi <- 10
      datfoot <- datfootm[  datfootm[,"hh"]==hi , ]
      print(paste( "Hour of day:", hi ) )
      logmat[(i-1)*24+(hi+1), 1:3] <- c(mon_ys[i,"yyyy"], mon_ys[i,"mon"], hi)
      
      
      for(cluster in sites){
        
        cluster <- sites[1]
        dts <- c(1:4, 9); canopy_h <- 0.5; 
        if(cluster=="SES"){dts <- c(5:8, 10); canopy_h <- 1.0}
        
        
        tpoint <- paste(substring(as.character(datfoot[i, "dt"]), 2, 9), " H:", datfoot[i, "hh"], sep="")
        tlab <- paste(datfoot[i,c("yyyy", "mon", "hh")], collapse="_")
        for(dt in dts){
          
          #dt <- dts[1]
          print(paste(towers[dt]))
          
          mt3 <- towers[dt]; if(dt %in% c(9,10)){mt3 <- sites[dt-8]}
          
          
          
          ## select footprints for the climatology
          
          is <- 1:nrow(datfoot)
          
          
          if(dt==10){ # this code is ugly, but actually needed. Do not change it
            ct <- "(20/12/18 14:45:00)" %in% as.character(as.chron(datfoot[,"dt"])) |
              "(25/04/19 08:15:00)" %in% as.character(as.chron(datfoot[,"dt"]))
            if(ct){   
              tbr <- which(as.character(as.chron(datfoot[,"dt"]))=="(20/12/18 14:45:00)" | 
                           as.character(as.chron(datfoot[,"dt"]))=="(25/04/19 08:15:00)"  )
              is <- is[-tbr]
            }
          }
          
          
          
          
          
          if(!(dt %in% c(9,10))){
            #zmax <- 0.00031
            FFP <-  calc_footprint_FFP_climatology(
              zm=   rep(6 - (0.67 * canopy_h), length(is)),    # Meas h. above displacement h.  
              z0=   rep(     0.15 * canopy_h,  length(is)),    # Roughness length
              umean=NA,
              h=    rep(1000, length(is)),             # PBL depth (set always at 1000)
              ol=   6 / datfoot[is,paste("MO_stability", adatasets[dt], sep="_")],
              sigmav=   datfoot[is,paste("sigma_V", adatasets[dt], sep="_")],
              ustar=    datfoot[is,paste("friction_velocity", adatasets[dt], sep="_")],
              wind_dir= datfoot[is,paste("Wind_Dir", adatasets[dt], sep="_")],
              domain=c(-xl,xl,-xl,xl),  # x and y cormers in m
              nx=2*xl,                      # 2x2 m each as default
              r=seq(10,80,10), smooth_data=1)
            
            
            
            #### translate SEG 1:4 and lines with respect to US-SEX (distances from arcGIS)
            if(mt3=="SEG_REC1"){xt <- (-15); yt <-   15 }
            if(mt3=="SEG_REC2"){xt <-  (-6); yt <-   81 }
            if(mt3=="SEG_REC3"){xt <-  (72); yt <- (-38)}
            if(mt3=="SEG_REC4"){xt <- (-76); yt <- (-32)}
            
            if(mt3=="SES_REC1"){xt <-  (15); yt <-  (-5)}
            if(mt3=="SES_REC2"){xt <-   (4); yt <-   83 }
            if(mt3=="SES_REC3"){xt <-  (70); yt <- (-44)}
            if(mt3=="SES_REC4"){xt <- (-72); yt <- (-38)}
            
            FFP$x_2d <- FFP$x_2d+xt; FFP$y_2d <- FFP$y_2d+yt
            for(j in 1:length(FFP$xr)){FFP$xr[[j]] <- FFP$xr[[j]]+xt; FFP$yr[[j]] <- FFP$yr[[j]]+yt}
            
            
          }
          
          
          
          #is <- 3086:3090
          
          
          if(dt %in% c(9,10)){
            FFP <-  calc_footprint_FFP_climatology(
              zm=   rep(3 - (0.67 * canopy_h), length(is)),    # Meas h. above displacement h.  
              z0=   rep(     0.15 * canopy_h,  length(is)),    # Roughness length
              umean=NA,
              h=    rep(1000, length(is)),             # PBL depth (set always at 1000)
              ol=   3 / datfoot[is,paste("zoL", adatasets[dt], sep="_")],
              sigmav=   datfoot[is,paste("V_SIGMA", adatasets[dt], sep="_")],
              ustar=    datfoot[is,paste("USTAR", adatasets[dt], sep="_")],
              wind_dir= datfoot[is,paste("WD", adatasets[dt], sep="_")],
              domain=c(-xl,xl,-xl,xl),  # x and y cormers in m
              nx=2*xl,                      # 2x2 m each as default
              r=seq(10,80,10), smooth_data=1)
          }
          
          
          # create 3-cols matrix from FFP
          xyf <- matrix(NA, ncol=3, nrow=nrow(FFP$x_2d)*ncol(FFP$y_2d))
          colnames(xyf) <- c("x_2d", "y_2d", "f_2d")
          
          xyf[,"x_2d"] <- round(FFP$x_2d, 1)  # common grid for all of the four object
          xyf[,"y_2d"] <- round(FFP$y_2d, 1)
          xyf[,"f_2d"] <- FFP$fclim_2d            
          
          
          
          print(paste("FFP sum:", sum(FFP$fclim_2d)))
          logmat[(i-1)*24+(hi+1), 3+dt] <- sum(FFP$fclim_2d)
          
          
          assign(paste("xyf", adatasets[dt], sep="_"), xyf)   # for the footprint
          assign(paste("FFP", adatasets[dt], sep="_"), FFP)   # for the lines
          
        }
        
        
        
        
        # merge different xyf matrices
        syn_list <- NULL
        for(j in 1:length(dts)){
          ## add something to change colnames of f_2d
          syn_list[[j]] <- get(paste("xyf", adatasets[dts[j]], sep="_"))
        }
        
        # 2 min for a single footprint (SEG)
        xyf5 <- Reduce(function(x,y) merge(x=x, y=y, by=c("x_2d", "y_2d"), all=T), syn_list)
        
        f5sum <- apply(xyf5[,3:ncol(xyf5)], 1, sum, na.rm=T) ### sum the 4 different f values 
        xyf5$f5sum <- f5sum
        
        
        
        id <- paste(xyf5[,"x_2d"], xyf5[,"y_2d"], sep="_")
        xyf5$id <- id
        
        

        fb <- function(x){x <- unique(x)}  # keep only the coordinate
        
        # use tapply for f,x,y, so that they remain in the same order
        fp <- tapply(xyf5$f5sum,   id, sum, na.rm=T)
        xp <- tapply(xyf5$x_2d, id, fb)
        yp <- tapply(xyf5$y_2d, id, fb)
        
        
        
        ### Plot =============================
        
        
        ppath <- paste("E:/REC_7_Data/10_Plots/footprints/monthly_avg_days_cluster/", cluster, "/", sep="")
        filenm <- paste(ppath, cluster, "_footprint_5_test_", tlab, ".png", sep="")
        
        png(filenm, width=600, height=600)
        par(mar=c(5,5,4,4), oma=c(1,1,1,2) )
        
        
        quilt.plot(xp, yp, fp, nx=500,ny=500, xlim=c(-250,250),ylim=c(-250,250)) 
        legend("bottomleft", c("REC 50%", "REC 80%", "EC 50%", "EC 80%"), 
               col=c("red", "orange", "red", "orange"), lty=c(1, 1, 3, 3), lwd=2)
        
        for(dt in dts){
          if(dt %in% dts[1:4]){
            FFP <- get(paste("FFP", adatasets[dt], sep="_"))      # plot RECs foot
            #lines(FFP$xr[[5]],FFP$yr[[5]], type="l", col="red")
            lines(FFP$xr[[8]],FFP$yr[[8]], type="l", col="orange", lty=dt)
          } else {
            FFP <- get(paste("FFP", adatasets[dt], sep="_"))      # plot EC foot
            #lines(FFP$xr[[5]],FFP$yr[[5]], type="l", col="red", lty=3)
            lines(FFP$xr[[8]],FFP$yr[[8]], type="l", col="orange", lty=5)
          }
        }
        
        mtext(paste(cluster, " - ", tpoint, sep=""), 3, line=1, font=2, cex=1.5)
        
        dev.off()
        
        
        
        
        
        ##### test: try to plot individual footprints
        if(F){
          ppath <- paste("E:/REC_7_Data/10_Plots/footprints/avg_day_mn_video_cluster/", cluster, "/", sep="")
          filenm <- paste(ppath, cluster, "_footprint_", tpoint, "_2x2_test.png", sep="")
          
          png(filenm, width=1200, height=1200)
          par(mfrow=c(2,2), mar=c(5,5,4,4), oma=c(1,1,1,1) )
          
          
          g1x <- c(FFP_g1$x_2d); g1y <- c(FFP_g1$y_2d); g1f <- c(FFP_g1$f_2d);
          quilt.plot(g1x, g1y, g1f, nx=1000,ny=1000, xlim=c(-499,500),ylim=c(-499,500))   # max: 0.0002
          mtext("SEG_REC1", 3, line=1, font=2, cex=1.5)
          g2x <- c(FFP_g2$x_2d); g2y <- c(FFP_g2$y_2d); g2f <- c(FFP_g2$f_2d);
          quilt.plot(g2x, g2y, g2f, nx=1000,ny=1000, xlim=c(-499,500),ylim=c(-499,500))   # max: 0.00015
          mtext("SEG_REC2", 3, line=1, font=2, cex=1.5)
          g3x <- c(FFP_g3$x_2d); g3y <- c(FFP_g3$y_2d); g3f <- c(FFP_g3$f_2d);
          quilt.plot(g3x, g3y, g3f, nx=1000,ny=1000, xlim=c(-499,500),ylim=c(-499,500))   # max: 0.00015
          mtext("SEG_REC3", 3, line=1, font=2, cex=1.5)
          g4x <- c(FFP_g4$x_2d); g4y <- c(FFP_g4$y_2d); g4f <- c(FFP_g4$f_2d);
          quilt.plot(g4x, g4y, g4f, nx=1000,ny=1000, xlim=c(-499,500),ylim=c(-499,500))   # max: 0.0004
          mtext("SEG_REC4", 3, line=1, font=2, cex=1.5)
          
          dev.off()
        }
        
        
        
        
        
        
        
        
        ## land cover probability ===============================================================
        ## need to lock the dt loop on a single dataset
        if(F){
          
          sel1 <- lcm==1
          sel2 <- lcm==2
          sel3 <- lcm==3
          
          pt <- sum(FFP$fclim_2d)
          p1 <- sum(FFP$fclim_2d[sel1])
          p2 <- sum(FFP$fclim_2d[sel2])
          p3 <- sum(FFP$fclim_2d[sel3])
          ps <- sum(p1+p2+p3)
          
          pmat[(i-1)*24+(hi+1), c("pt", "p1", "p2", "p3", "ps")] <- c(pt, p1, p2, p3, ps)
          
          
          
          
          
          mt3 <- towers[dt]; if(dt %in% c(9,10)){mt3 <- sites[dt-8]}
          
          ppath <- paste("E:/REC_7_Data/10_Plots/footprints/monthly_avg_days/", mt3, "/", sep="")
          filenm <- paste(ppath, mt3, "_prob_plot_", mon_ys[i,1], "_", mon_ys[i,2], "_", hi, ".png", sep="")
          
          png(filenm, width=600, height=600)
          
          plot(pmat[1:24,"pt"], type="n", ylim=c(0,1), axes=T, xlab="", ylab=""); box()
          #axis(1, at=1:24, labels=paste("2018_10", 0:23), las=3); axis(2); 
          mtext("Probability", 2, line=2.5); mtext("Hour of Day", 1, line=2.5)
          mtext(paste(mon_ys[i,"yyyy"], "/", mon_ys[i,"mon"], "-", towers[dt]), line=0.5, cex=2)
          lines(pmat[1:24,"pt"])
          lines(pmat[1:24,"p1"], col="cyan")
          lines(pmat[1:24,"p2"], col="blue")
          lines(pmat[1:24,"p3"], col="magenta")
          lines(pmat[1:24,"ps"], col="red", lty=3)
          
          dev.off()
          
        }
        
      }
      
    }
    
  }  
  
  
} # Kljun climatology for 2 clusters (monthly avg days)

### actual script for footprint generation / analysis

if(F){  
  
  
  # ========================================================================================== #
  ###### 6.3 hourly footprint for clusters / probability sum for clusters (/5) and towers ######
  # ========================================================================================== #
  

  
  mon_ys <- unique(datfoot0[,c("yyyy", "mon")])
  datfoot_t <- datfoot0
  abort_plot <- F
  
   
   
  
  
  for(k in 1:nrow(mon_ys)){  # loops over different months
    
    datfoot <- datfoot_t[  datfoot_t[,"yyyy"]==mon_ys[k,"yyyy"] & datfoot_t[,"mon"]==mon_ys[k,"mon"] , ]
    
    pmat <- matrix(NA, ncol=5+5*12, nrow=nrow(datfoot)); 
    colnames(pmat) <- c("yyyy", "mon", "dd", "hh", "min",
                      paste( rep( c("pt", "p1", "p2", "p3", "ps"), 12), # 10 towers and 2 clusters
                             rep( c(adatasets[1:10], sites), each=5), sep="_" )  )
    pmat_80 <- pmat
    
    
    for(i in 1:nrow(datfoot)){     # loops over different half-hourly data
      
      #i <- 4   # first that does not abort
      
      print(paste(i, "on a total of", nrow(datfoot)))
      
      for(cluster in sites){
        
        
        ### assign sel1, sel2 and sel3 to the right XXXmap
        if(cluster=="SEG"){sel1 <- segmap==0; sel2 <- segmap==1; sel3 <- segmap==2}
        if(cluster=="SES"){sel1 <- sesmap==0; sel2 <- sesmap==1; sel3 <- sesmap==2}
        
        
        #cluster <- sites[1]
        dts <- c(1:4, 9); canopy_h <- 0.5; 
        if(cluster=="SES"){dts <- c(5:8, 10); canopy_h <- 1.0}
        
        pmat[i, 1:5]    <- c(datfoot[i,"yyyy"], datfoot[i,"mon"], datfoot[i,"dd"], datfoot[i,"hh"],   datfoot[i,"min"])
        pmat_80[i, 1:5] <- c(datfoot[i,"yyyy"], datfoot[i,"mon"], datfoot[i,"dd"], datfoot[i,"hh"],   datfoot[i,"min"])
        
        tpoint <- paste(substring(as.character(datfoot[i, "dt"]), 2, 9), 
          " H:", datfoot[i, "hh"], datfoot[i, "min"], sep="")
        
        tlab <- paste(datfoot[i, "yyyy"], datfoot[i, "mon"],
          substring(as.character(datfoot[i, "dt"]), 2, 3), datfoot[i, "hh"], datfoot[i, "min"], sep="_")
        
        print(tlab)
        
        
        
        
        totfoot <- matrix(0, ncol=2*xl+1, nrow=2*xl+1)
        sel80c <- matrix(F, ncol=2*xl+1, nrow=2*xl+1)
        
        
        for(dt in dts){   # loops over different towers
          
          print(paste(towers[dt]))
          mt3 <- towers[dt]; if(dt %in% c(9,10)){mt3 <- sites[dt-8]}
          
          
          if(!(dt %in% c(9,10))){
            FFP <-  calc_footprint_FFP_climatology(
              zm=   rep(6 - (0.67 * canopy_h), length(i)),    # Meas h. above displacement h.  
              z0=   rep(     0.15 * canopy_h,  length(i)),    # Roughness length
              umean=NA,
              h=    rep(1000, length(i)),             # PBL depth (set always at 1000)
              ol=   6 / datfoot[i,paste("MO_stability", adatasets[dt], sep="_")],
              sigmav=   datfoot[i,paste("sigma_V", adatasets[dt], sep="_")],
              ustar=    datfoot[i,paste("friction_velocity", adatasets[dt], sep="_")],
              wind_dir= datfoot[i,paste("Wind_Dir", adatasets[dt], sep="_")],
              domain=c(-xl,xl,-xl,xl),  # x and y cormers in m
              nx=2*xl,                      # 2x2 m each as default
              r=seq(10,80,10), smooth_data=1)
            
            
            
            
            if(length(FFP$fclim_2d)==1){abort_plot <- T; break}
            
            
            #### traslate SEG 1:4 and lines with respect to US-SEX (distances from arcGIS)
            if(mt3=="SEG_REC1"){xt <- (-15); yt <-   15 }
            if(mt3=="SEG_REC2"){xt <-  (-6); yt <-   81 }
            if(mt3=="SEG_REC3"){xt <-  (72); yt <- (-38)}
            if(mt3=="SEG_REC4"){xt <- (-76); yt <- (-32)}
            
            if(mt3=="SES_REC1"){xt <-  (15); yt <-  (-5)}
            if(mt3=="SES_REC2"){xt <-   (4); yt <-   83 }
            if(mt3=="SES_REC3"){xt <-  (70); yt <- (-44)}
            if(mt3=="SES_REC4"){xt <- (-72); yt <- (-38)}
            
            FFP$x_2d <- FFP$x_2d+xt; FFP$y_2d <- FFP$y_2d+yt; 
            for(j in 1:length(FFP$xr)){FFP$xr[[j]] <- FFP$xr[[j]]+xt; FFP$yr[[j]] <- FFP$yr[[j]]+yt}
            
            
          }
          
          
          
          
          if(dt %in% c(9,10)){
            FFP <-  calc_footprint_FFP_climatology(
              zm=   rep(3 - (0.67 * canopy_h), length(i)),    # Meas h. above displacement h.  
              z0=   rep(     0.15 * canopy_h,  length(i)),    # Roughness length
              umean=NA,
              h=    rep(1000, length(i)),             # PBL depth (set always at 1000)
              ol=     3/datfoot[i,paste("zoL", adatasets[dt], sep="_")],
              sigmav=   datfoot[i,paste("V_SIGMA", adatasets[dt], sep="_")],
              ustar=    datfoot[i,paste("USTAR", adatasets[dt], sep="_")],
              wind_dir= datfoot[i,paste("WD", adatasets[dt], sep="_")],
              domain=c(-xl,xl,-xl,xl),  # x and y corners in m
              nx=2*xl,                      # 2x2 m each as default
              r=seq(10,80,10), smooth_data=1)
            
            xt <- yt <- 0
          }
          
          
          if(length(FFP$fclim_2d)==1){abort_plot <- T; break}
          
          
          
          
          
          # Single tower land cover probabilities
          
          neofoot <- matrix(0, ncol=2*xl+1, nrow=2*xl+1)
          
          # cut footprint 
          xfoot <- (-xl:xl)-xt; yfoot <- (-xl:xl)-yt
          xmin <- max(min(xfoot), -xl); xmax <- min(max(xfoot), xl);      # xl = 440
          ymin <- max(min(yfoot), -xl); ymax <- min(max(yfoot), xl);
          cutfoot <- FFP$fclim_2d[(xmin:xmax)+xl+1, (ymin:ymax)+xl+1]
          
          # place cutfoot into neofoot (opposite corner of cutting)
          xmin0 <- max(min(FFP$x_2d), -xl); xmax0 <- min(max(FFP$x_2d), xl);   
          ymin0 <- max(min(FFP$y_2d), -xl); ymax0 <- min(max(FFP$y_2d), xl);
          neofoot[(xmin0:xmax0)+xl+1, (ymin0:ymax0)+xl+1] <- cutfoot
          
          
          # calculate probabilities of entire footprint (probably not very useful)
          pt <- sum(neofoot)
          p1 <- sum(neofoot[sel1], na.rm=T); p2 <- sum(neofoot[sel2], na.rm=T); p3 <- sum(neofoot[sel3], na.rm=T)
          ps <- sum(p1+p2+p3)
          pmat[i , paste(c("pt", "p1", "p2", "p3", "ps"), adatasets[dt], sep="_")] <- c(pt, p1, p2, p3, ps)
          
          
          
          
          
          ###  calculate probabilities on 80%-flux footprint
          
          xs <- FFP$xr[[8]]; rx <- round(xs, 0)
          ys <- FFP$yr[[8]]; ry <- round(ys, 0) 
          
          
          
          
          # for when the lines are produced, but then are translated outside of the cluster footprint
          rx[rx<(-xl)] <- (-xl); rx[rx>xl] <- xl
          ry[ry<(-xl)] <- (-xl); ry[ry>xl] <- xl
          
          
          sel80 <- matrix(F, ncol=2*xl+1, nrow=2*xl+1)
          
          # do NOT execute if there is ANY NA in xs OR ys
          if(     !(any(is.na(xs)) | any(is.na(ys)))    ){
            
            for(ii in 1:((2*xl)+1)){          # fill the 80% foot
              if(sum(ry==(ii-xl))==0)next
              sel80[ (rx[ min( which(ry==(ii-xl)) ) ]+xl)  :   (rx[ max(  which(ry== (ii-xl))   )]+xl) ,ii   ]  <-  TRUE
              # this should be the correct orientation; tested with image.plot(sel80) and the Plot later in this script
            }
            
            pt <- sum(neofoot[sel80])
            p1 <- sum(neofoot[sel1&sel80], na.rm=T); p2 <- sum(neofoot[sel2&sel80], na.rm=T); p3 <- sum(neofoot[sel3&sel80], na.rm=T)
            ps <- sum(p1+p2+p3)
            pmat_80[i , paste(c("pt", "p1", "p2", "p3", "ps"), adatasets[dt], sep="_")] <- c(pt, p1, p2, p3, ps)
          }
          
          
           ### combine into cluster footprint
          totfoot <- totfoot+neofoot
          sel80c <-  sel80c | sel80
          
          assign(paste("FFP", adatasets[dt], sep="_"), FFP)   # for the lines
          
           
          
        } # end of dts loop over towers
        
        
        if(abort_plot){print("missing footprint"); abort_plot <- F; next}
        
        

        totfootn <- totfoot/5                 # normalize per number of towers
        #totfootn <- totfoot/sum(totfoot)       # part of the footprint is cut when translating, so the sum is not 5
        # re-orient because climatology's output is set for image.plot (starts filling from ll corner)
        #totfootn <- apply(totfootn,2,rev); totfootn <- t(totfootn)
        
        
        
        ### Plot =============================
        if(T){
          
          
          #file.exists()
          
          #exists("E:/REC_7_Data/10_Plots/footprints/hourly_cluster/SES/2018_11/SES_footprint_5_2018_11_01_0_1500.png")
          
          ppath <- paste("E:/REC_7_Data/10_Plots/footprints/hourly_cluster/", cluster,  "/",  #"_foot_movie/", 
            datfoot[i,"yyyy"], "_", datfoot[i,"mon"], "/", sep="")
          filenm <- paste(ppath, cluster, "_footprint_5_", tlab, ".png", sep="")
          
          
          if(!file.exists(filenm)){
            png(filenm, width=900, height=900)    # was 600
            par(mar=c(5,5,4,4), oma=c(1,1,1,2) )
            
            image.plot(FFP$y_2d, FFP$x_2d, totfootn, xlab="", ylab="", zlim=c(-0.00001, 0.0005),
                       cex.axis=1.5) #, legend.args=list(text="", cex.legend=1.5))   # FFP_gm
            
            
            
            
            
            #c("EC new 50%", "EC new 80%", "EC AF 50%", "EC AF 80%")
            
            legend("bottomleft", c(paste(cluster, "ECx 50%"), paste(cluster, "ECx 80%"), 
                                   paste("US-", cluster, "50%"), paste("US-", cluster, "80%")), 
                   col=c("red", "orange", "red", "orange"), lty=c(1, 1, 3, 3), lwd=2, cex=2)  # was 1.5
            
            for(dt in dts){
              if(dt %in% dts[1:4]){
                FFP <- get(paste("FFP", adatasets[dt], sep="_"))      # plot RECs foot
                lines(FFP$xr[[5]],FFP$yr[[5]], type="l", col="red")
                lines(FFP$xr[[8]],FFP$yr[[8]], type="l", col="orange")
              } else {
                FFP <- get(paste("FFP", adatasets[dt], sep="_"))      # plot EC foot
                lines(FFP$xr[[5]],FFP$yr[[5]], type="l", col="red", lty=3)
                lines(FFP$xr[[8]],FFP$yr[[8]], type="l", col="orange", lty=3)
              }
            }
            
            mtext(paste(cluster, " - ", tpoint, sep=""), 3, line=1, font=2, cex=2)
            
            dev.off() 
          }
          
        }
        
        
        
        
        
        ## land cover probability (cluster) ==================================================
        
        #pt <- sum(fp3); p1 <- sum(fp3[sel1]); p2 <- sum(fp3[sel2]); p3 <- sum(fp3[sel3]); ps <- sum(p1+p2+p3)
        pt <- sum(totfootn)
        p1 <- sum(totfootn[sel1], na.rm=T); p2 <- sum(totfootn[sel2], na.rm=T); p3 <- sum(totfootn[sel3], na.rm=T); 
        ps <- sum(p1+p2+p3)
        pmat[i, paste(c("pt", "p1", "p2", "p3", "ps"), cluster, sep="_")] <- c(pt, p1, p2, p3, ps)
        
        
        pt <- sum(totfootn[sel80c])
        p1 <- sum(totfootn[sel1&sel80c], na.rm=T); p2 <- sum(totfootn[sel2&sel80c], na.rm=T); p3 <- sum(totfootn[sel3&sel80c], na.rm=T)
        ps <- sum(p1+p2+p3)
        pmat_80[i , paste(c("pt", "p1", "p2", "p3", "ps"), cluster, sep="_")] <- c(pt, p1, p2, p3, ps)
        
        
        
        
      
      }
      
  
  
    }
    
    
    save(pmat, file = paste("E:/REC_7_Data/9_R/Rdata/", "pmat_", 
      mon_ys[k,"yyyy"], "_", mon_ys[k,"mon"], ".Rdata", sep=""))
    
    save(pmat_80, file = paste("E:/REC_7_Data/9_R/Rdata/", "pmat_80_", 
      mon_ys[k,"yyyy"], "_", mon_ys[k,"mon"], ".Rdata", sep=""))
    
  }
  
   
  
  
} # Kljun climatology for 2 clusters (30 min resolution)


















## S1 plotting functions ============

plot_avg_day <- function(avd=avd, mon=NULL, year=NULL, ylims=NULL, test_2=F){
  #browser()
  HOD <- x1 <- 0:23; x2 <- x1; x3 <- x2; x4 <- x3
  HOD <- as.data.frame(HOD)
  avd <- merge(avd, HOD, by="HOD", all=T)
  
  plot_id <- c("a)", "b)", "c)", "d)", "e)", "f)") 
  
  if(is.null(ylims)){
    ylim_H <- range(avd[,paste("Hc", datasets, "mn", sep="_")]+avd[,paste("Hc", datasets, "sde", sep="_")],
                  avd[,paste("Hc", datasets, "mn", sep="_")]-avd[,paste("Hc", datasets, "sde", sep="_")], na.rm=T)
    ylim_LE <- range(avd[,paste("cLEc", datasets, "mn", sep="_")]+avd[,paste("cLEc", datasets, "sde", sep="_")],
                   avd[,paste("cLEc", datasets, "mn", sep="_")]-avd[,paste("cLEc", datasets, "sde", sep="_")], na.rm=T)
    ylim_F <- range(avd[,paste("Fcc", datasets, "mn", sep="_")]+avd[,paste("Fcc", datasets, "sde", sep="_")],
                  avd[,paste("Fcc", datasets, "mn", sep="_")]-avd[,paste("Fcc", datasets, "sde", sep="_")], na.rm=T)
    
  } else {
    ylim_H <- ylims[[1]]
    ylim_LE <- ylims[[2]]
    ylim_F <- ylims[[3]]
  } 
  holl <- "holl_"; 
  titles <- c("Grassland (US-SEG)", "Shrubland (US-SES)", "", "", "", "")
  if(!(is.null(mon)&is.null(year))){titles[1:2] <- paste(titles[1:2], " / ", month.abb[mon], "-", year)}
  
  #ylabs <- c("H (W m-2)", "", "LE (W m-2)", "", "NEE (umolC m-2 s-1)", "")
  ylabs <- c(  expression("H (W m"^"-2"*")"), "",
             expression("LE (W m"^"-2"*")"), "",
             expression("NEE (umol CO"["2"]*" m"^"-2"*"s"^"-1"*")"), ""   )
  ylims_nm <- rep(c("ylim_H", "ylim_LE", "ylim_F"), each=2)
  
  fluxes <- rep(c("Hc_", "cLEc_", "Fcc_"), each=2)
  ids <- rep(c("g", "s"), 3); ns <- 1:4; lb <- ""
  if(test_2){ns <- c("a23", "a34", "a42"); lb <- "test_2_"}
  
  ppath <- "E:/REC_7_Data/10_Plots/2_avg_mon_video/"
  filenm <- paste(ppath, lb, xct, "avg_days_",year, "_", substring(1000+mon, 3,4), ".png", sep="")
  
  if((is.null(mon)&is.null(year))){filenm <- paste("E:/REC_7_Data/10_Plots/", xct, "avg_days_total.png", sep="")}
  
  
  png(filenm, width=600, height=500)
  par(mfrow=c(3,2), mar = c(0, 0, 0, 0), oma = c(6, 6, 4, 0.5), mgp=c(2, 0.5, 0), tck=-0.01)
  for(i in 1:6){
    #browser()
    mns <- avd[,paste(fluxes[i], ids[i], ns, "_mn", sep="")]
    sds <- avd[,paste(fluxes[i], ids[i], ns, "_sde", sep="")]
    
    plot(x1, mns[,1], ylim=get(ylims_nm[i]),pch=4, axes=F, xlab="", ylab="", col="purple"); 
    mtext(titles[i], side=3, line=1, cex=1); box(); mtext(ylabs[i], side=2, line=3)
    mtext (plot_id[i], side=3, adj=0.03, line=-2, cex=1.5) 
    if(i %in% c(1,3,5))axis(2, cex.axis=1.5); 
    if(i %in% c(5,6)){axis(1, cex.axis=1.5); mtext("Hour of Day", side=1, line=2)}
    arrows(x1, mns[,1]-sds[,1], x1, mns[,1]+sds[,1], length=0.05, angle=90, code=3, col="purple")
    points(x2, mns[,2], col="green", pch=4); 
    arrows(x2, mns[,2]-sds[,2], x2, mns[,2]+sds[,2], length=0.05, angle=90, code=3, col="green")
    points(x3, mns[,3], col="darkgreen", pch=4)
    arrows(x3, mns[,3]-sds[,3], x3, mns[,3]+sds[,3], length=0.05, angle=90, code=3, col="darkgreen")
    abline(h=0, col="gray")
    
    if(!test_2){
      points(x4, mns[,4], col="brown", pch=4)
      arrows(x4, mns[,4]-sds[,4], x4, mns[,4]+sds[,4], length=0.05, angle=90, code=3, col="brown")
      
      if(i==1)legend("topright", c("SEG EC1", "SEG EC2", "SEG EC3", "SEG EC4"), 
                     col=c("purple", "green", "darkgreen", "brown"), pch=16, cex=1)
      if(i==2)legend("topright", c("SES EC1", "SES EC2", "SES EC3", "SES EC4"), 
                     col=c("purple", "green", "darkgreen", "brown"), pch=16, cex=1)
    } else {
      if(i==1)legend("topright", c("mn EC23 ", "mn EC34", "mn EC42"), 
                     col=c("purple", "green", "darkgreen"), pch=16, cex=1)
    }
    
    
    
  }
  
  dev.off()
  
  
}

plot_avg_day_AmeriFlux <- function(avd=avd, mon=NULL, year=NULL, ylims=NULL, 
        add_P=F, pmd=pmd, pavd=pavd, irga_s=F, no_seg_nee=F, avg_rec3=F){
  
  #browser()
  HOD <- x1 <- 0:23; x2 <- x1; x3 <- x2; x4 <- x3
  HOD <- as.data.frame(HOD)
  avd <- merge(avd, HOD, by="HOD", all=T)
  
  if(irga_s){
    #avd <- avd[ avd[, "H_gm_mn"]>0 | avd[, "H_sm_mn"]>0 ,]
    avd <- avd[ avd[, "HOD"]>=6 & avd[, "HOD"]<=18 ,]
    x1 <- x2 <- x3 <- x4 <- avd[,"HOD"]
  }
  
  
  
  f2p_H  <- c("Hc_g1",   "Hc_ga4",   "Hc_ga3",  "H_gm",   "Hc_s1",   "Hc_sa4",   "Hc_sa3",   "H_sm")
  f2p_LE <- c("cLEc_g1", "cLEc_ga4", "cLEc_ga3","cLE_gm", "cLEc_s1", "cLEc_sa4", "cLEc_sa3", "cLE_sm")
  f2p_F  <- c("Fcc_g1",  "Fcc_ga4",  "Fcc_ga3", "Fc_gm",  "Fcc_s1",  "Fcc_sa4",  "Fcc_sa3",  "Fc_sm")
  fg <- c("f2p_H", "f2p_H", "f2p_LE", "f2p_LE", "f2p_F", "f2p_F")  
  
  
  if(is.null(ylims)){
    ylim_H <- range(avd[,paste(f2p_H, "mn", sep="_")]+avd[,paste(f2p_H, "sde", sep="_")],
                  avd[,paste(f2p_H, "mn", sep="_")]-avd[,paste(f2p_H, "sde", sep="_")], na.rm=T)
    ylim_LE <- range(avd[,paste(f2p_LE, "mn", sep="_")]+avd[,paste(f2p_LE, "sde", sep="_")],
                   avd[,paste(f2p_LE, "mn", sep="_")]-avd[,paste(f2p_LE, "sde", sep="_")], na.rm=T)
    ylim_F <- range(avd[,paste(f2p_F, "mn", sep="_")]+avd[,paste(f2p_F, "sde", sep="_")],
                  avd[,paste(f2p_F, "mn", sep="_")]-avd[,paste(f2p_F, "sde", sep="_")], na.rm=T)
    
  } else {
    ylim_H <- ylims[[1]]  *c(1, 1.03)
    ylim_LE <- ylims[[2]] *c(1, 1.03)
    ylim_F <- ylims[[3]]  *c(1, 1.03)
  } 
  
  
  
  
  
  holl <- "corr_holl_"; if(irga_s)holl <- paste("corr_irga_s_", holl, sep="")
  titles <- c("Grassland (US-Seg)", "Shrubland (US-Ses)", "", "", "", "")
  if(irga_s){titles[1:2] <- paste(titles[1:2], " / ", year)} # year here is e.g. switch_m1_m60
  if(!(is.null(mon) & is.null(year)) & !irga_s){
    titles[1:2] <- paste(titles[1:2], " / ", month.abb[mon], "-", year)
  }
  
  #ylabs <- c("H (W m-2)", "", "LE (W m-2)", "", "NEE (umolC m-2 s-1)", "")
  ylabs <- c(  expression("H (W m"^"-2"*")"), "", 
             expression("LE (W m"^"-2"*")"), "",
             expression("NEE (umol CO"["2"]*" m"^"-2"*"s"^"-1"*")"), ""           )
  ylims_nm <- rep(c("ylim_H", "ylim_LE", "ylim_F"), each=2)
  
  fluxes <- rep(c("Hc_", "cLEc_", "Fcc_"), each=2)
  ids <- rep(c("g1", "ga4", "ga3", "gm", "s1", "sa4", "sa3", "sm"), 3)
  
  plot_id <- c("a)", "b)", "c)", "d)", "e)", "f)")
  

  if(no_seg_nee){
    avd[, c("Fc_gm_mn", "Fc_gm_sde")] <- NA; holl <- paste("no_seg_nee_", holl, sep="")
  }
  
  cols <- c("purple", "black", "blue", "orange")
  if(!avg_rec3)cols <- c("purple", "black", "orange")
  
  
  ppath <- "E:/REC_7_Data/10_Plots/2_avg_mon_video/"
  ap <- ""; if(add_P)ap <- "add_P_"
  if(!irga_s)filenm <- paste(ppath, holl, xct, ap, "AmeriFlux_avg_days_", year, "_", substring(1000+mon, 3,4), "corr_EC0_test.png", sep="")
  if(irga_s)filenm <- paste(ppath, holl, xct, ap, "AmeriFlux_avg_days_", year, "_corr.png", sep="") # year=e.g Switch1_minus_60
  if((is.null(mon)&is.null(year))){ filenm <- paste("E:/REC_7_Data/10_Plots/", holl, xct, 
                                                  "AmeriFlux_avg_days_total_corr_EC0.png", sep="") }
  
  
  png(filenm, width=600, height=500)
  par(mfrow=c(3,2), mar = c(0, 0, 0, 0), oma = c(6, 6, 4, 6), mgp=c(2, 0.5, 0), tck=-0.01)
  
  
  
  for(i in 1:6){
    #browser()
    
    pcols <- 1:4; if(i %in% c(2, 4, 6))pcols <- 5:8
    mns <- avd[,paste(get(fg[i]), "_mn", sep="")[pcols]]
    sds <- avd[,paste(get(fg[i]), "_sde", sep="")[pcols]]
    
    
    plot(x1, mns[,1], ylim=get(ylims_nm[i]),pch=4, axes=F, xlab="", ylab="", col="purple"); 
    mtext(titles[i], side=3, line=1, cex=1); box(); mtext(ylabs[i], side=2, line=3)
    if(i %in% c(1,3,5))axis(2, cex.axis=1.5); 
    if(i %in% c(5,6)){axis(1, cex.axis=1.5); mtext("Hour of Day", side=1, line=2)}
    arrows(x1, mns[,1]-sds[,1], x1, mns[,1]+sds[,1], length=0.05, angle=90, code=3, col="purple")
    points(x2, mns[,2], col="black", pch=4); 
    arrows(x2, mns[,2]-sds[,2], x2, mns[,2]+sds[,2], length=0.05, angle=90, code=3, col="black")
    if(avg_rec3){
      points(x3, mns[,3], col="blue", pch=4); 
      arrows(x3, mns[,3]-sds[,3], x2, mns[,3]+sds[,3], length=0.05, angle=90, code=3, col="blue")
    }
    points(x4, mns[,4], col="orange", pch=4)
    arrows(x4, mns[,4]-sds[,4], x3, mns[,4]+sds[,4], length=0.05, angle=90, code=3, col="orange")
    mtext (plot_id[i], side=3, adj=0.03, line=-2, cex=1.5)    
    
    abline(h=0, col="gray")
    if(i==1)legend("topright", c("EC1", "EC1234", "EC0"), 
                   col=cols, lty=1, lwd=2, cex=1)    # "SEG mn 3", 
    #if(i==2)legend("topright", c("EC1", "EC1234", "EC0"), 
    #               col=cols, lty=1, lwd=2, cex=1)    # "SES mn 3", 
    if(irga_s & i==1) mtext(mon, line=-3, adj=0.2)
    if(add_P){
      pd <- pavd[pavd[,"yyyy"]==year & pavd[,"mon"]==mon,c("P_gm_mn", "P_sm_mn")]
      ylim_p <- c(0, max(pavd[,c("P_gm_mn", "P_sm_mn")])*1.03)
      wp <- round(pmd[pmd[,"yyyy"]==year & pmd[,"mon"]==mon, c("P_gm", "P_sm")], 1)
      if(i==3){
        mtext("Precip:", side=3, adj=0.85, line=-2, cex=1)
        mtext(paste(wp[,"P_gm"], "mm"), side=3, adj=0.85, line=-4, cex=1)
        par(new=T)
        plot(x1, pd[,"P_gm_mn"], axes=F, xlab="", ylab="", ylim=ylim_p, col="cyan", pch=16, cex=1); 
      }
      if(i==4){
        mtext("Precip:", side=3, adj=0.85, line=-2, cex=1)
        mtext(paste(wp[,"P_sm"], "mm"), side=3, adj=0.85, line=-4, cex=1)
        par(new=T)
        plot(x1, pd[,"P_sm_mn"], axes=F, xlab="", ylab="", ylim=ylim_p, col="cyan", pch=16, cex=1); 
        axis(4, cex.axis=1.5); mtext("Prec (mm/h)", side=4, line=4, cex=1.5)
      }
      
      
    }
    
  }
  
  dev.off()
  
  
} 

plot_avg_day_AmeriFlux_HR <- function(avd=avd, mon=NULL, year=NULL, ylims=NULL, 
        add_P=F, pmd=pmd, pavd=pavd, irga_s=F, no_seg_nee=F, avg_rec3=F){
  
  #browser()
  HOD <- x1 <- 0:23; x2 <- x1; x3 <- x2; x4 <- x3
  HOD <- as.data.frame(HOD)
  avd <- merge(avd, HOD, by="HOD", all=T)
  
  if(irga_s){
    #avd <- avd[ avd[, "H_gm_mn"]>0 | avd[, "H_sm_mn"]>0 ,]
    avd <- avd[ avd[, "HOD"]>=6 & avd[, "HOD"]<=18 ,]
    x1 <- x2 <- x3 <- x4 <- avd[,"HOD"]
  }
  
  
  
  f2p_H  <- c("Hc_g1",   "Hc_ga4",   "Hc_ga3",  "H_gm",   "Hc_s1",   "Hc_sa4",   "Hc_sa3",   "H_sm")
  f2p_LE <- c("cLEc_g1", "cLEc_ga4", "cLEc_ga3","cLE_gm", "cLEc_s1", "cLEc_sa4", "cLEc_sa3", "cLE_sm")
  f2p_F  <- c("Fcc_g1",  "Fcc_ga4",  "Fcc_ga3", "Fc_gm",  "Fcc_s1",  "Fcc_sa4",  "Fcc_sa3",  "Fc_sm")
  fg <- c("f2p_H", "f2p_H", "f2p_LE", "f2p_LE", "f2p_F", "f2p_F")  
  
  
  if(is.null(ylims)){
    ylim_H <- range(avd[,paste(f2p_H, "mn", sep="_")]+avd[,paste(f2p_H, "sde", sep="_")],
                  avd[,paste(f2p_H, "mn", sep="_")]-avd[,paste(f2p_H, "sde", sep="_")], na.rm=T)
    ylim_LE <- range(avd[,paste(f2p_LE, "mn", sep="_")]+avd[,paste(f2p_LE, "sde", sep="_")],
                   avd[,paste(f2p_LE, "mn", sep="_")]-avd[,paste(f2p_LE, "sde", sep="_")], na.rm=T)
    ylim_F <- range(avd[,paste(f2p_F, "mn", sep="_")]+avd[,paste(f2p_F, "sde", sep="_")],
                  avd[,paste(f2p_F, "mn", sep="_")]-avd[,paste(f2p_F, "sde", sep="_")], na.rm=T)
    
  } else {
    ylim_H <- ylims[[1]]  *c(1, 1.03)
    ylim_LE <- ylims[[2]] *c(1, 1.03)
    ylim_F <- ylims[[3]]  *c(1, 1.03)
  } 
  
  
  
  
  
  holl <- "corr_holl_"; if(irga_s)holl <- paste("corr_irga_s_", holl, sep="")
  titles <- c("Grassland (US-Seg)", "Shrubland (US-Ses)", "", "", "", "")
  if(irga_s){titles[1:2] <- paste(titles[1:2], " / ", year)} # year here is e.g. switch_m1_m60
  if(!(is.null(mon) & is.null(year)) & !irga_s){
    titles[1:2] <- paste(titles[1:2], " / ", month.abb[mon], "-", year)
  }
  
  #ylabs <- c("H (W m-2)", "", "LE (W m-2)", "", "NEE (umolC m-2 s-1)", "")
  ylabs <- c(  expression("H (W m"^"-2"*")"), "", 
             expression("LE (W m"^"-2"*")"), "",
             expression("NEE (umol CO"["2"]*" m"^"-2"*"s"^"-1"*")"), ""           )
  ylims_nm <- rep(c("ylim_H", "ylim_LE", "ylim_F"), each=2)
  
  fluxes <- rep(c("Hc_", "cLEc_", "Fcc_"), each=2)
  ids <- rep(c("g1", "ga4", "ga3", "gm", "s1", "sa4", "sa3", "sm"), 3)
  
  plot_id <- c("a)", "b)", "c)", "d)", "e)", "f)")
  
  
  if(no_seg_nee){
    avd[, c("Fc_gm_mn", "Fc_gm_sde")] <- NA; holl <- paste("no_seg_nee_", holl, sep="")
  }
  
  cols <- c("purple", "black", "blue", "orange")
  if(!avg_rec3)cols <- c("purple", "black", "orange")
  
  
  ppath <- "E:/REC_7_Data/10_Plots/2_avg_mon_video/"
  ap <- ""; if(add_P)ap <- "add_P_"
  if(!irga_s)filenm <- paste(ppath, holl, xct, ap, "AmeriFlux_avg_days_", year, "_", substring(1000+mon, 3,4), "corr_EC0_HR.png", sep="")
  if(irga_s)filenm <- paste(ppath, holl, xct, ap, "AmeriFlux_avg_days_", year, "_corr_HR.png", sep="") # year=e.g Switch1_minus_60
  if((is.null(mon)&is.null(year))){ filenm <- paste("E:/REC_7_Data/10_Plots/", holl, xct, 
                                                  "AmeriFlux_avg_days_total_corr_EC0_HR.png", sep="") }
  
  
  png(filenm, width=1200, height=1000)
  par(mfrow=c(3,2), mar = c(0, 0, 0, 0), oma = c(12, 12, 8, 12), mgp=c(4, 1.5, 0), tck=-0.02)
  
  
  
  for(i in 1:6){
    #browser()
    
    pcols <- 1:4; if(i %in% c(2, 4, 6))pcols <- 5:8
    mns <- avd[,paste(get(fg[i]), "_mn", sep="")[pcols]]
    sds <- avd[,paste(get(fg[i]), "_sde", sep="")[pcols]]
    
    
    plot(x1, mns[,1], ylim=get(ylims_nm[i]),pch=4, axes=F, xlab="", ylab="", col="purple"); 
    mtext(titles[i], side=3, line=1, cex=2); box(); mtext(ylabs[i], side=2, line=6, cex=2)
    if(i %in% c(1,3,5))axis(2, cex.axis=3); 
    if(i %in% c(5,6)){axis(1, cex.axis=3); mtext("Hour of Day", side=1, line=4, cex=2)}
    arrows(x1, mns[,1]-sds[,1], x1, mns[,1]+sds[,1], length=0.05, angle=90, code=3, col="purple", lwd=2)
    points(x2, mns[,2], col="black", pch=4, cex=2); 
    arrows(x2, mns[,2]-sds[,2], x2, mns[,2]+sds[,2], length=0.05, angle=90, code=3, col="black", lwd=2)
    if(avg_rec3){
      points(x3, mns[,3], col="blue", pch=4, cex=2); 
      arrows(x3, mns[,3]-sds[,3], x2, mns[,3]+sds[,3], length=0.05, angle=90, code=3, col="blue", lwd=2)
    }
    points(x4, mns[,4], col="orange", pch=4, cex=2)
    arrows(x4, mns[,4]-sds[,4], x3, mns[,4]+sds[,4], length=0.05, angle=90, code=3, col="orange", lwd=2)
    mtext (plot_id[i], side=3, adj=0.03, line=-4, cex=3)    
    
    abline(h=0, col="gray")
    if(i==1)legend("topright", c("EC1", "EC1234", "EC0"), 
                   col=cols, lty=1, lwd=2, cex=2)    # "SEG mn 3", 
    #if(i==2)legend("topright", c("EC1", "EC1234", "EC0"), 
    #               col=cols, lty=1, lwd=2, cex=1)    # "SES mn 3", 
    if(irga_s & i==1) mtext(mon, line=-6, adj=0.2, cex=2)
    if(add_P){
      pd <- pavd[pavd[,"yyyy"]==year & pavd[,"mon"]==mon,c("P_gm_mn", "P_sm_mn")]
      ylim_p <- c(0, max(pavd[,c("P_gm_mn", "P_sm_mn")])*1.03)
      wp <- round(pmd[pmd[,"yyyy"]==year & pmd[,"mon"]==mon, c("P_gm", "P_sm")], 1)
      if(i==3){
        mtext("Precip:", side=3, adj=0.85, line=-4, cex=2)
        mtext(paste(wp[,"P_gm"], "mm"), side=3, adj=0.85, line=-8, cex=2)
        par(new=T)
        plot(x1, pd[,"P_gm_mn"], axes=F, xlab="", ylab="", ylim=ylim_p, col="cyan", pch=16, cex=2); 
      }
      if(i==4){
        mtext("Precip:", side=3, adj=0.85, line=-4, cex=2)
        mtext(paste(wp[,"P_sm"], "mm"), side=3, adj=0.85, line=-8, cex=2)
        par(new=T)
        plot(x1, pd[,"P_sm_mn"], axes=F, xlab="", ylab="", ylim=ylim_p, col="cyan", pch=16, cex=2); 
        axis(4, cex.axis=3); mtext("Prec (mm/h)", side=4, line=8, cex=3)
      }
      
      
    }
    
  }
  
  dev.off()
  
  
} 

plot_months_3_1 <- function(dat=dat, mon=mm, year=yy, telemetry=F, RH="cell", corrected=T, 
        AmeriFlux=F, AmeriFlux_edire=F, avg_rec4=T, avg_rec3=F, no_rec234=T, no_rec1=F, no_seg_nee=F,
        add_wdir=F, add_ts=F, add_w=F){
  
  
  #telemetry <- F; RH <- "cell"; corrected <- T; AmeriFlux <- F; avg_rec4 <- T; avg_rec3 <- T; only_rec1 <- T
  
  
  cols <- c("purple", "green",  "darkgreen", "brown",  "black",    "orange", "cyan",          "blue",    "red")
  labg <- labs <- c("EC1","EC2","EC3",   "EC4","EC1234", "EC0", "Precip", "EC234", "EC0 EdiRe")
  
  sel <- c(T, T, T, T, F, F, F, F, F)
  
  
  
  xlim <- range(dat[,"dt"])
  if(mon==10 & year==2018){
    xlim <- range(dat[ (dat[,"dt"] > "22/10/18" & dat[,"dt"] < "30/10/18"), "dt"])
  }
  
  
  ## wanted columns
  
  fluxes <- c("H", "LE", "Fc")
  if(RH=="cell")fluxes[2] <- paste("c", fluxes[2], sep="")
  if(RH=="heat")fluxes[2] <- paste("r", fluxes[2], sep="")
  if(corrected)fluxes <- paste(fluxes, "c", sep="")
  recs <- 1:4; 
  
  
  if(avg_rec4){
    recs <- c(recs,"a4"); sel[5] <- T
    
    mat <- matrix(NA, ncol=6, nrow=nrow(dat)); 
    colnames(mat) <- paste(rep(fluxes,2), c("ga4", "ga4", "ga4", "sa4", "sa4", "sa4"), sep="_" )
    
    mat[,1]  <- apply( dat[,paste(fluxes[1],"_g", recs[1:4], sep="")], 1, mean)
    mat[,2] <- apply( dat[,paste(fluxes[2],"_g", recs[1:4], sep="")], 1, mean)
    mat[,3]  <- apply( dat[,paste(fluxes[3],"_g", recs[1:4], sep="")], 1, mean)
    
    mat[,4]  <- apply( dat[,paste(fluxes[1],"_s", recs[1:4], sep="")], 1, mean)
    mat[,5] <- apply( dat[,paste(fluxes[2],"_s", recs[1:4], sep="")], 1, mean)
    mat[,6]  <- apply( dat[,paste(fluxes[3],"_s", recs[1:4], sep="")], 1, mean)
    
    dat <- cbind(dat, mat)
  }
  
  
  
  
  if(avg_rec3){
    recs <- c(recs,"a3"); sel[8] <- T
    
    mat <- matrix(NA, ncol=6, nrow=nrow(dat)); 
    colnames(mat) <- paste(rep(fluxes,2), c("ga3", "ga3", "ga3", "sa3", "sa3", "sa3"), sep="_" )
    
    mat[,1]  <- apply( dat[,paste(fluxes[1],"_g", recs[2:4], sep="")], 1, mean)
    mat[,2] <- apply( dat[,paste(fluxes[2],"_g", recs[2:4], sep="")], 1, mean)
    mat[,3]  <- apply( dat[,paste(fluxes[3],"_g", recs[2:4], sep="")], 1, mean)
    
    mat[,4]  <- apply( dat[,paste(fluxes[1],"_s", recs[2:4], sep="")], 1, mean)
    mat[,5] <- apply( dat[,paste(fluxes[2],"_s", recs[2:4], sep="")], 1, mean)
    mat[,6]  <- apply( dat[,paste(fluxes[3],"_s", recs[2:4], sep="")], 1, mean)
    
    dat <- cbind(dat, mat)
  }
  
  
  
  
  
  
  
  
  if(AmeriFlux){recs <- c(recs,"m");     sel[c(6,7)] <- T}
  systems <- paste(rep(c("g", "s"), each=length(recs)), rep(recs, 2), sep="")
  cols_to_plot <- paste(rep(fluxes, each=length(systems)), rep(systems, 3), sep="_")
  if(AmeriFlux)cols_to_plot[grepl("m", cols_to_plot)] <- c("H_gm", "H_sm", "cLE_gm", "cLE_sm", "Fc_gm", "Fc_sm")
  
  
  if(no_rec234){
    cols_to_plot <- cols_to_plot[substring(cols_to_plot, nchar(cols_to_plot)-1, 
                    nchar(cols_to_plot)) %in% c("g1","s1", "gm", "sm", "a4", "a3")]
    sel[c(2,3,4)] <- F; recs <- recs[-c(2,3,4)]
  }
  
  
  
  
  if(no_rec1){
    cols_to_plot <- cols_to_plot[substring(cols_to_plot, nchar(cols_to_plot)-1, 
                    nchar(cols_to_plot)) %in% c("gm", "sm", "a4", "a3")]
    sel[c(1)] <- F; recs <- recs[-c(1)]
  }
  
  
  
  if(AmeriFlux_edire){
    cols_to_plot <- c("Hc_g1", "H_gm", "Hc_gp", "cLEc_g1", "cLE_gm", "LEcw_gp", "Fcc_g1", "Fc_gm", "Fccw_gp",
                    "Hc_s1", "H_sm", "Hc_sp", "cLEc_s1", "cLE_sm", "LEcw_sp", "Fcc_s1", "Fc_sm", "Fccw_sp")
    sel <- sel <- c(T, F, F, F, F, T, F, F, T)
    recs <- c("1", "m", "p")
  }
  
  
  
  
  
  prefix <- paste(c(recs,fluxes), collapse="_")
  if(add_wdir){
    prefix <- paste("wd_", prefix, sep=""); 
    dat[ dat[,"WD_gm"]<(-9000), "WD_gm"] <- NA
    dat[ dat[,"WD_sm"]<(-9000), "WD_sm"] <- NA
  }
  if(add_ts){prefix <- paste("ts_", prefix, sep="")}
  if(add_w){prefix <- paste("w_", prefix, sep="")}
  if(no_seg_nee){
    prefix <- paste("no_seg_nee_", prefix, sep="")
    if("Fc_gm" %in% colnames(dat)) dat[, "Fc_gm"] <- NA
  }
  
  
  gpath <- "E:/REC_7_Data/10_Plots/1_SEG_mon_flux_video/"
  png(paste(gpath, xct, prefix, ifelse(telemetry, "_telemetry", ""), "_SEG_mon_fluxes_", year, "_", 
            substring(1000+mon, 3,4), "_corr_00.png", sep=""), width=2500, height=1800)
  
  
  title_SEG <- "Grassland (US-Seg)"
  if(!(is.null(mon)&is.null(year))){title_SEG <- paste(title_SEG, " / ", month.abb[mon], "-", year)}
  
  par(mfrow=c(3,1), mar = c(0, 0, 0, 0), oma = c(22, 16, 10, 12), mgp=c(2, 2, 0))
  
  
  # SEG - Sensible heat
  scol <- grepl("H", cols_to_plot)&grepl("g", cols_to_plot)
  ylim <- range(dat[, cols_to_plot[scol]], na.rm=T)
  plot(dat[,"H_g1"]~dat[,"dt"], type='n', xlim=xlim, ylim=ylim, axes=F) 
  for(i in 1:sum(scol)){lines(dat[,cols_to_plot[scol][i]]~dat[,"dt"], col=cols[sel][i])}
  axis(2, cex.axis=4); box(); mtext(title_SEG, side=3, line=2, cex=5); 
  #mtext(paste("H (W m-2)",sep=""), side=2, line=8, cex=4)
  mtext(expression("H (W m"^"-2"*")"), 2, line=8, cex=4)
  mtext ("a)", side=3, adj=0.01, line=-5, cex=3)
  abline(h=0, col="grey")
  if(add_ts){ 
    par(new=T)
    plot(dat[,"TA_gm"]~dat[,"dt"], xlim=xlim, ylim=c(-100, 40), col='darkorange', type="l", axes=F, xlab="", ylab="")
    lines(dat[,"mean_gT_g1"]~dat[,"dt"], col="magenta")
    lines(dat[,"AirTC_Avg_gm"]~dat[,"dt"], col="darkorange", lty=3)
    axis(4, cex.axis=4); mtext("Air T (deg C)", side=4, line=9, cex=4)
    abline(h=180, col="grey", lty=3)
  }
  
  
  
  
  
  
  
  # SEG - Latent heat
  scol <- grepl("LE", cols_to_plot)&grepl("g", cols_to_plot)
  ylim <- range(dat[, cols_to_plot[scol]], na.rm=T)
  plot(dat[,"cLE_g1"]~dat[,"dt"], type='n', xlim=xlim, ylim=ylim, axes=F) 
  for(i in 1:sum(scol)){lines(dat[,cols_to_plot[scol][i]]~dat[,"dt"], col=cols[sel][i])}
  axis(2, cex.axis=4); box(); #mtext(paste("LE (W m-2)",sep=""), side=2, line=8, cex=4)
  #mtext(paste(fluxes[2]," (W m-2)",sep=""), side=2, line=8, cex=4)
  mtext(expression("LE (W m"^"-2"*")"), 2, line=8, cex=4)
  mtext ("b)", side=3, adj=0.01, line=-5, cex=3)
  legend("topright", labg[sel], ncol=2, col=cols[sel], pch=16, cex=4)
  abline(h=0, col="grey")
  if(sum(is.na(dat[,"P_gm"]))<nrow(dat)){ 
    par(new=T)
    plot(dat[,"P_hh_gm"]~dat[,"dt"], xlim=xlim, col='cyan', type="l", axes=F, xlab="", ylab="")
    axis(4, cex.axis=4); mtext("Prec (mm / h)", side=4, line=9, cex=4)
  }
  
  
  # SEG - NEE
  scol <- grepl("F", cols_to_plot)&grepl("g", cols_to_plot)
  ylim <- range(dat[, cols_to_plot[scol]], na.rm=T)
  plot(dat[,"Fc_g1"]~dat[,"dt"], type='n', xlim=xlim, ylim=ylim, axes=F) 
  for(i in 1:sum(scol)){lines(dat[,cols_to_plot[scol][i]]~dat[,"dt"], col=cols[sel][i])}
  axis(2, cex.axis=4); box(); mtext("Time", side=1, line=20, cex=4)
  #mtext("NEE (umol m-2 s-1)", side=2, line=8, cex=4);
  mtext(expression("NEE (umol CO"["2"]*" m"^"-2"*"s"^"-1"*")"), 2, line=8, cex=4)
  mtext ("c)", side=3, adj=0.01, line=-5, cex=3)
  abline(h=0, col="grey")
  axis.Date(1, at=seq(min(dat[,"dt"]), max(dat[,"dt"]), by="week"), cex.axis=4, las=3); 
  if(mon==10 & year==2018){
    axis.Date(1, at=seq(min(dat[,"dt"]), max(dat[,"dt"]), by="day"), cex.axis=4, las=3); 
  }
  if(add_wdir){ 
    par(new=T)
    plot(dat[,"WD_gm"]~dat[,"dt"], xlim=xlim, ylim=c(-700, 360), col='blue', type="l", axes=F, xlab="", ylab="")
    axis(4, cex.axis=4); mtext("WDir (deg)", side=4, line=9, cex=4)
    abline(h=180, col="grey", lty=3)
  }
  if(add_w){ 
    par(new=T)
    plot(dat[,"w_mean_unrot_gm"]~dat[,"dt"], xlim=xlim, ylim=c(-1, 7), col='darkorange', type="l", axes=F, xlab="", ylab="")
    lines(dat[,"mean_gW_pre_rot_g1"]~dat[,"dt"], col="magenta")
    axis(4, cex.axis=4, line=-8); mtext("unrot W (m-1 s-1)", side=4, line=-2, cex=3)
    abline(h=0, col="grey", lty=3)
  }
  
  
  
  
  
  dev.off()
  
  
  
  
  
  
  
  spath <- "E:/REC_7_Data/10_Plots/1_SES_mon_flux_video/"
  png(paste(spath, xct, prefix, ifelse(telemetry, "_telemetry", ""), "_SES_mon_fluxes_", year, "_", 
            substring(1000+mon, 3,4), "_corr_00.png", sep=""), width=2500, height=1800)
  
  title_SES <- "Shrubland (US-Ses)"
  if(!(is.null(mon)&is.null(year))){title_SES <- paste(title_SES, " / ", month.abb[mon], "-", year)}
  
  
  par(mfrow=c(3,1), mar = c(0, 0, 0, 0), oma = c(22, 16, 10, 12), mgp=c(2, 2, 0))
  
  # SES - Sensible heat
  scol <- grepl("H", cols_to_plot)&grepl("s", cols_to_plot)
  ylim <- range(dat[, cols_to_plot[scol]], na.rm=T)
  plot(dat[,"H_g1"]~dat[,"dt"], type='n', xlim=xlim, ylim=ylim, axes=F) 
  for(i in 1:sum(scol)){lines(dat[,cols_to_plot[scol][i]]~dat[,"dt"], col=cols[sel][i])}
  axis(2, cex.axis=4); box(); mtext(title_SES, side=3, line=2, cex=5); 
  #mtext(paste("H (W m-2)",sep=""), side=2, line=8, cex=4)
  mtext(expression("H (W m"^"-2"*")"), 2, line=8, cex=4)
  mtext ("a)", side=3, adj=0.01, line=-5, cex=3)
  abline(h=0, col="grey")
  if(add_ts){ 
    par(new=T)
    plot(dat[,"TA_sm"]~dat[,"dt"], xlim=xlim, ylim=c(-100, 40), col='darkorange', type="l", axes=F, xlab="", ylab="")
    lines(dat[,"mean_gT_s1"]~dat[,"dt"], col="magenta")
    lines(dat[,"AirTC_Avg_sm"]~dat[,"dt"], col="darkorange", lty=3)
    axis(4, cex.axis=4); mtext("Air T (deg C)", side=4, line=9, cex=4)
    abline(h=180, col="grey", lty=3)
  }
  
  
  # SES - Latent heat
  scol <- grepl("LE", cols_to_plot)&grepl("s", cols_to_plot)
  ylim <- range(dat[, cols_to_plot[scol]], na.rm=T)
  plot(dat[,"cLE_g1"]~dat[,"dt"], type='n', xlim=xlim, ylim=ylim, axes=F) 
  for(i in 1:sum(scol)){lines(dat[,cols_to_plot[scol][i]]~dat[,"dt"], col=cols[sel][i])}
  axis(2, cex.axis=4); box(); #mtext(paste("LE (W m-2)",sep=""), side=2, line=8, cex=4)
  #mtext(paste(fluxes[2]," (W m-2)",sep=""), side=2, line=8, cex=4)
  mtext(expression("LE (W m"^"-2"*")"), 2, line=8, cex=4)
  mtext ("b)", side=3, adj=0.01, line=-5, cex=3)
  legend("topright", labs[sel], ncol=2, col=cols[sel], pch=16, cex=4)
  abline(h=0, col="grey")
  if(sum(is.na(dat[,"P_gm"]))<nrow(dat)){ 
    par(new=T)
    plot(dat[,"P_hh_sm"]~dat[,"dt"], xlim=xlim, col='cyan', type="l", axes=F, xlab="", ylab="")
    axis(4, cex.axis=4); mtext("Prec (mm / h)", side=4, line=9, cex=4)
  }
  
  
  # SES - NEE
  scol <- grepl("F", cols_to_plot)&grepl("s", cols_to_plot)
  ylim <- range(dat[, cols_to_plot[scol]], na.rm=T)
  plot(dat[,"Fc_g1"]~dat[,"dt"], type='n', xlim=xlim, ylim=ylim, axes=F) 
  for(i in 1:sum(scol)){lines(dat[,cols_to_plot[scol][i]]~dat[,"dt"], col=cols[sel][i])}
  axis(2, cex.axis=4); box(); mtext("Time", side=1, line=20, cex=4)
  #mtext("NEE (umol m-2 s-1)", side=2, line=8, cex=4); 
  mtext(expression("NEE (umol CO"["2"]*" m"^"-2"*"s"^"-1"*")"), 2, line=8, cex=4)
  mtext ("c)", side=3, adj=0.01, line=-5, cex=3)
  abline(h=0, col="grey")
  axis.Date(1, at=seq(min(dat[,"dt"]), max(dat[,"dt"]), by="week"), cex.axis=4, las=3); 
  if(mon==10 & year==2018){
    axis.Date(1, at=seq(min(dat[,"dt"]), max(dat[,"dt"]), by="day"), cex.axis=4, las=3); 
  }
  if(add_wdir){ 
    par(new=T)
    plot(dat[,"WD_sm"]~dat[,"dt"], xlim=xlim, ylim=c(-700, 360), col='blue', type="l", axes=F, xlab="", ylab="")
    axis(4, cex.axis=4); mtext("WDir (deg)", side=4, line=9, cex=4)
    abline(h=180, col="grey", lty=3)
  }
  if(add_w){ 
    par(new=T)
    plot(dat[,"w_mean_unrot_sm"]~dat[,"dt"], xlim=xlim, ylim=c(-1, 7), col='darkorange', type="l", axes=F, xlab="", ylab="")
    lines(dat[,"mean_gW_pre_rot_s1"]~dat[,"dt"], col="magenta")
    axis(4, cex.axis=4, line=-8); mtext("unrot W (m-1 s-1)", side=4, line=-2, cex=3)
    abline(h=0, col="grey", lty=3)
  }
  
  
  
  
  
  dev.off()
}


plot_REC1_vs_conv1 <- function(mode, dat_ym=dat_ym, mon=mm, year=yy, no_seg_nee=F){
  
  # 'mode' parameter can be: 
  # 1) raw, 
  # 2) corrected (frequency corrected) , 
  # 3) reddy (u* and gap-fill of corrected fluxes)
  
  #browser()
  rfx <- c("H_g1", "cLE_g1", "Fc_g1", "H_s1", "cLE_s1", "Fc_s1"); attr <- ""
  mfx <- c("H_gm", "cLE_gm", "Fc_gm", "H_sm", "cLE_sm", "Fc_sm")
  if(mode=="corrected"){
    rfx <- c("Hc_g1", "cLEc_g1", "Fcc_g1", "Hc_s1", "cLEc_s1", "Fcc_s1"); attr <- "_corr"
  }
  if(mode=="reddy"){
    rfx <- c("H_f_g1", "LE_f_g1", "NEE_uStar_f_g1", "H_f_s1", "LE_f_s1", "NEE_uStar_f_s1"); attr <- "_reddy"
    mfx <- c("H_f_gm", "LE_f_gm", "NEE_uStar_f_gm", "H_f_sm", "LE_f_sm", "NEE_uStar_f_sm")
  }
  
    
  # Total least squares
  ints <- slopes <- cors <- rep(NA, 6)
  for(i in 1:6){
    aa <- dat_ym[,c(mfx[i], rfx[i])]; bb <- aa[complete.cases(aa),]
    cc <- prcomp(bb)$rotation
    cors[i]   <-  cor(bb, method="pearson")[1,2]
    slopes[i] <-  beta  <-  cc[2,1]/cc[1,1]
    ints[i]   <-  mean(bb[,2])-beta*mean(bb[,1])
    
    
    #lmo <- summary(lm(dat_ym[,rfx[i]]~dat_ym[,mfx[i]]))
    #slopes[i] <-  lmo$coefficients[2,1]
    #ints[i]   <-  lmo$coefficients[1,1]
  }
  
  if(no_seg_nee){
    dat_ym[,mfx[3]] <- NA
    dat_ym[,rfx[3]] <- NA
    attr <- paste(attr, "_no_seg_nee", sep="")
  }
  
  
  
  lims_h  <- range(dat_ym[, c("H_f_g1",  "H_f_s1",  "H_f_gm",  "H_f_sm")],  na.rm=T)
  lims_le <- range(dat_ym[, c("LE_f_g1", "LE_f_s1", "LE_f_gm", "LE_f_sm")], na.rm=T)
  lims_fc <- range(dat_ym[, c("NEE_uStar_f_g1",  "NEE_uStar_f_s1",  
                              "NEE_uStar_f_gm",  "NEE_uStar_f_sm")], na.rm=T)
  #ylims_11   <- list(ylim_H_11, ylim_LE_11, ylim_F_11)
  
  #lims_h <- ylims_11[[1]]
  #lims_le <- ylims_11[[2]]
  #lims_fc <- ylims_11[[3]]
  
  
  if(is.null(mon)&is.null(year)){attr <- paste(attr, "_all", sep="")}
  
  
  
  pl_name <- paste("E:/REC_7_Data/10_Plots/6_REC1_vs_Conv/corr_REC1_vs_Conv_", xct, year, "_", mon, attr, "_00.pdf", sep="")
  pdf(pl_name, width=100, height=150)
  par(mfrow=c(3,2), mar=c(8,9,5,1) , oma=c(0,0,10,0), mgp=c(5.5 ,2, 0))
  
  
  # H / US-SEG
  plot(dat_ym[,mfx[1]], dat_ym[,rfx[1]], ylim=lims_h, xlim=lims_h, pch=16, cex.axis=2.5, cex.main=2.5, cex.lab=2.5,
       #xlab="US-SEG - H (W m-2)", ylab="SEG EC1 - H (W m-2)", 
       xlab=expression("Seg EC0 - H (W m"^"-2"*")"), 
       ylab=expression("Seg EC1 - H (W m"^"-2"*")"),
       main=paste("Sensible Heat Flux    ", month.abb[mon], year), col=alpha(1, 0.2))
  mtext("Grassland", side=3, line=6, cex=4)
  mtext(paste("y =", round(ints[1],3), "+", round(slopes[1],3),"x"), 
        side=3, adj=0.05, line=-3, cex=2); abline(a=ints[1], b=slopes[1]); 
  mtext(paste("r:", round(cors[1],2)), side=3, adj=0.05, line=-6, cex=2)
  mtext ("a)", side=3, adj=0, line=1, cex=2.5)
  abline(a=0, b=1)
  
  
  # H / US-SES
  plot(dat_ym[,mfx[4]], dat_ym[,rfx[4]], ylim=lims_h, xlim=lims_h, pch=16, cex.axis=2.5, cex.main=2.5, cex.lab=2.5,
       #xlab="US-SES - H (W m-2)", ylab="SES EC1 - H (W m-2)",
       xlab=expression("Ses EC0 - H (W m"^"-2"*")"), 
       ylab=expression("Ses EC1 - H (W m"^"-2"*")"),
       main=paste("Sensible Heat Flux     ", month.abb[mon], year), col=alpha(1, 0.2))
  mtext("Shrubland", side=3, line=6, cex=4)
  mtext(paste("y =", round(ints[4],3), "+", round(slopes[4],3),"x"),
        side=3, adj=0.05, line=-3, cex=2); abline(a=ints[4], b=slopes[4]); 
  mtext(paste("r:", round(cors[4],2)), side=3, adj=0.05, line=-6, cex=2)
  mtext ("b)", side=3, adj=0, line=1, cex=2.5)
  abline(a=0, b=1)
  
  
  
  
  # LE / US-SEG
  plot(dat_ym[,mfx[2]], dat_ym[,rfx[2]], ylim=lims_le, xlim=lims_le, pch=16, cex.axis=2.5, cex.main=2.5, cex.lab=2.5,
       #xlab="US-SEG - LE (W m-2)", ylab="SEG EC1 - LE (W m-2)", 
       xlab=expression("Seg EC0 - LE (W m"^"-2"*")"), 
       ylab=expression("Seg EC1 - LE (W m"^"-2"*")"),
       main=paste("Latent Heat Flux    ", month.abb[mon], year), col=alpha(1, 0.2))
  mtext(paste("y =", round(ints[2],3), "+", round(slopes[2],3),"x"),
        side=3, line=-3, adj=0.05, cex=2); abline(a=ints[2], b=slopes[2]); 
  mtext(paste("r:", round(cors[2],2)), side=3, adj=0.05, line=-6, cex=2)
  mtext ("c)", side=3, adj=0, line=1, cex=2.5)
  abline(a=0, b=1)    
  
  
  # LE / US-SES
  plot(dat_ym[,mfx[5]], dat_ym[,rfx[5]], ylim=lims_le, xlim=lims_le, pch=16, cex.axis=2.5, cex.main=2.5, cex.lab=2.5,
       #xlab="US-SES - LE (W m-2)", ylab="SES EC1 - LE (W m-2)",
       xlab=expression("Ses EC0 LE (W m"^"-2"*")"), 
       ylab=expression("Ses EC1 - LE (W m"^"-2"*")"),
       main=paste("Latent Heat Flux    ", month.abb[mon], year), col=alpha(1, 0.2))
  mtext(paste("y =", round(ints[5],3), "+", round(slopes[5],3),"x"),
        side=3, line=-3, adj=0.05, cex=2); abline(a=ints[5], b=slopes[5]);
  mtext(paste("r:", round(cors[5],2)), side=3, adj=0.05, line=-6, cex=2)
  mtext ("d)", side=3, adj=0, line=1, cex=2.5)
  abline(a=0, b=1)
  
  
  
  
  
  # NEE / US-SEG
  plot(dat_ym[,mfx[3]], dat_ym[,rfx[3]], ylim=lims_fc, xlim=lims_fc, pch=16, cex.axis=2.5, cex.main=2.5, cex.lab=2.5,
       #xlab="US-SEG - NEE (umol m-2 s-1)", ylab="SEG EC1 - NEE (umol m-2 s-1)", 
       xlab=expression("Seg EC0 - NEE (umol CO"["2"]*" m"^"-2"*"s"^"-1"*")"),
       ylab=expression("Seg EC1 - NEE (umol CO"["2"]*" m"^"-2"*"s"^"-1"*")"),
       #main=paste("NEE    ", month.abb[mon], year), col=alpha(1, 0.2))
       main=paste("Net Ecosystem Exchange", month.abb[mon], year), col=alpha(1, 0.2))
  abline(a=0, b=1)
  if(!no_seg_nee){
    mtext(paste("y =", round(ints[3],3), "+", round(slopes[3],3),"x"),
          side=3, line=-3, adj=0.05, cex=2); abline(a=ints[3], b=slopes[3]); 
    mtext(paste("r:", round(cors[3],2)), side=3, adj=0.05, line=-6, cex=2)
    mtext ("e)", side=3, adj=0, line=1, cex=2.5)
  }

  
  
  # NEE / US-SES
  plot(dat_ym[,mfx[6]], dat_ym[,rfx[6]], ylim=lims_fc, xlim=lims_fc, pch=16, cex.axis=2.5, cex.main=2.5, cex.lab=2.5,
       #xlab="US-SES - NEE (umol m-2 s-1)", ylab="SES EC1 - NEE (umol m-2 s-1)",
       xlab=expression("Ses EC0 - NEE (umol CO"["2"]*" m"^"-2"*"s"^"-1"*")"),
       ylab=expression("Ses EC1 - NEE (umol CO"["2"]*" m"^"-2"*"s"^"-1"*")"),
       #main=paste("NEE    ", month.abb[mon], year), col=alpha(1, 0.2))
  main=paste("Net Ecosystem Exchange", month.abb[mon], year), col=alpha(1, 0.2))
  mtext(paste("y =", round(ints[6],3), "+", round(slopes[6],3),"x"),
        side=3, line=-3, adj=0.05, cex=2); abline(a=ints[6], b=slopes[6]);
  mtext(paste("r:", round(cors[6],2)), side=3, adj=0.05, line=-6, cex=2)
  mtext ("f)", side=3, adj=0, line=1, cex=2.5)
  abline(a=0, b=1)
  
  
  
  dev.off()
  
} 


plot_REC1_vs_conv1_ggplot <- function(mode, dat_ym=dat_ym, mon=mm, year=yy, no_seg_nee=F, seas=seas_i){
  
  # https://www.r-graph-gallery.com/2d-density-plot-with-ggplot2.html - for reference
  
  # 'mode' parameter can be: 
  # 1) raw, 
  # 2) corrected (frequency corrected) , 
  # 3) reddy (u* and gap-fill of corrected fluxes)
  
  #browser()
  rfx <- c("H_g1", "cLE_g1", "Fc_g1", "H_s1", "cLE_s1", "Fc_s1"); attr <- ""
  mfx <- c("H_gm", "cLE_gm", "Fc_gm", "H_sm", "cLE_sm", "Fc_sm")
  if(mode=="corrected"){
    rfx <- c("Hc_g1", "cLEc_g1", "Fcc_g1", "Hc_s1", "cLEc_s1", "Fcc_s1"); attr <- "_corr"
  }
  if(mode=="reddy"){
    rfx <- c("H_f_g1", "LE_f_g1", "NEE_uStar_f_g1", "H_f_s1", "LE_f_s1", "NEE_uStar_f_s1"); attr <- "_reddy"
    mfx <- c("H_f_gm", "LE_f_gm", "NEE_uStar_f_gm", "H_f_sm", "LE_f_sm", "NEE_uStar_f_sm")
  }
  
  
  
  sl <- ""
  titles <- c("Sensible Heat Flux - Seg", "Latent Heat Flux - Seg", "Net Ecosys. Exchange - Seg",
            "Sensible Heat Flux - Ses", "Latent Heat Flux - Ses", "Net Ecosys. Exchange - Ses")
  
  
  
  # Create list of plot titles for seasonal fluxes
  titles_b <- c("H - Seg", "LE - Seg", "NEE - Seg",
              "H - Ses", "LE - Ses", "NEE - Ses")
  if(seas=="gro"){titles <- paste(titles_b, "- growing season");   sl <- "_gro_seas"}
  if(seas=="sen"){titles <- paste(titles_b, "- senescent season"); sl <- "_sen_seas"} 

    
  

  # Total least squares
  ints <- slopes <- cors <- rep(NA, 6)
  for(i in 1:6){
    aa <- dat_ym[,c(mfx[i], rfx[i])]; bb <- aa[complete.cases(aa),]
    cc <- prcomp(bb)$rotation
    cors[i]   <-  cor(bb, method="pearson")[1,2]
    slopes[i] <-  beta  <-  cc[2,1]/cc[1,1]
    ints[i]   <-  mean(bb[,2])-beta*mean(bb[,1])
  }
  
  
  #lims_h  <- range(dat_ym[, c("H_f_g1",  "H_f_s1",  "H_f_gm",  "H_f_sm")],  na.rm=T)
  #lims_le <- range(dat_ym[, c("LE_f_g1", "LE_f_s1", "LE_f_gm", "LE_f_sm")], na.rm=T)
  #lims_fc <- range(dat_ym[, c("NEE_uStar_f_g1",  "NEE_uStar_f_s1",  
  #                         "NEE_uStar_f_gm",  "NEE_uStar_f_sm")], na.rm=T)
  ylims_11   <- list(ylim_H_11, ylim_LE_11, ylim_F_11)
  
  lims_h <- ylims_11[[1]]
  lims_le <- ylims_11[[2]]
  lims_fc <- ylims_11[[3]]
  
  r1 <- paste("y = ", round(ints, 3), "+", round(slopes, 3), "x")
  r2 <- paste("r:", round(cors, 2))
  
  
  
  if(is.null(mon)&is.null(year)){attr <- paste(attr, "_all", sep="")}
  
  
  
  pl_name <- paste("E:/REC_7_Data/10_Plots/6_REC1_vs_Conv/corr_REC1_vs_Conv_", 
                 xct, year, "_", mon, attr, "_ggplot", sl, "_00_test.pdf", sep="")
  
  
  # H / US-SEG
  
  gh <- ggplot(dat_ym, aes(x=H_f_gm, y=H_f_g1) ) +
    labs(x = expression("Seg EC0 - H (W m"^"-2"*")"),
         y = expression("Seg EC1 - H (W m"^"-2"*")"),    #,
         title = titles[1]) +
    geom_bin2d(bins = 150, show.legend=T) +   # Bin size control 
    scale_fill_continuous(type = "viridis") +     # colour palette
    theme_bw() + xlim(lims_h) + ylim(lims_h) +
    #theme_fancy() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + # remove grid 
    theme(text = element_text(size=14)) + 
    theme(legend.key.size = unit(0.5, "cm")) +   # legend size
    theme(legend.position = c(0.85, 0.25)) +       # legend position
    theme(plot.title = element_text(size = 14, face = "bold"))  +
    geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
    geom_abline(intercept = ints[1], slope = slopes[1]) +
    annotate("text", x = 0, y = 500, label = r1[1], size=4)  +
    annotate("text", x = 0, y = 420, label = r2[1], size=4)
  
  
  
  
  # H / US-SES
  
  sh <- ggplot(dat_ym, aes(x=H_f_sm, y=H_f_s1) ) +
    labs(x = expression("Ses EC0 - H (W m"^"-2"*")"),
         y = expression("Ses EC1 - H (W m"^"-2"*")"),
         title = titles[4]) +
    geom_bin2d(bins = 150) +                       # Bin size control 
    scale_fill_continuous(type = "viridis") +     # colour palette
    theme_bw() + xlim(lims_h) + ylim(lims_h) +
    #theme_fancy() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + # remove grid 
    theme(text = element_text(size=14)) + 
    theme(legend.key.size = unit(0.5, "cm")) +   # legend size
    theme(legend.position = c(0.85, 0.25)) +       # legend position
    theme(plot.title = element_text(size = 14, face = "bold"))  +
    geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
    geom_abline(intercept = ints[4], slope = slopes[4]) +
    annotate("text", x = 0, y = 500, label = r1[4], size=4)  +
    annotate("text", x = 0, y = 420, label = r2[4], size=4)
  
  
  # LE / US-SEG
  
  gl <- ggplot(dat_ym, aes(x=LE_f_gm, y=LE_f_g1) ) +
    labs(x = expression("Seg EC0 - LE (W m"^"-2"*")"),
         y = expression("Seg EC1 - LE (W m"^"-2"*")"),
         title = titles[2]) +
    geom_bin2d(bins = 150) +                       # Bin size control 
    scale_fill_continuous(type = "viridis") +     # colour palette
    theme_bw() + xlim(lims_le) + ylim(lims_le) +
    #theme_fancy() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + # remove grid 
    theme(text = element_text(size=14)) + 
    theme(legend.key.size = unit(0.5, "cm")) +   # legend size
    theme(legend.position = c(0.85, 0.25)) +       # legend position
    theme(plot.title = element_text(size = 14, face = "bold"))  +
    geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
    geom_abline(intercept = ints[2], slope = slopes[2]) +
    annotate("text", x = 20, y = 270, label = r1[2], size=4)  +
    annotate("text", x = 20, y = 220, label = r2[2], size=4)
  
  
  # LE / US-SES
  
  sl <- ggplot(dat_ym, aes(x=LE_f_sm, y=LE_f_s1) ) +
    labs(x = expression("Ses EC0 - LE (W m"^"-2"*")"),
         y = expression("Ses EC1 - LE (W m"^"-2"*")"),
         title = titles[5]) +
    geom_bin2d(bins = 150) +                       # Bin size control 
    scale_fill_continuous(type = "viridis") +     # colour palette
    theme_bw() + xlim(lims_le) + ylim(lims_le) +
    #theme_fancy() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + # remove grid 
    theme(text = element_text(size=14)) + 
    theme(legend.key.size = unit(0.5, "cm")) +   # legend size
    theme(legend.position = c(0.85, 0.25)) +       # legend position
    theme(plot.title = element_text(size = 14, face = "bold"))  +
    geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
    geom_abline(intercept = ints[5], slope = slopes[5]) +
    annotate("text", x = 20, y = 270, label = r1[5], size=4)  +
    annotate("text", x = 20, y = 220, label = r2[5], size=4)
  
  
  # NEE / US-SEG
  
  gn <- ggplot(dat_ym, aes(x=NEE_uStar_f_gm, y=NEE_uStar_f_g1) ) +
    labs(x = expression("Seg EC0 - NEE (umol CO"["2"]*" m"^"-2"*"s"^"-1"*")"),
         y = expression("Seg EC1 - NEE (umol CO"["2"]*" m"^"-2"*"s"^"-1"*")"),
         title = titles[3]) +
    geom_bin2d(bins = 150) +                       # Bin size control 
    scale_fill_continuous(type = "viridis") +     # colour palette
    theme_bw() + xlim(lims_fc) + ylim(lims_fc) +
    #theme_fancy() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + # remove grid 
    theme(text = element_text(size=14)) + 
    theme(legend.key.size = unit(0.5, "cm")) +   # legend size
    theme(legend.position = c(0.85, 0.25)) +       # legend position
    theme(plot.title = element_text(size = 14, face = "bold"))  +
    geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
    geom_abline(intercept = ints[3], slope = slopes[3]) +
    annotate("text", x = -7, y = 7, label = r1[3], size=4)  +
    annotate("text", x = -7, y = 5, label = r2[3], size=4)
  
  
  # NEE / US-SES
  
  sn <- ggplot(dat_ym, aes(x=NEE_uStar_f_sm, y=NEE_uStar_f_s1) ) +
    labs(x = expression("Seg EC0 - NEE (umol CO"["2"]*" m"^"-2"*"s"^"-1"*")"),
         y = expression("Seg EC1 - NEE (umol CO"["2"]*" m"^"-2"*"s"^"-1"*")"),
         title = titles[6]) +
    geom_bin2d(bins = 150) +                       # Bin size control 
    scale_fill_continuous(type = "viridis") +     # colour palette
    theme_bw() + xlim(lims_fc) + ylim(lims_fc) +
    #theme_fancy() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + # remove grid 
    theme(text = element_text(size=14)) + 
    theme(legend.key.size = unit(0.5, "cm")) +   # legend size
    theme(legend.position = c(0.85, 0.25)) +       # legend position
    theme(plot.title = element_text(size = 14, face = "bold"))  +
    geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
    geom_abline(intercept = ints[6], slope = slopes[6]) +
    annotate("text", x = -7, y = 7, label = r1[6], size=4)  +
    annotate("text", x = -7, y = 5, label = r2[6], size=4)
  
  
  ### combine plots using Patchwork
  pall <- (gh + sh) / (gl + sl) / (gn + sn) +
    plot_annotation(tag_levels = 'a') & theme(plot.tag.position = c(0.0, 0.97))
  
  ggsave(
    pall,
    filename = pl_name,
    width = 17,
    height = 26,
    units = "cm"
  )
  
  
}  # end function _ggplot


plot_NEE_months_10x3 <- function(dat=dat_ym, mon=mm, year=yy){
  
  dd <- day(dat[,"dt"]); 
  sel1 <- dd<=10; sel2 <- dd>10 & dd<=20; sel3 <- dd>20
  sels <- c("sel1", "sel2", "sel3")
  
  
  mdatasets <- c("gm", "sm")
  sites <- c("SEG", "SES")
  titles <- c("Grassland (US-SEG)", "Shrubland (US-SES)")
  
  pids <- c("_m_1", "_2_3_4")
  
  cols <- c("purple", "green",  "darkgreen", "brown",   "orange", "cyan"    )
  labs <- c("EC1",    "EC2",    "EC3",       "EC4",     "AmeriFlux",  "Precipit")
  
  dts <- c("g1",  "g2", "g3", "g4", "s1", "s2", "s3", "s4", "gm", "sm")
  plabs <- c("a)", "b)", "c)")
  
  
  for(i in 1:2){   # sites loop
    
    title <- paste(titles[i], " / ", month.abb[mon], "-", year)
    ci <- c(1:4, 9); if(i==2)ci <- c(5:8, 10)
    wcols <- paste("NEE_uStar_f", dts[ci], sep="_")
    pc <- paste("P_hh", mdatasets[i], sep="_")
    
    ylim_c <- range(dat[,wcols], na.rm=T) * c(1, 1.1)
    ylim_p <- range(dat[,pc], na.rm=T)
    
    
    
    for(ip in 1:2){   # plot  AmeriFlux and REC1 or REC 2_3_4 
      
      ks <- c(1, 5); if(ip==2)ks <- c(2, 3, 4)
      
      gpath <- "E:/REC_7_Data/10_Plots/20_CO2_rain_response/"
      png(paste(gpath, sites[i], pids[ip], "_mon_NEE_10x3_", year, "_", 
                substring(1000+mon, 3,4), ".png", sep=""), width=3500, height=1800)
      
      par(mfrow=c(3,1), mar = c(0, 0, 0, 0), oma = c(15, 16, 8, 12), mgp=c(2, 2, 0))
      
      
      
      
      for(j in 1:3){   # loop for 3 tabs
        
        sel <- get(sels[j])
        xlim <- range(dat[sel,"dt"])
        
        plot(dat[sel, "Fc_g1"]~dat[sel, "dt"], type='n', xlim=xlim, ylim=ylim_c, axes=F) 
        for(k in ks){lines(dat[sel, wcols[k]]~dat[sel, "dt"], col=cols[k])}
        axis(2, cex.axis=4); box(); abline(h=0, col="grey")
        mtext (plabs[j], side=3, adj=0.01, line=-5, cex=3)
        axis.Date(1, at=seq(min(dat[sel,"dt"]), max(dat[sel,"dt"]), by="day"), cex.axis=4, las=3, format="%d"); 
        if(j==1){
          mtext(title, side=3, line=1, cex=4)
          legend("topright", labs[c(ks,6)], col=cols[c(ks, 6)], lty=1, lwd=4, cex=4, ncol=length(ks)+1)
        }
        if(j==3){
          mtext("Time", side=1, line=11, cex=4)
          mtext("NEE (umol m-2 s-1)", side=2, line=8, cex=4, outer=T);
          mtext("Prec (mm / h)", side=4, line=9, cex=4, outer=T)
        }
        if(sum(is.na(dat[,pc]))<nrow(dat)){ 
          par(new=T)
          plot(dat[sel, pc]~dat[sel, "dt"], xlim=xlim, ylim=ylim_p, col='cyan', type="l", axes=F, xlab="", ylab="")
          axis(4, cex.axis=4); 
        }
        
        
      } # end tabs (image) loop
      
      
      dev.off()
      
      
    } # end m_1 vs 2_3_4 plots
    
    
    
  } # end sites loop
  
  
  
  
  
  
}







stop("prepare to plot")

# plotting functions --------- testing area / plot all data

mon <- mm <- NULL
year <- yy <- NULL
dat_ym <- dat <- datdd

# select a single month for plotting
dat_ym <- dat <- datdd[datdd[,"yyyy"]==yy & datdd[,"mon"]==mm,]


###### dataset for daily sums scatter plot
dat_ym <- aggregate(datdd[,dcols],by=list(dates(datdd[,"dt"])), sum, na.rm=T)



# plot REC1 vs US-SEX (all)
mon <- mm <- NULL; year <- yy <- NULL; seas_i <- seas <- "nope"; dat_ym <- dat <- datdd
plot_REC1_vs_conv1_ggplot(mode="reddy", dat_ym=dat_ym, mon=mm, year=yy, no_seg_nee = F)




# plot REC1 vs US-SEX (growing season)
seas <- seas_i <- "gro"
mon <- mm <- NULL; year <- yy <- NULL; dat_ym <- dat <- datdd
fg <- c("H_f_g1", "LE_f_g1", "NEE_uStar_f_g1", "H_f_gm", "LE_f_gm", "NEE_uStar_f_gm", "H_f_ga4", "LE_f_ga4", "NEE_uStar_f_ga4")
fs <- c("H_f_s1", "LE_f_s1", "NEE_uStar_f_s1", "H_f_sm", "LE_f_sm", "NEE_uStar_f_sm", "H_f_sa4", "LE_f_sa4", "NEE_uStar_f_sa4")
dat_ym[ dat_ym[,"g_seas_gm"]==F,  fg ] <- NA
dat_ym[ dat_ym[,"g_seas_sm"]==F,  fs ] <- NA
plot_REC1_vs_conv1_ggplot(mode="reddy", dat_ym=dat_ym, mon=mm, year=yy, no_seg_nee = F, seas=seas_i)





# plot REC1 vs US-SEX (senescent season)
seas <- seas_i <- "sen"
mon <- mm <- NULL; year <- yy <- NULL; dat_ym <- dat <- datdd
fg <- c("H_f_g1", "LE_f_g1", "NEE_uStar_f_g1", "H_f_gm", "LE_f_gm", "NEE_uStar_f_gm", "H_f_ga4", "LE_f_ga4", "NEE_uStar_f_ga4")
fs <- c("H_f_s1", "LE_f_s1", "NEE_uStar_f_s1", "H_f_sm", "LE_f_sm", "NEE_uStar_f_sm", "H_f_sa4", "LE_f_sa4", "NEE_uStar_f_sa4")
dat_ym[ dat_ym[,"g_seas_gm"]==T,  fg ] <- NA
dat_ym[ dat_ym[,"g_seas_sm"]==T,  fs ] <- NA
plot_REC1_vs_conv1_ggplot(mode="reddy", dat_ym=dat_ym, mon=mm, year=yy, no_seg_nee = F, seas=seas_i)




# plot avg_day AmeriFlux

fg <- c("Hc_g1",   "Hc_ga4",   "H_gm", "cLEc_g1", "cLEc_ga4", "cLE_gm", "Fcc_g1",  "Fcc_ga4",  "Fc_gm")
fs <- c("Hc_s1",   "Hc_sa4",   "H_sm", "cLEc_s1", "cLEc_sa4", "cLE_sm", "Fcc_s1",  "Fcc_sa4",  "Fc_sm")
dat_ym[ dat_ym[,"g_seas_gm"]==F,  fg ] <- NA
dat_ym[ dat_ym[,"g_seas_sm"]==F,  fs ] <- NA

avd_mn <- aggregate(dat_ym[,dcols],by=list(chron::hours(dat_ym[,"dt"])), mean, na.rm=T)
avd_sd <- aggregate(dat_ym[,dcols],by=list(chron::hours(dat_ym[,"dt"])), std.error, na.rm=T)
colnames(avd_mn) <- c("HOD", paste(colnames(avd_mn[-1]), "mn", sep="_"))
colnames(avd_sd) <- c("HOD", paste(colnames(avd_sd[-1]), "sde", sep="_"))
avd_m <- merge(avd_mn, avd_sd, by="HOD")

plot_avg_day_AmeriFlux(avd=avd_m, mon=mm, year=yy, ylims=ylims, add_P=F, pmd=pm, pavd=p_mn)






plot_NEE_months_10x3(dat=dat_ym, mon=mm, year=yy)



# plotting functions --------- operational area

if(F){
  ##### monthly plots - commnet (out) plots that you (do not) want to plot
  
  for(yy in unique(datdd[,"yyyy"])){
    for(mm in sort(unique(datdd[,"mon"]))){
      print(paste(yy, mm))
      dat_ym <- datdd[datdd[,"yyyy"]==yy & datdd[,"mon"]==mm,]
      if(nrow(dat_ym)>1){
        #plot_months_3_1(dat=dat_ym, mon=mm, year=yy, RH="cell", corrected=T, AmeriFlux=T, 
         #               AmeriFlux_edire=F, avg_rec4=T, avg_rec3=F, no_rec234=T, no_rec1=F, 
          #              no_seg_nee=F, add_wdir=F, add_ts=F, add_w=F)
        #plot_REC1_vs_conv1(mode="reddy", dat_ym=dat_ym, mon=mm, year=yy)
        ###   
        #gapfill_months_3_1_test(dat=dat_ym, mon=mm, year=yy)
        #gapfill_meteo_months_3_1_test(dat=dat_ym, mon=mm, year=yy)
        ###
        #plot_NEE_months_10x3(dat=dat_ym, mon=mm, year=yy)
        if(T){
          avd_mn <- aggregate(dat_ym[,dcols],by=list(chron::hours(dat_ym[,"dt"])), mean, na.rm=T)
          #avd_sd <- aggregate(dat_ym[,dcols],by=list(chron::hours(dat_ym[,"dt"])), std.error, na.rm=T)
          avd_sd <- aggregate(dat_ym[,dcols],by=list(chron::hours(dat_ym[,"dt"])), laplace_err)
          colnames(avd_mn) <- c("HOD", paste(colnames(avd_mn[-1]), "mn", sep="_"))
          colnames(avd_sd) <- c("HOD", paste(colnames(avd_sd[-1]), "sde", sep="_"))
          avd_m <- merge(avd_mn, avd_sd, by="HOD")
          
          #plot_avg_day(avd=avd_m, mon=mm, year=yy, ylims=ylims, test_2=T)
          #plot_avg_day_AmeriFlux(avd=avd_m, mon=mm, year=yy, ylims=ylims, add_P=F, 
           #                  pmd=pm, pavd=p_mn, irga_s=F, no_seg_nee = F)
          #plot_avg_day_AmeriFlux_HR(avd=avd_m, mon=mm, year=yy, ylims=ylims, add_P=F, 
           #                 pmd=pm, pavd=p_mn, irga_s=F, no_seg_nee = F)
        }
      }
    }
  }
}


 
 
## S2 Extra testing ================================

if(T){
  
  
  cols_seg <- paste(rep(c("H_f", "LE_f", "NEE_uStar_f"), each=7), 
                  rep(c("g1", "g2", "g3", "g4", "gm", "ga4", "ga3"), 3), sep="_")
  
  cols_ses <- paste(rep(c("H_f", "LE_f", "NEE_uStar_f"), each=7), 
                  rep(c("s1", "s2", "s3", "s4", "sm", "sa4", "sa3"), 3), sep="_")
  
  
  
  ffs <- c("H_f", "LE_f", "NEE_uStar_f")
  gs <- c("g1", "g2", "g3", "g4", "gm", "ga4", "ga3")
  ss <- c("s1", "s2", "s3", "s4", "sm", "sa4", "sa3")
  
  
  
  
  
  for(i in 1:7){  # loop over tower/mean fluxes
  
    bplot <- "E:/REC_7_Data/10_Plots/laplace_test/"
    filenm <- paste(bplot, "laplace_test_group_", i, ".png", sep="")
    
    png(filenm, width=600, height=400)  # was 600
    par(mfrow = c(3, 2), mar = c(3, 3, 3, 1), mgp=c(2, 0.3, 0), tck=-0.01)
    
    ids <- paste( rep(ffs, each=2), rep( c(gs[i], ss[i]), 3), sep="_")
    
    for(j in 1:6){   # loop over H/LE/NEE on both sites
    
      
      
      vec <- datdd[, ids[j]]
      

      # plot (breaks = # of bins)
      hs <- hist(vec, breaks=30, main=ids[j], xlab="residuals")
      
      xs <- (min(hs$mids)+0.5):(max(hs$mids)-0.5)
      if(j %in% c(5,6))xs <- seq(-5,5,0.1)
      
      # derive sd for normal and b for laplace distribution
      sdev <- sd(vec, na.rm=T)
      sdla <- mean(abs(vec - median(vec, na.rm=T)), na.rm=T)
      
      
      # points for curves 
      gaus <- 1/(sdev*sqrt(2*pi)) * 2.71^-((xs-mean(vec, na.rm=T))/sdev)^2
      lapl <- 1/(2*sdla) * 2.71^(-abs(xs-median(vec, na.rm=T))/sdla )  
      
      # normalixe points
      gys <- gaus * max(hs$counts)/max(gaus)
      lys <- lapl * max(hs$counts)/max(lapl)
      
      lines(xs, gys)
      lines(xs, lys, lty=3)
      
      mtext(paste("sd G:", round(sdev, 1)),             1, line=-3, adj=0.95, cex=1)
      mtext(paste("sd L:", round(sqrt(2) * sdla, 1)),   1, line=-2, adj=0.95, cex=1)
      
      if(j==1)legend("topright", c("Gauss", "Laplace"), lty=c(1, 3))
    }
    
    dev.off()
  
  }
  
  
  
}  # normal vs laplace distr. test (from Hollinger 2005)
#
if(T){
  
  # 1) run section 3.0 normally
  # 2) run section 3.0 with correct last_use from telemetry. It will stop at ir==15
  # 3) run other sections
  # 4) source the following function and run it in the monthly loop
  
  
  gapfill_months_3_1_test <- function(dat=dat, mon=mm, year=yy){
    
    
    #cols <- c("orange", "red", "violet", "purple",  "green", "darkgreen", "cyan")
    cols <- c( rep(c("orange", "blue"), 3), "cyan")
    
    tws <- c("SEG_REC1", "SEG_REC2", "SEG_REC3", "SEG_REC4", "SEG",
           "SES_REC1", "SES_REC2", "SES_REC3", "SES_REC4", "SES")
    
    dts <- c("g1", "g2", "g3", "g4", "gm", "s1", "s2", "s3", "s4", "sm")
    pis <- c("gm", "gm", "gm", "gm", "gm", "sm", "sm", "sm", "sm", "sm")
    
    gpath <- "E:/REC_7_Data/10_Plots/18_gapfilling_test/fluxes/"
    
    xlim <- range(dat[,"dt"])
    
    
    
    for(p in 1:10){
      
      png(paste(gpath,  dts[p], "_",  "gapfill_test_", year, "_", substring(1000+mon, 3,4),
                ".png", sep=""), width=2500, height=1800)
      par(mfrow=c(3,1), mar = c(0, 0, 0, 0), oma = c(22, 14, 10, 12), mgp=c(2, 2, 0))
      
      
      title <- paste(tws[p], "-", year, "/" ,mon, sep=" ")
      pi <- paste("P_hh", pis[p], sep="_")
      
      
      # Sensible heat
      hcs <- paste(c("H_f", "H_orig"), dts[p], sep="_"); 
      #if(p %in% c(5, 10)) hcs <- paste(c("H_f", "H_orig"), dts[p], sep="_"); 
      ylim <- range(dat[, hcs], na.rm=T)*c(1, 1.25)
      plot( dat[, hcs[1]]~dat[,"dt"], type='l', xlim=xlim, ylim=ylim, axes=F, col=cols[1], lwd=3) 
      lines(dat[,"dt"], dat[, hcs[2]], col=cols[2], lwd=3)
      axis(2, cex.axis=4); box(); mtext(title, side=3, line=2, cex=5); 
      mtext(paste("H (W m-2)",sep=""), side=2, line=8, cex=4)
      mtext ("a)", side=3, adj=0.01, line=-5, cex=3)
      legend("topright", c("MDS gapfill", "unfilled", "Precip"), ncol=2, col=cols[c(1, 2, 7)], pch=16, cex=4)
      abline(h=0, col="grey")
      
      
      
      # Latent heat
      lecs <- paste(c("LE_f", "LE_orig"), dts[p], sep="_"); 
      #if(p %in% c(5, 10)) lecs <- paste(c("LE_f", "LE_orig"), dts[p], sep="_");
      ylim <- range(dat[, lecs], na.rm=T)
      plot( dat[, lecs[1]]~dat[,"dt"], type='l', xlim=xlim, ylim=ylim, axes=F, col=cols[3], lwd=3) 
      lines(dat[,"dt"], dat[, lecs[2]], col=cols[4], lwd=3)
      axis(2, cex.axis=4); box(); #mtext(paste("LE (W m-2)",sep=""), side=2, line=8, cex=4)
      mtext(paste(fluxes[2]," (W m-2)",sep=""), side=2, line=8, cex=4)
      mtext ("b)", side=3, adj=0.01, line=-5, cex=3)
      #legend("topright", c("unfill", "MDS"), ncol=2, col=cols[c(3, 4)], pch=16, cex=4)
      abline(h=0, col="grey")
      if(sum(is.na(dat[,pi]))<nrow(dat)){ 
        par(new=T)
        plot(dat[,pi]~dat[,"dt"], xlim=xlim, col='cyan', type="l", axes=F, xlab="", ylab="")
        axis(4, cex.axis=4); mtext("Prec (mm / h)", side=4, line=9, cex=4)
      }
      
      
      # SEG - NEE
      neecs <- paste(c("NEE_uStar_f", "NEE_uStar_orig"), dts[p], sep="_"); 
      #if(p %in% c(5, 10)) neecs <- paste(c("NEE_uStar_f", "Fc"), dts[p], sep="_");
      ylim <- range(dat[, neecs], na.rm=T)
      plot( dat[, neecs[1]]~dat[,"dt"], type='l', xlim=xlim, ylim=ylim, axes=F, col=cols[5], lwd=3) 
      lines(dat[,"dt"], dat[, neecs[2]], col=cols[6], lwd=3)
      axis(2, cex.axis=4); box(); mtext("Time", side=1, line=20, cex=4)
      mtext("NEE (umol m-2 s-1)", side=2, line=8, cex=4);
      mtext ("c)", side=3, adj=0.01, line=-5, cex=3)
      #legend("topright", c("unfill", "MDS"), ncol=2, col=cols[c(5, 6)], pch=16, cex=4)
      abline(h=0, col="grey")
      axis.Date(1, at=seq(min(dat[,"dt"]), max(dat[,"dt"]), by="week"), cex.axis=4, las=3); 
      
      
      
      
      dev.off()
      
      
    }  # end loop to make figures
    
    
    
    
  }
  
  
  
  
  gapfill_meteo_months_3_1_test <- function(dat=dat, mon=mm, year=yy){
    
    
    cols <- c("orange", "violet", "purple", "cyan", "blue", "darkgreen")
    hc0 <- c( "SW_IN",  "TA_F",   "RH_F",   "P_hh", "VPD_f",  "USTAR")
    #cols <- c( rep(c("orange", "blue"), 3), "cyan")
    
    tws <- c("SEG", "SES")
    
    #dts <- c("g1", "g2", "g3", "g4", "gm", "s1", "s2", "s3", "s4", "sm")
    pis <- dts <- c("gm", "sm")
    
    gpath <- "E:/REC_7_Data/10_Plots/18_gapfilling_test/meteo/"
    
    xlim <- range(dat[,"dt"])
    
    
    
    for(p in 1:2){
      
      png(paste(gpath,  "meteo_", dts[p], "_",  "gapfill_test_", year, "_", substring(1000+mon, 3,4),
                ".png", sep=""), width=2500, height=1800)
      par(mfrow=c(3,1), mar = c(0, 0, 0, 0), oma = c(22, 14, 10, 12), mgp=c(2, 2, 0))
      
      
      title <- paste(tws[p], "-", year, "/" ,mon, sep=" ")
      #pi <- paste("P_hh", pis[p], sep="_")
      
      hcs <- paste(hc0, dts[p], sep="_"); 
      
      
      # Sensible heat
      ylim <- range(dat[, hcs[1]], na.rm=T)*c(1, 1.25)
      plot( dat[, hcs[1]]~dat[,"dt"], type='l', xlim=xlim, ylim=ylim, axes=F, col=cols[1], lwd=1) 
      axis(2, cex.axis=4); box(); mtext(title, side=3, line=2, cex=5); 
      mtext(paste("LW IN (W m-2)",sep=""), side=2, line=8, cex=4)
      mtext ("a)", side=3, adj=0.01, line=-5, cex=3)
      legend("topright", c("Global Rad", "Air Temp"), ncol=2, col=cols[c(1, 2)], pch=16, cex=4)
      abline(h=0, col="grey")
      par(new=T)
      ylim <- range(dat[, hcs[2]], na.rm=T)*c(1, 1.25)
      plot(dat[,hcs[2]]~dat[,"dt"], xlim=xlim, col=cols[2], type="l", axes=F, xlab="", ylab="", ylim=ylim)
      axis(4, cex.axis=4); mtext("Air T (C)", side=4, line=9, cex=4)
      
      
      
      # Latent heat
      ylim <- range(dat[, hcs[3]], na.rm=T)*c(1, 1.25)
      plot( dat[, hcs[3]]~dat[,"dt"], type='l', xlim=xlim, ylim=ylim, axes=F, col=cols[3], lwd=1) 
      axis(2, cex.axis=4); box(); mtext(paste("RH (%)",sep=""), side=2, line=8, cex=4)
      mtext ("b)", side=3, adj=0.01, line=-5, cex=3)
      legend("topright", c("RH", "Precip"), ncol=2, col=cols[c(3, 4)], pch=16, cex=4)
      abline(h=0, col="grey")
      if(sum(is.na(dat[,"P_gm"]))<nrow(dat)){ 
        par(new=T)
        plot(dat[,hcs[4]]~dat[,"dt"], xlim=xlim, col='cyan', type="l", axes=F, xlab="", ylab="")
        axis(4, cex.axis=4); mtext("Prec (mm / h)", side=4, line=9, cex=4)
      }
      
      
      # SEG - NEE
      ylim <- range(dat[, hcs[5]], na.rm=T)*c(1, 1.25)
      plot( dat[, hcs[5]]~dat[,"dt"], type='l', xlim=xlim, ylim=ylim, axes=F, col=cols[5], lwd=1) 
      axis(2, cex.axis=4); box(); mtext("Time", side=1, line=20, cex=4)
      mtext("VPD (hPa)", side=2, line=8, cex=4);
      mtext ("c)", side=3, adj=0.01, line=-5, cex=3)
      legend("topright", c("VPD", "u*"), ncol=2, col=cols[c(5, 6)], pch=16, cex=4)
      abline(h=0, col="grey")
      axis.Date(1, at=seq(min(dat[,"dt"]), max(dat[,"dt"]), by="week"), cex.axis=4, las=3); 
      par(new=T)
      ylim <- range(dat[, hcs[6]], na.rm=T)*c(1, 1.25)
      plot(dat[,hcs[6]]~dat[,"dt"], xlim=xlim, col=cols[6], type="l", axes=F, xlab="", ylab="", ylim=ylim)
      axis(4, cex.axis=4); mtext("u* (m s-1)", side=4, line=9, cex=4)
      
      
      
      
      dev.off()
      
      
    }  # end loop to make figures
    
    
    
    
  }
  
  
  
  
  
  
}  # test MDS gapfilling
#
if(T){
  # 2x sites
  # 4x RECx-EC vs WS_RECx
  # what fluxes? LE and NEE
  
  
  day_only <- F
  night_only <- F    # MUST be different from day_only !!!!
  pre_prec <- F
  post_prec <- F     # MUST be different from post_prec !!!!
  

  attr <- ""
  les <- paste("LE_f", adatasets[1:10], sep="_")
  nees <- paste("NEE_uStar_f", adatasets[1:10], sep="_")
  
  
  # define residuals matrix
  
  
  datp <- matrix(NA, nrow=nrow(datdd), ncol=16)
  colnames(datp) <- c(les[1:8], nees[1:8])
  #colnames(rmat) <- paste("res", rep(c("le", "nee"), each=8), rep(datasets, 2), sep="_")
  
  
  # fill residuals matrix
  
  
  for(i in 1:8){
    
    ic <- 9; if(i %in% c(5:8))ic <- 10
    datp[,i]   <- datdd[,les[ic]]  - datdd[,les[i]]
    datp[,i+8] <- datdd[,nees[ic]] - datdd[,nees[i]]
    
  }
  
  # add wind speed
  datp <- cbind(datp, datdd[, paste("mean_Wind_Spd", datasets, sep="_")])  # now a data.frame
  
  
  
  gcs <- c(les[grepl("_g", les)], nees[grepl("_g", nees)])
  scs <- c(les[grepl("_s", les)], nees[grepl("_s", nees)])
  
  # filter day/night (gm/sm specific filter for H)
  selh_gm <- datdd[,"H_gm"]>0;    selh_gm[is.na(selh_gm)] <- T     # data.frame does not allow NAs in sel
  selh_sm <- datdd[,"H_sm"]>0;    selh_sm[is.na(selh_sm)] <- T
  
  if(day_only){  datp[!selh_gm, gcs] <- NA; datp[!selh_sm, scs] <- NA; attr <- paste(attr, "_day_only", sep="")}
  if(night_only){datp[selh_gm, gcs] <- NA;  datp[selh_sm, scs] <- NA;  attr <- paste(attr, "_night_only", sep="")}
  
  
  

  #### filter before/after precipitation
  
  selp_gm <- selp_sm <- rep(F, nrow(datp))
  selp_gm[unique( unlist(   lapply( which( datdd[,"P_hh_gm"]>0 ) , day3_fun)    ) )] <- T
  selp_sm[unique( unlist(   lapply( which( datdd[,"P_hh_sm"]>0 ) , day3_fun)    ) )] <- T
  
  if(pre_prec){  datp[!selp_gm, gcs] <- NA; datp[!selp_sm, scs] <- NA; attr <- paste(attr, "_pre_prec", sep="")}
  if(post_prec){ datp[selp_gm, gcs] <- NA;  datp[selp_sm, scs] <- NA;  attr <- paste(attr, "_post_prec", sep="")}
  
  
  
  #xlim <- range(datp[, grepl("Wind", colnames(datp))])
  xlim <- c(0, 17)
  ylim_l <- c(-200, 300)
  ylim_c <- c(-10, 13)
  #ylim_l <- range(datp[, 1:8], na.rm=T)
  #ylim_c <- range(datp[, 9:16], na.rm=T)
  
  fls <- rep(c("LE", "NEE"), each=8)
  dts <- rep(datasets, 2)
  
  gpath <- "E:/REC_7_Data/10_Plots/19_REC_EC_residuals_vs_WS/"; plot_nm <- "REC_EC_res_vs_WSpeed"
  png(paste(gpath, plot_nm, attr, "_test.png", sep=""), width=1200, height=1200)
  
  par(mfrow=c(4, 4), mar = c(2, 2, 2, 2), oma = c(6, 6, 2, 2), mgp=c(3, 1.5, 0), tck=0.01)  
  
  for(i in 1:16){
    iw <- i+16; ylim <- ylim_l
    if(i>8){iw <- i+8; ylim <- ylim_c}
    plot(datp[, iw], datp[, i], type="p", pch=16, xlab="", ylab="", 
         xlim=xlim, ylim=ylim, cex.axis=3)
    mtext(  paste(fls[i], dts[i]), 3, line=-3, cex=2)
  }
  mtext("RECi wind speed (m s-1)", 1, line=3, cex=3, outer=TRUE)
  mtext("RECi-EC  (W m-2)", 2, line=3, cex=3, outer=TRUE, adj=0.9)
  mtext("RECi-EC  (umol m-2 s-1)", 2, line=3, cex=3, outer=TRUE, adj=0.2)
  
  dev.off()
  
  
  
  
  
}  # REC-EC residuals vs WSpeed





## S3 Extra plots ================================

if(T){
  
  # MAT and TAP
  
  sum(gm[,"P_F_gm"], na.rm=T)    # 516.874
  sum(sm[,"P_F_sm"], na.rm=T)    # 457.656
  
  mean(gm[,"TA_F_gm"], na.rm=T)   # 13.7384
  mean(sm[,"TA_F_sm"], na.rm=T)   # 14.35934
  
  range(datdd[,"TA_F_gm"], na.rm=T)   # -15.42842  35.04188
  range(datdd[,"TA_F_sm"], na.rm=T)   # -15.42842  35.15790
  
  quantile(datdd[,"TA_F_gm"], probs=seq(0,1,0.1), na.rm=T)
  quantile(datdd[,"TA_F_sm"], probs=seq(0,1,0.1), na.rm=T)
  
}  # MAT and TAP
#
if(T){
  
  # daily land cover probabilities
  
  for(k in 1:12){   # loops over months
    #k <- 12
    mon_ys <- unique(datfoot0[,c("yyyy", "mon")])
    load(file = paste("E:/REC_7_Data/9_R/Rdata/", "pmat_80_", 
                      mon_ys[k,"yyyy"], "_", mon_ys[k,"mon"], ".Rdata", sep=""))  # this load 'pmat_80'
    tlab0 <- paste(mon_ys[k,"yyyy"], "_", mon_ys[k,"mon"], sep="")
    
    
    
    for(di in unique(pmat_80[,"dd"])){      # loops over days
      
      tlab1 <- paste(mon_ys[k,"yyyy"], mon_ys[k,"mon"], di, sep="_")
      pmatd <- pmat_80[ pmat_80[,"dd"]==di, ]
      if(sum(pmat_80[,"dd"]==di)==0)next
      if(sum(pmat_80[,"dd"]==di)==1)pmatd <- t(as.matrix(pmatd, ncol=65, nrow=1))
      pmatd[  pmatd[,"min"]==45 ,"hh"] <- pmatd[ pmatd[,"min"]==45 ,"hh"]+0.5
      
      
      
      
      for(cluster in sites){     # loops over different sites
        
        tws <- c("SEG", "gm", "g1", "g2", "g3", "g4"); 
        leg <- c("SEG cluster", "US-SEG", "SEG1", "SEG2", "SEG3", "SEG4"); 
        if(cluster==sites[2]){tws <- c("SES", "sm", "s1", "s2", "s3", "s4");
        leg <- c("SES cluster", "US-SES", "SES1", "SES2", "SES3", "SES4")}
        
        ppath <- paste("E:/REC_7_Data/10_Plots/footprints/hourly_cluster/", cluster, "/", 
                     tlab0,  "_prob_plot/", sep="")
        filenm <- paste(ppath, cluster, "_80_prob_plot_", tlab1, "_test.png", sep="")
        
        png(filenm, width=900, height=600)
        par(mfrow=c(2,3), mar = c(0, 0, 0, 0), oma = c(6, 6, 4, 0.5), xpd=NA)
        
        
        ## make daily plots (48 points)
        
        for(i in 2:6){
          
          plot(seq(0, 23.5, 0.5), rep(1,48), type="n", ylim=c(0,1), axes=F, xlab="", ylab=""); box()
          if(i==2)mtext(tlab1,line=1, cex=2)
          if(i %in% c(2,5)){mtext("Probability", 2, line=3.5); axis(2, cex.axis=1.5)}
          if(i %in% 4:6){mtext("Hour of Day", 1, line=3.5); axis(1, at=seq(0, 24, 3), 
                                                                 labels=seq(0, 24, 3), las=1, cex.axis=1.5)}
          mtext(paste(leg[i]), line=-2, cex=1.5, adj=0.05)
          
            lines(pmatd[,"hh"], pmatd[, paste("pt", tws[i], sep="_")], lwd=2)
            lines(pmatd[,"hh"], pmatd[, paste("p1", tws[i], sep="_")], lwd=2, col="moccasin")
            lines(pmatd[,"hh"], pmatd[, paste("p2", tws[i], sep="_")], lwd=2, col="darkgreen")
            lines(pmatd[,"hh"], pmatd[, paste("p3", tws[i], sep="_")], lwd=2, col="olivedrab3")
            #lines(pmatd[,"hh"], pmatd[, paste("ps", tws[i], sep="_")], col="red", lty=3)
          
          if(i==6)legend(26, 0.8, c("P total", "P1 - bare", "P2 - shrubs", "P3 - herbaceous"), 
                         col=c("black", "moccasin", "darkgreen", "olivedrab3"), lty=1, lwd=2, cex=2)
          
        }
        
        
        dev.off()
        
        
      }
      
    }
  }
  
  
}  # daily land cover probabilities
#
if(T){
  
  # growing / senescent periods fluxes matrix / barchart
  
  reddy <- T
  hollinger <- T  # if reddy & hollinger
  ECs2 <- T       # use mean of 2 towers
  avg_all <- T    # no seasonality; uses laplace std.err
  no_seg_nee <- F
  
  
  attr <- ""; fid <- "Fc"; lb <- ""; wr <- 2:1; cols <- c("darkgreen", "orange")
  sp <- NULL
  
  
  cols_seg <- paste(rep(c("Hc", "cLEc", "Fcc"), each=7), 
                  rep(c("g1", "g2", "g3", "g4", "gm", "ga4", "ga3"), 3), sep="_")
  cols_seg[grep("gm", cols_seg)] <- c("H_gm", "cLE_gm", "Fc_gm")
  
  
  cols_ses <- paste(rep(c("Hc", "cLEc", "Fcc"), each=7), 
                  rep(c("s1", "s2", "s3", "s4", "sm", "sa4", "sa3"), 3), sep="_")
  cols_ses[grep("sm", cols_ses)] <- c("H_sm", "cLE_sm", "Fc_sm")      
  
  
  
  if(reddy){
    if(!ECs2){
      cols_seg <- paste(rep(c("H_f", "LE_f", "NEE_uStar_f"), each=7), 
                      rep(c("g1", "g2", "g3", "g4", "gm", "ga4", "ga3"), 3), sep="_")
      cols_ses <- paste(rep(c("H_f", "LE_f", "NEE_uStar_f"), each=7), 
                      rep(c("s1", "s2", "s3", "s4", "sm", "sa4", "sa3"), 3), sep="_")
    }else{
      cols_seg <- paste(rep(c("H_f", "LE_f", "NEE_uStar_f"), each=10), 
                      rep(c("g1", "g2", "g3", "g4", "gm", "ga4", "ga3", "ga23", "ga34", "ga42"), 3), sep="_")
      cols_ses <- paste(rep(c("H_f", "LE_f", "NEE_uStar_f"), each=10), 
                      rep(c("s1", "s2", "s3", "s4", "sm", "sa4", "sa3", "sa23", "sa34", "sa42"), 3), sep="_")
    }
    attr <- "_reddy"
    fid <- "NEE"
  }
  
  ll <- length(cols_seg)/3
  
  
  # aggregate everything 
  seg_af <- aggregate(datdd[,cols_seg],by=list(datdd[,"g_seas_gm"]), mean, na.rm=T)   
  ses_af <- aggregate(datdd[,cols_ses],by=list(datdd[,"g_seas_sm"]), mean, na.rm=T)
  
  seg_se0 <- aggregate(datdd[,cols_seg],by=list(datdd[,"g_seas_gm"]), std.error, na.rm=T)   
  ses_se0 <- aggregate(datdd[,cols_ses],by=list(datdd[,"g_seas_sm"]), std.error, na.rm=T)
  
  
  if(hollinger){
    seg_se <- aggregate( abs( datdd[,paste("res", cols_seg, sep="_")]), by=list(datdd[,"g_seas_gm"]), laplace_err)   
    ses_se <- aggregate( abs( datdd[,paste("res", cols_ses, sep="_")]), by=list(datdd[,"g_seas_sm"]), laplace_err)
    lb <- "_hollinger"
  } else {
    seg_se <- seg_se0
    ses_se <- ses_se0 
  }
  
  
  
  if(avg_all){
    if(!("Group.1" %in% colnames(datdd)))Group.1 <- 1; datdd$Group.1 <- Group.1
    
    seg_af <- aggregate(datdd[,cols_seg],by=list(datdd[,"Group.1"]), mean, na.rm=T)   
    ses_af <- aggregate(datdd[,cols_ses],by=list(datdd[,"Group.1"]), mean, na.rm=T)
    
    seg_se <- aggregate(  datdd[,paste("res", cols_seg, sep="_")], by=list(datdd[,"Group.1"]), laplace_err)   
    ses_se <- aggregate(  datdd[,paste("res", cols_ses, sep="_")], by=list(datdd[,"Group.1"]), laplace_err)
     
    lb <- "_avg_all"; wr <- 1; cols <- "darkgreen"; sp <- 0.6
  }
  
  
  
  ### barplot needs vector or matrix 
  bard <- merge(seg_af, ses_af, by="Group.1"); bard <- as.matrix(bard[,2:ncol(bard)])
  bare <- merge(seg_se, ses_se, by="Group.1"); bare <- as.matrix(bare[,2:ncol(bare)])
  
  # reorder to Means, AmeriFlux, RECs
  bard <- bard[,    ll*rep(0:5, each=ll) + rep(c(6:ll, 5, 1:4), 6)   ]
  bare <- bare[,    ll*rep(0:5, each=ll) + rep(c(6:ll, 5, 1:4), 6)   ]
  
  
  if(avg_all){
    bard <- as.matrix(bard, nrow=1); bard <- t(bard)
    bare <- as.matrix(bare, nrow=1); bare <- t(bare)
  }
  
  if(no_seg_nee){
    bard[, 3*ll-4] <- 0
    bare[, 3*ll-4] <- 0
  }
  
  
  
  colh   <- grep("H",  colnames(bard))
  colle  <- grep("LE", colnames(bard))
  colnee <- grep(fid, colnames(bard))      # Fc or NEE
  
  ylimh   <-  c(0, max( bard[, colh]   + bare[,colh],  na.rm=T)  * 1.15)
  ylimle  <-  c(0, max( bard[, colle]  + bare[,colle], na.rm=T)  * 1.15)
  ylimnee <-  c(min( bard[, colnee] - bare[,colnee], na.rm=T) * 1.05, 
              max( bard[, colnee] + bare[,colnee], na.rm=T) * 1.6)  # was 1.15
  
  
  x1 <- 6.5; x2 <- 9.5; x3 <- 27.5; x4 <- 30.5
  if(ECs2){x1 <- 15.5; x2 <- 18.5; x3 <- 45.5; x4 <- 48.5}
  if(avg_all){x1 <- 8.3; x2 <- 9.9; x3 <- 24.3; x4 <- 25.9}
  
## plotting ########### 
  
  
  
  bplot <- "E:/REC_7_Data/10_Plots/12_barplot_fluxes/"
  filenm <- paste(bplot, "3_1_barplot", xch, attr, lb, "_EC0_HR.png", sep="")
  
  png(filenm, width=1800, height=1800)
  par(mfrow=c(3,1), mar = c(0, 12, 0, 0), oma = c(26, 12, 16, 1), mgp=c(2, 0.5, 0), tck=-0.01)
  if(ECs2)par(oma=c(34, 5, 12, 1))
  
  
  bp_h <- barplot(bard[wr, colh], las=3, beside=T, axisnames=F, ylim=ylimh, cex.axis=6, 
                  col=cols, space=sp); box(); 
  mtext(expression("H (W m"^"-2"*")"), 2, line=7, cex=4)
  rect(x1, 0, x2, ylimh[2], col = "grey")
  rect(x3, 0, x4, ylimh[2], col = "grey")
  bp_h <- barplot(bard[wr, colh], las=3, beside=T, axisnames=F, ylim=ylimh,
                  cex.axis=6, col=cols, add=T, space=sp)
  arrows(x0=bp_h, y0=bard[wr, colh]+bare[wr, colh], x1=bp_h, y1=bard[wr, colh]-bare[wr, colh], 
         length=0, code=3, lwd=3); abline(v=mean(bp_h), lty=3); #abline(v=21.5, lty=3)
  mtext ("Grassland", side=3, adj=0.15, line=2, cex=8)
  mtext ("Shrubland", side=3, adj=0.85, line=2, cex=8)
  mtext ("a)", side=3, adj=0.01, line=-6, cex=4)
  
  
  barplot(bard[wr, colle],  las=3, beside=T, axisnames=F, ylim=ylimle, cex.axis=6, col=cols, space=sp); box(); 
  mtext(expression("LE (W m"^"-2"*")"), 2, line=7, cex=4)
  rect(x1, 0, x2, ylimle[2], col = "grey")
  rect(x3, 0, x4, ylimle[2], col = "grey")
  barplot(bard[wr, colle],  las=3, beside=T, axisnames=F, ylim=ylimle,
          cex.axis=6, col=cols, add=T, space=sp)
  arrows(x0=bp_h, y0=bard[wr, colle]+bare[wr, colle], x1=bp_h, y1=bard[wr, colle]-bare[wr, colle], 
         length=0, code=3, lwd=3); abline(v=mean(bp_h), lty=3); #abline(v=21.5, lty=3)
  mtext ("b)", side=3, adj=0.01, line=-6, cex=4)
  
  
  if(!ECs2){
    barplot(bard[wr, colnee], las=3, beside=T, ylim=ylimnee, cex.names=6, cex.axis=6, col=cols, space=sp,
            names.arg = c("Seg EC1234", "Seg EC234", "Seg EC0", "Seg EC1", "Seg EC2", "Seg EC3", "Seg EC4", 
                          "Ses EC1234", "Ses EC234", "Ses EC0", "Ses EC1", "Ses EC2", "Ses EC3", "Ses EC4"));
  } else {
    barplot(bard[wr, colnee], las=3, beside=T, ylim=ylimnee, cex.names=6, cex.axis=6, col=cols, space=sp,
            names.arg = c("Seg EC1234", "Seg EC234", "Seg EC23", "Seg EC34", "Seg EC42", "Seg EC0", "Seg EC1", "Seg EC2", "Seg EC3", "Seg EC4", 
                          "Ses EC1234", "Ses EC234", "Ses EC23", "Ses EC34", "Ses EC42", "Ses EC0", "Ses EC1", "Ses EC2", "Ses EC3", "Ses EC4"));
  }
  rect(x1, ylimnee[1], x2, ylimnee[2], col = "grey")
  rect(x3, ylimnee[1], x4, ylimnee[2], col = "grey")
  barplot(bard[wr, colnee], las=3, beside=T, ylim=ylimnee, cex.names=6, cex.axis=6, col=cols, add=T, names.arg =rep("", 20), space=sp) # was 14
  arrows(x0=bp_h, y0=bard[wr, colnee]+bare[wr, colnee], x1=bp_h, y1=bard[wr, colnee]-bare[wr, colnee], 
         length=0, code=3, lwd=3); box(); abline(v=mean(bp_h), lty=3); #abline(v=21.5, lty=3); 
  
  mtext(expression("NEE (umol CO"["2"]*" m"^"-2"*"s"^"-1"*")"), 2, line=7, cex=4)
  abline(h=0, col="black")
  mtext ("c)", side=3, adj=0.01, line=-6, cex=4)
  if(!avg_all)legend("bottomright", legend=c("Growing periods", "Dormant periods"), col=cols, pch=16, cex=5)
  
  # add horizontal error bars for mean fluxes
  arrows(x0=bp_h[c(1:5, 11:15)], 
         y0=bard[wr, colnee][c(1:5, 11:15)]+bare[wr, colnee][c(1:5, 11:15)], 
         x1=bp_h[c(1:5, 11:15)],
         y1=bard[wr, colnee][c(1:5, 11:15)]-bare[wr, colnee][c(1:5, 11:15)], 
         length=0.1, angle=90, code=3, lwd=3)
  
  dev.off()
  
  
  
  

  
  
  
  
}  # growing / senescent periods fluxes matrix / barchart
#
if(T){
  
  ### cumulative sum of LE and NEE
  
  cols <- rep(c("purple", "green", "darkgreen", "brown"), 2)
  add_AmeriFlux <- T
  add_AmeriFlux_edire <- F
  add_AmeriFlux_edire_no_WPL <- F
  add_prec <- T
  use_reddy <- T
  no_seg_nee <- F
  
  
  day_only <- F
  night_only <- F    # MUST be different from day_only !!!!
  pre_prec <- F
  post_prec <- F     # MUST be different from post_prec !!!!
  
  
  
  plot_nm <- last_date; tp1 <- 1:4; tp2 <- 5:8; attr <- ""
  les <- paste("cLEc", datasets, sep="_")
  nees <- paste("Fcc", datasets, sep="_")
  
  legend_g <- c("EC1", "EC2", "EC3", "EC4")
  # u* filtered (NEE only) and gapfilled
  
  
  
  if(add_AmeriFlux){
    cols <- c(cols, "orange", "orange")
    les <- c(les, "cLE_gm", "cLE_sm")
    nees <- c(nees, "Fc_gm", "Fc_sm")
    legend_g <- c(legend_g, "EC0")
    plot_nm <- paste(plot_nm, "_plusm", sep=""); tp1 <- c(tp1, 9); tp2 <- c(tp2, 10)
  }
  
  if(add_prec){    plot_nm <- paste(plot_nm, "_prec", sep="")  }
  
  if(use_reddy){
    les <- paste("LE_f", adatasets[1:10], sep="_")
    nees <- paste("NEE_uStar_f", adatasets[1:10], sep="_")
    attr <- "_reddy"
  }
  
  if(add_AmeriFlux_edire){
    cols <- c(cols, "red", "red")
    les <- c(les, "LEcw_gp", "LEcw_sp")
    nees <- c(nees, "Fccw_gp", "Fccw_sp")
    legend_g <- c(legend_g, "US-SEG EdiRe")
    #legend_s <- c(legend_s, "US-SES EdiRe")
    plot_nm <- paste(plot_nm, "_m_edire", sep=""); tp1 <- c(tp1, 11); tp2 <- c(tp2, 12)
    attr <- "_m_edire"
  }
  
  
  if(add_AmeriFlux_edire_no_WPL){
    cols <- c(cols, "blue4", "blue4")
    les <- c(les, "LEc_gp", "LEc_sp")
    nees <- c(nees, "Fcc_gp", "Fcc_sp")
    legend_g <- c(legend_g, "SEG Edi no WPL")
    #legend_s <- c(legend_s, "SES Edi no WPL")
    plot_nm <- paste(plot_nm, "_no_WPL", sep=""); tp1 <- c(tp1, 13); tp2 <- c(tp2, 14)
    attr <- "_m_edire_no_WPL"
  }
  
  
  
  
  datp <- datdd   # temp datasets
  
  gcs <- c(les[grepl("_g", les)], nees[grepl("_g", nees)])
  scs <- c(les[grepl("_s", les)], nees[grepl("_s", nees)])
  
  # filter day/night (gm/sm specific filter for H)
  selh_gm <- datp[,"H_gm"]>0;    selh_gm[is.na(selh_gm)] <- T     # data.frame does not allow NAs in sel
  selh_sm <- datp[,"H_sm"]>0;    selh_sm[is.na(selh_sm)] <- T

  if(day_only){  datp[!selh_gm, gcs] <- NA; datp[!selh_sm, scs] <- NA; attr <- paste(attr, "_day_only", sep="")}
  if(night_only){datp[selh_gm, gcs] <- NA;  datp[selh_sm, scs] <- NA;  attr <- paste(attr, "_night_only", sep="")}
  
  
  
  
  
  #### filter before/after precipitation

  selp_gm <- selp_sm <- rep(F, nrow(datp))
  selp_gm[unique( unlist(   lapply( which( datp[,"P_hh_gm"]>0 ) , day3_fun)    ) )] <- T
  selp_sm[unique( unlist(   lapply( which( datp[,"P_hh_sm"]>0 ) , day3_fun)    ) )] <- T
  
  if(pre_prec){  datp[!selp_gm, gcs] <- NA; datp[!selp_sm, scs] <- NA; attr <- paste(attr, "_pre_prec", sep="")}
  if(post_prec){ datp[selp_gm, gcs] <- NA;  datp[selp_sm, scs] <- NA;  attr <- paste(attr, "_post_prec", sep="")}
  
  
  
  
  ## cumsum
  dmat <- datp[, c(nees, les)]; dmat[is.na(dmat)] <- 0; 
  dmat$dt <- datp[,"dt"]; dmat$P_gm <- datp[,"P_hh_gm"]; dmat$P_sm <- datp[,"P_hh_sm"]
  dmat <- dmat[dmat[,"dt"]>="20/10/18",]
  for(i in 1:(ncol(dmat)-3)){dmat[,i] <- cumsum(dmat[,i])}

  
  
  ## convert umol m-2 s-1 to gC m-2 30min-1
  ccm <- grep("Fc", colnames(dmat))
  if(use_reddy){ccm <- grep("NEE", colnames(dmat))}
  if(add_AmeriFlux_edire)ccm <- c(ccm, grep("Fc", colnames(dmat)))
  
  dmat[,ccm] <- dmat[ccm]*  (12 / 10^6) * 1800     # 12 g C/mole * 1 gram /10^6 ugrams * time (1800 s)
  
  ## convert W m-2 into MW m-2 (30 min sum)
  lcm <- grep("LE", colnames(dmat))
  dmat[,lcm] <- dmat[lcm]*  (1 / 10^6) * 1800

  
  # exclude NEE of US-SEG
  if(no_seg_nee){ 
    ec <- grepl("_gm", colnames(dmat)) & 
       (grepl("NEE", colnames(dmat)) | grepl("Fcc", colnames(dmat)))
    dmat[, ec] <- NA; attr <- paste("_no_seg_nee", attr, sep="")  
  } 
  
  
  
    
  
  xlim <- range(dmat[,"dt"])
  ylim_c <- range(dmat[,ccm], na.rm=T)*c(1, 1.5)
  if(ylim_c[1]<0)ylim_c <- ylim_c*c(1.05, 1)
  #ylim_c[1] <- (-40)
  if(add_AmeriFlux_edire_no_WPL)ylim_c[1] <- (-1100)
  ylim_l <- range(dmat[,lcm], na.rm=T)*c(1, 1.5)
  ylim_p <- range(datdd[,c("P_hh_gm", "P_hh_sm")], na.rm=T)*c(1, 2.2)
  
  if(pre_prec | post_prec){ylim_l <- c(0, 450); ylim_c <- c(-30, 70)}
  if(day_only | night_only){ylim_c <- c(-60, 80)}
  
  
  gpath <- "E:/REC_7_Data/10_Plots/7_cumsum/"
  png(paste(gpath, plot_nm, "_cumsum", attr, "_h", xch, "_corr_00.png", sep=""), width=1200, height=1200)
  
  #par(mfrow=c(2,2), mar = c(3, 3, 3, 2), oma = c(3, 3, 5, 1), mgp=c(2, 0.5, 0))
  if(!add_prec)par(mfrow=c(2,2), mar = c(7, 6, 5, 2), mgp=c(2, 0.5, 0), tck=0.01)  # mar = c(5, 6, 5, 2)
  if(add_prec)par(mfrow=c(2,2), mar = c(7, 6, 5, 5), mgp=c(2, 0.5, 0), tck=0.01)
  
  
  
  
  ## plot 1
  plot(dmat[,1]~dmat[,"dt"], type='n', xlim=xlim, ylim=ylim_l, axes=F, xlab="", ylab="") 
  for(i in tp1){lines(dmat[,les[i]]~dmat[,"dt"], col=cols[i])}
  
  
  axis(2, cex.axis=2); box(); 
  #mtext(paste("Cumulative LE (MW m-2)",sep=""), side=2, line=3, cex=2)
  mtext(expression("Cumulative LE (MW m"^"-2"*")",sep=""), side=2, line=3, cex=2)
  mtext("LE Grassland", side=3, line=1, cex=3)
  mtext ("a)", side=3, adj=0, line=1, cex=3)
  
  #legend("top", les[tp1], ncol=2, col=cols[tp1], pch=16, cex=2)
  legend("left", c(legend_g, "Precip"), ncol=2, col=c(cols[tp1],"cyan"), pch=16, cex=1.5)
  axis.Date(1, at=seq(min(datdd[,"dt"]), max(datdd[,"dt"]), by="months"), cex.axis=2, las=2, format="%b-%y"); 
  if(add_prec){  par(new=T)
    plot(dmat[,"dt"], dmat[,"P_gm"], col='cyan', ylim=rev(ylim_p), type="l", axes=F, xlab="", ylab="")
    axis(4, cex.axis=2, mgp=c(4, 1, 0)); mtext("Precip (mm / h)", side=4, line=3, cex=2)
  }
  
  
  
  ## plot 2
  plot(dmat[,1]~dmat[,"dt"], type='n', xlim=xlim, ylim=ylim_l, axes=F, xlab="", ylab="") 
  for(i in tp2){lines(dmat[,les[i]]~dmat[,"dt"], col=cols[i])}
  
  axis(2, cex.axis=2); box(); 
  #mtext(paste("Cumulative LE (MW m-2)",sep=""), side=2, line=3, cex=2)
  mtext(expression("Cumulative LE (MW m"^"-2"*")",sep=""), side=2, line=3, cex=2)
  mtext("LE Shrubland", side=3, line=1 ,cex=3)
  mtext ("b)", side=3, adj=0, line=1, cex=3)
  
  #legend("top", les[tp2], ncol=2, col=cols[tp2], pch=16, cex=2)
  #legend("left", c(legend_s, "Precip"), ncol=2, col=c(cols[tp1],"cyan"), pch=16, cex=1.5)
  axis.Date(1, at=seq(min(datdd[,"dt"]), max(datdd[,"dt"]), by="months"), cex.axis=2, las=2, format="%b-%y"); 
  if(add_prec){  par(new=T)
    plot(dmat[,"dt"], dmat[,"P_sm"], col='cyan', ylim=rev(ylim_p), type="l", axes=F, xlab="", ylab="")
    axis(4, cex.axis=2, mgp=c(4, 1, 0)); mtext("Precip (mm / h)", side=4, line=3, cex=2)
  }
  
  
  
  
  
  ## plot 3
  plot(dmat[,1]~dmat[,"dt"], type='n', xlim=xlim, ylim=ylim_c, axes=F, xlab="", ylab="") 
  for(i in tp1){lines(dmat[,nees[i]]~dmat[,"dt"], col=cols[i])}
  abline(h=0, col="grey")
  
  axis(2, cex.axis=2); box(); 
  mtext(expression("Cumulative NEE (gC m"^"-2"*")",sep=""), side=2, line=3, cex=2)
  
  #mtext(expression("Cumulative LE (MW m"^"-2"*")",sep=""), side=2, line=3, cex=2)
  
  mtext("NEE Grassland", side=3, line=1, cex=3)
  mtext ("c)", side=3, adj=0, line=1, cex=3)
  
  #legend("top", nees[tp1], ncol=2, col=cols[tp1], pch=16, cex=2)
  #legend("bottomleft", c(legend_g, "Precip"), ncol=2, col=c(cols[tp1],"cyan"), pch=16, cex=1.5)
  axis.Date(1, at=seq(min(datdd[,"dt"]), max(datdd[,"dt"]), by="months"), cex.axis=2, las=2, format="%b-%y"); 
  if(add_prec){  par(new=T)
    plot(dmat[,"dt"], dmat[,"P_gm"], col='cyan', ylim=rev(ylim_p), type="l", axes=F, xlab="", ylab="")
    axis(4, cex.axis=2, mgp=c(4, 1, 0)); mtext("Precip (mm / h)", side=4, line=3, cex=2)
  }
  
  
  
  ## plot 4 
  plot(dmat[,1]~dmat[,"dt"], type='n', xlim=xlim, ylim=ylim_c, axes=F, xlab="", ylab="") 
  for(i in tp2){lines(dmat[,nees[i]]~dmat[,"dt"], col=cols[i])}
  abline(h=0, col="grey")
  
  axis(2, cex.axis=2); box(); 
  #mtext(paste("Cumulative NEE (gC m-2)",sep=""), side=2, line=3, cex=2)
  mtext(expression("Cumulative NEE (gC m"^"-2"*")",sep=""), side=2, line=3, cex=2)
  mtext("NEE Shrubland", side=3, line=1 ,cex=3)
  mtext ("d)", side=3, adj=0, line=1, cex=3)
  
  #legend("top", nees[tp2], ncol=2, col=cols[tp2], pch=16, cex=2)
  #legend("bottomleft", c(legend_s, "Precip"), ncol=2, col=c(cols[tp1],"cyan"), pch=16, cex=1.5)
  axis.Date(1, at=seq(min(datdd[,"dt"]), max(datdd[,"dt"]), by="months"), cex.axis=2, las=2, format="%b-%y"); 
  if(add_prec){  par(new=T)
    plot(dmat[,"dt"], dmat[,"P_sm"], col='cyan', ylim=rev(ylim_p), type="l", axes=F, xlab="", ylab="")
    axis(4, cex.axis=2, mgp=c(4, 1, 0)); mtext("Precip (mm / h)", side=4, line=3, cex=2)
  }
  
  
  
  dev.off()
  
  
  
  
  
  ### ggplot version
  if(T){
    
    gpath <- "E:/REC_7_Data/10_Plots/7_cumsum/"
    p_nm <- paste(gpath, plot_nm, "_cumsum", attr, "_h", xch, "_ggplot.png", sep="") # raster
    #p_nm <- paste(gpath, plot_nm, "_cumsum", attr, "_h", xch, "_ggplot.pdf", sep="") # vector
    
    # time stamp
    mthetimes <-  as.POSIXct(dmat[,"dt"], format="%d/%m/%y %H:%M:%S") 
    dmat$dt_2 <- mthetimes
    
    ylim_l<-c(0, 800)
    
    ya2<-expression("Precipitation (mm h"^"-1"*")",sep="")
    
    
    # ylim correction factor for precipitation
    
    # diff(ylim_l)/ylim_p[2]   = 20.41817
    # diff(ylim_l)/20.41817    = 36.32201    # ylim that I want
    
    # diff(ylim_c)/ylim_p[2]   =  1.629766
    # diff(ylim_c)/ 1.629766   =  36.322     # ylim that I want
    
    
    
    # add modified precipitation
    dmat$P_plot_le_gm <- (dmat$P_gm * (-diff(ylim_l)/ylim_p[2])) + ylim_l[2]
    dmat$P_plot_le_sm <- (dmat$P_sm * (-diff(ylim_l)/ylim_p[2])) + ylim_l[2]
    dmat$P_plot_nee_gm <- (dmat$P_gm * (-diff(ylim_c)/ylim_p[2])) + ylim_c[2]
    dmat$P_plot_nee_sm <- (dmat$P_sm * (-diff(ylim_c)/ylim_p[2])) + ylim_c[2]
    
    

    
    p1 <- ggplot(dmat, aes(x=dt_2)) +
      labs(x = "", y = expression("Cumulative LE (MW m"^"-2"*")",sep=""), title = "LE Grassland") +
      geom_line(aes(y = LE_f_g1, colour = "EC1")) + 
      geom_line(aes(y = LE_f_g2, colour = "EC2")) + 
      geom_line(aes(y = LE_f_g3, colour = "EC3")) + 
      geom_line(aes(y = LE_f_g4, colour = "EC4")) +
      geom_line(aes(y = LE_f_gm, colour = "EC0")) +
      geom_line(aes(y = P_plot_le_gm, colour = "prec")) +
      scale_y_continuous(sec.axis = sec_axis(~.*-ylim_p[2]/diff(ylim_l) + (ylim_p[2]), name = ya2)) +
      theme_bw() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + # remove grid 
      theme(axis.text.x = element_text(angle = 90)) +
      theme(legend.title = element_blank(), legend.position = c(0.15, 0.50)) +    # legend position
      scale_colour_manual(values = colours)
      
    
    p2 <- ggplot(dmat, aes(x=dt_2)) +
      labs(x = "", y = expression("Cumulative LE (MW m"^"-2"*")",sep=""), title = "LE Shrubland") +
      geom_line(aes(y = LE_f_s1, colour = "EC1")) + 
      geom_line(aes(y = LE_f_s2, colour = "EC2")) + 
      geom_line(aes(y = LE_f_s3, colour = "EC3")) + 
      geom_line(aes(y = LE_f_s4, colour = "EC4")) +
      geom_line(aes(y = LE_f_sm, colour = "EC0")) +
      geom_line(aes(y = P_plot_le_sm, colour = "prec")) +
      scale_y_continuous(sec.axis = sec_axis(~.*-ylim_p[2]/diff(ylim_l) + (ylim_p[2]), name = ya2)) +
      theme_bw() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + # remove grid 
      theme(axis.text.x = element_text(angle = 90)) +
      theme(legend.position="none") +  
      scale_colour_manual(values = colours)
      
    
    p3 <- ggplot(dmat, aes(x=dt_2)) +
      labs(x = "", y = expression("Cumulative NEE (g C m"^"-2"*")",sep=""), title = "NEE Grassland") +
      geom_line(aes(y = NEE_uStar_f_g1, colour = "EC1")) + 
      geom_line(aes(y = NEE_uStar_f_g2, colour = "EC2")) + 
      geom_line(aes(y = NEE_uStar_f_g3, colour = "EC3")) + 
      geom_line(aes(y = NEE_uStar_f_g4, colour = "EC4")) +
      geom_line(aes(y = NEE_uStar_f_gm, colour = "EC0")) +
      geom_line(aes(y = P_plot_nee_gm, colour = "prec")) +
      geom_point(aes(x=dt_2[1], y=ylim_c[1]), colour="white") + # only to specify lower end
      scale_y_continuous(sec.axis = sec_axis(~.*-ylim_p[2]/diff(ylim_c) + (ylim_p[2]), name = ya2)) +
      theme_bw() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + # remove grid 
      theme(axis.text.x = element_text(angle = 90)) +
      theme(legend.position="none") +  
      scale_colour_manual(values = colours)
    
    
    p4 <- ggplot(dmat, aes(x=dt_2)) +
      labs(x = "", y = expression("Cumulative NEE (g C m"^"-2"*")",sep=""), title = "NEE Grassland") +
      geom_line(aes(y = NEE_uStar_f_s1, colour = "EC1")) + 
      geom_line(aes(y = NEE_uStar_f_s2, colour = "EC2")) + 
      geom_line(aes(y = NEE_uStar_f_s3, colour = "EC3")) + 
      geom_line(aes(y = NEE_uStar_f_s4, colour = "EC4")) +
      geom_line(aes(y = NEE_uStar_f_sm, colour = "EC0")) +
      geom_line(aes(y = P_plot_nee_sm, colour = "prec")) + 
      geom_point(aes(x=dt_2[1], y=ylim_c[1]), colour="white") + # only to specify lower end
      scale_y_continuous(sec.axis = sec_axis(~.*-ylim_p[2]/diff(ylim_c) + (ylim_p[2]), name = ya2)) +
      theme_bw() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + # remove grid 
      theme(axis.text.x = element_text(angle = 90)) +
      theme(legend.position="none") +
      scale_colour_manual(values = colours)
    
    
    
    ## combine plot susing Patchwork
    pall <- (p1 + p2) / (p3 + p4) +
      plot_annotation(tag_levels = 'a') & theme(plot.tag.position = c(0.0, 0.97)) 
    
    
    ggsave(pall,
           filename = p_nm,
           width = 17,
           height = 17,
           units = "cm")
    
  }
  
  
  
  
  
  
}  # cumsum NEE and LE


###
###  Energy balance (EB) closure ####
###

if(T){

  # Rn + H + LE + G ~ 0     
  # G: soil heat flux + soil heat storage (neg)
  # ideal closure: Y=X
  # Burba et al. (2010) found 80% closure H+LE~Rn+G; r2=0.93

  
  ## select data
  xg <- datdd[,"NETRAD_gm"]-datdd[,"SHF_ALL_AVG_soilg"]
  xs <- datdd[,"NETRAD_sm"]-datdd[,"SHF_ALL_AVG_soils"]
  ygm <- datdd[,"H_gm"] + datdd[,"cLE_gm"]
  ysm <- datdd[,"H_sm"] + datdd[,"cLE_sm"]
  yg1 <- datdd[,"Hc_g1"] + datdd[,"cLEc_g1"]
  ys1 <- datdd[,"Hc_s1"] + datdd[,"cLEc_s1"]
  
  
  xg[xg==0] <- NA # xg has 500 zeros, but netg has 0, shfg 3 and xgs 5, so they all arise from the sums
  # The 500 zeros results in a feature that looks a lot like an artifact, but it really is not
  # with or without these 500 zeros, the slope of the plot is the same
  
  
  ##
  ## Calculate Energy Balance Ratio
  ##
  gm_ebr <- (sum(ygm[ygm>0], na.rm=T) + sum(xg[xg<0], na.rm=T)) / 
    (sum(ygm[ygm<0], na.rm=T) + sum(xg[xg>0], na.rm=T))    # 0.7820
  sm_ebr <- (sum(ysm[ysm>0], na.rm=T) + sum(xs[xs<0], na.rm=T)) / 
    (sum(ysm[ysm<0], na.rm=T) + sum(xs[xs>0], na.rm=T))    # 0.8446
  
  g1_ebr <- (sum(yg1[yg1>0], na.rm=T) + sum(xg[xg<0], na.rm=T)) / 
    (sum(yg1[yg1<0], na.rm=T) + sum(xg[xg>0], na.rm=T))    # 0.9496
  s1_ebr <- (sum(ys1[ys1>0], na.rm=T) + sum(xs[xs<0], na.rm=T)) / 
    (sum(ys1[ys1<0], na.rm=T) + sum(xs[xs>0], na.rm=T))    # 0.9525
  
  
  #                       (Intercept)        slope       AmeriFlux 
  #l_gm$coefficients          7.4636225   0.8293651           0.81          
  #l_sm$coefficients          7.3525987   0.8807114           0.88
  
  # estimate of bowen ratio beta = H/LE (on x,y axis)
  #x <- datdd[,"cLEc_g1"];y <- datdd[,"Hc_g1"];lim <- range(c(x,y), na.rm=T); plot(x, y, pch=16, xlim=lim, ylim=lim)
  #x <- datdd[,"cLE_gm"]; y <- datdd[,"H_gm"]; lim <- range(c(x,y), na.rm=T); plot(x, y, pch=16, xlim=lim, ylim=lim)
  
  
  
  ### visualize energy balance (EB) closure 
  
  # Determine axis range
  lim <- range(xg, xs, ygm, ysm, yg1, ys1, na.rm=T)
  
  # create dataframe for ggplot
  dtf <- data.frame(xg, xs, ygm, ysm, yg1, ys1)                                 
  
  # prepare axis labels
  ylab <- expression("H + LE (W m"^"-2"*")") 
  xlab <- expression("Rn - G (W m"^"-2"*")")
  
  
  # Plot 1
  aa <- cbind(xg, ygm); bb <- aa[complete.cases(aa),]
  cc <- prcomp(bb)$rotation
  m0 <-  beta  <-  round( cc[2,1]/cc[1,1], 2)
  y0   <-  round( mean(bb[,2])-beta*mean(bb[,1]), 0)
  r  <-  round( cor(bb, method="pearson")[1,2], 2)
  rr <- paste("y = ", y0, "+", m0, "x - r:", r)
  
  p1 <-  ggplot(dtf, aes(x=xg, y=ygm) ) +
    labs(x = xlab, y = ylab, title = "Seg EC0") +
    geom_bin2d(bins = 150) +                       # Bin size control 
    scale_fill_continuous(type = "viridis") +     # colour palette
    theme_bw() + xlim(lim) + ylim(lim) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + # remove grid 
    theme(text = element_text(size=14)) + 
    theme(legend.key.size = unit(0.4, "cm")) +   # legend size
    theme(legend.position = c(0.85, 0.25)) +       # legend position
    theme(plot.title = element_text(size = 14, face = "bold"))  +
    geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
    geom_abline(intercept = y0, slope = m0) +
    annotate("text", x = 200, y = 700, label = rr, size=4)  
  
  
  # Plot 2
  aa <- cbind(xs, ysm); bb <- aa[complete.cases(aa),]
  cc <- prcomp(bb)$rotation
  m0 <-  beta  <-  round( cc[2,1]/cc[1,1], 2)
  y0   <-  round( mean(bb[,2])-beta*mean(bb[,1]), 0)
  r  <-  round( cor(bb, method="pearson")[1,2], 2)
  rr <- paste("y = ", y0, "+", m0, "x - r:", r)
  
  p2 <-  ggplot(dtf, aes(x=xs, y=ysm) ) +
    labs(x = xlab, y = ylab, title = "Ses EC0") +
    geom_bin2d(bins = 150) +                       # Bin size control 
    scale_fill_continuous(type = "viridis") +     # colour palette
    theme_bw() + xlim(lim) + ylim(lim) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + # remove grid 
    theme(text = element_text(size=14)) + 
    theme(legend.key.size = unit(0.4, "cm")) +   # legend size
    theme(legend.position = c(0.85, 0.25)) +       # legend position
    theme(plot.title = element_text(size = 14, face = "bold"))  +
    geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
    geom_abline(intercept = y0, slope = m0) +
    annotate("text", x = 200, y = 700, label = rr, size=4)  
  
  
  # Plot 3
  aa <- cbind(xg, yg1); bb <- aa[complete.cases(aa),]
  cc <- prcomp(bb)$rotation
  m0 <-  beta  <-  round( cc[2,1]/cc[1,1], 2)
  y0   <-  round( mean(bb[,2])-beta*mean(bb[,1]), 0)
  r  <-  round( cor(bb, method="pearson")[1,2], 2)
  rr <- paste("y = ", y0, "+", m0, "x - r:", r)
  
  p3 <-  ggplot(dtf, aes(x=xg, y=yg1) ) +
    labs(x = xlab, y = ylab, title = "Seg EC1") +
    geom_bin2d(bins = 150) +                       # Bin size control 
    scale_fill_continuous(type = "viridis") +     # colour palette
    theme_bw() + xlim(lim) + ylim(lim) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + # remove grid 
    theme(text = element_text(size=14)) + 
    theme(legend.key.size = unit(0.4, "cm")) +   # legend size
    theme(legend.position = c(0.85, 0.25)) +       # legend position
    theme(plot.title = element_text(size = 14, face = "bold"))  +
    geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
    geom_abline(intercept = y0, slope = m0) +
    annotate("text", x = 200, y = 700, label = rr, size=4)  
  
  
  # Plot 4
  aa <- cbind(xs, ys1); bb <- aa[complete.cases(aa),]
  cc <- prcomp(bb)$rotation
  m0 <-  beta  <-  round( cc[2,1]/cc[1,1], 2)
  y0   <-  round( mean(bb[,2])-beta*mean(bb[,1]), 0)
  r  <-  round( cor(bb, method="pearson")[1,2], 2)
  rr <- paste("y = ", y0, "+", m0, "x - r:", r)
  
  p4 <-  ggplot(dtf, aes(x=xs, y=ys1) ) +
    labs(x = xlab, y = ylab, title = "Ses EC1") +
    geom_bin2d(bins = 150) +                       # Bin size control 
    scale_fill_continuous(type = "viridis") +     # colour palette
    theme_bw() + xlim(lim) + ylim(lim) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + # remove grid 
    theme(text = element_text(size=14)) + 
    theme(legend.key.size = unit(0.4, "cm")) +   # legend size
    theme(legend.position = c(0.85, 0.25)) +       # legend position
    theme(plot.title = element_text(size = 14, face = "bold"))  +
    geom_abline(intercept = 0, slope = 1, colour="grey", linetype="dashed") +
    geom_abline(intercept = y0, slope = m0) +
    annotate("text", x = 200, y = 700, label = rr, size=4)  
  
  
  ### combine plots using Patchwork
  pall <- (p1 + p2) / (p3 + p4) +
    plot_annotation(tag_levels = 'a') & theme(plot.tag.position = c(0.0, 0.97)) 
  
  
  ## save raster
  ggsave(pall,
         filename = "E:/REC_7_Data/10_Plots/11_energy_balance_closure/Figure 6 - energy_balance_TLS_ggplot.png",
         width = 17,
         height = 17,
         units = "cm")
  
  # save vector
  ggsave(pall,
         filename = "E:/REC_7_Data/10_Plots/11_energy_balance_closure/Figure 6 - energy_balance_TLS_ggplot.pdf",
         width = 17,
         height = 17,
         units = "cm")

}



###
### Land cover maps ####
###
if(T){

  ppath <- "E:/REC_7_Data/10_Plots/9_footprints/"
  filenm <- paste(ppath, "land_cover_maps.png", sep="")
  
  png(filenm, width=1400, height=600)
  #par(mar=c(5,5,4,8), oma=c(1,1,1,2), tck=-0.01)
  par(mfrow=c(1, 2), mar=c(0,0,0,0), oma=c(5,5,5,15), tck=-0.01)
  
  
  #par(xpd=TRUE)   # allows legend outside of the plot
  par(xpd=NA)      # even better !
  
  labels=c(-440, seq(-400, 400, 100), 440)
  ats <- c(0, seq(0.04, 0.96, 0.115), 1)
  
  cols <- c("moccasin", "darkgreen", "olivedrab3")
  
  image(segmap, col=cols, axes=F); box()
  axis(1, at=ats, labels=labels, las=3); mtext("Distance (m)", side=1, line=4, cex=2)
  axis(2, at=ats, labels=labels, las=1); mtext("Distance (m)", side=2, line=3, cex=2)
  mtext(paste("US-SEG Land Cover"), 3, line=1, font=2, cex=2)
  
  
  image(sesmap, col=cols, axes=F); box()
  axis(1, at=ats, labels=labels, las=3); mtext("Distance (m)", side=1, line=4, cex=2)
  axis(4, at=ats, labels=labels, las=1); mtext("Distance (m)", side=4, line=4, cex=2)
  mtext(paste("US-SES Land Cover"), 3, line=1, font=2, cex=2)
  
  legend(1.12, 1, c("Bare", "Shrubs", "Herbaceous"), pch = 16, col=cols, cex=1.5)
  
  
  dev.off() 
  
  
}  # Land cover maps
#
if(T){
  
  # land cover prob barplot
  
  mon_ys <- unique(datdd[,c("yyyy", "mon")])
  rpath <- "E:/REC_7_Data/9_R/Rdata/"
  pmat <- NULL
  
  for(i in 1:nrow(mon_ys)){
    load(paste(rpath, "pmat_80_", mon_ys[i, "yyyy"], "_", mon_ys[i, "mon"], ".RData", sep="")) # load "pmat_80
    pmat <- rbind(pmat, pmat_80)
  }
  
  pcols <- paste( rep(c("p1", "p2", "p3"), 10), rep(adatasets[1:10], each=3), sep="_" )
  
  pv <- apply(pmat[,pcols], 2, mean, na.r=T)
  pb <- matrix(pv, nrow=3); colnames(pb) <- adatasets[1:10]; rownames(pb) <- c("p1", "p2", "p3")
  pb <- pb[,c(9, 1:4, 10, 5:8)]
  
  ps <- apply(pmat[,pcols], 2, sd, na.r=T)
  pa <- matrix(ps, nrow=3); colnames(pa) <- adatasets[1:10]; rownames(pa) <- c("p1", "p2", "p3")
  pa <- pa[,c(9, 1:4, 10, 5:8)]
  
  
  cols <- c("darkgreen", "olivedrab3", "moccasin")

  
  
  #### reshape data
  pbi <- pb[c(2, 3, 1), 10:1];
  pai <- pa[c(2, 3, 1), 10:1];
  
  bplot <- "E:/REC_7_Data/10_Plots/9_footprints/"
  filenm <- paste(bplot, "land_cover_probabilities_all_year_h_EC0.png", sep="")
  
  png(filenm, width=900, height=900)
  par(mar = c(8, 13, 2, 2), mgp=c(2, 2, 0), tck=-0.01)
  
  pbi_h <- barplot(pbi, las=1, beside=T, xlim=c(0, 1), cex.names=3, cex.axis=3, col=cols, horiz=T,
          names.arg = c("Ses EC4", "Ses EC3", "Ses EC2", "Ses EC1", "Ses EC0", 
                        "Seg EC4", "Seg EC3", "Seg EC2", "Seg EC1", "Seg EC0"));
  arrows(x0=pbi+pai, y0=pbi_h, x1=pbi-pai, y1=pbi_h, length=0, code=3, lwd=3)
  mtext("Probability", 1, line=5, cex=3); abline(v=0.8, col="grey", lty=3); box()
  legend("bottomright", legend=c("Bare Ground", "Herbaceous", "Shrubs"), col=rev(cols), pch=16, cex=2.5)
  
  dev.off()

  
  
  
  
}  # land cover prob barplot
#
if(T){
  
  # RECs agreement xy plots with major axis
  
  vertical <- T
  
  
  dat_ym <- datdd
  
  # y-axis: always EC1
  rfx <- c("H_f_g1",          "H_f_g1",         "H_f_g1",         "H_f_s1",         "H_f_s1",         "H_f_s1",
         "LE_f_g1",         "LE_f_g1",        "LE_f_g1",        "LE_f_s1",        "LE_f_s1",        "LE_f_s1",
         "NEE_uStar_f_g1",  "NEE_uStar_f_g1", "NEE_uStar_f_g1", "NEE_uStar_f_s1", "NEE_uStar_f_s1", "NEE_uStar_f_s1")
  # x-axis: EC2, EC3, EC4
  mfx <- c("H_f_g2",          "H_f_g3",         "H_f_g4",         "H_f_s2",         "H_f_s3",         "H_f_s4",
         "LE_f_g2",         "LE_f_g3",        "LE_f_g4",        "LE_f_s2",        "LE_f_s3",        "LE_f_s4",
         "NEE_uStar_f_g2",  "NEE_uStar_f_g3", "NEE_uStar_f_g4", "NEE_uStar_f_s2", "NEE_uStar_f_s3", "NEE_uStar_f_s4")
  
  
  if(vertical){
    rfx <- c("H_f_g1",          "H_f_g1",         "H_f_g1",         
           "LE_f_g1",         "LE_f_g1",        "LE_f_g1",        
           "NEE_uStar_f_g1",  "NEE_uStar_f_g1", "NEE_uStar_f_g1", 
           "H_f_s1",         "H_f_s1",         "H_f_s1",
           "LE_f_s1",        "LE_f_s1",        "LE_f_s1",
           "NEE_uStar_f_s1", "NEE_uStar_f_s1", "NEE_uStar_f_s1")
    # x-axis: EC2, EC3, EC4
    mfx <- c("H_f_g2",          "H_f_g3",         "H_f_g4",         
           "LE_f_g2",         "LE_f_g3",        "LE_f_g4",        
           "NEE_uStar_f_g2",  "NEE_uStar_f_g3", "NEE_uStar_f_g4", 
           "H_f_s2",         "H_f_s3",         "H_f_s4",
           "LE_f_s2",        "LE_f_s3",        "LE_f_s4",
           "NEE_uStar_f_s2", "NEE_uStar_f_s3", "NEE_uStar_f_s4")
  }
  
  
  
  pid <- c("a)", "b)", "c)", "d)", "e)", "f)",
         "g)", "h)", "i)", "l)", "m)", "n)",
         "o)", "p)", "q)", "r)", "s)", "t)")
  
  
  
  #uid <- c("(W m-2)", "(W m-2)", "(umol CO2 m-2 s-1)")
  uid <- c(  expression("H (W m"^"-2"*")"),
             expression("LE (W m"^"-2"*")"),
             expression("NEE (umol CO"["2"]*" m"^"-2"*"s"^"-1"*")")   )
  
  
  
  
  # axis-sized labs
  slab <- rep( rep(mlabs, each=3), 3) # Seg and Ses
  ecx <- rep("EC1", 18)
  ecy <- rep(c("EC2", "EC3", "EC4"), 6)
  flab <- rep(c("H", "LE", "NEE"), each=6)
  ulab <- rep(uid, each=6)
  
  if(vertical){
    slab <- rep(mlabs, each=9)
    flab <- rep(c("H", "LE", "NEE", "H", "LE", "NEE"), each=3)
    ulab <- rep( rep(uid, each=3), 2)
  }
  
  xlab0 <- paste( slab, ecx, "-", flab)  
  ylab0 <- paste( slab, ecy, "-", flab)
  
  # expression() and paste() do not work together      !!!!!!
  #xlab <- paste( slab, ecx, "-", flab, ulab)   # use bquote instead (see below)
  
  
  # Total least squares
  ints <- slopes <- cors <- rep(NA, length(mfx))
  
  for(i in 1:length(mfx)){
  
    aa <- dat_ym[,c(mfx[i], rfx[i])]; bb <- aa[complete.cases(aa),]
    cc <- prcomp(bb)$rotation
    cors[i]   <-  cor(bb, method="pearson")[1,2]
    slopes[i] <-  beta  <-  cc[2,1]/cc[1,1]
    ints[i]   <-  mean(bb[,2])-beta*mean(bb[,1])
    
  }
  
  lims_h <-  range(datdd[, paste("H_f", datasets, sep="_")], na.rm=T)
  lims_le <- range(datdd[, paste("LE_f", datasets, sep="_")], na.rm=T)
  lims_fc <- range(datdd[, paste("NEE_uStar_f", datasets, sep="_")], na.rm=T)
  
  attr <- ""; if(vertical)attr <- "_vert"
  pl_name <- paste("E:/REC_7_Data/10_Plots/LEC_cluster_agreement", attr, "_00.png", sep="")
  if(!vertical){
    png(pl_name, width=2700, height=1350)   
    par(mfrow=c(3, 6), mar=c(8,10,5,1) , oma=c(0,0,10,0), mgp=c(4, 2, 0))
  } else {
    png(pl_name, width=1350, height=2700)   
    par(mfrow=c(6, 3), mar=c(8,10,5,2) , oma=c(0,0,10,0), mgp=c(4, 2, 0))
  }
  
  
  
  for(i in 1:length(mfx)){  
    
    if(vertical)par(mar=c(8,10,5,2))
    if(vertical & i %in% c(10:12)){par(mar=c(8,10,15,2))}
    
    
    xlab <- bquote(.(xlab0[i]) ~ "(W m"^"-2"*")")  # expression() and paste() do not work together!
    ylab <- bquote(.(ylab0[i]) ~ "(W m"^"-2"*")")
    
    
    
    if(!vertical){
      
      if(i %in% c( 1: 6 )){lims <- lims_h; } 
      if(i %in% c( 7: 12)){lims <- lims_le }
      if(i %in% c(13: 18)){lims <- lims_fc 
        xlab <- bquote(.(xlab0[i]) ~ "(umol CO"["2"]*" m"^"-2"*"s"^"-1"*")")
        ylab <- bquote(.(ylab0[i]) ~ "(umol CO"["2"]*" m"^"-2"*"s"^"-1"*")")
      }     
      
    } else {
      
      if(i %in% c( 1:3, 10:12 )){lims <- lims_h; } 
      if(i %in% c( 4:6, 13:15 )){lims <- lims_le }
      if(i %in% c( 7:9, 16:18 )){lims <- lims_fc 
        xlab <- bquote(.(xlab0[i]) ~ "(umol CO"["2"]*" m"^"-2"*"s"^"-1"*")")
        ylab <- bquote(.(ylab0[i]) ~ "(umol CO"["2"]*" m"^"-2"*"s"^"-1"*")")
      }     
      
    }
    
    
    
    
    
    plot(dat_ym[,mfx[i]], dat_ym[,rfx[i]], ylim=lims, xlim=lims, pch=16, cex.axis=2.5, cex.main=2.5, cex.lab=2.5,
         xlab="", ylab="", main="", col=alpha(1, 0.2) )
    mtext(xlab, 1, line=7, cex=2)
    mtext(ylab, 2, line=5, cex=2)
    if(i==2)mtext("Grassland", side=3, line=6, cex=4)
    
    if(!vertical){
      if(i==5)mtext("Shrubland", side=3, line=6, cex=4)
    } else {
      if(i==11)mtext("Shrubland", side=3, line=6, cex=4)
    }
    mtext(paste("y =", round(ints[i],3), "+", round(slopes[i],3),"x"), 
          side=3, adj=0.05, line=-3, cex=2); abline(a=ints[i], b=slopes[i]);  # total least squares
    mtext(paste("r:", round(cors[i],2)), side=3, adj=0.05, line=-6, cex=2)    # perason coef.
    mtext (pid[i], side=3, adj=0, line=1, cex=2.5)
    abline(a=0, b=1)
    
  }  
  
  dev.off()
  
  
}  # RECs agreement xy plots with major axis
#
if(T){
  
  # RECs agreement xy plots with major axis - two plots
  
  dat_ym <- datdd
  
  
  # y-axis: always EC1
  rfx <- c("H_f_g1",          "H_f_g1",         "H_f_g1",         
         "LE_f_g1",         "LE_f_g1",        "LE_f_g1",        
         "NEE_uStar_f_g1",  "NEE_uStar_f_g1", "NEE_uStar_f_g1", 
         "H_f_s1",         "H_f_s1",         "H_f_s1",
         "LE_f_s1",        "LE_f_s1",        "LE_f_s1",
         "NEE_uStar_f_s1", "NEE_uStar_f_s1", "NEE_uStar_f_s1")
  # x-axis: EC2, EC3, EC4
  mfx <- c("H_f_g2",          "H_f_g3",         "H_f_g4",         
         "LE_f_g2",         "LE_f_g3",        "LE_f_g4",        
         "NEE_uStar_f_g2",  "NEE_uStar_f_g3", "NEE_uStar_f_g4", 
         "H_f_s2",         "H_f_s3",         "H_f_s4",
         "LE_f_s2",        "LE_f_s3",        "LE_f_s4",
         "NEE_uStar_f_s2", "NEE_uStar_f_s3", "NEE_uStar_f_s4")
  
  
  pid <- c("a)", "b)", "c)", "d)", "e)", "f)", "g)", "h)", "i)")
  
  #uid <- c("(W m-2)", "(W m-2)", "(umol CO2 m-2 s-1)")
  uid <- c(  expression("H (W m"^"-2"*")"),
           expression("LE (W m"^"-2"*")"),
           expression("NEE (umol CO"["2"]*" m"^"-2"*"s"^"-1"*")")   )
  
  
  
  
  # axis-sized labs
  #slab <- rep( rep(mlabs, each=3), 3) # Seg and Ses
  ecx <- rep("EC1", 18)
  ecy <- rep(c("EC2", "EC3", "EC4"), 6)
  #flab <- rep(c("H", "LE", "NEE"), each=6)
  #ulab <- rep(uid, each=6)
  
  
  slab <- rep(mlabs, each=9)
  flab <- rep(c("H", "LE", "NEE", "H", "LE", "NEE"), each=3)
  ulab <- rep( rep(uid, each=3), 2)
  
  
  xlab0 <- paste( slab, ecx, "-", flab)  
  ylab0 <- paste( slab, ecy, "-", flab)
  
  # expression() and paste() do not work together      !!!!!!
  #xlab <- paste( slab, ecx, "-", flab, ulab)   # use bquote instead (see below)
  
  
  # Total least squares
  ints <- slopes <- cors <- rep(NA, length(mfx))
  
  for(i in 1:length(mfx)){
    
    aa <- dat_ym[,c(mfx[i], rfx[i])]; bb <- aa[complete.cases(aa),]
    cc <- prcomp(bb)$rotation
    cors[i]   <-  cor(bb, method="pearson")[1,2]
    slopes[i] <-  beta  <-  cc[2,1]/cc[1,1]
    ints[i]   <-  mean(bb[,2])-beta*mean(bb[,1])
    
  }
  
  lims_h <-  range(datdd[, paste("H_f", datasets, sep="_")], na.rm=T)
  lims_le <- range(datdd[, paste("LE_f", datasets, sep="_")], na.rm=T)
  lims_fc <- range(datdd[, paste("NEE_uStar_f", datasets, sep="_")], na.rm=T)
  
  
  
  
  for(st in 1:2){
  
  pl_name <- paste("E:/REC_7_Data/10_Plots/", sites[st], "_LEC_cluster_agreement_00.png", sep="")
  png(pl_name, width=1350, height=1350)   
  par(mfrow=c(3, 3), mar=c(8,10,5,1) , oma=c(0,0,10,0), mgp=c(4, 2, 0))
  
  
  for(i in 1:(length(mfx)/2)){  
    
    par(mar=c(8,10,5,2))
    #if(vertical & i %in% c(10:12)){par(mar=c(8,10,15,2))}
    
    k <- i; if(sites[st]=="SES")k <- i+length(mfx)/2
    
    xlab <- bquote(.(xlab0[k]) ~ "(W m"^"-2"*")")  # expression() and paste() do not work together!
    ylab <- bquote(.(ylab0[k]) ~ "(W m"^"-2"*")")
    
    if(i %in% c( 1: 3)){lims <- lims_h; } 
    if(i %in% c( 4: 6)){lims <- lims_le }
    if(i %in% c( 7: 9)){lims <- lims_fc 
    xlab <- bquote(.(xlab0[k]) ~ "(umol CO"["2"]*" m"^"-2"*"s"^"-1"*")")
    ylab <- bquote(.(ylab0[k]) ~ "(umol CO"["2"]*" m"^"-2"*"s"^"-1"*")")
    }
    
    
    
    
    plot(dat_ym[,mfx[k]], dat_ym[,rfx[k]], ylim=lims, xlim=lims, pch=16, cex.axis=2.5, cex.main=2.5, cex.lab=2.5,
         xlab="", ylab="", main="", col=alpha(1, 0.2) )
    mtext(xlab, 1, line=7, cex=2)
    mtext(ylab, 2, line=5, cex=2)
    
    if(k==2) mtext("Grassland", side=3, line=6, cex=4)
    if(k==11)mtext("Shrubland", side=3, line=6, cex=4)
    
    #if(!vertical){
    #  if(i==5)mtext("Shrubland", side=3, line=6, cex=4)
    #} else {
    #  if(i==11)mtext("Shrubland", side=3, line=6, cex=4)
    #}
    
    mtext(paste("y =", round(ints[k],3), "+", round(slopes[k],3),"x"), 
          side=3, adj=0.05, line=-3, cex=2); abline(a=ints[k], b=slopes[k]);  # total least squares
    mtext(paste("r:", round(cors[k],2)), side=3, adj=0.05, line=-6, cex=2)    # perason coef.
    mtext (pid[i], side=3, adj=0, line=1, cex=2.5)
    abline(a=0, b=1)
    
  }  
  
  dev.off()
  
  }
  
  
}  # RECs agreement xy plots with major axis - two plots
#
if(T){
  
  # calculate potential evapotranspiration
  
  # from: https://cran.r-project.org/web/packages/SPEI/SPEI.pdf
  # info on SPEI: https://spei.csic.es/home.html
  
  dts <- dates(datdd[, "dt"])     # daily   filter 
  
  
  pathg <- "E:/REC_7_Data/15_OneFlux/FLX_US-Seg_FLUXNET2015_FULLSET_2007-2017_beta-3/"
  erag  <- read.csv(file=paste(pathg, "FLX_US-Seg_FLUXNET2015_ERAI_YY_1989-2014_beta-3.csv", sep=""), header=TRUE, sep=",")
  lt_mapg <- mean(erag[,"P_ERA"])
  
  paths <- "E:/REC_7_Data/15_OneFlux/FLX_US-Ses_FLUXNET2015_FULLSET_2007-2017_beta-3/"
  eras  <- read.csv(file=paste(paths, "FLX_US-Ses_FLUXNET2015_ERAI_YY_1989-2014_beta-3.csv", sep=""), header=TRUE, sep=",")
  lt_maps <- mean(eras[,"P_ERA"])
  
  
  
  # prepare dataset
  dats <- datdd[, c("dt", "TA_F_gm", "TA_F_sm", "PA_gm", "PA_sm", "RH_F_gm", "RH_F_sm")]
  
  # Two ways to calculate U2 (mean daily wind at 2m heigt):
  # I think that 2) is better because of the monthly resolution and because SES has a canopy at 1m, quite close to 2m
  # 1) on short grass: more generally if your measurement is at height z (uz) (https://www.researchgate.net/post/Is_that_possible_to_convert_wind_speed_measured_in_10_m_height_to_a_possible_2_m_height_wind_speed)
  #    u2 = uz * 4.87 / ln ( 67.8 * z − 5.42 )
  # 2) Uz=(U*/K)*[ln( (Z-d)/Zo)]   (https://en.wikipedia.org/wiki/Log_wind_profile)
  #    Z0: (grassland) the typical range is 0.01-0.05 m. For brush/forest the ranges are 0.5-1.0 m.
  #    K = Von Kármán constant (~0.41); d = displacement height (0.67 * canopy height)
  
  # HH u2 time series
  dats$u2_gm <-  datdd[,"USTAR_gm"]/0.41 * log( (3-(0.67 * 0.5))/0.03 )
  dats$u2_sm <-  datdd[,"USTAR_sm"]/0.41 * log( (3-(0.67 * 1))  /0.5 ) 
  
  dats$rs_gm <- datdd[,"SW_IN_gm"] #+ datdd[,"LW_IN_gm"]
  dats$rs_sm <- datdd[,"SW_IN_sm"] #+ datdd[,"LW_IN_sm"]
  
  dats$lat_gm <-  34.3623
  dats$lat_sm <-  34.3349
  
  dats$z_gm <-  1596
  dats$z_sm <-  1604
  
  wcols <- colnames(dats)[-1]    # wanted columns
  
  
  
  # daily means
  dtd <- aggregate(dats[, wcols], by=list(dts), mean, na.rm=T)
  
  # monthly means
  # msel <- month(dtd[, "Group.1"]) # monthly filter
  msel <- paste(year(dtd[, "Group.1"]), month(dtd[, "Group.1"]), sep="_") # monthly filter
  dtm <-  aggregate(dtd[, wcols[-c(1,2)] ], by=list(msel), mean, na.rm=T)
  
  # monthly min/max
  mmin <-  aggregate(dtd[, c("TA_F_gm", "TA_F_sm")], by=list(msel), min, na.rm=T)
  mmax <-  aggregate(dtd[, c("TA_F_gm", "TA_F_sm")], by=list(msel), max, na.rm=T)

  # monthly precipitation
  mpsel <- paste(year(dats[, "dt"]), month(dats[, "dt"]), sep="_")
  mpre <-  aggregate(datdd[, c("P_F_gm", "P_F_sm")], by=list(mpsel), sum, na.rm=T)
  
  
  
  
  for(i in 1:2){
    
    tmin <- as.vector(mmin[, paste("TA_F", mdatasets[i], sep="_")])
    tmax <- as.vector(mmax[, paste("TA_F", mdatasets[i], sep="_")])
    u2 <- as.vector(dtm[, paste("u2", mdatasets[i], sep="_")])
    lat <- unique(dtm[, paste("lat", mdatasets[i], sep="_")]) 
    Rs <- as.vector(dtm[, paste("rs", mdatasets[i], sep="_")])
    RH <- as.vector(dtm[, paste("RH_F", mdatasets[i], sep="_")])
    P <- as.vector(dtm[, paste("PA", mdatasets[i], sep="_")])
    z <- unique(dtm[, paste("z", mdatasets[i], sep="_")])
    
    
    
    spei <- penman(Tmin=tmin, Tmax=tmax, U2=u2, Ra = NA, lat = lat, Rs = Rs, tsun = NA,
           CC = NA, ed = NA, Tdew = NA, RH = RH, P = P, P0 = NA,
           z = z, crop='tall', na.rm = F)    # "tall" = 0.5 m
    
    assign(paste("spei", mdatasets[i], sep="_"), spei)
  }
  

  # potential evapotranspiration PET
  
  range(spei_gm) #  148.0189 959.6889
  range(spei_sm) #  105.4671 1010.7329
  
  mean(spei_gm)  # 561.3179
  mean(spei_sm)  # 582.4898
  
  # long term aridity index (MAP/PET)
  lt_mapg/mean(spei_gm)  # 0.4569708
  lt_maps/mean(spei_sm)  # 0.4277289
  
  
  
}  # calculate potential evapotranspiration
#
if(T){
  
  # Precipitation from July to Sep 2019
  
  pathg <- "E:/REC_7_Data/15_OneFlux/FLX_US-Seg_FLUXNET2015_FULLSET_2007-2017_beta-3/"
  erag  <- read.csv(file=paste(pathg, "FLX_US-Seg_FLUXNET2015_ERAI_MM_1989-2014_beta-3.csv", sep=""), header=TRUE, sep=",")
  erag$mon <- as.integer(substring(as.character(erag[,1]), 5,6))   
  
  selm <- erag$mon %in% c(7:9)
  sum(erag$P_ERA[selm])/sum(erag$P_ERA)    
  
  #pg <- tapply(erag$P_ERA, erag$mon, mean)
  #sum(pg[7:9])/sum(pg)   # 0.3945975
  
  paths <- "E:/REC_7_Data/15_OneFlux/FLX_US-Ses_FLUXNET2015_FULLSET_2007-2017_beta-3/"
  eras  <- read.csv(file=paste(paths, "FLX_US-Ses_FLUXNET2015_ERAI_MM_1989-2014_beta-3.csv", sep=""), header=TRUE, sep=",")
  eras$mon <- as.integer(substring(as.character(eras[,1]), 5,6))   
  ps <- tapply(eras$P_ERA, eras$mon, mean)
  sum(ps[7:9])/sum(ps)   # 0.3945769
  
  
  
  
  
  # July to September precipitation during study
  
  mpg  <-  sum(     datdd[   datdd[,"mon"] %in% c(7:9)       ,"P_F_gm"]     , na.rm=T)
  mps  <-  sum(     datdd[   datdd[,"mon"] %in% c(7:9)       ,"P_F_sm"]     , na.rm=T)
  
  tpg  <-  sum(     datdd[,"P_F_gm"]     , na.rm=T)
  tps  <-  sum(     datdd[,"P_F_sm"]     , na.rm=T)
  
  mpg/tpg     # 0.4082687
  mps/tps     # 0.3319696
  
  
  
}  # Precipitation from July to September 2019
#
if(T){
  
  # confusion matrix
  
  dpath <- "E:/REC_7_Data/10_Plots/17_land_cover_maps/"
  seg <- read.csv(paste(dpath, "Confusion_matrix_SEG.csv", sep=""), header=T)

  sum(seg[,"Land_Cover"]=="bare")  # 115
  sum(seg[,"Land_Cover"]=="herb")  # 34
  sum(seg[,"Land_Cover"]=="shrub") # 1
  
  sum(seg[,"Land_Cover"]=="bare" & seg[,"Orthomozaic"]=="bare")  # 113
  sum(seg[,"Land_Cover"]=="herb" & seg[,"Orthomozaic"]=="herb")  # 31
  sum(seg[,"Land_Cover"]=="shrub"& seg[,"Orthomozaic"]=="shrub") # 1
  
  sum(seg[,"Land_Cover"]=="bare" & seg[,"Orthomozaic"]=="herb")  # 1
  sum(seg[,"Land_Cover"]=="bare" & seg[,"Orthomozaic"]=="shrub") # 1
  
  sum(seg[,"Land_Cover"]=="herb" & seg[,"Orthomozaic"]=="bare")  # 3
  sum(seg[,"Land_Cover"]=="herb" & seg[,"Orthomozaic"]=="shrub") # 0
  
  sum(seg[,"Land_Cover"]=="shrub"& seg[,"Orthomozaic"]=="bare")  # 0
  sum(seg[,"Land_Cover"]=="shrub"& seg[,"Orthomozaic"]=="herb")  # 0
  
  
  dpath <- "E:/REC_7_Data/10_Plots/17_land_cover_maps/"
  ses <- read.csv(paste(dpath, "Confusion_matrix_SES.csv", sep=""), header=T)
  
  sum(ses[,"Land_Cover"]=="bare")  # 99
  sum(ses[,"Land_Cover"]=="herb")  # 35
  sum(ses[,"Land_Cover"]=="shrub") # 16
  
  sum(ses[,"Land_Cover"]=="bare" & ses[,"Orthomozaic"]=="bare")  # 95
  sum(ses[,"Land_Cover"]=="herb" & ses[,"Orthomozaic"]=="herb")  # 35
  sum(ses[,"Land_Cover"]=="shrub"& ses[,"Orthomozaic"]=="shrub") # 14
  
  sum(ses[,"Land_Cover"]=="bare" & ses[,"Orthomozaic"]=="herb")  # 0
  sum(ses[,"Land_Cover"]=="bare" & ses[,"Orthomozaic"]=="shrub") # 4
  
  sum(ses[,"Land_Cover"]=="herb" & ses[,"Orthomozaic"]=="bare")  # 0
  sum(ses[,"Land_Cover"]=="herb" & ses[,"Orthomozaic"]=="shrub") # 0
  
  sum(ses[,"Land_Cover"]=="shrub"& ses[,"Orthomozaic"]=="bare")  # 0
  sum(ses[,"Land_Cover"]=="shrub"& ses[,"Orthomozaic"]=="herb")  # 2
  
  
  
  
  
}  # confusion matrix




