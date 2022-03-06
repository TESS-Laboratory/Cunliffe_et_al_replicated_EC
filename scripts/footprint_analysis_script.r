## This script performs footprint analysis for Cunliffe et al. In Prep

## Tested with R v3.6.2

# Note that this script currently includes many static file paths for outputs 
# preventing execution on different machines.



# ------ 0.0 Setup Environment ----------
## Load packages
library(chron)
library(fields)    # to plot footprint
library(EBImage)
library(spatialfil)
library(viridis)       # color-blind tested (ugly)
library(raster)  # for land cover maps
library(rgdal)
library(tidyverse)  # for ggplot2

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

#-------------- 0.1 Define paths --------------
path   <-  "E:/REC_7_Data/8_datasets/Wind_repro_20220211/"
mpath1 <- "E:/REC_7_Data/12_Marcys_data/csv_Marcy_old/"
mpath2 <- "E:/REC_7_Data/12_Marcys_data/hh_Marcy_2020_wind/"
footprintpath <- "data/footprint_probability_datasets/Footprint_Landcover_Results.csv"


#-------------- 0.2 Initialize lists --------------

sites  <-  c("SEG", "SES")

recs  <-  1:4

towers  <-  paste(rep(sites, each=4), rep(recs,2), sep="_REC")

systems  <-  c(towers, sites)

mlabs  <-  c("Seg", "Ses")

mdatasets  <-  c("gm", "sm") 

datasets  <-  c("g1", "g2", "g3", "g4", "s1", "s2", "s3", "s4")

adatasets <- c(datasets, mdatasets)

date_start  <-  "01/11/2018"       #1st Nov 2018 to 31st Oct 2020

date_end  <-    "01/11/2020"              



#-------------- 0.3 Create Custom Functions --------------

## count the number of NA rows/cols
sum_na <- function(x){sum(is.na(x))}




#-------------- 1 Read csv data --------------


#-------------- 1.1 Read REC csv data --------------

datg  <- read.csv(file=paste(path, "SEG_fluxes_WIND.csv", sep=""), header=TRUE, sep=",")  
dats  <- read.csv(file=paste(path, "SES_fluxes_WIND.csv", sep=""), header=TRUE, sep=",")  

footprint_df  <- read_csv(file=footprintpath)  



# range(datg[,"WS"]-datg[,"Licor.SEG.WS"], na.rm=T)        # 0 0
# range(datg[,"WD"]-datg[,"Licor.SEG.WD"], na.rm=T)        # 0 0

# range(dats[,"WD"]-dats[,"Licor.SES.WD"], na.rm=T)       # 0 0
# range(dats[,"WS"]-dats[,"Licor.SES.WS"], na.rm=T)       # 0 0

# range(datg[,"USTAR"]- datg[,"Licor.SEG.Ustar"], na.rm=T)    # 0 0
# range(dats[,"USTAR"]- dats[,"Licor.SES.Ustar"], na.rm=T)    # 0 0

# Wx and Licor.SES.Wx are exactly the same !!!!
# USTAR and Licor.SEy.Ustar are also exactly the same !!!!
# "Licor.SEG.MO.Stab", "Licor.SEG.Vstd", "Licor.SES.MO.Stab", "Licor.SES.Vstd" are ALL NAs !!!!!!

dropg<-c("WS", "WD", "USTAR", "Licor.SEG.MO.Stab", "Licor.SEG.Vstd"); 
drops<-c("WS", "WD", "USTAR", "Licor.SES.MO.Stab", "Licor.SES.Vstd"); 

dg = datg[,!(colnames(datg) %in% dropg)]
ds = dats[,!(colnames(dats) %in% drops)]


colnames(dg)<-c("datetime",    "FC_gm",       "FC_g1",      "FC_g2",        "FC_g3",        
                "FC_g4",        "LE_gm",      "LE_g1",      "LE_g2",        "LE_g3",       
                "LE_g4",        "H_gm",       "H_g1",       "H_g2",         "H_g3",        
                "H_g4",         "Ustar_gm",   "Ustar_g1",   "Ustar_g2",     "Ustar_g3",       
                "Ustar_g4",     "LW_IN_gm",   "LW_OUT_gm",  "NETRAD_gm",    "P_gm",                
                "PA_gm",        "PPFD_IN_gm", "P_F_gm",     "RH_gm",        "RH_F_gm",             
                "SW_IN_gm",     "SW_OUT_gm",  "TA_gm",      "TA_F_gm",      "TIMESTAMP_END_gm",    
                "TIMESTAMP_START_gm", "VPD_gm", "VPD_F_gm", "MO_Stab_g1",   "MO_Stab_g2",   
                "MO_Stab_g3",   "MO_Stab_g4", "WD_gm",      "WD_g1",        "WD_g2",        
                "WD_g3",      "WD_g4",      "WS_gm",        "WS_g1",        "WS_g2",        
                "WS_g3",      "WS_g4",      "Vstd_g1",      "Vstd_g2",      "Vstd_g3",      "Vstd_g4" )

colnames(ds)<-c("datetime",     "FC_sm",      "FC_s1",      "FC_s2",        "FC_s3",        
                "FC_s4",        "LE_sm",      "LE_s1",      "LE_s2",        "LE_s3",       
                "LE_s4",        "H_sm",       "H_s1",       "H_s2",         "H_g3",        
                "H_s4",         "Ustar_sm",   "Ustar_s1",   "Ustar_s2",     "Ustar_s3",       
                "Ustar_s4",     "LW_IN_sm",   "LW_OUT_sm",  "NETRAD_sm",    "P_sm",                
                "PA_sm",        "PPFD_IN_sm", "P_F_sm",     "RH_sm",        "RH_F_sm",             
                "SW_IN_sm",     "SW_OUT_sm",  "TA_sm",      "TA_F_sm",      "TIMESTAMP_END_sm",    
                "TIMESTAMP_START_sm", "VPD_sm", "VPD_F_sm", "MO_Stab_s1",   "MO_Stab_s2",   
                "MO_Stab_s3",   "MO_Stab_s4", "WD_sm",      "WD_s1",        "WD_s2",        
                "WD_s3",        "WD_s4",      "WS_sm",      "WS_s1",        "WS_s2",        
                "WS_s3",        "WS_s4",      "Vstd_s1",    "Vstd_s2",      "Vstd_s3",      "Vstd_s4" )


## ================================================ add date_time chron

#dat<-merge(dg, ds, by="datetime")  ##### aaaalllll rearranged !!!!
# range(as.numeric(dg[,1])-as.numeric(ds[,1]))    # 0 0 

dtimes <- as.character(dg[,1])
dtparts  <-  t(as.data.frame(strsplit(dtimes,' '))); row.names(dtparts) = NULL
if(unique(nchar(dtparts[,2]))==5)dtparts[,2] <- paste(dtparts[,2], ":00", sep="")
thetimes0  <-  chron(dates=dtparts[,1],times=dtparts[,2],format=c("d/m/y","h:m:s"))
dg$dt <- thetimes0


dtimes <- as.character(ds[,1])
dtparts  <-  t(as.data.frame(strsplit(dtimes,' '))); row.names(dtparts) = NULL
if(unique(nchar(dtparts[,2]))==5)dtparts[,2] <- paste(dtparts[,2], ":00", sep="")
thetimes0  <-  chron(dates=dtparts[,1],times=dtparts[,2],format=c("d/m/y","h:m:s"))
ds$dt <- thetimes0

# range(dg[,"dt"]-ds[,"dt"])  =#00:00:00 00:00:00




#-------------- 1.2 Read UNM csv data --------------


mg2018 <- read.csv(file=paste(mpath1, "US-Seg_HH_201801010000_201901010000_xtr_20200130.csv", sep=""), header=TRUE, sep=",")
mg2019 <- read.csv(file=paste(mpath1, "US-Seg_HH_201901010000_202001010000_xtr_20200130.csv", sep=""), header=TRUE, sep=",")
mg2020 <- read.csv(file=paste(mpath2, "US-Seg_HH_202001010000_202101010000_xtr.csv", sep=""), header=TRUE, sep=",")

ms2018 <- read.csv(file=paste(mpath1, "US-Ses_HH_201801010000_201901010000_xtr_20200130.csv", sep=""), header=TRUE, sep=",")
ms2019 <- read.csv(file=paste(mpath1, "US-Ses_HH_201901010000_202001010000_xtr_20200130.csv", sep=""), header=TRUE, sep=",")
ms2020 <- read.csv(file=paste(mpath2, "US-Ses_HH_202001010000_202101010000_xtr.csv", sep=""), header=TRUE, sep=",")


mg0 <- rbind(mg2018, mg2019, mg2020)
ms0 <- rbind(ms2018, ms2019, ms2020)


## ================================================ add date_time chron


#datm[datm==-9999] <- NA

date_string <- mg0[,"TIMESTAMP_START"]
yy <- substring(date_string, 3,4);  mm <- substring(date_string, 5,6); dd <- substring(date_string, 7,8)
hh <- substring(date_string, 9,10); min <- as.numeric(substring(date_string, 11,12))+15

mdates <- paste(dd,mm,yy, sep="/"); mtimes <- paste(hh, min, "00", sep=":")
mthetimes0  <-  chron(dates=mdates,times=mtimes,format=c("d/m/y","h:m:s"))
mg0$dt <- mthetimes0



date_string <- ms0[,"TIMESTAMP_START"]
yy <- substring(date_string, 3,4);  mm <- substring(date_string, 5,6); dd <- substring(date_string, 7,8)
hh <- substring(date_string, 9,10); min <- as.numeric(substring(date_string, 11,12))+15

mdates <- paste(dd,mm,yy, sep="/"); mtimes <- paste(hh, min, "00", sep=":")
mthetimes0  <-  chron(dates=mdates,times=mtimes,format=c("d/m/y","h:m:s"))
ms0$dt <- mthetimes0

# range(mg0$dt - ms0$dt)   # 00:00:00 00:00:00

mg <- mg0[,c("dt", "zoL", "V_SIGMA")]; colnames(mg) <- c("dt", "MO_Stab_gm", "Vstd_gm")
ms <- ms0[,c("dt", "zoL", "V_SIGMA")]; colnames(ms) <- c("dt", "MO_Stab_sm", "Vstd_sm")





#dat0<-merge(dg, ds, by="dt")   # ============================= merge 2 datasets


dat0 <- Reduce(function(x,y) merge(x=x, y=y, by="dt", all=T), 
               list(dg, ds, mg, ms))



# remove data preceding study period
dat0 <- dat0[dat0[,"dt"]>=date_start,]   
dat0 <- dat0[dat0[,"dt"]<=date_end,]   

# add month, year and hour columns
yyyy <- lubridate::year(dat0[,"dt"])
mon <- lubridate::month(dat0[,"dt"])
dd <- lubridate::day(dat0[,"dt"]); 
hh <- chron::hours(dat0[,"dt"])
min <- lubridate::minute(dat0[,"dt"])

dat <- cbind(dat0, min, hh, dd, mon, yyyy)


# switch around marcy's wind
dat[ is.na(dat[,"WD_gm"])  ,"WD_gm"] <- (-9999)
dat[ is.na(dat[,"WD_sm"])  ,"WD_sm"] <- (-9999)

mwg <- dat[,"WD_gm"]+180; sel_wg<-mwg>360; mwg[sel_wg]<-mwg[sel_wg]-360; dat[,"WD_gm"] <- mwg
mws < -dat[,"WD_sm"]+180; sel_ws<-mws>360; mws[sel_ws]<-mws[sel_ws]-360; dat[,"WD_sm"] <- mws

dat[ dat[,"WD_gm"]<(-1000)  ,"WD_gm"] <- NA
dat[ dat[,"WD_sm"]<(-1000)  ,"WD_sm"] <- NA




## kljun copyright: Copyright 2015, 2016, 2017, 2018 Natascha Kljun 
  
  source("E:/REC_7_Data/9_R/FFP_R/calc_footprint_FFP.R")
  source("E:/REC_7_Data/9_R/FFP_R/calc_footprint_FFP_climatology.R")

  datfoot0<-dat
  lb<-""
  time_f<-""    # can be "day_time", "night_time", ""
  AmeriFlux_f<-T
  xl<-440      # default
  
  if(time_f=="day_time")  {lb<-"_day";   xl<-250}
  if(time_f=="night_time"){lb<-"_night"; xl<-350}
  
  
  #wcols<-paste(rep( c("MO_stability", "sigma_V", "friction_velocity", "Wind_Dir"), length(datasets)),
   #            rep(datasets, each=4), sep="_") 
  
  # last AmeriFlux's corrected fluxes do not have the extra columns
  #if(AmeriFlux_f)wcols<-c(wcols, paste( rep( c("zoL", "V_SIGMA", "USTAR", "WD", "SW_IN" ), 2), 
   #                                     rep(mdatasets, each=5), sep="_"))
  
  
  wcols<-paste(rep( c("MO_Stab", "Vstd", "Ustar", "WD"), length(adatasets)),
                           rep(adatasets, each=4), sep="_") 
  
  wcols<-c(wcols, "SW_IN_gm", "SW_IN_sm")
  
  


  datfoot0[, c("MO_Stab_gm", "Vstd_gm", "MO_Stab_sm", "Vstd_sm")] <- datfoot0[, c("MO_Stab_g1", "Vstd_g1", "MO_Stab_s1", "Vstd_s1")]
  
  
  # remove all rows with some NAs
   datfoot0<-datfoot0[complete.cases(datfoot0[,wcols]),]  # 29236 from 35088
  
  #### land cover map ####
  
  ## land cover map (synthetic for development)
  # lcm<-matrix(1, ncol=(2*xl)+1, nrow=(2*xl)+1)
  # lcm[1:xl, (xl+1):((2*xl)+1)]<-2
  # lcm[(xl+1):((2*xl)+1), (xl+1):((2*xl)+1)]<-3
  # lcm<-apply(lcm, 2, rev); lcm<-t(lcm) # image.plot plots rotated images (FFP is right, so adjust lcm)
  # sel1<-lcm==1; sel2<-lcm==2; sel3<-lcm==3   
  
  
  if(T){ ## SEG - cover map
    
    # 0 = barren
    # 1 = shrubland
    # 2 = herbaceous
    # order of the categories in raster bands
    
    seg_tiff <- "data/land_cover_classifications/SEG.tif"
    segr <- raster(seg_tiff); segm0<-as.matrix(segr, nrow=955, ncol=937)
    segm <- t(apply(segm0, 2, rev))
    # re-orient because climatology's output is set for image.plot (starts filling from lower left corner)
    #image.plot(segm)   
    
    
    
    rr <- apply(segm, 2, sum_na)
    #sum(rr!=937)               # number of rows not entirely NA - 880
    #table(rr)                  # find row with min amount of data
    which(rr == 906)
    rrna0 <- which(rr == 937); rrna<-rrna0[rrna0 != 25]      # rows to remove
    selr <- !(1:length(rr) %in% rrna)
    
    cc <- apply(segm, 1, sum_na)
    #sum(cc!=955)               # number of cols not entirely NA - 880
    #table(cc)                  # find row with min amount of data
    #which(cc == 921)
    ccna0 <- which(cc == 955); ccna<-ccna0[ccna0 != 14]     # cols to remove
    selc <- !(1:length(cc) %in% ccna)
    
    segmap<-segm[selc, selr]
    
    
    ### cover map partitioning
    round( sum(segmap==0, na.rm=T)/sum(!is.na(segmap)) , 3)   # 0.779    / bare        
    round( sum(segmap==1, na.rm=T)/sum(!is.na(segmap)) , 3)   # 0.004    / shrubs      
    round( sum(segmap==2, na.rm=T)/sum(!is.na(segmap)) , 3)   # 0.217    / herbaceous  
    
  }  ## SEG - cover map
  
  
  if(T){ ## SES - cover map
    
    # 0 = barren
    # 1 = shrubland
    # 2 = herbaceous
    # order of the categories in raster bands
    
    
    ses_tiff <- "data/land_cover_classifications/SEG.tif"
    sesr <- raster(ses_tiff); sesm0<-as.matrix(sesr, nrow=994, ncol=929)
    sesm <- t(apply(sesm0, 2, rev))
    # re-orient because climatology's output is set for image.plot (starts filling from ll corner)
    # image.plot(sesm)   
    
    
    
    
    
    rr <- apply(sesm, 2, sum_na)  # find how many empty rows/cols there are here
    #sum(rr!=929)               # number of rows not entirely NA - 880
    #table(rr)                  # find row with min amont of data
    #which(rr == 892)
    rrna0 <- which(rr == 929); rrna<-rrna0[rrna0 != 941]      # rows to remove
    selr <- !(1:length(rr) %in% rrna)
    
    cc<-apply(sesm, 1, sum_na)
    #sum(cc!=994)               # number of cols not entirely NA - 880
    #table(cc)                  # find row with min amont of data
    #which(cc == 984)
    ccna0 <- which(cc == 994); ccna<-ccna0[ccna0 != 14]     # cols to remove
    selc <- !(1:length(cc) %in% ccna)
    
    sesmap<-sesm[selc, selr]
    
    
    ### cover map partitioning
    round( sum(sesmap==0, na.rm=T)/sum(!is.na(sesmap)) , 3)   # 0.667    / bare
    round( sum(sesmap==1, na.rm=T)/sum(!is.na(sesmap)) , 3)   # 0.234    / shrubs
    round( sum(sesmap==2, na.rm=T)/sum(!is.na(sesmap)) , 3)   # 0.099    / herbaceous
    
    
    
    
  }  ## SES - cover map
  
  
 
if(T){  
  
  
  # ===================================================================================== #
  ###### hourly footprint for clusters / probability sum for clusters and towers ######
  # ===================================================================================== #
  
  
  mon_ys <- unique(datfoot0[,c("yyyy", "mon")])
  datfoot_t <- datfoot0
  abort_plot <- F
  
  
  for(k in 1:nrow(mon_ys)){  # loops over different months   ////  nrow(mon_ys)
    
    datfoot <- datfoot_t[  datfoot_t[,"yyyy"]==mon_ys[k,"yyyy"] & datfoot_t[,"mon"]==mon_ys[k,"mon"] , ]
    
    pmat <- matrix(NA, ncol=5+5*12, nrow=nrow(datfoot))
    colnames(pmat) <- c("yyyy", "mon", "dd", "hh", "min",
                        paste( rep( c("pt", "p1", "p2", "p3", "ps"), 12), # 10 towers and 2 clusters
                               rep( c(adatasets[1:10], sites), each=5), sep="_" )  )
    pmat_80 <- pmat
    
    
    for(i in 1:nrow(datfoot)){     # loops over different half-hourly data / nrow(datfoot)
      
      
      #i <- 4   # first that does not abort
      
      print(paste(i, "on a total of", nrow(datfoot)))
      
      for(cluster in sites){
        
        
        ### assign sel1, sel2 and sel3 to the right XXXmap
        if(cluster=="SEG"){sel1 <- segmap==0; sel2 <- segmap==1; sel3 <- segmap==2}
        if(cluster=="SES"){sel1 <- sesmap==0; sel2 <- sesmap==1; sel3 <- sesmap==2}
        
        
        #cluster <- sites[1]
        dts <- c(1:4, 9)
        canopy_h <- 0.5
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
          
          
          #if(!(dt %in% c(9,10))){
            FFP <-  calc_footprint_FFP_climatology(
              zm=   rep(6 - (0.67 * canopy_h), length(i)),    # Meas h. above displacement h.  
              z0=   rep(     0.15 * canopy_h,  length(i)),    # Roughness length
              umean=NA,
              h=    rep(1000, length(i)),             # PBL depth (set always at 1000)
              ol=   6 / datfoot[i,paste("MO_Stab", adatasets[dt], sep="_")],
              sigmav=   datfoot[i,paste("Vstd", adatasets[dt], sep="_")],
              ustar=    datfoot[i,paste("Ustar", adatasets[dt], sep="_")],
              wind_dir= datfoot[i,paste("WD", adatasets[dt], sep="_")],
              domain=c(-xl,xl,-xl,xl),  # x and y corners in m
              nx=2*xl,                      # 2x2 m each as default
              r=seq(10,80,10), smooth_data=1)
            
            
            
            
            if(length(FFP$fclim_2d)==1){abort_plot <- T; break}
            
            
            #### translate SEG 1:4 and lines with respect to US-SEX (distances from GIS)
            if(mt3=="SEG_REC1"){xt <- (-15); yt <-   15 }
            if(mt3=="SEG_REC2"){xt <-  (-6); yt <-   81 }
            if(mt3=="SEG_REC3"){xt <-  (72); yt <- (-38)}
            if(mt3=="SEG_REC4"){xt <- (-76); yt <- (-32)}
            if(mt3=="SEG"){xt <- yt <- 0}
            
            if(mt3=="SES_REC1"){xt <-  (15); yt <-  (-5)}
            if(mt3=="SES_REC2"){xt <-   (4); yt <-   83 }
            if(mt3=="SES_REC3"){xt <-  (70); yt <- (-44)}
            if(mt3=="SES_REC4"){xt <- (-72); yt <- (-38)}
            if(mt3=="SES"){xt <- yt <- 0}
            
            FFP$x_2d <- FFP$x_2d+xt; FFP$y_2d <- FFP$y_2d+yt; 
            for(j in 1:length(FFP$xr)){FFP$xr[[j]] <- FFP$xr[[j]]+xt; FFP$yr[[j]] <- FFP$yr[[j]]+yt}
            
          #}
          
          
          
          # if(dt %in% c(9,10)){
          #   FFP <-  calc_footprint_FFP_climatology(
          #     zm=   rep(3 - (0.67 * canopy_h), length(i)),    # Meas h. above displacement h.  
          #     z0=   rep(     0.15 * canopy_h,  length(i)),    # Roughness length
          #     umean=NA,
          #     h=    rep(1000, length(i)),             # PBL depth (set always at 1000)
          #     ol=     3/datfoot[i,paste("zoL", adatasets[dt], sep="_")],
          #     sigmav=   datfoot[i,paste("V_SIGMA", adatasets[dt], sep="_")],
          #     ustar=    datfoot[i,paste("USTAR", adatasets[dt], sep="_")],
          #     wind_dir= datfoot[i,paste("WD", adatasets[dt], sep="_")],
          #     domain=c(-xl,xl,-xl,xl),  # x and y corners in m
          #     nx=2*xl,                      # 2x2 m each as default
          #     r=seq(10,80,10), smooth_data=1)
          #   
          #   xt <- yt <- 0
          # }
          
          
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
          
          
          # calculate probabilities of entire footprint (not used in analysis)
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
        
        
        
        ### Plot 
        if(F){
          
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
        
        
        
        
        ## land cover probability (cluster) =================
        
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
    
    
    save(pmat, file = paste("E:/REC_7_Data/9_R/Rdata/foot_data_20220211/", "pmat_", 
                            mon_ys[k,"yyyy"], "_", mon_ys[k,"mon"], ".Rdata", sep=""))
    
    save(pmat_80, file = paste("E:/REC_7_Data/9_R/Rdata/foot_data_20220211/", "pmat_80_", 
                               mon_ys[k,"yyyy"], "_", mon_ys[k,"mon"], ".Rdata", sep=""))
    
  }
  
  
  
  
} # Kljun climatology for 2 clusters (30 min resolution)













###
### 2.0 Land cover maps ####
###

# if(T){
#   
#   # land cover prob barplot
#   
#   mon_ys <- unique(dat[,c("yyyy", "mon")])
#   rpath <- "E:/REC_7_Data/9_R/Rdata/foot_data_20220211/"
#   pmat <- NULL
#   
#   for(i in 1:nrow(mon_ys)){
#     load(paste(rpath, "pmat_80_", mon_ys[i, "yyyy"], "_", mon_ys[i, "mon"], ".RData", sep="")) # load "pmat_80
#     pmat <- rbind(pmat, pmat_80)
#   }
#   
#   pcols <- paste( rep(c("p1", "p2", "p3"), 10), rep(adatasets[1:10], each=3), sep="_" )
#   
#   pv <- apply(pmat[,pcols], 2, mean, na.r=T)
#   pb <- matrix(pv, nrow=3)
#   colnames(pb) <- adatasets[1:10]
#   rownames(pb) <- c("p1", "p2", "p3")
#   pb <- pb[,c(9, 1:4, 10, 5:8)]
#   
#   ps <- apply(pmat[,pcols], 2, sd, na.r=T)
#   pa <- matrix(ps, nrow=3)
#   colnames(pa) <- adatasets[1:10]
#   rownames(pa) <- c("p1", "p2", "p3")
#   pa <- pa[,c(9, 1:4, 10, 5:8)]
#   
#   
#   cols <- c("darkgreen", "olivedrab3", "moccasin")
#   
#   
#   
#   #### reshape data
#   pbi <- pb[c(2, 3, 1), 10:1]
#   pai <- pa[c(2, 3, 1), 10:1]
#   
#   bplot <- "E:/REC_7_Data/10_Plots/9_footprints/"
#   filenm <- paste(bplot, "land_cover_probabilities_all_year_h_EC0_wind_repro.png", sep="")
#   
#   png(filenm, width=900, height=900)
#   par(mar = c(8, 13, 2, 2), mgp=c(2, 2, 0), tck=-0.01)
#   
#   pbi_h <- barplot(pbi, las=1, beside=T, xlim=c(0, 1), cex.names=3, cex.axis=3, col=cols, horiz=T,
#                    names.arg = c("Ses EC4", "Ses EC3", "Ses EC2", "Ses EC1", "Ses EC0", 
#                                  "Seg EC4", "Seg EC3", "Seg EC2", "Seg EC1", "Seg EC0"))
#   arrows(x0=pbi+pai, y0=pbi_h, x1=pbi-pai, y1=pbi_h, length=0, code=3, lwd=3)
#   mtext("Probability", 1, line=5, cex=3)
#   abline(v=0.8, col="grey", lty=3)
#   box()
#   legend("bottomright", legend=c("Bare Ground", "Herbaceous", "Shrubs"), col=rev(cols), pch=16, cex=2.5)
#   
#   dev.off()
#   
#   
#   
#   
#   
# }  # land cover prob barplot

   
footprint_plot <- ggplot(data=footprint_df, 
                         aes(fill=Landcover, 
                             y=Proportion, 
                             x=reorder(Station, desc(Station)))) + 
     geom_col(position="fill") +
     scale_fill_manual(values=c("moccasin",  "olivedrab3", "darkgreen")) +
     coord_flip() +
     labs(y = expression("Landcover Proportion"),
          x = expression("EC System (80% footprint)")
          ) +
     theme_fancy() + 
     theme(legend.position="top",
          legend.title = element_blank()) 
    
   ## save raster
   ggsave(footprint_plot,
          filename = "plots/footprint.png",
          width = 15,
          height = 15,
          units = "cm")
   
   # save vector
   ggsave(footprint_plot,
          filename = "plots/footprint.pdf",
          width = 15,
          height = 15,
          units = "cm")
   
      
   