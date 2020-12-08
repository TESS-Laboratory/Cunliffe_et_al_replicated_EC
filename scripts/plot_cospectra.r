



rm(list = ls())
library(chron); #library(oce); #library(ggplot2); library(gridExtra); #
#library(lubridate) # only for 'month' function
path   <-"E:/REC_7_Data/8_datasets/"
gpath <-"E:/REC_7_Data/10_Plots/cospectra/"
gpatho<-"E:/REC_7_Data/10_Plots/cospectra/X_output/"
sites<-c("SEG", "SES"); recs<-1:4; towers<-paste(rep(sites, each=4), rep(recs,2), sep="_REC")
sites_ids<-c(sites, towers[c(1, 5)])
specs_ids<-c("h", "l", "nd", "nn", "t")
fluxes<-c("H", "LE", "NEE_day", "NEE_night", "T")


#last_date<-"2019_10_01_from_flash_"
#last_date<-"2019_05_06_telemetry_"


last_datem<-""
last_date<-"2019_10_01_from_flash_Txcor_512_"   # format: 'yyyy_mm_dd_'
last_dates<-c(last_datem, last_datem, last_date, last_date)



selRH<-"c"     # can be "c" (cell) or "r" (RH block, beside heating)

datasets<-c("g1", "g2", "g3", "g4", "s1", "s2", "s3", "s4"); mdatasets<-c("gm", "sm")
sdatasets<-c(mdatasets, "g1", "s1")


xl<-c(2:10 %o% 10^(-4:1))    # create log sequence across different order of magnitude
lxl<-length(xl)

time_f<-""    # can be "day_time", "night_time", ""
lb<-"dns"          # dns == "day-night split"     adjusted later
tf<-"ind_timestamp_"


common_time<-T        # execute step 2)
seg_all<-T            # uses all raw data from US-SEG
seg_5<-F              # uses a subsample of seg_all to compare with original analysis over 5 months (Dec 2018 - Jan, Jun, Jul, Aug 2019)


is<-4






#### 1) Assign Flux and cospectra datasets =================


covm<-matrix(NA, ncol=5, nrow=4);
colnames(covm)<-c("covh", "covl", "covnd", "covnn", "covt")
rownames(covm)<-sdatasets

covm[1, ]<-c(0.008, 0.001, 0.269, 0.269, 21)
covm[2, ]<-c(0.006, 0.001, 0.353, 0.353, 10)
covm[3, ]<-c(0.007, 0.001, 0.003, 0.003, 50)
covm[4, ]<-c(0.006, 0.001, 0.004, 0.004, 74)









if(seg_all){
  gpatho<-"E:/REC_7_Data/10_Plots/cospectra/X_output_SEG_all/"
  
  is<-3; sdatasets<-c("ss1", "ss2", "ss3")   # sensor switches: 2018-12-11, and 2019-07-17
  sites_ids<-paste("all_SEG_", sdatasets, sep="")
  begs<-c("01/10/2018", "12/12/2018", "18/07/2019")
  ends<-c("11/12/2018", "17/07/2019", "18/01/2020")
  
  flx0<-read.csv(file=paste(path, "all_drivingc_SEG_marcy_flux.csv", sep=""), header=TRUE, sep=",")
  fn<-paste(path, "all_drivingc_SEG_marcy_spec.csv", sep="")
  
  header = read.csv(file=fn, skip = 1, header = F, nrows = 1, as.is = T)
  dat0<-read.csv(file=fn, skip=2, header=F, sep=","); 
  colnames(dat0)<-header; colnames(dat0)[2:5]<-c("Type", "Sorted", "Signal", "Co/Var")
  
  ## sometimes, empty factor columns that are not visible in excel are added
  flx0<-flx0[, !grepl("X", colnames(flx0))  ]
  dat0<-dat0[, !grepl("NA", colnames(dat0))  ]
  
  
  ### sometimes the 2 data frames have different date format (not visible in excel) 
  dtimes<-as.character(flx0[,1])
  dtparts <- t(as.data.frame(strsplit(dtimes,' '))); row.names(dtparts) = NULL
  if(unique(nchar(dtparts[,2]))==5)dtparts[,2]<-paste(dtparts[,2], ":00", sep="")
  thetimes_flx <- chron(dates=dtparts[,1],times=dtparts[,2],format=c("d/m/y","h:m:s"))
  flx0$dt<-thetimes_flx
  
  dtimes<-as.character(dat0[,1])
  dtparts <- t(as.data.frame(strsplit(dtimes,' '))); row.names(dtparts) = NULL
  if(unique(nchar(dtparts[,2]))==5)dtparts[,2]<-paste(dtparts[,2], ":00", sep="")
  thetimes_dat <- chron(dates=dtparts[,1],times=dtparts[,2],format=c("d/m/y","h:m:s"))
  dat0[,1]<-thetimes_dat; colnames(dat0)[1]<-"dt"
  
  
  
  covm<-matrix(NA, ncol=5, nrow=3);
  colnames(covm)<-c("covh", "covl", "covnd", "covnn", "covt")
  rownames(covm)<-sdatasets
  
  covm[1, ]<-c(0.003, 0.001, 0.284, 0.269, 15)
  covm[2, ]<-c(0.005, 0.001, 0.326, 0.353, 15)
  covm[3, ]<-c(0.006, 0.001, 0.516, 0.003, 17)

}




if(seg_5){
  is<-1; sites_ids<-"all_SEG_5"
}







### 
### 1.1) Main loop   =================
###

for(i in 1:is){   # default: 1:is
  #i<-4
  stat<-"median"     # can be "mean" or "median"
  self<-selc<-T
  
  
  if(!seg_all){    # gm, sm, g1, s1 analysis
    
    if(i %in% c(1,2)){
      flx<-read.csv(file=paste(path, last_dates[i], "five_months_", sites[i], "_marcy_flux.csv", sep=""), header=TRUE, sep=",")
      fn<-paste(path, last_dates[i], "five_months_", sites[i], "_marcy_spec.csv", sep="")
    }
    if(i %in% c(3,4)){
      flx<-read.csv(file=paste(path, last_dates[i], sites[i-2], "_REC1_flux.csv", sep=""), header=TRUE, sep=",")
      fn<-paste(path, last_dates[i], sites[i-2], "_REC1_spec.csv", sep="")
    }
    header = read.csv(file=fn, skip = 1, header = F, nrows = 1, as.is = T)
    dat<-read.csv(file=fn, skip=2, header=F, sep=","); 
    colnames(dat)<-header; colnames(dat)[2:5]<-c("Type", "Sorted", "Signal", "Co/Var")
    
    
    
    #dim(dat)  # 17280    36
    #dim(flx)  # 2880     51      2880*6 = 17280  (it is *8 for RECs)
    
    ## sometimes, empty factor columns that are not visible in excel are added
    flx<-flx[, !grepl("X", colnames(flx))  ]
    dat<-dat[, !grepl("NA", colnames(dat))  ]
    
    
    ### sometimes the 2 data frames have different date format (not visible in excel) 
    dtimes<-as.character(flx[,1])
    dtparts <- t(as.data.frame(strsplit(dtimes,' '))); row.names(dtparts) = NULL
    if(unique(nchar(dtparts[,2]))==5)dtparts[,2]<-paste(dtparts[,2], ":00", sep="")
    thetimes_flx <- chron(dates=dtparts[,1],times=dtparts[,2],format=c("d/m/y","h:m:s"))
    flx$dt<-thetimes_flx
    
    dtimes<-as.character(dat[,1])
    dtparts <- t(as.data.frame(strsplit(dtimes,' '))); row.names(dtparts) = NULL
    if(unique(nchar(dtparts[,2]))==5)dtparts[,2]<-paste(dtparts[,2], ":00", sep="")
    thetimes_dat <- chron(dates=dtparts[,1],times=dtparts[,2],format=c("d/m/y","h:m:s"))
    dat[,1]<-thetimes_dat; colnames(dat)[1]<-"dt"
    
    
    
    if(i %in% c(3,4)){
      dat<-dat[   (dat[,"dt"]>="01/12/18"  & dat[,"dt"]<"01/02/19")  | (dat[,"dt"]>="01/06/19"  & dat[,"dt"]<"01/09/19")  ,]
      flx<-flx[   (flx[,"dt"]>="01/12/18"  & flx[,"dt"]<"01/02/19")  | (flx[,"dt"]>="01/06/19"  & flx[,"dt"]<"01/09/19")  ,]
    }
    
    
    
  }
  
  
  
  
  
  if(seg_all & !seg_5){    # loop over three different sensor switches
    dat<-dat0[     dat0[,"dt"]>=begs[i] & dat0[,"dt"]<ends[i]   ,]
    flx<-flx0[     flx0[,"dt"]>=begs[i] & flx0[,"dt"]<ends[i]   ,]
  }
  
  if(seg_5){
    dat<-dat0[   (dat0[,"dt"]>="01/12/18"  & dat0[,"dt"]<"01/02/19")  | (dat0[,"dt"]>="01/06/19"  & dat0[,"dt"]<"01/09/19")  ,]
    flx<-flx0[   (flx0[,"dt"]>="01/12/18"  & flx0[,"dt"]<"01/02/19")  | (flx0[,"dt"]>="01/06/19"  & flx0[,"dt"]<"01/09/19")  ,]
  }
  
  
  
  
  #assign(paste(sdatasets[i], "s", sep=""), flx)
  #assign(paste(sdatasets[i], "0s", sep=""), dat)
#}  
  
  
  
  
  if(i %in% c(1,3) & ! exists("gm")){
    load("E:/REC_7_Data/9_R/Rdata/gm.RData") 
    sw<-gm[,c("dt", "SW_IN_gm")]; colnames(sw)<-c("dt", "SW_IN")
  } 
  if(i %in% c(2,4) & ! exists("sm")){
    load("E:/REC_7_Data/9_R/Rdata/sm.RData") 
    sw<-sm[,c("dt", "SW_IN_sm")]; colnames(sw)<-c("dt", "SW_IN")
  }
  
  
  
  #flx2<-merge(flx, sw, by="dt")
  #if(time_f=="day_time")  {self<-flx2[,"SW_IN"]!=0; self[is.na(self)]<-F;  selc<-rep(self, each=6); lb<-"day"  } # get rid of NAs in the self
  #if(time_f=="night_time"){self<-flx2[,"SW_IN"]==0; self[is.na(self)]<-F;  selc<-rep(self, each=6); lb<-"night"}

  
  if(any(is.na(flx[,"mean_Wind_Spd"]))){self<-self*!is.na(flx[,"mean_Wind_Spd"]); selc<-rep(self, each=6)}
  
  
  flx_f<-flx[self,]; dat_f<-dat[selc, ] # filter to day/night if needed
  
  
  
  cn<-header[6:35]   # this is the natural frequency;  Must be turned into NORMALIZED!

  ##### fn=f*(z-d/u); # d = displacement high
  canopy_h<-0.5; if(!seg_all & (i %in% c(2, 4)))canopy_h<-1.0
  z<-3; if(!seg_all & (i %in% c(3, 4)))z<-6
  d<-0.67 * canopy_h
  
  
  #paste(unique(dat$Signal))   
  # Marcy:" Ts_QiQivCtv" " Uz_RTs_QiQivCtv" " h2o_Ci"      " Uz_Rh2o_Ci"      " co2_Cgc"      " Uz_Rco2_Cgc" 
  # REC:  " gT_QiQivCtv" " gW_RgT_QiQivCtv" " rH2O_CgcLr"  " gW_RrH2O_CgcLr"  " cH2O_CgcLr"   " gW_RcH2O_CgcLr"  " vCO2_Lr"   " gW_RvCO2_Lr" 
  
  if(i %in% c(1, 2) | seg_all){
    co_h<-dat_f[ dat_f$Signal==" Uz_RTs_QiQivCtv"   ,]
    co_l<-dat_f[ dat_f$Signal==" Uz_Rh2o_Ci"   ,]
    co_n<-dat_f[ dat_f$Signal==" Uz_Rco2_Cgc"   ,]
    co_t<-dat_f[ dat_f$Signal==" Ts_QiQivCtv"   ,]
  } else {
    co_h<-dat_f[ dat_f$Signal==" gW_RgT_QiQivCtv"   ,]
    co_l<-dat_f[ dat_f$Signal==" gW_RcH2O_CgcLr"   ,]
    co_n<-dat_f[ dat_f$Signal==" gW_RvCO2_Lr"   ,]
    co_t<-dat_f[ dat_f$Signal==" gT_QiQivCtv"   ,]
  }
  
  
  
  
  # matrices to be filled with same-reference x-axis
  nh<-matrix(NA, nrow=nrow(co_h), ncol=lxl); colnames(nh)<-xl
  nt<-nl<-nn<-nh
  
  
  for(j in 1:nrow(co_h)){
    #j<-100
    if(j%%100==0)print(j)
    #if(is.na(flx_f[j,"mean_Wind_Spd"]))next

    if(is.na(co_h[j, "Co/Var"]) | is.na(co_l[j, "Co/Var"]) | is.na(co_n[j, "Co/Var"]) |
       co_h[j, "Co/Var"]==0     | co_l[j, "Co/Var"]==0     | co_n[j, "Co/Var"]==0     |
       is.na(flx_f[j,"mean_Wind_Spd"])) next
    
        
    x<-unlist(cn * (z-d) / as.numeric(flx_f[j,"mean_Wind_Spd"]) ) ### a data frame is a list of columns !!!!!!
    
    #if(is.na(co_h[j, "Co/Var"]) | is.na(co_l[j, "Co/Var"]) | is.na(co_n[j, "Co/Var"]))next
    #if(co_h[j, "Co/Var"]==0     | co_l[j, "Co/Var"]==0     | co_n[j, "Co/Var"]==0)    next
      
      yh<-unlist(co_h[j, 6:35])
      yl<-unlist(co_l[j, 6:35])
      yn<-unlist(co_n[j, 6:35])
      yt<-unlist(co_t[j, 6:35])
      
      
      # give a common reference
      ap_h<-approx(x=x, y=yh, xout=xl)
      ap_l<-approx(x=x, y=yl, xout=xl)
      ap_n<-approx(x=x, y=yn, xout=xl)
      ap_t<-approx(x=x, y=yt, xout=xl)
      
      # fill common-frame matrices
      nh[j,]<-ap_h$y
      nl[j,]<-ap_l$y
      nn[j,]<-ap_n$y
      nt[j,]<-ap_t$y
    
  }
  
  
  
  # add column with Co/Var
  nhc<-cbind(nh, co_h[,"Co/Var"]); colnames(nhc)[ncol(nhc)]<-"Co/Var"
  nlc<-cbind(nl, co_l[,"Co/Var"]); colnames(nlc)[ncol(nlc)]<-"Co/Var"
  nnc<-cbind(nn, co_n[,"Co/Var"]); colnames(nnc)[ncol(nnc)]<-"Co/Var"
  ntc<-cbind(nn, co_t[,"Co/Var"]); colnames(ntc)[ncol(ntc)]<-"Co/Var"
  
  # add column with Co/Var
  ntc<-cbind(ntc, flx[,"Hc"]); colnames(ntc)[ncol(ntc)]<-"Hc"
  
  
  
  # sort by Co/Var
  nhs<-nhc[order(abs(nhc[,"Co/Var"])),]
  nls<-nlc[order(abs(nlc[,"Co/Var"])),]
  nns<-nnc[order(abs(nnc[,"Co/Var"])),]
  #nts<-ntc[order(abs(ntc[,"Co/Var"])),]
  #nts<-ntc[order(abs(flx[,"Hc"])),]    # T is a SPECTRA, so high VAR may imply high error, unlike COVAR
  nts<-ntc[order(abs(ntc[,"Hc"])),]
  
  
  
  
  ### plot individual spectra with Co/Var each 200 positions
  for(j in 1:nrow(co_h)){
    if(j%%200==0){
      
      ppath<-paste("E:/REC_7_Data/10_Plots/cospectra/", sites_ids[i], "/", sep="")
  
      
      ## H cospectra
      if(lxl > sum(is.na(nhs[j, 1:54]))){
        png(paste(ppath, "H/H_ind_cospectra_", j, ".png", sep=""), width=600, height=600)
        plot(xl, nhs[j, 1:54], type="l", log="x", main=sites[i])
        abline(v=1, lty=3, col="grey")
        abline(h=0, lty=1, col="grey")
        mtext(   paste("Cov:", round(abs(nhs[j, "Co/Var"]), 3)   ), 3, line=-2, adj=0.95)
        mtext(   paste("j:", j, "/", nrow(nns)),                    3, line=-3, adj=0.95)
        dev.off()
      }
      
      ## LE cospectra
      if(lxl > sum(is.na(nls[j, 1:54]))){
        png(paste(ppath, "LE/LE_ind_cospectra_", j, ".png", sep=""), width=600, height=600)
        plot(xl, nls[j, 1:54], type="l", log="x", main=sites[i])
        abline(v=1, lty=3, col="grey")
        abline(h=0, lty=1, col="grey")
        mtext(   paste("Cov:", round(abs(nls[j, "Co/Var"]), 3)   ), 3, line=-2, adj=0.95)
        mtext(   paste("j:", j, "/", nrow(nns)),                    3, line=-3, adj=0.95)
        dev.off()
      }
      
      ## NEE cospectra
      if(lxl > sum(is.na(nns[j, 1:54]))){
        png(paste(ppath, "NEE/NEE_ind_cospectra_", j, ".png", sep=""), width=600, height=600)
        plot(xl, nns[j, 1:54], type="l", log="x", main=sites[i])
        abline(v=1, lty=3, col="grey")
        abline(h=0, lty=1, col="grey")
        mtext(   paste("Cov:", round(abs(nns[j, "Co/Var"]), 3)   ), 3, line=-2, adj=0.95)
        mtext(   paste("j:", j, "/", nrow(nns)),                    3, line=-3, adj=0.95)
        dev.off()
      }
      
      ## T cospectra
      if(lxl > sum(is.na(nts[j, 1:54]))){
        png(paste(ppath, "T/T_ind_cospectra_", j, ".png", sep=""), width=600, height=600)
        plot(xl, nts[j, 1:54], type="l", log="x", main=sites[i])
        abline(v=1, lty=3, col="grey")
        abline(h=0, lty=1, col="grey")
        #mtext(   paste("Cov:", round(abs(nts[j, "Co/Var"]), 3)   ), 3, line=-2, adj=0.95)
        #mtext(   paste("H:", round(   sort(abs(flx[, "Hc"])), 0)[j], "W/m2"   ), 3, line=-2, adj=0.95)
        mtext(   paste("H:", round(abs(nts[j, "Hc"]), 3)   ), 3, line=-2, adj=0.95)
        mtext(   paste("j:", j, "/", nrow(nns)),                    3, line=-3, adj=0.95)
        dev.off()
      }
      
      
      
      
    }
  }
  
  
  
  if(F){   # min covariance for !seg_all
    
    ### SEG:
    ###    H:  0.008 (j=2200)
    ###    LE: 0.001 (j=2800)
    ###    NEE:0.269 (j=1600)  
    ###    T:  21 W/m2 (j=2600)
    
    ### SES:
    ###    H:  0.006 (j=1800)
    ###    LE: 0.001 (j=2000)
    ###    NEE:0.353 (j=1600)    
    ###    T:  10 W/m2 (j=1600)
    
    ### SEG_REC1:
    ###    H:  0.007 (j=2400)
    ###    LE: 0.001 (j=3400)
    ###    NEE:0.022 (j=5600)
    ###    T:  50 W/m2 (j=4400)
    
    ### SES_REC1:
    ###    H:  0.006 (j=2400)
    ###    LE: 0.001 (j=3400)
    ###    NEE:0.024 (j=6200)
    ###    T:  36 w/m2 (j=4000)
    
  }  # min covariance for !seg_all
  
  if(F){  # min covariance for seg_all
    
    ### ss1:
    ###    H:  0.007 (j=600)
    ###    LE: 0.001 (j=1000)
    ###    NEE:0.388 (j=600)  
    ###    T:  9 W/m2 (j=400)
    
    ### ss2:
    ###    H:  0.005 (j=2000)
    ###    LE: 0.001 (j=2600)
    ###    NEE:0.326 (j=1800)    
    ###    T:  15 W/m2 (j=2400)
    
    ### ss3:
    ###    H:  0.006 (j=2200)
    ###    LE: 0.001 (j=2800)
    ###    NEE:0.516 (j=2800)
    ###    T:  17 W/m2 (j=2800)
    
  }  # min covariance for seg_all
  
  
  
  # min covariances for filter
  if(!seg_all | seg_5){
    vvi<-"vv7_"      # version; cfr COV thresholds.txt in gpath
    if(i==1){ covh<-0.008;  covl<-0.001; covn<-0.269; ht<-21}
    if(i==2){ covh<-0.006;  covl<-0.001; covn<-0.353; ht<-10}
    if(i==3){ covh<-0.007;  covl<-0.001; covn<-0.003; ht<-50}
    if(i==4){ covh<-0.006;  covl<-0.001; covn<-0.004; ht<-36}
  } else {
    vvi<-"vv1_"      # version; cfr COV thresholds SEG all.txt in gpath
    if(i==1){ covh<-0.003;  covl<-0.001; covn<-0.284; ht<-15} # vv1
    #if(i==1){ covh<-0.007;  covl<-0.001; covn<-0.388; ht<-9} # vv2
    if(i==2){ covh<-0.005;  covl<-0.001; covn<-0.326; ht<-15}
    if(i==3){ covh<-0.006;  covl<-0.001; covn<-0.516; ht<-17}
  }
  
  
  
  
  
  
  # add dt to datasets
  dth<-as.data.frame(nhc); dth$dt<-flx$dt
  dtl<-as.data.frame(nlc); dtl$dt<-flx$dt
  dtn<-as.data.frame(nnc); dtn$dt<-flx$dt
  dtt<-as.data.frame(ntc); dtt$dt<-flx$dt
  
  # apply filter - only positive cov for day time 
  dth_f <-dth[ dth[,"Co/Var"]>=covh ,]
  dtl_f <-dtl[ dtl[,"Co/Var"]>=covl ,]
  dtn_fd<-dtn[ dtn[,"Co/Var"]>=covn ,]
  dtn_fn<-dtn[ dtn[,"Co/Var"]<=(-covn) ,]     # negative for NEE night time
  #dtt_f<-dtt[ sort(abs(flx[, "Hc"])) >= ht ,]
  dtt_f <-dtt[ dtt[,"Hc"]>=ht ,]
  
  
  assign(paste("sh", sdatasets[i], sep="_"), dth_f)
  assign(paste("sl", sdatasets[i], sep="_"), dtl_f)
  assign(paste("snd", sdatasets[i], sep="_"), dtn_fd)
  assign(paste("snn", sdatasets[i], sep="_"), dtn_fn)
  assign(paste("st", sdatasets[i], sep="_"), dtt_f)
  
  # assign to perform Rob's test for NEE
  assign(paste("dds", sdatasets[i], sep="_"), list(dth, dtn))
  

}










### 2) Find common timestamp for different fluxes ===================

if(common_time & !seg_all & !seg_5){
  
  tf<-""
  
  dt_H  <-Reduce(intersect, list(sh_gm[,"dt"], sh_sm[,"dt"], sh_g1[,"dt"], sh_s1[,"dt"]))
  dt_LE <-Reduce(intersect, list(sl_gm[,"dt"], sl_sm[,"dt"], sl_g1[,"dt"], sl_s1[,"dt"]))
  dt_NEEd<-Reduce(intersect, list(snd_gm[,"dt"], snd_sm[,"dt"], snd_g1[,"dt"], snd_s1[,"dt"]))
  dt_NEEn<-Reduce(intersect, list(snn_gm[,"dt"], snn_sm[,"dt"], snn_g1[,"dt"], snn_s1[,"dt"]))
  dt_T  <-Reduce(intersect, list(st_gm[,"dt"], st_sm[,"dt"], st_g1[,"dt"], st_s1[,"dt"]))
  
  
  ### filter datasets to flux-specific common timestamp
  ### H
  sh_gm<-sh_gm[  as.numeric(sh_gm[,"dt"]) %in% dt_H  ,]    # range(as.numeric(sh_gm2[,"dt"])-dt_H)
  sh_sm<-sh_sm[  as.numeric(sh_sm[,"dt"]) %in% dt_H  ,]
  sh_g1<-sh_g1[  as.numeric(sh_g1[,"dt"]) %in% dt_H  ,]
  sh_s1<-sh_s1[  as.numeric(sh_s1[,"dt"]) %in% dt_H  ,]
  ### LE
  sl_gm<-sl_gm[  as.numeric(sl_gm[,"dt"]) %in% dt_LE  ,] 
  sl_sm<-sl_sm[  as.numeric(sl_sm[,"dt"]) %in% dt_LE  ,] 
  sl_g1<-sl_g1[  as.numeric(sl_g1[,"dt"]) %in% dt_LE  ,] 
  sl_s1<-sl_s1[  as.numeric(sl_s1[,"dt"]) %in% dt_LE  ,] 
  ### NEE day
  snd_gm<-snd_gm[  as.numeric(snd_gm[,"dt"]) %in% dt_NEEd  ,] 
  snd_sm<-snd_sm[  as.numeric(snd_sm[,"dt"]) %in% dt_NEEd  ,] 
  snd_g1<-snd_g1[  as.numeric(snd_g1[,"dt"]) %in% dt_NEEd  ,] 
  snd_s1<-snd_s1[  as.numeric(snd_s1[,"dt"]) %in% dt_NEEd  ,] 
  ### NEE night
  snn_gm<-snn_gm[  as.numeric(snn_gm[,"dt"]) %in% dt_NEEn  ,] 
  snn_sm<-snn_sm[  as.numeric(snn_sm[,"dt"]) %in% dt_NEEn  ,] 
  snn_g1<-snn_g1[  as.numeric(snn_g1[,"dt"]) %in% dt_NEEn  ,] 
  snn_s1<-snn_s1[  as.numeric(snn_s1[,"dt"]) %in% dt_NEEn  ,]   
  ### T
  st_gm<-st_gm[  as.numeric(st_gm[,"dt"]) %in% dt_T  ,]
  st_sm<-st_sm[  as.numeric(st_sm[,"dt"]) %in% dt_T  ,]
  st_g1<-st_g1[  as.numeric(st_g1[,"dt"]) %in% dt_T  ,]
  st_s1<-st_s1[  as.numeric(st_s1[,"dt"]) %in% dt_T  ,]
}












stop("Prepare to plot")




### 3) same-curve plotting ====================
if(!seg_all){
  for(i in 1:5){
    #i<-4
    
    # Co/Var is still a column
    ogm<-get(paste("s", specs_ids[i], "_gm", sep=""))
    osm<-get(paste("s", specs_ids[i], "_sm", sep=""))
    og1<-get(paste("s", specs_ids[i], "_g1", sep=""))
    os1<-get(paste("s", specs_ids[i], "_s1", sep=""))
    
    
    stat<-"median"
    
    
    ygm0<-apply(ogm[1:(lxl+1)], 2, stat, na.rm=T); 
    ysm0<-apply(osm[1:(lxl+1)], 2, stat, na.rm=T);
    yg10<-apply(og1[1:(lxl+1)], 2, stat, na.rm=T);
    ys10<-apply(os1[1:(lxl+1)], 2, stat, na.rm=T);
    
    # normalize (divide by Co/Var)
    ygm<-ygm0[1:lxl]/ygm0[lxl+1]
    ysm<-ysm0[1:lxl]/ysm0[lxl+1]
    yg1<-yg10[1:lxl]/yg10[lxl+1]
    ys1<-ys10[1:lxl]/ys10[lxl+1]
    
    
    ylim<-range(c(ygm, ysm, yg1, ys1), na.rm=T)*c(1.05)
    
    if(i!=5)ylim<-c(0, 0.4)
    
    
    
    plot_nm<-paste(tf, "cospectra_", vvi, lb, "_", fluxes[i], "_re_take_test.png", sep="")
    
    png(paste(gpath, "X_output/", plot_nm, sep=""), width=600, height=600)
    
    #par(mfrow=c(2,2), mar = c(3, 3, 3, 2), oma = c(3, 3, 5, 1), mgp=c(2, 0.5, 0))
    par(mfrow=c(1,1), mar = c(5, 7, 5, 4), mgp=c(3, 0.5, 0), tck=0.01)
    
    
    
    plot(xl, ygm, log="x", type="l", axes=F, xlab="", ylab="", ylim=ylim, col="cyan"); box()
    axis(1); mtext("n(z-d)/U", 1, line=3)
    axis(2); mtext("C(n)/Cov", 2, line=3)
    mtext(paste(fluxes[i]), 3, cex=2)
    lines(xl, ysm, col="blue", lty=1);  lines(xl, yg1, col="green"); lines(xl, ys1, col="darkgreen", lty=1)
    abline(h=0, col="grey");     abline(v=1, lty=3, col="grey")
    legend("topright", c("US-SEG", "US-SES", "SEG EC1", "SES EC1"), 
           col=c("cyan", "blue", "green", "darkgreen"), lty=c(1, 1, 1, 1), lwd=3)
    
    
    
    mtext(paste("US-SEG cov thresh: ", covm[1,i]), 3, line=-2, adj=0.02)
    mtext(paste("US-SES cov thresh: ", covm[2,i]), 3, line=-3, adj=0.02)
    mtext(paste("SEG EC1 cov thresh: ", covm[3,i]), 3, line=-4, adj=0.02)
    mtext(paste("SES EC1 cov thresh: ", covm[4,i]), 3, line=-5, adj=0.02)
    
    mtext(paste("US-SEG  cospectra: ", nrow(ogm), "/", nrow(dth)), 3, line=-8,  adj=0.02)
    mtext(paste("US-SES  cospectra: ", nrow(osm), "/", nrow(dtl)), 3, line=-9,  adj=0.02)
    mtext(paste("SEG EC1 cospectra: ", nrow(og1), "/", nrow(dtn)), 3, line=-10, adj=0.02)
    mtext(paste("SES EC1 cospectra: ", nrow(os1), "/", nrow(dtt)), 3, line=-11, adj=0.02)
    
    dev.off()
  }
}




if(seg_all){
  for(i in 1:5){
    #i<-4
    stat<-"median"
    
    if(!seg_5){
      # Co/Var is still a column
      o1<-get(paste("s", specs_ids[i], "_ss1", sep=""))
      o2<-get(paste("s", specs_ids[i], "_ss2", sep=""))
      o3<-get(paste("s", specs_ids[i], "_ss3", sep=""))
      
      y10<-apply(o1[1:(lxl+1)], 2, stat, na.rm=T); 
      y20<-apply(o2[1:(lxl+1)], 2, stat, na.rm=T);
      y30<-apply(o3[1:(lxl+1)], 2, stat, na.rm=T);
      
      # normalize (divide by Co/Var)
      y1<-y10[1:lxl]/y10[lxl+1]
      y2<-y20[1:lxl]/y20[lxl+1]
      y3<-y30[1:lxl]/y30[lxl+1]
      
      ylim<-range(c(y1, y2, y3), na.rm=T)*c(1.05)
      plot_nm<-paste(tf, "cospectra_", vvi, lb, "_", fluxes[i], "_SEG_all_test.png", sep="")
      
    }
    
    
    
    if(seg_5){
      o1<-get(paste("s", specs_ids[i], "_ss1", sep=""))
      y10<-apply(o1[1:(lxl+1)], 2, stat, na.rm=T); 
      y1<-y10[1:lxl]/y10[lxl+1]
      ylim<-range(y1, na.rm=T)*c(1.05)
      plot_nm<-paste(tf, "cospectra_", vvi, lb, "_", fluxes[i], "_SEG_5.png", sep="")
      
    }
    
    
    
    #if(i!=5)ylim<-c(0, 0.4)
    
    
    
    
    
    png(paste(gpath, "X_output_SEG_all/", plot_nm, sep=""), width=600, height=600)
    
    #par(mfrow=c(2,2), mar = c(3, 3, 3, 2), oma = c(3, 3, 5, 1), mgp=c(2, 0.5, 0))
    par(mfrow=c(1,1), mar = c(5, 7, 5, 4), mgp=c(3, 0.5, 0), tck=0.01)
    
    plot(xl, y1, log="x", type="l", axes=F, xlab="", ylab="", ylim=ylim, col="orange"); box()
    axis(1); mtext("n(z-d)/U", 1, line=3)
    axis(2); mtext("C(n)/Cov", 2, line=3)
    mtext(paste(fluxes[i]), 3, cex=2)
    abline(h=0, col="grey");     abline(v=1, lty=3, col="grey")
    
    mtext(paste("IRGA1 cov thresh: ", covm[1,i]), 3, line=-2, adj=0.02)
    mtext(paste("IRGA1 cospectra: ", nrow(o1), "/", nrow(dth)), 3, line=-8,  adj=0.02)
    
    
    if(!seg_5){
      lines(xl, y2, col="red", lty=1);  lines(xl, y3, col="darkred")
      legend("topright", paste("From", begs), col=c("orange", "red", "darkred"), lty=c(1, 1, 1), lwd=3)
      
      mtext(paste("IRGA2 cov thresh: ", covm[2,i]), 3, line=-3, adj=0.02)
      mtext(paste("IRGA3 cov thresh: ", covm[3,i]), 3, line=-4, adj=0.02)
      mtext(paste("IRGA2 cospectra: ", nrow(o2), "/", nrow(dtl)), 3, line=-9,  adj=0.02)
      mtext(paste("IRGA3 cospectra: ", nrow(o3), "/", nrow(dtn)), 3, line=-10, adj=0.02)
    }
    
    
    dev.off()
  }
}










### 4) four-groups NEE  ====================

if(!seg_all)dt4<-Reduce(intersect, list(dds_gm[[1]][,"dt"], dds_sm[[1]][,"dt"], dds_g1[[1]][,"dt"], dds_s1[[1]][,"dt"]))

flux<-"H"

for(i in 1:is){
  
  dds<-get(paste("dds", sdatasets[i], sep="_")); nhc<-dds[[1]]; nnc<-dds[[2]]
  
  if(!seg_all){nhc<-nhc[ nhc[,"dt"] %in% dt4, ]; nnc<-nnc[ nnc[,"dt"] %in% dt4, ]}   # common dt
  
  if(flux=="NEE"){
    group1<-nnc[ nhc[,"Co/Var"]>0 & nnc[,"Co/Var"]>0 ,]
    group2<-nnc[ nhc[,"Co/Var"]<0 & nnc[,"Co/Var"]>0 ,]
    group3<-nnc[ nhc[,"Co/Var"]>0 & nnc[,"Co/Var"]<0 ,]
    group4<-nnc[ nhc[,"Co/Var"]<0 & nnc[,"Co/Var"]<0 ,]
  } else {
    group1<-nhc[ nhc[,"Co/Var"]>0 & nnc[,"Co/Var"]>0 ,]
    group2<-nhc[ nhc[,"Co/Var"]<0 & nnc[,"Co/Var"]>0 ,]
    group3<-nhc[ nhc[,"Co/Var"]>0 & nnc[,"Co/Var"]<0 ,]
    group4<-nhc[ nhc[,"Co/Var"]<0 & nnc[,"Co/Var"]<0 ,]
  }
  
  
  
  gr1<-apply(group1, 2, stat, na.rm=T)
  gr2<-apply(group2, 2, stat, na.rm=T)
  gr3<-apply(group3, 2, stat, na.rm=T)
  gr4<-apply(group4, 2, stat, na.rm=T)
  
  y1<-gr1[1:lxl]/gr1[lxl+1]
  y2<-gr2[1:lxl]/gr2[lxl+1]
  y3<-gr3[1:lxl]/gr3[lxl+1]
  y4<-gr4[1:lxl]/gr4[lxl+1]
  
  nrs<-c(nrow(group1), nrow(group2), nrow(group3), nrow(group4), nrow(nnc))
  assign(paste("ys", sdatasets[i], sep="_"), list(y1, y2, y3, y4, nrs))
  
  #next
  
}




### 4.1) 4G NEE system plot  ====================

zoom<-F
zl<-""; if(zoom)zl<-"_zoom"

plot_nm<-paste("4_groups_", flux, "_cospectra_", vvi, "_SEG_all", zl, "_test.png", sep="")
png(paste(gpatho, plot_nm, sep=""), width=600, height=600)
par(mfrow=c(2,2))

for(i in 1:is){

  ys<-get(paste("ys", sdatasets[i], sep="_"))
  y1<-ys[[1]]; y2<-ys[[2]]; y3<-ys[[3]]; y4<-ys[[4]]; ddn<-ys[[5]]
  
  ylim<-c(0, max(y1, y2, y3, y4, na.rm=T))
  #ylim<-c(-0.2, 0.5)
  #ylim<-c(0, 0.35)
  if(seg_all){
    ylim<-range(y1, y2, y3, y4, na.rm=4)
    if(zoom)ylim<-c(-0.1, 0.5)   # zoom
  }
  
  
  plot(xl, y1, type="l", log="x", ylim=ylim, axes=F, xlab="", ylab=""); box()
  lines(xl, y2, col="grey"); lines(xl, y3, col="orange"); lines(xl, y4, col="red"); abline(v=1, col="grey", lty=3)
  axis(1); mtext("n(z-d)/U", 1, line=3)
  axis(2); mtext("C(n)/Cov", 2, line=3)
  if(!seg_all)mtext(paste(sites_ids[i]), 3, cex=2)
  if(seg_all) mtext(paste("SEG from", begs[i]), 3, cex=2)
  
  if(i==1)legend("topright", c("gr1: wTcov>0 wCcov>0", "gr2: wTcov<0 wCcov>0", 
                               "gr3: wTcov>0 wCcov<0", "gr4: wTcov<0 wCcov<0"), 
                 col=c("black", "grey", "orange", "red"), lty=1, lwd=3)
  
  mtext(paste("gr1:", ddn[1], "/", ddn[5]), 3, line=-1, adj=0.02)
  mtext(paste("gr2:", ddn[2], "/", ddn[5]), 3, line=-2, adj=0.02)
  mtext(paste("gr3:", ddn[3], "/", ddn[5]), 3, line=-3, adj=0.02)
  mtext(paste("gr4:", ddn[4], "/", ddn[5]), 3, line=-4, adj=0.02)
  
}

dev.off()






 ### 4.2) 4G NEE group plot  ====================


groups<-c("wTcov>0 wCcov>0", "wTcov<0 wCcov>0", "wTcov>0 wCcov<0", "wTcov<0 wCcov<0")


plot_nm<-paste("4G_", flux, "_groups_", vvi, ".png", sep="")
png(paste(gpath, plot_nm, sep=""), width=600, height=600)
par(mfrow=c(2,2))

for(i in 1:4){
  
  #ys<-get(paste("ys", sdatasets[i], sep="_"))
  y1<-ys_gm[[i]]; y2<-ys_sm[[i]]; y3<-ys_g1[[i]]; y4<-ys_s1[[i]]; #ddn<-ys[[5]]
  
  ylim<-c(0, max(y1, y2, y3, y4, na.rm=T))
  #ylim<-c(-0.2, 0.5)
  
  
  plot(xl, y1, type="l", log="x", ylim=ylim, axes=F, xlab="", ylab="", col="cyan"); box()
  lines(xl, y2, col="blue"); lines(xl, y3, col="green"); lines(xl, y4, col="darkgreen"); abline(v=1, col="grey", lty=3)
  axis(1); mtext("n(z-d)/U", 1, line=3)
  axis(2); mtext("C(n)/Cov", 2, line=3)
  mtext(paste(groups[i]), 3, cex=2)
  
  if(i==1)legend("topright", sites_ids, 
                 col=c("cyan", "blue", "green", "darkgreen"), lty=1, lwd=3)
  
  mtext(paste("l1:", ys_gm[[5]][i], "/", ddn[5]), 3, line=-1, adj=0.02)
  mtext(paste("l2:", ys_sm[[5]][i], "/", ddn[5]), 3, line=-2, adj=0.02)
  mtext(paste("l3:", ys_g1[[5]][i], "/", ddn[5]), 3, line=-3, adj=0.02)
  mtext(paste("l4:", ys_s1[[5]][i], "/", ddn[5]), 3, line=-4, adj=0.02)
  
}

dev.off()





















### test T spectra 

mean(gms[,"Co/Var"])    # 9982122        # ...
mean(sms[,"Co/Var"])    # 1.214353e+20   # ...
mean(g1s[,"Co/Var"])    # 4.877448
mean(s1s[,"Co/Var"])    # 0.4170974

median(gms[,"Co/Var"])  # 0.02104115
median(sms[,"Co/Var"])  # 0.0211026
median(g1s[,"Co/Var"])  # 0.00402
median(s1s[,"Co/Var"])  # 0.00326955







 











