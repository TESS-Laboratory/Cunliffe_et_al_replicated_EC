

rm(list = ls())
library(chron); #library(oce); #library(ggplot2); library(gridExtra); #
#library(lubridate) # only for 'month' function
path   <-"E:/REC_7_Data/8_datasets/old/"
pl_path<-"E:/REC_7_Data/10_Plots/lags_plots/"
sites<-c("SEG", "SES"); recs<-1:4; towers<-paste(rep(sites, each=4), rep(recs,2), sep="_REC")

last_date<-"2019_10_01_from_flash_LAGS_test_"   # format: 'yyyy_mm_dd_'
#last_date<-"2019_10_01_from_flash_"
#last_date<-"2019_05_06_telemetry_"
#last_date<-"SEP_DEC_2019_"

Tx<-F; if(grepl("LAGS_test_", last_date))Tx<-T

selRH<-"c"     # can be "c" (cell) or "r" (RH block, beside heating)

datasets<-c("g1", "g2", "g3", "g4", "s1", "s2", "s3", "s4")
x<-seq(-10, 40, 0.1)

time_filterTF<-F
mm1<-9; yy1<-2019; mm1c<-substring(as.character(1000+mm1), 3,4)
mm2<-10; yy2<-2019; mm2c<-substring(as.character(1000+mm2), 3,4)
tf1<-paste("01/", mm1c, "/", yy1, sep="")
tf2<-paste("01/", mm2c, "/", yy2, sep="")

dry_datasets<-T   # filter P from Marcy's datsets
dl<-""; if(dry_datasets)dl<-"dry_"

daylight<-T
dll<-""; if(daylight)dll<-"day_"



### plot lags - advanced
for(i in 1:8){
  #i<-1
  print(i)
  stat<-"mean"     # can be "mean" or "median"
  flx<-read.csv(file=paste(path, last_date, towers[i], "_flux.csv", sep=""), header=TRUE, sep=",")
  
  
  ## sometimes, empty fator columns that are not visible in excel are added
  flx<-flx[, !grepl("X", colnames(flx))  ]

  
  ### sometimes the 2 data frames have different date format (not visible in excel) 
  dtimes<-as.character(flx[,1])
  dtparts <- t(as.data.frame(strsplit(dtimes,' '))); row.names(dtparts) = NULL
  if(unique(nchar(dtparts[,2]))==5)dtparts[,2]<-paste(dtparts[,2], ":00", sep="")
  thetimes_flx <- chron(dates=dtparts[,1],times=dtparts[,2],format=c("d/m/y","h:m:s"))
  flx$dt<-thetimes_flx; flx[,1]<-thetimes_flx    # need both!
  
  
  #flx<-flx[(flx[,"Date.Time"] > "01/11/18" & flx[,"Date.Time"] < "01/10/19"),]
  
  
  if(dry_datasets | daylight){
    flx0<-flx
    if(!exists("gm"))load("E:/REC_7_Data/9_R/Rdata/gm.RData")   # so you load it only once
    if(!exists("sm"))load("E:/REC_7_Data/9_R/Rdata/sm.RData")   
    
    if(i %in% c(1:4)) pdat<-gm 
    if(i %in% c(5:8)) pdat<-sm 
    
    
    ## make colnames site-independent
    csel<-colnames(pdat)!="dt"
    colnames(pdat)[csel]<-substring(colnames(pdat)[csel], 1, nchar(colnames(pdat)[csel])-3)
    colnames(pdat)[colnames(pdat) %in% c("P", "P_F")]<-c("Prec", "Prec_F")   # in flx P is Pressure !!!!
    
    
    flx<-merge(flx, pdat, by="dt")   # left merge
  }

    
  if(dry_datasets)flx<-flx[flx[,"Prec"]==0,]        
  if(daylight)flx<-flx[flx[,"H"]>=0,]
    
    
    
  
  
  
  # plot peaktime ("cH2Oxcor", "rH2Oxcor", "CO2xcor", "gTxcor") vs Date.Time
  
  
  gpath<-"E:/REC_7_Data/10_Plots/lags_plots/"
  png(paste(gpath, last_date, "peaktimes_", dl, dll, towers[i], ".png", sep=""), width=2500, height=1800)
  par(mfrow=c(4,2), mar = c(0, 12, 0, 0), oma = c(22, 14, 10, 5), mgp=c(8, 2, 0))
  
  plot(flx[,"cH2Oxcor"]~flx[,"Date.Time"], pch=16, cex.axis=4, cex.lab=4); mtext(towers[i], 3, cex=4, line=2)
  if(Tx){plot(flx[,"cH2O_Txcor"]~flx[,"Date.Time"], pch=16, cex.axis=4, cex.lab=4); mtext(last_date, 3, cex=4, line=2)}
  if(!Tx){plot(flx[,"gTxcor"]~flx[,"Date.Time"], pch=16, type="n"); mtext(last_date, 3, cex=4, line=2)}
  
  plot(flx[,"rH2Oxcor"]~flx[,"Date.Time"], pch=16, cex.axis=4, cex.lab=4)
  if(Tx){plot(flx[,"rH2O_Txcor"]~flx[,"Date.Time"], pch=16, cex.axis=4, cex.lab=4)}
  if(!Tx){plot(flx[,"gTxcor"]~flx[,"Date.Time"], pch=16, type="n")}
  
  plot(flx[,"CO2xcor"]~flx[,"Date.Time"], pch=16, cex.axis=4, cex.lab=4)
  if(Tx){plot(flx[,"CO2_Txcor"]~flx[,"Date.Time"], pch=16, cex.axis=4, cex.lab=4)}
  if(!Tx){plot(flx[,"gTxcor"]~flx[,"Date.Time"], pch=16, type="n")}
  
  plot(flx[,"gTxcor"]~flx[,"Date.Time"], pch=16, cex.axis=4, cex.lab=4)
  axis.Date(1, at=seq(min(flx[,"Date.Time"], na.rm=T), max(flx[,"Date.Time"], na.rm=T), by="month"), cex.axis=4, las=3); 
  plot(flx[,"gTxcor"]~flx[,"Date.Time"], pch=16, type="n")
  
  mtext(paste("cH2Oxcor mean:", round(mean(flx[,"cH2Oxcor"], na.rm=T), 2),
      "  sde:", round(std.error(flx[,"cH2Oxcor"], na.rm=T), 2), sep=" "), 3, line=-4, cex=4)
  if(Tx)mtext(paste("cH2O_Txcor mean:", round(mean(flx[,"cH2O_Txcor"], na.rm=T), 2),
              "  sde:", round(std.error(flx[,"cH2O_Txcor"], na.rm=T), 2), sep=" "), 3, line=-8, cex=4)
  mtext(paste("rH2Oxcor mean:", round(mean(flx[,"rH2Oxcor"], na.rm=T), 2),
              "  sde:", round(std.error(flx[,"rH2Oxcor"], na.rm=T), 2), sep=" "), 3, line=-12, cex=4)
  if(Tx)mtext(paste("rH2O_Txcor mean:", round(mean(flx[,"rH2O_Txcor"], na.rm=T), 2),
              "  sde:", round(std.error(flx[,"rH2O_Txcor"], na.rm=T), 2), sep=" "), 3, line=-16, cex=4)
  mtext(paste("CO2xcor mean:", round(mean(flx[,"CO2xcor"], na.rm=T), 2),
              "  sde:", round(std.error(flx[,"CO2xcor"], na.rm=T), 2), sep=" "), 3, line=-20, cex=4)
  if(Tx)mtext(paste("CO2_Txcor mean:", round(mean(flx[,"CO2_Txcor"], na.rm=T), 2),
              "  sde:", round(std.error(flx[,"CO2_Txcor"], na.rm=T), 2), sep=" "), 3, line=-24, cex=4)
  mtext(paste("gTxcor mean:", round(mean(flx[,"gTxcor"], na.rm=T), 2),
              "  sde:", round(std.error(flx[,"gTxcor"], na.rm=T), 2), sep=" "), 3, line=-28, cex=4)
  
  axis.Date(1, at=seq(min(flx[,"Date.Time"], na.rm=T), max(flx[,"Date.Time"], na.rm=T), by="month"), cex.axis=4, las=3);
  
  dev.off()
  
}



 



# assign datasets / plot binned median lags of individual towers 
if(F){
  for(i in 1:8){
    #i<-2
    plot_meansTF<-T
    plot_qntTF<-T
    stat<-"mean"     # can be "mean" or "median"
    Ts_f<-(-300)     #50         # W m-2 (max: 5000)
    LE_f<-(-100)     #20         # W m-2 (max: 500)
    Fc_f<-(-5)  #0.5        # umol m-2 s-1 (max: 50)
    dat<-read.csv(file=paste(path, last_date, towers[i], "_lags.csv", sep=""), header=TRUE, sep=",")
    flx<-read.csv(file=paste(path, last_date, towers[i], "_flux.csv", sep=""), header=TRUE, sep=",")
    
    
    ## sometimes, empty fator columns that are not visible in excel are added
    flx<-flx[, !grepl("X", colnames(flx))  ]
    dat<-dat[,1:255]
    dcols<-colnames(dat)[4:(ncol(dat)-1)]
    
    
    ### sometimes the 2 data frames have different date format (not visible in excel) 
    dtimes<-as.character(flx[,1])
    dtparts <- t(as.data.frame(strsplit(dtimes,' '))); row.names(dtparts) = NULL
    if(unique(nchar(dtparts[,2]))==5)dtparts[,2]<-paste(dtparts[,2], ":00", sep="")
    thetimes_flx <- chron(dates=dtparts[,1],times=dtparts[,2],format=c("d/m/y","h:m:s"))
    flx[,1]<-thetimes_flx
    
    dtimes<-as.character(dat[,1])
    dtparts <- t(as.data.frame(strsplit(dtimes,' '))); row.names(dtparts) = NULL
    if(unique(nchar(dtparts[,2]))==5)dtparts[,2]<-paste(dtparts[,2], ":00", sep="")
    thetimes_dat <- chron(dates=dtparts[,1],times=dtparts[,2],format=c("d/m/y","h:m:s"))
    dat[,1]<-thetimes_dat
    
    
    
    if(time_filterTF){   
      dat0<-dat; flx0<-flx
      dat<-dat[ dat[,"Date"]>=tf1 & dat[,"Date"]<=tf2,]
      flx<-flx[ flx[,"Date.Time"]>=tf1 & flx[,"Date.Time"]<=tf2,]
    }
    
    
    
    if(F){   # plot flux vs covariances 
      
      # get common date-times
      d_flx<-flx[,"Date.Time"]
      d_dat<-unique(dat[,"Date"])
      
      flxp<-flx[flx[,"Date.Time"]%in%d_dat,]
      datp<-dat[dat[,"Date"]%in%d_flx,]
      
      
      # fluxes to plot
      H<-flxp[,"H"]; LE<-flxp[,"cLE"]; CO2<-flxp[,"Fc"]
      
      # put in "X" avg covs to plot
      datp[,"X"]<-apply(datp[,dcols], 1, mean)  
      cov_H  <-datp[datp[,"Signal"]==" gW_-gT_QiQivCtv - Covariance","X"]
      cov_LE <-datp[datp[,"Signal"]==" gW_-cH2O_Cgc - Covariance","X"]
      cov_CO2<-datp[datp[,"Signal"]==" gW_-vCO2_ - Covariance","X"]
      
      
      plot(H, cov_H, pch=16, cex=0.5)
      plot(LE, cov_LE, pch=16, cex=0.5)
      plot(CO2, cov_CO2, pch=16, cex=0.5)
    }
    
    
    
    
    
    # filtering
    # first filter non-significant fluxes, and then  overestimated covariances values 
    dat<-dat[dat[,"Peaktime"]!=(-9999),]
    
    # turn negative cov for CO2 into positive cov
    dat[dat[,100]<0,4:254]<-dat[dat[,100]<0,4:254]*(-1)
    
    # H: >Ts_f
    ndts_ts<-flx[flx[,"H"]<Ts_f | flx[,"H"]>5000, "Date.Time"] 
    nsel_ts<-dat[,"Date"]%in%ndts_ts & dat[,"Signal"]==" gW_-gT_QiQivCtv - Covariance"
    
    # LE: >LE_f
    ndts_le<-flx[flx[,"cLE"]<LE_f | flx[,"cLE"]>500, "Date.Time"]
    nsel_le<-dat[,"Date"]%in%ndts_le & dat[,"Signal"]==" gW_-cH2O_Cgc - Covariance"
    ndts_ler<-flx[flx[,"rLE"]<LE_f | flx[,"rLE"]>500, "Date.Time"]
    nsel_ler<-dat[,"Date"]%in%ndts_ler & dat[,"Signal"]==" gW_-rH2O_Cgc - Covariance"
    
    
    # CO2: > Fc_f
    #ndts_fc<-flx[flx[,"Fc"]< Fc_f*(-1) | flx[,"Fc"]>Fc_f, "Date.Time"]
    ndts_fc<-flx[flx[,"Fc"]<Fc_f | flx[,"H"]>50, "Date.Time"]
    nsel_fc<-dat[,"Date"]%in%ndts_fc & dat[,"Signal"]==" gW_-vCO2_ - Covariance"
    
    # apply filters
    #datf<-dat[!(nsel_le | nsel_fc | nsel_ts),]               
    datf<-dat[!(nsel_le | nsel_ler | nsel_fc | nsel_ts),]
    
    
    #plot some of the cov plots with outliers
    ### a data.frame is not going to be plotted directly; turn it into as.numeric 
    #o2<-dat[dat[,100]>1,4:254]   # 11 over 12000 rows
    #plot(as.numeric(o2[4,]))  # values: 0-500
    
    
    
    
    # outliers according to Tukey's Fence: 
    # 1.5 times the inter-quartile difference (3 times iqd, it is "far out") 
    
    # find inter-quartile difference of covariances 
    iqv<-lapply(datf[,dcols], quantile, probs=seq(0, 1, by=0.25), name=FALSE, na.rm=T)
    quant_mat<-matrix(unlist(iqv), nrow=5, ncol=251); colnames(quant_mat)<-dcols
    rownames(quant_mat)<-c("0%", "25%", "50%", "75%", "100%")
    iqd<-quant_mat["75%",]-quant_mat["25%",]
    
    
    # iqd*12 is 4 times what is considered "far out"
    sel_o<-datf[,dcols]>quant_mat["25%",]-iqd*12 & datf[,dcols]<quant_mat["75%",]+iqd*12
    sum_sel<-apply(sel_o, 1, sum) # 251 means that all of the data are not-far-out points
    datff<- datf[ sum_sel>200 ,]  # keep all data, expcept those where >80% of the cov are more than "4 times far out"
    
    
    
    
    # bin cH2o (o rH2o) according to RH, find 10 peaks and plot them
    mcol<-paste("mean_", selRH, "RH", sep="")    # can be "mean_cRH" or "mean_rRH"
    scol<-paste(" gW_-", selRH, "H2O_Cgc - Covariance", sep="")      # as above
    
    rhb<-quantile(flx[,mcol], seq(0, 1, by=0.1)); rhb[11]<-100 # 3 RH values >100%
    peaks<-matrix(NA, ncol=3, nrow=10); colnames(peaks)<-c("bin RH",  "Time",  "Cov")
    peaks[,"bin RH"]<-1:10  
    
    bin_plots<-matrix(NA, ncol=251, nrow=10); rownames(bin_plots)<-1:10
    bin_n<-vector(length=10)
    
    for(j in 1:10){
      #j<-10
      dts_d<-flx[flx[,mcol]>=rhb[j] & flx[,mcol]<rhb[j+1], "Date.Time"]
      dat_d<-datf[datf[,"Date"]%in%dts_d & datf[,"Signal"]==scol,]
      #dat_d<-dat_d[dat_d[,6]<0.3,]
      #print(paste(j, nrow(dat_d)))
      
      if(nrow(dat_d)>1){
        cov_d<-apply(dat_d[,c(5:ncol(dat_d)-1)], 2, median)
        peaks[j,"Time"]<-median(x[which(cov_d==max(cov_d))])
        peaks[j,"Cov"]<-max(cov_d)
        bin_plots[j,]<-cov_d
        bin_n[j]<-nrow(dat_d)
        #cov_d_mn<-apply(dat_d[,c(5:ncol(dat_d)-1)], 2, mean)
        #peaks[j,]<-c(median(x[which(cov_d==max(cov_d))]), max(cov_d))
        
        #print(paste(j, nrow(dat_d)))
        #print(paste("mean", mean(dat_d[,"Peaktime"]), "/ max_plot",
        #    median(x[which(cov_d==max(cov_d))]), "/ median", median(dat_d[,"Peaktime"])))
        #plot(x, cov_d, type="l", ylim=c(0, max(cov_d*1.05)));
        
        #lines(x, cov_d, col=2)
        #abline(v=mean(dat_d[,"Peaktime"]));
        #abline(v=median(dat_d[,"Peaktime"]), col="red")
        #abline(v=median(x[which(cov_d==max(cov_d))]), col="orange") # the winner !!!!!
      } #else {peaks[j,"bin RH"]<-NA}
    }
    
    
    
    #stop("enough for now")
    
    # Plot peaks
    if(plot_qntTF){
      
      dest<-paste(pl_path, last_date, substring(mcol,6,8),
                  "_RH_t_peaks_", stat, "_H", Ts_f, "_LE", LE_f, "_", towers[i], ".png", sep="")
      
      if(time_filterTF)dest<-paste(pl_path, "monthly/", yy1, "_", mm1c, "_", substring(mcol,6,8), 
                                   "_RH_t_peaks_",stat, "_H", Ts_f, "_LE", LE_f, "_", towers[i], ".png", sep="")
      
      png(dest , width=1200, height=500)
      par(mfrow=c(1,2))
      #plot(peaks[,"RH"], peaks[,"Cov"], main=towers[i], ylab="Peak in cov", xlab="RH (%)")
      plot(peaks[,"bin RH"], peaks[,"Time"], ylim=range(peaks[,"Time"], na.rm=T)*c(1, 1.1),
           main=scol, ylab="Peak time (s)", xlab="bin RH")
      
      text(2, max(peaks[,"Time"])*0.9, paste("RH:", trunc(min(flx[,"mean_cRH"])), "-", 
                                             trunc(max(flx[,"mean_cRH"])), "%" ))
      text(peaks[,"bin RH"], peaks[,"Time"]*1.1, labels=bin_n)
      if(time_filterTF)mtext(substring(tf1, 4,10), side=3, adj=0.05, line=-3)
      
      #for(k in 1:10)lines(bin_plots[k,])
      # lag = c1 + c2 exp(c3 * RH)     or       lag = c1 + c2^(c3 * RH)
      
      plot(x, bin_plots[1,], type="n", ylim=range(bin_plots, na.rm=T), main=towers[i], 
           xlab="Time lag (s)", ylab="Cov")
      for(k in 1:10){lines(x, bin_plots[k,], col="grey");
        text(peaks[k,"Time"]*1.05, peaks[k,"Cov"]*1.05, labels=k)}
      points(peaks[,"Time"], peaks[,"Cov"], pch=16)
      
      dev.off()
    }
    
    
    
    
    
    
    
    
    
    
    
    #### linear model for peaks not needed now; lag is constant!
    
    
    
    
    
    
    
    
    # ----------- Plot mean cov for cH2O, Ts and vCO2 
    
    
    #dcols<-colnames(dat)[4:ncol(dat)]
    if(stat=="mean"){lags<-aggregate(datff[,dcols],by=list(datff[,"Signal"]), mean); titles<-"- Means"}
    if(stat=="median"){lags<-aggregate(datff[,dcols],by=list(datff[,"Signal"]), median); titles<-"- Medians"}
    rownames(lags)<-c("cH2O", "Ts", "rH2O", "vCO2")
    titles<-paste(titles, " / f:",Ts_f, LE_f, Fc_f)
    
    
    #stop("enough")
    
    if(plot_meansTF){
      
      dest<-paste(pl_path, last_date, stat, "_H", Ts_f, "_crLE",LE_f, "_Fc", 
                  Fc_f,"_", towers[i], "_lags.png", sep="")
      if(time_filterTF)dest<-paste(pl_path, "monthly/", towers[i], "_", yy1, "_", mm1c, "_",
                                   stat, "_H", Ts_f, "_crLE",LE_f, "_Fc", Fc_f, "_lags.png", sep="")
      
      
      png(dest , width=600, height=500)
      
      par(mfrow=c(4,1), mar = c(0, 0, 0, 0), oma = c(6, 6, 4, 0.5), mgp=c(2, 0.5, 0), tck=-0.01)
      plot(x, t(lags["cH2O",2:252]), cex.axis=1.5); 
      mtext("cH2O", 2, line=3, cex=1.5); mtext(paste(towers[i], titles), 3, line=2, cex=2)
      if(time_filterTF)mtext(substring(tf1, 4,10), side=3, adj=0.05, line=-3)
      plot(x, t(lags["rH2O",2:252]), cex.axis=1.5); 
      mtext("rH2O", 2, line=3, cex=1.5)
      plot(x, t(lags["Ts",2:252]), cex.axis=1.5); 
      mtext("Ts", 2, line=3, cex=1.5) 
      plot(x, t(lags["vCO2",2:252]), cex.axis=1.5); 
      mtext("vCO2", 2, line=3, cex=1.5); mtext("Time (s)", 1, line=3, cex=1.5);
      
      dev.off()
    }
    
    assign(datasets[i], dat)
  }
}







# outlier of system4 - not in the flux data
if(F){
  i<-4
  dat<-read.csv(file=paste(path, last_date, towers[i], "_lags.csv", sep=""), header=TRUE, sep=",")
  flx<-read.csv(file=paste(path, last_date, towers[i], "_flux.csv", sep=""), header=TRUE, sep=",")
  
  plot(dat[,100])  # outlier at 2500000
  dto<-dat[dat[,100]>1000000,"Date"]   # 28/12/18 01:45:00
  flx[flx["Date.Time"]%in%dto, c("H", "cLE", "Fc")]    # flx["Date.Time"]%in%dto FALSE
}



#hist(as.vector(g1[,c(100:200)]))
     









