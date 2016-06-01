TablesB2B3prem=function(var,vartype) {
  attach(Xm)
  #"1993"/baseline prevalence values setup
  firsts=c(rep(0,3))
  #discrete variable
  if (vartype==1){
    for (i in 1:3) {
      firsts[i]=100*sum(var[SURVEY==1993 & ALL_UKB==(i-1)],na.rm=TRUE)/length(SURVEY[SURVEY==1993 & ALL_UKB==(i-1)][!is.na(SURVEY[SURVEY==1993 & ALL_UKB==(i-1)])])
    }
    #trend and interaction regression models
    fit_h=lm(100*var~AGE+Y_SURVEY+ALL_UKBL+ALL_UKBM+(Y_SURVEY:ALL_UKBL)+(Y_SURVEY:ALL_UKBM),data=Xm)
    fit_m=lm(100*var~AGE+Y_SURVEY+ALL_UKBL+ALL_UKBH+(Y_SURVEY:ALL_UKBL)+(Y_SURVEY:ALL_UKBH),data=Xm)
    fit_l=lm(100*var~AGE+Y_SURVEY+ALL_UKBM+ALL_UKBH+(Y_SURVEY:ALL_UKBM)+(Y_SURVEY:ALL_UKBH),data=Xm)
    interp = lm(100*var~AGE+Y_SURVEY+ALL_UKB+SURVEY:ALL_UKB,data=Xm) 
  }
  #continuous variable
  if (vartype==2){
    for (i in 1:3) {
      firsts[i]=mean(var[SURVEY==1993 & ALL_UKB==(i-1)],na.rm=TRUE)
    }
    fit_h=lm(var~AGE+Y_SURVEY+ALL_UKBL+ALL_UKBM+(Y_SURVEY:ALL_UKBL)+(Y_SURVEY:ALL_UKBM),data=Xm)
    fit_m=lm(var~AGE+Y_SURVEY+ALL_UKBL+ALL_UKBH+(Y_SURVEY:ALL_UKBL)+(Y_SURVEY:ALL_UKBH),data=Xm)
    fit_l=lm(var~AGE+Y_SURVEY+ALL_UKBM+ALL_UKBH+(Y_SURVEY:ALL_UKBM)+(Y_SURVEY:ALL_UKBH),data=Xm)
    interp = lm(var~AGE+Y_SURVEY+ALL_UKB+SURVEY:ALL_UKB,data=Xm)
  }
  #variable that needs a log-transform
  if (vartype==3){
    #take geometric means for prevalence 
    for (i in 1:3){
      firsts[i]=exp(mean(log(var[Xm$SURVEY==1993 & ALL_UKB==(i-1)]),na.rm=TRUE))
    }
    var3=log(var[!is.na(var)])
    Xm3=subset(Xm,var!="NA")
    fit_h=lm(var3~AGE+Y_SURVEY+ALL_UKBL+ALL_UKBM+(Y_SURVEY:ALL_UKBL)+(Y_SURVEY:ALL_UKBM),data=Xm3, na.action="na.exclude")
    fit_m=lm(var3~AGE+Y_SURVEY+ALL_UKBL+ALL_UKBH+(Y_SURVEY:ALL_UKBL)+(Y_SURVEY:ALL_UKBH),data=Xm3, na.action="na.exclude")
    fit_l=lm(var3~AGE+Y_SURVEY+ALL_UKBM+ALL_UKBH+(Y_SURVEY:ALL_UKBM)+(Y_SURVEY:ALL_UKBH),data=Xm3, na.action="na.exclude")
    interp = lm(var3~AGE+Y_SURVEY+ALL_UKB+SURVEY:ALL_UKB,data=Xm3, na.action="na.exclude")    
  }
  #current smoker pack-years, needs special treatment and log-transform  
  if (vartype==4){
    for (i in 1:3) {
      firsts[i]=exp(mean(log(PACKYRS[SMOKER3==2 & SURVEY==1993 & ALL_UKB==(i-1)]),na.rm=TRUE))
    }
    var4=log(var[!is.na(var)])
    Xm4=subset(Xm,PACKYRS!="NA" & SMOKER3==2)
    fit_h=lm(var4~AGE+Y_SURVEY+ALL_UKBL+ALL_UKBM+(Y_SURVEY:ALL_UKBL)+(Y_SURVEY:ALL_UKBM),data=Xm4, na.action="na.exclude")
    fit_m=lm(var4~AGE+Y_SURVEY+ALL_UKBL+ALL_UKBH+(Y_SURVEY:ALL_UKBL)+(Y_SURVEY:ALL_UKBH),data=Xm4, na.action="na.exclude")
    fit_l=lm(var4~AGE+Y_SURVEY+ALL_UKBM+ALL_UKBH+(Y_SURVEY:ALL_UKBM)+(Y_SURVEY:ALL_UKBH),data=Xm4, na.action="na.exclude")
    interp = lm(var4~AGE+Y_SURVEY+ALL_UKB+SURVEY:ALL_UKB,data=Xm4, na.action="na.exclude")    
  }
  #former smoker pack-years, needs special treatment and log-transform
  if (vartype==5){
    for (i in 1:3) {
      firsts[i]=exp(mean(log(PACKYRS[SMOKER3==1 & SURVEY==1993 & ALL_UKB==(i-1)]),na.rm=TRUE))
    }
    var5=log(var[!is.na(var)])
    Xm5=subset(Xm,PACKYRS!="NA" & SMOKER3==1)
    fit_h=lm(var5~AGE+Y_SURVEY+ALL_UKBL+ALL_UKBM+(Y_SURVEY:ALL_UKBL)+(Y_SURVEY:ALL_UKBM),data=Xm5, na.action="na.exclude")
    fit_m=lm(var5~AGE+Y_SURVEY+ALL_UKBL+ALL_UKBH+(Y_SURVEY:ALL_UKBL)+(Y_SURVEY:ALL_UKBH),data=Xm5, na.action="na.exclude")
    fit_l=lm(var5~AGE+Y_SURVEY+ALL_UKBM+ALL_UKBH+(Y_SURVEY:ALL_UKBM)+(Y_SURVEY:ALL_UKBH),data=Xm5, na.action="na.exclude")
    interp = lm(var5~AGE+Y_SURVEY+ALL_UKB+SURVEY:ALL_UKB,data=Xm5, na.action="na.exclude")    
  }
  detach(Xm)
  list(firsts=firsts, fit_h=fit_h, fit_m=fit_m, fit_l=fit_l, interp=interp)
}