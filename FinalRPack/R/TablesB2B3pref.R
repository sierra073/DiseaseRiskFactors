TablesB2B3pref=function(var,vartype) {
  attach(Xf)
  #"1993"/baseline prevalence values setup
  firsts=c(rep(0,3))
  #discrete variable
  if (vartype==1){
    for (i in 1:3) {
      firsts[i]=100*sum(var[SURVEY==1993 & ALL_UKB==(i-1)],na.rm=TRUE)/length(SURVEY[SURVEY==1993 & ALL_UKB==(i-1)][!is.na(SURVEY[SURVEY==1993 & ALL_UKB==(i-1)])])
    }
    #trend and interaction regression models
    fit_h=lm(100*var~AGE+Y_SURVEY+ALL_UKBL+ALL_UKBM+(Y_SURVEY:ALL_UKBL)+(Y_SURVEY:ALL_UKBM),data=Xf)
    fit_m=lm(100*var~AGE+Y_SURVEY+ALL_UKBL+ALL_UKBH+(Y_SURVEY:ALL_UKBL)+(Y_SURVEY:ALL_UKBH),data=Xf)
    fit_l=lm(100*var~AGE+Y_SURVEY+ALL_UKBM+ALL_UKBH+(Y_SURVEY:ALL_UKBM)+(Y_SURVEY:ALL_UKBH),data=Xf)
    interp = lm(100*var~AGE+Y_SURVEY+ALL_UKB+SURVEY:ALL_UKB,data=Xf) 
  }
  #continuous variable
  if (vartype==2){
    for (i in 1:3) {
      firsts[i]=mean(var[SURVEY==1993 & ALL_UKB==(i-1)],na.rm=TRUE)
    }
    fit_h=lm(var~AGE+Y_SURVEY+ALL_UKBL+ALL_UKBM+(Y_SURVEY:ALL_UKBL)+(Y_SURVEY:ALL_UKBM),data=Xf)
    fit_m=lm(var~AGE+Y_SURVEY+ALL_UKBL+ALL_UKBH+(Y_SURVEY:ALL_UKBL)+(Y_SURVEY:ALL_UKBH),data=Xf)
    fit_l=lm(var~AGE+Y_SURVEY+ALL_UKBM+ALL_UKBH+(Y_SURVEY:ALL_UKBM)+(Y_SURVEY:ALL_UKBH),data=Xf)
    interp = lm(var~AGE+Y_SURVEY+ALL_UKB+SURVEY:ALL_UKB,data=Xf)
  }
  #variable that needs a log-transform
  if (vartype==3){
    #take geometric means for prevalence 
    for (i in 1:3){
      firsts[i]=exp(mean(log(var[Xf$SURVEY==1993 & ALL_UKB==(i-1)]),na.rm=TRUE))
    }
    var3=log(var[!is.na(var)])
    Xf3=subset(Xf,var!="NA")
    fit_h=lm(var3~AGE+Y_SURVEY+ALL_UKBL+ALL_UKBM+(Y_SURVEY:ALL_UKBL)+(Y_SURVEY:ALL_UKBM),data=Xf3, na.action="na.exclude")
    fit_m=lm(var3~AGE+Y_SURVEY+ALL_UKBL+ALL_UKBH+(Y_SURVEY:ALL_UKBL)+(Y_SURVEY:ALL_UKBH),data=Xf3, na.action="na.exclude")
    fit_l=lm(var3~AGE+Y_SURVEY+ALL_UKBM+ALL_UKBH+(Y_SURVEY:ALL_UKBM)+(Y_SURVEY:ALL_UKBH),data=Xf3, na.action="na.exclude")
    interp = lm(var3~AGE+Y_SURVEY+ALL_UKB+SURVEY:ALL_UKB,data=Xf3, na.action="na.exclude")    
  }
  #current smoker pack-years, needs special treatment and log-transform  
  if (vartype==4){
    for (i in 1:3) {
      firsts[i]=exp(mean(log(PACKYRS[SMOKER3==2 & SURVEY==1993 & ALL_UKB==(i-1)]),na.rm=TRUE))
    }
    var4=log(var[!is.na(var)])
    Xf4=subset(Xf,PACKYRS!="NA" & SMOKER3==2)
    fit_h=lm(var4~AGE+Y_SURVEY+ALL_UKBL+ALL_UKBM+(Y_SURVEY:ALL_UKBL)+(Y_SURVEY:ALL_UKBM),data=Xf4, na.action="na.exclude")
    fit_m=lm(var4~AGE+Y_SURVEY+ALL_UKBL+ALL_UKBH+(Y_SURVEY:ALL_UKBL)+(Y_SURVEY:ALL_UKBH),data=Xf4, na.action="na.exclude")
    fit_l=lm(var4~AGE+Y_SURVEY+ALL_UKBM+ALL_UKBH+(Y_SURVEY:ALL_UKBM)+(Y_SURVEY:ALL_UKBH),data=Xf4, na.action="na.exclude")
    interp = lm(var4~AGE+Y_SURVEY+ALL_UKB+SURVEY:ALL_UKB,data=Xf4, na.action="na.exclude")    
  }
  #former smoker pack-years, needs special treatment and log-transform
  if (vartype==5){
    for (i in 1:3) {
      firsts[i]=exp(mean(log(PACKYRS[SMOKER3==1 & SURVEY==1993 & ALL_UKB==(i-1)]),na.rm=TRUE))
    }
    var5=log(var[!is.na(var)])
    Xf5=subset(Xf,PACKYRS!="NA" & SMOKER3==1)
    fit_h=lm(var5~AGE+Y_SURVEY+ALL_UKBL+ALL_UKBM+(Y_SURVEY:ALL_UKBL)+(Y_SURVEY:ALL_UKBM),data=Xf5, na.action="na.exclude")
    fit_m=lm(var5~AGE+Y_SURVEY+ALL_UKBL+ALL_UKBH+(Y_SURVEY:ALL_UKBL)+(Y_SURVEY:ALL_UKBH),data=Xf5, na.action="na.exclude")
    fit_l=lm(var5~AGE+Y_SURVEY+ALL_UKBM+ALL_UKBH+(Y_SURVEY:ALL_UKBM)+(Y_SURVEY:ALL_UKBH),data=Xf5, na.action="na.exclude")
    interp = lm(var5~AGE+Y_SURVEY+ALL_UKB+SURVEY:ALL_UKB,data=Xf5, na.action="na.exclude")    
  }
  detach(Xf)
  list(firsts=firsts, fit_h=fit_h, fit_m=fit_m, fit_l=fit_l, interp=interp)
}