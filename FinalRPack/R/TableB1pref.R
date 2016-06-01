TableB1pref=function(var,type){
  attach(Xf)
  #current smoker pack-years, special treatment
  if (type==1){  
    a=mean(PACKYRS[ALL_UKB==0 & SMOKER3==2],na.rm=TRUE)
    b=mean(PACKYRS[ALL_UKB==1 & SMOKER3==2],na.rm=TRUE)
    c=mean(PACKYRS[ALL_UKB==2 & SMOKER3==2],na.rm=TRUE)
    d=pairwise.t.test(PACKYRS[SMOKER3==2], ALL_UKB[SMOKER3==2],p.adjust.method = "none")}
  #former smoker pack-years, special treatment
  if (type==2){  
    a=mean(PACKYRS[ALL_UKB==0 & SMOKER3==1],na.rm=TRUE)
    b=mean(PACKYRS[ALL_UKB==1 & SMOKER3==1],na.rm=TRUE)
    c=mean(PACKYRS[ALL_UKB==2 & SMOKER3==1],na.rm=TRUE)
    d=pairwise.t.test(PACKYRS[SMOKER3==1], ALL_UKB[SMOKER3==1],p.adjust.method = "none")}
  #continuous variable
  if (type==0){
    a=mean(var[ALL_UKB==0],na.rm=TRUE)
    b=mean(var[ALL_UKB==1],na.rm=TRUE)
    c=mean(var[ALL_UKB==2],na.rm=TRUE)
    d=pairwise.t.test(var, ALL_UKB,p.adjust.method = "none")}
  #discrete variable
  if (type==3){
    a=sum(var[ALL_UKB==0],na.rm=TRUE)/length(ALL_UKB[ALL_UKB==0][!is.na(ALL_UKB[ALL_UKB==0])])
    b=sum(var[ALL_UKB==1],na.rm=TRUE)/length(ALL_UKB[ALL_UKB==1][!is.na(ALL_UKB[ALL_UKB==1])])
    c=sum(var[ALL_UKB==2],na.rm=TRUE)/length(ALL_UKB[ALL_UKB==2][!is.na(ALL_UKB[ALL_UKB==2])])
    d=pairwise.t.test(var, ALL_UKB,p.adjust.method = "none")
  }
  detach(Xf)
  l=list(high=a,medium=b,low=c,d)
  return(l)
}