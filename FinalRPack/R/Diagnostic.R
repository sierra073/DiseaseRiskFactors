Diagnostic=function(){
  #current smoker pack-years data set-up
  var=Xf$PACKYRS[Xf$SMOKER3==2]
  var=var[!is.na(var)]
  varl=log(var[!is.na(var)])
  Xf1=subset(Xf,Xf$PACKYRS!="NA" & Xf$SMOKER3==2)
  #model construction (4 models) and printing xtables of model summaries and AIC scores
  fitrawo=lm(var~AGE+Y_SURVEY+ALL_UKBL+ALL_UKBH+(Y_SURVEY:ALL_UKBL)+(Y_SURVEY:ALL_UKBH),data=Xf1, na.action="na.exclude")
  print(xtable(summary(fitrawo)))
  print(AIC(fitrawo))
  fitrawe=lm(var~AGE+Y_SURVEY+NEDUC3L+NEDUC3H+(Y_SURVEY:NEDUC3L)+(Y_SURVEY:NEDUC3H),data=Xf1, na.action="na.exclude")
  print(xtable(summary(fitrawe)))
  print(AIC(fitrawe))
  fitlogo=lm(varl~AGE+Y_SURVEY+ALL_UKBL+ALL_UKBH+(Y_SURVEY:ALL_UKBL)+(Y_SURVEY:ALL_UKBH),data=Xf1, na.action="na.exclude")
  print(xtable(summary(fitlogo)))
  print(AIC(fitlogo))
  fitloge=lm(varl~AGE+Y_SURVEY+NEDUC3L+NEDUC3H+(Y_SURVEY:NEDUC3L)+(Y_SURVEY:NEDUC3H),data=Xf1, na.action="na.exclude")
  print(xtable(summary(fitloge)))
  print(AIC(fitloge))
}