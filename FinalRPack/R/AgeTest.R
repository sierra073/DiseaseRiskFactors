AgeTest = function() {
  #To see if there was a significant difference in overall ages across the survey period:
  anova1m=aov(AGE~as.factor(Y_SURVEY),data=Xm)
  print(summary(anova1m))
  anova1f=aov(AGE~as.factor(Y_SURVEY),data=Xf)
  print(summary(anova1f))
  #To see if there was a significant difference in SEP-group ages across the survey period:
  #Occupation groups:
  anova2om=aov(AGE~as.factor(Y_SURVEY)+as.factor(ALL_UKB),data=Xm)
  print(summary(anova2om))
  anova2of=aov(AGE~as.factor(Y_SURVEY)+as.factor(ALL_UKB),data=Xf)
  print(summary(anova2of))
  #Education groups:
  anova2em=aov(AGE~as.factor(Y_SURVEY)+as.factor(NEDUC3),data=Xm)
  print(summary(anova2em))
  anova2ef=aov(AGE~as.factor(Y_SURVEY)+as.factor(NEDUC3),data=Xf)
  print(summary(anova2ef))
  
  #compute and display mean overall age +- sd (both genders separately)
  print(mean(Xm$AGE),na.rm=TRUE)
  print(sd(Xm$AGE),na.rm=TRUE)
  print(mean(Xf$AGE),na.rm=TRUE)
  print(sd(Xf$AGE),na.rm=TRUE)
  #compute and display mean overall age +- sd by occupation and education group
  print(tapply(Xm$AGE,Xm$ALL_UKB,mean))
  print(tapply(Xm$AGE,Xm$ALL_UKB,sd))
  print(tapply(Xf$AGE,Xf$ALL_UKB,mean))
  print(tapply(Xf$AGE,Xf$ALL_UKB,sd))
  print(tapply(Xm$AGE,Xm$NEDUC3,mean))
  print(tapply(Xm$AGE,Xm$NEDUC3,sd))
  print(tapply(Xf$AGE,Xf$NEDUC3,mean))
  print(tapply(Xf$AGE,Xf$NEDUC3,sd))
}