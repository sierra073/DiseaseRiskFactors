FigB1=function(){
  attach(Xm)
  
  #will output to a png file
  png('plot1m.png',width=600,height=420)  
  
  mage <-mean(AGE[!is.na(ROVERWT) & !is.na(ALL_UKB)])
  
  #initial plot dimensions setup
  plot(ROVERWT,YY_QUART,type='n',axes=F,cex=1.0,labels=c(48,11,3),xlim=c(1,48),xlab='',ylim=c(20,85),ylab='')
  
  #x-axis
  axis(side=1, at=c(1:48), labels=F)
  mtext('1993    1994    1995    1996    1997    1998    1999    2000    2001    2002    2003    2004',side=1,line=.5)
  mtext('Quarterly Data', side=1,line=1.5)
  
  #left y-axis
  axis(side=2, at=c(20,30,40,50,60,70,80),labels=c(20,30,40,50,60,70,80),las=2, cex.axis=0.8)
  mtext('% Overweight  (age-adjusted)',side=2,line=2.5)
  
  #right y-axis
  axis(side=4, at=c(20,30,40,50,60,70,80),labels=c(20,30,40,50,60,70,80),las=2, cex.axis=0.8)
  
  #set up title
  box()
  mtext('OVERWEIGHT PREVALENCE 1993-2004 : GENEVA MEN (35-74 yrs)',side=3,line=1,cex.axis=1.0)
  
  #plot background fluctuations
  means <- tapply(100*ROVERWT, list(ALL_UKB,YY_QUART), mean,na.rm=TRUE)
  lines(means[1,],type='l',col='red',lty=1,lwd=.65)
  lines(means[2,],type='l',col='purple',lty=4,lwd=.65)
  lines(means[3,],type='l',col='blue',lty=8,lwd=.65)
  
  #plot regression lines
  oc.lm<-lm(100*(ROVERWT)~ AGE+YY_QUART + ALL_UKBL + ALL_UKBM + YY_QUART:ALL_UKBL + YY_QUART:ALL_UKBM)
  abline(coef(oc.lm)[1]+coef(oc.lm)[2]*mage+coef(oc.lm)[4]*1+coef(oc.lm)[5]*0,coef(oc.lm)[3]+coef(oc.lm)[6]*1+coef(oc.lm)[7]*0, col='blue',lty=8,lwd=3)
  abline(coef(oc.lm)[1]+coef(oc.lm)[2]*mage+coef(oc.lm)[4]*0+coef(oc.lm)[5]*1,coef(oc.lm)[3]+coef(oc.lm)[6]*0+coef(oc.lm)[7]*1, col='purple',lty=4,lwd=3)
  abline(coef(oc.lm)[1]+coef(oc.lm)[2]*mage+coef(oc.lm)[4]*0+coef(oc.lm)[5]*0,coef(oc.lm)[3]+coef(oc.lm)[6]*0+coef(oc.lm)[7]*0, col='red',lty=1,lwd=3)
  
  #make legend
  groups<-c('LOW OCCUPATION','MED OCCUPATION','HIGH OCCUPATION')
  legend("bottomright", groups, col=c('blue','purple','red'), lty=c(8,4,1), lwd=2, cex=.75)
  
  dev.off()
  detach(Xm)}