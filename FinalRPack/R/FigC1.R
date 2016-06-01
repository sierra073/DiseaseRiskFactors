FigC1=function(){
  var=Xf$PACKYRS[Xf$SMOKER3==2]
  png('dhist.png',width=475,height=375)
  hist(var,col="lightblue",xlab="Current Pack-Years",main=NULL)
  dev.off()
}