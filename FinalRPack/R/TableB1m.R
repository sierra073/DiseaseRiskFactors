TableB1m=function(){
one=TableB1prem(Xm$SMOKCU,3)
PACKYRSCU=Xm$PACKYRS[Xm$SMOKER3==2]
two=TableB1prem(PACKYRSCU,1)
three=TableB1prem(Xm$SMOKEX,3)
PACKYRSFO=Xm$PACKYRS[Xm$SMOKER3==1]
four=TableB1prem(PACKYRSFO,2)
five=TableB1prem(Xm$INACTIVM,3)
six=TableB1prem(Xm$RPOLSAT,0)
seven=TableB1prem(Xm$SFIBRG,0)
eight=TableB1prem(Xm$TC,0)
nine=TableB1prem(Xm$HTC,3)
ten=TableB1prem(Xm$TT_CHOL,3)
eleven=TableB1prem(Xm$M_SBP,0)
twelve=TableB1prem(Xm$M_DBP,0)
thirt=TableB1prem(Xm$MBP0,3)
fourt=TableB1prem(Xm$HBP,3)
fift=TableB1prem(Xm$TT_BP,3)
sixt=TableB1prem(Xm$BMI,0)
sevent=TableB1prem(Xm$ROVERWT,0)
eightt=TableB1prem(Xm$ROBESE,0)
list(one=one,two=two,three=three,four=four,five=five,six=six,seven=seven,eight=eight,nine=nine,ten=ten,eleven=eleven,twelve=twelve,thirt=thirt,fourt=fourt,fift=fift,sixt=sixt,sevent=sevent,eightt=eightt)
}