table1<-function(data) {
# put summary of occupation by year into dataframe
output.o<-as.data.frame(unclass(table(data$SURVEY,data$ALL_UKB)))
colnames(output.o)<-c("Occ.high", "Occ.med", "Occ.low")
output.o$Occ.sum<-apply(output.o, 1, function(x) sum(x[1:3]))
output.o$Occ.high.per<-round(100*output.o$Occ.high/output.o$Occ.sum, digits=1)
output.o$Occ.med.per<-round(100*output.o$Occ.med/output.o$Occ.sum, digits=1)
output.o$Occ.low.per<-round(100*output.o$Occ.low/output.o$Occ.sum, digits=1)

# combine summary of eduation by year into the same dataframe
output.o.e<-cbind(output.o,as.data.frame(unclass(table(data$SURVEY,data$NEDUC3))))
colnames(output.o.e)[8:10]<-c("Edu.high", "Edu.med", "Edu.low")
output.o.e$Edu.sum<-apply(output.o.e, 1, function(x) sum(x[8:10]))
output.o.e$Edu.high.per<-round(100*output.o.e$Edu.high/output.o.e$Edu.sum, digits=1)
output.o.e$Edu.med.per<-round(100*output.o.e$Edu.med/output.o.e$Edu.sum, digits=1)
output.o.e$Edu.low.per<-round(100*output.o.e$Edu.low/output.o.e$Edu.sum, digits=1)

# data cleaning and table transpose
table1<-t(output.o.e[,c(4:7, 11:14)])
table1[1,]<-round(table1[1,], digits=0)

return (table1)
}


