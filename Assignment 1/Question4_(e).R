load("databp.Rdata")
data=databp
#Predictive mean matching
fit=lm(data$recovtime~.,data=data)
prediction=predict(fit,newdata=data)
donor=c()
for (k in c(4,10,22)){
distance_k=c()
for (i in 1:length(prediction)){
a=sqrt((prediction[i]-prediction[k])**2)
distance_k=c(distance_k,a)
}
distance_k[c(4,10,22)]=Inf
donor=c(donor,data$recovtime[which.min(distance_k)])
}
data$recovtime[c(4,10,22)]=donor
mean(data$recovtime)
sd(data$recovtime)/sqrt(length(data$recovtime))
cor(data$recovtime,data$logdose,method="pearson")
cor(data$recovtime,data$bloodp,method="pearson")