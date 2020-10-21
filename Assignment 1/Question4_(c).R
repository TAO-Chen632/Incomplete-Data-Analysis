load("databp.Rdata")
data=databp
#Mean regression imputation
fit=lm(data$recovtime~.,data=data)
prediction=predict(fit,newdata=data)
ind_na=which(is.na(databp$recovtime))
data$recovtime[ind_na]=prediction[ind_na]
mean(data$recovtime)
sd(data$recovtime)/sqrt(length(data$recovtime))
cor(data$recovtime,data$logdose,method="pearson")
cor(data$recovtime,data$bloodp,method="pearson")