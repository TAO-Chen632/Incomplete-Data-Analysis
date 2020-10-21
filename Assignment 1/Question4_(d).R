load("databp.Rdata")
data=databp
#Stochastic regression imputation
fit=lm(data$recovtime~.,data=data)
prediction=predict(fit,newdata=data)
ind_na=which(is.na(databp$recovtime))
set.seed(32)
data$recovtime[ind_na]=prediction[ind_na]+rnorm(length(ind_na),0,sigma(fit))
mean(data$recovtime)
sd(data$recovtime)/sqrt(length(data$recovtime))
cor(data$recovtime,data$logdose,method="pearson")
cor(data$recovtime,data$bloodp,method="pearson")