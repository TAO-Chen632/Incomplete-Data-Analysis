load("databp.Rdata")
data=databp
#Mean imputation
mean=mean(data$recovtime,na.rm = TRUE)
data$recovtime[which(is.na(data$recovtime))]=mean
mean(data$recovtime)
sd(data$recovtime)/sqrt(length(data$recovtime))
cor(data$recovtime,data$logdose,method="pearson")
cor(data$recovtime,data$bloodp,method="pearson")