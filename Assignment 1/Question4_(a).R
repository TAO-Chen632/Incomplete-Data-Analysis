load("databp.Rdata")
#Complete case analysis
ind_na=which(is.na(databp$recovtime))
mean(databp$recovtime,na.rm = TRUE)
sd(databp$recovtime,na.rm = TRUE)/sqrt(length(databp$recovtime[-ind_na]))
cor(databp$recovtime,databp$logdose,use="complete",method="pearson")
cor(databp$recovtime,databp$bloodp,use="complete",method="pearson")