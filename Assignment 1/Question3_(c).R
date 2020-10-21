#Simulating a complete dataset of size 500 on (Y1,Y2)
set.seed(632)
Z1=rnorm(500, mean=0, sd=1)
Z2=rnorm(500, mean=0, sd=1)
Z3=rnorm(500, mean=0, sd=1)
Y1=1+Z1
Y2=5+2*Z1+Z2
#Simulating the observed dataset, i.e., imposing missingness on Y2
a=0
b=2
Y2_missing=Y2
con=a*(Y1-1)+b*(Y2-5)+Z3
for (i in 1:500){
  if (con[i]<0){Y2_missing[i]=NA}
}
#Displaying the figures
pdf("Question3_(c).pdf")
plot(density(Y2_missing[-which(is.na(Y2_missing)==TRUE)]),col="red",main="",xlab="",xlim=c(-2,15),ylim = c(0,0.35))
lines(density(Y2),col="blue")
title(main=expression(paste("The marginal distribution of ",Y[2]," for the complete and observed data")),xlab = "X",ylab = "Density")
legend("top",legend=c("The complete data","The observed data after imposing missingness"),lty=c(1,1),col=c("blue","red"),bty="n")
par(lty=1,pin=c(3,2.25))
dev.off()