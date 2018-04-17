mlr.adw<-function(regression)
{
#This is Residuals Checking on Multiple Linear Regression using R
#by Astuti Dewi Warawati

resi<-resid(result) 
y.hat<-fitted(regression)

library(lawstat)
library(lmtest)
library(nortest)
library(car)
library(MASS)

normality<-lillie.test(resi)
homogenitas<-bptest(regression)
autokol<-runs.test(resi)
multikol<-vif(regression)

windows()  # make new device
split.screen(c(2,2)) #prepare screen 
screen(1)
qqnorm(resi,col="red",
xlab="Standardized Residuals",
ylab="Percent",
main="Normal Probality Plot of Residuals")
qqline(resi,col="blue")

screen(2)
plot(y.hat, resi,col="red",
xlab="Fitted values",
ylab="Residuals",
main="Residuals vs Fitted")
abline(h=0)


screen(3)
hist(resi,
main="Histogram of the Residuals",
xlab="Residual",
ylab="Frequency")

screen(4)
plot (y.hat,resi,col="red",
main="Residuals vs The Order of The Data", 
xlab="Observation Order", ylab="Residual")
abline(h=0)
lines(y.hat,resi,col="blue")

list(Regression.coefficient=coef(result),
     ANOVA=anova(result),
     Resduals=resi, 
     Fitted.value=y.hat)
}

#Now, lets test it!
y<- c(553,590,608,682,752,725,834,752,845,960) 
x<- c(31,38,48,52,63,67,75,84,89,99) 
z<-rnorm(10,5,.25)
my.data<-data.frame(x,y,z)
my.reg<-lm(y~.,data=my.data)
#y as response variable and the others are the regressors

mlr.adw(my.reg) #call the function 

