rm(list = ls())

# 1.Data --------------------------------------------------------------------

library(tidyverse)
library(hexView)
data<-readEViews("/Users/Mongol/Documents/Eviews/ElementsOfForecasting/fcst_05/fcst5input.wf1")
mydata<-filter(data,between(Date,as.Date("1955-01-01"),as.Date("1993-12-01")))
attach(mydata)
rtrr<-ts(mydata$RTRR,start = c(1955,1),end = c(1993,12),frequency = 12)
# View(rtrr)
print(rtrr)

# Figure 5.14
library(ggplot2)
library(graphics)
plot(RTRR~Date,type="l")
ggplot(mydata,aes(Date,RTRR))+geom_line(size=1)+theme_classic()


# 2.Linear trend ------------------------------------------------------------

# Table 5.1 Retail sales, linear trend regression

# First method to take a Eviews result : to use data.frame

library(lmtest)

TIME<-1:length(rtrr)
reg1<-lm(rtrr~TIME)
result1<-summary(reg1)
result1
"Rsquared"=signif(result1$r.squared,digits = 6)
"Adjusted Rsquared"=signif(result1$adj.r.squared,digits = 6)
"SE"=round(result1$sigma,digits = 6)
"SSR"=ifelse(deviance(reg1)>1000000,round(deviance(reg1),digits = -8),round(deviance(reg1),digits = 6))
"Log lokelihood"=logLik(reg1)
"Durbin-Watson stat."=round(as.numeric(dwtest(reg1))[1],digits = 6)
"Mean dependent var."=mean(RTRR)
"SD dependent var."=sd(RTRR)
"Akaike info criterion"=AIC(reg1)
"Schwarz criterion"=BIC(reg1)
"F-statistic"=result1$r.squared/(1-result1$r.squared)*(length(RTRR)-(length(reg1$coefficients))/(length(reg1$coefficients)-1))
"p-value"=round(pf(result1$r.squared/(1-result1$r.squared)*(length(RTRR)-(length(reg1$coefficients))/(length(reg1$coefficients)-1)),length(reg1$coefficients)-1,length(RTRR)-(length(reg1$coefficients)),lower.tail = F),digits = 6)
criteria<-c("Rsquared","Adjusted Rsquared","SE","SSR","Log lokelihood","Durbin-Watson stat.","Mean dependent var.","SD dependent var.","Akaike info criterion","Schwarz criterion","F-statistic","p-value")
Table5.1<-data.frame(Rsquared,`Adjusted Rsquared`,SE,SSR,`Log lokelihood`,`Durbin-Watson stat.`,`Mean dependent var.`,`SD dependent var.`,`Akaike info criterion`,`Schwarz criterion`,`F-statistic`,`p-value`)
print(Table5.1)


# Second method to take Eviews result : Create own function
# GER - Get Eviews Result
ger<-function(regression,result,dependent.var){print(data.frame(value=c(result$r.squared,result$adj.r.squared,result$sigma,deviance(regression),logLik(regression),sum(diff(regression$residuals,lag = 1)^2)/sum(regression$residuals^2),mean(dependent.var),sd(dependent.var),AIC(regression),BIC(regression),result$r.squared/(1-result$r.squared)*(length(dependent.var)-(length(regression$coefficients))/(length(regression$coefficients)-1)),pf(result$r.squared/(1-result$r.squared)*(length(dependent.var)-(length(regression$coefficients))/(length(regression$coefficients)-1)),length(regression$coefficients)-1,length(dependent.var)-(length(regression$coefficients)),lower.tail = F)),row.names = c("Rsquared","Adjusted Rsquared","SE","SSR","Log lokelihood","Durbin-Watson stat.","Mean dependent var.","SD dependent var.","Akaike info criterion","Schwarz criterion","F-statistic","p-value")))}
Table5.1.2<-ger(reg1,result1,RTRR)

print(result1$coefficients,digits = 6)

# Figure 5.15 Retail sales, linear trend residual plot

par(mar=c(4,4,4,4))
plot(Date,RTRR,type="l",lwd=2,main="Figure 5.15 Retail sales, linear trend residual plot",ylab="",yaxt="n",ylim=c(-220000,200000),bty="o",xlab="TIME")
par(new=T)
plot(Date,reg1$fitted.values,lwd=2,type="l",lty=2,ylab="",yaxt="n",ylim=c(-220000,200000),xaxt="n",xlab="")
axis(4,at=c(seq(-50000,200000,50000)),labels = c("-50000","0","50000","100000","150000","200000"),las=2,cex.axis=1,xlab="")
par(new=T)
plot(Date,reg1$residuals,col="red",lwd=2,type="l",ylab="",yaxt="n",ylim=c(-40000,110000),xaxt="n",xlab="")
axis(2,at=c(seq(-40000,40000,20000)),labels = c("-40000","-20000","0","20000","40000"),las=2,cex.axis=1)
abline(h=mean(reg1$residuals))
abline(h=mean(reg1$residuals)+result1$sigma)
abline(h=mean(reg1$residuals)-result1$sigma)

legend("topleft",c("Residual","Actual","Fitted"),lty=c(1,1,2),
       col=c("red","black","black"),cex=0.7)

# X axis simulation
par(mar=c(4,4,4,4))
plot.ts(rtrr,type="l",lwd=2,main="Figure 5.15 Retail sales, linear trend residual plot",ylab="",yaxt="n",xaxt="n",ylim=c(-220000,200000),bty="o",xlab="TIME")
lines(ts(reg1$fitted.values,start = c(1955,1),end = c(1993,12),frequency = 12),lwd=2,lty=2,col="black")
axis(1,at=c(seq(1955,1990,5)),labels =c(seq(55,90,5)))
axis(4,at=c(seq(-50000,200000,50000)),labels = c("-50000","0","50000","100000","150000","200000"),las=2,cex.axis=1,xlab="")
par(new=T)
resid1<-ts(reg1$residuals,start = c(1955,1),end = c(1993,12),frequency = 12)
plot(resid1,col="red",lwd=2,type="l",ylab="",yaxt="n",ylim=c(-40000,110000),xaxt="n",xlab="")
axis(2,at=c(seq(-40000,40000,20000)),labels = c("-40000","-20000","0","20000","40000"),las=2,cex.axis=1)
abline(h=mean(resid1))
abline(h=mean(resid1)+result1$sigma)
abline(h=mean(resid1)-result1$sigma)

legend("topleft",c("Residual","Actual","Fitted"),lty=c(1,1,2),
       col=c("red","black","black"),cex=0.7)


# 3.Quadratic trend ---------------------------------------------------------


# Table 5.2 Retail sales, quadratic trend regression

TIME2<-TIME^2
reg2<-lm(rtrr~TIME+I(TIME^2))

reg2<-lm(rtrr~TIME+TIME2)
result2<-summary(reg2)

print(result2$coefficients)
Table5.2<-ger(reg2,result2,rtrr) # this function use with lmtest function.

# Figure 5.16 Retail sales, quadratic trend residual plot

par(mar=c(4,4,4,4))
plot(rtrr,type="l",lwd=2,main="Figure 5.16 Retail sales, quadratic trend residual plot",ylab="",yaxt="n",xaxt="n",ylim=c(-140000,200000),bty="o",xlab="TIME")
lines(ts(reg2$fitted.values,start = c(1955,1),end = c(1993,12),frequency = 12),lwd=2,col="blue",lty=2)
axis(1,at=c(seq(1955,1990,5)),labels = c(seq(55,90,5)))
axis(4,at=c(seq(0,200000,50000)),labels = c("0","50000","100000","150000","200000"),las=2,cex.axis=1,xlab="")
#  curve(ts(reg2$residuals,start = c(1955,1),end = c(1993,12),frequency = 12),col="red",lwd=2,type="l",ylab="",yaxt="n",ylim=c(-10000,20000),xaxt="n",xlab="")
resid2<-ts(reg2$residuals,start = c(1955,1),end = c(1993,12),frequency = 12)
par(new=T)
plot(resid2,col="red",lwd=2,type="l",ylab="",yaxt="n",ylim=c(-10000,20000),xaxt="n",xlab="")
axis(2,at=c(seq(-10000,10000,5000)),labels = c("-10000","-5000","0","5000","10000"),las=2,cex.axis=1)
abline(h=mean(resid2))
abline(h=mean(resid2)+result2$sigma)
abline(h=mean(resid2)-result2$sigma)

legend("topleft",c("Residual","Actual","Fitted"),lty=c(1,1,2),
       col=c("red","black","blue"),cex=0.7)


# 4.Log-linear trend --------------------------------------------------------


# Table 5.3 Retail sales, log-linear trend regression

reg3<-lm(log(rtrr)~TIME)
result3<-summary(reg3)

print(result3$coefficients)
Table5.3<-ger(reg3,result3,log(rtrr))

par(mar=c(4,4,4,4))
plot(log(rtrr),type="l",lwd=2,main="Figure 5.17 Retail sales, log-linear trend residual plot",ylab="",yaxt="n",xaxt="n",ylim=c(6,13),bty="o",xlab="TIME")
lines(ts(reg3$fitted.values,start = c(1955,1),end = c(1993,12),frequency = 12),lwd=2,lty=2,col="blue")
axis(1,at=c(seq(1955,1990,5)),labels = c(seq(55,90,5)))
axis(4,at=c(seq(9,13,1)),labels = c("9","10","11","12","13"),las=2,cex.axis=1,xlab="")
resid3<-ts(reg3$residuals,start = c(1955,1),end = c(1993,12),frequency = 12)
par(new=T)
plot(resid3,col="red",lwd=2,type="l",ylab="",yaxt="n",ylim=c(-.2,.7),xaxt="n",xlab="")
axis(2,at=c(seq(-.2,.3,.1)),labels = c("-0.2","-0.1","0.0","0.1","0.2","0.3"),las=2,cex.axis=1)
abline(h=mean(resid3))
abline(h=mean(resid3)+result3$sigma)
abline(h=mean(resid3)-result3$sigma)

legend("topleft",c("Residual","Actual","Fitted"),lty=c(1,1,2),
       col=c("red","black","blue"),cex=.7)



# 5.Exponential trend -------------------------------------------------------


# table 5.4 Retail sales, exponential trend regression

parameters<-lm(log(rtrr)~TIME)
res<-summary(parameters)

reg4<-nls(rtrr~a*exp(b*TIME),start =c(a=exp(res$coefficients[1]),b=res$coefficients[2]),trace=T)
reg4
result4<-summary(reg4)

print(result4$coefficients)
result4
gernls<-function(regression,result,dependent.var,resid,parnum){print(data.frame(value=c(1-deviance(regression)/(sum((dependent.var-mean(dependent.var))^2)),1-(deviance(regression)/(sum((dependent.var-mean(dependent.var))^2)))*((length(dependent.var)-1)/(length(dependent.var)-parnum-1)),result$sigma,deviance(regression),logLik(regression),sum(diff(resid,lag = 1)^2)/sum(resid^2),mean(dependent.var),sd(dependent.var),AIC(regression),BIC(regression),as.numeric((1-deviance(regression)/(sum((dependent.var-mean(dependent.var))^2)))/(deviance(regression)/(sum((dependent.var-mean(dependent.var))^2)))*((length(dependent.var)-parnum)/(parnum-1))),pf((1-deviance(regression)/(sum((dependent.var-mean(dependent.var))^2)))/(deviance(regression)/(sum((dependent.var-mean(dependent.var))^2)))*((length(dependent.var)-parnum)/(parnum-1)),parnum-1,length(dependent.var)-parnum,lower.tail = F)),row.names = c("Rsquared","Adjusted Rsquared","SE","SSR","Log lokelihood","Durbin-Watson stat.","Mean dependent var.","SD dependent var.","Akaike info criterion","Schwarz criterion","F-statistic","p-value")))}
Table5.4<-gernls(reg4,result4,rtrr,result4$residuals,2)

# Figure 5.18 Retail sales, Exponential trend residual plot

par(mar=c(4,4,4,4))
plot(rtrr,type="l",lwd=2,main="Figure 5.18 Retail sales, exponential trend residual plot",ylab="",yaxt="n",xaxt="n",ylim=c(-110000,200000),bty="o",xlab="TIME")
lines(ts(predict(reg4,list(Date)),start = c(1955,1),end = c(1993,12),frequency = 12),col="blue",lty=2,lwd=2)
axis(1,at=c(seq(1955,1990,5)),labels = c(seq(55,90,5)))
axis(4,at=c(seq(0,200000,50000)),labels = c("0","50000","100000","150000","200000"),las=2,cex.axis=1,xlab="")
resid4<-rtrr-predict(reg4)
par(new=T)
plot(resid4,col="red",lwd=2,type="l",ylab="",yaxt="n",ylim=c(-20000,50000),xaxt="n",xlab="")
  
axis(2,at=c(seq(-20000,20000,10000)),labels = c("-20000","-10000","0","10000","20000"),las=2,cex.axis=1)
abline(h=mean(resid4))
abline(h=mean(resid4)+result4$sigma)
abline(h=mean(resid4)-result4$sigma)

legend("topleft",c("Residual","Actual","Fitted"),lty=c(1,1,2),
       col=c("red","black","black"),cex=.7)


# 6.Model selection ---------------------------------------------------------


# Table 5.5 Model selection criteria linear,quadratic and exponential trend models
linear_trend<-c(AIC(reg1),BIC(reg1))
quadratic_trend<-c(AIC(reg2),BIC(reg2))
exponential_trend<-c(AIC(reg4),BIC(reg4))
Table5.5<-data.frame(linear_trend,quadratic_trend,exponential_trend,row.names = c("AIC","SIC"))
print(Table5.5)


# 7.Quadratic trend forecast ------------------------------------------------


# Figure 5.19 Retail sales: History 1990.01-1993.12 and quadratic trend forecast 1994.01-1994.12

History<-ts(data[1081:1128,4],start = c(1990,1),end = c(1993,12),frequency = 12)
Realization<-ts(data[1081:1140,4],start = c(1990,1),end = c(1994,12),frequency = 12)

# R style forecast

library("forecast")
plot(forecast(History,h=12))



t<-1:length(History)
t2<-t^2
q<-lm(History~t+t2)
ts.q<-ts(q$fitted.values,start = c(1990,01),end = c(1993,12),frequency = 12)
qforecast<-forecast(ts.q,h=12,level=.95)
par(mar=c(4,6,4,2))
plot(History,ylim = c(150000,200000),lwd=2,type="l",xlim=c(1990.01,1995.01),yaxt="n",xaxt="n",ylab="History and quadratic forecast",main="Figure 5.19 History and forecast")
axis(1,at=c(seq(1990,1994.7,.5)),labels = c("90:01","90:07","91:01","91:07","92:01","92:07","93:01","93:07","94:01","94:07"),cex.axis=.7)
axis(2,at=c(seq(150000,200000,10000)),labels=c("150000","160000","170000","180000","190000","200000"),las=2,cex.axis=.8)
lines(qforecast$mean,lty=2,lwd=2)
lines(qforecast$upper,lty=2,lwd=2)
lines(qforecast$lower,lty=2,lwd=2)
legend("topleft",c("History","Forecast"),lty=c(1,2),lwd=2,col=c("black","black"),cex=.7)

# Figure 5.20 Retail sales: History 1990.01-1993.12 and quadratic trend forecast and realization 1994.01-1994.12
plot(Realization,type="l",lwd=2,ylim = c(150000,200000),xlim=c(1990.01,1995.01),yaxt="n",xaxt="n",ylab="History,quadratic forecast and realization",main="Figure 5.20 History,forecast and realization")
axis(1,at=c(seq(1990,1994.7,.5)),labels = c("90:01","90:07","91:01","91:07","92:01","92:07","93:01","93:07","94:01","94:07"),cex.axis=.7)
axis(2,at=c(seq(150000,200000,10000)),labels=c("150000","160000","170000","180000","190000","200000"),las=2,cex.axis=.8)
lines(qforecast$mean,lty=2,lwd=2,col="blue")
lines(qforecast$upper,lty=2,lwd=2,col="blue")
lines(qforecast$lower,lty=2,lwd=2,col="blue")

legend("topleft",c("Realization","Forecast"),lty=c(1,2),lwd=2,col=c("black","blue"),cex=.7)


# 8.Linear trend forecast ---------------------------------------------------


# Figure 5.21 Retail sales: History 1990.01-1993.12; and linear trend forecast,1994.01-1994.12

l<-lm(History~t)
ts.l<-ts(l$fitted.values,start = c(1990,01),end = c(1993,12),frequency = 12)
lforecast<-forecast(ts.l,h=12,level = .95)

par(mar=c(4,6,4,2))
plot(History,ylim = c(100000,200000),type="l",lwd=2,xlim=c(1990.01,1995.01),yaxt="n",xaxt="n",ylab="History and linear forecast",main="Figure 5.21 History and forecast")
axis(1,at=c(seq(1990,1994.7,.5)),labels = c("90:01","90:07","91:01","91:07","92:01","92:07","93:01","93:07","94:01","94:07"),cex.axis=.7)
axis(2,at=c(seq(100000,200000,20000)),labels=c("100000","120000","140000","160000","180000","200000"),las=2,cex.axis=.8)
lines(lforecast$mean,lty=2,lwd=2)
lines(lforecast$upper,lty=2,lwd=2)
lines(lforecast$lower,lty=2,lwd=2)
legend("topleft",c("History","Forecast"),lty=c(1,2),lwd=2,col=c("black","black"),cex=.7)
library(forecast)

# Figure 5.22 Retail sales: History 1990.01-1993.12; and linear trend forecast and realization,1994.01-1994.12
par(mar=c(4,6,4,2))
plot(Realization,lty=c(1,2),lwd=2,ylim = c(100000,200000),type="l",xlim=c(1990.01,1995.01),ylab="History, linear forecast and realization",yaxt="n",xaxt="n",main="Figure 5.22 History, forecast and realization")
axis(1,at=c(seq(1990,1994.7,.5)),labels = c("90:01","90:07","91:01","91:07","92:01","92:07","93:01","93:07","94:01","94:07"),cex.axis=.7)
axis(2,at=c(seq(100000,200000,20000)),labels=c("100000","120000","140000","160000","180000","200000"),las=2,cex.axis=.8)
lines(lforecast$mean,lty=2,lwd=2,col="blue")
legend("topleft",c("Realization","Forecast"),lty=c(1,2),lwd=2,col=c("black","blue"),cex=.7)

