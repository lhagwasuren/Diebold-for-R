rm(list = ls())

# 1.Data --------------------------------------------------------------------
library(tidyverse)
library(hexView)
library(uroot)
library(forecast)

data<-readEViews("/Users/Mongol/Documents/Eviews/ElementsOfForecasting/fcst_06/fcst6input.wf1")
mydata<-filter(data,between(Date,as.Date("1946-01-01"),as.Date("1994-11-01")))
house.starts<-ts(mydata$HSTARTS,start = c(1946,1),end = c(1994,11),frequency = 12)
house_starts<-head(house.starts,576)
house_starts2<-tail(house.starts,59)

# Figure 6.4 Housing starts, 1946.01-1994.11
plot.ts(house_starts,ylab="Starts",main="Figure 6.4 Housing Starts 1946.01-1994.11",bty="l",ylim=c(0,250),yaxt="n",xaxt="n",type="l",lwd=2)
axis(2,at=c(seq(0,250,50)),labels = c(seq(0,250,50)),las=1)
axis(1,at=c(seq(1950,1990,5)),labels = c(seq(50,90,5)))


# 2.Seasonal factor -------------------------------------------------------

# Figure 6.5 Housing starts, 1990.01-1994.11
plot(house_starts2,ylab="Starts",yaxt="n",type="l",lwd=2,main="Figure 6.5 Housing starts, 1990.01-1994.11",bty="l",ylim=c(40,160))
# axis(1,at=c(seq(as.Date("1990/1/1"),as.Date("1994/11/1"),by = "2 quarter")),labels = c("90:01","90:07","91:01","91:07","92:01","92:07","93:01","93:07","94:01","94:07"))
axis(2,at=c(seq(40,160,20)),labels = c(seq(40,160,20)),las=1)

D<-seasonal.dummies(house_starts)
reg1<-lm(house_starts~D-1)
result1<-summary(reg1)
result1$coefficients
ger<-function(regression,result,dependent.var){print(data.frame(value=c(result$r.squared,result$adj.r.squared,result$sigma,deviance(regression),logLik(regression),sum(diff(regression$residuals,lag = 1)^2)/sum(regression$residuals^2),mean(dependent.var),sd(dependent.var),AIC(regression),BIC(regression),result$r.squared/(1-result$r.squared)*(length(dependent.var)-(length(regression$coefficients))/(length(regression$coefficients)-1)),pf(result$r.squared/(1-result$r.squared)*(length(dependent.var)-(length(regression$coefficients))/(length(regression$coefficients)-1)),length(regression$coefficients)-1,length(dependent.var)-(length(regression$coefficients)),lower.tail = F)),row.names = c("Rsquared","Adjusted Rsquared","SE","SSR","Log lokelihood","Durbin-Watson stat.","Mean dependent var.","SD dependent var.","Akaike info criterion","Schwarz criterion","F-statistic","p-value")))}
ger(reg1,result1,house_starts)

result1$r.squared


# Figure 6.6 Residual plot
par(mar=c(4,4,4,4))
plot.ts(house_starts,type="l",lwd=2,main="Figure 6.6 Residual plot",ylab="",yaxt="n",xaxt="n",ylim=c(-150,250),bty="o")
lines(ts(reg1$fitted.values,start = c(1946,1),end = c(1993,12),frequency = 12),lwd=2,lty=2,col="blue")
axis(4,at=c(seq(0,250,50)),labels =c(seq(0,250,50)),las=2)
axis(1,at=c(seq(1950,1990,5)),labels = c(seq(50,90,5)),cex.axis=1,xlab="")
par(new=T)
resid1<-ts(reg1$residuals,start = c(1946,1),end = c(1993,12),frequency = 12)
plot(resid1,col="red",lwd=2,type="l",ylab="",yaxt="n",ylim=c(-100,200),xaxt="n",xlab="")
axis(2,at=c(seq(-100,100,50)),labels = c(seq(-100,100,50)),las=2,cex.axis=1)
abline(h=mean(resid1))
abline(h=mean(resid1)+result1$sigma)
abline(h=mean(resid1)-result1$sigma)

legend("topleft",c("Residual","Actual","Fitted"),lty=c(1,1,2),
       col=c("red","black","blue"),cex=0.7)

# Figure 6.7 Estimated seasonal factors, housing starts
plot(reg1$coefficients,maion="Figure 6.7 Estimated seasonal factors, housing starts",type="l",lwd=2,yaxt="n",xlab="Season",ylab="Seasonal factors",ylim=c(80,160),bty="l")
axis(2,at=c(seq(80,160,20)),labels = c(seq(80,160,20)),las=2)


# 3.Forecast --------------------------------------------------------------

house.forecast<-forecast(tail(house_starts,48),h=11,level = .95)
plot(house.forecast,xlim=c(1990,1995),type="l",lwd=2,ylim = c(0,250),ylab="History forecast",xlab = "Time",yaxt="n",main="Figure 6.8 Housing starts:History, 1990.01-1993.12; and forecast, 1994.01-1994.11")
axis(2,at=c(seq(0,250,50)),labels = c(seq(0,250,50)),las=2)

# Eviews plot
plot(tail(house_starts,48),xlim=c(1990,1995),type="l",lwd=2,ylim = c(0,250),ylab="History forecast",xlab = "Time",yaxt="n",main="Figure 6.8 Housing starts:History, 1990.01-1993.12; and forecast, 1994.01-1994.11")
axis(2,at=c(seq(0,250,50)),labels = c(seq(0,250,50)),las=2)
lines(house.forecast$mean,lwd=2,lty=2,col="black")
lines(house.forecast$upper,lty=2,col="black")
lines(house.forecast$lower,lty=2,col="black")

# Figure 6.9 Housing starts: History, 1990.01-1993.12; and forecast and realization, 1994.01-1994.11
plot(house_starts2,xlim=c(1990,1995),type="l",lwd=2,ylim = c(0,250),ylab="History forecast",xlab = "Time",yaxt="n",main="Figure 6.8 Housing starts:History, 1990.01-1993.12; and forecast, 1994.01-1994.11")
axis(2,at=c(seq(0,250,50)),labels = c(seq(0,250,50)),las=2)
lines(house.forecast$mean,lwd=2,col="blue")
lines(house.forecast$upper,lwd=2,col="gray")
lines(house.forecast$lower,lwd=2,col="gray")



