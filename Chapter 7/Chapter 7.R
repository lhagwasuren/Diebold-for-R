rm(list = ls())

# 1.Data ------------------------------------------------------------------
library(hexView)
library(tidyverse)
data<-readEViews("/Users/Mongol/Documents/Eviews/ElementsOfForecasting/fcst_07/fcst7input.wf1")
data1<-filter(data,between(Date,as.Date("1962-01-01"),as.Date("1993-04-01")))
caemp<-ts(data1$CAEMP,start = c(1962,1),end = c(1993,4),frequency = 4)

# Figure 7.9 Canadian employment index
plot(caemp,ylim=c(80,115),type="l",lwd=2,yaxt="n",ylab="Canadian Employment",main="Figure 7.9 Canadian Employment Index",bty="l")
axis(2,at=c(seq(80,115,5)),labels = c(seq(80,115,5)),las=2)

# 2. Correlogram ---------------------------------------------------------
# Table 7.1 Canadian employment index, Correlogram
correlogram <- function(x, order)
{
  if (order >= (length(x)-1))
  {
    print("Vector length falls short of autocorrelations to compute.")
  }
  else
  {
    Total <- length(x)
    factor <- Total*(Total+2)
    xacf <- acf(x, lag=order, plot=FALSE)
    xpacf<-pacf(x,lag=order,plot=FALSE)
    denom <- rep(Total, order)-seq(1, order)
    Lag <- seq(1, order)
    tausq <- xacf$acf[2:length(xacf$acf)]^2/denom
    Q <- 0[1:order]
    temp <- 0[1:order]
    for(i in 1:order)
    {
      temp[i] <- sum(tausq[1:i])
    }
    Qstat <- factor*temp
    pval <- 1 - pchisq(Qstat,i)
    Correlogram <- data.frame("Acorr,"=xacf$acf[2:(order+1)],"P.Acorr."=as.vector(xpacf$acf),"Std.Error"=1/sqrt(length(x)), "Ljung-Box"=Qstat, "P-value"=pval)
    round(Correlogram,digits = 3)
  }
}
correlogram(caemp,order = 12)

# 3.ACF and PACF ----------------------------------------------------------

# Figure 7.10 Canadian employment index: Sample acf and pacf, with plus or minus two standart-error bands
par(mfrow=c(2,1))
acf(caemp,xaxt="n",yaxt="n",lag.max = 12,main="Sample acf",xlim=c(.2,3))
axis(1,at=c(seq(0.5,3,.5)),labels = c(seq(2,12,2)))
axis(2,at=c(seq(-.2,1.0,.2)),labels = c(seq(-.2,1.0,.2)),las=2)
pacf(CAEMP,yaxt="n",lag.max=12,main="Sample pacf",ylim=c(-.4,1))
axis(2,at=c(seq(-.2,1.0,.2)),labels = c(seq(-.2,1.0,.2)),las=2)





