# read data
 nyc <- read.csv("nyc.csv")

# view data
 View(nyc)

# View top/bottom rows of data
 head(nyc)
 tail(nyc)

# Pairwise scatter plot
 plot(nyc, main="Pairwise Scatter Plot")
 
 #correlation
 round(cor(nyc),3)

# Building MLRM
 nycmod1 <- lm(Price~Food+Decor+Service+East, data = nyc)

# summary of model
 summary(nycmod1)

# Building new model by dropping service
 nycmod2 <- lm(Price~Food+Decor+East, data=nyc)
 
 summary(nycmod2)

# New model dropping Food
 nycmod3 <- lm(Price~Service+Decor+East, data = nyc)

 summary(nycmod3)

#Residual Analysis
 plot(nycmod2$fitted.values, rstandard(nycmod2),
      main = "Residual Plot",
      xlab = "Predicted Values",
      ylab = "Standardised Residual")
 abline(h=2, lty=2)
 abline(h=-2, lty=2)
 
 #identity Outliers
 plot(nycmod2$fitted.values, rstandard(nycmod2),
      main = "Residual Plot",
      xlab = "Prected Values",
      ylab = "Standardised Residual")
 abline(h=2, lty=2)
 abline(h=-2, lty=2)
 identify(nycmod2$fitted.values, rstandard(nycmod2))
 
 #remove outlier
 nycnew<- nyc[-30,]
nyc1<-nycnew[-48,] 
nyc2<- nyc1[-56,]
nyc3<- nyc2[-103,]
nyc4<- nyc3[-109,]
nyc5<- nyc4[-130,]
nyc6<- nyc5[-141,]

#after removing outliers plot new regression
plot(nyc6$Food, nyc6$Price, main="Price vs food")
abline(nycmod2)