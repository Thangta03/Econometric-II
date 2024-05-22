	 	rm(list=ls()) 

########################################### Load necessary libraries 

library(forecast) 

library(vars) 

library(urca) 

library(tseries) 

library(aTSA) 

  

############################################ Load data  

data <- read.csv("/home/admin1/Downloads") 

as.Date(data$DATE, format = "%m/%d/%Y") 

# Box-Jenkins Methodology 

ts_data <- ts(data,start = c(2015, 1),frequency=12) 

# Plot the time series 

ts.plot(ts_data, main = "Time Series Plot", ylab = "Price", col = c("white", "red", "green","blue","lightblue"), lty = 5:9) 

legend("topright", legend = colnames(data[,2:5]), col = 2:5, lty = 7:9) 

  

########################################Brent cude oil 

Brent <-data[, c( 'Brent.Cude.oil')] 

ts_Brent<-as.ts(Brent) 

plot(ts_Brent) 

as.numeric(ts_Brent) 

adf.test(ts_Brent) 

#P value>0.05 

#fail to reject H0:there is autocorrelation within the series 

#not stationary 

# Differencing the time series data to make it stationary 

ts_diff_Brent <- diff(ts_Brent, differences = 1) 

plot(ts_diff_Brent) 

as.numeric(ts_diff_Brent) 

adf.test(ts_diff_Brent) 

#P value<0.05 

#reject H0:there is autocorrelation within the series 

#stationary 

#model could be use 

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2)) 

acf(ts_diff_Brent) 

#acf breaks off after 3 lag - MA(3) 

pacf(ts_diff_Brent) 

#acf breaks off after 2 lag - AR(2) 

#all the possible models AR(1),AR(2),MA(1),MA(2),MA(3)ARMA(1,1),ARMA(1,2),ARMA(1,3),ARMA(2,1),ARMA(2,2),ARMA(2,3) 

ar_Brent1<-arima(ts_diff_Brent,order=c(1,0,0)) 

Box.test(ar_Brent1$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals 

#our residuals are white noise 

ar_Brent2<-arima(ts_diff_Brent,order=c(2,0,0)) 

Box.test(ar_Brent2$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals 

#our residuals are white noisen  

ma_Brent1<-arima(ts_diff_Brent,order=c(0,0,1)) 

Box.test(ma_Brent1$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals  

#our residuals are white noise 

ma_Brent2<-arima(ts_diff_Brent,order=c(0,0,2)) 

Box.test(ma_Brent2$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals  

#our residuals are white noise 

ma_Brent3<-arima(ts_diff_Brent,order=c(0,0,3)) 

Box.test(ma_Brent3$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals  

#our residuals are white noise 

arma_Brent11<-arima(ts_diff_Brent,order=c(1,0,1)) 

Box.test(arma_Brent11$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals  

#our residuals are white noise 

arma_Brent12<-arima(ts_diff_Brent,order=c(1,0,2)) 

Box.test(arma_Brent12$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals  

#our residuals are white noise 

arma_Brent13<-arima(ts_diff_Brent,order=c(1,0,3)) 

Box.test(arma_Brent13$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals  

#our residuals are white noise 

arma_Brent21<-arima(ts_diff_Brent,order=c(2,0,1)) 

Box.test(arma_Brent21$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals  

#our residuals are white noise 

arma_Brent22<-arima(ts_diff_Brent,order=c(2,0,2)) 

Box.test(arma_Brent11$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals  

#our residuals are white noise 

arma_Brent23<-arima(ts_diff_Brent,order=c(2,0,3)) 

Box.test(arma_Brent23$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals  

#our residuals are white noise 

###final model only with p value >0.05 

AIC(ar_Brent1,ar_Brent2,ma_Brent1,ma_Brent2,ma_Brent3,arma_Brent11,arma_Brent12,arma_Brent13,arma_Brent21,arma_Brent22,arma_Brent23) 

BIC(ar_Brent1,ar_Brent2,ma_Brent1,ma_Brent2,ma_Brent3,arma_Brent11,arma_Brent12,arma_Brent13,arma_Brent21,arma_Brent22,arma_Brent23) 

#lowest us ma_Bent3 

#ma_Bent3 gives the most precise and reliable forecasts 

hist(ma_Brent3$residuals) 

shapiro.test(resid(ma_Brent3)) 

#low p value reject #H0: normal distribution 

  

#####################################BP stock price 

BP <-data[, c( 'BP.stock.price')] 

ts_BP<-as.ts(BP) 

plot(ts_BP) 

as.numeric(ts_BP) 

adf.test(ts_BP) 

#P value>0.05 

#fail to reject H0:there is autocorrelation within the series 

#not stationary 

#Differencing the time series data to make it stationary 

ts_diff_BP <- diff(ts_BP, differences = 1) 

plot(ts_diff_BP) 

as.numeric(ts_diff_BP) 

adf.test(ts_diff_BP) 

#P value<0.05 

#reject H0:there is autocorrelation within the series 

#stationary 

#model could be use 

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2)) 

acf(ts_diff_BP) 

#acf breaks off after 2 lag - MA(2) 

pacf(ts_diff_BP) 

#acf breaks off after 1 lag - AR(1) 

#all the possible models AR(1),MA(1),MA(2),ARMA(1,1),ARMA(1,2) 

ar_BP1<-arima(ts_diff_BP,order=c(1,0,0)) 

Box.test(ar_BP1$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals 

#our residuals are white noise 

ma_BP1<-arima(ts_diff_BP,order=c(0,0,1)) 

Box.test(ma_BP1$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals  

#our residuals are white noise 

ma_BP2<-arima(ts_diff_BP,order=c(0,0,2)) 

Box.test(ma_BP2$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals  

#our residuals are white noise 

arma_BP11<-arima(ts_diff_BP,order=c(1,0,1)) 

Box.test(arma_BP11$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals  

#our residuals are white noise 

arma_BP12<-arima(ts_diff_BP,order=c(1,0,2)) 

Box.test(arma_BP12$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals  

#our residuals are white noise 

###final model only with p value >0.05 

AIC(ar_BP1,ma_BP1,ma_BP2,arma_BP11,arma_BP12) 

BIC(ar_BP1,ma_BP1,ma_BP2,arma_BP11,arma_BP12) 

#lowest us ma_BP1 

#ma_BP1 gives the most precise and reliable forecasts 

hist(ma_BP1$residuals) 

shapiro.test(resid(ma_BP1)) 

#high p value #H0: normal distribution 

  

######################################Exxon stock price 

Exxon <-data[, c( 'Exxon.Mobil.stock.price')] 

ts_Exxon<-as.ts(Exxon) 

plot(ts_Exxon) 

as.numeric(ts_Exxon) 

adf.test(ts_Exxon) 

#P value>0.05 

#fail to reject H0:there is autocorrelation within the series 

#not stationary 

# Differencing the time series data to make it stationary 

ts_diff_Exxon <- diff(ts_Exxon, differences = 1) 

plot(ts_diff_Exxon) 

as.numeric(ts_diff_Exxon) 

adf.test(ts_diff_Exxon) 

#P value<0.05 

#reject H0:there is autocorrelation within the series 

#stationary 

#model could be use 

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2)) 

acf(ts_diff_Exxon) 

#acf breaks off after 2 lag - MA(2) 

pacf(ts_diff_Exxon) 

#acf breaks off after 1 lag - AR(1) 

#all the possible models AR(1),MA(1),MA(2),ARMA(1,1),ARMA(1,2) 

ar_Exxon1<-arima(ts_diff_Exxon,order=c(1,0,0)) 

Box.test(ar_Exxon1$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals 

#our residuals are white noise 

ma_Exxon1<-arima(ts_diff_Exxon,order=c(0,0,1)) 

Box.test(ma_Exxon1$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals  

#our residuals are white noise 

ma_Exxon2<-arima(ts_diff_Exxon,order=c(0,0,2)) 

Box.test(ma_Exxon2$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals  

#our residuals are white noise 

arma_Exxon11<-arima(ts_diff_Exxon,order=c(1,0,1)) 

Box.test(arma_Exxon11$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals  

#our residuals are white noise 

arma_Exxon12<-arima(ts_diff_Exxon,order=c(1,0,2)) 

Box.test(arma_Exxon12$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals  

#our residuals are white noise 

###final model only with p value >0.05 

AIC(ar_Exxon1,ma_Exxon1,ma_Exxon2,arma_Exxon11,arma_Exxon12) 

BIC(ar_Exxon1,ma_Exxon1,ma_Exxon2,arma_Exxon11,arma_Exxon12) 

#lowest us ma_Exxon1 

#ma_Exxon1 gives the most precise and reliable forecasts 

hist(ma_Exxon1$residuals) 

shapiro.test(resid(ma_Exxon1)) 

#low p value reject #H0: normal distribution 

  

###########################################USD/EURO 

exchange <-data[, c( 'USD.EURO')] 

ts_exchange<-as.ts(exchange) 

plot(ts_exchange) 

as.numeric(ts_exchange) 

adf.test(ts_exchange) 

#P value>0.05 

#fail to reject H0:there is autocorrelation within the series 

#not stationary 

# Differencing the time series data to make it stationary 

ts_diff_exchange <- diff(ts_exchange, differences = 1) 

plot(ts_diff_exchange) 

as.numeric(ts_diff_exchange) 

adf.test(ts_diff_exchange) 

#P value<0.05 

#reject H0:there is autocorrelation within the series 

#stationary 

#model could be use 

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2)) 

acf(ts_diff_exchange) 

#acf breaks off after 3 lag - MA(3) 

pacf(ts_diff_exchange) 

#acf breaks off after 2 lag - AR(2) 

#all the possible models AR(1),AR(2),MA(1),MA(2),MA(3)ARMA(1,1),ARMA(1,2),ARMA(1,3),ARMA(2,1),ARMA(2,2),ARMA(2,3) 

ar_exchange1<-arima(ts_diff_Brent,order=c(1,0,0)) 

Box.test(ar_exchange1$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals 

#our residuals are white noise 

ar_exchange2<-arima(ts_diff_exchange,order=c(2,0,0)) 

Box.test(ar_exchange2$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals 

#our residuals are white noisen  

ma_exchange1<-arima(ts_diff_exchange,order=c(0,0,1)) 

Box.test(ma_exchange1$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals  

#our residuals are white noise 

ma_exchange2<-arima(ts_diff_exchange,order=c(0,0,2)) 

Box.test(ma_exchange2$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals  

#our residuals are white noise 

ma_exchange3<-arima(ts_diff_exchange,order=c(0,0,3)) 

Box.test(ma_exchange3$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals  

#our residuals are white noise 

arma_exchange11<-arima(ts_diff_exchange,order=c(1,0,1)) 

Box.test(arma_exchange11$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals  

#our residuals are white noise 

arma_exchange12<-arima(ts_diff_exchange,order=c(1,0,2)) 

Box.test(arma_exchange12$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals  

#our residuals are white noise 

arma_exchange13<-arima(ts_diff_exchange,order=c(1,0,3)) 

Box.test(arma_exchange13$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals  

#our residuals are white noise 

arma_exchange21<-arima(ts_diff_exchange,order=c(2,0,1)) 

Box.test(arma_exchange21$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals  

#our residuals are white noise 

arma_exchange22<-arima(ts_diff_exchange,order=c(2,0,2)) 

Box.test(arma_exchange11$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals  

#our residuals are white noise 

arma_exchange23<-arima(ts_diff_exchange,order=c(2,0,3)) 

Box.test(arma_exchange23$residuals,lag=10,type="Ljung") 

#p value>0.05 

#H0: there is no autocorrelation within our residuals  

#our residuals are white noise 

###final model only with p value >0.05 

AIC(ar_exchange1,ar_exchange2,ma_exchange1,ma_exchange2,ma_exchange3,arma_exchange11,arma_exchange12,arma_exchange13,arma_exchange21,arma_exchange22,arma_exchange23) 

#lowest is arma_exchange22 

BIC(ar_exchange1,ar_exchange2,ma_exchange1,ma_exchange2,ma_exchange3,arma_exchange11,arma_exchange12,arma_exchange13,arma_exchange21,arma_exchange22,arma_exchange23) 

#lowest is ma_exchange1 

#ma_exchange1 and arma_exchange22 gives the most precise and reliable forecasts 

hist(ma_exchange1$residuals) 

shapiro.test(resid(ma_exchange1)) 

#high p value #H0: normal distribution 

hist(arma_exchange22$residuals) 

shapiro.test(resid(arma_exchange22)) 

#high p value #H0: normal distribution 

  

######################Multi-Time Series 

############### Differencing the time series data to make it stationary 

ts_diff <- diff(ts_data, differences = 1) 

  

# Plot differenced time series data 

ts.plot(ts_diff, main = "Differenced Time Series Plot", ylab = "Differenced", col = c("white", "red", "green","blue","lightblue"), lty = 5:9) 

# Johansen cointegration test 

jo_test <- ca.jo(ts_data, type = "trace", ecdet = "const", spec = "longrun") 

# Display the summary of the Johansen cointegration test 

summary(jo_test) 

# Estimate the VECM model using the Johansen test results 

vec_model <- cajorls(jo_test, r = 4)   

  

# Convert VECM to VAR for IRF and FEVD analysis 

var_model <- vec2var(jo_test, r = 4) 

# Calculate the IRF for the impulse of Brent cude oil  

irf <- irf(var_model, impulse = "Brent.Cude.oil", response = NULL, n.ahead = 10, ortho = TRUE) 

# Plot the IRF 

plot(irf) 

# Calculate the FEVD 

fevd <- fevd(var_model, n.ahead = 10) 

# Plot the FEVD 

pdf("plot.pdf", width = 8, height = 6)  # Adjust width and height as needed 

plot(fevd) 

dev.off() 

 