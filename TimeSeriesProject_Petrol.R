library(tseries)
library(forecast)

# Read Time Series Data
petrol.data.full <- read.csv("US-OIL-IMPORT-DATA.csv")

#ACF Plot full data
acf(petrol.data.full$X1000.Barrels.Day, main=paste(c("ACF Plot", "Full Data Set")))

#Get training data
petrol.data <- petrol.data.full[1:169,]

#Plot Training and Test Data
par(mfrow=c(1,1))
plot(petrol.data$X1000.Barrels.Day, ylab="1000 Barrels per Day",xlab = "Month",main=paste(c("Number of Barrels Imported", "Training Data")), col="red",type = "l")
plot(petrol.data.full$X1000.Barrels.Day, ylab="1000 Barrels/Day", xlab = "Month",main=paste(c("Number of Barrels Imported","Full Data Set")), col="dark green", type = "l")

# Fit polynomial trend to data succesively
    # Linear
    fit_1_trend <- lm(petrol.data$X1000.Barrels.Day ~ poly(petrol.data$Time, 1, raw=TRUE))
    fit_1_trend
    RSS_Trend_1 <- sum(fit_1_trend$residuals^2)
    RSS_Trend_1
    adf.test(fit_1_trend$residuals, alternative = "stationary")
    # Polynomial order 2
    fit_2_trend <- lm(petrol.data$X1000.Barrels.Day ~ poly(petrol.data$Time, 2, raw=TRUE))
    fit_2_trend
    RSS_Trend_2 <- sum(fit_2_trend$residuals^2)
    RSS_Trend_2
    F_stat_1_2 = ((RSS_Trend_1 - RSS_Trend_2)/1)/(RSS_Trend_2/(160-2))
    F_stat_1_2
    adf.test(fit_2_trend$residuals, alternative = "stationary")
    # Polynomial order 3
    fit_3_trend <- lm(petrol.data$X1000.Barrels.Day ~ poly(petrol.data$Time, 3, raw=TRUE))
    fit_3_trend
    RSS_Trend_3 <- sum(fit_3_trend$residuals^2)
    RSS_Trend_3
    F_stat_2_3 = ((RSS_Trend_2 - RSS_Trend_3)/1)/(RSS_Trend_3/(160-3))
    F_stat_2_3
    adf.test(fit_3_trend$residuals, alternative = "stationary")
    # Polynomial order 4
    fit_4_trend <- lm(petrol.data$X1000.Barrels.Day ~ poly(petrol.data$Time, 4, raw=TRUE))
    fit_4_trend
    RSS_Trend_4 <- sum(fit_4_trend$residuals^2)
    RSS_Trend_4
    F_stat_3_4 = ((RSS_Trend_3 - RSS_Trend_4)/1)/(RSS_Trend_4/(160-4))
    F_stat_3_4
    adf.test(fit_4_trend$residuals, alternative = "stationary")
    # Polynomial order 5
    fit_5_trend <- lm(petrol.data$X1000.Barrels.Day ~ poly(petrol.data$Time, 5, raw=TRUE))
    fit_5_trend
    RSS_Trend_5 <- sum(fit_5_trend$residuals^2)
    RSS_Trend_5
    F_stat_4_5 = ((RSS_Trend_4 - RSS_Trend_5)/1)/(RSS_Trend_5/(160-5))
    F_stat_4_5
    adf.test(fit_5_trend$residuals, alternative = "stationary")

# Create Data frame of residuals
Residual.Petrol.Data <- data.frame(fit_1_trend$residuals, fit_2_trend$residuals, fit_3_trend$residuals, fit_4_trend$residuals, fit_5_trend$residuals)

# Write Residuals for F-test Manually from Excel
write.csv(Residual.Petrol.Data,"Petrol-Residuals.csv")

#Fit 4th order Polynomial which was found adequate from F-test
Petrol.Trend.Model <- fit_4_trend

#Summary & Plot Trend Adjusted Residuals
summary(Petrol.Trend.Model)
hist(fit_4_trend$residuals,ylab = "Frequency", xlab = "Residual",main=paste(c("Histogram Of Residuals")))
par(mfrow=c(1,1))
plot(Petrol.Trend.Model$residuals, type = 'l',pch=20, col="red",xlab = "Month",ylab = "Residuals", main=paste("Trend Adjusted Residuals"))
abline(h=0, col="black", lty="dotted")
plot(Petrol.Trend.Model$residuals, pch=20, col="red", xlab = "Month",ylab = "Residuals",main=paste( "Trend Adjusted Residuals Scatter Plot"))

#acf & pacf plot for Trend Residuals
acf(Petrol.Trend.Model$residuals, main=paste(c("ACF Plot", "Residuals")))
pacf(Petrol.Trend.Model$residuals, main=paste(c("PACF Plot", "Residuals")))

#Plot fitted Trend Line
plot(petrol.data$X1000.Barrels.Day,type="l",col=2,ylab = "1000 Barrel/Day",xlab = "Month",main=paste("Fitted Trend Line"))
lines(Petrol.Trend.Model$fitted.values,col="blue")

# Modeling
    # ARMA(2,0,1)
    arma_2_1 = arima(Petrol.Trend.Model$residuals, order=c(2,0,1))
    arma_2_1
    RSS_2_1 = arma_2_1$sigma2*length(arma_2_1$residuals)
    RSS_2_1
    write.csv(arma_2_1$residuals,"ARMA_2_1_Residuals.csv")

    # ARIMA(4,0,3)
    arma_4_3 = arima(Petrol.Trend.Model$residuals, order=c(4,0,3))
    arma_4_3
    RSS_4_3 = arma_4_3$sigma2*length(arma_4_3$residuals)
    RSS_4_3
    write.csv(arma_4_3$residuals,"ARMA_4_3_Residuals.csv")
    F_stat_21_43 = ((RSS_2_1 - RSS_4_3)/4)/(RSS_4_3/(160-8))
    F_stat_21_43
    #1-pf(F_stat,4,length(arma_4_3$residuals)-4*160)
    
    # ARIMA(2,0,0)
    arma_2 = arima(Petrol.Trend.Model$residuals, order=c(2,0,0))
    arma_2
    RSS_2 = arma_2$sigma2*length(arma_2$residuals)
    RSS_2
    write.csv(arma_2$residuals,"ARMA_2Residuals.csv")
    F_stat_21_2 = ((RSS_2 - RSS_2_1)/1)/(RSS_2_1/(160-4))
    F_stat_21_2
    
    # ARIMA(1,0,0)
    arma_1 = arima(Petrol.Trend.Model$residuals, order=c(1,0,0))
    arma_1
    RSS_1 = arma_1$sigma2*length(arma_1$residuals)
    RSS_1
    write.csv(arma_1$residuals,"ARMA_2Residuals.csv")
    F_stat_2_1_1 = ((RSS_1 - RSS_2_1)/2)/(RSS_2_1/(160-4))
    F_stat_2_1_1
    F_stat_2_1 = ((RSS_1 - RSS_2)/1)/(RSS_2/(160-3))
    F_stat_2_1
    
#Acf Arma Residual
acf(arma_2$residuals, main=paste(c("ACF Plot", "AR 2 Residuals")))
pacf(arma_2$residuals, main=paste(c("PACF Plot", "AR 2 Residuals")))

#Joint Optimization
data.jop<-read.csv("JOP_R.csv")
joint_optimized_params = nls(data.jop$Raw.Value ~ b0 - (b1*data.jop$Time) + (b2*(data.jop$Time^2)) - (b3*(data.jop$Time^3)) + (b4*(data.jop$Time^4)) + (phi1*data.jop$Xt.1) + (phi2*data.jop$Xt.2), start = list(b0 = 270,b1 = 1.51, b2 = 0.0314, b3 = 0.000439, b4 = 0.000001965, phi1 = 0.0446, phi2 =0.2057),data = data.jop)
joint_optimized_params
summary(joint_optimized_params)

#Fitted AR 2 Values with trend and residuals
arma_fitted <- read.csv("ArmaFitted.csv")
plot(arma_fitted$Raw.Values,ylab = "1000 barrels per day", xlab = "Time",col = "red",type = "l" ,main=paste(c("AR (2 ) + Trend Line Fitted Values - Ar(2) Residuals")))
lines(arma_fitted$AR.2..Trend.at, col="blue")

plot(arma_fitted$Xt,ylab = "Residuals", xlab = "Time",col = "red",type = "l" )
lines(arma_fitted$Fitted.AR.2....ø1Xt.1...ø2Xt.2, col="blue")

#Plot Forecast and 95% Confidence Interval
Arma_Forecast <- read.csv("Forecast_R.csv")
plot(Arma_Forecast$Actual.Forecast,ylab = "1000 barrels per day", xlab = "Time",col = "red",type = "l" ,main=paste(c("Forecast vs Raw Values")),ylim=c(min(Arma_Forecast$Raw.Values),max(Arma_Forecast$Actual.Forecast)))
lines(Arma_Forecast$Raw.Values, col="blue")
plot(Arma_Forecast$Raw.Values,col="red", pch=19, type='b',main='Raw Values and 95% Interval', ylab='Raw Values and 95% CI', xlab='time',ylim=c(min(Arma_Forecast$Lower.Limit),max(Arma_Forecast$Upper.Limit)))
lines(Arma_Forecast$Lower.Limit, col="black", lty = 3) 
lines(Arma_Forecast$Upper.Limit, col="black",lty= 3)

#Interated model with external regressor not used for project
test_xreg = vector()
# test_xreg_6 = vector()
i = 170
j = 1
for(i in 170:172){
    test_xreg[j] = 270-1.51*i + 0.03144*i^2 - 0.000439*i^3 + 0.000001965*i^4
    
    i = i+1
    j = j+1
}
test_xreg

integrated_model_a = arima(petrol.data$X1000.Barrels.Day, order=c(2,0,0), xreg=fit_4_trend$fitted.values)
integrated_model_A = Arima(petrol.data$X1000.Barrels.Day, order=c(2,0,0), xreg=fit_4_trend$fitted.values)
#integrated_model
integrated_model_A
integrated_model_a


joint_pred = predict(integrated_model_a, n.ahead=4, newxreg=test_xreg)
U = joint_pred$pred + 1.96*joint_pred$se
L = joint_pred$pred - 1.96*joint_pred$se
joint_pred$pred

plot(petrol.data.full$X1000.Barrels.Day,type = "l", col = 2) 
plot(joint_pred$pred, pch=19, type='b',main='ARMA Forecast', ylab='Forecast and 95% CI', xlab='time',ylim=c(min(L),max(U)))
lines(L, col="black", lty = 3) 
lines(U, col="black",lty= 3)
lines(petrol.data.full$X1000.Barrels.Day,col = "blue")

forecast <- forecast.Arima(integrated_model_A, xreg = test_xreg)
plot(forecast,xlab = "1000 barrels per day", ylab = "Time",main=paste(c("Forecast", "Arma 2")))
lines(L, col="black", lty = 3) 
lines(U, col="black",lty= 3)
lines(petrol.data.full$X1000.Barrels.Day, col="black")

forecast

Forecast_R <- data.frame(forecast)

# Write Forecast from R
write.csv(Forecast_R,"Forecast_R.csv")




