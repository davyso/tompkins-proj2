library(forecast)
library(Metrics)
library(MAPA)
library(hts)
library(TStools)
library(devtools)
library(TTR)

#clearing the variable lists
rm(list=ls())

#Phase 1 using out-of-sample 1

#fixed parameters
freq=12
forecast_horizon=12
ma_order=3


#Reading in the input data
df=read.csv(file="~/../Desktop/tompkins-proj2/reconcile_skus.csv",sep=",", header=TRUE,stringsAsFactors=FALSE )
Num_of_ts=ncol(df)
ts_length=36
train_lentgh=24
train=df[1:train_lentgh,]
test=df[(train_lentgh+1):ts_length,]
train_ts<-ts(train,frequency=freq)
test_ts<-ts(test,frequency=freq) 
ma_ts=ts(df)


#Moving Average
ma_total=matrix(NA, nrow = 45, ncol = Num_of_ts)
ma_forecast=matrix(NA, nrow = forecast_horizon, ncol = Num_of_ts)
ma_fit=matrix(NA, nrow = 21, ncol = Num_of_ts)
ma_forecast2=matrix(NA, nrow = forecast_horizon, ncol = Num_of_ts)
ma_fit2=matrix(NA, nrow = 33, ncol = Num_of_ts)

for(i in 1:Num_of_ts){
  ma_total[,i]=SMA(ma_ts[,i],n=ma_order)[ma_order:47]
#  ma_forecast[,i]=ma_total[22:33,i]
#  ma_forecast2[,i]=ma_total[34:45,i]
  ma_forecast[,i]=ma_total[22,i]
  ma_forecast2[,i]=ma_total[34,i]
  ma_fit[,i]=ma_total[1:21,i]
  ma_fit2[,i]=ma_total[1:33,i]
}


#naive forecasting
naive_total=matrix(NA, nrow = 47, ncol = Num_of_ts)
naive_forecast=matrix(NA, nrow = forecast_horizon, ncol = Num_of_ts)
naive_fit=matrix(NA, nrow = 23, ncol = Num_of_ts)
naive_forecast2=matrix(NA, nrow = forecast_horizon, ncol = Num_of_ts)
naive_fit2=matrix(NA, nrow = 35, ncol = Num_of_ts)

for(i in 1:Num_of_ts){
  naive_total[,i]=ma_ts[,i][1:47]
#  naive_forecast[,i]=naive_total[24:35,i]
#  naive_forecast2[,i]=naive_total[36:47,i]
  naive_forecast[,i]=naive_total[24,i]
  naive_forecast2[,i]=naive_total[36,i]
  naive_fit[,i]=naive_total[1:23,i]
  naive_fit2[,i]=naive_total[1:35,i]
}


#Auto_arima forecasting with aic as informaction criteria
arima_aic_forecast=matrix(NA, nrow = forecast_horizon, ncol = Num_of_ts)
arima_aic_fit=matrix(NA, nrow = train_lentgh, ncol = Num_of_ts)
for(i in 1:Num_of_ts){

  for(t in 1:forecast_horizon) {
    train <- df[1:((train_lentgh-1)+t),]
    train_ts <- ts(train, frequency = freq)
    q=forecast(ets(train_ts[,i]), h = 1)
    arima_aic_forecast[t,i]<-q$mean
    # TODO: Store q$fitted
  }
  
}

#Auto_arima forecasting with aic as informaction criteria
arima_bic_forecast=matrix(NA, nrow = forecast_horizon, ncol = Num_of_ts)
arima_bic_fit=matrix(NA, nrow = train_lentgh, ncol = Num_of_ts)
for(i in 1:Num_of_ts){
  
  for(t in 1:forecast_horizon) {
    train <- df[1:((train_lentgh-1)+t),]
    train_ts <- ts(train, frequency = freq)
    q=forecast(auto.arima(train_ts[,i],ic = c("aic")), h = 1)
    arima_bic_forecast[t,i]<-q$mean
    # TODO: Store q$fitted
    
  }
}

#Theta forecasting
theta_forecast=matrix(NA, nrow = forecast_horizon, ncol = Num_of_ts)
theta_fit=matrix(NA, nrow = train_lentgh, ncol = Num_of_ts)
for(i in 1:Num_of_ts){
  
  for(t in 1:forecast_horizon){
    train <- df[1:((train_lentgh-1)+t),]
    train_ts <- ts(train, frequency = freq)
    q=forecast(thetaf(train_ts[,i]), h = 1) # h=1: forecast the next 1 month
    theta_forecast[t,i] <- (q$mean)
    # TODO: Store q$fitted
  }  
}

#out of sample MSEs
mse_aic_test <- array(NA, 65)
mse_bic_test <- array(NA, 65)
mse_theta_test <- array(NA, 65)
mse_ma_test <- array(NA, 65)
mse_naive_test <- array(NA, 65)
for(i in 1:65){
  mse_aic_test[i]=mse(test_ts[,i],arima_aic_forecast[,i])
  mse_bic_test[i]=mse(test_ts[,i],arima_bic_forecast[,i])
  mse_theta_test[i]=mse(test_ts[,i],theta_forecast[,i])
  mse_ma_test[i]=mse(test_ts[,i],ma_forecast[,i])
  mse_naive_test[i]=mse(test_ts[,i],naive_forecast[,i])
}

mse_os1<-data.frame(cbind(mse_aic_test,mse_bic_test,mse_theta_test,mse_ma_test,mse_naive_test))
col_names=c("model1","model2","model3","model4","model5")
colnames(mse_os1) <- col_names
mse_os1$min=apply(mse_os1,1,FUN=min)
mse_os1$win=ifelse(mse_os1$model1==mse_os1$min,1,ifelse(mse_os1$model2==mse_os1$min,2,ifelse(mse_os1$model3==mse_os1$min,3,ifelse(mse_os1$model4==mse_os1$min,4,5))))

#Phase 2 using out-of-sample 2
ts_length=48
train_lentgh=36
train2=df[1:train_lentgh,]
test2=df[(train_lentgh+1):ts_length,]

train2_ts<-ts(train2,frequency=freq)
test2_ts<-ts(test2,frequency=freq) 


#Auto_arima forecasting with aic as informaction criteria
arima_aic_forecast2=matrix(NA, nrow = forecast_horizon, ncol = Num_of_ts)
arima_aic_fit2=matrix(NA, nrow = train_lentgh, ncol = Num_of_ts)
for(i in 1:Num_of_ts){

  for(t in 1:forecast_horizon) {
    train2 <- df[1:((train_lentgh-1)+t),]
    train2_ts <- ts(train2, frequency = freq)
    q=forecast(ets(train2_ts[,i]), h = 1)
    arima_aic_forecast2[t,i]<-q$mean
    # TODO: Store q$fitted
  }
  
}

#Auto_arima forecasting with aic as informaction criteria
arima_bic_forecast2=matrix(NA, nrow = forecast_horizon, ncol = Num_of_ts)
arima_bic_fit2=matrix(NA, nrow = train_lentgh, ncol = Num_of_ts)
for(i in 1:Num_of_ts){

  for(t in 1:forecast_horizon) {
    train2 <- df[1:((train_lentgh-1)+t),]
    train2_ts <- ts(train2, frequency = freq)
    q=forecast(auto.arima(train2_ts[,i],ic = c("bic")), h = 1)
    arima_bic_forecast2[t,i]<-q$mean
    # TODO: Store q$fitted
  }

}

#Theta forecasting
theta_forecast2=matrix(NA, nrow = forecast_horizon, ncol = Num_of_ts)
theta_fit2=matrix(NA, nrow = train_lentgh, ncol = Num_of_ts)
for(i in 1:Num_of_ts){
  
  for(t in 1:forecast_horizon){
    train2 <- df[1:((train_lentgh-1)+t),]
    train2_ts <- ts(train2, frequency = freq)
    q=forecast(thetaf(train2_ts[,i]), h = 1) # h=1: forecast the next 1 month
    theta_forecast2[t,i] <- (q$mean)
    # TODO: Store q$fitted
  }
  
}

#out of sample MSEs
mse_aic_test2 <- array(NA, 65)
mse_bic_test2 <- array(NA, 65)
mse_theta_test2 <- array(NA, 65)
mse_ma_test2 <- array(NA, 65)
mse_naive_test2 <- array(NA, 65)
for(i in 1:65){
  mse_aic_test2[i]=mse(test2_ts[,i],arima_aic_forecast2[,i])
  mse_bic_test2[i]=mse(test2_ts[,i],arima_bic_forecast2[,i])
  mse_theta_test2[i]=mse(test2_ts[,i],theta_forecast2[,i])
  mse_ma_test2[i]=mse(test2_ts[,i],ma_forecast2[,i])
  mse_naive_test2[i]=mse(test2_ts[,i],naive_forecast2[,i])
  
}
mse_os2<-data.frame(cbind(mse_aic_test2,mse_bic_test2,mse_theta_test2,mse_ma_test2,mse_naive_test2))
col_names=c("model1","model2","model3","model4","model5")
colnames(mse_os2) <- col_names
mse_os2$min=apply(mse_os2,1,FUN=min)
mse_os2$win=ifelse(mse_os2$model1==mse_os2$min,1,ifelse(mse_os2$model2==mse_os2$min,2,ifelse(mse_os2$model3==mse_os2$min,3,ifelse(mse_os2$model4==mse_os2$min,4,5))))


#Comparing the results

compare_wins=ifelse(mse_os1$win==mse_os2$win,1,0)
results_trad=sum(compare_wins)





