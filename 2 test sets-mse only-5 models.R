library(forecast)
library(Metrics)
library(MAPA)
library(hts)
library(TStools)
library(devtools)
library(e1071)

#clearing the variable lists
rm(list=ls())

#fixed parameters
freq=12
forecast_horizon=12
ma_order=3

#Reading in the input data
df=read.csv(file="~/Desktop/Reconciliation/reconcile_skus.csv",sep=",", header=TRUE,stringsAsFactors=FALSE )
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
  #ma_forecast[,i]=ma_total[22:33,i]
  ma_forecast[,i]=ma_total[22,i]
  #ma_forecast2[,i]=ma_total[34:45,i]
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
  q=forecast(ets(train_ts[,i]), h = forecast_horizon)
  arima_aic_forecast[,i]<-q$mean
  arima_aic_fit[,i]<-q$fitted
}

#Auto_arima forecasting with bic as informaction criteria
arima_bic_forecast=matrix(NA, nrow = forecast_horizon, ncol = Num_of_ts)
arima_bic_fit=matrix(NA, nrow = train_lentgh, ncol = Num_of_ts)
for(i in 1:Num_of_ts){
  q=forecast(auto.arima(train_ts[,i],ic = c("aic")), h = forecast_horizon)
  arima_bic_forecast[,i]<-q$mean
  arima_bic_fit[,i]<-q$fitted
}

#Theta forecasting
theta_forecast=matrix(NA, nrow = forecast_horizon, ncol = Num_of_ts)
theta_fit=matrix(NA, nrow = train_lentgh, ncol = Num_of_ts)
for(i in 1:Num_of_ts){
  q=forecast(thetaf(train_ts[,i]), h = forecast_horizon)
  theta_forecast[,i]<-q$mean
  theta_fit[,i]<-q$fitted
}


#train MSEs
mse_aic_train <- array(NA, 65)
mse_bic_train <- array(NA, 65)
mse_theta_train <- array(NA, 65)
mse_ma_train <- array(NA, 65)
mse_naive_train <- array(NA, 65)
for(i in 1:65){
  mse_aic_train[i]=mse(train_ts[,i],arima_aic_fit[,i])
  mse_bic_train[i]=mse(train_ts[,i],arima_bic_fit[,i])
  mse_theta_train[i]=mse(train_ts[,i],theta_fit[,i])
  mse_ma_train[i]=mse(train_ts[4:24,i],ma_fit[,i])
  mse_naive_train[i]=mse(train_ts[2:24,i],naive_fit[,i])
}


#test1 MSEs
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

#data aggregation
mse_train<-data.frame(cbind(mse_aic_train,mse_bic_train,mse_theta_train,mse_ma_train,mse_naive_train))
mse_test<-data.frame(cbind(mse_aic_test,mse_bic_test,mse_theta_test,mse_ma_test,mse_naive_test))

col_names=c("model1","model2","model3","model4","model5")
colnames(mse_train) <- col_names
colnames(mse_test) <- col_names

#data normalizatoin
mse_train$min=apply(mse_train,1,FUN=min)
mse_test$min=apply(mse_test,1,FUN=min)

mse_train$n1=mse_train$model1/mse_train$min
mse_train$n2=mse_train$model2/mse_train$min
mse_train$n3=mse_train$model3/mse_train$min
mse_train$n4=mse_train$model4/mse_train$min
mse_train$n5=mse_train$model5/mse_train$min


#transforming to column vectors
mse_train_col=stack(mse_train,select = c("n1","n2","n3","n4","n5"))

#Finding the winner model in test set
mse_test$win1=ifelse(mse_test$model1==mse_test$min,1,0)
mse_test$win2=ifelse(mse_test$model2==mse_test$min,1,0)
mse_test$win3=ifelse(mse_test$model3==mse_test$min,1,0)
mse_test$win4=ifelse(mse_test$model4==mse_test$min,1,0)
mse_test$win5=ifelse(mse_test$model5==mse_test$min,1,0)


win_col=stack(mse_test, select =c("win1","win2","win3","win4","win5") )


#Phase 2 with test 2 as test set 

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
  q=forecast(ets(train2_ts[,i]), h = forecast_horizon)
  arima_aic_forecast2[,i]<-q$mean
  arima_aic_fit2[,i]<-q$fitted
}

#Auto_arima forecasting with aic as informaction criteria
arima_bic_forecast2=matrix(NA, nrow = forecast_horizon, ncol = Num_of_ts)
arima_bic_fit2=matrix(NA, nrow = train_lentgh, ncol = Num_of_ts)
for(i in 1:Num_of_ts){
  q=forecast(auto.arima(train2_ts[,i],ic = c("bic")), h = forecast_horizon)
  arima_bic_forecast2[,i]<-q$mean
  arima_bic_fit2[,i]<-q$fitted
}

#Theta forecasting
theta_forecast2=matrix(NA, nrow = forecast_horizon, ncol = Num_of_ts)
theta_fit2=matrix(NA, nrow = train_lentgh, ncol = Num_of_ts)
for(i in 1:Num_of_ts){
  q=forecast(thetaf(train2_ts[,i]), h = forecast_horizon)
  theta_forecast2[,i]<-q$mean
  theta_fit2[,i]<-q$fitted
}

#train MSEs
mse_aic_train2 <- array(NA, 65)
mse_bic_train2 <- array(NA, 65)
mse_theta_train2 <- array(NA, 65)
mse_ma_train2<- array(NA, 65)
mse_naive_train2<- array(NA, 65)
for(i in 1:65){
  mse_aic_train2[i]=mse(train2_ts[,i],arima_aic_fit2[,i])
  mse_bic_train2[i]=mse(train2_ts[,i],arima_bic_fit2[,i])
  mse_theta_train2[i]=mse(train2_ts[,i],theta_fit2[,i])
  mse_ma_train2[i]=mse(train2_ts[4:36,i],ma_fit2[,i])
  mse_naive_train2[i]=mse(train2_ts[2:36,i],naive_fit2[,i])
  
}

#test2 MSEs
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

#data aggregation
mse_train2<-data.frame(cbind(mse_aic_train2,mse_bic_train2,mse_theta_train2,mse_ma_train2,mse_naive_train2))
mse_test2<-data.frame(cbind(mse_aic_test2,mse_bic_test2,mse_theta_test2,mse_ma_test2,mse_naive_test2))

col_names=c("model1","model2","model3","model4","model5")
colnames(mse_train2) <- col_names
colnames(mse_test2) <- col_names

#data normalizatoin
mse_train2$min=apply(mse_train2,1,FUN=min)
mse_test2$min=apply(mse_test2,1,FUN=min)

mse_train2$n1=mse_train2$model1/mse_train2$min
mse_train2$n2=mse_train2$model2/mse_train2$min
mse_train2$n3=mse_train2$model3/mse_train2$min
mse_train2$n4=mse_train2$model4/mse_train2$min
mse_train2$n5=mse_train2$model5/mse_train2$min

#transforming to column vectors
mse_train_col2=stack(mse_train2,select = c("n1","n2","n3","n4","n5"))

#Finding the winner model in test set
mse_test2$win1=ifelse(mse_test2$model1==mse_test2$min,1,0)
mse_test2$win2=ifelse(mse_test2$model2==mse_test2$min,1,0)
mse_test2$win3=ifelse(mse_test2$model3==mse_test2$min,1,0)
mse_test2$win4=ifelse(mse_test2$model4==mse_test2$min,1,0)
mse_test2$win5=ifelse(mse_test2$model5==mse_test2$min,1,0)

win_col2=stack(mse_test2, select =c("win1","win2","win3","win4","win5") )

#Logistics regression model
mod_data=cbind(mse_train_col[1],win_col[1])
colnames(mod_data)=c("M","Best")

mod=glm(Best~M,data=mod_data,family=binomial(link='logit'))
summary(mod)

mod_data2=cbind(mse_train_col2[1],win_col2[1])
colnames(mod_data2)=c("M","Best")

prob <- predict(mod,newdata=subset(mod_data2,select=c("M")),type='response')


#Comparing results
temp=data.frame(cbind(prob[1:Num_of_ts],prob[(Num_of_ts+1):(2*Num_of_ts)],prob[(2*Num_of_ts+1):(3*Num_of_ts)],prob[(3*Num_of_ts+1):(4*Num_of_ts)],prob[(4*Num_of_ts+1):(5*Num_of_ts)]))
colnames(temp)<-col_names
temp$max=apply(temp,1,FUN=max)

win_mod=ifelse(temp$model1==temp$max,1,ifelse(temp$model2==temp$max,2,ifelse(temp$model3==temp$max,3,ifelse(temp$model4==temp$max,4,5))))
win_orig=ifelse(mse_test2$model1==mse_test2$min,1,ifelse(mse_test2$model2==mse_test2$min,2,ifelse(mse_test2$model3==mse_test2$min,3,ifelse(mse_test2$model4==mse_test2$min,4,5))))

compare_mod=ifelse(win_mod==win_orig,1,0)
results_mod=sum(compare_mod)


#SVM model
svm.model_mse <- svm(Best~M,data=mod_data, cost = 1000, gamma = 0.1)
svm.pred_mse  <- predict(svm.model_mse, newdata=subset(mod_data2,select=c("M")))

#Comparing results SVM
temp2=data.frame(cbind(svm.pred_mse[1:Num_of_ts],svm.pred_mse[(Num_of_ts+1):(2*Num_of_ts)],svm.pred_mse[(2*Num_of_ts+1):(3*Num_of_ts)],svm.pred_mse[(3*Num_of_ts+1):(4*Num_of_ts)],svm.pred_mse[(4*Num_of_ts+1):(5*Num_of_ts)]))
colnames(temp2)<-col_names
temp2$max=apply(temp2,1,FUN=max)

win_mod_svm=ifelse(temp2$model1==temp2$max,1,ifelse(temp2$model2==temp2$max,2,ifelse(temp2$model3==temp2$max,3,ifelse(temp2$model4==temp2$max,4,5))))

compare_mod_svm=ifelse(win_mod_svm==win_orig,1,0)
results_mod_svm=sum(compare_mod_svm)


#method 2 for classification (one vs rest)
#input1=data.frame(cbind(mse_train$model1,mse_train$model2,mse_train$model3,mse_test$win1))
#input2=data.frame(cbind(mse_train$model1,mse_train$model2,mse_train$model3,mse_test$win2))
#input3=data.frame(cbind(mse_train$model1,mse_train$model2,mse_train$model3,mse_test$win3))

#input12=data.frame(cbind(mse_train2$model1,mse_train2$model2,mse_train2$model3,mse_test2$win1))
#input22=data.frame(cbind(mse_train2$model1,mse_train2$model2,mse_train2$model3,mse_test2$win2))
#input32=data.frame(cbind(mse_train2$model1,mse_train2$model2,mse_train2$model3,mse_test2$win3))

input1=data.frame(cbind(mse_train$n1,mse_train$n2,mse_train$n3,mse_train$n4,mse_train$n5,mse_test$win1))
input2=data.frame(cbind(mse_train$n1,mse_train$n2,mse_train$n3,mse_train$n4,mse_train$n5,mse_test$win2))
input3=data.frame(cbind(mse_train$n1,mse_train$n2,mse_train$n3,mse_train$n4,mse_train$n5,mse_test$win3))
input4=data.frame(cbind(mse_train$n1,mse_train$n2,mse_train$n3,mse_train$n4,mse_train$n5,mse_test$win4))
input5=data.frame(cbind(mse_train$n1,mse_train$n2,mse_train$n3,mse_train$n4,mse_train$n5,mse_test$win5))

input12=data.frame(cbind(mse_train2$n1,mse_train2$n2,mse_train2$n3,mse_train2$n4,mse_train2$n5,mse_test2$win1))
input22=data.frame(cbind(mse_train2$n1,mse_train2$n2,mse_train2$n3,mse_train2$n4,mse_train2$n5,mse_test2$win2))
input32=data.frame(cbind(mse_train2$n1,mse_train2$n2,mse_train2$n3,mse_train2$n4,mse_train2$n5,mse_test2$win3))
input42=data.frame(cbind(mse_train2$n1,mse_train2$n2,mse_train2$n3,mse_train2$n4,mse_train2$n5,mse_test2$win4))
input52=data.frame(cbind(mse_train2$n1,mse_train2$n2,mse_train2$n3,mse_train2$n4,mse_train2$n5,mse_test2$win5))


colnames2<-c("m1","m2","m3","m4","m5","win")
colnames(input1)<-colnames2
colnames(input2)<-colnames2
colnames(input3)<-colnames2
colnames(input4)<-colnames2
colnames(input5)<-colnames2


colnames(input12)<-colnames2
colnames(input22)<-colnames2
colnames(input32)<-colnames2
colnames(input42)<-colnames2
colnames(input52)<-colnames2

#logistic regression
mod1=glm(win~.,data=input1,family=binomial(link='logit'))
mod2=glm(win~.,data=input2,family=binomial(link='logit'))
mod3=glm(win~.,data=input3,family=binomial(link='logit'))
mod4=glm(win~.,data=input4,family=binomial(link='logit'))
mod5=glm(win~.,data=input5,family=binomial(link='logit'))


prob1 <- predict(mod1,newdata=subset(input12,select=c("m1","m2","m3","m4","m5")),type='response')
prob2 <- predict(mod2,newdata=subset(input22,select=c("m1","m2","m3","m4","m5")),type='response')
prob3 <- predict(mod3,newdata=subset(input32,select=c("m1","m2","m3","m4","m5")),type='response')
prob4 <- predict(mod4,newdata=subset(input42,select=c("m1","m2","m3","m4","m5")),type='response')
prob5 <- predict(mod5,newdata=subset(input52,select=c("m1","m2","m3","m4","m5")),type='response')


prob_output=data.frame(cbind(prob1,prob2,prob3,prob4,prob5))
colnames(prob_output)<-col_names
prob_output$max=apply(prob_output,1,FUN=max)

win_mod_LR=ifelse(prob_output$model1==prob_output$max,1,ifelse(prob_output$model2==prob_output$max,2,ifelse(prob_output$model3==prob_output$max,3,ifelse(prob_output$model4==prob_output$max,4,5))))
compare_mod_LR=ifelse(win_mod_LR==win_orig,1,0)
results_mod_LR=sum(compare_mod_LR)


#SVM
svm.model1 <- svm(win~.,data=input1, cost = 10, gamma = 0.1)
svm.model2 <- svm(win~.,data=input2, cost = 10, gamma = 0.1)
svm.model3 <- svm(win~.,data=input3, cost = 10, gamma = 0.1)
svm.model4 <- svm(win~.,data=input4, cost = 10, gamma = 0.1)
svm.model5 <- svm(win~.,data=input5, cost = 10, gamma = 0.1)


svm.pred1  <- predict(svm.model1, newdata=subset(input12,select=c("m1","m2","m3","m4","m5")))
svm.pred2  <- predict(svm.model2, newdata=subset(input22,select=c("m1","m2","m3","m4","m5")))
svm.pred3  <- predict(svm.model3, newdata=subset(input32,select=c("m1","m2","m3","m4","m5")))
svm.pred4  <- predict(svm.model4, newdata=subset(input42,select=c("m1","m2","m3","m4","m5")))
svm.pred5  <- predict(svm.model5, newdata=subset(input52,select=c("m1","m2","m3","m4","m5")))


prob_output_svm=data.frame(cbind(svm.pred1,svm.pred2,svm.pred3,svm.pred4,svm.pred5))
colnames(prob_output_svm)<-col_names
prob_output_svm$max=apply(prob_output_svm,1,FUN=max)

win_mod_svm2=ifelse(prob_output_svm$model1==prob_output_svm$max,1,ifelse(prob_output_svm$model2==prob_output_svm$max,2,ifelse(prob_output_svm$model3==prob_output_svm$max,3,ifelse(prob_output_svm$model4==prob_output_svm$max,4,5))))

compare_mod_svm2=ifelse(win_mod_svm2==win_orig,1,0)
results_mod_svm2=sum(compare_mod_svm2)















