install.packages("Matrix")
install.packages("foreach")
install.packages('glmnet')
library(rdd)
library(weatherData)
library(lubridate)
library(readxl)
library(tidyverse) # includes dplyr
library(ggplot2)
library(dplyr)
library(robust)
library(foreach)
library(Matrix)
library(glmnet)
library(MatrixModels)
# For Demonstration purposes only
# x=matrix(rnorm(100*20),100,20)
# y=rnorm(100)
# g2=sample(1:2,100,replace=TRUE)
# g4=sample(1:4,100,replace=TRUE)
# fit1=glmnet(x,y)
# predict(fit1,newx=x[1:5,],s=c(0.01,0.005))
# predict(fit1,type="coef")
# plot(fit1,xvar="lambda")
# fit2=glmnet(x,g2,family="binomial")
# predict(fit2,type="response",newx=x[2:5,])
# predict(fit2,type="nonzero")
# fit3=glmnet(x,g4,family="multinomial")
# predict(fit3,newx=x[1:3,],type="response",s=0.01)


setwd("~/Dropbox (MIT)/UROP 2017/data and R analysis")
#Dates is a column vector with dates for training, testing and predicting dates
# Training: start_date: Dates[1] end_date=Dates[2] ; 
# Testing: start_date: Dates[3] end_date = Dates[4] ; etc...

Dates <- c("2014-04-01", "2015-01-31", "2015-02-01", "2015-05-31", "2015-06-01", "2016-12-30")
########################################################################################################################
#Importing the data frames 

 ########################################################################################################################
chw_stm_kw_CY2014 <- read_excel("chw_stm_kw_CY2014_1.xls")
chw_stm_kw_CY2015 <- read_excel("chw_stm_kw_CY2015_corrected_dates.xls", sheet = "corrected_dates")
chw_stm_kw_CY2016 <- read_excel("chw_stm_kw_CY2016_corrected_dates.xls", sheet= "corrected_dates")

#Creating data frames for one building per year

util.data.m13.2016  = data.frame(chw_stm_kw_CY2016$M13CHWTONS, chw_stm_kw_CY2016$M13STMFLOW, chw_stm_kw_CY2016$M13TFRARealPower, chw_stm_kw_CY2016$M13TFRBRealPower) 
util.data.m13.2016$DATE_TIME = paste(dmy(chw_stm_kw_CY2016$DATE), hms::as.hms(chw_stm_kw_CY2016$TIME))

colnames(util.data.m13.2016) <- c("M13CHWTONS", "M13STMFLOW","M13PowerA" ,"M13PowerB", "DATE_TIME")
util.data.m13.2016$Interval = as.POSIXct(util.data.m13.2016$DATE_TIME, format ="%Y-%m-%d %H:%M:%S")

util.data.m13.2014 = data.frame(chw_stm_kw_CY2014$M13CHWTONS, chw_stm_kw_CY2014$M13STMFLOW, chw_stm_kw_CY2014$M13TFRARealPower, chw_stm_kw_CY2014$M13TFRBRealPower, 
                                chw_stm_kw_CY2014$DATE_TIME)     

colnames(util.data.m13.2014) <- c("M13CHWTONS", "M13STMFLOW","M13PowerA" ,"M13PowerB", "DATE_TIME")
util.data.m13.2014$Interval = as.POSIXct(util.data.m13.2014$DATE_TIME, format ="%Y-%m-%d %H:%M:%S")

util.data.m13.2015= data.frame(chw_stm_kw_CY2015$M13CHWTONS, chw_stm_kw_CY2015$M13STMFLOW,chw_stm_kw_CY2015$M13TFRARealPower, chw_stm_kw_CY2015$M13TFRBRealPower)
util.data.m13.2015$DATE_TIME = paste(dmy(chw_stm_kw_CY2015$DATE), hms::as.hms(chw_stm_kw_CY2015$TIME))
colnames(util.data.m13.2015) <- c("M13CHWTONS", "M13STMFLOW","M13PowerA" ,"M13PowerB", "DATE_TIME") 
util.data.m13.2015$Interval = as.POSIXct(util.data.m13.2015$DATE_TIME, format ="%Y-%m-%d %H:%M:%S")

#Biding the three years into one data frame and aggregating the power consumption into one column
util.data.all.5buildings = do.call(rbind, list(util.data.m13.2014, util.data.m13.2015, util.data.m13.2016))
util.data.all.5buildings$M13Power = as.numeric(util.data.all.5buildings$M13PowerA)+as.numeric(util.data.all.5buildings$M13PowerB)
#Formating the dates
#util.data.all.5buildings$Interval = as.POSIXct(util.data.all.5buildings$DATE_TIME, format ="%Y-%m-%d %H:%M:%S")
#Weather data
weatherbos.training = getWeatherForDate("KBOS", opt_detailed = TRUE, Dates[1], end_date=Dates[6], opt_all_columns = TRUE)
weatherbos.training <- mutate(weatherbos.training, 
                              Year = year(DateUTC),
                              Month = month(DateUTC, label = TRUE),
                              WeekDay = wday(DateUTC, label = TRUE),
                              Hr = hour(DateUTC),
                              Time = minute(DateUTC))
#comment this code after running once so util.weather or weatherbos are not altered. 
weatherbos.training$DateUTC2 <- ymd_hms(weatherbos.training$DateUTC)
# 
# #do not run this line more than once in a given iteration. 
# hour(weatherbos.training$DateUTC2) <- hour(weatherbos.training$DateUTC2)-3
# # 
# if(minute(weatherbos.training$DateUTC)<)
 minute(weatherbos.training$DateUTC2) <- ifelse(minute(weatherbos.training$DateUTC)<54, 55, 0)
# 
util.weather.5buildings.training = merge(util.data.all.5buildings, weatherbos.training,by.x ="Interval", by.y = "DateUTC2")
util.weather.5buildings.training = util.weather.5buildings.training[!(util.weather.5buildings.training$TemperatureF < -999.0),]

dataset <- util.weather.5buildings.training
#dataset <- dataset[dataset$problematic_obs==0,]
#dataset <- dataset[dataset$qkw_hour>0,]
dataset <- dataset[is.na(dataset$M13CHWTONS)==FALSE,]
dataset <- dataset[is.nan(dataset$M13CHWTONS)==FALSE,]

dataset <- dataset[is.na(dataset$M13STMFLOW)==FALSE,]
dataset <- dataset[is.nan(dataset$M13STMFLOW)==FALSE,]
dataset <- dataset[which(dataset$M13STMFLOW>0),]
dataset <- dataset[which(dataset$M13CHWTONS>0),]
dataset <- dataset[is.na(dataset$M13Power)==FALSE,]
dataset <- dataset[is.nan(dataset$M13Power)==FALSE,]

#plot(dataset$Interval, dataset$M13CHWTONS)
#Creating a training data set 
train <-c(which(dataset$Interval=="2014-04-02 00:00:00" ,arr.ind = F):which(dataset$Interval=="2015-01-31 09:00:00"))
test <- c(which(dataset$Interval=="2015-01-31 10:00:00" ,arr.ind = F):(which(dataset$Interval=="2015-01-31 10:00:00" ,arr.ind = F)+length(train)-1))
#Training data set
X = sparse.model.matrix(M13CHWTONS ~TemperatureF+Year +Interval+factor(WeekDay)+factor(Month)+Hr, data=dataset[train, ])
Y = dataset$M13CHWTONS[train]
X1 = model.Matrix(M13CHWTONS ~TemperatureF+Year +Interval+factor(WeekDay)+Month+Hr, data=dataset[train, ], sparse=TRUE) 
#Test 
Xtest = sparse.model.matrix(M13CHWTONS ~TemperatureF +Interval+factor(WeekDay)+factor(Month)+Hr, data=dataset[test, ])
Ytest = dataset$M13CHWTONS[test]
X1test = model.Matrix(M13CHWTONS ~TemperatureF+Year +Interval+factor(WeekDay)+Month+Hr, data=dataset[test, ], sparse=TRUE) 
#Xall =sparse.model.matrix(M13CHWTONS ~TemperatureF+Year+Sea_Level_PressureIn +Interval+factor(WeekDay)+factor(Month)+Hr, data=dataset[-train,])


# x = data.frame(util.data.all.5buildings[2:4])
# x = as.matrix(x)
# for(i in c(1:length(x))){
#   if(is.na(x[i])&&(!is.na(x[i-1])&!is.na(x[i+1]))){
#     x[i]<- max(x)
#   }
# } 
# 
# y = util.data.all.5buildings$M13CHWTONS
# y[which(is.nan(y))] = NA
# y[which(y==Inf)] = NA
# y = as.vector(y)

fit.lasso=glmnet(X1,Y)
plot(fit.lasso,xvar="lambda",label=TRUE)
cv.lasso=cv.glmnet(X1,Y)
plot(cv.lasso)
coef(cv.lasso)

fit.ridge=glmnet(X1,Y,alpha=0)
plot(fit.ridge,xvar="lambda",label=TRUE)
cv.ridge=cv.glmnet(X1,Y,alpha=0)
plot(cv.ridge)
coef(cv.ridge)

a = 0.00001
fit.elnet=glmnet(X1,Y,alpha=a)
plot(fit.elnet,xvar="lambda",label=TRUE, main = paste("alpha", a, sep=""))
cv.elnet=cv.glmnet(X1,Y,alpha=a)
plot(cv.elnet)
coef(cv.elnet)

normal_regression = lm(M13CHWTONS ~ TemperatureF+Year+Sea_Level_PressureIn +Interval+WeekDay+Month+Hr, data=dataset)
summary(normal_regression)
# # Code from Statistical Learning Theory 
#  lasso.tr=glmnet(X[train,],Y[train])
# # 
#  pred=predict(lasso.tr,X[-train,])
# dim(pred)
#  rmse= sqrt(apply((Y[-train]-pred)^2,2,mean))
#  plot(log(lasso.tr$lambda),rmse,type="b",xlab="Log(lambda)")
#  lam.best=lasso.tr$lambda[order(rmse)[1]]
#  lam.best
#  coef(lasso.tr,s=lam.best)
dataset$prediction_lasso <- dataset$M13CHWTONS
dataset[train, which(colnames(dataset)=='prediction_lasso')] <- c(predict(fit.lasso,X1,type="link", s = min(fit.lasso$lambda)))
dataset[test, which(colnames(dataset)=='prediction_lasso')] <- c(predict(cv.lasso,X1test,type="link", s = min(fit.lasso$lambda)))
#dataset$prediction_lasso <- c(predict(fit.lasso,Xall)) #,type="link", s = min(fit.lasso$lambda))) 
pred = dataset[test, which(colnames(dataset)=='prediction_lasso')]
sq_error = (Ytest-pred)^2
####Predicted Residual Sum of Squares (PRESS) statistic:!!!!!!!!!!!!!
sum(sq_error)
# root_sq_error = sqrt((Y-pred)^2)
# plot(dataset[test, which(colnames(dataset)=='Interval')], sq_error)
# mse = mean((Ytest-pred)^2) 
# mse
# rmse = mean(sqrt((Ytest-pred)^2))
 rmse


dataset$prediction_ridge <- dataset$M13CHWTONS
dataset[train, which(colnames(dataset)=='prediction_ridge')] <- c(predict(fit.ridge,X1,type="link", s = min(fit.ridge$lambda)))
dataset[test, which(colnames(dataset)=='prediction_ridge')] <- c(predict(fit.ridge,X1test,type="link", s = min(fit.ridge$lambda)))
#prediction_ridge <- dataset[train, which(colnames(dataset)=='prediction_ridge')]
prediction_ridge = dataset[test, which(colnames(dataset)=='prediction_ridge')]
sq_error_ridge = (Ytest-prediction_ridge)^2
####Predicted Residual Sum of Squares (PRESS) statistic:!!!!!!!!!!!!!
sum(sq_error_ridge)
# root_sq_error_ridge = sqrt((Y-prediction_ridge)^2)
# plot(dataset[test, which(colnames(dataset)=='Interval')], sq_error_ridge)
# mse_ridge = mean((Y[test]-prediction_ridge)^2) 
# mse_ridge
# rmse_ridge = mean(sqrt((Y[test]-prediction_ridge)^2))
rmse_ridge


dataset$prediction_elnet  <- dataset$M13CHWTONS
dataset[train, which(colnames(dataset)=='prediction_elnet')] <- c(predict(fit.elnet,X1,type="link", s = min(fit.elnet$lambda)))
dataset[test, which(colnames(dataset)=='prediction_elnet')] <- c(predict(fit.elnet,X1test,type="link", s = min(fit.elnet$lambda)))

#prediction_ridge <- dataset[train, which(colnames(dataset)=='prediction_ridge')]
pred = dataset[test, which(colnames(dataset)=='prediction_elnet')]
sq_error_elnet = (Ytest-pred)^2
####Predicted Residual Sum of Squares (PRESS) statistic:!!!!!!!!!!!!!
sum(sq_error_elnet)
root_sq_error_elnet = sqrt((Y-dataset$prediction_elnet)^2)
# plot(dataset$Interval, sq_error_elnet)
# mse_elnet = mean((Y-dataset$prediction_elnet)^2) 
# mse_elnet
# rmse_elnet = mean(sqrt((Y-dataset$prediction_elnet)^2))
# rmse_elnet

v1 <- c("elnet", "ridge", "lasso")
v2 <- c('   ', '   ', '   ')
v3 <- c(sum(sq_error_elnet), sum(sq_error_ridge), sum(sq_error))
v4<- c(mean(sq_error_elnet), mean(sq_error_ridge), mean(sq_error))
PRESS_df <- cbind(v1,v2, v3, v4)
colnames(PRESS_df)<-c("Method","   " , "PRESS", "AVERAGE RESIDUAL SQUARED ERROR")
PRESS_df

dataset$prediction_elnet[-append(train,test)]<- c(integer(length(dataset$M13CHWTONS)-length(train)-length(test))) 
dataset$prediction_lasso[-append(train,test)]<- c(integer(length(dataset$M13CHWTONS)-length(train)-length(test))) 
dataset$prediction_ridge[-append(train,test)]<- c(integer(length(dataset$M13CHWTONS)-length(train)-length(test))) 
#Data Visualization (Plotting)
ggplot(dataset, aes(Interval)) + geom_line(aes(y=M13CHWTONS, color = "M13 Chilled Water (tons)")) + geom_line(aes(y=prediction_elnet, color = "predicted with Elastic Net")) 
ggplot(dataset, aes(Interval)) + geom_line(aes(y=M13CHWTONS, color = "M13 Chilled Water (tons)")) + geom_line(aes(y=prediction_lasso, color = "predicted with Lasso")) 
ggplot(dataset, aes(Interval)) + geom_line(aes(y=M13CHWTONS, color = "M13 Chilled Water (tons)")) + geom_line(aes(y=prediction_ridge, color = "predicted with Ridge")) 
