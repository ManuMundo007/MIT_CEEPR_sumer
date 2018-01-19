install.packages('mutate')
install.packages('robust')
library(rdd)
library(weatherData)
library(lubridate)
library(readxl)
library(tidyverse) # includes dplyr
library(ggplot2)
library(dplyr)
library(robust)
setwd("~/Desktop/UROP 2017/data and R analysis")

chw_stm_kw_CY2014 <- read_excel("chw_stm_kw_CY2014_1.xls")
chw_stm_kw_CY2015_before <- read_excel("chw_stm_kw_CY2015_corrected_dates_before_fault.xls", sheet = "corrected_dates")
chw_stm_kw_CY2015_test <- read_excel("chw_stm_kw_CY2015_corrected_dates  test.xls", sheet = "corrected_dates")
chw_stm_kw_CY2015_fault <- read_excel("chw_stm_kw_CY2015_corrected_dates_fault.xls", sheet = "corrected_dates")
chw_stm_kw_CY2016 <- read_excel("chw_stm_kw_CY2016_corrected_dates.xls", sheet= "corrected_dates")

#Building data frames to organize data 
util.data.e51.2016  = data.frame(chw_stm_kw_CY2016$M18CHWTONS, chw_stm_kw_CY2016$M18STMFLOW, chw_stm_kw_CY2016$M18TFRARealPower, chw_stm_kw_CY2016$M18TFRBRealPower) 
util.data.e51.2016$DATE_TIME = paste(dmy(chw_stm_kw_CY2016$DATE), hms::as.hms(chw_stm_kw_CY2016$TIME))

colnames(util.data.e51.2016) <- c("M18CHWTONS", "M18STMFLOW","M18PowerA" ,"M18PowerB", "DATE_TIME")


util.data.e51.2014 = data.frame(chw_stm_kw_CY2014$M18CHWTONS, chw_stm_kw_CY2014$M18STMFLOW, chw_stm_kw_CY2014$M18TFRARealPower, chw_stm_kw_CY2014$M18TFRBRealPower, #M18
                                chw_stm_kw_CY2014$DATE_TIME) #W20      

colnames(util.data.e51.2014) <- c("M18CHWTONS", "M18STMFLOW","M18PowerA" ,"M18PowerB", "DATE_TIME")


#2015 before fault
util.data.e51.2015.before.fault = data.frame(chw_stm_kw_CY2015_before$M18CHWTONS, as.numeric(chw_stm_kw_CY2015_before$M18STMFLOW),chw_stm_kw_CY2015_before$M18TFRARealPower, chw_stm_kw_CY2015_before$M18TFRBRealPower)
util.data.e51.2015.before.fault$DATE_TIME = paste(dmy(chw_stm_kw_CY2015_before$DATE), hms::as.hms(chw_stm_kw_CY2015_before$TIME))
colnames(util.data.e51.2015.before.fault) <- c("M18CHWTONS", "M18STMFLOW","M18PowerA" ,"M18PowerB", "DATE_TIME")
#2015 after fault test
util.data.e51.2015.test = data.frame(chw_stm_kw_CY2015_test$M18CHWTONS, chw_stm_kw_CY2015_test$M18STMFLOW,chw_stm_kw_CY2015_test$M18TFRARealPower, chw_stm_kw_CY2015_test$M18TFRBRealPower)
util.data.e51.2015.test$DATE_TIME = paste(dmy(chw_stm_kw_CY2015_test$DATE), hms::as.hms(chw_stm_kw_CY2015_test$TIME))
colnames(util.data.e51.2015.test) <- c("M18CHWTONS", "M18STMFLOW","M18PowerA" ,"M18PowerB", "DATE_TIME")
#2015 fault training 
util.data.e51.2015.fault = data.frame(chw_stm_kw_CY2015_fault$M18CHWTONS, chw_stm_kw_CY2015_fault$M18STMFLOW,chw_stm_kw_CY2015_fault$M18TFRARealPower, chw_stm_kw_CY2015_fault$M18TFRBRealPower)
util.data.e51.2015.fault$DATE_TIME = paste(dmy(chw_stm_kw_CY2015_fault$DATE), hms::as.hms(chw_stm_kw_CY2015_fault$TIME))
colnames(util.data.e51.2015.fault) <- c("M18CHWTONS", "M18STMFLOW","M18PowerA" ,"M18PowerB", "DATE_TIME")                                  

#before fault set 
util.data.all.5buildings.before = do.call(rbind, list(util.data.e51.2014, util.data.e51.2015.before.fault))

#test set
util.data.all.5buildings.test = do.call(rbind, list( util.data.e51.2015.test))

#training set
util.data.all.5buildings.fault = do.call(rbind, list(util.data.e51.2015.fault, util.data.e51.2016))


util.data.all.5buildings.before$M18Power = as.numeric(util.data.all.5buildings.before$M18PowerA)+as.numeric(util.data.all.5buildings.before$M18PowerB)

util.data.all.5buildings.test$M18Power = as.numeric(util.data.all.5buildings.test$M18PowerA)+as.numeric(util.data.all.5buildings.test$M18PowerB)

util.data.all.5buildings.fault$M18Power = as.numeric(util.data.all.5buildings.fault$M18PowerA)+as.numeric(util.data.all.5buildings.fault$M18PowerB)

util.data.all.5buildings.before$Interval = as.POSIXct(util.data.all.5buildings.before$DATE_TIME, format ="%Y-%m-%d %H:%M:%S")
util.data.all.5buildings.test$Interval = as.POSIXct(util.data.all.5buildings.test$DATE_TIME, format ="%Y-%m-%d %H:%M:%S")
util.data.all.5buildings.fault$Interval = as.POSIXct(util.data.all.5buildings.fault$DATE_TIME, format ="%Y-%m-%d %H:%M:%S")
#Getting weather data before fault 05-01-2015
weatherbos.before = getWeatherForDate("KBOS", opt_detailed = TRUE, "2014-01-01", end_date="2015-04-30", opt_all_columns = TRUE)
weatherbos.before <- mutate(weatherbos.before, 
                               Year = year(DateUTC),
                               Month = month(DateUTC, label = TRUE),
                               WeekDay = wday(DateUTC, label = TRUE),
                               Hr = hour(DateUTC),
                               Time = minute(DateUTC))
#comment this code after running once so util.weather or weatherbos are not altered. 
weatherbos.before$DateUTC2 <- ymd_hms(weatherbos.before$DateUTC)
# 
# #do not run this line more than once in a given iteration. 
hour(weatherbos.before$DateUTC2) <- hour(weatherbos.before$DateUTC2)-3
# 
minute(weatherbos.before$DateUTC2) <- ifelse(minute(weatherbos.before$DateUTC)<54,55, 0)
# 
util.weather.5buildings.before = merge(util.data.all.5buildings.before, weatherbos.before,by.x ="Interval", by.y = "DateUTC2")

weatherbos.test = getWeatherForDate("KBOS", opt_detailed = TRUE, "2015-05-01", end_date="2015-06-30", opt_all_columns = TRUE)
weatherbos.test <- mutate(weatherbos.test, 
                            Year = year(DateUTC),
                            Month = month(DateUTC, label = TRUE),
                            WeekDay = wday(DateUTC, label = TRUE),
                            Hr = hour(DateUTC),
                            Time = minute(DateUTC))
#comment this code after running once so util.weather or weatherbos are not altered. 
weatherbos.test$DateUTC2 <- ymd_hms(weatherbos.test$DateUTC)
# 
# #do not run this line more than once in a given iteration. 
hour(weatherbos.test$DateUTC2) <- hour(weatherbos.test$DateUTC2)-3
# 
minute(weatherbos.test$DateUTC2) <- ifelse(minute(weatherbos.test$DateUTC)<54,55, 0)
# 
util.weather.5buildings.test = merge(util.data.all.5buildings.test, weatherbos.test,by.x ="Interval", by.y = "DateUTC2")


weatherbos.fault = getWeatherForDate("KBOS", opt_detailed = TRUE, "2015-07-01", end_date="2016-09-30", opt_all_columns = TRUE)
weatherbos.fault <- mutate(weatherbos.fault, 
                          Year = year(DateUTC),
                          Month = month(DateUTC, label = TRUE),
                          WeekDay = wday(DateUTC, label = TRUE),
                          Hr = hour(DateUTC),
                          Time = minute(DateUTC))
#comment this code after running once so util.weather or weatherbos are not altered. 
weatherbos.fault$DateUTC2 <- ymd_hms(weatherbos.fault$DateUTC)
# 
# #do not run this line more than once in a given iteration. 
hour(weatherbos.fault$DateUTC2) <- hour(weatherbos.fault$DateUTC2)-3
# 
minute(weatherbos.fault$DateUTC2) <- ifelse(minute(weatherbos.fault$DateUTC)<54,55, 0)
# 
util.weather.5buildings.fault = merge(util.data.all.5buildings.fault, weatherbos.fault,by.x ="Interval", by.y = "DateUTC2")

model  = lm(M18Power ~ M18CHWTONS + as.numeric(M18STMFLOW) +   Dew_PointF + TemperatureF + Year + Month + WeekDay + Hr, data = util.weather.5buildings.fault)
summary(model)
# model_test = lm(M18Power~ . , data = util.weather.5buildings.fault )
# update(model_tes, .~.-Events)
# .
# .
# .
summary(model_test)
#Estimating the model for Chilled Water 
modelCHW = lm(  M18CHWTONS ~ as.numeric(Humidity) + Dew_PointF + TemperatureF+Year+Month+WeekDay+Hr  , data = util.weather.5buildings.fault)
summary(modelCHW)
#Estimating the model for Steam Flow
modelSTM = lm(  as.numeric(M18STMFLOW) ~ as.numeric(Humidity) + Dew_PointF + TemperatureF+Year+Month+WeekDay+Hr  , data = util.weather.5buildings.fault)
summary(modelSTM)

print(model)
plot(model$residuals)
qqnorm(model$residuals)
qqline(model$residuals)

anova(model)
anova(modelCHW)
anova(modelSTM)

#Creating Prediction
util.weather.5buildings.test$predict = predict(modelSTM,newdata=util.weather.5buildings.test)
util.weather.5buildings.fault$predict = predict(modelSTM,newdata=util.weather.5buildings.fault)
util.weather.5buildings.before$predict = predict(modelSTM,newdata=util.weather.5buildings.before)

#Plots of prediction vs actual data
ggplot(util.weather.5buildings.test, aes(Interval)) + geom_line(aes(y=M18CHWTONS, color = "M18 Chilled Water (tons)")) + geom_line(aes(y=predict, color = "predict")) 

ggplot(util.weather.5buildings.fault, aes(Interval)) + geom_line(aes(y=M18STMFLOW, color = "M18 Steam Flow")) + geom_line(aes(y=predict, color = "predict")) 

ggplot(util.weather.5buildings.before, aes(Interval)) + geom_line(aes(y=M18CHWTONS, color = "M18 Chilled Water (tons)")) + geom_line(aes(y=predict, color = "predict")) 



# ANOVA analysis
#Create data frame with columns: one for Treatment factor and one for date and one for estimated 




util.weather.5buildings.fault$difference = util.weather.5buildings.fault$predict - util.weather.5buildings.fault$M18Electricity 
util.weather.5buildings.fault = na.omit(util.weather.5buildings.fault)
mean(util.weather.5buildings.fault$difference) 
util.weather.5buildings.test = na.omit(util.weather.5buildings.test)
mean(util.weather.5buildings.test$M18Electricity) 

theme_set(theme_bw())
ggplot(util.weather.train, aes(x = util.weather.train$tons, y = util.weather.train$predict)) +
  geom_point(alpha = .05)