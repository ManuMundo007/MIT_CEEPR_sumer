install.packages("Matrix")  ; install.packages("foreach") ; 
install.packages('glmnet')  ;  install.packages('readxl')       ; install.packages('ggplot2') ; 
install.packages('weatherData') ; install.packages('lubridate') ; install.packages('MatrixModels') ;
install.packages('tidyverse') ; install.packages('robust')      ; install.packages('rdd') ;
 
library(rdd)      ; library(weatherData)  ;  library(lubridate)  
library(readxl)   ; library(tidyverse)    ;  library(ggplot2)
library(dplyr)    ; library(robust)       ;  library(foreach)  ; 
library(Matrix)   ; library(glmnet)       ;  library(MatrixModels)

setwd("~/Dropbox (MIT)/UROP 2017/data and R analysis")

model <- M18CHWTONS ~outTemp+inTemp+Year +Interval+Hr+WeekDay+Month+Hr
Indep_Var <- "CHWTONS"
Dates <-  c("2014-04-01", "2015-01-31", "2015-02-01", "2015-05-31", "2015-06-01", "2016-12-30")


      #Dahan_Project <-function(Indep_Var = c("CHWTONS", "STMFLOW", "Power"), model , a, Dates)  {
      #parameters of a function:
          #building 
          #Independent Variable: CHWTONS; STMFLOW; Power
          # model: List of Dependent variables as in
  
          #                              M18CHWTONS ~TemperatureF+Year +Interval+factor(WeekDay)+factor(Month)+Hr
  
          #Dates for fault: c(Start1,End1, Start2, End2, Start3, End3)
          #Dates is a column vector with dates for training, testing and predicting dates
          # Training: start_date: Dates[1] end_date=Dates[2] ; 
          # Testing: start_date: Dates[3] end_date = Dates[4] ; etc...
          
                            #Dates <- c("2014-04-01", "2015-01-31", "2015-02-01", "2015-05-31", "2015-06-01", "2016-12-30")
  
          #alpha for glmnet
          # (Names of Files to load maybe?)
          #
        #Output:
          #Sum of Predicted Squared Error
          #Mean Square Error
          #Percentage Error: MeanSquareError / MeanIndependentVariableValue
        

      
      # ########################################################################################################################
      # #Importing the data frames 
      # 
      #########################################################################################################################
      chw_stm_kw_CY2014 <- read_excel("chw_stm_kw_CY2014_1.xls")
      chw_stm_kw_CY2015 <- read_excel("chw_stm_kw_CY2015_corrected_dates.xls", sheet = "corrected_dates")
      chw_stm_kw_CY2016 <- read_excel("chw_stm_kw_CY2016_corrected_dates.xls", sheet= "corrected_dates")

      #Creating data frames for one building per year

      util.data.M18.2016  = data.frame(chw_stm_kw_CY2016$M18CHWTONS, chw_stm_kw_CY2016$M18STMFLOW, chw_stm_kw_CY2016$M18TFRARealPower, chw_stm_kw_CY2016$M18TFRBRealPower)
      util.data.M18.2016$DATE_TIME = paste(dmy(chw_stm_kw_CY2016$DATE), hms::as.hms(chw_stm_kw_CY2016$TIME))

      colnames(util.data.M18.2016) <- c("M18CHWTONS", "M18STMFLOW","M18PowerA" ,"M18PowerB", "DATE_TIME")
      util.data.M18.2016$Interval = as.POSIXct(util.data.M18.2016$DATE_TIME, format ="%Y-%m-%d %H:%M:%S")

      util.data.M18.2014 = data.frame(chw_stm_kw_CY2014$M18CHWTONS, chw_stm_kw_CY2014$M18STMFLOW, chw_stm_kw_CY2014$M18TFRARealPower, chw_stm_kw_CY2014$M18TFRBRealPower,
                                      chw_stm_kw_CY2014$DATE_TIME)

      colnames(util.data.M18.2014) <- c("M18CHWTONS", "M18STMFLOW","M18PowerA" ,"M18PowerB", "DATE_TIME")
      util.data.M18.2014$Interval = as.POSIXct(util.data.M18.2014$DATE_TIME, format ="%Y-%m-%d %H:%M:%S")

      util.data.M18.2015= data.frame(chw_stm_kw_CY2015$M18CHWTONS, chw_stm_kw_CY2015$M18STMFLOW,chw_stm_kw_CY2015$M18TFRARealPower, chw_stm_kw_CY2015$M18TFRBRealPower)
      util.data.M18.2015$DATE_TIME = paste(dmy(chw_stm_kw_CY2015$DATE), hms::as.hms(chw_stm_kw_CY2015$TIME))
      colnames(util.data.M18.2015) <- c("M18CHWTONS", "M18STMFLOW","M18PowerA" ,"M18PowerB", "DATE_TIME")
      util.data.M18.2015$Interval = as.POSIXct(util.data.M18.2015$DATE_TIME, format ="%Y-%m-%d %H:%M:%S")

      #Biding the three years into one data frame and aggregating the power consumption into one column
      util.data.all.5buildings = do.call(rbind, list(util.data.M18.2014, util.data.M18.2015, util.data.M18.2016))
      util.data.all.5buildings$M18Power = as.numeric(util.data.all.5buildings$M18PowerA)+as.numeric(util.data.all.5buildings$M18PowerB)
      #Formating the dates
      #util.data.all.5buildings$Interval = as.POSIXct(util.data.all.5buildings$DATE_TIME, format ="%Y-%m-%d %H:%M:%S")
      
      
      #Weather data
      bldg54roof <- read_csv("~/Dropbox (MIT)/UROP 2017/data and R analysis/bldg54roof.csv")
      
      bldg54roof$dateTime <- as.POSIXct.numeric(as.numeric(bldg54roof$dateTime), origin = "1970-01-01")
      weatherbos.training = bldg54roof
      
      weatherbos.training <- mutate(weatherbos.training,
                                    Year = year(dateTime),
                                    Month = month(dateTime, label = TRUE),
                                    WeekDay = wday(dateTime, label = TRUE),
                                    Hr = hour(dateTime),
                                    Time = minute(dateTime))
      #comment this code after running once so util.weather or weatherbos are not altered.
      weatherbos.training$dateTime2 <- ymd_hms(weatherbos.training$dateTime)
      #
      # #do not run this line more than once in a given iteration.
      # hour(weatherbos.training$DateUTC2) <- hour(weatherbos.training$DateUTC2)-3
      # #
      # if(minute(weatherbos.training$DateUTC)<)
      #minute(weatherbos.training$dateTime2) <- ifelse(minute(weatherbos.training$dateTime2)<54, 55, 0)
      for (i in c(1:length(weatherbos.training$dateTime2))){
        #print(minute(weatherbos.training[[i, "dateTime2"]]))
        if(minute(weatherbos.training[[i, "dateTime2"]])==20){
          #print(minute(weatherbos.training[[i, "dateTime2"]]))
          minute(weatherbos.training[[i, "dateTime2"]])<- 15
        } 
        else if(minute(weatherbos.training[[i, "dateTime2"]])==50){
          minute(weatherbos.training[[i, "dateTime2"]])<- 45
        }
      }
      
      
      #
      util.weather.5buildings.training = merge(util.data.all.5buildings, weatherbos.training,by.x ="Interval", by.y = "dateTime2")
      #util.weather.5buildings.training = util.weather.5buildings.training[!(util.weather.5buildings.training$inTemp < -999.0),]

      dataset <- util.weather.5buildings.training
      #dataset <- dataset[dataset$problematic_obs==0,]
      #dataset <- dataset[dataset$qkw_hour>0,]
      dataset <- dataset[is.na(dataset$M18CHWTONS)==FALSE,]
      dataset <- dataset[is.nan(dataset$M18CHWTONS)==FALSE,]

      dataset <- dataset[is.na(dataset$M18STMFLOW)==FALSE,]
      dataset <- dataset[is.nan(dataset$M18STMFLOW)==FALSE,]
      dataset <- dataset[which(dataset$M18STMFLOW>0),]
      dataset <- dataset[which(dataset$M18CHWTONS>0),]
      dataset <- dataset[is.na(dataset$M18Power)==FALSE,]
      dataset <- dataset[is.nan(dataset$M18Power)==FALSE,]

      #plot(dataset$Interval, dataset$M18CHWTONS)
      #Creating a training data set
      train <-c(which(dataset$dateTime==paste(Dates[1], "00:00:00") ,arr.ind = F):which(dataset$dateTime==paste(Dates[2], "23:00:00")))
      test <- c(which(dataset$dateTime==paste(Dates[3], "00:00:00") ,arr.ind = F):(which(dataset$dateTime==paste(Dates[4], "23:00:00") ,arr.ind = F)+length(train)-1))
      #Training data set
      X = model.matrix(model, data=dataset[train,], drop.unused.levels = FALSE)
      
      Xtest = model.matrix(model,data=dataset[test, ])
      
      if(Indep_Var=="CHWTONS"){Y = dataset$M18CHWTONS[c(1:dim(X)[1])] ; Ytest = dataset$M18CHWTONS[c(1:dim(Xtest)[1])]
      } else if(Indep_Var=="STMFLOW"){Y = dataset$M18STMFLOW[c(1:dim(X)[1])] ; Ytest = dataset$M18STMFLOW[c(1:dim(Xtest)[1])]
      } else{Y = dataset$M18Power[c(1:dim(X)[1])] ; Ytest = dataset$M18Power[c(1:dim(Xtest)[1])]}
      
      #Model Fitting 
      fit.lasso=glmnet(X ,Y)
      # plot(fit.lasso,xvar="lambda",label=TRUE)
       cv.lasso=cv.glmnet(X,Y)
      # plot(cv.lasso)
       coef(cv.lasso)
      # 
       fit.ridge=glmnet(X,Y,alpha=0)
      # plot(fit.ridge,xvar="lambda",label=TRUE)
       cv.ridge=cv.glmnet(X,Y,alpha=0)
      # plot(cv.ridge)
       coef(cv.ridge)
      # 
       a = 0.00001
       fit.elnet=glmnet(X,Y,alpha=a)
      # plot(fit.elnet,xvar="lambda",label=TRUE, main = paste("alpha", a, sep=""))
       cv.elnet=cv.glmnet(X,Y,alpha=a)
      # plot(cv.elnet)
       coef(cv.elnet)
      # 
       normal_regression = lm(model, data=dataset)
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
      dataset$prediction_lasso <- dataset$M18CHWTONS
      dataset[train, which(colnames(dataset)=='prediction_lasso')] <- c(predict(cv.lasso,X, s = "lambda.min"))
      dataset[test, which(colnames(dataset)=='prediction_lasso')] <- c(predict(cv.lasso,Xtest,type="link", s = min(fit.lasso$lambda)))
      #dataset$prediction_lasso <- c(predict(fit.lasso,Xall)) #,type="link", s = min(fit.lasso$lambda)))
      pred = dataset[test, which(colnames(dataset)=='prediction_lasso')]
      sq_error = (Ytest-c(predict(cv.lasso,Xtest, s = "lambda.min")))^2
      ####Predicted Residual Sum of Squares (PRESS) statistic:!!!!!!!!!!!!!
      sum(sq_error)
      # root_sq_error = sqrt((Y-pred)^2)
      # plot(dataset[test, which(colnames(dataset)=='Interval')], sq_error)
      # mse = mean((Ytest-pred)^2)
      # mse
      # rmse = mean(sqrt((Ytest-pred)^2))
      #rmse
      # 
      # 
      dataset$prediction_ridge <- dataset$M18CHWTONS
      dataset[train, which(colnames(dataset)=='prediction_ridge')] <- c(predict(cv.ridge,X, s = "lambda.min"))
      dataset[test, which(colnames(dataset)=='prediction_ridge')] <- c(predict(cv.ridge,Xtest, s = "lambda.min"))
      #prediction_ridge <- dataset[train, which(colnames(dataset)=='prediction_ridge')]
      prediction_ridge = dataset[test, which(colnames(dataset)=='prediction_ridge')]
      sq_error_ridge = (Ytest-c(predict(cv.ridge,Xtest, s = "lambda.min")))^2
      ####Predicted Residual Sum of Squares (PRESS) statistic:!!!!!!!!!!!!!
      sum(sq_error_ridge)
      # root_sq_error_ridge = sqrt((Y-prediction_ridge)^2)
      # plot(dataset[test, which(colnames(dataset)=='Interval')], sq_error_ridge)
      # mse_ridge = mean((Y[test]-prediction_ridge)^2)
      # mse_ridge
      # rmse_ridge = mean(sqrt((Y[test]-prediction_ridge)^2))
      #rmse_ridge
      # 
      # 
       #dataset$prediction_elnet  <- dataset$M18CHWTONS
       dataset[train, which(colnames(dataset)=='prediction_elnet')] <- c(predict(cv.elnet,X, s = "lambda.min"))
       dataset[test, which(colnames(dataset)=='prediction_elnet')] <- c(predict(cv.elnet,Xtest, s = "lambda.min"))
      # 
       #prediction_ridge <- dataset[train, which(colnames(dataset)=='prediction_ridge')]
       pred = dataset[test, which(colnames(dataset)=='prediction_elnet')]
       sq_error_elnet = (Ytest-c(predict(cv.elnet,Xtest, s = "lambda.min")))^2
      # ####Predicted Residual Sum of Squares (PRESS) statistic:!!!!!!!!!!!!!
      # sum(sq_error_elnet)
      # root_sq_error_elnet = sqrt((Y-dataset$prediction_elnet)^2)
      # # plot(dataset$Interval, sq_error_elnet)
      # # mse_elnet = mean((Y-dataset$prediction_elnet)^2) 
      # # mse_elnet
      # # rmse_elnet = mean(sqrt((Y-dataset$prediction_elnet)^2))
      # # rmse_elnet
      # 
       v1 <- c("elnet", "ridge", "lasso")
       v2 <- c('   ', '   ', '   ')
       v3 <- c(sum(sq_error_elnet), sum(sq_error_ridge), sum(sq_error))
       v4<- c(mean(sq_error_elnet), mean(sq_error_ridge), mean(sq_error))
       v5<- c(mean(sqrt(sq_error_elnet)), mean(sqrt(sq_error_ridge)), mean(sqrt(sq_error)))
       v6 <- c(mean(sqrt(sq_error_elnet))/mean(dataset$M18CHWTONS)*100, mean(sqrt(sq_error_ridge))/mean(dataset$M18CHWTONS)*100, mean(sqrt(sq_error))/mean(dataset$M18CHWTONS)*100)
       PRESS_df <- cbind(v1,v2, v3, v4, v5, v6)
       colnames(PRESS_df)<-c("Method","   " , "PRESS", "AVERAGE RESIDUAL SQUARED ERROR", "Mean square root of PRESS", "Percent Error")
      # 
      # 
      # mean(dataset$M18CHWTONS)
      # 
      # dataset$prediction_elnet[-append(train,test)]<- c(integer(length(dataset$M18CHWTONS)-length(train)-length(test))) 
      # dataset$prediction_lasso[-append(train,test)]<- c(integer(length(dataset$M18CHWTONS)-length(train)-length(test))) 
      # dataset$prediction_ridge[-append(train,test)]<- c(integer(length(dataset$M18CHWTONS)-length(train)-length(test))) 
      # #Data Visualization (Plotting)
      # ggplot(dataset, aes(Interval)) + geom_line(aes(y=M18CHWTONS, color = "M18 Chilled Water (tons)")) + geom_line(aes(y=prediction_elnet, color = "predicted with Elastic Net")) 
      # ggplot(dataset, aes(Interval)) + geom_line(aes(y=M18CHWTONS, color = "M18 Chilled Water (tons)")) + geom_line(aes(y=prediction_lasso, color = "predicted with Lasso")) 
      # ggplot(dataset, aes(Interval)) + geom_line(aes(y=M18CHWTONS, color = "M18 Chilled Water (tons)")) + geom_line(aes(y=prediction_ridge, color = "predicted with Ridge")) 
      # 
      # 
      # 
       Vie(PRESS_df)
#}


