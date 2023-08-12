#Direcionado o R para o Diretorio a ser trabalhado
setwd('C:/Users/User/Desktop')

#Limpa o Ambiente Global
rm(list=ls())

#Inicio do Script
#Analise de Regressão com o R


#Pacotes a serem utilizados

library(highcharter)
library(stargazer)
library(pander)
library(readxl)
library(tidyverse)# ggplot(), %>%, mutate(), and friends
library(broom) #Converte modelos em data frame
library(ggplot2)
library(rddtools)
library(magrittr)
library(rdrobust)  # For robust nonparametric regression discontinuity
library(rddensity)  # For nonparametric regression discontinuity density tests
library(modelsummary)  # Create side-by-side regression tables
library(tidyquant)
library(TSA)
library(fpp2)
library(ggfortify)
library(tseries)
library(TTR)
library(tsutils)
library(kableExtra)
library(urca)
library(foreign)
library(readxl)
library(mFilter)
library(dplyr)
library(xts)
library(ggthemes)
library(FinTS)
library(scales)
library(quantmod)
library(patchwork)#unir graficos ggplot
library(httr)
library(jsonlite)
library(optimr)
library(deSolve)
library(tidyverse)
library(lubridate)
library("growthrates")
library(growthmodels)
library(greybox)##pacote para função MIS
library(nlme)
library(car)
library(ggpubr)
library(tidyquant)
library(dotwhisker)
library(splines2)
library(foreign)
library(vcd)
library(tsModel)
library(Epi)
library(readr)
library(lmtest)
library(outliers)
library(pillar)
library(psych)
library(QuantPsyc)
library(scatterplot3d)
library(rstatix)
library(AER)
library(stargazer)
library(robustbase)
library(gmm)
library(ggpmisc)
library(dlookr)
library(corrplot)
library(gridExtra)
library(zoo)#Para calcular a média móvel
library(magrittr)# pipe operations
library(DT)
library(TTR)
library(dygraphs)
library(BETS)
library(stats)
library(e1071) 
library(astsa)
library(gss)
library(fBasics)
library(fExtremes)
library(fUnitRoots)
library(tsDyn)
library(vars)
library(tidyverse)
library(forecast)
library(tidyquant)
library(timetk)
library(modeltime)
library(tidymodels)
library(workflows)
library(fpp)
library(markovchain)
library(moments)
library(data.table)
library(forecast)
library(tidyverse)
library(readxl)
library(TSstudio)
library(lmtest)
library(Metrics)
library(uroot)
library(urca)
library(aTSA)
library(FinTS)
library(TSA)
library(tseries)
library(gt)
library(rpart)
library(randomForest)
library(yardstick)
library(Metrics)
library(generics)


#Entrando dados
dados <- read_excel("C:/Users/User/Desktop/dados19.xlsx")
attach(dados)
View(dados)

#Plotando a série completa
hchart(dados,type = "line", hcaes(x = dia, y = obitosdia),
       color = "green", name = "dados") %>% 
  hc_title(text = "Obitos por COVID em PE") %>% 
  hc_yAxis(title = list(text = "Obitos SARScoV"))



#Selecionando os casos maiores que 0
obitosdia <- dados %>% filter(obitosdia>=1)

#Setando como série temporal
df_semanal <- ts(df_semanal, start=c(2020,04), frequency = 7) #definindo como serie temporal
dados <- ts(dados, start=c(2020,04), frequency = 365)
#Decomposicao de serie temporal
x.decompClassic<-decompose(df_semanal,type="multiplicative")

plot(x.decompClassic)


y_lab<- "Obitos por COVID em PE"   # input name of data
Actual_date_interval <- c("2020/04/03","2022/10/04")
Forecast_date_interval <- c("2022/10/05","2022/10/11")
validation_data_days <-7
frequency<-"days"

rows <- NROW(df_semanal)
training_data<-df_semanal[1:(rows-validation_data_days)]
testing_data<-df_semanal[(rows-validation_data_days+1):rows]
AD<-fulldate<-seq(as.Date(Actual_date_interval[1]),as.Date(Actual_date_interval[2]), frequency)  #input range for actual date
FD<-seq(as.Date(Forecast_date_interval[1]),as.Date(Forecast_date_interval[2]), frequency)  #input range forecasting date
N_forecasting_days<-nrow(data.frame(FD)) 
validation_dates<-tail(AD,validation_data_days)
validation_data_by_name<-weekdays(validation_dates)
forecasting_data_by_name<-weekdays(FD)

data_series<-ts(training_data)
autoplot(data_series ,xlab=paste ("Time in  ", frequency, sep=" "), ylab = y_lab, main=paste ("Actual Data :", y_lab, sep=" "))

model_bats<-bats(data_series)
accuracy(model_bats)  # accuracy on training data
plot(model_bats,xlab = paste ("Time in  ", frequency ,y_lab , sep=" "),  col.main="black", col.lab="blue", col.sub="black", cex.main=1, cex.lab=1, cex.sub=1,font.main=4, font.lab=4)

# Testing Data Evaluation
forecasting_bats <- predict(model_bats, h=N_forecasting_days+validation_data_days)
validation_forecast<-head(forecasting_bats$mean,validation_data_days)
MAPE_Per_Day<-round(  abs(((testing_data-validation_forecast)/testing_data)*100)  ,3)
paste ("MAPE % For ",validation_data_days,frequency,"by using bats Model for  ==> ",y_lab, sep=" ")

MAPE_Mean_All<-paste(round(mean(MAPE_Per_Day),3),"% MAPE ",validation_data_days,frequency,y_lab,sep=" ")
MAPE_bats<-paste(round(MAPE_Per_Day,3),"%")
MAPE_bats_Model<-paste(MAPE_Per_Day ,"%")
paste (" MAPE that's Error of Forecasting for ",validation_data_days," days in bats Model for  ==> ",y_lab, sep=" ")

paste(MAPE_Mean_All,"%")

paste ("MAPE that's Error of Forecasting day by day for ",validation_data_days," days in bats Model for  ==> ",y_lab, sep=" ")

data.frame(date_bats=validation_dates,validation_data_by_name,actual_data=testing_data,forecasting_bats=validation_forecast,MAPE_bats_Model)

data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_bats=tail(forecasting_bats$mean,N_forecasting_days))

plot(forecasting_bats)
x1_test <- ts(testing_data, start =(rows-validation_data_days+1) )
lines(x1_test, col='red',lwd=2)

graph1<-autoplot(forecasting_bats,xlab = paste ("Time in  ", frequency ,y_lab , sep=" "),  col.main="black", col.lab="blue", col.sub="black", cex.main=1, cex.lab=1, cex.sub=1,font.main=4, font.lab=4, ylab=y_lab)
graph1

## Error of forecasting
Error_bats<-abs(testing_data-validation_forecast)  # Absolute error of forecast (AEOF)
REOF_A_bats<-abs(((testing_data-validation_forecast)/testing_data)*100)  #Relative error of forecast (divided by actual)(REOF_A)
REOF_F_bats<-abs(((testing_data-validation_forecast)/validation_forecast)*100)  #Relative error of forecast (divided by forecast)(REOF_F)
correlation_bats<-cor(testing_data,validation_forecast, method = c("pearson"))     # correlation coefficient between predicted and actual values 
RMSE_bats<-sqrt(sum((Error_bats^2))/validation_data_days)   #  Root mean square forecast error
MAD_bats<-abs((sum(testing_data-validation_forecast))/validation_data_days)   # average forecast accuracy
AEOF_bats<-c(Error_bats)
REOF_Abats<-c(paste(round(REOF_A_bats,3),"%"))
REOF_Fbats<-c(paste(round(REOF_F_bats,3),"%"))
data.frame(correlation_bats,RMSE_bats,MAPE_Mean_All,MAD_bats) # analysis of Error  by using Bats Model shows result of correlation ,MSE ,MPER

data.frame(validation_dates,Validation_day_name=validation_data_by_name,AEOF_bats,REOF_Abats,REOF_Fbats)   # Analysis of error shows result AEOF,REOF_A,REOF_F

## TBATS Model

# Data Modeling
data_series<-ts(training_data)
model_TBATS<-forecast:::fitSpecificTBATS(data_series,use.box.cox=FALSE, use.beta=TRUE,  seasonal.periods=c(6),use.damping=FALSE,k.vector=c(2))
accuracy(model_TBATS)  # accuracy on training data

# Print Model Parameters
model_TBATS
model_bats

plot(model_TBATS,xlab = paste ("Time in  ", frequency ,y_lab , sep=" "),  col.main="black", col.lab="blue", col.sub="black", cex.main=1, cex.lab=1, cex.sub=1,font.main=4, font.lab=4, ylab=y_lab)

# Testing Data Evaluation
forecasting_bats <- predict(model_bats, h=N_forecasting_days+validation_data_days)
validation_forecast<-head(forecasting_bats$mean,validation_data_days)
MAPE_Per_Day<-round(  abs(((testing_data-validation_forecast)/testing_data)*100)  ,3)
paste ("MAPE % For ",validation_data_days,frequency,"by using bats Model for  ==> ",y_lab, sep=" ")

MAPE_Mean_All<-paste(round(mean(MAPE_Per_Day),3),"% MAPE ",validation_data_days,frequency,y_lab,sep=" ")
MAPE_bats<-paste(round(MAPE_Per_Day,3),"%")
MAPE_bats_Model<-paste(MAPE_Per_Day ,"%")
paste (" MAPE that's Error of Forecasting for ",validation_data_days," days in bats Model for  ==> ",y_lab, sep=" ")

MAPE_Mean_All<-paste(round(mean(MAPE_Per_Day),3),"% MAPE ",validation_data_days,frequency,y_lab,sep=" ")
MAPE_bats<-paste(round(MAPE_Per_Day,3),"%")
MAPE_bats_Model<-paste(MAPE_Per_Day ,"%")
paste (" MAPE that's Error of Forecasting for ",validation_data_days," days in bats Model for  ==> ",y_lab, sep=" ")


paste(MAPE_Mean_All,"%")

paste ("MAPE that's Error of Forecasting day by day for ",validation_data_days," days in bats Model for  ==> ",y_lab, sep=" ")

data.frame(date_bats=validation_dates,validation_data_by_name,actual_data=testing_data,forecasting_bats=validation_forecast,MAPE_bats_Model)

data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_bats=tail(forecasting_bats$mean,N_forecasting_days))

plot(forecasting_bats)
x1_test <- ts(testing_data, start =(rows-validation_data_days+1) )
lines(x1_test, col='red',lwd=2)

graph1<-autoplot(forecasting_bats,xlab = paste ("Time in  ", frequency ,y_lab , sep=" "),  col.main="black", col.lab="blue", col.sub="black", cex.main=1, cex.lab=1, cex.sub=1,font.main=4, font.lab=4, ylab=y_lab)
graph1

## Error of forecasting
Error_bats<-abs(testing_data-validation_forecast)  # Absolute error of forecast (AEOF)
REOF_A_bats<-abs(((testing_data-validation_forecast)/testing_data)*100)  #Relative error of forecast (divided by actual)(REOF_A)
REOF_F_bats<-abs(((testing_data-validation_forecast)/validation_forecast)*100)  #Relative error of forecast (divided by forecast)(REOF_F)
correlation_bats<-cor(testing_data,validation_forecast, method = c("pearson"))     # correlation coefficient between predicted and actual values 
RMSE_bats<-sqrt(sum((Error_bats^2))/validation_data_days)   #  Root mean square forecast error
MAD_bats<-abs((sum(testing_data-validation_forecast))/validation_data_days)   # average forecast accuracy
AEOF_bats<-c(Error_bats)
REOF_Abats<-c(paste(round(REOF_A_bats,3),"%"))
REOF_Fbats<-c(paste(round(REOF_F_bats,3),"%"))
data.frame(correlation_bats,RMSE_bats,MAPE_Mean_All,MAD_bats) # analysis of Error  by using Bats Model shows result of correlation ,MSE ,MPER


data.frame(validation_dates,Validation_day_name=validation_data_by_name,AEOF_bats,REOF_Abats,REOF_Fbats)   # Analysis of error shows result AEOF,REOF_A,REOF_F

## TBATS Model

# Data Modeling
data_series<-ts(training_data)
model_TBATS<-forecast:::fitSpecificTBATS(data_series,use.box.cox=FALSE, use.beta=TRUE,  seasonal.periods=c(6),use.damping=FALSE,k.vector=c(2))
accuracy(model_TBATS)  # accuracy on training data

model_TBATS

plot(model_TBATS,xlab = paste ("Time in  ", frequency ,y_lab , sep=" "),  col.main="black", col.lab="blue", col.sub="black", cex.main=1, cex.lab=1, cex.sub=1,font.main=4, font.lab=4, ylab=y_lab)

# Testing Data Evaluation
forecasting_tbats <- predict(model_TBATS, h=N_forecasting_days+validation_data_days)
validation_forecast<-head(forecasting_tbats$mean,validation_data_days)
MAPE_Per_Day<-round(  abs(((testing_data-validation_forecast)/testing_data)*100)  ,3)
paste ("MAPE % For ",validation_data_days,frequency,"by using TBATS Model for  ==> ",y_lab, sep=" ")

MAPE_Mean_All<-paste(round(mean(MAPE_Per_Day),3),"% MAPE ",validation_data_days,frequency,y_lab,sep=" ")
MAPE_TBATS<-paste(round(MAPE_Per_Day,3),"%")
MAPE_TBATS_Model<-paste(MAPE_Per_Day ,"%")
paste (" MAPE that's Error of Forecasting for ",validation_data_days," days in TBATS Model for  ==> ",y_lab, sep=" ")


paste(MAPE_Mean_All,"%")

paste ("MAPE that's Error of Forecasting day by day for ",validation_data_days," days in TBATS Model for  ==> ",y_lab, sep=" ")

data.frame(date_TBATS=validation_dates,validation_data_by_name,actual_data=testing_data,forecasting_TBATS=validation_forecast,MAPE_TBATS_Model)

data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_TBATS=tail(forecasting_tbats$mean,N_forecasting_days))

plot(forecasting_tbats)
x1_test <- ts(testing_data, start =(rows-validation_data_days+1) )
lines(x1_test, col='red',lwd=2)

graph2<-autoplot(forecasting_tbats,xlab = paste ("Time in  ", frequency ,y_lab , sep=" "),  col.main="black", col.lab="blue", col.sub="black", cex.main=1, cex.lab=1, cex.sub=1,font.main=4, font.lab=4, ylab=y_lab)
graph2

## Error of forecasting TBATS Model

Error_tbats<-abs(testing_data-validation_forecast)  # Absolute error of forecast (AEOF)
REOF_A_tbats1<-abs(((testing_data-validation_forecast)/testing_data)*100)  #Relative error of forecast (divided by actual)(REOF_A)
REOF_F_tbats<-abs(((testing_data-validation_forecast)/validation_forecast)*100)  #Relative error of forecast (divided by forecast)(REOF_F)
correlation_tbats<-cor(testing_data,validation_forecast, method = c("pearson"))     # correlation coefficient between predicted and actual values 
RMSE_tbats<-sqrt(sum((Error_tbats^2))/validation_data_days)   #  Root mean square forecast error
MAD_tbats<-abs((sum(testing_data-validation_forecast))/validation_data_days)   # average forecast accuracy
AEOF_tbats<-c(Error_tbats)
REOF_A_tbats<-c(paste(round(REOF_A_tbats1,3),"%"))
REOF_F_tbats<-c(paste(round(REOF_F_tbats,3),"%"))
data.frame(correlation_tbats,RMSE_tbats,MAPE_Mean_All,MAD_tbats) # analysis of Error  by using Holt's linear model shows result of correlation ,MSE ,MPER

data.frame(validation_dates,Validation_day_name=validation_data_by_name,AEOF_tbats,REOF_A_tbats,REOF_F_tbats)   # Analysis of error shows result AEOF,REOF_A,REOF_F

## Holt's linear trend


# Data Modeling
data_series<-ts(training_data)
model_holt<-holt(data_series,h=N_forecasting_days+validation_data_days,lambda = "auto")
accuracy(model_holt)  # accuracy on training data

# Print Model Parameters
summary(model_holt$model)

# Testing Data Evaluation
forecasting_holt <- predict(model_holt, h=N_forecasting_days+validation_data_days,lambda = "auto")
validation_forecast<-head(forecasting_holt$mean,validation_data_days)
MAPE_Per_Day<-round(  abs(((testing_data-validation_forecast)/testing_data)*100)  ,3)
paste ("MAPE % For ",validation_data_days,frequency,"by using holt Model for  ==> ",y_lab, sep=" ")

MAPE_Mean_All<-paste(round(mean(MAPE_Per_Day),3),"% MAPE ",validation_data_days,frequency,y_lab,sep=" ")
MAPE_holt<-paste(round(MAPE_Per_Day,3),"%")
MAPE_holt_Model<-paste(MAPE_Per_Day ,"%")
paste (" MAPE that's Error of Forecasting for ",validation_data_days," days in holt Model for  ==> ",y_lab, sep=" ")

paste(MAPE_Mean_All,"%")


paste ("MAPE that's Error of Forecasting day by day for ",validation_data_days," days in holt Model for  ==> ",y_lab, sep=" ")

data.frame(date_holt=validation_dates,validation_data_by_name,actual_data=testing_data,forecasting_holt=validation_forecast,MAPE_holt_Model)

data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_holt=tail(forecasting_holt$mean,N_forecasting_days))

plot(forecasting_holt)
x1_test <- ts(testing_data, start =(rows-validation_data_days+1) )
lines(x1_test, col='red',lwd=2)


graph3<-autoplot(forecasting_holt,xlab = paste ("Time in  ", frequency ,y_lab , sep=" "),  col.main="black", col.lab="blue", col.sub="black", cex.main=1, cex.lab=1, cex.sub=1,font.main=4, font.lab=4, ylab=y_lab)
graph3

## Error of forecasting by using Holt's linear model
Error_Holt<-abs(testing_data-validation_forecast)  # Absolute error of forecast (AEOF)
REOF_A_Holt1<-abs(((testing_data-validation_forecast)/testing_data)*100)  #Relative error of forecast (divided by actual)(REOF_A)
REOF_F_Holt<-abs(((testing_data-validation_forecast)/validation_forecast)*100)  #Relative error of forecast (divided by forecast)(REOF_F)
correlation_Holt<-cor(testing_data,validation_forecast, method = c("pearson"))     # correlation coefficient between predicted and actual values 
RMSE_Holt<-sqrt(sum((Error_Holt^2))/validation_data_days)   #  Root mean square forecast error
MAD_Holt<-abs((sum(testing_data-validation_forecast))/validation_data_days)   # average forecast accuracy
AEOF_Holt<-c(Error_Holt)
REOF_A_Holt<-c(paste(round(REOF_A_Holt1,3),"%"))
REOF_F_Holt<-c(paste(round(REOF_F_Holt,3),"%"))
REOF_A_Holt11<-mean(abs(((testing_data-validation_forecast)/testing_data)*100))
data.frame(correlation_Holt,RMSE_Holt,MAPE_Mean_All,MAD_Holt) # analysis of Error  by using Holt's linear model shows result of correlation ,MSE ,MPER

data.frame(validation_dates,Validation_day_name=validation_data_by_name,AEOF_Holt,REOF_A_Holt,REOF_F_Holt)   # Analysis of error shows result AEOF,REOF_A,REOF_F

#Auto arima model
##################

require(tseries) # need to install tseries tj test Stationarity in time series 
paste ("tests For Check Stationarity in series  ==> ",y_lab, sep=" ")

kpss.test(data_series) # applay kpss test

pp.test(data_series)   # applay pp test

adf.test(data_series)  # applay adf test

ndiffs(data_series)    # Doing first diffrencing on data

#Taking the first difference
diff1_x1<-diff(data_series)
autoplot(diff1_x1, xlab = paste ("Dias observados", sep=" "),  col.main="black", col.lab="blue", col.sub="black", cex.main=1, cex.lab=1, cex.sub=1,font.main=4, font.lab=4, ylab=y_lab,main = "Série em primeira diferença")

##Testing the stationary of the first differenced series
kpss.test(diff1_x1)   # applay kpss test after taking first differences
pp.test(diff1_x1)     # applay pp test after taking first differences
adf.test(diff1_x1)    # applay adf test after taking first differences

####Fitting an ARIMA Model
#1. Using auto arima function

model1 <- auto.arima(data_series,stepwise=FALSE, approximation=FALSE, trace=T, test = c("kpss", "adf", "pp"))  #applaying auto arima
model1

#Make changes in the source of auto arima to run the best model
arima.string <- function (object, padding = FALSE) 
{
  order <- object$arma[c(1, 6, 2, 3, 7, 4, 5)]
  m <- order[7]
  result <- paste("ARIMA(", order[1], ",", order[2], ",", 
                  order[3], ")", sep = "")
  if (m > 1 && sum(order[4:6]) > 0) {
    result <- paste(result, "(", order[4], ",", order[5], 
                    ",", order[6], ")[", m, "]", sep = "")
  }
  if (padding && m > 1 && sum(order[4:6]) == 0) {
    result <- paste(result, "         ", sep = "")
    if (m <= 9) {
      result <- paste(result, " ", sep = "")
    }
    else if (m <= 99) {
      result <- paste(result, "  ", sep = "")
    }
    else {
      result <- paste(result, "   ", sep = "")
    }
  }
  if (!is.null(object$xreg)) {
    if (NCOL(object$xreg) == 1 && is.element("drift", names(object$coef))) {
      result <- paste(result, "with drift        ")
    }
    else {
      result <- paste("Regression with", result, "errors")
    }
  }
  else {
    if (is.element("constant", names(object$coef)) || is.element("intercept", 
                                                                 names(object$coef))) {
      result <- paste(result, "with non-zero mean")
    }
    else if (order[2] == 0 && order[5] == 0) {
      result <- paste(result, "with zero mean    ")
    }
    else {
      result <- paste(result, "                  ")
    }
  }
  if (!padding) {
    result <- gsub("[ ]*$", "", result)
  }
  return(result)
}






source("stringthearima.R")  
bestmodel <- arima.string(model1, padding = TRUE)
bestmodel <- substring(bestmodel,7,11)
bestmodel <- gsub(" ", "", bestmodel)
bestmodel <- gsub(")", "", bestmodel)
bestmodel <- strsplit(bestmodel, ",")[[1]]
bestmodel <- c(strtoi(bestmodel[1]),strtoi(bestmodel[2]),strtoi(bestmodel[3]))
bestmodel

strtoi(bestmodel[3])

library(forecast)   # install library forecast             
x1_model1= Arima(data_series, order=c(bestmodel)) # Run Best model of auto arima  for forecasting
x1_model1  # Show result of best model of auto arima 

accuracy(x1_model1)  # aacuracy of best model from auto arima
x1_model1$x 

checkresiduals(x1_model1,xlab = paste ("Time in  ", frequency ,y_lab , sep=" "),  col.main="black", col.lab="blue", col.sub="black", cex.main=1, cex.lab=1, cex.sub=1,font.main=4, font.lab=4, ylab=y_lab)  # checkresiduals from best model from using auto arima 

Box.test(x1_model1$residuals^2, lag=20, type="Ljung-Box")   # Do test for resdulas by using Box-Ljung test , Ljung-Box test For Modelling
jarque.bera.test(x1_model1$residuals)  # Do test jarque.bera.test 

#Actual Vs Fitted
plot(data_series, col='red',lwd=2, main="Actual vs Fitted Plot", xlab='Time in (days)', ylab=y_lab) # plot actual and Fitted model 
lines(fitted(x1_model1), col='blue')

#Test data

x1_test <- ts(testing_data, start =(rows-validation_data_days+1) ) # make testing data in time series and start from rows-6
forecasting_auto_arima <- forecast(x1_test, h = N_forecasting_days+validation_data_days)
validation_forecast<-head(forecasting_auto_arima$mean,validation_data_days)
MAPE_Per_Day<-round(abs(((testing_data-validation_forecast)/testing_data)*100)  ,3)
paste ("MAPE % For ",validation_data_days,frequency,"by using bats Model for  ==> ",y_lab, sep=" ")

MAPE_Mean_All<-paste(round(mean(MAPE_Per_Day),3),"% MAPE ",validation_data_days,frequency,y_lab,sep=" ")
MAPE_auto_arima<-paste(round(MAPE_Per_Day,3),"%")
MAPE_auto.arima_Model<-paste(MAPE_Per_Day ,"%")
paste (" MAPE that's Error of Forecasting for ",validation_data_days," days in bats Model for  ==> ",y_lab, sep=" ")


data.frame(date_auto.arima=validation_dates,validation_data_by_name,actual_data=testing_data,forecasting_auto.arima=validation_forecast,MAPE_auto.arima_Model)

data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_auto.arima=tail(forecasting_auto_arima$mean,N_forecasting_days))

plot(forecasting_auto_arima)
x1_test <- ts(testing_data, start =(rows-validation_data_days+1) )
lines(x1_test, col='red',lwd=2)

graph4<-autoplot(forecasting_auto_arima,xlab = paste ("Time in  ", frequency ,y_lab , sep=" "),  col.main="black", col.lab="blue", col.sub="black", cex.main=1, cex.lab=1, cex.sub=1,font.main=4, font.lab=4, ylab=y_lab)
graph4


## Error of forecasting
Error_auto.arima<-abs(testing_data-validation_forecast)  # Absolute error of forecast (AEOF)
REOF_A_auto.arima<-abs(((testing_data-validation_forecast)/testing_data)*100)  #Relative error of forecast (divided by actual)(REOF_A)
REOF_F_auto.arima<-abs(((testing_data-validation_forecast)/validation_forecast)*100)  #Relative error of forecast (divided by forecast)(REOF_F)
correlation_auto.arima<-cor(testing_data,validation_forecast, method = c("pearson"))     # correlation coefficient between predicted and actual values 
RMSE_auto.arima<-sqrt(sum((Error_auto.arima^2))/validation_data_days)   #  Root mean square forecast error
MAD_auto.arima<-abs((sum(testing_data-validation_forecast))/validation_data_days)   # average forecast accuracy
AEOF_auto.arima<-c(Error_auto.arima)
REOF_auto.arima1<-c(paste(round(REOF_A_auto.arima,3),"%"))
REOF_auto.arima2<-c(paste(round(REOF_F_auto.arima,3),"%"))
data.frame(correlation_auto.arima,RMSE_auto.arima,MAPE_Mean_All,MAD_auto.arima) # analysis of Error  by using Holt's linear model shows result of correlation ,MSE ,MPER
data.frame(validation_dates,Validation_day_name=validation_data_by_name,AEOF_auto.arima,REOF_A_auto.arima=REOF_auto.arima1,REOF_F_auto.arima=REOF_auto.arima2)   # Analysis of error shows result AEOF,REOF_A,REOF_F

require(markovchain)
require(data.table)
xx9<-df_semanal[rows]
xx8<-df_semanal[rows-1]
xx7<-df_semanal[rows-2]
xx6<-df_semanal[rows-3]
xx5<-df_semanal[rows-4]
xx4<-df_semanal[rows-5]
xx3<-df_semanal[rows-6]
xx2<-df_semanal[rows-7]
xx1<-df_semanal[rows-8]
infection_vector1<-c(xx1,xx2,xx3)
infection_vector2<-c(xx4,xx5,xx6)
infection_vector3<-c(xx7,xx8,xx9)
sum_vector1<-sum(infection_vector1)
sum_vector2<-sum(infection_vector2)
sum_vector3<-sum(infection_vector3)
proba_vector1<-c(infection_vector1/sum_vector1)
proba_vector2<-c(infection_vector2/sum_vector2)
proba_vector3<-c(infection_vector3/sum_vector3)
CovidStates = c("Low Infections", "Mid Infections", "Hight Infections")
byRow = TRUE


CovidMatrix = matrix(data = c(proba_vector1,
                              proba_vector2,
                              proba_vector3), byrow = byRow, nrow = 3,
                     
                     dimnames = list(CovidStates, CovidStates))


mcCovid = new("markovchain", states = CovidStates, byrow = byRow,
              transitionMatrix = CovidMatrix, name = "Cvid 19")

mcCovid = new("markovchain", states = c("Baixa Infecção", "Média Infecção", "Alta infeção"),
              transitionMatrix = matrix(data = c(proba_vector1,
                                                 proba_vector2,
                                                 proba_vector3), byrow = byRow, nrow = 3),
              name = "Cvid 19")

name = ("Covid 19")
initialState = c(0,1,0)
after2Days = initialState * (mcCovid * mcCovid)
after7Days = initialState * (mcCovid^7)
after30days =initialState * (mcCovid^30)
after7Days


plot(mcCovid,xlab = paste ("Dias observados de" ,y_lab , sep=" "),  col.main="black", col.lab="blue", col.sub="black", cex.main=1, cex.lab=1, cex.sub=1,font.main=4, font.lab=4, ylab=y_lab,main = "Cadeia de Markov")


par(mfrow=c(2,1))
acf(diff1_x1)
pacf(diff1_x1)

previsoes<-predict(x1_model1, n.ahead=7)$pred
previsoes

previsoes1 <- forecast(x1_model1,h=7)# prevê valores futuros, baseados no modelo estimado
prev1 <- forecast(model1, h=7)
prev1
plot(prev1)
coxstuart()

kruskal_effsize(obitosdia ~ dia, dados)
  kruskal_effsize(obitosdia ~ dia)
  
obitos.per= spec.pgram(obitosdia, taper=0, log="no",main="Periodograma",
                           ylab="espectro")

obitos.per$spec
max(obitos.per$spec)
obitos.per$freq
a<-1/(obitos.per$freq[44])
365*a



auto.arima(obitosdia)
auto.arima()