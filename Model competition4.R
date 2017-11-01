

library(readxl) 
library(tseries) 
library(forecast) 
library(urca)  
library(readxl)
library(TSA)  
library(lmtest) 
library(ggplot2) 
library(seasonal)

# DATA
# DATA 
rm(list=ls())
setwd("E:/E/MINCIT/TRABAJO/Pronostico_turismo") 

data<-read_excel("Llegadasrats2.xls")  
countries<-read_excel("paises.xlsx")  
series<-cbind(data[2],countries[3:7])  
colnames(series)[c(2,3,5)]<-c("EU","Mercosur","UE")
data<-ts(series, start = c(2010,1), frequency =12) 

datav<-apply(data,2,function (x,periodsPerYear=12){
  if(NROW(x)<=periodsPerYear){
    stop("too few rows")
  }
  else{
    indexes<-1:(NROW(x)-periodsPerYear)
    return(c(100*(x[indexes+periodsPerYear]/x[indexes]-1)))
  }
}) 
datav<-ts(datav, start = c(2011,1), frequency =12)


adjusted<-diff(log(data),12)
rm(countries)
rm(series)


setwd("E:/E/MINCIT/TRABAJO/Pronostico_turismo/Results")
######################### 

for( i in 1:6){
  index<-i
  
  series<-ts(na.omit(adjusted[,index]), start = c(2011,1), frequency = 12)  
  testing<- window(series, start=c(2016, 8))
  training <- window(series, end=c(2016, 7))
  set.seed(123) 
  
  models<-list( 
    ARIMA=  auto.arima(training)  ,
    ETS = ets(training, ic = "bic"),
    "Red Neuronal" = nnetar(training,  repeats = 20)
  ) 
  
  ###############################
  ###### MODEL COMPETITION ###### 
  ############################### 
  
  ############ h = 12 ########### 
  horizon = 12 
  training<-window(series, end=c(2016, 7)) 
  
  models <- list(
    ARIMA= Arima(training,model = models$ARIMA),
    ETS = ets(training,model = models$ETS, use.initial.values=TRUE),
    "Red Neuronal" = nnetar(training,model = models$`Red Neuronal`)
  )
  forecasts_h12 <- lapply(models, forecast, horizon)
  forecasts_h12$'Caminata Alt.' <- rwf(training, horizon, drift = T)
  
  ############ h = 11 ########### 
  horizon = 11
  training<-window(series, end=c(2016, 8)) 
  
  models <- list(
    ARIMA= Arima(training,model = models$ARIMA),
    ETS = ets(training,model = models$ETS,  use.initial.values=TRUE),
    "Red Neuronal" = nnetar(training,model = models$`Red Neuronal`)
  )
  forecasts_h11 <- lapply(models, forecast, horizon)
  forecasts_h11$'Caminata Alt.' <- rwf(training, horizon, drift = T)
  ############ h = 10 ########### 
  horizon = 10
  training<-window(series, end=c(2016, 9)) 
  
  models <- list(
    ARIMA= Arima(training,model = models$ARIMA),
    ETS = ets(training,model = models$ETS,  use.initial.values=TRUE),
    "Red Neuronal" = nnetar(training,model = models$`Red Neuronal`)
  )
  forecasts_h10 <- lapply(models, forecast, horizon)
  forecasts_h10$'Caminata Alt.' <- rwf(training, horizon, drift = T)
  ############ h = 9 ########### 
  horizon = 9
  training<-window(series, end=c(2016, 10)) 
  
  models <- list(
    ARIMA= Arima(training,model = models$ARIMA),
    ETS = ets(training,model = models$ETS,  use.initial.values=TRUE),
    "Red Neuronal" = nnetar(training,model = models$`Red Neuronal`)
  )
  forecasts_h9 <- lapply(models, forecast, horizon)
  forecasts_h9$'Caminata Alt.' <- rwf(training, horizon, drift = T)
  ############ h = 8 ########### 
  horizon = 8
  training<-window(series, end=c(2016, 11)) 
  
  models <- list(
    ARIMA= Arima(training,model = models$ARIMA),
    ETS = ets(training,model = models$ETS,  use.initial.values=TRUE),
    "Red Neuronal" = nnetar(training,model = models$`Red Neuronal`)
  )
  forecasts_h8 <- lapply(models, forecast, horizon)
  forecasts_h8$'Caminata Alt.' <- rwf(training, horizon, drift = T)
  ############ h = 7 ########### 
  horizon = 7
  training<-window(series, end=c(2016, 12)) 
  
  models <- list(
    ARIMA= Arima(training,model = models$ARIMA),
    ETS = ets(training,model = models$ETS,  use.initial.values=TRUE),
    "Red Neuronal" = nnetar(training,model = models$`Red Neuronal`)
  )
  forecasts_h7 <- lapply(models, forecast, horizon)
  forecasts_h7$'Caminata Alt.' <- rwf(training, horizon, drift = T)
  ############ h = 6 ########### 
  horizon = 6
  training<-window(series, end=c(2017, 1)) 
  
  models <- list(
    ARIMA= Arima(training,model = models$ARIMA),
    ETS = ets(training,model = models$ETS,  use.initial.values=TRUE),
    "Red Neuronal" = nnetar(training,model = models$`Red Neuronal`)
  )
  forecasts_h6 <- lapply(models, forecast, horizon)
  forecasts_h6$'Caminata Alt.' <- rwf(training, horizon, drift = T)
  ############ h = 5 ########### 
  horizon = 5
  training<-window(series, end=c(2017, 2)) 
  models <- list(
    ARIMA= Arima(training,model = models$ARIMA),
    ETS = ets(training,model = models$ETS,  use.initial.values=TRUE),
    "Red Neuronal" = nnetar(training,model = models$`Red Neuronal`)
  )
  forecasts_h5 <- lapply(models, forecast, horizon)
  forecasts_h5$'Caminata Alt.' <- rwf(training, horizon, drift = T)
  ############ h = 4 ########### 
  horizon = 4
  training<-window(series, end=c(2017, 3)) 
  
  models <- list(
    ARIMA= Arima(training,model = models$ARIMA),
    ETS = ets(training,model = models$ETS,  use.initial.values=TRUE),
    "Red Neuronal" = nnetar(training,model = models$`Red Neuronal`)
  )
  forecasts_h4 <- lapply(models, forecast, horizon)
  forecasts_h4$'Caminata Alt.' <- rwf(training, horizon, drift = T)
  ############ h = 3 ########### 
  horizon = 3
  training<-window(series, end=c(2017, 4)) 
  
  models <- list(
    ARIMA= Arima(training,model = models$ARIMA),
    ETS = ets(training,model = models$ETS,  use.initial.values=TRUE),
    "Red Neuronal" = nnetar(training,model = models$`Red Neuronal`)
  )
  forecasts_h3 <- lapply(models, forecast, horizon)
  forecasts_h3$'Caminata Alt.' <- rwf(training, horizon, drift = T)
  ############ h = 2 ########### 
  horizon = 2
  training<-window(series, end=c(2017, 5)) 
  
  models <- list(
    ARIMA= Arima(training,model = models$ARIMA),
    ETS = ets(training,model = models$ETS,  use.initial.values=TRUE),
    "Red Neuronal" = nnetar(training,model = models$`Red Neuronal`)
  )
  forecasts_h2 <- lapply(models, forecast, horizon)
  forecasts_h2$'Caminata Alt.' <- rwf(training, horizon, drift = T)
  ############ h = 1 ########### 
  horizon = 1
  training<-window(series, end=c(2017, 6)) 
  
  models <- list(
    ARIMA= Arima(training,model = models$ARIMA),
    ETS = ets(training,model = models$ETS,  use.initial.values=TRUE),
    "Red Neuronal" = nnetar(training,model = models$`Red Neuronal`)
  )
  forecasts_h1 <- lapply(models, forecast, horizon) 
  forecasts_h1$'Caminata Alt.' <- rwf(training, horizon, drift = T)
  ###############################
  #Forecast by horizon
  f_h12<-sapply(forecasts_h12, "[[","mean") 
  f_h11<-sapply(forecasts_h11, "[[","mean")
  f_h10<-sapply(forecasts_h10, "[[","mean") 
  f_h9<-sapply(forecasts_h9, "[[","mean")
  f_h8<-sapply(forecasts_h8, "[[","mean")
  f_h7<-sapply(forecasts_h7, "[[","mean")
  f_h6<-sapply(forecasts_h6, "[[","mean")
  f_h5<-sapply(forecasts_h5, "[[","mean")
  f_h4<-sapply(forecasts_h4, "[[","mean")
  f_h3<-sapply(forecasts_h3, "[[","mean")
  f_h2<-sapply(forecasts_h2, "[[","mean")
  f_h1<-sapply(forecasts_h1, "[[","mean") 
  
  #COMBINATION  
  f_h12<-as.data.frame(f_h12)
  f_h12$Combinacion=(1/3)*f_h12[,1]+(1/3)*f_h12[,2]+(1/3)*f_h12[,3]
  f_h11<-as.data.frame(f_h11)
  f_h11$Combinacion=(1/3)*f_h11[,1]+(1/3)*f_h11[,2]+(1/3)*f_h11[,3]
  f_h10<-as.data.frame(f_h10)
  f_h10$Combinacion=(1/3)*f_h10[,1]+(1/3)*f_h10[,2]+(1/3)*f_h10[,3]
  f_h9<-as.data.frame(f_h9)
  f_h9$Combinacion=(1/3)*f_h9[,1]+(1/3)*f_h9[,2]+(1/3)*f_h9[,3]
  f_h8<-as.data.frame(f_h8)
  f_h8$Combinacion=(1/3)*f_h8[,1]+(1/3)*f_h8[,2]+(1/3)*f_h8[,3]
  f_h7<-as.data.frame(f_h7)
  f_h7$Combinacion=(1/3)*f_h7[,1]+(1/3)*f_h7[,2]+(1/3)*f_h7[,3]
  f_h6<-as.data.frame(f_h6)
  f_h6$Combinacion=(1/3)*f_h6[,1]+(1/3)*f_h6[,2]+(1/3)*f_h6[,3]
  f_h5<-as.data.frame(f_h5)
  f_h5$Combinacion=(1/3)*f_h5[,1]+(1/3)*f_h5[,2]+(1/3)*f_h5[,3]
  f_h4<-as.data.frame(f_h4)
  f_h4$Combinacion=(1/3)*f_h4[,1]+(1/3)*f_h4[,2]+(1/3)*f_h4[,3]
  f_h3<-as.data.frame(f_h3)
  f_h3$Combinacion=(1/3)*f_h3[,1]+(1/3)*f_h3[,2]+(1/3)*f_h3[,3]
  f_h2<-as.data.frame(f_h2)
  f_h2$Combinacion=(1/3)*f_h2[,1]+(1/3)*f_h2[,2]+(1/3)*f_h2[,3]
  
  f_h1[5]=(1/3)*f_h1[1]+(1/3)*f_h1[2]+(1/3)*f_h1[3]
  names(f_h1)[5]<-"Combinacion" 
  
  modelsnames<-c(names(forecasts_h1),"Combinacion")
  
  # ONE MONTH
  month<-1
  one_month<-rbind(f_h12[month,],f_h11[month,],f_h10[month,],
                   f_h9[month,],f_h8[month,],f_h7[month,],
                   f_h6[month,],f_h5[month,],f_h4[month,],
                   f_h3[month,],f_h2[month,],f_h1)  
  testing<-window(series, start=c(2016, 8)) 
  acc_h1<-list()
  for(i in 1:ncol(one_month)){
    acc_h1[[i]]<-accuracy(as.vector(one_month[,i]), testing)
  }
  acc_h1 <- Reduce(rbind, acc_h1)
  row.names(acc_h1) <- modelsnames
  
  #  Two Months
  month<-2
  two_month<-rbind(f_h12[month,],f_h11[month,],f_h10[month,],
                   f_h9[month,],f_h8[month,],f_h7[month,],
                   f_h6[month,],f_h5[month,],f_h4[month,],
                   f_h3[month,],f_h2[month,]) 
  testing<-window(series, start=c(2016, 9)) 
  acc_h2<-list()
  for(i in 1:ncol(two_month)){
    acc_h2[[i]]<-accuracy(as.vector(two_month[,i]), testing)
  }
  acc_h2 <- Reduce(rbind, acc_h2)
  row.names(acc_h2) <- modelsnames
  
  # Three months
  month<-3
  three_month<-rbind(f_h12[month,],f_h11[month,],f_h10[month,],
                     f_h9[month,],f_h8[month,],f_h7[month,],
                     f_h6[month,],f_h5[month,],f_h4[month,],
                     f_h3[month,]) 
  testing<-window(series, start=c(2016, 10)) 
  acc_h3<-list()
  for(i in 1:ncol(three_month)){
    acc_h3[[i]]<-accuracy(as.vector(three_month[,i]), testing)
  }
  acc_h3 <- Reduce(rbind, acc_h3)
  row.names(acc_h3) <- modelsnames
  
  
  #Six months
  month<-6
  six_month<-rbind(f_h12[month,],f_h11[month,],f_h10[month,],
                   f_h9[month,],f_h8[month,],f_h7[month,],
                   f_h6[month,])  
  testing<-window(series, start=c(2017, 1))
  acc_h6<-list()
  for(i in 1:ncol(six_month)){
    acc_h6[[i]]<-accuracy(as.vector(six_month[,i]), testing)
  }
  acc_h6 <- Reduce(rbind, acc_h6)
  row.names(acc_h6) <- modelsnames
  
  #Twelve months 
  month<-12
  twelve_month<-rbind(f_h12[month,]) 
  testing<-window(series, start=c(2017, 7))
  acc_h12<-list()
  for(i in 1:ncol(twelve_month)){
    acc_h12[[i]]<-accuracy(as.vector(twelve_month[,i]), testing)
  }
  acc_h12 <- Reduce(rbind, acc_h12)
  row.names(acc_h12) <- modelsnames
  
  
  Totales<-cbind(round(acc_h1[,c(2,7)], 2),round(acc_h2[,c(2,7)], 2),
                 round(acc_h3[,c(2,7)], 2),round(acc_h6[,c(2,7)], 2),
                 round(acc_h12[,2], 2)  ) 
  colnames(Totales)[9]<-"RMSE" 
  Totales 
  colnames(datav)[index]
  #row.names(Totales) <- names(forecasts_h12)
  write.table(Totales,paste(colnames(datav)[index], "AD.csv",sep=""))
  
}
