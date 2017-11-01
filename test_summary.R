#tests 
setwd("F:/E/MINCIT/TRABAJO/Pronostico_turismo") 
library(readxl) 
library(tseries) 
library(forecast) 
library(urca)  
library(readxl)
library(TSA)  
library(lmtest) 
library(ggplot2) 
library(seasonal)
library(moments)

# DATA 
rm(list=ls())
data<-read_excel("Llegadasrats2.xls")  
countries<-read_excel("paises.xlsx")  
series<-cbind(data[2],countries[3:7])  
colnames(series)[c(2,3,5)]<-c("EU","Mercosur","UE")
series<-ts(series, start = c(2010,1), frequency =12) 


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


#setwd("F:/E/MINCIT/TRABAJO/Pronostico_turismo/Test") 
test<-apply(data,2, function(x){ 
  adf_t=summary( ur.df(x, type = "trend",lags = 20, selectlags = "BIC" )) 
  pp=summary( ur.pp(x, type = "Z-tau", model = "trend", lags = "long")) 
  kpss=summary(ur.kpss(x, type="tau" ,lags="short" ))
  round(c(adf_t@teststat[,"tau3"], pp@teststat ,kpss@teststat), 2)
  
})  

testdiff<-apply(data,2, function(x){ 
  adf_c<-summary( ur.df(diff(x), type = "drift",lags = 20, selectlags = "BIC" )) 
  pp<-summary( ur.pp(diff(x), type = "Z-tau", model = "constant", lags = "long")) 
  kpss<-summary(ur.kpss(diff(x), type="mu" ,lags="short" ))
  round(c( adf_c@teststat[,"tau2"], pp@teststat, kpss@teststat), 2)
  
})



test_v<-apply(datav,2, function(x){ 
  adf_c<-summary( ur.df(x, type = "drift",lags = 20, selectlags = "BIC" )) 
  pp<-summary( ur.pp(x, type = "Z-tau", model = "constant", lags = "long")) 
  kpss<-summary(ur.kpss(x, type="mu" ,lags="short" ))
  round(c( adf_c@teststat[,"tau2"], pp@teststat, kpss@teststat), 2)
  
})

test_vdiff<-apply(datav,2, function(x){ 
  adf_c<-summary( ur.df(diff(x), type = "drift",lags = 20, selectlags = "BIC" )) 
  pp<-summary( ur.pp(diff(x), type = "Z-tau", model = "constant", lags = "long")) 
  kpss<-summary(ur.kpss(diff(x), type="mu" ,lags="short" ))
  round(c( adf_c@teststat[,"tau2"], pp@teststat, kpss@teststat), 2)
  
})


test_ad<-apply(adjusted,2, function(x){ 
  adf_c<-summary( ur.df(x, type = "drift",lags = 20, selectlags = "BIC" )) 
  pp<-summary( ur.pp(x, type = "Z-tau", model = "constant", lags = "long")) 
  kpss<-summary(ur.kpss(x, type="mu" ,lags="short" ))
  round(c( adf_c@teststat[,"tau2"], pp@teststat, kpss@teststat), 2)
  
})

test_addiff<-apply(adjusted,2, function(x){ 
  adf_c<-summary( ur.df(diff(x), type = "drift",lags = 20, selectlags = "BIC" )) 
  pp<-summary( ur.pp(diff(x), type = "Z-tau", model = "constant", lags = "long")) 
  kpss<-summary(ur.kpss(diff(x), type="mu" ,lags="short" ))
  round(c( adf_c@teststat[,"tau2"], pp@teststat, kpss@teststat), 2)
  
})



rownames(test)<-c("ADF","PP","KPSS") 
rownames(test_v)<-c("ADF","PP","KPSS") 
rownames(testdiff)<-c("ADF","PP","KPSS") 
rownames(test_vdiff)<-c("ADF","PP","KPSS") 
rownames(test_ad)<-c("ADF","PP","KPSS") 
rownames(test_addiff)<-c("ADF","PP","KPSS")  

#cbind(test,test_v) 
setwd("E:/E/MINCIT/TRABAJO/Pronostico_turismo/Results")

summary<-apply(data,2,function(x){c(
  round(mean(x),2), 
  round(sd(x),2),
  round(skewness(x),2),
  round(kurtosis(x),2))})  
rownames(summary)<-c("Media","Des.","Asim.","Kurt.")

summaryv<-apply(datav,2,function(x){c(
  round(mean(x),2), 
  round(sd(x),2),
  round(skewness(x),2),
  round(kurtosis(x),2))})
rownames(summaryv)<-c("Media","Des.","Asim.","Kurt.")

summaryad<-apply(adjusted,2,function(x){c(
  round(mean(x),2), 
  round(sd(x),2),
  round(skewness(x),2),
  round(kurtosis(x),2))})
rownames(summaryad)<-c("Media","Des.","Asim.","Kurt.")

write.csv(test,"nivel.csv")
#write.csv(test_v,"crecimiento.csv")
write.csv(testdiff,"niveldiff.csv")
#write.csv(test_vdiff,"crecimetodiff.csv") 
write.csv(test_ad,"adjusted.csv")
write.csv(test_addiff,"adjusteddiff.csv") 

write.csv(summary,"summary.csv")
write.csv(summaryv,"summaryV.csv")
write.csv(summaryad,"summaryad.csv")

