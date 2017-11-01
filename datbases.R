# managing Data bases 
library(dplyr)
setwd("C:/Users/dzapata/Documents/R")
chicago <- readRDS("chicago.rds")
dim(chicago) 
str(chicago)

subset<-select(chicago,city:date)
head(subset)
subset <- select(chicago, ends_with("2"))
str(subset)
subset<-select(chicago, starts_with("d"))
str(subset)

chic.f<-filter(chicago, pm25tmean2==c(15,17))
str(chic.f)
summary(chic.f)
chic.f<-filter(chicago, pm25tmean2>30 & tmpd>80)
select(chic.f,tmpd,pm25tmean2)

chicago<-arrange(chicago, desc(date))
head(select(chicago,date,pm25tmean2))

chicago<-arrange(chicago, date)
head(select(chicago,date,pm25tmean2))

chicago<-rename(chicago, dewpoint=dptp, pm15=pm25tmean2)

chicago<-mutate(chicago, pm25detrend=pm15-mean(pm15,na.rm = T))
head(chicago)

chicago2<-transmute(chicago, 
                                  pm10detrend = pm10tmean2 - mean(pm10tmean2, na.rm = TRUE),
                                  o3detrend = o3tmean2 - mean(o3tmean2, na.rm = TRUE))

chicago <- mutate(chicago, year = as.POSIXlt(date)$year + 1900)
chicago<-mutate(chicago, year2=as.numeric(substring(date,1,4)))
chicago<-select(chicago,-year2)
years<-group_by(chicago,year)
summarize(chicago, pm25=sum(pm15, na.rm=T)) 

qq<-quantile(select(chicago,pm15), seq(0,1,0.2), na.rm = T)
chicago<-mutate(chicago, pm25.quint=cut(pm15,qq))
quint<-group_by(chicago,pm25.quint) 
summarize(quint, o3=mean(o3tmean2, na.rm= T), 
          no2=mean(no2tmean2,na.rm=T)) 

qq<-quantile(chicago$pm15, seq(0,1,0.2), na.rm = T) 

mutate(chicago, pm25.quint=cut(pm15,qq)) %>% 
  group_by(pm25.quint)%>%
  summarize(o3=mean(o3tmean2,na.rm=T)) 

mutate(chicago, month=as.numeric(substring(date,6,7)))%>%
  group_by(month)%>%
  summarise(pm25=mean(pm15,na.rm=T),
            o3=max(o3tmean2,na.rm=T),
            no2=median(no2tmean2,na.rm=T))
  
x <- list(a = as.data.frame(matrix(1:6, ncol=2)), b = as.data.frame(matrix(rnorm(6), ncol=2))) 
lapply(x$a,mean)
lapply(x,mean) 
x<-1:4 
lapply(x,runif)

x <- list(a = matrix(1:4, 2, 2), b = matrix(1:6, 3, 2)) 
lapply(x, function(x){x[,1]})
 

x <- c(rnorm(10), runif(10), rnorm(10, 1)) 
f <- gl(3, 10) 
split(x, f)
lapply(split(x, f), mean) 

library(datasets)
s <- split(airquality, airquality$Month)

split(airquality,airquality$Month)%>%
  lapply(function(x){
    colMeans(x[,c("Ozone", "Solar.R", "Wind")])
  }) 


x <- c(rnorm(10), runif(10), rnorm(10, 1)) 
f<-gl(3,10) 
tapply(x,f,mean) 

a <- array(rnorm(2 * 2 * 10), c(2, 2, 10)) 

apply(a,c(1,2),mean)

apply(a,c(1,3),mean)



