# Libraries  
library(haven) 
library(ISLR)
library(boot)  
library(dplyr) 
library(parallel)
library(doMC)
library(doParallel) 
library(foreach)
rm(list=ls())
# fix seed 
set.seed(1) 
### Partidas total 
setwd("//srvobservatorio/TRABAJO/OEE-Contratistas-2017/Daniel Zapata/Zapatos")
calzado<-read_dta("calzado_total_partida.dta")  
calzado$partida<-as.numeric(calzado$partida)
calzado$TimePeriod<- calzado$year + (calzado$mes - 0.5) / 12 
partidas<-calzado$partida[!duplicated(calzado$partida)] 
partidas<-partidas[order(partidas)] 
umbral<-c(3,2,7,3,4,2)
base<-subset(calzado, partida==partidas[2])
base<-base[order(base$TimePeriod),]
y<-base$pkilo 
x<-base$TimePeriod

table(cut(y, 5))
local_reg <- lm(y~cut(x, 5))
prediccion <- predict(local_reg, data = y, se.fit = T)
IC <- data.frame(inf = prediccion$fit - 1.96*prediccion$se.fit,
                 sup = prediccion$fit + 1.96*prediccion$se.fit)

plot(x,y,  bty = "l", xlab="", ylab= "Precio por kilo")
lines(x,predict(local_reg), col="blue", lwd=2)
lines(x, IC$inf, col = "red", lwd = 1)
lines(x, IC$sup, col = "red", lwd = 1)    
legend("topright", c("Ajuste local", "95% IC"), lwd=1, col = c("blue", "red"), ncol = 1, bty="n",cex=0.8, pt.cex=0.8)
title(paste("Regresión Lineal Local partida", partidas[1], sep = " "), cex.main = 1.5 ) 
abline(h=umbral[1],v=0, lwd =2, col = "green4", lty = "dashed") 
mtext(side = 1, "Fuente: DANE-DIAN, calculos OEE", adj = 1, cex = 1, line = 3)  

set.seed(1)
deltas <- rep(NA, 15) 
base<-select(base,c("pkilo", "TimePeriod" ) )
for (i in 1:15) {
  
  fit <- glm(pkilo ~ poly(TimePeriod, i), data = base)
  deltas[i] <- cv.glm(base, fit)$delta[1]
}
plot(1:15, deltas, xlab = "Grado", ylab = "Prueba Error Cuadrádico Medio", type = "l")
d.min <- which.min(deltas)
points(which.min(deltas), deltas[which.min(deltas)], col = "red", cex = 2, pch = 20)

no_cores <- detectCores() - 1
cl<-makeCluster(no_cores)
registerDoParallel(cl)
delta2<-foreach(i = 1:15, .combine = c, 
                .packages = "boot") %dopar% {
                  d.min2<-cv.glm(base, 
                                 glm(pkilo ~ poly(TimePeriod, i),
                                     data = base))$delta[1] 
                  return(d.min2)
}
stopImplicitCluster(cl)
stopImplicitCluster()
plot(1:15, delta2, xlab = "Grado", ylab = "Prueba Error Cuadrádico Medio", type = "l")
d.min2 <- which.min(delta2)
points(which.min(delta2), deltas[which.min(delta2)], col = "red", cex = 2, pch = 20)



plot(pkilo ~ TimePeriod, data = base,  bty = "l", xlab="", ylab= "Precio por kilo")
agelims <- range(base$TimePeriod)
age.grid <- seq(from = agelims[1], to = (agelims[2]+.5))
fit <- lm(pkilo ~ poly(TimePeriod, d.min2), data = base)
preds <- predict(fit, newdata = list(TimePeriod = age.grid))
lines(age.grid, preds, col = "blue", lwd = 2) 
abline(h=umbral[1],v=0, lwd =2, col = "green4", lty = "dashed") 
title(paste("Regresión polinómica partida", partidas[1], sep = " "), cex.main = 1.5 ) 
mtext(side = 1, "Menor error cuadrático medio. Fuente: DANE-DIAN, calculos OEE", 
      adj = 1, cex = 1, line = 3)  




## Total Partida
setwd("//srvobservatorio/TRABAJO/OEE-Contratistas-2017/Daniel Zapata/Zapatos")
rm(list=ls())
calzado<-read_dta("calzado_total_partida.dta")  
calzado$partida<-as.numeric(calzado$partida)
calzado$TimePeriod<- calzado$year + (calzado$mes - 0.5) / 12 
partidas<-calzado$partida[!duplicated(calzado$partida)] 
partidas<-partidas[order(partidas)] 
umbral<-c(3,2,7,3,4,2)

# regresion cuartilica
setwd("//srvobservatorio/TRABAJO/OEE-Contratistas-2017/Daniel Zapata/Zapatos/Img partidas total")
for(i in 1:length(partidas)){  

  base<-subset(calzado, partida==partidas[i])
  base<-base[order(base$TimePeriod),]
  y<-base$pkilo 
  x<-base$TimePeriod
  
  table(cut(y, 5))
  local_reg <- lm(y~cut(x, 5))
  prediccion <- predict(local_reg, data = y, se.fit = T)
  IC <- data.frame(inf = prediccion$fit - 1.96*prediccion$se.fit,
                   sup = prediccion$fit + 1.96*prediccion$se.fit) 
  png(paste(partidas[i], "local.png",sep=""),width = 1500,height = 900) 
  plot(x,y,  bty = "l", xlab="", ylab= "Precio por kilo")
  lines(x,predict(local_reg), col="blue", lwd=2)
  lines(x, IC$inf, col = "red", lwd = 1)
  lines(x, IC$sup, col = "red", lwd = 1)    
  legend("topright", c("Ajuste local", "95% IC"), lwd=1, col = c("blue", "red"), ncol = 1, bty="n",cex=1, pt.cex=1)
  title(paste("Regresión Lineal Local partida", partidas[i], sep = " "), cex.main = 2.5 ) 
  abline(h=umbral[i],v=0, lwd =2, col = "green4", lty = "dashed")  
  mtext(side = 1, "Fuente: DANE-DIAN, calculos OEE", adj = 1, cex = 2, line = 3)  
  dev.off()
}   
# regresion polinomica
for(i in  1:length(partidas)){ 

  set.seed(1)
  deltas <- rep(NA, 15)  
  base<-subset(calzado, partida==partidas[i]) 
  base<-base[order(base$TimePeriod),]
  base<-select(base,c("pkilo", "TimePeriod" ) )
  no_cores <- detectCores() - 1
  cl<-makeCluster(no_cores)
  registerDoParallel(cl)
  delta2<-foreach(i = 1:15, .combine = c, 
                  .packages = "boot") %dopar% {
                    d.min2<-cv.glm(base, 
                                   glm(pkilo ~ poly(TimePeriod, i),
                                       data = base))$delta[1] 
                    return(d.min2)
                  }
 # stopImplicitCluster(cl)
  d.min2 <- which.min(delta2)

  png(paste(partidas[i], "poly.png",sep=""),width = 1500,height = 900) 
  plot(pkilo ~ TimePeriod, data = base,  bty = "l", xlab="", ylab= "Precio por kilo")
  agelims <- range(base$TimePeriod)
  age.grid <- seq(from = agelims[1], to = (agelims[2]+.5))
  fit <- lm(pkilo ~ poly(TimePeriod, d.min2), data = base)
  preds <- predict(fit, newdata = list(TimePeriod = age.grid))
  lines(age.grid, preds, col = "blue", lwd = 2) 
  abline(h=umbral[i],v=0, lwd =2, col = "green4", lty = "dashed") 
  title(paste("Regresión polinómica partida", partidas[i], sep = " "), cex.main = 1.5 ) 
  mtext(side = 1, "Menor error cuadrático medio. Fuente: DANE-DIAN, calculos OEE", 
        adj = 1, cex = 1, line = 3)   
  dev.off()

}

## Total ventana
setwd("//srvobservatorio/TRABAJO/OEE-Contratistas-2017/Daniel Zapata/Zapatos")
rm(list=ls())
calzado<-read_dta("calzado_ventana_partida.dta")  
calzado$partida<-as.numeric(calzado$partida)
calzado$TimePeriod<- calzado$year + (calzado$mes - 0.5) / 12 
partidas<-calzado$partida[!duplicated(calzado$partida)] 
partidas<-partidas[order(partidas)] 
umbral<-c(3,2,7,3,4,2)

# regresion cuartilica
setwd("//srvobservatorio/TRABAJO/OEE-Contratistas-2017/Daniel Zapata/Zapatos/Img partidas ventana")
for(i in 1:length(partidas)){  
  
  base<-subset(calzado, partida==partidas[i])
  base<-base[order(base$TimePeriod),]
  y<-base$pkilo 
  x<-base$TimePeriod
  
  table(cut(y, 5))
  local_reg <- lm(y~cut(x, 5))
  prediccion <- predict(local_reg, data = y, se.fit = T)
  IC <- data.frame(inf = prediccion$fit - 1.96*prediccion$se.fit,
                   sup = prediccion$fit + 1.96*prediccion$se.fit) 
  png(paste(partidas[i], "local.png",sep=""),width = 1500,height = 900) 
  plot(x,y,  bty = "l", xlab="", ylab= "Precio por unidad", cex.lab=1.5)
  lines(x,predict(local_reg), col="blue", lwd=2)
  lines(x, IC$inf, col = "red", lwd = 1)
  lines(x, IC$sup, col = "red", lwd = 1)    
  legend("topright", c("Ajuste local", "95% IC"), lwd=1, col = c("blue", "red"), ncol = 1, bty="n",cex=2, pt.cex=2)
  title(paste("Regresión Lineal Local partida", partidas[i], sep = " "), cex.main = 2.5 ) 
  abline(h=umbral[i],v=0, lwd =2, col = "green4", lty = "dashed")  
  mtext(side = 1, "Fuente: DANE-DIAN, calculos OEE", adj = 1, cex = 2, line = 3)  
  dev.off()
}   
# regresion polinomica
for(i in  1:length(partidas)){ 
  
  set.seed(1)
  deltas <- rep(NA, 15)  
  base<-subset(calzado, partida==partidas[i])
  base<-base[order(base$TimePeriod),]
  base<-select(base,c("pkilo", "TimePeriod" ) )
  no_cores <- detectCores() - 1
  cl<-makeCluster(no_cores)
  registerDoParallel(cl)
  delta2<-foreach(i = 1:15, .combine = c, 
                  .packages = "boot") %dopar% {
                    d.min2<-cv.glm(base, 
                                   glm(pkilo ~ poly(TimePeriod, i),
                                       data = base))$delta[1] 
                    return(d.min2)
                  }
  # stopImplicitCluster(cl)
  d.min2 <- which.min(delta2)
  
  png(paste(partidas[i], "poly.png",sep=""),width = 1500,height = 900) 
  plot(pkilo ~ TimePeriod, data = base,  bty = "l", xlab="", ylab= "Precio por kilo")
  agelims <- range(base$TimePeriod)
  age.grid <- seq(from = agelims[1], to = (agelims[2]+.5))
  fit <- lm(pkilo ~ poly(TimePeriod, d.min2), data = base)
  preds <- predict(fit, newdata = list(TimePeriod = age.grid))
  lines(age.grid, preds, col = "blue", lwd = 2) 
  abline(h=umbral[i],v=0, lwd =2, col = "green4", lty = "dashed") 
  title(paste("Regresión polinómica partida", partidas[i], sep = " "), cex.main = 1.5 ) 
  mtext(side = 1, "Menor error cuadrático medio. Fuente: DANE-DIAN, calculos OEE", 
        adj = 1, cex = 2, line = 3)   
  dev.off()
  
}

## capitulo Total
setwd("//srvobservatorio/TRABAJO/OEE-Contratistas-2017/Daniel Zapata/Zapatos")
rm(list=ls())
calzado<-read_dta("calzado_total_capitulo.dta")  
calzado$capitulo<-as.numeric(calzado$capitulo)
calzado$TimePeriod<- calzado$year + (calzado$mes - 0.5) / 12 
capitulos<-calzado$capitulo[!duplicated(calzado$capitulo)] 
capitulos<-capitulos[order(capitulos)] 
umbral<-c(3,2,7,3,4,2)

# regresion cuartilica
setwd("//srvobservatorio/TRABAJO/OEE-Contratistas-2017/Daniel Zapata/Zapatos/Img capitulo total")
for(i in 1:length(capitulos)){  
  
  base<-subset(calzado, capitulo==capitulos[i])
  
  y<-base$pkilo 
  x<-base$TimePeriod
  
  table(cut(y, 5))
  local_reg <- lm(y~cut(x, 5))
  prediccion <- predict(local_reg, data = y, se.fit = T)
  IC <- data.frame(inf = prediccion$fit - 1.96*prediccion$se.fit,
                   sup = prediccion$fit + 1.96*prediccion$se.fit) 
  png(paste(capitulos[i], "local.png",sep=""),width = 1500,height = 900) 
  plot(x,y,  bty = "l", xlab="", ylab= "Precio por kilo")
  lines(x,predict(local_reg), col="blue", lwd=2)
  lines(x, IC$inf, col = "red", lwd = 1)
  lines(x, IC$sup, col = "red", lwd = 1)    
  legend("topright", c("Ajuste local", "95% IC"), lwd=1, col = c("blue", "red"), ncol = 1, bty="n",cex=0.8, pt.cex=0.8)
  title(paste("Regresión Lineal Local capitulo", capitulos[i], sep = " "), cex.main = 1.5 )  
  mtext(side = 1, "Fuente: DANE-DIAN, calculos OEE", adj = 1, cex = 1, line = 3)  
  #abline(h=umbral[i],v=0, lwd =2, col = "green4", lty = "dashed") 
  dev.off()
}   
# regresion polinomica
for(i in  1:length(capitulos)){ 
  
  set.seed(1)
  deltas <- rep(NA, 15)  
  base<-subset(calzado, capitulo==capitulos[i])
  base<-select(base,c("pkilo", "TimePeriod" ) )
  no_cores <- detectCores() - 1
  cl<-makeCluster(no_cores)
  registerDoParallel(cl)
  delta2<-foreach(i = 1:15, .combine = c, 
                  .packages = "boot") %dopar% {
                    d.min2<-cv.glm(base, 
                                   glm(pkilo ~ poly(TimePeriod, i),
                                       data = base))$delta[1] 
                    return(d.min2)
                  }
  # stopImplicitCluster(cl)
  d.min2 <- which.min(delta2)
  
  png(paste(capitulos[i], "poly.png",sep=""),width = 1500,height = 900) 
  plot(pkilo ~ TimePeriod, data = base,  bty = "l", xlab="", ylab= "Precio por kilo")
  agelims <- range(base$TimePeriod)
  age.grid <- seq(from = agelims[1], to = (agelims[2]+.5))
  fit <- lm(pkilo ~ poly(TimePeriod, d.min2), data = base)
  preds <- predict(fit, newdata = list(TimePeriod = age.grid))
  lines(age.grid, preds, col = "blue", lwd = 2) 
  #abline(h=umbral[i],v=0, lwd =2, col = "green4", lty = "dashed") 
  title(paste("Regresión polinómica partida", capitulos[i], sep = " "), cex.main = 1.5 ) 
  mtext(side = 1, "Menor error cuadrático medio. Fuente: DANE-DIAN, calculos OEE", 
        adj = 1, cex = 1, line = 3)   
  dev.off()
  
}
## capitulo ventana
setwd("//srvobservatorio/TRABAJO/OEE-Contratistas-2017/Daniel Zapata/Zapatos")
rm(list=ls())
calzado<-read_dta("calzado_ventana_capitulo.dta")  
calzado$capitulo<-as.numeric(calzado$capitulo)
calzado$TimePeriod<- calzado$year + (calzado$mes - 0.5) / 12 
capitulos<-calzado$capitulo[!duplicated(calzado$capitulo)] 
capitulos<-capitulos[order(capitulos)] 
umbral<-c(3,2,7,3,4,2)

# regresion cuartilica
setwd("//srvobservatorio/TRABAJO/OEE-Contratistas-2017/Daniel Zapata/Zapatos/Img capitulo ventana")
for(i in 1:length(capitulos)){  
  
  base<-subset(calzado, capitulo==capitulos[i])
  
  y<-base$pkilo 
  x<-base$TimePeriod
  
  table(cut(y, 5))
  local_reg <- lm(y~cut(x, 5))
  prediccion <- predict(local_reg, data = y, se.fit = T)
  IC <- data.frame(inf = prediccion$fit - 1.96*prediccion$se.fit,
                   sup = prediccion$fit + 1.96*prediccion$se.fit) 
  png(paste(capitulos[i], "local.png",sep=""),width = 1500,height = 900) 
  plot(x,y,  bty = "l", xlab="", ylab= "Precio por kilo")
  lines(x,predict(local_reg), col="blue", lwd=2)
  lines(x, IC$inf, col = "red", lwd = 1)
  lines(x, IC$sup, col = "red", lwd = 1)    
  legend("topright", c("Ajuste local", "95% IC"), lwd=1, col = c("blue", "red"), ncol = 1, bty="n",cex=0.8, pt.cex=0.8)
  title(paste("Regresión Lineal Local capitulo", capitulos[i], sep = " "), cex.main = 1.5 )  
  mtext(side = 1, "Fuente: DANE-DIAN, calculos OEE", adj = 1, cex = 1, line = 3)  
  #abline(h=umbral[i],v=0, lwd =2, col = "green4", lty = "dashed") 
  dev.off()
}  
# regresion polinomica
for(i in  1:length(capitulos)){ 
  
  set.seed(1)
  deltas <- rep(NA, 15)  
  base<-subset(calzado, capitulo==capitulos[i])
  base<-select(base,c("pkilo", "TimePeriod" ) )
  no_cores <- detectCores() - 1
  cl<-makeCluster(no_cores)
  registerDoParallel(cl)
  delta2<-foreach(i = 1:15, .combine = c, 
                  .packages = "boot") %dopar% {
                    d.min2<-cv.glm(base, 
                                   glm(pkilo ~ poly(TimePeriod, i),
                                       data = base))$delta[1] 
                    return(d.min2)
                  }
  # stopImplicitCluster(cl)
  d.min2 <- which.min(delta2)
  
  png(paste(capitulos[i], "poly.png",sep=""),width = 1500,height = 900) 
  plot(pkilo ~ TimePeriod, data = base,  bty = "l", xlab="", ylab= "Precio por kilo")
  agelims <- range(base$TimePeriod)
  age.grid <- seq(from = agelims[1], to = (agelims[2]+.5))
  fit <- lm(pkilo ~ poly(TimePeriod, d.min2), data = base)
  preds <- predict(fit, newdata = list(TimePeriod = age.grid))
  lines(age.grid, preds, col = "blue", lwd = 2) 
  #abline(h=umbral[i],v=0, lwd =2, col = "green4", lty = "dashed") 
  title(paste("Regresión polinómica partida", capitulos[i], sep = " "), cex.main = 1.5 ) 
  mtext(side = 1, "Menor error cuadrático medio. Fuente: DANE-DIAN, calculos OEE", 
        adj = 1, cex = 1, line = 3)   
  dev.off()
  
}
