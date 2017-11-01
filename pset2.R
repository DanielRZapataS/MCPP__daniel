#Librerias necesarias
library("ggplot2")

#Limpiar espacio de trabajo
remove(list = ls())

#Punto 1
kernels <- function(x, type){
  
  if(type == "Triangular") {
    (1-abs(x))*(abs(x)<=1)
  } else if(type == "Uniforme") {
    (1/2)*(abs(x)<=1)
  } else  if(type == "Epanechnikov") {
    (3/4)*(1-x^2)*(abs(x)<=1)
  } else  if(type == "Boxcar") {
    (1/2)*(abs(x)<1)
  } else  if(type == "Quartic") {
    (15/16)*(1-x^2)^2*(abs(x)<=1)
  } else  if(type == "Tricubico") {
    (70/81)*(1-abs(x)^3)^3*(abs(x)<=1)
  } else  if(type == "Gausiano") {
    (1/sqrt(2*pi))*exp(-1/2*x^2)*(abs(x)<=1)
  } 
  
}

x <- c(seq(-1,1,by=0.1))

data <- data.frame(x, y1 = kernels(x, "Triangular"), y2 = kernels(x, "Uniforme"), 
                   y3 = kernels(x, "Epanechnikov"), y4 = kernels(x, "Boxcar"),
                   y5 = kernels(x, "Quartic"), y6 = kernels(x, "Tricubico"), 
                   y7 = kernels(x, "Gausiano"))

plot <- ggplot(data = data, aes(x = x)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  geom_line(aes(y = y2, colour = "Uniforme"), size=1) +
  geom_line(aes(y = y1, colour = "Triangular"), size=1) +
  geom_line(aes(y = y3, colour = "Epanechnikov"), size=1) +
  geom_line(aes(y = y4, colour = "Boxcar"), size=1) +
  geom_line(aes(y = y5, colour = "Quartic"), size=1) +
  geom_line(aes(y = y6, colour = "Tricubico"), size=1) +
  geom_line(aes(y = y7, colour = "Gausiano"), size=1) +  
  xlab("x") +
  scale_y_continuous("K(x) - Función Kernel") + 
  scale_colour_discrete(name = "K(x) - Función")
plot

#Punto 2

#Datos simulados para ajustar metodos no parametricos
set.seed(126)
x = sort(runif(500, min = 0, max = 2))
f = f = sin(2*pi*x)
y = f + rnorm(500, sd=0.3)

#Regresiones Polinomicas Globales
plot(x,y)
legend <- length(7)

for(degree in 1:7){
  fm = lm(y~poly(x, degree))
  lines(x,predict(fm), col=degree, lwd=2)
  
  legend[degree] <- paste0("Poly ", degree, "°")
}

lines(x, f, col = "purple", lwd=2)
legend("top", c(legend, "sin(2*pi*x) + e"), lwd=1, col=c(1:7, "purple"), ncol=3, bty="n",cex=0.8, pt.cex=0.8)
#title("Regresión Polinomica Global")

#Polinomio Grado 7
summary(fm)
prediccion <- predict(fm, data = y, se.fit = T)
IC <- data.frame(inf = prediccion$fit - 1.96*prediccion$se.fit,
                 sup = prediccion$fit + 1.96*prediccion$se.fit)

plot(x,y)
lines(x,predict(fm), col="blue", lwd=2)
lines(x, IC$inf, col = "red", lwd = 2)
lines(x, IC$sup, col = "red", lwd = 2)    
legend("topright", c("Poly 7° ", "95% IC"), lwd=1, col = c("blue", "red"), ncol = 1, bty="n",cex=0.8, pt.cex=0.8)
title("Regresión Polinomica Grado 7")

#Regresion lineal local (Cuartiles)
table(cut(y, 50))
local_reg <- lm(y~cut(x, 50))
prediccion <- predict(local_reg, data = y, se.fit = T)
IC <- data.frame(inf = prediccion$fit - 1.96*prediccion$se.fit,
                 sup = prediccion$fit + 1.96*prediccion$se.fit)

plot(x,y)
lines(x,predict(local_reg), col="blue", lwd=2)
lines(x, IC$inf, col = "red", lwd = 1)
lines(x, IC$sup, col = "red", lwd = 1)    
legend("topright", c("Local Fit", "95% IC"), lwd=1, col = c("blue", "red"), ncol = 1, bty="n",cex=0.8, pt.cex=0.8)
title("Regresión Lineal Local")

#Regresion Splines 
spline_reg=lm(y~bs(x, knots=c(0.5, 1, 1.5)))
prediccion <- predict(spline_reg, data = y, se.fit = T)
IC <- data.frame(inf = prediccion$fit - 1.96*prediccion$se.fit,
                 sup = prediccion$fit + 1.96*prediccion$se.fit)

plot(x,y)
lines(x,predict(spline_reg), col="blue", lwd=2)
lines(x, IC$inf, col = "red", lwd = 1)
lines(x, IC$sup, col = "red", lwd = 1)
abline(v=c(0.5, 1, 1.5))
legend("topright", c("Splines Fit", "95% IC"), lwd=1, col = c("blue", "red"), ncol = 1, bty="n",cex=0.8, pt.cex=0.8)
title("Regresión Lineal Local")




#Punto 3

library(ISLR)
library(boot)
set.seed(1)
deltas <- rep(NA, 15)
for (i in 1:15) {
  fit <- glm(wage ~ poly(age, i), data = Wage)
  deltas[i] <- cv.glm(Wage, fit, K = 15)$delta[1]
}
plot(1:15, deltas, xlab = "Grado", ylab = "Prueba Error Cuadrádico Medio", type = "l")
d.min <- which.min(deltas)
points(which.min(deltas), deltas[which.min(deltas)], col = "red", cex = 2, pch = 20)

#Análisis de Varianza

library(xtable)

fit1 <- lm(wage ~ age, data = Wage)
fit2 <- lm(wage ~ poly(age, 2), data = Wage)
fit3 <- lm(wage ~ poly(age, 3), data = Wage)
fit4 <- lm(wage ~ poly(age, 4), data = Wage)
fit5 <- lm(wage ~ poly(age, 5), data = Wage)
anova(fit1, fit2, fit3, fit4, fit5)

newobject2<-anova(fit1, fit2, fit3, fit4, fit5)

xtable(newobject2, type = "latex", file = "filename2.tex")

#Gráfica

plot(wage ~ age, data = Wage, col = "blue")
agelims <- range(Wage$age)
age.grid <- seq(from = agelims[1], to = agelims[2])
fit <- lm(wage ~ poly(age, 3), data = Wage)
preds <- predict(fit, newdata = list(age = age.grid))
lines(age.grid, preds, col = "red", lwd = 2)

# Función Step

cvs <- rep(NA, 15)
for (i in 2:15) {
  Wage$age.cut <- cut(Wage$age, i)
  fit <- glm(wage ~ age.cut, data = Wage)
  cvs[i] <- cv.glm(Wage, fit, K = 15)$delta[1]
}
plot(2:15, cvs[-1], xlab = "Puntos de Corte", ylab = "Prueba Error Cuadrádico Medio", type = "l")
d.min <- which.min(cvs)
points(which.min(cvs), cvs[which.min(cvs)], col = "red", cex = 2, pch = 20)

# Creando la Gráfica de Ajuste Escalonada

plot(wage ~ age, data = Wage, col = "blue")
agelims <- range(Wage$age)
age.grid <- seq(from = agelims[1], to = agelims[2])
fit <- glm(wage ~ cut(age, 8), data = Wage)
preds <- predict(fit, data.frame(age = age.grid))
lines(age.grid, preds, col = "red", lwd = 2)

#Exploración de Estatus Marital y Tipo de Trabajo

set.seed(1)
summary(Wage$maritl)
#tabla3 <-summary(Wage$maritl)
#xtable(tabla3, type = "latex", file = "filename2.tex")
summary(Wage$jobclass)
par(mfrow = c(1, 2))
plot(Wage$maritl, Wage$wage)
plot(Wage$jobclass, Wage$wage)

#Ajuste de Modelos No Lineales

library(splines)
library(gam)

fit0 <- gam(wage ~ lo(year, span = 0.7) + s(age, 5) + education, data = Wage)
fit1 <- gam(wage ~ lo(year, span = 0.7) + s(age, 5) + education + jobclass, data = Wage)
fit2 <- gam(wage ~ lo(year, span = 0.7) + s(age, 5) + education + maritl, data = Wage)
fit3 <- gam(wage ~ lo(year, span = 0.7) + s(age, 5) + education + jobclass + maritl, data = Wage)
table3<-anova(fit0, fit1, fit2, fit3)
xtable(table3, type = "latex", file = "filename2.tex")


#Gráfica de ajuste

par(mfrow = c(3, 3))
plot(fit3, se = T, col = "red")


# Punto 4

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

data("Auto")
names(Auto)
attach(Auto)  
?Auto

#Estadisticas descriptivas
summary(Auto)
xtable(summary(Auto[, -9]))

pairs(Auto[, c(-8,-9)])

par(mfrow = c(4, 4))
p1<-ggplot(data = Auto, aes(x = cylinders)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  geom_point(aes(y = mpg), color="black", alpha=0.3) +
  geom_smooth(aes(y = mpg), method = "lm", formula = y ~ poly(x, 4), color="orange")  +  
  xlab("cylinders") + ylab("Miles per Galon")


p2<-ggplot(data = Auto, aes(x = displacement)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  geom_point(aes(y = mpg), color="black", alpha=0.3) +
  geom_smooth(aes(y = mpg), method = "lm", formula = y ~ poly(x, 4), color="red")  +  
  xlab("Displacement") + ylab("Miles per Galon")


p3<-ggplot(data = Auto, aes(x = horsepower)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  geom_point(aes(y = mpg), color="black", alpha=0.3) +
  geom_smooth(aes(y = mpg), method = "lm", formula = y ~ poly(x, 4), color="blue")  +  
  xlab("Horsepower") + ylab("Miles per Galon")

p4<-ggplot(data = Auto, aes(x = weight)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  geom_point(aes(y = mpg), color="black", alpha=0.3) +
  geom_smooth(aes(y = mpg), method = "lm", formula = y ~ poly(x, 4), color="green")  +  
  xlab("weight") + ylab("Miles per Galon")

multiplot(p1, p2, p3, p4, cols=2)



# Cylinders

#Validacion Cruzada - Polinomio
set.seed(4)
deltas <- rep(NA, 15)
for (i in 1:15) {
  fit <- glm(mpg ~ poly(cylinders, i), data = Auto)
  deltas[i] <- cv.glm(Auto, fit, K = 10)$delta[1]
}
plot(1:15, deltas, xlab = "Grado", ylab = "Prueba ECM", type = "l")
d.min <- which.min(deltas)
points(which.min(deltas), deltas[which.min(deltas)], col = "red", cex = 2, pch = 20)

#Validacion Cruzada - Step
set.seed(4)
cvs <- rep(NA, 10)
for (i in 2:10) {
  Auto$dis.cut <- cut(Auto$cylinders, i)
  fit <- glm(mpg ~ dis.cut, data = Auto)
  cvs[i] <- cv.glm(Auto, fit, K = 10)$delta[1]
}
plot(2:10, cvs[-1], xlab = "Intervalos", ylab = "Prueba ECM", type = "l")
d.min <- which.min(cvs)
points(which.min(cvs), cvs[which.min(cvs)], col = "red", cex = 2, pch = 20)  


# Horsepower

#Validacion Cruzada - Polinomio
set.seed(4)
deltas <- rep(NA, 15)
for (i in 1:15) {
  fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  deltas[i] <- cv.glm(Auto, fit, K = 10)$delta[1]
}
plot(1:15, deltas, xlab = "Grado", ylab = "Prueba ECM", type = "l")
d.min <- which.min(deltas)
points(which.min(deltas), deltas[which.min(deltas)], col = "red", cex = 2, pch = 20)

#Validacion Cruzada - Step
set.seed(4)
cvs <- rep(NA, 10)
for (i in 2:10) {
  Auto$dis.cut <- cut(Auto$horsepower, i)
  fit <- glm(mpg ~ dis.cut, data = Auto)
  cvs[i] <- cv.glm(Auto, fit, K = 10)$delta[1]
}
plot(2:10, cvs[-1], xlab = "Intervalos", ylab = "Prueba ECM", type = "l")
d.min <- which.min(cvs)
points(which.min(cvs), cvs[which.min(cvs)], col = "red", cex = 2, pch = 20)  

# Displacement

#Validacion Cruzada - Polinomio
set.seed(4)
deltas <- rep(NA, 15)
for (i in 1:15) {
  fit <- glm(mpg ~ poly(displacement, i), data = Auto)
  deltas[i] <- cv.glm(Auto, fit, K = 10)$delta[1]
}
plot(1:15, deltas, xlab = "Grado", ylab = "Prueba ECM", type = "l")
d.min <- which.min(deltas)
points(which.min(deltas), deltas[which.min(deltas)], col = "red", cex = 2, pch = 20)

#Validacion Cruzada - Step
set.seed(4)
cvs <- rep(NA, 10)
for (i in 2:10) {
  Auto$dis.cut <- cut(Auto$displacement, i)
  fit <- glm(mpg ~ dis.cut, data = Auto)
  cvs[i] <- cv.glm(Auto, fit, K = 10)$delta[1]
}
plot(2:10, cvs[-1], xlab = "Intervalos", ylab = "Prueba ECM", type = "l")
d.min <- which.min(cvs)
points(which.min(cvs), cvs[which.min(cvs)], col = "red", cex = 2, pch = 20) 

#Weight

set.seed(4)
deltas <- rep(NA, 15)
for (i in 1:15) {
  fit <- glm(mpg ~ poly(weight, i), data = Auto)
  deltas[i] <- cv.glm(Auto, fit, K = 10)$delta[1]
}
plot(1:15, deltas, xlab = "Grado", ylab = "Prueba ECM", type = "l")
d.min <- which.min(deltas)
points(which.min(deltas), deltas[which.min(deltas)], col = "red", cex = 2, pch = 20)

#Validacion Cruzada - Step
set.seed(4)
cvs <- rep(NA, 10)
for (i in 2:10) {
  Auto$dis.cut <- cut(Auto$weight, i)
  fit <- glm(mpg ~ dis.cut, data = Auto)
  cvs[i] <- cv.glm(Auto, fit, K = 10)$delta[1]
}
plot(2:10, cvs[-1], xlab = "Intervalos", ylab = "Prueba ECM", type = "l")
d.min <- which.min(cvs)
points(which.min(cvs), cvs[which.min(cvs)], col = "red", cex = 2, pch = 20)  


#Ajuste usando resultados de CV

g1<- ggplot(data = Auto, aes(x = cylinders)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  geom_point(aes(y = mpg), color="black", alpha=0.3) +
  geom_smooth(aes(y = mpg), method = "lm", formula = y ~ poly(x, 3), color="orange")  +
  geom_smooth(aes(y = mpg), method = "lm", formula = y ~ cut(x, 4), color="blue")  +        
  xlab("Cylinders") + ylab("Miles per Galon")

g2<- ggplot(data = Auto, aes(x = horsepower)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  geom_point(aes(y = mpg), color="black", alpha=0.3) +
  geom_smooth(aes(y = mpg), method = "lm", formula = y ~ poly(x, 6), color="red")  +
  geom_smooth(aes(y = mpg), method = "lm", formula = y ~ cut(x, 8), color="blue")  +        
  xlab("Horsepower") + ylab("Miles per Galon")

g3<- ggplot(data = Auto, aes(x = displacement)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  geom_point(aes(y = mpg), color="black", alpha=0.3) +
  geom_smooth(aes(y = mpg), method = "lm", formula = y ~ poly(x, 11), color="cyan")  +
  geom_smooth(aes(y = mpg), method = "lm", formula = y ~ cut(x, 9), color="blue")  +        
  xlab("Horsepower") + ylab("Miles per Galon")

g4<- ggplot(data = Auto, aes(x = weight)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  geom_point(aes(y = mpg), color="black", alpha=0.3) +
  geom_smooth(aes(y = mpg), method = "lm", formula = y ~ poly(x, 7), color="green")  +
  geom_smooth(aes(y = mpg), method = "lm", formula = y ~ cut(x, 6), color="blue")  +        
  xlab("Weight") + ylab("Miles per Galon")

multiplot(g1, g2, g3, g4, cols=2)

# Punto 5

library(ggcorrplot)
data("Weekly")
names(Weekly)
?Weekly
attach(Weekly)

#Estadisticas descriptivas
summary(Weekly)
xtable(summary(Weekly[, c(1:9)]))
xtable(summary(Weekly[, c(6:9)]))

#Matrix de Correlacion
rho <- cor(Weekly[, c(-1, -9)])
ggcorrplot(rho, hc.order = TRUE, type = "lower", lab = TRUE)

#Correlograma  
pairs(Weekly[, c(-1, -9)])

#Algunos scatterplots
par(mfrow = c(1, 1))
plot(Year, Volume)


#Regresion Logistica
fit1 <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)
summary(fit1)

xtable(summary(fit1))

Weekly$yhat <- predict(fit1, type = "response")
Weekly$prediccion <- rep("Down", length(Weekly$yhat))
Weekly$prediccion[Weekly$yhat > 0.5] <- "Up"

xtable(table(Weekly$prediccion, Direction))

Weekly$prediccion <- NULL
Weekly$yhat <- NULL

#Trainig
train <- (Year < 2009)
Weekly0910 <- Weekly[!train, ]
Direction0910 <- Direction[!train]
fit2 <- glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
summary(fit2)

xtable(summary(fit2))

Weekly0910$yhat <- predict(fit2, Weekly0910,  type = "response")
Weekly0910$prediccion <- rep("Down", length(Weekly0910$yhat))
Weekly0910$prediccion[Weekly0910$yhat > 0.5] <- "Up"

xtable(table(Weekly0910$prediccion, Weekly0910$Direction))

#LDA
library(MASS)
fit3 <- lda(Direction ~ Lag2, data = Weekly, subset = train)
fit3  

pred_lda <- predict(fit3, Weekly0910)
xtable(table(pred_lda$class, Weekly0910$Direction))

detach(Weekly)


