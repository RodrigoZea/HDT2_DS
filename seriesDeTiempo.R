# install.packages("forecast")
# install.packages("fUnitRoots")
# install.packages("ggfortify")

library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)

data("AirPassengers")
class(AirPassengers)
#Saber cuando empieza la serie y cuando termina
start(AirPassengers)
end(AirPassengers)
#Saber la frecuencia de la serie
frequency(AirPassengers)
#Ver el gráfico de la serie
plot(AirPassengers)
abline(reg=lm(AirPassengers~time(AirPassengers)), col=c("red"))

plot(aggregate(AirPassengers,FUN=mean))
dec.AirPass<-decompose(AirPassengers)
plot(dec.AirPass)
plot(dec.AirPass$seasonal)

#Aplicaremos una transformación logarítmica
logAirPassengers <- log(AirPassengers)
plot(decompose(logAirPassengers))

#Ver el gráfico de la serie
plot(logAirPassengers)

#Para saber si hay raíces unitarias
adfTest(logAirPassengers)
adfTest(diff(logAirPassengers))
#Gráfico de autocorrelación
acf(logAirPassengers)
# funciones de autocorrelación y autocorrelación parcial
acf(diff(logAirPassengers),12)
pacf(diff(logAirPassengers))

# Hacer el modelo

auto.arima(AirPassengers)

fit <- arima(log(AirPassengers), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))
pred <- predict(fit, n.ahead = 10*12)
ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))

fit2 <- arima(log(AirPassengers), c(2, 1, 1),seasonal = list(order = c(0, 1, 0), period = 12))

forecastAP <- forecast(fit2, level = c(95), h = 120)
autoplot(forecastAP)
