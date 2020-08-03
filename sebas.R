# HDT #2 - Series de Tiempo
# Rodrigo Zea - 17058
# Gustavo de Leon - 17085
# Sebastian Arriola - 11463

library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)
library(tabulizer)
library(corrplot)
library(dplyr)
library(stringr)

# Lectura de Datos
pages<-extract_tables("data.pdf") #datos2020
datosImp <- do.call(rbind, pages)
nombresVar<-datosImp[1,]
datosImp<-as.data.frame(datosImp[2:nrow(datosImp),])
nombresVar[c(1,4,5,6,8,10,11,15,16,21,23,24)]<-c("Anio","GasAviacion","GasSuperior","GasRegular","rTurboJet","DieselLS","DieselULS","AceitesLub","GrasasLub","PetroleoReconst","Orimulsion","MezclasOleosas")
names(datosImp)<-nombresVar
View(datosImp)

# Limpieza de datos - quitar headers
datosImp<-datosImp[-c(46, 96, 146, 196),]

# Funcion para quitar comas
replaceCommas<-function(x){
  x<-as.numeric(gsub("\\,", "", x))
}

for(i in names(datosImp)) {
  datosImp[[i]]<-replaceCommas(datosImp[[i]])
}

# Datos to num
datosImp[, c(1:25)] <- sapply(datosImp[, c(1:25)], as.numeric)

# Mapa de correlacion
corrMatrix<-cor(datosImp,use = "pairwise.complete.obs")
corrplot(corrMatrix)

# 1. Diesel
dataDiesel<-ts(datosImp$Diesel, start=c(2001, 01), end=c(2020, 03), 12)
# omitir na
dataDiesel<-na.omit(dataDiesel)
start(dataDiesel)
end(dataDiesel)
frequency(dataDiesel)
plot(dataDiesel)
abline(reg=lm(dataDiesel~time(dataDiesel)), col=c("red"))
decDiesel<-decompose(dataDiesel)
plot(decDiesel)
plot(decDiesel$trend)
plot(decDiesel$seasonal)

# division en train / test
trainDiesel<-head(dataDiesel, round(length(dataDiesel) * 0.7))
testDiesel<-tail(dataDiesel, round(length(dataDiesel) * 0.3))
# h se utiliza al generar el modelo y predecir
h<-length(trainDiesel) - length(testDiesel)

# transformacion logaritmica
logDiesel<-log(trainDiesel)
plot(decompose(logDiesel))

# Dickey Fuller Test
# p > 0.05
adfTest(trainDiesel)

# raices unitarias
# p > 0.05
unitrootTest(trainDiesel)

# diferenciacion, es necesario
# d = 1
adfTest(diff(trainDiesel))
unitrootTest(diff(trainDiesel))

# intentar identificar parametros p y q
# q = 3
acf(logDiesel, 100, na.action = na.pass)
# p = 2
pacf(logDiesel, 100, na.action = na.pass)

# Para tener una idea de los parametros estacionales.
Acf(diff(logDiesel), 36)
Pacf(diff(logDiesel), 36)

# arima
fitArima<-arima(log(trainDiesel), order=c(2, 1, 3), seasonal=c(1, 1, 0))
fitArima2<-arima(log(trainDiesel), order=c(2, 1, 3), seasonal=c(0, 1, 1))

# ajustar n.ahead al numero de meses que tengamos en conjunto de prueba
pred<-predict(fitArima, 7*12)
ts.plot(trainDiesel, exp(pred$pred), log="y", lty=c(1,3))
pred2<-predict(fitArima2, 7*12)
ts.plot(trainDiesel, exp(pred2$pred), log="y", lty=c(1,3))

# h es el numero de meses a predecir
forecastDiesel<-forecast(fitArima, level=c(95), h=7*12)
autoplot(forecastDiesel) + autolayer(log(testDiesel))
forecastDiesel2<-forecast(fitArima2, level=c(95), h=7*12)
autoplot(forecastDiesel2) + autolayer(log(testDiesel))


# prophet con diesel
library(prophet)
library(zoo)
dataFrame<-data.frame(ds=as.Date(as.yearmon(time(trainDiesel))), y=as.matrix(trainDiesel))
testDataFrame<-data.frame(ds=as.Date(as.yearmon(time(testDiesel))), y=as.matrix(testDiesel))
fitProphet<-prophet(dataFrame, yearly.seasonality = T, weekly.seasonality = T)
future<-make_future_dataframe(fitProphet, periods = 7 * 12, freq = "month", include_history = T)
pred<-predict(fitProphet, future)
pred<-pred[, c("ds", "yhat", "yhat_lower", "yhat_upper")]
plot(fitProphet, pred)
prediction<-tail(pred, 61)
prediction$y<-testDataFrame$y
ggplot(prediction, aes(x=ds, y=yhat)) +
  geom_line(size=1, color="blue", alpha=0.8) +
  geom_ribbon(aes(ymin=yhat_lower, ymax=yhat_upper), fill="blue", alpha=0.2) +
  geom_line(data=prediction, aes(x=ds, y=y), color="red")

# ------------------------------------------------------------------------------------------------

# 2. Regular
dataRegular<-ts(datosImp$GasRegular, start=c(2001, 01), end=c(2020, 03), 12)
start(dataRegular)
end(dataRegular)
frequency(dataRegular)
plot(dataRegular)
abline(reg=lm(dataRegular~time(dataRegular)), col=c("red"))
decRegular<-decompose(dataRegular)
plot(decRegular)
plot(decRegular$trend)
plot(decRegular$seasonal)

# division en train / test
trainRegular<-head(dataRegular, round(length(dataRegular) * 0.7))
testRegular<-tail(dataRegular, round(length(dataRegular) * 0.3))
# h se utiliza al generar el modelo y predecir
h<-length(trainRegular) - length(testRegular)

# transformacion logaritmica
logRegular<-log(trainRegular)
plot(decompose(logRegular))

# Dickey Fuller Test
# p > 0.05
adfTest(trainRegular)

# raices unitarias
# p > 0.05
unitrootTest(trainRegular)

# diferenciacion, es necesario
# d = 1
adfTest(diff(trainRegular))
unitrootTest(diff(trainRegular))

# intentar identificar parametros p y q
# q = 3
acf(logRegular, 100, na.action = na.pass)
# p = 2
pacf(logRegular, 100, na.action = na.pass)

# Para tener una idea de los parametros estacionales.
Acf(diff(logRegular), 36)
Pacf(diff(logRegular), 36)

# arima
fitArima<-arima(log(trainRegular), order=c(2, 1, 3), seasonal=c(1, 1, 0))
fitArima2<-arima(log(trainRegular), order=c(2, 1, 3), seasonal=c(0, 1, 1))

# ajustar n.ahead al numero de meses que tengamos en conjunto de prueba
pred<-predict(fitArima, 7*12)
ts.plot(trainRegular, exp(pred$pred), log="y", lty=c(1,3))
pred2<-predict(fitArima2, 7*12)
ts.plot(trainRegular, exp(pred2$pred), log="y", lty=c(1,3))

# h es el numero de meses a predecir
forecastRegular<-forecast(fitArima, level=c(95), h=7*12)
autoplot(forecastRegular) + autolayer(log(testRegular))
forecastRegular2<-forecast(fitArima2, level=c(95), h=7*12)
autoplot(forecastRegular2) + autolayer(log(testRegular))


# prophet con diesel
library(prophet)
library(zoo)
dataFrame<-data.frame(ds=as.Date(as.yearmon(time(trainRegular))), y=as.matrix(trainRegular))
testDataFrame<-data.frame(ds=as.Date(as.yearmon(time(testRegular))), y=as.matrix(testRegular))
fitProphet<-prophet(dataFrame, yearly.seasonality = T, weekly.seasonality = T)
future<-make_future_dataframe(fitProphet, periods = 7 * 12, freq = "month", include_history = T)
pred<-predict(fitProphet, future)
pred<-pred[, c("ds", "yhat", "yhat_lower", "yhat_upper")]
plot(fitProphet, pred)
prediction<-tail(pred, 61)
prediction$y<-testDataFrame$y
ggplot(prediction, aes(x=ds, y=yhat)) +
  geom_line(size=1, color="blue", alpha=0.8) +
  geom_ribbon(aes(ymin=yhat_lower, ymax=yhat_upper), fill="blue", alpha=0.2) +
  geom_line(data=prediction, aes(x=ds, y=y), color="red")


# ------------------------------------------------------------------------------------------------


# 3. Superior
dataSuperior<-ts(datosImp$GasSuperior, start=c(2001, 01), end=c(2020, 03), 12)
start(dataSuperior)
end(dataSuperior)
frequency(dataSuperior)
plot(dataSuperior)
abline(reg=lm(dataSuperior~time(dataSuperior)), col=c("red"))
decSuperior<-decompose(dataSuperior)
plot(decSuperior)
plot(decSuperior$trend)
plot(decSuperior$seasonal)

# division en train / test
trainSuperior<-head(dataSuperior, round(length(dataSuperior) * 0.7))
testSuperior<-tail(dataSuperior, round(length(dataSuperior) * 0.3))
# h se utiliza al generar el modelo y predecir
h<-length(trainSuperior) - length(testSuperior)

# transformacion logaritmica
logSuperior<-log(trainSuperior)
plot(decompose(logSuperior))

# Dickey Fuller Test
# p > 0.05
adfTest(trainSuperior)

# raices unitarias
# p > 0.05
unitrootTest(trainSuperior)

# diferenciacion, es necesario
# d = 1
adfTest(diff(trainSuperior))
unitrootTest(diff(trainSuperior))

# intentar identificar parametros p y q
# q = 3
acf(logSuperior, 100, na.action = na.pass)
# p = 2
pacf(logSuperior, 100, na.action = na.pass)

# Para tener una idea de los parametros estacionales.
Acf(diff(logSuperior), 36)
Pacf(diff(logSuperior), 36)

# arima
fitArima<-arima(log(trainSuperior), order=c(2, 1, 3), seasonal=c(1, 1, 0))
fitArima2<-arima(log(trainSuperior), order=c(2, 1, 3), seasonal=c(0, 1, 1))

# ajustar n.ahead al numero de meses que tengamos en conjunto de prueba
pred<-predict(fitArima, 7*12)
ts.plot(trainSuperior, exp(pred$pred), log="y", lty=c(1,3))
pred2<-predict(fitArima2, 7*12)
ts.plot(trainSuperior, exp(pred2$pred), log="y", lty=c(1,3))

# h es el numero de meses a predecir
forecastSuperior<-forecast(fitArima, level=c(95), h=7*12)
autoplot(forecastSuperior) + autolayer(log(testSuperior))
forecastSuperior2<-forecast(fitArima2, level=c(95), h=7*12)
autoplot(forecastSuperior2) + autolayer(log(testSuperior))


# prophet con Superior
library(prophet)
library(zoo)
dataFrame<-data.frame(ds=as.Date(as.yearmon(time(trainSuperior))), y=as.matrix(trainSuperior))
testDataFrame<-data.frame(ds=as.Date(as.yearmon(time(testSuperior))), y=as.matrix(testSuperior))
fitProphet<-prophet(dataFrame, yearly.seasonality = T, weekly.seasonality = T)
future<-make_future_dataframe(fitProphet, periods = 7 * 12, freq = "month", include_history = T)
pred<-predict(fitProphet, future)
pred<-pred[, c("ds", "yhat", "yhat_lower", "yhat_upper")]
plot(fitProphet, pred)
prediction<-tail(pred, 61)
prediction$y<-testDataFrame$y
ggplot(prediction, aes(x=ds, y=yhat)) +
  geom_line(size=1, color="blue", alpha=0.8) +
  geom_ribbon(aes(ymin=yhat_lower, ymax=yhat_upper), fill="blue", alpha=0.2) +
  geom_line(data=prediction, aes(x=ds, y=y), color="red")
