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


# Quitar comas
datosImp$GLP <- replaceCommas(datosImp$GLP)
datosImp$GasAviacion <- replaceCommas(datosImp$GasAviacion)
datosImp$GasSuperior <- replaceCommas(datosImp$GasSuperior)
datosImp$GasRegular <- replaceCommas(datosImp$GasRegular)
datosImp$Kerosina <- replaceCommas(datosImp$Kerosina)
datosImp$rTurboJet <- replaceCommas(datosImp$rTurboJet)
datosImp$Diesel <- replaceCommas(datosImp$Diesel)
datosImp$DieselLS <- replaceCommas(datosImp$DieselLS)
datosImp$DieselULS <- replaceCommas(datosImp$DieselULS)
datosImp$Bunker <- replaceCommas(datosImp$Bunker)
datosImp$Asfalto <- replaceCommas(datosImp$Asfalto)
datosImp$PetCoke <- replaceCommas(datosImp$PetCoke)
datosImp$AceitesLub <- replaceCommas(datosImp$AceitesLub)
datosImp$GrasasLub <- replaceCommas(datosImp$GrasasLub)
datosImp$Solventes <- replaceCommas(datosImp$Solventes)
datosImp$Naftas <- replaceCommas(datosImp$Naftas)
datosImp$Ceras <- replaceCommas(datosImp$Ceras)
datosImp$Butano <- replaceCommas(datosImp$Butano)
datosImp$PetroleoReconst <- replaceCommas(datosImp$PetroleoReconst)
datosImp$MTBE <- replaceCommas(datosImp$MTBE)
datosImp$Orimulsion <- replaceCommas(datosImp$Orimulsion)
datosImp$MezclasOleosas <- replaceCommas(datosImp$MezclasOleosas)
datosImp$Total <- replaceCommas(datosImp$Total)

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

# 
Acf(diff(logDiesel), 36)
Pacf(diff(logDiesel), 36)

# arima
fitArima<-arima(log(trainDiesel), order=c(2, 1, 3), seasonal=c(1, 1, 0))

# ajustar n.ahead al numero de meses que tengamos en conjunto de prueba
pred<-predict(fitArima, 7*12)
ts.plot(trainDiesel, exp^(pred$pred), log="y", lty=c(1,3))

# h es el numero de meses a predecir
forecastDiesel<-forecast(fitArima, level=c(95), h=7*12)
autoplot(forecastDiesel) + autolayer(log(testDiesel))

# prophet
library(prophet)

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
