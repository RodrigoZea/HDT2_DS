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

# Limpieza de datos
datosImp <- datosImp[-c(46, 96, 146, 196), ]

# Funcion para quitar comas
replaceCommas<-function(x){
  x<-as.numeric(gsub("\\,", "", x))
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


as.numeric(unlist(datosImp))

# Mapa de correlacion
corrMatrix<-cor(as.numeric(datosImp[,-1],use = "pairwise.complete.obs"))
corrplot(corrMatrix)




