# HDT #2 - Series de Tiempo
# Rodrigo Zea - 17058
# Gustavo de Leon - 17085
# Sebastian Arriola - 11463

library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)
library(tabulizer)
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


