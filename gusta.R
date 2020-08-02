library(fUnitRoots)
library(ggfortify)
library(tabulizer)
library(corrplot)
library(dplyr)
library(stringr)
library(lmtest)

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

for(i in names(datosImp)) {
  datosImp[[i]]<-replaceCommas(datosImp[[i]])
}
View(datosImp)
summary(datosImp)

for (col in 6:24) {
  hist( datosImp[,col], main=colnames(datosImp[col]), xlab=colnames(datosImp[col]) )
}


paraMes <- datosImp[,c(2,25)]
paraAnio <- datosImp[,c(1,25)]

paraMes$Mes <- as.factor(paraMes$Mes)
paraAnio$Anio <- as.factor(paraAnio$Anio)

meses <- aggregate(paraMes$Total, by=list(Category=paraMes$Mes), FUN=sum)
anios <- aggregate(paraAnio$Total, by=list(Category=paraAnio$Anio), FUN=sum)

colnames(meses) <- c("Mes", "Total")
colnames(anios) <- c("Año", "Total")

View(meses)
View(anios)







