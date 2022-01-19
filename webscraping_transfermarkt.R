#-------------------------------------------------------------------------------
# 1. Limpiar el workspace y fijar carpeta
rm(list=ls())
getwd()
setwd("C:/Users/PC-1/Desktop/Proyecto") #Fijar directorio personal
#-------------------------------------------------------------------------------
# 2. Cargamos librerias necesarias
library(rvest)
library(curl)
library(dplyr)
library(tidyverse)
#install.packages("remotes")
#remotes::install_github("cienciadedatos/datos")
library(datos)
#-------------------------------------------------------------------------------
# 3. Fijar URLs
#* En esta sección se puede cambiar las urls por la de otra liga
Años<-seq(1992,2020,1) #Periodo de interes

# 3.1 Valor de mercado
url1<-"https://www.transfermarkt.es/premier-league/startseite/wettbewerb/GB1"
urlf1<-c(1)
for (i in Años[1]:Años[length(Años)]){
  urlf1[i-Años[1]+1]=print(paste0(url1,"/plus/?saison_id=",i))
}
# 3.2 Tabla final de temporada
url2<-"https://www.transfermarkt.es/premier-league/tabelle/wettbewerb/GB1/saison_id/"
urlf2<-c(1)
for (i in Años[1]:Años[length(Años)]){
  urlf2[i-Años[1]+1]=print(paste0(url2,i))
}
# 3.3 Tabla mitad de temporada
url3<-"https://www.transfermarkt.es/premier-league/heimtabelle/wettbewerb/GB1/saison_id/"
urlf3<-c(1)
for (i in Años[1]:Años[length(Años)]){
  urlf3[i-Años[1]+1]=print(paste0(url3,i))
}
#* Nombres para guardar los resultados
nomb<-c(1)
for(i in 1:length(urlf1)){
  nomb[i]<-print(paste0('tabla',Años[i],'_',Años[i]+1,'.csv'))
}
#-------------------------------------------------------------------------------
# 4.Descarga de datos y arreglos
for (i in 1:length(urlf1)){
  
# 4.1 Cargamos tablas
  trmkt_html<-read_html(curl(print(urlf1[i]), handle = curl::new_handle("useragent" = "Mozilla/5.0")))
  
  trmktp_html<-read_html(curl(print(urlf2[i]), handle = curl::new_handle("useragent" = "Mozilla/5.0")))
  
  trmktpM_html<-read_html(curl(print(urlf3[i]), handle = curl::new_handle("useragent" = "Mozilla/5.0")))
  
# 4.2 Descarga como tabla
  
  tabla<-trmkt_html%>%
    html_table()
  tabla<-tabla[[4]]
  
  tpoints<-trmktp_html%>%
    html_table()
  tpoints<-tpoints[[4]]
  
  tpointsM<-trmktpM_html%>%
    html_table()
  tpointsM<-tpointsM[[4]]

# 4.3 Arreglo de tabla
  
  tabla<-tabla[-1,c(seq(2,8,1))]
  Puesto<-data.frame(Puesto=seq(1,nrow(tabla),1))
  tabla<-cbind(Puesto,tabla)
  names(tabla)<-c("Puesto","Club","Name","Equipo","Edad",
                  "Extranjeros","ø-Valor de mercado (mill. ???)",
                  "Valor de mercado total (mill. ???)")
  
  tpoints<-tpoints[,-2]
  names(tpoints)<-c("RnkT","Name","PartidosT","GanadoT","EmpatadoT",
                    "PerdidoT","GolesT","Dif.golT","PuntosT")
  
  tpointsM<-tpointsM[,-2]
  names(tpointsM)<-c("RnkM","Name","PartidosM","GanadoM","EmpatadoM",
                     "PerdidoM","GolesM","Dif.golM","PuntosM")
  
# 4.4 Arreglo de datos  
  tabla$`Valor de mercado total (mill. ???)`<-gsub(" mill. ???","",tabla$`Valor de mercado total (mill. ???)`)
  tabla$`Valor de mercado total (mill. ???)`<-gsub(",",".",tabla$`Valor de mercado total (mill. ???)`)
  tabla$`ø-Valor de mercado (mill. ???)`<-gsub(" mill. ???","",tabla$`ø-Valor de mercado (mill. ???)`)
  tabla$`ø-Valor de mercado (mill. ???)`<-gsub(",",".",tabla$`ø-Valor de mercado (mill. ???)`)
  tabla$Edad<-gsub(",",".",tabla$Edad)
  
  # Datos especiales: Valor de mercado total (mill. ???) y 
  # ø-Valor de mercado  (mill. ???)
  
  tabla$detect_VMT<-str_detect(tabla$`Valor de mercado total (mill. ???)`, " mil")
  tabla$detect_ø<-str_detect(tabla$`ø-Valor de mercado (mill. ???)`, " mil")
  
  tabla$`Valor de mercado total (mill. ???)`<-gsub(" mil","",tabla$`Valor de mercado total (mill. ???)`)
  tabla$`ø-Valor de mercado (mill. ???)`<-gsub(" mil","",tabla$`ø-Valor de mercado (mill. ???)`)
  
  # Convertimos estos datos a números
  tabla$`Valor de mercado total (mill. ???)`<-as.numeric(tabla$`Valor de mercado total (mill. ???)`)
  tabla$`ø-Valor de mercado (mill. ???)`<-as.numeric(tabla$`ø-Valor de mercado (mill. ???)`)
  tabla$Edad<-as.numeric(tabla$Edad)
  
  # Arreglamos el Valor del mercado total (mill. ???)
  for (j in 1:ncol(tabla)){
    if(tabla$detect_VMT[j] == TRUE){
      tabla$`Valor de mercado total (mill. ???)`[j]= tabla$`Valor de mercado total (mill. ???)`[j]*1000
    }else {
      tabla$`Valor de mercado total (mill. ???)`[j]= tabla$`Valor de mercado total (mill. ???)`[j]
    }
  }
  
  # Arreglamos el ø-Valor de mercado (mill. ???)
  for (j in 1:ncol(tabla)){
    if(tabla$detect_ø[j] == TRUE){
      tabla$`ø-Valor de mercado (mill. ???)`[j]= tabla$`ø-Valor de mercado (mill. ???)`[j]*1000
    }else {
      tabla$`ø-Valor de mercado (mill. ???)`[j]= tabla$`ø-Valor de mercado (mill. ???)`[j]
    }
  }
  tabla<-tabla[,c(seq(1,ncol(tabla)-2,1))]
  
# 4.5 Unir ambas tablas de datos: Tabla y Tponits

  final<-merge(x = tpoints, y = tpointsM, by=c("Name"))
  final<-merge(x = tabla, y = final, by=c("Name"))
  final$Año<-Años[i]
  write.csv(final,nomb[i],col.names = T,row.names = F)
}    
#-------------------------------------------------------------------------------
# 5. Base de datos final
rm(list=ls())
# 5.1 Abrir todos los archivos para crear una base conjunta de datos
files<-list.files(path=getwd(), pattern="*.csv")
myfiles<-lapply(files, read.csv, header = T)
Final<-do.call(rbind, myfiles)

# 5.2 Exportamos la base de datos final
write.csv(Final,"Final.csv",col.names = T,row.names = F)
#-------------------------------------------------------------------------------

