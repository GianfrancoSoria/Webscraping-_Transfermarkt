
#----
#Limpiar el workspace
rm(list=ls())

#Obtener directorio de trabajo
getwd()
setwd("C:/Users/PC-1/Desktop/Proyecto")


#Cargamos librerias necesarias
#install.packages("rvest")
library(rvest)

#Buscamos pagina que deseamos descargar
url<-"https://www.transfermarkt.es/premier-league/startseite/wettbewerb/GB1"
#####

#(1) Descargamos toda la pagina
#----
trmkt_html<-read_html(url)
#####

#(2) Ubicacion de etiquetas y extracción de datos
#----
clubes<-trmkt_html%>%
  html_nodes("div.responsive-table")%>%
  html_nodes("a")%>%
  html_text()

clubes<-trmkt_html%>%
  html_nodes(".items")%>%
  html_nodes("tr")%>%
  html_nodes("td")%>%
  html_text()

clubes<-trmkt_html%>%
  html_nodes("a")%>%
  html_text()

#####
# Descargando como tabla
library(dplyr)
tabla21_22<-trmkt_html%>%
  html_table()
tabla21_22<-tabla21_22[[4]]
#####
# Arreglando tabla
tabla21_22<-tabla21_22[-1,c(seq(2,8,1))]
names(tabla21_22)<-c("Club","Name","Equipo","Edad",
                     "Extranjeros","ø-Valor de mercado",
                     "Valor de mercado total")

#-------------------------------------------------------------------------------
library(rvest)
library(dplyr)
library(tidyverse)
#install.packages("remotes")
#remotes::install_github("cienciadedatos/datos")
library(datos)

url1<-"https://www.transfermarkt.es/premier-league/startseite/wettbewerb/GB1"
urlf1<-c(1)
Años<-seq(1992,2020,1)
for (i in Años[1]:Años[length(Años)]){
  urlf1[i-Años[1]+1]=print(paste0(url1,"/plus/?saison_id=",i))
}

url2<-"https://www.transfermarkt.es/premier-league/tabelle/wettbewerb/GB1/saison_id/"
urlf2<-c(1)

for (i in Años[1]:Años[length(Años)]){
  urlf2[i-Años[1]+1]=print(paste0(url2,i))
}

url3<-"https://www.transfermarkt.es/premier-league/heimtabelle/wettbewerb/GB1/saison_id/"
urlf3<-c(1)

for (i in Años[1]:Años[length(Años)]){
  urlf3[i-Años[1]+1]=print(paste0(url3,i))
}

nomb<-c(1)
for(i in 1:length(urlf1)){
  nomb[i]<-print(paste0('tabla',Años[i],'_',Años[i]+1,'.csv'))
}

################################################################################

#-------------------------------------------------------------------------------
for (i in 1:length(urlf1)){
  
  #Cargamos tablas
  
  trmkt_html<-read_html(print(urlf1[i]))
  
  trmktp_html<-read_html(print(urlf2[i]))
  
  trmktpM_html<-read_html(print(urlf3[i]))
  
  # Descargando como tabla
  
  tabla<-trmkt_html%>%
  html_table()
  tabla<-tabla[[4]]
  
  tpoints<-trmktp_html%>%
  html_table()
  tpoints<-tpoints[[4]]
  
  tpointsM<-trmktpM_html%>%
    html_table()
  tpointsM<-tpointsM[[4]]

# Arreglando tabla
  
  tabla<-tabla[-1,c(seq(2,8,1))]
  Puesto<-data.frame(Puesto=seq(1,nrow(tabla),1))
  tabla<-cbind(Puesto,tabla)
  names(tabla)<-c("Puesto","Club","Name","Equipo","Edad",
                     "Extranjeros","ø-Valor de mercado (mill. €)",
                     "Valor de mercado total (mill. €)")
  
  tpoints<-tpoints[,-2]
  names(tpoints)<-c("RnkT","Name","PartidosT","GanadoT","EmpatadoT",
                    "PerdidoT","GolesT","Dif.golT","PuntosT")
  
  tpointsM<-tpointsM[,-2]
  names(tpointsM)<-c("RnkM","Name","PartidosM","GanadoM","EmpatadoM",
                    "PerdidoM","GolesM","Dif.golM","PuntosM")
  
# Arreglamos datos  
  tabla$`Valor de mercado total (mill. €)`<-gsub(" mill. €","",tabla$`Valor de mercado total (mill. €)`)
  tabla$`Valor de mercado total (mill. €)`<-gsub(",",".",tabla$`Valor de mercado total (mill. €)`)
  tabla$`ø-Valor de mercado (mill. €)`<-gsub(" mill. €","",tabla$`ø-Valor de mercado (mill. €)`)
  tabla$`ø-Valor de mercado (mill. €)`<-gsub(",",".",tabla$`ø-Valor de mercado (mill. €)`)
  tabla$Edad<-gsub(",",".",tabla$Edad)
  
  # Datos especiales: Valor de mercado total (mill. €) y 
  # ø-Valor de mercado  (mill. €)
  
  tabla$detect_VMT<-str_detect(tabla$`Valor de mercado total (mill. €)`, " mil")
  tabla$detect_ø<-str_detect(tabla$`ø-Valor de mercado (mill. €)`, " mil")
  
  tabla$`Valor de mercado total (mill. €)`<-gsub(" mil","",tabla$`Valor de mercado total (mill. €)`)
  tabla$`ø-Valor de mercado (mill. €)`<-gsub(" mil","",tabla$`ø-Valor de mercado (mill. €)`)
  
  #Convertimos estos datos a números
  tabla$`Valor de mercado total (mill. €)`<-as.numeric(tabla$`Valor de mercado total (mill. €)`)
  tabla$`ø-Valor de mercado (mill. €)`<-as.numeric(tabla$`ø-Valor de mercado (mill. €)`)
  tabla$Edad<-as.numeric(tabla$Edad)
  
  #Usamos
    
    # Arreglamos el Valor del mercado total (mill. €)
  for (j in 1:ncol(tabla)){
    if(tabla$detect_VMT[j] == TRUE){
      tabla$`Valor de mercado total (mill. €)`[j]= tabla$`Valor de mercado total (mill. €)`[j]*1000
    }else {
      tabla$`Valor de mercado total (mill. €)`[j]= tabla$`Valor de mercado total (mill. €)`[j]
    }
  }
  
    # Arreglamos el ø-Valor de mercado (mill. €)
  for (j in 1:ncol(tabla)){
    if(tabla$detect_ø[j] == TRUE){
      tabla$`ø-Valor de mercado (mill. €)`[j]= tabla$`ø-Valor de mercado (mill. €)`[j]*1000
    }else {
      tabla$`ø-Valor de mercado (mill. €)`[j]= tabla$`ø-Valor de mercado (mill. €)`[j]
    }
  }
  tabla<-tabla[,c(seq(1,ncol(tabla)-2,1))]
  
# Unir ambas tablas de datos: Tabla y Tponits
  
  final<-merge(x = tpoints, y = tpointsM, by=c("Name"))
  final<-merge(x = tabla, y = final, by=c("Name"))
  final$Año<-Años[i]
  write.csv(final,nomb[i],col.names = T,row.names = F)
}  
################################################################################  


rm(list=ls())

# Base de datos final
#-------------------------------------------------------------------------------

# Abrir todos los archivos para crear una base conjunta de datos
files<-list.files(path=getwd(), pattern="*.csv")
myfiles<-lapply(files, read.csv, header = T)
Final<-do.call(rbind, myfiles)

# Exportamos la base de datos final
write.csv(Final,"Final.csv",col.names = T,row.names = F)

################################################################################  

#-------------------------------------------------------------------------------
library(ggplot2)
library(plotly)

Apariciones<-matrix(table(Final$Name))
Equipo<-rownames(as.matrix(table(Final$Name)))
tabla1<-data.frame(Equipo=Equipo,Apariciones=Apariciones)
tabla1<-tabla1[order(tabla1$Apariciones,decreasing = TRUE), ]

#Gráfico 1
png(filename = "Gráfico 1.png", width = 400, height = 380)
ggplot(data = tabla1,
       mapping = aes(x = tabla1$Apariciones))+
  geom_histogram(aes(y =..count..),
                 bins = 30,
                 position = 'identity',
                 fill="red")+
  ggtitle ("Gráfico 1.")+
  labs(x = "Apariciones",y = "N° de equipos")+
  scale_x_continuous(breaks = seq(0,max(tabla1$Apariciones),by=4))+
  theme_bw()
dev.off()

#Gráfico 2
png(filename = "Gráfico 2.png", width = 400, height = 380)
grafico2<-ggplot(data = Final,aes(y=RnkT,x=Año, col=Name))+
  geom_point(aes(size=PuntosT))+
  theme_bw()
ggplotly(grafico2)
dev.off()
plot(Final$RnkT)


unique(Final$Name)
summary(tabla1)
dim(tabla1)
################################################################################  