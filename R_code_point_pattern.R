setwd("C:/lab")

# importazione dei dati
covid <- read.table("covid_agg.csv",header=T)

head(covid)

plot(covid$country,covid$cases)
# "las" argomento per posizionamento delle etichette sugli assi nel grafico, las sta per "labels"
plot(covid$country,covid$cases,las=0) #las=0 etichette parallele agli assi
plot(covid$country,covid$cases,las=1) #las=1 etichette orizzontali
plot(covid$country,covid$cases,las=2) #las=2 perpendicolari agli assi
plot(covid$country,covid$cases,las=3) #las=3 etichette verticali

library(ggplot2)
data(mpg)
head(mpg)


# componenti: dati, aes (sta per "estetica"), geometria
ggplot(mpg,aes(x=displ,y=hwy))+geom_point() 
ggplot(mpg,aes(x=displ,y=hwy))+geom_line() 

ggplot(covid,aes(x=lon,y=lat,size=cases)) + geom_point()

attach(covid)
# con la funzione ppp si crea il point pattern
covids <- ppp(lon, lat, c(-180,180), c(-90,90))
d<-density(covids)

plot(d) #viene creato un plot di densità dei punti, non prende in considerazione il numero di casi
points(covids) #aggiunge i punti relativi ai vari paesi

# palette
# scegliere colori da utilizzare nella mappa
cl<- colorRampPalette(c('yellow','orange','red')) (100) #il numero tra parentesi indica il numero di gradazioni fra i vari colori, questa funzione è disponibile in alcune librerie, ad esempio spatstat
plot(d,col=cl)
points(covids)

library(rgdal) # la libreria 'gdal' permette a qualsiasi software di leggere dati geospaziali, in R si chiama rgdal
coastlines<-readOGR("ne_10m_coastline.shp") #funzione per leggere file di tipo geografico
plot(coastlines,add=T) # con add=T si aggiungono le linee all'ultimo grafico creato

# exercise: cambiare colori alla mappa di densità
cl2<-colorRampPalette(c('white','blue','green','red','orange','yellow')) (150)
plot(d,col=cl2)
plot(coastlines,add=T,col="grey")



# exercise: creare mappa di densità con dati covid
library(spatstat)
library(rgdal) # for the coastlines


setwd("C:/lab")
load("point_pattern.RData")
ls()

cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(d, col=cl5, main="density")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)


# interpolazione

head(covid)
marks(covids)<-covid$cases #funzione marks è in spatstat


###### San Marino
 # setwd("C:/lab")
 # library(spatstat)
 
 head(Tesi)
 attach(Tesi)
 
 summary(Tesi)
 
 # x varia da 12.42 a 12.46
 # y varia da 43.91 a 43.94
 # point pattern: x,y,c(xmin,xmax),c(ymin,ymax)
 Tesippp<-ppp(Longitude,Latitude,c(12.41,12.47),c(43.9,43.95))
 
 dT<-density(Tesippp)
 plot(dT)
 points(Tesippp,col="green")
