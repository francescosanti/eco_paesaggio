### 

setwd("C:/lab")

# importazione dei dati
covid <- read.table("covid_agg.csv", header = T) # header=T per considerare la prima riga come intestazione dei campi
head(covid)

# plot con numero di casi per ogni paese
plot(covid$country, covid$cases)
# "las" argomento per posizionamento delle etichette sugli assi nel grafico, las sta per "labels"
# cambiando il valore di "las" si può rendere il grafico più leggibile
plot(covid$country, covid$cases, las = 0) # las=0 etichette parallele agli assi
plot(covid$country, covid$cases, las = 1) # las=1 etichette orizzontali
plot(covid$country, covid$cases, las = 2) # las=2 perpendicolari agli assi
plot(covid$country, covid$cases, las = 3) # las=3 etichette verticali


install.packages("ggplot2")
# caricamento libreria e dataset
library(ggplot2)
data(mpg)
head(mpg)


# per il comando 'ggplot' vanno specificate le componenti: dati, aes (sta per "estetica"), geometria
ggplot(mpg, aes(x = displ, y = hwy)) + geom_point() # all'interno di aes si indicano i campi del dataset da utilizzare come coordinate
                                                    # per disporre i punti spazialmente

ggplot(mpg, aes(x = displ, y = hwy)) + geom_line()  # con 'geom_point()', 'geom_line()', i siti vengono rappresentati rispettivamente come
                                                    # punti o uniti da una linea; esistono anche altre possibilità, ad esempio 
                                                    # 'geom_polygon()', in questo caso non funzionale


# rappresentazione grafica del numero di casi di covid19 per ogni paese, con dimensione del punto proporzionale al valore dei casi (size=cases)
ggplot(covid, aes(x = lon, y = lat, size = cases)) + geom_point()

attach(covid)

# installazione della libreria spatstat per analisi spaziali
install.packages("spatsat")
library(spatstat)

# creazione del point pattern tramite la funzione ppp
covids <- ppp(lon, lat, c(-180, 180), c(-90, 90))

# con la funzione 'density' si va a stimare la diffusione del fenomeno analizzato, considerando la densità dei punti di partenza
d <- density(covids)

plot(d) # rappresentazione grafica; a seconda del colore di un pixel, ci si aspetta un numero più o meno alto di punti con casi accertati
        # di covid19 (il numero di casi non è contemplato)
points(covids) # aggiunta dei punti relativi ai vari paesi


# per salvare gli oggetti attualmente caricati
save.image("C:/lab/point_pattern.RData")

# caricare il workspace precedentemente salvato
setwd("C:/lab") #impostazione della working directory
load("point_pattern.RData") # caricamento del file "rdata" desiderato

ls() # mostra gli oggetti attualmente caricati


# creazione di una palette
# scegliere i colori da utilizzare nella mappa
cl <- colorRampPalette(c('yellow', 'orange', 'red')) (100) # il numero tra parentesi indica il numero di gradazioni fra i vari colori
                                                           # questa funzione è disponibile in alcune librerie, ad esempio in 'spatstat'
plot(d, col = cl) # assegnare al grafico i colori della palette creata
points(covids)

library(rgdal) # la libreria 'gdal' permette a qualsiasi software di leggere dati geospaziali, in R si chiama 'rgdal'
coastlines <- readOGR("ne_10m_coastline.shp") # funzione per leggere file di tipo geografico, in questo caso uno shapefile

# aggiunta al grafico di densità delle linee di coste mondiali
plot(coastlines, add = T) # con add=T si aggiunge l'oggetto all'ultimo grafico creato

# exercise: cambiare colori alla mappa di densità
cl2 <- colorRampPalette(c('white', 'blue', 'green', 'red', 'orange', 'yellow')) (150)
plot(d, col = cl2)
plot(coastlines, add = T, col = "grey")



# exercise: creare mappa di densità con dati covid
library(spatstat)
library(rgdal) # for the coastlines


save.image("C:/lab/point_pattern.RData")

setwd("C:/lab")
load("point_pattern.RData")
ls()

cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(d, col=cl5, main="density")
points(covids)
library(rgdal)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add = T)


# interpolazione

head(covid)
marks(covids) <- covid$cases # funzione 'marks' è in 'spatstat'
# 'marks' associa i valori del campo scelto ai punti di un point pattern (di default il primo del campo al primo del pp, il secondo al secondo ecc)
s <- Smooth(covids) # interpolazione spaziale del numero di casi di covid19 nel mondo, a partire dal dataset 'covid'
# 'Smooth' esegue l'interpolazione
plot(s) # analogamente alla mappa di densità, il colore di un pixel rappresenta il valore atteso della variabile scelta
 
plot(coastlines, add = T)
points(covids)



##### mappa finale
par(mfrow = c(2, 1)) # la schermata grafica viene suddivisa in due righe e una colonna
                     # in questo modo si possono comparare la mappa di densità e di interpolazione del numero di casi
 
# densità
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(d, col = cl5, main= "density")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add = T)

# interpolazione del numero di casi
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(s, col = cl5, main = "estimate of cases")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add = T)

# Exercise: plot multiframe di densità e interpolazione uno acacnto all'alto
par(mfrow = c(1, 2)) # in questo caso la schermata grafica è suddivisa in una riga e due colonne 

plot(dT, main = "Density of points")
points(Tesippp, col = "green")

plot(interpol, main = "Estimate of species richness")
points(Tesippp, col = "green")


###### Analisi dati di tesi svolta a San Marino
setwd("C:/lab")
library(spatstat)
 
load("Tesi.RData")
ls()

head(Tesi) # per avere un'idea di com'è fatto l'oggetto
           # sono presenti vari campi relativi a variabili misurate nei siti di campionamento
attach(Tesi)
 
summary(Tesi) # statistiche principali dei campi di 'Tesi'

# i campi 'Longitude' e 'Latitude' indicano le coordinate dei siti di campionamento
# longitudine varia da 12.42 a 12.46
# latitudine varia da 43.91 a 43.94
# point pattern: x,y,c(xmin,xmax),c(ymin,ymax)
Tesippp <- ppp(Longitude, Latitude, c(12.41, 12.47), c(43.9, 43.95))
 
dT <- density(Tesippp)
plot(dT) # grafico di densità dei punti di campionamento, che risultano più concentrati nella parte centrale
points(Tesippp, col = "green") # aggiunta dei punti relativi ai siti di campionamento

colors() # elenco dei nomi di tutti i 657 colori disponbili su R


######### 

setwd("C:/lab")
load("sanmarino.RData")
library(spatstat)

ls()
# dT = density map, Tesi = dataset, Tesippp = point pattern

plot(dT)
points(Tesippp, col="green")

head(Tesi)

marks(Tesippp) <- Tesi$Species_richness # consideriamo la ricchezza specifica nei plot per effettuare l'inerpolazione
interpol <- Smooth(Tesippp) # interpolazione dei dati di ricchezza specifica

plot(interpol) # rappresentazione grafica dell'interpolazione
               # i plot a nord sono i meno ricchi di specie, al centro-ovest e ad est le zone con ricchezza specifica maggiore
points(Tesippp, col="green")

library(rgdal)

sanmarino <- readOGR("San_Marino.shp") # caricamento dello shapefile relativo al confine del territorio di San Marino

plot(sanmarino, add = T) # il confine è aggiunto alla precedente mappa di interpolazione
points(Tesippp, col = "green")


# Exercise: plot multiframe di densità e interpolazione
par(mfrow = c(2, 1))

plot(dT, main = "Density of points")
points(Tesippp, col = "green")

plot(interpol, main = "Estimate of species richness")
points(Tesippp, col = "green")



# Exercise: plot multiframe di densità e interpolazione uno accanto all'alto
par(mfrow = c(1, 2))

plot(dT, main = "Density of points")
points(Tesippp, col = "green")

plot(interpol, main = "Estimate of species richness")
points(Tesippp, col = "green")

