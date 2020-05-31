# R_code_exam.r

# Copernicus data: https://land.copernicus.vgt.vito.be/PDF/portal/Application.html


# 1. R_code_first.r	
# 2. R_code_spatial.r	
# 3. R_code_spatial2.r
# 4. R_code_point_pattern	
# 5. R_code_teleril.r	
# 6. R_code_landcover.r	
# 7. R_code_multitemp.r	
# 8. R_code_multitemp_NO2.r	
# 9. R_code_snow.r	
# 10. R_code_patches.r

#############################################################################
#############################################################################

# primo codice R
# FS  dopo il cancelletto (#) si può scrivere quello che si vuole, non viene eseguito come comando

install.packages("sp") # FS  comando per scaricare pacchetti -> contengono funzioni specifiche per ambiti
library(sp) # FS  caricare un pacchetto precedentemente installato
require(sp) # FS  altro modo per caricare pacchetto

data("meuse")  # FS  richiamo del database 'meuse' inserito nella libreria 'sp'
head(meuse)    # FS  visualizzo le prime 6 righe del database per avere un'idea di come è costituito l'oggetto
names(meuse)   # FS  nomi delle variabili
summary(meuse) # FS  riporta le statistiche di base per le variabili di meuse

pairs(meuse) # FS  grafici a coppie fra tutte le variabili
pairs(~ cadmium + copper + lead, data = meuse) # FS  grafici a coppie fra le tre variabili selezionate
# FS  il simbolo ~ spesso sta per "uguale" in R

pairs(meuse[, 3:6]) # FS  anzichè scrivere varie colonne prendo subset del database meuse
pairs(meuse[, 3:6],
      col = "orange",        # FS  scelgo il colore dei simboli
      pch=19,                # FS  pch sta per "point character", scelgo tipo di simbolo
      cex = 2,               # FS  cex sta per "character exageration", scelgo la grandezza del simbolo (di base cex=1)
      main = "Primo pairs")  # FS  titolo del grafico


panel.correlations <- function(x, y, digits=1, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r1=cor(x,y,use="pairwise.complete.obs")
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  txt <- format(c(r1, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.9/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * r)
}
# FS  funzione per calcolare la correlazione fra due variabili


panel.smoothing <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                             cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
          col = 1, ...)
}
# FS  smoothing fa una sorta di regressione fra due variabili



panel.histograms <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}
# FS  funzione per creare istogramma di una variabile


# FS  grafici a coppie fra le quattro variabili selezionate, in cui vengono mostrati anche coefficiente di correlazione
# FS  fra le variabili e istogramma delle singole variabili, utilizzando le funzioni precedentemente create
pairs(meuse[, 3:6], lower.panel = panel.correlations, upper.panel = panel.smoothing, diag.panel = panel.histograms)
# FS  lower.panel è la parte sotto la diagonale
# FS  upper.panel è la parte sopra la diagonale
# FS  diag.panel è la diagonale

pairs(meuse[, 3:6], lower.panel = panel.smoothing, upper.panel = panel.correlations, diag.panel = panel.histograms)
# FS  correlazione e interpolazione invertite di posto rispetto alla diagonale

# FS  plot fra due variabili
plot(meuse$cadmium, meuse$copper)
attach(meuse) # FS  permette di richimare i campi dell'oggetto 'meuse' senza dover richiamare l'oggetto stesso (non serve più 'meuse$')
plot(cadmium, copper)
plot(cadmium, copper, pch=17, col = "green", main = "Primo plot", xlab = "Cadmio", ylab = "Rame")


#######################################################
#######################################################

# 2 r code spatial
#### analisi dati spaziali 24/03/20

# FS  carico libreria e dati
library(sp)
data(meuse)

head(meuse)

# FS  plot di cadmio e piombo
# FS  allegare dataframe
attach(meuse)

plot(cadmium, lead, col = "red", pch = 19, cex = 2)

# exercise: plot di copper e zinc con simbolo triangolo (pch=17) e colore verde
plot(copper, zinc, col = "green", pch = 17, cex = 2, xlab = "rame", ylab = "zinco")

# FS  multiframe o multipanel (inserire più grafici in una finestra)
par(mfrow = c(1, 2)) # FS  divide lo schermo in due colonne e una riga
plot(cadmium, lead, col = "red", pch = 19, cex = 2)
plot(copper, zinc, col = "green", pch = 17, cex = 2, xlab = "rame", ylab = "zinco")

# FS  inversione di grafici riga/colonna in colonna/riga
par(mfrow = c(2, 1))
plot(cadmium, lead, col = "red", pch = 19, cex = 2)
plot(copper, zinc, col = "green", pch = 17, cex = 2, xlab = "rame", ylab = "zinco")

# FS  multiframe automatico
install.packages("GGally")
library(GGally)
ggpairs(meuse[, 3:6]) # FS  in diagonale distribuzione della frequenza dei dati della variabile,
                      # FS  nella parte bassa la distribuzione dei punti di due variabili
                      # FS  nella parte alta la correlazione fra due variabili

# spatial
library(sp)
head(meuse)

coordinates(meuse) = ~ x + y # FS  far capire al software che le colonne x e y sono da interpretare come coordinate
plot(meuse)

# FS  creare grafico spaziale con funzione spplot
spplot(meuse, "zinc") # FS  crea distribuzione spaziale di variabile zinc,
                      # FS  i siti sono colorati in base al valore della variabile selezionata

### continuazione analisi spaziale  25/03/20
names(meuse) # FS  visualizzazione dei nomi delle colonne del dataframe
spplot(meuse, "copper")

bubble(meuse, "zinc") # FS  dà rappresentazione spaziale analoga a quella di spplot,
                      # FS  i punti hanno dimensione proporzionale al valore della variabile

bubble(meuse, "copper", col = "red")

# formainiferi (Sofia), carbon capture (Marco)
# creazione di array
foram <- c(12, 20, 35, 55, 67, 80) # FS  la lettera c sta per il termine inglese 'concatenate'
carbon <- c(5, 15, 30, 70, 85, 99) # FS  c() si utilizza quando si deve specificare più di un oggetto
plot(foram, carbon, pch = 19, col = "green", cex = 2)


#######################################################
#######################################################

# R code point pattern
### Analisi di dati spaziali

setwd("C:/lab")

# FS  importazione dei dati
covid <- read.table("covid_agg.csv", header = T) # header=T per considerare la prima riga come intestazione dei campi
head(covid)

# FS  plot con numero di casi per ogni paese
plot(covid$country, covid$cases)
# FS  "las" argomento per posizionamento delle etichette sugli assi nel grafico, las sta per "labels"
# FS  cambiando il valore di "las" si può rendere il grafico più leggibile
plot(covid$country, covid$cases, las = 0) # FS  las=0 etichette parallele agli assi
plot(covid$country, covid$cases, las = 1) # FS  las=1 etichette orizzontali
plot(covid$country, covid$cases, las = 2) # FS  las=2 perpendicolari agli assi
plot(covid$country, covid$cases, las = 3) # FS  las=3 etichette verticali


install.packages("ggplot2")
# FS  caricamento libreria e dataset
library(ggplot2)
data(mpg)
head(mpg)


# FS  per il comando 'ggplot' vanno specificate le componenti: dati, aes (sta per "estetica"), geometria
ggplot(mpg, aes(x = displ, y = hwy)) + geom_point() # FS  all'interno di aes si indicano i campi del dataset da utilizzare come coordinate
                                                    # FS  per disporre i punti spazialmente

ggplot(mpg, aes(x = displ, y = hwy)) + geom_line()  # FS  con 'geom_point()', 'geom_line()', i siti vengono rappresentati rispettivamente come
                                                    # FS  punti o uniti da una linea; esistono anche altre possibilità, ad esempio 
                                                    # FS  'geom_polygon()', in questo caso non funzionale


# FS  rappresentazione grafica del numero di casi di covid19 per ogni paese, con dimensione del punto proporzionale al valore dei casi (size=cases)
ggplot(covid, aes(x = lon, y = lat, size = cases)) + geom_point()

attach(covid)

# FS  installazione della libreria spatstat per analisi spaziali
install.packages("spatsat")
library(spatstat)

# FS  creazione del point pattern tramite la funzione ppp
covids <- ppp(lon, lat, c(-180, 180), c(-90, 90))

# FS  con la funzione 'density' si va a stimare la diffusione del fenomeno analizzato, considerando la densità dei punti di partenza
d <- density(covids)

plot(d) # FS  rappresentazione grafica; a seconda del colore di un pixel, ci si aspetta un numero più o meno alto di punti con casi accertati
        # FS  di covid19 (il numero di casi non è contemplato)
points(covids) # FS  aggiunta dei punti relativi ai vari paesi


# FS  per salvare tutti gli oggetti attualmente caricati
save.image("C:/lab/point_pattern.RData")
# FS oppure
save(oggetto1, oggetto2, file = "nome_file.rdata") # FS  per salvare alcuni oggetti fra quelli esistenti


# FS  caricare il workspace precedentemente salvato
setwd("C:/lab") # FS  impostazione della working directory
load("point_pattern.RData") # FS  caricamento del file ".rdata" desiderato

ls() # FS  mostra gli oggetti attualmente caricati


# FS  creazione di una palette
# FS  scegliere i colori da utilizzare nella mappa
cl <- colorRampPalette(c('yellow', 'orange', 'red')) (100) # FS  il numero tra parentesi indica il numero di gradazioni fra i vari colori
                                                           # FS  questa funzione è disponibile in alcune librerie, ad esempio in 'spatstat'
plot(d, col = cl) # FS  assegnare al grafico i colori della palette creata
points(covids)

library(rgdal) # FS  la libreria 'gdal' permette a qualsiasi software di leggere dati geospaziali, in R si chiama 'rgdal'
coastlines <- readOGR("ne_10m_coastline.shp") # FS  funzione per leggere file di tipo geografico, in questo caso uno shapefile

# FS  aggiunta al grafico di densità delle linee di coste mondiali
plot(coastlines, add = T) # FS  con add=T si aggiunge l'oggetto all'ultimo grafico creato

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
marks(covids) <- covid$cases # FS  funzione 'marks' è in 'spatstat'
# FS  'marks' associa i valori del campo scelto ai punti di un point pattern (di default il primo del campo al primo del pp,
# FS  il secondo al secondo ecc)
s <- Smooth(covids) # FS  interpolazione spaziale del numero di casi di covid19 nel mondo, a partire dal dataset 'covid'
# FS  'Smooth' esegue l'interpolazione
plot(s) # FS  analogamente alla mappa di densità, il colore di un pixel rappresenta il valore atteso della variabile scelta
 
plot(coastlines, add = T)
points(covids)



##### mappa finale
par(mfrow = c(2, 1)) # FS  la schermata grafica viene suddivisa in due righe e una colonna
                     # FS  in questo modo si possono comparare la mappa di densità e di interpolazione del numero di casi
 
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



###### Analisi dati di tesi svolta a San Marino
setwd("C:/lab")
library(spatstat)
 
load("Tesi.RData")
ls()

head(Tesi) # FS  per avere un'idea di com'è fatto l'oggetto
           # FS  sono presenti vari campi relativi a variabili misurate nei siti di campionamento
attach(Tesi)
 
summary(Tesi) # FS  statistiche principali dei campi di 'Tesi'

# FS  i campi 'Longitude' e 'Latitude' indicano le coordinate dei siti di campionamento
# longitudine varia da 12.42 a 12.46
# latitudine varia da 43.91 a 43.94
# FS  per effettuare un point pattern sono richieste: x,y,c(xmin,xmax),c(ymin,ymax) (rispettivamente longitudine e latitudine dei punti
# FS  e limiti di variazione di longitudine e latitudine)
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



# Exercise: plot multiframe di densità e interpolazione uno accanto all'altro
par(mfrow = c(1, 2)) # in questo caso la schermata grafica è suddivisa in una riga e due colonne 

plot(dT, main = "Density of points")
points(Tesippp, col = "green")

plot(interpol, main = "Estimate of species richness")
points(Tesippp, col = "green")



###############################################################
###############################################################

# R code teleril
# Codice R per le immagini satellitari

install.packages("raster")
library(raster)
 
setwd("C:/lab_eco_pae")
 
p224r63_2011 <- brick("p224r63_2011_masked.grd")
# il comando brick permette di importare file raster
# immagine acquisita dal satellite Landsat nel 2011, relativa all'incrocio fra path 224 e row 63

plot(p224r63_2011) # vengono mostrate tutte le bande relative alla riflettanza nelle diverse lunghezze d'onda
# B1: blue
# B2: green
# B3: red
# B4: near infrared (nir)
# B5: medium infrared
# B6: thermal infrared
# B7: medium infrared


# salvo il workspace
save.image("C:/lab/teleril.RData")


# giorno 2
setwd("C:/lab/")
load("teleril.RData")

ls()
# "p224r63_2011"

library(raster)
plot(p224r63_2011)


# cambio dei colori
cl <- colorRampPalette(c('black', 'grey', 'light grey'))(100)
plot(p224r63_2011, col = cl) # con col=cl i colori dei grafici saranno quelli della palette creata

cllow <- colorRampPalette(c('black', 'grey', 'light grey'))(5) # palette con meno gradazioni di colori
plot(p224r63_2011, col = cllow) # il dettaglio dell'immagine è molto minore rispetto a prima

names(p224r63_2011)
# "B1_sre" "B2_sre" "B3_sre" "B4_sre" "B5_sre" "B6_bt" "B7_sre"

# riflettanza nel blu
clb <- colorRampPalette(c('dark blue', 'blue', 'light blue'))(100)
plot(p224r63_2011$B1_sre, col = clb)
# la funzione attach(dataframe) non vale con il pacchetto 'raster', quindi si utilizza il simbolo '$' per richiamare i campi dell'oggetto

# Exercise: plottare la banda del vicino infrarosso vicino con 
# colorRampPalette che varia dal rosso, all'arancione, al giallo
clnir <- colorRampPalette(c('red', 'orange', 'yellow'))(100)
plot(p224r63_2011$B4_sre, col = clnir)


# multiframe
par(mfrow = c(2, 2)) # due righe e due colonne

# blue
clb <- colorRampPalette(c('dark blue', 'blue', 'light blue'))(100)
plot(p224r63_2011$B1_sre, col = clb)

# green
clg <- colorRampPalette(c('dark green', 'green', 'light green'))(100) #
plot(p224r63_2011$B2_sre, col = clg)

# red
clr <- colorRampPalette(c('dark red', 'red', 'pink'))(100) #
plot(p224r63_2011$B3_sre, col = clr)

# nir
clnir <- colorRampPalette(c('red', 'orange', 'yellow'))(100) #
plot(p224r63_2011$B4_sre, col = clnir)
# valori alti di infrarosso sono molto presenti e si trovano in corrispondenza di bassi valori di radiazione rossa e blu,
# quindi l'immagine è relativa ad una zona vegetata

dev.off() # chiude la finestra grafica, ripristinando le impostazioni grafiche di base (schermata unica)


# natural colours
# il computer visualizza colori come sovrapposizione di gradazioni di rosso, verde e blue
# per visualizzazre l'immagine come la vedono i nostri occhi bisogna sovrapporre le bande del rosso, del verde e del blu

plotRGB(p224r63_2011, r = 3, g = 2, b = 1)
 
plotRGB(p224r63_2011, r = 3, g = 2, b = 1, stretch = "Lin") # stretch="Lin" permette di ampliare la gamma di colori
                                                            # e rendere l'immagine visibile
  
# aggiunta della banda infrarossa, che permette di rendere l'immagine più leggibile (fa capire dov'è la vegetazione)
# bisogna eliminare una delle tre di prima, per esempio la blu e far scalare i valori delle altre
plotRGB(p224r63_2011, r = 4, g = 3, b = 2, stretch = "Lin")
# il colore rosso corrisponde ad un'alta riflettanza della componente infrarossa

# creare un pdf
pdf("primografico.pdf")
# le immagini eseguite successivamente vengono inserite nel pdf
plotRGB(p224r63_2011, r = 4, g = 3, b = 2, stretch = "Lin")
dev.off() # il pdf è creato, è stato salvato nell cartella su cui è impostata la working directory

# multiframe
par(mfrow = c(2, 1))
plotRGB(p224r63_2011, r = 3, g = 2, b = 1, stretch = "Lin")
plotRGB(p224r63_2011, r = 4, g = 3, b = 2, stretch = "Lin")
# affiancando le immagini con colori naturali e banda infrarossa, queste possono essere facilmente confontate
# si può perciò capire meglio dov'è la vegetazione (per esempio)
dev.off()
  
# nir nella componente red
plotRGB(p224r63_2011, r = 4, g = 3, b = 2, stretch = "Lin")
# nir nella componente green
plotRGB(p224r63_2011, r = 3, g = 4, b = 2, stretch = "Lin")
# nir nella componente blu
plotRGB(p224r63_2011, r = 3, g = 2, b = 4, stretch = "Lin")
# a seconda della banda su cui è montata la radiazione infrarossa, i pixel con alti valori di essa assumeranno
# colorazione rispettivamente rossa, blu e verde


library(raster)
setwd("C:/lab")
load("teleril.RData")
  
p224r63_1988 <- brick("p224r63_1988_masked.grd") # importazione dell'immagine del 1988 della stessa area già analizzata per il 2011
plot(p224r63_1988)

# multiframe # rappresentazione compatta delle 4 bande di interesse
par(mfrow=c(2,2))
  
# blue
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_1988$B1_sre, col = clb)

# green
clg <- colorRampPalette(c('dark green','green','light green'))(100)
plot(p224r63_1988$B2_sre, col = clg)

# red
clr <- colorRampPalette(c('dark red','red','pink'))(100)
plot(p224r63_1988$B3_sre, col = clr)

# nir
clnir <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_1988$B4_sre, col = clnir)
 
dev.off()

# B1: blue - 1
# B2: green - 2
# B3: red - 3
# B4: near infrared (nir) - 4
  
# immagine con colori a noi visibili
plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin")
  
# Exercise: plot the image using nir on the "r" componenent in RGB space
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
  
# confronto fra le immagini del 1988 e del 2011
par(mfrow = c(2,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin", main="1988")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin", main="2011")
# si nota una marcata diminuzione dei pixel rossi, corrispondenti ai punti con alta riflettanza nel vicino infrarosso => deforestazione
  
dev.off()

#######################################################################
# calcolo dell'indice spettrale dvi per il 1988
# dvi = nir - red
dvi1988 <- p224r63_1988$B4_sre - p224r63_1988$B3_sre
plot(dvi1988)
  
# ex: calcolo dvi per il 2011
dvi2011 <- p224r63_2011$B4_sre - p224r63_2011$B3_sre
plot(dvi2011)
  
cldvi <- colorRampPalette(c('light blue','light green','green'))(100)
plot(dvi2011, col=cldvi)

# multitemporal analysis
# cambiamento di dvi nel tempo
difdvi <- dvi2011 - dvi1988
plot(difdvi)

# cambiamento della palette per rendere più evidenti le variazioni
cldifdvi <- colorRampPalette(c('red','white','blue'))(100)
# le zone rosse mostrano un calo del valore di DVI, mentre quelle blu un suo aumento, quelle bianche sono stabili
plot(difdvi, col=cldifdvi)
  
  
# visualize the output
# multiframe 1988rgb, 2011rgb, difdiv
par(mfrow=c(3,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plot(difdvi, col=cldifdvi)
dev.off()
  
  
# variare la grana dell'immagine
p224r63_2011lr <- aggregate(p224r63_2011, fact=10) # i pixel assumono una dimensione 10 volte maggiore (di lato)
  
p224r63_2011
p224r63_2011lr

# multiframe con immagine di partenza e a risoluzione più bassa
par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")


# risluzione ancora più bassa
p224r63_2011lr50 <- aggregate(p224r63_2011, fact=50)
  
p224r63_2011lr50
# originale ogni pixel ha lato di 30m -> si passa a lato di 1500m
  
par(mfrow=c(3,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr50, r=4, g=3, b=2, stretch="Lin")
dev.off()
  
# dvi2011 a bassa risoluzione
dvi2011lr50 <- p224r63_2011lr50$B4_sre - p224r63_2011lr50$B4_sre
plot(dvi2011lr50)
  
  
# dvi1988 a bassa risoluzione
p224r63_1988lr50 <- aggregate(p224r63_1988, fact=50)
dvi1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B4_sre
  
# differenza dvi a bassa risoluzione
dvi2011lr50 <- p224r63_2011lr50$B4_sre - p224r63_2011lr50$B3_sre
  
dvi1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre

difdvilr50 <- dvi2011lr50 - dvi1988lr50
plot(difdvilr50,col=cldifdvi)
  
# multiframe con dvi base e a bassa risoluzione
par(mfrow=c(2,1))
plot(difdvi, col=cldifdvi)
plot(difdvilr50, col=cldifdvi)
# non è più possibile ottenere il livello di dettaglio precedente, l'analisi risulta perciò più grossolana


################################################
################################################

# r code landcover
# codice R per analisi multitemporale della land cover

setwd("C:/lab")

install.packages("Rstoolbox") # libreria per analisi immagini da telerilevamento
library(RStoolbox)

 

# importazione del file raster
p224r63_2011 <- brick("p224r63_2011_masked.grd")



plotRGB(p224r63_2011, r = 4, g = 3, b = 2, stretch = "Lin") # banda infrarossa nel rosso

# suddivisione dei pixel in 4 classi
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses = 4) # 'unsuperClass' sta per "classificazione non supervisionata"
                                                 # le classi vengono create automaticamente dal software, senza intervento dall'esterno

p224r63_2011c # visualizzare lcom'è composto l'oggetto, per visualizzarlo graficamente "p224r63_2011c$map"
plot(p224r63_2011c$map) # i pixel appaiono colorati di quattro colori differenti, come scelto precedentemente



# cambiare i colori sul plot
clclass <- colorRampPalette(c('red', 'green', 'blue', 'black'))(100) 
plot(p224r63_2011c$map, col = clclass)



# suddivisione dei pixel in un minor numero di classi
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses = 2)
clclass <- colorRampPalette(c('red', 'green', 'blue', 'black'))(100) 
plot(p224r63_2011c$map, col = clclass)


# se il numero di classi è alto, la classe di appartenenza dei singoli pixel non è stabile
# con un numero di classi più basso, esse hanno una distinzione più netta



# in funzione del numero di classi aumenta l'incertezza dell'algoritmo automatico di classificazione
# riportando potenzialmente classi leggermente differenti


################################################
################################################

# R code multitemp
# analisi multitemporale

setwd("C:/lab")

library(raster)

# importazione delle immagini
defor1 <- brick("defor1_.png")
defor2 <- brick("defor2_.png")

defor1
# names(defor1), alternativa al comando precedente per visualizzare nome dei campi dell'oggetto 'defor1'
# "defor1_.1" "defor1_.2" "defor1_.3"

# defor1_.1 = NIR
# defor1_.2 = red
# defor1_.3 = green


plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")


# Exercise: plot della seconda data
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")


# multiframe, confronto della stessa area in due momenti differenti
par(mfrow = c(2,1))
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")


# classificazione non supervisionata
library(RStoolbox)

d1c <- unsuperClass(defor1, nClasses = 2)
# si creano classi di foresta e non foresta
plot(d1c$map)

cl <- colorRampPalette(c('green','black'))(100)
plot(d1c$map, col=cl)

# esempio sul significato del $
# mappageologica <- geomap(im_sat,nClasses=....)
# plot(mappageologica$lito)
# plot(mappageologica$lineaments)
# quando un oggetto contiene degli elementi suddivisi in sottocartelle si utilizza il '$' per richiamarli


d2c <- unsuperClass(defor2, nClasses = 2) # classificazione dei pixel in due gruppi in modo analogo al precedente
plot(d2c$map)

# visualizzazione contemporanea dei due periodi con pixel classificati
par(mfrow = c(2,1))
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)


# calcolo della frequenza delle due classi di pixel nella prima immagine
freq(d1c$map)

# aree aperte = 37039
# foresta = 304253

# numero di pixel totali nella prima immagine
totd1 <- 37039 + 304253 # questo calcolo poteva anche essere fatto fare automaticamente al software

totd1
# 341292

# calcolo delle frequenze percentuali
percent1 <- freq(d1c$map) * 100 / totd1

# foreste: 89.1 %
# aree aperte: 10.9 %


# calcolo analogo al precendente per le due classi della seconda immagine
freq(d2c$map)

# aree aperte: 165055
# foreste: 177671

totd2 <- 165055 + 177671
totd2
# 342726

percent2 <- freq(d2c$map) * 100 / totd2

# aree aperte: 48.2 %
# foreste: 51.8 %


# creazione di vettori per analisi grafica
cover <- c("Agriculture","Forest")
before <- c(10.9,89.1)
after <- c(48.2,51.8)

# creazione di un dataframe con i vettori precedentemente creati
output <- data.frame(cover,before,after)
output

################
library(ggplot2)

p1<-ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white")
# 
p2<-ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white")

# https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
install.packages("gridExtra")
library(gridExtra)

grid.arrange(p1, p2, nrow = 1) # this needs griExtra

setwd("C:/lab")
load("defor.RData")

ls()

library(raster)

par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) # 
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)


output

library(ggplot2)

ggplot(output, aes(x=cover, y=before, color=cover)) + 
  geom_bar(stat="identity", fill="white")

# Exercise: plottare istogrammi di land cover dopo la deforestazione
ggplot(output, aes(x=cover, y=after, color=cover)) + 
  geom_bar(stat="identity", fill="white")

# la funzione "par" per più grafici in stessa schermata non funziona in ggplot2
# è necessario installare il pacchetto "gridExtra"

install.packages("gridExtra")
library(gridExtra)

# grid.arrange(plot1,plot2,nrow=1)

grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + 
  geom_bar(stat="identity", fill="white")


grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + 
  geom_bar(stat="identity", fill="white")

# ex: utilizzare grid.arrange() per plottare i due grafici
grid.arrange(grafico1,grafico2,nrow=1)


# day 2

setwd("C:/lab")
load("defor.RData")

ls()

library(raster)

par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) # 
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)


output

library(ggplot2)

ggplot(output, aes(x=cover, y=before, color=cover)) + 
  geom_bar(stat="identity", fill="white")

# Exercise: plottare istogrammi di land cover dopo la deforestazione
ggplot(output, aes(x=cover, y=after, color=cover)) + 
  geom_bar(stat="identity", fill="white")

# la funzione "par" per più grafici in stessa schermata non funziona in ggplot2
# è necessario installare il pacchetto "gridExtra"

install.packages("gridExtra")
library(gridExtra)

# grid.arrange(plot1,plot2,nrow=1)

grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + 
  geom_bar(stat="identity", fill="white")


grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + 
  geom_bar(stat="identity", fill="white")

# ex: utilizzare grid.arrange() per plottare i due grafici
grid.arrange(grafico1,grafico2,nrow=1)


grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + 
  geom_bar(stat="identity", fill="white") +
  ylim(0, 100)

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + 
  geom_bar(stat="identity", fill="white") +
  ylim(0, 100)


grid.arrange(grafico1, grafico2, nrow = 1)
# i grafici finali risultano più leggibili e confrontabili perchè hanno stesso intervallo per l'ordinata



##############################################################
##############################################################


# r code NO2

# analisi immagini relative a emissioni NO2 prima e dopo lock-down legato a covid19
# immagini del satellite sentinel dell'ESA

setwd("C:/lab")

library(raster)
# importazione immagine
EN01 <- raster("EN_0001.png")
# va bene il comando "raster" perchè l'immagine ha una sola banda; con più bande si utilizza "brick"

plot(EN01)

# exercise: importare tutte le immagini
EN02 <- raster("EN_0002.png")
EN03 <- raster("EN_0003.png")
EN04 <- raster("EN_0004.png")
EN05 <- raster("EN_0005.png")
EN06 <- raster("EN_0006.png")
EN07 <- raster("EN_0007.png")
EN08 <- raster("EN_0008.png")
EN09 <- raster("EN_0009.png")
EN10 <- raster("EN_0010.png")
EN11 <- raster("EN_0011.png")
EN12 <- raster("EN_0012.png")
EN13 <- raster("EN_0013.png")


# altra possibilità, con ciclo
for(a in 1:13){
  assign(paste("EN", rep("0", 1 - as.integer(log10(a))), a, sep = ""),
         raster(paste("EN_00", rep("0", 1 - as.integer(log10(a))), a, ".png", sep = "")))
}
# comando specifico per questo esercizio, non direttamente generalizzabile => un po' limitato

# altra possibilità di ciclo
library(raster)

# file da importare inseriti in un'unica cartella
setwd("~/lab/esa_no2")

# file con estensione .png inseriti in un oggetto lista
rlist <- list.files(pattern = ".png", full.names = T)

# importazione vera e propria dei file raster
list_rast <- lapply(rlist, raster) # con lapply si applica un comando a tutti gli oggetti di una lista

# importazione può essere effettuata anche con ciclo for
list_rast = list()
for(i in 1:length(rlist)){
  r <- raster(rlist[[i]])
  list_rast[[i]] <- r
}


########
cl <- colorRampPalette(c('red', 'orange', 'yellow'))(100)
plot(EN01, col = cl)

plot(EN13, col = cl)

# confronto fra la prima e l'ultima immagine (in senso temporale) disponibile delle registrazioni relative all'emissione di NO2 in atmosfera 
par(mfrow=c(1,2))
plot(EN01, col=cl)
plot(EN13, col=cl)


# difference
difno2 <- EN13 - EN01
cldif <- colorRampPalette(c('blue', 'black', 'yellow'))(100)
plot(difno2, col = cldif)
# in questo modo risaltano nettamente i punti in cui l'emissione di NO2 è diminuita (blu), aumentata (giallo) o rimasta stabile (nero)


# multiframe
par(mfrow = c(4, 4))
plot(EN01, col = cl)
plot(EN02, col = cl)
plot(EN03, col = cl)
plot(EN04, col = cl)
plot(EN05, col = cl)
plot(EN06, col = cl)
plot(EN07, col = cl)
plot(EN08, col = cl)
plot(EN09, col = cl)
plot(EN10, col = cl)
plot(EN11, col = cl)
plot(EN12, col = cl)
plot(EN13, col = cl)


### day 2
setwd("C:/lab")

load("no2.RData")
ls()


setwd("C:/lab/esa_no2")
rlist <- list.files(pattern = ".png")
rlist

listafinale <- lapply(rlist, raster) # lapply sta per "list apply"
# lapply si può usare per vettori o liste
listafinale

EN <- stack(listafinale) # crea un oggetto compatto con tutte le immagini, in questo modo si possono plottare tutte con un solo comando

difEN <- EN$EN_0013 - EN$EN_0001
cld <- colorRampPalette(c('blue', 'white', 'red'))(100)
plot(difEN, col = cld) # rappresentazione analoga a quella già plottata precedentemente, ma con colori diversi

cl <- colorRampPalette(c('red', 'orange', 'yellow'))(100)
plot(EN, col = cl)


boxplot(EN, horizontal = T,  # le barre dei boxplot vengono poste orizzontalmente
        outline = F,         # vengono eliminati gli outliners, i punti che stanno all'esterno dell'intervallo primo-terzo quartile
        axes = T,            # axes=T è di default, con axes=F non vengono rappresentati gli assi nel plot
        las = 1)
  
# a livello continentale, non cambiano molto i valori medi di NO2
# ciò che cambia sono i valori max, che calano dai primi frame agli ultimi
  
  
  
##################################################################
##################################################################

# r code snow

# snow cover

install.packages("ncdf4")

setwd("C:/lab")
library(ncdf4)
library(raster)

# importazione raster con copertura nevosa del 18/05/20
# file ottenuto da un satellite del programma Copernicus
snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

cl <- colorRampPalette(c('darkblue','blue', 'light blue'))(100)

# ex: plot snow cover with created cl
plot(snowmay, col = cl)

# cambio della working directory
setwd("C:/lab/snow")

# importazione delle immagini di copertura della neve nei vari anni
rlist <- list.files(pattern = ".tif", full.names = T) # tutti i file della cartella che hanno estensione ".tif" vengono raggruppati nell'oggetto rlist

# importazione vera e propria degli file precedentemente inseriti nell'oggetto rlist
list_rast <- lapply(rlist, raster) # ogni file viene importato con il comando "raster"
snow.multitemp <- stack(list_rast) # con il comando stack i raster vengono raggruppati ed è possibile plottare tutti i raster automaticamente in un'unica finestra 
plot(snow.multitemp, col=cl)

# confronto fra copertura nevosa nel 2000 e nel 2020
par(mfrow = c(1, 2))
plot(snow.multitemp$snow2000r, col = cl, zlim = c(0, 250)) # copertura nel nord europa esagerata per fini didattici
plot(snow.multitemp$snow2020r, col = cl, zlim = c(0, 250))

# accentuazione delle differenze tra i due periodi
difsnow <- snow.multitemp$snow2020r - snow.multitemp$snow2000r
cldiff <- colorRampPalette(c('blue', 'white', 'red'))(100) 
par(mfrow = c(1, 1))
plot(difsnow, col = cldiff) # i pixel blu indicano diminuzione di copertura nevose, quelli rossi guadagno, i bianchi situazione stazionaria


source("prediction.r") # con "source" si possono eseguire comandi da un file esterno
# con comandi eseguiti si va a creare un modello di variazione dei valori di riflettanza di ogni pixel nel tempo
# in questo modo si può creare un nuovo raster che faccia da previsione per il futuro, utilizzando il modello calcolato

########
# codice prediction
require(raster)
require(rgdal)

# define the extent
ext <- c(-180, 180, -90, 90)
extension <- crop(snow.multitemp, ext)
    
# make a time variable (to be used in regression)
time <- 1:nlayers(snow.multitemp)

# run the regression
fun <- function(x) {if (is.na(x[1])){ NA } else {lm(x ~ time)$coefficients[2] }} 
predicted.snow.2025 <- calc(extension, fun) # time consuming: make a pause!
predicted.snow.2025.norm <- predicted.snow.2025*255/53.90828
########



predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif") # copertura di neve nel 2025, stimata dal modello precedente

plot(predicted.snow.2025.norm, col=cl)


###################################################
###################################################

# R_code_patches.r

library(raster)

setwd("C:/lab")

d1c <- raster("d1c.tif")
d2c <- raster("d2c.tif")

par(mfrow = c(1, 2))
cl <- colorRampPalette(c('black', 'green'))(100) #
plot(d1c, col = cl)
plot(d2c, col = cl)


# lasciare solo pixel relativi ad aree di foresta
d1c.for <- reclassify(d1c, cbind(1, NA)) # con cbind si associa un valore prescelto ad uno assunto dai pixel
                                         # in questo caso i pixel di valore 1 assumono il valore NA

d2c.for <- reclassify(d2c, cbind(1, NA))



par(mfrow = c(1, 2))
cl <- colorRampPalette(c('black', 'green'))(100) #
plot(d1c, col = cl)
plot(d1c.for, col = cl)


install.packages("igraph") # non è un pacchetto vero e proprio, ma una dependency di raster
library(igraph)

# contare il numero di patch 
d1c.for.patches <- clump(d1c.for)
d2c.for.patches <- clump(d2c.for)

# per salvare un raster all'esterno di r si utilizza la funzione "writeRaster"
writeRaster(d1c.for.patches,"d1c.for.patches.tif")
writeRaster(d2c.for.patches,"d2c.for.patches.tif")

# Exercise: plottare entrambe le mappe, una a fianco all'altra
par(mfrow=c(1,2))
plot(d1c.for.patches, col = cl)
plot(d2cforp, col = cl)


cl <- colorRampPalette(c('dark blue','blue','green','orange','yellow','red'))(100)

# Exercise: plottare entrambe le mappe una accanto all'altra
clp <- colorRampPalette(c('dark blue','blue','green','orange','yellow','red'))(100) # 
par(mfrow=c(1,2))
plot(d1c.for.pacthes, col=clp)
plot(d2c.for.pacthes, col=clp)


time <- c("Before deforestation","After deforestation")
npatches <- c(301,1212)

output <- data.frame(time,npatches)
attach(output)


library(ggplot2)
ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white")

