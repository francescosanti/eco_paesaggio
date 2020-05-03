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

