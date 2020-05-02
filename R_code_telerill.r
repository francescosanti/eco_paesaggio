# Codice R per le immagini satellitari

install.packages("raster")
library(raster)
 
setwd("C:/lab_eco_pae")
 
p224r63_2011 <- brick("p224r63_2011_masked.grd")
# il comando brick permette di importare file raster

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
# a seconda della banda su cui è montata la radiazione infrarossa, alti valori di essa assumeranno
# colorazione rispettivamente rossa, blu e verde
