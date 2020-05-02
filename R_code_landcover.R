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
plot(p224r63_2011c$map)

# se il numero di classi è alto, la classe di appartenenza dei singoli pixel non è stabile
# con un numero di classi più basso, esse sono distinte più nettamente
