setwd("C:/lab")
library(RStoolbox)
 
# importazione del file raster 
p224r63_2011 <- brick("p224r63_2011_masked.grd")

plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# suddivisione dei pixel in 4 classi
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4)
p224r63_2011c
plot(p224r63_2011c$map)

# cambiare i colori sul plot
clclass <- colorRampPalette(c('red', 'green', 'blue', 'black'))(100) 
plot(p224r63_2011c$map, col=clclass)


# suddivisione dei pixel in un minor numero di classi
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=2)
plot(p224r63_2011c$map)

# se il numero di classi è alto, la classe di appartenenza dei singoli pixel non è stabile
