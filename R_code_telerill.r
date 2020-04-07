# Codice R per le immagini satellitari

install.packages("raster")
library(raster)
 
setwd("C:/lab_eco_pae")
 
p224r63_2011 <- brick("p224r63_2011_masked.grd")
# il comando brick permette di importare file raster
