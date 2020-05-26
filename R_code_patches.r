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

