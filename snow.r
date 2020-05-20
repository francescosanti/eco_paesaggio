# snow cover

# install.packages("ncdf4")

setwd("C:/lab")
library(ncdf4)
library(raster)

snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)

# ex: plot snow cover with created cl
plot(snowmay, col=cl)

setwd("C:/lab/snow")

# importazione delle immagini di copertura della neve nei vari anni
rlist=list.files(pattern=".tif", full.names=T)

# save raster into list
# con lappy
list_rast=lapply(rlist, raster)
snow.multitemp <- stack(list_rast)
plot(snow.multitemp, col=cl)

par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl, zlim=c(0,250))
plot(snow.multitemp$snow2020r, col=cl, zlim=c(0,250))


difsnow <- snow.multitemp$snow2020r - snow.multitemp$snow2000r
cldiff <- colorRampPalette(c('blue','white','red'))(100) 
par(mfrow=c(1,1))
plot(difsnow, col=cldiff)

source("prediction.r") # con "source" si possono eseguire comandi da un file esterno


predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")

plot(predicted.snow.2025.norm, col=cl)

