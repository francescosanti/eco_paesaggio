# snow cover

# install.packages("ncdf4")

setwd("C://lab")
library(ncdf4)
library(raster)

snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)

# ex: plot snow cover with created cl
plot(snowmay, col=cl)

setwd("C://lab/snow")

rlist=list.files(pattern=".tif", full.names=T)

#save raster into list
#con lappy
list_rast=lapply(rlist, raster)
snow.multitemp <- stack(list_rast)
plot(snow.multitemp)
