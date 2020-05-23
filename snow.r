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



predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif") # risultato 

plot(predicted.snow.2025.norm, col=cl)

