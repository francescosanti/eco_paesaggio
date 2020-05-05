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
