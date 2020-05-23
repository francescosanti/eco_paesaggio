# analisi immagini relative a emissioni NO2 prima e dopo lock-down legato a covid19
# immagini del satellite sentinel dell'ESA

setwd("C:/lab")

library(raster)
# importazione immagine
EN01 <- raster("EN_0001.png")
# va bene il comando "raster" perchè l'immagine ha una sola banda; con più bande si utilizza "brick"

plot(EN01)

# exercise: importare tutte le immagini
EN02 <- raster("EN_0002.png")
EN03 <- raster("EN_0003.png")
EN04 <- raster("EN_0004.png")
EN05 <- raster("EN_0005.png")
EN06 <- raster("EN_0006.png")
EN07 <- raster("EN_0007.png")
EN08 <- raster("EN_0008.png")
EN09 <- raster("EN_0009.png")
EN10 <- raster("EN_0010.png")
EN11 <- raster("EN_0011.png")
EN12 <- raster("EN_0012.png")
EN13 <- raster("EN_0013.png")


# altra possibilità, con ciclo
for(a in 1:13){
  assign(paste("EN",rep("0",1-as.integer(log10(a))),a,sep=""),
         raster(paste("EN_00",rep("0",1-as.integer(log10(a))),a,".png",sep="")))
}
# comando specifico per questo esercizio, non direttamente generalizzabile => un po' limitato

# altra possibilità di ciclo
library(raster)

# file da importare inseriti in un'unica cartella
setwd("~/lab/esa_no2")

# file con estensione .png inseriti in un oggetto
rlist <- list.files(pattern = ".png", full.names = T)

# importazione vera e propria dei raster
list_rast <- lapply(rlist, raster) # con lapply si applica un comando a tutti gli oggetti di una lista

# importazione può essere effettuata anche con ciclo for
list_rast = list()
for(i in 1:length(rlist)){
  r <- raster(rlist[[i]])
  list_rast[[i]] <- r
}


########
cl <- colorRampPalette(c('red','orange','yellow'))(100) #
plot(EN01,col=cl)

plot(EN13, col=cl)

par(mfrow=c(1,2))
plot(EN01, col=cl)
plot(EN13, col=cl)


# difference
difno2 <- EN13 - EN01
cldif <- colorRampPalette(c('blue','black','yellow'))(100) #
plot(difno2, col=cldif)
# in questo modo risaltano nettamente i punti in cui l'emissione di NO2 è diminuita (giallo???????) e in cui è aumentata (blu)


# multiframe
par(mfrow=c(4,4))
plot(EN01, col=cl)
plot(EN02, col=cl)
plot(EN03, col=cl)
plot(EN04, col=cl)
plot(EN05, col=cl)
plot(EN06, col=cl)
plot(EN07, col=cl)
plot(EN08, col=cl)
plot(EN09, col=cl)
plot(EN10, col=cl)
plot(EN11, col=cl)
plot(EN12, col=cl)
plot(EN13, col=cl)


### day 2
setwd("C:/lab")

load("no2.RData")
ls()


setwd("C:/lab/esa_no2")
rlist <- list.files(pattern=".png")
rlist

listafinale <- lapply(rlist, raster) # lapply sta per "list apply"
# lapply si può usare per vettori o liste
listafinale

EN <- stack(listafinale) # crea un oggetto compatto con tutte le immagini, in questo modo si possono plottare tutte con un solo comando

difEN <- EN$EN_0013 - EN$EN_0001
cld <- colorRampPalette(c('blue','white','red'))(100) # 
plot(difEN, col=cld)

cl <- colorRampPalette(c('red','orange','yellow'))(100) #
plot(EN, col=cl)


boxplot(EN, horizontal=T,  # le barre dei boxplot vengono poste orizzontalmente
        outline=F,         # vengono eliminati gli outliners, i punti che stanno all'esterno dell'intervallo primo-terzo quartile
        axes=T)            # axes=T è di default, con axes=F non vengono rappresentati gli assi nel plot
  
  # a livello continentale, non cambiano molto i valori medi di NO2, ciò che cambia sono i valori max, che calano dai primi frame agli ultimi
  
  
  
