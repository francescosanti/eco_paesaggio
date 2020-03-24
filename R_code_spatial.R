#analisi dati spaziali

#carico libreria e dati
library(sp)
data(meuse)

head(meuse)

#plot cadmium e lead
#allegare dataframe
attach(meuse)

plot(cadmiu,lead,col="red",pch=19,cex=2)

#exercise: plot di copper e zinc con simbolo triangolo (pch=17) e colore verde
plot(copper,zinc,col="green",pch=17,cex=2,xlab="rame",ylab="zinco")

#multiframe o multipanel
par(mfrow=c(1,2)) #divide lo schermo in due colonne e una riga
plot(cadmium,lead,col="red",pch=19,cex=2)
plot(copper,zinc,col="green",pch=17,cex=2,xlab="rame",ylab="zinco")

#inversione di grafici riga/colonna in colonna/riga
par(mfrow=c(2,1))
plot(cadmium,lead,col="red",pch=19,cex=2)
plot(copper,zinc,col="green",pch=17,cex=2,xlab="rame",ylab="zinco")

#multiframe automatico
install.packages("GGally")
ggpairs(meuse[,3:6]) #in diagonale distribuzione della frequenza dei dati della variabile, sotto la distribuzione dei punti di due variabili, sopra la correlazione fra due var

#spatial
head(meuse)

coordinates(meuse)=~x+y #colonne x e y sono le coordinate
plot(meuse)

#creare grafico spaziale con funzione spplot
spplot(meuse, "zinc") #crea distribuzione spaziale di variabile zinc
