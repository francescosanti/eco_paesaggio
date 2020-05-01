#### analisi dati spaziali 24/03/20

# carico libreria e dati
library(sp)
data(meuse)

head(meuse)

# plot di cadmio e piombo
# allegare dataframe
attach(meuse)

plot(cadmium, lead, col = "red", pch = 19, cex = 2)

# exercise: plot di copper e zinc con simbolo triangolo (pch=17) e colore verde
plot(copper, zinc, col = "green", pch = 17, cex = 2, xlab = "rame", ylab = "zinco")

# multiframe o multipanel (inserire più grafici in una finestra)
par(mfrow = c(1, 2)) #divide lo schermo in due colonne e una riga
plot(cadmium, lead, col = "red", pch = 19, cex = 2)
plot(copper, zinc, col = "green", pch = 17, cex = 2, xlab = "rame", ylab = "zinco")

# inversione di grafici riga/colonna in colonna/riga
par(mfrow = c(2, 1))
plot(cadmium, lead, col = "red", pch = 19, cex = 2)
plot(copper, zinc, col = "green", pch = 17, cex = 2, xlab = "rame", ylab = "zinco")

# multiframe automatico
install.packages("GGally")
library(GGally)
ggpairs(meuse[, 3:6]) # in diagonale distribuzione della frequenza dei dati della variabile,
                      # nella parte bassa la distribuzione dei punti di due variabili
                      # nella parte alta la correlazione fra due variabili

# spatial
head(meuse)

coordinates(meuse) = ~ x + y # colonne x e y sono da interpretare come coordinate
plot(meuse)

# creare grafico spaziale con funzione spplot
spplot(meuse, "zinc") # crea distribuzione spaziale di variabile zinc, i siti sono colorati in base al valore della variabile selezionata


### continuazione analisi spaziale  25/03/20
names(meuse) # visualizzazione dei nomi delle colonne del dataframe
spplot(meuse, "copper")

bubble(meuse, "zinc") # dà rappresentazione spaziale analoga a quella di spplot, i punti hanno dimensione proporzionale al valore della variabile

bubble(meuse, "copper", col = "red")

# formainiferi (Sofia), carbon capture (Marco)
# creazione di array
foram <- c(12, 20, 35, 55, 67, 80) # la lettera c sta per il termine inglese 'concatenate'
carbon <- c(5, 15, 30, 70, 85, 99) # c() si utilizza quando si deve specificare più di un oggetto
plot(foram, carbon, pch = 19, col = "green", cex = 2)
