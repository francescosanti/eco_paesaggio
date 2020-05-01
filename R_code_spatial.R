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

# multiframe o multipanel
par(mfrow = c(1, 2)) #divide lo schermo in due colonne e una riga
plot(cadmium, lead, col = "red", pch = 19, cex = 2)
plot(copper, zinc, col = "green", pch = 17, cex = 2, xlab = "rame", ylab = "zinco")

# inversione di grafici riga/colonna in colonna/riga
par(mfrow = c(2, 1))
plot(cadmium, lead, col = "red", pch = 19, cex = 2)
plot(copper, zinc, col = "green", pch = 17, cex = 2, xlab = "rame", ylab = "zinco")

# multiframe automatico
install.packages("GGally")
ggpairs(meuse[, 3:6]) #in diagonale distribuzione della frequenza dei dati della variabile, sotto la distribuzione dei punti di due variabili, sopra la correlazione fra due var

# spatial
head(meuse)

coordinates(meuse) = ~ x + y #colonne x e y sono da interpretare come coordinate
plot(meuse)

# creare grafico spaziale con funzione spplot
spplot(meuse, "zinc") #crea distribuzione spaziale di variabile zinc


### 25/03/20 continua analisi spaziale
names(meuse) #fa vedere i nomi delle colonne del dataframe
spplot(meuse, "copper")

bubble(meuse, "zinc") #dà rappresentazione spaziale analoga a quella di spplot, ma i punti, anzichè essere colorati diversamente a seconda del valore della variabile, hanno dimensione proporzionale al valore della variabile

bubble(meuse, "copper", col = "red")

# formainiferi (Sofia), carbon capture (Marco)
# array
foram <- c(12, 20, 35, 55, 67, 80)
carbon <- c(5, 15, 30, 70, 85, 99)
plot(foram, carbon, pch = 19, col = "green", cex = 2)


# leggere tabella
covid <- read.table("covid_agg.csv", header = T)
