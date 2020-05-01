# primo codice R

install.packages("sp")
library(sp)
require(sp) # altro modo per caricare pacchetto

data("meuse")
head(meuse)
names(meuse) # nomi delle variabili
summary(meuse) # riporta le statistiche di base per le variabili di meuse

pairs(meuse) # grafici a coppie fra tutte le variabili
pairs(~ cadmium + copper + lead, data = meuse) # grafici a coppie fra le tre variabili selezionate
# il simbolo ~ spesso sta per "uguale" in R

pairs(meuse[, 3:6]) #anzichè scrivere varie colonne prendo subset del database meuse
pairs(meuse[, 3:6],
      col = "orange",        # scelgo il colore dei simboli
      pch=19,                # pch sta per "point character", scelgo tipo di simbolo
      cex = 2,               # cex sta per "character exageration", scelgo la grandezza del simbolo (di base cex=1)
      main = "Primo pairs")  # titolo del grafico


panel.correlations <- function(x, y, digits=1, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r1=cor(x,y,use="pairwise.complete.obs")
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  txt <- format(c(r1, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.9/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * r)
}
# funzione per calcolare la correlazione fra due variabili


panel.smoothing <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                             cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
          col = 1, ...)
}
# smoothing fa una sorta di regressione fra due variabili



panel.histograms <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}
# funzione per creare istogramma di una variabile


# grafici a coppie fra le quattro variabili selezionate, in cui vengono mostrati anche coefficiente di correlazione
# fra le variabili e istogramma delle singole variabili, utilizzando le funzioni precedentemente create
pairs(meuse[, 3:6], lower.panel = panel.correlations, upper.panel = panel.smoothing, diag.panel = panel.histograms)
# lower.panel è la parte sotto la diagonale
# upper.panel è la parte sopra la diagonale
# diag.panel è la diagonale

pairs(meuse[, 3:6], lower.panel = panel.smoothing, upper.panel = panel.correlations, diag.panel = panel.histograms)
# correlazione e interpolazione invertite di posto rispetto alla diagonale

# plot fra due variabili
plot(meuse$cadmium, meuse$copper)
attach(meuse) # permette di richimare i campi dell'oggetto 'meuse' senza dover richiamare l'oggetto stesso (non serve più 'meuse$')
plot(cadmium, copper)
plot(cadmium, copper, pch=17, col = "green", main = "Primo plot", xlab = "Cadmio", ylab = "Rame")
