setwd("C:/lab_eco_pae")

# importazione dei dati
covid <- read.table("covid_agg.csv",header=T,sep=";")

head(covid)

plot(covid$country,covid$cases)
#las argomento per posizionamento delle etichette sugli assi nel grafico, las sta per "labels"
plot(covid$country,covid$cases,las=0) #las=0 etichette parallele agli assi
plot(covid$country,covid$cases,las=1) #las=1 etichette orizzontali
plot(covid$country,covid$cases,las=2) #las=2 perpendicolari agli assi
plot(covid$country,covid$cases,las=3) #las=3 etichette verticali

library(ggplot2)
data(mpg)
head(mpg)

ggplot(mpg,aes(x=displ,y=hwy))+geom_point() 
ggplot(mpg,aes(x=displ,y=hwy))+geom_line() 

ggplot(covid,aes(x=lon,y=lat,size=cases)) + geom_point()

attach(covid)
covids <- ppp(lon, lat, c(-180,180), c(-90,90))
d<-density(covids)

plot(d)
points(covid)
