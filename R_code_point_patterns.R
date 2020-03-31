setwd("C:/lab_eco_pae")

# importazione dei dati
covid <- read.table("covid_agg.csv",header=T,sep=";")

head(covid)

plot(covid$country,covid$cases)
#las argomento per posizionamento delle etichette sugli assi nel grafico, las sta per "labels"
plot(covid$country,covid$cases,las=0) #las=0 
plot(covid$country,covid$cases,las=1) #las=1 
plot(covid$country,covid$cases,las=2) #las=2 
plot(covid$country,covid$cases,las=3) #las=3 verticali
