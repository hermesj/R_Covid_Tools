library(stringr)
library(lubridate)
library(dplyr)

# Einlesen Infektionen - Quelle: https://github.com/robert-koch-institut 2022-05-29
infections <- read.csv("data/Aktuell_Deutschland_SarsCov2_Infektionen.txt", 
                       header = TRUE)
infections$IdLandkreis <- str_pad(infections$IdLandkreis, 5, pad = "0")
infections$IdLandkreis <- factor(infections$IdLandkreis)
infections$Altersgruppe <- factor(infections$Altersgruppe)
infections$Geschlecht <- factor(infections$Geschlecht)
infections$Meldedatum <- as.Date(infections$Meldedatum, format = "%Y-%m-%d")
infections$Refdatum <- as.Date(infections$Refdatum, format = "%Y-%m-%d")
infections$IstErkrankungsbeginn <- factor(infections$IstErkrankungsbeginn)
infections$NeuerFall <- factor(infections$NeuerFall)
infections$NeuerTodesfall <- factor(infections$NeuerTodesfall)
infections$NeuGenesen <- factor(infections$NeuGenesen)
summary(infections)

# Einlesen Impfungen - Quelle: https://github.com/robert-koch-institut 2022-05-29
vaccinations <- read.csv("data/Aktuell_Deutschland_Landkreise_COVID-19-Impfungen.txt")
vaccinations$Impfdatum <- as.Date(vaccinations$Impfdatum, format = "%Y-%m-%d")
vaccinations$LandkreisId_Impfort <- factor(vaccinations$LandkreisId_Impfort)
vaccinations$Altersgruppe <- factor(vaccinations$Altersgruppe)
vaccinations$Impfschutz <- factor(vaccinations$Impfschutz)
summary(vaccinations)

# Einlesen Hospitalisierungen - Quelle: https://github.com/robert-koch-institut 2022-05-29
# Leider keine Kreis-, sondern nur Bundesländerdaten
hospitalizations <- read.csv("data/Aktuell_Deutschland_COVID-19-Hospitalisierungen.txt")

# Einlesen Kreis IDs
kreisInfos <- read.table("resources/LKs_Infos_merged.csv", 
                       header = TRUE, sep = "\t", skip = 4)
kreisInfos$Kennzahl <- str_pad(kreisInfos$Kennzahl, 5, pad = "0")
summary(kreisInfos)

# Unterschiede in Zahl der Landkreise --> Berlin?
# Differenzmenge (müsste es doch eigentlich einfacher geben, hier eine Lösung
# aus https://www.r-bloggers.com/2017/06/algebra-of-sets-in-r/ )
relcomp <- function(a, b) {
  comp <- vector()
  for (i in a) {
    if (i %in% a && !(i %in% b)) {
      comp <- append(comp, i)
    }
  }
  return(comp)
}
LK_IDs_Vacc <- levels(vaccinations$LandkreisId_Impfort)
LK_IDs_Inf <- levels(infections$IdLandkreis)
print("LKs mit Infektionen, ohne Impfungen: ")
print(relcomp(LK_IDs_Inf,LK_IDs_Vacc))
print("LKs mit Impfungen, ohne Infektionen: ")
print(relcomp(LK_IDs_Vacc, LK_IDs_Inf))
# kreisIDs$ags[which(kreisIDs$name=="Wartburgkreis")]


#Behandlung Berlin: Rückführung Bezirke auf Kreisebene in Infektionstabelle
infections$IdLandkreis <- gsub(pattern="^110..", replacement="11000", x=infections$IdLandkreis)
infections$IdLandkreis <- factor(infections$IdLandkreis)

#Behandlung Eisenach: Aufnahme in LK Wartburgkreis bei Impfungen
vaccinations$LandkreisId_Impfort <- gsub(pattern="^16056", replacement="16063", x=vaccinations$LandkreisId_Impfort) 
vaccinations$LandkreisId_Impfort <- factor(vaccinations$LandkreisId_Impfort)


# Check, o zu allen Landkreisen mit Impfung / Infektion auch LK-Daten vorliegen
LK_IDs_Inf <- levels(infections$IdLandkreis)
print(relcomp(LK_IDs_Inf,kreisInfos$Kennzahl))
print(relcomp(kreisInfos$Kennzahl, LK_IDs_Inf))
length(kreisInfos$Kennzahl)
length(LK_IDs_Inf)

#tapply(X=infections$AnzahlTodesfall, INDEX=infections$Refdatum, FUN=sum)
#tapply(X=infections$AnzahlTodesfall, INDEX=infections$IdLandkreis, FUN=sum)

# Infektionen gruppieren nach Wochen
infections$week <- floor_date(infections$Refdatum, "week")
vaccinations$week <- floor_date(vaccinations$Impfdatum, "week")

# Infektionen pro Woche addieren (alle Kreise)
infPerWeek = data.frame(tapply(X=infections$AnzahlFall/837.56, INDEX=infections$week, FUN=sum));
dates <- function(x) dimnames(x)[[1]]
datesList <- dates(infPerWeek)
infPerWeek$Date = as.Date(datesList)
infPerWeek$Value = infPerWeek$tapply.X...infections.AnzahlFall.837.56..INDEX...infections.week..;

# Tägliche Todesfälle pro Woche ermitteln (alle Kreise)
deathsPerWeek = data.frame(tapply(X=infections$AnzahlTodesfall/7, INDEX=infections$week, FUN=sum));
names(deathsPerWeek)[1] = "DailyDeaths"
datesListD <- dates(deathsPerWeek)
deathsPerWeek$Date = as.Date(datesListD)
#deathsPerWeek$Value = deathsPerWeek$tapply.X...infections.AnzahlTodesfall..INDEX...infections.week../7;

# Impfungen pro Woche addieren (alle Kreise)
vaccsPerWeek = data.frame(tapply(X=vaccinations$Anzahl, INDEX=vaccinations$week, FUN=sum));
datesListV <- dates(vaccsPerWeek)
vaccsPerWeek$Date = as.Date(datesListV)
vaccsPerWeek$Value = vaccsPerWeek$tapply.X...vaccinations.Anzahl..INDEX...vaccinations.week..FUN...sum.

#Impfungen pro LK addieren
vaccsPerLK = data.frame(tapply(X=vaccinations$Anzahl, INDEX=vaccinations$LandkreisId_Impfort, FUN=sum));
names(vaccsPerLK)[1] <- "Count"
LKListV <- dates(vaccsPerLK)
vaccsPerLK$LK = LKListV
vaccsPerLK = merge(vaccsPerLK, kreisInfos, by.x="LK", by.y="Kennzahl", all=TRUE)
vaccsPerLK$VaccIncidence <-  vaccsPerLK$Count / vaccsPerLK$Einwohner;

# Todesfälle per LK addieren
deathsPerLK = data.frame(tapply(X=infections$AnzahlTodesfall, INDEX=infections$IdLandkreis, FUN=sum));
names(deathsPerLK)[1] = "DeathCount"
datesListLK <- dates(deathsPerLK)
deathsPerLK$LK = as.factor(datesListLK)
deathsPerLK = merge(deathsPerLK, vaccsPerLK)
deathsPerLK$DeathIncidence <- deathsPerLK$DeathCount / deathsPerLK$Einwohner * 100000

# Plottet ganzen Zeitraum - Impfungen
plot(x=vaccsPerWeek$Date, y=vaccsPerWeek$Value, ylab="", xlab="", pch=8, type="b", 
     col="darkgreen" ,main ="Impfungen Deutschland")

# Plottet ganzen Zeitraum - Todesfälle
plot(x=deathsPerLK$Date, y=deathsPerLK$Value, ylab="", xlab="", pch=8, type="b", 
     col="black" ,main ="Todesfälle Deutschland")

# Barplot zu Impfungen pro EW auf LK-Ebene
barplot(VaccIncidence ~ LK, data = vaccsPerLK)

# Barplot zu Todesfällen pro 100k EW auf LK-Ebene
barplot(DeathIncidence ~ LK, data = deathsPerLK)

#Korrelation zwischen Impfungen und Todesfällen (gesamte Pandemie) auf LK-Ebene
plot(x=deathsPerLK$VaccIncidence, y = deathsPerLK$DeathIncidence, ylab="Todesfälle pro 100k", xlab="Impfungen pro Einwohner")
abline(lm(deathsPerLK$DeathIncidence~deathsPerLK$VaccIncidence)$coef, col ="red")

# Zusammenfassung von Regionen (Erste DREI Ziffern der Kreis-ID gleich)
deathsPerLK$Region <- as.factor(substr(deathsPerLK$LK, start=1, stop=3))

perRegion <- data.frame(tapply(X=deathsPerLK$Einwohner, INDEX=deathsPerLK$Region, FUN=function(x) sum(x)))
names(perRegion)[1] <- "Einwohner"
perRegion$regionID <- as.factor(dates(perRegion))

perRegion$Vaccs <- tapply(X=deathsPerLK$Count, INDEX=deathsPerLK$Region, FUN=function(x) sum(x))
names(perRegion)[3] <- "Impfungen"

perRegion$Vaccs <- tapply(X=deathsPerLK$DeathCount, INDEX=deathsPerLK$Region, FUN=function(x) sum(x))
names(perRegion)[4] <- "Todesfälle"

perRegion$ImpfInzidenz <-  perRegion$Impfungen /perRegion$Einwohner;
perRegion$TFInzidenz <-  perRegion$Todesfälle /perRegion$Einwohner * 100000;

#Korrelation zwischen Impfungen und Todesfällen (gesamte Pandemie) auf Region-Ebene
plot(x=perRegion$ImpfInzidenz, y = perRegion$TFInzidenz, ylab="Todesfälle pro 100k", xlab="Impfungen pro Einwohner")
abline(lm(perRegion$TFInzidenz~perRegion$ImpfInzidenz)$coef, col ="red")

# Zusammenfassung von Bundesländern (Erste ZWEI Ziffern der Kreis-ID gleich)
deathsPerLK$BL <- as.factor(substr(deathsPerLK$LK, start=1, stop=2))

perBL <- data.frame(tapply(X=deathsPerLK$Einwohner, INDEX=deathsPerLK$BL, FUN=function(x) sum(x)))
names(perBL)[1] <- "Einwohner"
perBL$BL_ID <- as.factor(dates(perBL))

perBL$Vaccs <- tapply(X=deathsPerLK$Count, INDEX=deathsPerLK$BL, FUN=function(x) sum(x))
names(perBL)[3] <- "Impfungen"

perBL$Vaccs <- tapply(X=deathsPerLK$DeathCount, INDEX=deathsPerLK$BL, FUN=function(x) sum(x))
names(perBL)[4] <- "Todesfälle"

perBL$ImpfInzidenz <-  perBL$Impfungen /perBL$Einwohner;
perBL$TFInzidenz <-  perBL$Todesfälle /perBL$Einwohner * 100000;

#Korrelation zwischen Impfungen und Todesfällen (gesamte Pandemie) auf Bundesländer-Ebene
plot(x=perBL$ImpfInzidenz, y = perBL$TFInzidenz, ylab="Todesfälle pro 100k", xlab="Impfungen pro Einwohner")
abline(lm(perBL$TFInzidenz~perBL$ImpfInzidenz)$coef, col ="red")


# TODO: Verschiedene Zeitfenster modellieren


# Plots für "Gefangene der Gegenwart" 
# Plottet gesamten Zeitraum 
plot(x=infPerWeek$Date, y=infPerWeek$Value, ylab="", xlab="", pch=8, type="b", 
     col="red" ,main ="Pandemieverlauf Deutschland Januar 2020 - Mai 2022")
lines(x=deathsPerWeek$Date, y=deathsPerWeek$DailyDeaths, ylab="", xlab="", pch=3, 
      bg="black", type="b")
lines(x=vaccsPerWeek$Date, y=vaccsPerWeek$Value/10000, ylab="", xlab="", pch=8, type="b", 
      col="darkgreen" )
legend("topleft",legend=c("Wocheninzidenz","Tägliche Todesfälle in der Woche"), 
       col=c("red","black"), pch=c(8,3),lty=c(1,2), ncol=1)

#Plottet Akt 1
plot(x=infPerWeek$Date, y=infPerWeek$Value, ylab="", xlab="", pch=8, type="b", 
     col="red", xlim=as.Date(c("2020-02-01","2020-05-06")), ylim=c(0,300), 
     main ="Pandemieverlauf Deutschland Januar - Mai 2020")
lines(x=deathsPerWeek$Date, y=deathsPerWeek$DailyDeaths, ylab="", xlab="", pch=3, 
      bg="black", type="b")
legend("topleft",legend=c("Wocheninzidenz","Tägliche Todesfälle in der Woche"), 
       col=c("red","black"), pch=c(8,3),lty=c(1,2), ncol=1)

#Plottet Akt 2
plot(x=infPerWeek$Date, y=infPerWeek$Value, ylab="", xlab="", pch=8, type="b", 
     col="red", xlim=as.Date(c("2020-05-07","2020-10-19")), ylim=c(0,250),
     main ="Pandemieverlauf Deutschland Mai - Oktober 2020")
  lines(x=deathsPerWeek$Date, y=deathsPerWeek$DailyDeaths, ylab="", xlab="", pch=3, 
      bg="black", type="b")
legend("topleft",legend=c("Wocheninzidenz","Tägliche Todesfälle in der Woche"), 
       col=c("red","black"), pch=c(8,3),lty=c(1,2), ncol=1)

#Plottet Akt 3
plot(x=infPerWeek$Date, y=infPerWeek$Value, ylab="", xlab="", pch=8, type="b", 
     col="red", xlim=as.Date(c("2020-10-20","2021-05-21")), ylim=c(0,1000),
     main ="Pandemieverlauf Deutschland Oktober 2020 - Mai 2021")
lines(x=deathsPerWeek$Date, y=deathsPerWeek$DailyDeaths, ylab="", xlab="", pch=3, 
      bg="black", type="b")
legend("topright",legend=c("Wocheninzidenz","Tägliche Todesfälle in der Woche"), 
       col=c("red","black"), pch=c(8,3),lty=c(1,2), ncol=1)

#Plottet Akt 4
plot(x=infPerWeek$Date, y=infPerWeek$Value, ylab="", xlab="", pch=8, type="b", 
     col="red", xlim=as.Date(c("2021-05-22","2021-11-11")), ylim=c(0,500),
     main ="Pandemieverlauf Deutschland Mai - November 2021")
lines(x=deathsPerWeek$Date, y=deathsPerWeek$DailyDeaths, ylab="", xlab="", pch=3,
      bg="black", type="b")
legend("topleft",legend=c("Wocheninzidenz","Tägliche Todesfälle in der Woche"), 
       col=c("red","black"), pch=c(8,3),lty=c(1,2), ncol=1)

#Plottet Akt 5
plot(x=infPerWeek$Date, y=infPerWeek$Value, ylab="", xlab="", pch=8, type="b", 
     col="red", xlim=as.Date(c("2021-11-12","2022-05-25")), ylim=c(0,2000),
     main ="Pandemieverlauf Deutschland November 2021 - Mai 2022")
lines(x=deathsPerWeek$Date, y=deathsPerWeek$DailyDeaths, ylab="", xlab="", pch=3, 
      bg="black", type="b")
legend("topleft",legend=c("Wocheninzidenz","Tägliche Todesfälle in der Woche"), 
       col=c("red","black"), pch=c(8,3),lty=c(1,2), ncol=1)

