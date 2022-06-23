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

# Einlesen Hospitalisierungen - Quelle: https://github.com/robert-koch-institut 2022-05-29
hospitalizations <- read.csv("data/Aktuell_Deutschland_COVID-19-Hospitalisierungen.txt")

# Einlesen Kreis IDs
kreisIDs <- read.table("resources/Corona BL - Kreiskennzahlen.tsv", 
                       header = TRUE, sep = "\t")
kreisIDs$ags <- str_pad(kreisIDs$ags, 5, pad = "0")

#tapply(X=infections$AnzahlTodesfall, INDEX=infections$Refdatum, FUN=sum)
#tapply(X=infections$AnzahlTodesfall, INDEX=infections$IdLandkreis, FUN=sum)

# Infektionen gruppieren nach Wochen und Monaten
infections$month <- floor_date(infections$Refdatum, "month")
infections$week <- floor_date(infections$Refdatum, "week")

# Infektionen pro Woche addieren (alle Kreise)
infPerWeek = data.frame(tapply(X=infections$AnzahlFall/837.56, INDEX=infections$week, FUN=sum));
dates <- function(x) dimnames(x)[[1]]
datesList <- dates(infPerWeek)
infPerWeek$Date = as.Date(datesList)
infPerWeek$Value = infPerWeek$tapply.X...infections.AnzahlFall.837.56..INDEX...infections.week..;

# Todesfälle pro Woche addieren (alle Kreise)
deathsPerWeek = data.frame(tapply(X=infections$AnzahlTodesfall, INDEX=infections$week, FUN=sum));
datesListD <- dates(deathsPerWeek)
deathsPerWeek$Date = as.Date(datesListD)
deathsPerWeek$Value = deathsPerWeek$tapply.X...infections.AnzahlTodesfall..INDEX...infections.week../7;

#Plottet ganzen Zeitraum 
plot(x=infPerWeek$Date, y=infPerWeek$Value, ylab="", xlab="", pch=8, type="b", 
     col="red" ,main ="Pandemieverlauf Deutschland Januar 2020 - Mai 2022")
lines(x=deathsPerWeek$Date, y=deathsPerWeek$Value, ylab="", xlab="", pch=3, 
      bg="black", type="b")
legend("topleft",legend=c("Wocheninzidenz","Tägliche Todesfälle in der Woche"), 
       col=c("red","black"), pch=c(8,3),lty=c(1,2), ncol=1)

#Plottet Akt 1
plot(x=infPerWeek$Date, y=infPerWeek$Value, ylab="", xlab="", pch=8, type="b", 
     col="red", xlim=as.Date(c("2020-02-01","2020-05-06")), ylim=c(0,300), 
     main ="Pandemieverlauf Deutschland Januar - Mai 2020")
lines(x=deathsPerWeek$Date, y=deathsPerWeek$Value, ylab="", xlab="", pch=3, 
      bg="black", type="b")
legend("topleft",legend=c("Wocheninzidenz","Tägliche Todesfälle in der Woche"), 
       col=c("red","black"), pch=c(8,3),lty=c(1,2), ncol=1)

#Plottet Akt 2
plot(x=infPerWeek$Date, y=infPerWeek$Value, ylab="", xlab="", pch=8, type="b", 
     col="red", xlim=as.Date(c("2020-05-07","2020-10-19")), ylim=c(0,250),
     main ="Pandemieverlauf Deutschland Mai - Oktober 2020")
lines(x=deathsPerWeek$Date, y=deathsPerWeek$Value, ylab="", xlab="", pch=3, 
      bg="black", type="b")
legend("topleft",legend=c("Wocheninzidenz","Tägliche Todesfälle in der Woche"), 
       col=c("red","black"), pch=c(8,3),lty=c(1,2), ncol=1)

#Plottet Akt 3
plot(x=infPerWeek$Date, y=infPerWeek$Value, ylab="", xlab="", pch=8, type="b", 
     col="red", xlim=as.Date(c("2020-10-20","2021-05-21")), ylim=c(0,1000),
     main ="Pandemieverlauf Deutschland Oktober 2020 - Mai 2021")
lines(x=deathsPerWeek$Date, y=deathsPerWeek$Value, ylab="", xlab="", pch=3, 
      bg="black", type="b")
legend("topright",legend=c("Wocheninzidenz","Tägliche Todesfälle in der Woche"), 
       col=c("red","black"), pch=c(8,3),lty=c(1,2), ncol=1)

#Plottet Akt 4
plot(x=infPerWeek$Date, y=infPerWeek$Value, ylab="", xlab="", pch=8, type="b", 
     col="red", xlim=as.Date(c("2021-05-22","2021-11-11")), ylim=c(0,500),
     main ="Pandemieverlauf Deutschland Mai - November 2021")
lines(x=deathsPerWeek$Date, y=deathsPerWeek$Value, ylab="", xlab="", pch=3,
      bg="black", type="b")
legend("topleft",legend=c("Wocheninzidenz","Tägliche Todesfälle in der Woche"), 
       col=c("red","black"), pch=c(8,3),lty=c(1,2), ncol=1)

#Plottet Akt 4
plot(x=infPerWeek$Date, y=infPerWeek$Value, ylab="", xlab="", pch=8, type="b", 
     col="red", xlim=as.Date(c("2021-11-12","2022-05-25")), ylim=c(0,2000),
     main ="Pandemieverlauf Deutschland November 2021 - Mai 2022")
lines(x=deathsPerWeek$Date, y=deathsPerWeek$Value, ylab="", xlab="", pch=3, 
      bg="black", type="b")
legend("topleft",legend=c("Wocheninzidenz","Tägliche Todesfälle in der Woche"), 
       col=c("red","black"), pch=c(8,3),lty=c(1,2), ncol=1)

