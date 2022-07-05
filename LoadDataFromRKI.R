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
hospitalizations$Datum <- as.Date(hospitalizations$Datum, format = "%Y-%m-%d")
hospitalizations$Bundesland <- factor(hospitalizations$Bundesland)
hospitalizations$Bundesland_Id <- str_pad(hospitalizations$Bundesland_Id, 2, pad = "0")
hospitalizations$Bundesland_Id <- factor(hospitalizations$Bundesland_Id)
hospitalizations$Altersgruppe <- factor(hospitalizations$Altersgruppe)


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


# Check, ob zu allen Landkreisen mit Impfung / Infektion auch LK-Daten vorliegen
LK_IDs_Inf <- levels(infections$IdLandkreis)
print(relcomp(LK_IDs_Inf,kreisInfos$Kennzahl))
print(relcomp(kreisInfos$Kennzahl, LK_IDs_Inf))
length(kreisInfos$Kennzahl)
length(LK_IDs_Inf)

dates <- function(x) dimnames(x)[[1]]
