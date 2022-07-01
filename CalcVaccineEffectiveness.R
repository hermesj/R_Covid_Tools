# !Make sure that you executed LoadDataFromRKI.R before you execute this skript!

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

