# !Make sure that you've executed LoadDataFromRKI.R before you execute this skript!

# Infektionen gruppieren nach Wochen
infections$week <- floor_date(infections$Refdatum, "week")
vaccinations$week <- floor_date(vaccinations$Impfdatum, "week")

# Infektionen pro Woche addieren (alle Kreise)
infPerWeek = data.frame(tapply(X=infections$AnzahlFall/837.56, INDEX=infections$week, FUN=sum));
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

# Plots für "Gefangene der Gegenwart" 
# Plottet gesamten Zeitraum 
plot(x=infPerWeek$Date, y=infPerWeek$Value, ylab="", xlab="", pch=8, type="b", 
     col="red" ,main ="Pandemieverlauf Deutschland Januar 2020 - Mai 2022")
lines(x=deathsPerWeek$Date, y=deathsPerWeek$DailyDeaths, ylab="", xlab="", pch=3, 
      bg="black", type="b")
# lines(x=vaccsPerWeek$Date, y=vaccsPerWeek$Value/10000, ylab="", xlab="", pch=8, type="b", 
#       col="darkgreen" )
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

