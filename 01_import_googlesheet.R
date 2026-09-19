#library(dplyr)
library(dplyr)


# standorte2024 <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSrcomkkwzl7-XESOTZLhk0XOCQMq5cz1kkcMif7sl8PGybv_nHK8ite3eMM_-UKLKC1hHEHVHlx_lc/pub?gid=1641924839&single=true&output=csv", skip = 0) %>% 
#   select(Ort, Standort, lon, lat) %>% 
#   rename(place=Standort)
# graswachstum2024 <- read.csv(
#   "https://docs.google.com/spreadsheets/d/e/2PACX-1vSrcomkkwzl7-XESOTZLhk0XOCQMq5cz1kkcMif7sl8PGybv_nHK8ite3eMM_-UKLKC1hHEHVHlx_lc/pub?gid=513247593&single=true&output=csv"
#   ) %>% 
#   select(Standort, Erhebungsdatum, Graswachstum..kg.TS.ha.Tag., AFC.Average.Farm.Cover.AFC..kg.TS.ha.) %>% 
#   rename(growth=Graswachstum..kg.TS.ha.Tag., afc=AFC.Average.Farm.Cover.AFC..kg.TS.ha.,date=Erhebungsdatum, place=Standort)

standorte <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vS0e9CDB7EvsOzwUo6gs5G4WvdXewECJIVGy8tgdjl7za-Zv25zQsEVuJoPk6bI8SwhYwP20y6Ky9Gq/pub?gid=1641924839&single=true&output=csv", skip = 0) %>%
  select(Ort, Standort, lon, lat, müM) %>%
  rename(place=Standort, masl=müM) %>%
  # Schweizer Tausender-Trennzeichen (Apostroph, z.B. "1'059") vor
  # as.numeric() entfernen - liefert direkt die Standort-Hoehe aus dem
  # Sheet, ohne fuer jeden Standort einzeln die swisstopo-Hoehen-API
  # abzufragen (siehe 26_.../27_...R).
  mutate(masl = as.numeric(gsub("'", "", masl)))

graswachstum <- read.csv(
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vS0e9CDB7EvsOzwUo6gs5G4WvdXewECJIVGy8tgdjl7za-Zv25zQsEVuJoPk6bI8SwhYwP20y6Ky9Gq/pub?gid=339537904&single=true&output=csv"
) %>% 
  select(Standort, Erhebungsdatum, Graswachstum..kg.TS.ha.Tag., AFC.Average.Farm.Cover.AFC..kg.TS.ha., ignore) %>% 
  rename(growth=Graswachstum..kg.TS.ha.Tag., afc=AFC.Average.Farm.Cover.AFC..kg.TS.ha.,date=Erhebungsdatum, place=Standort)



standardkurven <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSrcomkkwzl7-XESOTZLhk0XOCQMq5cz1kkcMif7sl8PGybv_nHK8ite3eMM_-UKLKC1hHEHVHlx_lc/pub?gid=1040229080&single=true&output=csv") 

standorte <- standorte %>% filter(place != "Sorens, école (CDAX)") %>% filter(place != "Posieux, Grangeneuve IAG (RPM)")

# Daten nachbearbeiten und ignore-Einträge filtern
graswachstum <- graswachstum %>% filter(place != "Sorens, école (CDAX)") %>% filter(place != "Posieux, Grangeneuve IAG (RPM)")
graswachstum <- graswachstum %>% filter(is.na(ignore))
graswachstum %>% distinct(place)
graswachstum %>% filter(ignore == T)


# Kombinieren Sie Standortdaten und Graswachstumsdaten
daten <- merge(standorte, graswachstum, by = "place")
daten <- daten %>% mutate(Ort = factor(Ort)) 

daten$date <- as.Date(daten$date,format="%d.%m.%Y")
daten$weeknum <- as.integer(strftime(daten$date, format = '%V'))

# Doppelte Eintraege (derselbe Standort UND dasselbe Datum) kommen im Sheet
# gelegentlich vor (z.B. versehentlich zweimal erfasste Erhebung, teils mit
# unterschiedlichen Werten) und wuerden sonst ueberall, wo "daten" verwendet
# wird (Karte, Kurve, Datenexplorer), zwei Punkte fuer denselben Standort am
# selben Tag erzeugen. Nur eine Warnung mit den betroffenen Zeilen - die
# eigentliche Korrektur/Entscheidung, welcher Wert stimmt, gehoert ins Sheet
# (z.B. per "ignore"-Spalte, siehe oben bei graswachstum) - hier wird
# defensiv nur die ERSTE Zeile behalten statt beide anzuzeigen.
duplikate <- daten %>% group_by(place, date) %>% filter(n() > 1) %>% ungroup()
if (nrow(duplikate) > 0) {
  warning("Doppelte Eintraege (Standort + Datum) im Sheet gefunden - bitte pruefen/bereinigen (z.B. per ignore-Spalte):", call. = FALSE)
  print(duplikate %>% select(place, Ort, date, growth, afc) %>% arrange(place, date))
}
daten <- daten %>% group_by(place, date) %>% filter(row_number() == 1) %>% ungroup()



today <- Sys.Date()
week <- as.integer(strftime(today, format = "%V"))
#week <- 17
weeks <- c(week, week-1)
weeks <- c(week, week-1)
daten$daysold <- today - daten$date
#weeks <- c(week)


currentdaten <- daten  %>% group_by(place) %>%
  filter(daysold < 16) %>%
  group_by(place) %>%
  filter(date == max(date))


maxdaten <- currentdaten

maxdaten <- maxdaten %>%
  mutate(afc = case_when(
    Ort == "Les Reusilles" ~ afc - 1500,
    TRUE  ~ afc, 
  ))


Jahr <- strftime(maxdaten$date[1], format = "%Y")
#Datum <- as.Date(week, format="%V")
Datum = ""
Kalenderwoche <- paste("KW ",week,Datum,sep="") 

#FIXME
#Jahr = 2024
#Jahr = 2025
daten$year <-  format(as.Date(daten$date, format="%d/%m/%Y"),"%Y")
jahresdaten <- daten %>% filter(year == Jahr)
#FIXME

jahresdaten <- jahresdaten %>%
  mutate(afc = case_when(
    Ort == "Les Reusilles" ~ afc - 1500,
    TRUE  ~ afc, 
  ))


jahresdaten <- jahresdaten %>%
  arrange(Ort, date)



#daten_std <- standardkurven %>% mutate(place="Durchschnitt_", p)
  
  
#daten <- append(daten, standardkurven)

# #Testdaten
# # Standortdaten (Längengrad, Breitengrad)
# standorte <- data.frame(
#   Name = c("Carrouge, Bourgeois", "Sorens, Agroscope", "Gampelen, Pitt+Käch", ...),  # Fügen Sie die restlichen Standorte hinzu
#   lon = c(6.78499, 7.05184, 7.07273),  # Fügen Sie die restlichen Längengrade hinzu
#   lat = c(46.60420, 46.66269, 47.00703)  # Fügen Sie die restlichen Breitengrade hinzu
# )
# 
# # Graswachstumsdaten
# graswachstum <- data.frame(
#   Standort = c("Muri AG, Malurahof", "Flawil, Mattenhof", "Gampelen, Pitt+Käch"),  # Fügen Sie die restlichen Standorte hinzu
#   Datum = as.Date(c("2024-01-23", "2024-02-07", "2024-03-12")),  # Fügen Sie die restlichen Erhebungsdaten hinzu
#   Wachstum = c(0, 0, 0)  # Fügen Sie die restlichen Graswachstumsdaten hinzu
# )

