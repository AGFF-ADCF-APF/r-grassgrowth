library(dplyr)
standorte <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSrcomkkwzl7-XESOTZLhk0XOCQMq5cz1kkcMif7sl8PGybv_nHK8ite3eMM_-UKLKC1hHEHVHlx_lc/pub?gid=1641924839&single=true&output=csv", skip = 0) %>% 
  select(Ort, Standort, lon, lat) %>% 
  rename(place=Standort)
graswachstum <- read.csv(
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vSrcomkkwzl7-XESOTZLhk0XOCQMq5cz1kkcMif7sl8PGybv_nHK8ite3eMM_-UKLKC1hHEHVHlx_lc/pub?gid=513247593&single=true&output=csv"
  ) %>% 
  select(Standort, Erhebungsdatum, Graswachstum..kg.TS.ha.Tag.) %>% 
  rename(growth=Graswachstum..kg.TS.ha.Tag., date=Erhebungsdatum, place=Standort)
standardkurven <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSrcomkkwzl7-XESOTZLhk0XOCQMq5cz1kkcMif7sl8PGybv_nHK8ite3eMM_-UKLKC1hHEHVHlx_lc/pub?gid=1040229080&single=true&output=csv") 


# Kombinieren Sie Standortdaten und Graswachstumsdaten
daten <- merge(standorte, graswachstum, by = "place")
daten <- daten %>% mutate(Ort = factor(Ort))

daten$date <- as.Date(daten$date,format="%d.%m.%Y")
daten$weeknum <- as.integer(strftime(daten$date, format = '%V'))

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
