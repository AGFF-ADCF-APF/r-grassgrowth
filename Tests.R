library(raster)
library(geodata)
library(ggmap)
library(leaflet)
library(ggswissmaps)

df %>%
  group_by(age) %>%
  summarise(mean_score = mean(score)) %>%
  ggplot(aes(x = factor(age), y = mean_score)) +
  geom_col() +
  labs(x = "Age", y = "Mean score")

# Erstellen Sie eine Karte mit ggplot2
ggplot(daten, aes(x = lon, y = lat, color = growth)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "blue", high = "red", name = "Graswachstum (kg TS/ha/Tag)") +
  labs(title = "Graswachstum in der Schweiz", subtitle = "Erhebungsdaten von 2024") +
  theme_minimal()


# Create a leaflet map
map <- leaflet(daten) %>%
  addTiles() %>%
  addMarkers(
    lng = ~lon,
    lat = ~lat,
    popup = ~paste("Standort: ", Standort, "<br>Graswachstum: ", Graswachstum, " kg TS/ha/Tag")
  )
print(map)
maps2[["g1s15"]]
