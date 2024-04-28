# Install and load necessary packages
#install.packages(c("ggplot2", "ggmap"))
library(ggplot2)
library(sf)
library(ggswissmaps)
library(ggrepel)
library(plotly)



data("shp_df")
sw_shp <- split(shp_df[["g1k15"]], shp_df[["g1k15"]]$id) |>
  lapply(function(x) list(as.matrix(subset(x, piece == 1)[1:2]))) |>
  sf::st_multipolygon() |>
  sf::st_sfc(crs = 21781) |>
  sf::st_transform(crs = 'WGS84')


today <- Sys.Date()
week <- as.integer(strftime(today, format = "%V"))
#week <- 14
weeks <- c(week, week-1)
weeks <- c(week, week-1)
#weeks <- c(week)



currentdaten <- daten  %>% group_by(place) %>%
  filter(weeknum %in% weeks)

maxdaten <- currentdaten  %>% group_by(place) %>%
  filter(date == max(date))

Jahr <- strftime(maxdaten$date[1], format = "%Y")
#Datum <- as.Date(week, format="%V")
Datum = ""
Kalenderwoche <- paste("Kalenderwoche",week,Datum ) 


grassgrowth_map <- ggplot() +
  geom_sf(data = sw_shp, fill="#b6d69a", color = "white") +
  #geom_point(data = daten, aes(x = lon, y = lat, color = growth), size = 5) +
  #scale_color_gradient(low = "blue", high = "red", name = "kg TS/ha/Tag") +
  #geom_text(data = maxdaten, aes(x = lon, y = lat, label = round(growth,0)), 
  #         color = "black", size = 4, nudge_y = 0.05, nudge_x = 0.07) +
  geom_point(data = maxdaten, aes(x = lon, y = lat,fill=weeknum), shape=21, 
           color="black", stroke=1.5, size=8) +
  #scale_fill_manual(values=c("#999999", "#E69F00")) +
  scale_fill_gradient(low="gray46", high="white") +
  geom_text(data = maxdaten, aes(x = lon, y = lat, label = round(growth,0)), fontface="bold" ) +
  #geom_sf_text(data = maxdaten, aes(label = Ort)) +
  geom_text(data = maxdaten, aes(x = lon, y = lat, label = Ort), 
            fontface="bold", nudge_x = 0.35) +
  labs(title = paste("Graswachstum",Jahr), subtitle = Kalenderwoche ) +
  theme_void()

grassgrowth_map
mapfile <- paste("outputs/Graswachstum_", Jahr, "KW",week, ".svg", sep="")
ggsave(file=mapfile, width=10, height=7.5)


ggplotly(grassgrowth_map, tooltip=c("growth"))


