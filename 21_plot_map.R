# Install and load necessary packages
#install.packages(c("ggplot2", "ggmap"))
#devtools::install_github("gibonet/ggswissmaps", force=T)
library(ggplot2)
library(sf)
library(ggswissmaps)
library(ggrepel)
library(plotly)
library(packcircles)


data(shp_sf)

swk_shp <- shp_sf[["g1k15"]] %>%
  st_as_sfc() |>
  sf::st_sfc(crs = 21781) |>
  sf::st_transform(crs = 'WGS84')

swl_shp <- shp_sf[["g1s15"]] %>%
  st_as_sfc() |>
  sf::st_sfc(crs = 21781) |>
  sf::st_transform(crs = 'WGS84')


set.seed(42)

grassgrowth_map <- ggplot() +
  geom_sf(data = swk_shp, fill="#b6d69a", color = "white") +
  geom_sf(data = swl_shp, fill="#acd2ef", color = "#acd2ef") +
  #geom_point(data = daten, aes(x = lon, y = lat, color = growth), size = 5) +
  #scale_color_gradient(low = "blue", high = "red", name = "kg TS/ha/Tag") +
  #geom_text(data = maxdaten, aes(x = lon, y = lat, label = round(growth,0)), 
  #         color = "black", size = 4, nudge_y = 0.05, nudge_x = 0.07) +
  geom_point(data = maxdaten, aes(x = lon, y = lat, fill=daysold), shape=21, 
           color="black", stroke=1.5, size=8, show.legend=T) +
  #scale_fill_manual(values=c("#999999", "#E69F00")) +
  scale_fill_gradient(name = "Tage seit\n Messung", high="gray46", low="white", 
                      guide = guide_legend(theme = theme(
                      legend.direction = "vertical",
                      legend.title.position = "top",
                      legend.text.position = "right",
                      #breaks = c(3,7,10,14)
                      
                      #legend.text = element_text(hjust = 0.5, vjust = 1, 
                      #angle = 0)
       ))) +
  geom_text(data = maxdaten, aes(x = lon, y = lat, label = round(growth,0)), 
            fontface="bold",size=3.5 ) +
  #geom_sf_text(data = maxdaten, aes(label = Ort)) +
  geom_text_repel(data = maxdaten, aes(x = lon, y = lat, label = Ort), 
            fontface="bold", 
            nudge_x =0.1, 
            #point.padding=NA,
            # force = 0,
            # #force_pull = 0,
             point.padding = 1,
             box.padding = 0.7,
             min.segment.length = 3,
            max.overlaps = 50,
            # direction = "both"
            ) +
  #guides(fill = FALSE, alpha = FALSE) + 
  labs(title = paste("Graswachstum ", strftime(today, format = "%a %d.%m.%Y"), 
                     ", ", Kalenderwoche, " ", Jahr, sep=""), 
       subtitle = "Zuwachs in kg TS/ha/Tag, Quelle: AGFF-Grasmessnetzwerk")+
  theme_void()


grassgrowth_map
mapfile <- paste("outputs/Graswachstum_", Jahr, "KW",week, ".svg", sep="")
ggsave(file=mapfile, width=10, height=7.5)



#ggplotly(grassgrowth_map, tooltip=c("growth"))


