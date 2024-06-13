week <- 7

olddaten <- daten  %>% group_by(place) %>%
  filter(weeknum == week) %>% 
  select(-daysold) %>%
  group_by(place) %>%
  filter(date == max(date))

maxdaten <- olddaten

Jahr <- strftime(maxdaten$date[1], format = "%Y")
#Datum <- as.Date(week, format="%V")
Datum = ""
Kalenderwoche <- paste("KW ",week,Datum,sep="") 



grassgrowth_map <- ggplot() +
  geom_sf(data = swk_shp, fill="#b6d69a", color = "white") +
  geom_sf(data = swl_shp, fill="#acd2ef", color = "#acd2ef") +
  #geom_point(data = daten, aes(x = lon, y = lat, color = growth), size = 5) +
  #scale_color_gradient(low = "blue", high = "red", name = "kg TS/ha/Tag") +
  #geom_text(data = maxdaten, aes(x = lon, y = lat, label = round(growth,0)), 
  #         color = "black", size = 4, nudge_y = 0.05, nudge_x = 0.07) +
  geom_point(data = maxdaten, aes(x = lon, y = lat), shape=21, 
           color="black", stroke=1.5, size=8, show.legend=T) +
  #scale_fill_manual(values=c("#999999", "#E69F00")) +
  scale_fill_gradient(name = "Tage seit\n Messung", high="gray46", low="white", 
                      guide = guide_legend(theme = theme(
                      legend.direction = "vertical",
                      legend.title.position = "top",
                      legend.text.position = "right",
                      #legend.text = element_text(hjust = 0.5, vjust = 1, angle = 0)
       ))) +
  geom_text(data = maxdaten, aes(x = lon, y = lat, label = round(growth,0)), fontface="bold",size=3.5 ) +
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
  labs(title = paste("Graswachstum ", Kalenderwoche, " ", Jahr, sep=""), subtitle = "Zuwachs in kg TS/ha/Tag, Quelle: AGFF-Grasmessnetzwerk")+
  theme_void()


grassgrowth_map
mapfile <- paste("outputs/Graswachstum_", Jahr, "KW",week, ".svg", sep="")
ggsave(file=mapfile, width=10, height=7.5)

mapfile_ftp <- paste("Graswachstumkarte_", Jahr, "KW",week, ".svg", sep="")
ftpurl <- paste("ftp://",ftpuser,":",ftppasswd,"@",ftpserver,"/archive/",mapfile_ftp,sep="")
ftpUpload(mapfile,ftpurl)



#ggplotly(grassgrowth_map, tooltip=c("growth"))


