




library(ggplot2)

#Kurve detailliert
grassgrowth_curve <- ggplot(jahresdaten, aes(x = weeknum, y = growth, color = Ort)) +
  #geom_point(aes(shape=Ort), size=2) + 
  geom_point(size=1,  show.legend = TRUE, na.rm=TRUE) +
  geom_line(linewidth = 0.5, na.rm=TRUE) +
  #stat_summary(fun = "mean", geom = "line") +
  geom_line(stat = "summary", fun = "mean", linetype="dashed", color="black", 
            linewidth=1, aes(color="mean"), show.legend = F, na.rm=TRUE) +
  
  geom_line(data=standardkurven, aes(
            x = weeknum, 
            y = Durchschnitt...700.m.ü.M...tiefgründig..frisch
            ), color="red", linetype="dotted", linewidth=1, show.legend = F) +
  labs(x = "Kalenderwoche", y = "Graswachstum (kg TS/ha/Tag)",
            title = paste0("Graswachstumskurven ",Jahr)) +
  #scale_colour_manual(name = "",
  #                    values = "Dodger Blue 3",
  #                   labels = "c") +
  #guides(color = "none", fill = "none") +
  xlim(0,52) +
  #geom_line(aes(y = durchschnitt), color = "black", size = 2, linetype = "dotted") +
  theme_minimal() +
  theme(legend.position = "right")


grassgrowth_curve
curvefile <- paste("outputs/Graswachstumskurve_", Jahr, ".svg", sep="")

ggsave(file=curvefile, width=10, height=7.5)
# Stabil benannte Kopie ("aktuell") - fuer die statische Vorschau im
# Datenexplorer-Wrapper (siehe 27_plot_datenexplorer.R), die anders als die
# Jahres-Datei oben nicht jedes Jahr umbenannt wird.
file.copy(curvefile, "outputs/Graswachstumskurve_aktuell.svg", overwrite = TRUE)

ggplotly(grassgrowth_curve, tooltip=c("Ort", "growth"))

#library(ggiraph)
#devtools::install_github("hrbrmstr/albersusa")

# Kurve vereinfacht
grassgrowth_curve <- ggplot(jahresdaten, aes(x = weeknum, y = growth, color = Ort)) +
  #geom_point(aes(shape=Ort), size=2) + 
  #geom_point(size=1,  show.legend = FALSE) +
  geom_line(linewidth = 0.5, show.legend=F) +
  #stat_summary(fun = "mean", geom = "line") +
  geom_line(stat = "summary", fun = "mean", linetype="dashed", color="black", 
            linewidth=1, aes(color="mean"), show.legend = F) +
  
  geom_line(data=standardkurven, aes(
    x = weeknum, 
    y = Durchschnitt...700.m.ü.M...tiefgründig..frisch
  ), color="red", linetype="dotted", linewidth=1, show.legend = F) +
  labs(x = "Kalenderwoche", y = "Graswachstum (kg TS/ha/Tag)",
       title = paste0("Graswachstumskurven ",Jahr)) +
  #scale_colour_manual(name = "",
  #                    values = "Dodger Blue 3",
  #                   labels = "c") +
  #guides(color = "none", fill = "none") +
  xlim(0,52) +
  #geom_line(aes(y = durchschnitt), color = "black", size = 2, linetype = "dotted") +
  theme_minimal() +
  theme(legend.position = "right")

grassgrowth_curve
curvefile_simple <- paste0("outputs/Graswachstumskurve_ohneLegende_", Jahr, ".svg")
ggsave(file=curvefile_simple, width=10, height=7.5)


# plotly -----------------
library(tidyr)
library(plotly);

#fstr(daten)
# df <- as_tibble(daten)
# str(df)
# df <- df %>% select(Ort, weeknum, growth)
# dfw <- reshape(df, idvar = "Ort", timevar = "weeknum", direction = "wide")
# 
# str(dfw)
# View(dfw)
# plot(daten$weeknum, daten$growth)
# 
# plot(daten, x=growth, y=weeknum)
# durchschnitt <- rowMeans(data.frame(standort_a, standort_b, standort_c))
