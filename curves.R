
library(ggplot2)

#Kurve detailliert
grassgrowth_curve <- ggplot(daten, aes(x = weeknum, y = growth, color = Ort)) +
  #geom_point(aes(shape=Ort), size=2) + 
  geom_point(size=1,  show.legend = TRUE) +
  geom_line(linewidth = 0.5) +
  #stat_summary(fun = "mean", geom = "line") +
  geom_line(stat = "summary", fun = "mean", linetype="dashed", color="black", 
            linewidth=1, aes(color="mean"), show.legend = F) +
  
  geom_line(data=standardkurven, aes(
            x = weeknum, 
            y = Durchschnitt...700.m.ü.M...tiefgründig..frisch
            ), color="red", linetype="dotted", linewidth=1, show.legend = F) +
  labs(x = "Kalenderwoche", y = "Graswachstum (kg TS/ha/Tag)",
            title = "Graswachstumskurven 2024") +
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

ggplotly(grassgrowth_curve, tooltip=c("Ort", "growth"))

#library(ggiraph)
#devtools::install_github("hrbrmstr/albersusa")

# Kurve vereinfacht
grassgrowth_curve <- ggplot(daten, aes(x = weeknum, y = growth, color = Ort)) +
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
       title = "Graswachstumskurven 2024") +
  #scale_colour_manual(name = "",
  #                    values = "Dodger Blue 3",
  #                   labels = "c") +
  #guides(color = "none", fill = "none") +
  xlim(0,52) +
  #geom_line(aes(y = durchschnitt), color = "black", size = 2, linetype = "dotted") +
  theme_minimal() +
  theme(legend.position = "right")

grassgrowth_curve
curvefile_simple <- paste("outputs/Graswachstumskurve_ohneLegende_", Jahr, ".svg", sep="")
ggsave(file=curvefile_simple, width=10, height=7.5)


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
