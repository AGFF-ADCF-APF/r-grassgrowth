# install.packages("tidyverse") # Kommentiere dies aus, wenn du das Paket noch nicht installiert hast
# install.packages("lubridate") # Kommentiere dies aus, wenn du das Paket noch nicht installiert hast
# install.packages("readr") # Kommentiere dies aus, wenn du das Paket noch nicht installiert hast

library(tidyverse)
library(lubridate)
library(readr)
library(splines)

df <- jahresdaten

# 2. Daten bereinigen, kumulierten Ertrag berechnen und filtern
df_processed <- df %>%
  # Konvertiere das Datum in das richtige Format
  mutate(date = ymd(date)) %>%
  # Gruppiere die Daten nach Standort
  group_by(Ort) %>%
  # Filtere Standorte, die die Kriterien erfüllen
  filter(
    # Kriterium 1: Mindestens eine Messung ab Mai
    any(date >= ymd("2025-04-01")),
    # Kriterium 2: Keine Messlücke grösser als 5 Wochen (35 Tage)
    max(as.numeric(date - lag(date, default = first(date))), na.rm = TRUE) <= 40, 
    # NEUES KRITERIUM 3: Daten vorhanden nach dem 1. August
    max(date) >= ymd("2025-08-01")
  ) %>%
  # Überprüfe, ob der erste growth-Wert bereits 0 ist. Wenn nicht, füge eine Nullzeile ein.
  do({
    if (first(.$growth) != 0) {
      first_date <- min(.$date)
      zero_date <- first_date - days(21)
      zero_point_df <- tibble(
        Ort = first(.$Ort),
        date = zero_date,
        growth = 0,
        # Weitere Spalten mit NA-Werten, um die Struktur zu erhalten
        place = NA_character_,
        lon = NA, lat = NA, afc = NA, ignore = NA, weeknum = NA, daysold = NA, year = NA
      )
      bind_rows(zero_point_df, .)
    } else {
      .
    }
  }) %>%
  group_by(Ort) %>%
  # Sortiere die Daten neu nach Datum
  arrange(date) %>%
  # Berechne die Differenz in Tagen und den kumulierten Ertrag
  mutate(
    days_diff = as.numeric(date - lag(date, default = first(date))),
    # Berechnung des inkrementellen Ertrags
    incremental_yield_kg = (growth + lag(growth, default = first(growth))) / 2 * days_diff
  ) %>%
  # Setze den ersten inkrementellen Ertrag auf 0
  mutate(
    incremental_yield_kg = replace(incremental_yield_kg, row_number() == 1, 0),
    # Kumulierten Ertrag in dt berechnen
    cumulative_yield_dt = cumsum(coalesce(incremental_yield_kg, 0)) / 100
  ) %>%
  ungroup()

print(df_processed)

# Datenrahmen mit den kumulierten Endwerten für die Textlabels
final_yield_labels <- df_processed %>%
  group_by(Ort) %>%
  filter(row_number() == n()) %>%
  ungroup()


# Filtern, um die Endwerte zu sehen
cumulative_yield_summary <- df_processed %>%
  group_by(Ort) %>%
  filter(row_number() == n()) %>% 
  select(Ort, cumulative_yield_dt)

print(cumulative_yield_summary)


# 3. Visualisierung mit Textlabels im Plot
## ERTRAGSZUWACHS
ggplot(df_processed, aes(x = date, y = cumulative_yield_dt)) +
  # Schattierte Fläche, die den kumulierten Ertrag visualisiert
  geom_ribbon(aes(ymin = 0, ymax = cumulative_yield_dt, fill = Ort), alpha = 0.3) +
  # Linien, die den kumulierten Ertrag zeigen
  geom_line(aes(color = Ort), size = 1.2) +
  # Punkte für die Messungen
  geom_point(aes(color = Ort), size = 3) +
  # Textlabel für den kumulierten Gesamtertrag
  geom_text(data = final_yield_labels, 
            aes(x = date, y = cumulative_yield_dt, 
                label = paste0(round(cumulative_yield_dt, 2), " dt TS")), 
            hjust = -0.1, vjust = -0.5, size = 4) +
  # Titel und Achsenbeschriftungen
  labs(
    title = "Kumulierte Grasertragsbildung pro Ort (gefilterte Daten)",
    subtitle = "Fläche und Textlabel repräsentieren den kumulierten Ertrag in dt TS",
    x = "Datum",
    y = "Kumulierter Ertrag (dt TS)"
  ) +
  # Ein Thema für bessere Optik
  theme_minimal() +
  facet_wrap(~Ort, scales = "free_y")




# Monatsdaten berechnen (Summe der inkrementellen Erträge pro Monat)
monthly_data <- df_processed %>%
  mutate(month_year = floor_date(date, "month")) %>%
  group_by(Ort, month_year) %>%
  summarise(monthly_yield_dt = sum(incremental_yield_kg, na.rm = TRUE) / 100,
            .groups = 'drop') %>%
  mutate(month_label = format(month_year, "%b %Y"))



## GRASWACHSTUMSKURVEN MIT ERTRAG nebeneinander
# Visualisierung des Graswachstums pro Ort
p <- ggplot(df_processed, aes(x = date, y = growth, fill = Ort)) +
  # Linien, um die Wachstumskurve zu zeigen
  #geom_line(aes(color = Ort), size = 1) +
  # Geglättete Kurve, die den Wachstumstrend darstellt
  #geom_smooth(aes(color = Ort), se = FALSE, span = 0.5, size = 1.5) +
  # Angepasste, geglättete Kurve, die keine negativen Werte annimmt
  # **Nur positive Werte für die Glättung verwenden**
  geom_smooth(data = filter(df_processed, cumulative_yield_dt > 0), 
              aes(color = Ort), 
              method = "glm", 
              formula = y ~ splines::ns(x, df = 7), 
              method.args = list(family = Gamma(link = "log")), 
              se = FALSE, 
              size = 1.5) +
  # Punkte für die Messungen
  geom_point(aes(color = Ort), size = 2) +
  # Schraffierte Fläche unter der Kurve
  geom_ribbon(aes(ymin = 0, ymax = growth, group = Ort), alpha = 0.3) +
  geom_text(data = final_yield_labels, 
            aes(x = as.Date("2025-04-15"), y = 2, 
                label = paste0(round(cumulative_yield_dt, 2), " dt TS/ha")), 
            hjust = -0.1, vjust = -0.5, size = 4) +
  # Titel und Achsenbeschriftungen
  labs(
    title = "Graswachstum pro Ort",
    subtitle = paste0("Mit Trendlinie und kumuliertem Ertrag bis KW", week," (schraffierte Fläche)."),
    x = "Datum",
    y = "Wachstum (kg TS/ha/Tag)"
  ) +
  # **Hinzufügen der monatlichen Gitterlinien**
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  # Sicherstellen, dass die y-Achse bei 0 beginnt
  scale_y_continuous(limits = c(0, NA)) +
  # Ein Thema für bessere Optik
  theme_minimal() +
  facet_wrap(~Ort, scales = "fixed")

print(p)
graswachstumskurven_file <- paste("outputs/Graswachstumskurven-mit-Ertrag_", Jahr, "_bis-KW",week, ".svg", sep="")
ggsave(file=graswachstumskurven_file, width=10, height=7.5)


## GRASWACHSTUMSKURVEN MIT ERTRAG nebeneinander
## nur Pitt-Käch
# Visualisierung des Graswachstums pro Ort
df_processed_1 <- df_processed %>% 
  filter(Ort == "Gampelen")
p <- ggplot(df_processed_1, aes(x = date, y = growth, fill = Ort)) +
  # Linien, um die Wachstumskurve zu zeigen
  #geom_line(aes(color = Ort), size = 1) +
  # Geglättete Kurve, die den Wachstumstrend darstellt
  #geom_smooth(aes(color = Ort), se = FALSE, span = 0.5, size = 1.5) +
  # Angepasste, geglättete Kurve, die keine negativen Werte annimmt
  # **Nur positive Werte für die Glättung verwenden**
  geom_smooth(data = filter(df_processed_1, cumulative_yield_dt > 0), 
              aes(color = Ort), 
              method = "glm", 
              formula = y ~ splines::ns(x, df = 7), 
              method.args = list(family = Gamma(link = "log")), 
              se = FALSE, 
              size = 1.5) +
  # Punkte für die Messungen
  geom_point(aes(color = Ort), size = 2) +
  # Schraffierte Fläche unter der Kurve
  labs(
    title = "Graswachstumskurve 2025 für Gampelen",
    subtitle = "Jahresbeginn bis 2. September 2025",
    x = "Datum",
    y = "Wachstum (kg TS/ha/Tag)"
  ) +
  # **Hinzufügen der monatlichen Gitterlinien**
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  # Sicherstellen, dass die y-Achse bei 0 beginnt
  scale_y_continuous(limits = c(0, NA)) +
  # Ein Thema für bessere Optik
  theme_minimal()

print(p)
graswachstumskurven_file_1 <- paste("outputs/Graswachstumskurven-mit-Ertrag_", Jahr, "_bis-KW",week, "_Gampelen.svg", sep="")
ggsave(file=graswachstumskurven_file_1, width=10, height=7.5)



## GRASWACHSTUMSKURVEN nebeneinander
grassgrowth_curve <- ggplot(df_processed, aes(x = weeknum, y = growth, color = Ort)) +
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
  theme(legend.position = "right") +   
  facet_wrap(~Ort, scales = "free_y")


ggplotly(grassgrowth_curve, tooltip=c("Ort", "growth"))






