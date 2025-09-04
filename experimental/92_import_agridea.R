

# 1. Bibliotheken laden
# readxl wird für das Einlesen von Excel-Dateien benötigt
library(tidyverse)
library(lubridate)
library(readxl)

# ---
# 2. Daten direkt aus der Excel-Datei laden
# ---

# Dateinamen und die Namen der relevanten Tabellenblätter definieren
excel_file <- "inputs/2025_Messdaten_Herbommeter.xlsx"
messungen_sheets <- c("APRIL - JUNI", "JULI-AUGUST", "SEPTEMBER -NOVEMBER")

# Messdaten aus den definierten Tabellenblättern laden und kombinieren
# Wir verwenden `read_excel` anstelle von `read_csv`
messungen_roh <- map_dfr(
  messungen_sheets,
  ~read_excel(excel_file, sheet = .x, skip = 2, col_types = "text")
)

# Spaltennamen bereinigen und festlegen
colnames(messungen_roh) <- c("Parzelle", "KW", "Datum", "Anzahl_Messungen", "clics", "Beweidet", "Ertrag_kg_TS", "Summe_Ertrag", "Summe_Flaeche")


# ---
# 3. Datenbereinigung und -transformation (unverändert)
# ---

messungen <- messungen_roh %>%
  # Nur die relevanten Spalten auswählen
  select(Parzelle, Datum, clics, Beweidet) %>%
  # Excel importiert Daten manchmal als Datum-Zeit-Format, wir stellen sicher, dass es nur ein Datum ist.
  # POSIXct to Date. Und Text-Zahlen (z.B. 45395) zu Datum
  mutate(
    Datum = as.Date(as.numeric(Datum), origin = "1899-12-30"),
    clics = as.numeric(clics),
    Beweidet = str_trim(toupper(Beweidet)) # Leerzeichen entfernen und zu Grossbuchstaben
  ) %>%
  # Zeilen ohne gültiges Datum oder clics-Wert entfernen
  drop_na(Datum, clics)

# Nutzbare Futtermenge berechnen
messungen <- messungen %>%
  mutate(
    Nutzbare_Futtermenge = pmax(0, (140 * clics) - 980) # pmax stellt sicher, dass der Wert nicht negativ wird
  )

# ---
# 4. Graswachstum berechnen (unverändert)
# ---

# Daten sortieren und vorherige Messwerte für die Berechnung holen
wachstum_df <- messungen %>%
  arrange(Parzelle, Datum) %>%
  group_by(Parzelle) %>%
  mutate(
    Datum_von = lag(Datum, 1),
    clics_prev = lag(clics, 1),
    Beweidet_prev = lag(Beweidet, 1),
    NFM_prev_raw = pmax(0, (140 * clics_prev) - 980)
  ) %>%
  ungroup() %>%
  # Erste Messung jeder Parzelle entfernen (hat keine "Datum_von")
  drop_na(Datum_von) %>%
  # Tage zwischen den Messungen berechnen
  mutate(
    Tage = as.numeric(Datum - Datum_von),
    # Start-Futtermenge auf 0 setzen, wenn vorher beweidet wurde
    NFM_prev = if_else(Beweidet_prev == "X", 0, NFM_prev_raw, missing = NFM_prev_raw),
    # Täglichen Zuwachs berechnen
    Graszuwachs_pro_Tag = (Nutzbare_Futtermenge - NFM_prev) / Tage
  )

# ---
# 5. Ergebnisse filtern gemäss Anforderungen (unverändert)
# ---

wachstum_final <- wachstum_df %>%
  filter(
    is.na(Beweidet) | Beweidet != "X",
    Graszuwachs_pro_Tag >= 0
  )

# ---
# 6. Finale Tabellen erstellen (unverändert)
# ---

# Tabelle 1: Detailansicht pro Parzelle
tabelle1 <- wachstum_final %>%
  select(
    Parzelle,
    `Datum von` = Datum_von,
    `Datum bis` = Datum,
    Tage,
    `Grashöhe am letzten Tag (clics)` = clics,
    `Futtermenge am letzten Tag (kg TS/ha)` = Nutzbare_Futtermenge,
    `Graszuwachs pro Tag (kg TS/ha/Tag)` = Graszuwachs_pro_Tag
  )

# Tabelle 2: Wöchentliche Zusammenfassung
avg_wachstum <- wachstum_final %>%
  group_by(`Datum Messung` = Datum) %>%
  summarise(
    `Durchschnittliches Graswachstum seit letzter Messung (kg TS/ha/Tag)` = mean(Graszuwachs_pro_Tag, na.rm = TRUE)
  )

avg_farm_cover <- messungen %>%
  group_by(`Datum Messung` = Datum) %>%
  summarise(
    `Durchschnittlicher nutzbarer Grasvorrat (kg TS/ha)` = mean(Nutzbare_Futtermenge, na.rm = TRUE)
  )

tabelle2 <- left_join(avg_wachstum, avg_farm_cover, by = "Datum Messung") %>%
  arrange(`Datum Messung`)

# ---
# 7. Ergebnisse ausgeben (unverändert)
# ---

print("--- Tabelle 1: Nutzbare Futtermenge und Graswachstum pro Parzelle (nur positiver Zuwachs) ---")
print(tabelle1)

print("--- Tabelle 2: Wöchentliche Zusammenfassung des Betriebs (nur positiver Zuwachs) ---")
print(tabelle2, n = 100)


# ---
# 8. Graswachstumskurve mit ggplot2 erstellen (unverändert)
# ---

wachstumskurve <- ggplot(tabelle2, aes(x = `Datum Messung`, y = `Durchschnittliches Graswachstum seit letzter Messung (kg TS/ha/Tag)`)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "darkgreen", size = 3) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(
    title = "Durchschnittliches Graswachstum pro Woche",
    subtitle = "Berechnet aus den Messungen von April bis September 2025",
    x = "Datum der Messung",
    y = "Graszuwachs (kg TS/ha/Tag)"
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(wachstumskurve)



