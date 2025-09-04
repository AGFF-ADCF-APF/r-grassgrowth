# Benötigte Bibliotheken laden
# Falls nicht installiert, führen Sie aus: install.packages(c("readxl", "dplyr", "tidyr", "lubridate", "plotly"))
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(plotly)

# --- 1. Daten einlesen (Korrigierte, robuste Methode) ---
# Annahme: Die Excel-Datei heisst "2025_Messdaten_Herbommeter.xlsx" 
# und befindet sich im selben Verzeichnis wie das Skript.
excel_file <- "inputs/2025_Messdaten_Herbommeter.xlsx"

# Namen der relevanten Tabellenblätter (Sheets)
sheet_names <- excel_sheets(excel_file)
data_sheets <- sheet_names[!grepl("Parzellenplan", sheet_names, ignore.case = TRUE)]

# Alle relevanten Sheets einlesen, wobei alle Spalten als Text eingelesen werden, um Typenkonflikte zu vermeiden.
df_raw <- lapply(data_sheets, function(sheet) {
  read_excel(excel_file, sheet = sheet, skip = 2, col_types = "text")
}) %>% 
  bind_rows()

# --- 2. Datenaufbereitung ---
# Spaltennamen bereinigen und auswählen
df_clean <- df_raw %>%
  # Spalten umbenennen und auswählen (Namen könnten leicht variieren)
  select(
    Parzelle = 1, # Erste Spalte
    Datum = 3,    # Dritte Spalte
    clics = 5,    # Fünfte Spalte
    beweidet = 6  # Sechste Spalte
  ) %>%
  # Parzellennamen nach unten auffüllen
  fill(Parzelle, .direction = "down") %>%
  # Datentypen korrekt konvertieren
  mutate(
    # Konvertiere die Text-Spalte 'Datum' in ein echtes Datum-Objekt. 
    # ymd() von lubridate ist flexibel bei der Erkennung. Fehler werden zu NA.
    Datum = ymd(Datum, quiet = TRUE), 
    clics = as.numeric(clics),
    beweidet = tolower(trimws(beweidet))
  ) %>%
  # Zeilen ohne gültiges Datum oder Clics-Wert entfernen
  filter(!is.na(Datum) & !is.na(clics)) %>%
  # Beweidete Parzellen für das jeweilige Datum herausfiltern
  filter(beweidet != "x" | is.na(beweidet))


# --- 3. Berechnungen (unverändert) ---
# Nutzbare Futtermenge und Graszuwachs berechnen
df_growth <- df_clean %>%
  mutate(Futtermenge_nutzbar = 140 * clics + 500 - 1480) %>%
  arrange(Parzelle, Datum) %>%
  group_by(Parzelle) %>%
  mutate(
    Datum_von = lag(Datum, 1),
    Futtermenge_vorher = lag(Futtermenge_nutzbar, 1)
  ) %>%
  filter(!is.na(Datum_von)) %>%
  mutate(
    Tage = as.numeric(Datum - Datum_von),
    Graszuwachs_pro_Tag = (Futtermenge_nutzbar - Futtermenge_vorher) / Tage
  ) %>%
  filter(Tage > 0) %>%
  ungroup()

# --- 4. Tabelle 1: Graswachstum pro Parzelle (unverändert) ---
table1 <- df_growth %>%
  select(
    Parzelle,
    `Datum von` = Datum_von,
    `Datum bis` = Datum,
    Tage,
    `Grashöhe am letzten Tag (clics)` = clics,
    `Futtermenge am letzten Tag (kg TS/ha)` = Futtermenge_nutzbar,
    `Graszuwachs pro Tag (kg TS/ha/Tag)` = Graszuwachs_pro_Tag
  ) %>%
  mutate(
    `Futtermenge am letzten Tag (kg TS/ha)` = round(`Futtermenge am letzten Tag (kg TS/ha)`, 0),
    `Graszuwachs pro Tag (kg TS/ha/Tag)` = round(`Graszuwachs pro Tag (kg TS/ha/Tag)`, 1)
  )

# --- 5. Tabelle 2: Wöchentliche Zusammenfassung (unverändert) ---
avg_growth_summary <- df_growth %>%
  filter(Graszuwachs_pro_Tag >= 0) %>%
  group_by(Datum) %>%
  summarise(Avg_Graswachstum = mean(Graszuwachs_pro_Tag, na.rm = TRUE))

avg_farm_cover_summary <- df_clean %>%
  mutate(Futtermenge_nutzbar = 140 * clics + 500 - 1480) %>%
  group_by(Datum) %>%
  summarise(Avg_Farm_Cover = mean(Futtermenge_nutzbar, na.rm = TRUE))

table2 <- left_join(avg_growth_summary, avg_farm_cover_summary, by = "Datum") %>%
  rename(
    `Datum Messung` = Datum,
    `Durchschnittliches Graswachstum seit letzter Messung (kg TS/ha/Tag)` = Avg_Graswachstum,
    `Durchschnittlicher nutzbarer Grasvorrat (kg TS/ha)` = Avg_Farm_Cover
  ) %>%
  mutate(
    `Durchschnittliches Graswachstum seit letzter Messung (kg TS/ha/Tag)` = round(`Durchschnittliches Graswachstum seit letzter Messung (kg TS/ha/Tag)`, 1),
    `Durchschnittlicher nutzbarer Grasvorrat (kg TS/ha)` = round(`Durchschnittlicher nutzbarer Grasvorrat (kg TS/ha)`, 0)
  ) %>%
  # Entferne Zeilen, in denen keine Berechnungen möglich waren
  filter(!is.na(`Durchschnittliches Graswachstum seit letzter Messung (kg TS/ha/Tag)`))


# --- 6. Ergebnisse ausgeben (unverändert) ---
print("Tabelle 1: Graswachstum pro Parzelle (erste 6 Zeilen)")
print(head(table1))

print("Tabelle 2: Wöchentliche Zusammenfassung (erste 6 Zeilen)")
print(head(table2))

# Optional: Tabellen als CSV-Dateien speichern
# write.csv(table1, "graswachstum_parzellen.csv", row.names = FALSE, fileEncoding = "UTF-8")
# write.csv(table2, "wochenuebersicht_graswachstum.csv", row.names = FALSE, fileEncoding = "UTF-8")


# --- 7. Interaktive Graswachstumskurve erstellen (unverändert) ---
plot <- plot_ly(data = table2, x = ~`Datum Messung`) %>%
  add_trace(
    y = ~`Durchschnittliches Graswachstum seit letzter Messung (kg TS/ha/Tag)`,
    type = 'scatter',
    mode = 'lines+markers',
    name = 'Graswachstum (kg TS/ha/Tag)',
    yaxis = "y1"
  ) %>%
  add_trace(
    y = ~`Durchschnittlicher nutzbarer Grasvorrat (kg TS/ha)`,
    type = 'scatter',
    mode = 'lines+markers',
    name = 'Nutzbarer Grasvorrat (kg TS/ha)',
    yaxis = "y2"
  ) %>%
  layout(
    title = "Interaktive Graswachstumskurve",
    xaxis = list(title = "Datum"),
    yaxis = list(
      title = "Graswachstum (kg TS/ha/Tag)",
      side = "left",
      zeroline = FALSE
    ),
    yaxis2 = list(
      title = "Nutzbarer Grasvorrat (kg TS/ha)",
      overlaying = "y",
      side = "right",
      showgrid = FALSE,
      zeroline = FALSE
    ),
    legend = list(x = 0.1, y = -0.2, orientation = 'h')
  )

# Grafik anzeigen
plot
