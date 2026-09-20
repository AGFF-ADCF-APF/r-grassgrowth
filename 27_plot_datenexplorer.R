# 27_plot_datenexplorer.R
#
# Datenexplorer: verknuepft die interaktive Graswachstumskurve (Jahr, Region/
# Hoehenlage, Standort - wie in 26_plot_interaktiv_wachstum_niederschlag.R)
# mit einer Schweizer Karte (Graswachstum je Standort), die ueber einen
# Kalenderwochen-Schieberegler den jeweiligen Standeswert zeigt - analog zur
# bisherigen statischen Karte (21_plot_map.R), aber interaktiv, fuer alle
# Jahre durchsuchbar und mit der Kurve verknuepft. (AFC vorerst weggelassen.)
#
# Seitenaufbau von oben nach unten: Wachstumskarte (mit optionalen
# Hintergrund-Ebenen rechts daneben), Kalenderwochen-Schieberegler, Kurve.
#
# Architektur: ZWEI separate Plotly-Widgets (Wachstumskarte, Kurve) werden
# ueber htmltools::save_html() zu EINER Seite kombiniert - das dedupliziert
# automatisch die gemeinsame plotly.js-Bibliothek. Ein gemeinsamer
# <script>-Block (Teil des Kurven-onRender()) verknuepft sie:
#   - Jahr-Auswahl (neues <select> neben der bestehenden Gruppen-/Standort-
#     Combobox) filtert Kurven-Traces UND wechselt die Kartenjahres-Ebene.
#   - Kalenderwochen-Schieberegler blendet auf der Karte die Standort-Marker
#     der gewaehlten Woche ein.
#   - Standorte der in der Kurven-Sidebar gewaehlten Gruppe/Region werden
#     auf der Karte zusaetzlich hervorgehoben (dickerer schwarzer Rand),
#     unabhaengig von der 14-Tage-Aktualitaetsfilterung der Marker selbst.
#   - Optionale Hintergrund-Ebenen (Radiobuttons rechts neben der Karte,
#     standardmaessig "Keine"): Niederschlag der VORANGEHENDEN Kalenderwoche,
#     oder Bodenwasserbilanz zum Stichtag ANFANG der gewaehlten Kalenderwoche
#     (Montag) - je ein vorgerendertes PNG pro Woche (siehe Abschnitt weiter
#     unten), als Plotly layout.images unter die Kantons-/Seen-Ebene gelegt
#     (die Kantonsflaeche wird dabei durchsichtig geschaltet, die Seen bleiben
#     als Referenz sichtbar).
#
# Kartenbasis (Kantone/Seen) via ggswissmaps (identisch zu 21_plot_map.R),
# als Plotly-Polygone in WGS84 lon/lat gezeichnet (keine echte Kartenprojek-
# tion - wie schon bisher mit geom_sf()+theme_void() nur eine Naeherung,
# fuer die Schweiz aber vernachlaessigbar verzerrt; yaxis.scaleratio
# korrigiert das lon/lat-Seitenverhaeltnis fuer die geografische Breite der
# Schweiz: 1 Grad Laengengrad entspricht dort nur rund cos(46.8°) Grad
# Breitengrad in echter Distanz, die y-Achse muss daher pro Grad ENTSPRECHEND
# MEHR Pixel bekommen als die x-Achse, also scaleratio = 1/cos(46.8°).
#
# Kartenwerte je (Jahr, Kalenderwoche): letzte Messung pro Standort BIS ZU
# jener Woche (Montag als Stichtag), nur wenn nicht aelter als 14 Tage (sonst
# kein Marker fuer diesen Standort in jener Woche) - analog "daysold < 16"
# in 01_import_googlesheet.R, hier aber explizit 14 Tage und relativ zur
# gewaehlten Woche statt zu "heute".
#
# Niederschlag in der Kurve UND als Hintergrund-Ebene: wie in 26_...R nur
# fuer Jahre mit lokal vorhandenen MeteoSchweiz-Rasterdaten (siehe
# geodata_dir) verfuegbar - fuer andere Jahre sind die entsprechenden
# Umschalter deaktiviert (ausgegraut). Bodenwasserbilanz stammt aus dem
# Eimermodell-Ergebnis von r-futterbaugutachten (_checkpoint_speicher.tif) -
# nur verfuegbar, wenn dieses Nachbarrepo lokal vorhanden UND fuer das
# jeweilige Jahr berechnet ist (aktuell nur 2026, April-September).
#
# Voraussetzung: 01_import_googlesheet.R wurde bereits ausgefuehrt (liefert
# daten, standardkurven).

library(sf)
library(ggswissmaps)
library(dplyr)
library(terra)
library(plotly)
library(htmltools)
library(htmlwidgets)
library(jsonlite)
library(RColorBrewer)
library(png)
library(base64enc)

out_dir <- "outputs"
geodata_dir <- "../geodata/meteoschweiz"
wasserhaushalt_dir <- "../r-futterbaugutachten/outputs/wasserhaushalt"

## Daten laden, falls noch nicht vorhanden ------------------------------------
if (!exists("daten")) source("01_import_googlesheet.R")

## Jahre bereinigen: vereinzelte Tippfehler bei Datumswerten (z.B. "25-06-09"
## statt "2025-06-09") erzeugen ungueltige 2-stellige "Jahre" - diese werden
## fuer den Datenexplorer ignoriert (Datenkorrektur selbst ist nicht Teil
## dieses Skripts).
daten_korr <- daten %>%
  filter(!is.na(lon), !is.na(lat), !is.na(date), nchar(year) == 4, as.numeric(year) >= 2020) %>%
  mutate(afc = case_when(Ort == "Les Reusilles" ~ afc - 1500, TRUE ~ afc))

alle_jahre <- sort(unique(daten_korr$year))
neuestes_jahr <- max(alle_jahre)

# Fuer den "Heute"-Knopf im Kalenderwochen-Schieberegler.
heutige_woche <- as.integer(strftime(Sys.Date(), format = "%V"))

# Start-Woche beim Laden: die Woche der TATSAECHLICH LETZTEN Messung im
# neuesten Jahr (nicht einfach die hoechste Kalenderwochennummer, die evtl.
# schon in der Nebensaison liegt und nur noch vereinzelte/keine Standorte
# zeigt - "letzte Messungen" meint die Woche mit dem juengsten Erhebungs-
# datum im Datensatz).
start_woche <- {
  letzte_messung_datum <- max(daten_korr$date[daten_korr$year == neuestes_jahr], na.rm = TRUE)
  as.integer(strftime(letzte_messung_datum, format = "%V"))
}

## Standorte, Region/Hoehenlage ueber ALLE Jahre hinweg (stabile Gruppen-
## Zugehoerigkeit unabhaengig davon, in welchem Jahr ein Standort aktiv war -
## Lage/Hoehe eines Standorts aendert sich schliesslich nicht). ---------------
standorte_alle <- daten_korr %>% distinct(place, Ort, lon, lat, masl) %>% rename(elevation = masl)

# Hoehe kommt jetzt primaer direkt aus dem Sheet (Spalte müM, siehe
# 01_import_googlesheet.R) statt fuer jeden Standort einzeln per
# swisstopo-Hoehen-API abgefragt zu werden - deutlich schneller. Die API
# (mit lokalem Cache) dient nur noch als Fallback fuer Standorte OHNE
# Sheet-Wert (z.B. Eintragsluecke).
elevation_cache_file <- "standorte_elevation.csv"
elevation_cache <- if (file.exists(elevation_cache_file)) read.csv(elevation_cache_file) else data.frame(place = character(), elevation = numeric())
fehlende <- standorte_alle %>% filter(is.na(elevation), !place %in% elevation_cache$place)
if (nrow(fehlende) > 0) {
  pts_lv95 <- st_as_sf(fehlende, coords = c("lon", "lat"), crs = 4326) %>% st_transform(2056)
  xy <- st_coordinates(pts_lv95)
  fehlende$elevation <- mapply(function(x, y) {
    url <- paste0("https://api3.geo.admin.ch/rest/services/height?easting=", x, "&northing=", y, "&sr=2056")
    as.numeric(fromJSON(url)$height)
  }, xy[, 1], xy[, 2])
  elevation_cache <- bind_rows(elevation_cache, fehlende %>% select(place, elevation))
  write.csv(elevation_cache, elevation_cache_file, row.names = FALSE)
}
standorte_alle <- standorte_alle %>%
  left_join(elevation_cache, by = "place", suffix = c("", "_api")) %>%
  mutate(elevation = coalesce(elevation, elevation_api)) %>%
  select(-elevation_api)

region_levels <- c("West", "Mitte", "Ost")
hoehen_levels <- c("<500m", "500-650m", "650-800m", ">800m")
standorte_alle <- standorte_alle %>%
  mutate(
    region = factor(case_when(lon < 7.3 ~ "West", lon < 8.5 ~ "Mitte", TRUE ~ "Ost"), levels = region_levels),
    hoehenlage = factor(case_when(
      elevation < 500 ~ "<500m", elevation < 650 ~ "500-650m", elevation < 800 ~ "650-800m", TRUE ~ ">800m"
    ), levels = hoehen_levels)
  )

alle_orte <- sort(unique(as.character(standorte_alle$Ort)))

gruppen <- c(
  list(list(id = "alle", label = "Alle Standorte", sites = alle_orte)),
  lapply(region_levels, function(r) list(id = paste0("region_", r), label = paste0("Region: ", r),
                                          sites = as.character(standorte_alle$Ort[standorte_alle$region == r]))),
  lapply(hoehen_levels, function(h) list(id = paste0("hoehe_", h), label = paste0("Hoehenlage: ", h),
                                          sites = as.character(standorte_alle$Ort[standorte_alle$hoehenlage == h])))
)
gruppen <- gruppen[vapply(gruppen, function(g) length(g$sites) > 0, logical(1))]
gruppen_labels <- vapply(gruppen, function(g) g$label, character(1))
gruppen_ids <- vapply(gruppen, function(g) g$id, character(1))
site_sichtbar_je_gruppe <- lapply(gruppen, function(g) alle_orte %in% g$sites)

site_farben <- setNames(
  colorRampPalette(RColorBrewer::brewer.pal(min(12, max(3, length(alle_orte))), "Paired"))(length(alle_orte)),
  alle_orte
)
site_farben_je_ort <- unname(site_farben[alle_orte])

## Montag je (Jahr, Kalenderwoche), nach ISO-8601 -----------------------------
montag_woche1_jahr <- function(jahr) {
  jan4 <- as.Date(sprintf("%s-01-04", jahr))
  wd <- as.integer(format(jan4, "%u"))
  jan4 - (wd - 1)
}
montag_von_woche <- function(jahr, w) montag_woche1_jahr(jahr) + (w - 1) * 7
wochen_tickvals <- seq(0, 50, by = 5)
wochen_ticktext <- as.character(wochen_tickvals)
datum_ticktext_je_jahr <- setNames(
  lapply(alle_jahre, function(jr) format(montag_von_woche(jr, wochen_tickvals), "%d.%m.")),
  alle_jahre
)

# Datumsbereich (Montag - Sonntag) je (Jahr, Kalenderwoche) - zusaetzlich
# zur Wochennummer oberhalb des Zeitstrahl-Schiebereglers angezeigt (siehe
# weekLabel in onRender() weiter unten).
wochen_datum_bereich_je_woche <- list()
for (jr in alle_jahre) {
  for (w in 1:52) {
    von <- montag_von_woche(jr, w)
    wochen_datum_bereich_je_woche[[paste(jr, w)]] <- paste0(format(von, "%d.%m."), " – ", format(von + 6, "%d.%m.%Y"))
  }
}

cat("Jahre im Datenexplorer:", paste(alle_jahre, collapse = ", "), "- Standorte:", length(alle_orte), "\n")

########################################################################
## 1. Graswachstumskurve (mehrjaehrig) ---------------------------------
########################################################################

## Niederschlag: nur fuer Jahre mit lokal vorhandenen MeteoSchweiz-Rasterdaten
## (siehe 26_...R) - Downloads fuer vergangene Jahre sind nicht Teil dieses
## Skripts.
jahr_hat_niederschlag <- function(jr) {
  any(grepl(paste0("^(rhiresd|rprelimd)_", jr), list.files(geodata_dir)))
}
jahre_mit_niederschlag <- alle_jahre[vapply(alle_jahre, jahr_hat_niederschlag, logical(1))]
cat("Niederschlagsdaten lokal vorhanden fuer:", paste(jahre_mit_niederschlag, collapse = ", "), "\n")

niederschlag_woche_je_jahr <- list()
# Roh-Rasterstapel (taeglich, LV95) je Jahr - wird unten fuer die
# Niederschlags-Hintergrundebene (Vorwochen-Summe) weiterverwendet, damit
# die Daten nicht ein zweites Mal von der Platte geladen werden muessen.
niederschlag_raster_je_jahr <- list()
if (length(jahre_mit_niederschlag) > 0) {
  lade_var_monatlich <- function(prodcode, jahr_monat) {
    f <- file.path(geodata_dir, paste0(prodcode, "_", jahr_monat, ".nc"))
    if (!file.exists(f)) return(NULL)
    rast(f)
  }
  lade_var_taeglich <- function(prodcode, tag) {
    tag_id <- gsub("-", "", as.character(tag))
    f <- file.path(geodata_dir, paste0(prodcode, "_", tag_id, ".nc"))
    if (!file.exists(f)) return(NULL)
    rast(f)
  }
  for (jr in jahre_mit_niederschlag) {
    monate_konsolidiert <- sprintf("%s%02d", jr, 1:12)
    teile <- lapply(monate_konsolidiert, lade_var_monatlich, prodcode = "rhiresd")
    if (jr == neuestes_jahr) {
      tage_ohne_monatsdatei <- seq(as.Date(paste0(jr, "-08-01")), Sys.Date() - 1, by = "day")
      teile <- c(teile, lapply(tage_ohne_monatsdatei, lade_var_taeglich, prodcode = "rprelimd"))
    }
    teile <- teile[!vapply(teile, is.null, logical(1))]
    if (length(teile) == 0) next
    precip_alle <- rast(teile)
    tage_precip <- as.Date(time(precip_alle))
    niederschlag_raster_je_jahr[[jr]] <- precip_alle

    standorte_jahr <- daten_korr %>% filter(year == jr) %>% distinct(place, Ort, lon, lat)
    pts_lv95 <- st_as_sf(standorte_jahr, coords = c("lon", "lat"), crs = 4326) %>% st_transform(2056)
    werte <- terra::extract(precip_alle, vect(pts_lv95))[, -1]
    niederschlag_taeglich <- data.frame(
      Ort = rep(standorte_jahr$Ort, each = length(tage_precip)),
      date = rep(tage_precip, times = nrow(standorte_jahr)),
      precip = as.numeric(t(werte))
    )
    niederschlag_taeglich$weeknum <- as.integer(strftime(niederschlag_taeglich$date, format = "%V"))
    niederschlag_woche_je_jahr[[jr]] <- niederschlag_taeglich %>%
      group_by(Ort, weeknum) %>%
      summarise(precip_week = sum(precip, na.rm = TRUE), .groups = "drop")
    cat("Niederschlag", jr, "geladen:", format(min(tage_precip), "%d.%m.%Y"), "-", format(max(tage_precip), "%d.%m.%Y"), "\n")
  }
}

## Temperatur 2m: nur fuer Jahre mit lokal vorhandenen TabsD-Rasterdaten.
## TabsD wird (anders als RhiresD) durchgehend unter demselben Produktcode
## taeglich publiziert - keine separate "prelim"-Variante fuer die
## juengsten Tage noetig.
jahr_hat_temperatur <- function(jr) any(grepl(paste0("^tabsd_", jr), list.files(geodata_dir)))
jahre_mit_temperatur <- alle_jahre[vapply(alle_jahre, jahr_hat_temperatur, logical(1))]
cat("Temperaturdaten lokal vorhanden fuer:", paste(jahre_mit_temperatur, collapse = ", "), "\n")

temperatur_raster_je_jahr <- list()
if (length(jahre_mit_temperatur) > 0) {
  lade_temp_monatlich <- function(jahr_monat) {
    f <- file.path(geodata_dir, paste0("tabsd_", jahr_monat, ".nc"))
    if (!file.exists(f)) return(NULL)
    rast(f)
  }
  lade_temp_taeglich <- function(tag) {
    tag_id <- gsub("-", "", as.character(tag))
    f <- file.path(geodata_dir, paste0("tabsd_", tag_id, ".nc"))
    if (!file.exists(f)) return(NULL)
    rast(f)
  }
  for (jr in jahre_mit_temperatur) {
    monate_konsolidiert <- sprintf("%s%02d", jr, 1:12)
    teile <- lapply(monate_konsolidiert, lade_temp_monatlich)
    if (jr == neuestes_jahr) {
      tage_ohne_monatsdatei <- seq(as.Date(paste0(jr, "-08-01")), Sys.Date() - 1, by = "day")
      teile <- c(teile, lapply(tage_ohne_monatsdatei, lade_temp_taeglich))
    }
    teile <- teile[!vapply(teile, is.null, logical(1))]
    if (length(teile) == 0) next
    temperatur_raster_je_jahr[[jr]] <- rast(teile)
    tage_temp <- as.Date(time(temperatur_raster_je_jahr[[jr]]))
    cat("Temperatur", jr, "geladen:", format(min(tage_temp), "%d.%m.%Y"), "-", format(max(tage_temp), "%d.%m.%Y"), "\n")
  }
}

## Sonnenscheindauer (relativ, SrelD): anders als Niederschlag/Temperatur
## NICHT vom Nachbarrepo (r-futterbaugutachten) vorbereitet - hier selbst per
## STAC-API heruntergeladen, aus derselben Sammlung (ch.meteoschweiz.ogd-
## surface-derived-grid) wie RhiresD/TabsD. KEINE "prelim"-Variante fuer den
## laufenden Monat vorhanden - fuer die letzten 1-2 Monate fehlen die Daten
## deshalb oft noch (die betroffenen Wochen werden wie bei fehlenden
## Niederschlagsdaten automatisch als nicht verfuegbar behandelt).
sonnenschein_stac_base <- "https://data.geo.admin.ch/api/stac/v1/collections/ch.meteoschweiz.ogd-surface-derived-grid/items/"
lade_sonnenschein_monat <- function(jahr_monat) {
  dest <- file.path(geodata_dir, paste0("sreld_", jahr_monat, ".nc"))
  if (file.exists(dest)) return(invisible(TRUE))
  item <- tryCatch(jsonlite::fromJSON(paste0(sonnenschein_stac_base, jahr_monat, "-ch"), simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(item) || length(item$assets) == 0) return(invisible(FALSE))
  key <- names(item$assets)[grepl("\\.sreld_", names(item$assets))]
  if (length(key) == 0) return(invisible(FALSE))
  href <- item$assets[[key[1]]]$href
  tryCatch({
    download.file(href, destfile = dest, quiet = TRUE, mode = "wb")
    invisible(TRUE)
  }, error = function(e) { unlink(dest); invisible(FALSE) })
}
# Nur fuer Jahre versucht, fuer die ohnehin schon TabsD/RhiresD lokal
# vorhanden sind (jahre_mit_temperatur) - fuer andere Jahre hat dieses Projekt
# ohnehin keine sonstigen Rasterdaten, ein Versuch waere reine Netzwerklast.
for (jr in jahre_mit_temperatur) {
  for (m in 1:12) {
    monatsanfang <- as.Date(sprintf("%s-%02d-01", jr, m))
    if (monatsanfang > Sys.Date()) next
    lade_sonnenschein_monat(sprintf("%s%02d", jr, m))
  }
}
jahr_hat_sonnenschein <- function(jr) any(grepl(paste0("^sreld_", jr), list.files(geodata_dir)))
jahre_mit_sonnenschein <- alle_jahre[vapply(alle_jahre, jahr_hat_sonnenschein, logical(1))]
cat("Sonnenscheindaten lokal vorhanden fuer:", paste(jahre_mit_sonnenschein, collapse = ", "), "\n")

sonnenschein_raster_je_jahr <- list()
if (length(jahre_mit_sonnenschein) > 0) {
  for (jr in jahre_mit_sonnenschein) {
    monat_dateien <- file.path(geodata_dir, paste0("sreld_", sprintf("%s%02d", jr, 1:12), ".nc"))
    monat_dateien <- monat_dateien[file.exists(monat_dateien)]
    if (length(monat_dateien) == 0) next
    sonnenschein_raster_je_jahr[[jr]] <- rast(lapply(monat_dateien, rast))
    tage_sonne <- as.Date(time(sonnenschein_raster_je_jahr[[jr]]))
    cat("Sonnenschein", jr, "geladen:", format(min(tage_sonne), "%d.%m.%Y"), "-", format(max(tage_sonne), "%d.%m.%Y"), "\n")
  }
}

## Kumulierte Wachstumsgradtage (Basis 5 Grad C) seit Beginn der lokal
## vorhandenen Temperaturdaten (siehe temperatur_raster_je_jahr oben) - Mass
## fuer die pflanzenverfuegbare Waermesumme seit Saisonbeginn (Faustregel:
## Tagesmitteltemperatur minus 5 Grad C, negative Tage zaehlen als 0,
## fortlaufend aufsummiert - siehe auch i-Button-Erklaerung weiter unten).
gdd_kumuliert_je_jahr <- list()
for (jr in names(temperatur_raster_je_jahr)) {
  r_jahr <- temperatur_raster_je_jahr[[jr]]
  gdd_taeglich <- clamp(r_jahr - 5, lower = 0)
  gdd_kum <- rast(gdd_taeglich)
  lauf <- gdd_taeglich[[1]] * 0
  for (i in seq_len(nlyr(gdd_taeglich))) {
    lauf <- lauf + gdd_taeglich[[i]]
    gdd_kum[[i]] <- lauf
  }
  # Erst NACH der Schlaufe benennen (wie beim Bucket-Modell in
  # 44_wasserhaushalt_meteoschweiz.R) - jede Zuweisung gdd_kum[[i]] <- lauf
  # uebernimmt sonst zwischenzeitlich den generischen Namen von lauf.
  time(gdd_kum) <- time(r_jahr)
  gdd_kumuliert_je_jahr[[jr]] <- gdd_kum
}

wochen_tooltip <- function(jr, w) paste0("KW ", w, " (Woche ab ", format(montag_von_woche(jr, w), "%d.%m.%Y"), ")")

# Fixer Y-Achsen-Bereich fuer BEIDE Achsen (Graswachstum links, Niederschlag
# rechts) - unabhaengig von der aktuell gewaehlten Gruppe/Standort/Jahr, statt
# wie bisher per Autorange bei jedem Filterwechsel neu zu skalieren (dadurch
# waren Kurven zwischen zwei Auswahlen optisch kaum vergleichbar - derselbe
# Kurvenverlauf sah je nach Skala mal steil, mal flach aus). Bewusst fest
# gewaehlte (nicht vom Datenmaximum abgeleitete) Obergrenzen - einzelne
# Ausreisser darueber werden an der Grenze gekappt und mit einem Dreieck-
# Marker markiert (siehe kappe_und_markiere_ausreisser() weiter unten),
# statt die ganze Skala fuer wenige Extremwerte zu strecken.
graswachstum_y_max <- 150
niederschlag_y_max <- 70

fig_kurve <- plot_ly(height = 520)
site_growth_meta <- list()
group_growth_meta <- list()
site_precip_meta <- list()
group_precip_meta <- list()

# Naechster (0-basierter) Plotly-Trace-Index, wird nach JEDEM add_trace()
# um 1 erhoeht und in jedem Meta-Eintrag mitgespeichert (traceIdx) - die
# Traces werden unten pro Jahr VERSCHACHTELT angelegt (Standort-Wachstum,
# Gruppen-Wachstum, ggf. Standort-/Gruppen-Niederschlag, pro Jahr
# wiederholt), NICHT blockweise nach Typ ueber alle Jahre hinweg. Ohne den
# expliziten Index wuerde applyState() in der JS-Seite (die je Meta-Liste
# EN BLOC ueber alle Jahre iteriert) die visible-Flags auf die falschen
# Trace-Positionen anwenden, sobald sich die Anzahl Standorte/Gruppen
# zwischen den Bloecken unterscheidet (praktisch immer der Fall) - das
# aeusserte sich als: Filter auf einen Standort zeigte die Kurve eines
# GANZ ANDEREN Standorts an.
next_trace_idx <- 0

# Baut eine zusaetzliche Marker-Trace fuer Punkte oberhalb von 'cap' (Dreieck
# an der Kappungsgrenze, echter Wert im Tooltip) - die Haupt-Trace selbst
# wird an gleicher Stelle auf 'cap' gekappt (siehe pmin() in den Aufrufer-
# Schleifen), statt die feste Y-Achse (graswachstum_y_max/niederschlag_y_max)
# fuer wenige Ausreisser zu strecken. Gibt fig UND ob eine Trace angelegt
# wurde zurueck (fuer den Meta-Eintrag/next_trace_idx beim Aufrufer - siehe
# Kommentar oben zu next_trace_idx: dieselbe siteIdx/groupIdx/year wie die
# Haupt-Trace, damit die Sichtbarkeit beim Filtern synchron bleibt).
fuege_ausreisser_hinzu <- function(fig, d, wertespalte, cap, name, farbe, einheit, yaxis = "y") {
  ausreisser <- d[!is.na(d[[wertespalte]]) & d[[wertespalte]] > cap, ]
  if (nrow(ausreisser) == 0) return(list(fig = fig, hinzugefuegt = FALSE))
  fig <- fig %>% add_trace(
    data = ausreisser, x = ~weeknum, y = cap, type = "scatter", mode = "markers", yaxis = yaxis,
    marker = list(symbol = "triangle-up", size = 11, color = farbe, line = list(color = "black", width = 1)),
    customdata = ausreisser[[wertespalte]],
    hovertemplate = paste0(name, ": %{customdata:.0f} ", einheit, " (ausserhalb der Skala)<extra></extra>"),
    showlegend = FALSE, visible = FALSE, name = paste(name, "Ausreisser")
  )
  list(fig = fig, hinzugefuegt = TRUE)
}

for (jr in alle_jahre) {
  jd <- daten_korr %>% filter(year == jr) %>% arrange(Ort, date)
  orte_jahr <- sort(unique(as.character(jd$Ort)))

  for (ort in orte_jahr) {
    d <- jd %>% filter(Ort == ort) %>% arrange(weeknum)
    if (nrow(d) == 0) next
    d$tooltip <- paste0("KW ", d$weeknum, " (erhoben am ", format(d$date, "%d.%m.%Y"), ")")
    d$growth_gekappt <- pmin(d$growth, graswachstum_y_max)
    site_idx <- match(ort, alle_orte)
    fig_kurve <- fig_kurve %>% add_trace(
      data = d, x = ~weeknum, y = ~growth_gekappt, type = "scatter", mode = "lines+markers",
      name = ort, line = list(color = site_farben[[ort]], width = 1.5),
      marker = list(color = site_farben[[ort]], size = 5),
      customdata = lapply(seq_len(nrow(d)), function(i) list(d$tooltip[i], d$growth[i])),
      hovertemplate = paste0(ort, ": %{customdata[1]:.0f} kg TS/ha/Tag<br>%{customdata[0]}<extra></extra>"),
      showlegend = FALSE, visible = FALSE
    )
    site_growth_meta[[length(site_growth_meta) + 1]] <- list(year = jr, siteIdx = site_idx - 1, traceIdx = next_trace_idx)
    next_trace_idx <- next_trace_idx + 1
    a <- fuege_ausreisser_hinzu(fig_kurve, d, "growth", graswachstum_y_max, ort, site_farben[[ort]], "kg TS/ha/Tag")
    fig_kurve <- a$fig
    if (a$hinzugefuegt) {
      site_growth_meta[[length(site_growth_meta) + 1]] <- list(year = jr, siteIdx = site_idx - 1, traceIdx = next_trace_idx)
      next_trace_idx <- next_trace_idx + 1
    }
  }

  for (g in gruppen) {
    d <- jd %>% filter(Ort %in% g$sites) %>% group_by(weeknum) %>%
      summarise(mean_growth = mean(growth, na.rm = TRUE), .groups = "drop")
    d$tooltip <- wochen_tooltip(jr, d$weeknum)
    d$mean_growth_gekappt <- pmin(d$mean_growth, graswachstum_y_max)
    fig_kurve <- fig_kurve %>% add_trace(
      data = d, x = ~weeknum, y = ~mean_growth_gekappt, type = "scatter", mode = "lines",
      name = "Mittleres Wachstum", line = list(color = "black", dash = "dash", width = 2.5),
      customdata = lapply(seq_len(nrow(d)), function(i) list(d$tooltip[i], d$mean_growth[i])),
      hovertemplate = paste0("Mittel ", g$label, ": %{customdata[1]:.0f} kg TS/ha/Tag<br>%{customdata[0]}<extra></extra>"),
      showlegend = FALSE, visible = FALSE
    )
    group_idx <- match(g$id, gruppen_ids) - 1
    group_growth_meta[[length(group_growth_meta) + 1]] <- list(year = jr, groupIdx = group_idx, traceIdx = next_trace_idx)
    next_trace_idx <- next_trace_idx + 1
    a <- fuege_ausreisser_hinzu(fig_kurve, d, "mean_growth", graswachstum_y_max, paste("Mittel", g$label), "black", "kg TS/ha/Tag")
    fig_kurve <- a$fig
    if (a$hinzugefuegt) {
      group_growth_meta[[length(group_growth_meta) + 1]] <- list(year = jr, groupIdx = group_idx, traceIdx = next_trace_idx)
      next_trace_idx <- next_trace_idx + 1
    }
  }

  if (jr %in% names(niederschlag_woche_je_jahr)) {
    nw <- niederschlag_woche_je_jahr[[jr]]
    for (ort in orte_jahr) {
      d <- nw %>% filter(Ort == ort) %>% arrange(weeknum)
      if (nrow(d) == 0) next
      d$tooltip <- wochen_tooltip(jr, d$weeknum)
      d$precip_week_gekappt <- pmin(d$precip_week, niederschlag_y_max)
      fig_kurve <- fig_kurve %>% add_trace(
        data = d, x = ~weeknum, y = ~precip_week_gekappt, type = "bar", yaxis = "y2", width = 0.7,
        name = paste("Niederschlag", ort), marker = list(color = "steelblue", opacity = 0.4),
        customdata = lapply(seq_len(nrow(d)), function(i) list(d$tooltip[i], d$precip_week[i])),
        hovertemplate = paste0("Niederschlag ", ort, ": %{customdata[1]:.0f} mm<br>%{customdata[0]}<extra></extra>"),
        showlegend = FALSE, visible = FALSE
      )
      site_idx <- match(ort, alle_orte) - 1
      site_precip_meta[[length(site_precip_meta) + 1]] <- list(year = jr, siteIdx = site_idx, traceIdx = next_trace_idx)
      next_trace_idx <- next_trace_idx + 1
      a <- fuege_ausreisser_hinzu(fig_kurve, d, "precip_week", niederschlag_y_max, paste("Niederschlag", ort), "steelblue", "mm", yaxis = "y2")
      fig_kurve <- a$fig
      if (a$hinzugefuegt) {
        site_precip_meta[[length(site_precip_meta) + 1]] <- list(year = jr, siteIdx = site_idx, traceIdx = next_trace_idx)
        next_trace_idx <- next_trace_idx + 1
      }
    }
    for (g in gruppen) {
      d <- nw %>% filter(Ort %in% g$sites) %>% group_by(weeknum) %>%
        summarise(mean_mm = mean(precip_week, na.rm = TRUE),
                  min_mm = min(precip_week, na.rm = TRUE),
                  max_mm = max(precip_week, na.rm = TRUE), .groups = "drop")
      d$tooltip <- wochen_tooltip(jr, d$weeknum)
      # Balken UND Fehlerbalken-Obergrenze auf niederschlag_y_max gekappt -
      # sonst wuerde der Whisker (bis max_mm) visuell ueber die feste Achse
      # hinausragen, auch wenn der Mittelwert selbst darunter liegt.
      d$mean_mm_gekappt <- pmin(d$mean_mm, niederschlag_y_max)
      # Beide Whisker relativ zur GEKAPPTEN Balkenhoehe berechnet (nicht zum
      # echten mean_mm) - sonst wuerde der untere Whisker im (seltenen) Fall
      # eines gekappten Balkens zu tief unter min_mm hinausschiessen.
      d$error_oben_gekappt <- pmin(d$max_mm, niederschlag_y_max) - d$mean_mm_gekappt
      d$error_unten_gekappt <- pmax(d$mean_mm_gekappt - d$min_mm, 0)
      fig_kurve <- fig_kurve %>% add_trace(
        data = d, x = ~weeknum, y = ~mean_mm_gekappt, type = "bar", yaxis = "y2", width = 0.7,
        name = paste("Niederschlag", g$label), marker = list(color = "steelblue", opacity = 0.4),
        error_y = list(type = "data", symmetric = FALSE, array = ~error_oben_gekappt, arrayminus = ~error_unten_gekappt, color = "steelblue"),
        customdata = lapply(seq_len(nrow(d)), function(i) list(d$tooltip[i], d$mean_mm[i])),
        hovertemplate = paste0("Niederschlag ", g$label, ": %{customdata[1]:.0f} mm im Mittel<br>%{customdata[0]}<extra></extra>"),
        showlegend = FALSE, visible = FALSE
      )
      group_idx <- match(g$id, gruppen_ids) - 1
      group_precip_meta[[length(group_precip_meta) + 1]] <- list(year = jr, groupIdx = group_idx, traceIdx = next_trace_idx)
      next_trace_idx <- next_trace_idx + 1
      a <- fuege_ausreisser_hinzu(fig_kurve, d, "mean_mm", niederschlag_y_max, paste("Niederschlag", g$label), "steelblue", "mm", yaxis = "y2")
      fig_kurve <- a$fig
      if (a$hinzugefuegt) {
        group_precip_meta[[length(group_precip_meta) + 1]] <- list(year = jr, groupIdx = group_idx, traceIdx = next_trace_idx)
        next_trace_idx <- next_trace_idx + 1
      }
    }
  }
}

## Referenzkurve "Durchschnitt Mittelland" - jahresunabhaengig, immer sichtbar
standardkurven$tooltip <- paste0("KW ", standardkurven$weeknum)
standard_kurve_trace_idx <- next_trace_idx
fig_kurve <- fig_kurve %>% add_trace(
  data = standardkurven, x = ~weeknum, y = ~Durchschnitt...700.m.ü.M...tiefgründig..frisch,
  type = "scatter", mode = "lines", name = "Durchschnitt Mittelland",
  line = list(color = "red", dash = "dot", width = 2.5), customdata = ~tooltip,
  hovertemplate = "Durchschnitt Mittelland: %{y:.0f} kg TS/ha/Tag<br>%{customdata}<extra></extra>",
  showlegend = TRUE, visible = TRUE
)

fig_kurve <- fig_kurve %>% layout(
  xaxis = list(title = "Kalenderwoche", range = c(0, 52), automargin = TRUE,
               tickmode = "array", tickvals = wochen_tickvals, ticktext = wochen_ticktext),
  yaxis = list(title = "Graswachstum (kg TS/ha/Tag)", range = c(0, graswachstum_y_max)),
  yaxis2 = list(title = "Niederschlag (mm/Woche)", overlaying = "y", side = "right", showgrid = FALSE, range = c(0, niederschlag_y_max)),
  barmode = "overlay",
  showlegend = FALSE,
  margin = list(t = 40, b = 70, r = 30),
  # Vertikale Markierung fuer die aktuell im Kalenderwochen-Schieberegler
  # gewaehlte Woche (Position wird per JS bei jeder Schieberegler-Aenderung
  # aktualisiert, siehe onRender()-Block).
  shapes = list(list(type = "line", x0 = 1, x1 = 1, y0 = 0, y1 = 1, yref = "paper",
                      line = list(color = "#999", width = 1.5, dash = "dot")))
) %>% config(responsive = TRUE, scrollZoom = TRUE)

########################################################################
## 2. Kartenbasis (Kantone, Seen) - identisch zu 21_plot_map.R --------
########################################################################

# ggswissmaps' Kantons-/Seen-Polygone tragen ein veraltetes CRS-Format aus
# einer alteren PROJ/GDAL-Version. Beim Ueberschreiben mit dem tatsaechlichen
# CRS (21781) gibt GDAL/PROJ dafuer "old-style crs object detected..." direkt
# auf stderr aus - UNABHAENGIG vom R-Warnungssystem (suppressWarnings() faengt
# das deshalb nicht ab). Harmlos (das Zielkoordinatensystem stimmt trotzdem),
# aber stoert bei jedem Lauf mehrfach die Konsolenausgabe - hier gezielt nur
# fuer diesen einen Aufruf weggefiltert.
ohne_veraltete_crs_meldung <- function(expr) {
  puffer_verbindung <- textConnection("crs_meldung_puffer", "w", local = TRUE)
  sink(puffer_verbindung, type = "message")
  on.exit({ sink(type = "message"); close(puffer_verbindung) }, add = TRUE)
  force(expr)
}

data(shp_sf)
swk_shp <- shp_sf[["g1k15"]] %>% st_as_sfc() %>%
  { ohne_veraltete_crs_meldung(sf::st_sfc(., crs = 21781)) } %>%
  sf::st_transform(crs = "WGS84")
swl_shp <- shp_sf[["g1s15"]] %>% st_as_sfc() %>%
  { ohne_veraltete_crs_meldung(sf::st_sfc(., crs = 21781)) } %>%
  sf::st_transform(crs = "WGS84")
# Landesgrenze (alle Kantone zu einer Flaeche vereinigt) - dient unten dazu,
# die Niederschlags-/Bodenwasserbilanz-Rasterbilder auf die Schweiz zu
# maskieren (sonst waeren sie ein Rechteck bis zum Rand der Bounding Box).
schweiz_grenze_vect <- terra::vect(sf::st_union(swk_shp))

# Seitenverhaeltnis-Korrektur fuer lon/lat als Kartesische Achsen: 1 Grad
# Laengengrad entspricht bei der mittleren Breite der Schweiz (~46.8°N) nur
# rund cos(46.8°) Grad Breitengrad in echter Distanz - die y-Achse (Breite)
# muss also pro Grad ENTSPRECHEND MEHR Pixel bekommen als die x-Achse
# (Laenge), damit die Karte nicht in die Breite gezogen wirkt.
lat_mittel <- mean(standorte_alle$lat, na.rm = TRUE)
karten_scaleratio <- 1 / cos(lat_mittel * pi / 180)
kartenbbox <- sf::st_bbox(swk_shp)
lon_range <- c(kartenbbox[["xmin"]], kartenbbox[["xmax"]])
lat_range <- c(kartenbbox[["ymin"]], kartenbbox[["ymax"]])
# Erweiterter x-Bereich NUR fuer die sichtbare Achse und das AFC-Ring-Bild:
# reserviert einen Rand rechts neben der Schweiz fuer die AFC-Ziel-Legende
# (auf den einzelnen, kleinen Standort-Ringen selbst waeren Tick-
# Beschriftungen unleserlich). Kartenbasis/Niederschlag/Bodenwasser-Bilder
# bleiben unveraendert auf den echten Schweizer Ausmassen (lon_range) - der
# zusaetzliche Rand zeigt dort einfach nichts/transparent.
lon_range_erweitert <- c(lon_range[1], lon_range[2] + diff(lon_range) * 0.22)

# Kantone/Seen als STATISCHES Hintergrundbild (ggplot -> PNG -> data:-URI,
# via Plotly layout.images) statt als live gerenderte Plotly-Vektor-Traces:
# Kantone sind teils komplexe Multipolygone mit mehreren Teilen/Loechern -
# als einzelne NA-getrennte scatter-Trace mit fill="toself" gezeichnet, hat
# Plotly das gelegentlich fehlerhaft dargestellt (einzelne Teile falsch
# gefuellt bzw. mit einer Linie verbunden). Ein vorgerendertes Bild (wie
# schon bei den Niederschlags-/Bodenwasserbilanz-Ebenen) ist unabhaengig
# von Plotlys Vektor-Fuell-Logik und sieht exakt wie die bisherigen
# statischen Karten (21_plot_map.R) aus.
kartenbild_hintergrund <- local({
  p <- ggplot() +
    geom_sf(data = swk_shp, fill = "#b6d69a", color = "white", linewidth = 0.3) +
    geom_sf(data = swl_shp, fill = "#acd2ef", color = "#acd2ef") +
    coord_sf(xlim = lon_range, ylim = lat_range, expand = FALSE) +
    theme_void()
  tmp_png <- tempfile(fileext = ".png")
  hoehe_zoll <- 9 * diff(lat_range) / diff(lon_range) * karten_scaleratio
  ggsave(tmp_png, p, width = 9, height = hoehe_zoll, dpi = 130, bg = "transparent")
  b64 <- base64enc::base64encode(tmp_png)
  unlink(tmp_png)
  list(
    source = paste0("data:image/png;base64,", b64),
    xref = "x", yref = "y",
    x = lon_range[1], y = lat_range[2],
    sizex = diff(lon_range), sizey = diff(lat_range),
    xanchor = "left", yanchor = "top", sizing = "stretch", layer = "below"
  )
})

########################################################################
## 3. Kartenwerte je (Jahr, Kalenderwoche): letzte Messung <= 14 Tage -
########################################################################

alle_wochen <- 1:52
map_snapshots <- bind_rows(lapply(alle_jahre, function(jr) {
  dj <- daten_korr %>% filter(year == jr)
  bind_rows(lapply(alle_wochen, function(w) {
    refdate <- montag_von_woche(jr, w)
    # Vor Saisonbeginn (keine Messung <= refdate) ist d_bis leer - max(date)
    # auf einem leeren Vektor liefert -Inf samt Warnung; das anschliessende
    # filter(daysold <= 14) wuerde die Zeile ohnehin verwerfen, der Check
    # spart also nur die Warnung, aendert das Ergebnis nicht.
    d_bis <- dj %>% filter(date <= refdate)
    if (nrow(d_bis) == 0) return(NULL)
    d_bis %>%
      group_by(Ort, place, lon, lat) %>%
      filter(date == max(date)) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(daysold = as.numeric(refdate - date)) %>%
      filter(daysold <= 14) %>%
      mutate(jahr = jr, week = w)
  }))
}))
cat("Kartenschnappschuesse (Jahr x Woche mit Daten):", nrow(map_snapshots %>% distinct(jahr, week)), "\n")

map_wochen <- map_snapshots %>% distinct(jahr, week) %>% arrange(jahr, week)

## AFC-Fortschrittsring um die Wachstumszahl - adaptiert von einer parallel
## entwickelten Repo-Version (auf einem anderen Rechner erstellt, per
## Datenexplorer-Anfrage vom Nutzer eingebracht). Statt eines einzelnen
## AFC-Zahlenwerts zeigt ein Ring um den Wachstums-Kreis, wie nahe der
## Grasvorrat am jahreszeitlich passenden Zielkorridor liegt: gruen = im
## Zielbereich, rot = deutlich zu wenig, dunkelblau/tuerkis = deutlich zu
## viel. Als vorgerendertes Bild (wie die Kartenbasis) statt als Plotly-
## Vektor-Traces, aus denselben Gruenden (siehe Kommentar bei
## kartenbild_hintergrund oben) - Ringe/Boegen sind zwar einfacher als
## Kantonspolygone, aber die Zuverlaessigkeit eines fertigen Bildes ist hier
## wichtiger als Live-Interaktivitaet der Ring-Geometrie selbst (Hover-
## Tooltips bleiben ueber unsichtbare Plotly-Marker an derselben Position
## erhalten, siehe baue_kartenwerte_trace()).
afc_min <- 0
afc_max <- 1500
afc_red_full <- 200
afc_optimum_windows <- data.frame(
  start_mmdd = c("01-01", "05-02", "09-01", "11-01"),
  end_mmdd = c("05-01", "08-31", "10-31", "12-31"),
  optimum_low = c(500, 700, 900, 600),
  optimum_high = c(700, 800, 1200, 700),
  stringsAsFactors = FALSE
)
get_afc_targets <- function(reference_date) {
  mmdd <- format(as.Date(reference_date), "%m-%d")
  match_idx <- which(mmdd >= afc_optimum_windows$start_mmdd & mmdd <= afc_optimum_windows$end_mmdd)[1]
  if (is.na(match_idx)) match_idx <- 2
  cbind(afc_optimum_windows[match_idx, ], fenster_idx = match_idx)
}
afc_to_color <- function(x, optimum_low, optimum_high) {
  x <- pmax(afc_min, pmin(x, afc_max))
  out <- rep("#4CAF50", length(x))
  low_idx <- which(x < optimum_low)
  if (length(low_idx) > 0) {
    t <- (pmax(x[low_idx], afc_red_full) - afc_red_full) / (optimum_low - afc_red_full)
    t <- pmax(0, pmin(t, 1)); t_fast <- t ^ 2
    r <- round((1 - t_fast) * 242 + t_fast * 76)
    g <- round((1 - t_fast) * 200 + t_fast * 175)
    b <- round((1 - t_fast) * 75 + t_fast * 80)
    very_low_idx <- which(x[low_idx] < afc_red_full)
    if (length(very_low_idx) > 0) {
      red_t <- pmax(x[low_idx][very_low_idx], 0) / afc_red_full
      r[very_low_idx] <- 255; g[very_low_idx] <- round(red_t * 200); b[very_low_idx] <- round(red_t * 75)
    }
    out[low_idx] <- sprintf("#%02X%02X%02X", r, g, b)
  }
  high_idx <- which(x > optimum_high)
  if (length(high_idx) > 0) {
    t <- (x[high_idx] - optimum_high) / (afc_max - optimum_high)
    t <- pmax(0, pmin(t, 1)); t_fast <- t ^ 0.6
    r <- round((1 - t_fast) * 76 + t_fast * 8)
    g <- round((1 - t_fast) * 175 + t_fast * 29)
    b <- round((1 - t_fast) * 80 + t_fast * 88)
    out[high_idx] <- sprintf("#%02X%02X%02X", r, g, b)
  }
  out
}

ring_radius <- 0.045
# 50% breiter als zuvor (0.008 -> 0.012) UND der Ring-Pfad selbst weiter
# aussen platziert (nicht nur eine dickere Linie auf gleicher Position) -
# damit die zusaetzliche Breite nach aussen wächst statt den Wachstums-
# kreis in der Mitte (ring_radius, unveraendert) zu ueberlappen.
ring_radius_outer <- ring_radius + 0.008 * 1.5
ring_steps <- 360
start_angle <- pi / 2

# Schrift fuer die "Tage seit Messung"-Legende (Plotly-natives colorbar).
legenden_font <- list(family = "Arial, sans-serif", size = 11, color = "black")

tage_farbe <- function(daysold) {
  anteil <- pmax(0, pmin(1, daysold / 14))
  rampe <- grDevices::colorRamp(c("white", "gray46"))
  rgb_w <- rampe(anteil)
  grDevices::rgb(rgb_w[, 1], rgb_w[, 2], rgb_w[, 3], maxColorValue = 255)
}

# Aufgeteilt in ZWEI unabhaengige Bild-Ebenen (frueher ein einziges Bild) -
# Graswachstum (Kreis+Zahl) und AFC (Ring+Ringlegende) sind seither je ueber
# einen eigenen Schieberegler im Ebenen-Kasten unabhaengig ein-/ausblendbar.
# Beide auf denselben Koordinaten (lon_range_erweitert/lat_range) gerendert,
# damit sie deckungsgleich uebereinander liegen, wenn beide aktiv sind.
baue_graswachstum_bild <- function(snap) {
  if (nrow(snap) == 0) return(NULL)
  snap$daysold_col <- tage_farbe(snap$daysold)
  snap$lon_scale <- 1 / pmax(cos(snap$lat * pi / 180), 1e-6)

  center_circles <- do.call(rbind, lapply(seq_len(nrow(snap)), function(i) {
    theta <- seq(0, 2 * pi, length.out = 100)
    data.frame(id = i, x = snap$lon[i] + ring_radius * snap$lon_scale[i] * cos(theta),
               y = snap$lat[i] + ring_radius * sin(theta), col = snap$daysold_col[i])
  }))

  p <- ggplot() +
    geom_polygon(data = center_circles, aes(x = x, y = y, group = id, fill = col), color = "black", linewidth = 0.6, show.legend = FALSE) +
    geom_text(data = snap, aes(x = lon, y = lat, label = round(growth, 0)), fontface = "bold", size = 3) +
    scale_fill_identity() +
    coord_sf(crs = sf::st_crs(4326), xlim = lon_range_erweitert, ylim = lat_range, expand = FALSE) +
    theme_void()

  tmp_png <- tempfile(fileext = ".png")
  hoehe_zoll <- 9 * diff(lat_range) / diff(lon_range_erweitert) * karten_scaleratio
  ggsave(tmp_png, p, width = 9, height = hoehe_zoll, dpi = 130, bg = "transparent")
  b64 <- base64enc::base64encode(tmp_png)
  unlink(tmp_png)
  list(
    source = paste0("data:image/png;base64,", b64),
    xref = "x", yref = "y",
    x = lon_range_erweitert[1], y = lat_range[2],
    sizex = diff(lon_range_erweitert), sizey = diff(lat_range),
    xanchor = "left", yanchor = "top", sizing = "stretch", layer = "above"
  )
}

# Gibt list(bild=..., fensterIdx=...) zurueck - fensterIdx (Index in
# afc_optimum_windows) wird auch dann geliefert, wenn kein Standort diese
# Woche einen AFC-Wert hat (bild dann NULL), damit die kompakte AFC-Legende
# im Ebenen-Kasten den jahreszeitlichen Zielkorridor trotzdem anzeigen kann.
baue_afc_ring_bild <- function(snap, referenzdatum) {
  targets <- get_afc_targets(referenzdatum)
  opt_low <- targets$optimum_low[[1]]; opt_high <- targets$optimum_high[[1]]
  fenster_idx <- targets$fenster_idx[[1]]
  if (nrow(snap) == 0) return(list(bild = NULL, fensterIdx = fenster_idx))
  snap$has_afc <- !is.na(snap$afc)
  snap$afc_progress <- (pmax(afc_min, pmin(snap$afc, afc_max)) - afc_min) / (afc_max - afc_min)
  snap$afc_ring_color <- afc_to_color(snap$afc, opt_low, opt_high)
  snap$daysold_col <- tage_farbe(snap$daysold)
  snap$lon_scale <- 1 / pmax(cos(snap$lat * pi / 180), 1e-6)

  ring_hat_afc <- which(snap$has_afc)
  if (length(ring_hat_afc) == 0) return(list(bild = NULL, fensterIdx = fenster_idx))
  # col = daysold_col (wie der Wachstumskreis) - der Ring-Hintergrund war
  # bisher fest auf "gray80" gesetzt, unabhaengig von "Tage seit Messung".
  ring_bg <- do.call(rbind, lapply(ring_hat_afc, function(i) {
    theta <- seq(start_angle, start_angle - 2 * pi, length.out = ring_steps)
    data.frame(id = i, x = snap$lon[i] + ring_radius_outer * snap$lon_scale[i] * cos(theta),
               y = snap$lat[i] + ring_radius_outer * sin(theta), col = snap$daysold_col[i])
  }))
  ring_vorne <- which(snap$has_afc & snap$afc_progress > 0)
  ring_fg <- if (length(ring_vorne) > 0) do.call(rbind, lapply(ring_vorne, function(i) {
    prog <- snap$afc_progress[i]
    theta <- seq(start_angle, start_angle - 2 * pi * prog, length.out = max(2, ceiling(ring_steps * prog) + 1))
    data.frame(id = i, x = snap$lon[i] + ring_radius_outer * snap$lon_scale[i] * cos(theta),
               y = snap$lat[i] + ring_radius_outer * sin(theta), color = snap$afc_ring_color[i])
  })) else NULL

  # Kleine Tick-Striche am Standort-Ring selbst, an den Positionen des
  # jahreszeitlichen Zielbereichs (opt_low/opt_high) - ersetzt die frueher
  # separate, grosse Referenz-Ring-Legende auf der Karte (jetzt nur noch
  # kompakt im Ebenen-Kasten, siehe aktualisiereAfcLegende()/JS). Direkt an
  # jedem Standort-Ring zeigt das sofort, wo der Zielbereich fuer DIESEN
  # Standort beginnt/endet, ohne zwischen Karte und separater Legende hin-
  # und herschauen zu muessen.
  tick_theta <- start_angle - 2 * pi * (c(opt_low, opt_high) - afc_min) / (afc_max - afc_min)
  tick_offset <- 0.006
  ring_ticks <- do.call(rbind, lapply(ring_hat_afc, function(i) {
    data.frame(
      id = i,
      x = snap$lon[i] + (ring_radius_outer - tick_offset) * snap$lon_scale[i] * cos(tick_theta),
      y = snap$lat[i] + (ring_radius_outer - tick_offset) * sin(tick_theta),
      xend = snap$lon[i] + (ring_radius_outer + tick_offset) * snap$lon_scale[i] * cos(tick_theta),
      yend = snap$lat[i] + (ring_radius_outer + tick_offset) * sin(tick_theta)
    )
  }))

  p <- ggplot() +
    geom_path(data = ring_bg, aes(x = x, y = y, group = id, color = col), linewidth = 1.95, lineend = "round") +
    { if (!is.null(ring_fg)) geom_path(data = ring_fg, aes(x = x, y = y, group = id, color = color), linewidth = 3.15, lineend = "butt", show.legend = FALSE) } +
    geom_segment(data = ring_ticks, aes(x = x, y = y, xend = xend, yend = yend, group = id), color = "black", linewidth = 0.8) +
    scale_color_identity() +
    coord_sf(crs = sf::st_crs(4326), xlim = lon_range_erweitert, ylim = lat_range, expand = FALSE) +
    theme_void()

  tmp_png <- tempfile(fileext = ".png")
  hoehe_zoll <- 9 * diff(lat_range) / diff(lon_range_erweitert) * karten_scaleratio
  ggsave(tmp_png, p, width = 9, height = hoehe_zoll, dpi = 130, bg = "transparent")
  b64 <- base64enc::base64encode(tmp_png)
  unlink(tmp_png)
  list(
    bild = list(
      source = paste0("data:image/png;base64,", b64),
      xref = "x", yref = "y",
      x = lon_range_erweitert[1], y = lat_range[2],
      sizex = diff(lon_range_erweitert), sizey = diff(lat_range),
      xanchor = "left", yanchor = "top", sizing = "stretch", layer = "above"
    ),
    fensterIdx = fenster_idx
  )
}

# Vorgerechneter Farbverlauf je Zielkorridor-Fenster (nur 4 verschiedene
# Fenster ueber das ganze Jahr, siehe afc_optimum_windows) - fuer die
# kompakte AFC-Legende im Ebenen-Kasten (CSS-Farbverlauf aus vielen
# Stuetzstellen angenaehert, da afc_to_color() kein einfacher linearer
# Verlauf ist).
afc_verlauf_stuetzstellen <- seq(afc_min, afc_max, length.out = 30)
afc_verlaeufe_je_fenster <- lapply(seq_len(nrow(afc_optimum_windows)), function(i) {
  low <- afc_optimum_windows$optimum_low[i]; high <- afc_optimum_windows$optimum_high[i]
  list(
    farben = afc_to_color(afc_verlauf_stuetzstellen, low, high),
    low = low, high = high
  )
})

baue_kartenwerte_trace <- function(fig, snap, wertspalte, einheit, titel) {
  snap$wert <- snap[[wertspalte]]
  snap$hover <- paste0(snap$Ort, ": ", round(snap$wert, 0), " ", einheit,
                        "<br>AFC: ", ifelse(is.na(snap$afc), "keine Angabe", paste0(round(snap$afc, 0), " kg TS/ha")),
                        "<br>erhoben am ", format(snap$date, "%d.%m.%Y"),
                        " (vor ", snap$daysold, " Tagen)")
  # Marker unsichtbar (opacity=0): der Wachstumskreis + AFC-Ring wird als
  # Bild gezeichnet (baue_afc_ring_bild()), die Trace selbst dient nur noch
  # dem Hover-Tooltip (Plotlys Naeherungs-Erkennung fuer Hover basiert auf
  # den Datenkoordinaten, nicht auf der sichtbaren Bildschirmdarstellung -
  # unsichtbare Marker sind daher trotzdem hoverbar) sowie der "Tage seit
  # Messung"-Farblegende (colorbar bleibt an eine sichtbare TRACE gebunden,
  # nicht an einen sichtbaren MARKER).
  # WICHTIG: list(list(0,"white"), list(1,"gray46")) statt
  # list(c(0,"white"), c(1,"gray46")) - c() zwingt gemischte Typen (Zahl +
  # String) auf einen gemeinsamen Typ, aus 0/1 wurde also "0"/"1" (Strings).
  # Plotly.js konnte diese String-Positionen nicht als Farbverlaufs-Stuetz-
  # stellen interpretieren und ist auf seine eigene (bunte) Standard-
  # Farbskala zurueckgefallen, statt der beabsichtigten reinen Grauskala
  # weiss/grau46 (wie im Original 21_plot_map.R).
  # WICHTIG #2: "gray46" (R/X11-Farbname) statt "#757575" (Hex) fuehrte zum
  # selben Symptom aus einem anderen Grund - Plotly.js' Farbparser (d3-color)
  # kennt nur die ~148 CSS/SVG-Standardfarbnamen, nicht R's erweiterte X11-
  # Graustufen-Namen (gray0...gray100). "gray46" wurde daher still verworfen
  # und die Legende fiel auf eine Standard-Warmfarbskala zurueck, obwohl die
  # rechte JSON-Daten (colorscale) korrekt aussahen - die im Bild gebackenen
  # Wachstumskreise blieben davon unberuehrt, da tage_farbe() ueber R's
  # EIGENE Grafik-Engine (die "gray46" kennt) gerendert wird. Fix: expliziter
  # Hex-Code statt Farbname.
  # hoverlabel.bgcolor je Punkt = dieselbe Graustufe wie der Wachstumskreis
  # (tage_farbe()) - der Tooltip-Hintergrund passt dadurch farblich zum
  # angeklickten/gehoverten Standort statt Plotlys Standardfarbe zu zeigen.
  # Schrift bleibt konstant schwarz (bei weiss bis gray46 immer lesbar).
  snap$daysold_col <- tage_farbe(snap$daysold)
  fig %>% add_trace(
    data = snap, x = ~lon, y = ~lat, type = "scatter", mode = "markers",
    marker = list(size = 30, color = ~daysold, colorscale = list(list(0, "white"), list(1, "#757575")),
                  cmin = 0, cmax = 14, showscale = TRUE, opacity = 0,
                  colorbar = list(
                    title = list(text = "Tage seit\nMessung", font = legenden_font),
                    tickfont = legenden_font, len = 0.32, y = 0.1, yanchor = "bottom"
                  )),
    hovertext = ~hover, hoverinfo = "text",
    hoverlabel = list(bgcolor = ~daysold_col, font = list(color = "black")),
    showlegend = FALSE, visible = FALSE, name = titel
  )
}

fig_wachstum <- plot_ly(height = 560)
map_point_orts <- list()
graswachstum_bild_je_woche <- list()
afc_ring_bild_je_woche <- list()
afc_fenster_je_woche <- list()

for (i in seq_len(nrow(map_wochen))) {
  jr <- map_wochen$jahr[i]; w <- map_wochen$week[i]
  snap <- map_snapshots %>% filter(jahr == jr, week == w) %>% arrange(Ort)
  fig_wachstum <- baue_kartenwerte_trace(fig_wachstum, snap, "growth", "kg TS/ha/Tag",
                                          paste("Wachstum", jr, "KW", w))
  map_point_orts[[i]] <- as.character(snap$Ort)
  graswachstum_bild_je_woche[[paste(jr, w)]] <- baue_graswachstum_bild(snap)
  afc_ergebnis <- baue_afc_ring_bild(snap, montag_von_woche(jr, w))
  afc_ring_bild_je_woche[[paste(jr, w)]] <- afc_ergebnis$bild
  afc_fenster_je_woche[[paste(jr, w)]] <- afc_ergebnis$fensterIdx
}
cat("Graswachstums-Hintergrundbilder erzeugt:", sum(!vapply(graswachstum_bild_je_woche, is.null, logical(1))), "\n")
cat("AFC-Ring-Hintergrundbilder erzeugt:", sum(!vapply(afc_ring_bild_je_woche, is.null, logical(1))), "\n")

########################################################################
## 1b. MeteoSchweiz-Wetterstationen (Referenz-Ebene, standardmaessig aus) -
##     eigener Umschalter neben "Messnetz-Standorte": zeigt die oeffentlich
##     gemeldeten MeteoSchweiz-Automatikstationen (SwissMetNet) mit ihren
##     aktuellsten Tageswerten (Lufttemperatur, Bodentemperatur - nur an
##     einem Teil der Stationen gemessen -, Niederschlag, Globalstrahlung,
##     Sonnenscheindauer). Von der Kalenderwoche UNABHAENGIG (immer der
##     aktuellste verfuegbare Wert) - anders als die AGFF-Grasmessungen sind
##     dies rein meteorologische Referenzstationen, kein Vegetationsbezug.
## Quelle: ch.meteoschweiz.ogd-smn (opendata.swiss). Bodentemperatur wird
## (Stand 2026) nur an rund 20 der 158 Automatikstationen gemessen - an
## allen anderen zeigt der Tooltip dafuer "keine Daten".
########################################################################
smn_basis_url <- "https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/"
smn_dir <- file.path(geodata_dir, "smn")
dir.create(smn_dir, recursive = TRUE, showWarnings = FALSE)

# Nur noch die STATIONS-METADATEN (Name/Lage/Kanton/Hoehe) werden hier beim
# R-Lauf geladen - die taeglich aktuellen Messwerte (Temperatur, Nieder-
# schlag etc.) holt die Webapp SELBST per fetch() direkt vom MeteoSchweiz-
# Open-Data-Server (data.geo.admin.ch liefert Access-Control-Allow-Origin:
# * - CORS erlaubt das, siehe ladeSmnAktuellwerte()/JS), NICHT mehr hier
# beim Bauen der Seite. Grund: eine einmal hochgeladene Seite zeigt so
# IMMER die aktuellsten MeteoSchweiz-Werte, auch wenn der Rechner, der die
# Seite erzeugt/hochlaedt, laengst nicht mehr laeuft/online ist - vorher
# waren die Werte nur so aktuell wie der letzte R-Lauf. Nebenbei entfaellt
# der bisher langsamste Teil des R-Laufs (bis zu ~150 einzelne CSV-
# Downloads nacheinander).
smn_stationen_meta_liste <- tryCatch({
  # Metadaten (Stationsliste, Parameterverfuegbarkeit je Station) - guenstig
  # dauerhaft zwischengespeichert (aendert sich praktisch nie).
  smn_stationen_datei <- file.path(smn_dir, "meta_stations.csv")
  if (!file.exists(smn_stationen_datei)) {
    download.file(paste0(smn_basis_url, "ogd-smn_meta_stations.csv"), smn_stationen_datei, quiet = TRUE, mode = "wb")
  }
  smn_inventar_datei <- file.path(smn_dir, "meta_datainventory.csv")
  if (!file.exists(smn_inventar_datei)) {
    download.file(paste0(smn_basis_url, "ogd-smn_meta_datainventory.csv"), smn_inventar_datei, quiet = TRUE, mode = "wb")
  }
  smn_stationen_meta <- read.csv(smn_stationen_datei, sep = ";", fileEncoding = "ISO-8859-1", stringsAsFactors = FALSE)
  smn_inventar <- read.csv(smn_inventar_datei, sep = ";", fileEncoding = "ISO-8859-1", stringsAsFactors = FALSE)

  # Nur Stationen, die AKTUELL (kein Enddatum) Lufttemperatur melden -
  # filtert stillgelegte/rein historische Stationen heraus.
  smn_aktive_abbr <- unique(smn_inventar$station_abbr[
    smn_inventar$parameter_shortname == "tre200d0" & trimws(smn_inventar$data_till) == ""
  ])
  smn_stationen_meta <- smn_stationen_meta[smn_stationen_meta$station_abbr %in% smn_aktive_abbr, ]
  cat("MeteoSchweiz-Stationen (aktiv):", nrow(smn_stationen_meta), "\n")

  smn_stationen_meta %>%
    transmute(
      abbr = station_abbr,
      lon = station_coordinates_wgs84_lon,
      lat = station_coordinates_wgs84_lat,
      name = station_name,
      kanton = station_canton,
      hoehe = round(station_height_masl)
    )
}, error = function(e) {
  cat("MeteoSchweiz-Stationen: Metadaten laden fehlgeschlagen -", conditionMessage(e), "\n")
  data.frame(abbr = character(0), lon = numeric(0), lat = numeric(0), name = character(0), kanton = character(0), hoehe = numeric(0))
})

# Naechster (0-basierter) Trace-Index in fig_wachstum: die per-Woche-
# Snapshot-Traces oben belegen exakt die Indizes 0..(nrow(map_wochen)-1), die
# neue Stationen-Trace kommt direkt danach - unabhaengig von Jahr/Woche
# EINMALIG angelegt, nicht Teil des mapWochen-Sichtbarkeits-Arrays in
# applyMapState() (JS), daher ein eigener, separat gemerkter Index.
smn_stationen_trace_idx <- nrow(map_wochen)
fig_wachstum <- fig_wachstum %>% add_trace(
  data = smn_stationen_meta_liste, x = ~lon, y = ~lat, type = "scatter", mode = "markers",
  marker = list(symbol = "diamond", size = 9, color = "#2b2b2b", line = list(color = "white", width = 1)),
  # Platzhalter bis ladeSmnAktuellwerte()/JS die echten Tageswerte per
  # fetch() nachgeladen und den Hovertext per restyle() ersetzt hat (siehe
  # js_template).
  hovertext = ~paste0("<b>", name, "</b> (", kanton, ", ", hoehe, " m ü. M.)<br>Lädt aktuelle Werte..."),
  hoverinfo = "text",
  showlegend = FALSE, visible = FALSE, name = "MeteoSchweiz-Stationen"
)

fig_wachstum <- fig_wachstum %>% layout(
  title = list(text = "Graswachstum (kg TS/ha/Tag)", font = list(size = 16)),
  xaxis = list(visible = FALSE, range = lon_range_erweitert, fixedrange = FALSE),
  yaxis = list(visible = FALSE, range = lat_range, scaleanchor = "x", scaleratio = karten_scaleratio),
  margin = list(t = 40, b = 10, l = 10, r = 10),
  images = list(kartenbild_hintergrund)
) %>% config(responsive = FALSE, scrollZoom = TRUE)
# responsive=FALSE: mit responsive=TRUE hat Plotly die Karte in der
# kombinierten Seite (htmltools::save_html(), mehrere Widgets) wiederholt
# auf eine falsche, zu grosse Hoehe aufgeblasen (Ueberlappung mit dem
# darunterliegenden Schieberegler/Diagramm) - vermutlich weil der
# ResizeObserver beim initialen Seitenaufbau (Flexbox-Reflow durch das
# zweite Widget) einen falschen Zwischenzustand misst. Fixe Hoehe (560px)
# statt dessen, wie gewuenscht - die Seite scrollt bei Bedarf normal.
#
# ABER: htmlwidgets setzt dem Plotly-Container-Div initial ein eigenes
# Inline-Style "height:400px" (unabhaengig vom hier gesetzten layout$height)
# - ohne responsive=TRUE (das dies sonst via ResizeObserver korrigiert haette,
# siehe oben) bleibt es bei diesen falschen 400px haengen, auch wenn Plotlys
# INTERNER layout-Zustand korrekt 560 zeigt. Ein einmaliger, expliziter
# Resize-Aufruf beim Binden des Widgets erzwingt die korrekte Groesse, ohne
# die Ueberlappungs-Problematik von responsive=TRUE wieder einzufuehren.
# scrollZoom=TRUE: erlaubt Zwei-Finger-Pinch-Zoom auf Touchgeraeten (und
# Mausrad-Zoom) - ohne diese Option liess sich die Karte auf Mobile gar
# nicht vergroessern (Ein-Finger-Ziehen scrollt nur die Seite).
fig_wachstum <- htmlwidgets::onRender(fig_wachstum, sprintf("
function(el, x) {
  // Feste Kartenausmasse aus R (lon_range_erweitert/lat_range/
  // karten_scaleratio) - fuer die Ansichts-Berechnung unten EXPLIZIT
  // mitgegeben statt sich auf Plotlys eigene scaleanchor/scaleratio-
  // Bereichsanpassung zu verlassen: die hat sich bei mehreren
  // relayout()-Aufrufen (Breite/Hoehe aendert sich mehrfach) als instabil
  // erwiesen - je nach vorherigem Zwischenzustand blieb mal die x-, mal die
  // y-Achse auf einen viel zu grossen Bereich gestreckt, mit einem winzigen
  // Kartenfleck inmitten viel Leerraum als Resultat.
  var xMin = %s, xMax = %s, yMitte = %s, scaleratio = %s;
  var xSpan = xMax - xMin;
  // Nur die Standort-Snapshot-Traces (eine je Jahr/Woche) haben ueberhaupt
  // eine sinnvolle 'Tage seit Messung'-Farbskala - explizit auf diese
  // Indizes beschraenkt, statt showscale ohne Index-Array zu setzen (das
  // wuerde JEDE Trace treffen, auch spaeter hinzugefuegte wie die
  // MeteoSchweiz-Stationen, und dort eine bedeutungslose Leer-Colorbar
  // erzeugen).
  var snapshotTraceIdx = Array.from({ length: %s }, function(_, i) { return i; });

  // 'Ganze Schweiz'-Ansicht (x-/y-Achsenbereich) fuer eine gegebene
  // Containergroesse - x bleibt immer auf dem vollen lon_range_erweitert
  // (nutzt die volle Breite), y wird so berechnet, dass bei diesem
  // Seitenverhaeltnis exakt keine Rand-Leerflaeche entsteht (weder
  // gestaucht noch gestreckt). Wird unten sowohl fuer die initiale/
  // Resize-Ansicht als auch fuer die Zoom-Sperre und den 'Ganze Schweiz'-
  // Knopf gebraucht - deshalb als eigene Funktion statt nur inline fuer
  // den Mobile-Fall (wie zuvor).
  function vollAnsichtBerechnen(breite, hoehe) {
    var plotBreite = breite - 20, plotHoehe = hoehe - 50;
    var ySpan = xSpan * plotHoehe / (scaleratio * plotBreite);
    return { x: [xMin, xMax], y: [yMitte - ySpan / 2, yMitte + ySpan / 2] };
  }
  var vollX = null, vollY = null;

  function fixiereGroesse() {
    var breite = el.parentElement.clientWidth;
    var mobil = breite < 700;
    // Schmaler (Mobile-)Container: eigene, kleinere Hoehe statt der festen
    // Desktop-Hoehe (560px) - der y-Achsenbereich wird fuer BEIDE Faelle
    // ueber vollAnsichtBerechnen() explizit gesetzt (nicht Plotlys eigene,
    // s.o. instabile Bereichsanpassung).
    var hoehe = mobil ? Math.round(Math.max(200, (breite - 20) * 0.65 + 50)) : 560;
    var voll = vollAnsichtBerechnen(breite, hoehe);
    vollX = voll.x; vollY = voll.y;
    // Tage-seit-Messung-Farblegende (Colorbar) auf dem schmalen
    // Handy-Bildschirm ausgeblendet: sie nimmt proportional viel Platz
    // weg, die Graustufen sind an den Standort-Kreisen selbst ohnehin
    // ablesbar. Per restyle (nicht nur CSS), damit Plotly den dafuer
    // reservierten Rand auch wirklich freigibt.
    Plotly.restyle(el, { 'marker.showscale': !mobil }, snapshotTraceIdx);
    Plotly.relayout(el, { width: breite, height: hoehe, 'xaxis.range': voll.x, 'yaxis.range': voll.y });
    // Container-Hoehe (CSS, fest 560px im HTML) der tatsaechlichen, hier
    // berechneten Kartenhoehe nachfuehren - sonst bleibt auf Mobile (kleinere
    // hoehe) darunter Leerraum im Container stehen, in dem die Zoom-
    // Steuerung (position:absolute, bottom:10px relativ zu diesem Container)
    // dann weit unterhalb der sichtbar gezeichneten Karte haengen wuerde.
    el.parentElement.style.height = hoehe + 'px';
  }
  fixiereGroesse();
  window.addEventListener('resize', fixiereGroesse);

  // Weiteres Herauszoomen ueber die 'ganze Schweiz'-Ansicht hinaus sperren
  // und dabei automatisch zentrieren: sobald der sichtbare Bereich (per
  // Mausrad/Pinch/Doppelklick/Plotly-eigener Modebar) die volle Ansicht
  // erreicht oder ueberschreitet, sofort auf die EXAKTE volle Ansicht
  // zurueckspringen - unabhaengig davon, WIE gezoomt/verschoben wurde.
  // zoomKorrekturLaeuft verhindert eine Endlosschleife durch den
  // relayout()-Aufruf der Korrektur selbst (loest wieder plotly_relayout
  // aus).
  var zoomKorrekturLaeuft = false;
  el.on('plotly_relayout', function(ev) {
    if (zoomKorrekturLaeuft || !vollX) return;
    var betroffen = Object.keys(ev).some(function(k) {
      return k.indexOf('xaxis') === 0 || k.indexOf('yaxis') === 0;
    });
    if (!betroffen) return;
    var xr = el.layout.xaxis.range;
    if (!xr || (xr[1] - xr[0]) >= (vollX[1] - vollX[0]) - 1e-6) {
      zoomKorrekturLaeuft = true;
      Plotly.relayout(el, { 'xaxis.range': vollX, 'yaxis.range': vollY })
        .then(function() { zoomKorrekturLaeuft = false; });
    }
  });

  // Keine eigenen +/-/CH-Knoepfe (mehr) - Plotlys eigene Modebar (oben,
  // bei Hover ueber der Karte eingeblendet) hat bereits Zoom-In/-Out/
  // Autoscale/Reset-Achsen-Knoepfe, die dasselbe leisten; die Zoom-Sperre
  // oben (plotly_relayout-Listener) greift unabhaengig davon, WIE gezoomt
  // wird (Mausrad, Pinch, Doppelklick, Modebar).
}
", lon_range_erweitert[1], lon_range_erweitert[2], mean(lat_range), karten_scaleratio, nrow(map_wochen)))

########################################################################
## 3b. Optionale Hintergrund-Ebenen: Niederschlag (Vorwoche) und
##     Bodenwasserbilanz (Stichtag Wochenbeginn) - je ein vorgerendertes
##     PNG pro (Jahr, Kalenderwoche), als data:-URI eingebettet und ueber
##     Plotly layout.images unter die Kartenmarker gelegt.
########################################################################

# Raster (LV95) -> data:-URI (PNG, WGS84) fuer layout.images. NA-Zellen
# werden transparent, damit darunter die Kantonsflaeche (bzw. bei aktiver
# Ebene: deren transparent geschaltete Flaeche, siehe onRender()) plus See-
# Ebene durchscheinen. ziel_ncol haelt die Bilder klein (schnelleres Laden,
# kleinere HTML-Datei) - fuer eine Hintergrund-Ebene reicht diese Aufloesung.
# Auf die Landesgrenze maskiert (wie in 25_plot_niederschlag_wasserhaushalt_
# karte.R) - sonst waere die Flaeche ein Rechteck bis zum Rand der Bounding
# Box statt nur innerhalb der Schweiz sichtbar. farben: Vektor mit >=2
# Farben, z.B. der mehrstufige Niederschlags-/Bodenwasserbilanz-Farbverlauf
# aus den frueheren Export-Karten (21_plot_map.R / 25_...R). stuetzstellen
# (optional, Werte 0..1, gleiche Laenge wie farben): erlaubt einen NICHT
# gleichmaessig verteilten Farbverlauf (z.B. Niederschlag Vorwoche: die
# ersten 5 Farben auf 0-30mm konzentriert, die letzten 2 erst ab 30mm) -
# ohne Angabe gleichmaessig verteilt wie bisher.
#
# Gibt zusaetzlich zum Bild ein grobes WERTE-Gitter zurueck (dieselbe,
# bereits fuer das Bild heruntergerechnete Aufloesung - keine zusaetzliche
# Neuberechnung noetig): Grundlage fuer die Cursor-Wertabfrage im Ebenen-
# Kasten (siehe onRender() weiter unten), da die Bild-Ebene selbst ein
# reines PNG ist und keine Plotly-Hoverdaten hat.
raster_zu_datauri <- function(r, farben, wertebereich, ziel_ncol = 180, alpha = 0.75, stuetzstellen = NULL) {
  r_wgs <- terra::project(r, "EPSG:4326", method = "bilinear")
  r_wgs <- terra::mask(r_wgs, schweiz_grenze_vect)
  faktor <- max(1, round(terra::ncol(r_wgs) / ziel_ncol))
  if (faktor > 1) r_wgs <- terra::aggregate(r_wgs, fact = faktor, fun = "mean", na.rm = TRUE)
  m <- terra::as.matrix(r_wgs, wide = TRUE)
  h <- nrow(m); w <- ncol(m)
  anteil <- (m - wertebereich[1]) / diff(wertebereich)
  anteil[anteil < 0] <- 0
  anteil[anteil > 1] <- 1
  if (is.null(stuetzstellen)) stuetzstellen <- seq(0, 1, length.out = length(farben))
  palette <- scales::gradient_n_pal(farben, values = stuetzstellen)
  na_maske <- is.na(as.vector(anteil))
  anteil_ohne_na <- anteil; anteil_ohne_na[is.na(anteil_ohne_na)] <- 0
  rgb_werte <- t(grDevices::col2rgb(palette(as.vector(anteil_ohne_na))))
  img <- array(0, dim = c(h, w, 4))
  img[, , 1] <- matrix(rgb_werte[, 1] / 255, nrow = h, ncol = w)
  img[, , 2] <- matrix(rgb_werte[, 2] / 255, nrow = h, ncol = w)
  img[, , 3] <- matrix(rgb_werte[, 3] / 255, nrow = h, ncol = w)
  img[, , 4] <- matrix(ifelse(na_maske, 0, alpha), nrow = h, ncol = w)
  tmp <- tempfile(fileext = ".png")
  png::writePNG(img, tmp)
  b64 <- base64enc::base64encode(tmp)
  unlink(tmp)
  ext_r <- terra::ext(r_wgs)
  list(
    bild = list(
      source = paste0("data:image/png;base64,", b64),
      xref = "x", yref = "y",
      x = ext_r$xmin, y = ext_r$ymax,
      sizex = ext_r$xmax - ext_r$xmin, sizey = ext_r$ymax - ext_r$ymin,
      xanchor = "left", yanchor = "top", sizing = "stretch", layer = "below"
    ),
    werte = list(
      x0 = ext_r$xmin, x1 = ext_r$xmax, y0 = ext_r$ymin, y1 = ext_r$ymax,
      ncol = w, nrow = h,
      m = lapply(seq_len(h), function(i) round(m[i, ], 1))
    )
  )
}

# Farbverlaeufe UND Quellenangaben, abgeleitet von den frueheren Export-
# Karten (25_plot_niederschlag_wasserhaushalt_karte.R): mehrstufiger
# trocken(rot)-nass(blau)-Verlauf fuer Niederschlag bzw. firebrick-khaki1-
# steelblue fuer die Bodenwasserbilanz, statt der bisherigen einfachen
# Zweifarben-Verlaeufe.
niederschlag_farben <- c("darkred", "red", "orange", "gold", "lightskyblue", "steelblue", "darkblue")
bodenwasser_farben <- c("firebrick", "khaki1", "steelblue")
niederschlag_quelle <- "MeteoSchweiz RhiresD/RprelimD, 1km-Raster."
bodenwasser_quelle <- "Bucket-Modell (Niederschlag - Hargreaves-ET0) - kein Ersatz fuer Feldmessung."

# Waehlbare Fenstergroessen (Tage) fuer alle "gleitendes Fenster"-Ebenen
# (Summe/Mittelwert der letzten N Tage vor dem Stichtag) - im Ebenen-Kasten
# per Schieberegler waehlbar (JS: meteoFenster), Vorbelegung je Ebene siehe
# meteoFensterStandard (JS). Ersetzt die bisher fest verdrahteten
# Einzelfenster (7-Tage-"Vorwoche", 28-Tage-Kalendermonat fuer Niederschlag,
# 14-Tage-Bodentemperatur-Daempfung).
fenstergroessen_tage <- c(7, 14, 21, 28)

niederschlag_stuetzstellen <- c(0, 7.5, 15, 22.5, 30, 65, 100) / 100

# Baut eine "gleitendes Fenster"-Hintergrund-Ebene fuer ALLE Fenstergroessen
# (fenstergroessen_tage) auf einmal - raster_holen(jr) liefert je Jahr
# list(raster=<SpatRaster>, tage=<Date-Vektor>) oder NULL (keine Daten fuer
# dieses Jahr). Bei Summen (aggregat="summe") skaliert der Wertebereich
# (bereich_je_7tage) PROPORTIONAL zur Fenstergroesse mit (4x mehr Tage ->
# 4x hoehere moegliche Summe); bei Mittelwerten (aggregat="mittel") bleibt
# er FIX - ein laengeres Fenster liefert nur einen gedaempfteren, nicht
# systematisch groesseren/kleineren Wert. stuetzstellen (optional, Werte
# 0..1) gilt unveraendert fuer alle Fenstergroessen, da sie sich auf den
# ANTEIL des (mitskalierenden) Bereichs bezieht, nicht auf absolute Werte.
# Rueckgabe: benannte Liste je Fenstergroesse mit $bilder/$werte (siehe
# schreibe_fenster_ebenen_dateien() weiter unten).
baue_fenster_ebenen <- function(jahre, raster_holen, aggregat, farben, bereich_je_7tage, stuetzstellen = NULL) {
  ergebnis <- list()
  for (fenster in fenstergroessen_tage) {
    bild_je_woche <- list()
    werte_je_woche <- list()
    bereich <- if (aggregat == "summe") bereich_je_7tage * fenster / 7 else bereich_je_7tage
    for (jr in jahre) {
      r_info <- raster_holen(jr)
      if (is.null(r_info)) next
      tage_r <- r_info$tage
      for (w in alle_wochen) {
        fenster_ende <- montag_von_woche(jr, w) - 1
        fenster_start <- fenster_ende - (fenster - 1)
        if (fenster_start < min(tage_r) || fenster_ende > max(tage_r)) next
        idx <- which(tage_r >= fenster_start & tage_r <= fenster_ende)
        if (length(idx) == 0) next
        r_wert <- if (aggregat == "summe") {
          clamp(sum(r_info$raster[[idx]], na.rm = TRUE), lower = 0)
        } else {
          mean(r_info$raster[[idx]], na.rm = TRUE)
        }
        bild_ergebnis <- raster_zu_datauri(r_wert, farben, bereich, stuetzstellen = stuetzstellen)
        bild_je_woche[[paste(jr, w)]] <- bild_ergebnis$bild
        werte_je_woche[[paste(jr, w)]] <- bild_ergebnis$werte
      }
    }
    ergebnis[[as.character(fenster)]] <- list(bilder = bild_je_woche, werte = werte_je_woche)
  }
  ergebnis
}

## Niederschlag: gleitendes Fenster (Summe) --------------------------------
niederschlag_fenster_ergebnisse <- baue_fenster_ebenen(
  jahre_mit_niederschlag,
  function(jr) {
    if (!jr %in% names(niederschlag_raster_je_jahr)) return(NULL)
    r <- niederschlag_raster_je_jahr[[jr]]
    list(raster = r, tage = as.Date(time(r)))
  },
  aggregat = "summe", farben = niederschlag_farben, bereich_je_7tage = c(0, 100),
  stuetzstellen = niederschlag_stuetzstellen
)
cat("Niederschlags-Hintergrundbilder erzeugt:",
    sum(vapply(niederschlag_fenster_ergebnisse, function(x) length(x$bilder), integer(1))), "\n")

## Temperatur 2m: gleitendes Fenster (Mittelwert TabsD) --------------------
## Farbskala orientiert an fuer Graswachstum relevanten Schwellen: unter ca.
## 5°C kaum Wachstum, 15-20°C guenstig, ueber 25°C Hitzestress (siehe
## i-Button-Erklaerung weiter unten).
temperatur_farben <- c("darkblue", "steelblue", "lightskyblue", "palegreen3", "gold", "orange", "red")
temperatur_quelle <- "MeteoSchweiz TabsD, 1km-Raster (Tagesmitteltemperatur 2m)."

temperatur_fenster_ergebnisse <- baue_fenster_ebenen(
  jahre_mit_temperatur,
  function(jr) {
    if (!jr %in% names(temperatur_raster_je_jahr)) return(NULL)
    r <- temperatur_raster_je_jahr[[jr]]
    list(raster = r, tage = as.Date(time(r)))
  },
  aggregat = "mittel", farben = temperatur_farben, bereich_je_7tage = c(0, 30)
)
cat("Temperatur-Hintergrundbilder erzeugt:",
    sum(vapply(temperatur_fenster_ergebnisse, function(x) length(x$bilder), integer(1))), "\n")

## Bodentemperatur (SCHAETZUNG): gleitendes Fenster (Mittelwert TabsD) -----
## Kein eigenes Bodentemperatur-Rasterprodukt frei verfuegbar (MeteoSchweiz
## misst Bodentemperatur nur an einzelnen Stationen, nicht flaechendeckend
## als Karte). Der gleitende Mittelwert der Lufttemperatur bildet naeherungs-
## weise die Daempfung/Verzoegerung ab, mit der die oberste Bodenschicht
## (ca. 5-10cm) der Lufttemperatur folgt - eine in der Agrarmeteorologie
## gebraeuchliche Naeherung, aber KEINE Feldmessung (siehe i-Button-
## Erklaerung weiter unten). Ueber den Schieberegler laesst sich das Fenster
## verlaengern, um mehr Daempfung zu simulieren.
bodentemperatur_quelle <- "Schaetzung: gleitender Mittelwert aus MeteoSchweiz TabsD (2m-Lufttemperatur) - keine direkte Bodenmessung."

bodentemperatur_fenster_ergebnisse <- baue_fenster_ebenen(
  jahre_mit_temperatur,
  function(jr) {
    if (!jr %in% names(temperatur_raster_je_jahr)) return(NULL)
    r <- temperatur_raster_je_jahr[[jr]]
    list(raster = r, tage = as.Date(time(r)))
  },
  aggregat = "mittel", farben = temperatur_farben, bereich_je_7tage = c(0, 30)
)
cat("Bodentemperatur-Hintergrundbilder erzeugt (Schaetzung):",
    sum(vapply(bodentemperatur_fenster_ergebnisse, function(x) length(x$bilder), integer(1))), "\n")

## Bodenwasserbilanz zum Stichtag (Montag) der gewaehlten Kalenderwoche -----
bodenwasser_bild_je_woche <- list()
bodenwasser_werte_je_woche <- list()
# Tatsaechlich verwendetes Datum je Woche (siehe Fallback unten - nicht
# immer exakt der Montag) - fuer die Anzeige in der Ebenen-Legende
# (aktualisiereLayerLabels() in onRender()), damit dort das ECHTE Datum
# des Snapshots steht statt ein pauschales "Wochenbeginn".
bodenwasser_datum_je_woche <- list()
speicher_tif <- file.path(wasserhaushalt_dir, "_checkpoint_speicher.tif")
if (file.exists(speicher_tif)) {
  speicher_r <- terra::rast(speicher_tif)
  speicher_daten_tage <- as.Date(sub("^Speicher_", "", names(speicher_r)))
  for (jr in alle_jahre) {
    for (w in alle_wochen) {
      stichtag <- montag_von_woche(jr, w)
      # Der Speicher-Checkpoint hinkt der Verarbeitung oft 1-2 Tage hinterher
      # (siehe Kommentar in 25_plot_niederschlag_wasserhaushalt_karte.R) - ein
      # exakter Treffer auf den Montag fehlt dadurch regelmaessig ausgerechnet
      # fuer die jeweils AKTUELLE (laufende) Woche. Stattdessen der
      # naechstgelegene VERFUEGBARE Tag bis zu 6 Tage VOR dem Montag (nie
      # danach - sonst waere es keine "Wochenbeginn"-Momentaufnahme mehr).
      passende_tage <- which(speicher_daten_tage <= stichtag & speicher_daten_tage >= stichtag - 6)
      if (length(passende_tage) == 0) next
      idx <- passende_tage[which.max(speicher_daten_tage[passende_tage])]
      ergebnis <- raster_zu_datauri(speicher_r[[idx]], bodenwasser_farben, c(0, 100))
      bodenwasser_bild_je_woche[[paste(jr, w)]] <- ergebnis$bild
      bodenwasser_werte_je_woche[[paste(jr, w)]] <- ergebnis$werte
      bodenwasser_datum_je_woche[[paste(jr, w)]] <- format(speicher_daten_tage[idx], "%d.%m.%Y")
    }
  }
  cat("Bodenwasserbilanz-Hintergrundbilder erzeugt:", length(bodenwasser_bild_je_woche), "\n")
} else {
  cat("Bodenwasserbilanz nicht verfuegbar (", speicher_tif, " nicht gefunden)\n")
}

## Sonnenscheindauer (relativ): gleitendes Fenster (Mittelwert) -----------
sonnenschein_farben <- c("dimgray", "gray70", "khaki1", "gold", "orange")
sonnenschein_quelle <- "MeteoSchweiz SrelD, 1km-Raster (Sonnenscheindauer relativ zum astronomisch Moeglichen)."

sonnenschein_fenster_ergebnisse <- baue_fenster_ebenen(
  jahre_mit_sonnenschein,
  function(jr) {
    if (!jr %in% names(sonnenschein_raster_je_jahr)) return(NULL)
    r <- sonnenschein_raster_je_jahr[[jr]]
    list(raster = r, tage = as.Date(time(r)))
  },
  aggregat = "mittel", farben = sonnenschein_farben, bereich_je_7tage = c(0, 100)
)
cat("Sonnenschein-Hintergrundbilder erzeugt:",
    sum(vapply(sonnenschein_fenster_ergebnisse, function(x) length(x$bilder), integer(1))), "\n")

## Verdunstung ET0 (Hargreaves): gleitendes Fenster (Summe) ---------------
## Aus dem bereits in r-futterbaugutachten berechneten ET0-Raster (siehe
## wasserhaushalt_dir, gleiche Quelle wie die Bodenwasserbilanz oben).
et0_farben <- c("lightyellow", "gold", "orange", "red")
et0_quelle <- "Bucket-Modell-Verdunstung (Hargreaves/FAO-56) aus r-futterbaugutachten - kein Ersatz fuer Feldmessung."
et0_tif <- file.path(wasserhaushalt_dir, "et0_hargreaves.tif")
if (file.exists(et0_tif)) {
  et0_r <- terra::rast(et0_tif)
  et0_tage <- as.Date(sub("^ET0_", "", names(et0_r)))
  et0_fenster_ergebnisse <- baue_fenster_ebenen(
    alle_jahre,
    function(jr) list(raster = et0_r, tage = et0_tage),
    aggregat = "summe", farben = et0_farben, bereich_je_7tage = c(0, 25)
  )
  cat("ET0-Hintergrundbilder erzeugt:",
      sum(vapply(et0_fenster_ergebnisse, function(x) length(x$bilder), integer(1))), "\n")
} else {
  et0_fenster_ergebnisse <- setNames(
    lapply(fenstergroessen_tage, function(f) list(bilder = list(), werte = list())),
    as.character(fenstergroessen_tage)
  )
  cat("ET0 nicht verfuegbar (", et0_tif, " nicht gefunden)\n")
}

## Kumulierte Wachstumsgradtage zum Stichtag (Montag) der gewaehlten
## Kalenderwoche - aus gdd_kumuliert_je_jahr oben (bereits laufend
## aufsummiert), exakter Tagestreffer statt Fallback wie bei der
## Bodenwasserbilanz (temperatur_raster_je_jahr ist eine LUECKENLOSE
## Tagesreihe, siehe Ladelogik oben - ein Fallback auf den naechstgelegenen
## Tag ist daher nicht noetig).
gdd_farben <- c("white", "yellow", "orange", "darkred")
gdd_quelle <- "Kumuliert aus MeteoSchweiz TabsD (Basis 5 Grad C) seit Beginn der lokal vorhandenen Temperaturdaten."
gdd_bild_je_woche <- list()
gdd_werte_je_woche <- list()
for (jr in names(gdd_kumuliert_je_jahr)) {
  r_jahr <- gdd_kumuliert_je_jahr[[jr]]
  tage_r <- as.Date(time(r_jahr))
  for (w in alle_wochen) {
    stichtag <- montag_von_woche(jr, w)
    idx <- which(tage_r == stichtag)
    if (length(idx) == 0) next
    ergebnis <- raster_zu_datauri(r_jahr[[idx]], gdd_farben, c(0, 2500))
    gdd_bild_je_woche[[paste(jr, w)]] <- ergebnis$bild
    gdd_werte_je_woche[[paste(jr, w)]] <- ergebnis$werte
  }
}
cat("Wachstumsgradtage-Hintergrundbilder erzeugt:", length(gdd_bild_je_woche), "\n")

## Optionale Hintergrund-Ebenen NICHT in die Haupt-HTML einbetten, sondern
## je Ebene in eine EIGENE JSON-Datei schreiben (outputs/ebenen/<name>.json) -
## wird clientseitig erst beim ERSTEN Auswaehlen der jeweiligen Ebene per
## fetch() nachgeladen (siehe ladeEbene() im js_template) und danach im
## Browser zwischengespeichert. Reduziert die Groesse der Haupt-HTML massiv:
## die meisten Betrachter sehen nie alle 8 optionalen Ebenen, muessen also
## auch nicht deren ~200+ Bilder mitladen, nur um die Seite zu oeffnen.
## Graswachstum/AFC (Standard AN) und die Kantons-/Seen-Basiskarte bleiben
## bewusst eingebettet - die sieht ohnehin jede/r sofort beim Laden.
ebenen_dir <- file.path(out_dir, "ebenen")
dir.create(ebenen_dir, recursive = TRUE, showWarnings = FALSE)

# Schreibt eine Ebene als JSON-Datei und gibt ihre (Jahr, Woche)-Schluessel
# zurueck - diese Schluessel-Liste allein (winzig gegenueber den Bildern)
# wird weiterhin eingebettet, damit z.B. "Jahr X hat keine Daten fuer Ebene Y"
# (Radiobutton ausgrauen) OHNE die grosse JSON-Datei geladen werden muss.
schreibe_ebene_datei <- function(name, bilder, werte, datum = NULL) {
  inhalt <- list(bilder = bilder, werte = werte)
  if (!is.null(datum)) inhalt$datum <- datum
  jsonlite::write_json(inhalt, file.path(ebenen_dir, paste0(name, ".json")), auto_unbox = TRUE, na = "null")
  names(bilder)
}

# Schreibt ALLE Fenstergroessen einer "gleitendes Fenster"-Ebene als je
# eigene Datei (<name>_<fenster>.json, siehe baue_fenster_ebenen() oben) und
# liefert die UNION ihrer (Jahr, Woche)-Schluessel zurueck - fuer die
# Jahres-Verfuegbarkeit des Radiobuttons (aktualisiereLayerVerfuegbarkeit(),
# JS), unabhaengig davon, welche Fenstergroesse gerade gewaehlt ist.
schreibe_fenster_ebenen_dateien <- function(name, fenster_ergebnisse) {
  alle_schluessel <- character(0)
  for (fenster in names(fenster_ergebnisse)) {
    r <- fenster_ergebnisse[[fenster]]
    neue_schluessel <- schreibe_ebene_datei(paste0(name, "_", fenster), r$bilder, r$werte)
    alle_schluessel <- union(alle_schluessel, neue_schluessel)
  }
  alle_schluessel
}

ebenen_schluessel <- list(
  niederschlag = schreibe_fenster_ebenen_dateien("niederschlag", niederschlag_fenster_ergebnisse),
  boden = schreibe_ebene_datei("boden", bodenwasser_bild_je_woche, bodenwasser_werte_je_woche, bodenwasser_datum_je_woche),
  temperatur = schreibe_fenster_ebenen_dateien("temperatur", temperatur_fenster_ergebnisse),
  bodentemperatur = schreibe_fenster_ebenen_dateien("bodentemperatur", bodentemperatur_fenster_ergebnisse),
  sonnenschein = schreibe_fenster_ebenen_dateien("sonnenschein", sonnenschein_fenster_ergebnisse),
  et0 = schreibe_fenster_ebenen_dateien("et0", et0_fenster_ergebnisse),
  gdd = schreibe_ebene_datei("gdd", gdd_bild_je_woche, gdd_werte_je_woche)
)
cat("Ebenen-Dateien geschrieben in:", ebenen_dir, "\n")

# Farbnamen -> Hex, fuer die JS-Farbverlauf-Legende (CSS linear-gradient):
# R/X11-Farbnamen wie "khaki1" sind kein gueltiges CSS (nur das GRUND-
# Farbwort "khaki" ist standardisiert) - ein ungueltiger Farbname macht die
# gesamte "background"-Deklaration im Browser wirkungslos (leerer Balken).
# Dasselbe Problem wie bei "gray46" im Plotly-colorscale oben, nur diesmal
# im CSS statt in Plotly.js - deshalb hier generell in Hex uebersetzt, egal
# wie die Farbe im R-Farbverlauf selbst geschrieben ist (dort unproblematisch,
# da R's eigene Grafik-Engine alle X11-Namen kennt).
farben_zu_hex <- function(farben) {
  rgb_mat <- grDevices::col2rgb(farben)
  apply(rgb_mat, 2, function(x) sprintf("#%02X%02X%02X", x[1], x[2], x[3]))
}

# Legenden-Metadaten je Hintergrund-Ebene (Farbverlauf, Wertebereich,
# Quellenangabe) - eine einzige Quelle fuer den Farbverlauf-Balken UND die
# Tooltip-Quellenangabe im "Hintergrund-Ebene"-Kasten (siehe onRender()
# weiter unten), statt Farben/Text dort ein zweites Mal von Hand nachzubauen.
layer_legenden <- list(
  # symbol: "sum"/"avg" markiert eine "gleitendes Fenster"-Ebene - JS haengt
  # dafuer den Schieberegler-Wert kompakt ans Label an (z.B. "Σ 28d"/
  # "⌀ 7d", siehe aktualisiereLayerLabels()) statt eines ausgeschriebenen
  # "(Summe/Mittel, N Tage)". fensterSkaliert = TRUE laesst den Wertebereich
  # in der Legende mit der Fenstergroesse mitwachsen (nur bei Summen
  # sinnvoll - siehe baue_fenster_ebenen()/R).
  niederschlag = list(label = "Niederschlagssumme", farben = farben_zu_hex(niederschlag_farben), bereich = c(0, 100), fensterSkaliert = TRUE, symbol = "sum", einheit = "mm", quelle = niederschlag_quelle),
  # label OHNE Datum - das tatsaechliche Datum des Snapshots (siehe
  # bodenwasser_datum_je_woche, kann je nach Verfuegbarkeit vom Wochenbeginn
  # abweichen) wird in aktualisiereLayerLabels() live ergaenzt.
  boden = list(label = "Bodenwasserbilanz", farben = farben_zu_hex(bodenwasser_farben), bereich = c(0, 100), fensterSkaliert = FALSE, symbol = NULL, einheit = "mm, von 100", quelle = bodenwasser_quelle),
  temperatur = list(label = "Temperatur 2m", farben = farben_zu_hex(temperatur_farben), bereich = c(0, 30), fensterSkaliert = FALSE, symbol = "avg", einheit = "°C", quelle = temperatur_quelle),
  bodentemperatur = list(label = "Bodentemperatur", farben = farben_zu_hex(temperatur_farben), bereich = c(0, 30), fensterSkaliert = FALSE, symbol = "avg", einheit = "°C", quelle = bodentemperatur_quelle),
  sonnenschein = list(label = "Sonnenscheindauer", farben = farben_zu_hex(sonnenschein_farben), bereich = c(0, 100), fensterSkaliert = FALSE, symbol = "avg", einheit = "%", quelle = sonnenschein_quelle),
  et0 = list(label = "Verdunstung ET0", farben = farben_zu_hex(et0_farben), bereich = c(0, 25), fensterSkaliert = TRUE, symbol = "sum", einheit = "mm", quelle = et0_quelle),
  gdd = list(label = "Wachstumsgradtage", farben = farben_zu_hex(gdd_farben), bereich = c(0, 2500), fensterSkaliert = FALSE, symbol = NULL, einheit = "°C-Tage", quelle = gdd_quelle)
)

########################################################################
## 4. Verknuepfung: Jahr-Auswahl, Standort-Sidebar, Kalenderwochen-
##    Schieberegler, Karten-Hervorhebung - als onRender() auf der Kurve.
########################################################################

js_template <- "
function(el, x) {
  var alleJahre = __ALLE_JAHRE__;
  var neuestesJahr = __NEUESTES_JAHR__;
  var jahreMitNiederschlag = __JAHRE_MIT_NIEDERSCHLAG__;
  var nSiteGrowth = __N_SITE_GROWTH__, nGroupGrowth = __N_GROUP_GROWTH__, nSitePrecip = __N_SITE_PRECIP__, nGroupPrecip = __N_GROUP_PRECIP__;
  var siteGrowthMeta = __SITE_GROWTH_META__;
  var groupGrowthMeta = __GROUP_GROWTH_META__;
  var sitePrecipMeta = __SITE_PRECIP_META__;
  var groupPrecipMeta = __GROUP_PRECIP_META__;
  var groupLabels = __GROUP_LABELS__;
  var siteNames = __SITE_NAMES__;
  var siteVisible = __SITE_VISIBLE__;
  var wochenTickvals = __WOCHEN_TICKVALS__;
  var wochenTicktext = wochenTickvals.map(String);
  var datumTicktextJeJahr = __DATUM_TICKTEXT_JE_JAHR__;
  var wochenDatumBereich = __WOCHEN_DATUM_BEREICH__;
  var siteColors = __SITE_COLORS__;
  var mapWochen = __MAP_WOCHEN__;
  var mapPointOrts = __MAP_POINT_ORTS__;
  var layerLegenden = __LAYER_LEGENDEN__;
  // Ebenen-Verfuegbarkeit (welche Jahr/Woche-Schluessel existieren je Ebene)
  // - klein genug fuer die Haupt-HTML; die eigentlichen Bilder/Werte-Gitter
  // liegen in outputs/ebenen/<name>.json und werden per ladeEbene() erst
  // beim ersten Auswaehlen der jeweiligen Ebene nachgeladen (siehe unten) -
  // ebenenCache haelt sie danach im Speicher (kein wiederholtes Nachladen).
  var ebenenSchluessel = __EBENEN_SCHLUESSEL__;
  var ebenenCache = {};
  // Ebenen mit waehlbarer Fenstergroesse (Schieberegler oberhalb der
  // Legende, siehe macheMeteoFensterSchieberegler() weiter unten) - deren
  // Datei-/Cache-Schluessel ist <name>_<meteoFenster> statt nur <name>
  // (jede Fenstergroesse ist eine eigene JSON-Datei, siehe R:
  // schreibe_fenster_ebenen_dateien()). meteoFensterStandard legt fest, auf
  // welchen Wert der Schieberegler bei Auswahl der jeweiligen Ebene
  // automatisch zurueckspringt (siehe makeLayerRadio()-Aufrufe unten).
  var meteoFensterEbenen = ['niederschlag', 'temperatur', 'bodentemperatur', 'sonnenschein', 'et0'];
  var meteoFensterStandard = { niederschlag: 28, temperatur: 7, bodentemperatur: 7, sonnenschein: 7, et0: 7 };
  // Diskrete Schieberegler-Stufen (Tage) - siehe R: fenstergroessen_tage.
  var meteoFensterStufen = [7, 14, 21, 28];
  var meteoFenster = 7;
  function istFensterEbene(name) { return meteoFensterEbenen.indexOf(name) !== -1; }
  function ebeneDateiSchluessel(name) { return istFensterEbene(name) ? name + '_' + meteoFenster : name; }
  function ebeneHatJahr(name, jahr) {
    var schluessel = ebenenSchluessel[name];
    if (!schluessel) return false;
    for (var i = 0; i < schluessel.length; i++) {
      if (schluessel[i].indexOf(jahr + ' ') === 0) return true;
    }
    return false;
  }
  // Wie ebeneHatJahr(), nur fuer die GENAUE (Jahr, Woche)-Kombination -
  // manche Ebenen (v.a. Sonnenschein, mit 1-2 Monaten Aufbereitungs-
  // verzoegerung) haben zwar Daten fuer das Jahr, aber nicht (mehr) fuer
  // die allerneuesten Wochen darin. ebeneHatJahr() allein wuerde das nicht
  // erkennen (Radio bliebe aktiv), die Karte zeigte dann fuer diese Woche
  // einfach nichts, ohne erkennbaren Unterschied zu laedt noch - siehe
  // aktualisiereLayerLegende().
  function ebeneHatWoche(name, jahr, woche) {
    var schluessel = ebenenSchluessel[name];
    return !!schluessel && schluessel.indexOf(jahr + ' ' + woche) !== -1;
  }
  // Laedt die JSON-Datei einer Ebene genau EINMAL je Datei-Schluessel
  // (Cache-Treffer bei jedem weiteren Aufruf mit demselben Schluessel) und
  // ruft dann callback(daten) auf; daten hat die Form { bilder: {...},
  // werte: {...}, datum: {...} (nur bei boden) }. dateiSchluessel ist bei
  // Fenster-Ebenen NAME_FENSTER (siehe ebeneDateiSchluessel()), sonst nur
  // NAME. Bricht eine noch laufende Anfrage NICHT ab, wenn zwischenzeitlich
  // eine andere Ebene/Fenstergroesse gewaehlt wurde - die Aufrufer
  // (aktualisiereHintergrundEbene() etc.) pruefen deshalb nach Abschluss
  // jeweils selbst, ob ihr Schluessel noch aktuell ist, bevor sie das
  // Ergebnis anwenden.
  function ladeEbene(dateiSchluessel, callback) {
    if (ebenenCache[dateiSchluessel]) { callback(ebenenCache[dateiSchluessel]); return; }
    fetch('ebenen/' + dateiSchluessel + '.json')
      .then(function(r) { return r.json(); })
      .then(function(daten) { ebenenCache[dateiSchluessel] = daten; callback(daten); })
      .catch(function(err) { console.error('Ebene ' + dateiSchluessel + ' konnte nicht geladen werden:', err); });
  }
  var graswachstumBilder = __GRASWACHSTUM_BILDER__;
  var afcRingBilder = __AFC_RING_BILDER__;
  // Index (in afc_optimum_windows/afcVerlaeufe) des jahreszeitlichen AFC-
  // Zielkorridors je (Jahr, Woche) - fuer die kompakte AFC-Legende im
  // Ebenen-Kasten (siehe aktualisiereAfcLegende()), unabhaengig davon, ob
  // diese Woche ueberhaupt ein Standort mit AFC-Wert hat.
  var afcFensterJeWoche = __AFC_FENSTER_JE_WOCHE__;
  var afcVerlaeufe = __AFC_VERLAEUFE__;
  var kartenbildHintergrund = __KARTENBILD_HINTERGRUND__;
  var heutigeWoche = __HEUTIGE_WOCHE__;
  var standardKurveTraceIdx = __STANDARD_KURVE_TRACE_IDX__;

  var selection = { type: 'group', idx: 0 };
  // Vorherige Auswahl, gesetzt beim Wechsel per Klick auf einen Legenden-
  // oder Karteneintrag (siehe waehleSiteViaKlick()) - ein erneuter Klick
  // auf denselben (bereits aktiven) Eintrag stellt sie wieder her. Die
  // Combobox selbst setzt sie bewusst NICHT (dort nur Vorwaerts-Auswahl).
  var vorherigeSelection = null;
  var precipOn = true;
  var datumOn = false;
  var vorjahrOn = false;
  // Trace-Indizes, deren Linien-/Marker-Farbe aktuell fuer die Vorjahres-
  // Ueberlagerung auf Grau umgestellt ist, mit der jeweiligen Original-
  // farbe - noetig, um sie bei Auswahl-/Jahreswechsel oder Ausschalten des
  // Schalters wieder korrekt einzufaerben (siehe aktualisiereVorjahrOverlay()).
  var vorjahrStyledTraceIdx = [];
  // Wie vorjahrStyledTraceIdx, aber fuer die Niederschlags-Vorjahresbalken
  // (siehe aktualisiereVorjahrOverlay()) - separat gefuehrt, da Balken andere
  // Style-Attribute (Farbe/Breite/Fehlerbalken statt Linien-/Markerfarbe)
  // brauchen als die Wachstumskurven.
  var vorjahrPrecipStyledTraceIdx = [];
  var growthMapKlickGebunden = false;
  var growthMapHoverGebunden = false;
  var growthMapZeigerGebunden = false;
  // Schalter Messnetz-Standorte (Ebenen-Kasten): blendet die (unsichtbaren,
  // nur fuer Hover benoetigten) Marker der Wachstumskarte komplett aus -
  // z.B. um eine Hintergrund-Ebene (Niederschlag/Bodenwasserbilanz)
  // ungestoert zu betrachten. Default an (Messnetz-Standorte sind der
  // Hauptzweck der Karte). Graswachstum-Kreis und AFC-Ring (die BILD-
  // Ebenen) haben je einen EIGENEN Schalter, siehe graswachstumOn/afcOn.
  var messnetzOn = true;
  var graswachstumOn = true;
  var afcOn = true;
  // Schalter MeteoSchweiz-Stationen (Ebenen-Kasten, neben Messnetz-
  // Standorte): Default AUS - reine Referenz-Ebene, nicht Teil der
  // eigentlichen AGFF-Auswertung. smnStationenTraceIdx zeigt auf die EINE,
  // von Jahr/Woche unabhaengige Trace (siehe R: smn_stationen_trace_idx).
  var smnStationenOn = false;
  var smnStationenTraceIdx = __SMN_STATIONEN_TRACE_IDX__;
  // Stations-Metadaten (Name/Lage/Kanton/Hoehe, aus R gebacken - aendert
  // sich praktisch nie) fuer den client-seitigen Live-Fetch der taeglich
  // aktuellen Messwerte, siehe ladeSmnAktuellwerte() unten.
  var smnStationenMeta = __SMN_STATIONEN_META__;
  var smnBasisUrl = __SMN_BASIS_URL__;
  var smnWerteGeladen = false, smnLaedt = false;
  // Trace-Index des per PLZ/Ort-Suche gesetzten Fadenkreuz-Markers auf der
  // Wachstumskarte (siehe platziereFadenkreuz()) - null, solange noch nie
  // gesucht wurde; die Trace wird beim ersten Treffer einmalig per
  // Plotly.addTraces() angelegt und danach nur noch verschoben.
  var fadenkreuzTraceIdx = null;
  var hintergrundEbene = 'keine';
  var selectedYear = neuestesJahr;
  var selectedWeek = __START_WOCHE__;

  // Zukuenftige Wochen (nach der aktuellen Kalenderwoche, nur im neuesten
  // Jahr relevant) sind noch nicht gemessen - weder per Regler/Pfeilen
  // noch per Klick auf die x-Achse darf darauf verschoben werden.
  function maxWocheFuerJahr(jahr) {
    return jahr === neuestesJahr ? heutigeWoche : 52;
  }

  function applyState() {
    // vis wird ueber den in R mitgelieferten traceIdx (echte Plotly-Trace-
    // Position) befuellt, NICHT durch positionsweises Anhaengen (push) je
    // Meta-Liste - die Traces wurden auf R-Seite pro Jahr VERSCHACHTELT
    // angelegt (Standort-Wachstum, Gruppen-Wachstum, ggf. Niederschlag,
    // pro Jahr wiederholt), ein einfaches Aneinanderhaengen aller Eintraege
    // JE TYP ueber alle Jahre haette hier (ausser bei zufaellig gleicher
    // Standort-/Gruppenanzahl pro Jahr) zu falsch zugeordneten visible-
    // Flags gefuehrt (sichtbar wurde ein VOELLIG ANDERER Standort als der
    // gewaehlte).
    var vis = new Array(standardKurveTraceIdx + 1).fill(false);
    siteGrowthMeta.forEach(function(m) {
      var match = selection.type === 'group' ? siteVisible[selection.idx][m.siteIdx] : (selection.type === 'site' && m.siteIdx === selection.idx);
      vis[m.traceIdx] = m.year === selectedYear && match;
    });
    groupGrowthMeta.forEach(function(m) {
      vis[m.traceIdx] = m.year === selectedYear && selection.type === 'group' && m.groupIdx === selection.idx;
    });
    sitePrecipMeta.forEach(function(m) {
      var match = selection.type === 'site' && m.siteIdx === selection.idx;
      vis[m.traceIdx] = precipOn && m.year === selectedYear && match;
    });
    groupPrecipMeta.forEach(function(m) {
      vis[m.traceIdx] = precipOn && m.year === selectedYear && selection.type === 'group' && m.groupIdx === selection.idx;
    });
    vis[standardKurveTraceIdx] = true;
    Plotly.restyle(el, { visible: vis });
    aktualisiereVorjahrOverlay();
    renderLegendItems();
    applyMapState();
  }

  // Schalter Vorjahresdaten: blendet zusaetzlich zur aktuellen Auswahl
  // die Kurve(n) des VORJAHRS ein, in Grau statt der Standort-/Gruppen-
  // eigenen Farbe - reine Vergleichsreferenz, aendert selection/selectedYear
  // nicht. Faerbt zuerst alle zuvor grau gestellten Traces (aus einer
  // fruaheren Auswahl) wieder auf ihre Originalfarbe zurueck, damit keine
  // Trace faelschlich grau bleibt, wenn sich Auswahl, Jahr oder der
  // Schalter selbst aendert.
  function aktualisiereVorjahrOverlay() {
    if (vorjahrStyledTraceIdx.length > 0) {
      var origFarben = vorjahrStyledTraceIdx.map(function(e) { return e.color; });
      Plotly.restyle(el,
        { 'line.color': origFarben, 'marker.color': origFarben },
        vorjahrStyledTraceIdx.map(function(e) { return e.traceIdx; })
      );
      vorjahrStyledTraceIdx = [];
    }
    // Nur Farbe/Breite/Fehlerbalken zuruecksetzen, NICHT 'visible' - die
    // eigentliche Sichtbarkeit je Trace wird bereits durch das volle vis[]-
    // Array in applyState() (vor dem Aufruf dieser Funktion) korrekt gesetzt;
    // ein zusaetzliches 'visible:false' hier wuerde einen frisch auf true
    // gesetzten Balken sofort wieder ausblenden, falls das Vorjahr der
    // vorherigen Auswahl zufaellig das NEU gewaehlte Jahr ist.
    if (vorjahrPrecipStyledTraceIdx.length > 0) {
      Plotly.restyle(el,
        { 'marker.color': 'steelblue', width: 0.7, 'error_y.visible': true },
        vorjahrPrecipStyledTraceIdx
      );
      vorjahrPrecipStyledTraceIdx = [];
    }
    if (!vorjahrOn) return;

    var vorjahr = String(parseInt(selectedYear, 10) - 1);
    if (alleJahre.indexOf(vorjahr) === -1) return;

    var grau = 'rgba(140,140,140,0.7)';
    var traceIdxListe = [];
    siteGrowthMeta.forEach(function(m) {
      if (m.year !== vorjahr) return;
      var match = selection.type === 'group' ? siteVisible[selection.idx][m.siteIdx] : (selection.type === 'site' && m.siteIdx === selection.idx);
      if (!match) return;
      traceIdxListe.push(m.traceIdx);
      vorjahrStyledTraceIdx.push({ traceIdx: m.traceIdx, color: siteColors[m.siteIdx] });
    });
    groupGrowthMeta.forEach(function(m) {
      if (m.year !== vorjahr || selection.type !== 'group' || m.groupIdx !== selection.idx) return;
      traceIdxListe.push(m.traceIdx);
      vorjahrStyledTraceIdx.push({ traceIdx: m.traceIdx, color: 'black' });
    });
    if (traceIdxListe.length > 0) Plotly.restyle(el, { visible: true, 'line.color': grau, 'marker.color': grau }, traceIdxListe);

    // Niederschlag-Vorjahresbalken: schmalere, graue Saeule fuer das Vorjahr,
    // sichtbar HINTER dem (bereits halbtransparenten, breiteren) Balken des
    // aktuellen Jahres - beide Traces liegen dank barmode=overlay und
    // aufsteigend sortierter Jahresreihenfolge bereits in der richtigen
    // Zeichenreihenfolge (Vorjahr zuerst angelegt = unten, aktuelles Jahr
    // danach = oben), es muss also nur noch Sichtbarkeit/Stil umgeschaltet
    // werden. Fehlerbalken (nur bei Gruppen-Niederschlag) werden fuer die
    // Vorjahres-Saeule ausgeblendet, um die Darstellung nicht zu ueberladen.
    if (precipOn) {
      var precipTraceIdxListe = [];
      sitePrecipMeta.forEach(function(m) {
        if (m.year !== vorjahr || !el.data[m.traceIdx] || el.data[m.traceIdx].type !== 'bar') return;
        if (!(selection.type === 'site' && m.siteIdx === selection.idx)) return;
        precipTraceIdxListe.push(m.traceIdx);
      });
      groupPrecipMeta.forEach(function(m) {
        if (m.year !== vorjahr || !el.data[m.traceIdx] || el.data[m.traceIdx].type !== 'bar') return;
        if (selection.type !== 'group' || m.groupIdx !== selection.idx) return;
        precipTraceIdxListe.push(m.traceIdx);
      });
      if (precipTraceIdxListe.length > 0) {
        Plotly.restyle(el,
          { visible: true, 'marker.color': 'rgba(90,90,90,0.9)', width: 0.35, 'error_y.visible': false },
          precipTraceIdxListe
        );
        vorjahrPrecipStyledTraceIdx = precipTraceIdxListe;
      }
    }
  }

  function applyXAxis() {
    var ticktext = datumOn ? datumTicktextJeJahr[selectedYear] : wochenTicktext;
    Plotly.relayout(el, {
      'xaxis.tickmode': 'array',
      'xaxis.tickvals': wochenTickvals,
      'xaxis.ticktext': ticktext,
      'xaxis.title.text': datumOn ? 'Datum (Montag der Woche)' : 'Kalenderwoche'
    });
  }

  function mapTraceIndexFor(jahr, week) {
    for (var i = 0; i < mapWochen.length; i++) {
      if (mapWochen[i].jahr === jahr && mapWochen[i].week === week) return i;
    }
    return -1;
  }

  function highlightArrayFor(idx) {
    var orts = mapPointOrts[idx];
    return orts.map(function(o) {
      var oi = siteNames.indexOf(o);
      var inSel = selection.type === 'group' ? (oi >= 0 && siteVisible[selection.idx][oi]) : (selection.type === 'site' && siteNames[selection.idx] === o);
      return inSel ? 4 : 1;
    });
  }

  function applyMapState() {
    // Frisch abfragen statt einmalig cachen: beim allerersten Aufruf (aus
    // applyState() am Ende von onRender()) existiert das Karten-Widget
    // evtl. noch nicht im DOM, da beide Widgets (Kurve, Wachstumskarte)
    // unabhaengig voneinander gebunden werden und die Reihenfolge nicht
    // garantiert ist - eine einmalig gecachte (dann dauerhaft null
    // bleibende) Referenz wuerde die Karte nie mehr aktualisieren.
    var growthMapGd = document.querySelector('#datenexplorer-growthmap .js-plotly-plot');
    var idx = mapTraceIndexFor(selectedYear, selectedWeek);
    // messnetzOn=false blendet auch die (unsichtbare) Hover-Marker-Trace
    // aus - sonst waeren die Standorte trotz ausgeblendetem Bild weiterhin
    // geisterhaft hoverbar.
    var vis = mapWochen.map(function(m, i) { return messnetzOn && i === idx; });
    if (growthMapGd) Plotly.restyle(growthMapGd, { visible: vis });
    if (idx >= 0 && growthMapGd) {
      var hl = highlightArrayFor(idx);
      Plotly.restyle(growthMapGd, { 'marker.line.width': [hl] }, [idx]);
    }
    // Standort-Filter auch per Klick auf einen Kartenpunkt (nur einmal
    // binden - growthMapGd wird bei jedem Aufruf neu abgefragt, s.o.).
    // mapPointOrts[curveNumber] ist die je Snapshot-Trace passende Liste
    // der Ort-Namen in Punktreihenfolge (siehe R: map_point_orts[[i]]),
    // curveNumber entspricht direkt dem Snapshot-Trace-Index, da fig_wachstum
    // pro (Jahr, Woche) genau EINE Trace in dieser Reihenfolge enthaelt.
    if (growthMapGd && !growthMapKlickGebunden) {
      growthMapGd.on('plotly_click', function(data) {
        if (!data.points || data.points.length === 0) return;
        var p = data.points[0];
        var orte = mapPointOrts[p.curveNumber];
        if (!orte) return;
        var ort = orte[p.pointNumber];
        var siteIdx = siteNames.indexOf(ort);
        if (siteIdx === -1) return;
        waehleSiteViaKlick(siteIdx);
      });
      growthMapKlickGebunden = true;
    }
    // Eigenes Tooltip-Modal statt Plotlys nativer (moeglicherweise
    // abgeschnittener) Hover-Box - siehe tooltipModalEl weiter oben.
    // Reagiert auf dieselben Punkte wie Plotlys eigenes Hover (Graswachstum-
    // Kreise, MeteoSchweiz-Stationen, Such-Fadenkreuz), zeigt aber deren
    // hovertext in einem fixen, garantiert vollstaendig sichtbaren Modal.
    if (growthMapGd && !growthMapHoverGebunden) {
      growthMapGd.on('plotly_hover', function(data) {
        if (!data.points || data.points.length === 0) return;
        var text = data.points[0].text || data.points[0].hovertext;
        if (!text) return;
        tooltipModalEl.innerHTML = text;
        tooltipModalEl.style.display = 'block';
      });
      growthMapGd.on('plotly_unhover', function() { tooltipModalEl.style.display = 'none'; });
      growthMapHoverGebunden = true;
    }
    // Cursor-Wertabfrage fuer die Hintergrund-Ebenen (siehe
    // verarbeiteKartenZeiger() weiter oben) - mousemove fuer Desktop,
    // touchmove/touchstart fuers Tippen auf Touch-Geraeten, mouseleave
    // setzt das Anzeigefeld zurueck.
    if (growthMapGd && !growthMapZeigerGebunden) {
      growthMapGd.addEventListener('mousemove', verarbeiteKartenZeiger);
      growthMapGd.addEventListener('touchstart', verarbeiteKartenZeiger, { passive: true });
      growthMapGd.addEventListener('touchmove', verarbeiteKartenZeiger, { passive: true });
      growthMapGd.addEventListener('mouseleave', versteckeWertAnzeige);
      growthMapZeigerGebunden = true;
    }
    if (weekLabel) {
      weekLabel.innerHTML = '';
      var kwZeile = document.createElement('div');
      kwZeile.textContent = 'KW ' + selectedWeek + ' ' + selectedYear;
      var datumZeile = document.createElement('div');
      datumZeile.className = 'gw-slider-datum';
      datumZeile.textContent = wochenDatumBereich[selectedYear + ' ' + selectedWeek] || '';
      weekLabel.appendChild(kwZeile);
      weekLabel.appendChild(datumZeile);
    }
    Plotly.relayout(el, { 'shapes[0].x0': selectedWeek, 'shapes[0].x1': selectedWeek });
    aktualisiereHintergrundEbene();
    aktualisiereLayerLabels();
    aktualisiereLayerLegende();
    aktualisiereAfcLegende();
    aktualisiereSmnStationen();
  }

  // Haengt an die Labels der Fenster-Ebenen (siehe meteoFensterEbenen) das
  // Symbol (Σ Summe / ⌀ Mittel) und die aktuelle Fenstergroesse an (z.B.
  // Niederschlagssumme Sigma 28d) und ergaenzt das tatsaechliche Datum des
  // Bodenwasserbilanz-Snapshots im Radio-Label (kann je nach Verfuegbarkeit
  // vom Wochenbeginn abweichen, siehe Kommentar bei bodenwasser_datum_je_
  // woche/R) - ausserhalb von aktualisiereHintergrundEbene() aufgerufen,
  // damit die Labels auch dann aktuell bleiben, wenn die Wachstumskarte
  // selbst noch nicht gebunden ist.
  function aktualisiereLayerLabels() {
    meteoFensterEbenen.forEach(function(name) {
      var radio = radioJeEbene[name];
      if (!radio || !radio.labelTextEl) return;
      var info = layerLegenden[name];
      var symbol = info.symbol === 'sum' ? 'Σ' : '⌀';
      // Nur die AKTUELL gewaehlte Ebene zeigt den live am Schieberegler
      // eingestellten Wert - alle anderen (nicht ausgewaehlten) Fenster-
      // Ebenen zeigen weiterhin ihren eigenen Standard (meteoFensterStandard),
      // da genau DAS beim Auswaehlen tatsaechlich passieren wuerde (der
      // Schieberegler springt ja bei jedem Ebenenwechsel auf den Standard
      // der neuen Ebene zurueck) - sonst waere hier voruebergehend ein
      // Wert zu sehen, der beim Klick gar nicht eintritt.
      var tage = (name === hintergrundEbene) ? meteoFenster : meteoFensterStandard[name];
      radio.labelTextEl.textContent = info.label + ' ' + symbol + ' ' + tage + 'd';
    });
    if (!radioBoden || !radioBoden.labelTextEl) return;
    // bodenCache.datum existiert erst, NACHDEM die Ebene einmal geladen
    // wurde (siehe ladeEbene()) - bis dahin steht im Label schlicht kein
    // Datum, statt die Ebene allein fuer dieses Label vorzuladen.
    var bodenCache = ebenenCache.boden;
    var datum = bodenCache && bodenCache.datum && bodenCache.datum[selectedYear + ' ' + selectedWeek];
    radioBoden.labelTextEl.textContent = layerLegenden.boden.label + ' (berechnet)' + (datum ? ' ' + datum : '');
  }

  // Optionale Hintergrund-Ebenen (Niederschlag Vorwoche / Bodenwasserbilanz
  // Wochenbeginn) - vorgerenderte PNGs je (Jahr, Woche), siehe R-Code
  // (raster_zu_datauri()). Nur eine Ebene gleichzeitig aktiv, standardmaessig
  // keine. Das Kantone/Seen-Hintergrundbild (kartenbildHintergrund) ist
  // IMMER die unterste Bild-Ebene; eine gewaehlte Niederschlags-/
  // Bodenwasserbilanz-Ebene wird als zweites, halbtransparentes Bild
  // darueber gelegt (Plotly zeichnet layout.images in Array-Reihenfolge).
  // Zeichnet die Bild-Ebenen der Karte (Kantone/Seen-Basis, optionale
  // Hintergrund-Ebene, AFC-Ring, Graswachstum-Kreis) - 'bild' ist entweder
  // das bereits geladene Bild der optionalen Ebene oder null (keine Ebene
  // gewaehlt, oder deren Daten werden gerade erst nachgeladen).
  function zeichneKartenBilder(bild) {
    var growthMapGd = document.querySelector('#datenexplorer-growthmap .js-plotly-plot');
    if (!growthMapGd) return;
    var schluessel = selectedYear + ' ' + selectedWeek;
    var basisBilder = bild ? [kartenbildHintergrund, bild] : [kartenbildHintergrund];
    // AFC-Ring und Graswachstum-Kreis liegen IMMER ueber der Kartenbasis/
    // optionalen Hintergrund-Ebene, unabhaengig von deren Auswahl - jede
    // Ebene einzeln per eigenem Schalter (Ebenen-Kasten) ein-/ausblendbar.
    // Reihenfolge wichtig: AFC-Ring zuerst, Graswachstum-Kreis darueber
    // (deckt sonst den Ring-Innenbereich zu, wie im urspruenglichen
    // kombinierten Bild).
    var zusatzBilder = [];
    if (afcOn && afcRingBilder[schluessel]) zusatzBilder.push(afcRingBilder[schluessel]);
    if (graswachstumOn && graswachstumBilder[schluessel]) zusatzBilder.push(graswachstumBilder[schluessel]);
    var alleBilder = basisBilder.concat(zusatzBilder);
    Plotly.relayout(growthMapGd, { images: alleBilder });
  }

  // Optionale Hintergrund-Ebenen werden erst bei Bedarf nachgeladen (siehe
  // ladeEbene() oben) - beim allerersten Auswaehlen einer noch nicht
  // zwischengespeicherten Ebene zeigt die Karte kurz KEINE Ebene (statt der
  // vorherigen, jetzt nicht mehr passenden), bis die Datei eingetroffen ist.
  // ebeneBeimStart wird nach Abschluss der Anfrage GEGENGEPRUEFT: haben
  // Nutzer inzwischen eine andere Ebene gewaehlt, wird das (jetzt veraltete)
  // Ergebnis verworfen statt faelschlich angezeigt.
  function aktualisiereHintergrundEbene() {
    if (hintergrundEbene === 'keine') { zeichneKartenBilder(null); return; }
    var ebeneBeimStart = hintergrundEbene;
    var dateiSchluesselBeimStart = ebeneDateiSchluessel(ebeneBeimStart);
    if (!ebenenCache[dateiSchluesselBeimStart]) zeichneKartenBilder(null);
    ladeEbene(dateiSchluesselBeimStart, function(daten) {
      // aktualisiereLayerLabels() unabhaengig vom Noch-aktuell-Check unten
      // aufgerufen: das Bodenwasserbilanz-Datum im Radio-Label soll auch
      // dann erscheinen, wenn zwischenzeitlich eine ANDERE Ebene gewaehlt
      // wurde - das Label selbst blendet sich ja nur ein, waehrend boden
      // ausgewaehlt ist, ist also nie faelschlich sichtbar.
      if (ebeneBeimStart === 'boden') aktualisiereLayerLabels();
      // Vergleich ueber den Datei-Schluessel (nicht nur den Ebenennamen):
      // bei Fenster-Ebenen zaehlt auch ein zwischenzeitlicher Wechsel der
      // Fenstergroesse (Schieberegler) als nicht mehr aktuell.
      if (ebeneDateiSchluessel(hintergrundEbene) !== dateiSchluesselBeimStart) return;
      // Voller Neuaufbau statt nur ladeHinweisEl auszublenden: erst jetzt
      // (Ebene fertig geladen) laesst sich beurteilen, ob die AKTUELLE
      // Woche tatsaechlich Daten hat oder nicht (siehe keinDatenHinweisEl
      // in aktualisiereLayerLegende()).
      aktualisiereLayerLegende();
      zeichneKartenBilder(daten.bilder[selectedYear + ' ' + selectedWeek]);
    });
  }

  // MeteoSchweiz-Stationen: einzige, von Jahr/Woche unabhaengige Trace
  // (smnStationenTraceIdx) - Position/Name/Kanton/Hoehe sind bereits beim
  // Seitenaufbau in R gebacken (smnStationenMeta), die taeglich aktuellen
  // Messwerte holt ladeSmnAktuellwerte() unten aber SELBST per fetch() -
  // beim ERSTEN Einschalten je Seitenaufruf (danach zwischengespeichert,
  // ein erneutes Ein-/Ausschalten loest keinen neuen Download aus).
  function aktualisiereSmnStationen() {
    var growthMapGd = document.querySelector('#datenexplorer-growthmap .js-plotly-plot');
    if (!growthMapGd) return;
    Plotly.restyle(growthMapGd, { visible: smnStationenOn }, [smnStationenTraceIdx]);
    if (smnStationenOn && !smnWerteGeladen && !smnLaedt) ladeSmnAktuellwerte(growthMapGd);
  }

  // Parst die letzte Datenzeile einer MeteoSchweiz-Tages-CSV (Semikolon-
  // getrennt, keine Quotes/Escapes in diesen Dateien - einfaches split()
  // genuegt) - Entsprechung zu lade_smn_aktuellwert()/R, nur eben im
  // Browser statt beim R-Lauf ausgefuehrt.
  function smnZeileParsen(csvText) {
    var zeilen = csvText.replace(/\\r/g, '').split('\\n').filter(function(z) { return z.length > 0; });
    if (zeilen.length < 2) return null;
    var header = zeilen[0].split(';');
    var letzte = zeilen[zeilen.length - 1].split(';');
    var idx = {};
    header.forEach(function(h, i) { idx[h] = i; });
    function feld(name) {
      var i = idx[name];
      if (i === undefined) return NaN;
      return parseFloat(letzte[i]);
    }
    return {
      reference_timestamp: letzte[idx.reference_timestamp] || '',
      tre200d0: feld('tre200d0'), tso005d0: feld('tso005d0'), tso010d0: feld('tso010d0'),
      tso020d0: feld('tso020d0'), rre150d0: feld('rre150d0'), gre000d0: feld('gre000d0'), sre000d0: feld('sre000d0')
    };
  }

  // Baut den Hovertext fuer eine Station - identischer Aufbau/Wortlaut wie
  // zuvor in R (siehe Git-Historie von smn_daten), nur die Formatierung
  // (fmt1/fmt0) hier eben in JS statt formatC()/ifelse().
  function smnHoverBauen(meta, werte) {
    function fmt1(x) { return isNaN(x) ? '-' : x.toFixed(1); }
    function fmt0(x) { return isNaN(x) ? 'keine Daten' : Math.round(x).toString(); }
    var tsoZeile = (isNaN(werte.tso005d0) && isNaN(werte.tso010d0) && isNaN(werte.tso020d0))
      ? 'keine Daten'
      : (fmt1(werte.tso005d0) + ' / ' + fmt1(werte.tso010d0) + ' / ' + fmt1(werte.tso020d0) + ' °C');
    return '<b>' + meta.name + '</b> (' + meta.kanton + ', ' + meta.hoehe + ' m ü. M.)' +
      '<br>Lufttemperatur (Tagesmittel): ' + (isNaN(werte.tre200d0) ? 'keine Daten' : fmt1(werte.tre200d0) + ' °C') +
      '<br>Bodentemperatur 5/10/20cm: ' + tsoZeile +
      '<br>Niederschlag (Vortag): ' + (isNaN(werte.rre150d0) ? 'keine Daten' : fmt1(werte.rre150d0) + ' mm') +
      '<br>Globalstrahlung (Tagesmittel): ' + (isNaN(werte.gre000d0) ? 'keine Daten' : fmt0(werte.gre000d0) + ' W/m²') +
      '<br>Sonnenscheindauer: ' + (isNaN(werte.sre000d0) ? 'keine Daten' : fmt0(werte.sre000d0) + ' Min') +
      '<br>Stand: ' + werte.reference_timestamp;
  }

  // Holt die aktuellen Tageswerte fuer ALLE Stationen parallel direkt vom
  // MeteoSchweiz-Open-Data-Server (CORS-freigegeben) und ersetzt den
  // Platzhalter-Hovertext per EINEM restyle() sobald alle Anfragen fertig
  // sind (einzelne fehlgeschlagene Stationen behalten ihren Platzhalter -
  // kein Abbruch der uebrigen).
  function ladeSmnAktuellwerte(growthMapGd) {
    smnLaedt = true;
    var hovertext = smnStationenMeta.map(function(s) {
      return '<b>' + s.name + '</b> (' + s.kanton + ', ' + s.hoehe + ' m ü. M.)<br>Lädt aktuelle Werte...';
    });
    var anfragen = smnStationenMeta.map(function(s, i) {
      var url = smnBasisUrl + s.abbr.toLowerCase() + '/ogd-smn_' + s.abbr.toLowerCase() + '_d_recent.csv';
      return fetch(url).then(function(r) { return r.ok ? r.text() : null; }).then(function(text) {
        var werte = text ? smnZeileParsen(text) : null;
        if (werte) hovertext[i] = smnHoverBauen(s, werte);
      }).catch(function() { /* einzelne Station fehlgeschlagen - Platzhaltertext bleibt stehen */ });
    });
    Promise.all(anfragen).then(function() {
      Plotly.restyle(growthMapGd, { hovertext: [hovertext] }, [smnStationenTraceIdx]);
      smnWerteGeladen = true;
      smnLaedt = false;
    });
  }

  // Styles --------------------------------------------------------------
  var style = document.createElement('style');
  style.textContent = [
    // Globaler box-sizing-Reset: mehrere Elemente kombinieren feste width
    // mit padding/border (z.B. .gw-combo input, .gw-info-btn) - ohne
    // border-box wuerden solche Elemente breiter als angegeben, was auf
    // schmalen (Mobile-)Viewports zu horizontalem Ueberlauf fuehren kann.
    '*, *:before, *:after { box-sizing: border-box; }',
    '.gw-title { font-family: sans-serif; font-size: 22px; font-weight: 600; margin: 4px 0 10px 0; }',
    '.gw-controls { margin-bottom: 10px; font-family: sans-serif; font-size: 14px; display: flex; flex-wrap: wrap; align-items: center; gap: 20px; }',
    '.gw-combo { position: relative; display: inline-block; max-width: 100%; }',
    '.gw-combo input { padding: 5px 8px; font-size: 14px; width: 240px; max-width: 100%; border: 1px solid #bbb; border-radius: 4px; }',
    '.gw-combo-list { position: absolute; z-index: 1000; top: 100%; left: 0; background: white; border: 1px solid #bbb; border-radius: 4px; max-height: 260px; overflow-y: auto; width: 240px; max-width: 100%; box-shadow: 0 2px 8px rgba(0,0,0,0.15); }',
    '.gw-combo-item { padding: 6px 9px; cursor: pointer; }',
    '.gw-combo-item:hover, .gw-combo-item.active { background: #eaf2fb; }',
    '.gw-combo-sep { padding: 4px 9px; font-size: 11px; color: #888; border-top: 1px solid #eee; margin-top: 2px; user-select: none; }',
    '.gw-year-select { padding: 5px 8px; font-size: 14px; border: 1px solid #bbb; border-radius: 4px; }',
    '.gw-layer-panel { font-family: sans-serif; font-size: 13px; background: #f7f7f7; border-radius: 6px; padding: 12px 14px; }',
    // Plotlys eigene Hover-Box fuer die Karte ausgeblendet (siehe
    // tooltipModalEl/plotly_hover weiter oben) - sie wird vom
    // overflow:hidden des Kartencontainers bzw. der Iframe-Groesse
    // abgeschnitten, sobald ein Punkt nahe am Rand liegt.
    '#datenexplorer-growthmap .hoverlayer { display: none !important; }',
    '.gw-tooltip-modal { position: fixed; top: 50%; left: 50%; transform: translate(-50%,-50%); z-index: 2000; background: white; border: 1px solid #999; border-radius: 8px; padding: 10px 14px; box-shadow: 0 4px 20px rgba(0,0,0,0.3); max-width: 85vw; max-height: 80vh; overflow-y: auto; font-family: sans-serif; font-size: 13px; line-height: 1.5; color: #222; pointer-events: none; }',
    '.gw-layer-heading { font-weight: 600; margin-bottom: 8px; }',
    '.gw-layer-option { display: flex; align-items: center; gap: 8px; padding: 4px 0; cursor: pointer; }',
    '.gw-layer-option-zeile { display: flex; align-items: center; gap: 4px; }',
    '.gw-info-wrap { position: relative; display: inline-flex; }',
    '.gw-info-btn { width: 16px; height: 16px; border-radius: 50%; border: 1px solid #888; background: white; color: #555; font-size: 11px; line-height: 1; cursor: pointer; padding: 0; display: flex; align-items: center; justify-content: center; font-style: italic; font-family: Georgia, serif; flex-shrink: 0; }',
    '.gw-info-btn:hover { background: #eaf2fb; border-color: #4a90d9; color: #2a6fbf; }',
    '.gw-info-popup { position: absolute; z-index: 20; top: 20px; left: 0; width: 210px; max-width: 85vw; background: white; border: 1px solid #bbb; border-radius: 6px; padding: 10px 12px; font-size: 12px; line-height: 1.4; color: #333; box-shadow: 0 2px 10px rgba(0,0,0,0.15); cursor: auto; }',
    '.gw-layer-option input:disabled + span { color: #aaa; }',
    '.gw-meteo-fenster { margin-top: 10px; }',
    '.gw-meteo-fenster-label { font-size: 11px; color: #555; margin-bottom: 3px; }',
    '.gw-meteo-fenster input[type=range] { width: 100%; margin: 0; }',
    // Kein Trennstrich (border-top) mehr davor - weder vor der AFC-Ring-
    // Legende (direkt unter dem AFC-Schalter) noch vor der Meteodaten-
    // Legende (direkt unter dem Schieberegler): beide gehoeren optisch zum
    // jeweils direkt darueberliegenden Schalter/Schieberegler.
    '.gw-layer-legende { margin-top: 10px; }',
    '.gw-layer-legende-balken { height: 12px; border-radius: 3px; border: 1px solid rgba(0,0,0,0.15); }',
    // Donut-Ring per Masken-Trick (radial-gradient schneidet die Mitte
    // transparent) statt eines SVG - conic-gradient uebernimmt die
    // Farbverlauf-Stuetzstellen 1:1 vom vorherigen linear-gradient-Balken.
    '.gw-afc-ring-wrap { position: relative; display: flex; justify-content: center; align-items: center; margin-bottom: 2px; }',
    '.gw-afc-ring { width: 56px; height: 56px; border-radius: 50%; -webkit-mask: radial-gradient(farthest-side, transparent calc(100% - 10px), #000 calc(100% - 10px)); mask: radial-gradient(farthest-side, transparent calc(100% - 10px), #000 calc(100% - 10px)); }',
    // Tick-Strich als Uhrzeiger vom Ringzentrum nach aussen (Standard-CSS-
    // Technik: transform-origin unten am Strich = Ringzentrum, rotate()
    // schwenkt den Strich dadurch sauber um das Zentrum statt exzentrisch).
    '.gw-afc-tick { position: absolute; top: 50%; left: 50%; width: 2px; height: 30px; background: #000; transform-origin: 50% 100%; margin-left: -1px; margin-top: -30px; }',
    // Deckt den Teil des Tick-Strichs ab, der durch das Loch in der Mitte
    // des Rings ragt (wie beim Ring auf der Karte selbst - dort sind die
    // Ticks als kurze Segmente NUR im farbigen Band gezeichnet, siehe
    // ring_ticks in baue_afc_ring_bild()/R). Durchmesser = Ring-Loch
    // (Ring-Radius 28px minus Bandbreite 10px = 18px Loch-Radius, siehe
    // .gw-afc-ring-Maske oben) - Panel-Hintergrundfarbe (#f7f7f7, siehe
    // .gw-layer-panel) statt der Ring-Elemente selbst, da Letztere die
    // Ticks nicht ueberdecken koennten (Maske wirkt nur auf den Ring, nicht
    // auf seine Geschwister-Elemente).
    '.gw-afc-ring-mitte { position: absolute; top: 50%; left: 50%; width: 36px; height: 36px; margin-left: -18px; margin-top: -18px; border-radius: 50%; background: #f7f7f7; }',
    '.gw-layer-legende-skala { display: flex; justify-content: space-between; font-size: 11px; color: #555; margin-top: 3px; }',
    '.gw-layer-legende-quelle { font-size: 10px; color: #888; margin-top: 4px; }',
    '.gw-layer-wert-anzeige { font-size: 12px; font-weight: 600; margin-top: 8px; padding-top: 8px; border-top: 1px solid #eee; }',
    '.gw-layer-wert-zusatz { font-weight: 400; color: #555; margin-top: 2px; padding-top: 0; border-top: none; }',
    '.gw-lade-punkte { display: inline-flex; gap: 2px; vertical-align: middle; }',
    '.gw-lade-punkte span { width: 4px; height: 4px; border-radius: 50%; background: #888; display: inline-block; animation: gwBlink 1s infinite ease-in-out; }',
    '.gw-lade-punkte span:nth-child(2) { animation-delay: 0.15s; }',
    '.gw-lade-punkte span:nth-child(3) { animation-delay: 0.3s; }',
    '@keyframes gwBlink { 0%, 80%, 100% { opacity: 0.2; } 40% { opacity: 1; } }',
    '.gw-toggle-wrap { display: flex; align-items: center; gap: 8px; }',
    '.gw-layer-messnetz-toggle { margin-bottom: 10px; padding-bottom: 10px; border-bottom: 1px solid #ddd; }',
    // Trennlinie vor dem Meteodaten-Abschnitt (AFC-Schalter/-Legende darueber,
    // keine Meteodaten & Co. darunter) - dieselbe Technik wie bei
    // .gw-layer-messnetz-toggle (border-bottom auf der letzten Zeile davor).
    '.gw-layer-vor-meteodaten { margin-bottom: 10px; padding-bottom: 10px; border-bottom: 1px solid #ddd; }',
    // Trennlinie vor Bodenwasserbilanz (SELBST BERECHNETE Groesse, siehe
    // Kommentar bei deren makeLayerRadio()-Aufruf) - hier als border-TOP auf
    // der Zeile selbst, da sie (anders als bei den anderen Trennlinien) die
    // LETZTE Ebenen-Option ist, kein nachfolgendes Element fuer border-bottom.
    '.gw-layer-vor-boden { margin-top: 10px; padding-top: 10px; border-top: 1px solid #ddd; }',
    '.gw-toggle { position: relative; display: inline-block; width: 42px; height: 22px; flex-shrink: 0; }',
    '.gw-toggle input { opacity: 0; width: 0; height: 0; }',
    '.gw-toggle-slider { position: absolute; inset: 0; background-color: #ccc; transition: .15s; border-radius: 22px; cursor: pointer; }',
    '.gw-toggle-slider:before { position: absolute; content: \"\"; height: 16px; width: 16px; left: 3px; bottom: 3px; background-color: white; transition: .15s; border-radius: 50%; }',
    '.gw-toggle input:checked + .gw-toggle-slider { background-color: #4a90d9; }',
    '.gw-toggle input:checked + .gw-toggle-slider:before { transform: translateX(20px); }',
    '.gw-toggle input:disabled + .gw-toggle-slider { opacity: 0.4; cursor: not-allowed; }',
    '.gw-chart-row { display: flex; flex-direction: row; width: 100%; }',
    '.gw-legend-panel { flex: 0 0 210px; width: 210px; overflow-y: auto; overflow-x: hidden; border-left: 1px solid #ddd; box-sizing: border-box; padding: 10px 14px; font-family: sans-serif; font-size: 13px; transition: flex-basis .15s ease, width .15s ease, padding .15s ease, border-color .15s ease; }',
    '.gw-legend-panel.collapsed { flex-basis: 0; width: 0; padding-left: 0; padding-right: 0; border-left-color: transparent; }',
    '.gw-legend-header { display: flex; align-items: center; justify-content: space-between; font-weight: 600; margin-bottom: 8px; white-space: nowrap; }',
    '.gw-legend-options { display: flex; flex-direction: column; gap: 8px; padding-bottom: 10px; margin-bottom: 10px; border-bottom: 1px solid #eee; }',
    '.gw-legend-close { background: none; border: none; cursor: pointer; font-size: 16px; color: #777; line-height: 1; padding: 2px 4px; }',
    '.gw-legend-close:hover { color: #000; }',
    '.gw-legend-item { display: flex; align-items: center; gap: 8px; padding: 3px 4px; white-space: nowrap; border-radius: 3px; }',
    '.gw-legend-item-clickable { cursor: pointer; }',
    '.gw-legend-item-clickable:hover { background: #eef4fb; }',
    '.gw-legend-swatch { display: inline-block; width: 22px; height: 0; border-top-width: 3px; border-top-style: solid; flex-shrink: 0; }',
    '.gw-legend-edge { flex: 0 0 34px; width: 34px; border-left: 1px solid #ddd; display: flex; flex-direction: column; align-items: center; padding-top: 6px; box-sizing: border-box; }',
    '.gw-edge-btn { width: 26px; height: 26px; border: 1px solid #bbb; border-radius: 4px; background: white; cursor: pointer; font-size: 15px; display: flex; align-items: center; justify-content: center; color: #333; padding: 0; }',
    '.gw-edge-btn:hover { background: #f2f2f2; }',
    '.gw-edge-btn.active { background: #eaf2fb; border-color: #4a90d9; color: #2a6fbf; }',
    '.gw-slider-row { font-family: sans-serif; font-size: 14px; display: flex; align-items: center; margin: 20px 0; padding: 12px 16px; background: #f7f7f7; border-radius: 6px; }',
    '.gw-slider-aligned { flex: 0 0 auto; box-sizing: border-box; min-width: 0; }',
    '.gw-slider-label-row { display: flex; align-items: center; gap: 10px; width: 100%; margin-bottom: 8px; }',
    '.gw-slider-track-row { display: flex; align-items: center; width: 100%; }',
    '.gw-slider-track-row input[type=range] { flex: 1 1 auto; min-width: 0; width: 100%; }',
    '.gw-slider-label { flex: 1 1 auto; font-weight: 600; text-align: center; }',
    '.gw-slider-datum { font-weight: 400; font-size: 12px; color: #666; margin-top: 2px; }',
    '.gw-slider-tooltip { position: absolute; top: -8px; transform: translate(-50%, -100%); background: #333; color: white; padding: 3px 9px; border-radius: 4px; font-size: 12px; white-space: nowrap; pointer-events: none; z-index: 10; }',
    '.gw-slider-tooltip:after { content: \"\"; position: absolute; top: 100%; left: 50%; transform: translateX(-50%); border: 5px solid transparent; border-top-color: #333; }',
    '.gw-step-btn { flex: 0 0 auto; width: 30px; height: 30px; border: 1px solid #bbb; border-radius: 4px; background: white; cursor: pointer; font-size: 12px; display: flex; align-items: center; justify-content: center; color: #333; }',
    '.gw-step-btn:hover { background: #eaf2fb; border-color: #4a90d9; }',
    '.gw-today-btn { width: auto; padding: 0 12px; font-size: 13px; font-weight: 600; margin-left: auto; }',
    '.gw-zukunft-maske { position: absolute; background: rgba(0,0,0,0.4); border-radius: 3px; pointer-events: none; z-index: 2; }',
    // Mobile: Kurve + Standort-Legende nebeneinander (Legende fix 210px)
    // liesse auf einem Telefon (~375px) fuer die Kurve selbst kaum noch
    // Platz - deshalb unterhalb 700px gestapelt statt nebeneinander.
    // .collapsed kombiniert sich mit der Basisregel (dort width:0/flex-
    // basis:0 - im Spaltenlayout die HORIZONTALE Ausdehnung) und kappt hier
    // zusaetzlich die Hoehe (max-height/padding/overflow), damit die
    // Legende beim Einklappen in beiden Layouts vollstaendig verschwindet.
    '@media (max-width: 700px) {' +
    '  .gw-chart-row { flex-direction: column; }' +
    '  .gw-legend-panel { flex: 1 1 auto; width: 100%; border-left: none; border-top: 1px solid #ddd; max-height: 260px; }' +
    '  .gw-legend-panel.collapsed { max-height: 0; overflow: hidden; padding-top: 0; padding-bottom: 0; border-top-color: transparent; }' +
    '  .gw-legend-edge { order: -1; flex-direction: row; width: 100%; justify-content: flex-end; border-left: none; border-top: 1px solid #ddd; padding: 6px 0; }' +
    '  .gw-info-btn { width: 22px; height: 22px; font-size: 13px; }' +
    '  .gw-step-btn { width: 34px; height: 34px; }' +
    '}'
  ].join(' ');
  document.head.appendChild(style);

  // Eigenes Tooltip-Modal STATT Plotlys nativer Hover-Box (siehe unten,
  // .hoverlayer wird per CSS ausgeblendet): die native Box wird von
  // umgebendem overflow:hidden bzw. der begrenzten Iframe-Groesse
  // abgeschnitten, sobald ein Punkt nahe am Kartenrand liegt - betroffener
  // Text war dadurch oft gar nicht lesbar. position:fixed + Zentrierung
  // macht das Modal unabhaengig von der Punktposition immer vollstaendig
  // sichtbar. An document.body gehaengt (nicht in die Karte selbst), damit
  // es auch das overflow:hidden des Kartencontainers sicher umgeht.
  var tooltipModalEl = document.createElement('div');
  tooltipModalEl.className = 'gw-tooltip-modal';
  tooltipModalEl.style.display = 'none';
  document.body.appendChild(tooltipModalEl);

  var options = groupLabels.map(function(label, i) { return { type: 'group', idx: i, label: label }; });
  var siteOptions = siteNames.map(function(name, i) { return { type: 'site', idx: i, label: name }; });

  var controls = document.createElement('div');
  controls.className = 'gw-controls';

  var comboWrap = document.createElement('div');
  comboWrap.className = 'gw-combo';
  var input = document.createElement('input');
  input.type = 'text';
  input.placeholder = 'Gruppe oder Standort…';
  input.value = groupLabels[0];
  var list = document.createElement('div');
  list.className = 'gw-combo-list';
  list.style.display = 'none';

  function renderList(query) {
    query = (query || '').toLowerCase();
    list.innerHTML = '';
    var matchedGroups = options.filter(function(o) { return o.label.toLowerCase().indexOf(query) !== -1; });
    var matchedSites = siteOptions.filter(function(o) { return o.label.toLowerCase().indexOf(query) !== -1; });
    matchedGroups.forEach(function(o) { list.appendChild(makeItem(o)); });
    if (matchedSites.length > 0) {
      var sep = document.createElement('div');
      sep.className = 'gw-combo-sep';
      sep.textContent = 'Einzelstandorte';
      list.appendChild(sep);
      matchedSites.forEach(function(o) { list.appendChild(makeItem(o)); });
    }
    list.style.display = (matchedGroups.length + matchedSites.length > 0) ? 'block' : 'none';
  }

  function makeItem(o) {
    var item = document.createElement('div');
    item.className = 'gw-combo-item';
    item.textContent = o.label;
    item.addEventListener('mousedown', function(e) {
      e.preventDefault();
      selection = { type: o.type, idx: o.idx };
      if (o.type === 'site') aktiviereVorjahrFuerEinzelstandort();
      input.value = o.label;
      list.style.display = 'none';
      applyState();
    });
    return item;
  }

  input.addEventListener('focus', function() { renderList(''); });
  input.addEventListener('input', function() { renderList(input.value); });
  input.addEventListener('blur', function() { setTimeout(function() { list.style.display = 'none'; }, 150); });
  comboWrap.appendChild(input);
  comboWrap.appendChild(list);

  // Standort-Filter per Klick auf einen Legenden- oder Karteneintrag (statt
  // nur ueber die Combobox oben): ein Klick auf einen Standort, der NICHT
  // bereits einzeln ausgewaehlt ist, filtert die Kurve darauf (die vorherige
  // Auswahl - Gruppe oder anderer Standort - wird gemerkt). Ein erneuter
  // Klick auf denselben, bereits ausgewaehlten Standort stellt diese
  // vorherige Auswahl wieder her (Toggle). Die Combobox selbst nutzt diese
  // Funktion bewusst NICHT (siehe makeItem() oben) - dort bleibt jede
  // Auswahl eine reine Vorwaerts-Auswahl.
  function waehleSiteViaKlick(siteIdx) {
    if (selection.type === 'site' && selection.idx === siteIdx) {
      if (!vorherigeSelection) return;
      selection = vorherigeSelection;
      vorherigeSelection = null;
    } else {
      vorherigeSelection = { type: selection.type, idx: selection.idx };
      selection = { type: 'site', idx: siteIdx };
      aktiviereVorjahrFuerEinzelstandort();
    }
    input.value = selection.type === 'group' ? groupLabels[selection.idx] : siteNames[selection.idx];
    applyState();
  }

  // Bei Auswahl eines EINZELNEN Standorts (statt einer Gruppe) ist der
  // Vergleich mit dem Vorjahr besonders aussagekraeftig (nur eine Kurve statt
  // vieler ueberlagerter) - der Schalter wird deshalb automatisch aktiviert,
  // falls er noch aus war. Manuelles Wiederausschalten durch die Nutzerin
  // bleibt danach erhalten (wird NICHT bei jedem applyState() erneut erzwungen,
  // nur genau bei diesem Auswahlwechsel).
  function aktiviereVorjahrFuerEinzelstandort() {
    if (vorjahrOn) return;
    vorjahrOn = true;
    if (vorjahrToggleWrap && vorjahrToggleWrap.checkbox) vorjahrToggleWrap.checkbox.checked = true;
  }

  var yearSelect = document.createElement('select');
  yearSelect.className = 'gw-year-select';
  alleJahre.forEach(function(jr) {
    var opt = document.createElement('option');
    opt.value = jr;
    opt.textContent = jr;
    if (jr === neuestesJahr) opt.selected = true;
    yearSelect.appendChild(opt);
  });
  yearSelect.addEventListener('change', function() {
    selectedYear = yearSelect.value;
    var hatNiederschlag = jahreMitNiederschlag.indexOf(selectedYear) !== -1;
    precipCheckbox.disabled = !hatNiederschlag;
    if (!hatNiederschlag) { precipCheckbox.checked = false; precipOn = false; }
    aktualisiereLayerVerfuegbarkeit();
    applyXAxis();
    var maxW = maxWocheFuerJahr(selectedYear);
    if (selectedWeek > maxW) {
      selectedWeek = maxW;
      if (sliderInput) sliderInput.value = String(selectedWeek);
    }
    if (typeof aktualisiereZukunftMaske === 'function') aktualisiereZukunftMaske();
    applyState();
  });

  // Radiobuttons rechts neben der Wachstumskarte fuer die optionalen
  // Hintergrund-Ebenen - befuellen einen leeren Platzhalter-Container aus
  // dem HTML (analog zum Kalenderwochen-Schieberegler weiter unten).
  var mapControlsContainer = document.getElementById('datenexplorer-map-controls');
  var radioNiederschlag = null, radioBoden = null, radioTemperatur = null, radioBodentemperatur = null;
  var radioSonnenschein = null, radioEt0 = null, radioGdd = null;
  var radioJeEbene = {};
  function aktualisiereLayerVerfuegbarkeit() {
    if (radioNiederschlag) radioNiederschlag.disabled = !ebeneHatJahr('niederschlag', selectedYear);
    if (radioBoden) radioBoden.disabled = !ebeneHatJahr('boden', selectedYear);
    if (radioTemperatur) radioTemperatur.disabled = !ebeneHatJahr('temperatur', selectedYear);
    if (radioBodentemperatur) radioBodentemperatur.disabled = !ebeneHatJahr('bodentemperatur', selectedYear);
    if (radioSonnenschein) radioSonnenschein.disabled = !ebeneHatJahr('sonnenschein', selectedYear);
    if (radioEt0) radioEt0.disabled = !ebeneHatJahr('et0', selectedYear);
    if (radioGdd) radioGdd.disabled = !ebeneHatJahr('gdd', selectedYear);
    if ((hintergrundEbene === 'niederschlag' && radioNiederschlag && radioNiederschlag.disabled) ||
        (hintergrundEbene === 'boden' && radioBoden && radioBoden.disabled) ||
        (hintergrundEbene === 'temperatur' && radioTemperatur && radioTemperatur.disabled) ||
        (hintergrundEbene === 'bodentemperatur' && radioBodentemperatur && radioBodentemperatur.disabled) ||
        (hintergrundEbene === 'sonnenschein' && radioSonnenschein && radioSonnenschein.disabled) ||
        (hintergrundEbene === 'et0' && radioEt0 && radioEt0.disabled) ||
        (hintergrundEbene === 'gdd' && radioGdd && radioGdd.disabled)) {
      hintergrundEbene = 'keine';
      var radioKeineEl = mapControlsContainer && mapControlsContainer.querySelector('input[value=keine]');
      if (radioKeineEl) radioKeineEl.checked = true;
      aktualisiereLayerLegende();
    }
  }
  // Farbverlauf-Balken + Quellenangabe der aktuell gewaehlten Hintergrund-
  // Ebene, direkt aus layerLegenden (R: layer_legenden) - EINE Quelle fuer
  // Farben/Wertebereich/Quelle statt sie hier ein zweites Mal nachzubauen.
  // Bei Auswahl Keine bleibt der Bereich leer (kein Farbverlauf ohne aktive Ebene).
  var layerLegendeBox = null;
  var wertAnzeigeEl = null;
  var koordinatenEl = null;
  var ortschaftEl = null;
  var ladeHinweisEl = null;
  var afcLegendeBox = null;
  // Kompakte AFC-Legende im Ebenen-Kasten (zusaetzlich zur grossen Ring-
  // Legende auf der Karte selbst) - nur sichtbar, wenn der AFC-Schalter an
  // ist (Default), und mit dem jahreszeitlichen Zielbereich der GERADE
  // gewaehlten Kalenderwoche (kann je nach Woche wechseln, siehe
  // afc_optimum_windows/R).
  function aktualisiereAfcLegende() {
    if (!afcLegendeBox) return;
    if (!afcOn) { afcLegendeBox.style.display = 'none'; return; }
    var fensterIdx = afcFensterJeWoche[selectedYear + ' ' + selectedWeek];
    var verlauf = fensterIdx ? afcVerlaeufe[fensterIdx - 1] : null;
    if (!verlauf) { afcLegendeBox.style.display = 'none'; return; }
    afcLegendeBox.style.display = 'block';
    afcLegendeBox.innerHTML = '';
    // Ring statt Balken - passend zum grossen AFC-Ring auf der Karte selbst
    // (baue_afc_ring_bild()/R): conic-gradient beginnt wie dort bei 12 Uhr
    // und laeuft im Uhrzeigersinn von 0 bis 1500 kg, dieselben Farb-
    // Stuetzstellen (verlauf.farben) wie zuvor beim linear-gradient-Balken.
    var ringWrap = document.createElement('div');
    ringWrap.className = 'gw-afc-ring-wrap';
    var ring = document.createElement('div');
    ring.className = 'gw-afc-ring';
    ring.style.background = 'conic-gradient(' + verlauf.farben.join(',') + ')';
    ringWrap.appendChild(ring);
    // Tick-Striche am Zielbereich (low/high) - conic-gradient beginnt bei
    // 0deg (12 Uhr) und laeuft im Uhrzeigersinn, CSS rotate() ebenso, daher
    // genuegt eine einfache Prozent-zu-Grad-Umrechnung ohne Trigonometrie.
    [verlauf.low, verlauf.high].forEach(function(wert) {
      var tick = document.createElement('div');
      tick.className = 'gw-afc-tick';
      tick.style.transform = 'rotate(' + (wert / 1500 * 360) + 'deg)';
      ringWrap.appendChild(tick);
    });
    // Nach den Ticks angehaengt, damit sie darueber liegt: deckt den Teil
    // der Tick-Striche im Ring-Loch ab, sichtbar bleibt nur das kurze Stueck
    // im farbigen Band (wie bei den Ticks auf der Karte selbst).
    var ringMitte = document.createElement('div');
    ringMitte.className = 'gw-afc-ring-mitte';
    ringWrap.appendChild(ringMitte);
    var skala = document.createElement('div');
    skala.className = 'gw-layer-legende-skala';
    var minEl = document.createElement('span'); minEl.textContent = '0 kg';
    var maxEl = document.createElement('span'); maxEl.textContent = '1500 kg';
    skala.appendChild(minEl);
    skala.appendChild(maxEl);
    var ziel = document.createElement('div');
    ziel.className = 'gw-layer-legende-quelle';
    ziel.textContent = 'Zielbereich (aktuelle Woche): ' + verlauf.low + '–' + verlauf.high + ' kg TS/ha';
    afcLegendeBox.appendChild(ringWrap);
    afcLegendeBox.appendChild(skala);
    afcLegendeBox.appendChild(ziel);
  }
  function aktualisiereLayerLegende() {
    aktualisiereMeteoFensterSichtbarkeit();
    if (!layerLegendeBox) return;
    var info = layerLegenden[hintergrundEbene];
    if (!info) { layerLegendeBox.style.display = 'none'; wertAnzeigeEl = null; koordinatenEl = null; ortschaftEl = null; return; }
    layerLegendeBox.style.display = 'block';
    layerLegendeBox.innerHTML = '';
    // Wertebereich bei Summen-Ebenen (info.fensterSkaliert) proportional zur
    // aktuellen Fenstergroesse hochskaliert (siehe R: baue_fenster_ebenen())
    // - bei Mittelwert-Ebenen bleibt der Bereich unveraendert.
    var bereich = info.fensterSkaliert ? [info.bereich[0], Math.round(info.bereich[1] * meteoFenster / 7)] : info.bereich;
    var balken = document.createElement('div');
    balken.className = 'gw-layer-legende-balken';
    balken.style.background = 'linear-gradient(to right, ' + info.farben.join(',') + ')';
    var skala = document.createElement('div');
    skala.className = 'gw-layer-legende-skala';
    var minEl = document.createElement('span'); minEl.textContent = bereich[0] + ' ' + info.einheit;
    var maxEl = document.createElement('span'); maxEl.textContent = bereich[1] + ' ' + info.einheit;
    skala.appendChild(minEl);
    skala.appendChild(maxEl);
    var quelle = document.createElement('div');
    quelle.className = 'gw-layer-legende-quelle';
    quelle.textContent = 'Quelle: ' + info.quelle;
    wertAnzeigeEl = document.createElement('div');
    wertAnzeigeEl.className = 'gw-layer-wert-anzeige';
    wertAnzeigeEl.textContent = 'Wert am Cursor: –';
    koordinatenEl = document.createElement('div');
    koordinatenEl.className = 'gw-layer-wert-anzeige gw-layer-wert-zusatz';
    koordinatenEl.textContent = 'Koordinaten: –';
    ortschaftEl = document.createElement('div');
    ortschaftEl.className = 'gw-layer-wert-anzeige gw-layer-wert-zusatz';
    ortschaftEl.textContent = 'Ort: –';
    // Ladehinweis (blinkende Punkte, wie beim Ort/PLZ-Nachschlagen) - nur
    // sichtbar, waehrend diese Ebene NOCH NICHT im ebenenCache liegt (siehe
    // ladeEbene()/aktualisiereHintergrundEbene() oben): Bild und Werte-
    // Gitter treffen typischerweise erst nach einem kurzen fetch() ein,
    // ohne diesen Hinweis waere die Karte in der Zwischenzeit einfach leer
    // und nicht von keine Daten fuer diese Woche zu unterscheiden.
    ladeHinweisEl = document.createElement('div');
    ladeHinweisEl.className = 'gw-layer-wert-anzeige';
    ladeHinweisEl.innerHTML = 'Ebene wird geladen ' + LADE_PUNKTE_HTML;
    ladeHinweisEl.style.display = ebenenCache[ebeneDateiSchluessel(hintergrundEbene)] ? 'none' : 'block';
    // Von laedt noch (ladeHinweisEl) UNTERSCHEIDEN: die Ebene ist bereits
    // geladen, hat aber fuer die AKTUELL gewaehlte Woche keine Daten (z.B.
    // Sonnenschein nahe am aktuellen Datum, siehe ebeneHatWoche()) - sonst
    // zeigt die Karte einfach kommentarlos nichts.
    var keinDatenHinweisEl = document.createElement('div');
    keinDatenHinweisEl.className = 'gw-layer-wert-anzeige';
    keinDatenHinweisEl.textContent = 'Keine Daten für diese Woche';
    keinDatenHinweisEl.style.display = (ladeHinweisEl.style.display === 'none' && !ebeneHatWoche(hintergrundEbene, selectedYear, selectedWeek)) ? 'block' : 'none';
    layerLegendeBox.appendChild(balken);
    layerLegendeBox.appendChild(skala);
    layerLegendeBox.appendChild(quelle);
    layerLegendeBox.appendChild(ladeHinweisEl);
    layerLegendeBox.appendChild(keinDatenHinweisEl);
    layerLegendeBox.appendChild(wertAnzeigeEl);
    layerLegendeBox.appendChild(koordinatenEl);
    layerLegendeBox.appendChild(ortschaftEl);
  }

  // Anzeigefeld statt Plotly-Tooltip: die Hintergrund-Ebenen sind reine
  // PNGs (kein Hover moeglich) - beim Bewegen/Tippen ueber der Karte wird
  // per Pixel->Daten-Umrechnung (Plotlys eigene xaxis/yaxis.p2d()) die
  // naechstgelegene Zelle des mitgelieferten, groben Werte-Gitters (siehe
  // R: raster_zu_datauri(), dieselbe Aufloesung wie das jeweilige Bild)
  // nachgeschlagen - kein zusaetzlicher Server, keine Plotly-Trace noetig.
  // Liefert das Werte-Gitter der AKTUELL gewaehlten Ebene nur, wenn sie
  // bereits vollstaendig geladen ist (siehe ladeEbene()/ebenenCache oben) -
  // waehrend des ersten Ladens (kurzes Zeitfenster) liefert die Funktion
  // null, die Cursor-Wertabfrage zeigt dann keine Daten statt eines
  // veralteten/falschen Werts.
  function aktivesWerteGitter() {
    if (hintergrundEbene === 'keine') return null;
    var cache = ebenenCache[ebeneDateiSchluessel(hintergrundEbene)];
    return cache ? cache.werte : null;
  }
  // Naeherungsformel swisstopo (WGS84 -> LV95, Genauigkeit ca. 1-2m,
  // Approximate formulas for the transformation between Swiss projection
  // coordinates and WGS84 - rein clientseitig ohne Serveraufruf, fuer die
  // Koordinatenanzeige beim Cursor.
  function wgs84ZuLv95(lon, lat) {
    var phiSek = (lat * 3600 - 169028.66) / 10000;
    var lambdaSek = (lon * 3600 - 26782.5) / 10000;
    var e = 2600072.37
      + 211455.93 * lambdaSek
      - 10938.51 * lambdaSek * phiSek
      - 0.36 * lambdaSek * Math.pow(phiSek, 2)
      - 44.54 * Math.pow(lambdaSek, 3);
    var n = 1200147.07
      + 308807.95 * phiSek
      + 3745.25 * Math.pow(lambdaSek, 2)
      + 76.63 * Math.pow(phiSek, 2)
      - 194.56 * Math.pow(lambdaSek, 2) * phiSek
      + 119.79 * Math.pow(phiSek, 3);
    return { e: e, n: n };
  }
  // Ortschaft/PLZ per swisstopo-Identify-Abfrage (amtliches Ortschaften-
  // verzeichnis) - im Gegensatz zu Koordinaten/Rasterwert (rein lokal) ein
  // echter Serveraufruf, daher verzoegert (erst wenn der Cursor kurz
  // stillsteht) statt bei jedem mousemove, um die oeffentliche API nicht
  // unnoetig oft anzufragen. Bei JEDER Cursorbewegung wird der alte Ort
  // sofort ausgeblendet (blinkende Punkte statt eines veralteten Werts) -
  // ortschaftAnfrageId veraltet dabei auch eine evtl. noch laufende
  // vorherige Anfrage sofort (deren Antwort koennte sonst NACH einer
  // neueren eintreffen und den Ort faelschlich wieder zuruecksetzen).
  var ortschaftAbfrageTimer = null;
  var ortschaftAnfrageId = 0;
  var LADE_PUNKTE_HTML = '<span class=gw-lade-punkte><span></span><span></span><span></span></span>';
  function sucheOrtschaftPlz(e, n, meineId) {
    var url = 'https://api3.geo.admin.ch/rest/services/api/MapServer/identify' +
      '?geometry=' + e + ',' + n + '&geometryType=esriGeometryPoint&imageDisplay=1,1,1' +
      '&mapExtent=' + e + ',' + n + ',' + e + ',' + n + '&tolerance=50' +
      '&layers=all:ch.swisstopo-vd.ortschaftenverzeichnis_plz&returnGeometry=false&sr=2056';
    fetch(url).then(function(r) { return r.json(); }).then(function(daten) {
      if (!ortschaftEl || meineId !== ortschaftAnfrageId) return;
      var treffer = daten && daten.results && daten.results[0];
      ortschaftEl.textContent = treffer ?
        ('Ort: ' + treffer.attributes.plz + ' ' + treffer.attributes.langtext) :
        'Ort: ausserhalb der Schweiz';
    }).catch(function() {
      if (ortschaftEl && meineId === ortschaftAnfrageId) ortschaftEl.textContent = 'Ort: nicht abrufbar (offline?)';
    });
  }
  function zeigeWertAmPunkt(lon, lat) {
    if (!wertAnzeigeEl) return;
    var lv95 = wgs84ZuLv95(lon, lat);
    if (koordinatenEl) {
      koordinatenEl.textContent = 'Koordinaten: ' + Math.round(lv95.e) + ' / ' + Math.round(lv95.n) + ' (LV95)';
    }
    if (ortschaftEl) ortschaftEl.innerHTML = 'Ort: ' + LADE_PUNKTE_HTML;
    ortschaftAnfrageId++;
    clearTimeout(ortschaftAbfrageTimer);
    ortschaftAbfrageTimer = setTimeout(function() {
      var meineId = ++ortschaftAnfrageId;
      sucheOrtschaftPlz(lv95.e, lv95.n, meineId);
    }, 350);
    var gitterJeWoche = aktivesWerteGitter();
    var info = layerLegenden[hintergrundEbene];
    var gitter = gitterJeWoche ? gitterJeWoche[selectedYear + ' ' + selectedWeek] : null;
    if (!gitter || !info) return;
    var col = Math.floor((lon - gitter.x0) / (gitter.x1 - gitter.x0) * gitter.ncol);
    var row = Math.floor((gitter.y1 - lat) / (gitter.y1 - gitter.y0) * gitter.nrow);
    if (col < 0 || col >= gitter.ncol || row < 0 || row >= gitter.nrow) {
      wertAnzeigeEl.textContent = 'Wert am Cursor: ausserhalb der Schweiz';
      return;
    }
    var wert = gitter.m[row][col];
    wertAnzeigeEl.textContent = (wert === null || wert === undefined) ?
      'Wert am Cursor: keine Daten' : 'Wert am Cursor: ' + wert + ' ' + info.einheit;
  }
  function versteckeWertAnzeige() {
    if (wertAnzeigeEl) wertAnzeigeEl.textContent = 'Wert am Cursor: –';
    if (koordinatenEl) koordinatenEl.textContent = 'Koordinaten: –';
    if (ortschaftEl) ortschaftEl.textContent = 'Ort: –';
    ortschaftAnfrageId++;
    clearTimeout(ortschaftAbfrageTimer);
  }
  function verarbeiteKartenZeiger(evt) {
    var growthMapGd = document.querySelector('#datenexplorer-growthmap .js-plotly-plot');
    if (!growthMapGd) return;
    var fl = growthMapGd._fullLayout;
    if (!fl || !fl.xaxis || !fl.yaxis) return;
    var punkt = (evt.touches && evt.touches.length > 0) ? evt.touches[0] : evt;
    var rect = growthMapGd.getBoundingClientRect();
    var xPixel = punkt.clientX - rect.left;
    var yPixel = punkt.clientY - rect.top;
    var lon = fl.xaxis.p2d(xPixel - fl.xaxis._offset);
    var lat = fl.yaxis.p2d(yPixel - fl.yaxis._offset);
    zeigeWertAmPunkt(lon, lat);
  }
  if (mapControlsContainer) {
    var layerPanel = document.createElement('div');
    layerPanel.className = 'gw-layer-panel';
    var layerHeading = document.createElement('div');
    layerHeading.className = 'gw-layer-heading';
    layerHeading.textContent = 'Ebenen';
    layerPanel.appendChild(layerHeading);

    // PLZ/Ort-Suche: swisstopo-SearchServer (dieselbe oeffentliche API wie
    // fuer die Cursor-Ortsabfrage) liefert Vorschlaege waehrend des Tippens
    // (origins=zipcode,gg25 deckt Postleitzahlen UND Gemeindenamen ab).
    // Bei Auswahl (Klick oder Enter) wird ein Fadenkreuz-Marker auf die
    // Karte gesetzt und per Plotly.Fx.hover() dessen Tooltip (Ortsname,
    // Ebenen-Wert an dieser Stelle, LV95-Koordinaten) sofort angezeigt -
    // zusaetzlich aktualisiert sich das normale Cursor-Anzeigefeld gleich
    // mit (zeigeWertAmPunkt()).
    var sucheWrap = document.createElement('div');
    sucheWrap.className = 'gw-combo';
    sucheWrap.style.marginBottom = '10px';
    sucheWrap.style.display = 'block';
    var sucheInput = document.createElement('input');
    sucheInput.type = 'text';
    sucheInput.placeholder = 'PLZ oder Ort suchen…';
    sucheInput.style.width = '100%';
    var sucheListe = document.createElement('div');
    sucheListe.className = 'gw-combo-list';
    sucheListe.style.display = 'none';
    sucheListe.style.width = '100%';
    sucheWrap.appendChild(sucheInput);
    sucheWrap.appendChild(sucheListe);
    layerPanel.appendChild(sucheWrap);

    var sucheTimer = null;
    var sucheErgebnisse = [];
    function ortsLabelKlartext(treffer) { return treffer.attrs.label.replace(new RegExp('</?b>', 'g'), ''); }
    function sucheOrteVorschlaege(text) {
      if (!text || text.length < 2) { sucheListe.style.display = 'none'; return; }
      var url = 'https://api3.geo.admin.ch/rest/services/api/SearchServer' +
        '?searchText=' + encodeURIComponent(text) + '&type=locations&origins=zipcode,gg25&limit=8&sr=2056';
      fetch(url).then(function(r) { return r.json(); }).then(function(daten) {
        sucheErgebnisse = (daten && daten.results) || [];
        sucheListe.innerHTML = '';
        sucheErgebnisse.forEach(function(treffer) {
          var item = document.createElement('div');
          item.className = 'gw-combo-item';
          item.textContent = ortsLabelKlartext(treffer);
          item.addEventListener('mousedown', function(evt) {
            evt.preventDefault();
            waehleSucheErgebnis(treffer);
          });
          sucheListe.appendChild(item);
        });
        sucheListe.style.display = sucheErgebnisse.length > 0 ? 'block' : 'none';
      }).catch(function() { sucheListe.style.display = 'none'; });
    }
    function waehleSucheErgebnis(treffer) {
      sucheInput.value = ortsLabelKlartext(treffer);
      sucheListe.style.display = 'none';
      platziereFadenkreuz(treffer.attrs.lon, treffer.attrs.lat, ortsLabelKlartext(treffer));
    }
    sucheInput.addEventListener('input', function() {
      clearTimeout(sucheTimer);
      var text = sucheInput.value;
      sucheTimer = setTimeout(function() { sucheOrteVorschlaege(text); }, 300);
    });
    sucheInput.addEventListener('keydown', function(evt) {
      if (evt.key === 'Enter' && sucheErgebnisse.length > 0) {
        evt.preventDefault();
        waehleSucheErgebnis(sucheErgebnisse[0]);
      }
    });
    sucheInput.addEventListener('blur', function() {
      setTimeout(function() { sucheListe.style.display = 'none'; }, 150);
    });

    // Setzt (bzw. verschiebt) den Fadenkreuz-Marker auf lon/lat und zeigt
    // per Plotly.Fx.hover() sofort dessen Tooltip - der Ortsname kommt
    // direkt aus dem Suchtreffer (keine erneute Ortsabfrage noetig, wir
    // haben ja gerade danach gesucht), Ebenen-Wert/Koordinaten wie beim
    // Cursor-Anzeigefeld berechnet.
    function platziereFadenkreuz(lon, lat, ortsName) {
      var growthMapGd = document.querySelector('#datenexplorer-growthmap .js-plotly-plot');
      if (!growthMapGd) return;
      var lv95 = wgs84ZuLv95(lon, lat);
      var zeilen = ['<b>' + ortsName + '</b>'];
      var gitterJeWoche = aktivesWerteGitter();
      var info = layerLegenden[hintergrundEbene];
      var gitter = gitterJeWoche ? gitterJeWoche[selectedYear + ' ' + selectedWeek] : null;
      if (gitter && info) {
        var col = Math.floor((lon - gitter.x0) / (gitter.x1 - gitter.x0) * gitter.ncol);
        var row = Math.floor((gitter.y1 - lat) / (gitter.y1 - gitter.y0) * gitter.nrow);
        if (col >= 0 && col < gitter.ncol && row >= 0 && row < gitter.nrow) {
          var wert = gitter.m[row][col];
          zeilen.push((wert === null || wert === undefined) ? 'Wert: keine Daten' : 'Wert: ' + wert + ' ' + info.einheit);
        } else {
          zeilen.push('Wert: ausserhalb der Schweiz');
        }
      }
      zeilen.push('Koordinaten: ' + Math.round(lv95.e) + ' / ' + Math.round(lv95.n) + ' (LV95)');
      var hoverText = zeilen.join('<br>');
      if (fadenkreuzTraceIdx === null) {
        Plotly.addTraces(growthMapGd, {
          x: [lon], y: [lat], type: 'scatter', mode: 'markers',
          marker: { symbol: 'cross-thin-open', size: 26, color: '#e6194b', line: { width: 2.5 } },
          hoverinfo: 'text', hovertext: [hoverText], showlegend: false, name: 'Suche'
        });
        fadenkreuzTraceIdx = growthMapGd.data.length - 1;
      } else {
        Plotly.restyle(growthMapGd, { x: [[lon]], y: [[lat]], hovertext: [[hoverText]], visible: [true] }, [fadenkreuzTraceIdx]);
      }
      Plotly.Fx.hover(growthMapGd, [{ curveNumber: fadenkreuzTraceIdx, pointNumber: 0 }]);
      zeigeWertAmPunkt(lon, lat);
    }

    // Messnetz-Standorte (die Standort-Positionen selbst, per Hover
    // abfragbar) ist die Basisebene, unabhaengig von der optionalen
    // Hintergrund-Rasterebene weiter unten - deshalb als eigener Schalter,
    // standardmaessig an, statt als weitere Radio-Option. Graswachstum-
    // Kreis und AFC-Ring sind je eigene Bild-Ebenen mit eigenem Schalter
    // (siehe weiter unten), unabhaengig ein-/ausblendbar.
    var messnetzToggleWrap = schalterLinksbuendig(makeToggle('Messnetz-Standorte', true, function(checked) { messnetzOn = checked; applyState(); }));
    messnetzToggleWrap.title = 'Hoverbare Standort-Positionen auf der Karte ein-/ausblenden';
    layerPanel.appendChild(messnetzToggleWrap);
    // Trennlinie (gw-layer-messnetz-toggle) liegt jetzt auf dieser Zeile
    // (MeteoSchweiz-Stationen), nicht mehr auf Messnetz-Standorte - beide
    // gehoeren als Standort-Ebenen zusammen ueber die Linie, Graswachstum/
    // AFC (Bild-Ebenen) darunter.
    macheLayerToggle('MeteoSchweiz-Stationen', false, function(checked) { smnStationenOn = checked; aktualisiereSmnStationen(); },
      'Zeigt die oeffentlichen MeteoSchweiz-Automatikstationen (SwissMetNet) mit ihren aktuellsten Tageswerten (Lufttemperatur, Bodentemperatur, Niederschlag, Globalstrahlung, Sonnenscheindauer) als Diamant-Symbole. Reine Wetter-Referenzstationen, unabhaengig von der gewaehlten Kalenderwoche und NICHT Teil der AGFF-Grasmessungen. Bodentemperatur wird nur an einem Teil der rund 150 Stationen gemessen - dort steht im Tooltip entsprechend keine Daten.',
      'gw-layer-messnetz-toggle');

    // Kleiner i-Knopf mit Klapp-Popup fuer laengere Erklaerungstexte (die
    // Quellenangabe als nativer title-Tooltip reicht fuer eine ganze
    // Absatz-Erklaerung nicht) - per Klick statt nur Hover, damit es auch
    // auf Touch-Geraeten funktioniert; ein Klick ausserhalb schliesst das
    // Popup wieder.
    function macheInfoKnopf(text) {
      var wrap = document.createElement('span');
      wrap.className = 'gw-info-wrap';
      var btn = document.createElement('button');
      btn.type = 'button';
      btn.className = 'gw-info-btn';
      btn.textContent = 'i';
      btn.title = 'Erklaerung anzeigen';
      var popup = document.createElement('div');
      popup.className = 'gw-info-popup';
      popup.textContent = text;
      popup.style.display = 'none';
      btn.addEventListener('click', function(evt) {
        evt.preventDefault();
        evt.stopPropagation();
        var offen = popup.style.display === 'block';
        document.querySelectorAll('.gw-info-popup').forEach(function(p) { p.style.display = 'none'; });
        popup.style.display = offen ? 'none' : 'block';
      });
      wrap.appendChild(btn);
      wrap.appendChild(popup);
      return wrap;
    }
    document.addEventListener('click', function() {
      document.querySelectorAll('.gw-info-popup').forEach(function(p) { p.style.display = 'none'; });
    });

    // makeToggle() setzt normalerweise Text VOR den Schalter (so in der
    // Kurven-Legende gewuenscht) - im Ebenen-Kasten sollen alle Schalter
    // wie die Radiobuttons darunter linksbuendig ausgerichtet sein
    // (Schalter/Radio links, Text rechts daneben), deshalb hier vertauscht.
    function schalterLinksbuendig(toggleWrap) {
      if (toggleWrap.children.length === 2) toggleWrap.insertBefore(toggleWrap.children[1], toggleWrap.children[0]);
      return toggleWrap;
    }

    // Wie makeLayerRadio() unten, nur fuer einen Umschalter (Toggle) statt
    // eines Radiobuttons - fuer Graswachstum/AFC, die (anders als die
    // Hintergrund-Raster-Ebenen) unabhaengig VONEINANDER ein-/ausblendbar
    // sein sollen, nicht als Radiogruppe.
    function macheLayerToggle(labelText, checked, onChange, erklaerung, zusatzKlasse) {
      var zeile = document.createElement('div');
      zeile.className = 'gw-layer-option-zeile' + (zusatzKlasse ? ' ' + zusatzKlasse : '');
      var toggleWrap = schalterLinksbuendig(makeToggle(labelText, checked, onChange));
      zeile.appendChild(toggleWrap);
      if (erklaerung) zeile.appendChild(macheInfoKnopf(erklaerung));
      layerPanel.appendChild(zeile);
      return toggleWrap;
    }
    macheLayerToggle('Graswachstum', true, function(checked) { graswachstumOn = checked; applyState(); },
      'Die Zahl im Kreis zeigt das zuletzt gemessene Graswachstum in kg TS/ha/Tag (Trockensubstanz-Zuwachs pro Hektare und Tag). Die Graufaerbung des Kreises zeigt, wie lange die Messung zurueckliegt: weiss = frisch gemessen (0 Tage), dunkelgrau = bis zu 14 Tage alt. Standorte ohne Messung in den letzten 14 Tagen werden nicht mehr angezeigt.');
    macheLayerToggle('AFC', true, function(checked) { afcOn = checked; applyState(); },
      'AFC (Average Farm Cover) schaetzt den aktuellen Grasvorrat des Betriebs in kg Trockensubstanz pro Hektare (kg TS/ha). Der Ring zeigt diesen Vorrat als Fortschrittsbalken auf einer Skala von 0 bis 1500 kg TS/ha und faerbt ihn nach dem jahreszeitlichen Zielbereich: rot = deutlich zu wenig (unter 200 kg praktisch leer), gruen = im Zielbereich, blaugruen = deutlich mehr als noetig. Der Zielbereich verschiebt sich uebers Jahr, z.B. Fruehling ca. 500-700, Sommer ca. 700-800, Herbst ca. 900-1200 kg TS/ha.',
      'gw-layer-vor-meteodaten');

    // Kompakte AFC-Legende (Ring) DIREKT nach dem AFC-Schalter, statt erst
    // ganz unten nach den Meteodaten-Ebenen/der Legende dazu - gehoert
    // inhaltlich zum AFC-Schalter direkt darueber. aktualisiereAfcLegende()
    // (siehe unten) blendet die Box aus, sobald AFC ausgeschaltet ist.
    afcLegendeBox = document.createElement('div');
    afcLegendeBox.className = 'gw-layer-legende';
    afcLegendeBox.style.display = 'none';
    layerPanel.appendChild(afcLegendeBox);

    // Schieberegler fuer die Fenstergroesse (Tage) der gleitendes-Fenster-
    // Ebenen (meteoFensterEbenen, siehe oben) - wird OBERHALB der Legende
    // eingefuegt (layerPanel.appendChild() hier laeuft VOR dem der Legende
    // weiter unten) und ist nur sichtbar, waehrend eine Fenster-Ebene aktiv
    // ist (nicht bei keine Meteodaten, Bodenwasserbilanz, Wachstumsgrad-
    // tage - siehe aktualisiereMeteoFensterSichtbarkeit()).
    var meteoFensterWrap = null, meteoFensterInput = null, meteoFensterLabel = null;
    function macheMeteoFensterSchieberegler() {
      meteoFensterWrap = document.createElement('div');
      meteoFensterWrap.className = 'gw-meteo-fenster';
      meteoFensterLabel = document.createElement('div');
      meteoFensterLabel.className = 'gw-meteo-fenster-label';
      meteoFensterInput = document.createElement('input');
      meteoFensterInput.type = 'range';
      meteoFensterInput.min = '0';
      meteoFensterInput.max = String(meteoFensterStufen.length - 1);
      meteoFensterInput.step = '1';
      meteoFensterInput.addEventListener('input', function() {
        meteoFenster = meteoFensterStufen[parseInt(meteoFensterInput.value, 10)];
        aktualisiereMeteoFensterAnzeige();
        aktualisiereHintergrundEbene();
        aktualisiereLayerLegende();
      });
      meteoFensterWrap.appendChild(meteoFensterLabel);
      meteoFensterWrap.appendChild(meteoFensterInput);
      layerPanel.appendChild(meteoFensterWrap);
      aktualisiereMeteoFensterAnzeige();
      aktualisiereMeteoFensterSichtbarkeit();
    }
    function aktualisiereMeteoFensterAnzeige() {
      if (meteoFensterInput) meteoFensterInput.value = String(meteoFensterStufen.indexOf(meteoFenster));
      if (meteoFensterLabel) meteoFensterLabel.textContent = 'Zeitraum: ' + meteoFenster + ' Tage';
      aktualisiereLayerLabels();
    }
    function aktualisiereMeteoFensterSichtbarkeit() {
      if (meteoFensterWrap) meteoFensterWrap.style.display = istFensterEbene(hintergrundEbene) ? 'block' : 'none';
    }

    // title (nativer Browser-Tooltip) je Option mit der Quellenangabe, wie
    // einst als Untertitel bei den Export-Grafiken (siehe layerLegenden.quelle).
    // erklaerung (optional): zusaetzlicher i-Knopf mit laengerem Klartext.
    function makeLayerRadio(value, labelText, erklaerung, zusatzKlasse) {
      var zeile = document.createElement('div');
      zeile.className = 'gw-layer-option-zeile' + (zusatzKlasse ? ' ' + zusatzKlasse : '');
      var wrap = document.createElement('label');
      wrap.className = 'gw-layer-option';
      if (layerLegenden[value]) wrap.title = 'Quelle: ' + layerLegenden[value].quelle;
      var radio = document.createElement('input');
      radio.type = 'radio';
      radio.name = 'gw-layer';
      radio.value = value;
      radio.checked = (value === 'keine');
      radio.addEventListener('change', function() {
        if (!radio.checked) return;
        hintergrundEbene = value;
        // Schieberegler-Fenstergroesse springt bei JEDEM Ebenenwechsel auf
        // den fuer die neue Ebene hinterlegten Standard zurueck (siehe
        // meteoFensterStandard oben) - kein Merken eines individuellen
        // Werts je Ebene.
        if (istFensterEbene(value)) meteoFenster = meteoFensterStandard[value];
        aktualisiereMeteoFensterAnzeige();
        aktualisiereHintergrundEbene();
        aktualisiereLayerLegende();
      });
      var text = document.createElement('span');
      text.textContent = labelText;
      radio.labelTextEl = text;
      wrap.appendChild(radio);
      wrap.appendChild(text);
      zeile.appendChild(wrap);
      if (erklaerung) zeile.appendChild(macheInfoKnopf(erklaerung));
      layerPanel.appendChild(zeile);
      return radio;
    }
    makeLayerRadio('keine', 'keine Meteodaten');
    radioNiederschlag = makeLayerRadio('niederschlag', layerLegenden.niederschlag.label);
    radioTemperatur = makeLayerRadio('temperatur', layerLegenden.temperatur.label,
      'Mittlere Lufttemperatur (2m) im oben gewaehlten Zeitraum vor dem Stichtag. Graswachstum beginnt erst ab einer Basistemperatur von ca. 5 Grad C spuerbar (darunter praktisch Wachstumsstillstand), das Optimum liegt bei ca. 15-20 Grad C. Ueber ca. 25 Grad C bremst Hitzestress das Wachstum trotz ausreichend Wasser wieder. Als Faustregel fuer den Wachstumsantrieb ueber mehrere Tage dient die Wachstumsgradtagsumme: Summe aus (Tagesmitteltemperatur minus 5 Grad C) an allen Tagen mit Werten darueber.');
    radioBodentemperatur = makeLayerRadio('bodentemperatur', layerLegenden.bodentemperatur.label,
      'ACHTUNG SCHAETZUNG, keine Feldmessung: MeteoSchweiz misst Bodentemperatur nur an einzelnen Stationen, nicht flaechendeckend als Karte. Gezeigt wird stattdessen der gleitende Mittelwert der Lufttemperatur (2m) im oben gewaehlten Zeitraum - eine grobe Naeherung an die traegere, gedaempfte oberste Bodenschicht (ca. 5-10cm); ein laengerer Zeitraum simuliert mehr Daempfung. Bodentemperatur ist u.a. fuer den Vegetationsbeginn im Fruehling und die Stickstoff-Mineralisierung im Boden relevant: beides kommt unter ca. 5-8 Grad C weitgehend zum Erliegen.');
    radioSonnenschein = makeLayerRadio('sonnenschein', layerLegenden.sonnenschein.label,
      'Sonnenscheindauer im oben gewaehlten Zeitraum vor dem Stichtag, relativ zur astronomisch maximal moeglichen Tagesdauer (0-100%, MeteoSchweiz SrelD). Mehr Sonne treibt die Photosynthese und damit das Wachstum an, erhoeht aber auch die Verdunstung (siehe ET0/Bodenwasserbilanz). Diese Daten werden erst mit 1-2 Monaten Verzoegerung aufbereitet - die allerneuesten Wochen sind deshalb oft noch nicht verfuegbar.');
    radioEt0 = makeLayerRadio('et0', layerLegenden.et0.label,
      'Potenzielle Verdunstung (Evapotranspiration) nach der Hargreaves-Formel (FAO-56), Summe im oben gewaehlten Zeitraum vor dem Stichtag - dieselbe Berechnung, die auch ins Bucket-Modell der Bodenwasserbilanz einfliesst. Zeigt, wie viel Wasser dem Boden allein durch Verdunstung entzogen wird: hohe Werte bei gleichzeitig wenig Niederschlag beguenstigen Trockenstress.');
    radioGdd = makeLayerRadio('gdd', layerLegenden.gdd.label,
      'Kumulierte Wachstumsgradtage seit Beginn der lokal vorhandenen Temperaturdaten: Summe aus (Tagesmitteltemperatur minus 5 Grad C) an allen Tagen mit Werten darueber, laufend aufaddiert (MeteoSchweiz TabsD). Eine in der Agronomie gebraeuchliche Faustregel fuer die pflanzenverfuegbare Waermesumme seit Vegetationsbeginn - hoehere Werte bedeuten mehr angesammelte Wachstumsbedingungen.');
    // Bodenwasserbilanz ganz am Schluss, mit Trennlinie abgesetzt: anders
    // als die anderen Ebenen (direkte MeteoSchweiz-Messwerte/-Aggregate) ist
    // dies eine SELBST BERECHNETE Groesse (Eimer-Modell aus Niederschlag +
    // ET0, siehe Erklaerung) - das Label macht das zusaetzlich explizit.
    radioBoden = makeLayerRadio('boden', layerLegenden.boden.label,
      'Der Boden wird vereinfacht wie ein Eimer betrachtet: Regen fuellt ihn, Verdunstung leert ihn. Ist der Eimer voll, laeuft der Ueberschuss ungenutzt ab. Wie viel taeglich verdunstet, wird aus den Temperaturen geschaetzt - ein feuchter Boden verdunstet mehr als ein bereits trockener. Der Wert zeigt den aktuellen Fuellstand: 100 mm = Boden gut mit Wasser versorgt, 0 mm = ausgetrocknet.',
      'gw-layer-vor-boden');
    // Nachschlagetabelle Ebenenname -> Radio, fuer aktualisiereLayerLabels()
    // (haengt dort das Symbol/die Fenstergroesse an alle 5 Fenster-Ebenen).
    radioJeEbene = { niederschlag: radioNiederschlag, temperatur: radioTemperatur, bodentemperatur: radioBodentemperatur, sonnenschein: radioSonnenschein, et0: radioEt0 };
    macheMeteoFensterSchieberegler();
    aktualisiereLayerLabels();
    layerLegendeBox = document.createElement('div');
    layerLegendeBox.className = 'gw-layer-legende';
    layerLegendeBox.style.display = 'none';
    layerPanel.appendChild(layerLegendeBox);
    mapControlsContainer.appendChild(layerPanel);
    aktualisiereLayerVerfuegbarkeit();
    aktualisiereLayerLegende();
  }

  function makeToggle(labelText, checked, onChange) {
    var wrap = document.createElement('div');
    wrap.className = 'gw-toggle-wrap';
    var text = document.createElement('span');
    text.textContent = labelText;
    var label = document.createElement('label');
    label.className = 'gw-toggle';
    var checkbox = document.createElement('input');
    checkbox.type = 'checkbox';
    checkbox.checked = checked;
    var slider = document.createElement('span');
    slider.className = 'gw-toggle-slider';
    checkbox.addEventListener('change', function() { onChange(checkbox.checked); });
    label.appendChild(checkbox);
    label.appendChild(slider);
    wrap.appendChild(text);
    wrap.appendChild(label);
    wrap.checkbox = checkbox;
    return wrap;
  }

  var precipToggleWrap = makeToggle('Niederschlag', true, function(checked) { precipOn = checked; applyState(); });
  var precipCheckbox = precipToggleWrap.checkbox;
  var xAxisToggleWrap = makeToggle('Kalenderwochen', true, function(checked) { datumOn = !checked; applyXAxis(); });
  var vorjahrToggleWrap = makeToggle('Vorjahresdaten', false, function(checked) { vorjahrOn = checked; applyState(); });
  vorjahrToggleWrap.title = 'Kurve(n) des Vorjahres zum Vergleich in Grau einblenden';

  controls.appendChild(comboWrap);
  controls.appendChild(yearSelect);

  var titleEl = document.createElement('div');
  titleEl.className = 'gw-title';
  titleEl.textContent = 'Graswachstumskurve';

  var fillHost = document.createElement('div');
  fillHost.style.width = '100%';
  el.parentNode.insertBefore(fillHost, el);
  fillHost.appendChild(titleEl);
  fillHost.appendChild(controls);

  var chartRow = document.createElement('div');
  chartRow.className = 'gw-chart-row';

  var legendPanel = document.createElement('div');
  legendPanel.className = 'gw-legend-panel';
  var legendHeader = document.createElement('div');
  legendHeader.className = 'gw-legend-header';
  var legendTitle = document.createElement('span');
  legendTitle.textContent = 'Standort';
  var legendClose = document.createElement('button');
  legendClose.type = 'button';
  legendClose.className = 'gw-legend-close';
  legendClose.title = 'Legende ausblenden';
  legendClose.textContent = String.fromCharCode(215);
  legendHeader.appendChild(legendTitle);
  legendHeader.appendChild(legendClose);
  var legendOptions = document.createElement('div');
  legendOptions.className = 'gw-legend-options';
  legendOptions.appendChild(precipToggleWrap);
  legendOptions.appendChild(xAxisToggleWrap);
  legendOptions.appendChild(vorjahrToggleWrap);
  var legendList = document.createElement('div');
  legendPanel.appendChild(legendHeader);
  legendPanel.appendChild(legendOptions);
  legendPanel.appendChild(legendList);

  var legendEdge = document.createElement('div');
  legendEdge.className = 'gw-legend-edge';
  var edgeBtn = document.createElement('button');
  edgeBtn.type = 'button';
  edgeBtn.className = 'gw-edge-btn active';
  edgeBtn.title = 'Legende ein-/ausblenden';
  edgeBtn.textContent = String.fromCharCode(9776);
  legendEdge.appendChild(edgeBtn);

  function setLegendOpen(open) {
    legendPanel.classList.toggle('collapsed', !open);
    edgeBtn.classList.toggle('active', open);
    // Plotlys responsive-Modus reagiert zwar von selbst auf die Breiten-
    // aenderung (per ResizeObserver auf el), ein expliziter Resize-Aufruf
    // NACH Abschluss der CSS-Breiten-Transition (150ms) stellt aber
    // zuverlaessig sicher, dass die Kurve den frei werdenden Platz nutzt,
    // unabhaengig von Browser-spezifischen Details der ResizeObserver-Timing.
    setTimeout(function() { Plotly.Plots.resize(el); }, 200);
  }
  edgeBtn.addEventListener('click', function() { setLegendOpen(legendPanel.classList.contains('collapsed')); });
  legendClose.addEventListener('click', function() { setLegendOpen(false); });

  // siteIdx (optional): macht den Eintrag klickbar (Standort-Filter, siehe
  // waehleSiteViaKlick()) - fuer die nicht-standortbezogenen Eintraege
  // (Mittleres Wachstum, Durchschnitt Mittelland) wird kein siteIdx uebergeben.
  function addLegendItem(label, color, style, siteIdx) {
    var item = document.createElement('div');
    item.className = 'gw-legend-item';
    if (siteIdx !== undefined) {
      item.classList.add('gw-legend-item-clickable');
      item.title = 'Nur ' + label + ' anzeigen';
      item.addEventListener('click', function() { waehleSiteViaKlick(siteIdx); });
    }
    var swatch = document.createElement('span');
    swatch.className = 'gw-legend-swatch';
    swatch.style.borderTopColor = color;
    swatch.style.borderTopStyle = style;
    var text = document.createElement('span');
    text.textContent = label;
    item.appendChild(swatch);
    item.appendChild(text);
    legendList.appendChild(item);
  }
  function renderLegendItems() {
    legendList.innerHTML = '';
    if (selection.type === 'group') {
      for (var i = 0; i < siteNames.length; i++) {
        if (siteVisible[selection.idx][i]) addLegendItem(siteNames[i], siteColors[i], 'solid', i);
      }
      addLegendItem('Mittleres Wachstum', 'black', 'dashed');
    } else {
      addLegendItem(siteNames[selection.idx], siteColors[selection.idx], 'solid', selection.idx);
    }
    addLegendItem('Durchschnitt Mittelland', 'red', 'dotted');
  }

  el.style.flex = '1 1 auto';
  el.style.minWidth = '0';
  chartRow.appendChild(el);
  chartRow.appendChild(legendPanel);
  chartRow.appendChild(legendEdge);
  fillHost.appendChild(chartRow);

  // el wurde bereits von Plotly (responsive=TRUE) auf seine urspruengliche
  // volle Breite gerendert, BEVOR es hier in chartRow neben legendPanel
  // (210px) eingefuegt wurde. Der ResizeObserver, den responsive=TRUE
  // registriert, erkennt diese synchrone DOM-Umstrukturierung nicht
  // zuverlaessig sofort - die Kurve blieb bisher zu breit (Ueberlappung
  // mit der Sidebar) und korrigierte sich erst bei einem echten Browser-
  // Resize/Zoom, der einen Reflow ausloest. Ein expliziter Resize-Aufruf
  // nach dem naechsten Layout-Frame (wenn die neue Flex-Breite bereits
  // feststeht) erzwingt die korrekte Breite von Anfang an, analog zum
  // Resize-Aufruf in setLegendOpen() weiter oben.
  requestAnimationFrame(function() { Plotly.Plots.resize(el); });

  // Kalenderwochen-Schieberegler: eigener Container zwischen Kurve und
  // Karten (im HTML bereits als leeres <div id=\"datenexplorer-slider\">
  // angelegt), wird hier befuellt - steuert beide Karten gemeinsam.
  var sliderContainer = document.getElementById('datenexplorer-slider');
  var weekLabel = null;
  var sliderInput = null;
  if (sliderContainer) {
    var sliderRow = document.createElement('div');
    sliderRow.className = 'gw-slider-row';

    // alignedBox wird per JS exakt auf die Breite/Position der x-Achse
    // (Zeichenflaeche) der Kurve darunter ausgerichtet (siehe
    // syncSliderZuAchse() weiter unten) - Wochenlabel, Pfeile und
    // Schieberegler liegen alle darin, Heute-Button bewusst ausserhalb
    // (rechts davon, siehe sliderRow.appendChild(todayBtn) unten).
    var alignedBox = document.createElement('div');
    alignedBox.className = 'gw-slider-aligned';
    alignedBox.style.position = 'relative';

    weekLabel = document.createElement('div');
    weekLabel.className = 'gw-slider-label';

    var trackRow = document.createElement('div');
    trackRow.className = 'gw-slider-track-row';

    sliderInput = document.createElement('input');
    sliderInput.type = 'range';
    sliderInput.min = '1';
    sliderInput.max = '52';
    sliderInput.step = '1';
    sliderInput.value = String(selectedWeek);

    // Schwebender Tooltip ueber dem Schieberegler-Griff, der WAEHREND des
    // Ziehens (nicht erst nach Loslassen) live die gewaehlte Woche zeigt -
    // die bisherige Anzeige (weekLabel) bleibt zusaetzlich bestehen.
    var sliderTooltip = document.createElement('div');
    sliderTooltip.className = 'gw-slider-tooltip';
    sliderTooltip.style.display = 'none';
    alignedBox.appendChild(sliderTooltip);

    var zukunftMaske = document.createElement('div');
    zukunftMaske.className = 'gw-zukunft-maske';
    zukunftMaske.style.display = 'none';
    alignedBox.appendChild(zukunftMaske);

    function aktualisiereZukunftMaske() {
      var maxW = maxWocheFuerJahr(selectedYear);
      if (maxW >= 52) { zukunftMaske.style.display = 'none'; return; }
      var min = parseFloat(sliderInput.min), max = parseFloat(sliderInput.max);
      var anteil = (maxW - min) / (max - min);
      var sliderRect = sliderInput.getBoundingClientRect();
      var boxRect = alignedBox.getBoundingClientRect();
      var maskLinks = (sliderRect.left - boxRect.left) + anteil * sliderRect.width;
      zukunftMaske.style.left = maskLinks + 'px';
      zukunftMaske.style.width = Math.max(0, (sliderRect.right - boxRect.left) - maskLinks) + 'px';
      zukunftMaske.style.top = (sliderRect.top - boxRect.top) + 'px';
      zukunftMaske.style.height = sliderRect.height + 'px';
      zukunftMaske.style.display = 'block';
    }

    function positioniereSliderTooltip() {
      var min = parseFloat(sliderInput.min), max = parseFloat(sliderInput.max);
      var anteil = (selectedWeek - min) / (max - min);
      var sliderRect = sliderInput.getBoundingClientRect();
      var boxRect = alignedBox.getBoundingClientRect();
      var thumbX = (sliderRect.left - boxRect.left) + anteil * sliderRect.width;
      sliderTooltip.style.left = thumbX + 'px';
      sliderTooltip.textContent = 'KW ' + selectedWeek;
    }
    function zeigeSliderTooltip() { sliderTooltip.style.display = 'block'; positioniereSliderTooltip(); }
    function verstecke_SliderTooltip() { sliderTooltip.style.display = 'none'; }
    sliderInput.addEventListener('mousedown', zeigeSliderTooltip);
    sliderInput.addEventListener('touchstart', zeigeSliderTooltip);
    window.addEventListener('mouseup', verstecke_SliderTooltip);
    window.addEventListener('touchend', verstecke_SliderTooltip);

    // 'input' feuert bei <input type=range> laufend WAEHREND des Ziehens
    // (anders als 'change', das erst beim Loslassen feuert) - Karten und
    // Tooltip aktualisieren sich daher schon live beim Verschieben.
    sliderInput.addEventListener('input', function() {
      var w = parseInt(sliderInput.value, 10);
      var maxW = maxWocheFuerJahr(selectedYear);
      if (w > maxW) { w = maxW; sliderInput.value = String(w); }
      selectedWeek = w;
      applyMapState();
      positioniereSliderTooltip();
    });

    function springeZuWoche(w) {
      selectedWeek = Math.max(1, Math.min(maxWocheFuerJahr(selectedYear), w));
      sliderInput.value = String(selectedWeek);
      applyMapState();
    }

    var prevBtn = document.createElement('button');
    prevBtn.type = 'button';
    prevBtn.className = 'gw-step-btn';
    prevBtn.title = 'Eine Woche zurueck';
    prevBtn.textContent = String.fromCharCode(9664);
    prevBtn.addEventListener('click', function() { springeZuWoche(selectedWeek - 1); });

    var nextBtn = document.createElement('button');
    nextBtn.type = 'button';
    nextBtn.className = 'gw-step-btn';
    nextBtn.title = 'Eine Woche vor';
    nextBtn.textContent = String.fromCharCode(9654);
    nextBtn.addEventListener('click', function() { springeZuWoche(selectedWeek + 1); });

    var todayBtn = document.createElement('button');
    todayBtn.type = 'button';
    todayBtn.className = 'gw-step-btn gw-today-btn';
    todayBtn.title = 'Aktuelle Kalenderwoche (heutiges Jahr)';
    todayBtn.textContent = 'Heute';
    todayBtn.addEventListener('click', function() {
      selectedYear = neuestesJahr;
      yearSelect.value = neuestesJahr;
      var hatNiederschlag = jahreMitNiederschlag.indexOf(selectedYear) !== -1;
      precipCheckbox.disabled = !hatNiederschlag;
      aktualisiereLayerVerfuegbarkeit();
      applyXAxis();
      aktualisiereZukunftMaske();
      springeZuWoche(heutigeWoche);
      applyState();
    });

    // Pfeile sitzen neben der Wochenbeschriftung, oberhalb des eigentlichen
    // Schiebereglers (nicht mehr links/rechts vom Regler selbst).
    var labelRow = document.createElement('div');
    labelRow.className = 'gw-slider-label-row';
    labelRow.appendChild(prevBtn);
    labelRow.appendChild(weekLabel);
    labelRow.appendChild(nextBtn);
    trackRow.appendChild(sliderInput);
    alignedBox.appendChild(labelRow);
    alignedBox.appendChild(trackRow);
    sliderRow.appendChild(alignedBox);
    sliderRow.appendChild(todayBtn);
    sliderContainer.appendChild(sliderRow);

    // Der Zeitstrahl (Wochenlabel + Pfeile + Schieberegler, alles in
    // alignedBox) soll IMMER exakt gleich breit sein wie die x-Achse
    // (Zeichenflaeche) der Kurve darunter - Heute-Button bewusst ausserhalb
    // dieser Ausrichtung, ganz rechts. el._fullLayout._size liefert Plotlys
    // tatsaechlich berechnete Zeichenflaeche relativ zu el SELBST (l=linker
    // Rand fuer die y-Achsenbeschriftung, w=Breite der Zeichenflaeche) -
    // die absoluten Bildschirmpositionen von el und sliderRow werden hier
    // bewusst per getBoundingClientRect() verglichen statt Gleichheit
    // anzunehmen, da sliderRow ein eigenes Padding hat (.gw-slider-row),
    // das el nicht hat.
    function syncSliderZuAchse() {
      var fl = el._fullLayout;
      if (!fl || !fl._size) return;
      var curveRect = el.getBoundingClientRect();
      var rowRect = sliderRow.getBoundingClientRect();
      // margin wird ab dem Ende der PADDING-Box gemessen, nicht ab
      // rowRect.left selbst - .gw-slider-row hat ein eigenes Padding
      // (16px), das hier mit abgezogen werden muss, sonst landet
      // alignedBox um genau diesen Betrag zu weit rechts.
      var rowPaddingLinks = parseFloat(getComputedStyle(sliderRow).paddingLeft) || 0;
      var plotLinksAbs = curveRect.left + fl._size.l;
      var plotRechtsAbs = plotLinksAbs + fl._size.w;
      alignedBox.style.marginLeft = (plotLinksAbs - rowRect.left - rowPaddingLinks) + 'px';
      alignedBox.style.width = (plotRechtsAbs - plotLinksAbs) + 'px';
      positioniereSliderTooltip();
      aktualisiereZukunftMaske();
    }
    syncSliderZuAchse();
    window.addEventListener('resize', syncSliderZuAchse);
    // Feuert nach JEDEM Redraw der Kurve (Fenstergroesse, Sidebar auf/zu,
    // Legende ein/aus - alles, was die Zeichenflaechen-Breite aendern kann).
    el.on('plotly_afterplot', syncSliderZuAchse);
  }

  // Kalenderwoche zusaetzlich per Mausklick auf die x-Achse der Kurve
  // waehlbar - Plotlys eigenes 'plotly_click' feuert nur bei Klicks auf
  // Datenpunkte, nicht im Achsenbereich darunter. Die Woche wird daher
  // direkt aus der Klick-Pixelposition berechnet (ueber die interne
  // Pixel-zu-Daten-Umrechnung der x-Achse), aber NUR fuer Klicks unterhalb
  // der eigentlichen Zeichenflaeche (sonst wuerde ein Klick zum Zoomen in
  // der Grafik selbst versehentlich auch die Woche aendern).
  el.addEventListener('click', function(evt) {
    var fl = el._fullLayout;
    if (!fl || !fl.xaxis || !fl._size) return;
    var rect = el.getBoundingClientRect();
    var yPixel = evt.clientY - rect.top;
    var plotBottom = fl._size.t + fl._size.h;
    if (yPixel < plotBottom) return;
    var xPixel = evt.clientX - rect.left;
    var xData = fl.xaxis.p2d(xPixel - fl.xaxis._offset);
    var woche = Math.max(1, Math.min(maxWocheFuerJahr(selectedYear), Math.round(xData)));
    selectedWeek = woche;
    if (sliderInput) sliderInput.value = String(woche);
    applyMapState();
  });

  applyState();
  // Erneuter Aufruf leicht verzoegert: beim allerersten applyState() oben
  // (synchron waehrend des Bindens DIESES Widgets) sind die beiden
  // Karten-Widgets moeglicherweise noch nicht gebunden (siehe Kommentar in
  // applyMapState()), wodurch applyMapState() dort ins Leere laeuft. Nach
  // 100ms sind alle drei Widgets garantiert initialisiert.
  setTimeout(applyState, 100);
}
"

js_ersetzungen <- list(
  "__ALLE_JAHRE__" = jsonlite::toJSON(alle_jahre),
  "__NEUESTES_JAHR__" = jsonlite::toJSON(neuestes_jahr, auto_unbox = TRUE),
  "__JAHRE_MIT_NIEDERSCHLAG__" = jsonlite::toJSON(jahre_mit_niederschlag),
  "__N_SITE_GROWTH__" = as.character(length(site_growth_meta)),
  "__N_GROUP_GROWTH__" = as.character(length(group_growth_meta)),
  "__N_SITE_PRECIP__" = as.character(length(site_precip_meta)),
  "__N_GROUP_PRECIP__" = as.character(length(group_precip_meta)),
  "__STANDARD_KURVE_TRACE_IDX__" = as.character(standard_kurve_trace_idx),
  "__SITE_GROWTH_META__" = jsonlite::toJSON(site_growth_meta, auto_unbox = TRUE),
  "__GROUP_GROWTH_META__" = jsonlite::toJSON(group_growth_meta, auto_unbox = TRUE),
  "__SITE_PRECIP_META__" = jsonlite::toJSON(site_precip_meta, auto_unbox = TRUE),
  "__GROUP_PRECIP_META__" = jsonlite::toJSON(group_precip_meta, auto_unbox = TRUE),
  "__GROUP_LABELS__" = jsonlite::toJSON(gruppen_labels),
  "__SITE_NAMES__" = jsonlite::toJSON(alle_orte),
  "__SITE_VISIBLE__" = jsonlite::toJSON(site_sichtbar_je_gruppe),
  "__WOCHEN_TICKVALS__" = jsonlite::toJSON(wochen_tickvals),
  "__DATUM_TICKTEXT_JE_JAHR__" = jsonlite::toJSON(datum_ticktext_je_jahr, auto_unbox = TRUE),
  "__WOCHEN_DATUM_BEREICH__" = jsonlite::toJSON(wochen_datum_bereich_je_woche, auto_unbox = TRUE),
  "__SITE_COLORS__" = jsonlite::toJSON(site_farben_je_ort),
  "__MAP_WOCHEN__" = jsonlite::toJSON(map_wochen, auto_unbox = TRUE),
  "__MAP_POINT_ORTS__" = jsonlite::toJSON(map_point_orts),
  # Nur der schlanke Verfuegbarkeits-Index (Jahr/Woche-Schluessel je Ebene) -
  # die eigentlichen Bilder/Werte-Gitter liegen in outputs/ebenen/*.json und
  # werden erst bei Bedarf per fetch() nachgeladen (siehe schreibe_ebene_
  # datei() oben und ladeEbene() im js_template).
  "__EBENEN_SCHLUESSEL__" = jsonlite::toJSON(ebenen_schluessel, auto_unbox = TRUE),
  "__SMN_STATIONEN_TRACE_IDX__" = as.character(smn_stationen_trace_idx),
  "__SMN_STATIONEN_META__" = jsonlite::toJSON(smn_stationen_meta_liste, auto_unbox = TRUE),
  "__SMN_BASIS_URL__" = jsonlite::toJSON(smn_basis_url, auto_unbox = TRUE),
  "__LAYER_LEGENDEN__" = jsonlite::toJSON(layer_legenden, auto_unbox = TRUE),
  "__GRASWACHSTUM_BILDER__" = jsonlite::toJSON(graswachstum_bild_je_woche, auto_unbox = TRUE),
  "__AFC_RING_BILDER__" = jsonlite::toJSON(afc_ring_bild_je_woche, auto_unbox = TRUE),
  "__AFC_FENSTER_JE_WOCHE__" = jsonlite::toJSON(afc_fenster_je_woche, auto_unbox = TRUE),
  "__AFC_VERLAEUFE__" = jsonlite::toJSON(afc_verlaeufe_je_fenster, auto_unbox = TRUE),
  "__KARTENBILD_HINTERGRUND__" = jsonlite::toJSON(kartenbild_hintergrund, auto_unbox = TRUE),
  "__HEUTIGE_WOCHE__" = as.character(heutige_woche),
  "__START_WOCHE__" = as.character(start_woche)
)
js_code <- js_template
for (token in names(js_ersetzungen)) {
  js_code <- gsub(token, js_ersetzungen[[token]], js_code, fixed = TRUE)
}

fig_kurve <- htmlwidgets::onRender(fig_kurve, js_code)

########################################################################
## 5. Seite zusammensetzen und speichern -------------------------------
########################################################################

seite <- htmltools::tagList(
  # OHNE viewport-Meta-Tag rendern mobile Browser die Seite auf einem
  # virtuellen Desktop-Layout-Viewport (typischerweise ~980px) und skalieren
  # sie nur optisch herunter - die @media(max-width:700px)-Regeln (siehe
  # oben, gw-chart-row etc.) wuerden dadurch NIE greifen, selbst auf einem
  # echten Telefon.
  htmltools::tags$head(
    htmltools::tags$title(paste0("Datenexplorer Graswachstum")),
    htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
  ),
  htmltools::div(style = "font-family: sans-serif; max-width: 1400px; margin: 0 auto; padding: 20px;",
    htmltools::div(style = "display: flex; gap: 20px; flex-wrap: wrap; align-items: flex-start;",
      htmltools::div(id = "datenexplorer-growthmap", style = "flex: 1 1 700px; min-width: 320px; height: 560px; overflow: hidden;", fig_wachstum),
      # flex-grow:1 (statt 0) statt einer festen 220px-Box: faellt die
      # Ebenen-Box auf einem schmalen (Mobile-)Bildschirm per flex-wrap in
      # eine eigene Zeile, fuellt sie so deren volle Breite aus, statt
      # nutzlosen Leerraum daneben zu lassen; neben der Karte (genug Platz)
      # bleibt sie effektiv bei ihrer min-width von 220px.
      htmltools::div(id = "datenexplorer-map-controls", style = "flex: 1 1 220px; min-width: 220px;")
    ),
    htmltools::div(id = "datenexplorer-slider"),
    fig_kurve
  )
)

# Unter "Datenexplorer_app.html" statt "Datenexplorer.html" gespeichert: die
# eigentliche interaktive App wird jetzt per Klick aus einer schlanken
# statischen Vorschauseite nachgeladen (siehe unten) - "Datenexplorer.html"
# ist ab jetzt diese Vorschauseite, nicht mehr die App selbst. Bestehende
# Links/Einbettungen auf "Datenexplorer.html" (der oeffentliche Name)
# bleiben dadurch gueltig UND laden beim ersten Aufruf nur noch die paar KB
# der Vorschau statt der vollen ~7.6MB App.
datenexplorer_app_datei <- file.path(out_dir, "Datenexplorer_app.html")
htmltools::save_html(seite, datenexplorer_app_datei)
cat("Datenexplorer-App gespeichert in:", datenexplorer_app_datei, "\n")

########################################################################
## 5. Statische Vorschauseite (Klick-zum-Laden) ------------------------
##    Zeigt standardmaessig nur die aktuellsten statischen SVGs (Karte +
##    Kurve, aus 21_plot_map.R/22_plot_year.R, "_aktuell"-Kopien) - klein
##    und schnell fuer Besucher, die nur den aktuellen Stand sehen wollen.
##    Erst ein Klick laedt die volle interaktive App (Datenexplorer_app.html)
##    per <iframe> nach, dessen Hoehe sich per ResizeObserver automatisch an
##    den tatsaechlichen Inhalt anpasst (kein Innen-Scrollbalken).
########################################################################
vorschau_html <- '
<!doctype html>
<html lang="de">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Datenexplorer Graswachstum</title>
<style>
  body { font-family: sans-serif; max-width: 1000px; margin: 0 auto; padding: 20px; color: #222; }
  h1 { font-size: 20px; margin: 0 0 4px; }
  .gw-vorschau-hinweis { color: #555; font-size: 14px; margin: 0 0 16px; }
  .gw-vorschau-bilder { display: block; border: none; background: none; padding: 0; margin: 0; width: 100%; text-align: left; cursor: pointer; }
  .gw-vorschau-bilder img { width: 100%; display: block; margin-bottom: 14px; border: 1px solid #ddd; border-radius: 6px; transition: opacity .15s; }
  .gw-vorschau-bilder:hover img { opacity: 0.88; }
  .gw-vorschau-knopf { display: inline-block; margin-top: 4px; padding: 10px 18px; background: #2b6cb0; color: white; border: none;
                       border-radius: 6px; font-size: 15px; cursor: pointer; }
  .gw-vorschau-knopf:hover { background: #235a92; }
  #gw-app-frame { width: 100%; border: none; display: block; }
  [hidden] { display: none !important; }
</style>
</head>
<body>
  <h1>Datenexplorer Graswachstum</h1>
  <p class="gw-vorschau-hinweis">Aktuellster Stand als statische Ansicht. Fuer Kalenderwochen-Verlauf, Standort-Filter und Hintergrund-Ebenen die interaktive Version oeffnen.</p>
  <div id="gw-vorschau">
    <button type=button class="gw-vorschau-bilder" id="gw-oeffnen-karte" title="Interaktive Version oeffnen">
      <img src="Graswachstumskarte_aktuell.svg" alt="Aktuelle Graswachstumskarte">
      <img src="Graswachstumskurve_aktuell.svg" alt="Aktuelle Graswachstumskurve">
    </button>
    <button type=button class="gw-vorschau-knopf" id="gw-oeffnen-knopf">Interaktive Version oeffnen</button>
  </div>
  <script>
    function ladeApp() {
      var vorschau = document.getElementById("gw-vorschau");
      var frame = document.createElement("iframe");
      frame.id = "gw-app-frame";
      frame.src = "Datenexplorer_app.html";
      frame.height = "800";
      frame.addEventListener("load", function() {
        function anpassen() {
          try {
            var doc = frame.contentDocument;
            if (doc && doc.documentElement) frame.style.height = doc.documentElement.scrollHeight + "px";
          } catch (e) {}
        }
        anpassen();
        try {
          new ResizeObserver(anpassen).observe(frame.contentDocument.body);
        } catch (e) {
          setInterval(anpassen, 1000);
        }
      });
      vorschau.replaceWith(frame);
    }
    document.getElementById("gw-oeffnen-karte").addEventListener("click", ladeApp);
    document.getElementById("gw-oeffnen-knopf").addEventListener("click", ladeApp);
  </script>
</body>
</html>
'
writeLines(vorschau_html, file.path(out_dir, "Datenexplorer.html"))
cat("Datenexplorer-Vorschau gespeichert in:", file.path(out_dir, "Datenexplorer.html"), "\n")
