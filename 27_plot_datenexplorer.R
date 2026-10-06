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
# Niederschlag/Temperatur/Sonnenschein/ET0/Bodenwasserbilanz: alle direkt aus
# MeteoSchweiz-Open-Data selbst heruntergeladen und (Wasserhaushalt) selbst
# berechnet - keine Abhaengigkeit mehr auf ein separates Nachbarprojekt.
# Nur fuer Jahre mit tatsaechlich vorhandenen Rasterdaten verfuegbar - fuer
# andere Jahre sind die entsprechenden Umschalter deaktiviert (ausgegraut).
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

# Alle Pfade per Umgebungsvariable ueberschreibbar (Default = bisheriges
# Verhalten fuer den lokalen/Laptop-Gebrauch unveraendert) - im Docker-
# Container zeigen sie stattdessen auf ein persistentes Volume, siehe
# docker-compose.yml.
out_dir <- Sys.getenv("GRASSGROWTH_OUT_DIR", "outputs")
geodata_dir <- Sys.getenv("GRASSGROWTH_GEODATA_DIR", "../geodata/meteoschweiz")

########################################################################
## Persistenter Cache fuer Karten-/Ebenen-Bilder ABGESCHLOSSENER
## (vergangener) Jahre --------------------------------------------------
## Ohne diesen Cache rendert JEDER Lauf saemtliche Kartenschnappschuesse,
## AFC-Ringe und Meteo-Hintergrundebenen fuer ALLE Jahre komplett neu -
## auch fuer laengst abgeschlossene Jahre, deren Rohdaten sich nie mehr
## aendern (AGFF-Sheet-Historie, RhiresD/TabsD-Endprodukte). Das war mit
## Abstand der groesste Zeitkostenfaktor des Skripts. "Abgeschlossen" =
## Jahr < aktuelles Kalenderjahr: NUR das laufende Jahr wird bei jedem Lauf
## neu berechnet (dort aendert sich taeglich etwas: neue Messungen, die
## aktuelle Woche, "prelim"-Werte die spaeter durch finale ersetzt werden).
## Liegt NEBEN outputs/ (nicht darin) und wird NICHT committet - der Name
## endet bewusst auf "_cache" (.gitignore hat dafuer schon eine Regel) - rein
## lokaler Performance-Cache, kein Teil des veroeffentlichten Standes.
ebenen_cache_dir <- Sys.getenv("GRASSGROWTH_EBENEN_CACHE_DIR", "ebenen_cache")
dir.create(ebenen_cache_dir, recursive = TRUE, showWarnings = FALSE)
aktuelles_kalenderjahr <- format(Sys.Date(), "%Y")

lade_ebenen_cache <- function(name) {
  datei <- file.path(ebenen_cache_dir, paste0(name, ".rds"))
  if (file.exists(datei)) tryCatch(readRDS(datei), error = function(e) list()) else list()
}
speichere_ebenen_cache <- function(name, cache) {
  saveRDS(cache, file.path(ebenen_cache_dir, paste0(name, ".rds")))
}
# Fuer die drei Bild-Ebenen ausserhalb von baue_fenster_ebenen() (siehe
# dort fuer die aufwendigere Variante, die bei einem komplett gecachten
# abgeschlossenen Jahr zusaetzlich auch dessen Rohraster gar nicht erst
# laedt): liefert den gecachten Eintrag nur fuer ein abgeschlossenes Jahr,
# sonst NULL (immer frisch berechnen).
cache_eintrag_holen <- function(cache, jahr, woche) {
  if (jahr >= aktuelles_kalenderjahr) return(NULL)
  cache[[paste(jahr, woche)]]
}

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
elevation_cache_file <- Sys.getenv("GRASSGROWTH_ELEVATION_CACHE_FILE", "standorte_elevation.csv")
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

## Gemeinsame MeteoSchweiz-Rasterdaten (Niederschlag, Temperatur 2m/Max/Min,
## Sonnenscheindauer) -------------------------------------------------------
## FRUEHER lasen Niederschlag/Temperatur NUR aus einem von einem FREMDEN,
## separaten Projekt (r-futterbaugutachten) befuellten Cache-Ordner, ohne
## selbst jemals etwas herunterzuladen - lief jenes Projekt eine Weile nicht,
## blieben hier klaglos Wochen ohne Daten (auf der Karte nicht von echten
## MeteoSchweiz-Verzoegerungen zu unterscheiden). Jetzt eigenstaendig: alle
## fuenf Groessen kommen aus DERSELBEN STAC-Collection/denselben Monats-Items
## (ch.meteoschweiz.ogd-surface-derived-grid) - ein Set von Downloadfunktionen
## bedient sie gemeinsam (ein API-Aufruf pro Monat/Tag liefert alle Varianten
## auf einmal, statt fuenf getrennte).
meteo_stac_base <- "https://data.geo.admin.ch/api/stac/v1/collections/ch.meteoschweiz.ogd-surface-derived-grid/items/"
# Produktcode je Variable UND Zeitraster - Niederschlag heisst im
# konsolidierten Monats-Item anders (rhiresd, endgueltig geprueft) als im
# taeglichen Item des noch laufenden Monats (rprelimd, vorlaeufig) -
# Temperatur/Sonnenschein behalten in beiden Faellen denselben Produktcode.
meteo_produktcode_monat <- c(precip = "rhiresd", tabs = "tabsd", tmax = "tmaxd", tmin = "tmind", sreld = "sreld")
meteo_produktcode_tag <- c(precip = "rprelimd", tabs = "tabsd", tmax = "tmaxd", tmin = "tmind", sreld = "sreld")

meteo_datei <- function(prodcode, zeitschluessel) file.path(geodata_dir, paste0(prodcode, "_", zeitschluessel, ".nc"))

# Laedt (nur was lokal fehlt) die angegebenen Variablen fuer EIN STAC-Item
# (Monat "YYYYMM" ODER Tag "YYYYMMDD") - ein gemeinsamer API-Aufruf liefert
# alle Variablen-Assets dieses Items auf einmal.
lade_meteo_item <- function(item_id, zeitschluessel, variablen, produktcodes) {
  ziel_dateien <- meteo_datei(produktcodes[variablen], zeitschluessel)
  fehlend <- variablen[!file.exists(ziel_dateien)]
  if (length(fehlend) == 0) return(invisible(TRUE))
  item <- tryCatch(jsonlite::fromJSON(paste0(meteo_stac_base, item_id), simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(item) || length(item$assets) == 0) return(invisible(FALSE))
  for (v in fehlend) {
    prodcode <- produktcodes[[v]]
    key <- names(item$assets)[grepl(paste0("\\.", prodcode, "_"), names(item$assets))]
    if (length(key) == 0) next
    dest <- meteo_datei(prodcode, zeitschluessel)
    tryCatch(download.file(item$assets[[key[1]]]$href, destfile = dest, quiet = TRUE, mode = "wb"),
             error = function(e) unlink(dest))
  }
  invisible(TRUE)
}
lade_meteo_monat <- function(jahr_monat, variablen) lade_meteo_item(paste0(jahr_monat, "-ch"), jahr_monat, variablen, meteo_produktcode_monat)
lade_meteo_tag <- function(tag, variablen) {
  tag_id <- gsub("-", "", as.character(tag))
  lade_meteo_item(paste0(tag_id, "-ch"), tag_id, variablen, meteo_produktcode_tag)
}
meteo_monat_konsolidiert_vorhanden <- function(jahr_monat) file.exists(meteo_datei(meteo_produktcode_monat[["precip"]], jahr_monat))

# Konsolidierte Monats-Dateien fuer ALLE Jahre/Monate bis heute (einmal
# lokal vorhanden, nie erneut heruntergeladen - reiner file.exists()-Check).
meteo_variablen_gesamt <- c("precip", "tabs", "tmax", "tmin", "sreld")
for (jr in alle_jahre) {
  monate_konsolidiert <- sprintf("%s%02d", jr, 1:12)
  for (jahr_monat in monate_konsolidiert) {
    if (as.Date(paste0(jahr_monat, "01"), format = "%Y%m%d") > Sys.Date()) next
    lade_meteo_monat(jahr_monat, meteo_variablen_gesamt)
  }
}
# Fuer die letzten zwei Monate (laufender + Vormonat) zusaetzlich TAEGLICH
# nachladen, WENN das konsolidierte Monats-Item (noch) fehlt - dessen
# Veroeffentlichung hinkt dem Monatsende typischerweise ein paar Tage
# hinterher. Sonnenscheindauer hat KEINE taegliche/vorlaeufige Variante
# (siehe meteo_produktcode_tag) - fuer sie bleiben diese Tage bis zur
# konsolidierten Monatsdatei unverfuegbar, das ist normal (kein Fehler).
kandidaten_monate <- unique(format(seq(as.Date(format(Sys.Date(), "%Y-%m-01")), by = "-1 month", length.out = 2), "%Y%m"))
for (jahr_monat in kandidaten_monate) {
  if (meteo_monat_konsolidiert_vorhanden(jahr_monat)) next
  monatsanfang <- as.Date(paste0(jahr_monat, "01"), format = "%Y%m%d")
  monatsende <- seq(monatsanfang, by = "month", length.out = 2)[2] - 1
  tage_ende <- min(monatsende, Sys.Date() - 1)
  # Am 1. eines Monats liegt Sys.Date()-1 noch im VORMONAT, also vor
  # monatsanfang des gerade erst begonnenen Monats - dann gibt es fuer
  # diesen Monat schlicht noch keine Tage zum Nachladen (sonst wuerde
  # seq() mit einem Enddatum vor dem Startdatum abstuerzen).
  if (tage_ende >= monatsanfang) {
    tage <- seq(monatsanfang, tage_ende, by = "day")
    for (tag in as.character(tage)) lade_meteo_tag(tag, setdiff(meteo_variablen_gesamt, "sreld"))
  }
}

# Liest (rein lokal, kein Download mehr) alle vorhandenen Dateien einer
# Variable fuer ein Jahr zu EINEM Rasterstapel zusammen - konsolidierte
# Monats-Dateien plus (fuer die letzten zwei Monate) etwaige Tages-Dateien.
lade_jahresstapel <- function(variable, jr, produktcode_monat, produktcode_tag = NULL) {
  monate_konsolidiert <- sprintf("%s%02d", jr, 1:12)
  dateien <- meteo_datei(produktcode_monat, monate_konsolidiert)
  teile <- lapply(dateien[file.exists(dateien)], rast)
  if (!is.null(produktcode_tag)) {
    tage_dateien <- Sys.glob(meteo_datei(produktcode_tag, paste0(jr, "[0-9][0-9][0-9][0-9]"))) # jr(YYYY) + MMDD
    teile <- c(teile, lapply(tage_dateien, rast))
  }
  if (length(teile) == 0) return(NULL)
  rast(teile)
}

## Niederschlag: nur fuer Jahre mit lokal vorhandenen MeteoSchweiz-Rasterdaten.
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
for (jr in jahre_mit_niederschlag) {
  precip_alle <- lade_jahresstapel("precip", jr, meteo_produktcode_monat[["precip"]], meteo_produktcode_tag[["precip"]])
  if (is.null(precip_alle)) next
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

## Temperatur 2m (Mittel/Max/Min): nur fuer Jahre mit lokal vorhandenen
## TabsD-Rasterdaten. Max/Min werden nur fuers Hargreaves-ET0 (siehe
## Wasserhaushalt-Abschnitt weiter unten) gebraucht, nicht fuer die
## Temperatur-Ebene selbst.
jahr_hat_temperatur <- function(jr) any(grepl(paste0("^tabsd_", jr), list.files(geodata_dir)))
jahre_mit_temperatur <- alle_jahre[vapply(alle_jahre, jahr_hat_temperatur, logical(1))]
cat("Temperaturdaten lokal vorhanden fuer:", paste(jahre_mit_temperatur, collapse = ", "), "\n")

temperatur_raster_je_jahr <- list()
tmax_raster_je_jahr <- list()
tmin_raster_je_jahr <- list()
for (jr in jahre_mit_temperatur) {
  temperatur_raster_je_jahr[[jr]] <- lade_jahresstapel("tabs", jr, "tabsd", "tabsd")
  tmax_raster_je_jahr[[jr]] <- lade_jahresstapel("tmax", jr, "tmaxd", "tmaxd")
  tmin_raster_je_jahr[[jr]] <- lade_jahresstapel("tmin", jr, "tmind", "tmind")
  tage_temp <- as.Date(time(temperatur_raster_je_jahr[[jr]]))
  cat("Temperatur", jr, "geladen:", format(min(tage_temp), "%d.%m.%Y"), "-", format(max(tage_temp), "%d.%m.%Y"), "\n")
}

## Sonnenscheindauer (relativ, SrelD): KEINE taegliche/vorlaeufige Variante
## vorhanden (siehe meteo_produktcode_tag) - fuer die letzten 1-2 Monate
## fehlen die Daten deshalb oft noch (die betroffenen Wochen werden wie bei
## fehlenden Niederschlagsdaten automatisch als nicht verfuegbar behandelt).
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
  # Als reine Zahlenmatrix (Zelle x Tag) kumulieren, NICHT schichtweise per
  # [[i]]<- in ein SpatRaster - Letzteres kopiert (v.a. in aelteren terra-
  # Versionen) bei JEDER Einzelzuweisung moeglicherweise das gesamte,
  # wachsende Mehrschicht-Objekt neu, was bei einem vollen Jahr (~365
  # Iterationen) zu einem Speicher-Absturz fuehrte (siehe Docker-Testlauf).
  gdd_taeglich_mat <- terra::values(clamp(r_jahr - 5, lower = 0))
  gdd_kum_mat <- gdd_taeglich_mat
  for (i in seq_len(ncol(gdd_kum_mat))[-1]) gdd_kum_mat[, i] <- gdd_kum_mat[, i - 1] + gdd_taeglich_mat[, i]
  gdd_kum <- rast(r_jahr)
  values(gdd_kum) <- gdd_kum_mat
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
  # Wie beim Kartentitel oben: Datum der Aufbereitung (R-Lauf) IMMER
  # sichtbar direkt auf der Grafik, unabhaengig von Standort-/Jahresauswahl.
  title = list(text = paste0("Graswachstumskurve — Stand: ", strftime(today, format = "%d.%m.%Y")), font = list(size = 16)),
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
  # Kennzahlen fuer das Standortblatt (JS: zeigeStandortBlatt()), mit | getrennt
  snap$blatt <- paste(snap$place, if ("masl" %in% names(snap)) round(snap$masl) else "",
                      round(snap$wert, 0), ifelse(is.na(snap$afc), "", round(snap$afc, 0)),
                      format(snap$date, "%d.%m.%Y"), snap$daysold, sep = "|")
  fig %>% add_trace(
    data = snap, x = ~lon, y = ~lat, type = "scatter", mode = "markers",
    # showscale = FALSE: die "Tage seit Messung"-Legende ist jetzt eine
    # eigene HTML-Box im Ebenen-Kasten (JS: aktualisiereTageSeitMessungLegende())
    # statt des Plotly-nativen Colorbars hier - Letzterer kollidierte auf der
    # Karte mit dem (nur bei Hover sichtbaren) Modebar-Bereich oben rechts.
    marker = list(size = 30, color = ~daysold, colorscale = list(list(0, "white"), list(1, "#757575")),
                  cmin = 0, cmax = 14, showscale = FALSE, opacity = 0),
    hovertext = ~hover, hoverinfo = "text", customdata = ~blatt,
    hoverlabel = list(bgcolor = ~daysold_col, font = list(color = "black")),
    showlegend = FALSE, visible = FALSE, name = titel
  )
}

fig_wachstum <- plot_ly(height = 560)
map_point_orts <- list()
graswachstum_bild_je_woche <- list()
afc_ring_bild_je_woche <- list()
afc_fenster_je_woche <- list()

graswachstum_afc_cache_alt <- lade_ebenen_cache("graswachstum_afc")
graswachstum_afc_cache_neu <- list()
ga_aus_cache <- 0L; ga_neu <- 0L
for (i in seq_len(nrow(map_wochen))) {
  jr <- map_wochen$jahr[i]; w <- map_wochen$week[i]
  schluessel <- paste(jr, w)
  snap <- map_snapshots %>% filter(jahr == jr, week == w) %>% arrange(Ort)
  # Die Plotly-Trace (Standort-Positionen/-Werte dieser Woche) wird IMMER neu
  # angelegt - billig (keine ggplot-Rendering), und rein strukturell Teil der
  # interaktiven Figur. Nur die beiden teuren ggplot-Bilder darunter
  # (baue_graswachstum_bild()/baue_afc_ring_bild()) werden fuer ein
  # abgeschlossenes Jahr aus dem Cache uebernommen statt neu gerendert.
  fig_wachstum <- baue_kartenwerte_trace(fig_wachstum, snap, "growth", "kg TS/ha/Tag",
                                          paste("Wachstum", jr, "KW", w))
  map_point_orts[[i]] <- as.character(snap$Ort)
  cached <- cache_eintrag_holen(graswachstum_afc_cache_alt, jr, w)
  if (!is.null(cached)) {
    graswachstum_bild_je_woche[[schluessel]] <- cached$graswachstum
    afc_ring_bild_je_woche[[schluessel]] <- cached$afc_bild
    afc_fenster_je_woche[[schluessel]] <- cached$afc_fenster
    ga_aus_cache <- ga_aus_cache + 1L
  } else {
    graswachstum_bild_je_woche[[schluessel]] <- baue_graswachstum_bild(snap)
    afc_ergebnis <- baue_afc_ring_bild(snap, montag_von_woche(jr, w))
    afc_ring_bild_je_woche[[schluessel]] <- afc_ergebnis$bild
    afc_fenster_je_woche[[schluessel]] <- afc_ergebnis$fensterIdx
    ga_neu <- ga_neu + 1L
  }
  graswachstum_afc_cache_neu[[schluessel]] <- list(
    graswachstum = graswachstum_bild_je_woche[[schluessel]],
    afc_bild = afc_ring_bild_je_woche[[schluessel]],
    afc_fenster = afc_fenster_je_woche[[schluessel]]
  )
}
speichere_ebenen_cache("graswachstum_afc", graswachstum_afc_cache_neu)
cat("Graswachstums-Hintergrundbilder erzeugt:", sum(!vapply(graswachstum_bild_je_woche, is.null, logical(1))),
    "(aus Cache:", ga_aus_cache, "/ neu:", ga_neu, ")\n")
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
  # Nur ein Platzhalter-Anfangstitel - JS (aktualisiereKartentitel()) macht
  # ihn sofort dynamisch: zeigt IMMER, was gerade zu sehen ist (Kalenderwoche
  # UND, falls eine Meteo-Ebene aktiv ist, deren Name + tatsaechliches
  # Datenstand-Datum) statt eines statischen Build-Datums, das mit der
  # Aktualitaet der einzelnen Ebenen nichts zu tun haben muss.
  title = list(text = paste0("<b>Graswachstum</b><br><span style='font-size:12px'>KW ", start_woche, " ", neuestes_jahr,
                             " · Grafik vom ", format(Sys.Date(), "%d.%m.%Y"), "</span>"), font = list(size = 16)),
  xaxis = list(visible = FALSE, range = lon_range_erweitert, fixedrange = FALSE),
  yaxis = list(visible = FALSE, range = lat_range, scaleanchor = "x", scaleratio = karten_scaleratio),
  margin = list(t = 58, b = 10, l = 10, r = 10),
  images = list(kartenbild_hintergrund)
) %>% config(responsive = FALSE, scrollZoom = TRUE, displayModeBar = FALSE)
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

  // 'Ganze Schweiz'-Ansicht (x-/y-Achsenbereich) fuer eine gegebene
  // Containergroesse - x bleibt immer auf dem vollen lon_range_erweitert
  // (nutzt die volle Breite), y wird so berechnet, dass bei diesem
  // Seitenverhaeltnis exakt keine Rand-Leerflaeche entsteht (weder
  // gestaucht noch gestreckt). Wird unten sowohl fuer die initiale/
  // Resize-Ansicht als auch fuer die Zoom-Sperre und den 'Ganze Schweiz'-
  // Knopf gebraucht - deshalb als eigene Funktion statt nur inline fuer
  // den Mobile-Fall (wie zuvor).
  function vollAnsichtBerechnen(breite, hoehe) {
    // Mobile: kein Plotly-Titel (Kopfzeile ist HTML), Rand oben nur 6px.
    var plotBreite = breite - 20, plotHoehe = hoehe - (breite < 700 ? 16 : 50);
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
    var hoehe = mobil ? Math.round(Math.max(200, (breite - 20) * 0.65 + 16)) : 560;
    var voll = vollAnsichtBerechnen(breite, hoehe);
    vollX = voll.x; vollY = voll.y;
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
", lon_range_erweitert[1], lon_range_erweitert[2], mean(lat_range), karten_scaleratio))

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
baue_fenster_ebenen <- function(name, jahre, raster_holen, aggregat, farben, bereich_je_7tage, stuetzstellen = NULL) {
  ergebnis <- list()
  gesamt_cache <- 0L; gesamt_neu <- 0L
  for (fenster in fenstergroessen_tage) {
    fenster_key <- as.character(fenster)
    cache_name <- paste0(name, "_", fenster_key)
    cache_alt <- lade_ebenen_cache(cache_name)
    bild_je_woche <- list()
    werte_je_woche <- list()
    bereich <- if (aggregat == "summe") bereich_je_7tage * fenster / 7 else bereich_je_7tage
    for (jr in jahre) {
      # Abgeschlossenes Jahr, das schon einmal (mit dieser Fenstergroesse)
      # verarbeitet wurde: komplett aus dem Cache uebernehmen - das Rohraster
      # (raster_holen(jr), oft das teuerste an dieser Stelle: Laden +
      # Reprojizieren mehrerer Monats-Dateien) wird dafuer gar nicht erst
      # angefasst.
      alte_schluessel_jahr <- Filter(function(k) startsWith(k, paste0(jr, " ")), names(cache_alt))
      if (jr < aktuelles_kalenderjahr && length(alte_schluessel_jahr) > 0) {
        for (schluessel in alte_schluessel_jahr) {
          bild_je_woche[[schluessel]] <- cache_alt[[schluessel]]$bild
          werte_je_woche[[schluessel]] <- cache_alt[[schluessel]]$werte
        }
        gesamt_cache <- gesamt_cache + length(alte_schluessel_jahr)
        next
      }
      r_info <- raster_holen(jr)
      if (is.null(r_info)) next
      tage_r <- r_info$tage
      for (w in alle_wochen) {
        # Noch nicht begonnene (zukuenftige) Wochen NIE anzeigen - sonst
        # wuerde die Kappung unten (fuer die AKTUELLE Woche gedacht) bei
        # laengeren Fenstern (z.B. 28 Tage) faelschlich auch fuer Wochen
        # weit in der Zukunft noch eine (dann voellig veraltete) Ueberlappung
        # finden und deren Daten faelschlich dieser Zukunftswoche zuordnen.
        if (montag_von_woche(jr, w) > Sys.Date() + 1) next
        fenster_ende_ideal <- montag_von_woche(jr, w) - 1
        fenster_start <- fenster_ende_ideal - (fenster - 1)
        if (fenster_start < min(tage_r)) next
        # Fuer die AKTUELLSTE Woche reicht das ideale Fensterende oft noch
        # nicht (Publikationsverzoegerung der Rohdaten) - statt die Woche
        # komplett auszulassen, wird das Fenster auf das tatsaechlich
        # verfuegbare Enddatum gekappt (kuerzeres Fenster, aber so aktuell
        # wie moeglich - "ab Dienstag Stand Montag" statt "erst wieder
        # naechste Woche"). Nur wenn ueberhaupt noch eine sinnvolle
        # Ueberlappung besteht (fenster_ende >= fenster_start).
        fenster_ende <- min(fenster_ende_ideal, max(tage_r))
        if (fenster_ende < fenster_start) next
        idx <- which(tage_r >= fenster_start & tage_r <= fenster_ende)
        if (length(idx) == 0) next
        r_wert <- if (aggregat == "summe") {
          clamp(sum(r_info$raster[[idx]], na.rm = TRUE), lower = 0)
        } else {
          mean(r_info$raster[[idx]], na.rm = TRUE)
        }
        bild_ergebnis <- raster_zu_datauri(r_wert, farben, bereich, stuetzstellen = stuetzstellen)
        # bis/tage: tatsaechlich verwendetes Fensterende und Tagesanzahl -
        # bei Kappung (siehe oben) kuerzer als die nominelle Fenstergroesse.
        # Fuer die ehrliche Datenstand-Anzeige im Kartentitel (JS).
        bild_ergebnis$werte$bis <- format(fenster_ende, "%d.%m.%Y")
        bild_ergebnis$werte$tage <- length(idx)
        schluessel <- paste(jr, w)
        bild_je_woche[[schluessel]] <- bild_ergebnis$bild
        werte_je_woche[[schluessel]] <- bild_ergebnis$werte
        gesamt_neu <- gesamt_neu + 1L
      }
    }
    # Cache fuer den naechsten Lauf aktualisieren - enthaelt jetzt sowohl
    # unveraendert uebernommene (abgeschlossene Jahre) als auch frisch
    # berechnete Eintraege (laufendes bzw. erstmals verarbeitetes Jahr).
    speichere_ebenen_cache(cache_name, Map(function(b, w) list(bild = b, werte = w), bild_je_woche, werte_je_woche))
    ergebnis[[fenster_key]] <- list(bilder = bild_je_woche, werte = werte_je_woche)
  }
  cat("  ", name, "- aus Cache:", gesamt_cache, "/ neu berechnet:", gesamt_neu, "\n")
  ergebnis
}

## Niederschlag: gleitendes Fenster (Summe) --------------------------------
niederschlag_fenster_ergebnisse <- baue_fenster_ebenen(
  "niederschlag", jahre_mit_niederschlag,
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
  "temperatur", jahre_mit_temperatur,
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
  "bodentemperatur", jahre_mit_temperatur,
  function(jr) {
    if (!jr %in% names(temperatur_raster_je_jahr)) return(NULL)
    r <- temperatur_raster_je_jahr[[jr]]
    list(raster = r, tage = as.Date(time(r)))
  },
  aggregat = "mittel", farben = temperatur_farben, bereich_je_7tage = c(0, 30)
)
cat("Bodentemperatur-Hintergrundbilder erzeugt (Schaetzung):",
    sum(vapply(bodentemperatur_fenster_ergebnisse, function(x) length(x$bilder), integer(1))), "\n")

## Wasserhaushalt (Bucket-Modell: Niederschlag - potenzielle Verdunstung) --
## Frueher aus dem separaten Projekt r-futterbaugutachten uebernommen (dort
## 44_wasserhaushalt_meteoschweiz.R) - jetzt direkt hier berechnet, da es NUR
## die ohnehin schon geladenen MeteoSchweiz-Rasterdaten braucht (kein
## Hoehenmodell, keine Kantonsdaten) - entfernt die letzte Abhaengigkeit auf
## ein fremdes, separat laufendes Projekt.
##
## ET0 nach Hargreaves (FAO-56): ET0 = 0.0023*(Tmean+17.8)*sqrt(Tmax-Tmin)*Ra,
## mit Ra = rein astronomisch (Breitengrad, Tag im Jahr) berechneter
## extraterrestrischer Strahlung (FAO Irrigation and Drainage Paper 56,
## Allen et al. 1998, Gleichungen 21-25) - keine Messgroesse ausser
## Temperatur noetig.
##
## Bucket-Modell Bodenwasserspeicher: S(t) = clamp(S(t-1) + Niederschlag(t)
## - ET0(t), 0, Smax). Smax = 100mm: grobe, gaengige Annahme fuer die
## pflanzenverfuegbare Feldkapazitaet von Gruenlandboeden im Mittelland
## (nicht kalibriert, einfach anpassbar). Start S = Smax am 1. April (nach
## Winter i.d.R. aufgefuellter Bodenspeicher).
Smax_boden <- 100
# phi als reiner Zahlenvektor (nicht als SpatRaster) - siehe Aufrufstelle
# unten: eine SpatRaster-Schicht PRO TAG waere bei einem vollen Jahr bis zu
# ~365 gleichzeitig im Speicher gehaltene Rasterobjekte, nur um sie danach
# sofort wieder zu einem einzigen Stapel zusammenzufuehren - unnoetig
# speicherintensiv (Ursache eines Speicher-Absturzes beim ersten Docker-
# Testlauf). pmin/pmax statt terra::clamp, da phi hier ein normaler
# Zahlenvektor ist.
berechne_ra <- function(J, phi) {
  dr <- 1 + 0.033 * cos(2 * pi * J / 365)
  delta <- 0.409 * sin(2 * pi * J / 365 - 1.39)
  ws <- acos(pmin(pmax(-tan(phi) * tan(delta), -1), 1))
  (24 * 60 / pi) * 0.0820 * dr * (ws * sin(phi) * sin(delta) + cos(phi) * cos(delta) * sin(ws))
}

et0_raster_je_jahr <- list()
speicher_raster_je_jahr <- list()
for (jr in jahre_mit_temperatur) {
  if (is.null(tmax_raster_je_jahr[[jr]]) || is.null(tmin_raster_je_jahr[[jr]]) || is.null(niederschlag_raster_je_jahr[[jr]])) next
  # Niederschlags- und Temperaturraster haben NICHT dieselbe raeumliche
  # Ausdehnung - auf die gemeinsame (kleinste) Flaeche zuschneiden, sonst
  # schlaegt die Rasterarithmetik unten fehl.
  gemeinsame_ext <- terra::ext(tmax_raster_je_jahr[[jr]])
  precip_jr <- terra::crop(niederschlag_raster_je_jahr[[jr]], gemeinsame_ext)
  tmax_jr <- terra::crop(tmax_raster_je_jahr[[jr]], gemeinsame_ext)
  tmin_jr <- terra::crop(tmin_raster_je_jahr[[jr]], gemeinsame_ext)

  # Nur Tage, an denen ALLE drei Groessen vorliegen (Publikationsluecken
  # koennen je Variable leicht variieren).
  gemeinsame_tage <- sort(Reduce(intersect, list(
    as.Date(time(precip_jr)), as.Date(time(tmax_jr)), as.Date(time(tmin_jr))
  )))
  gemeinsame_tage <- as.Date(gemeinsame_tage, origin = "1970-01-01")
  if (length(gemeinsame_tage) == 0) next
  precip_jr <- precip_jr[[match(gemeinsame_tage, as.Date(time(precip_jr)))]]
  tmax_jr <- tmax_jr[[match(gemeinsame_tage, as.Date(time(tmax_jr)))]]
  tmin_jr <- tmin_jr[[match(gemeinsame_tage, as.Date(time(tmin_jr)))]]

  r0 <- precip_jr[[1]]
  xy <- xyFromCell(r0, 1:ncell(r0))
  ll <- project(xy, from = crs(r0), to = "EPSG:4326")
  lat_vec <- ll[, 2] * pi / 180
  tag_des_jahres <- as.integer(format(gemeinsame_tage, "%j"))
  # Ra als reine Zahlenmatrix (Zelle x Tag) berechnen, ERST am Schluss zu
  # EINEM SpatRaster zusammenfassen (siehe Kommentar bei berechne_ra()).
  ra_mat <- vapply(tag_des_jahres, function(J) berechne_ra(J, lat_vec), numeric(length(lat_vec)))
  ra_mm <- rast(r0, nlyrs = length(tag_des_jahres))
  values(ra_mm) <- ra_mat * 0.408 # MJ/m2/Tag -> mm/Tag

  tmean_calc <- (tmax_jr + tmin_jr) / 2 # konsistent mit Hargreaves-Definition, statt tabs
  et0_jr <- 0.0023 * (tmean_calc + 17.8) * sqrt(clamp(tmax_jr - tmin_jr, lower = 0)) * ra_mm
  names(et0_jr) <- paste0("ET0_", format(gemeinsame_tage, "%Y-%m-%d"))
  time(et0_jr) <- gemeinsame_tage
  et0_raster_je_jahr[[jr]] <- et0_jr

  fruehlingsanfang <- as.Date(paste0(jr, "-04-01"))
  ab_idx <- which(gemeinsame_tage >= fruehlingsanfang)
  if (length(ab_idx) == 0) next
  n <- length(ab_idx)
  # Als reine Zahlenmatrix (Zelle x Tag) rekursiv aufbauen, NICHT schichtweise
  # per [[i]]<- in ein SpatRaster (siehe Kommentar bei der GDD-Kumulierung
  # oben - gleiches Muster, gleiches Speicherproblem bei ~180+ Tagen).
  precip_mat <- terra::values(precip_jr[[ab_idx]])
  et0_mat <- terra::values(et0_jr[[ab_idx]])
  speicher_mat <- matrix(NA_real_, nrow(precip_mat), n)
  s_prev <- rep(Smax_boden, nrow(precip_mat))
  for (i in seq_len(n)) {
    s_prev <- pmin(pmax(s_prev + precip_mat[, i] - et0_mat[, i], 0), Smax_boden)
    speicher_mat[, i] <- s_prev
  }
  speicher_jr <- rast(precip_jr[[ab_idx]])
  values(speicher_jr) <- speicher_mat
  names(speicher_jr) <- paste0("Speicher_", format(gemeinsame_tage[ab_idx], "%Y-%m-%d"))
  time(speicher_jr) <- gemeinsame_tage[ab_idx]
  speicher_raster_je_jahr[[jr]] <- speicher_jr
  cat("Wasserhaushalt", jr, "berechnet:", format(gemeinsame_tage[ab_idx][1], "%d.%m.%Y"),
      "-", format(gemeinsame_tage[ab_idx][n], "%d.%m.%Y"), "\n")
}

## Bodenwasserbilanz zum Stichtag (Montag) der gewaehlten Kalenderwoche -----
bodenwasser_bild_je_woche <- list()
bodenwasser_werte_je_woche <- list()
# Tatsaechlich verwendetes Datum je Woche (siehe Fallback unten - nicht
# immer exakt der Montag) - fuer die Anzeige in der Ebenen-Legende
# (aktualisiereLayerLabels() in onRender()), damit dort das ECHTE Datum
# des Snapshots steht statt ein pauschales "Wochenbeginn".
bodenwasser_datum_je_woche <- list()
boden_cache_alt <- lade_ebenen_cache("boden")
boden_aus_cache <- 0L; boden_neu <- 0L
for (jr in names(speicher_raster_je_jahr)) {
  speicher_r <- speicher_raster_je_jahr[[jr]]
  speicher_daten_tage <- as.Date(time(speicher_r))
  for (w in alle_wochen) {
    cached <- cache_eintrag_holen(boden_cache_alt, jr, w)
    if (!is.null(cached)) {
      bodenwasser_bild_je_woche[[paste(jr, w)]] <- cached$bild
      bodenwasser_werte_je_woche[[paste(jr, w)]] <- cached$werte
      bodenwasser_datum_je_woche[[paste(jr, w)]] <- cached$datum
      boden_aus_cache <- boden_aus_cache + 1L
      next
    }
    stichtag <- montag_von_woche(jr, w)
    if (stichtag > Sys.Date() + 1) next # noch nicht begonnene Woche nie anzeigen
    # Ein exakter Treffer auf den Montag fehlt regelmaessig ausgerechnet fuer
    # die jeweils AKTUELLE (laufende) Woche (Publikationsverzoegerung der
    # Rohdaten) - stattdessen der naechstgelegene VERFUEGBARE Tag bis zu 6
    # Tage VOR dem Montag (nie danach - sonst waere es keine
    # "Wochenbeginn"-Momentaufnahme mehr).
    passende_tage <- which(speicher_daten_tage <= stichtag & speicher_daten_tage >= stichtag - 6)
    if (length(passende_tage) == 0) next
    idx <- passende_tage[which.max(speicher_daten_tage[passende_tage])]
    ergebnis <- raster_zu_datauri(speicher_r[[idx]], bodenwasser_farben, c(0, 100))
    ergebnis$werte$bis <- format(speicher_daten_tage[idx], "%d.%m.%Y")
    bodenwasser_bild_je_woche[[paste(jr, w)]] <- ergebnis$bild
    bodenwasser_werte_je_woche[[paste(jr, w)]] <- ergebnis$werte
    bodenwasser_datum_je_woche[[paste(jr, w)]] <- format(speicher_daten_tage[idx], "%d.%m.%Y")
    boden_neu <- boden_neu + 1L
  }
}
speichere_ebenen_cache("boden", Map(
  function(b, w, d) list(bild = b, werte = w, datum = d),
  bodenwasser_bild_je_woche, bodenwasser_werte_je_woche, bodenwasser_datum_je_woche
))
cat("Bodenwasserbilanz-Hintergrundbilder erzeugt:", length(bodenwasser_bild_je_woche),
    "(aus Cache:", boden_aus_cache, "/ neu:", boden_neu, ")\n")

########################################################################
## Potenzielles Graswachstum (ModVege/growR, vektorisiert) --------------
########################################################################
## Vollstaendiger Port von growR's ModVege-Modell (Jouven et al. 2006) als
## Matrix-Schleife (Zeile=Zelle, Spalte=Tag) - EXAKT (Gleitkomma-Praezision)
## validiert gegen growR selbst an 7 echten AGFF-Standorten, siehe
## 44_wachstumspotenzial_prototyp.R und den Plan in
## .claude/plans/temporal-sauteeing-clover.md. "Potenziell" heisst hier:
## NI=1 (keine Duenge-/Naehrstofflimitierung) und KEIN Schnitt-Management
## (Wachstum unter dem Jahr ungenutzt/ungemaeht, wie es das Klima erlauben
## wuerde) - nur Temperatur/Strahlung/Wasserhaushalt begrenzen das Wachstum.
##
## Strahlung: KEINE flaechendeckende Messung verfuegbar (anders als an den
## 7 Validierungs-Standorten, wo echte SMN-Stationsmessung genutzt wurde) -
## deshalb per Angstroem-Prescott aus der relativen Sonnenscheindauer SrelD
## (= n/N) geschaetzt. Koeffizienten an 9 SMN-Stationen kalibriert (80'000
## Tageswerte, 43_vergleiche_wachstumsmodelle.R): Bias -0.9 W/m2, RMSE 24.
## Die fruehere Fassung (b_s=1.017) war auf n/24h statt n/N kalibriert und
## ueberschaetzte die Strahlung im Raster um rund 40%.
angstrom_a_s <- 0.245
angstrom_b_s <- 0.561

## fT/fW/fPAR/SEA - 1:1 aus growR's eigenem Quellcode vektorisiert (siehe
## Validierungsskript fuer die Herleitung/Fundstellen, insb. die beiden
## dort gefundenen Stolperfallen: fW()'s switch()-Eintraege 'tief'-Zweig
## 4-6 und 'mittel'-Zweig 5-6 sind LITERALE Konstanten 1, keine linearen
## Formeln "1*W").
modvege_fT <- function(t, T0, T1, T2) {
  ifelse(t < T0, 0,
    ifelse(t < T1, (t - T0) / (T1 - T0),
      ifelse(t < T2, 1,
        ifelse(t < 40, (40 - t) / (40 - T2), 0))))
}
modvege_fW <- function(W, PET) {
  bucket <- pmin(floor(W / 0.2) + 1, 6)
  hoch <- W
  mittel_formel <- c(2, 1.5, 1, 0.5, NA, NA)[bucket] * W + c(0, 0.1, 0.3, 0.6, NA, NA)[bucket]
  mittel <- ifelse(bucket >= 5, 1, mittel_formel)
  tief_formel <- c(4, 0.75, 0.25, NA, NA, NA)[bucket] * W + c(0, 0.65, 0.85, NA, NA, NA)[bucket]
  tief <- ifelse(bucket >= 4, 1, tief_formel)
  ifelse(PET > 6.5, hoch, ifelse(PET > 3.8, mittel, tief))
}
modvege_fPAR <- function(PAR) pmax(0, pmin(1, 1 - 0.0445 * (PAR - 5)))
modvege_SEA <- function(ST, minSEA, maxSEA, ST1, ST2) {
  ifelse(ST < 200, minSEA,
    ifelse(ST < (ST1 - 200), minSEA + (maxSEA - minSEA) * (ST - 200) / (ST1 - 400),
      ifelse(ST < (ST1 - 100), maxSEA,
        ifelse(ST < ST2, maxSEA + (minSEA - maxSEA) * (ST - ST1 + 100) / (ST2 - ST1 + 100),
          minSEA))))
}
modvege_atmospheric_CO2 <- function(jahr) 0.0156961 * jahr^2 - 60.962 * jahr + 59508.7
modvege_fCO2_growth <- function(c_CO2, b) 1 + b * log(c_CO2 / 360)
modvege_fCO2_transpiration <- function(c_CO2) 1 - 1e-04 * (c_CO2 - 360)

## Parameter: Posieux-Kalibrierung (growR-Beispiel, Funktionsgruppen-
## Mischung 70% FG_A/30% FG_B) als EINHEITLICHE Konstanten fuer ganz
## Schweiz - keine raeumliche Kalibrierung vorgesehen (wie bei den anderen
## Ebenen auch). Werte 1:1 aus einem aufgeloesten growR-ModvegeParameters-
## Objekt ausgelesen (siehe Validierungsskript), NICHT aus der Vignette
## nacherfunden.
modvege_P <- list(
  SLA = 0.0306, pcLAM = 0.68, ST1 = 630, ST2 = 1245, minSEA = 0.77, maxSEA = 1.23,
  LLS = 590, stubble_height = 0.02, crop_coefficient = 1.15, senescence_cap = 0.7,
  BDGV = 850, BDGR = 300, T0 = 5, T1 = 10, T2 = 20,
  KGV = 0.002, KGR = 0.001, KlDV = 0.001, KlDR = 5e-04,
  sigmaGV = 0.4, sigmaGR = 0.2, RUEmax = 3, WHC = 130, NI = 1, CO2_growth_factor = 0.5
)
modvege_P$minBMGV <- modvege_P$stubble_height * 10 * modvege_P$BDGV
modvege_P$minBMGR <- modvege_P$stubble_height * 10 * modvege_P$BDGR
modvege_P$REP_ON <- 0.25 + (0.75 * (modvege_P$NI - 0.35)) / 0.65 # = 1.0 bei NI=1
# Nur fuer das optionale Schnittverfahren (growR-Defaults und Tabelle
# management_parameters, Intensitaet "high").
modvege_P$BDDV <- 500; modvege_P$BDDR <- 150
modvege_P$maxOMDGV <- 0.9; modvege_P$minOMDGV <- 0.705; modvege_P$maxOMDGR <- 0.9; modvege_P$minOMDGR <- 0.59
modvege_P$OMDDV <- 0.45; modvege_P$OMDDR <- 0.4
modvege_P$cut_height <- 0.05; modvege_P$last_DOY_for_initial_cut <- 150; modvege_P$max_cut_delay <- 5
modvege_schnitt_hoehen <- c(500, 700, 900, 1100, 1300)
modvege_schnitt_anzahl <- c(5.5, 5, 4, 3.5, 3)
modvege_init <- list(AgeGV = 100, AgeGR = 2000, AgeDV = 500, AgeDR = 500,
                      BMGV = 420, BMGR = 0, BMDV = 300, BMDR = 30, WR = 130)

# Simuliert ganz Schweiz fuer EIN Jahr: Ta/Tmax/Tmin/precip/PAR/ET0 je eine
# Matrix (Zelle x Tag, Jan-Dez), liefert taegliches GRO (wasser-/
# temperaturlimitiertes, NICHT-kumuliertes Wachstum) und cBM (seit 1. Jan.
# kumuliert, wie growR selbst startet - NICHT ab 1. April wie das einfachere
# Bucket-Modell oben, da hier die Temperatursumme/Vegetationsbeginn-Suche
# dieselbe Jan-1-Konvention wie growR brauchen).
# erholung_tage > 0: Erholungsverzoegerung nach Trockenheit (NICHT Teil von
# ModVege/growR, eigene Erweiterung). Der Wasserstress-Faktor fuers Wachstum
# darf je Tag hoechstens um 1/erholung_tage steigen, Verschlechterungen
# wirken sofort - volle Erholung aus totalem Stress dauert also
# erholung_tage Tage. Der Wasserhaushalt (Transpiration) bleibt unveraendert.
# schnitt_hoehe (m ue. M. je Zelle): automatische Schnitte nach growR
# (determine_cut_automatically/apply_cuts, Intensitaet "high") - nur fuer
# Auswertungen (45_sentinel_erholung.R), die Karte rechnet ohne Schnitt.
# schnitt_matrix (logisch, Zelle x Tag): Schnitte an vorgegebenen Tagen (wie
# growR's determine_cut_from_input), hat Vorrang vor schnitt_hoehe.
# parameter/init: Listen, die einzelne Eintraege von modvege_P/modvege_init
# ueberschreiben (z.B. andere Artenmischung, NI, Startwerte je Standort).
# mit_lai: zusaetzlich gruenes LAI, Verdaulichkeit OMD (vor dem Schnitt),
# Erntemenge und Schnitttage je Zelle/Tag zurueckgeben.
simuliere_wachstumspotenzial <- function(Ta, precip, PAR, ET0, jahr, erholung_tage = 0,
                                         schnitt_hoehe = NULL, mit_lai = FALSE,
                                         schnitt_matrix = NULL, parameter = NULL, init = NULL) {
  n_zellen <- nrow(Ta); n_tage <- ncol(Ta)
  if (!is.null(parameter)) {
    for (n in names(parameter)) modvege_P[[n]] <- parameter[[n]]
    modvege_P$minBMGV <- modvege_P$stubble_height * 10 * modvege_P$BDGV
    modvege_P$minBMGR <- modvege_P$stubble_height * 10 * modvege_P$BDGR
    modvege_P$REP_ON <- 0.25 + (0.75 * (modvege_P$NI - 0.35)) / 0.65
  }
  if (!is.null(init)) for (n in names(init)) modvege_init[[n]] <- init[[n]]
  co2_ppm <- modvege_atmospheric_CO2(jahr)
  co2_wachstum <- modvege_fCO2_growth(co2_ppm, modvege_P$CO2_growth_factor)
  co2_transpiration <- modvege_fCO2_transpiration(co2_ppm)

  # Vegetationsbeginn (MTD-Methode, 10-Tage-Aussen-/5-Tage-Innenfenster) -
  # sequentiell ueber Tagespositionen, aber vektorisiert ueber alle Zellen
  # gleichzeitig je Position (exakt wie im validierten Prototyp). Rein lesend
  # auf Ta, braucht keine grosse Zwischenmatrix.
  j_start <- rep(NA_integer_, n_zellen)
  noch_offen <- rep(TRUE, n_zellen)
  for (j in 30:(n_tage - 10)) {
    if (!any(noch_offen)) break
    # na.rm/NA-sicher: Zellen ausserhalb der Schweiz (Rasterrand, maskiert)
    # haben NA-Wetterwerte - ein NA darf die Vegetationsbeginn-Suche nicht
    # zum Absturz bringen (if() mit NA), soll fuer diese Zellen aber auch nie
    # "zutreffen" (sie fallen am Ende auf den n_tage-Fallback zurueck, siehe
    # unten).
    aussen <- Ta[, j:(j + 9), drop = FALSE]
    aussen_ok <- apply(aussen >= 2, 1, function(x) isTRUE(all(x))) & (rowMeans(aussen) >= 6)
    aussen_ok[is.na(aussen_ok)] <- FALSE
    for (j_inner in 1:6) {
      innen <- Ta[, (j + j_inner - 1):(j + j_inner + 3), drop = FALSE]
      innen_ok <- apply(innen > 5, 1, function(x) isTRUE(all(x)))
      treffer <- noch_offen & aussen_ok & innen_ok
      treffer[is.na(treffer)] <- FALSE
      if (any(treffer)) { j_start[treffer] <- j + j_inner - 1; noch_offen[treffer] <- FALSE }
      if (!any(noch_offen)) break
    }
  }
  j_start[is.na(j_start)] <- n_tage

  # Nur die beiden TATSAECHLICH gebrauchten Ergebnisgroessen (GRO, cBM)
  # werden als volle Zelle-x-Tag-Matrix gehalten - alle anderen ModVege-
  # Zustandsgroessen (Alter/Biomasse je Pool, Temperatursumme, Schnee) sind
  # reine Tag-zu-Tag-Zwischenwerte (nur "gestern"/"heute" gleichzeitig noetig,
  # siehe update unten) und werden bewusst NICHT als Zeitreihe gespeichert -
  # erster Versuch mit 9 vollen Zusatz-Matrizen sprengte bei voller
  # Schweiz-Rasterung das Speicherlimit (siehe Kommentar bei berechne_ra()
  # oben zum selben Fallstrick beim Bucket-Modell, hier nur mit mehr
  # Zustandsgroessen).
  GRO <- matrix(0, n_zellen, n_tage)
  cBM <- matrix(0, n_zellen, n_tage)

  ST_prev <- rep(0, n_zellen)
  schnee_prev <- rep(0, n_zellen)
  AgeGVp <- rep(modvege_init$AgeGV, n_zellen); AgeGRp <- rep(modvege_init$AgeGR, n_zellen)
  AgeDVp <- rep(modvege_init$AgeDV, n_zellen); AgeDRp <- rep(modvege_init$AgeDR, n_zellen)
  BMGVp <- rep(modvege_init$BMGV, n_zellen); BMGRp <- rep(modvege_init$BMGR, n_zellen)
  BMDVp <- rep(modvege_init$BMDV, n_zellen); BMDRp <- rep(modvege_init$BMDR, n_zellen)
  SENGV <- rep(0, n_zellen); SENGR <- rep(0, n_zellen); ABSDV <- rep(0, n_zellen); ABSDR <- rep(0, n_zellen)
  WRp <- rep(modvege_init$WR, n_zellen); cBMp <- rep(0, n_zellen)
  fW_wachstum_prev <- rep(1, n_zellen)
  schnitt_in_wachstumsphase <- rep(FALSE, n_zellen)
  if (mit_lai) {
    LAI <- matrix(0, n_zellen, n_tage); SCHNITT <- matrix(FALSE, n_zellen, n_tage)
    OMD <- matrix(NA_real_, n_zellen, n_tage); ERNTE <- matrix(0, n_zellen, n_tage)
  }

  schnitt_vorgegeben <- !is.null(schnitt_matrix)
  schneiden_aktiv <- schnitt_vorgegeben || !is.null(schnitt_hoehe)
  if (schneiden_aktiv && !schnitt_vorgegeben) {
    P <- modvege_P
    bm_nach_schnitt <- P$cut_height * 10 * (P$BDGV + P$BDGR + P$BDDV + P$BDDR)
    jahresertrag <- (15.9 - 0.0058 * pmax(schnitt_hoehe, 500)) * 1000
    ende_schnittsaison <- (bm_nach_schnitt - jahresertrag * 0.4896) / (jahresertrag * -0.001228)
    erwartete_schnitte <- vapply(schnitt_hoehe, function(h) {
      i0 <- which.min(abs(modvege_schnitt_hoehen - h)); i1 <- which.min(abs(modvege_schnitt_hoehen[-i0] - h))
      a0 <- modvege_schnitt_hoehen[i0]; a1 <- modvege_schnitt_hoehen[-i0][i1]
      n0 <- modvege_schnitt_anzahl[i0]; n1 <- modvege_schnitt_anzahl[-i0][i1]
      (n1 - n0) / (a1 - a0) * (h - a0) + n0
    }, numeric(1))
    max_schnittintervall <- round((ende_schnittsaison - 150) / erwartete_schnitte)
    trocken <- precip < 1
  }
  if (schneiden_aktiv) { letzter_schnitt <- rep(NA_real_, n_zellen); n_schnitte <- rep(0L, n_zellen); verzoegerung <- rep(0, n_zellen) }

  T_melt <- -1; C_melt <- 3; C_freeze <- 0.05
  for (j in 1:n_tage) {
    T_avg <- Ta[, j]
    ST_heute <- ST_prev + pmax(T_avg, 0)

    # Schnee/Regen-Aufteilung + Grad-Tag-Schmelzmodell (growR berechnet dies
    # IMMER selbst aus Ta/precip, unabhaengig von einem evtl. mitgelieferten
    # Schnee-Input) - hier Tag fuer Tag statt als vorab berechnete Matrix.
    liquidP_heute <- (1 / (1 + exp(-1.5 * (T_avg - 2)))) * precip[, j]
    solidP_heute <- precip[, j] - liquidP_heute
    schmelze_heute <- ifelse(schnee_prev > 0 & T_avg >= T_melt, pmin(schnee_prev, C_melt * (T_avg - T_melt)), 0)
    frieren_heute <- ifelse(T_avg < T_melt, pmin(liquidP_heute, C_freeze * (T_melt - T_avg)), 0)
    schnee_heute <- pmax(0, schnee_prev + solidP_heute + frieren_heute - schmelze_heute)
    wasser_zufuhr_heute <- liquidP_heute + schmelze_heute

    LAIGV <- modvege_P$SLA * modvege_P$pcLAM * BMGVp / 10
    LAI_ET <- modvege_P$SLA * modvege_P$pcLAM * (BMGVp + BMGRp) / 10
    PETeff <- ifelse(precip[, j] > 1, 0.7 * modvege_P$crop_coefficient * ET0[, j], modvege_P$crop_coefficient * ET0[, j])
    PETeff <- ifelse(schnee_heute > 5, 0.2, PETeff) # growR: unter Schneedecke minimale Verdunstung
    PETeff <- PETeff * co2_transpiration
    PTr <- PETeff * (1 - exp(-0.6 * LAI_ET))
    ATr <- PTr * modvege_fW(WRp / modvege_P$WHC, PETeff)
    PEv <- PETeff - PTr
    AEv <- PEv * WRp / modvege_P$WHC
    AET <- ATr + AEv
    WR_heute <- pmax(0, pmin(modvege_P$WHC, WRp + wasser_zufuhr_heute - AET))
    ENVfPAR <- modvege_fPAR(PAR[, j])
    ENVfT <- modvege_fT(T_avg, modvege_P$T0, modvege_P$T1, modvege_P$T2)
    ENVfW <- modvege_fW(WR_heute / modvege_P$WHC, PETeff)
    if (erholung_tage > 0) ENVfW <- pmin(ENVfW, fW_wachstum_prev + 1 / erholung_tage)
    fW_wachstum_prev <- ENVfW
    ENV <- ENVfPAR * ENVfT * ENVfW
    vor_saisonstart <- j < j_start
    PGRO_tag <- ifelse(vor_saisonstart, 0, PAR[, j] * modvege_P$RUEmax * (1 - exp(-0.6 * LAIGV)) * 10 * co2_wachstum)
    GRO[, j] <- ifelse(vor_saisonstart, 0, modvege_P$NI * PGRO_tag * ENV *
      modvege_SEA(ST_heute, modvege_P$minSEA, modvege_P$maxSEA, modvege_P$ST1, modvege_P$ST2))

    REP <- ifelse(!schnitt_in_wachstumsphase & ST_heute >= modvege_P$ST1 & ST_heute <= modvege_P$ST2, modvege_P$REP_ON, 0)
    GROGV <- GRO[, j] * (1 - REP)
    GROGR <- GRO[, j] * REP

    dAgeGV <- ifelse(BMGVp - SENGV + GROGV != 0,
      (BMGVp - SENGV) / (BMGVp - SENGV + GROGV) * (AgeGVp + pmax(0, T_avg)) - AgeGVp, -AgeGVp)
    AgeGV_heute <- AgeGVp + dAgeGV
    dAgeGR <- ifelse(BMGRp - SENGR + GROGR != 0,
      (BMGRp - SENGR) / (BMGRp - SENGR + GROGR) * (AgeGRp + pmax(0, T_avg)) - AgeGRp, -AgeGRp)
    AgeGR_heute <- AgeGRp + dAgeGR

    ratio1 <- AgeGV_heute / modvege_P$LLS
    fAgeGV <- ifelse(ratio1 < 1 / 3, 1, ifelse(ratio1 < 1, 3 * ratio1, 3))
    ratio2 <- AgeGR_heute / (modvege_P$ST2 - modvege_P$ST1)
    fAgeGR <- ifelse(ratio2 < 1 / 3, 1, ifelse(ratio2 < 1, 3 * ratio2, 3))

    SENGV_neu <- ifelse(T_avg > modvege_P$T0, modvege_P$KGV * BMGVp * T_avg * fAgeGV,
      ifelse(T_avg > 0, 0, modvege_P$KGV * BMGVp * abs(T_avg)))
    SENGR_neu <- ifelse(T_avg > modvege_P$T0, modvege_P$KGR * BMGRp * T_avg * fAgeGR,
      ifelse(T_avg > 0, 0, modvege_P$KGR * BMGRp * abs(T_avg)))
    SENGV <- ifelse(abs(SENGV_neu) > modvege_P$senescence_cap * abs(GROGV), modvege_P$senescence_cap * GROGV, SENGV_neu)
    SENGR <- ifelse(abs(SENGR_neu) > modvege_P$senescence_cap * abs(GROGR), modvege_P$senescence_cap * GROGR, SENGR_neu)

    dAgeDV <- ifelse(BMDVp - ABSDV + SENGV != 0,
      (BMDVp - ABSDV) / (BMDVp - ABSDV + SENGV) * (AgeDVp + pmax(0, T_avg)) - AgeDVp, -AgeDVp)
    AgeDV_heute <- AgeDVp + dAgeDV
    dAgeDR <- ifelse(BMDRp - ABSDR + SENGR != 0,
      (BMDRp - ABSDR) / (BMDRp - ABSDR + SENGR) * (AgeDRp + pmax(0, T_avg)) - AgeDRp, -AgeDRp)
    AgeDR_heute <- AgeDRp + dAgeDR

    ratio3 <- AgeDV_heute / modvege_P$LLS
    fAgeDV <- ifelse(ratio3 < 1 / 3, 1, ifelse(ratio3 < 2 / 3, 2, 3))
    ratio4 <- AgeDR_heute / (modvege_P$ST2 - modvege_P$ST1)
    fAgeDR <- ifelse(ratio4 < 1 / 3, 1, ifelse(ratio4 < 2 / 3, 2, 3))
    ABSDV <- ifelse(T_avg > 0, modvege_P$KlDV * BMDVp * T_avg * fAgeDV, 0)
    ABSDR <- ifelse(T_avg > 0, modvege_P$KlDR * BMDRp * T_avg * fAgeDR, 0)

    dBMGV <- GROGV - SENGV
    dBMGR <- GROGR - SENGR
    BMGV_heute <- BMGVp + dBMGV
    BMGR_heute <- BMGRp + dBMGR
    # Mindestbestand ab ST2 - wie growR's update_biomass() inkl. angepasster
    # Tageszunahme (zaehlt fuer dBM/cBM).
    spaetsaison <- ST_heute >= modvege_P$ST2
    gv_boden <- spaetsaison & BMGV_heute < modvege_P$minBMGV
    gr_boden <- spaetsaison & BMGR_heute < modvege_P$minBMGR
    BMGV_heute <- ifelse(gv_boden, modvege_P$minBMGV, BMGV_heute)
    BMGR_heute <- ifelse(gr_boden, modvege_P$minBMGR, BMGR_heute)
    dBMGV <- ifelse(gv_boden, BMGV_heute - BMGVp, dBMGV)
    dBMGR <- ifelse(gr_boden, BMGR_heute - BMGRp, dBMGR)

    dBMDV <- (1 - modvege_P$sigmaGV) * SENGV - ABSDV
    dBMDR <- (1 - modvege_P$sigmaGR) * SENGR - ABSDR
    BMDV_heute <- BMDVp + dBMDV
    BMDR_heute <- BMDRp + dBMDR
    dBM_heute <- dBMGV + dBMGR + dBMDV + dBMDR
    cBM[, j] <- cBMp + pmax(0, dBM_heute)

    if (mit_lai) {
      # growR's calculate_digestibility() - vor dem Schnitt, also die
      # Qualitaet des gesamten stehenden Bestands am Schnitttag.
      bm_vor <- BMGV_heute + BMGR_heute + BMDV_heute + BMDR_heute
      omdgv <- modvege_P$maxOMDGV - AgeGV_heute * (modvege_P$maxOMDGV - modvege_P$minOMDGV) / modvege_P$LLS
      omdgr <- modvege_P$maxOMDGR - AgeGR_heute * (modvege_P$maxOMDGR - modvege_P$minOMDGR) / (modvege_P$ST2 - modvege_P$ST1)
      OMD[, j] <- (omdgv * BMGV_heute + omdgr * BMGR_heute + modvege_P$OMDDV * BMDV_heute + modvege_P$OMDDR * BMDR_heute) / bm_vor
    }
    if (schneiden_aktiv) {
      if (schnitt_vorgegeben) {
        schnitt <- schnitt_matrix[, j]
      } else {
        bm_heute <- BMGV_heute + BMGR_heute + BMDV_heute + BMDR_heute
        zielbiomasse <- pmax((-0.1228 * max(130, j) + 48.96) * 0.01 * jahresertrag, bm_nach_schnitt)
        faellig <- bm_heute >= zielbiomasse
        faellig <- faellig | ifelse(n_schnitte == 0, j > modvege_P$last_DOY_for_initial_cut,
                                    j - letzter_schnitt > max_schnittintervall)
        faellig <- faellig & j <= ende_schnittsaison
        trockenfenster <- rowSums(!trocken[, max(j - 1, 1):min(j + 2, n_tage), drop = FALSE]) == 0
        schnitt <- faellig & (trockenfenster | verzoegerung >= modvege_P$max_cut_delay)
        verzoegerung <- ifelse(schnitt, 0, ifelse(faellig, verzoegerung + 1, verzoegerung))
      }
      schnitt_in_wachstumsphase <- schnitt_in_wachstumsphase |
        (schnitt & ST_heute >= modvege_P$ST1 & ST_heute <= modvege_P$ST2)
      rest <- function(bm_vortag, dichte) pmin(bm_vortag, modvege_P$cut_height * 10 * dichte)
      neu_gv <- ifelse(schnitt, rest(BMGVp, modvege_P$BDGV), BMGV_heute)
      neu_dv <- ifelse(schnitt, rest(BMDVp, modvege_P$BDDV), BMDV_heute)
      neu_gr <- ifelse(schnitt, rest(BMGRp, modvege_P$BDGR), BMGR_heute)
      neu_dr <- ifelse(schnitt, rest(BMDRp, modvege_P$BDDR), BMDR_heute)
      # Erntemenge wie growR's hvBM-Zuwachs: Vortagesbestand minus Rest
      if (mit_lai) ERNTE[, j] <- ifelse(schnitt, (BMGVp - neu_gv) + (BMGRp - neu_gr) + (BMDVp - neu_dv) + (BMDRp - neu_dr), 0)
      BMGV_heute <- neu_gv; BMDV_heute <- neu_dv; BMGR_heute <- neu_gr; BMDR_heute <- neu_dr
      letzter_schnitt <- ifelse(schnitt, j, letzter_schnitt)
      n_schnitte <- n_schnitte + schnitt
      if (mit_lai) SCHNITT[, j] <- schnitt
    }
    if (mit_lai) LAI[, j] <- modvege_P$SLA * modvege_P$pcLAM * (BMGV_heute + BMGR_heute) / 10

    ST_prev <- ST_heute; schnee_prev <- schnee_heute
    AgeGVp <- AgeGV_heute; AgeGRp <- AgeGR_heute; AgeDVp <- AgeDV_heute; AgeDRp <- AgeDR_heute
    BMGVp <- BMGV_heute; BMGRp <- BMGR_heute; BMDVp <- BMDV_heute; BMDRp <- BMDR_heute
    cBMp <- cBM[, j]; WRp <- WR_heute
  }
  if (mit_lai) return(list(GRO = GRO, cBM = cBM, LAI = LAI, SCHNITT = SCHNITT, OMD = OMD, ERNTE = ERNTE))
  list(GRO = GRO, cBM = cBM)
}

## Experimentell: Die Ebenen werden berechnet, in der App aber nur mit dem
## URL-Parameter ?experimentell angezeigt (siehe experimentellerModus im
## JS-Teil). Grund: Abgleich mit AGFF-Messungen (Duerre 2026) zeigte, dass
## ModVege's Ein-Eimer-Wassermodell nach Regen sofort auf volles Potenzial
## zurueckspringt (keine Erholungsverzoegerung) und Grundwasserboeden (z.B.
## Gampelen) nicht abbildet. FALSE ueberspringt die Berechnung ganz.
wachstumspotenzial_freigeschaltet <- TRUE

# Stufen der Erholungsverzoegerung (Tage) - je Stufe eine eigene Ebenen-Datei
# <name>_e<stufe>.json, im JS per Schieberegler waehlbar (wie die Zeitfenster
# der Meteo-Ebenen). 0 = unveraendertes growR-Modell.
erholung_stufen_tage <- c(0, 7, 14, 21)

# Bereitet die Modell-Eingaben eines Jahres als Matrizen (Zelle x Tag) vor.
# Strahlung: Angstroem-Prescott aus der Sonnenscheindauer (SrelD). SrelD kommt
# mit 1-2 Monaten Verzoegerung - fuer die juengsten Tage ohne SrelD wird die
# Strahlung aus der Temperaturspanne geschaetzt (Hargreaves-Samani, FAO-56
# Gl. 50: Rs = kRs * sqrt(Tmax - Tmin) * Ra), mit kRs je Zelle an den letzten
# 60 Tagen mit SrelD kalibriert. Sonst endete das Modell am letzten SrelD-Tag.
bereite_wachstumspotenzial_eingaben <- function(jr) {
  if (is.null(tmax_raster_je_jahr[[jr]]) || is.null(tmin_raster_je_jahr[[jr]]) ||
      is.null(niederschlag_raster_je_jahr[[jr]]) || is.null(temperatur_raster_je_jahr[[jr]]) ||
      is.null(sonnenschein_raster_je_jahr[[jr]])) return(NULL)
  ext_mv <- terra::ext(tmax_raster_je_jahr[[jr]])
  precip_mv <- terra::crop(niederschlag_raster_je_jahr[[jr]], ext_mv)
  tmax_mv <- terra::crop(tmax_raster_je_jahr[[jr]], ext_mv)
  tmin_mv <- terra::crop(tmin_raster_je_jahr[[jr]], ext_mv)
  tabs_mv <- terra::crop(temperatur_raster_je_jahr[[jr]], ext_mv)
  sonne_mv <- terra::crop(sonnenschein_raster_je_jahr[[jr]], ext_mv)

  tage <- sort(Reduce(intersect, list(
    as.Date(time(precip_mv)), as.Date(time(tmax_mv)), as.Date(time(tmin_mv)), as.Date(time(tabs_mv))
  )))
  tage <- as.Date(tage, origin = "1970-01-01")
  # Vegetationsbeginn-Suche braucht min. 40 Tage, die Hargreaves-Kalibrierung
  # mindestens einige Tage mit SrelD.
  sonne_tage <- as.Date(time(sonne_mv))
  if (length(tage) < 40 || sum(tage %in% sonne_tage) < 20) return(NULL)

  r0 <- precip_mv[[1]]
  wahl <- function(r) terra::values(r[[match(tage, as.Date(time(r)))]])
  precip <- wahl(precip_mv); Ta <- wahl(tabs_mv)
  dT_wurzel <- sqrt(pmax(wahl(tmax_mv) - wahl(tmin_mv), 0))

  xy <- xyFromCell(r0, 1:ncell(r0))
  lat_vec <- project(xy, from = crs(r0), to = "EPSG:4326")[, 2] * pi / 180
  ra <- vapply(as.integer(format(tage, "%j")), function(J) berechne_ra(J, lat_vec), numeric(length(lat_vec))) # MJ/m2/Tag

  ET0 <- 0.0023 * (Ta + 17.8) * dT_wurzel * ra * 0.408

  spalten_sonne <- which(tage %in% sonne_tage)
  rs <- dT_wurzel * ra # Hargreaves ohne kRs
  sonne <- terra::values(sonne_mv[[match(tage[spalten_sonne], sonne_tage)]])
  rs_ang <- (angstrom_a_s + angstrom_b_s * (sonne / 100)) * ra[, spalten_sonne, drop = FALSE]
  kalib <- tail(seq_along(spalten_sonne), 60)
  x_k <- rs[, spalten_sonne[kalib], drop = FALSE]
  kRs <- rowSums(rs_ang[, kalib, drop = FALSE] * x_k, na.rm = TRUE) / rowSums(x_k^2, na.rm = TRUE)
  rs <- rs * kRs
  rs[, spalten_sonne] <- rs_ang
  PAR <- rs * 11.574 * 0.0406 # MJ/m2/Tag -> W/m2 -> PAR

  letzter_sonnentag <- max(tage[spalten_sonne])
  cat("Potenzielles Wachstum", jr, "- Eingaben:", format(tage[1], "%d.%m.%Y"), "-", format(max(tage), "%d.%m.%Y"),
      "| Strahlung aus Temperaturspanne ab", format(letzter_sonnentag + 1, "%d.%m.%Y"),
      "(kRs Median", round(median(kRs, na.rm = TRUE), 3), ")\n")
  list(Ta = Ta, precip = precip, PAR = PAR, ET0 = ET0, tage = tage, vorlage = r0,
       strahlung_geschaetzt_ab = if (letzter_sonnentag < max(tage)) letzter_sonnentag + 1 else NA)
}

wachstumspotenzial_rate_farben <- c("white", "beige", "yellowgreen", "forestgreen", "darkgreen")
wachstumspotenzial_kum_farben <- c("white", "beige", "yellowgreen", "forestgreen", "darkgreen")
wachstumspotenzial_quelle <- "Potenzielles Wachstum (ModVege/growR, selbst berechnet aus Temperatur/Strahlung/Bodenwasserhaushalt, NI=1/ohne Schnitt; Strahlung der juengsten Tage ohne Sonnenscheindaten aus der Temperaturspanne geschaetzt) - experimentell, kein Ersatz fuer Feldmessung."

# Wochenbilder eines Jahres und einer Stufe. Rate: Mittel der 7 Tage vor dem
# Wochenstichtag (Wochen, deren Fenster mehr als 3 Tage nach dem letzten
# Datentag endet, werden ausgelassen statt mit veralteten Daten gezeigt).
# Kumuliert: cBM am letzten Datentag der Woche vor dem Stichtag.
wachstumspotenzial_wochenbilder <- function(gro, kum, tage, jr, geschaetzt_ab) {
  aus <- list(rate = list(bilder = list(), werte = list()), kum = list(bilder = list(), werte = list()))
  for (w in alle_wochen) {
    stichtag <- montag_von_woche(jr, w)
    if (stichtag > Sys.Date() + 1) next
    schluessel <- paste(jr, w)
    fenster_ende <- min(stichtag - 1, max(tage))
    idx <- which(tage >= fenster_ende - 6 & tage <= fenster_ende)
    if (stichtag - 1 - max(tage) <= 3 && length(idx) == 7) {
      e <- raster_zu_datauri(mean(gro[[idx]]), wachstumspotenzial_rate_farben, c(0, 250))
      e$werte$bis <- format(fenster_ende, "%d.%m.%Y")
      if (!is.na(geschaetzt_ab) && fenster_ende >= geschaetzt_ab) e$werte$bis <- paste0(e$werte$bis, ", Strahlung teils geschaetzt")
      aus$rate$bilder[[schluessel]] <- e$bild; aus$rate$werte[[schluessel]] <- e$werte
    }
    passend <- which(tage <= stichtag & tage >= stichtag - 6)
    if (length(passend) > 0) {
      i <- passend[which.max(tage[passend])]
      e <- raster_zu_datauri(kum[[i]], wachstumspotenzial_kum_farben, c(0, 18000))
      e$werte$bis <- format(tage[i], "%d.%m.%Y")
      if (!is.na(geschaetzt_ab) && tage[i] >= geschaetzt_ab) e$werte$bis <- paste0(e$werte$bis, ", Strahlung teils geschaetzt")
      aus$kum$bilder[[schluessel]] <- e$bild; aus$kum$werte[[schluessel]] <- e$werte
    }
  }
  aus
}

# wachstumspotenzial_ebenen$<rate|kum>$e<stufe> = list(bilder, werte) - Form
# wie bei den Fenster-Ebenen, damit schreibe_fenster_ebenen_dateien() greift.
wachstumspotenzial_ebenen <- list(rate = list(), kum = list())
wsp_cache_alt <- list(rate = list(), kum = list())
for (art in c("rate", "kum")) for (stufe in erholung_stufen_tage) {
  sk <- paste0("e", stufe)
  wachstumspotenzial_ebenen[[art]][[sk]] <- list(bilder = list(), werte = list())
  wsp_cache_alt[[art]][[sk]] <- lade_ebenen_cache(paste0("wachstumspotenzial_", art, "_", sk))
}
wsp_aus_cache <- 0L; wsp_neu <- 0L
for (jr in if (wachstumspotenzial_freigeschaltet) jahre_mit_temperatur else character(0)) {
  # Abgeschlossene Jahre aus dem Cache, wenn ALLE Stufen dort vorhanden sind.
  if (jr < aktuelles_kalenderjahr) {
    alle_da <- all(unlist(lapply(wsp_cache_alt, function(je_stufe) vapply(je_stufe, function(c_alt)
      any(startsWith(as.character(names(c_alt)), paste0(jr, " "))), logical(1)))))
    if (alle_da) {
      for (art in c("rate", "kum")) for (sk in names(wsp_cache_alt[[art]])) {
        c_alt <- wsp_cache_alt[[art]][[sk]]
        for (schluessel in Filter(function(k) startsWith(k, paste0(jr, " ")), names(c_alt))) {
          wachstumspotenzial_ebenen[[art]][[sk]]$bilder[[schluessel]] <- c_alt[[schluessel]]$bild
          wachstumspotenzial_ebenen[[art]][[sk]]$werte[[schluessel]] <- c_alt[[schluessel]]$werte
          wsp_aus_cache <- wsp_aus_cache + 1L
        }
      }
      next
    }
  }
  ein <- bereite_wachstumspotenzial_eingaben(jr)
  if (is.null(ein)) next
  for (stufe in erholung_stufen_tage) {
    sk <- paste0("e", stufe)
    erg <- simuliere_wachstumspotenzial(Ta = ein$Ta, precip = ein$precip, PAR = ein$PAR, ET0 = ein$ET0,
                                        jahr = as.integer(jr), erholung_tage = stufe)
    gro <- rast(ein$vorlage, nlyrs = length(ein$tage)); values(gro) <- erg$GRO
    kum <- rast(ein$vorlage, nlyrs = length(ein$tage)); values(kum) <- erg$cBM
    rm(erg)
    bilder <- wachstumspotenzial_wochenbilder(gro, kum, ein$tage, jr, ein$strahlung_geschaetzt_ab)
    rm(gro, kum); invisible(gc())
    for (art in c("rate", "kum")) {
      wachstumspotenzial_ebenen[[art]][[sk]]$bilder <- c(wachstumspotenzial_ebenen[[art]][[sk]]$bilder, bilder[[art]]$bilder)
      wachstumspotenzial_ebenen[[art]][[sk]]$werte <- c(wachstumspotenzial_ebenen[[art]][[sk]]$werte, bilder[[art]]$werte)
      wsp_neu <- wsp_neu + length(bilder[[art]]$bilder)
    }
    cat("Potenzielles Wachstum", jr, "Erholungsverzoegerung", stufe, "Tage: berechnet\n")
  }
  rm(ein); invisible(gc())
}
for (art in c("rate", "kum")) for (sk in names(wachstumspotenzial_ebenen[[art]])) {
  e <- wachstumspotenzial_ebenen[[art]][[sk]]
  speichere_ebenen_cache(paste0("wachstumspotenzial_", art, "_", sk),
                         Map(function(b, w) list(bild = b, werte = w), e$bilder, e$werte))
}
cat("Potenzielles-Wachstum-Hintergrundbilder (alle Stufen):", wsp_aus_cache + wsp_neu,
    "(aus Cache:", wsp_aus_cache, "/ neu:", wsp_neu, ")\n")

## Sonnenscheindauer (relativ): gleitendes Fenster (Mittelwert) -----------
sonnenschein_farben <- c("dimgray", "gray70", "khaki1", "gold", "orange")
sonnenschein_quelle <- "MeteoSchweiz SrelD, 1km-Raster (Sonnenscheindauer relativ zum astronomisch Moeglichen)."

sonnenschein_fenster_ergebnisse <- baue_fenster_ebenen(
  "sonnenschein", jahre_mit_sonnenschein,
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
## Aus et0_raster_je_jahr oben (selbst berechnet, siehe Wasserhaushalt-
## Abschnitt) - je Jahr ein eigener Rasterstapel, wie bei den uebrigen
## "gleitendes Fenster"-Ebenen.
et0_farben <- c("lightyellow", "gold", "orange", "red")
et0_quelle <- "Bucket-Modell-Verdunstung (Hargreaves/FAO-56), berechnet aus MeteoSchweiz TabsD/TmaxD/TminD - kein Ersatz fuer Feldmessung."
et0_fenster_ergebnisse <- baue_fenster_ebenen(
  "et0", names(et0_raster_je_jahr),
  function(jr) {
    if (!jr %in% names(et0_raster_je_jahr)) return(NULL)
    r <- et0_raster_je_jahr[[jr]]
    list(raster = r, tage = as.Date(time(r)))
  },
  aggregat = "summe", farben = et0_farben, bereich_je_7tage = c(0, 25)
)
cat("ET0-Hintergrundbilder erzeugt:",
    sum(vapply(et0_fenster_ergebnisse, function(x) length(x$bilder), integer(1))), "\n")

## Kumulierte Wachstumsgradtage zum Stichtag (Montag) der gewaehlten
## Kalenderwoche - aus gdd_kumuliert_je_jahr oben (bereits laufend
## aufsummiert). temperatur_raster_je_jahr ist fuer VERGANGENE Tage eine
## lueckenlose Tagesreihe - fuer die AKTUELLSTE Woche kann der exakte Montag
## aber noch fehlen (Publikationsverzoegerung), deshalb wie bei der
## Bodenwasserbilanz ein Fallback auf den naechstgelegenen VERFUEGBAREN Tag
## bis zu 6 Tage davor ("so aktuell wie moeglich" statt keine Daten).
gdd_farben <- c("white", "yellow", "orange", "darkred")
gdd_quelle <- "Kumuliert aus MeteoSchweiz TabsD (Basis 5 Grad C) seit Beginn der lokal vorhandenen Temperaturdaten."
gdd_bild_je_woche <- list()
gdd_werte_je_woche <- list()
gdd_cache_alt <- lade_ebenen_cache("gdd")
gdd_aus_cache <- 0L; gdd_neu <- 0L
for (jr in names(gdd_kumuliert_je_jahr)) {
  # Abgeschlossenes Jahr, schon einmal verarbeitet: komplett aus dem Cache
  # uebernehmen (kumulierte Wachstumsgradtage bis zu einem vergangenen
  # Stichtag aendern sich nie mehr) - das Rohraster wird dafuer gar nicht
  # erst angefasst.
  alte_schluessel_jahr <- Filter(function(k) startsWith(k, paste0(jr, " ")), names(gdd_cache_alt))
  if (jr < aktuelles_kalenderjahr && length(alte_schluessel_jahr) > 0) {
    for (schluessel in alte_schluessel_jahr) {
      gdd_bild_je_woche[[schluessel]] <- gdd_cache_alt[[schluessel]]$bild
      gdd_werte_je_woche[[schluessel]] <- gdd_cache_alt[[schluessel]]$werte
    }
    gdd_aus_cache <- gdd_aus_cache + length(alte_schluessel_jahr)
    next
  }
  r_jahr <- gdd_kumuliert_je_jahr[[jr]]
  tage_r <- as.Date(time(r_jahr))
  for (w in alle_wochen) {
    stichtag <- montag_von_woche(jr, w)
    if (stichtag > Sys.Date() + 1) next # noch nicht begonnene Woche nie anzeigen
    passende_tage <- which(tage_r <= stichtag & tage_r >= stichtag - 6)
    if (length(passende_tage) == 0) next
    idx <- passende_tage[which.max(tage_r[passende_tage])]
    ergebnis <- raster_zu_datauri(r_jahr[[idx]], gdd_farben, c(0, 2500))
    ergebnis$werte$bis <- format(tage_r[idx], "%d.%m.%Y")
    schluessel <- paste(jr, w)
    gdd_bild_je_woche[[schluessel]] <- ergebnis$bild
    gdd_werte_je_woche[[schluessel]] <- ergebnis$werte
    gdd_neu <- gdd_neu + 1L
  }
}
speichere_ebenen_cache("gdd", Map(function(b, w) list(bild = b, werte = w), gdd_bild_je_woche, gdd_werte_je_woche))
cat("Wachstumsgradtage-Hintergrundbilder erzeugt:", length(gdd_bild_je_woche),
    "(aus Cache:", gdd_aus_cache, "/ neu:", gdd_neu, ")\n")

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
  gdd = schreibe_ebene_datei("gdd", gdd_bild_je_woche, gdd_werte_je_woche),
  wachstumspotenzial_rate = schreibe_fenster_ebenen_dateien("wachstumspotenzial_rate", wachstumspotenzial_ebenen$rate),
  wachstumspotenzial_kum = schreibe_fenster_ebenen_dateien("wachstumspotenzial_kum", wachstumspotenzial_ebenen$kum)
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
  gdd = list(label = "Wachstumsgradtage", farben = farben_zu_hex(gdd_farben), bereich = c(0, 2500), fensterSkaliert = FALSE, symbol = NULL, einheit = "°C-Tage", quelle = gdd_quelle),
  # bereich empirisch aus dem ersten Testlauf kalibriert (siehe Plan) -
  # Rate erreicht spaet in der Saison (ungemaeht, LAI baut sich das ganze
  # Jahr ungebremst auf) bis gegen 200 kg TS/ha/Tag, Kumuliert bis gegen
  # 17'000 kg TS/ha bis Ende August.
  wachstumspotenzial_rate = list(label = "Potenzielles Wachstum", farben = farben_zu_hex(wachstumspotenzial_rate_farben), bereich = c(0, 250), fensterSkaliert = FALSE, symbol = NULL, einheit = "kg TS/ha/Tag", quelle = wachstumspotenzial_quelle),
  # einheit bewusst kurz gehalten (nur "kg TS/ha", nicht zusaetzlich "seit
  # 1. Jan." wie urspruenglich) - bei den beiden langen Zahlen der Min/Max-
  # Skala (0.../18000...) brach der laengere Text in der schmalen
  # Seitenleiste um und ueberlagerte sich optisch. "Seit 1. Januar" steht
  # bereits im Label/der i-Button-Erklaerung.
  wachstumspotenzial_kum = list(label = "Potenzielles Wachstum, kumuliert", farben = farben_zu_hex(wachstumspotenzial_kum_farben), bereich = c(0, 18000), fensterSkaliert = FALSE, symbol = NULL, einheit = "kg TS/ha", quelle = wachstumspotenzial_quelle)
)

# Schnittanalyse Testgebiet (experimentell): Bilder/Werte schreibt
# 47_ertrag_qualitaet.R einmalig nach outputs/ebenen/ (nicht naechtlich);
# hier nur die kleine Indexdatei lesen und Schluessel/Legenden uebernehmen.
schnittanalyse_index <- file.path(ebenen_dir, "schnittanalyse_index.json")
schnittanalyse_gebiet <- NULL
if (file.exists(schnittanalyse_index)) {
  sa <- jsonlite::fromJSON(schnittanalyse_index, simplifyVector = FALSE)
  schnittanalyse_gebiet <- sa$gebiet
  for (n in names(sa$ebenen)) {
    e <- sa$ebenen[[n]]
    ebenen_schluessel[[n]] <- unlist(e$schluessel)
    layer_legenden[[n]] <- list(label = e$label, farben = unlist(e$farben), bereich = unlist(e$bereich), fensterSkaliert = FALSE,
                                symbol = NULL, einheit = e$einheit, quelle = e$quelle, ausserhalb = "ausserhalb des Testgebiets")
  }
}

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
  var standortVerlaeufe = __STANDORT_VERLAEUFE__;
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
  var schnittanalyseGebiet = __SCHNITTANALYSE_GEBIET__;
  var schnittRadios = [];
  function istSchnittEbene(name) { return name.indexOf('schnittanalyse_') === 0; }
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
  // Potenzielles Wachstum: je Stufe der Erholungsverzoegerung eine eigene
  // Datei <name>_e<stufe> (siehe R: erholung_stufen_tage).
  var erholungStufen = [0, 7, 14, 21];
  var erholung = 14;
  function istErholungsEbene(name) { return name === 'wachstumspotenzial_rate' || name === 'wachstumspotenzial_kum'; }
  function ebeneDateiSchluessel(name) {
    if (istFensterEbene(name)) return name + '_' + meteoFenster;
    if (istErholungsEbene(name)) return name + '_e' + erholung;
    return name;
  }
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
  var grafikDatum = '__GRAFIK_DATUM__';
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
  // Graswachstum-Kreis und DGV-Ring (AFC, die BILD-Ebenen) haben je einen
  // eigenen Schalter. Die (unsichtbaren, nur fuer Hover + die \"Tage seit
  // Messung\"-Farblegende benoetigten) Standort-Marker selbst haben KEINEN
  // eigenen Schalter mehr (vormals \"Messnetz-Standorte\") - sie sind
  // hoverbar, sobald mindestens einer der beiden Schalter an ist (siehe
  // applyMapState()), da die Legende ja genau zu diesen beiden Ebenen
  // gehoert.
  var graswachstumOn = true;
  var afcOn = true;
  // Schalter MeteoSchweiz-Stationen (Ebenen-Kasten): Default AUS - reine
  // Referenz-Ebene, nicht Teil der eigentlichen AGFF-Auswertung.
  // smnStationenTraceIdx zeigt auf die EINE,
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
    // Hover-Marker-Trace (inkl. \"Tage seit Messung\"-Farblegende) nur
    // sichtbar, wenn Graswachstum oder DGV eingeschaltet ist - sonst waeren
    // die Standorte trotz ausgeblendeten Bildern weiterhin geisterhaft
    // hoverbar bzw. die Legende ohne zugehoerige Ebene sichtbar.
    var vis = mapWochen.map(function(m, i) { return (graswachstumOn || afcOn) && i === idx; });
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
        var ort = orte ? orte[p.pointNumber] : null;
        var siteIdx = ort ? siteNames.indexOf(ort) : -1;
        if (siteIdx !== -1) {
          waehleSite(siteIdx);
          zeigeStandortBlatt(ort, siteIdx, p.customdata);
          return;
        }
        // Andere Punkte (MeteoSchweiz-Station, Suchmarker): auf Mobile gibt es
        // kein Hover - deren Text deshalb im Blatt zeigen.
        var text = p.text || p.hovertext;
        if (istMobil() && text) {
          var d = document.createElement('div'); d.className = 'gw-blatt-text'; d.innerHTML = text;
          zeigeBlatt('', '', d);
        }
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
        if (istMobil()) return;
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
    aktualisiereTageSeitMessungLegende();
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
    // Potenzielles Wachstum: NUR \"(berechnet)\"-Hinweis, OHNE eigenes Datum
    // im Label (anders als boden) - das tatsaechliche Datenstand-Datum kommt
    // hier ausschliesslich ueber den allgemeinen \"Stand ...\"-Mechanismus im
    // Kartentitel (aktualisiereKartentitel(), ueber werte.bis), sonst
    // entstuende dieselbe Doppel-Datum-Anzeige, die dort fuer boden extra
    // behoben werden musste.
    if (radioWachstumspotenzialRate && radioWachstumspotenzialRate.labelTextEl) {
      radioWachstumspotenzialRate.labelTextEl.textContent = layerLegenden.wachstumspotenzial_rate.label + ' (experimentell)';
    }
    if (radioWachstumspotenzialKum && radioWachstumspotenzialKum.labelTextEl) {
      radioWachstumspotenzialKum.labelTextEl.textContent = layerLegenden.wachstumspotenzial_kum.label + ' (experimentell)';
    }
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
    '.gw-layer-legende-balken-wrap { position: relative; padding-bottom: 7px; }',
    '.gw-layer-legende-balken { height: 12px; border-radius: 3px; border: 1px solid rgba(0,0,0,0.15); }',
    // Kleines Dreieck unter dem Farbverlaufs-Balken, zeigt per left:X% die
    // Position des Werts am Cursor (siehe aktualisierePfeilPosition()).
    '.gw-legende-pfeil { position: absolute; top: 12px; width: 0; height: 0; border-left: 5px solid transparent; border-right: 5px solid transparent; border-bottom: 6px solid #333; transform: translateX(-50%); pointer-events: none; }',
    // Donut-Ring per Masken-Trick (radial-gradient schneidet die Mitte
    // transparent) statt eines SVG - conic-gradient uebernimmt die
    // Farbverlauf-Stuetzstellen 1:1 vom vorherigen linear-gradient-Balken.
    // AFC-Legende: CSS-Donut (conic-gradient, Mitte per Maske transparent)
    // plus SVG-Ebene darueber fuer Nullpunkt-Strich und Zielbereich-Pfeil.
    '.gw-afc-ring-wrap { position: relative; width: 80px; height: 80px; margin: 0 auto; }',
    '.gw-afc-ring { position: absolute; top: 12px; left: 12px; width: 56px; height: 56px; border-radius: 50%; -webkit-mask: radial-gradient(farthest-side, transparent calc(100% - 10px), #000 calc(100% - 10px)); mask: radial-gradient(farthest-side, transparent calc(100% - 10px), #000 calc(100% - 10px)); }',
    '.gw-afc-ring-svg { position: absolute; top: 0; left: 0; overflow: visible; }',
    '.gw-afc-null { text-align: center; font-size: 10px; color: #555; margin-bottom: -6px; }',
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
    '.gw-slider-row { font-family: sans-serif; font-size: 14px; display: flex; align-items: center; margin: 10px 0; padding: 10px 16px; background: #f7f7f7; border-radius: 6px; }',
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
    // Ebenen-Box neben der Karte: flex-grow:0 (statt 1) + max-width, damit
    // sie auf breiten Bildschirmen NICHT am uebrigen Platz mitwaechst (das
    // tat sie vorher, da auch die Karte flex-grow:1 hat - beide teilten
    // sich den Rest 50/50, die Box wurde dadurch doppelt so breit wie
    // beabsichtigt). Im Mobile-Stack-Layout (@media 700px) wieder
    // flex-grow:1, damit sie dort weiterhin ihre volle Zeile ausfuellt,
    // statt als schmale Box mit Leerraum daneben zu stehen.
    '.gw-map-controls-panel { flex: 0 1 220px; min-width: 220px; max-width: 240px; }',
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
    '  .gw-map-controls-panel { flex: 1 1 auto; max-width: none; }' +
    '}'
  ].join(' ');
  document.head.appendChild(style);

  // Blatt: gemeinsames Panel fuer Standort-Kennzahlen und (auf Mobile) die
  // Erklaerungstexte der i-Knoepfe. Auf dem Handy ein Bottom Sheet (immer
  // bildschirmbreit, kann nicht am Rand abgeschnitten werden, die Karte
  // bleibt oben sichtbar), auf dem Desktop eine Karte unten rechts.
  // Mobile-Layout (<= 700px): Kopfzeile statt Plotly-Titel, randlose Karte,
  // Legende/Wert in einer Leiste unter der Karte, Ebenen und Kurve hinter
  // Knoepfen - Karte, Wert und Wochenumschalter passen ohne Scrollen.
  var stilMobil = document.createElement('style');
  stilMobil.textContent = [
    '.gw-blatt { position: fixed; z-index: 3000; background: white; box-shadow: 0 4px 24px rgba(0,0,0,0.25); font-family: sans-serif; font-size: 13px; color: #222; overflow-y: auto; right: 20px; bottom: 20px; width: 340px; max-height: 70vh; border-radius: 10px; padding: 12px 16px; box-sizing: border-box; }',
    '.gw-blatt-griff { display: none; width: 36px; height: 4px; border-radius: 2px; background: #ccc; margin: 0 auto 8px; }',
    '.gw-blatt-kopf { display: flex; align-items: flex-start; gap: 8px; margin-bottom: 4px; }',
    '.gw-blatt-titel { flex: 1; font-weight: 600; font-size: 15px; }',
    '.gw-blatt-zu { border: none; background: none; font-size: 22px; line-height: 1; cursor: pointer; color: #555; padding: 0 2px; }',
    '.gw-blatt-unter { color: #666; font-size: 12px; margin-bottom: 8px; }',
    '.gw-blatt-zeile { display: flex; justify-content: space-between; gap: 8px; padding: 5px 0; border-bottom: 1px solid #eee; }',
    '.gw-blatt-zeile b { font-weight: 600; text-align: right; }',
    '.gw-blatt-knopf { display: block; width: 100%; margin-top: 10px; padding: 9px; border: 1px solid #bbb; border-radius: 6px; background: white; font-size: 13px; cursor: pointer; }',
    '.gw-blatt-text { font-size: 13px; line-height: 1.5; }',
    '.gw-mobil-only { display: none; }',
    '.gw-mobil-kopf-titel { font-weight: 600; font-size: 17px; }',
    '.gw-mobil-kopf-unter { font-size: 12px; color: #555; margin-top: 1px; }',
    '.gw-kartenleiste { padding: 6px 10px; border-bottom: 1px solid #eee; font-size: 12px; }',
    '.gw-kartenleiste-legende { display: flex; align-items: center; gap: 6px; margin-bottom: 4px; }',
    '.gw-kartenleiste-balken { flex: 1; height: 8px; border-radius: 2px; min-width: 40px; }',
    '.gw-kartenleiste-wert { font-weight: 600; }',
    '.gw-mobil-knoepfe { gap: 8px; padding: 4px 10px 10px; }',
    '.gw-mobil-knoepfe button { flex: 1; padding: 9px; border: 1px solid #bbb; border-radius: 6px; background: white; font-size: 14px; cursor: pointer; }',
    '.gw-ebenen-zu-zeile { justify-content: space-between; align-items: center; margin-bottom: 4px; }',
    '@media (max-width: 700px) {' +
    '  .gw-blatt { left: 0; right: 0; bottom: 0; width: auto; max-height: 65vh; border-radius: 14px 14px 0 0; padding: 8px 16px calc(14px + env(safe-area-inset-bottom)); }' +
    '  .gw-blatt-griff { display: block; }' +
    '  .gw-desktop-only { display: none !important; }' +
    '  .gw-mobil-only { display: block; }' +
    '  .gw-mobil-knoepfe, .gw-ebenen-zu-zeile { display: flex; }' +
    '  #gw-seite { padding: 0 !important; }' +
    '  #gw-kartenzeile { gap: 0 !important; }' +
    '  .gw-mobil-kopf { padding: 8px 10px 2px; }' +
    '  .gw-map-controls-panel { display: none; }' +
    '  body.gw-ebenen-offen .gw-map-controls-panel { display: block; position: fixed; left: 0; right: 0; bottom: 0; z-index: 2900; max-height: 70vh; overflow-y: auto; border-radius: 14px 14px 0 0; box-shadow: 0 -4px 24px rgba(0,0,0,0.25); padding-bottom: env(safe-area-inset-bottom); background: #f7f7f7; }' +
    '  .gw-afc-legende-box { display: none !important; }' +
    '  .gw-kurvenbereich { display: none; }' +
    '  body.gw-kurve-offen .gw-kurvenbereich { display: block; }' +
    '  .gw-slider-row { margin: 4px 10px; padding: 6px 8px; }' +
    '  .gw-slider-aligned { margin-left: 0 !important; width: auto !important; flex: 1 1 auto !important; }' +
    '  .gw-slider-label-row { gap: 6px; margin-bottom: 4px; }' +
    '  .gw-today-btn { width: auto !important; min-width: 56px; padding: 0 8px; margin-left: 8px; }' +
    '}'
  ].join(' ');
  document.head.appendChild(stilMobil);

  function istMobil() { return window.matchMedia('(max-width: 700px)').matches; }
  var blattEl = document.createElement('div');
  blattEl.className = 'gw-blatt';
  blattEl.style.display = 'none';
  document.body.appendChild(blattEl);
  var blattGeoeffnetUm = 0;
  blattEl.addEventListener('click', function(evt) { evt.stopPropagation(); });
  function schliesseBlatt() { blattEl.style.display = 'none'; }
  function zeigeBlatt(titel, unter, inhalt) {
    blattEl.innerHTML = '';
    var griff = document.createElement('div'); griff.className = 'gw-blatt-griff'; blattEl.appendChild(griff);
    var kopf = document.createElement('div'); kopf.className = 'gw-blatt-kopf';
    var t = document.createElement('div'); t.className = 'gw-blatt-titel'; t.textContent = titel || '';
    var zu = document.createElement('button'); zu.type = 'button'; zu.className = 'gw-blatt-zu'; zu.textContent = '×';
    zu.setAttribute('aria-label', 'Schliessen');
    zu.addEventListener('click', schliesseBlatt);
    kopf.appendChild(t); kopf.appendChild(zu); blattEl.appendChild(kopf);
    if (unter) { var u = document.createElement('div'); u.className = 'gw-blatt-unter'; u.textContent = unter; blattEl.appendChild(u); }
    if (inhalt) blattEl.appendChild(inhalt);
    blattEl.style.display = 'block';
    blattEl.scrollTop = 0;
    blattGeoeffnetUm = Date.now();
  }
  document.addEventListener('keydown', function(evt) { if (evt.key === 'Escape') { schliesseBlatt(); document.body.classList.remove('gw-ebenen-offen'); } });
  // Klick/Tipp ausserhalb schliesst Blatt und Ebenen-Blatt - kurz nach dem
  // Oeffnen ignoriert, weil der oeffnende Klick (z.B. auf einen Kartenpunkt)
  // selbst noch bis zum document hochblubbert.
  document.addEventListener('click', function(evt) {
    if (Date.now() - blattGeoeffnetUm < 400) return;
    schliesseBlatt();
    var panel = document.getElementById('datenexplorer-map-controls');
    if (document.body.classList.contains('gw-ebenen-offen') && panel && !panel.contains(evt.target)) {
      document.body.classList.remove('gw-ebenen-offen');
    }
  });

  // Mobile-Elemente rund um die Karte (auf dem Desktop per CSS ausgeblendet)
  var kartenzeileEl = document.getElementById('gw-kartenzeile');
  var mobilKopfUnterEl = null, kartenleisteLegendeEl = null, kartenleisteWertEl = null, kurveKnopfEl = null;
  if (kartenzeileEl) {
    var mobilKopf = document.createElement('div');
    mobilKopf.className = 'gw-mobil-only gw-mobil-kopf';
    var mkTitel = document.createElement('div'); mkTitel.className = 'gw-mobil-kopf-titel'; mkTitel.textContent = 'Graswachstum';
    mobilKopfUnterEl = document.createElement('div'); mobilKopfUnterEl.className = 'gw-mobil-kopf-unter';
    mobilKopf.appendChild(mkTitel); mobilKopf.appendChild(mobilKopfUnterEl);
    kartenzeileEl.parentNode.insertBefore(mobilKopf, kartenzeileEl);
    var kartenleiste = document.createElement('div');
    kartenleiste.className = 'gw-mobil-only gw-kartenleiste';
    kartenleisteLegendeEl = document.createElement('div');
    kartenleisteLegendeEl.className = 'gw-kartenleiste-legende';
    kartenleisteLegendeEl.style.display = 'none';
    kartenleisteWertEl = document.createElement('div');
    kartenleisteWertEl.className = 'gw-kartenleiste-wert';
    kartenleisteWertEl.style.display = 'none';
    kartenleiste.appendChild(kartenleisteLegendeEl); kartenleiste.appendChild(kartenleisteWertEl);
    kartenzeileEl.parentNode.insertBefore(kartenleiste, kartenzeileEl.nextSibling);
    var knoepfe = document.createElement('div');
    knoepfe.className = 'gw-mobil-knoepfe gw-mobil-only';
    var ebenenKnopf = document.createElement('button'); ebenenKnopf.type = 'button'; ebenenKnopf.textContent = 'Ebenen';
    ebenenKnopf.addEventListener('click', function(evt) {
      evt.stopPropagation();
      schliesseBlatt();
      document.body.classList.toggle('gw-ebenen-offen');
      blattGeoeffnetUm = Date.now();
    });
    kurveKnopfEl = document.createElement('button'); kurveKnopfEl.type = 'button'; kurveKnopfEl.textContent = 'Kurve';
    kurveKnopfEl.addEventListener('click', function(evt) {
      evt.stopPropagation();
      schliesseBlatt();
      if (document.body.classList.contains('gw-kurve-offen')) {
        document.body.classList.remove('gw-kurve-offen');
        kurveKnopfEl.textContent = 'Kurve';
      } else {
        zeigeKurve();
      }
    });
    knoepfe.appendChild(ebenenKnopf); knoepfe.appendChild(kurveKnopfEl);
    var sliderHost = document.getElementById('datenexplorer-slider');
    if (sliderHost) sliderHost.parentNode.insertBefore(knoepfe, sliderHost.nextSibling);
  }
  function setzeWertText(text) {
    if (wertAnzeigeEl) wertAnzeigeEl.textContent = text;
    if (kartenleisteWertEl) kartenleisteWertEl.textContent = text.replace('Wert am Cursor: ', '').replace('–', 'Auf die Karte tippen für den Wert');
  }
  // Legende der aktiven Hintergrund-Ebene als schmale Leiste unter der Karte
  // (Mobile), i-Knopf zeigt Bezeichnung und Quelle im Blatt.
  function aktualisiereKartenleiste(info) {
    if (!kartenleisteLegendeEl) return;
    kartenleisteLegendeEl.innerHTML = '';
    // Wertzeile nur mit aktiver Ebene - ohne gibt es am Cursor nichts abzufragen
    if (kartenleisteWertEl) kartenleisteWertEl.style.display = info ? 'block' : 'none';
    if (!info) { kartenleisteLegendeEl.style.display = 'none'; return; }
    kartenleisteLegendeEl.style.display = 'flex';
    var min = document.createElement('span'); min.textContent = info.bereich[0];
    var balken = document.createElement('span'); balken.className = 'gw-kartenleiste-balken';
    balken.style.background = 'linear-gradient(to right,' + info.farben.join(',') + ')';
    var max = document.createElement('span'); max.textContent = info.bereich[1] + ' ' + info.einheit;
    kartenleisteLegendeEl.appendChild(min); kartenleisteLegendeEl.appendChild(balken); kartenleisteLegendeEl.appendChild(max);
    if (typeof macheInfoKnopf === 'function') kartenleisteLegendeEl.appendChild(macheInfoKnopf(info.quelle, null, info.label));
  }
  window.addEventListener('resize', function() { aktualisiereKartentitel(); });

  // Mini-Saisonkurve (SVG) fuer das Standortblatt: gemessener Zuwachs des
  // gewaehlten Jahres, senkrechter Strich = gewaehlte Woche.
  function miniKurve(punkte, markDoy) {
    var NS = 'http://www.w3.org/2000/svg';
    var b = 300, h = 70, x0 = 60, x1 = 330, yMax = 150;
    var svg = document.createElementNS(NS, 'svg');
    svg.setAttribute('viewBox', '0 0 ' + b + ' ' + h); svg.setAttribute('width', '100%'); svg.setAttribute('height', h);
    var px = function(d) { return Math.max(0, Math.min(b, (d - x0) / (x1 - x0) * b)); };
    var py = function(g) { return h - 4 - Math.min(g, yMax) / yMax * (h - 12); };
    var basis = document.createElementNS(NS, 'line');
    basis.setAttribute('x1', 0); basis.setAttribute('x2', b); basis.setAttribute('y1', h - 4); basis.setAttribute('y2', h - 4);
    basis.setAttribute('stroke', '#ccc'); svg.appendChild(basis);
    if (markDoy) {
      var m = document.createElementNS(NS, 'line');
      m.setAttribute('x1', px(markDoy)); m.setAttribute('x2', px(markDoy)); m.setAttribute('y1', 0); m.setAttribute('y2', h - 4);
      m.setAttribute('stroke', '#999'); m.setAttribute('stroke-dasharray', '3,3'); svg.appendChild(m);
    }
    if (punkte && punkte.length) {
      var pl = document.createElementNS(NS, 'polyline');
      pl.setAttribute('points', punkte.map(function(p) { return px(p[0]) + ',' + py(p[1]); }).join(' '));
      pl.setAttribute('fill', 'none'); pl.setAttribute('stroke', '#3B6D11'); pl.setAttribute('stroke-width', 2);
      svg.appendChild(pl);
    }
    return svg;
  }

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

  // Kartenklick: Standort waehlen OHNE Umschalten (ein zweiter Klick auf
  // denselben Standort oeffnet nur wieder das Blatt).
  function waehleSite(siteIdx) {
    if (selection.type === 'site' && selection.idx === siteIdx) return;
    vorherigeSelection = { type: selection.type, idx: selection.idx };
    selection = { type: 'site', idx: siteIdx };
    aktiviereVorjahrFuerEinzelstandort();
    input.value = siteNames[siteIdx];
    applyState();
  }
  function zeigeKurve() {
    if (istMobil()) {
      document.body.classList.add('gw-kurve-offen');
      if (kurveKnopfEl) kurveKnopfEl.textContent = 'Kurve ausblenden';
      Plotly.Plots.resize(el);
    }
    var bereich = document.querySelector('.gw-kurvenbereich');
    if (bereich) bereich.scrollIntoView({ behavior: 'smooth', block: 'start' });
  }
  function zeigeStandortBlatt(ort, siteIdx, kennzahlen) {
    var t = (kennzahlen || '').split('|');
    var inhalt = document.createElement('div');
    var zeile = function(l, w) {
      var z = document.createElement('div'); z.className = 'gw-blatt-zeile';
      var a = document.createElement('span'); a.textContent = l;
      var b = document.createElement('b'); b.textContent = w;
      z.appendChild(a); z.appendChild(b); inhalt.appendChild(z);
    };
    zeile('Graswachstum', (t[2] || '–') + ' kg TS/ha/Tag');
    zeile('DGV', t[3] ? t[3] + ' kg TS/ha' : 'keine Angabe');
    var fi = afcFensterJeWoche[selectedYear + ' ' + selectedWeek];
    var v = fi ? afcVerlaeufe[fi - 1] : null;
    if (v) zeile('Ziel-DGV dieser Woche', v.low + '–' + v.high + ' kg TS/ha');
    var verlauf = standortVerlaeufe[ort] && standortVerlaeufe[ort][selectedYear];
    if (verlauf && verlauf.length > 1) {
      var titelKurve = document.createElement('div');
      titelKurve.className = 'gw-blatt-unter';
      titelKurve.style.marginTop = '8px';
      titelKurve.textContent = 'Graswachstum ' + selectedYear + ' (Strich = gewaehlte Woche)';
      inhalt.appendChild(titelKurve);
      var montag = new Date(Date.UTC(selectedYear, 0, 4));
      montag.setUTCDate(montag.getUTCDate() - ((montag.getUTCDay() + 6) % 7) + (selectedWeek - 1) * 7);
      var doy = Math.round((montag - Date.UTC(selectedYear, 0, 1)) / 86400000) + 1;
      inhalt.appendChild(miniKurve(verlauf, doy));
    }
    var knopf = document.createElement('button');
    knopf.type = 'button'; knopf.className = 'gw-blatt-knopf';
    knopf.textContent = 'Ganze Graswachstumskurve anzeigen';
    knopf.addEventListener('click', function() { schliesseBlatt(); zeigeKurve(); });
    inhalt.appendChild(knopf);
    var alter = t[5] === '0' ? 'heute' : (t[5] === '1' ? 'gestern' : 'vor ' + t[5] + ' Tagen');
    var unter = (t[1] ? t[1] + ' m ü. M. · ' : '') + (t[4] ? 'gemessen am ' + t[4] + ' (' + alter + ')' : '');
    zeigeBlatt(t[0] || ort, unter, inhalt);
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
  var radioWachstumspotenzialRate = null, radioWachstumspotenzialKum = null;
  var experimentellerModus = new URLSearchParams(window.location.search).has('experimentell');
  var radioJeEbene = {};
  function aktualisiereLayerVerfuegbarkeit() {
    if (radioNiederschlag) radioNiederschlag.disabled = !ebeneHatJahr('niederschlag', selectedYear);
    if (radioBoden) radioBoden.disabled = !ebeneHatJahr('boden', selectedYear);
    if (radioTemperatur) radioTemperatur.disabled = !ebeneHatJahr('temperatur', selectedYear);
    if (radioBodentemperatur) radioBodentemperatur.disabled = !ebeneHatJahr('bodentemperatur', selectedYear);
    if (radioSonnenschein) radioSonnenschein.disabled = !ebeneHatJahr('sonnenschein', selectedYear);
    if (radioEt0) radioEt0.disabled = !ebeneHatJahr('et0', selectedYear);
    if (radioGdd) radioGdd.disabled = !ebeneHatJahr('gdd', selectedYear);
    if (radioWachstumspotenzialRate) radioWachstumspotenzialRate.disabled = !ebeneHatJahr('wachstumspotenzial_rate', selectedYear);
    if (radioWachstumspotenzialKum) radioWachstumspotenzialKum.disabled = !ebeneHatJahr('wachstumspotenzial_kum', selectedYear);
    schnittRadios.forEach(function(r) { r.disabled = !ebeneHatJahr(r.value, selectedYear); });
    var schnittWeg = schnittRadios.some(function(r) { return hintergrundEbene === r.value && r.disabled; });
    if (schnittWeg || (hintergrundEbene === 'niederschlag' && radioNiederschlag && radioNiederschlag.disabled) ||
        (hintergrundEbene === 'boden' && radioBoden && radioBoden.disabled) ||
        (hintergrundEbene === 'temperatur' && radioTemperatur && radioTemperatur.disabled) ||
        (hintergrundEbene === 'bodentemperatur' && radioBodentemperatur && radioBodentemperatur.disabled) ||
        (hintergrundEbene === 'sonnenschein' && radioSonnenschein && radioSonnenschein.disabled) ||
        (hintergrundEbene === 'et0' && radioEt0 && radioEt0.disabled) ||
        (hintergrundEbene === 'gdd' && radioGdd && radioGdd.disabled) ||
        (hintergrundEbene === 'wachstumspotenzial_rate' && radioWachstumspotenzialRate && radioWachstumspotenzialRate.disabled) ||
        (hintergrundEbene === 'wachstumspotenzial_kum' && radioWachstumspotenzialKum && radioWachstumspotenzialKum.disabled)) {
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
  // Pfeil auf dem Farbverlaufs-Balken, der die Position des Werts am Cursor
  // zeigt (zeigeWertAmPunkt()) - legendeBereich ist der [min,max]-Wertebereich
  // der gerade aktiven Ebene, fuer die Prozent-Umrechnung.
  var legendePfeilEl = null;
  var legendeBereich = null;
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
    // Ring wie der AFC-Ring auf der Karte: conic-gradient beginnt bei 12 Uhr
    // und laeuft im Uhrzeigersinn von 0 bis 1500 kg - 0 und 1500 liegen also
    // beide oben, dort markiert ein Strich den Nullpunkt (mit Beschriftung).
    // Der Zielbereich der Woche: duenne Striche an den Grenzen plus ein
    // gebogener Doppelpfeil aussen am Ring.
    var ringWrap = document.createElement('div');
    ringWrap.className = 'gw-afc-ring-wrap';
    var ring = document.createElement('div');
    ring.className = 'gw-afc-ring';
    ring.style.background = 'conic-gradient(' + verlauf.farben.join(',') + ')';
    ringWrap.appendChild(ring);
    var NS = 'http://www.w3.org/2000/svg';
    var svg = document.createElementNS(NS, 'svg');
    svg.setAttribute('class', 'gw-afc-ring-svg');
    svg.setAttribute('width', '80'); svg.setAttribute('height', '80'); svg.setAttribute('viewBox', '0 0 80 80');
    var el = function(name, attrs) { var e = document.createElementNS(NS, name); Object.keys(attrs).forEach(function(k) { e.setAttribute(k, attrs[k]); }); return e; };
    var defs = el('defs', {});
    var marker = el('marker', { id: 'gw-afc-pfeil', viewBox: '0 0 10 10', refX: '6', refY: '5', markerWidth: '5', markerHeight: '5', orient: 'auto-start-reverse' });
    marker.appendChild(el('path', { d: 'M0,0 L10,5 L0,10 z', fill: '#000' }));
    defs.appendChild(marker); svg.appendChild(defs);
    svg.appendChild(el('line', { x1: 40, y1: 9, x2: 40, y2: 24, stroke: '#000', 'stroke-width': 2 }));
    var punkt = function(wert, r) { var a = wert / 1500 * 2 * Math.PI; return [40 + r * Math.sin(a), 40 - r * Math.cos(a)]; };
    // Striche an Zielunter-/-obergrenze quer ueber das Farbband, wie auf den
    // AFC-Ringen der Karte - der Pfeil aussen verbindet sie.
    [verlauf.low, verlauf.high].forEach(function(wert) {
      var innen = punkt(wert, 16), aussen = punkt(wert, 30);
      svg.appendChild(el('line', { x1: innen[0], y1: innen[1], x2: aussen[0], y2: aussen[1], stroke: '#000', 'stroke-width': 1.5 }));
    });
    var p1 = punkt(verlauf.low, 34), p2 = punkt(verlauf.high, 34);
    var gross = (verlauf.high - verlauf.low) / 1500 > 0.5 ? 1 : 0;
    svg.appendChild(el('path', { d: 'M' + p1[0] + ',' + p1[1] + ' A34,34 0 ' + gross + ',1 ' + p2[0] + ',' + p2[1],
      fill: 'none', stroke: '#000', 'stroke-width': 1.5, 'marker-start': 'url(#gw-afc-pfeil)', 'marker-end': 'url(#gw-afc-pfeil)' }));
    ringWrap.appendChild(svg);
    var ringZeile = document.createElement('div');
    var nullEl = document.createElement('div');
    nullEl.className = 'gw-afc-null';
    nullEl.textContent = '0 / 1500 kg';
    ringZeile.appendChild(nullEl);
    ringZeile.appendChild(ringWrap);
    var ziel = document.createElement('div');
    ziel.className = 'gw-layer-legende-quelle';
    ziel.textContent = 'DGV im Uhrzeigersinn ab 0 (oben). Pfeil = Zielbereich dieser Woche: ' + verlauf.low + '–' + verlauf.high + ' kg TS/ha';
    afcLegendeBox.appendChild(ringZeile);
    afcLegendeBox.appendChild(ziel);
  }

  // \"Tage seit Messung\"-Legende (Graufaerbung von Graswachstum-Kreis/AFC-Ring)
  // als kompakte Box im Ebenen-Kasten statt als Plotly-natives colorbar auf
  // der Karte selbst - Letzteres kollidierte dort mit dem (nur bei Hover
  // sichtbaren) Modebar-Bereich und war auf der Karte zudem schwer zu finden.
  // Dieselbe Sichtbarkeitsregel wie die Hover-Marker-Trace selbst
  // (graswachstumOn || afcOn, siehe applyMapState()) - die Faerbung gehoert
  // zu BEIDEN Ebenen gemeinsam, nicht nur zu AFC.
  var tageSeitMessungBox = null;
  function aktualisiereTageSeitMessungLegende() {
    if (!tageSeitMessungBox) return;
    if (!graswachstumOn && !afcOn) { tageSeitMessungBox.style.display = 'none'; return; }
    tageSeitMessungBox.style.display = 'block';
  }
  // Kartentitel: fester Kopf \"Graswachstum\", darunter Kalenderwoche und
  // Grafikdatum (Erstellung der Seite). Ist eine Meteo-Ebene aktiv, folgt deren
  // Name mit dem tatsaechlichen Datenstand (werte.bis) - der kann, v.a. in der
  // neuesten Woche, wegen Publikationsverzoegerung hinterherhinken.
  function aktualisiereKartentitel() {
    var growthMapGd = document.querySelector('#datenexplorer-growthmap .js-plotly-plot');
    if (!growthMapGd) return;
    // Kurz (auch fuer Mobile): fester Titel, darunter Woche und Grafikdatum,
    // bei aktiver Meteo-Ebene eine dritte Zeile mit Ebene und Datenstand.
    var unterzeile = 'KW ' + selectedWeek + ' ' + selectedYear + ' · Grafik vom ' + grafikDatum;
    var ebenenZeile = '';
    var titel = '<b>Graswachstum</b><br><span style=\"font-size:12px\">' + unterzeile + '</span>';
    var zeilen = 2;
    if (hintergrundEbene !== 'keine') {
      // Bodenwasserbilanz traegt ihr \"(berechnet) <Datum>\" bereits im
      // Radio-Label (siehe aktualisiereLayerLabels()) - hier deshalb die
      // Basis-Bezeichnung OHNE das Datum verwenden, das unten per \"Stand
      // ...\" ohnehin einmal dazukommt. Sonst stuende dasselbe Datum zweimal
      // im Titel.
      var radio = radioJeEbene[hintergrundEbene];
      var label = (hintergrundEbene === 'boden')
        ? (layerLegenden.boden.label + ' (berechnet)')
        : (hintergrundEbene === 'wachstumspotenzial_rate' || hintergrundEbene === 'wachstumspotenzial_kum')
        ? (layerLegenden[hintergrundEbene].label + ' (experimentell, Erholung ' + erholung + ' Tage)')
        : ((radio && radio.labelTextEl) ? radio.labelTextEl.textContent
           : (layerLegenden[hintergrundEbene] ? layerLegenden[hintergrundEbene].label : hintergrundEbene));
      var cacheEintrag = ebenenCache[ebeneDateiSchluessel(hintergrundEbene)];
      var werteEintrag = cacheEintrag && cacheEintrag.werte && cacheEintrag.werte[selectedYear + ' ' + selectedWeek];
      var stand = (werteEintrag && werteEintrag.bis) ? ('Stand ' + werteEintrag.bis) : 'lädt…';
      ebenenZeile = label + ', ' + stand;
      titel += '<br><span style=\"font-size:11px\">' + ebenenZeile + '</span>';
      zeilen = 3;
    }
    if (mobilKopfUnterEl) mobilKopfUnterEl.textContent = unterzeile + (ebenenZeile ? ' · ' + ebenenZeile : '');
    if (istMobil()) Plotly.relayout(growthMapGd, { 'title.text': '', 'margin.t': 6 });
    else Plotly.relayout(growthMapGd, { 'title.text': titel, 'margin.t': zeilen === 3 ? 74 : 58 });
  }

  function aktualisiereLayerLegende() {
    aktualisiereMeteoFensterSichtbarkeit();
    aktualisiereKartentitel();
    if (!layerLegendeBox) return;
    var info = layerLegenden[hintergrundEbene];
    aktualisiereKartenleiste(info);
    if (!info) { layerLegendeBox.style.display = 'none'; wertAnzeigeEl = null; koordinatenEl = null; ortschaftEl = null; legendePfeilEl = null; legendeBereich = null; return; }
    layerLegendeBox.style.display = 'block';
    layerLegendeBox.innerHTML = '';
    // Wertebereich bei Summen-Ebenen (info.fensterSkaliert) proportional zur
    // aktuellen Fenstergroesse hochskaliert (siehe R: baue_fenster_ebenen())
    // - bei Mittelwert-Ebenen bleibt der Bereich unveraendert.
    var bereich = info.fensterSkaliert ? [info.bereich[0], Math.round(info.bereich[1] * meteoFenster / 7)] : info.bereich;
    legendeBereich = bereich;
    var balkenWrap = document.createElement('div');
    balkenWrap.className = 'gw-layer-legende-balken-wrap';
    var balken = document.createElement('div');
    balken.className = 'gw-layer-legende-balken';
    balken.style.background = 'linear-gradient(to right, ' + info.farben.join(',') + ')';
    // Pfeil zeigt die Position des Werts am Cursor auf dem Farbverlauf -
    // Positionierung/Sichtbarkeit uebernimmt zeigeWertAmPunkt()/
    // versteckeWertAnzeige(), hier nur frisch angelegt und initial versteckt.
    legendePfeilEl = document.createElement('div');
    legendePfeilEl.className = 'gw-legende-pfeil';
    legendePfeilEl.style.display = 'none';
    balkenWrap.appendChild(balken);
    balkenWrap.appendChild(legendePfeilEl);
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
    setzeWertText('Wert am Cursor: –');
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
    layerLegendeBox.appendChild(balkenWrap);
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
  // Positioniert den Pfeil auf dem Farbverlaufs-Balken proportional zum Wert
  // innerhalb legendeBereich ([min,max] der aktiven Ebene) - versteckt ihn,
  // wenn kein gueltiger Zahlenwert vorliegt (keine Daten/ausserhalb der
  // Schweiz/keine Ebene aktiv).
  function aktualisierePfeilPosition(wert) {
    if (!legendePfeilEl) return;
    if (wert === null || wert === undefined || isNaN(wert) || !legendeBereich) {
      legendePfeilEl.style.display = 'none';
      return;
    }
    var anteil = (wert - legendeBereich[0]) / (legendeBereich[1] - legendeBereich[0]);
    anteil = Math.max(0, Math.min(1, anteil));
    legendePfeilEl.style.left = (anteil * 100) + '%';
    legendePfeilEl.style.display = 'block';
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
    if (!gitter || !info) { aktualisierePfeilPosition(null); return; }
    var col = Math.floor((lon - gitter.x0) / (gitter.x1 - gitter.x0) * gitter.ncol);
    var row = Math.floor((gitter.y1 - lat) / (gitter.y1 - gitter.y0) * gitter.nrow);
    if (col < 0 || col >= gitter.ncol || row < 0 || row >= gitter.nrow) {
      setzeWertText('Wert am Cursor: ' + (info.ausserhalb || 'ausserhalb der Schweiz'));
      aktualisierePfeilPosition(null);
      return;
    }
    var wert = gitter.m[row][col];
    setzeWertText((wert === null || wert === undefined) ?
      'Wert am Cursor: keine Daten' : 'Wert am Cursor: ' + wert + ' ' + info.einheit);
    aktualisierePfeilPosition(wert);
  }
  function versteckeWertAnzeige() {
    setzeWertText('Wert am Cursor: –');
    if (koordinatenEl) koordinatenEl.textContent = 'Koordinaten: –';
    if (ortschaftEl) ortschaftEl.textContent = 'Ort: –';
    aktualisierePfeilPosition(null);
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
    var ebenenZuZeile = document.createElement('div');
    ebenenZuZeile.className = 'gw-ebenen-zu-zeile gw-mobil-only';
    var ebenenZu = document.createElement('button'); ebenenZu.type = 'button'; ebenenZu.className = 'gw-blatt-zu'; ebenenZu.textContent = '×';
    ebenenZu.setAttribute('aria-label', 'Ebenen schliessen');
    ebenenZu.addEventListener('click', function(evt) { evt.stopPropagation(); document.body.classList.remove('gw-ebenen-offen'); });
    ebenenZuZeile.appendChild(layerHeading); ebenenZuZeile.appendChild(ebenenZu);
    layerPanel.appendChild(ebenenZuZeile);
    var layerHeadingDesktop = layerHeading.cloneNode(true);
    layerHeadingDesktop.classList.add('gw-desktop-only');
    layerPanel.insertBefore(layerHeadingDesktop, ebenenZuZeile);

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

    // Kleiner i-Knopf mit Klapp-Popup fuer laengere Erklaerungstexte (die
    // Quellenangabe als nativer title-Tooltip reicht fuer eine ganze
    // Absatz-Erklaerung nicht) - per Klick statt nur Hover, damit es auch
    // auf Touch-Geraeten funktioniert; ein Klick ausserhalb schliesst das
    // Popup wieder.
    function macheInfoKnopf(text, zusatz, titel) {
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
        if (istMobil()) {
          var inhalt = document.createElement('div');
          if (zusatz) { var z = zusatz(); if (z) inhalt.appendChild(z); }
          var p = document.createElement('div'); p.className = 'gw-blatt-text'; p.textContent = text;
          inhalt.appendChild(p);
          zeigeBlatt(titel || 'Erklaerung', '', inhalt);
          return;
        }
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
    // eines Radiobuttons - fuer Graswachstum/DGV/MeteoSchweiz-Stationen, die
    // (anders als die Hintergrund-Raster-Ebenen) unabhaengig VONEINANDER
    // ein-/ausblendbar sein sollen, nicht als Radiogruppe.
    function macheLayerToggle(labelText, checked, onChange, erklaerung, zusatzKlasse, zusatzInfo) {
      var zeile = document.createElement('div');
      zeile.className = 'gw-layer-option-zeile' + (zusatzKlasse ? ' ' + zusatzKlasse : '');
      var toggleWrap = schalterLinksbuendig(makeToggle(labelText, checked, onChange));
      zeile.appendChild(toggleWrap);
      if (erklaerung) zeile.appendChild(macheInfoKnopf(erklaerung, zusatzInfo, labelText));
      layerPanel.appendChild(zeile);
      return toggleWrap;
    }
    // Reihenfolge Graswachstum / DGV / MeteoSchweiz-Stationen: die beiden
    // Betriebs-Ebenen (Graswachstum-Kreis, DGV-Ring) zuerst, je eigener
    // Schalter - danach MeteoSchweiz-Stationen als reine Wetter-
    // Referenzebene. DGV = Durchschnittlicher GrasVorrat, der intern/in der
    // Erklaerung weiterhin als AFC (Average Farm Cover) referenzierte
    // Fachbegriff.
    macheLayerToggle('Graswachstum (kg TS/ha/Tag)', true, function(checked) { graswachstumOn = checked; applyState(); },
      'Die Zahl im Kreis zeigt das zuletzt gemessene Graswachstum in kg TS/ha/Tag (Trockensubstanz-Zuwachs pro Hektare und Tag). Die Graufaerbung des Kreises zeigt, wie lange die Messung zurueckliegt: weiss = frisch gemessen (0 Tage), dunkelgrau = bis zu 14 Tage alt. Standorte ohne Messung in den letzten 14 Tagen werden nicht mehr angezeigt.');
    macheLayerToggle('DGV (kg TS/ha)', true, function(checked) { afcOn = checked; applyState(); },
      'DGV (Durchschnittlicher GrasVorrat, international AFC = Average Farm Cover) schaetzt den aktuellen Grasvorrat des Betriebs in kg Trockensubstanz pro Hektare (kg TS/ha). Der Ring zeigt diesen Vorrat als Fortschrittsbalken auf einer Skala von 0 bis 1500 kg TS/ha und faerbt ihn nach dem jahreszeitlichen Zielbereich: rot = deutlich zu wenig (unter 200 kg praktisch leer), gruen = im Zielbereich, blaugruen = deutlich mehr als noetig. Der Zielbereich verschiebt sich uebers Jahr, z.B. Fruehling ca. 500-700, Sommer ca. 700-800, Herbst ca. 900-1200 kg TS/ha.', null,
      function() { if (!afcLegendeBox || afcLegendeBox.style.display === 'none') return null; var c = afcLegendeBox.cloneNode(true); c.className = 'gw-layer-legende'; c.style.display = 'block'; c.style.marginBottom = '10px'; return c; });

    // Kompakte DGV-Legende (Ring) DIREKT nach dem DGV-Schalter - gehoert
    // inhaltlich dazu. aktualisiereAfcLegende() (siehe unten) blendet die
    // Box aus, sobald DGV ausgeschaltet ist.
    afcLegendeBox = document.createElement('div');
    afcLegendeBox.className = 'gw-layer-legende gw-afc-legende-box';
    afcLegendeBox.style.display = 'none';
    layerPanel.appendChild(afcLegendeBox);

    // \"Tage seit Messung\" (Graufaerbung Graswachstum-Kreis/AFC-Ring) - statt
    // eines Plotly-nativen Colorbars auf der Karte (kollidierte dort mit dem
    // Hover-Modebar-Bereich) als kompakte, statische Box direkt hier neben
    // Graswachstum/DGV. Inhalt aendert sich nie (fixe Skala 0-14 Tage), daher
    // einmalig aufgebaut statt bei jedem Wochenwechsel neu gerendert.
    tageSeitMessungBox = document.createElement('div');
    tageSeitMessungBox.className = 'gw-layer-legende';
    tageSeitMessungBox.style.display = 'none';
    var tsmBalkenWrap = document.createElement('div');
    tsmBalkenWrap.className = 'gw-layer-legende-balken-wrap';
    var tsmBalken = document.createElement('div');
    tsmBalken.className = 'gw-layer-legende-balken';
    tsmBalken.style.background = 'linear-gradient(to right, white, #757575)';
    tsmBalkenWrap.appendChild(tsmBalken);
    var tsmSkala = document.createElement('div');
    tsmSkala.className = 'gw-layer-legende-skala';
    var tsmMinEl = document.createElement('span'); tsmMinEl.textContent = '0';
    var tsmMaxEl = document.createElement('span'); tsmMaxEl.textContent = '14 Tage';
    tsmSkala.appendChild(tsmMinEl);
    tsmSkala.appendChild(tsmMaxEl);
    var tsmLabel = document.createElement('div');
    tsmLabel.className = 'gw-layer-legende-quelle';
    tsmLabel.textContent = 'Tage seit Messung (Graswachstum/DGV)';
    tageSeitMessungBox.appendChild(tsmBalkenWrap);
    tageSeitMessungBox.appendChild(tsmSkala);
    tageSeitMessungBox.appendChild(tsmLabel);
    layerPanel.appendChild(tageSeitMessungBox);

    macheLayerToggle('MeteoSchweiz-Stationen', false, function(checked) { smnStationenOn = checked; aktualisiereSmnStationen(); },
      'Zeigt die oeffentlichen MeteoSchweiz-Automatikstationen (SwissMetNet) mit ihren aktuellsten Tageswerten (Lufttemperatur, Bodentemperatur, Niederschlag, Globalstrahlung, Sonnenscheindauer) als Diamant-Symbole. Reine Wetter-Referenzstationen, unabhaengig von der gewaehlten Kalenderwoche und NICHT Teil der AGFF-Grasmessungen. Bodentemperatur wird nur an einem Teil der rund 150 Stationen gemessen - dort steht im Tooltip entsprechend keine Daten.');

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
      if (erholungWrap) erholungWrap.style.display = istErholungsEbene(hintergrundEbene) ? 'block' : 'none';
    }
    var erholungWrap = null;
    function macheErholungsSchieberegler() {
      erholungWrap = document.createElement('div');
      erholungWrap.className = 'gw-meteo-fenster';
      erholungWrap.title = 'Eigene Erweiterung, nicht Teil von ModVege: Nach Trockenheit steigt der Wasserstress-Faktor fuers Wachstum hoechstens so schnell, dass die volle Erholung diese Anzahl Tage dauert. 0 = unveraendertes Modell (springt nach Regen sofort zurueck).';
      var label = document.createElement('div');
      label.className = 'gw-meteo-fenster-label';
      var input = document.createElement('input');
      input.type = 'range';
      input.min = '0';
      input.max = String(erholungStufen.length - 1);
      input.step = '1';
      input.value = String(erholungStufen.indexOf(erholung));
      function zeige() { label.textContent = 'Erholung nach Trockenheit: ' + erholung + ' Tage'; }
      input.addEventListener('input', function() {
        erholung = erholungStufen[parseInt(input.value, 10)];
        zeige();
        aktualisiereHintergrundEbene();
        aktualisiereLayerLegende();
      });
      erholungWrap.appendChild(label);
      erholungWrap.appendChild(input);
      layerPanel.appendChild(erholungWrap);
      zeige();
      aktualisiereMeteoFensterSichtbarkeit();
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
      if (erklaerung) zeile.appendChild(macheInfoKnopf(erklaerung, null, labelText));
      layerPanel.appendChild(zeile);
      return radio;
    }
    // Ueberschrift statt Trennlinie: macht den Abschnittswechsel von den
    // Standort-/Stations-Schaltern oben zu den flaechendeckenden MeteoSchweiz-
    // Gitterdaten-Ebenen klar, ohne zusaetzlich eine Trennlinie zu brauchen.
    // \"Gitterdatensatz\" ist MeteoSchweiz' eigener Fachbegriff fuer diese
    // raeumlich interpolierten Produkte (RhiresD/TabsD/SrelD/...).
    var meteoGitterHeading = document.createElement('div');
    meteoGitterHeading.className = 'gw-layer-heading';
    meteoGitterHeading.style.marginTop = '10px';
    meteoGitterHeading.textContent = 'MeteoSchweiz-Gitterdaten';
    layerPanel.appendChild(meteoGitterHeading);
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
    // Potenzielles Wachstum (ModVege/growR): experimentell, nur mit
    // ?experimentell in der URL sichtbar (siehe wachstumspotenzial_
    // freigeschaltet im R-Teil fuer die bekannten Schwaechen).
    if (experimentellerModus) {
      radioWachstumspotenzialRate = makeLayerRadio('wachstumspotenzial_rate', layerLegenden.wachstumspotenzial_rate.label,
        'EXPERIMENTELL. Zeigt, wie viel Graswachstum das Klima (Temperatur, Strahlung, Wasserhaushalt) diese Woche pro Pixel maximal zulassen wuerde - ohne Naehrstofflimitierung und ohne Schnitt/Beweidung. Berechnet mit ModVege (Jouven et al. 2006, R-Paket growR). Bekannte Schwaechen: nach Regen auf eine Trockenperiode springt das Modell sofort auf volles Potenzial zurueck (reale Wiesen brauchen dafuer Wochen), und Grundwasserboeden werden nicht abgebildet.');
      radioWachstumspotenzialKum = makeLayerRadio('wachstumspotenzial_kum', layerLegenden.wachstumspotenzial_kum.label,
        'EXPERIMENTELL. Wie Potenzielles Wachstum, aber seit 1. Januar aufsummiert - zeigt, wie viel sich uebers Jahr an klimatisch moeglichem (ungenutztem) Wachstum angesammelt hat. Gleiche bekannte Schwaechen bei Trockenheit.');
    }
    // Schnittanalyse Testgebiet (experimentell): Radios nur, wenn die Daten
    // vorliegen (siehe R: schnittanalyse_index). Beim Auswaehlen zoomt die
    // Karte auf das Testgebiet - auf der Schweizkarte waere es nur ein Punkt.
    if (experimentellerModus && schnittanalyseGebiet) {
      var saHeading = document.createElement('div');
      saHeading.className = 'gw-layer-heading';
      saHeading.style.marginTop = '10px';
      saHeading.textContent = 'Schnittanalyse ' + schnittanalyseGebiet.name;
      layerPanel.appendChild(saHeading);
      Object.keys(ebenenSchluessel).filter(istSchnittEbene).forEach(function(n) {
        var r = makeLayerRadio(n, layerLegenden[n].label, 'EXPERIMENTELL. ' + layerLegenden[n].quelle + ' Stand jeweils bis Montag der gewaehlten Woche.');
        r.addEventListener('change', function() {
          if (!r.checked) return;
          var gd = document.querySelector('#datenexplorer-growthmap .js-plotly-plot');
          var g = schnittanalyseGebiet, rand = 0.15;
          var dx = (g.lon1 - g.lon0) * rand, dy = (g.lat1 - g.lat0) * rand;
          if (gd) Plotly.relayout(gd, { 'xaxis.range': [g.lon0 - dx, g.lon1 + dx], 'yaxis.range': [g.lat0 - dy, g.lat1 + dy] });
        });
        schnittRadios.push(r);
      });
    }
    // Nachschlagetabelle Ebenenname -> Radio, fuer aktualisiereLayerLabels()
    // (haengt dort das Symbol/die Fenstergroesse an alle 5 Fenster-Ebenen).
    radioJeEbene = { niederschlag: radioNiederschlag, temperatur: radioTemperatur, bodentemperatur: radioBodentemperatur, sonnenschein: radioSonnenschein, et0: radioEt0 };
    macheMeteoFensterSchieberegler();
    if (experimentellerModus) macheErholungsSchieberegler();
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
  fillHost.className = 'gw-kurvenbereich';
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
  # Saisonverlauf je Standort und Jahr fuer die Mini-Kurve im Standortblatt
  "__STANDORT_VERLAEUFE__" = jsonlite::toJSON(lapply(split(daten_korr, as.character(daten_korr$Ort)), function(d)
    lapply(split(d, d$year), function(dj) unname(lapply(order(dj$date), function(k)
      c(as.integer(format(dj$date[k], "%j")), round(dj$growth[k])))))), auto_unbox = FALSE),
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
  "__SCHNITTANALYSE_GEBIET__" = if (is.null(schnittanalyse_gebiet)) "null" else jsonlite::toJSON(schnittanalyse_gebiet, auto_unbox = TRUE, digits = NA),
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
  "__GRAFIK_DATUM__" = format(Sys.Date(), "%d.%m.%Y"),
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
  htmltools::div(id = "gw-seite", style = "font-family: sans-serif; max-width: 1400px; margin: 0 auto; padding: 20px;",
    htmltools::div(id = "gw-kartenzeile", style = "display: flex; gap: 20px; flex-wrap: wrap; align-items: flex-start;",
      htmltools::div(id = "datenexplorer-growthmap", style = "flex: 1 1 700px; min-width: 320px; height: 560px; overflow: hidden;", fig_wachstum),
      # class statt nur inline-style: flex-grow:1 auf BEIDEN Geschwistern
      # (Karte UND Ebenen-Box) verteilte uebrigen Platz 50/50 statt der Karte
      # allein zugutekommen zu lassen - die Ebenen-Box wurde dadurch auf
      # breiten Bildschirmen viel breiter als beabsichtigt (sichtbar z.B.
      # bei max-width:1400px). gw-map-controls-panel (siehe Styles weiter
      # unten) setzt flex-grow auf 0 (Karte absorbiert den ganzen Rest),
      # ausser im Mobile-Stack-Layout (dort wieder 1, siehe @media 700px),
      # damit die Box dort weiterhin ihre volle Zeile ausfuellt.
      htmltools::div(id = "datenexplorer-map-controls", class = "gw-map-controls-panel")
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
