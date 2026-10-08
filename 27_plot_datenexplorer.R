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
# art: woraus die Ebene gerechnet wird - "meteo" (MeteoSchweiz-Dateien) oder
# "daten" (AGFF-Messungen); bestimmt den Fingerabdruck fuer den Schnellmodus.
gespeicherte_caches <- list()
speichere_ebenen_cache <- function(name, cache, art = "meteo") {
  saveRDS(cache, file.path(ebenen_cache_dir, paste0(name, ".rds")))
  gespeicherte_caches[[name]] <<- art
}

## Schnellmodus (GRASSGROWTH_SCHNELL=1, z.B. nach einem Code-Deploy): auch das
## LAUFENDE Jahr kommt aus dem Cache, wenn sich seit dem letzten vollstaendigen
## Lauf weder die Eingangsdaten (MeteoSchweiz-Dateien bzw. AGFF-Messungen)
## noch der R-Rechencode geaendert haben. Das Frontend (frontend/*.js, .css)
## zaehlt nicht dazu - reine Darstellungsaenderungen brauchen kein
## Neurechnen. Ohne die Variable (naechtlicher Lauf) wird wie bisher alles
## frisch gerechnet; die Staende werden am Ende des Laufs festgehalten.
schnellmodus <- identical(Sys.getenv("GRASSGROWTH_SCHNELL"), "1")
staende_datei <- file.path(ebenen_cache_dir, "_staende.rds")
staende_alt <- if (file.exists(staende_datei)) tryCatch(readRDS(staende_datei), error = function(e) list()) else list()
code_stand <- digest::digest(paste(readLines("27_plot_datenexplorer.R", warn = FALSE), collapse = "\n"), algo = "md5")
geodata_stand <- function() {
  f <- list.files(geodata_dir, recursive = TRUE, full.names = TRUE)
  i <- file.info(f)
  digest::digest(paste(f, i$size, as.numeric(i$mtime), collapse = "\n"), algo = "md5")
}
geodata_stand_jetzt <- if (schnellmodus) geodata_stand() else NA_character_
daten_stand <- NA_character_ # wird nach dem Laden der Messdaten gesetzt
eingangsstand <- function(art) paste(code_stand, if (art == "daten") daten_stand else geodata_stand_jetzt)
# TRUE = dieses Jahr der Ebene aus dem Cache uebernehmen
jahr_aus_cache <- function(name, jahr, art = "meteo") {
  jahr < aktuelles_kalenderjahr ||
    (schnellmodus && !is.null(name) && identical(staende_alt[[name]], eingangsstand(art)))
}
if (schnellmodus) cat("Schnellmodus: laufendes Jahr aus dem Cache, sofern Daten und Rechencode unveraendert\n")
# Fuer die drei Bild-Ebenen ausserhalb von baue_fenster_ebenen() (siehe
# dort fuer die aufwendigere Variante, die bei einem komplett gecachten
# abgeschlossenen Jahr zusaetzlich auch dessen Rohraster gar nicht erst
# laedt): liefert den gecachten Eintrag nur fuer ein abgeschlossenes Jahr,
# sonst NULL (immer frisch berechnen).
cache_eintrag_holen <- function(cache, jahr, woche, name = NULL, art = "meteo") {
  if (!jahr_aus_cache(name, jahr, art)) return(NULL)
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
daten_stand <- digest::digest(daten_korr, algo = "md5")

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
  cached <- cache_eintrag_holen(graswachstum_afc_cache_alt, jr, w, "graswachstum_afc", "daten")
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
speichere_ebenen_cache("graswachstum_afc", graswachstum_afc_cache_neu, art = "daten")
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
fig_wachstum <- htmlwidgets::onRender(fig_wachstum, sprintf(
  "function(el, x) { GWDatenexplorer.karte(el, x, %s); }",
  jsonlite::toJSON(list(
    xMin = lon_range_erweitert[1], xMax = lon_range_erweitert[2], yMitte = mean(lat_range),
    scaleratio = karten_scaleratio, xMaxSchweiz = lon_range[2] + diff(lon_range) * 0.01,
    ySpanSchweiz = diff(lat_range)
  ), auto_unbox = TRUE, digits = NA)))

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
      if (jahr_aus_cache(cache_name, jr) && length(alte_schluessel_jahr) > 0) {
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
    cached <- cache_eintrag_holen(boden_cache_alt, jr, w, "boden")
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
  # Aus dem Cache, wenn ALLE Stufen dort vorhanden sind - abgeschlossene Jahre
  # immer, das laufende Jahr nur im Schnellmodus bei unveraendertem Stand.
  wsp_namen <- as.vector(outer(c("rate", "kum"), paste0("e", erholung_stufen_tage), function(a, s) paste0("wachstumspotenzial_", a, "_", s)))
  if (all(vapply(wsp_namen, function(n) jahr_aus_cache(n, jr), logical(1)))) {
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
  if (jahr_aus_cache("gdd", jr) && length(alte_schluessel_jahr) > 0) {
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

# Kurven-Logik, Karten-Logik und Stile liegen in frontend/datenexplorer.js
# und frontend/datenexplorer.css (eingebunden weiter unten als Abhaengigkeit).

# Standorte mit regelmaessigen Messungen je Jahr (mindestens 8 Kalenderwochen
# mit Wachstumswert): die Kurve zeigt beim Start nur diese, die uebrigen sind
# zuschaltbar (JS: siteInAuswahl()). 0-basierte Indizes in alle_orte.
regelmaessig_basis <- daten_korr[!is.na(daten_korr$growth) &
  (is.na(daten_korr$ignore) | daten_korr$ignore %in% c(FALSE, 0, "", "FALSE")), ]
regelmaessig_je_jahr <- lapply(split(regelmaessig_basis, as.character(regelmaessig_basis$year)), function(dj) {
  wochen <- tapply(as.integer(strftime(dj$date, "%V")), as.character(dj$Ort), function(w) length(unique(w)))
  I(sort(match(names(wochen)[wochen >= 8], alle_orte) - 1L))
})

js_ersetzungen <- list(
  "__REGELMAESSIG__" = jsonlite::toJSON(regelmaessig_je_jahr),
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
# Daten fuer GWDatenexplorer.kurve() als Objekt-Literal: Schluessel ohne die
# __-Klammern, Werte als JSON (wie bisher erzeugt), Texte als JSON-String.
js_als_text <- c("__GRAFIK_DATUM__")
kurve_daten_js <- paste0("{\n", paste0("  ", gsub("^__|__$", "", names(js_ersetzungen)), ": ",
  vapply(names(js_ersetzungen), function(n) {
    v <- js_ersetzungen[[n]]
    if (n %in% js_als_text) as.character(jsonlite::toJSON(as.character(v), auto_unbox = TRUE)) else as.character(v)
  }, character(1)), collapse = ",\n"), "\n}")
fig_kurve <- htmlwidgets::onRender(fig_kurve, paste0("function(el, x) { GWDatenexplorer.kurve(el, x, ", kurve_daten_js, "); }"))

# Frontend als eigene Abhaengigkeit an beide Widgets: save_html() kopiert es
# nach lib/gw-datenexplorer-<version>/, die Einbettung listet es mit. Die
# Version folgt dem Inhalt, damit Browser nach Aenderungen neu laden.
frontend_dateien <- file.path("frontend", c("datenexplorer.js", "datenexplorer.css"))
frontend_version <- paste0("1.", strtoi(substr(digest::digest(
  paste(unlist(lapply(frontend_dateien, readLines, warn = FALSE)), collapse = "\n"), algo = "md5"), 1, 7), 16L))
gw_frontend <- htmltools::htmlDependency("gw-datenexplorer", frontend_version,
  src = c(file = normalizePath("frontend")), script = "datenexplorer.js", stylesheet = "datenexplorer.css")
fig_wachstum$dependencies <- c(fig_wachstum$dependencies, list(gw_frontend))
fig_kurve$dependencies <- c(fig_kurve$dependencies, list(gw_frontend))
unlink(Sys.glob(file.path(out_dir, "lib", "gw-datenexplorer-*")), recursive = TRUE)

########################################################################
## 5. Seite zusammensetzen und speichern -------------------------------
########################################################################

seite_inhalt <- htmltools::div(id = "gw-seite", style = "font-family: sans-serif; max-width: 1400px; margin: 0 auto; padding: 20px;",
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
  seite_inhalt
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

## Einbettung in eine andere Seite (Grav-Plugin datenexplorer): dieselbe App
## als HTML-Fragment plus Liste der Skripte/Stylesheets (relativ zu outputs/,
## save_html() hat sie eben nach lib/ kopiert). Der Lader auf der Website holt
## diese Datei, setzt Stylesheets und Fragment ein, laedt die Skripte und
## startet die Widgets - die App erscheint so im Seitenrahmen der Website.
einbettung_deps <- lapply(
  htmltools::resolveDependencies(htmltools::findDependencies(seite)),
  function(d) htmltools::makeDependencyRelative(
    htmltools::copyDependencyToDir(d, file.path(out_dir, "lib"), mustWork = FALSE), out_dir)
)
einbettung_dateien <- function(d, feld) {
  x <- d[[feld]]
  if (is.null(x) || length(x) == 0) return(character())
  x <- vapply(x, function(s) if (is.list(s)) s$src else s, character(1))
  utils::URLencode(file.path(d$src[["file"]], x))
}
einbettung <- list(
  stand = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  css = I(unname(unlist(lapply(einbettung_deps, einbettung_dateien, "stylesheet")))),
  js = I(unname(unlist(lapply(einbettung_deps, einbettung_dateien, "script")))),
  html = as.character(htmltools::renderTags(seite_inhalt)$html)
)
jsonlite::write_json(einbettung, file.path(out_dir, "Datenexplorer_einbettung.json"), auto_unbox = TRUE)
cat("Datenexplorer-Einbettung gespeichert in:", file.path(out_dir, "Datenexplorer_einbettung.json"), "\n")

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

## Stand der Ebenen-Caches festhalten (nur nach einem vollstaendigen Lauf) -
## erst hier am Ende, damit alle im Lauf heruntergeladenen MeteoSchweiz-Dateien
## schon im Fingerabdruck stecken. Grundlage fuer den Schnellmodus.
if (!schnellmodus) {
  geodata_stand_jetzt <- geodata_stand()
  staende_neu <- staende_alt
  for (n in names(gespeicherte_caches)) staende_neu[[n]] <- eingangsstand(gespeicherte_caches[[n]])
  saveRDS(staende_neu, staende_datei)
  cat("Cache-Staende festgehalten:", length(gespeicherte_caches), "Ebenen\n")
}
