# 26_plot_interaktiv_wachstum_niederschlag.R
#
# Interaktive Graswachstumskurve (Plotly, eigenständige HTML-Datei) mit
# MeteoSchweiz-Niederschlag als zweiter Achse (Wochen-Balken, Mittelwert +
# Min/Max-Spannweite als Fehlerbalken).
#
# Auswahl-Steuerelemente (eigene HTML-Controls oberhalb der Grafik, nicht
# Plotlys eingebaute "updatemenus" - sie brauchen gemeinsamen Zustand, siehe
# onRender()-Block unten):
#   - Durchsuchbare Combobox (Texteingabe + Vorschlagsliste) für "Alle
#     Standorte", die 3 Regionen (West/Mitte/Ost nach Längengrad) und die
#     4 Höhenlagen (<500/500-650/650-800/>800m) - diese stehen ganz oben in
#     der Liste. Einzelne Standorte stehen darunter, durch eine Trennlinie
#     abgesetzt ("versteckt", d.h. nicht auf den ersten Blick, aber per
#     Scrollen oder durch Tippen des Standortnamens direkt filterbar).
#   - Ist ein einzelner Standort gewählt: Niederschlag zeigt dessen eigene
#     MeteoSchweiz-Zeitreihe (an seiner Koordinate aus dem 1km-Raster
#     entnommen, kein Flächenmittel); die schwarze Mittelwertlinie wird
#     ausgeblendet (identisch mit der Standort-Linie selbst).
#   - Ist eine Gruppe gewählt (Alle/Region/Höhenlage): Niederschlag zeigt
#     Mittelwert + Spannweite über deren Standorte, Wachstum zeigt alle
#     Standorte der Gruppe einzeln plus die schwarze Mittelwertlinie.
#   - Titel ("Graswachstumskurve <Jahr>") wird als eigenes HTML-Element
#     über der Grafik eingefügt (nicht von Plotly selbst gezeichnet),
#     damit die Bedienelemente sauber zwischen Titel und Diagramm
#     platziert werden können.
#
# Legende als eigene HTML-Sidebar rechts neben dem Diagramm (nicht Plotlys
# eingebaute Legende, showlegend=FALSE), standardmässig eingeblendet -
# ähnlich der rechten Sidebar auf openstreetmap.org: ein schmaler Rand mit
# Umschalt-Icon bleibt immer sichtbar, das eigentliche Panel klappt
# seitwärts ein/aus (Schliessen auch über X im Panel). Oberhalb der
# Standortfarben-Liste liegen dort auch die Toggle-Switches:
#   - "Niederschlag an/aus" schaltet die entsprechende Niederschlags-
#     Darstellung unabhängig von der Auswahl ein/aus.
#   - "Kalenderwochen" (standardmässig eingeschaltet) fürs x-Achsen-Format:
#     ein = Kalenderwoche, aus = Montagsdatum der Woche (in beiden Fällen
#     nur alle 5 Wochen ein beschrifteter Tick).
# Der Legendeninhalt (Standortfarben, "Mittleres Wachstum", "Durchschnitt
# Mittelland") wird bei jeder Auswahländerung neu aufgebaut, passend zu den
# gerade sichtbaren Traces.
#
# Referenzkurve "Durchschnitt Mittelland" (rot gepunktet, aus standardkurven
# in 01_import_googlesheet.R) ist immer sichtbar, wie in den statischen
# Karten (22_plot_year.R). Sowohl sie als auch die schwarze Mittelwertlinie
# haben jetzt eigene Legendeneinträge (vorher ausgeblendet).
#
# Voraussetzung: 01_import_googlesheet.R wurde bereits ausgeführt (liefert
# jahresdaten, Jahr).
#
# Höhenlage: swisstopo-Höhen-API (api3.geo.admin.ch/rest/services/height) -
# punktgenau statt eines DEM-Rasters, weil das lokal vorhandene DEM
# (../geodata/swissaltiregio_..._BE.tif) nur den Kanton Bern abdeckt, das
# AGFF-Messnetz aber die ganze Schweiz. Resultat wird in
# standorte_elevation.csv gecacht (neue Standorte werden ergänzt, ohne
# bestehende neu abzufragen).
#
# Region (West/Mitte/Ost): grobe Einteilung nach Längengrad (nicht nach
# Kantons-/Sprachgrenzen) - Grenzen bei 7.3 und 8.5 Grad, so gewählt, dass
# sie das aktuelle Standortnetz (Suisse Romande/Jura bis Ostschweiz/
# Graubünden) einigermassen gleichmässig in 3 Gruppen teilen. Bei einem
# stark veränderten Standortnetz ggf. anpassen.

library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(plotly)
library(htmlwidgets)
library(jsonlite)

out_dir <- "outputs"
geodata_dir <- "../geodata/meteoschweiz"

## Aktive Standorte dieses Jahr (wie in 25_...R) -----------------------------
standorte_aktiv <- jahresdaten %>%
  distinct(place, Ort, lon, lat, masl) %>%
  rename(elevation = masl)

## Höhenlage je Standort: primär direkt aus dem Sheet (Spalte müM, siehe
## 01_import_googlesheet.R) - die swisstopo-API (mit lokalem Cache) dient
## nur noch als Fallback fuer Standorte ohne Sheet-Wert. -------------------
elevation_cache_file <- "standorte_elevation.csv"
elevation_cache <- if (file.exists(elevation_cache_file)) read.csv(elevation_cache_file) else data.frame(place = character(), elevation = numeric())

fehlende <- standorte_aktiv %>% filter(is.na(elevation), !place %in% elevation_cache$place)
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
standorte_aktiv <- standorte_aktiv %>%
  left_join(elevation_cache, by = "place", suffix = c("", "_api")) %>%
  mutate(elevation = coalesce(elevation, elevation_api)) %>%
  select(-elevation_api)

## Region und Höhenlage klassieren -------------------------------------------
standorte_aktiv <- standorte_aktiv %>%
  mutate(
    region = case_when(
      lon < 7.3  ~ "West",
      lon < 8.5  ~ "Mitte",
      TRUE       ~ "Ost"
    ),
    hoehenlage = case_when(
      elevation < 500 ~ "<500m",
      elevation < 650 ~ "500-650m",
      elevation < 800 ~ "650-800m",
      TRUE             ~ ">800m"
    )
  )
region_levels <- c("West", "Mitte", "Ost")
hoehen_levels <- c("<500m", "500-650m", "650-800m", ">800m")
standorte_aktiv$region <- factor(standorte_aktiv$region, levels = region_levels)
standorte_aktiv$hoehenlage <- factor(standorte_aktiv$hoehenlage, levels = hoehen_levels)

cat("Standorte mit Region/Höhenlage:\n")
print(standorte_aktiv %>% select(Ort, lon, elevation, region, hoehenlage) %>% arrange(lon))

## Täglicher Niederschlag 1.1. bis heute laden -------------------------------
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
monate_konsolidiert <- c("202601", "202602", "202603", "202604", "202605", "202606", "202607")
tage_ohne_monatsdatei <- seq(as.Date("2026-08-01"), Sys.Date() - 1, by = "day")
teile <- c(
  lapply(monate_konsolidiert, lade_var_monatlich, prodcode = "rhiresd"),
  lapply(tage_ohne_monatsdatei, lade_var_taeglich, prodcode = "rprelimd")
)
teile <- teile[!vapply(teile, is.null, logical(1))]
precip_alle <- rast(teile)
# Datum je Layer aus den eingebetteten NetCDF-Zeitstempeln (nicht als
# durchgehende Tagesfolge annehmen - einzelne Tage können in den
# MeteoSchweiz-Rohdaten fehlen, z.B. 2026-08-18).
tage_precip <- as.Date(time(precip_alle))
cat("Niederschlag geladen:", format(min(tage_precip), "%d.%m.%Y"), "-", format(max(tage_precip), "%d.%m.%Y"), "\n")

## Niederschlag an den Standort-Koordinaten entnehmen (Punktwert aus dem
## 1km-Raster, kein Flächenmittel) und auf Kalenderwochen summieren ---------
pts_lv95 <- st_as_sf(standorte_aktiv, coords = c("lon", "lat"), crs = 4326) %>% st_transform(2056)
werte <- terra::extract(precip_alle, vect(pts_lv95))[, -1]  # tidyr::extract() maskiert terra::extract()

niederschlag_taeglich <- data.frame(
  Ort = rep(standorte_aktiv$Ort, each = length(tage_precip)),
  date = rep(tage_precip, times = nrow(standorte_aktiv)),
  precip = as.numeric(t(werte))
)
niederschlag_taeglich$weeknum <- as.integer(strftime(niederschlag_taeglich$date, format = "%V"))

niederschlag_woche <- niederschlag_taeglich %>%
  group_by(Ort, weeknum) %>%
  summarise(precip_week = sum(precip, na.rm = TRUE), .groups = "drop") %>%
  left_join(standorte_aktiv %>% select(Ort, region, hoehenlage), by = "Ort")

## Gruppen definieren: Alle, 3 Regionen, 4 Höhenlagen ------------------------
gruppen <- c(
  list(list(id = "alle", label = "Alle Standorte", sites = standorte_aktiv$Ort)),
  lapply(region_levels, function(r) list(id = paste0("region_", r), label = paste0("Region: ", r),
                                          sites = standorte_aktiv$Ort[standorte_aktiv$region == r])),
  lapply(hoehen_levels, function(h) list(id = paste0("hoehe_", h), label = paste0("Höhenlage: ", h),
                                          sites = standorte_aktiv$Ort[standorte_aktiv$hoehenlage == h]))
)
gruppen <- gruppen[vapply(gruppen, function(g) length(g$sites) > 0, logical(1))]

alle_orte <- sort(unique(standorte_aktiv$Ort))

## Niederschlag: Mittelwert + Min/Max je Gruppe ------------------------------
niederschlag_gruppe <- lapply(gruppen, function(g) {
  niederschlag_woche %>%
    filter(Ort %in% g$sites) %>%
    group_by(weeknum) %>%
    summarise(mean_mm = mean(precip_week, na.rm = TRUE),
              min_mm = min(precip_week, na.rm = TRUE),
              max_mm = max(precip_week, na.rm = TRUE), .groups = "drop") %>%
    mutate(gruppe_id = g$id)
})
names(niederschlag_gruppe) <- vapply(gruppen, function(g) g$id, character(1))

## Wachstum: Mittelwert je Gruppe (nur zur Anzeige, wenn >1 Standort) --------
wachstum_gruppe <- lapply(gruppen, function(g) {
  jahresdaten %>%
    filter(Ort %in% g$sites) %>%
    group_by(weeknum) %>%
    summarise(mean_growth = mean(growth, na.rm = TRUE), .groups = "drop") %>%
    mutate(gruppe_id = g$id)
})
names(wachstum_gruppe) <- vapply(gruppen, function(g) g$id, character(1))

## Plotly-Farben je Standort (konsistent zwischen Wachstum und Niederschlag) -
site_farben <- setNames(
  colorRampPalette(RColorBrewer::brewer.pal(min(12, max(3, length(alle_orte))), "Paired"))(length(alle_orte)),
  alle_orte
)

## Montag je Kalenderwoche 1..53 für Jahr, nach ISO-8601 (der 4. Januar liegt
## immer in Woche 1) - für die Tooltips (Kalenderwoche bleibt IMMER sichtbar,
## unabhängig vom x-Achsen-Anzeigemodus, siehe onRender()-Block) sowie für den
## Datums-Anzeigemodus der x-Achse selbst. --------------------------------
jan4 <- as.Date(sprintf("%s-01-04", Jahr))
iso_wochentag_jan4 <- as.integer(format(jan4, "%u"))  # 1=Montag..7=Sonntag
montag_woche1 <- jan4 - (iso_wochentag_jan4 - 1)
montag_von_woche <- function(w) montag_woche1 + (w - 1) * 7
# Nur alle 5 Wochen ein beschrifteter Tick (0,5,10,...,50) - sonst wird die
# Achse bei 52 Einzelwochen unleserlich eng.
wochen_tickvals <- seq(0, 50, by = 5)
datum_ticktext <- format(montag_von_woche(wochen_tickvals), "%d.%m.")
wochen_ticktext <- as.character(wochen_tickvals)

# Tooltip-Text für Aggregat-Traces (Gruppen-Mittelwert, Niederschlag,
# Referenzkurve) - keine einzelne Erhebung, daher der Wochenbeginn (Montag).
wochen_tooltip <- function(w) paste0("KW ", w, " (Woche ab ", format(montag_von_woche(w), "%d.%m.%Y"), ")")

fig <- plot_ly()

## Traces 1..n: Graswachstum je Standort (immer vorhanden, Sichtbarkeit wird
## über das Dropdown gesteuert) - Tooltip zeigt Kalenderwoche UND das
## tatsächliche Erhebungsdatum dieses Punkts (aus jahresdaten$date), nicht
## den Wochenbeginn - unabhängig vom x-Achsen-Anzeigemodus (siehe
## Kommentar bei montag_von_woche() oben: %{x} würde im Datums-Modus die
## Tick-Beschriftung statt der Wochennummer zeigen, daher fixer Text via
## customdata statt %{x}). -------------------------------------------------
for (ort in alle_orte) {
  d <- jahresdaten %>% filter(Ort == ort) %>% arrange(weeknum)
  d$tooltip <- paste0("KW ", d$weeknum, " (erhoben am ", format(d$date, "%d.%m.%Y"), ")")
  fig <- fig %>% add_trace(
    data = d, x = ~weeknum, y = ~growth, type = "scatter", mode = "lines+markers",
    name = ort, legendgroup = ort, line = list(color = site_farben[[ort]], width = 1.5),
    marker = list(color = site_farben[[ort]], size = 5), customdata = ~tooltip,
    hovertemplate = paste0(ort, ": %{y:.0f} kg TS/ha/Tag<br>%{customdata}<extra></extra>"),
    visible = TRUE
  )
}
n_site_growth <- length(alle_orte)

## Traces: Wachstums-Mittelwert je Gruppe (gestrichelt schwarz) --------------
## showlegend = TRUE bei allen: da immer höchstens einer sichtbar ist
## (visible = FALSE bei den anderen schliesst deren Legendeneintrag ganz aus,
## nicht nur die Linie), erscheint effektiv genau EIN "Mittleres Wachstum"
## in der Legende - passend zur jeweils gewählten Gruppe.
for (g in gruppen) {
  d <- wachstum_gruppe[[g$id]]
  d$tooltip <- wochen_tooltip(d$weeknum)
  fig <- fig %>% add_trace(
    data = d, x = ~weeknum, y = ~mean_growth, type = "scatter", mode = "lines",
    name = "Mittleres Wachstum", line = list(color = "black", dash = "dash", width = 2.5),
    customdata = ~tooltip,
    hovertemplate = paste0("Mittel ", g$label, ": %{y:.0f} kg TS/ha/Tag<br>%{customdata}<extra></extra>"),
    showlegend = TRUE, visible = (g$id == "alle")
  )
}
n_group_growth <- length(gruppen)

## Traces: Niederschlag je einzelnem Standort (Balken, 2. Achse) - nur
## sichtbar, wenn genau dieser eine Standort gewählt ist ---------------------
for (ort in alle_orte) {
  d <- niederschlag_woche %>% filter(Ort == ort) %>% arrange(weeknum)
  d$tooltip <- wochen_tooltip(d$weeknum)
  fig <- fig %>% add_trace(
    data = d, x = ~weeknum, y = ~precip_week, type = "bar", yaxis = "y2",
    name = paste("Niederschlag", ort), marker = list(color = "steelblue", opacity = 0.4),
    customdata = ~tooltip,
    hovertemplate = paste0("Niederschlag ", ort, ": %{y:.0f} mm<br>%{customdata}<extra></extra>"),
    showlegend = FALSE, visible = FALSE
  )
}
n_site_precip <- length(alle_orte)

## Traces: Niederschlag Mittelwert + Spannweite je Gruppe (2. Achse) --------
## Ein Balken pro Woche (Mittelwert über die Standorte der Gruppe), Spannweite
## (Min/Max) als asymmetrische Fehlerbalken.
for (g in gruppen) {
  d <- niederschlag_gruppe[[g$id]] %>% arrange(weeknum)
  d$tooltip <- wochen_tooltip(d$weeknum)
  fig <- fig %>%
    add_trace(data = d, x = ~weeknum, y = ~mean_mm, type = "bar", yaxis = "y2",
               name = paste("Niederschlag", g$label), marker = list(color = "steelblue", opacity = 0.4),
               error_y = list(type = "data", symmetric = FALSE, array = ~max_mm - mean_mm, arrayminus = ~mean_mm - min_mm,
                               color = "steelblue"),
               customdata = ~tooltip,
               hovertemplate = paste0("Niederschlag ", g$label, ": %{y:.0f} mm im Mittel<br>%{customdata}<extra></extra>"),
               showlegend = FALSE, visible = (g$id == "alle"))
}
n_group_precip <- length(gruppen)

## Trace: Referenzkurve "Durchschnitt Mittelland" (rot gepunktet), immer
## sichtbar - wie in 22_plot_year.R/24_plot_afc_year.R -----------------------
standardkurven$tooltip <- wochen_tooltip(standardkurven$weeknum)
fig <- fig %>% add_trace(
  data = standardkurven, x = ~weeknum, y = ~Durchschnitt...700.m.ü.M...tiefgründig..frisch,
  type = "scatter", mode = "lines", name = "Durchschnitt Mittelland",
  line = list(color = "red", dash = "dot", width = 2.5), customdata = ~tooltip,
  hovertemplate = "Durchschnitt Mittelland: %{y:.0f} kg TS/ha/Tag<br>%{customdata}<extra></extra>",
  showlegend = TRUE, visible = TRUE
)

n_total <- n_site_growth + n_group_growth + n_site_precip + n_group_precip + 1

fig <- fig %>% layout(
  # Kein Plotly-Titel mehr - der Titel wird als eigenes HTML-Element über
  # der Grafik eingefügt (siehe onRender() unten), damit die Bedienelemente
  # sauber ZWISCHEN Titel und Diagramm platziert werden können (mit einem
  # reinen Plotly-Titel liegt das Diagramm inkl. Titel in derselben
  # Zeichenfläche, ein Dazwischenschieben per HTML ist dort nicht möglich).
  # automargin=TRUE: Plotly reserviert automatisch genug Platz für die
  # Tick-Beschriftung (im Datums-Modus länger als reine Wochennummern,
  # sonst am unteren Rand abgeschnitten); zusätzlich margin$b als
  # garantierter Mindestabstand.
  xaxis = list(title = "Kalenderwoche", range = c(0, 52), automargin = TRUE,
               tickmode = "array", tickvals = wochen_tickvals, ticktext = wochen_ticktext),
  # rangemode="tozero" auf BEIDEN Achsen, nicht nur y2: sonst pinnt nur die
  # Niederschlagsachse ihre Null an den unteren Rand, während die
  # Wachstumsachse leicht unter 0 auffüllt (Plotlys Standard-Autorange) -
  # die beiden Null-Linien liegen dann nicht auf derselben Höhe.
  yaxis = list(title = "Graswachstum (kg TS/ha/Tag)", rangemode = "tozero"),
  yaxis2 = list(title = "Niederschlag (mm/Woche)", overlaying = "y", side = "right", showgrid = FALSE, rangemode = "tozero"),
  barmode = "overlay",
  # Plotlys eingebaute Legende ist komplett abgeschaltet (showlegend=FALSE
  # ueberstimmt die showlegend=TRUE der einzelnen Traces) - die Legende wird
  # stattdessen als eigene HTML-Sidebar neben dem Diagramm gebaut (siehe
  # onRender()-Block), aehnlich der rechten Sidebar auf openstreetmap.org.
  # Dadurch braucht Plotly selbst keinen grossen rechten Rand mehr, nur noch
  # etwas Platz fuer die y2-Achsenbeschriftung.
  showlegend = FALSE,
  margin = list(t = 40, b = 85, r = 30)
)

# responsive=FALSE: Plotlys eingebautes Auto-Resize (config$responsive) misst
# beim Anpassen der Fenstergroesse den Platz falsch, sobald zusaetzliches
# HTML (Titel/Bedienelemente, siehe onRender() unten) in denselben
# Container eingefuegt wird - es bemisst die Groesse am gesamten Container
# statt an der tatsaechlich fuer das Diagramm vorgesehenen Flaeche, wodurch
# die x-Achsen-Beschriftung nach einer Fenstergroessen-Aenderung wieder
# abgeschnitten wird. Die Groesse wird stattdessen unten im onRender()-Block
# selbst (unter Beruecksichtigung von Titel- und Bedienelement-Hoehe) explizit
# per Plotly.relayout() gesetzt.
fig <- fig %>% config(responsive = FALSE)

## Auswahl-Steuerelemente als eigene HTML-Controls OBERHALB der Grafik statt
## Plotlys eingebaute "updatemenus" (die brauchen gemeinsamen Zustand -
## gewählte Gruppe/Standort UND Niederschlag an/aus - das leisten Plotlys
## zustandslose Dropdown-Buttons nicht sauber):
## - Durchsuchbare Combobox (Texteingabe + Vorschlagsliste, kein
##   externes JS nötig): Gruppen zuoberst, Standorte darunter hinter einer
##   Trennlinie ("versteckt", per Scrollen oder Tippen erreichbar).
## - Toggle-Switch fürs Ein-/Ausblenden des Niederschlags.
## - Toggle-Switch für die x-Achsen-Beschriftung: Kalenderwoche <-> Datum
##   (Montag der jeweiligen Woche). Die x-Werte selbst bleiben immer
##   weeknum (Trace-Daten unverändert) - nur die Tick-Beschriftung wechselt
##   (Plotly tickvals/ticktext), da Woche und Datum ohnehin 1:1 linear
##   zusammenhängen (jede Woche = 7 Tage).
gruppen_labels <- vapply(gruppen, function(g) g$label, character(1))
site_sichtbar_je_gruppe <- lapply(gruppen, function(g) alle_orte %in% g$sites)
# Standortfarben in derselben Reihenfolge wie alle_orte - fuer die
# HTML-Legenden-Sidebar (siehe onRender()-Block), synchron mit den
# Trace-Farben oben. as.character() ist noetig, weil alle_orte ein Factor
# ist (Ort wird in 01_import_googlesheet.R als Factor angelegt) - beim
# Indizieren eines benannten Vektors mit einem Factor (anders als z.B. bei
# for-Schleifen oder setNames()) nutzt R dessen interne Integer-Codes als
# POSITION statt die Werte als Namen zu matchen, was ohne as.character()
# zu falschen (bzw. fehlenden) Farben ab dem 7. Standort fuehrte.
site_farben_je_ort <- unname(site_farben[as.character(alle_orte)])
# wochen_tickvals/datum_ticktext: siehe Berechnung weiter oben (vor den
# Traces - dort auch für die Tooltip-Texte verwendet).

# js_template mit Platzhalter-Token statt sprintf(): der komplette JS-Block
# ist länger als sprintf()s Format-Längenlimit von 8192 Zeichen ("'fmt'
# Länge überschreitet maximale Formatlänge 8192") - gsub() kennt dieses
# Limit nicht, daher Ersetzung Token für Token weiter unten.
js_template <- "
function(el, x) {
  var nSiteGrowth = __N_SITE_GROWTH__, nGroups = __N_GROUPS__, nSitePrecip = __N_SITE_PRECIP__, nGroupPrecip = __N_GROUP_PRECIP__;
  var groupLabels = __GROUP_LABELS__;
  var siteNames = __SITE_NAMES__;
  var siteVisible = __SITE_VISIBLE__; // [gruppe][standort] -> bool
  var wochenTickvals = __WOCHEN_TICKVALS__;
  var datumTicktext = __DATUM_TICKTEXT__;
  var wochenTicktext = wochenTickvals.map(String);
  var chartTitle = __CHART_TITLE__;
  var siteColors = __SITE_COLORS__;
  // Auswahl: {type: 'group'|'site', idx: N}; Standard = erste Gruppe ('Alle Standorte').
  var selection = { type: 'group', idx: 0 };
  var precipOn = true;
  var datumOn = false;

  function applyState() {
    var vis = [];
    for (var i = 0; i < nSiteGrowth; i++) {
      vis.push(selection.type === 'group' ? siteVisible[selection.idx][i] : (selection.type === 'site' && i === selection.idx));
    }
    for (var i = 0; i < nGroups; i++) vis.push(selection.type === 'group' && i === selection.idx);
    for (var i = 0; i < nSitePrecip; i++) vis.push(precipOn && selection.type === 'site' && i === selection.idx);
    for (var i = 0; i < nGroupPrecip; i++) vis.push(precipOn && selection.type === 'group' && i === selection.idx);
    vis.push(true); // Referenzkurve immer sichtbar
    Plotly.restyle(el, { visible: vis });
    renderLegendItems();
  }

  function applyXAxis() {
    // Immer tickmode 'array' (alle 5 Wochen ein beschrifteter Tick) - nur
    // der Text (Wochennummer vs. Datum) wechselt.
    if (datumOn) {
      Plotly.relayout(el, {
        'xaxis.tickmode': 'array',
        'xaxis.tickvals': wochenTickvals,
        'xaxis.ticktext': datumTicktext,
        'xaxis.title.text': 'Datum (Montag der Woche)'
      });
    } else {
      Plotly.relayout(el, {
        'xaxis.tickmode': 'array',
        'xaxis.tickvals': wochenTickvals,
        'xaxis.ticktext': wochenTicktext,
        'xaxis.title.text': 'Kalenderwoche'
      });
    }
  }

  // Styles --------------------------------------------------------------
  var style = document.createElement('style');
  style.textContent = [
    '.gw-title { font-family: sans-serif; font-size: 22px; font-weight: 600; margin: 4px 0 10px 0; }',
    '.gw-controls { margin-bottom: 10px; font-family: sans-serif; font-size: 14px; display: flex; flex-wrap: wrap; align-items: center; gap: 20px; }',
    '.gw-combo { position: relative; display: inline-block; }',
    '.gw-combo input { padding: 5px 8px; font-size: 14px; width: 240px; border: 1px solid #bbb; border-radius: 4px; }',
    '.gw-combo-list { position: absolute; z-index: 1000; top: 100%; left: 0; background: white; border: 1px solid #bbb; border-radius: 4px; max-height: 260px; overflow-y: auto; width: 240px; box-shadow: 0 2px 8px rgba(0,0,0,0.15); }',
    '.gw-combo-item { padding: 6px 9px; cursor: pointer; }',
    '.gw-combo-item:hover, .gw-combo-item.active { background: #eaf2fb; }',
    '.gw-combo-sep { padding: 4px 9px; font-size: 11px; color: #888; border-top: 1px solid #eee; margin-top: 2px; user-select: none; }',
    '.gw-toggle-wrap { display: flex; align-items: center; gap: 8px; }',
    '.gw-toggle { position: relative; display: inline-block; width: 42px; height: 22px; flex-shrink: 0; }',
    '.gw-toggle input { opacity: 0; width: 0; height: 0; }',
    '.gw-toggle-slider { position: absolute; inset: 0; background-color: #ccc; transition: .15s; border-radius: 22px; cursor: pointer; }',
    '.gw-toggle-slider:before { position: absolute; content: \"\"; height: 16px; width: 16px; left: 3px; bottom: 3px; background-color: white; transition: .15s; border-radius: 50%; }',
    '.gw-toggle input:checked + .gw-toggle-slider { background-color: #4a90d9; }',
    '.gw-toggle input:checked + .gw-toggle-slider:before { transform: translateX(20px); }',
    '.gw-chart-row { display: flex; flex-direction: row; width: 100%; }',
    '.gw-legend-panel { flex: 0 0 210px; width: 210px; overflow-y: auto; overflow-x: hidden; border-left: 1px solid #ddd; box-sizing: border-box; padding: 10px 14px; font-family: sans-serif; font-size: 13px; transition: flex-basis .15s ease, width .15s ease, padding .15s ease, border-color .15s ease; }',
    '.gw-legend-panel.collapsed { flex-basis: 0; width: 0; padding-left: 0; padding-right: 0; border-left-color: transparent; }',
    '.gw-legend-header { display: flex; align-items: center; justify-content: space-between; font-weight: 600; margin-bottom: 8px; white-space: nowrap; }',
    '.gw-legend-options { display: flex; flex-direction: column; gap: 8px; padding-bottom: 10px; margin-bottom: 10px; border-bottom: 1px solid #eee; }',
    '.gw-legend-close { background: none; border: none; cursor: pointer; font-size: 16px; color: #777; line-height: 1; padding: 2px 4px; }',
    '.gw-legend-close:hover { color: #000; }',
    '.gw-legend-item { display: flex; align-items: center; gap: 8px; padding: 3px 0; white-space: nowrap; }',
    '.gw-legend-swatch { display: inline-block; width: 22px; height: 0; border-top-width: 3px; border-top-style: solid; flex-shrink: 0; }',
    '.gw-legend-edge { flex: 0 0 34px; width: 34px; border-left: 1px solid #ddd; display: flex; flex-direction: column; align-items: center; padding-top: 6px; box-sizing: border-box; }',
    '.gw-edge-btn { width: 26px; height: 26px; border: 1px solid #bbb; border-radius: 4px; background: white; cursor: pointer; font-size: 15px; display: flex; align-items: center; justify-content: center; color: #333; padding: 0; }',
    '.gw-edge-btn:hover { background: #f2f2f2; }',
    '.gw-edge-btn.active { background: #eaf2fb; border-color: #4a90d9; color: #2a6fbf; }'
  ].join(' ');
  document.head.appendChild(style);

  // Kombinierte Optionsliste: Gruppen, dann Trennlinie, dann Standorte -----
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
    return wrap;
  }

  var precipToggle = makeToggle('Niederschlag', true, function(checked) { precipOn = checked; applyState(); });
  // Kalenderwochen-Toggle ein (Standard) = Wochennummern auf der x-Achse,
  // aus = Montagsdatum der Woche - datumOn ist daher die Negation von checked.
  var xAxisToggle = makeToggle('Kalenderwochen', true, function(checked) { datumOn = !checked; applyXAxis(); });

  controls.appendChild(comboWrap);

  var titleEl = document.createElement('div');
  titleEl.className = 'gw-title';
  titleEl.textContent = chartTitle;

  // Der umgebende Container (htmlwidgets-Fill-Layout) hat eine fixe, vom
  // Browserfenster abgeleitete Hoehe (body { height:100%; overflow:hidden }).
  // Titel und Bedienelemente werden zusaetzlich in diesen Container
  // eingefuegt, ohne dass er dafuer waechst - das Diagramm muss die
  // verbleibende Restflaeche bekommen.
  //
  // Ein Flexbox-Wrapper (fillHost) sorgt zwar dafuer, dass die BOX von el
  // korrekt auf die Restflaeche schrumpft - Plotlys eigenes Auto-Resize
  // (Plotly.Plots.resize()/config$responsive) liest dabei aber nachweislich
  // die Groesse des UMGEBENDEN Containers (fillHost, 100% Hoehe) statt der
  // von el selbst aus und zeichnet die SVG dadurch weiterhin zu gross -
  // sichtbar erst NACH einer Fenstergroessen-Aenderung, wenn Plotlys eigener
  // (config$responsive=TRUE) Resize-Handler feuert und die vorher korrekte
  // Groesse wieder ueberschreibt. Deshalb responsive=FALSE in R (siehe
  // layout()-Block oben) und die Groesse hier stattdessen bei jeder
  // Fensteraenderung explizit per Plotly.relayout() gesetzt, direkt
  // berechnet aus der Restflaeche von fillHost minus Titel/Bedienelemente.
  var fillHost = document.createElement('div');
  fillHost.style.width = '100%';
  fillHost.style.height = '100%';
  el.parentNode.insertBefore(fillHost, el);
  fillHost.appendChild(titleEl);
  fillHost.appendChild(controls);

  // Legenden-Sidebar (aehnlich der rechten Sidebar auf openstreetmap.org):
  // ein schmaler Rand mit Umschalt-Icon (legendEdge) bleibt IMMER sichtbar
  // am rechten Rand; das eigentliche Panel (legendPanel) klappt seitwaerts
  // ein/aus (per Icon oder X im Panel), standardmaessig eingeblendet. Das
  // Diagramm (el) und die Sidebar liegen nebeneinander in einer eigenen
  // Zeile (chartRow) unterhalb von Titel/Bedienelementen.
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
  // Niederschlag- und Kalenderwochen-Umschalter sitzen in der Sidebar,
  // oberhalb der Standort-Legende, statt in der Bedienleiste ueber der
  // Grafik.
  var legendOptions = document.createElement('div');
  legendOptions.className = 'gw-legend-options';
  legendOptions.appendChild(precipToggle);
  legendOptions.appendChild(xAxisToggle);
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
    fitPlotSize();
  }
  edgeBtn.addEventListener('click', function() { setLegendOpen(legendPanel.classList.contains('collapsed')); });
  legendClose.addEventListener('click', function() { setLegendOpen(false); });

  // Legendeninhalt passend zu den aktuell sichtbaren Traces (siehe
  // applyState() oben, ruft dies bei jeder Auswahl-/Sichtbarkeitsaenderung
  // erneut auf) - Farben synchron mit den Trace-Farben (siteColors).
  function addLegendItem(label, color, style) {
    var item = document.createElement('div');
    item.className = 'gw-legend-item';
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
        if (siteVisible[selection.idx][i]) addLegendItem(siteNames[i], siteColors[i], 'solid');
      }
      addLegendItem('Mittleres Wachstum', 'black', 'dashed');
    } else {
      addLegendItem(siteNames[selection.idx], siteColors[selection.idx], 'solid');
    }
    addLegendItem('Durchschnitt Mittelland', 'red', 'dotted');
  }

  el.style.flex = '1 1 auto';
  el.style.minWidth = '0';
  chartRow.appendChild(el);
  chartRow.appendChild(legendPanel);
  chartRow.appendChild(legendEdge);
  fillHost.appendChild(chartRow);

  // Hoehe wird explizit berechnet und per Plotly.relayout() gesetzt (siehe
  // Kommentar oben: Plotlys eigenes Auto-Resize misst bei verschachteltem
  // HTML den falschen Container). Die Breite ergibt sich automatisch aus
  // dem Flexbox-Layout von chartRow (el waechst/schrumpft, wenn die
  // Legenden-Sidebar auf-/zuklappt) - el.clientWidth danach direkt auslesen.
  function fitPlotSize() {
    var fillTop = fillHost.getBoundingClientRect().top;
    var rowTop = chartRow.getBoundingClientRect().top;
    var used = rowTop - fillTop;
    var h = Math.max(200, fillHost.clientHeight - used);
    chartRow.style.height = h + 'px';
    var w = el.clientWidth;
    Plotly.relayout(el, { width: w, height: h });
  }
  fitPlotSize();
  window.addEventListener('resize', fitPlotSize);

  applyState();
}
"

js_ersetzungen <- list(
  "__N_SITE_GROWTH__" = as.character(n_site_growth),
  "__N_GROUPS__" = as.character(length(gruppen)),
  "__N_SITE_PRECIP__" = as.character(n_site_growth),
  "__N_GROUP_PRECIP__" = as.character(length(gruppen)),
  "__GROUP_LABELS__" = jsonlite::toJSON(gruppen_labels),
  "__SITE_NAMES__" = jsonlite::toJSON(alle_orte),
  "__SITE_VISIBLE__" = jsonlite::toJSON(site_sichtbar_je_gruppe),
  "__WOCHEN_TICKVALS__" = jsonlite::toJSON(wochen_tickvals),
  "__DATUM_TICKTEXT__" = jsonlite::toJSON(datum_ticktext),
  "__CHART_TITLE__" = jsonlite::toJSON(paste0("Graswachstumskurve ", Jahr), auto_unbox = TRUE),
  "__SITE_COLORS__" = jsonlite::toJSON(site_farben_je_ort)
)
js_code <- js_template
for (token in names(js_ersetzungen)) {
  js_code <- gsub(token, js_ersetzungen[[token]], js_code, fixed = TRUE)
}

fig <- htmlwidgets::onRender(fig, js_code)

fig
htmlwidgets::saveWidget(fig, file.path(normalizePath(out_dir), paste0("Wachstum_Niederschlag_interaktiv_", Jahr, ".html")), selfcontained = FALSE)
cat("Interaktive Karte gespeichert in:", out_dir, "\n")
