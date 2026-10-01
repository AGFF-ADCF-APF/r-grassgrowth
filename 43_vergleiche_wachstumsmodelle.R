# Phase A des "Potenzielles Graswachstum"-Vorhabens: ModVege (growR) und
# LINGRA (LingraNR) an Punkten mit regelmaessig gelieferten Daten
# vergleichen, BEVOR eine Rasterloesung fuer ganz Schweiz geplant wird
# (siehe Plan in .claude/plans/temporal-sauteeing-clover.md).
#
# Eigenstaendiges Skript wie 41_review_year.R/42_review_year_place_
# experimental.R - NICHT Teil von 00_automate.R/der veroeffentlichten App.
#
# install.packages("growR")
# Fuer LingraNR siehe Hinweis weiter unten (Java-Abhaengigkeit).

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(ggplot2)
  library(growR)
})

ausgabe_dir <- "outputs/vergleich_wachstumsmodelle"
growr_input_dir <- file.path(ausgabe_dir, "growr_input")
dir.create(growr_input_dir, recursive = TRUE, showWarnings = FALSE)

########################################################################
## A1. Punkte auswaehlen -------------------------------------------------
########################################################################

if (!exists("daten")) source("01_import_googlesheet.R")
daten_korr <- daten %>%
  filter(!is.na(lon), !is.na(lat), !is.na(date), nchar(year) == 4, as.numeric(year) >= 2020)
alle_jahre <- sort(unique(daten_korr$year))

# "Regelmaessig" = ueber ALLE drei Projektjahre gemeldet (n_jahre == 3) -
# eine reproduzierbare, nicht willkuerliche Auswahlregel statt eines frei
# gewaehlten Mindest-Messwert-Schwellenwerts.
standorte_dichte <- daten_korr %>%
  group_by(place, Ort, lon, lat) %>%
  summarise(n_daten = n_distinct(date), n_jahre = n_distinct(year), .groups = "drop") %>%
  arrange(desc(n_daten))
agff_regelmaessig <- standorte_dichte %>% filter(n_jahre == 3)
cat("Regelmaessige AGFF-Standorte (n_jahre==3):", nrow(agff_regelmaessig), "\n")
print(agff_regelmaessig)

# SMN-Stationsmetadaten (wie 27_plot_datenexplorer.R) - nur aktive, Temperatur
# meldende Stationen.
smn_basis_url <- "https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/"
smn_meta_dir <- "ebenen_cache/smn_vergleich"
dir.create(smn_meta_dir, recursive = TRUE, showWarnings = FALSE)
f_meta <- file.path(smn_meta_dir, "meta_stations.csv")
f_inv <- file.path(smn_meta_dir, "meta_datainventory.csv")
if (!file.exists(f_meta)) download.file(paste0(smn_basis_url, "ogd-smn_meta_stations.csv"), f_meta, quiet = TRUE, mode = "wb")
if (!file.exists(f_inv)) download.file(paste0(smn_basis_url, "ogd-smn_meta_datainventory.csv"), f_inv, quiet = TRUE, mode = "wb")
smn_meta_roh <- read.csv(f_meta, sep = ";", fileEncoding = "ISO-8859-1", stringsAsFactors = FALSE)
smn_inv <- read.csv(f_inv, sep = ";", fileEncoding = "ISO-8859-1", stringsAsFactors = FALSE)
smn_aktiv_abbr <- unique(smn_inv$station_abbr[smn_inv$parameter_shortname == "tre200d0" & trimws(smn_inv$data_till) == ""])
smn_meta <- smn_meta_roh[smn_meta_roh$station_abbr %in% smn_aktiv_abbr, ] %>%
  transmute(abbr = station_abbr, lon = station_coordinates_wgs84_lon, lat = station_coordinates_wgs84_lat,
            name = station_name, kanton = station_canton, hoehe = round(station_height_masl))

# Naechste SMN-Station je AGFF-Standort (Luftlinie in LV95).
smn_sf <- st_as_sf(smn_meta, coords = c("lon", "lat"), crs = 4326) %>% st_transform(2056)
agff_sf <- st_as_sf(agff_regelmaessig, coords = c("lon", "lat"), crs = 4326) %>% st_transform(2056)
dmat <- st_distance(agff_sf, smn_sf)
nearest_idx <- apply(dmat, 1, which.min)
agff_regelmaessig$smn_abbr <- smn_meta$abbr[nearest_idx]
agff_regelmaessig$smn_hoehe <- smn_meta$hoehe[nearest_idx]
agff_regelmaessig$smn_dist_km <- round(apply(dmat, 1, min) / 1000, 1)
cat("\nAGFF-Standort -> naechste SMN-Station:\n")
print(agff_regelmaessig %>% select(place, n_daten, n_jahre, smn_abbr, smn_dist_km))

# Drei "reine" Klimavergleichspunkte (keine AGFF-Messung, nur Modell-vs-
# Modell-Plausibilitaet ueber Hoehenlage/Region): Tiefland Tessin (Sueden),
# zwei alpine Punkte mit unterschiedlicher Klimaexposition (GR/noerdlich vs.
# TI/suedlich der Alpen) auf aehnlicher Hoehe - bewusst KEINE Hochgebirgs-
# Extremwerte (z.B. Jungfraujoch), da dort gar kein Gruenland existiert.
klima_punkte <- smn_meta %>% filter(abbr %in% c("MAG", "DAV", "GEN"))
cat("\nReine Klimavergleichspunkte:\n")
print(klima_punkte)

alle_punkte <- bind_rows(
  agff_regelmaessig %>% transmute(id = place, typ = "AGFF", smn_abbr, lon, lat, hoehe = smn_hoehe),
  klima_punkte %>% transmute(id = paste0("Klima: ", name), typ = "Klima", smn_abbr = abbr, lon, lat, hoehe)
)
saveRDS(alle_punkte, file.path(ausgabe_dir, "punkte_auswahl.rds"))
cat("\nGesamt", nrow(alle_punkte), "Vergleichspunkte ausgewaehlt.\n")

########################################################################
## A2. SMN-Wetterdaten laden und ins growR-Format bringen ---------------
########################################################################

# growR erwartet (siehe posieux_weather/?WeatherData): year, DOY, Ta (Mittel),
# Tmin, Tmax, precip (mm), rSSD (relative Sonnenscheindauer 0-1), SRad
# (MJ/m2/Tag), ET0 (mm/Tag, MUSS mitgeliefert werden - growR berechnet es
# nicht selbst), snow (0-1, hier mangels Datenquelle auf 0 gesetzt -
# explizite Vereinfachung, siehe Hinweis im Bericht).
#
# ET0 nach Hargreaves (FAO-56) - IDENTISCHE Formel/Funktion wie in
# 27_plot_datenexplorer.R (dort: berechne_ra()), hier als Skalarversion
# (ein Punkt statt eines ganzen Rasters), damit beide Validierungswege
# (Phase A hier, spaetere Rasterphase dort) garantiert dieselbe Physik
# verwenden.
berechne_ra_punkt <- function(J, phi) {
  dr <- 1 + 0.033 * cos(2 * pi * J / 365)
  delta <- 0.409 * sin(2 * pi * J / 365 - 1.39)
  ws <- acos(pmin(pmax(-tan(phi) * tan(delta), -1), 1))
  (24 * 60 / pi) * 0.0820 * dr * (ws * sin(phi) * sin(delta) + cos(phi) * cos(delta) * sin(ws))
}

# Laedt die SMN-Tageswerte (historisch + aktuell) einer Station und bringt
# sie ins growR-Format. gre000d0 (Globalstrahlung, W/m2 Tagesmittel) ist
# eine ECHTE Messung - fuer Phase A besser als die Angstroem-Prescott-
# Schaetzung aus der Sonnenscheindauer, die erst in der spaeteren
# Rasterphase (mangels flaechendeckender Strahlungsmessung) noetig wird.
lade_smn_wetter <- function(abbr, lat) {
  abbr_klein <- tolower(abbr)
  ziel_dir <- file.path(smn_meta_dir, abbr_klein)
  dir.create(ziel_dir, recursive = TRUE, showWarnings = FALSE)
  dateien <- c("historical", "recent")
  roh <- lapply(dateien, function(teil) {
    f <- file.path(ziel_dir, paste0("d_", teil, ".csv"))
    if (!file.exists(f)) {
      url <- paste0(smn_basis_url, abbr_klein, "/ogd-smn_", abbr_klein, "_d_", teil, ".csv")
      tryCatch(download.file(url, f, quiet = TRUE, mode = "wb"), error = function(e) NULL)
    }
    if (file.exists(f)) read.csv(f, sep = ";", stringsAsFactors = FALSE) else NULL
  })
  roh <- bind_rows(roh)
  if (nrow(roh) == 0) return(NULL)
  roh <- roh %>% distinct(reference_timestamp, .keep_all = TRUE)
  roh$datum <- as.Date(roh$reference_timestamp, format = "%d.%m.%Y %H:%M")
  roh <- roh[!is.na(roh$datum), ]
  roh <- roh[order(roh$datum), ]

  # pva200d0 (Dampfdruck) und fu3010d0 (Wind) werden hier NICHT gebraucht -
  # growR braucht nur Temperatur/Niederschlag/Strahlung/ET0/Schnee (siehe
  # oben). Sie sind trotzdem fuer eine spaetere LINGRA-Auswertung bereits
  # in derselben Rohdatei enthalten (siehe Kommentar zum Java-Blocker weiter
  # unten).
  phi <- lat * pi / 180
  out <- data.frame(
    year = as.integer(format(roh$datum, "%Y")),
    DOY = as.integer(format(roh$datum, "%j")),
    Ta = roh$tre200d0, Tmin = roh$tre200dn, Tmax = roh$tre200dx,
    precip = roh$rre150d0,
    rSSD = roh$sre000d0 / (24 * 60), # Minuten Sonnenschein -> Anteil des Tages
    # SRad bleibt in MeteoSchweiz' eigener Einheit (W/m2 Tagesmittel, wie
    # gre000d0) - growR's eigene Beispieldaten (posieux_weather.txt) liegen
    # ERWIESENERMASSEN in genau dieser Groessenordnung (Werte bis ~350),
    # NICHT in MJ/m2/Tag wie der Spaltenname vermuten liesse. KEINE
    # Einheitenumrechnung noetig/sinnvoll.
    SRad = roh$gre000d0,
    snow = 0
  )
  # PAR = SRad * 0.0406: in growR's Vignette NICHT dokumentiert, aber aus den
  # mitgelieferten posieux_weather.txt-Beispieldaten empirisch bestimmt -
  # ueber 9 Stichproben verschiedener Jahreszeiten/Jahre konstant auf
  # +/-0.5% (0.0405-0.0407), siehe Recherche-Notiz im Bericht.
  out$PAR <- out$SRad * 0.0406
  out$Ra_mm <- berechne_ra_punkt(out$DOY, phi) * 0.408
  out$ET0 <- with(out, 0.0023 * (Ta + 17.8) * sqrt(pmax(Tmax - Tmin, 0)) * Ra_mm)
  out$Ra_mm <- NULL
  out <- out[complete.cases(out[c("Ta", "Tmin", "Tmax", "precip", "SRad", "PAR", "ET0")]), ]
  out
}

# Fuer alle Vergleichspunkte laden (10 eindeutige Stationen).
wetter_je_station <- list()
for (i in seq_len(nrow(alle_punkte))) {
  abbr <- alle_punkte$smn_abbr[i]
  if (!is.null(wetter_je_station[[abbr]])) next
  cat("Lade SMN-Wetter:", abbr, "...\n")
  wetter_je_station[[abbr]] <- lade_smn_wetter(abbr, lat = alle_punkte$lat[i])
}
cat("\nStationen geladen:", length(wetter_je_station), "\n")

########################################################################
## A2b. Strahlungs-Schaetzfehler quantifizieren --------------------------
########################################################################
## Fuer die spaetere Rasterphase gibt es KEINE flaechendeckende Strahlungs-
## messung (siehe Plan) - dort muesste Rs ueber Angstroem-Prescott aus der
## Sonnenscheindauer geschaetzt werden: Rs = (a_s + b_s*n/N) * Ra (FAO-56
## Gl. 50, a_s=0.25/b_s=0.50 Standardwerte). Hier wird diese Schaetzung
## gegen die ECHTE Stationsmessung (gre000d0, bereits als SRad geladen) an
## allen 10 Vergleichspunkten getestet - ueber die GESAMTE verfuegbare
## SMN-Historie (nicht nur alle_jahre), da hier mehr Datenpunkte die
## Fehlerabschaetzung robuster machen und das ohnehin schon im Cache liegt.
smn_lat_je_abbr <- setNames(alle_punkte$lat, alle_punkte$smn_abbr)
strahlung_vergleich <- purrr::imap_dfr(wetter_je_station, function(wetter, abbr) {
  if (is.null(wetter) || nrow(wetter) == 0) return(NULL)
  phi <- smn_lat_je_abbr[[abbr]] * pi / 180
  ra_mj <- berechne_ra_punkt(wetter$DOY, phi) # MJ/m2/Tag, UNKONVERTIERT (siehe Hinweis bei et0-Berechnung)
  rs_ap_mj <- (0.25 + 0.50 * wetter$rSSD) * ra_mj
  data.frame(
    abbr = abbr, year = wetter$year, DOY = wetter$DOY,
    rSSD = wetter$rSSD, Ra_mj = ra_mj, # fuer eine spaetere Rekalibrierung von a_s/b_s mitgefuehrt
    SRad_gemessen = wetter$SRad, # W/m2, echte Messung (gre000d0)
    SRad_geschaetzt = rs_ap_mj * 11.574 # MJ/m2/Tag -> W/m2 Tagesmittel (*1e6/86400)
  )
})
strahlung_vergleich <- strahlung_vergleich %>% filter(!is.na(SRad_gemessen), !is.na(SRad_geschaetzt))

cat("\n=== Strahlungs-Schaetzfehler (Angstroem-Prescott vs. SMN-Messung) ===\n")
cat("Datenpunkte:", nrow(strahlung_vergleich), "ueber", n_distinct(strahlung_vergleich$abbr), "Stationen,",
    "Zeitraum", min(strahlung_vergleich$year), "-", max(strahlung_vergleich$year), "\n")
cat("Bias (geschaetzt - gemessen):", round(mean(strahlung_vergleich$SRad_geschaetzt - strahlung_vergleich$SRad_gemessen), 1), "W/m2\n")
cat("RMSE:", round(sqrt(mean((strahlung_vergleich$SRad_geschaetzt - strahlung_vergleich$SRad_gemessen)^2)), 1), "W/m2\n")
cat("Korrelation:", round(cor(strahlung_vergleich$SRad_geschaetzt, strahlung_vergleich$SRad_gemessen), 3), "\n")
cat("Mittlere Messung (Referenzgroesse):", round(mean(strahlung_vergleich$SRad_gemessen), 1), "W/m2\n")

cat("\nJe Station:\n")
print(strahlung_vergleich %>% group_by(abbr) %>%
  summarise(n = n(), bias = round(mean(SRad_geschaetzt - SRad_gemessen), 1),
            rmse = round(sqrt(mean((SRad_geschaetzt - SRad_gemessen)^2)), 1),
            korr = round(cor(SRad_geschaetzt, SRad_gemessen), 2), .groups = "drop") %>%
  arrange(desc(rmse)), n = 20)

# Saisonale Aufschluesselung - Angstroem-Prescott ist bekanntermassen bei
# Nebel/Hochnebel-Lagen (Mittelland-Winter) weniger zuverlaessig, da
# Sonnenscheindauer=0 trotz diffusem Restlicht nicht denselben Effekt hat
# wie eine echte Strahlungsmessung.
strahlung_vergleich$monat <- as.integer(format(as.Date(sprintf("%d-01-01", strahlung_vergleich$year)) + strahlung_vergleich$DOY - 1, "%m"))
p_strahlung <- ggplot(strahlung_vergleich, aes(x = SRad_gemessen, y = SRad_geschaetzt)) +
  geom_point(alpha = 0.15, size = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~abbr) +
  labs(title = "Angstroem-Prescott-Schaetzung vs. echte SMN-Strahlungsmessung",
       x = "gemessen (W/m2)", y = "geschaetzt aus Sonnenscheindauer (W/m2)") +
  theme_minimal()
ggsave(file.path(ausgabe_dir, "strahlung_schaetzfehler_scatter.png"), p_strahlung, width = 12, height = 8, dpi = 120)

p_monat <- strahlung_vergleich %>% group_by(abbr, monat) %>%
  summarise(bias = mean(SRad_geschaetzt - SRad_gemessen), .groups = "drop") %>%
  ggplot(aes(x = monat, y = bias, color = abbr)) + geom_line() + geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_x_continuous(breaks = 1:12) +
  labs(title = "Strahlungs-Schaetzfehler (geschaetzt - gemessen) nach Monat", x = "Monat", y = "Bias (W/m2)") +
  theme_minimal()
ggsave(file.path(ausgabe_dir, "strahlung_schaetzfehler_monat.png"), p_monat, width = 10, height = 6, dpi = 120)
saveRDS(strahlung_vergleich, file.path(ausgabe_dir, "strahlung_vergleich.rds"))
cat("\nPlots gespeichert:  strahlung_schaetzfehler_scatter.png, strahlung_schaetzfehler_monat.png\n")

# Der Fehler ist gross, aber SEHR systematisch (Korrelation 0.947) - die
# FAO-56-Standardwerte a_s=0.25/b_s=0.50 sind fuer Schweizer Verhaeltnisse
# offenbar nicht passend. Re-Kalibrierung ueber dieselben Daten:
# SRad_gemessen/(Ra*11.574) = a_s + b_s*rSSD  - eine einfache lineare
# Regression liefert die Schweiz-spezifischen Koeffizienten direkt aus den
# Daten statt den generischen FAO-Default weiterzuverwenden.
strahlung_vergleich$y_kalibrierung <- strahlung_vergleich$SRad_gemessen / (strahlung_vergleich$Ra_mj * 11.574)
kalib_modell <- lm(y_kalibrierung ~ rSSD, data = strahlung_vergleich)
a_s_neu <- coef(kalib_modell)[["(Intercept)"]]
b_s_neu <- coef(kalib_modell)[["rSSD"]]
cat("\n=== Schweiz-kalibrierte Angstroem-Prescott-Koeffizienten ===\n")
cat("a_s =", round(a_s_neu, 3), "(FAO-56-Standard: 0.25)\n")
cat("b_s =", round(b_s_neu, 3), "(FAO-56-Standard: 0.50)\n")

strahlung_vergleich$SRad_kalibriert <- (a_s_neu + b_s_neu * strahlung_vergleich$rSSD) * strahlung_vergleich$Ra_mj * 11.574
cat("\nMit kalibrierten Koeffizienten:\n")
cat("Bias:", round(mean(strahlung_vergleich$SRad_kalibriert - strahlung_vergleich$SRad_gemessen), 1), "W/m2\n")
cat("RMSE:", round(sqrt(mean((strahlung_vergleich$SRad_kalibriert - strahlung_vergleich$SRad_gemessen)^2)), 1), "W/m2\n")
cat("Korrelation:", round(cor(strahlung_vergleich$SRad_kalibriert, strahlung_vergleich$SRad_gemessen), 3), "(unveraendert, reine Verschiebung/Streckung)\n")

cat("\nJe Station (kalibriert):\n")
print(strahlung_vergleich %>% group_by(abbr) %>%
  summarise(bias = round(mean(SRad_kalibriert - SRad_gemessen), 1),
            rmse = round(sqrt(mean((SRad_kalibriert - SRad_gemessen)^2)), 1), .groups = "drop") %>%
  arrange(desc(abs(bias))))

saveRDS(list(a_s = a_s_neu, b_s = b_s_neu, modell = kalib_modell), file.path(ausgabe_dir, "angstrom_prescott_kalibrierung.rds"))

p_kalibriert <- ggplot(strahlung_vergleich, aes(x = SRad_gemessen)) +
  geom_point(aes(y = SRad_geschaetzt, color = "FAO-56-Standard (a=0.25, b=0.50)"), alpha = 0.1, size = 0.5) +
  geom_point(aes(y = SRad_kalibriert, color = "Schweiz-kalibriert"), alpha = 0.1, size = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  labs(title = "Angstroem-Prescott: FAO-Standard vs. Schweiz-kalibriert",
       x = "gemessen (W/m2)", y = "geschaetzt (W/m2)", color = "") +
  theme_minimal() + theme(legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2)))
ggsave(file.path(ausgabe_dir, "strahlung_kalibriert_vs_standard.png"), p_kalibriert, width = 9, height = 7, dpi = 120)

########################################################################
## A3. growR: Parameter-/Wetterdateien schreiben, Umgebungen bauen, laufen
########################################################################

# Posieux-Parametersatz (growR-eigenes Beispiel, Agroscope-kalibriert) als
# generische Vorlage fuer alle Punkte OHNE eigene Standort-Kalibrierung -
# nur Site/LON/LAT/ELV werden je Punkt angepasst, der Rest (T0/T1/T2,
# RUEmax, WHC, Funktionsgruppen-Mischung etc.) bleibt die veroeffentlichte
# Agroscope-Kalibrierung statt selbst erfundener Werte (siehe Plan).
posieux_param_datei <- system.file("extdata", "posieux_parameters.csv", package = "growR")
posieux_param_basis <- read.csv(posieux_param_datei, stringsAsFactors = FALSE)

schreibe_parameter_datei <- function(punkt_id, lon, lat, hoehe) {
  p <- posieux_param_basis
  p$value[p$name == "LON"] <- lon
  p$value[p$name == "LAT"] <- lat
  p$value[p$name == "ELV"] <- hoehe
  pfad <- file.path(growr_input_dir, paste0(punkt_id, "_parameters.csv"))
  write.csv(p, pfad, row.names = FALSE)
  pfad
}

schreibe_wetter_datei <- function(punkt_id, wetter) {
  pfad <- file.path(growr_input_dir, paste0(punkt_id, "_weather.txt"))
  write.table(wetter, pfad, sep = "\t", row.names = FALSE, quote = FALSE)
  pfad
}

alle_punkte$punkt_id <- paste0("p", seq_len(nrow(alle_punkte)), "_", gsub("[^A-Za-z0-9]", "", alle_punkte$smn_abbr))

umgebungen <- list()
for (i in seq_len(nrow(alle_punkte))) {
  pid <- alle_punkte$punkt_id[i]
  wetter <- wetter_je_station[[alle_punkte$smn_abbr[i]]]
  if (is.null(wetter) || nrow(wetter) == 0) {
    cat("UEBERSPRUNGEN (kein Wetter):", pid, "\n")
    next
  }
  param_pfad <- schreibe_parameter_datei(pid, alle_punkte$lon[i], alle_punkte$lat[i], alle_punkte$hoehe[i])
  wetter_pfad <- schreibe_wetter_datei(pid, wetter)
  # An alle_jahre gekoppelt (siehe Plan) - nicht die ganze, teils
  # jahrzehntelange SMN-Historie simulieren, die fuer den Abgleich mit den
  # AGFF-Messungen (nur 2024-2026) ohnehin nicht gebraucht wird.
  jahre <- sort(unique(wetter$year))
  jahre <- jahre[jahre %in% as.integer(alle_jahre)]
  # "high" ist KEIN vorhandener Dateiname -> growR faellt automatisch auf
  # Autocut mit Management-Intensitaet "high" zurueck (siehe
  # ManagementData$read_management() - kein Fehler, gewolltes Verhalten),
  # da wir die tatsaechlichen Schnitttermine der echten Betriebe nicht
  # kennen. Explizite Vereinfachung, siehe Bericht.
  umgebungen[[pid]] <- ModvegeEnvironment$new(
    site_name = pid, run_name = alle_punkte$id[i], years = jahre,
    param_file = basename(param_pfad), weather_file = basename(wetter_pfad),
    management_file = "high", input_dir = growr_input_dir
  )
}
cat("\n", length(umgebungen), "growR-Umgebungen gebaut, starte Simulation...\n")
ergebnisse <- growR_run_loop(umgebungen, output_dir = "")
cat("Simulation abgeschlossen.\n")
saveRDS(ergebnisse, file.path(ausgabe_dir, "growr_ergebnisse.rds"))

########################################################################
## A4. Vergleich gegen echte Messungen (AGFF) + Plausibilitaet (Klima) --
########################################################################

sammle_simulation <- function(run_ergebnisse, punkt_id) {
  purrr::map_dfr(run_ergebnisse, function(s) {
    data.frame(punkt_id = punkt_id, year = s$year, DOY = seq_along(s$BM),
               dBM_sim = s$dBM, GRO_sim = s$GRO, PGRO_sim = s$PGRO, BM_sim = s$BM)
  })
}
sim_alle <- purrr::imap_dfr(ergebnisse, function(run_res, idx) {
  pid <- names(umgebungen)[idx]
  sammle_simulation(run_res, pid)
}) %>% mutate(date = as.Date(sprintf("%d-01-01", year)) + DOY - 1)

# AGFF-Messungen (echtes Graswachstum kg TS/ha/Tag) den Simulationspunkten
# zuordnen - ueber punkt_id/place verknuepft (1:1, da je AGFF-Standort genau
# EIN Simulationspunkt gebaut wurde).
agff_punkte <- alle_punkte %>% filter(typ == "AGFF")
messungen_agff <- daten_korr %>%
  filter(place %in% agff_punkte$id) %>%
  left_join(agff_punkte %>% select(id, punkt_id), by = c("place" = "id")) %>%
  transmute(punkt_id, place, date, growth_gemessen = growth)

verglichen <- inner_join(
  sim_alle, messungen_agff, by = c("punkt_id", "date")
)
cat("\nVergleichspunkte (Simulation trifft Messdatum):", nrow(verglichen), "von", nrow(messungen_agff), "Messungen\n")

if (nrow(verglichen) > 0) {
  cat("\n=== growR dBM (tatsaechlich, wasser/temp-limitiert) vs. AGFF-Messung ===\n")
  cat("Bias:", round(get_bias(verglichen$dBM_sim, verglichen$growth_gemessen), 1), "kg TS/ha/Tag\n")
  cat("Korrelation:", round(cor(verglichen$dBM_sim, verglichen$growth_gemessen, use = "complete.obs"), 2), "\n")
  cat("\n=== growR PGRO (potenziell, unlimitiert) vs. AGFF-Messung ===\n")
  cat("Bias:", round(get_bias(verglichen$PGRO_sim, verglichen$growth_gemessen), 1), "kg TS/ha/Tag\n")
  cat("Korrelation:", round(cor(verglichen$PGRO_sim, verglichen$growth_gemessen, use = "complete.obs"), 2), "\n")

  p_scatter <- ggplot(verglichen, aes(x = growth_gemessen)) +
    geom_point(aes(y = dBM_sim, color = "tatsaechlich (dBM)")) +
    geom_point(aes(y = PGRO_sim, color = "potenziell (PGRO)")) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
    facet_wrap(~punkt_id, scales = "free") +
    labs(title = "growR simuliert vs. AGFF-Graswachstum gemessen", x = "gemessen (kg TS/ha/Tag)", y = "simuliert", color = "") +
    theme_minimal() + theme(legend.position = "bottom")
  ggsave(file.path(ausgabe_dir, "growr_agff_scatter.png"), p_scatter, width = 12, height = 8, dpi = 120)
}

p_zeitreihe <- ggplot(sim_alle, aes(x = date)) +
  geom_line(aes(y = PGRO_sim, color = "potenziell (PGRO)")) +
  geom_line(aes(y = dBM_sim, color = "tatsaechlich (dBM)")) +
  geom_point(data = messungen_agff, aes(x = date, y = growth_gemessen, color = "gemessen"), inherit.aes = FALSE, size = 1.5) +
  facet_wrap(~punkt_id, scales = "free_y", ncol = 2) +
  labs(title = "growR: potenziell vs. tatsaechlich simuliert vs. AGFF-Messung (2024-2026)", y = "kg TS/ha/Tag", color = "") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave(file.path(ausgabe_dir, "growr_zeitreihen.png"), p_zeitreihe, width = 14, height = 16, dpi = 110)

# Reine Klimapunkte: nur Modell-Plausibilitaet ueber Hoehenlage - ohne
# Wachstums-Grundwahrheit, deshalb nur die potenzielle Rate ueber die
# Saison geplottet.
klima_ids <- alle_punkte %>% filter(typ == "Klima") %>% pull(punkt_id)
p_klima <- sim_alle %>% filter(punkt_id %in% klima_ids) %>%
  left_join(alle_punkte %>% select(punkt_id, id, hoehe), by = "punkt_id") %>%
  ggplot(aes(x = date, y = PGRO_sim, color = sprintf("%s (%dm)", id, hoehe))) +
  geom_line() + facet_wrap(~year, scales = "free_x") +
  labs(title = "Potenzielles Wachstum (PGRO) an Klimavergleichspunkten - Plausibilitaet ueber Hoehenlage", y = "kg TS/ha/Tag (potenziell)", color = "") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave(file.path(ausgabe_dir, "growr_klimapunkte_plausibilitaet.png"), p_klima, width = 12, height = 6, dpi = 120)

cat("\nAlle Plots gespeichert in:", ausgabe_dir, "\n")
saveRDS(list(sim_alle = sim_alle, messungen_agff = messungen_agff, verglichen = verglichen), file.path(ausgabe_dir, "vergleich_ergebnisse.rds"))
