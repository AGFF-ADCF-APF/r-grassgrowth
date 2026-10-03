# Trainingsdaten fuer Methode 3 (statistisches Ertragsmodell wie SatGrass):
# AGFF-Zuwachsmessungen 2024-2026 mit Satelliten- und Wettermerkmalen.
#
# Die AGFF-Messungen sind betriebs-, nicht parzellengenau (nur Koordinaten
# des Betriebs). Die Satellitenmerkmale beschreiben deshalb das Gruenland im
# Umkreis von 300 m (ESA WorldCover Klasse 30, Median-NDVI der wolkenfreien
# Pixel je Aufnahme). Ein darauf trainiertes Modell schaetzt den Zuwachs des
# umliegenden Gruenlands, nicht einer einzelnen Parzelle.
#
# Wetter: naechste SMN-Station (Distanz plus 5 km Strafe je 100 m Hoehen-
# unterschied), Temperaturen mit 0.65 Grad/100 m auf die Standorthoehe
# korrigiert. Zusaetzlich ModVege (Port aus 27, automatische Schnitte,
# Posieux-Parameter mit NI 0.7) als Vergleich am selben Messpunkt.
#
# Ergebnis: outputs/ertrag_qualitaet/trainingsdaten.rds
# Eigenstaendig wie 43-47 - NICHT Teil von 00_automate.R.

suppressPackageStartupMessages({
  library(dplyr); library(sf); library(terra); library(data.table); library(jsonlite); library(parallel)
})

aus_dir <- "outputs/ertrag_qualitaet"
ndvi_dir <- file.path(aus_dir, "ndvi_agff")
dir.create(ndvi_dir, recursive = TRUE, showWarnings = FALSE)
radius_m <- 300
jahre <- 2024:2026

## Hilfsfunktionen aus 43 (SMN-Wetter) und 27 (ModVege-Port) uebernehmen
lade_definitionen <- function(datei, muster) {
  for (a in parse(datei, keep.source = FALSE)) {
    if (is.call(a) && identical(a[[1]], as.name("<-")) &&
        ((is.name(a[[2]]) && grepl(muster, as.character(a[[2]]))) ||
         (is.call(a[[2]]) && identical(a[[2]][[2]], as.name("modvege_P"))))) eval(a, envir = globalenv())
  }
}
lade_definitionen("43_vergleiche_wachstumsmodelle.R", "^(smn_basis_url|smn_meta_dir|berechne_ra_punkt|lade_smn_wetter)$")
lade_definitionen("27_plot_datenexplorer.R", "^(modvege_|simuliere_wachstumspotenzial$)")

## Messungen und Standorte
suppressMessages(source("01_import_googlesheet.R"))
mess <- daten %>% filter(!is.na(growth), !is.na(lon), as.integer(year) %in% jahre) %>%
  group_by(place, date) %>% summarise(growth = mean(growth), lon = first(lon), lat = first(lat), masl = first(masl), .groups = "drop") %>%
  arrange(place, date) %>% group_by(place) %>%
  mutate(start = pmax(lag(date), date - 14)) %>% ungroup() %>%
  mutate(start = if_else(is.na(start), date - 7, start), jahr = as.integer(format(date, "%Y")))
standorte <- mess %>% group_by(place) %>% summarise(lon = first(lon), lat = first(lat), masl = first(masl),
                                                    jahre = list(sort(unique(jahr))), .groups = "drop")
cat("Standorte:", nrow(standorte), " Messungen:", nrow(mess), "\n")

## Wetterstation je Standort
meta <- read.csv(file.path(smn_meta_dir, "meta_stations.csv"), sep = ";", fileEncoding = "ISO-8859-1")
inv <- read.csv(file.path(smn_meta_dir, "meta_datainventory.csv"), sep = ";", fileEncoding = "ISO-8859-1")
mit_strahlung <- unique(inv$station_abbr[inv$parameter_shortname == "gre000d0" & trimws(inv$data_till) == ""])
meta <- meta[meta$station_abbr %in% mit_strahlung, ]
standorte$station <- NA_character_; standorte$station_hoehe <- NA_real_
for (i in seq_len(nrow(standorte))) {
  dist_km <- sqrt(((meta$station_coordinates_wgs84_lon - standorte$lon[i]) * cos(standorte$lat[i] * pi / 180) * 111)^2 +
                  ((meta$station_coordinates_wgs84_lat - standorte$lat[i]) * 111)^2)
  k <- which.min(dist_km + 5 * abs(meta$station_height_masl - standorte$masl[i]) / 100)
  standorte$station[i] <- meta$station_abbr[k]; standorte$station_hoehe[i] <- meta$station_height_masl[k]
}
print(standorte %>% select(place, masl, station, station_hoehe))

wetter <- list()
for (i in seq_len(nrow(standorte))) {
  w <- lade_smn_wetter(standorte$station[i], standorte$lat[i])
  korr <- (standorte$station_hoehe[i] - standorte$masl[i]) * 0.0065
  w$Ta <- w$Ta + korr; w$Tmin <- w$Tmin + korr; w$Tmax <- w$Tmax + korr
  # ET0 mit korrigierten Temperaturen neu (Hargreaves wie in 43)
  ra_mm <- berechne_ra_punkt(w$DOY, standorte$lat[i] * pi / 180) * 0.408
  w$ET0 <- 0.0023 * (w$Ta + 17.8) * sqrt(pmax(w$Tmax - w$Tmin, 0)) * ra_mm
  w$datum <- as.Date(sprintf("%d-01-01", w$year)) + w$DOY - 1
  wetter[[standorte$place[i]]] <- w
}

## Sentinel-2-NDVI im 300-m-Umkreis je Standort und Jahr (parallel, Cache)
stac_suche_punkt <- function(lon, lat, von, bis) {
  body <- list(collections = list("sentinel-2-c1-l2a"), intersects = list(type = "Point", coordinates = c(lon, lat)),
               datetime = paste0(von, "T00:00:00Z/", bis, "T23:59:59Z"), limit = 500,
               query = list(`eo:cloud_cover` = list(lt = 80)))
  f <- tempfile(fileext = ".json"); writeLines(toJSON(body, auto_unbox = TRUE, digits = 8), f)
  a <- system2("curl", c("-s", "-m", "120", "-X", "POST", "https://earth-search.aws.element84.com/v1/search",
                         "-H", shQuote("Content-Type: application/json"), "--data", paste0("@", f)), stdout = TRUE)
  fromJSON(paste(a, collapse = ""), simplifyVector = FALSE)$features
}
ndvi_standort_jahr <- function(i, jahr) {
  datei <- file.path(ndvi_dir, sprintf("%s_%d.rds", gsub("[^A-Za-z0-9]", "_", standorte$place[i]), jahr))
  if (file.exists(datei)) return(readRDS(datei))
  pt <- st_sfc(st_point(c(standorte$lon[i], standorte$lat[i])), crs = 4326)
  kreis <- st_buffer(st_transform(pt, 32632), radius_m)
  gitter <- rast(ext(vect(kreis)), resolution = 10, crs = "EPSG:32632")
  la <- floor(standorte$lat[i] / 3) * 3; lo <- floor(standorte$lon[i] / 3) * 3
  wc <- rast(sprintf("/vsicurl/https://esa-worldcover.s3.eu-central-1.amazonaws.com/v200/2021/map/ESA_WorldCover_10m_2021_v200_N%02dE%03d_Map.tif", la, lo))
  wc <- crop(wc, ext(project(vect(kreis), crs(wc))))
  maske <- values(project(wc, gitter, method = "near"))[, 1] == 30 &
           values(rasterize(vect(kreis), gitter))[, 1] %in% 1
  maske[is.na(maske)] <- FALSE
  items <- stac_suche_punkt(standorte$lon[i], standorte$lat[i], sprintf("%d-03-01", jahr), sprintf("%d-11-30", jahr))
  zeilen <- lapply(items, function(it) tryCatch({
    lese <- function(key, m) { r <- rast(paste0("/vsicurl/", it$assets[[key]]$href)); project(crop(r, ext(project(vect(kreis), crs(r)))), gitter, method = m) }
    scl <- values(lese("scl", "near"))[, 1]
    ok <- maske & scl %in% c(4, 5)
    if (sum(ok) < 0.5 * sum(maske)) NULL else {
      rot <- values(lese("red", "bilinear"))[, 1]; nir <- values(lese("nir", "bilinear"))[, 1]
      data.table(datum = as.Date(substr(it$properties$datetime, 1, 10)), ndvi = median(((nir - rot) / (nir + rot))[ok], na.rm = TRUE),
                 anteil = sum(ok) / sum(maske))
    }
  }, error = function(e) NULL))
  z <- rbindlist(zeilen)
  if (nrow(z) > 0) z <- z[order(-anteil)][, .SD[1], by = datum][order(datum)]
  z[, `:=`(place = standorte$place[i], gruen_pixel = sum(maske))]
  saveRDS(z, datei)
  z
}
auftraege <- do.call(rbind, lapply(seq_len(nrow(standorte)), function(i) data.frame(i = i, jahr = standorte$jahre[[i]])))
cat("Sentinel-Abfragen (Standort x Jahr):", nrow(auftraege), "\n")
ndvi_liste <- mclapply(seq_len(nrow(auftraege)), function(k) ndvi_standort_jahr(auftraege$i[k], auftraege$jahr[k]),
                       mc.cores = 4, mc.preschedule = FALSE)
fehler <- vapply(ndvi_liste, inherits, logical(1), "try-error")
if (any(fehler)) cat("Fehlgeschlagen:", sum(fehler), "\n")
ndvi <- rbindlist(ndvi_liste[!fehler], fill = TRUE)
cat("NDVI-Termine:", nrow(ndvi), "\n")

## ModVege am Messpunkt (Vergleich): automatische Schnitte, Posieux-Parameter
gp <- local({
  pr <- growR::ModvegeParameters$new(system.file("extdata", "posieux_parameters.csv", package = "growR"))
  felder <- intersect(names(modvege_P), names(pr))
  felder <- felder[vapply(felder, function(n) is.numeric(pr[[n]]) && length(pr[[n]]) == 1, logical(1))]
  setNames(lapply(felder, function(n) pr[[n]]), felder)
})
modvege_tag <- list()
for (i in seq_len(nrow(standorte))) for (jahr in standorte$jahre[[i]]) {
  w <- wetter[[standorte$place[i]]]; w <- w[w$year == jahr, ]
  if (nrow(w) < 200) next
  m1 <- function(x) matrix(x, nrow = 1)
  e <- simuliere_wachstumspotenzial(m1(w$Ta), m1(w$precip), m1(w$PAR), m1(w$ET0), jahr, mit_lai = TRUE,
                                    schnitt_hoehe = standorte$masl[i], parameter = gp)
  dbm <- c(0, diff(e$cBM[1, ]))
  modvege_tag[[length(modvege_tag) + 1]] <- data.table(place = standorte$place[i], datum = w$datum, gro = e$GRO[1, ], dbm = dbm)
}
modvege_tag <- rbindlist(modvege_tag)

## Merkmale je Messintervall
ndvi_bei <- function(n, tage) {
  if (nrow(n) < 2) return(rep(NA_real_, length(tage)))
  approx(as.numeric(n$datum), n$ndvi, xout = as.numeric(tage), rule = 1)$y
}
merkmale <- rbindlist(lapply(seq_len(nrow(mess)), function(k) {
  r <- mess[k, ]; w <- wetter[[r$place]]; n <- ndvi[place == r$place][order(datum)]
  iv <- w[w$datum > r$start & w$datum <= r$date, ]; v30 <- w[w$datum > r$date - 30 & w$datum <= r$date, ]
  n_nah <- n[datum >= r$start - 10 & datum <= r$date + 10]
  mv <- modvege_tag[place == r$place & datum > r$start & datum <= r$date]
  data.table(place = r$place, datum = r$date, jahr = r$jahr, growth = r$growth, tage = as.numeric(r$date - r$start),
    doy = as.integer(format(r$date, "%j")), hoehe = r$masl,
    ndvi_start = ndvi_bei(n, r$start), ndvi_ende = ndvi_bei(n, r$date),
    ndvi_max30 = if (nrow(n[datum > r$date - 30 & datum <= r$date])) max(n[datum > r$date - 30 & datum <= r$date]$ndvi) else NA_real_,
    ndvi_abstand = if (nrow(n_nah)) min(abs(as.numeric(n_nah$datum - r$date))) else NA_real_,
    ta = mean(iv$Ta), gdd = sum(pmax(iv$Ta - 5, 0)), niederschlag = sum(iv$precip), strahlung = mean(iv$SRad),
    et0 = sum(iv$ET0), wasserbilanz30 = sum(v30$precip - v30$ET0),
    modvege_dbm = if (nrow(mv)) mean(mv$dbm) else NA_real_, modvege_gro = if (nrow(mv)) mean(mv$gro) else NA_real_)
}))
merkmale[, ndvi_diff := ndvi_ende - ndvi_start]
saveRDS(list(merkmale = merkmale, standorte = standorte, ndvi = ndvi), file.path(aus_dir, "trainingsdaten.rds"))
cat("\nMerkmale:", nrow(merkmale), "Messungen, davon mit NDVI an beiden Intervallenden:",
    sum(!is.na(merkmale$ndvi_start) & !is.na(merkmale$ndvi_ende)), "\n")
print(merkmale[, .(n = .N, mit_ndvi = sum(!is.na(ndvi_ende)), growth = round(mean(growth), 1)), by = place])
