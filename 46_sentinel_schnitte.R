# Schnitterkennung je Gruenlandparzelle aus Sentinel-2 fuer ein Testgebiet.
#
# 1. Parzellen: LANDKULP-Nutzungsflaechen (Wiesen/Weiden) im Testgebiet,
#    10 m nach innen gepuffert, damit nur reine Parzellenpixel zaehlen.
# 2. NDVI je Aufnahme und Parzelle (Median der wolkenfreien Pixel laut
#    Szenenklassifikation SCL) - Sentinel-2 L2A Collection 1 ueber Earth
#    Search (AWS), ohne Login. Zwischenstand je Jahr im Cache, abbrechbar.
# 3. Schnitterkennung nach der optischen Sen4CAP-Logik (Port von
#    schnitte_erkennen() aus r-futterbaugutachten/lib_hrvpp.R, Schwellen auf
#    NDVI statt PPI abgestimmt): scharfer Einbruch zwischen zwei
#    wolkenfreien Beobachtungen, bestaetigt durch Wiederaufwuchs.
#    Plausibilitaet: extensive Wiesen (611) duerfen gesetzlich erst ab
#    15.6. (Tal/Huegel), 1.7. (Bergzone I/II) bzw. 15.7. (III/IV) geschnitten
#    werden - erkannte Schnitte davor sind Fehlalarme oder Beweidung.
#    Gegen Fehlalarme: Ausreisserfilter, der Bestand muss nach dem Einbruch
#    einige Tage tief bleiben, keine Schnitte vor dem 10. April.
#
# Steuerung: SCHNITT_GEBIET (Default emmental), SCHNITT_JAHRE (2025,2026),
# SCHNITT_MAX_AUFNAHMEN (>0 = Testlauf mit so vielen Aufnahmen).
# Eigenstaendig wie 43-45 - NICHT Teil von 00_automate.R.

suppressPackageStartupMessages({
  library(dplyr); library(sf); library(terra); library(data.table); library(jsonlite); library(sfarrow); library(ggplot2)
})

gebiete <- list(
  # 10 x 10 km um Sumiswald/Affoltern i.E. - das Fenster mit den meisten
  # begutachteten Betrieben (Futterbaugutachten) in Emmental/Oberaargau.
  emmental = list(xmin = 2619000, ymin = 1208000, xmax = 2629000, ymax = 1218000)
)
gebiet_name <- Sys.getenv("SCHNITT_GEBIET", "emmental")
gebiet <- gebiete[[gebiet_name]]
jahre <- as.integer(strsplit(Sys.getenv("SCHNITT_JAHRE", "2025,2026"), ",")[[1]])
max_aufnahmen <- as.integer(Sys.getenv("SCHNITT_MAX_AUFNAHMEN", "0")) # >0: Testlauf
gruenland_codes <- c(601, 611, 612, 613, 616, 617)
innenpuffer_m <- 10
min_pixel <- 4
min_anteil_wolkenfrei <- 0.6
aus_dir <- file.path("outputs/sentinel_schnitte", gebiet_name)
dir.create(aus_dir, recursive = TRUE, showWarnings = FALSE)
kontext_datei <- "../r-futterbaugutachten/outputs/region_emmental_oberaargau/_zwischen_kennwerte.rds"

## 1. Parzellen ---------------------------------------------------------------
parz_datei <- file.path(aus_dir, "parzellen.rds")
if (file.exists(parz_datei)) {
  parz <- readRDS(parz_datei)
} else {
  nf <- st_read_parquet("../geodata/landkulp_nuflpro.parquet") |> select(-bbox) |> st_set_crs(2056)
  nf <- nf[nf$lnf_code %in% gruenland_codes & !st_is_empty(nf), ]
  # Mittelpunkt der Bounding Box statt Zentroid: einzelne LANDKULP-Geometrien
  # sind defekt, st_centroid() bricht daran ab.
  bb <- do.call(rbind, lapply(st_geometry(nf), function(g) as.numeric(st_bbox(g))))
  mx <- (bb[, 1] + bb[, 3]) / 2; my <- (bb[, 2] + bb[, 4]) / 2
  parz <- nf[mx >= gebiet$xmin & mx < gebiet$xmax & my >= gebiet$ymin & my < gebiet$ymax, ] |>
    st_make_valid() |>
    transmute(geoid, lnf_code, kultur = kultprot_kultur_de, beweid)
  parz$flaeche_m2 <- as.numeric(st_area(parz))
  if (file.exists(kontext_datei)) {
    kontext <- readRDS(kontext_datei)$parzellen |> distinct(geoid, .keep_all = TRUE) |>
      select(geoid, betrieb_nr, zone, hoehe, begutachtet)
    parz <- parz |> left_join(kontext, by = "geoid")
  }
  parz$parz_id <- seq_len(nrow(parz))
  saveRDS(parz, parz_datei)
}
cat("Gruenlandparzellen im Gebiet:", nrow(parz), "\n")

fenster_ll <- st_transform(st_as_sfc(st_bbox(unlist(gebiet)[c("xmin", "ymin", "xmax", "ymax")], crs = st_crs(2056))), 4326)
fenster_utm <- st_transform(st_as_sfc(st_bbox(unlist(gebiet)[c("xmin", "ymin", "xmax", "ymax")], crs = st_crs(2056))), 32632)
e <- ext(vect(fenster_utm))
gitter <- rast(ext(floor(e$xmin / 10) * 10, ceiling(e$xmax / 10) * 10, floor(e$ymin / 10) * 10, ceiling(e$ymax / 10) * 10),
               resolution = 10, crs = "EPSG:32632")
innen <- st_buffer(st_transform(parz, 32632), -innenpuffer_m)
innen <- innen[!st_is_empty(innen), ]
id_r <- rasterize(vect(innen), gitter, field = "parz_id")
zell_id <- values(id_r)[, 1]
n_pixel <- tabulate(zell_id[!is.na(zell_id)], nbins = nrow(parz))
parz$n_pixel <- n_pixel
cat("Parzellen mit >=", min_pixel, "reinen Pixeln:", sum(n_pixel >= min_pixel), "\n")
print(parz |> st_drop_geometry() |> group_by(lnf_code) |> summarise(n = n(), auswertbar = sum(n_pixel >= min_pixel)))
saveRDS(parz, parz_datei)

## 2. NDVI je Aufnahme und Parzelle --------------------------------------------
stac_suche <- function(von, bis) {
  koord <- st_coordinates(fenster_ll)[, 1:2]
  body <- list(collections = list("sentinel-2-c1-l2a"),
               intersects = list(type = "Polygon", coordinates = list(unname(split(koord, seq_len(nrow(koord)))))),
               datetime = paste0(von, "T00:00:00Z/", bis, "T23:59:59Z"), limit = 1000,
               query = list(`eo:cloud_cover` = list(lt = 85)))
  f <- tempfile(fileext = ".json"); writeLines(toJSON(body, auto_unbox = TRUE, digits = 8), f)
  antwort <- system2("curl", c("-s", "-m", "120", "-X", "POST", "https://earth-search.aws.element84.com/v1/search",
                               "-H", shQuote("Content-Type: application/json"), "--data", paste0("@", f)), stdout = TRUE)
  a <- fromJSON(paste(antwort, collapse = ""), simplifyVector = FALSE)
  if (!is.null(a$context) && a$context$matched > a$context$returned) warning("Nicht alle Aufnahmen geliefert: ", a$context$matched)
  a$features
}

lese_band <- function(item, key, methode) {
  r <- rast(paste0("/vsicurl/", item$assets[[key]]$href))
  r <- crop(r, ext(project(vect(fenster_utm), crs(r))))
  project(r, gitter, method = methode)
}

ndvi_je_jahr <- list()
for (jahr in jahre) {
  cache <- file.path(aus_dir, sprintf("ndvi_%d.rds", jahr))
  zw <- if (file.exists(cache)) readRDS(cache) else list(daten = data.table(), erledigt = character(0))
  items <- stac_suche(sprintf("%d-03-15", jahr), sprintf("%d-11-15", jahr))
  offen <- Filter(function(it) !(it$id %in% zw$erledigt), items)
  if (max_aufnahmen > 0) offen <- head(offen, max_aufnahmen)
  cat("\n", jahr, ": Aufnahmen", length(items), "- noch offen", length(offen), "\n")
  for (k in seq_along(offen)) {
    it <- offen[[k]]
    erg <- tryCatch({
      scl <- values(lese_band(it, "scl", "near"))[, 1]
      gueltig <- scl %in% c(4, 5) & !is.na(zell_id)
      if (sum(gueltig) < 1000) NULL else {
        rot <- values(lese_band(it, "red", "bilinear"))[, 1]
        nir <- values(lese_band(it, "nir", "bilinear"))[, 1]
        dt <- data.table(parz_id = zell_id[gueltig], ndvi = ((nir - rot) / (nir + rot))[gueltig])[is.finite(ndvi)]
        r <- dt[, .(ndvi = median(ndvi), n = .N), by = parz_id]
        r[, anteil := n / n_pixel[parz_id]]
        r <- r[anteil >= min_anteil_wolkenfrei & n_pixel[parz_id] >= min_pixel]
        r[, `:=`(datum = as.Date(substr(it$properties$datetime, 1, 10)), item = it$id)]
        r
      }
    }, error = function(e) { cat("  Fehler", it$id, ":", conditionMessage(e), "\n"); NULL })
    if (!is.null(erg)) zw$daten <- rbind(zw$daten, erg)
    zw$erledigt <- c(zw$erledigt, it$id)
    if (k %% 5 == 0 || k == length(offen)) {
      saveRDS(zw, cache)
      cat("  ", k, "/", length(offen), "- Beobachtungen bisher:", nrow(zw$daten), "\n")
    }
  }
  # Ueberlappende Kacheln/Orbits liefern denselben Tag doppelt: je Parzelle
  # und Tag die Beobachtung mit dem groessten wolkenfreien Anteil.
  d <- zw$daten[order(-anteil)][, .SD[1], by = .(parz_id, datum)]
  d[, jahr := jahr]
  ndvi_je_jahr[[as.character(jahr)]] <- d
}
ndvi <- rbindlist(ndvi_je_jahr)
saveRDS(ndvi, file.path(aus_dir, "ndvi_parzellen.rds"))
cat("\nNDVI-Beobachtungen gesamt:", nrow(ndvi), " Termine je Parzelle und Jahr (Median):",
    median(ndvi[, .N, by = .(parz_id, jahr)]$N), "\n")

## 3. Schnitterkennung --------------------------------------------------------
# Port von schnitte_erkennen() (r-futterbaugutachten/lib_hrvpp.R), Schwellen
# fuer NDVI: Gruenland liegt vor dem Schnitt meist bei 0.75-0.9, danach bei
# 0.4-0.6.
schnitte_erkennen <- function(datum, ndvi, min_rel = 0.15, min_abs = 0.12, max_luecke = 20, min_abstand = 18,
                              min_vor = 0.60, nachwuchs_min = 0.10, nachwuchs_fenster = 45,
                              frueheste = NULL, tief_fenster = 8, tief_toleranz = 0.05, spike = 0.10) {
  o <- order(datum); d <- datum[o]; v <- ndvi[o]
  # Ausreisser (unerkannte Wolken/Dunst/Schatten): einzelner tiefer Wert, beide
  # Nachbarn (je <= 10 Tage entfernt) hoch und untereinander aehnlich.
  if (length(v) >= 3) {
    weg <- vapply(2:(length(v) - 1), function(i) {
      as.numeric(d[i] - d[i - 1]) <= 10 && as.numeric(d[i + 1] - d[i]) <= 10 &&
        v[i] < min(v[i - 1], v[i + 1]) - spike && abs(v[i + 1] - v[i - 1]) < tief_toleranz
    }, logical(1))
    weg <- c(FALSE, weg, FALSE); d <- d[!weg]; v <- v[!weg]
  }
  leer <- data.table(schnitt = as.Date(character(0)), letzte_hoch = as.Date(character(0)), erste_tief = as.Date(character(0)))
  if (length(v) < 4) return(leer)
  kand <- as.Date(character(0)); staerke <- numeric(0); hoch <- as.Date(character(0)); tief <- as.Date(character(0))
  for (i in seq_len(length(v) - 1)) {
    luecke <- as.numeric(d[i + 1] - d[i])
    if (luecke <= 0 || luecke > max_luecke || v[i] < min_vor) next
    abfall <- v[i] - v[i + 1]
    if (abfall < min_abs || abfall / v[i] < min_rel) next
    # Echter Schnitt: der Bestand bleibt einige Tage tief - kommt der alte
    # Wert innert tief_fenster Tagen fast zurueck, war es kein Schnitt.
    kurz <- which(d > d[i + 1] & d <= d[i + 1] + tief_fenster)
    if (length(kurz) > 0 && max(v[kurz]) >= v[i] - tief_toleranz) next
    spaeter <- which(d > d[i + 1] & d <= d[i + 1] + nachwuchs_fenster)
    if (length(spaeter) > 0 && max(v[spaeter]) - v[i + 1] < nachwuchs_min) next
    termin <- d[i] + round(luecke / 2)
    if (!is.null(frueheste) && termin < frueheste) next
    kand <- c(kand, termin); staerke <- c(staerke, abfall); hoch <- c(hoch, d[i]); tief <- c(tief, d[i + 1])
  }
  behalten <- integer(0)
  for (i in order(staerke, decreasing = TRUE)) {
    if (length(behalten) == 0 || min(abs(as.numeric(kand[i] - kand[behalten]))) >= min_abstand) behalten <- c(behalten, i)
  }
  if (length(behalten) == 0) return(leer)
  behalten <- behalten[order(kand[behalten])]
  data.table(schnitt = kand[behalten], letzte_hoch = hoch[behalten], erste_tief = tief[behalten])
}

schnitte <- ndvi[, schnitte_erkennen(datum, ndvi, frueheste = as.Date(sprintf("%d-04-10", jahr[1]))), by = .(parz_id, jahr)]
schnitte <- schnitte[!is.na(schnitt)]
saveRDS(schnitte, file.path(aus_dir, "schnitte.rds"))

attr_tab <- as.data.table(st_drop_geometry(parz))
kennwerte <- ndvi[, .(termine = .N), by = .(parz_id, jahr)] |>
  merge(schnitte[, .(n_schnitte = .N, erster_schnitt = min(schnitt), erster_tief = min(erste_tief)), by = .(parz_id, jahr)], all.x = TRUE) |>
  merge(attr_tab, by = "parz_id")
kennwerte[is.na(n_schnitte), n_schnitte := 0L]
saveRDS(kennwerte, file.path(aus_dir, "kennwerte.rds"))

cat("\n=== Erkannte Schnitte je Kultur (Median, Anteil Parzellen mit 0 Schnitten) ===\n")
print(kennwerte[, .(parzellen = .N, termine_median = as.numeric(median(termine)), schnitte_median = as.numeric(median(n_schnitte)),
                    schnitte_mittel = round(mean(n_schnitte), 2), anteil_ohne = round(mean(n_schnitte == 0), 2)),
                by = .(jahr, lnf_code, kultur)][order(jahr, lnf_code)])

# Plausibilitaet: fruehester erlaubter Schnitt extensiver Wiesen
fruehester <- function(jahr, zone) as.Date(ifelse(zone %in% c("31", "41"), sprintf("%d-06-15", jahr),
                                           ifelse(zone %in% c("51", "52"), sprintf("%d-07-01", jahr), sprintf("%d-07-15", jahr))))
ext611 <- kennwerte[lnf_code == 611 & !is.na(zone)]
if (nrow(ext611) > 0) {
  ext611[, frueh := fruehester(jahr, as.character(zone))]
  # Sicher zu frueh nur, wenn schon die erste tiefe Aufnahme vor dem erlaubten
  # Termin liegt (der geschaetzte Schnitttag liegt irgendwo in der Wolkenluecke).
  ext611[, zu_frueh := !is.na(erster_tief) & erster_tief < frueh]
  cat("\n=== Extensive Wiesen (611): erster Schnitt sicher vor dem erlaubten Termin (erste tiefe Aufnahme davor) ===\n")
  print(ext611[, .(parzellen = .N, mit_schnitt = sum(n_schnitte > 0), zu_frueh = sum(zu_frueh),
                   anteil_zu_frueh = round(sum(zu_frueh) / max(1, sum(n_schnitte > 0)), 2)), by = .(jahr, zone)][order(jahr, zone)])
}

# Grafiken: Verteilung Schnittzahl und Beispielverlaeufe
p1 <- ggplot(kennwerte, aes(factor(n_schnitte), fill = factor(jahr))) + geom_bar(position = "dodge") +
  facet_wrap(~kultur, scales = "free_y") + labs(x = "erkannte Schnitte", y = "Parzellen", fill = "Jahr",
  title = paste("Erkannte Schnitte je Parzelle -", gebiet_name)) + theme_minimal()
ggsave(file.path(aus_dir, "schnittzahl_je_kultur.png"), p1, width = 12, height = 7, dpi = 120)
set.seed(1)
beispiele <- kennwerte[jahr == max(jahre) & lnf_code %in% c(601, 611, 613, 616) & termine >= 15][, .SD[sample(.N, min(.N, 3))], by = lnf_code]
bsp <- ndvi[jahr == max(jahre) & parz_id %in% beispiele$parz_id] |> merge(beispiele[, .(parz_id, kultur)], by = "parz_id")
bsp_s <- schnitte[jahr == max(jahre) & parz_id %in% beispiele$parz_id] |> merge(beispiele[, .(parz_id, kultur)], by = "parz_id")
p2 <- ggplot(bsp, aes(datum, ndvi)) + geom_line(color = "grey50") + geom_point(size = 1) +
  geom_vline(data = bsp_s, aes(xintercept = schnitt), color = "red", linetype = "dashed") +
  facet_wrap(~ paste(kultur, parz_id), ncol = 3) + labs(title = paste("NDVI und erkannte Schnitte (rot),", max(jahre)), x = NULL, y = "NDVI") +
  theme_minimal() + theme(strip.text = element_text(size = 7))
ggsave(file.path(aus_dir, "beispiele_schnitte.png"), p2, width = 12, height = 10, dpi = 120)
cat("\nErgebnisse in", aus_dir, "\n")
