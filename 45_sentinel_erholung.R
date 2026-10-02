# Prueft die Erholungsverzoegerung des Potenziellen Wachstums (Stufen 0/7/14/
# 21 Tage, siehe erholung_stufen_tage in 27_plot_datenexplorer.R) an
# Sentinel-2: Wie schnell ergruent Gruenland rund um die AGFF-Standorte nach
# dem Regen, der die Sommertrockenheit 2026 beendet - und welche Stufe trifft
# diesen Verlauf am besten?
#
# Modell: ModVege-Port MIT automatischen Schnitten nach growR (Intensitaet
# "high", gegen growR exakt validiert) - ein geschnittener Bestand hat wie
# die reale Landschaft wenig Blattflaeche, sodass Trockenheitseinbruch und
# Erholung in der Bodenbedeckung sichtbar werden. Der NDVI ist ein Median
# ueber viele Parzellen mit verschiedenen Schnittterminen, deshalb wird ueber
# 5 gestaffelte Schnittregime gemittelt (Zielbiomasse/Intervall wie fuer
# Hoehe +-200 m; das Wachstum selbst bleibt gleich).
# Bodenbedeckung = 1 - exp(-0.6 * LAI_gruen) (gleiche Extinktion wie ModVege).
#
# Vergleich je Standort im Fenster 30 Tage vor bis 35 Tage nach dem Regen:
# NDVI = a + b * Bedeckung (lineare Anpassung je Standort und Stufe), die
# Stufe mit dem kleinsten Restfehler passt den Verlauf am besten.
# Einschraenkungen: Wolkenluecken, Schnittregime nur angenaehert, NDVI ist
# nicht streng linear in der Bedeckung.
#
# Daten (ohne Login): Sentinel-2 L2A Collection 1 ueber Earth Search (AWS),
# Gruenlandmaske ESA WorldCover 2021 (Klasse 30). Eigenstaendig wie 43/44 -
# NICHT Teil von 00_automate.R.

suppressPackageStartupMessages({
  library(dplyr); library(sf); library(terra); library(ggplot2); library(jsonlite)
})

ausgabe_dir <- "outputs/vergleich_wachstumsmodelle/sentinel"
dir.create(ausgabe_dir, recursive = TRUE, showWarnings = FALSE)
growr_input_dir <- "outputs/vergleich_wachstumsmodelle/growr_input"
jahr <- 2026
fenster_tage <- 35
vorlauf_tage <- 30
schnitt_versatz_m <- c(-200, -100, 0, 100, 200)

## ModVege-Port aus 27 uebernehmen (nur die Definitionen, nicht die Pipeline) -
## so rechnet die Pruefung garantiert mit demselben Code wie die Karte.
ausdruecke <- parse("27_plot_datenexplorer.R", keep.source = FALSE)
for (a in ausdruecke) {
  if (is.call(a) && identical(a[[1]], as.name("<-")) && is.name(a[[2]]) &&
      grepl("^(modvege_|simuliere_wachstumspotenzial$|erholung_stufen_tage$)", as.character(a[[2]]))) {
    eval(a, envir = globalenv())
  }
}
stopifnot(exists("simuliere_wachstumspotenzial"), exists("erholung_stufen_tage"))
# modvege_P$minBMGV etc. sind Folgezuweisungen ($<-) - separat nachziehen.
for (a in ausdruecke) {
  if (is.call(a) && identical(a[[1]], as.name("<-")) && is.call(a[[2]]) &&
      identical(a[[2]][[1]], as.name("$")) && identical(a[[2]][[2]], as.name("modvege_P"))) {
    eval(a, envir = globalenv())
  }
}

punkte <- readRDS("outputs/vergleich_wachstumsmodelle/punkte_auswahl.rds")
punkte$punkt_id <- paste0("p", seq_len(nrow(punkte)), "_", gsub("[^A-Za-z0-9]", "", punkte$smn_abbr))
punkte <- punkte[punkte$typ == "AGFF", ]

## 1. Modell je Standort und Stufe, Regentag nach der Trockenheit -------------
modell <- list(); regentage <- list()
for (i in seq_len(nrow(punkte))) {
  w <- read.table(file.path(growr_input_dir, paste0(punkte$punkt_id[i], "_weather.txt")), header = TRUE, sep = "\t")
  w <- w[w$year == jahr, ]
  w <- w[order(w$DOY), ]
  tage <- as.Date(sprintf("%d-01-01", jahr)) + w$DOY - 1
  m <- function(x) matrix(x, nrow = 1)
  laeufe <- lapply(erholung_stufen_tage, function(st) {
    bed <- sapply(schnitt_versatz_m, function(dh) {
      e <- simuliere_wachstumspotenzial(Ta = m(w$Ta), precip = m(w$precip), PAR = m(w$PAR), ET0 = m(w$ET0),
                                        jahr = jahr, erholung_tage = st, schnitt_hoehe = punkte$hoehe[i] + dh, mit_lai = TRUE)
      1 - exp(-0.6 * e$LAI[1, ])
    })
    rowMeans(bed)
  })
  names(laeufe) <- paste0("e", erholung_stufen_tage)

  # Regentag: erster Tag mit >= 15 mm nach dem trockensten Punkt Jul-Aug. Der
  # trockenste Punkt wird ueber 21-Tage-Niederschlagssummen bestimmt (robust,
  # unabhaengig vom Modell-Wasserhaushalt).
  sommer <- which(format(tage, "%m") %in% c("07", "08"))
  summe21 <- stats::filter(w$precip, rep(1, 21), sides = 1)
  trockenst <- sommer[which.min(summe21[sommer])]
  kandidaten <- which(seq_along(tage) > trockenst & w$precip >= 15)
  if (length(kandidaten) == 0) { cat(punkte$id[i], ": kein Regentag >= 15 mm nach der Trockenheit\n"); next }
  regentag <- tage[kandidaten[1]]
  regentage[[punkte$id[i]]] <- data.frame(id = punkte$id[i], regentag = regentag,
    niederschlag_21d_vorher = round(summe21[trockenst]), regen_mm = w$precip[kandidaten[1]])
  modell[[punkte$id[i]]] <- bind_rows(lapply(names(laeufe), function(sk)
    data.frame(id = punkte$id[i], stufe = sk, datum = tage, bedeckung = laeufe[[sk]])))
}
regentage <- bind_rows(regentage)
cat("\nRegentage nach der Trockenheit:\n"); print(regentage)

## 2. Sentinel-2-NDVI je Standort ---------------------------------------------
stac_suche <- function(lon, lat, von, bis) {
  body <- list(collections = list("sentinel-2-c1-l2a"),
               intersects = list(type = "Point", coordinates = c(lon, lat)),
               datetime = paste0(von, "T00:00:00Z/", bis, "T23:59:59Z"), limit = 200,
               query = list(`eo:cloud_cover` = list(lt = 90)))
  f <- tempfile(fileext = ".json")
  writeLines(toJSON(body, auto_unbox = TRUE), f)
  antwort <- system2("curl", c("-s", "-m", "60", "-X", "POST", "https://earth-search.aws.element84.com/v1/search",
                               "-H", shQuote("Content-Type: application/json"), "--data", paste0("@", f)), stdout = TRUE)
  fromJSON(paste(antwort, collapse = ""), simplifyVector = FALSE)$features
}

worldcover_kachel <- function(lon, lat) {
  la <- floor(lat / 3) * 3; lo <- floor(lon / 3) * 3
  sprintf("/vsicurl/https://esa-worldcover.s3.eu-central-1.amazonaws.com/v200/2021/map/ESA_WorldCover_10m_2021_v200_N%02dE%03d_Map.tif", la, lo)
}

ndvi_cache_datei <- file.path(ausgabe_dir, "ndvi_zeitreihen.rds")
ndvi_alt <- if (file.exists(ndvi_cache_datei)) readRDS(ndvi_cache_datei) else NULL
ndvi_alle <- list()
for (i in seq_len(nrow(punkte))) {
  sid <- punkte$id[i]
  if (!is.null(ndvi_alt) && sid %in% ndvi_alt$id) { ndvi_alle[[sid]] <- ndvi_alt[ndvi_alt$id == sid, ]; next }
  pt <- st_sfc(st_point(c(punkte$lon[i], punkte$lat[i])), crs = 4326)
  fenster_utm <- st_buffer(st_transform(pt, 32632), 1500, endCapStyle = "SQUARE")
  ext_utm <- ext(vect(fenster_utm))
  # Gruenlandmaske auf dem 10m-UTM-Gitter des Fensters
  gitter <- rast(ext_utm, resolution = 10, crs = "EPSG:32632")
  wc <- rast(worldcover_kachel(punkte$lon[i], punkte$lat[i]))
  wc_fenster <- crop(wc, ext(project(vect(fenster_utm), crs(wc))))
  gras <- project(wc_fenster, gitter, method = "near") == 30
  n_gras <- global(gras, "sum", na.rm = TRUE)[[1]]
  cat("\n", sid, "- Gruenlandpixel im Fenster:", n_gras, "\n")

  items <- stac_suche(punkte$lon[i], punkte$lat[i], sprintf("%d-07-01", jahr), sprintf("%d-09-30", jahr))
  cat("  Aufnahmen (Wolken < 90%):", length(items), "\n")
  zeilen <- list()
  for (it in items) {
    lese <- function(key, methode) {
      r <- rast(paste0("/vsicurl/", it$assets[[key]]$href))
      r <- crop(r, project(vect(fenster_utm), crs(r)))
      project(r, gitter, method = methode)
    }
    erg <- tryCatch({
      scl <- lese("scl", "near")
      gueltig <- (scl == 4 | scl == 5) & gras
      n_gueltig <- global(gueltig, "sum", na.rm = TRUE)[[1]]
      if (n_gueltig < 0.3 * n_gras) NULL else {
        # terra wendet Skalierung/Offset aus den COG-Metadaten selbst an
        rot <- lese("red", "bilinear")
        nir <- lese("nir", "bilinear")
        ndvi <- (nir - rot) / (nir + rot)
        v <- values(ndvi)[values(gueltig) %in% TRUE]
        data.frame(id = sid, datum = as.Date(substr(it$properties$datetime, 1, 10)),
                   ndvi = median(v, na.rm = TRUE), anteil_gueltig = n_gueltig / n_gras)
      }
    }, error = function(e) { cat("  Fehler", it$id, ":", conditionMessage(e), "\n"); NULL })
    if (!is.null(erg)) zeilen[[length(zeilen) + 1]] <- erg
  }
  z <- bind_rows(zeilen)
  # Doppelte Daten (ueberlappende Kacheln/Orbits): den mit mehr gueltigen Pixeln
  if (nrow(z) > 0) z <- z %>% group_by(id, datum) %>% slice_max(anteil_gueltig, n = 1, with_ties = FALSE) %>% ungroup()
  cat("  Brauchbare Termine:", nrow(z), "\n")
  ndvi_alle[[sid]] <- z
  saveRDS(bind_rows(ndvi_alle), ndvi_cache_datei)
}
ndvi <- bind_rows(ndvi_alle)

## 3. Vergleich NDVI vs. Modell-Bedeckung -----------------------------------
vergleich <- list(); kurven <- list()
for (k in seq_len(nrow(regentage))) {
  sid <- regentage$id[k]; t0 <- regentage$regentag[k]
  obs <- ndvi %>% filter(id == sid, datum >= t0 - vorlauf_tage, datum <= t0 + fenster_tage)
  if (nrow(obs) < 6 || sum(obs$datum > t0) < 3) { cat(sid, ": zu wenige wolkenfreie Termine im Fenster\n"); next }
  for (sk in paste0("e", erholung_stufen_tage)) {
    mk <- modell[[sid]] %>% filter(stufe == sk)
    d <- obs %>% inner_join(mk %>% select(datum, bedeckung), by = "datum")
    fit <- lm(ndvi ~ bedeckung, data = d)
    nach <- d$datum > t0
    vergleich[[length(vergleich) + 1]] <- data.frame(id = sid, stufe = sk, n_termine = nrow(d),
      rmse = sqrt(mean(resid(fit)^2)), rmse_nach_regen = sqrt(mean(resid(fit)[nach]^2)),
      r2 = summary(fit)$r.squared, steigung = coef(fit)[["bedeckung"]])
    kurven[[paste(sid, sk)]] <- mk %>% filter(datum >= t0 - vorlauf_tage, datum <= t0 + fenster_tage) %>%
      mutate(ndvi_modell = predict(fit, newdata = .), tag = as.numeric(datum - t0))
  }
}
vergleich <- bind_rows(vergleich)
cat("\n=== Restfehler (RMSE NDVI) je Standort und Stufe, Fenster -30 bis +35 Tage ===\n")
print(vergleich %>% select(id, stufe, rmse) %>% tidyr::pivot_wider(names_from = stufe, values_from = rmse) %>%
        mutate(across(where(is.numeric), ~ round(.x, 4))), width = 200)
cat("\nNur nach dem Regen:\n")
print(vergleich %>% select(id, stufe, rmse_nach_regen) %>% tidyr::pivot_wider(names_from = stufe, values_from = rmse_nach_regen) %>%
        mutate(across(where(is.numeric), ~ round(.x, 4))), width = 200)
cat("\nBeste Stufe je Standort:\n")
print(vergleich %>% group_by(id) %>% slice_min(rmse, n = 1) %>% ungroup() %>%
        transmute(id, stufe, rmse = round(rmse, 4), r2 = round(r2, 2), steigung = round(steigung, 2), n_termine))
cat("\nUeber alle Standorte (Mittel):\n")
print(vergleich %>% group_by(stufe) %>% summarise(rmse = round(mean(rmse), 4), rmse_nach_regen = round(mean(rmse_nach_regen), 4),
                                                   r2 = round(mean(r2), 2), standorte = n()))

if (length(kurven) > 0) {
  kv <- bind_rows(kurven)
  obs_alle <- ndvi %>% inner_join(regentage %>% select(id, regentag), by = "id") %>%
    mutate(tag = as.numeric(datum - regentag)) %>% filter(tag >= -vorlauf_tage, tag <= fenster_tage)
  p <- ggplot() +
    geom_vline(xintercept = 0, color = "steelblue", linetype = "dashed") +
    geom_line(data = kv, aes(tag, ndvi_modell, color = stufe), linewidth = 0.8) +
    geom_point(data = obs_alle, aes(tag, ndvi), size = 2) +
    facet_wrap(~id, scales = "free_y") +
    labs(title = "Sentinel-2-NDVI (Punkte) vs. Modell mit Schnitten, je Erholungsstufe (an NDVI angepasst)",
         x = "Tage seit Regen", y = "NDVI", color = "Stufe") +
    theme_minimal()
  ggsave(file.path(ausgabe_dir, "erholung_sentinel_vs_modell.png"), p, width = 12, height = 8, dpi = 120)
  p2 <- ggplot(ndvi, aes(datum, ndvi)) + geom_line(color = "grey50") + geom_point(aes(alpha = anteil_gueltig)) +
    geom_vline(data = regentage, aes(xintercept = regentag), color = "steelblue", linetype = "dashed") +
    facet_wrap(~id) + labs(title = "Sentinel-2-NDVI Gruenland Jul-Sep 2026 (gestrichelt: Regentag)", x = NULL, y = "NDVI (Median)") +
    theme_minimal()
  ggsave(file.path(ausgabe_dir, "ndvi_zeitreihen.png"), p2, width = 11, height = 7, dpi = 120)
}
write.csv(vergleich, file.path(ausgabe_dir, "erholung_vergleich.csv"), row.names = FALSE)
cat("\nErgebnisse in", ausgabe_dir, "\n")
