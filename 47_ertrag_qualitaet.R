# Ertrag und Qualitaet je Schnitt - zwei Methoden im Vergleich:
#   Methode 1 (Satellit + ModVege): Schnitttermine aus Sentinel-2
#     (46_sentinel_schnitte.R) gehen in den ModVege-Port (exakt gegen growR
#     validiert), der je Schnitt Erntemenge (kg TS/ha) und Verdaulichkeit
#     (OMD, vor dem Schnitt) liefert.
#   Methode 3 (statistisches Modell wie SatGrass): siehe Abschnitt C.
#
# A. Validierung Methode 1 an den Agroscope-Exaktversuchen Posieux und Sorens
#    (2013-2022, Schnitt alle 28 Tage, zwei um 14 Tage versetzte Reihen; im
#    R-Paket growR mitgeliefert). Gemessen ist der Zuwachs je Aufwuchs
#    (kg TS/ha/Tag) - verglichen mit der Modell-Erntemenge desselben Schnitts
#    geteilt durch die Aufwuchsdauer. Die Versuchsparzellen sind fuer
#    Sentinel-2 zu klein; hier wird nur der Modellteil von Methode 1
#    geprueft (mit den bekannten Schnittterminen).
#
# Eigenstaendig wie 43-46 - NICHT Teil von 00_automate.R.

suppressPackageStartupMessages({
  library(dplyr); library(data.table); library(ggplot2); library(growR); library(sf); library(terra)
})

aus_dir <- "outputs/ertrag_qualitaet"
dir.create(aus_dir, recursive = TRUE, showWarnings = FALSE)

## ModVege-Port aus 27 (nur Definitionen, siehe 45_sentinel_erholung.R)
for (a in parse("27_plot_datenexplorer.R", keep.source = FALSE)) {
  if (is.call(a) && identical(a[[1]], as.name("<-")) &&
      ((is.name(a[[2]]) && grepl("^(modvege_|simuliere_wachstumspotenzial$)", as.character(a[[2]]))) ||
       (is.call(a[[2]]) && identical(a[[2]][[2]], as.name("modvege_P"))))) eval(a, envir = globalenv())
}
m1 <- function(x) matrix(x, nrow = 1)

# Von growR aufgeloeste Parameter eines Standorts (Artenmischung -> Pflanzen-
# parameter) als Ueberschreibung fuer den Port.
growr_parameter <- function(param_datei, input_dir) {
  pr <- ModvegeParameters$new(file.path(input_dir, param_datei))
  felder <- intersect(names(modvege_P), names(pr))
  felder <- felder[vapply(felder, function(n) is.numeric(pr[[n]]) && length(pr[[n]]) == 1, logical(1))]
  list(parameter = setNames(lapply(felder, function(n) pr[[n]]), felder),
       init = c(setNames(lapply(setdiff(names(modvege_init), "WR"), function(n) pr[[n]]), setdiff(names(modvege_init), "WR")),
                list(WR = pr$WR0)))
}

## A. Validierung an Posieux/Sorens -----------------------------------------
D <- system.file("extdata", package = "growR")
versuch <- list()
for (ort in c("posieux", "sorens")) {
  gp <- growr_parameter(paste0(ort, "_parameters.csv"), D)
  w_all <- read.table(file.path(D, paste0(ort, "_weather.txt")), header = TRUE)
  for (reihe in 1:2) {
    mg <- read.table(file.path(D, paste0(ort, "_management", reihe, ".txt")), header = TRUE)
    for (jahr in sort(unique(mg$year))) {
      w <- w_all[w_all$year == jahr, ]
      sm <- m1(seq_len(nrow(w)) %in% mg$DOY[mg$year == jahr])
      e <- simuliere_wachstumspotenzial(m1(w$Ta), m1(w$precip), m1(w$PAR), m1(w$ET0), jahr, mit_lai = TRUE,
                                        schnitt_matrix = sm, parameter = gp$parameter, init = gp$init)
      tage <- which(sm[1, ])
      versuch[[length(versuch) + 1]] <- data.frame(ort = ort, reihe = reihe, year = jahr, DOY = tage,
        aufwuchs_tage = c(NA, diff(tage)), ernte = e$ERNTE[1, tage], omd = e$OMD[1, tage])
    }
  }
}
versuch <- bind_rows(versuch)
messung <- bind_rows(lapply(c("posieux", "sorens"), function(ort) bind_rows(lapply(1:2, function(beh) {
  f <- file.path(D, paste0(ort, beh, ".csv"))
  x <- read.csv(f, sep = if (grepl(";", readLines(f, 1))) ";" else ",")
  data.frame(ort = ort, behandlung = beh, year = x$year, DOY = x$DOY, gemessen = x$dBM)
}))))
vgl <- versuch %>% filter(!is.na(aufwuchs_tage)) %>%
  mutate(modell = ernte / aufwuchs_tage) %>%
  inner_join(messung, by = c("ort", "year", "DOY"), relationship = "many-to-many") %>%
  filter(!is.na(gemessen))
stat <- function(x) x %>% summarise(n = n(), r = cor(modell, gemessen), bias = mean(modell - gemessen),
  rmse = sqrt(mean((modell - gemessen)^2)), mittel_gemessen = mean(gemessen), mittel_modell = mean(modell), .groups = "drop")
cat("=== A. Methode 1 (ModVege mit bekannten Schnittterminen) vs. Exaktversuch, Zuwachs kg TS/ha/Tag je Aufwuchs ===\n")
print(stat(vgl %>% group_by(ort)), digits = 3)
print(stat(vgl), digits = 3)
cat("\nErtrag je Schnitt (kg TS/ha, Aufwuchs 28 Tage):\n")
print(vgl %>% filter(aufwuchs_tage == 28) %>% group_by(ort) %>%
        summarise(r = round(cor(ernte, gemessen * 28), 2), mittel_gemessen = round(mean(gemessen * 28)), mittel_modell = round(mean(ernte))))
cat("\nJahresertrag je Reihe (Summe aller Schnitte ohne den ersten, kg TS/ha):\n")
print(vgl %>% group_by(ort, behandlung, reihe, year) %>%
        summarise(gemessen = sum(gemessen * aufwuchs_tage), modell = sum(ernte), .groups = "drop") %>%
        group_by(ort) %>% summarise(r = round(cor(gemessen, modell), 2), mittel_gemessen = round(mean(gemessen)), mittel_modell = round(mean(modell))))
cat("\nModell-Verdaulichkeit OMD am Schnitttag (keine Messung vorhanden):\n")
print(versuch %>% group_by(ort) %>% summarise(omd_mittel = round(mean(omd), 3), omd_min = round(min(omd), 3), omd_max = round(max(omd), 3)))
saveRDS(vgl, file.path(aus_dir, "validierung_versuch.rds"))

p <- ggplot(vgl, aes(gemessen, modell, color = ort)) + geom_point(alpha = 0.5) +
  geom_abline(linetype = "dashed") + coord_equal() +
  labs(title = "Methode 1: ModVege mit bekannten Schnittterminen vs. Exaktversuch",
       x = "gemessen (kg TS/ha/Tag je Aufwuchs)", y = "Modell (kg TS/ha/Tag je Aufwuchs)", color = NULL) + theme_minimal()
ggsave(file.path(aus_dir, "validierung_versuch.png"), p, width = 8, height = 7, dpi = 120)

## B. Methode 1 im Testgebiet: Sentinel-Schnitttermine + ModVege je Parzelle
# Wetter: SMN-Station Langnau i.E. (744 m, ~15 km), Temperaturen mit
# 0.65 Grad/100 m auf die Parzellenhoehe korrigiert. Parameter: Posieux-
# Satz (NI 0.7) fuer ALLE Parzellen - fuer extensive Wiesen/Weiden (611,
# 617) ist das zu hoch, deren Ertraege sind entsprechend markiert.
gebiet_name <- Sys.getenv("SCHNITT_GEBIET", "emmental")
sat_dir <- file.path("outputs/sentinel_schnitte", gebiet_name)
if (file.exists(file.path(sat_dir, "schnitte.rds"))) {
  for (a in parse("43_vergleiche_wachstumsmodelle.R", keep.source = FALSE)) {
    if (is.call(a) && identical(a[[1]], as.name("<-")) && is.name(a[[2]]) &&
        as.character(a[[2]]) %in% c("smn_basis_url", "smn_meta_dir", "berechne_ra_punkt", "lade_smn_wetter")) eval(a, envir = globalenv())
  }
  station <- list(abbr = "LAG", hoehe = 744, lat = 46.94)
  parz <- readRDS(file.path(sat_dir, "parzellen.rds"))
  kw <- readRDS(file.path(sat_dir, "kennwerte.rds"))
  schnitte <- readRDS(file.path(sat_dir, "schnitte.rds"))
  gp_pos <- growr_parameter("posieux_parameters.csv", D)
  w_all <- lade_smn_wetter(station$abbr, station$lat)
  w_all$datum <- as.Date(sprintf("%d-01-01", w_all$year)) + w_all$DOY - 1
  ergebnis_m1 <- list()
  for (jahr in sort(unique(kw$jahr))) {
    w <- w_all[w_all$year == jahr, ]
    j_akt <- jahr
    p <- kw[jahr == j_akt]
    hoehe <- ifelse(is.na(p$hoehe), 756, p$hoehe)
    korr <- matrix((station$hoehe - hoehe) * 0.0065, nrow(p), nrow(w))
    rep_m <- function(x) matrix(x, nrow(p), length(x), byrow = TRUE)
    ta <- rep_m(w$Ta) + korr
    ra_mm <- berechne_ra_punkt(w$DOY, station$lat * pi / 180) * 0.408
    et0 <- 0.0023 * (ta + 17.8) * sqrt(rep_m(pmax(w$Tmax - w$Tmin, 0))) * rep_m(ra_mm)
    s_j <- schnitte[jahr == j_akt]
    sm <- matrix(FALSE, nrow(p), nrow(w))
    zeile <- match(s_j$parz_id, p$parz_id); spalte <- match(s_j$schnitt, w$datum)
    ok <- !is.na(zeile) & !is.na(spalte); sm[cbind(zeile[ok], spalte[ok])] <- TRUE
    e <- simuliere_wachstumspotenzial(ta, rep_m(w$precip), rep_m(w$PAR), et0, jahr, mit_lai = TRUE,
                                      schnitt_matrix = sm, parameter = gp_pos$parameter, init = gp_pos$init)
    idx <- which(sm, arr.ind = TRUE)
    ergebnis_m1[[as.character(jahr)]] <- data.table(parz_id = p$parz_id[idx[, 1]], jahr = jahr, schnitt = w$datum[idx[, 2]],
                                                   ernte_m1 = e$ERNTE[idx], omd_m1 = e$OMD[idx])
    cat("Methode 1,", jahr, ":", nrow(p), "Parzellen,", nrow(idx), "Schnitte gerechnet\n")
  }
  m1_schnitte <- rbindlist(ergebnis_m1)
  m1_parz <- m1_schnitte[, .(ertrag_m1 = sum(ernte_m1), omd_m1 = sum(omd_m1 * ernte_m1) / sum(ernte_m1)), by = .(parz_id, jahr)]
  kw_m1 <- merge(kw, m1_parz, by = c("parz_id", "jahr"), all.x = TRUE)
  kw_m1[is.na(ertrag_m1), ertrag_m1 := 0]
  kw_m1[, ni_zu_hoch := lnf_code %in% c(611, 617)]
  saveRDS(list(schnitte = m1_schnitte, parzellen = kw_m1), file.path(aus_dir, paste0("methode1_", gebiet_name, ".rds")))
  cat("\n=== B. Methode 1 im Testgebiet: Jahresertrag (kg TS/ha) und OMD je Kultur, Parzellen mit >=1 Schnitt ===\n")
  print(kw_m1[n_schnitte > 0, .(parzellen = .N, schnitte = round(mean(n_schnitte), 1), ertrag_median = round(median(ertrag_m1)),
                                ertrag_p10 = round(quantile(ertrag_m1, 0.1)), ertrag_p90 = round(quantile(ertrag_m1, 0.9)),
                                omd = round(median(omd_m1, na.rm = TRUE), 3)), by = .(jahr, lnf_code, kultur)][order(jahr, lnf_code)])
  print(m1_schnitte[, .(schnitte = .N, ernte_median = round(median(ernte_m1)), omd_median = round(median(omd_m1), 3)),
                    by = .(jahr, monat = format(schnitt, "%m"))][order(jahr, monat)])
}

## C. Methode 3: statistisches Modell (Random Forest) an AGFF-Messungen
# Trainingsdaten aus 48_satgrass_trainingsdaten.R. Pruefung per Leave-one-
# site-out: jeder Standort wird mit einem Modell geschaetzt, das ihn nicht
# gesehen hat. Vergleich mit (a) demselben Modell nur mit Wettermerkmalen,
# (b) ModVege am Messpunkt (automatische Schnitte), (c) Nullmodell
# Saisonmittel (mittlerer Zuwachs der anderen Standorte zur selben
# Jahreszeit). Qualitaet kann Methode 3 nicht schaetzen - es gibt keine
# Qualitaetsmessungen zum Trainieren.
td_datei <- file.path(aus_dir, "trainingsdaten.rds")
merkmale_sat <- c("ndvi_start", "ndvi_ende", "ndvi_diff", "ndvi_max30")
merkmale_wetter <- c("doy", "hoehe", "ta", "gdd", "niederschlag", "strahlung", "et0", "wasserbilanz30", "tage")
if (file.exists(td_datei)) {
  td <- as.data.table(readRDS(td_datei)$merkmale)
  alle_m <- c(merkmale_sat, merkmale_wetter)
  d <- td[complete.cases(td[, ..alle_m]) & ndvi_abstand <= 10 & tage >= 5 & tage <= 14 & !is.na(modvege_dbm)]
  cat("\n=== C. Methode 3 - Trainingsdaten:", nrow(d), "Messungen an", uniqueN(d$place), "Standorten ===\n")
  loso <- function(merkm) {
    v <- rep(NA_real_, nrow(d))
    for (st in unique(d$place)) {
      tr <- d$place != st
      fit <- ranger::ranger(growth ~ ., data = d[tr, c("growth", merkm), with = FALSE], num.trees = 500, seed = 1)
      v[!tr] <- predict(fit, d[!tr])$predictions
    }
    v
  }
  d[, m3 := loso(alle_m)]
  d[, m3_wetter := loso(merkmale_wetter)]
  d[, saisonmittel := vapply(seq_len(.N), function(k) {
    andere <- d$place != place[k] & abs(d$doy - doy[k]) <= 10
    mean(d$growth[andere])
  }, numeric(1))]
  guete <- function(x, name) data.table(variante = name, r = cor(x, d$growth), rmse = sqrt(mean((x - d$growth)^2)), bias = mean(x - d$growth))
  vergleich_m3 <- rbind(guete(d$m3, "Methode 3: Satellit + Wetter"), guete(d$m3_wetter, "Methode 3: nur Wetter"),
                        guete(d$modvege_dbm, "ModVege am Messpunkt (Autoschnitt)"), guete(d$saisonmittel, "Nullmodell Saisonmittel"))
  print(vergleich_m3[, .(variante, r = round(r, 3), rmse = round(rmse, 1), bias = round(bias, 1))])
  cat("\nJe Standort (r, Methode 3 vs. Saisonmittel):\n")
  print(d[, .(n = .N, r_m3 = round(cor(m3, growth), 2), r_saison = round(cor(saisonmittel, growth), 2),
              rmse_m3 = round(sqrt(mean((m3 - growth)^2)), 1), rmse_saison = round(sqrt(mean((saisonmittel - growth)^2)), 1)), by = place][order(-n)])
  fit_alle <- ranger::ranger(growth ~ ., data = d[, c("growth", alle_m), with = FALSE], num.trees = 500, seed = 1, importance = "permutation")
  cat("\nWichtigkeit der Merkmale (Permutation):\n")
  print(round(sort(fit_alle$variable.importance, decreasing = TRUE), 1))
  saveRDS(list(modell = fit_alle, merkmale = alle_m, cv = d, guete = vergleich_m3), file.path(aus_dir, "methode3_modell.rds"))
  p <- ggplot(melt(d[, .(growth, `Methode 3` = m3, ModVege = modvege_dbm, Saisonmittel = saisonmittel)], id.vars = "growth"),
              aes(growth, value)) + geom_point(alpha = 0.3) + geom_abline(linetype = "dashed") + facet_wrap(~variable) +
    labs(title = "Zuwachs an AGFF-Standorten: Schaetzung vs. Messung (Leave-one-site-out)",
         x = "gemessen (kg TS/ha/Tag)", y = "geschaetzt (kg TS/ha/Tag)") + theme_minimal()
  ggsave(file.path(aus_dir, "methode3_kreuzvalidierung.png"), p, width = 12, height = 5, dpi = 120)
}

## D. Methode 3 im Testgebiet und Vergleich mit Methode 1
# Gleiche Merkmale wie im Training: Satellit = Median-NDVI des Gruenlands im
# 300-m-Umkreis (hier: aller Gruenlandparzellen, deren Mittelpunkt im
# Umkreis liegt), Wetter Langnau i.E. hoehenkorrigiert. Wochenweise
# Zuwachs-Schaetzung April-Oktober; Ertrag je Schnitt = Summe des geschaetzten
# Zuwachses seit dem vorherigen Schnitt (erster Schnitt: seit 1. April).
m3_datei <- file.path(aus_dir, "methode3_modell.rds")
m1_datei <- file.path(aus_dir, paste0("methode1_", gebiet_name, ".rds"))
if (file.exists(m3_datei) && file.exists(m1_datei)) {
  m3 <- readRDS(m3_datei); m1 <- readRDS(m1_datei)
  ndvi_p <- readRDS(file.path(sat_dir, "ndvi_parzellen.rds"))
  parz <- readRDS(file.path(sat_dir, "parzellen.rds"))
  ausw <- parz[parz$n_pixel >= 4, ]
  mp <- suppressWarnings(st_point_on_surface(st_geometry(ausw)))
  nachbarn <- st_is_within_distance(mp, mp, dist = 300)
  paare <- data.table(parz_id = rep(ausw$parz_id, lengths(nachbarn)), nachbar = ausw$parz_id[unlist(nachbarn)])
  umgebung <- merge(paare, ndvi_p[, .(nachbar = parz_id, datum, jahr, ndvi)], by = "nachbar", allow.cartesian = TRUE)[
    , .(ndvi = median(ndvi), n = .N), by = .(parz_id, jahr, datum)][n >= 3]
  hoehe_p <- setNames(ifelse(is.na(ausw$hoehe), 756, ausw$hoehe), ausw$parz_id)
  vorhersage <- list()
  for (j_akt in sort(unique(umgebung$jahr))) {
    w <- w_all[w_all$year == j_akt, ]
    stichtage <- seq(as.Date(sprintf("%d-04-08", j_akt)), as.Date(sprintf("%d-10-31", j_akt)), by = 7)
    ids <- intersect(ausw$parz_id, unique(umgebung[jahr == j_akt]$parz_id))
    korr <- (station$hoehe - hoehe_p[as.character(ids)]) * 0.0065
    ra_mm <- berechne_ra_punkt(w$DOY, station$lat * pi / 180) * 0.408
    ta_m <- outer(korr, w$Ta, "+")
    et0_m <- 0.0023 * (ta_m + 17.8) * matrix(sqrt(pmax(w$Tmax - w$Tmin, 0)) * ra_mm, length(ids), nrow(w), byrow = TRUE)
    u_j <- umgebung[jahr == j_akt]; setkey(u_j, parz_id)
    zeilen <- lapply(stichtage, function(d) {
      iv <- which(w$datum > d - 7 & w$datum <= d); v30 <- which(w$datum > d - 30 & w$datum <= d)
      data.table(parz_id = ids, datum = d, tage = 7, doy = as.integer(format(d, "%j")), hoehe = hoehe_p[as.character(ids)],
                 ta = rowMeans(ta_m[, iv, drop = FALSE]), gdd = rowSums(pmax(ta_m[, iv, drop = FALSE] - 5, 0)),
                 niederschlag = sum(w$precip[iv]), strahlung = mean(w$SRad[iv]), et0 = rowSums(et0_m[, iv, drop = FALSE]),
                 wasserbilanz30 = sum(w$precip[v30]) - rowSums(et0_m[, v30, drop = FALSE]))
    })
    x <- rbindlist(zeilen)
    nd <- u_j[, {
      f <- function(t) if (.N >= 2) approx(as.numeric(datum), ndvi, xout = as.numeric(t), rule = 1)$y else rep(NA_real_, length(t))
      list(datum = stichtage, ndvi_start = f(stichtage - 7), ndvi_ende = f(stichtage),
           ndvi_max30 = vapply(stichtage, function(t) { k <- datum > t - 30 & datum <= t; if (any(k)) max(ndvi[k]) else NA_real_ }, numeric(1)))
    }, by = parz_id]
    x <- merge(x, nd, by = c("parz_id", "datum"))
    x[, ndvi_diff := ndvi_ende - ndvi_start]
    x <- x[complete.cases(x[, m3$merkmale, with = FALSE])]
    x[, zuwachs_m3 := predict(m3$modell, x)$predictions]
    x[, jahr := j_akt]
    vorhersage[[as.character(j_akt)]] <- x[, .(parz_id, jahr, datum, zuwachs_m3)]
    cat("Methode 3,", j_akt, ":", uniqueN(x$parz_id), "Parzellen,", nrow(x), "Wochenschaetzungen\n")
  }
  vh <- rbindlist(vorhersage)
  # Tageswerte (Wochenwert gilt fuer die 7 Tage bis zum Stichtag), Summen
  tag <- vh[, .(tag = datum - 6:0, zuwachs = zuwachs_m3), by = .(parz_id, jahr, datum)][, datum := NULL]
  m3_jahr <- tag[, .(zuwachs_m3_jahr = sum(zuwachs)), by = .(parz_id, jahr)]
  s1 <- copy(m1$schnitte)[order(parz_id, jahr, schnitt)]
  s1[, von := shift(schnitt, fill = NA), by = .(parz_id, jahr)]
  s1[is.na(von), von := as.Date(sprintf("%d-04-01", jahr))]
  s1[, ernte_m3 := mapply(function(p, j, a, b) sum(tag$zuwachs[tag$parz_id == p & tag$jahr == j & tag$tag > a & tag$tag <= b]),
                          parz_id, jahr, von, schnitt)]
  vergleich <- merge(m1$parzellen, m3_jahr, by = c("parz_id", "jahr"))
  vergleich[, norm_dt := as.numeric(NA)]
  if (file.exists(kontext_datei <- "../r-futterbaugutachten/outputs/region_emmental_oberaargau/_zwischen_kennwerte.rds")) {
    nw <- as.data.table(readRDS(kontext_datei)$parzellen)[, .(geoid, norm_wiese)][, .SD[1], by = geoid]
    vergleich <- merge(vergleich, nw, by = "geoid", all.x = TRUE)
  }
  saveRDS(list(schnitte = s1, parzellen = vergleich, woche = vh), file.path(aus_dir, paste0("vergleich_", gebiet_name, ".rds")))
  cat("\n=== D. Jahresertrag je Kultur (dt TS/ha, Median): Methode 1, Methode 3 (Jahreszuwachs), Normertrag ===\n")
  print(vergleich[n_schnitte > 0, .(parzellen = .N, m1 = round(median(ertrag_m1) / 100), m3 = round(median(zuwachs_m3_jahr) / 100),
                                    norm = round(median(norm_wiese, na.rm = TRUE)), r_m1_m3 = round(cor(ertrag_m1, zuwachs_m3_jahr), 2)),
                  by = .(jahr, lnf_code)][order(jahr, lnf_code)])
  cat("\nErtrag je Schnitt (kg TS/ha): Methode 1 vs. Methode 3\n")
  print(s1[, .(schnitte = .N, m1 = round(median(ernte_m1)), m3 = round(median(ernte_m3)), r = round(cor(ernte_m1, ernte_m3), 2)), by = jahr])
}

## E. Ebenen fuer den Datenexplorer (experimentell, ?experimentell)
# Je Woche der Stand bis zum Montag dieser Woche (wie die Meteo-Ebenen): Bild
# des Testgebiets (Parzellen ~20 m) plus grobes Wertegitter (~100 m) fuer
# die Wertanzeige am Cursor. 27_plot_datenexplorer.R liest nur die kleine
# Indexdatei schnittanalyse_index.json und baut daraus Radios/Legenden.
vergleich_datei <- file.path(aus_dir, paste0("vergleich_", gebiet_name, ".rds"))
if (file.exists(vergleich_datei)) {
  v <- readRDS(vergleich_datei)
  parz <- readRDS(file.path(sat_dir, "parzellen.rds"))
  ausw <- parz[parz$n_pixel >= 4, ]
  ebenen_dir <- "outputs/ebenen"
  montag_woche1 <- function(j) { jan4 <- as.Date(sprintf("%d-01-04", j)); jan4 - (as.integer(format(jan4, "%u")) - 1) }
  bb <- st_bbox(st_transform(ausw, 4326))
  vorlage_bild <- rast(ext(bb$xmin, bb$xmax, bb$ymin, bb$ymax), ncol = 400, nrow = round(400 * (bb$ymax - bb$ymin) / (bb$xmax - bb$xmin) / cos(mean(c(bb$ymin, bb$ymax)) * pi / 180)), crs = "EPSG:4326")
  vorlage_gitter <- rast(ext(vorlage_bild), ncol = 100, nrow = round(100 * nrow(vorlage_bild) / ncol(vorlage_bild)), crs = "EPSG:4326")
  pv <- vect(st_transform(ausw, 4326))
  id_bild <- as.matrix(rasterize(pv, vorlage_bild, field = "parz_id"), wide = TRUE)
  id_gitter <- as.matrix(rasterize(pv, vorlage_gitter, field = "parz_id"), wide = TRUE)
  e_b <- ext(vorlage_bild)
  bild_aus_werten <- function(werte, farben, bereich) {
    m <- matrix(werte[as.character(id_bild)], nrow(id_bild))
    anteil <- pmin(pmax((m - bereich[1]) / diff(bereich), 0), 1)
    pal <- scales::gradient_n_pal(farben)
    na_m <- is.na(anteil); anteil[na_m] <- 0
    rgbw <- t(grDevices::col2rgb(pal(as.vector(anteil)))) / 255
    img <- array(0, dim = c(nrow(m), ncol(m), 4))
    for (k in 1:3) img[, , k] <- matrix(rgbw[, k], nrow(m))
    img[, , 4] <- ifelse(na_m, 0, 0.85)
    tmp <- tempfile(fileext = ".png"); png::writePNG(img, tmp); b64 <- base64enc::base64encode(tmp); unlink(tmp)
    g <- matrix(werte[as.character(id_gitter)], nrow(id_gitter))
    list(bild = list(source = paste0("data:image/png;base64,", b64), xref = "x", yref = "y", x = e_b$xmin, y = e_b$ymax,
                     sizex = e_b$xmax - e_b$xmin, sizey = e_b$ymax - e_b$ymin, xanchor = "left", yanchor = "top",
                     sizing = "stretch", layer = "below"),
         werte = list(x0 = e_b$xmin, x1 = e_b$xmax, y0 = e_b$ymin, y1 = e_b$ymax, ncol = ncol(g), nrow = nrow(g),
                      m = lapply(seq_len(nrow(g)), function(i) signif(g[i, ], 3))))
  }
  ebenen_def <- list(
    schnittanalyse_schnitte = list(label = "Erkannte Schnitte", einheit = "Schnitte", bereich = c(0, 6),
      farben = c("#F7F7F7", "#C7E9C0", "#74C476", "#238B45", "#00441B"),
      quelle = "Sentinel-2 L2A (Earth Search), Schnitterkennung nach Sen4CAP-Logik - experimentell, bei extensiven Wiesen und Weiden unzuverlaessig."),
    schnittanalyse_ertrag_m1 = list(label = "Ertrag, Methode 1 (Sentinel + ModVege)", einheit = "dt TS/ha", bereich = c(0, 120),
      farben = c("#FFFFE5", "#D9F0A3", "#78C679", "#238443", "#004529"),
      quelle = "Erntemenge der erkannten Schnitte, ModVege (growR-Port, Posieux-Parameter NI 0.7, Wetter Langnau i.E.) - experimentell, extensive Flaechen ueberschaetzt."),
    schnittanalyse_ertrag_m3 = list(label = "Zuwachs, Methode 3 (statistisch)", einheit = "dt TS/ha", bereich = c(0, 120),
      farben = c("#FFFFE5", "#D9F0A3", "#78C679", "#238443", "#004529"),
      quelle = "Random Forest aus Sentinel-2 (Gruenland im 300-m-Umkreis) und Wetter, trainiert an AGFF-Messungen - experimentell, schaetzt den Zuwachs der Umgebung, nicht der einzelnen Parzelle."),
    schnittanalyse_omd = list(label = "Verdaulichkeit letzter Schnitt (OMD)", einheit = "", bereich = c(0.5, 0.85),
      farben = c("#D73027", "#FC8D59", "#FEE08B", "#91CF60", "#1A9850"),
      quelle = "Verdaulichkeit der organischen Substanz am letzten erkannten Schnitt, ModVege (Methode 1) - experimentell, nicht an Messungen geprueft.")
  )
  s1 <- v$schnitte; tag_m3 <- v$woche[, .(tag = datum - 6:0, zuwachs = zuwachs_m3), by = .(parz_id, jahr, datum)]
  index <- list(gebiet = list(name = "Testgebiet Emmental (Sumiswald/Affoltern i.E.)",
                              lon0 = unname(bb$xmin), lon1 = unname(bb$xmax), lat0 = unname(bb$ymin), lat1 = unname(bb$ymax)),
                ebenen = list())
  ergebnisse <- setNames(lapply(names(ebenen_def), function(n) list(bilder = list(), werte = list())), names(ebenen_def))
  for (j_akt in sort(unique(s1$jahr))) {
    for (w in 14:46) {
      stichtag <- montag_woche1(j_akt) + (w - 1) * 7
      if (stichtag > Sys.Date() + 1) next
      sch <- s1[jahr == j_akt & schnitt < stichtag]
      zahl <- setNames(rep(0, nrow(ausw)), ausw$parz_id)
      z <- sch[, .N, by = parz_id]; zahl[as.character(z$parz_id)] <- z$N
      e1 <- setNames(rep(0, nrow(ausw)), ausw$parz_id)
      z <- sch[, .(e = sum(ernte_m1) / 100), by = parz_id]; e1[as.character(z$parz_id)] <- z$e
      e3 <- setNames(rep(NA_real_, nrow(ausw)), ausw$parz_id)
      z <- tag_m3[jahr == j_akt & tag < stichtag, .(e = sum(zuwachs) / 100), by = parz_id]; e3[as.character(z$parz_id)] <- z$e
      om <- setNames(rep(NA_real_, nrow(ausw)), ausw$parz_id)
      z <- sch[order(schnitt)][, .(o = omd_m1[.N]), by = parz_id]; om[as.character(z$parz_id)] <- z$o
      werte_je <- list(schnittanalyse_schnitte = zahl, schnittanalyse_ertrag_m1 = e1, schnittanalyse_ertrag_m3 = e3, schnittanalyse_omd = om)
      for (n in names(ebenen_def)) {
        b <- bild_aus_werten(werte_je[[n]], ebenen_def[[n]]$farben, ebenen_def[[n]]$bereich)
        b$werte$bis <- format(stichtag - 1, "%d.%m.%Y")
        ergebnisse[[n]]$bilder[[paste(j_akt, w)]] <- b$bild
        ergebnisse[[n]]$werte[[paste(j_akt, w)]] <- b$werte
      }
    }
  }
  for (n in names(ebenen_def)) {
    jsonlite::write_json(ergebnisse[[n]], file.path(ebenen_dir, paste0(n, ".json")), auto_unbox = TRUE, na = "null", digits = NA)
    index$ebenen[[n]] <- c(ebenen_def[[n]], list(schluessel = names(ergebnisse[[n]]$bilder)))
  }
  jsonlite::write_json(index, file.path(ebenen_dir, "schnittanalyse_index.json"), auto_unbox = TRUE, digits = NA, pretty = TRUE)
  cat("\nE. Datenexplorer-Ebenen geschrieben:", paste(names(ebenen_def), collapse = ", "), "-",
      length(ergebnisse[[1]]$bilder), "Wochenbilder je Ebene\n")
}
