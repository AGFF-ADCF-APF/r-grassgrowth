## Klimanormale 1991-2020 fuer den Mehrjahresvergleich im Datenexplorer
##
## Laedt die Jahresdateien des MeteoSchweiz-Archivs (STAC-Item archive-ch der
## Collection ch.meteoschweiz.ogd-surface-derived-grid, Tagesraster 1 km LV95)
## Jahr fuer Jahr herunter, rechnet je Rasterzelle und Kalenderwoche dieselben
## Groessen wie die Ebenen in 27_plot_datenexplorer.R und loescht die
## Rohdatei gleich wieder (je Datei ~140 MB):
##   niederschlag_kum  Niederschlag seit 1. Januar bis zum Montag der Woche
##   niederschlag_28   Niederschlag der 28 Tage vor dem Montag der Woche
##   gdd               Wachstumsgradtage (Tagesmittel - 5 Grad, >= 0) seit 1. Januar bis zum Montag
##   boden             Bodenwasserbilanz (Eimer 130 mm ab 1. April) am Montag
##   wachstumspotenzial_kum  potenzielles Graswachstum (ModVege, Erholung 14 Tage) seit 1. Januar, kg TS/ha
## Bodenwasser und Wachstumspotenzial rechnen mit den Funktionen und Parametern
## aus 27_plot_datenexplorer.R (dieselbe Rechnung wie in der App) und brauchen
## zusaetzlich TmaxD, TminD und SrelD (ab 1971).
## Zwischenergebnis je Jahr: <ziel>/jahre/<groesse>_<jahr>.rds (Zelle x Woche).
## Am Schluss je Groesse <ziel>/normale_<groesse>.rds mit Mittel und den
## Perzentilen P10..P90 je Zelle und Woche (fuer «nasser als x von 10 Jahren»).
##
## Laeuft lokal und einmalig (Stunden, ~9 GB Download), bricht ab und setzt fort:
##   Rscript 28_klimanormale.R            alle Jahre der Normperiode
##   Rscript 28_klimanormale.R 2018 2019  nur diese Jahre (z.B. zum Testen)
## Nicht Teil des Nachtlaufs; der Server braucht nur die normale_*.rds.
suppressPackageStartupMessages(library(terra))
options(timeout = 3600) # Jahresdateien ~140 MB

normperiode <- 1991:2020
ziel_dir <- Sys.getenv("GRASSGROWTH_NORMALE_DIR", "../geodata/klimanormale")
dir.create(file.path(ziel_dir, "jahre"), recursive = TRUE, showWarnings = FALSE)
archiv_url <- "https://data.geo.admin.ch/ch.meteoschweiz.ogd-surface-derived-grid/archive-ch/ogd-surface-derived-grid-archive.%s_%s.swiss.lv95_%d0101000000_%d1231000000.nc"
groessen <- c("niederschlag_kum", "niederschlag_28", "gdd")
modell_groessen <- c("boden", "wachstumspotenzial_kum")
erholung_normal <- 14 # Vorgabe der App (JS: erholung)
wochen <- 1:52

# wie in 27_plot_datenexplorer.R
montag_woche1_jahr <- function(jahr) {
  d <- as.Date(paste0(jahr, "-01-04"))
  d - (as.integer(format(d, "%u")) - 1)
}
montag_von_woche <- function(jahr, w) montag_woche1_jahr(jahr) + (w - 1) * 7

jahr_datei <- function(groesse, jr) file.path(ziel_dir, "jahre", paste0(groesse, "_", jr, ".rds"))

lade_archiv <- function(produkt, jr) {
  dest <- file.path(ziel_dir, sprintf("%s_%d.nc", produkt, jr))
  if (!file.exists(dest)) {
    tmp <- paste0(dest, ".teil")
    ok <- tryCatch({ download.file(sprintf(archiv_url, produkt, if (produkt == "rhiresd") "ch01h" else "ch01r", jr, jr), tmp, mode = "wb", quiet = TRUE); TRUE },
                   error = function(e) { message("Download fehlgeschlagen: ", produkt, " ", jr, ": ", conditionMessage(e)); FALSE })
    if (!ok) { unlink(tmp); return(NULL) }
    file.rename(tmp, dest)
  }
  dest
}

# Tageswerte (Zelle x Tag) aufsummieren und je Woche den Stand am Montag
# (bzw. am letzten Tag davor im Jahr) herausgreifen
wochen_stand <- function(kum, tage, jr) {
  sapply(wochen, function(w) {
    i <- max(which(tage <= montag_von_woche(jr, w)), 0)
    if (i == 0) rep(0, nrow(kum)) else kum[, i]
  })
}
kumulieren <- function(m) {
  m[is.na(m)] <- 0
  for (i in seq_len(ncol(m))[-1]) m[, i] <- m[, i - 1] + m[, i]
  m
}

# Rastergeometrie je Produkt (fuer die spaetere Zuordnung Zelle -> Koordinate) -
# Niederschlag (ch01h) und Temperatur (ch01r) haben verschieden grosse Raster
vorlage_datei_fuer <- function(groesse) file.path(ziel_dir, paste0("raster_", if (startsWith(groesse, "niederschlag")) "niederschlag" else "temperatur", ".rds"))
merke_raster <- function(r, groesse) {
  vorlage_datei <- vorlage_datei_fuer(groesse)
  g <- list(ext = as.vector(ext(r)), nrow = nrow(r), ncol = ncol(r), crs = crs(r))
  if (!file.exists(vorlage_datei)) saveRDS(g, vorlage_datei)
  else if (!identical(g[c("ext", "nrow", "ncol")], readRDS(vorlage_datei)[c("ext", "nrow", "ncol")])) stop("Raster weicht von der Vorlage ab")
}
rechne_jahr <- function(jr) {
  fehlend <- groessen[!file.exists(jahr_datei(groessen, jr))]
  if (length(fehlend) == 0) return(invisible(TRUE))
  t0 <- Sys.time()
  # Niederschlag
  if (any(c("niederschlag_kum", "niederschlag_28") %in% fehlend)) {
    f <- lade_archiv("rhiresd", jr); if (is.null(f)) return(invisible(FALSE))
    r <- rast(f); tage <- as.Date(time(r))
    merke_raster(r, "niederschlag")
    m <- values(r); maske <- is.na(m[, 1]); rm(r)
    kum <- kumulieren(m)
    nk <- wochen_stand(kum, tage, jr); nk[maske, ] <- NA
    # 28 Tage vor dem Montag (wie die Fenster-Ebenen: Ende = Sonntag)
    n28 <- sapply(wochen, function(w) {
      ende <- montag_von_woche(jr, w) - 1
      i1 <- max(which(tage <= ende), 0); i0 <- max(which(tage <= ende - 28), 0)
      if (ende - 27 < tage[1]) return(rep(NA_real_, nrow(kum)))
      (if (i1 > 0) kum[, i1] else 0) - (if (i0 > 0) kum[, i0] else 0)
    })
    n28[maske, ] <- NA
    saveRDS(round(nk, 1), jahr_datei("niederschlag_kum", jr))
    saveRDS(round(n28, 1), jahr_datei("niederschlag_28", jr))
    rm(m, kum, nk, n28); gc(verbose = FALSE)
  }
  # Temperatursumme
  if ("gdd" %in% fehlend) {
    f <- lade_archiv("tabsd", jr); if (is.null(f)) return(invisible(FALSE))
    r <- rast(f); tage <- as.Date(time(r))
    merke_raster(r, "gdd")
    m <- values(r); maske <- is.na(m[, 1]); rm(r)
    m <- pmax(m - 5, 0)
    g <- wochen_stand(kumulieren(m), tage, jr); g[maske, ] <- NA
    saveRDS(round(g, 1), jahr_datei("gdd", jr))
    rm(m, g); gc(verbose = FALSE)
  }
  cat(jr, "fertig in", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "min\n")
  invisible(TRUE)
}

# Funktionen und Parameter des Eimer-Modells und von ModVege aus
# 27_plot_datenexplorer.R uebernehmen (nur die Zuweisungen dieser Namen,
# auch modvege_P$... <- ...), damit beide Skripte gleich rechnen
lade_modell_code <- function(datei = "27_plot_datenexplorer.R") {
  namen <- c("Smax_boden", "berechne_ra", "angstrom_a_s", "angstrom_b_s", "modvege_fT", "modvege_fW", "modvege_fPAR",
             "modvege_SEA", "modvege_atmospheric_CO2", "modvege_fCO2_growth", "modvege_fCO2_transpiration", "modvege_P",
             "modvege_schnitt_hoehen", "modvege_schnitt_anzahl", "modvege_init", "simuliere_wachstumspotenzial")
  for (e in parse(datei, keep.source = FALSE)) {
    if (!is.call(e) || !identical(e[[1]], as.name("<-"))) next
    ziel <- e[[2]]
    while (is.call(ziel)) ziel <- ziel[[2]]
    if (as.character(ziel) %in% namen) eval(e, globalenv())
  }
  fehlt <- namen[!vapply(namen, exists, logical(1), envir = globalenv())]
  if (length(fehlt)) stop("Aus ", datei, " nicht gefunden: ", paste(fehlt, collapse = ", "))
}

# Bodenwasser und Wachstumspotenzial eines Jahres (Temperaturraster; der
# Niederschlag wird wie in 27 auf dessen Ausdehnung zugeschnitten)
rechne_modell_jahr <- function(jr) {
  fehlend <- modell_groessen[!file.exists(jahr_datei(modell_groessen, jr))]
  if (length(fehlend) == 0) return(invisible(TRUE))
  if (!exists("simuliere_wachstumspotenzial")) lade_modell_code()
  t0 <- Sys.time()
  dateien <- lapply(c(precip = "rhiresd", tabs = "tabsd", tmax = "tmaxd", tmin = "tmind", srel = "sreld"), lade_archiv, jr = jr)
  if (any(vapply(dateien, is.null, logical(1)))) return(invisible(FALSE))
  r <- lapply(dateien, rast)
  r$precip <- crop(r$precip, ext(r$tmax))
  tage <- sort(Reduce(intersect, lapply(r, function(x) as.Date(time(x)))))
  tage <- as.Date(tage, origin = "1970-01-01")
  merke_raster(r$tmax, "boden")
  wahl <- function(x) values(x[[match(tage, as.Date(time(x)))]])
  xy <- xyFromCell(r$tmax, 1:ncell(r$tmax))
  lat_vec <- project(xy, from = crs(r$tmax), to = "EPSG:4326")[, 2] * pi / 180
  ra <- vapply(as.integer(format(tage, "%j")), function(J) berechne_ra(J, lat_vec), numeric(length(lat_vec))) # MJ/m2/Tag
  precip <- wahl(r$precip); tmax <- wahl(r$tmax); tmin <- wahl(r$tmin)
  dT_wurzel <- sqrt(pmax(tmax - tmin, 0))
  maske <- is.na(tmax[, 1])
  # Bodenwasserbilanz: Hargreaves-ET0 mit (Tmax+Tmin)/2, Eimer ab 1. April voll
  if ("boden" %in% fehlend) {
    et0 <- 0.0023 * ((tmax + tmin) / 2 + 17.8) * dT_wurzel * ra * 0.408
    ab <- which(tage >= as.Date(paste0(jr, "-04-01")))
    s <- rep(Smax_boden, nrow(precip)); speicher <- matrix(NA_real_, nrow(precip), length(tage))
    for (i in ab) { s <- pmin(pmax(s + precip[, i] - et0[, i], 0), Smax_boden); speicher[, i] <- s }
    b <- sapply(wochen, function(w) {
      i <- max(which(tage <= montag_von_woche(jr, w) & tage >= as.Date(paste0(jr, "-04-01"))), 0)
      if (i == 0) rep(NA_real_, nrow(speicher)) else speicher[, i]
    })
    b[maske, ] <- NA
    saveRDS(round(b, 1), jahr_datei("boden", jr))
    rm(et0, speicher, b)
  }
  rm(tmax, tmin); gc(verbose = FALSE)
  # Potenzielles Wachstum: ET0 mit TabsD, Strahlung per Angstroem-Prescott aus SrelD
  if ("wachstumspotenzial_kum" %in% fehlend) {
    Ta <- wahl(r$tabs)
    ET0 <- 0.0023 * (Ta + 17.8) * dT_wurzel * ra * 0.408
    PAR <- (angstrom_a_s + angstrom_b_s * (wahl(r$srel) / 100)) * ra * 11.574 * 0.0406
    rm(ra, dT_wurzel); gc(verbose = FALSE)
    erg <- simuliere_wachstumspotenzial(Ta = Ta, precip = precip, PAR = PAR, ET0 = ET0, jahr = jr, erholung_tage = erholung_normal)
    k <- wochen_stand(erg$cBM, tage, jr); k[maske, ] <- NA
    saveRDS(round(k), jahr_datei("wachstumspotenzial_kum", jr))
    rm(erg, Ta, ET0, PAR, k)
  }
  rm(precip, r); gc(verbose = FALSE)
  cat(jr, "Modell fertig in", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "min\n")
  invisible(TRUE)
}
aufraeumen <- function(jr) unlink(Sys.glob(file.path(ziel_dir, paste0("*_", jr, ".nc"))))

# Perzentile je Zeile (Typ 7 wie quantile()), vektorisiert ueber alle Zellen
zeilen_quantile <- function(m, probs) {
  n <- ncol(m)
  s <- matrix(m[order(row(m), m)], ncol = n, byrow = TRUE)
  sapply(probs, function(p) {
    h <- (n - 1) * p + 1; lo <- floor(h); hi <- ceiling(h)
    s[, lo] + (h - lo) * (s[, hi] - s[, lo])
  })
}

fasse_zusammen <- function(groesse) {
  jahre <- normperiode[file.exists(jahr_datei(groesse, normperiode))]
  if (length(jahre) < length(normperiode)) {
    cat(groesse, ": erst", length(jahre), "von", length(normperiode), "Jahren - keine Normale\n")
    return(invisible(NULL))
  }
  daten <- lapply(jahre, function(jr) readRDS(jahr_datei(groesse, jr)))
  probs <- seq(0.1, 0.9, by = 0.1)
  n_zellen <- nrow(daten[[1]])
  p <- array(NA_real_, dim = c(n_zellen, length(wochen), length(probs)), dimnames = list(NULL, wochen, paste0("p", probs * 100)))
  mittel <- matrix(NA_real_, n_zellen, length(wochen))
  for (w in wochen) {
    m <- sapply(daten, function(d) d[, w])
    ok <- rowSums(is.na(m)) == 0
    if (any(ok)) {
      p[ok, w, ] <- round(zeilen_quantile(m[ok, , drop = FALSE], probs), 1)
      mittel[ok, w] <- round(rowMeans(m[ok, , drop = FALSE]), 1)
    }
  }
  saveRDS(list(groesse = groesse, periode = range(jahre), wochen = wochen, stichtag = "Montag der Kalenderwoche",
               raster = readRDS(vorlage_datei_fuer(groesse)),
               mittel = mittel, perzentile = p),
          file.path(ziel_dir, paste0("normale_", groesse, ".rds")))
  cat(groesse, ": Normale", paste(range(jahre), collapse = "-"), "gespeichert\n")
}

# Fuer den Bericht im Browser: Perzentile je Woche grob (wie die Ebenen nach
# WGS84, aber ~5 km) als <ziel>/normale.json - 27_plot_datenexplorer.R kopiert sie
# nach outputs/ebenen/; der Bericht liest daraus die Werte an den eigenen Orten
# (Band «8 von 10 Jahren», Rang des Jahres)
schreibe_bericht_json <- function(ziel_ncol = 70) {
  aus <- list()
  for (g in c(groessen, modell_groessen)) {
    f <- file.path(ziel_dir, paste0("normale_", g, ".rds"))
    if (!file.exists(f)) next
    n <- readRDS(f)
    v <- n$raster
    vorlage <- rast(ext(v$ext), nrows = v$nrow, ncols = v$ncol, crs = v$crs)
    # fuer den Browser genuegen P10, P30, P50, P70, P90 (Datei ~8 MB, gezippt ~1.5 MB)
    stats <- c("p10", "p30", "p50", "p70", "p90")
    r <- rast(vorlage, nlyrs = length(n$wochen) * length(stats))
    values(r) <- matrix(n$perzentile[, , stats], nrow = nrow(n$perzentile))   # Schichten: Woche schnell, Perzentil langsam
    r_wgs <- project(r, "EPSG:4326", method = "bilinear")
    faktor <- max(1, round(ncol(r_wgs) / ziel_ncol))
    if (faktor > 1) r_wgs <- aggregate(r_wgs, fact = faktor, fun = "mean", na.rm = TRUE)
    e <- ext(r_wgs)
    eintrag <- list(periode = paste(n$periode, collapse = "–"), x0 = e$xmin, x1 = e$xmax, y0 = e$ymin, y1 = e$ymax,
                    ncol = ncol(r_wgs), nrow = nrow(r_wgs), stichtag = n$stichtag, werte = list())
    for (si in seq_along(stats)) {
      eintrag$werte[[stats[si]]] <- lapply(seq_along(n$wochen), function(wi) {
        m <- as.matrix(r_wgs[[(si - 1) * length(n$wochen) + wi]], wide = TRUE)
        lapply(seq_len(nrow(m)), function(i) round(m[i, ]))
      })
    }
    aus[[g]] <- eintrag
  }
  if (!length(aus)) return(invisible(NULL))
  ziel <- file.path(ziel_dir, "normale.json")
  jsonlite::write_json(aus, ziel, auto_unbox = TRUE, na = "null", digits = NA)
  cat("Bericht-Normale geschrieben:", ziel, round(file.size(ziel) / 1e6, 1), "MB\n")
}

args <- commandArgs(trailingOnly = TRUE)
jahre <- if (length(args)) as.integer(args) else normperiode
for (jr in jahre) { rechne_jahr(jr); rechne_modell_jahr(jr); aufraeumen(jr) }
for (g in c(groessen, modell_groessen)) fasse_zusammen(g)
schreibe_bericht_json()
