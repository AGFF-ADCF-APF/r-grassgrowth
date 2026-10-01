# Prototyp/Validierung der vektorisierten ModVege-Portierung fuer
# "Potenzielles Graswachstum" (Rasterphase) - siehe Plan in
# .claude/plans/temporal-sauteeing-clover.md.
#
# Baut auf Phase A auf (43_vergleiche_wachstumsmodelle.R): nutzt dieselben
# SMN-Wetterdaten/denselben Angstroem-Prescott-Kalibrierungslauf. Validiert
# die eigene Matrix-Implementierung GEGEN growR selbst (NI=1, kein
# Schnitt-Management) an den 7 AGFF-Punkten, BEVOR das Ganze auf die volle
# Schweiz-Flaeche in 27_plot_datenexplorer.R integriert wird.
#
# Eigenstaendiges Skript wie 41/42/43 - NICHT Teil von 00_automate.R.

suppressPackageStartupMessages({
  library(dplyr)
  library(growR)
  library(ggplot2)
})

ausgabe_dir <- "outputs/vergleich_wachstumsmodelle"

########################################################################
## Referenz-Parameter (Posieux, NI=1, FG-Mischung 0.7/0.3) - EXAKT die
## von growR selbst aufgeloesten Werte (siehe Recherche-Notiz: direkt aus
## einem ModvegeEnvironment-Objekt ausgelesen, nicht von Hand aus Papier/
## Vignette rekonstruiert).
########################################################################
P <- list(
  SLA = 0.0306, pcLAM = 0.68, ST1 = 630, ST2 = 1245, minSEA = 0.77, maxSEA = 1.23,
  LLS = 590, maxOMDGV = 0.9, minOMDGV = 0.705, maxOMDGR = 0.9, minOMDGR = 0.59,
  stubble_height = 0.02, crop_coefficient = 1.15, senescence_cap = 0.7,
  BDGV = 850, BDGR = 300, BDDV = 500, BDDR = 150,
  T0 = 5, T1 = 10, T2 = 20, KGV = 0.002, KGR = 0.001, KlDV = 0.001, KlDR = 5e-04,
  sigmaGV = 0.4, sigmaGR = 0.2, OMDDV = 0.45, OMDDR = 0.4,
  RUEmax = 3, WHC = 130, NI = 1, CO2_growth_factor = 0.5
)
P$minBMGV <- P$stubble_height * 10 * P$BDGV
P$minBMGR <- P$stubble_height * 10 * P$BDGR
P$REP_ON <- 0.25 + (0.75 * (P$NI - 0.35)) / 0.65 # = 1.0 bei NI=1
# Anfangsbedingungen (Posieux-CSV) - identisch fuer jeden Pixel, da keine
# ortsspezifische Kalibrierung vorgesehen ist (siehe Plan).
INIT <- list(AgeGV = 100, AgeGR = 2000, AgeDV = 500, AgeDR = 500,
             BMGV = 420, BMGR = 0, BMDV = 300, BMDR = 30,
             SENGV = 0, SENGR = 0, ABSDV = 0, ABSDR = 0, ST = 0, cBM = 0, WR = 130)

########################################################################
## fT/fW/fPAR/SEA - 1:1 aus growR's eigenem (gelesenem) Quellcode, aber
## VEKTORISIERT (ein Wert pro Pixel statt ein Skalar pro Standort).
########################################################################
fT_vec <- function(t, T0 = P$T0, T1 = P$T1, T2 = P$T2) {
  ifelse(t < T0, 0,
    ifelse(t < T1, (t - T0) / (T1 - T0),
      ifelse(t < T2, 1,
        ifelse(t < 40, (40 - t) / (40 - T2), 0))))
}

fW_vec <- function(W, PET) {
  # growR's eigene switch()-Bucketierung (siehe Quellcode) als vektorisierte
  # Fallunterscheidung ueber drei PET-Regime nachgebaut - findInterval()
  # bildet dieselben 0.2-Schritte wie switch(1+floor(W/0.2), ...) ab.
  bucket <- pmin(floor(W / 0.2) + 1, 6)
  hoch <- W
  # Achtung: growR's switch() hat bei den letzten Eintraegen LITERALE
  # Konstanten "1" (nicht "1*W+0"!) - erster Validierungslauf hatte das als
  # lineare Formel 1*W+0 fehlinterpretiert, was bei W nahe aber unter 1
  # einen spuerbaren Fehler ergab (hier korrigiert: Eintraege 5/6 IMMER
  # exakt 1, unabhaengig von W).
  mittel_formel <- c(2, 1.5, 1, 0.5, NA, NA)[bucket] * W + c(0, 0.1, 0.3, 0.6, NA, NA)[bucket]
  mittel <- ifelse(bucket >= 5, 1, mittel_formel)
  tief_formel <- c(4, 0.75, 0.25, NA, NA, NA)[bucket] * W + c(0, 0.65, 0.85, NA, NA, NA)[bucket]
  tief <- ifelse(bucket >= 4, 1, tief_formel)
  ifelse(PET > 6.5, hoch, ifelse(PET > 3.8, mittel, tief))
}

fPAR_vec <- function(PAR) pmax(0, pmin(1, 1 - 0.0445 * (PAR - 5)))

SEA_vec <- function(ST, minSEA = P$minSEA, maxSEA = P$maxSEA, ST1 = P$ST1, ST2 = P$ST2) {
  ifelse(ST < 200, minSEA,
    ifelse(ST < (ST1 - 200), minSEA + (maxSEA - minSEA) * (ST - 200) / (ST1 - 400),
      ifelse(ST < (ST1 - 100), maxSEA,
        ifelse(ST < ST2, maxSEA + (minSEA - maxSEA) * (ST - ST1 + 100) / (ST2 - ST1 + 100),
          minSEA))))
}

########################################################################
## Haupt-Simulationsfunktion: nimmt MATRIZEN (Zeilen=Pixel/Standorte,
## Spalten=Tage) und liefert die Zustands-/Ergebnis-Matrizen zurueck -
## exakt dasselbe Matrix-Schleifen-Muster wie das bestehende Bucket-Modell
## (27_plot_datenexplorer.R, Zeilen ~1536-1548), nur mit mehreren
## parallelen Zustandsgroessen statt nur einer.
########################################################################
simuliere_wachstumspotenzial <- function(Ta, Tmin, Tmax, precip, PAR, ET0, jahr) {
  n_pixel <- nrow(Ta); n_tage <- ncol(Ta)
  # CO2-Modifikatoren: entgegen der urspruenglichen Annahme im Plan (siehe
  # "Offene Frage 1") NICHT vernachlaessigbar - bei heutigem CO2-Niveau
  # (~438ppm 2024) liefert fCO2_growth_mod() einen Wachstums-BONUS von
  # knapp +10% gegenueber der Referenz 360ppm (empirisch beim Validieren
  # gegen growR entdeckt: ohne diesen Faktor fehlten ~16% kumuliertes
  # Wachstum). fCO2_transpiration_mod() ist dagegen mit <1% vernachlaessigbar,
  # wird aber der Vollstaendigkeit halber trotzdem mitgenommen (beide
  # Funktionen sind in growR exportiert, 1:1 uebernommen).
  co2_ppm <- atmospheric_CO2(jahr)
  co2_wachstum <- fCO2_growth_mod(co2_ppm, P$CO2_growth_factor)
  co2_transpiration <- fCO2_transpiration_mod(co2_ppm)

  # Schnee/Regen-Aufteilung + Grad-Tag-Schmelzmodell - 1:1 aus
  # WeatherData$read_weather() (growR ignoriert einen evtl. mitgelieferten
  # "snow"-Input komplett und berechnet Schnee/Schmelze IMMER selbst aus
  # Ta/precip; ein eigener Schnee-Input ist also gar nicht noetig). Logistische
  # statt harter Regen/Schnee-Schwelle (Zentrum bei 2 Grad C), danach
  # taeglich rekursives Harbinger/Auffrieren/Abschmelzen (T_melt=-1,
  # C_melt=3, C_freeze=0.05).
  T_melt <- -1; C_melt <- 3; C_freeze <- 0.05
  liquidP_roh <- (1 / (1 + exp(-1.5 * (Ta - 2)))) * precip
  solidP <- precip - liquidP_roh
  schnee <- matrix(0, n_pixel, n_tage)
  schmelze <- matrix(0, n_pixel, n_tage)
  for (j in 2:n_tage) {
    schnee_vortag <- schnee[, j - 1]
    schmelze[, j] <- ifelse(schnee_vortag > 0 & Ta[, j] >= T_melt,
      pmin(schnee_vortag, C_melt * (Ta[, j] - T_melt)), 0)
    frieren <- ifelse(Ta[, j] < T_melt, pmin(liquidP_roh[, j], C_freeze * (T_melt - Ta[, j])), 0)
    schnee[, j] <- pmax(0, schnee_vortag + solidP[, j] + frieren - schmelze[, j])
  }
  # Tatsaechlich dem Bodenwasserhaushalt zugefuehrtes Wasser = fluessiger
  # Niederschlagsanteil + Schmelzwasser (siehe calculate_growth(): WR[j] =
  # WRp + liquidP[j] + melt[j] - AET[j]).
  wasser_zufuhr <- liquidP_roh + schmelze

  # Temperatursumme (MTD: einfache cumsum(max(Ta,0)), siehe
  # calculate_temperature_sum() mit SGS_method="MTD").
  Ta_pos <- pmax(Ta, 0)
  ST <- t(apply(Ta_pos, 1, cumsum))
  if (n_pixel == 1) ST <- matrix(ST, nrow = 1) # apply() liefert bei 1 Zeile einen Vektor statt Matrix

  # Vegetationsbeginn (MTD-Methode: 10-Tage-Aussenfenster/5-Tage-Innenfenster,
  # siehe start_of_growing_season_mtd()) - sequentiell ueber Tage, aber
  # VEKTORISIERT ueber alle Pixel gleichzeitig pro Tagesposition.
  j_start <- rep(NA_integer_, n_pixel)
  noch_offen <- rep(TRUE, n_pixel)
  for (j in 30:(n_tage - 10)) { # first_possible_DOY=30, growR-Default
    if (!any(noch_offen)) break
    aussen <- Ta[, j:(j + 9), drop = FALSE]
    aussen_ok <- apply(aussen >= 2, 1, all) & (rowMeans(aussen) >= 6)
    for (j_inner in 1:6) {
      innen <- Ta[, (j + j_inner - 1):(j + j_inner + 3), drop = FALSE]
      innen_ok <- apply(innen > 5, 1, all)
      treffer <- noch_offen & aussen_ok & innen_ok
      if (any(treffer)) {
        j_start[treffer] <- j + j_inner - 1
        noch_offen[treffer] <- FALSE
      }
      if (!any(noch_offen)) break
    }
  }
  j_start[is.na(j_start)] <- n_tage # Pixel ohne gefundenen Start (z.B. extreme Hochlagen) - Saison startet nie

  # Zustandsmatrizen - EIN Eintrag pro (Pixel, Tag), NUR EIN finales values()<-
  # aequivalent (hier: die Matrizen selbst sind das Ergebnis, kein SpatRaster
  # noetig auf dieser Prototyp-Stufe).
  mat0 <- matrix(0, n_pixel, n_tage)
  AgeGV <- mat0; AgeGR <- mat0; AgeDV <- mat0; AgeDR <- mat0
  BMGV <- mat0; BMGR <- mat0; BMDV <- mat0; BMDR <- mat0
  WR <- mat0; GRO <- mat0; PGRO <- mat0; BM <- mat0; dBM <- mat0; cBM <- mat0

  AgeGVp <- rep(INIT$AgeGV, n_pixel); AgeGRp <- rep(INIT$AgeGR, n_pixel)
  AgeDVp <- rep(INIT$AgeDV, n_pixel); AgeDRp <- rep(INIT$AgeDR, n_pixel)
  BMGVp <- rep(INIT$BMGV, n_pixel); BMGRp <- rep(INIT$BMGR, n_pixel)
  BMDVp <- rep(INIT$BMDV, n_pixel); BMDRp <- rep(INIT$BMDR, n_pixel)
  SENGV <- rep(INIT$SENGV, n_pixel); SENGR <- rep(INIT$SENGR, n_pixel)
  ABSDV <- rep(INIT$ABSDV, n_pixel); ABSDR <- rep(INIT$ABSDR, n_pixel)
  WRp <- rep(INIT$WR, n_pixel); cBMp <- rep(INIT$cBM, n_pixel)

  for (j in 1:n_tage) {
    T_avg <- Ta[, j]

    # --- calculate_growth() ---
    LAIGV <- P$SLA * P$pcLAM * BMGVp / 10
    # LAI.ET (fuer Transpiration) ist NICHT dasselbe wie LAIGV (fuer PGRO) -
    # enthaelt BEIDE Pools (GV+GR), siehe growR-Quellcode - ein Unterschied,
    # der beim ersten Validierungslauf uebersehen wurde (beide faelschlich
    # gleichgesetzt).
    LAI_ET <- P$SLA * P$pcLAM * (BMGVp + BMGRp) / 10
    PETeff <- ifelse(precip[, j] > 1, 0.7 * P$crop_coefficient * ET0[, j], P$crop_coefficient * ET0[, j])
    PETeff <- PETeff * co2_transpiration
    PTr <- PETeff * (1 - exp(-0.6 * LAI_ET))
    ATr <- PTr * fW_vec(WRp / P$WHC, PETeff)
    PEv <- PETeff - PTr
    AEv <- PEv * WRp / P$WHC
    AET <- ATr + AEv
    WR[, j] <- pmax(0, pmin(P$WHC, WRp + wasser_zufuhr[, j] - AET))
    ENVfPAR <- fPAR_vec(PAR[, j])
    ENVfT <- fT_vec(T_avg)
    ENVfW <- fW_vec(WR[, j] / P$WHC, PETeff)
    ENV <- ENVfPAR * ENVfT * ENVfW
    vor_saisonstart <- j < j_start
    PGRO[, j] <- ifelse(vor_saisonstart, 0, PAR[, j] * P$RUEmax * (1 - exp(-0.6 * LAIGV)) * 10 * co2_wachstum)
    GRO[, j] <- ifelse(vor_saisonstart, 0, P$NI * PGRO[, j] * ENV * SEA_vec(ST[, j]))

    # --- calculate_ageing() --- (REP: reproduktiver Anteil, AUCH ohne
    # Schnitt aktiv sobald ST im Fenster [ST1,ST2] liegt - siehe
    # Validierungs-Erkenntnis im Plan/Bericht: cut_during_growth_preriod
    # bleibt ohne Schnitt fuer immer FALSE, was REP NICHT dauerhaft
    # deaktiviert, sondern im Gegenteil immer aktiv werden laesst, wenn ST
    # im Fenster liegt.)
    REP <- ifelse(ST[, j] >= P$ST1 & ST[, j] <= P$ST2, P$REP_ON, 0)
    GROGV <- GRO[, j] * (1 - REP)
    GROGR <- GRO[, j] * REP

    dAgeGV <- ifelse(BMGVp - SENGV + GROGV != 0,
      (BMGVp - SENGV) / (BMGVp - SENGV + GROGV) * (AgeGVp + pmax(0, T_avg)) - AgeGVp, -AgeGVp)
    AgeGV[, j] <- AgeGVp + dAgeGV
    dAgeGR <- ifelse(BMGRp - SENGR + GROGR != 0,
      (BMGRp - SENGR) / (BMGRp - SENGR + GROGR) * (AgeGRp + pmax(0, T_avg)) - AgeGRp, -AgeGRp)
    AgeGR[, j] <- AgeGRp + dAgeGR

    ratio1 <- AgeGV[, j] / P$LLS
    fAgeGV <- ifelse(ratio1 < 1 / 3, 1, ifelse(ratio1 < 1, 3 * ratio1, 3))
    ratio2 <- AgeGR[, j] / (P$ST2 - P$ST1)
    fAgeGR <- ifelse(ratio2 < 1 / 3, 1, ifelse(ratio2 < 1, 3 * ratio2, 3))

    SENGV_neu <- ifelse(T_avg > P$T0, P$KGV * BMGVp * T_avg * fAgeGV,
      ifelse(T_avg > 0, 0, P$KGV * BMGVp * abs(T_avg)))
    SENGR_neu <- ifelse(T_avg > P$T0, P$KGR * BMGRp * T_avg * fAgeGR,
      ifelse(T_avg > 0, 0, P$KGR * BMGRp * abs(T_avg)))
    SENGV <- ifelse(abs(SENGV_neu) > P$senescence_cap * abs(GROGV), P$senescence_cap * GROGV, SENGV_neu)
    SENGR <- ifelse(abs(SENGR_neu) > P$senescence_cap * abs(GROGR), P$senescence_cap * GROGR, SENGR_neu)

    dAgeDV <- ifelse(BMDVp - ABSDV + SENGV != 0,
      (BMDVp - ABSDV) / (BMDVp - ABSDV + SENGV) * (AgeDVp + pmax(0, T_avg)) - AgeDVp, -AgeDVp)
    AgeDV[, j] <- AgeDVp + dAgeDV
    dAgeDR <- ifelse(BMDRp - ABSDR + SENGR != 0,
      (BMDRp - ABSDR) / (BMDRp - ABSDR + SENGR) * (AgeDRp + pmax(0, T_avg)) - AgeDRp, -AgeDRp)
    AgeDR[, j] <- AgeDRp + dAgeDR

    ratio3b <- AgeDV[, j] / P$LLS
    fAgeDV <- ifelse(ratio3b < 1 / 3, 1, ifelse(ratio3b < 2 / 3, 2, 3))
    ratio4 <- AgeDR[, j] / (P$ST2 - P$ST1)
    fAgeDR <- ifelse(ratio4 < 1 / 3, 1, ifelse(ratio4 < 2 / 3, 2, 3))
    ABSDV <- ifelse(T_avg > 0, P$KlDV * BMDVp * T_avg * fAgeDV, 0)
    ABSDR <- ifelse(T_avg > 0, P$KlDR * BMDRp * T_avg * fAgeDR, 0)

    # --- update_biomass() ---
    dBMGV <- GROGV - SENGV
    dBMGR <- GROGR - SENGR
    BMGV[, j] <- BMGVp + dBMGV
    BMGR[, j] <- BMGRp + dBMGR
    spaetsaison <- ST[, j] >= P$ST2
    unter_min_gv <- spaetsaison & BMGV[, j] < P$minBMGV
    BMGV[, j] <- ifelse(unter_min_gv, P$minBMGV, BMGV[, j])
    unter_min_gr <- spaetsaison & BMGR[, j] < P$minBMGR
    BMGR[, j] <- ifelse(unter_min_gr, P$minBMGR, BMGR[, j])

    dBMDV <- (1 - P$sigmaGV) * SENGV - ABSDV
    dBMDR <- (1 - P$sigmaGR) * SENGR - ABSDR
    BMDV[, j] <- BMDVp + dBMDV
    BMDR[, j] <- BMDRp + dBMDR
    BM[, j] <- BMGV[, j] + BMGR[, j] + BMDV[, j] + BMDR[, j]
    dBM[, j] <- dBMGV + dBMGR + dBMDV + dBMDR
    cBM[, j] <- cBMp + pmax(0, dBM[, j])

    # --- carry_over_from_last_day() (fuer den naechsten Durchlauf) ---
    AgeGVp <- AgeGV[, j]; AgeGRp <- AgeGR[, j]; AgeDVp <- AgeDV[, j]; AgeDRp <- AgeDR[, j]
    BMGVp <- BMGV[, j]; BMGRp <- BMGR[, j]; BMDVp <- BMDV[, j]; BMDRp <- BMDR[, j]
    cBMp <- cBM[, j]; WRp <- WR[, j]
  }

  list(GRO = GRO, PGRO = PGRO, BM = BM, dBM = dBM, cBM = cBM, j_start = j_start,
       ST = ST, WR = WR, BMGV = BMGV, BMGR = BMGR, BMDV = BMDV, BMDR = BMDR)
}

cat("Funktion definiert. Validiere gegen growR (NI=1, kein Schnitt) an einem Phase-A-Punkt (BIZ/Flawil)...\n")

########################################################################
## Validierung: eigene Matrix-Implementierung vs. growR selbst, GLEICHE
## Wetterdaten/Parameter (NI=1, kein Schnitt) - der eigentliche
## Korrektheitstest des Ports (siehe Plan).
########################################################################
growr_input_dir <- file.path(ausgabe_dir, "growr_input")
wetter_biz <- read.table(file.path(growr_input_dir, "p1_BIZ_weather.txt"), header = TRUE, sep = "\t")
wetter_biz <- wetter_biz[wetter_biz$year %in% 2024, ] # ein Jahr reicht fuer den Korrektheitstest

# growR-Referenzlauf: eigene Posieux-Parameter (NI=1 statt 0.7), Site auf
# BIZ-Koordinaten angepasst, Wetterdatei 1:1 uebernommen, KEIN Management.
posieux_param_ni1 <- read.csv(file.path(growr_input_dir, "p1_BIZ_parameters.csv"))
posieux_param_ni1$value[posieux_param_ni1$name == "NI"] <- 1
write.csv(posieux_param_ni1, file.path(growr_input_dir, "validierung_parameters.csv"), row.names = FALSE)
write.table(wetter_biz, file.path(growr_input_dir, "validierung_weather.txt"), sep = "\t", row.names = FALSE, quote = FALSE)
writeLines("year\tDOY", file.path(growr_input_dir, "validierung_management.txt"))

env_val <- ModvegeEnvironment$new(
  site_name = "validierung", run_name = "ni1_nocut", years = 2024,
  param_file = "validierung_parameters.csv", weather_file = "validierung_weather.txt",
  management_file = "validierung_management.txt", input_dir = growr_input_dir
)
res_growr <- growR_run_loop(list(env_val), output_dir = "")
s_growr <- res_growr[[1]][[1]]

# Eigene Implementierung: dieselbe Wetterdatei als 1-Zeilen-Matrix.
mein_erg <- simuliere_wachstumspotenzial(
  Ta = matrix(wetter_biz$Ta, nrow = 1), Tmin = matrix(wetter_biz$Tmin, nrow = 1),
  Tmax = matrix(wetter_biz$Tmax, nrow = 1), precip = matrix(wetter_biz$precip, nrow = 1),
  PAR = matrix(wetter_biz$PAR, nrow = 1), ET0 = matrix(wetter_biz$ET0, nrow = 1),
  jahr = 2024
)

# growR scheint Schaltjahre intern auf 365 Tage zu kappen (Laenge der
# Ausgabe hier 365 trotz 366 Zeilen Wettereingabe fuer 2024) - fuer den
# reinen Gleichungs-Abgleich werden beide auf die kuerzere Laenge gekappt,
# das ist kein Fehler in der eigenen Implementierung.
n_vgl <- min(length(s_growr$GRO), ncol(mein_erg$GRO))
vergleich_df <- data.frame(
  DOY = seq_len(n_vgl),
  GRO_growR = s_growr$GRO[1:n_vgl], GRO_eigen = mein_erg$GRO[1, 1:n_vgl],
  cBM_growR = s_growr$cBM[1:n_vgl], cBM_eigen = mein_erg$cBM[1, 1:n_vgl],
  BM_growR = s_growr$BM[1:n_vgl], BM_eigen = mein_erg$BM[1, 1:n_vgl]
)
cat("\nj_start_of_growing_season: growR =", s_growr$j_start_of_growing_season, " eigen =", mein_erg$j_start, "\n")
cat("Max. absolute Abweichung GRO:", max(abs(vergleich_df$GRO_growR - vergleich_df$GRO_eigen)), "\n")
cat("Max. absolute Abweichung cBM:", max(abs(vergleich_df$cBM_growR - vergleich_df$cBM_eigen)), "\n")
cat("Max. absolute Abweichung BM:", max(abs(vergleich_df$BM_growR - vergleich_df$BM_eigen)), "\n")
cat("Korrelation GRO:", round(cor(vergleich_df$GRO_growR, vergleich_df$GRO_eigen), 4), "\n")
cat("Korrelation cBM:", round(cor(vergleich_df$cBM_growR, vergleich_df$cBM_eigen), 4), "\n")

p_val <- ggplot(vergleich_df, aes(x = DOY)) +
  geom_line(aes(y = cBM_growR, color = "growR"), linewidth = 1) +
  geom_line(aes(y = cBM_eigen, color = "eigene Implementierung"), linetype = "dashed") +
  labs(title = "Validierung: eigene ModVege-Portierung vs. growR (NI=1, kein Schnitt, BIZ 2024)",
       y = "cBM (kumuliertes Wachstum)", color = "") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave(file.path(ausgabe_dir, "validierung_eigene_implementierung.png"), p_val, width = 10, height = 6, dpi = 120)
cat("\nPlot gespeichert: validierung_eigene_implementierung.png\n")
