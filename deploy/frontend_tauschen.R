# Frontend (frontend/*) im fertigen Datenexplorer austauschen, ohne ihn neu zu
# erzeugen: keine Datenabfrage, kein Rechnen, wenige Sekunden. Aufruf im
# Container aus /app (deploy.sh, wenn nur Dateien unter frontend/ deployt
# werden):  Rscript /opt/deploy/frontend_tauschen.R
#
# Legt lib/gw-datenexplorer-<neue Version>/ an (Version wie in
# 27_plot_datenexplorer.R aus dem Inhalt von JS und CSS, damit Browser neu
# laden), stellt die Verweise in den Ausgabedateien um und entfernt den alten
# Ordner. Der naechste volle Lauf erzeugt dasselbe Ergebnis.
out_dir <- "outputs"
frontend_dateien <- file.path("frontend", c("datenexplorer.js", "datenexplorer.css"))
neu <- paste0("1.", strtoi(substr(digest::digest(
  paste(unlist(lapply(frontend_dateien, readLines, warn = FALSE)), collapse = "\n"), algo = "md5"), 1, 7), 16L))
alt_ordner <- Sys.glob(file.path(out_dir, "lib", "gw-datenexplorer-*"))
alt <- sub("^gw-datenexplorer-", "", basename(alt_ordner))
if (length(alt) == 0) stop("Kein lib/gw-datenexplorer-* in ", out_dir, " - erst einen vollen Lauf machen")
ziel <- file.path(out_dir, "lib", paste0("gw-datenexplorer-", neu))
dir.create(ziel, showWarnings = FALSE, recursive = TRUE)
if (!all(file.copy(list.files("frontend", full.names = TRUE), ziel, overwrite = TRUE))) stop("Kopieren nach ", ziel, " fehlgeschlagen")
if (identical(alt, neu)) {
  cat("Frontend-Version unveraendert (", neu, "), Dateien aktualisiert\n", sep = "")
  quit(save = "no", status = 0)
}
ausgaben <- file.path(out_dir, c("Datenexplorer_app.html", "Datenexplorer_einbettung.json", "Datenexplorer.html"))
for (f in ausgaben[file.exists(ausgaben)]) {
  text <- readLines(f, warn = FALSE, encoding = "UTF-8")
  for (a in setdiff(alt, neu)) text <- gsub(paste0("gw-datenexplorer-", a), paste0("gw-datenexplorer-", neu), text, fixed = TRUE)
  tmp <- paste0(f, ".tmp")
  writeLines(text, tmp, useBytes = TRUE)
  file.rename(tmp, f)
}
unlink(setdiff(alt_ordner, ziel), recursive = TRUE)
cat("Frontend ausgetauscht:", paste(alt, collapse = ", "), "->", neu, "\n")
