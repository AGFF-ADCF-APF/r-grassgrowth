# upload_svgs.R (Container-Variante von 31_upload_ftp.R)
#
# Laedt NUR noch die zwei aktuellen SVGs (Karte, Kurve) zu graswachstum.ch
# hoch - die interaktive App/lib/ebenen-Dateien braucht dieses Ziel nicht
# mehr, die interaktive Version laeuft jetzt direkt unter
# apps.graswachstum.ch/growth/ (dieser Container liefert sie selbst aus).
#
# Zugangsdaten NIE im Repo/Image - kommen als Umgebungsvariablen aus einer
# .env-Datei (siehe docker-compose.yml: env_file), die nur auf dem Server
# liegt und nicht versioniert ist.
library(RCurl)

ftp_host <- Sys.getenv("FTP_HOST")
ftp_user <- Sys.getenv("FTP_USER")
ftp_passwd <- Sys.getenv("FTP_PASSWORD")

if (!nzchar(ftp_host) || !nzchar(ftp_user) || !nzchar(ftp_passwd)) {
  cat("FTP-Upload uebersprungen: FTP_HOST/FTP_USER/FTP_PASSWORD nicht gesetzt (siehe deploy/.env).\n")
  quit(save = "no", status = 0)
}

out_dir <- Sys.getenv("GRASSGROWTH_OUT_DIR", "outputs")
mapfile <- file.path(out_dir, "Graswachstumskarte_aktuell.svg")
curvefile <- file.path(out_dir, "Graswachstumskurve_aktuell.svg")

ftp_base_url <- paste0("ftp://", ftp_host, "/")
ftp_handle <- RCurl::getCurlHandle(userpwd = paste0(ftp_user, ":", ftp_passwd), ftp.create.missing.dirs = TRUE)

# Wie in 31_upload_ftp.R (siehe dort fuer die ausfuehrliche Herleitung):
# bei "530" einmaliger Fallback auf eine separate Verbindung mit in die URL
# eingebetteten Zugangsdaten.
ftp_upload_file <- function(local_path, remote_rel_path) {
  remote_url <- paste0(ftp_base_url, utils::URLencode(remote_rel_path, reserved = TRUE))
  tryCatch({
    RCurl::ftpUpload(local_path, remote_url, curl = ftp_handle)
    TRUE
  }, error = function(e) {
    if (!grepl("530", conditionMessage(e), fixed = TRUE)) {
      warning("FTP-Upload fehlgeschlagen: ", remote_rel_path, " - ", conditionMessage(e))
      return(FALSE)
    }
    user_enc <- utils::URLencode(ftp_user, reserved = TRUE)
    pass_enc <- utils::URLencode(ftp_passwd, reserved = TRUE)
    remote_url_auth <- sub("^ftp://", paste0("ftp://", user_enc, ":", pass_enc, "@"), remote_url)
    tryCatch({
      RCurl::ftpUpload(local_path, remote_url_auth, .opts = list(ftp.create.missing.dirs = TRUE))
      TRUE
    }, error = function(e2) {
      warning("FTP-Upload fehlgeschlagen: ", remote_rel_path, " - ", conditionMessage(e2))
      FALSE
    })
  })
}

if (file.exists(mapfile)) {
  ok <- ftp_upload_file(mapfile, "Graswachstumskarte_aktuell.svg")
  cat("Graswachstumskarte_aktuell.svg hochgeladen:", ok, "\n")
} else {
  cat("Graswachstumskarte_aktuell.svg nicht gefunden (", mapfile, ") - 27_plot_datenexplorer.R zuerst ausfuehren.\n")
}
if (file.exists(curvefile)) {
  ok <- ftp_upload_file(curvefile, "Graswachstumskurve_aktuell.svg")
  cat("Graswachstumskurve_aktuell.svg hochgeladen:", ok, "\n")
} else {
  cat("Graswachstumskurve_aktuell.svg nicht gefunden (", curvefile, ") - 22_plot_year.R zuerst ausfuehren.\n")
}
