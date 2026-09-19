
library(RCurl)

mapfile_ftp <- paste0("Graswachstumkarte_", Jahr, "KW",week, ".svg")
mapprintfile_ftp <- paste0("Graswachstum_print_", Jahr, "KW",week, ".svg")
mapfile_current_ftp <- paste0("Graswachstumskarte_aktuell", ".svg")
mapprintfile_current_ftp <- paste0("Graswachstum_print_aktuell_", ".svg")

curvefile_ftp <- paste0("Graswachstumskurve_", Jahr, ".svg")
curvefile_current_ftp <- "Graswachstumskurve_aktuell.svg"

# Datenexplorer (27_plot_datenexplorer.R) ersetzt die bisherige einfache
# Plotly-Kurve (curvefile_plotly aus 22_plot_year.R). Eigener, jahrloser
# Zielname (die Jahresauswahl ist Teil des Datenexplorers selbst, keine
# separate Datei pro Jahr mehr noetig). Die Datei ist (anders als die alte,
# selfcontained gespeicherte Kurve) NICHT selfcontained, da sie mehrere
# Plotly-Widgets kombiniert (htmltools::save_html()) - der lib/-Ordner mit
# den JS/CSS-Abhaengigkeiten muss deshalb zusaetzlich hochgeladen werden.
#
# ZWEI Dateien statt einer: "Datenexplorer.html" ist die schlanke statische
# Vorschauseite (paar KB) - der oeffentliche, von aussen verlinkte Name.
# "Datenexplorer_app.html" ist die eigentliche interaktive App (~7.6MB),
# wird von der Vorschauseite erst per Klick nachgeladen (siehe deren
# eingebettetes <script> in 27_plot_datenexplorer.R).
datenexplorer_vorschau_datei <- "outputs/Datenexplorer.html"
datenexplorer_vorschau_ftp_ziel <- "Datenexplorer.html"
datenexplorer_app_datei <- "outputs/Datenexplorer_app.html"
datenexplorer_app_ftp_ziel <- "Datenexplorer_app.html"
datenexplorer_lib_dir <- "outputs/lib"
# Optionale Hintergrund-Ebenen (Niederschlag/Temperatur/Sonnenschein/etc.)
# liegen NICHT mehr in der Haupt-HTML, sondern als eigene JSON-Dateien
# (siehe schreibe_ebene_datei() in 27_plot_datenexplorer.R) und werden vom
# Browser erst beim Auswaehlen der jeweiligen Ebene per fetch() nachgeladen -
# dieser Ordner muss deshalb ZWINGEND mithochgeladen werden, sonst schlaegt
# das Nachladen live mit einem 404 fehl.
datenexplorer_ebenen_dir <- "outputs/ebenen"


## configure your own ftp settings
#ftpserver <- ""
#ftpuser <- ""
#ftppasswd <- kb$get("")

# Laedt eine einzelne Datei per FTP hoch. Primaer ueber Curl-Optionen
# (userpwd), mit Fallback auf in die URL eingebettete (URL-encodierte)
# Zugangsdaten, falls der Server auf den ersten Versuch mit "530" (nicht
# eingeloggt) statt der erwarteten Begruessung antwortet - manche FTP-Server
# akzeptieren userpwd ueber Curl-Optionen nicht zuverlaessig.
ftp_upload_file <- function(local_path, ftp_base_url, remote_rel_path, user, passwd) {
  remote_rel_path <- gsub("\\\\", "/", remote_rel_path)
  remote_rel_path <- utils::URLencode(remote_rel_path, reserved = TRUE)
  remote_url <- paste0(ftp_base_url, remote_rel_path)

  tryCatch({
    RCurl::ftpUpload(
      local_path, remote_url,
      .opts = list(userpwd = paste0(user, ":", passwd), ftp.create.missing.dirs = TRUE)
    )
  }, error = function(e) {
    if (!grepl("530", conditionMessage(e), fixed = TRUE)) stop(e)
    user_enc <- utils::URLencode(user, reserved = TRUE)
    pass_enc <- utils::URLencode(passwd, reserved = TRUE)
    remote_url_auth <- sub("^ftp://", paste0("ftp://", user_enc, ":", pass_enc, "@"), remote_url)
    RCurl::ftpUpload(local_path, remote_url_auth, .opts = list(ftp.create.missing.dirs = TRUE))
  })
}

ftp_upload_file_with_handle <- function(local_path, ftp_base_url, remote_rel_path, curl_handle) {
  remote_rel_path <- gsub("\\\\", "/", remote_rel_path)
  remote_rel_path <- utils::URLencode(remote_rel_path, reserved = TRUE)
  remote_url <- paste0(ftp_base_url, remote_rel_path)
  RCurl::ftpUpload(local_path, remote_url, curl = curl_handle)
}

# Legt einen Ordner direkt unter der FTP-Wurzel explizit per MKD an, STATT
# sich allein auf ftp.create.missing.dirs beim eigentlichen Datei-Upload zu
# verlassen. Grund: bei einem noch NIE existierenden Ordner (z.B. "ebenen/"
# beim allerersten Lauf mit den neuen Hintergrund-Ebenen-Dateien) hat sich
# ftp.create.missing.dirs auf diesem Server als unzuverlaessig erwiesen -
# alle Datei-Uploads in diesen Ordner scheiterten mit "530", obwohl Login
# und ein bereits bestehender Ordner (lib/) im selben Lauf einwandfrei
# funktionierten. Ein expliziter MKD-Versuch VORAB behebt das; existiert der
# Ordner schon (Normalfall bei jedem weiteren Lauf), liefert MKD einen
# harmlosen Fehler, der hier bewusst verschluckt wird - das eigentliche
# Scheitern zeigt sich sonst ohnehin beim folgenden Datei-Upload selbst.
ftp_ordner_erstellen <- function(ftp_root_url, ordner_name, user, passwd) {
  tryCatch({
    RCurl::curlPerform(
      url = ftp_root_url,
      userpwd = paste0(user, ":", passwd),
      quote = paste0("MKD ", ordner_name),
      dirlistonly = TRUE
    )
    invisible(TRUE)
  }, error = function(e) invisible(FALSE))
}

# Laedt alle Dateien eines lokalen Ordners rekursiv per FTP hoch (z.B. den
# lib/-Abhaengigkeitsordner des Datenexplorers). Eine wiederverwendete
# Session (curl-Handle) statt einer neuen Verbindung pro Datei reduziert
# Re-Logins und dadurch "530"-Folgefehler bei vielen Dateien; bei einem
# einzelnen 530 wird einmalig mit frischem Handle erneut versucht, bevor
# die Datei als fehlgeschlagen gemeldet wird. ordner_name (z.B. "ebenen")
# wird zuerst per ftp_ordner_erstellen() an der FTP-Wurzel (ftp_root_url)
# angelegt, bevor die Dateien in ftp_root_url/ordner_name/ hochgeladen
# werden - siehe Kommentar dort.
ftp_upload_recursive <- function(local_dir, ftp_root_url, ordner_name, user, passwd) {
  if (!dir.exists(local_dir)) return(invisible(0L))
  files <- list.files(local_dir, recursive = TRUE, full.names = FALSE)
  if (length(files) == 0L) return(invisible(0L))

  ftp_ordner_erstellen(ftp_root_url, ordner_name, user, passwd)
  ftp_base_url <- paste0(ftp_root_url, ordner_name, "/")
  curl_handle <- RCurl::getCurlHandle(userpwd = paste0(user, ":", passwd), ftp.create.missing.dirs = TRUE)

  uploaded <- 0L
  for (f in files) {
    local_path <- file.path(local_dir, f)
    ok <- tryCatch({
      ftp_upload_file_with_handle(local_path, ftp_base_url, f, curl_handle)
      TRUE
    }, error = function(e) {
      if (!grepl("530", conditionMessage(e), fixed = TRUE)) {
        warning("FTP-Upload fehlgeschlagen: ", f, " - ", conditionMessage(e))
        return(FALSE)
      }
      retry_err <- tryCatch({
        curl_retry <- RCurl::getCurlHandle(userpwd = paste0(user, ":", passwd), ftp.create.missing.dirs = TRUE)
        ftp_upload_file_with_handle(local_path, ftp_base_url, f, curl_retry)
        NULL
      }, error = function(e2) conditionMessage(e2))
      if (is.null(retry_err)) TRUE else { warning("FTP-Upload fehlgeschlagen: ", f, " - ", retry_err); FALSE }
    })
    if (isTRUE(ok)) uploaded <- uploaded + 1L
    Sys.sleep(0.2)
  }
  invisible(uploaded)
}

# Validiert die FTP-Zugangsdaten VOR dem Netzwerkzugriff, mit klaren
# Fehlermeldungen statt eines kryptischen Curl-Fehlers erst mitten im Lauf.
ftp_validate_credentials <- function(ftp_host, user, passwd) {
  if (!nzchar(trimws(ftp_host))) stop("FTP-Konfiguration unvollstaendig: ftpserver ist leer.", call. = FALSE)
  if (!nzchar(trimws(user))) stop("FTP-Konfiguration unvollstaendig: ftpuser ist leer.", call. = FALSE)
  if (!nzchar(passwd)) stop("FTP-Konfiguration unvollstaendig: ftppasswd ist leer (Keyring-Eintrag pruefen).", call. = FALSE)
  invisible(TRUE)
}

# Prueft Login + Schreibzugriff mit einer winzigen Testdatei, BEVOR der
# eigentliche Upload-Lauf (mehrere SVGs + der Datenexplorer samt lib/-
# Ordner) beginnt - schlaegt der Login grundsaetzlich fehl (falsches
# Passwort, gesperrter Account), bricht der Lauf hier klar ab statt bei
# jedem der ~20 Uploads einzeln denselben Fehler zu werfen.
ftp_preflight <- function(ftp_base_url, user, passwd, remote_dir = "archive") {
  remote_dir <- sub("/+$", "", remote_dir)
  probe_file <- tempfile(pattern = "ftp_preflight_", fileext = ".txt")
  on.exit(unlink(probe_file), add = TRUE)
  writeLines(sprintf("preflight %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), probe_file)

  probe_remote <- paste0(remote_dir, "/.ftp_preflight/healthcheck.txt")
  tryCatch({
    ftp_upload_file(probe_file, ftp_base_url, probe_remote, user, passwd)
    message("FTP-Preflight erfolgreich: Login + Schreibzugriff ok")
    invisible(TRUE)
  }, error = function(e) {
    stop(
      paste0("FTP-Preflight fehlgeschlagen (Login/Schreibzugriff): ", conditionMessage(e),
             " | Host=", sub("/+$", "", sub("^ftp://", "", ftp_base_url)), " | User=", user),
      call. = FALSE
    )
  })
}

ftp_host <- sub("^ftp://", "", ftpserver)
ftp_host <- sub("^.*@", "", ftp_host)
ftp_host <- sub("/+$", "", ftp_host)
ftp_base_url <- paste0("ftp://", ftp_host, "/")

ftp_validate_credentials(ftp_host, ftpuser, ftppasswd)
ftp_preflight(ftp_base_url, ftpuser, ftppasswd, remote_dir = "archive")

#weeknum
tryCatch(ftp_upload_file(mapfile, ftp_base_url, paste0("archive/", mapfile_ftp), ftpuser, ftppasswd),
         error = function(e) warning("FTP-Upload fehlgeschlagen: ", mapfile_ftp, " - ", conditionMessage(e)))
tryCatch(ftp_upload_file(mapprintfile, ftp_base_url, paste0("archive/print/", mapprintfile_ftp), ftpuser, ftppasswd),
         error = function(e) warning("FTP-Upload fehlgeschlagen: ", mapprintfile_ftp, " - ", conditionMessage(e)))

#current
tryCatch(ftp_upload_file(mapfile, ftp_base_url, mapfile_current_ftp, ftpuser, ftppasswd),
         error = function(e) warning("FTP-Upload fehlgeschlagen: ", mapfile_current_ftp, " - ", conditionMessage(e)))
tryCatch(ftp_upload_file(mapprintfile, ftp_base_url, mapprintfile_current_ftp, ftpuser, ftppasswd),
         error = function(e) warning("FTP-Upload fehlgeschlagen: ", mapprintfile_current_ftp, " - ", conditionMessage(e)))

#curves
tryCatch(ftp_upload_file(curvefile, ftp_base_url, curvefile_ftp, ftpuser, ftppasswd),
         error = function(e) warning("FTP-Upload fehlgeschlagen: ", curvefile_ftp, " - ", conditionMessage(e)))
tryCatch(ftp_upload_file(curvefile, ftp_base_url, curvefile_current_ftp, ftpuser, ftppasswd),
         error = function(e) warning("FTP-Upload fehlgeschlagen: ", curvefile_current_ftp, " - ", conditionMessage(e)))

#datenexplorer: schlanke Vorschauseite (oeffentlicher Name) + interaktive App
if (file.exists(datenexplorer_vorschau_datei)) {
  ok_vorschau <- tryCatch({
    ftp_upload_file(datenexplorer_vorschau_datei, ftp_base_url, datenexplorer_vorschau_ftp_ziel, ftpuser, ftppasswd)
    TRUE
  }, error = function(e) {
    warning("FTP-Upload fehlgeschlagen: ", datenexplorer_vorschau_ftp_ziel, " - ", conditionMessage(e))
    FALSE
  })
  cat("Datenexplorer-Vorschau hochgeladen (", datenexplorer_vorschau_ftp_ziel, "):", ok_vorschau, "\n")
} else {
  warning("Datenexplorer-Vorschau nicht gefunden (", datenexplorer_vorschau_datei, ") - 27_plot_datenexplorer.R zuerst ausfuehren.")
}
if (file.exists(datenexplorer_app_datei)) {
  ok <- tryCatch({
    ftp_upload_file(datenexplorer_app_datei, ftp_base_url, datenexplorer_app_ftp_ziel, ftpuser, ftppasswd)
    TRUE
  }, error = function(e) {
    warning("FTP-Upload fehlgeschlagen: ", datenexplorer_app_ftp_ziel, " - ", conditionMessage(e))
    FALSE
  })
  n_lib <- ftp_upload_recursive(datenexplorer_lib_dir, ftp_base_url, "lib", ftpuser, ftppasswd)
  n_lib_total <- length(list.files(datenexplorer_lib_dir, recursive = TRUE))
  n_ebenen <- ftp_upload_recursive(datenexplorer_ebenen_dir, ftp_base_url, "ebenen", ftpuser, ftppasswd)
  n_ebenen_total <- length(list.files(datenexplorer_ebenen_dir, recursive = TRUE))
  cat("Datenexplorer-App hochgeladen (", datenexplorer_app_ftp_ziel, "):", ok,
      "- Abhaengigkeits-Dateien in lib/:", n_lib, "von", n_lib_total,
      "- Ebenen-Dateien:", n_ebenen, "von", n_ebenen_total, "\n")
} else {
  warning("Datenexplorer-App nicht gefunden (", datenexplorer_app_datei, ") - 27_plot_datenexplorer.R zuerst ausfuehren.")
}
