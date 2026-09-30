# automate.R (Container-Variante von 00_automate.R)
#
# Die interaktive App braucht keinen FTP-Upload mehr (31_upload_ftp.R) - der
# Container liefert outputs/ direkt per nginx aus (siehe docker-compose.yml).
# graswachstum.ch (die bisherige, separate Seite) zeigt weiterhin die zwei
# statischen SVGs (Karte, Kurve) wie bis anhin - dafuer bleibt ein schlanker
# FTP-Upload noetig, siehe upload_svgs.R.
source("01_import_googlesheet.R")
source("21_plot_map.R")
source("21a_plot_map_print.R")
source("22_plot_year.R")
source("27_plot_datenexplorer.R")
source("upload_svgs.R")
