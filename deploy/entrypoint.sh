#!/bin/bash
# nginx startet SOFORT (liefert den letzten erfolgreichen Stand aus dem
# outputs/-Volume aus) - der R-Lauf laeuft im HINTERGRUND daneben, nicht
# davor. Ohne das wuerde JEDER Container-Neustart (auch nur wegen einer
# Config-Aenderung, nicht nur beim taeglichen Cron-Lauf) die Seite fuer die
# gesamte Laufzeit der Pipeline (mehrere Minuten) mit 502 lahmlegen - bei
# einem frischen Volume ganz ohne vorherigen Lauf ist outputs/ zunaechst
# leer, das wird bewusst in Kauf genommen (kurzzeitig 404 statt 502, bis
# der erste Lauf durch ist). Schlaegt der R-Lauf fehl (z.B. durch das
# Speicherlimit gestoppt) wird das nur geloggt - nginx liefert einfach
# weiter den letzten erfolgreichen Stand aus.
set -u
cd /app

lauf() {
  echo "=== Graswachstum-Lauf gestartet: $(date -Is) ==="
  Rscript automate.R
  echo "=== Graswachstum-Lauf beendet: $(date -Is), Exit-Code $? ==="
}

lauf >> /var/log/grassgrowth-lauf.log 2>&1 &

# Cron startet Jobs NICHT mit der Umgebung des Containers, sondern mit einer
# minimalen eigenen (PATH meist nur /usr/bin:/bin, keine env_file-Variablen).
# Ohne PATH-Zeile: "Rscript: not found". Ohne die Variablen: der naechtliche
# Lauf rechnete zwar, uebersprang aber den FTP-Upload (FTP_* fehlten) und
# nutzte die Standard-Pfade statt GRASSGROWTH_*. Deshalb hier die noetigen
# Variablen (bash-sicher gequotet) in eine nur fuer root lesbare Datei, die
# der Cron-Job vor dem Lauf laedt.
export -p | grep -E '^declare -x (FTP_|GRASSGROWTH_)' > /etc/grassgrowth.env
chmod 600 /etc/grassgrowth.env
{
  echo "SHELL=/bin/bash"
  echo "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
  echo '0 2 * * * . /etc/grassgrowth.env && cd /app && { echo "=== Graswachstum-Lauf gestartet (Cron): $(date -Is) ==="; Rscript automate.R; echo "=== Graswachstum-Lauf beendet (Cron): $(date -Is), Exit-Code $? ==="; } >> /var/log/grassgrowth-lauf.log 2>&1'
} | crontab -
cron

nginx -g "daemon off;"
