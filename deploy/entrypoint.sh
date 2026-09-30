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

echo "0 3 * * * cd /app && Rscript automate.R >> /var/log/grassgrowth-lauf.log 2>&1" | crontab -
cron

nginx -g "daemon off;"
