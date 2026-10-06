#!/bin/bash
# nginx startet SOFORT (liefert den letzten erfolgreichen Stand aus dem
# outputs/-Volume aus) - ein R-Lauf laeuft im HINTERGRUND daneben, nicht
# davor. Schlaegt er fehl (z.B. durch das Speicherlimit gestoppt), wird das
# nur geloggt - nginx liefert einfach weiter den letzten erfolgreichen Stand.
#
# Code und Deploy-Skripte sind als Volume eingebunden (docker-compose.yml),
# nicht ins Image kopiert: ein Code-Deploy braucht weder Image-Neubau noch
# Neustart, nur deploy.sh (Dateien kopieren + "lauf.sh seite").
set -u
cd /app

# Cron startet Jobs NICHT mit der Umgebung des Containers, sondern mit einer
# minimalen eigenen (PATH meist nur /usr/bin:/bin, keine env_file-Variablen).
# Ohne PATH: "Rscript: not found". Ohne die Variablen: kein FTP-Upload und
# Standard-Pfade statt GRASSGROWTH_*. Deshalb die noetigen Variablen (bash-
# sicher gequotet) in eine nur fuer root lesbare Datei, die lauf.sh laedt.
export -p | grep -E '^declare -x (FTP_|GRASSGROWTH_)' > /etc/grassgrowth.env
chmod 600 /etc/grassgrowth.env

# Startlauf nur, wenn heute noch kein voller Lauf erfolgreich war - ein
# Neustart am selben Tag (z.B. nach einer Konfigurationsaenderung) rechnet
# nicht nochmals eine Stunde lang dasselbe.
if [ "$(cat /app/ebenen_cache/.letzter-voller-lauf 2>/dev/null)" != "$(date +%F)" ]; then
  /bin/bash /opt/deploy/lauf.sh voll >> /var/log/grassgrowth-lauf.log 2>&1 &
else
  echo "=== Startlauf uebersprungen: heute bereits ein erfolgreicher voller Lauf ($(date -Is)) ===" >> /var/log/grassgrowth-lauf.log
fi

{
  echo "SHELL=/bin/bash"
  echo "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
  echo '0 2 * * * /bin/bash /opt/deploy/lauf.sh voll >> /var/log/grassgrowth-lauf.log 2>&1'
} | crontab -
cron

nginx -g "daemon off;"
