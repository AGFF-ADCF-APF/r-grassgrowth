#!/bin/bash
# Ein Pipeline-Lauf im Container.
#   lauf.sh voll    kompletter Lauf: Daten, Karten, Datenexplorer, SVG-Upload
#                   (naechtlich per Cron und beim Container-Start)
#   lauf.sh seite   nur den Datenexplorer neu erzeugen, laufendes Jahr aus dem
#                   Cache, sofern Daten und Rechencode unveraendert sind
#                   (Schnellmodus, nach Code-Deploys - siehe deploy.sh)
# Nie zwei Laeufe gleichzeitig (Speicherlimit 10 GB): ein zweiter wartet, bis
# der erste fertig ist.
set -u
modus="${1:-voll}"
# Cron startet ohne die Container-Umgebung (siehe entrypoint.sh)
[ -f /etc/grassgrowth.env ] && . /etc/grassgrowth.env
export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
cd /app

exec 9>/tmp/grassgrowth-lauf.lock
if ! flock -w 7200 9; then
  echo "=== Abbruch ($modus): ein anderer Lauf blockiert seit 2 Stunden ($(date -Is)) ==="
  exit 1
fi

echo "=== Graswachstum-Lauf ($modus) gestartet: $(date -Is) ==="
case "$modus" in
  voll)
    Rscript /opt/deploy/automate.R
    rc=$? ;;
  seite)
    GRASSGROWTH_SCHNELL=1 Rscript -e 'source("01_import_googlesheet.R"); source("27_plot_datenexplorer.R")'
    rc=$? ;;
  *)
    echo "Unbekannter Modus: $modus (voll oder seite)"
    rc=2 ;;
esac
# Merker fuer entrypoint.sh: heute schon ein erfolgreicher voller Lauf
if [ "$rc" -eq 0 ] && [ "$modus" = voll ]; then
  date +%F > /app/ebenen_cache/.letzter-voller-lauf
fi
echo "=== Graswachstum-Lauf ($modus) beendet: $(date -Is), Exit-Code $rc ==="
exit "$rc"
