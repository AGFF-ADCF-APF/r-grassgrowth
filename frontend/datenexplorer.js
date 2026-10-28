/*
 * Datenexplorer Graswachstum - Frontend.
 * Wird von 27_plot_datenexplorer.R als htmltools-Abhaengigkeit an beide
 * Plotly-Widgets gehaengt (lib/gw-datenexplorer-<version>/). R liefert nur
 * Daten: die onRender-Hooks rufen GWDatenexplorer.karte(el, x, d) bzw.
 * GWDatenexplorer.kurve(el, x, daten) mit einem Datenobjekt auf.
 */
window.GWDatenexplorer = window.GWDatenexplorer || {};

// Geteilter Bericht (#…ber=1…): sofort eine ruhige Ladeanzeige ueber die ganze Seite, damit der
// Aufbau des Datenexplorers (Ebenen, Karte) nicht sichtbar ist. berichtAnzeigen() uebernimmt sie.
GWDatenexplorer.berichtLaden = function() {
  if (!/[#&]ber=1(&|$)/.test(window.location.hash)) return null;
  var d = document.querySelector('.gw-bericht');
  if (d) return d;
  if (!document.getElementById('gw-bericht-stil')) {
    var st = document.createElement('style');
    st.id = 'gw-bericht-stil';
    st.textContent = '.gw-bericht{position:fixed;inset:0;z-index:2147483000;background:#eef1ee;display:flex;flex-direction:column;font-family:system-ui,-apple-system,"Segoe UI",Roboto,sans-serif}' +
      '.gw-bericht-laden{margin:auto;display:flex;flex-direction:column;align-items:center;gap:18px;color:#2f4f47;font-size:16px}' +
      '.gw-bericht-rad{position:relative;width:56px;height:56px;animation:gw-drehen 1.1s steps(8) infinite}' +
      '.gw-bericht-rad i{position:absolute;left:24px;top:0;width:8px;height:8px;border-radius:50%;background:#2f6f5e;transform-origin:4px 28px}' +
      '@keyframes gw-drehen{to{transform:rotate(360deg)}}@media (prefers-reduced-motion:reduce){.gw-bericht-rad{animation-duration:4s}}';
    document.head.appendChild(st);
  }
  d = document.createElement('div');
  d.className = 'gw-bericht';
  var rad = '';
  for (var i = 0; i < 8; i++) rad += '<i style="transform:rotate(' + (i * 45) + 'deg);opacity:' + (0.2 + i * 0.1).toFixed(1) + '"></i>';
  d.innerHTML = '<div class="gw-bericht-laden" role="status"><div class="gw-bericht-rad" aria-hidden="true">' + rad + '</div><span>Bericht wird aufgebaut …</span></div>';
  (document.body || document.documentElement).appendChild(d);
  return d;
};
if (document.body) GWDatenexplorer.berichtLaden(); else document.addEventListener('DOMContentLoaded', function() { GWDatenexplorer.berichtLaden(); });

GWDatenexplorer.karte = function(el, x, d) {
  // Feste Kartenausmasse aus R (lon_range_erweitert/lat_range/
  // karten_scaleratio) - fuer die Ansichts-Berechnung unten EXPLIZIT
  // mitgegeben statt sich auf Plotlys eigene scaleanchor/scaleratio-
  // Bereichsanpassung zu verlassen: die hat sich bei mehreren
  // relayout()-Aufrufen (Breite/Hoehe aendert sich mehrfach) als instabil
  // erwiesen - je nach vorherigem Zwischenzustand blieb mal die x-, mal die
  // y-Achse auf einen viel zu grossen Bereich gestreckt, mit einem winzigen
  // Kartenfleck inmitten viel Leerraum als Resultat.
  var xMin = d.xMin, xMax = d.xMax, yMitte = d.yMitte, scaleratio = d.scaleratio;
  // Mobile: rechter Rand fuer die AFC-Legende entfaellt (Legende dort hinter
  // dem i-Knopf), die Schweiz fuellt die ganze Breite.
  var xMaxSchweiz = d.xMaxSchweiz, ySpanSchweiz = d.ySpanSchweiz;
  // fuer den Export (ganze Schweiz als Figur, siehe kartenFigur())
  GWDatenexplorer.kartenInfo = { xMin: xMin, xMax: xMax, yMitte: yMitte, scaleratio: scaleratio, xMaxSchweiz: xMaxSchweiz, ySpanSchweiz: ySpanSchweiz };
  var xSpan = xMax - xMin;

  // 'Ganze Schweiz'-Ansicht (x-/y-Achsenbereich) fuer eine gegebene
  // Containergroesse - x bleibt immer auf dem vollen lon_range_erweitert
  // (nutzt die volle Breite), y wird so berechnet, dass bei diesem
  // Seitenverhaeltnis exakt keine Rand-Leerflaeche entsteht (weder
  // gestaucht noch gestreckt). Wird unten sowohl fuer die initiale/
  // Resize-Ansicht als auch fuer die Zoom-Sperre und den 'Ganze Schweiz'-
  // Knopf gebraucht - deshalb als eigene Funktion statt nur inline fuer
  // den Mobile-Fall (wie zuvor).
  // App-Layout (Vollbild bzw. eigene Seite ab 700px, Klasse gw-app auf
  // #gw-seite, gesetzt vom Kurven-Skript): die Karte fuellt den Bereich,
  // den ihr das Raster laesst, statt einer festen Hoehe.
  function istAppLayout() {
    var s = document.getElementById('gw-seite');
    return !!(s && (s.classList.contains('gw-app') || s.classList.contains('gw-mobil-app')));
  }
  function vollAnsichtBerechnen(breite, hoehe) {
    // Mobile und App-Layout: kein Plotly-Titel (Kopfzeile ist HTML), Rand
    // oben nur 6px.
    var mobil = breite < 700 || window.matchMedia('(max-height: 500px) and (pointer: coarse)').matches;
    var xHi = mobil ? xMaxSchweiz : xMax, span = xHi - xMin;
    var plotBreite = breite - 20, plotHoehe = hoehe - ((mobil || istAppLayout()) ? 16 : 50);
    var ySpan = span * plotHoehe / (scaleratio * plotBreite);
    // Breiter als die Schweiz hoch ist (z.B. Querformat im App-Layout): die
    // ganze Schweiz bleibt sichtbar, links und rechts kommt Rand dazu.
    var ySpanMin = ySpanSchweiz * 1.04;
    if (ySpan < ySpanMin) {
      var xSpanNeu = ySpanMin * scaleratio * plotBreite / plotHoehe, xMitte = (xMin + xHi) / 2;
      return { x: [xMitte - xSpanNeu / 2, xMitte + xSpanNeu / 2], y: [yMitte - ySpanMin / 2, yMitte + ySpanMin / 2] };
    }
    return { x: [xMin, xHi], y: [yMitte - ySpan / 2, yMitte + ySpan / 2] };
  }
  var vollX = null, vollY = null;

  function fixiereGroesse() {
    var host = el.parentElement;
    var breite = host.clientWidth;
    var mobil = breite < 700;
    var app = istAppLayout();
    // Schmaler (Mobile-)Container: Hoehe aus dem Seitenverhaeltnis der
    // Schweiz. App-Layout: Hoehe des Rasterbereichs. Eingebettet in eine
    // Website: Bildschirmhoehe abzueglich Kopf und Zeitleiste, sonst 560px.
    // Der y-Achsenbereich wird in allen Faellen ueber vollAnsichtBerechnen()
    // explizit gesetzt (nicht Plotlys eigene, s.o. instabile Anpassung).
    var hoehe;
    if (app) hoehe = Math.max(200, host.clientHeight);
    else if (mobil) hoehe = Math.round(Math.max(200, (breite - 20) * ySpanSchweiz * scaleratio / (xMaxSchweiz - xMin) * 1.03 + 16));
    else if (window.GW_DATENEXPLORER_EINGEBETTET) hoehe = Math.round(Math.min(640, Math.max(400, window.innerHeight - 190)));
    else hoehe = 560;
    if (breite < 50 || hoehe < 50) return;
    var voll = vollAnsichtBerechnen(breite, hoehe);
    // Hineingezoomt: Ausschnitt behalten (Mitte und Massstab), nur die Hoehe
    // an das neue Seitenverhaeltnis anpassen - sonst sprang die Karte bei
    // jeder Groessenaenderung (Drehen, Leiste darunter) auf die ganze Schweiz
    var xr = el.layout.xaxis && el.layout.xaxis.range, yr = el.layout.yaxis && el.layout.yaxis.range;
    var gezoomt = vollX && xr && yr && (xr[1] - xr[0]) < (vollX[1] - vollX[0]) * 0.98;
    vollX = voll.x; vollY = voll.y;
    if (gezoomt) {
      var mobil = breite < 700 || window.matchMedia('(max-height: 500px) and (pointer: coarse)').matches;
      var plotHoehe = hoehe - ((mobil || istAppLayout()) ? 16 : 50);
      var ySpan = (xr[1] - xr[0]) * plotHoehe / (scaleratio * (breite - 20)), yM = (yr[0] + yr[1]) / 2;
      Plotly.relayout(el, { width: breite, height: hoehe, 'xaxis.range': xr.slice(), 'yaxis.range': [yM - ySpan / 2, yM + ySpan / 2] });
    } else {
      Plotly.relayout(el, { width: breite, height: hoehe, 'xaxis.range': voll.x, 'yaxis.range': voll.y });
    }
    // Container-Hoehe (CSS, fest 560px im HTML) der tatsaechlichen, hier
    // berechneten Kartenhoehe nachfuehren - sonst bleibt auf Mobile (kleinere
    // hoehe) darunter Leerraum im Container stehen, in dem die Zoom-
    // Steuerung (position:absolute, bottom:10px relativ zu diesem Container)
    // dann weit unterhalb der sichtbar gezeichneten Karte haengen wuerde.
    // Im App-Layout bestimmt das Raster die Hoehe (kein fester Wert).
    host.style.height = app ? '' : hoehe + 'px';
  }
  fixiereGroesse();
  window.addEventListener('resize', fixiereGroesse);
  // App-Layout: Ebenen ein-/ausklappen oder Kurve aufziehen aendert die
  // Kartenflaeche ohne Fenster-Resize.
  if (window.ResizeObserver) {
    var groesseGeplant = false;
    new ResizeObserver(function() {
      if (!istAppLayout() || groesseGeplant) return;
      groesseGeplant = true;
      requestAnimationFrame(function() { groesseGeplant = false; fixiereGroesse(); });
    }).observe(el.parentElement);
  }

  // Weiteres Herauszoomen ueber die 'ganze Schweiz'-Ansicht hinaus sperren
  // und dabei automatisch zentrieren: sobald der sichtbare Bereich (per
  // Mausrad/Pinch/Doppelklick/Plotly-eigener Modebar) die volle Ansicht
  // erreicht oder ueberschreitet, sofort auf die EXAKTE volle Ansicht
  // zurueckspringen - unabhaengig davon, WIE gezoomt/verschoben wurde.
  // zoomKorrekturLaeuft verhindert eine Endlosschleife durch den
  // relayout()-Aufruf der Korrektur selbst (loest wieder plotly_relayout
  // aus).
  var zoomKorrekturLaeuft = false;
  el.on('plotly_relayout', function(ev) {
    if (zoomKorrekturLaeuft || !vollX) return;
    var betroffen = Object.keys(ev).some(function(k) {
      return k.indexOf('xaxis') === 0 || k.indexOf('yaxis') === 0;
    });
    if (!betroffen) return;
    var xr = el.layout.xaxis.range;
    if (!xr || (xr[1] - xr[0]) >= (vollX[1] - vollX[0]) - 1e-6) {
      zoomKorrekturLaeuft = true;
      Plotly.relayout(el, { 'xaxis.range': vollX, 'yaxis.range': vollY })
        .then(function() { zoomKorrekturLaeuft = false; });
    }
  });

  // Keine eigenen +/-/CH-Knoepfe (mehr) - Plotlys eigene Modebar (oben,
  // bei Hover ueber der Karte eingeblendet) hat bereits Zoom-In/-Out/
  // Autoscale/Reset-Achsen-Knoepfe, die dasselbe leisten; die Zoom-Sperre
  // oben (plotly_relayout-Listener) greift unabhaengig davon, WIE gezoomt
  // wird (Mausrad, Pinch, Doppelklick, Modebar).
};

GWDatenexplorer.kurve = function(el, x, daten) {
  // Eingebettet in eine andere Seite (Grav-Plugin datenexplorer): der Lader
  // setzt die Adresse der App, damit nachgeladene Ebenen von dort kommen.
  var gwBasis = window.GW_DATENEXPLORER_BASIS || '';
  var eingebettet = !!window.GW_DATENEXPLORER_EINGEBETTET;
  var alleJahre = daten.ALLE_JAHRE;
  var neuestesJahr = daten.NEUESTES_JAHR;
  var jahreMitNiederschlag = daten.JAHRE_MIT_NIEDERSCHLAG;
  var nSiteGrowth = daten.N_SITE_GROWTH, nGroupGrowth = daten.N_GROUP_GROWTH, nSitePrecip = daten.N_SITE_PRECIP, nGroupPrecip = daten.N_GROUP_PRECIP;
  var siteGrowthMeta = daten.SITE_GROWTH_META;
  var groupGrowthMeta = daten.GROUP_GROWTH_META;
  var sitePrecipMeta = daten.SITE_PRECIP_META;
  var groupPrecipMeta = daten.GROUP_PRECIP_META;
  var groupLabels = daten.GROUP_LABELS;
  var siteNames = daten.SITE_NAMES;
  var standortVerlaeufe = daten.STANDORT_VERLAEUFE;
  var siteVisible = daten.SITE_VISIBLE;
  var regelmaessig = daten.REGELMAESSIG;
  var alleStandorteZeigen = false;
  var wochenTickvals = daten.WOCHEN_TICKVALS;
  var wochenTicktext = wochenTickvals.map(String);
  var datumTicktextJeJahr = daten.DATUM_TICKTEXT_JE_JAHR;
  var wochenDatumBereich = daten.WOCHEN_DATUM_BEREICH;
  var siteColors = daten.SITE_COLORS;
  var mapWochen = daten.MAP_WOCHEN;
  var mapPointOrts = daten.MAP_POINT_ORTS;
  var layerLegenden = daten.LAYER_LEGENDEN;
  // Ebenen-Verfuegbarkeit (welche Jahr/Woche-Schluessel existieren je Ebene)
  // - klein genug fuer die Haupt-HTML; die eigentlichen Bilder/Werte-Gitter
  // liegen in outputs/ebenen/<name>.json und werden per ladeEbene() erst
  // beim ersten Auswaehlen der jeweiligen Ebene nachgeladen (siehe unten) -
  // ebenenCache haelt sie danach im Speicher (kein wiederholtes Nachladen).
  var ebenenSchluessel = daten.EBENEN_SCHLUESSEL;
  var schnittanalyseGebiet = daten.SCHNITTANALYSE_GEBIET;
  // Radios, deren Verfuegbarkeit nur aus ebenenSchluessel folgt (Schnittanalyse, Vergleich mit dem Normal)
  var schnittRadios = [];
  function istSchnittEbene(name) { return name.indexOf('schnittanalyse_') === 0; }
  var ebenenCache = {};
  // Ebenen mit waehlbarer Fenstergroesse (Schieberegler oberhalb der
  // Legende, siehe macheMeteoFensterSchieberegler() weiter unten) - deren
  // Datei-/Cache-Schluessel ist <name>_<meteoFenster> statt nur <name>
  // (jede Fenstergroesse ist eine eigene JSON-Datei, siehe R:
  // schreibe_fenster_ebenen_dateien()). meteoFensterStandard legt fest, auf
  // welchen Wert der Schieberegler bei Auswahl der jeweiligen Ebene
  // automatisch zurueckspringt (siehe makeLayerRadio()-Aufrufe unten).
  var meteoFensterEbenen = ['niederschlag', 'temperatur', 'bodentemperatur', 'sonnenschein', 'et0'];
  var meteoFensterStandard = { niederschlag: 28, temperatur: 7, bodentemperatur: 7, sonnenschein: 7, et0: 7 };
  // Diskrete Schieberegler-Stufen (Tage) - siehe R: fenstergroessen_tage.
  var meteoFensterStufen = [7, 14, 21, 28];
  var meteoFenster = 7;
  function istFensterEbene(name) { return meteoFensterEbenen.indexOf(name) !== -1; }
  // Potenzielles Wachstum: je Stufe der Erholungsverzoegerung eine eigene
  // Datei <name>_e<stufe> (siehe R: erholung_stufen_tage).
  var erholungStufen = [0, 7, 14, 21];
  var erholung = 14;
  function istErholungsEbene(name) { return name === 'wachstumspotenzial_rate' || name === 'wachstumspotenzial_kum'; }
  function ebeneDateiSchluessel(name) {
    if (istFensterEbene(name)) return name + '_' + meteoFenster;
    if (istErholungsEbene(name)) return name + '_e' + erholung;
    return name;
  }
  function ebeneHatJahr(name, jahr) {
    var schluessel = ebenenSchluessel[name];
    if (!schluessel) return false;
    for (var i = 0; i < schluessel.length; i++) {
      if (schluessel[i].indexOf(jahr + ' ') === 0) return true;
    }
    return false;
  }
  // Wie ebeneHatJahr(), nur fuer die GENAUE (Jahr, Woche)-Kombination -
  // manche Ebenen (v.a. Sonnenschein, mit 1-2 Monaten Aufbereitungs-
  // verzoegerung) haben zwar Daten fuer das Jahr, aber nicht (mehr) fuer
  // die allerneuesten Wochen darin. ebeneHatJahr() allein wuerde das nicht
  // erkennen (Radio bliebe aktiv), die Karte zeigte dann fuer diese Woche
  // einfach nichts, ohne erkennbaren Unterschied zu laedt noch - siehe
  // aktualisiereLayerLegende().
  function ebeneHatWoche(name, jahr, woche) {
    var schluessel = ebenenSchluessel[name];
    return !!schluessel && schluessel.indexOf(jahr + ' ' + woche) !== -1;
  }
  // Laedt die JSON-Datei einer Ebene genau EINMAL je Datei-Schluessel
  // (Cache-Treffer bei jedem weiteren Aufruf mit demselben Schluessel) und
  // ruft dann callback(daten) auf; daten hat die Form { bilder: {...},
  // werte: {...}, datum: {...} (nur bei boden) }. dateiSchluessel ist bei
  // Fenster-Ebenen NAME_FENSTER (siehe ebeneDateiSchluessel()), sonst nur
  // NAME. Bricht eine noch laufende Anfrage NICHT ab, wenn zwischenzeitlich
  // eine andere Ebene/Fenstergroesse gewaehlt wurde - die Aufrufer
  // (aktualisiereHintergrundEbene() etc.) pruefen deshalb nach Abschluss
  // jeweils selbst, ob ihr Schluessel noch aktuell ist, bevor sie das
  // Ergebnis anwenden.
  function ladeEbene(dateiSchluessel, callback) {
    if (ebenenCache[dateiSchluessel]) { callback(ebenenCache[dateiSchluessel]); return; }
    fetch(gwBasis + 'ebenen/' + dateiSchluessel + '.json')
      .then(function(r) { return r.json(); })
      .then(function(daten) { ebenenCache[dateiSchluessel] = daten; callback(daten); })
      .catch(function(err) { console.error('Ebene ' + dateiSchluessel + ' konnte nicht geladen werden:', err); });
  }
  var graswachstumBilder = daten.GRASWACHSTUM_BILDER;
  var afcRingBilder = daten.AFC_RING_BILDER;
  // Index (in afc_optimum_windows/afcVerlaeufe) des jahreszeitlichen AFC-
  // Zielkorridors je (Jahr, Woche) - fuer die kompakte AFC-Legende im
  // Ebenen-Kasten (siehe aktualisiereAfcLegende()), unabhaengig davon, ob
  // diese Woche ueberhaupt ein Standort mit AFC-Wert hat.
  var afcFensterJeWoche = daten.AFC_FENSTER_JE_WOCHE;
  var afcVerlaeufe = daten.AFC_VERLAEUFE;
  var kartenbildHintergrund = daten.KARTENBILD_HINTERGRUND;
  var heutigeWoche = daten.HEUTIGE_WOCHE;
  var grafikDatum = daten.GRAFIK_DATUM;
  var messDatumJeJahr = daten.MESS_DATUM_JE_JAHR || {};
  var standardKurveTraceIdx = daten.STANDARD_KURVE_TRACE_IDX;

  var selection = { type: 'group', idx: 0 };
  // Vorherige Auswahl, gesetzt beim Wechsel per Klick auf einen Legenden-
  // oder Karteneintrag (siehe waehleSiteViaKlick()) - ein erneuter Klick
  // auf denselben (bereits aktiven) Eintrag stellt sie wieder her. Die
  // Combobox selbst setzt sie bewusst NICHT (dort nur Vorwaerts-Auswahl).
  var vorherigeSelection = null;
  var precipOn = true;
  var datumOn = false;
  var vorjahrOn = false;
  // Trace-Indizes, deren Linien-/Marker-Farbe aktuell fuer die Vorjahres-
  // Ueberlagerung auf Grau umgestellt ist, mit der jeweiligen Original-
  // farbe - noetig, um sie bei Auswahl-/Jahreswechsel oder Ausschalten des
  // Schalters wieder korrekt einzufaerben (siehe aktualisiereVorjahrOverlay()).
  var vorjahrStyledTraceIdx = [];
  // Wie vorjahrStyledTraceIdx, aber fuer die Niederschlags-Vorjahresbalken
  // (siehe aktualisiereVorjahrOverlay()) - separat gefuehrt, da Balken andere
  // Style-Attribute (Farbe/Breite/Fehlerbalken statt Linien-/Markerfarbe)
  // brauchen als die Wachstumskurven.
  var vorjahrPrecipStyledTraceIdx = [];
  var growthMapKlickGebunden = false;
  var growthMapHoverGebunden = false;
  var growthMapZeigerGebunden = false;
  // Graswachstum-Kreis und DGV-Ring (AFC, die BILD-Ebenen) haben je einen
  // eigenen Schalter. Die (unsichtbaren, nur fuer Hover + die "Tage seit
  // Messung"-Farblegende benoetigten) Standort-Marker selbst haben KEINEN
  // eigenen Schalter mehr (vormals "Messnetz-Standorte") - sie sind
  // hoverbar, sobald mindestens einer der beiden Schalter an ist (siehe
  // applyMapState()), da die Legende ja genau zu diesen beiden Ebenen
  // gehoert.
  var graswachstumOn = true;
  var afcOn = true;
  // Schalter MeteoSchweiz-Stationen (Ebenen-Kasten): Default AUS - reine
  // Referenz-Ebene, nicht Teil der eigentlichen AGFF-Auswertung.
  // smnStationenTraceIdx zeigt auf die EINE,
  // von Jahr/Woche unabhaengige Trace (siehe R: smn_stationen_trace_idx).
  var smnStationenOn = false;
  var smnStationenTraceIdx = daten.SMN_STATIONEN_TRACE_IDX;
  // Stations-Metadaten (Name/Lage/Kanton/Hoehe, aus R gebacken - aendert
  // sich praktisch nie) fuer den client-seitigen Live-Fetch der taeglich
  // aktuellen Messwerte, siehe ladeSmnAktuellwerte() unten.
  var smnStationenMeta = daten.SMN_STATIONEN_META;
  var smnBasisUrl = daten.SMN_BASIS_URL;
  var smnWerteGeladen = false, smnLaedt = false;
  // Trace-Index des per PLZ/Ort-Suche gesetzten Fadenkreuz-Markers auf der
  // Wachstumskarte (siehe platziereFadenkreuz()) - null, solange noch nie
  // gesucht wurde; die Trace wird beim ersten Treffer einmalig per
  // Plotly.addTraces() angelegt und danach nur noch verschoben.
  var fadenkreuzTraceIdx = null;
  // Eigene Orte (siehe «Eigene Orte» weiter unten) - frueh angelegt, weil die
  // Ebenen-Leiste ihren Schalter schon beim Aufbau registriert
  var eigeneOrte = [];
  var orteAnzeigen = false;
  var ortLinkAusHash = false;
  var orteTraceIdx = null;
  var orteHoerer = [];
  var ORTE_FARBE = '#c2185b';
  var ORTE_SPEICHER = 'gw-eigene-orte';
  try {
    var gespeicherteOrte = JSON.parse(localStorage.getItem(ORTE_SPEICHER) || '[]');
    if (Array.isArray(gespeicherteOrte)) eigeneOrte = gespeicherteOrte.filter(function(o) { return o && o.name && isFinite(o.lon) && isFinite(o.lat); });
  } catch (e) {}
  var hintergrundEbene = 'keine';
  var selectedYear = neuestesJahr;
  var selectedWeek = daten.START_WOCHE;

  // Zukuenftige Wochen (nach der aktuellen Kalenderwoche, nur im neuesten
  // Jahr relevant) sind noch nicht gemessen - weder per Regler/Pfeilen
  // noch per Klick auf die x-Achse darf darauf verschoben werden.
  function maxWocheFuerJahr(jahr) {
    return jahr === neuestesJahr ? heutigeWoche : 52;
  }

  // Gruppenansicht: zuerst nur regelmaessig messende Standorte (siehe R
  // regelmaessig_je_jahr), die uebrigen per Legende zuschaltbar. Hat ein
  // Jahr (noch) keinen solchen Standort, erscheinen alle.
  function regelListe() { return regelmaessig[selectedYear] || []; }
  // Auswahl: eine Gruppe ('group'), ein Standort ('site') oder beliebig viele
  // Standorte und Gruppen kombiniert ('multi', mit Listen sites/groups).
  function auswahlListen() {
    if (selection.type === 'group') return { groups: [selection.idx], sites: [] };
    if (selection.type === 'site') return { groups: [], sites: [selection.idx] };
    return { groups: selection.groups.slice(), sites: selection.sites.slice() };
  }
  function kopiereAuswahl(a) { return a.type === 'multi' ? { type: 'multi', groups: a.groups.slice(), sites: a.sites.slice() } : { type: a.type, idx: a.idx }; }
  function setzeAuswahl(groups, sites) {
    var vorher = selection;
    if (!groups.length && !sites.length) groups = [0];
    if (groups.length === 1 && !sites.length) selection = { type: 'group', idx: groups[0] };
    else if (sites.length === 1 && !groups.length) selection = { type: 'site', idx: sites[0] };
    else selection = { type: 'multi', groups: groups.slice(), sites: sites.slice() };
    if (selection.type === 'site' && vorher.type !== 'site') aktiviereVorjahrFuerEinzelstandort();
    // Kombinationen: automatisch eingeschaltetes Vorjahr wieder aus (zu unruhig)
    if (selection.type === 'multi' && vorjahrAutomatisch && vorjahrOn) {
      vorjahrOn = false; vorjahrAutomatisch = false;
      if (vorjahrToggleWrap && vorjahrToggleWrap.checkbox) vorjahrToggleWrap.checkbox.checked = false;
    }
  }
  function gruppeGewaehlt(gIdx) {
    return selection.type === 'group' ? selection.idx === gIdx : (selection.type === 'multi' && selection.groups.indexOf(gIdx) !== -1);
  }
  // Kombinierte Auswahl: jede gewaehlte Kurve bekommt eine eigene, gut
  // unterscheidbare Farbe (farbenblind-tauglich nach Okabe-Ito, ergaenzt;
  // bewusst ohne Rot - das ist der Durchschnitt Mittelland), vergeben in der
  // Reihenfolge der Kaertchen. Ein einzelnes Gruppenmittel bleibt schwarz.
  var AUSWAHL_FARBEN = ['#0072B2', '#E69F00', '#009E73', '#CC79A7', '#56B4E9', '#D55E00', '#7B3294', '#8C510A', '#999933', '#332288', '#117733', '#AA4499'];
  function auswahlFarbIndex(type, idx) {
    if (selection.type !== 'multi') return -1;
    var gruppenFarbig = selection.groups.length >= 2;
    if (type === 'group') return gruppenFarbig ? selection.groups.indexOf(idx) : -1;
    var i = selection.sites.indexOf(idx);
    return i === -1 ? -1 : i + (gruppenFarbig ? selection.groups.length : 0);
  }
  function gruppenFarbe(gIdx) {
    var i = auswahlFarbIndex('group', gIdx);
    return i === -1 ? 'black' : AUSWAHL_FARBEN[i % AUSWAHL_FARBEN.length];
  }
  function siteFarbe(siteIdx) {
    var i = auswahlFarbIndex('site', siteIdx);
    return i === -1 ? siteColors[siteIdx] : AUSWAHL_FARBEN[i % AUSWAHL_FARBEN.length];
  }
  function gruppenName(gIdx) { return groupLabels[gIdx].replace(/^Region: /, '').replace(/^Hoehenlage: /, 'Höhenlage '); }
  function auswahlText() {
    var a = auswahlListen();
    if (selection.type === 'group') return groupLabels[selection.idx].replace('Hoehenlage', 'Höhenlage');
    if (selection.type === 'site') return siteNames[selection.idx];
    var namen = a.groups.map(gruppenName).concat(a.sites.map(function(i) { return siteNames[i]; }));
    if (namen.length <= 3) return namen.join(', ');
    return namen.slice(0, 2).join(', ') + ' und ' + (namen.length - 2) + ' weitere';
  }
  function siteInAuswahl(siteIdx) {
    if (selection.type === 'multi') return selection.sites.indexOf(siteIdx) !== -1;
    if (selection.type !== 'group') return selection.type === 'site' && siteIdx === selection.idx;
    if (!siteVisible[selection.idx][siteIdx]) return false;
    var liste = regelListe();
    return alleStandorteZeigen || liste.length === 0 || liste.indexOf(siteIdx) !== -1;
  }
  // Nur Standorte mit einer Kurve im gewaehlten Jahr zaehlen
  var sitesMitDaten = {};
  siteGrowthMeta.forEach(function(m) { (sitesMitDaten[m.year] = sitesMitDaten[m.year] || {})[m.siteIdx] = true; });
  function weitereStandorte() {
    if (selection.type !== 'group') return 0;
    var mitDaten = sitesMitDaten[selectedYear] || {}, n = 0;
    for (var i = 0; i < siteNames.length; i++) if (mitDaten[i] && siteVisible[selection.idx][i] && !siteInAuswahl(i)) n++;
    return n;
  }

  function applyState() {
    if (GWDatenexplorer.schreibeUrlBald) GWDatenexplorer.schreibeUrlBald();
    setTimeout(aktualisiereKurvenGriff, 0);
    setTimeout(aktualisiereTeaser, 0);
    // vis wird ueber den in R mitgelieferten traceIdx (echte Plotly-Trace-
    // Position) befuellt, NICHT durch positionsweises Anhaengen (push) je
    // Meta-Liste - die Traces wurden auf R-Seite pro Jahr VERSCHACHTELT
    // angelegt (Standort-Wachstum, Gruppen-Wachstum, ggf. Niederschlag,
    // pro Jahr wiederholt), ein einfaches Aneinanderhaengen aller Eintraege
    // JE TYP ueber alle Jahre haette hier (ausser bei zufaellig gleicher
    // Standort-/Gruppenanzahl pro Jahr) zu falsch zugeordneten visible-
    // Flags gefuehrt (sichtbar wurde ein VOELLIG ANDERER Standort als der
    // gewaehlte).
    var vis = new Array(standardKurveTraceIdx + 1).fill(false);
    siteGrowthMeta.forEach(function(m) {
      vis[m.traceIdx] = m.year === selectedYear && siteInAuswahl(m.siteIdx);
    });
    groupGrowthMeta.forEach(function(m) {
      vis[m.traceIdx] = m.year === selectedYear && gruppeGewaehlt(m.groupIdx);
    });
    sitePrecipMeta.forEach(function(m) {
      var match = selection.type === 'site' && m.siteIdx === selection.idx;
      vis[m.traceIdx] = precipOn && m.year === selectedYear && match;
    });
    groupPrecipMeta.forEach(function(m) {
      vis[m.traceIdx] = precipOn && m.year === selectedYear && selection.type === 'group' && m.groupIdx === selection.idx;
    });
    vis[standardKurveTraceIdx] = true;
    Plotly.restyle(el, { visible: vis });
    aktualisiereVorjahrOverlay();
    // Farben der Kurven des gewaehlten Jahres (kombinierte Auswahl: eigene
    // Palette, sonst Standortfarbe bzw. schwarzes Mittel) - nach dem
    // Vorjahres-Overlay, das zurueckgesetzte Traces sonst ueberschreiben koennte
    var fIdx = [], fWerte = [];
    var setzeFarbe = function(m, f) {
      var d = el.data[m.traceIdx];
      if (m.year !== selectedYear || !d) return;
      if ((d.line && d.line.color) !== f) { fIdx.push(m.traceIdx); fWerte.push(f); }
    };
    siteGrowthMeta.forEach(function(m) { setzeFarbe(m, siteFarbe(m.siteIdx)); });
    groupGrowthMeta.forEach(function(m) { setzeFarbe(m, gruppenFarbe(m.groupIdx)); });
    if (fIdx.length) Plotly.restyle(el, { 'line.color': fWerte, 'marker.color': fWerte }, fIdx);
    aktualisiereSkala();
    renderLegendItems();
    applyMapState();
  }

  // Schalter Vorjahresdaten: blendet zusaetzlich zur aktuellen Auswahl
  // die Kurve(n) des VORJAHRS ein, in Grau statt der Standort-/Gruppen-
  // eigenen Farbe - reine Vergleichsreferenz, aendert selection/selectedYear
  // nicht. Faerbt zuerst alle zuvor grau gestellten Traces (aus einer
  // fruaheren Auswahl) wieder auf ihre Originalfarbe zurueck, damit keine
  // Trace faelschlich grau bleibt, wenn sich Auswahl, Jahr oder der
  // Schalter selbst aendert.
  function aktualisiereVorjahrOverlay() {
    if (vorjahrStyledTraceIdx.length > 0) {
      var origFarben = vorjahrStyledTraceIdx.map(function(e) { return e.color; });
      Plotly.restyle(el,
        { 'line.color': origFarben, 'marker.color': origFarben },
        vorjahrStyledTraceIdx.map(function(e) { return e.traceIdx; })
      );
      vorjahrStyledTraceIdx = [];
    }
    // Nur Farbe/Breite/Fehlerbalken zuruecksetzen, NICHT 'visible' - die
    // eigentliche Sichtbarkeit je Trace wird bereits durch das volle vis[]-
    // Array in applyState() (vor dem Aufruf dieser Funktion) korrekt gesetzt;
    // ein zusaetzliches 'visible:false' hier wuerde einen frisch auf true
    // gesetzten Balken sofort wieder ausblenden, falls das Vorjahr der
    // vorherigen Auswahl zufaellig das NEU gewaehlte Jahr ist.
    if (vorjahrPrecipStyledTraceIdx.length > 0) {
      Plotly.restyle(el,
        { 'marker.color': 'steelblue', width: 0.7, 'error_y.visible': true },
        vorjahrPrecipStyledTraceIdx
      );
      vorjahrPrecipStyledTraceIdx = [];
    }
    if (!vorjahrOn) return;

    var vorjahr = String(parseInt(selectedYear, 10) - 1);

    // Alle anderen Jahre in Grau: das Vorjahr dunkler, aeltere Jahre heller (Jahr steht im Tooltip)
    var grauFuer = function(jahr) { return jahr === vorjahr ? 'rgba(120,120,120,0.75)' : 'rgba(150,150,150,0.45)'; };
    var traceIdxListe = [], grauListe = [];
    var anderesJahr = function(m) { return m.year !== selectedYear && alleJahre.indexOf(m.year) !== -1; };
    siteGrowthMeta.forEach(function(m) {
      // Gruppe: von den anderen Jahren nur das Mittel (sonst ein Gewirr grauer Linien)
      if (!anderesJahr(m) || selection.type === 'group' || !siteInAuswahl(m.siteIdx)) return;
      traceIdxListe.push(m.traceIdx); grauListe.push(grauFuer(m.year));
      vorjahrStyledTraceIdx.push({ traceIdx: m.traceIdx, color: siteColors[m.siteIdx] });
    });
    groupGrowthMeta.forEach(function(m) {
      if (!anderesJahr(m) || !gruppeGewaehlt(m.groupIdx)) return;
      traceIdxListe.push(m.traceIdx); grauListe.push(grauFuer(m.year));
      vorjahrStyledTraceIdx.push({ traceIdx: m.traceIdx, color: gruppenFarbe(m.groupIdx) });
    });
    if (traceIdxListe.length > 0) Plotly.restyle(el, { visible: true, 'line.color': grauListe, 'marker.color': grauListe }, traceIdxListe);
    if (alleJahre.indexOf(vorjahr) === -1) return;

    // Niederschlag-Vorjahresbalken: schmalere, graue Saeule fuer das Vorjahr,
    // sichtbar HINTER dem (bereits halbtransparenten, breiteren) Balken des
    // aktuellen Jahres - beide Traces liegen dank barmode=overlay und
    // aufsteigend sortierter Jahresreihenfolge bereits in der richtigen
    // Zeichenreihenfolge (Vorjahr zuerst angelegt = unten, aktuelles Jahr
    // danach = oben), es muss also nur noch Sichtbarkeit/Stil umgeschaltet
    // werden. Fehlerbalken (nur bei Gruppen-Niederschlag) werden fuer die
    // Vorjahres-Saeule ausgeblendet, um die Darstellung nicht zu ueberladen.
    if (precipOn) {
      var precipTraceIdxListe = [];
      sitePrecipMeta.forEach(function(m) {
        if (m.year !== vorjahr || !el.data[m.traceIdx] || el.data[m.traceIdx].type !== 'bar') return;
        if (!(selection.type === 'site' && m.siteIdx === selection.idx)) return;
        precipTraceIdxListe.push(m.traceIdx);
      });
      groupPrecipMeta.forEach(function(m) {
        if (m.year !== vorjahr || !el.data[m.traceIdx] || el.data[m.traceIdx].type !== 'bar') return;
        if (selection.type !== 'group' || m.groupIdx !== selection.idx) return;
        precipTraceIdxListe.push(m.traceIdx);
      });
      if (precipTraceIdxListe.length > 0) {
        Plotly.restyle(el,
          { visible: true, 'marker.color': 'rgba(90,90,90,0.9)', width: 0.35, 'error_y.visible': false },
          precipTraceIdxListe
        );
        vorjahrPrecipStyledTraceIdx = precipTraceIdxListe;
      }
    }
  }

  // Werte ausserhalb der Skala: die Achsen bleiben fest (150 kg TS/ha/Tag,
  // 70 mm/Woche, aus R), damit Kurven vergleichbar bleiben. Was darueber
  // liegt, schneidet Plotly am Rand ab; hier bekommt es ein Bruchzeichen
  // (gezackter Riss am oberen Rand) und den echten Wert als Schild. Ein Knopf
  // neben der Jahreswahl (Handy-Detail: im Kopf) passt die Skala an.
  var skalaFest = { y: el.layout.yaxis.range.slice(), y2: el.layout.yaxis2 ? el.layout.yaxis2.range.slice() : null };
  var skalaAuto = false;
  var skalaKnopf = null;
  function traceFarbe(t) {
    var f = t.type === 'bar' ? (t.marker && t.marker.color) : ((t.line && t.line.color) || (t.marker && t.marker.color));
    return typeof f === 'string' ? f : '#333';
  }
  function balkenBreite(t, k) {
    var w = Array.isArray(t.width) ? t.width[k] : t.width;
    return (typeof w === 'number' && isFinite(w)) ? w : 0.7;
  }
  // Sichtbare Punkte ueber der festen Skala: [{x, y, achse, farbe, balken}]
  function werteUeberSkala() {
    var liste = [];
    (el.data || []).forEach(function(t) {
      if (t.visible !== true || !t.y || !t.x) return;
      var achse = t.yaxis === 'y2' ? 'y2' : 'y';
      var grenze = skalaFest[achse] && skalaFest[achse][1];
      if (!grenze) return;
      for (var k = 0; k < t.y.length; k++) {
        var v = t.y[k];
        if (v !== null && isFinite(v) && v > grenze) liste.push({ x: t.x[k], y: v, achse: achse, farbe: traceFarbe(t), balken: t.type === 'bar', breite: balkenBreite(t, k) });
      }
    });
    return liste;
  }
  function maxSichtbar(achse) {
    var m = 0;
    (el.data || []).forEach(function(t) {
      if (t.visible !== true || !t.y || (t.yaxis === 'y2' ? 'y2' : 'y') !== achse) return;
      // nur die Werte selbst (Balken/Punkte), nicht die Spanne der
      // Fehlerbalken - sonst werden die Balken sehr flach
      for (var k = 0; k < t.y.length; k++) {
        var v = t.y[k];
        if (v !== null && isFinite(v) && v > m) m = v;
      }
    });
    return m;
  }
  function schoeneGrenze(v) {
    var schritt = v > 300 ? 50 : (v > 100 ? 20 : 10);
    return Math.ceil(v * 1.06 / schritt) * schritt;
  }
  // Bruchzeichen und Wertschilder fuer eine Plotflaeche von hPx x wPx
  // (Pixel) - auch fuer den Export (andere Groesse als am Bildschirm).
  function skalaMarkierungen(punkte, hPx, wPx, xBereich) {
    var shapes = [], annotations = [];
    if (!punkte.length || hPx < 40) return { shapes: shapes, annotations: annotations };
    var p = function(px) { return 1 - px / hPx; };
    var proWoche = wPx / Math.max(1, xBereich[1] - xBereich[0]);
    punkte.forEach(function(pt) {
      // halbe Breite in Wochen: Balken etwas breiter als der Balken, Linie
      // mind. 11 px; Riss 2-13 px unter dem Rand (zwei Zacken-Linien)
      var hw = Math.max(pt.balken ? pt.breite / 2 + 3 / proWoche : 0, 11 / proWoche), n = 4, oben = [], unten = [];
      for (var i = 0; i <= n; i++) {
        var xi = pt.x - hw + i * 2 * hw / n;
        oben.push(xi + ',' + p(i % 2 ? 7 : 2));
        unten.unshift(xi + ',' + p(i % 2 ? 13 : 8));
      }
      shapes.push({ type: 'path', xref: 'x', yref: 'paper', path: 'M' + oben.join('L') + 'L' + unten.join('L') + 'Z',
        fillcolor: 'white', line: { color: pt.farbe, width: 1.5 }, layer: 'above' });
    });
    // Schilder: je Folge benachbarter Wochen derselben Kurve nur der hoechste
    // Wert; ueberlappende Schilder weichen in bis zu drei Zeilen aus.
    var gruppen = [];
    punkte.slice().sort(function(a, b) { return a.farbe < b.farbe ? -1 : a.farbe > b.farbe ? 1 : (a.achse < b.achse ? -1 : a.achse > b.achse ? 1 : a.x - b.x); })
      .forEach(function(pt) {
        var g = gruppen[gruppen.length - 1];
        if (g && g.farbe === pt.farbe && g.achse === pt.achse && pt.x - g.letztesX <= 1) {
          g.letztesX = pt.x;
          if (pt.y > g.y) { g.y = pt.y; g.x = pt.x; }
        } else gruppen.push({ farbe: pt.farbe, achse: pt.achse, x: pt.x, y: pt.y, letztesX: pt.x });
      });
    var zeilen = [[], [], []];
    gruppen.sort(function(a, b) { return b.y - a.y; }).forEach(function(g) {
      var text = Math.round(g.y) + (g.achse === 'y2' ? ' mm' : '');
      var halb = (text.length * 6.5 + 22) / 2 / proWoche;
      for (var z = 0; z < zeilen.length; z++) {
        var frei = zeilen[z].every(function(b) { return g.x + halb < b[0] || g.x - halb > b[1]; });
        if (!frei) continue;
        zeilen[z].push([g.x - halb, g.x + halb]);
        annotations.push({ x: g.x, xref: 'x', y: 1, yref: 'paper', yanchor: 'top', yshift: -16 - z * 21, showarrow: false,
          text: '<span style="color:' + g.farbe + '">▲</span> <b>' + text + '</b>', font: { size: 11, color: '#222' },
          bgcolor: 'rgba(255,255,255,0.92)', bordercolor: g.farbe, borderwidth: 1.2, borderpad: 2,
          hovertext: 'Wert über der Skala: ' + text + (g.achse === 'y2' ? '' : ' kg TS/ha/Tag'), name: 'gw-skala' });
        return;
      }
    });
    return { shapes: shapes, annotations: annotations };
  }
  function aktualisiereSkala() {
    if (!el.layout || !el._fullLayout || !el._fullLayout._size) return;
    var punkte = werteUeberSkala();
    var neu = {};
    if (skalaAuto) {
      neu['yaxis.range'] = [0, Math.max(skalaFest.y[1], schoeneGrenze(maxSichtbar('y')))];
      if (skalaFest.y2) neu['yaxis2.range'] = [0, Math.max(skalaFest.y2[1], schoeneGrenze(maxSichtbar('y2')))];
    } else {
      neu['yaxis.range'] = skalaFest.y.slice();
      if (skalaFest.y2) neu['yaxis2.range'] = skalaFest.y2.slice();
    }
    var groesse = el._fullLayout._size;
    var m = skalaAuto ? { shapes: [], annotations: [] } : skalaMarkierungen(punkte, groesse.h, groesse.w, el.layout.xaxis.range || [0, 52]);
    neu.shapes = [(el.layout.shapes || [])[0]].filter(Boolean).concat(m.shapes);
    neu.annotations = (el.layout.annotations || []).filter(function(a) { return a.name !== 'gw-skala'; }).concat(m.annotations);
    Plotly.relayout(el, neu);
    zeigeSkalaKnopf(punkte.length);
  }
  function zeigeSkalaKnopf(anzahl) {
    if (!controls) return;
    if (!skalaKnopf) {
      skalaKnopf = document.createElement('button');
      skalaKnopf.type = 'button';
      skalaKnopf.className = 'gw-skala-knopf';
      skalaKnopf.addEventListener('click', function(evt) {
        evt.stopPropagation();
        skalaAuto = !skalaAuto;
        aktualisiereSkala();
        if (GWDatenexplorer.schreibeUrlBald) GWDatenexplorer.schreibeUrlBald();
      });
    }
    // Handy-Detail quer: Bedienelemente ausgeblendet, Knopf in den Kopf
    var quer = detailOffen && istMobil() && window.matchMedia('(orientation: landscape) and (max-height: 500px)').matches;
    var ort = quer ? detailKopf : controls;
    if (ort && skalaKnopf.parentNode !== ort) {
      if (ort === detailKopf && querKnopf) ort.insertBefore(skalaKnopf, querKnopf); else ort.appendChild(skalaKnopf);
    }
    var sichtbar = anzahl > 0;
    skalaKnopf.hidden = !sichtbar;
    if (!sichtbar) return;
    skalaKnopf.textContent = skalaAuto
      ? '↧ Feste Skala'
      : (istMobil() ? '↥ Skala anpassen (' + anzahl + ')' : '↥ ' + (anzahl === 1 ? '1 Wert' : anzahl + ' Werte') + ' über der Skala · anpassen');
    skalaKnopf.title = skalaAuto
      ? 'Zurück zur festen Skala (' + skalaFest.y[1] + ' kg TS/ha/Tag' + (skalaFest.y2 ? ', ' + skalaFest.y2[1] + ' mm' : '') + '), damit Kurven vergleichbar bleiben'
      : 'Skala so erweitern, dass alle Werte ganz sichtbar sind';
    skalaKnopf.classList.toggle('an', skalaAuto);
  }

  function applyXAxis() {
    if (GWDatenexplorer.schreibeUrlBald) GWDatenexplorer.schreibeUrlBald();
    var ticktext = datumOn ? datumTicktextJeJahr[selectedYear] : wochenTicktext;
    Plotly.relayout(el, {
      'xaxis.tickmode': 'array',
      'xaxis.tickvals': wochenTickvals,
      'xaxis.ticktext': ticktext,
      'xaxis.title.text': istAppModus() ? '' : (datumOn ? 'Datum (Montag der Woche)' : 'Kalenderwoche')
    });
  }

  function mapTraceIndexFor(jahr, week) {
    for (var i = 0; i < mapWochen.length; i++) {
      if (mapWochen[i].jahr === jahr && mapWochen[i].week === week) return i;
    }
    return -1;
  }

  function highlightArrayFor(idx) {
    var orts = mapPointOrts[idx];
    return orts.map(function(o) {
      var oi = siteNames.indexOf(o);
      var a = auswahlListen();
      var inSel = oi >= 0 && (a.sites.indexOf(oi) !== -1 || a.groups.some(function(g) { return siteVisible[g][oi]; }));
      return inSel ? 4 : 1;
    });
  }

  function applyMapState() {
    if (GWDatenexplorer.schreibeUrlBald) GWDatenexplorer.schreibeUrlBald();
    // Frisch abfragen statt einmalig cachen: beim allerersten Aufruf (aus
    // applyState() am Ende von onRender()) existiert das Karten-Widget
    // evtl. noch nicht im DOM, da beide Widgets (Kurve, Wachstumskarte)
    // unabhaengig voneinander gebunden werden und die Reihenfolge nicht
    // garantiert ist - eine einmalig gecachte (dann dauerhaft null
    // bleibende) Referenz wuerde die Karte nie mehr aktualisieren.
    var growthMapGd = document.querySelector('#datenexplorer-growthmap .js-plotly-plot');
    var idx = mapTraceIndexFor(selectedYear, selectedWeek);
    // Hover-Marker-Trace (inkl. "Tage seit Messung"-Farblegende) nur
    // sichtbar, wenn Graswachstum oder DGV eingeschaltet ist - sonst waeren
    // die Standorte trotz ausgeblendeten Bildern weiterhin geisterhaft
    // hoverbar bzw. die Legende ohne zugehoerige Ebene sichtbar.
    var vis = mapWochen.map(function(m, i) { return (graswachstumOn || afcOn) && i === idx; });
    if (growthMapGd) Plotly.restyle(growthMapGd, { visible: vis }, vis.map(function(v, i) { return i; }));
    if (growthMapGd && orteTraceIdx === null && orteAnzeigen && eigeneOrte.length) aktualisiereOrteSpur();
    if (idx >= 0 && growthMapGd) {
      var hl = highlightArrayFor(idx);
      Plotly.restyle(growthMapGd, { 'marker.line.width': [hl] }, [idx]);
    }
    // Standort-Filter auch per Klick auf einen Kartenpunkt (nur einmal
    // binden - growthMapGd wird bei jedem Aufruf neu abgefragt, s.o.).
    // mapPointOrts[curveNumber] ist die je Snapshot-Trace passende Liste
    // der Ort-Namen in Punktreihenfolge (siehe R: map_point_orts[[i]]),
    // curveNumber entspricht direkt dem Snapshot-Trace-Index, da fig_wachstum
    // pro (Jahr, Woche) genau EINE Trace in dieser Reihenfolge enthaelt.
    if (growthMapGd && !growthMapKlickGebunden) {
      growthMapGd.on('plotly_click', function(data) {
        if (!data.points || data.points.length === 0) return;
        // Ende einer Zwei-Finger-Geste, Doppeltipp oder Gedrueckthalten ist kein Tipp
        if (Date.now() - kartenGestenEnde < 500) return;
        standortGetipptZeit = Date.now();
        var p = data.points[0];
        var orte = mapPointOrts[p.curveNumber];
        var ort = orte ? orte[p.pointNumber] : null;
        var siteIdx = ort ? siteNames.indexOf(ort) : -1;
        if (siteIdx !== -1) {
          waehleSite(siteIdx);
          zeigeStandortBlatt(ort, siteIdx, p.customdata);
          return;
        }
        // Andere Punkte (MeteoSchweiz-Station, Suchmarker): auf Mobile gibt es
        // kein Hover - deren Text deshalb im Blatt zeigen.
        var text = p.text || p.hovertext;
        if (istTouch() && text) {
          var d = document.createElement('div'); d.className = 'gw-blatt-text'; d.innerHTML = text;
          zeigeBlatt('', '', d);
        }
      });
      growthMapKlickGebunden = true;
    }
    // Eigenes Tooltip-Modal statt Plotlys nativer (moeglicherweise
    // abgeschnittener) Hover-Box - siehe tooltipModalEl weiter oben.
    // Reagiert auf dieselben Punkte wie Plotlys eigenes Hover (Graswachstum-
    // Kreise, MeteoSchweiz-Stationen, Such-Fadenkreuz), zeigt aber deren
    // hovertext in einem fixen, garantiert vollstaendig sichtbaren Modal.
    if (growthMapGd && !growthMapHoverGebunden) {
      growthMapGd.on('plotly_hover', function(data) {
        if (istTouch()) return;
        if (!data.points || data.points.length === 0) return;
        var text = data.points[0].text || data.points[0].hovertext;
        if (!text) return;
        tooltipModalEl.innerHTML = text;
        tooltipModalEl.style.display = 'block';
      });
      growthMapGd.on('plotly_unhover', function() { tooltipModalEl.style.display = 'none'; });
      growthMapHoverGebunden = true;
    }
    // Cursor-Wertabfrage fuer die Hintergrund-Ebenen (siehe
    // verarbeiteKartenZeiger() weiter oben) - mousemove fuer Desktop,
    // touchmove/touchstart fuers Tippen auf Touch-Geraeten, mouseleave
    // setzt das Anzeigefeld zurueck.
    if (growthMapGd && !growthMapZeigerGebunden) {
      growthMapGd.addEventListener('mousemove', verarbeiteKartenZeiger);
      growthMapGd.addEventListener('mouseleave', versteckeWertAnzeige);
      bindeKartenGesten(growthMapGd);
      growthMapZeigerGebunden = true;
    }
    if (weekLabel) {
      weekLabel.innerHTML = '';
      var kwZeile = document.createElement('div');
      kwZeile.textContent = 'KW ' + selectedWeek + ' ' + selectedYear;
      var datumZeile = document.createElement('div');
      datumZeile.className = 'gw-slider-datum';
      datumZeile.textContent = wochenDatumBereich[selectedYear + ' ' + selectedWeek] || '';
      weekLabel.appendChild(kwZeile);
      weekLabel.appendChild(datumZeile);
    }
    Plotly.relayout(el, { 'shapes[0].x0': selectedWeek, 'shapes[0].x1': selectedWeek });
    aktualisiereHintergrundEbene();
    aktualisiereLayerLabels();
    aktualisiereLayerLegende();
    aktualisiereAfcLegende();
    aktualisiereTageSeitMessungLegende();
    aktualisiereSmnStationen();
  }

  // Haengt an die Labels der Fenster-Ebenen (siehe meteoFensterEbenen) das
  // Symbol (Σ Summe / ⌀ Mittel) und die aktuelle Fenstergroesse an (z.B.
  // Niederschlagssumme Sigma 28d) und ergaenzt das tatsaechliche Datum des
  // Bodenwasserbilanz-Snapshots im Radio-Label (kann je nach Verfuegbarkeit
  // vom Wochenbeginn abweichen, siehe Kommentar bei bodenwasser_datum_je_
  // woche/R) - ausserhalb von aktualisiereHintergrundEbene() aufgerufen,
  // damit die Labels auch dann aktuell bleiben, wenn die Wachstumskarte
  // selbst noch nicht gebunden ist.
  function aktualisiereLayerLabels() {
    meteoFensterEbenen.forEach(function(name) {
      var radio = radioJeEbene[name];
      if (!radio || !radio.labelTextEl) return;
      var info = layerLegenden[name];
      var symbol = info.symbol === 'sum' ? 'Σ' : '⌀';
      // Nur die AKTUELL gewaehlte Ebene zeigt den live am Schieberegler
      // eingestellten Wert - alle anderen (nicht ausgewaehlten) Fenster-
      // Ebenen zeigen weiterhin ihren eigenen Standard (meteoFensterStandard),
      // da genau DAS beim Auswaehlen tatsaechlich passieren wuerde (der
      // Schieberegler springt ja bei jedem Ebenenwechsel auf den Standard
      // der neuen Ebene zurueck) - sonst waere hier voruebergehend ein
      // Wert zu sehen, der beim Klick gar nicht eintritt.
      var tage = (name === hintergrundEbene) ? meteoFenster : meteoFensterStandard[name];
      radio.labelTextEl.textContent = info.label + ' ' + symbol + ' ' + tage + 'd';
    });
    // Potenzielles Wachstum: NUR "(berechnet)"-Hinweis, OHNE eigenes Datum
    // im Label (anders als boden) - das tatsaechliche Datenstand-Datum kommt
    // hier ausschliesslich ueber den allgemeinen "Stand ..."-Mechanismus im
    // Kartentitel (aktualisiereKartentitel(), ueber werte.bis), sonst
    // entstuende dieselbe Doppel-Datum-Anzeige, die dort fuer boden extra
    // behoben werden musste.
    if (radioWachstumspotenzialRate && radioWachstumspotenzialRate.labelTextEl) {
      radioWachstumspotenzialRate.labelTextEl.textContent = layerLegenden.wachstumspotenzial_rate.label + ' (experimentell)';
    }
    if (radioWachstumspotenzialKum && radioWachstumspotenzialKum.labelTextEl) {
      radioWachstumspotenzialKum.labelTextEl.textContent = layerLegenden.wachstumspotenzial_kum.label + ' (experimentell)';
    }
    if (!radioBoden || !radioBoden.labelTextEl) return;
    // bodenCache.datum existiert erst, NACHDEM die Ebene einmal geladen
    // wurde (siehe ladeEbene()) - bis dahin steht im Label schlicht kein
    // Datum, statt die Ebene allein fuer dieses Label vorzuladen.
    var bodenCache = ebenenCache.boden;
    var datum = bodenCache && bodenCache.datum && bodenCache.datum[selectedYear + ' ' + selectedWeek];
    radioBoden.labelTextEl.textContent = layerLegenden.boden.label + ' (berechnet)' + (datum ? ' ' + datum : '');
  }

  // Optionale Hintergrund-Ebenen (Niederschlag Vorwoche / Bodenwasserbilanz
  // Wochenbeginn) - vorgerenderte PNGs je (Jahr, Woche), siehe R-Code
  // (raster_zu_datauri()). Nur eine Ebene gleichzeitig aktiv, standardmaessig
  // keine. Das Kantone/Seen-Hintergrundbild (kartenbildHintergrund) ist
  // IMMER die unterste Bild-Ebene; eine gewaehlte Niederschlags-/
  // Bodenwasserbilanz-Ebene wird als zweites, halbtransparentes Bild
  // darueber gelegt (Plotly zeichnet layout.images in Array-Reihenfolge).
  // Zeichnet die Bild-Ebenen der Karte (Kantone/Seen-Basis, optionale
  // Hintergrund-Ebene, AFC-Ring, Graswachstum-Kreis) - 'bild' ist entweder
  // das bereits geladene Bild der optionalen Ebene oder null (keine Ebene
  // gewaehlt, oder deren Daten werden gerade erst nachgeladen).
  function zeichneKartenBilder(bild) {
    var growthMapGd = document.querySelector('#datenexplorer-growthmap .js-plotly-plot');
    if (!growthMapGd) return;
    var schluessel = selectedYear + ' ' + selectedWeek;
    var basisBilder = bild ? [kartenbildHintergrund, bild] : [kartenbildHintergrund];
    // AFC-Ring und Graswachstum-Kreis liegen IMMER ueber der Kartenbasis/
    // optionalen Hintergrund-Ebene, unabhaengig von deren Auswahl - jede
    // Ebene einzeln per eigenem Schalter (Ebenen-Kasten) ein-/ausblendbar.
    // Reihenfolge wichtig: AFC-Ring zuerst, Graswachstum-Kreis darueber
    // (deckt sonst den Ring-Innenbereich zu, wie im urspruenglichen
    // kombinierten Bild).
    var zusatzBilder = [];
    if (afcOn && afcRingBilder[schluessel]) zusatzBilder.push(afcRingBilder[schluessel]);
    if (graswachstumOn && graswachstumBilder[schluessel]) zusatzBilder.push(graswachstumBilder[schluessel]);
    var alleBilder = basisBilder.concat(zusatzBilder);
    Plotly.relayout(growthMapGd, { images: alleBilder });
  }

  // Optionale Hintergrund-Ebenen werden erst bei Bedarf nachgeladen (siehe
  // ladeEbene() oben) - beim allerersten Auswaehlen einer noch nicht
  // zwischengespeicherten Ebene zeigt die Karte kurz KEINE Ebene (statt der
  // vorherigen, jetzt nicht mehr passenden), bis die Datei eingetroffen ist.
  // ebeneBeimStart wird nach Abschluss der Anfrage GEGENGEPRUEFT: haben
  // Nutzer inzwischen eine andere Ebene gewaehlt, wird das (jetzt veraltete)
  // Ergebnis verworfen statt faelschlich angezeigt.
  function aktualisiereHintergrundEbene() {
    if (GWDatenexplorer.schreibeUrlBald) GWDatenexplorer.schreibeUrlBald();
    if (hintergrundEbene === 'keine') { zeichneKartenBilder(null); return; }
    var ebeneBeimStart = hintergrundEbene;
    var dateiSchluesselBeimStart = ebeneDateiSchluessel(ebeneBeimStart);
    if (!ebenenCache[dateiSchluesselBeimStart]) zeichneKartenBilder(null);
    ladeEbene(dateiSchluesselBeimStart, function(daten) {
      // aktualisiereLayerLabels() unabhaengig vom Noch-aktuell-Check unten
      // aufgerufen: das Bodenwasserbilanz-Datum im Radio-Label soll auch
      // dann erscheinen, wenn zwischenzeitlich eine ANDERE Ebene gewaehlt
      // wurde - das Label selbst blendet sich ja nur ein, waehrend boden
      // ausgewaehlt ist, ist also nie faelschlich sichtbar.
      if (ebeneBeimStart === 'boden') aktualisiereLayerLabels();
      // Vergleich ueber den Datei-Schluessel (nicht nur den Ebenennamen):
      // bei Fenster-Ebenen zaehlt auch ein zwischenzeitlicher Wechsel der
      // Fenstergroesse (Schieberegler) als nicht mehr aktuell.
      if (ebeneDateiSchluessel(hintergrundEbene) !== dateiSchluesselBeimStart) return;
      // Voller Neuaufbau statt nur ladeHinweisEl auszublenden: erst jetzt
      // (Ebene fertig geladen) laesst sich beurteilen, ob die AKTUELLE
      // Woche tatsaechlich Daten hat oder nicht (siehe keinDatenHinweisEl
      // in aktualisiereLayerLegende()).
      aktualisiereLayerLegende();
      zeichneKartenBilder(daten.bilder[selectedYear + ' ' + selectedWeek]);
    });
  }

  // MeteoSchweiz-Stationen: einzige, von Jahr/Woche unabhaengige Trace
  // (smnStationenTraceIdx) - Position/Name/Kanton/Hoehe sind bereits beim
  // Seitenaufbau in R gebacken (smnStationenMeta), die taeglich aktuellen
  // Messwerte holt ladeSmnAktuellwerte() unten aber SELBST per fetch() -
  // beim ERSTEN Einschalten je Seitenaufruf (danach zwischengespeichert,
  // ein erneutes Ein-/Ausschalten loest keinen neuen Download aus).
  function aktualisiereSmnStationen() {
    if (GWDatenexplorer.schreibeUrlBald) GWDatenexplorer.schreibeUrlBald();
    var growthMapGd = document.querySelector('#datenexplorer-growthmap .js-plotly-plot');
    if (!growthMapGd) return;
    Plotly.restyle(growthMapGd, { visible: smnStationenOn }, [smnStationenTraceIdx]);
    if (smnStationenOn && !smnWerteGeladen && !smnLaedt) ladeSmnAktuellwerte(growthMapGd);
  }

  // Parst die letzte Datenzeile einer MeteoSchweiz-Tages-CSV (Semikolon-
  // getrennt, keine Quotes/Escapes in diesen Dateien - einfaches split()
  // genuegt) - Entsprechung zu lade_smn_aktuellwert()/R, nur eben im
  // Browser statt beim R-Lauf ausgefuehrt.
  function smnZeileParsen(csvText) {
    var zeilen = csvText.replace(/\r/g, '').split('\n').filter(function(z) { return z.length > 0; });
    if (zeilen.length < 2) return null;
    var header = zeilen[0].split(';');
    var letzte = zeilen[zeilen.length - 1].split(';');
    var idx = {};
    header.forEach(function(h, i) { idx[h] = i; });
    function feld(name) {
      var i = idx[name];
      if (i === undefined) return NaN;
      return parseFloat(letzte[i]);
    }
    return {
      reference_timestamp: letzte[idx.reference_timestamp] || '',
      tre200d0: feld('tre200d0'), tso005d0: feld('tso005d0'), tso010d0: feld('tso010d0'),
      tso020d0: feld('tso020d0'), rre150d0: feld('rre150d0'), gre000d0: feld('gre000d0'), sre000d0: feld('sre000d0')
    };
  }

  // Baut den Hovertext fuer eine Station - identischer Aufbau/Wortlaut wie
  // zuvor in R (siehe Git-Historie von smn_daten), nur die Formatierung
  // (fmt1/fmt0) hier eben in JS statt formatC()/ifelse().
  function smnHoverBauen(meta, werte) {
    function fmt1(x) { return isNaN(x) ? '-' : x.toFixed(1); }
    function fmt0(x) { return isNaN(x) ? 'keine Daten' : Math.round(x).toString(); }
    var tsoZeile = (isNaN(werte.tso005d0) && isNaN(werte.tso010d0) && isNaN(werte.tso020d0))
      ? 'keine Daten'
      : (fmt1(werte.tso005d0) + ' / ' + fmt1(werte.tso010d0) + ' / ' + fmt1(werte.tso020d0) + ' °C');
    return '<b>' + meta.name + '</b> (' + meta.kanton + ', ' + meta.hoehe + ' m ü. M.)' +
      '<br>Lufttemperatur (Tagesmittel): ' + (isNaN(werte.tre200d0) ? 'keine Daten' : fmt1(werte.tre200d0) + ' °C') +
      '<br>Bodentemperatur 5/10/20cm: ' + tsoZeile +
      '<br>Niederschlag (Vortag): ' + (isNaN(werte.rre150d0) ? 'keine Daten' : fmt1(werte.rre150d0) + ' mm') +
      '<br>Globalstrahlung (Tagesmittel): ' + (isNaN(werte.gre000d0) ? 'keine Daten' : fmt0(werte.gre000d0) + ' W/m²') +
      '<br>Sonnenscheindauer: ' + (isNaN(werte.sre000d0) ? 'keine Daten' : fmt0(werte.sre000d0) + ' Min') +
      '<br>Stand: ' + werte.reference_timestamp;
  }

  // Holt die aktuellen Tageswerte fuer ALLE Stationen parallel direkt vom
  // MeteoSchweiz-Open-Data-Server (CORS-freigegeben) und ersetzt den
  // Platzhalter-Hovertext per EINEM restyle() sobald alle Anfragen fertig
  // sind (einzelne fehlgeschlagene Stationen behalten ihren Platzhalter -
  // kein Abbruch der uebrigen).
  function ladeSmnAktuellwerte(growthMapGd) {
    smnLaedt = true;
    var hovertext = smnStationenMeta.map(function(s) {
      return '<b>' + s.name + '</b> (' + s.kanton + ', ' + s.hoehe + ' m ü. M.)<br>Lädt aktuelle Werte...';
    });
    var anfragen = smnStationenMeta.map(function(s, i) {
      var url = smnBasisUrl + s.abbr.toLowerCase() + '/ogd-smn_' + s.abbr.toLowerCase() + '_d_recent.csv';
      return fetch(url).then(function(r) { return r.ok ? r.text() : null; }).then(function(text) {
        var werte = text ? smnZeileParsen(text) : null;
        if (werte) hovertext[i] = smnHoverBauen(s, werte);
      }).catch(function() { /* einzelne Station fehlgeschlagen - Platzhaltertext bleibt stehen */ });
    });
    Promise.all(anfragen).then(function() {
      Plotly.restyle(growthMapGd, { hovertext: [hovertext] }, [smnStationenTraceIdx]);
      smnWerteGeladen = true;
      smnLaedt = false;
    });
  }

  // Styles --------------------------------------------------------------
  // Stile: frontend/datenexplorer.css

  // Blatt: gemeinsames Panel fuer Standort-Kennzahlen und (auf Mobile) die
  // Erklaerungstexte der i-Knoepfe. Auf dem Handy ein Bottom Sheet (immer
  // bildschirmbreit, kann nicht am Rand abgeschnitten werden, die Karte
  // bleibt oben sichtbar), auf dem Desktop eine Karte unten rechts.
  // Mobile-Layout (<= 700px): Kopfzeile statt Plotly-Titel, randlose Karte,
  // Legende/Wert in einer Leiste unter der Karte, Ebenen und Kurve hinter
  // Knoepfen - Karte, Wert und Wochenumschalter passen ohne Scrollen.
  // Stile: frontend/datenexplorer.css

  // Handy: schmal oder (Querformat) niedrig mit Touch
  function istMobil() { return window.matchMedia('(max-width: 700px), (max-height: 500px) and (pointer: coarse)').matches; }
  // Touch ohne Maus (auch Tablets ueber 700px): kein Hover - Erklaerungen und
  // Punkttexte deshalb im Blatt statt als Tooltip.
  function istTouch() { return istMobil() || window.matchMedia('(hover: none)').matches; }
  function istAppModus() { return !!(seiteEl && seiteEl.classList.contains('gw-app')); }
  var blattEl = document.createElement('div');
  blattEl.className = 'gw-blatt';
  blattEl.style.display = 'none';
  document.body.appendChild(blattEl);
  var blattGeoeffnetUm = 0;
  blattEl.addEventListener('click', function(evt) { evt.stopPropagation(); });
  function schliesseBlatt() { blattEl.style.display = 'none'; }
  function zeigeBlatt(titel, unter, inhalt) {
    blattEl.innerHTML = '';
    var griff = document.createElement('div'); griff.className = 'gw-blatt-griff'; blattEl.appendChild(griff);
    var kopf = document.createElement('div'); kopf.className = 'gw-blatt-kopf';
    var t = document.createElement('div'); t.className = 'gw-blatt-titel'; t.textContent = titel || '';
    var zu = document.createElement('button'); zu.type = 'button'; zu.className = 'gw-blatt-zu'; zu.textContent = '×';
    zu.setAttribute('aria-label', 'Schliessen');
    zu.addEventListener('click', schliesseBlatt);
    kopf.appendChild(t); kopf.appendChild(zu); blattEl.appendChild(kopf);
    if (unter) { var u = document.createElement('div'); u.className = 'gw-blatt-unter'; u.textContent = unter; blattEl.appendChild(u); }
    if (inhalt) blattEl.appendChild(inhalt);
    blattEl.style.display = 'block';
    blattEl.scrollTop = 0;
    blattGeoeffnetUm = Date.now();
  }
  document.addEventListener('keydown', function(evt) {
    if (evt.key !== 'Escape') return;
    schliesseBlatt();
    schliesseDetail();
    document.body.classList.remove('gw-ebenen-offen');
    if (istAppModus() && seiteEl.classList.contains('gw-app-schmal')) setzeEbenenOffen(false);
  });
  // Klick/Tipp ausserhalb schliesst Blatt und Ebenen-Blatt - kurz nach dem
  // Oeffnen ignoriert, weil der oeffnende Klick (z.B. auf einen Kartenpunkt)
  // selbst noch bis zum document hochblubbert.
  document.addEventListener('click', function(evt) {
    if (Date.now() - blattGeoeffnetUm < 400) return;
    schliesseBlatt();
    var panel = document.getElementById('datenexplorer-map-controls');
    if (document.body.classList.contains('gw-ebenen-offen') && panel && !panel.contains(evt.target)) {
      document.body.classList.remove('gw-ebenen-offen');
    }
    if (istAppModus() && seiteEl.classList.contains('gw-app-schmal') && !seiteEl.classList.contains('gw-ebenen-zu') &&
        panel && !panel.contains(evt.target)) {
      setzeEbenenOffen(false);
    }
  });

  // Icons (Tabler, MIT) als SVG per DOM - im R-String keine Anfuehrungszeichen
  var GW_ICON_PFADE = {
    ebenen: ['M12 4l-8 4l8 4l8 -4l-8 -4', 'M4 12l8 4l8 -4', 'M4 16l8 4l8 -4'],
    kurve: ['M4 19l16 0', 'M4 15l4 -6l4 2l4 -5l4 4'],
    vollbild: ['M16 4l4 0l0 4', 'M14 10l6 -6', 'M8 20l-4 0l0 -4', 'M4 20l6 -6', 'M16 20l4 0l0 -4', 'M14 14l6 6', 'M8 4l-4 0l0 4', 'M4 4l6 6'],
    verkleinern: ['M5 9l4 0l0 -4', 'M3 3l6 6', 'M5 15l4 0l0 4', 'M3 21l6 -6', 'M19 9l-4 0l0 -4', 'M15 9l6 -6', 'M19 15l-4 0l0 4', 'M15 15l6 6'],
    hoch: ['M6 15l6 -6l6 6'],
    export: ['M4 17v2a2 2 0 0 0 2 2h12a2 2 0 0 0 2 -2v-2', 'M7 11l5 5l5 -5', 'M12 4l0 12'],
    hilfe: ['M12 12m-9 0a9 9 0 1 0 18 0a9 9 0 1 0 -18 0', 'M12 17l0 .01', 'M12 13.5a1.5 1.5 0 0 1 1 -1.5a2.6 2.6 0 1 0 -3 -4'],
    runter: ['M6 9l6 6l6 -6'],
    zurueck: ['M5 12l14 0', 'M5 12l6 6', 'M5 12l6 -6'],
    // Griff wie in Grafikprogrammen (Tabler grip-horizontal): zwei Reihen Punkte
    griff: ['M5 9m-1 0a1 1 0 1 0 2 0a1 1 0 1 0 -2 0', 'M5 15m-1 0a1 1 0 1 0 2 0a1 1 0 1 0 -2 0', 'M12 9m-1 0a1 1 0 1 0 2 0a1 1 0 1 0 -2 0',
      'M12 15m-1 0a1 1 0 1 0 2 0a1 1 0 1 0 -2 0', 'M19 9m-1 0a1 1 0 1 0 2 0a1 1 0 1 0 -2 0', 'M19 15m-1 0a1 1 0 1 0 2 0a1 1 0 1 0 -2 0'],
    drehen: ['M10 3h4a1 1 0 0 1 1 1v16a1 1 0 0 1 -1 1h-4a1 1 0 0 1 -1 -1v-16a1 1 0 0 1 1 -1z', 'M17 7a4 4 0 0 1 4 4', 'M19 9l2 2l2 -2'],
    play: ['M7 4v16l13 -8z'],
    pause: ['M6 5m0 1a1 1 0 0 1 1 -1h2a1 1 0 0 1 1 1v12a1 1 0 0 1 -1 1h-2a1 1 0 0 1 -1 -1z', 'M14 5m0 1a1 1 0 0 1 1 -1h2a1 1 0 0 1 1 1v12a1 1 0 0 1 -1 1h-2a1 1 0 0 1 -1 -1z']
  };
  function gwIcon(name) {
    var ns = 'http://www.w3.org/2000/svg';
    var svg = document.createElementNS(ns, 'svg');
    var attr = { viewBox: '0 0 24 24', width: '18', height: '18', fill: 'none', stroke: 'currentColor',
      'stroke-width': '2', 'stroke-linecap': 'round', 'stroke-linejoin': 'round', 'aria-hidden': 'true', 'class': 'gw-icon' };
    Object.keys(attr).forEach(function(k) { svg.setAttribute(k, attr[k]); });
    GW_ICON_PFADE[name].forEach(function(d) { var pf = document.createElementNS(ns, 'path'); pf.setAttribute('d', d); svg.appendChild(pf); });
    return svg;
  }
  function setzeKnopfInhalt(knopf, icon, text) {
    knopf.innerHTML = '';
    knopf.appendChild(gwIcon(icon));
    var sp = document.createElement('span'); sp.textContent = text; knopf.appendChild(sp);
    knopf.setAttribute('aria-label', text);
  }

  // ---------- Hilfe und Dokumentation ----------
  // Ein Fenster fuer alle Erklaerungen: Menue, Suche (Tippfehler, Umlaute,
  // Wortteile, Synonyme) und je Thema Text plus Kopie des zugehoerigen
  // Legendenelements. Die i-Knoepfe oeffnen es beim passenden Thema.
  var dokuExperimentell = new URLSearchParams(window.location.search).has('experimentell');
  var DOKU_GRUPPEN = ['Erste Schritte', 'Messnetz', 'Wetter-Ebenen', 'Berechnete Ebenen', 'Daten und Quellen', 'Weitere'];
  var doku = [];
  function dokuQuelle(schluessel) {
    return function() { return (layerLegenden[schluessel] && layerLegenden[schluessel].quelle) ? 'Quelle: ' + layerLegenden[schluessel].quelle : ''; };
  }
  function dokuEl(tag, klasse, text) {
    var e = document.createElement(tag);
    if (klasse) e.className = klasse;
    if (text !== undefined) e.textContent = text;
    return e;
  }
  function dokuLinien(eintraege) {
    var box = dokuEl('div', 'gw-doku-linien');
    eintraege.forEach(function(e) {
      var z = dokuEl('span', 'gw-doku-linie');
      var sw = dokuEl('span', 'gw-legend-swatch');
      if (e[2] === 'balken') { sw.style.borderTopWidth = '8px'; sw.style.width = '10px'; sw.style.borderTopColor = e[1]; }
      else { sw.style.borderTopColor = e[1]; sw.style.borderTopStyle = e[2]; }
      z.appendChild(sw); z.appendChild(document.createTextNode(e[0]));
      box.appendChild(z);
    });
    return box;
  }
  function dokuKlon(knoten) {
    if (!knoten || !knoten.childNodes.length) return null;
    var c = knoten.cloneNode(true);
    c.style.display = '';
    c.classList.remove('gw-afc-legende-box');
    return c;
  }
  function dokuFarbskala(schluessel) {
    return function() {
      var info = layerLegenden[schluessel];
      if (!info) return null;
      if (hintergrundEbene === schluessel && layerLegendeBox && layerLegendeBox.childNodes.length) return dokuKlon(layerLegendeBox);
      var box = dokuEl('div', 'gw-layer-legende');
      var wrap = dokuEl('div', 'gw-layer-legende-balken-wrap');
      var b = dokuEl('div', 'gw-layer-legende-balken');
      b.style.background = 'linear-gradient(to right,' + info.farben.join(',') + ')';
      wrap.appendChild(b);
      var skala = dokuEl('div', 'gw-layer-legende-skala');
      skala.appendChild(dokuEl('span', '', String(info.bereich[0])));
      skala.appendChild(dokuEl('span', '', info.bereich[1] + ' ' + info.einheit));
      box.appendChild(wrap); box.appendChild(skala);
      return box;
    };
  }
  function dokuEintrag(e) { e.alias = e.alias || []; doku.push(e); return e; }

  dokuEintrag({ id: 'ueberblick', gruppe: 'Erste Schritte', titel: 'Über den Datenexplorer',
    stichworte: 'start einstieg hilfe anleitung graswachstum.ch agff',
    text: ['Der Datenexplorer zeigt die Graswachstumsmessungen des AGFF-Messnetzes Woche für Woche: auf der Karte als Zahl im Kreis je Standort, darunter als Wachstumskurve über die Saison. Ältere Jahre lassen sich zum Vergleich wählen.',
      'Als Hintergrund der Karte können Wetter- und Bodendaten von MeteoSchweiz eingeblendet werden, z. B. Niederschlag, Temperatur oder die berechnete Bodenwasserbilanz. Die Daten werden jede Nacht aktualisiert.',
      'Im Menü links finden Sie alle Themen, oben die Suche. Die kleinen i-Knöpfe neben den Ebenen öffnen dieses Fenster direkt beim passenden Thema.'] });
  dokuEintrag({ id: 'karte', gruppe: 'Erste Schritte', titel: 'Karte bedienen',
    stichworte: 'zoom vergroessern verschieben standort antippen klicken plz ort suche fadenkreuz wert cursor standortblatt zwei finger pinch doppeltipp gedrueckt halten lupe koordinaten touch handy',
    text: ['Am Computer: vergrössern mit dem Mausrad, einen Ausschnitt durch Aufziehen eines Rechtecks; das Haus-Symbol der Werkzeugleiste zeigt wieder die ganze Schweiz. Über die ganze Schweiz hinaus lässt sich nicht verkleinern.',
      'Auf Handy und Tablet wie bei Karten-Apps: mit zwei Fingern zoomen und verschieben, doppelt tippen vergrössert. Mit einem Finger scrollt die Seite weiter. Kurz auf einen Standort tippen öffnet sein Blatt; daneben tippen oder den Finger gedrückt halten (und ziehen) zeigt den Wert der Hintergrund-Ebene mit Ort und Koordinaten – beim Gedrückthalten in einer Lupe über dem Finger. Das Fadenkreuz bleibt danach einige Sekunden stehen.',
      'Ein Klick auf einen Standort öffnet das Standortblatt mit dem letzten Messwert, dem DGV, dem Zielbereich der Woche und einer kleinen Saisonkurve. Von dort führt «Ganze Graswachstumskurve anzeigen» zur grossen Kurve dieses Standorts.',
      'Das Feld «PLZ oder Ort suchen» bei den Ebenen setzt ein Fadenkreuz auf den Ort. Ist eine Hintergrund-Ebene aktiv, zeigt der Datenexplorer deren Wert an dieser Stelle (auf dem Desktop auch laufend unter dem Mauszeiger).'] });
  dokuEintrag({ id: 'zeitleiste', gruppe: 'Erste Schritte', titel: 'Zeitleiste und Abspielen',
    alias: ['Zeitleiste'], stichworte: 'kalenderwoche kw woche schieberegler heute pfeile play abspielen animation zukunft',
    text: ['Die Zeitleiste wählt die Kalenderwoche für Karte und Kurve: mit dem Schieberegler, den Pfeilen (eine Woche zurück oder vor) oder «Heute» für die aktuelle Woche. Der grau hinterlegte Teil liegt in der Zukunft.',
      'Der Abspielen-Knopf zeigt Woche für Woche bis zur letzten verfügbaren Woche. Steht der Regler schon am Ende, beginnt er bei der ersten Woche mit Messungen. Jede andere Bedienung der Zeitleiste hält das Abspielen an.',
      'In der Wachstumskurve wählt auch ein Klick auf die Achse unter der Grafik die Woche.'] });
  dokuEintrag({ id: 'kurve', gruppe: 'Erste Schritte', titel: 'Wachstumskurve',
    alias: ['Wachstumskurve', 'Graswachstumskurve'], stichworte: 'kurve linie mittel durchschnitt mittelland vorjahr niederschlag balken gruppe region hoehenlage jahr legende vergleichen kombinieren mehrere auswahl mehrfachauswahl skala achse ausreisser abgeschnitten ueber maximum anpassen',
    legende: function() { return dokuLinien([['Standort (je eigene Farbe)', '#1D9E75', 'solid'], ['Mittleres Wachstum der Auswahl', 'black', 'dashed'], ['Durchschnitt Mittelland, langjährig', 'red', 'dotted'], ['Andere Jahre zum Vergleich (Vorjahr dunkler)', 'rgba(120,120,120,0.9)', 'solid'], ['Niederschlag pro Woche', 'steelblue', 'balken']]); },
    text: ['Die Kurve zeigt das gemessene Graswachstum in kg TS/ha/Tag über die Saison. Im Auswahlfeld oben links lassen sich Standorte und Gruppen (alle Standorte, Region West/Mitte/Ost, Höhenlagen) suchen und beliebig kombinieren: Antippen setzt oder entfernt ein Häkchen, gewählte Einträge stehen als Kärtchen im Feld und lassen sich mit × entfernen. Daneben wählen Sie das Jahr.',
      'Eine einzelne Gruppe zeigt ihre Standorte und das Gruppenmittel, ein einzelner Standort seine Kurve mit Niederschlag. Bei Kombinationen erscheint je gewählter Standort eine Kurve und je Gruppe das Mittel (gestrichelt, bei mehreren Gruppen in eigenen Farben); Niederschlag wird dann nicht gezeigt. «Auswahl zurücksetzen» am Ende der Liste führt zurück zu allen Standorten.',
      'Rechts steht die Legende. Ein Klick auf einen Standort zeigt nur noch diesen. Die Schalter darüber blenden Niederschlag (Balken, mm pro Woche) und die Kurven der anderen Jahre (grau, das Vorjahr dunkler; das Jahr steht beim Darüberfahren) ein, oder stellen die Achse von Kalenderwochen auf Datum um.',
      'Beim Start erscheinen nur Standorte, die regelmässig messen (siehe dort). Die übrigen lassen sich in der Legende dazuschalten.',
      'Die Skala ist fest (Wachstum bis 150 kg TS/ha/Tag, Niederschlag bis 70 mm pro Woche), damit Jahre und Standorte vergleichbar bleiben. Höhere Werte laufen oben aus der Grafik: ein gezackter Riss am oberen Rand und ein Schild mit dem echten Wert (▲ 312) markieren sie. «… über der Skala · anpassen» oben links erweitert die Skala, bis alle Werte ganz sichtbar sind; «Feste Skala» stellt sie zurück.'] });
  dokuEintrag({ id: 'regelmaessig', gruppe: 'Erste Schritte', titel: 'Regelmässig messende Standorte',
    stichworte: 'startansicht filter weitere selten gemessen standorte acht wochen',
    text: ['Damit die Kurve beim Start übersichtlich bleibt, zeigt sie in der Gruppenansicht nur Standorte mit mindestens 8 Kalenderwochen mit Messung im gewählten Jahr. Das Mittel der Gruppe (schwarz gestrichelt) wird weiterhin aus allen Standorten berechnet.',
      'In der Legende blendet «+ … weitere (selten gemessen)» die übrigen Standorte mit Daten im Jahr ein, «Nur regelmässig messende Standorte» blendet sie wieder aus.'] });
  dokuEintrag({ id: 'ansicht', gruppe: 'Erste Schritte', titel: 'Ansicht anpassen (Desktop)',
    alias: ['Grösse von Karte und Kurve verschieben'], stichworte: 'griff ziehen schieben klicken zuklappen aufklappen pfeile gross klein vollbild ebenen einklappen layout tastatur',
    text: ['Zwischen Karte und Kurve liegt ein Griff (die Lasche mit den Punkten): Ziehen verschiebt die Grenze, ein Klick ohne Ziehen klappt die Kurve zu, ein weiterer Klick wieder auf. Der Pfeil nach oben macht die Kurve ganz gross, der Pfeil nach unten die Karte. Mit der Tastatur: Griff anwählen, Enter klappt zu oder auf, Pfeiltasten, Pos1 oder Ende verschieben.',
      'Die Icon-Leiste links blendet die Ebenen und die Kurve ein und aus. Auf der Website vergrössert «Vollbild» den Datenexplorer auf den ganzen Bildschirm; Escape oder der Knopf beenden das Vollbild.'] });
  dokuEintrag({ id: 'handy', gruppe: 'Erste Schritte', titel: 'Auf dem Handy',
    stichworte: 'mobile smartphone teaser detail querformat quer drehen zurueck geste regionen',
    legende: function() { return dokuLinien([['Region West', '#378ADD', 'solid'], ['Region Mitte', '#1D9E75', 'solid'], ['Region Ost', '#BA7517', 'solid'], ['Langjähriges Mittel', '#E24B4A', 'dotted']]); },
    text: ['Unter der Zeitleiste zeigt eine flache Kurve das Wachstum der drei Regionen, über drei Wochen geglättet, und das langjährige Mittel. Antippen, «Alle Kurven» oder «Standort wählen …» öffnet die grosse Kurve als eigene Ansicht; die Zurück-Geste des Handys schliesst sie wieder.',
      'Im Hochformat blinkt «Quer ansehen». Wo das Handy es erlaubt, dreht der Knopf die Ansicht; sonst bitte das Handy quer halten. Bei eingeschalteter automatischer Drehung erscheint die Grafik im Querformat über den ganzen Bildschirm.'] });

  dokuEintrag({ id: 'eigene-orte', gruppe: 'Erste Schritte', titel: 'Eigene Orte und Karten für Veranstaltungen',
    alias: ['Eigene Orte', 'Karte für eine Veranstaltung'], stichworte: 'betriebe plz ortschaft liste marker veranstaltung kurs tagung pdf svg html qr jahresrueckblick vergleich teilen',
    text: ['Unter «Export» › «Karte für eine Veranstaltung» lassen sich eigene Orte erfassen, z.B. die Betriebe der Teilnehmenden: PLZ oder Ortschaft eintippen und einen Vorschlag wählen, oder eine ganze Liste mit Komma oder Zeilenumbruch einfügen. Ein Klick auf einen Namen ändert ihn (z.B. in den Betriebsnamen). Der Schalter «Eigene Orte» zeigt sie als Marker auf der Karte; die Liste bleibt in diesem Browser gespeichert.',
      '«Karten erstellen» macht je Thema (Niederschlag seit 1. Januar und der letzten 28 Tage, Bodenwasserbilanz, potenzielles Graswachstum) eine Seite mit Kartenausschnitt auf der Landeskarte, nummerierten Orten, Legende und Wertetabelle; wo Platz bleibt, erklärt ein kurzer Text, wie die Werte entstehen. Als PDF zum Drucken, als SVG zum Weiterbearbeiten oder als HTML-Datei für Präsentation und Weitergabe: dort lassen sich die Themen umschalten, die Orte antippen und ein Jahresrückblick zeigt den Verlauf an Ihren Orten im Vergleich zur ganzen Schweiz.',
      'Auf jeder Seite führt ein QR-Code zur selben Ansicht im Datenexplorer, mit allen Orten. Der Code enthält einen Kurzlink (graswachstum.ch/g/…), damit er auch klein gedruckt gut scanbar bleibt. Die Orte stehen im Link; wer ihn erhält, sieht sie.',
      'Die Farbskala ist an den Kartenausschnitt angepasst, damit Unterschiede zwischen den Orten sichtbar werden (wahlweise «wie im Datenexplorer»). In der HTML-Fassung lassen sich Tiefst- und Höchstwert mit den Pfeilen unter dem Zahlenstrahl verschieben; beim Niederschlag gibt es dort einen Umschalter: seit 1. Januar, letzte 28 Tage oder ein frei gewählter Zeitraum (von KW bis KW). Der Jahresrückblick zeigt zusätzlich die Wasserbilanz je Woche (Niederschlag minus Verdunstung); jedes Diagramm lässt sich im Vollbild zeigen.',
      '«Bericht als Link teilen» erzeugt einen Kurzlink auf den Bericht selbst: Wer ihn öffnet, sieht die interaktive Ansicht mit allen Orten sofort, ohne Datei und stets mit den aktuellen Daten. Dort lässt sich der Bericht auch als HTML-Datei speichern.'] });
  dokuEintrag({ id: 'graswachstum', gruppe: 'Messnetz', titel: 'Graswachstum',
    alias: ['Graswachstum (kg TS/ha/Tag)'], stichworte: 'zuwachs wachstum kreis zahl messung kg ts ha tag',
    legende: function() { return dokuKlon(tageSeitMessungBox); },
    text: ['Die Zahl im Kreis zeigt das zuletzt gemessene Graswachstum in kg TS/ha/Tag, also den Zuwachs an Trockensubstanz pro Hektare und Tag.',
      'Die Graufärbung des Kreises zeigt, wie lange die Messung zurückliegt: weiss = frisch gemessen, dunkelgrau = bis 14 Tage alt. Standorte ohne Messung in den letzten 14 Tagen erscheinen nicht.'] });
  dokuEintrag({ id: 'dgv', gruppe: 'Messnetz', titel: 'DGV (Grasvorrat)',
    alias: ['DGV (kg TS/ha)', 'DGV'], stichworte: 'afc average farm cover grasvorrat vorrat ring zielbereich weide futter',
    legende: function() {
      var k = dokuKlon(afcLegendeBox);
      if (!k) return null;
      var box = dokuEl('div', 'gw-doku-dgv');
      box.appendChild(k);
      box.appendChild(dokuEl('p', 'gw-doku-legende-text', 'Oben liegen 0 und 1500 kg TS/ha (Strich), der Ring füllt sich im Uhrzeigersinn. Der Doppelpfeil zeigt den Zielbereich der gewählten Woche: ' +
        afcLegendeBox.dataset.zielLow + '–' + afcLegendeBox.dataset.zielHigh + ' kg TS/ha (die beiden Zahlen am Ring).'));
      return box;
    },
    text: ['DGV (Durchschnittlicher GrasVorrat, international AFC = Average Farm Cover) schätzt den aktuellen Grasvorrat des Betriebs in kg Trockensubstanz pro Hektare.',
      'Der Ring zeigt den Vorrat auf einer Skala von 0 bis 1500 kg TS/ha und färbt ihn nach dem Zielbereich der Jahreszeit: rot = deutlich zu wenig (unter 200 kg praktisch leer), grün = im Zielbereich, blaugrün = deutlich mehr als nötig. Der Doppelpfeil markiert den Zielbereich der gewählten Woche.',
      'Der Zielbereich verschiebt sich übers Jahr, etwa Frühling 500–700, Sommer 700–800, Herbst 900–1200 kg TS/ha.'] });
  dokuEintrag({ id: 'tage', gruppe: 'Messnetz', titel: 'Tage seit Messung',
    alias: ['Tage seit Messung (Graswachstum/DGV)'], stichworte: 'alter grau graufaerbung aktualitaet frisch',
    legende: function() { return dokuKlon(tageSeitMessungBox); },
    text: ['Kreis und Ring der Standorte werden umso dunkler, je älter die letzte Messung ist: weiss am Messtag, dunkelgrau nach 14 Tagen. Danach verschwindet der Standort von der Karte, bis wieder eine Messung eintrifft.'] });
  dokuEintrag({ id: 'stationen', gruppe: 'Messnetz', titel: 'MeteoSchweiz-Stationen',
    alias: ['MeteoSchweiz-Stationen'], stichworte: 'swissmetnet station wetterstation diamant temperatur bodentemperatur niederschlag strahlung sonnenschein tageswerte',
    text: ['Zeigt die öffentlichen Automatikstationen von MeteoSchweiz (SwissMetNet) mit ihren neuesten Tageswerten: Luft- und Bodentemperatur, Niederschlag, Globalstrahlung und Sonnenscheindauer.',
      'Die Stationen sind eine reine Wetter-Referenz, unabhängig von der gewählten Woche und nicht Teil der AGFF-Messungen. Bodentemperatur messen nur ein Teil der rund 150 Stationen.'] });

  dokuEintrag({ id: 'zeitraum', gruppe: 'Wetter-Ebenen', titel: 'Zeitraum der Wetter-Ebenen',
    alias: ['Zeitraum'], stichworte: 'fenster tage schieberegler summe mittel stichtag',
    text: ['Bei Niederschlag, Temperatur, Bodentemperatur, Sonnenschein und Verdunstung bestimmt der Schieberegler «Zeitraum», über wie viele Tage vor dem Stichtag summiert oder gemittelt wird (z. B. 7 oder 28 Tage). Stichtag ist jeweils der Montag der gewählten Woche.'] });
  dokuEintrag({ id: 'niederschlag', gruppe: 'Wetter-Ebenen', titel: 'Niederschlagssumme',
    alias: ['Niederschlagssumme'], stichworte: 'regen regenmenge niederschlag mm nass rhiresd',
    legende: dokuFarbskala('niederschlag'), quelle: dokuQuelle('niederschlag'),
    text: ['Summe des Niederschlags im gewählten Zeitraum vor dem Stichtag, als flächendeckendes Raster von MeteoSchweiz. Zusammen mit Verdunstung und Bodenwasserbilanz zeigt sie, ob das Wachstum durch Wassermangel gebremst sein könnte.'] });
  dokuEintrag({ id: 'temperatur', gruppe: 'Wetter-Ebenen', titel: 'Temperatur 2m',
    alias: ['Temperatur 2m'], stichworte: 'lufttemperatur waerme hitze kaelte grad basistemperatur tabsd',
    legende: dokuFarbskala('temperatur'), quelle: dokuQuelle('temperatur'),
    text: ['Mittlere Lufttemperatur (2 m über Boden) im gewählten Zeitraum vor dem Stichtag.',
      'Gras wächst erst ab etwa 5 °C spürbar, das Optimum liegt bei etwa 15–20 °C. Über etwa 25 °C bremst Hitzestress das Wachstum auch bei genügend Wasser.'] });
  dokuEintrag({ id: 'bodentemperatur', gruppe: 'Wetter-Ebenen', titel: 'Bodentemperatur (Schätzung)',
    alias: ['Bodentemperatur'], stichworte: 'boden temperatur schaetzung vegetationsbeginn mineralisierung stickstoff',
    legende: dokuFarbskala('bodentemperatur'), quelle: dokuQuelle('bodentemperatur'),
    text: ['Achtung, Schätzung und keine Messung: MeteoSchweiz misst die Bodentemperatur nur an einzelnen Stationen. Gezeigt wird deshalb das gleitende Mittel der Lufttemperatur im gewählten Zeitraum, als grobe Näherung an die trägere oberste Bodenschicht (etwa 5–10 cm). Ein längerer Zeitraum entspricht einer stärkeren Dämpfung.',
      'Die Bodentemperatur ist wichtig für den Vegetationsbeginn im Frühling und die Stickstoff-Mineralisierung; beides kommt unter etwa 5–8 °C weitgehend zum Erliegen.'] });
  dokuEintrag({ id: 'sonnenschein', gruppe: 'Wetter-Ebenen', titel: 'Sonnenscheindauer',
    alias: ['Sonnenscheindauer'], stichworte: 'sonne strahlung licht photosynthese srel',
    legende: dokuFarbskala('sonnenschein'), quelle: dokuQuelle('sonnenschein'),
    text: ['Sonnenscheindauer im gewählten Zeitraum, relativ zur astronomisch möglichen Dauer (0–100 %). Mehr Sonne treibt die Photosynthese an, erhöht aber auch die Verdunstung.',
      'MeteoSchweiz bereitet diese Daten mit ein bis zwei Monaten Verzögerung auf; die neuesten Wochen fehlen deshalb oft noch.'] });
  dokuEintrag({ id: 'et0', gruppe: 'Wetter-Ebenen', titel: 'Verdunstung ET0',
    alias: ['Verdunstung ET0'], stichworte: 'verdunstung evapotranspiration hargreaves trockenstress wasserverbrauch',
    legende: dokuFarbskala('et0'), quelle: dokuQuelle('et0'),
    text: ['Potenzielle Verdunstung (Evapotranspiration) nach Hargreaves (FAO-56), als Summe im gewählten Zeitraum. Sie zeigt, wie viel Wasser dem Boden allein durch Verdunstung entzogen wird; hohe Werte bei wenig Niederschlag begünstigen Trockenstress. Dieselbe Berechnung fliesst in die Bodenwasserbilanz ein.'] });
  dokuEintrag({ id: 'niederschlag_kum', gruppe: 'Wetter-Ebenen', titel: 'Niederschlag seit 1. Januar',
    alias: ['Niederschlag seit 1. Januar'], stichworte: 'niederschlag regen jahressumme jahr summe kumuliert seit januar',
    legende: dokuFarbskala('niederschlag_kum'), quelle: dokuQuelle('niederschlag_kum'),
    text: ['Gesamter Niederschlag des gewählten Jahres vom 1. Januar bis zum Stichtag (Montag der gewählten Woche), aus den täglichen MeteoSchweiz-Gitterdaten aufsummiert. Zeigt, wie nass oder trocken das Jahr bisher verlaufen ist; für die letzten Wochen allein eignet sich die Ebene «Niederschlagssumme».'] });
  dokuEintrag({ id: 'gdd', gruppe: 'Wetter-Ebenen', titel: 'Wachstumsgradtage',
    alias: ['Wachstumsgradtage'], stichworte: 'gdd gradtage waermesumme temperatursumme vegetation',
    legende: dokuFarbskala('gdd'), quelle: dokuQuelle('gdd'),
    text: ['Aufsummierte Wärme seit Jahresbeginn: an jedem Tag die Tagesmitteltemperatur minus 5 °C, sofern positiv. Eine verbreitete Faustregel für die pflanzenverfügbare Wärme seit Vegetationsbeginn; höhere Werte bedeuten mehr angesammelte Wachstumsbedingungen.'] });

  dokuEintrag({ id: 'boden', gruppe: 'Berechnete Ebenen', titel: 'Bodenwasserbilanz',
    alias: ['Bodenwasserbilanz'], stichworte: 'bodenwasser wasser eimer bucket trocken trockenheit duerre fuellstand speicher',
    legende: dokuFarbskala('boden'), quelle: dokuQuelle('boden'),
    text: ['Der Boden wird vereinfacht als Eimer betrachtet: Regen füllt ihn, Verdunstung leert ihn, ist er voll, läuft der Überschuss ab. Ein feuchter Boden verdunstet mehr als ein bereits trockener.',
      'Der Wert zeigt den Füllstand am Stichtag: 130 mm = gut mit Wasser versorgt, 0 mm = ausgetrocknet. Die Ebene ist selbst berechnet und keine Messung.'] });
  if (dokuExperimentell) {
    dokuEintrag({ id: 'potenzial', gruppe: 'Berechnete Ebenen', titel: 'Potenzielles Wachstum (experimentell)',
      alias: ['Potenzielles Wachstum'], stichworte: 'modvege growr modell potenzial erholung experimentell kumuliert',
      legende: dokuFarbskala('wachstumspotenzial_rate'), quelle: dokuQuelle('wachstumspotenzial_rate'),
      text: ['Experimentell: wie viel Graswachstum Temperatur, Strahlung und Wasserhaushalt diese Woche zulassen würden, ohne Nährstoffmangel und ohne Schnitt oder Beweidung. Berechnet mit ModVege (Jouven et al. 2006, R-Paket growR).',
        'Bekannte Schwächen: Nach einer Trockenperiode springt das Modell bei Regen sofort auf das volle Potenzial; die Erholungsverzögerung dämpft das nur grob. Grundwasserböden werden nicht abgebildet.'] });
  }

  dokuEintrag({ id: 'quellen', gruppe: 'Daten und Quellen', titel: 'Datenquellen',
    stichworte: 'quelle daten agff meteoschweiz open data swisstopo lizenz aktualisierung',
    text: ['Graswachstum und DGV: Messungen der Betriebe im Messnetz Graswachstum der AGFF (graswachstum.ch).',
      'Wetter: MeteoSchweiz, Open Data – Gitterdaten für Niederschlag, Temperatur und Sonnenschein sowie die Tageswerte der SwissMetNet-Stationen. Ortssuche: swisstopo. Bodenwasserbilanz und Verdunstung sind daraus berechnet.',
      'Die Seite wird jede Nacht neu erzeugt. Fehlen bei einer Ebene die neuesten Wochen, sind die Daten bei der Quelle noch nicht verfügbar.'] });

  // Synonyme fuer die Suche (normalisiert, siehe dokuNorm)
  var DOKU_SYNONYME = {
    afc: ['dgv', 'grasvorrat'], vorrat: ['dgv', 'grasvorrat'], futter: ['dgv', 'graswachstum'],
    regen: ['niederschlag'], nass: ['niederschlag'], niederschlag: ['regen'],
    verdunstung: ['et0', 'evapotranspiration'], et: ['et0'], trocken: ['bodenwasser', 'trockenstress', 'trockenheit'],
    duerre: ['trockenheit', 'bodenwasser'], wasser: ['bodenwasser', 'niederschlag'],
    hitze: ['temperatur'], kaelte: ['temperatur'], waerme: ['temperatur', 'wachstumsgradtage'], warm: ['temperatur'],
    sonne: ['sonnenschein'], licht: ['sonnenschein'], gdd: ['wachstumsgradtage'], gradtage: ['wachstumsgradtage'],
    zuwachs: ['graswachstum'], wachstum: ['graswachstum', 'wachstumskurve'], station: ['stationen'],
    play: ['abspielen'], animation: ['abspielen'], abspielen: ['play'], woche: ['kalenderwoche', 'zeitleiste'], kw: ['kalenderwoche'],
    handy: ['smartphone', 'mobile'], smartphone: ['handy'], quer: ['querformat'], drehen: ['querformat', 'quer'],
    suche: ['suchen', 'plz'], zoom: ['vergroessern'], gross: ['vollbild', 'griff'], legende: ['linie', 'farbe']
  };
  function dokuNorm(s) {
    return String(s || '').toLowerCase().replace(/ä/g, 'ae').replace(/ö/g, 'oe').replace(/ü/g, 'ue').replace(/ß/g, 'ss')
      .replace(/[éèê]/g, 'e').replace(/[àâ]/g, 'a').replace(/ç/g, 'c');
  }
  function dokuWoerter(s) { return dokuNorm(s).split(/[^a-z0-9]+/).filter(Boolean); }
  function dokuAbstand(a, b, max) {
    if (Math.abs(a.length - b.length) > max) return max + 1;
    var vor = [], i, j;
    for (j = 0; j <= b.length; j++) vor[j] = j;
    for (i = 1; i <= a.length; i++) {
      var akt = [i], best = i;
      for (j = 1; j <= b.length; j++) {
        akt[j] = Math.min(vor[j] + 1, akt[j - 1] + 1, vor[j - 1] + (a[i - 1] === b[j - 1] ? 0 : 1));
        if (akt[j] < best) best = akt[j];
      }
      if (best > max) return max + 1;
      vor = akt;
    }
    return vor[b.length];
  }
  function dokuIndex(e) {
    if (e._idx) return e._idx;
    var text = e.text.join(' ');
    e._idx = {
      titelW: dokuWoerter(e.titel), titelN: dokuNorm(e.titel),
      aliasW: dokuWoerter(e.alias.join(' ') + ' ' + (e.stichworte || '')),
      textW: dokuWoerter(text), textN: dokuNorm(text)
    };
    return e._idx;
  }
  // Treffer je Suchwort: Wortanfang im Titel > Stichwort > Wortteil im Titel >
  // Wortanfang im Text > Wortteil im Text > Tippfehler. Alle Wörter müssen
  // passen; Synonyme zählen etwas weniger.
  function dokuBewerte(e, woerter) {
    var ix = dokuIndex(e), summe = 0, markier = [];
    for (var w = 0; w < woerter.length; w++) {
      var t = woerter[w], varianten = [[t, 1]];
      (DOKU_SYNONYME[t] || []).forEach(function(s) { varianten.push([s, 0.7]); });
      var best = 0;
      varianten.forEach(function(v) {
        var q = v[0], g = v[1], p = 0, toleranz = q.length >= 8 ? 2 : (q.length >= 4 ? 1 : 0);
        if (ix.titelW.some(function(x) { return x.indexOf(q) === 0; })) p = 10;
        else if (q.length >= 3 && ix.titelN.indexOf(q) !== -1) p = 8;
        else if (ix.aliasW.some(function(x) { return x.indexOf(q) === 0; })) p = 7;
        else if (ix.textW.some(function(x) { return x.indexOf(q) === 0; })) p = 3;
        else if (q.length >= 4 && ix.textN.indexOf(q) !== -1) p = 2;
        else if (toleranz) {
          var fuzzy = function(x) { return x.length >= 3 && dokuAbstand(q, x.slice(0, Math.max(q.length, Math.min(x.length, q.length + 1))), toleranz) <= toleranz; };
          var tf = ix.titelW.filter(fuzzy), af = ix.aliasW.filter(fuzzy), xf = ix.textW.filter(fuzzy);
          if (tf.length) { p = 4; markier = markier.concat(tf); }
          else if (af.length) { p = 3; }
          else if (xf.length) { p = 1.5; markier = markier.concat(xf); }
        }
        if (p > 0) markier.push(q);
        if (p * g > best) best = p * g;
      });
      if (best === 0) return null;
      summe += best;
    }
    return { e: e, punkte: summe, markier: markier };
  }
  function dokuSuche(text) {
    var woerter = dokuWoerter(text);
    if (!woerter.length) return null;
    return doku.map(function(e) { return dokuBewerte(e, woerter); }).filter(Boolean)
      .sort(function(a, b) { return b.punkte - a.punkte; });
  }
  // Text mit markierten Treffern als DOM (keine HTML-Strings)
  function dokuMarkiert(text, markier) {
    var frag = document.createDocumentFragment();
    if (!markier || !markier.length) { frag.appendChild(document.createTextNode(text)); return frag; }
    String(text).split(/([A-Za-z0-9ÄÖÜäöüßéèêàâç]+)/).forEach(function(teil) {
      if (!teil) return;
      var n = dokuNorm(teil);
      var treffer = /[a-z0-9]/.test(n) && markier.some(function(q) { return n.indexOf(q) === 0 || (q.length >= 4 && n.indexOf(q) !== -1) || n === q; });
      if (treffer) frag.appendChild(dokuEl('mark', '', teil));
      else frag.appendChild(document.createTextNode(teil));
    });
    return frag;
  }
  function dokuAuszug(e, markier) {
    var text = e.text.join(' ');
    var n = dokuNorm(text), pos = -1;
    markier.forEach(function(q) { var p = n.indexOf(q); if (p !== -1 && (pos === -1 || p < pos)) pos = p; });
    if (pos === -1) return text.slice(0, 90) + (text.length > 90 ? ' …' : '');
    var a = Math.max(0, pos - 35);
    return (a > 0 ? '… ' : '') + text.slice(a, a + 100) + (a + 100 < text.length ? ' …' : '');
  }

  var dokuRoot = null, dokuNavEl, dokuInhaltEl, dokuSucheEl, dokuAktiv = null, dokuOpener = null, dokuMarkierAktiv = [];
  function dokuFinde(label) {
    var n = dokuNorm(label), best = null, bestLaenge = -1;
    doku.forEach(function(e) {
      [e.titel].concat(e.alias).forEach(function(a) {
        var an = dokuNorm(a);
        if (n === an && an.length > bestLaenge + 1000) return;
        if (n === an) { best = e; bestLaenge = 1e6; }
        else if (n.indexOf(an) === 0 && an.length > bestLaenge && bestLaenge < 1e6) { best = e; bestLaenge = an.length; }
      });
    });
    return best;
  }
  function dokuNeu(titel, text, legende) {
    return dokuEintrag({ id: 'weitere-' + doku.length, gruppe: 'Weitere', titel: titel || 'Erklärung', text: [text || ''], legende: legende || null });
  }
  function baueDoku() {
    if (dokuRoot) return;
    dokuRoot = dokuEl('div', 'gw-doku');
    dokuRoot.setAttribute('role', 'dialog');
    dokuRoot.setAttribute('aria-modal', 'true');
    dokuRoot.setAttribute('aria-labelledby', 'gw-doku-titel');
    dokuRoot.addEventListener('click', function(evt) { evt.stopPropagation(); if (evt.target === dokuRoot) schliesseDoku(); });
    var fenster = dokuEl('div', 'gw-doku-fenster');
    var kopf = dokuEl('div', 'gw-doku-kopf');
    var h = dokuEl('h2', 'gw-doku-h', 'Hilfe');
    h.id = 'gw-doku-titel';
    var menue = dokuEl('button', 'gw-doku-menue', 'Inhalt');
    menue.type = 'button';
    menue.addEventListener('click', function() { dokuRoot.classList.remove('gw-doku-ergebnis'); dokuRoot.classList.toggle('gw-doku-nav-offen'); });
    dokuSucheEl = dokuEl('input', 'gw-doku-suche');
    dokuSucheEl.type = 'search';
    dokuSucheEl.placeholder = 'Suchen, z. B. Regen, DGV, abspielen …';
    dokuSucheEl.setAttribute('aria-label', 'Dokumentation durchsuchen');
    dokuSucheEl.addEventListener('input', function() { dokuRoot.classList.remove('gw-doku-ergebnis'); zeichneDokuNav(); });
    dokuSucheEl.addEventListener('keydown', function(evt) {
      var links = Array.prototype.slice.call(dokuNavEl.querySelectorAll('.gw-doku-link'));
      var i = links.findIndex(function(l) { return l.classList.contains('aktiv'); });
      if (evt.key === 'ArrowDown' || evt.key === 'ArrowUp') {
        evt.preventDefault();
        if (!links.length) return;
        var n = evt.key === 'ArrowDown' ? Math.min(links.length - 1, i + 1) : Math.max(0, i - 1);
        links[n].click();
        dokuSucheEl.focus();
      } else if (evt.key === 'Enter' && links.length) {
        evt.preventDefault();
        (links[Math.max(0, i)]).click();
        dokuRoot.classList.remove('gw-doku-nav-offen');
      }
    });
    var zu = dokuEl('button', 'gw-doku-zu', String.fromCharCode(215));
    zu.type = 'button';
    zu.setAttribute('aria-label', 'Hilfe schliessen');
    zu.addEventListener('click', schliesseDoku);
    kopf.appendChild(h); kopf.appendChild(menue); kopf.appendChild(dokuSucheEl); kopf.appendChild(zu);
    var rumpf = dokuEl('div', 'gw-doku-rumpf');
    dokuNavEl = dokuEl('nav', 'gw-doku-nav');
    dokuNavEl.setAttribute('aria-label', 'Themen');
    dokuInhaltEl = dokuEl('article', 'gw-doku-inhalt');
    rumpf.appendChild(dokuNavEl); rumpf.appendChild(dokuInhaltEl);
    fenster.appendChild(kopf); fenster.appendChild(rumpf);
    dokuRoot.appendChild(fenster);
    dokuRoot.addEventListener('keydown', function(evt) {
      if (evt.key !== 'Escape') return;
      evt.stopPropagation();
      if (dokuSucheEl.value) { dokuSucheEl.value = ''; zeichneDokuNav(); dokuSucheEl.focus(); }
      else schliesseDoku();
    });
    document.body.appendChild(dokuRoot);
  }
  function zeichneDokuNav() {
    dokuNavEl.innerHTML = '';
    var treffer = dokuSuche(dokuSucheEl.value);
    var link = function(e, auszug, markier) {
      var a = dokuEl('button', 'gw-doku-link');
      a.type = 'button';
      a.appendChild(dokuMarkiert(e.titel, markier));
      if (auszug) { var s = dokuEl('span', 'gw-doku-auszug'); s.appendChild(dokuMarkiert(auszug, markier)); a.appendChild(s); }
      if (dokuAktiv === e.id) a.classList.add('aktiv');
      a.addEventListener('click', function() {
        dokuMarkierAktiv = markier || [];
        zeigeDokuEintrag(e.id);
        // Handy: Liste schliessen, Text zeigen
        dokuRoot.classList.remove('gw-doku-nav-offen');
        dokuRoot.classList.add('gw-doku-ergebnis');
      });
      return a;
    };
    if (treffer) {
      dokuRoot.classList.add('gw-doku-sucht');
      var kopf = dokuEl('div', 'gw-doku-gruppe', treffer.length ? treffer.length + (treffer.length === 1 ? ' Treffer' : ' Treffer') : 'Keine Treffer');
      dokuNavEl.appendChild(kopf);
      if (!treffer.length) dokuNavEl.appendChild(dokuEl('p', 'gw-doku-leer', 'Andere Begriffe versuchen, z. B. Regen, Temperatur, Kurve oder Handy.'));
      treffer.forEach(function(t) { dokuNavEl.appendChild(link(t.e, dokuAuszug(t.e, t.markier), t.markier)); });
      if (treffer.length) { dokuMarkierAktiv = treffer[0].markier; zeigeDokuEintrag(treffer[0].e.id, true); }
      return;
    }
    dokuRoot.classList.remove('gw-doku-sucht');
    dokuMarkierAktiv = [];
    DOKU_GRUPPEN.forEach(function(g) {
      var eintraege = doku.filter(function(e) { return e.gruppe === g; });
      if (!eintraege.length) return;
      dokuNavEl.appendChild(dokuEl('div', 'gw-doku-gruppe', g));
      eintraege.forEach(function(e) { dokuNavEl.appendChild(link(e)); });
    });
  }
  function zeigeDokuEintrag(id, ausSuche) {
    var i = doku.findIndex(function(e) { return e.id === id; });
    if (i === -1) i = 0;
    var e = doku[i];
    dokuAktiv = e.id;
    dokuNavEl.querySelectorAll('.gw-doku-link').forEach(function(l, k) { l.classList.remove('aktiv'); });
    var aktivLink = Array.prototype.slice.call(dokuNavEl.querySelectorAll('.gw-doku-link')).filter(function(l) { return l.firstChild && l.textContent.indexOf(e.titel) === 0; })[0];
    if (aktivLink) { aktivLink.classList.add('aktiv'); if (aktivLink.scrollIntoView) aktivLink.scrollIntoView({ block: 'nearest' }); }
    dokuInhaltEl.innerHTML = '';
    dokuInhaltEl.appendChild(dokuEl('div', 'gw-doku-pfad', e.gruppe));
    var t = dokuEl('h3', 'gw-doku-thema'); t.appendChild(dokuMarkiert(e.titel, dokuMarkierAktiv)); dokuInhaltEl.appendChild(t);
    var leg = null;
    try { leg = e.legende ? e.legende() : null; } catch (err) { leg = null; }
    if (leg) { var lw = dokuEl('div', 'gw-doku-legende'); lw.appendChild(leg); dokuInhaltEl.appendChild(lw); }
    e.text.forEach(function(absatz) { var p = dokuEl('p'); p.appendChild(dokuMarkiert(absatz, dokuMarkierAktiv)); dokuInhaltEl.appendChild(p); });
    var q = typeof e.quelle === 'function' ? e.quelle() : (e.quelle || '');
    if (q) dokuInhaltEl.appendChild(dokuEl('p', 'gw-doku-quelle', q));
    var nav = dokuEl('div', 'gw-doku-blaettern');
    [[i - 1, 'zurueck', 'Zurück: '], [i + 1, 'weiter', 'Weiter: ']].forEach(function(v) {
      var ziel = doku[v[0]];
      var b = dokuEl('button', 'gw-doku-blatt-knopf gw-doku-' + v[1], ziel ? v[2] + ziel.titel : '');
      b.type = 'button';
      if (!ziel) { b.style.visibility = 'hidden'; } else b.addEventListener('click', function() { dokuMarkierAktiv = []; zeigeDokuEintrag(ziel.id); });
      nav.appendChild(b);
    });
    dokuInhaltEl.appendChild(nav);
    if (!ausSuche) dokuInhaltEl.scrollTop = 0;
    if (GWDatenexplorer.schreibeUrlBald) GWDatenexplorer.schreibeUrlBald();
  }
  function istDokuOffen() { return !!(dokuRoot && dokuRoot.classList.contains('offen')); }
  function oeffneDoku(id, opener) {
    baueDoku();
    dokuOpener = opener || document.activeElement;
    dokuSucheEl.value = '';
    dokuMarkierAktiv = [];
    dokuAktiv = id || 'ueberblick';
    zeichneDokuNav();
    zeigeDokuEintrag(dokuAktiv);
    dokuRoot.classList.remove('gw-doku-nav-offen');
    dokuRoot.classList.add('offen');
    document.documentElement.classList.add('gw-doku-offen');
    setTimeout(function() { (id ? dokuRoot.querySelector('.gw-doku-zu') : dokuSucheEl).focus(); }, 0);
  }
  function schliesseDoku() {
    if (!dokuRoot) return;
    dokuRoot.classList.remove('offen');
    document.documentElement.classList.remove('gw-doku-offen');
    if (GWDatenexplorer.schreibeUrlBald) GWDatenexplorer.schreibeUrlBald();
    if (dokuOpener && dokuOpener.focus) dokuOpener.focus();
  }

  // Eingebettet (Grav-Plugin datenexplorer, siehe Datenexplorer_einbettung.json):
  // Schrift und Akzentfarbe der Website, Vollbild nur auf Wunsch.
  var seiteEl = document.getElementById('gw-seite');
  if (eingebettet && seiteEl) {
    seiteEl.classList.add('gw-eingebettet');
    var wurzel = document.documentElement;
    var schrift = getComputedStyle(seiteEl.parentElement || document.body).fontFamily;
    wurzel.style.setProperty('--gw-schrift', schrift);
    var akzent = getComputedStyle(wurzel).getPropertyValue('--custom-color-primary').trim();
    if (akzent) {
      wurzel.style.setProperty('--gw-akzent', akzent);
      wurzel.style.setProperty('--gw-akzent-dunkel', getComputedStyle(wurzel).getPropertyValue('--custom-color-primary-darker').trim() || akzent);
      wurzel.style.setProperty('--gw-akzent-hell', 'color-mix(in srgb, ' + akzent + ' 14%, white)');
    }
    setTimeout(function() {
      document.querySelectorAll('#gw-seite .js-plotly-plot').forEach(function(g) {
        try { Plotly.relayout(g, { 'font.family': schrift }); } catch (e) {}
      });
    }, 0);
  }
  // Eigene Seite: Logo als Symbol im Browser-Tab (Pfad aus dem Stylesheet)
  if (!eingebettet && !document.querySelector('link[rel~=icon]')) {
    var css = Array.prototype.slice.call(document.querySelectorAll('link[rel=stylesheet]')).filter(function(l) { return /gw-datenexplorer[^/]*\/datenexplorer\.css/.test(l.href); })[0];
    if (css) {
      var ico = document.createElement('link');
      ico.rel = 'icon'; ico.type = 'image/svg+xml';
      ico.href = css.href.replace(/datenexplorer\.css(\?.*)?$/, 'logo-graswachstum.svg');
      document.head.appendChild(ico);
    }
  }
  var vollbildKnopfEl = null;
  function istVollbild() { return !!(seiteEl && seiteEl.classList.contains('gw-vollbild')); }
  function setzeVollbild(an) {
    if (!seiteEl) return;
    seiteEl.classList.toggle('gw-vollbild', an);
    document.documentElement.classList.toggle('gw-vollbild-aktiv', an);
    if (vollbildKnopfEl) setzeKnopfInhalt(vollbildKnopfEl, an ? 'verkleinern' : 'vollbild', an ? 'Vollbild beenden' : 'Vollbild');
    bestimmeAppModus();
    setzeBlockHoehe();
    if (kurveAnteil !== null) setTimeout(function() { setzeKurvenHoehe(kurveAnteil * kurveMax()); }, 0);
    if (!an) seiteEl.scrollIntoView({ block: 'start' });
    window.dispatchEvent(new Event('resize'));
  }
  if (seiteEl) {
    var werkzeugleiste = document.createElement('div');
    werkzeugleiste.className = 'gw-werkzeugleiste';
    var hilfeKnopf = document.createElement('button');
    hilfeKnopf.type = 'button'; hilfeKnopf.className = 'gw-werkzeug-knopf';
    setzeKnopfInhalt(hilfeKnopf, 'hilfe', 'Hilfe');
    hilfeKnopf.addEventListener('click', function(evt) { evt.stopPropagation(); oeffneDoku(null, hilfeKnopf); });
    werkzeugleiste.appendChild(hilfeKnopf);
    var exportKnopf = document.createElement('button');
    exportKnopf.type = 'button'; exportKnopf.className = 'gw-werkzeug-knopf';
    setzeKnopfInhalt(exportKnopf, 'export', 'Export');
    exportKnopf.addEventListener('click', function(evt) { evt.stopPropagation(); if (istExportOffen()) schliesseExport(); else oeffneExport(exportKnopf); });
    werkzeugleiste.appendChild(exportKnopf);
    if (eingebettet) {
      vollbildKnopfEl = document.createElement('button');
      vollbildKnopfEl.type = 'button'; vollbildKnopfEl.className = 'gw-werkzeug-knopf';
      setzeKnopfInhalt(vollbildKnopfEl, 'vollbild', 'Vollbild');
      vollbildKnopfEl.addEventListener('click', function(evt) { evt.stopPropagation(); setzeVollbild(!istVollbild()); });
      werkzeugleiste.appendChild(vollbildKnopfEl);
    }
    seiteEl.insertBefore(werkzeugleiste, seiteEl.firstChild);
  }
  document.addEventListener('keydown', function(evt) {
    if (evt.key !== 'Escape' || !istVollbild() || istDokuOffen() || istExportOffen()) return;
    var offen = blattEl.style.display !== 'none' || document.body.classList.contains('gw-ebenen-offen') ||
      (seiteEl.classList.contains('gw-app-schmal') && !seiteEl.classList.contains('gw-ebenen-zu'));
    if (!offen) setzeVollbild(false);
  }, true);

  // Mobile-Elemente rund um die Karte (auf dem Desktop per CSS ausgeblendet)
  var kartenzeileEl = document.getElementById('gw-kartenzeile');
  var mobilKopfUnterEl = null, kartenleisteLegendeEl = null, kartenleisteWertEl = null, kartenleisteZusatzEl = null, kurveKnopfEl = null, logoEl = null;
  var teaserEl = null, teaserTitelEl = null, teaserJahr = null, teaserKoerper = null, teaserKopfEl = null;
  if (kartenzeileEl) {
    var mobilKopf = document.createElement('div');
    mobilKopf.className = 'gw-mobil-only gw-mobil-kopf';
    var mkTitel = document.createElement('div'); mkTitel.className = 'gw-mobil-kopf-titel'; mkTitel.textContent = 'Graswachstum';
    mobilKopfUnterEl = document.createElement('div'); mobilKopfUnterEl.className = 'gw-mobil-kopf-unter';
    // Logo (frontend/logo-graswachstum.svg, als CSS-Hintergrund): nur auf der
    // eigenen Seite und im Vollbild - auf der Website steht es im Seitenkopf.
    // Eigene Seite: Link zur Website; Vollbild auf der Website: beendet es.
    logoEl = document.createElement('a');
    logoEl.className = 'gw-logo';
    logoEl.href = 'https://graswachstum.ch/de/growth';
    logoEl.title = 'graswachstum.ch';
    logoEl.setAttribute('aria-label', 'Graswachstum – Messnetz der AGFF (graswachstum.ch)');
    logoEl.addEventListener('click', function(evt) {
      if (eingebettet && istVollbild()) { evt.preventDefault(); setzeVollbild(false); }
    });
    mobilKopf.appendChild(logoEl); mobilKopf.appendChild(mkTitel); mobilKopf.appendChild(mobilKopfUnterEl);
    kartenzeileEl.parentNode.insertBefore(mobilKopf, kartenzeileEl);
    var kartenleiste = document.createElement('div');
    kartenleiste.className = 'gw-mobil-only gw-kartenleiste';
    kartenleisteLegendeEl = document.createElement('div');
    kartenleisteLegendeEl.className = 'gw-kartenleiste-legende';
    kartenleisteLegendeEl.style.display = 'none';
    kartenleisteWertEl = document.createElement('div');
    kartenleisteWertEl.className = 'gw-kartenleiste-wert';
    kartenleisteWertEl.style.display = 'none';
    kartenleisteZusatzEl = document.createElement('div');
    kartenleisteZusatzEl.className = 'gw-kartenleiste-zusatz';
    kartenleiste.appendChild(kartenleisteLegendeEl); kartenleiste.appendChild(kartenleisteWertEl); kartenleiste.appendChild(kartenleisteZusatzEl);
    kartenzeileEl.parentNode.insertBefore(kartenleiste, kartenzeileEl.nextSibling);
    var knoepfe = document.createElement('div');
    knoepfe.className = 'gw-mobil-knoepfe gw-mobil-only';
    var ebenenKnopf = document.createElement('button'); ebenenKnopf.type = 'button'; setzeKnopfInhalt(ebenenKnopf, 'ebenen', 'Ebenen');
    ebenenKnopf.addEventListener('click', function(evt) {
      evt.stopPropagation();
      schliesseBlatt();
      document.body.classList.toggle('gw-ebenen-offen');
      blattGeoeffnetUm = Date.now();
    });
    kurveKnopfEl = document.createElement('button'); kurveKnopfEl.type = 'button'; setzeKnopfInhalt(kurveKnopfEl, 'kurve', 'Alle Kurven');
    kurveKnopfEl.addEventListener('click', function(evt) {
      evt.stopPropagation();
      schliesseBlatt();
      oeffneDetail();
    });
    knoepfe.appendChild(ebenenKnopf); knoepfe.appendChild(kurveKnopfEl);
    var sliderHost = document.getElementById('datenexplorer-slider');
    if (sliderHost) sliderHost.parentNode.insertBefore(knoepfe, sliderHost.nextSibling);

    // Handy: flache Kurve als Teaser unter der Zeitleiste - drei Regionen
    // und das langjaehrige Mittel; Antippen oder Standortwahl oeffnet die
    // Detailansicht.
    teaserEl = document.createElement('div');
    teaserEl.className = 'gw-teaser gw-mobil-only';
    var teaserKopf = document.createElement('div');
    teaserKopf.className = 'gw-teaser-kopf';
    teaserTitelEl = document.createElement('span');
    teaserTitelEl.className = 'gw-teaser-titel';
    var teaserWahl = document.createElement('select');
    teaserWahl.className = 'gw-teaser-wahl';
    teaserWahl.setAttribute('aria-label', 'Standort wählen und Kurve anzeigen');
    var leer = document.createElement('option'); leer.value = ''; leer.textContent = 'Standort wählen …';
    teaserWahl.appendChild(leer);
    siteNames.map(function(n, i) { return [n, i]; }).sort(function(a, b) { return a[0].localeCompare(b[0], 'de'); }).forEach(function(e) {
      var o = document.createElement('option'); o.value = String(e[1]); o.textContent = e[0]; teaserWahl.appendChild(o);
    });
    teaserWahl.addEventListener('change', function() {
      if (teaserWahl.value === '') return;
      waehleSite(parseInt(teaserWahl.value, 10));
      teaserWahl.value = '';
      oeffneDetail();
    });
    teaserWahl.addEventListener('pointerdown', function(evt) { evt.stopPropagation(); });
    // Handy-Layout: der Kopf ist zugleich der Griff zwischen Karte und Kurve
    teaserKopf.setAttribute('role', 'separator');
    teaserKopf.setAttribute('aria-orientation', 'horizontal');
    teaserKopf.setAttribute('aria-label', 'Grösse von Karte und Kurve verschieben');
    teaserKopf.tabIndex = 0;
    var teaserPfeile = document.createElement('span');
    teaserPfeile.className = 'gw-teaser-pfeile';
    [['hoch', 'Kurve ganz nach oben', function() { return teaserMax(); }], ['runter', 'Karte ganz gross', function() { return 0; }]].forEach(function(v) {
      var b = document.createElement('button');
      b.type = 'button'; b.className = 'gw-griff-pfeil'; b.title = v[1]; b.setAttribute('aria-label', v[1]);
      b.appendChild(gwIcon(v[0]));
      b.addEventListener('pointerdown', function(evt) { evt.stopPropagation(); });
      b.addEventListener('click', function(evt) { evt.stopPropagation(); setzeTeaserHoehe(v[2]()); });
      teaserPfeile.appendChild(b);
    });
    teaserKopf.appendChild(teaserTitelEl); teaserKopf.appendChild(teaserWahl); teaserKopf.appendChild(teaserPfeile);
    var teaserPlot = document.createElement('div');
    teaserPlot.className = 'gw-teaser-plot';
    teaserPlot.setAttribute('role', 'button');
    teaserPlot.setAttribute('aria-label', 'Alle Kurven anzeigen');
    teaserPlot.tabIndex = 0;
    teaserPlot.addEventListener('click', function(evt) { evt.stopPropagation(); oeffneDetail(); });
    teaserPlot.addEventListener('keydown', function(evt) { if (evt.key === 'Enter' || evt.key === ' ') { evt.preventDefault(); oeffneDetail(); } });
    var teaserLegende = document.createElement('div');
    teaserLegende.className = 'gw-teaser-legende';
    [['West', '#378ADD', 'solid'], ['Mitte', '#1D9E75', 'solid'], ['Ost', '#BA7517', 'solid'], ['langjähriges Mittel', '#E24B4A', 'dotted']].forEach(function(e) {
      var sp = document.createElement('span');
      var sw = document.createElement('span'); sw.className = 'gw-legend-swatch'; sw.style.borderTopColor = e[1]; sw.style.borderTopStyle = e[2];
      sp.appendChild(sw); sp.appendChild(document.createTextNode(e[0])); teaserLegende.appendChild(sp);
    });
    teaserKoerper = document.createElement('div');
    teaserKoerper.className = 'gw-teaser-koerper';
    teaserKoerper.appendChild(teaserPlot); teaserKoerper.appendChild(teaserLegende);
    teaserEl.appendChild(teaserKopf); teaserEl.appendChild(teaserKoerper);
    knoepfe.parentNode.insertBefore(teaserEl, knoepfe);
    teaserKopfEl = teaserKopf;
  }
  // Handy-Layout: Seite in Bildschirmhoehe, Karte fuellt den Rest; der
  // Teaser-Kopf ist ein Griff - ziehen verschiebt die Grenze, die Pfeile
  // schieben sie ganz nach oben oder unten (Karte bildschirmfuellend).
  var teaserHoehe = null, teaserAnteil = null;
  function istMobilApp() { return !!(seiteEl && seiteEl.classList.contains('gw-mobil-app')); }
  function teaserMax() {
    if (!seiteEl || !teaserEl) return 0;
    var belegt = 0;
    Array.prototype.forEach.call(seiteEl.children, function(k) {
      if (k === teaserEl || k.id === 'gw-kartenzeile') return;
      var cs = getComputedStyle(k);
      if (cs.display === 'none' || cs.position === 'fixed' || cs.position === 'absolute') return;
      belegt += k.offsetHeight;
    });
    belegt += teaserKopfEl ? teaserKopfEl.offsetHeight : 0;
    return Math.max(0, seiteEl.clientHeight - belegt);
  }
  function teaserStandard() {
    var max = teaserMax();
    return Math.min(max, Math.max(110, Math.min(190, Math.round(max * 0.32))));
  }
  function teaserPlotHoehe() {
    if (!istMobilApp() || teaserHoehe === null) return 140;
    var leg = teaserEl.querySelector('.gw-teaser-legende');
    return Math.max(60, teaserHoehe - (leg ? leg.offsetHeight + 4 : 0));
  }
  function passeTeaserPlotAn() {
    var plotEl = teaserEl && teaserEl.querySelector('.gw-teaser-plot');
    if (!plotEl) return;
    var h = teaserPlotHoehe();
    plotEl.style.height = h + 'px';
    if (plotEl.data && (!istMobilApp() || teaserHoehe >= 60)) Plotly.relayout(plotEl, { height: h });
  }
  function setzeTeaserHoehe(h, ohnePlot) {
    if (!teaserKoerper || !istMobilApp()) return;
    var max = teaserMax();
    teaserHoehe = Math.max(0, Math.min(max, Math.round(h)));
    if (max > 0) teaserAnteil = teaserHoehe / max;
    teaserKoerper.style.height = teaserHoehe + 'px';
    seiteEl.classList.toggle('gw-teaser-zu', teaserHoehe < 1);
    seiteEl.classList.toggle('gw-teaser-voll', max > 0 && teaserHoehe >= max - 1);
    if (teaserKopfEl) teaserKopfEl.setAttribute('aria-valuenow', String(max ? Math.round(100 * teaserHoehe / max) : 0));
    if (!ohnePlot) passeTeaserPlotAn();
  }
  if (teaserKopfEl) (function() {
    var startY = 0, startH = 0, bewegt = false, aktiv = false;
    teaserKopfEl.addEventListener('pointerdown', function(evt) {
      if (!istMobilApp()) return;
      aktiv = true; bewegt = false; startY = evt.clientY; startH = teaserHoehe || 0;
      teaserKopfEl.setPointerCapture(evt.pointerId);
    });
    teaserKopfEl.addEventListener('pointermove', function(evt) {
      if (!aktiv) return;
      var d = startY - evt.clientY;
      if (Math.abs(d) > 4) bewegt = true;
      if (bewegt) setzeTeaserHoehe(startH + d, true);
    });
    function ende() {
      if (!aktiv) return;
      aktiv = false;
      var max = teaserMax();
      if (!bewegt) { if (teaserHoehe < 1 || teaserHoehe >= max - 1) setzeTeaserHoehe(teaserStandard()); return; }
      if (teaserHoehe < 50) setzeTeaserHoehe(0);
      else if (teaserHoehe > max - 50) setzeTeaserHoehe(max);
      else setzeTeaserHoehe(teaserHoehe);
    }
    teaserKopfEl.addEventListener('pointerup', ende);
    teaserKopfEl.addEventListener('pointercancel', ende);
    teaserKopfEl.addEventListener('keydown', function(evt) {
      if (!istMobilApp() || evt.target !== teaserKopfEl) return;
      var h = teaserHoehe || 0, max = teaserMax();
      var neu = evt.key === 'ArrowUp' ? h + 40 : evt.key === 'ArrowDown' ? h - 40 : evt.key === 'Home' ? max : evt.key === 'End' ? 0 : null;
      if (neu === null) return;
      evt.preventDefault();
      setzeTeaserHoehe(neu);
    });
  })();
  window.addEventListener('resize', function() {
    if (istMobilApp() && teaserAnteil !== null) setzeTeaserHoehe(teaserAnteil * teaserMax());
  });

  // App-Layout (eigene Seite oder Vollbild, ab 700px): Icon-Leiste links mit
  // Ebenen und Kurve. Ab 1100px stehen die Ebenen fest neben der Karte
  // (einklappbar), darunter klappen sie als Schublade ueber die Karte.
  var appLeisteEbenenEl = null, appLeisteKurveEl = null;
  if (kartenzeileEl) {
    var appLeiste = document.createElement('div');
    appLeiste.className = 'gw-app-leiste';
    appLeisteEbenenEl = document.createElement('button');
    appLeisteEbenenEl.type = 'button'; appLeisteEbenenEl.className = 'gw-leiste-knopf';
    appLeisteEbenenEl.title = 'Ebenen'; appLeisteEbenenEl.setAttribute('aria-label', 'Ebenen');
    appLeisteEbenenEl.appendChild(gwIcon('ebenen'));
    appLeisteEbenenEl.addEventListener('click', function(evt) {
      evt.stopPropagation();
      setzeEbenenOffen(seiteEl.classList.contains('gw-ebenen-zu'));
    });
    appLeisteKurveEl = document.createElement('button');
    appLeisteKurveEl.type = 'button'; appLeisteKurveEl.className = 'gw-leiste-knopf';
    appLeisteKurveEl.title = 'Kurve'; appLeisteKurveEl.setAttribute('aria-label', 'Kurve');
    appLeisteKurveEl.appendChild(gwIcon('kurve'));
    appLeisteKurveEl.addEventListener('click', function(evt) {
      evt.stopPropagation();
      setzeKurveAuf(!(kurveHoehe > 0));
    });
    appLeiste.appendChild(appLeisteEbenenEl); appLeiste.appendChild(appLeisteKurveEl);
    kartenzeileEl.insertBefore(appLeiste, kartenzeileEl.firstChild);
  }
  function setzeEbenenOffen(offen) {
    if (!seiteEl) return;
    seiteEl.classList.toggle('gw-ebenen-zu', !offen);
    if (appLeisteEbenenEl) appLeisteEbenenEl.classList.toggle('aktiv', offen);
    blattGeoeffnetUm = Date.now();
  }
  var appModusVorher = null, appSchmalVorher = null, mobilAppVorher = null;
  // App-Layout ab 700px - eigene Seite, Vollbild und auch eingebettet in
  // eine Website: dort als Block in Bildschirmhoehe unter dem Seitenkopf.
  function setzeBlockHoehe() {
    if ((istAppModus() || istMobilApp()) && eingebettet && !istVollbild()) {
      var oben = seiteEl.getBoundingClientRect().top + window.scrollY;
      seiteEl.style.height = Math.max(520, Math.round(window.innerHeight - oben)) + 'px';
    } else {
      seiteEl.style.height = '';
    }
  }
  function bestimmeAppModus() {
    if (!seiteEl) return;
    var an = !istMobil();
    var schmal = an && window.innerWidth < 1100;
    seiteEl.classList.toggle('gw-app', an);
    seiteEl.classList.toggle('gw-app-schmal', schmal);
    seiteEl.classList.toggle('gw-mobil-app', !an);
    seiteEl.classList.toggle('gw-mit-logo', !eingebettet || istVollbild());
    document.documentElement.classList.toggle('gw-app-aktiv', an);
    document.documentElement.classList.toggle('gw-app-seite', !eingebettet);
    setzeBlockHoehe();
    if (an) schliesseDetail();
    if (!an !== mobilAppVorher) {
      mobilAppVorher = !an;
      if (!an) setTimeout(function() { setzeTeaserHoehe(teaserStandard()); }, 0);
      else if (teaserKoerper) { teaserKoerper.style.height = ''; teaserHoehe = null; passeTeaserPlotAn(); }
    }
    if (an === appModusVorher && schmal === appSchmalVorher) return;
    appModusVorher = an; appSchmalVorher = schmal;
    setzeEbenenOffen(!schmal);
    if (an) {
      setTimeout(function() { setzeKurvenHoehe(kurveStandard()); }, 0);
    } else if (kurveInhalt) {
      kurveInhalt.style.height = '';
      kurveHoehe = null;
      seiteEl.classList.remove('gw-kurve-zu', 'gw-kurve-voll');
      passeKurvenHoeheAn();
    }
    aktualisiereKartentitel();
    setTimeout(function() { window.dispatchEvent(new Event('resize')); }, 0);
  }
  window.addEventListener('resize', bestimmeAppModus);

  function setzeWertText(text) {
    if (wertAnzeigeEl) wertAnzeigeEl.textContent = text;
    wertKurzText = text.replace('Wert am Cursor: ', '');
    if (kartenleisteWertEl) kartenleisteWertEl.textContent = wertKurzText.replace('–', 'Karte antippen oder gedrückt halten für den Wert');
    aktualisiereLupe();
  }
  // Koordinaten und Ort zum zuletzt abgefragten Punkt (Legende, Leiste unter
  // der Karte auf dem Handy, Lupe beim Gedrückthalten)
  var wertKurzText = '', koordText = '', ortText = '';
  function setzeOrtUndKoordinaten(koord, ort) {
    if (koord !== null) koordText = koord;
    if (ort !== null) ortText = ort;
    if (kartenleisteZusatzEl) {
      kartenleisteZusatzEl.innerHTML = '';
      if (ortText || koordText) kartenleisteZusatzEl.innerHTML = [ortText !== wertKurzText ? ortText : '', koordText ? 'LV95 ' + koordText : ''].filter(Boolean).join(' · ');
    }
    aktualisiereLupe();
  }
  // Legende der aktiven Hintergrund-Ebene als schmale Leiste unter der Karte
  // (Mobile), i-Knopf zeigt Bezeichnung und Quelle im Blatt.
  function aktualisiereKartenleiste(info) {
    if (!kartenleisteLegendeEl) return;
    kartenleisteLegendeEl.innerHTML = '';
    // Wertzeile nur mit aktiver Ebene - ohne gibt es am Cursor nichts abzufragen
    if (kartenleisteWertEl) kartenleisteWertEl.style.display = info ? 'block' : 'none';
    if (!info) { kartenleisteLegendeEl.style.display = 'none'; return; }
    kartenleisteLegendeEl.style.display = 'flex';
    var min = document.createElement('span'); min.textContent = info.bereich[0];
    var balken = document.createElement('span'); balken.className = 'gw-kartenleiste-balken';
    balken.style.background = 'linear-gradient(to right,' + info.farben.join(',') + ')';
    var max = document.createElement('span'); max.textContent = info.bereich[1] + ' ' + info.einheit;
    kartenleisteLegendeEl.appendChild(min); kartenleisteLegendeEl.appendChild(balken); kartenleisteLegendeEl.appendChild(max);
    if (typeof macheInfoKnopf === 'function') kartenleisteLegendeEl.appendChild(macheInfoKnopf(info.quelle, null, info.label));
  }
  window.addEventListener('resize', function() { aktualisiereKartentitel(); });

  // Mini-Saisonkurve (SVG) fuer das Standortblatt: gemessener Zuwachs des
  // gewaehlten Jahres, senkrechter Strich = gewaehlte Woche.
  function miniKurve(punkte, markDoy) {
    var NS = 'http://www.w3.org/2000/svg';
    var b = 300, h = 70, x0 = 60, x1 = 330, yMax = 150;
    var svg = document.createElementNS(NS, 'svg');
    svg.setAttribute('viewBox', '0 0 ' + b + ' ' + h); svg.setAttribute('width', '100%'); svg.setAttribute('height', h);
    var px = function(d) { return Math.max(0, Math.min(b, (d - x0) / (x1 - x0) * b)); };
    var py = function(g) { return h - 4 - Math.min(g, yMax) / yMax * (h - 12); };
    var basis = document.createElementNS(NS, 'line');
    basis.setAttribute('x1', 0); basis.setAttribute('x2', b); basis.setAttribute('y1', h - 4); basis.setAttribute('y2', h - 4);
    basis.setAttribute('stroke', '#ccc'); svg.appendChild(basis);
    if (markDoy) {
      var m = document.createElementNS(NS, 'line');
      m.setAttribute('x1', px(markDoy)); m.setAttribute('x2', px(markDoy)); m.setAttribute('y1', 0); m.setAttribute('y2', h - 4);
      m.setAttribute('stroke', '#999'); m.setAttribute('stroke-dasharray', '3,3'); svg.appendChild(m);
    }
    if (punkte && punkte.length) {
      var pl = document.createElementNS(NS, 'polyline');
      pl.setAttribute('points', punkte.map(function(p) { return px(p[0]) + ',' + py(p[1]); }).join(' '));
      pl.setAttribute('fill', 'none'); pl.setAttribute('stroke', '#3B6D11'); pl.setAttribute('stroke-width', 2);
      svg.appendChild(pl);
    }
    return svg;
  }

  // Eigenes Tooltip-Modal STATT Plotlys nativer Hover-Box (siehe unten,
  // .hoverlayer wird per CSS ausgeblendet): die native Box wird von
  // umgebendem overflow:hidden bzw. der begrenzten Iframe-Groesse
  // abgeschnitten, sobald ein Punkt nahe am Kartenrand liegt - betroffener
  // Text war dadurch oft gar nicht lesbar. position:fixed + Zentrierung
  // macht das Modal unabhaengig von der Punktposition immer vollstaendig
  // sichtbar. An document.body gehaengt (nicht in die Karte selbst), damit
  // es auch das overflow:hidden des Kartencontainers sicher umgeht.
  var tooltipModalEl = document.createElement('div');
  tooltipModalEl.className = 'gw-tooltip-modal';
  tooltipModalEl.style.display = 'none';
  document.body.appendChild(tooltipModalEl);

  var options = groupLabels.map(function(label, i) { return { type: 'group', idx: i, label: label.replace('Hoehenlage', 'Höhenlage') }; });
  var siteOptions = siteNames.map(function(name, i) { return { type: 'site', idx: i, label: name }; });

  var controls = document.createElement('div');
  controls.className = 'gw-controls';

  // Durchsuchbare Mehrfachauswahl: gewaehlte Gruppen und Standorte als Chips
  // im Feld, Liste mit Haekchen; Antippen schaltet einen Eintrag um, die Liste
  // bleibt offen. Ohne Auswahl gilt wieder 'Alle Standorte'.
  var comboWrap = document.createElement('div');
  comboWrap.className = 'gw-combo gw-multi';
  var chipsEl = document.createElement('div');
  chipsEl.className = 'gw-chips';
  var input = document.createElement('input');
  input.type = 'text';
  input.placeholder = 'Standort oder Gruppe hinzufügen …';
  input.setAttribute('aria-label', 'Kurven wählen: Standorte und Gruppen suchen und kombinieren');
  var list = document.createElement('div');
  list.className = 'gw-combo-list';
  list.setAttribute('role', 'listbox');
  list.setAttribute('aria-multiselectable', 'true');
  list.style.display = 'none';
  chipsEl.appendChild(input);
  chipsEl.addEventListener('mousedown', function(e) { if (e.target === chipsEl) { e.preventDefault(); input.focus(); } });

  function istGewaehlt(o) {
    var a = auswahlListen();
    return o.type === 'group' ? a.groups.indexOf(o.idx) !== -1 : a.sites.indexOf(o.idx) !== -1;
  }
  function umschalten(o) {
    var a = auswahlListen();
    var liste = o.type === 'group' ? a.groups : a.sites;
    var pos = liste.indexOf(o.idx);
    if (pos === -1) liste.push(o.idx); else liste.splice(pos, 1);
    // 'Alle Standorte' als Start wird ersetzt, sobald etwas anderes dazukommt
    if (pos === -1 && !(o.type === 'group' && o.idx === 0) && selection.type === 'group' && selection.idx === 0) a.groups = a.groups.filter(function(g) { return g !== 0; });
    vorherigeSelection = null;
    setzeAuswahl(a.groups, a.sites);
    aktualisiereAuswahlChips();
    applyState();
  }
  function aktualisiereAuswahlChips() {
    Array.prototype.slice.call(chipsEl.querySelectorAll('.gw-chip')).forEach(function(c) { c.remove(); });
    var a = auswahlListen();
    var eintraege = a.groups.map(function(g) { return { type: 'group', idx: g, label: gruppenName(g), farbe: gruppenFarbe(g) }; })
      .concat(a.sites.map(function(i) { return { type: 'site', idx: i, label: siteNames[i], farbe: siteFarbe(i) }; }));
    eintraege.forEach(function(o) {
      var chip = document.createElement('span');
      chip.className = 'gw-chip' + (o.type === 'group' ? ' gw-chip-gruppe' : '');
      var punkt = document.createElement('span'); punkt.className = 'gw-chip-punkt'; punkt.style.background = o.farbe;
      chip.appendChild(punkt); chip.appendChild(document.createTextNode(o.label));
      var x = document.createElement('button');
      x.type = 'button'; x.className = 'gw-chip-x'; x.textContent = String.fromCharCode(215);
      x.setAttribute('aria-label', o.label + ' entfernen');
      x.addEventListener('mousedown', function(e) { e.preventDefault(); e.stopPropagation(); umschalten(o); });
      chip.appendChild(x);
      chipsEl.insertBefore(chip, input);
    });
    input.placeholder = eintraege.length > 1 || selection.type !== 'group' || selection.idx !== 0 ? 'hinzufügen …' : 'Standort oder Gruppe hinzufügen …';
    if (list.style.display === 'block') renderList(input.value);
  }

  function renderList(query) {
    query = (query || '').toLowerCase();
    list.innerHTML = '';
    var matchedGroups = options.filter(function(o) { return o.label.toLowerCase().indexOf(query) !== -1; });
    var matchedSites = siteOptions.filter(function(o) { return o.label.toLowerCase().indexOf(query) !== -1; });
    var kopf = document.createElement('div');
    kopf.className = 'gw-combo-sep gw-combo-kopf';
    kopf.textContent = 'Gruppen (Mittelwert)';
    if (matchedGroups.length) list.appendChild(kopf);
    matchedGroups.forEach(function(o) { list.appendChild(makeItem(o)); });
    if (matchedSites.length > 0) {
      var sep = document.createElement('div');
      sep.className = 'gw-combo-sep';
      sep.textContent = 'Einzelstandorte';
      list.appendChild(sep);
      matchedSites.forEach(function(o) { list.appendChild(makeItem(o)); });
    }
    var a = auswahlListen();
    if (a.groups.length + a.sites.length > 1 || selection.type !== 'group' || selection.idx !== 0) {
      var zurueck = document.createElement('div');
      zurueck.className = 'gw-combo-zurueck';
      zurueck.textContent = 'Auswahl zurücksetzen (Alle Standorte)';
      zurueck.addEventListener('mousedown', function(e) { e.preventDefault(); setzeAuswahl([0], []); aktualisiereAuswahlChips(); applyState(); });
      list.appendChild(zurueck);
    }
    list.style.display = (matchedGroups.length + matchedSites.length > 0) ? 'block' : 'none';
  }

  function makeItem(o) {
    var item = document.createElement('div');
    item.className = 'gw-combo-item';
    item.setAttribute('role', 'option');
    var an = istGewaehlt(o);
    item.setAttribute('aria-selected', an ? 'true' : 'false');
    if (an) item.classList.add('gewaehlt');
    var haken = document.createElement('span'); haken.className = 'gw-combo-haken'; haken.textContent = an ? String.fromCharCode(10003) : '';
    var punkt = document.createElement('span'); punkt.className = 'gw-chip-punkt';
    punkt.style.background = o.type === 'site' ? siteColors[o.idx] : 'transparent';
    if (o.type === 'group') punkt.style.borderTop = '2px dashed #555';
    item.appendChild(haken); item.appendChild(punkt); item.appendChild(document.createTextNode(o.label));
    item.addEventListener('mousedown', function(e) {
      e.preventDefault();
      umschalten(o);
      input.value = '';
      renderList('');
    });
    return item;
  }

  input.addEventListener('focus', function() { renderList(input.value); });
  input.addEventListener('input', function() { renderList(input.value); });
  input.addEventListener('keydown', function(e) {
    if (e.key === 'Backspace' && input.value === '') {
      var a = auswahlListen();
      if (a.sites.length) a.sites.pop(); else if (a.groups.length) a.groups.pop();
      setzeAuswahl(a.groups, a.sites); aktualisiereAuswahlChips(); applyState();
    } else if (e.key === 'Enter') {
      var erstes = list.querySelector('.gw-combo-item');
      if (erstes) { e.preventDefault(); erstes.dispatchEvent(new MouseEvent('mousedown')); }
    } else if (e.key === 'Escape') { list.style.display = 'none'; input.blur(); }
  });
  input.addEventListener('blur', function() { setTimeout(function() { list.style.display = 'none'; input.value = ''; }, 150); });
  comboWrap.appendChild(chipsEl);
  comboWrap.appendChild(list);
  aktualisiereAuswahlChips();

  // Standort-Filter per Klick auf einen Legenden- oder Karteneintrag (statt
  // nur ueber die Combobox oben): ein Klick auf einen Standort, der NICHT
  // bereits einzeln ausgewaehlt ist, filtert die Kurve darauf (die vorherige
  // Auswahl - Gruppe oder anderer Standort - wird gemerkt). Ein erneuter
  // Klick auf denselben, bereits ausgewaehlten Standort stellt diese
  // vorherige Auswahl wieder her (Toggle). Die Combobox selbst nutzt diese
  // Funktion bewusst NICHT (siehe makeItem() oben) - dort bleibt jede
  // Auswahl eine reine Vorwaerts-Auswahl.
  function waehleSiteViaKlick(siteIdx) {
    if (selection.type === 'site' && selection.idx === siteIdx) {
      if (!vorherigeSelection) return;
      selection = vorherigeSelection;
      vorherigeSelection = null;
    } else {
      vorherigeSelection = kopiereAuswahl(selection);
      selection = { type: 'site', idx: siteIdx };
      aktiviereVorjahrFuerEinzelstandort();
    }
    aktualisiereAuswahlChips();
    applyState();
  }

  // Kartenklick: Standort waehlen OHNE Umschalten (ein zweiter Klick auf
  // denselben Standort oeffnet nur wieder das Blatt).
  function waehleSite(siteIdx) {
    if (selection.type === 'site' && selection.idx === siteIdx) return;
    vorherigeSelection = kopiereAuswahl(selection);
    selection = { type: 'site', idx: siteIdx };
    aktiviereVorjahrFuerEinzelstandort();
    aktualisiereAuswahlChips();
    applyState();
  }
  function zeigeKurve() {
    if (istMobil()) { oeffneDetail(); return; }
    if (istAppModus()) {
      if (!(kurveHoehe >= 80)) setzeKurvenHoehe(kurveStandard());
      return;
    }
    var bereich = document.querySelector('.gw-kurvenbereich');
    if (bereich) bereich.scrollIntoView({ behavior: 'smooth', block: 'start' });
  }

  // Teaser (Handy): eigene kleine Plotly-Grafik aus den Daten der
  // Regionen-Mittel und des langjaehrigen Mittels der grossen Kurve.
  // Gleitendes Mittel ueber 3 Wochen (Teaser: Verlauf statt Wochenrauschen)
  function glaetten(y) {
    var a = Array.prototype.slice.call(y || []);
    return a.map(function(v, i) {
      if (v === null || v === undefined || isNaN(v)) return v;
      var sum = 0, n = 0;
      for (var k = i - 1; k <= i + 1; k++) { var w = a[k]; if (w !== null && w !== undefined && !isNaN(w)) { sum += w; n++; } }
      return n ? sum / n : v;
    });
  }
  function aktualisiereTeaser() {
    if (!teaserEl || typeof Plotly === 'undefined') return;
    var plotEl = teaserEl.querySelector('.gw-teaser-plot');
    if (teaserTitelEl) { teaserTitelEl.textContent = 'Kurve ' + selectedYear; teaserTitelEl.title = 'Wachstumskurve ' + selectedYear; }
    var strich = [{ type: 'line', x0: selectedWeek, x1: selectedWeek, yref: 'paper', y0: 0, y1: 1, line: { color: '#999', width: 1, dash: 'dot' } }];
    if (teaserJahr === selectedYear && plotEl.data) { Plotly.relayout(plotEl, { shapes: strich }); return; }
    if (!istMobil() || !el.data) return;
    teaserJahr = selectedYear;
    var farben = { West: '#378ADD', Mitte: '#1D9E75', Ost: '#BA7517' };
    var spuren = [];
    Object.keys(farben).forEach(function(region) {
      var gIdx = groupLabels.indexOf('Region: ' + region);
      groupGrowthMeta.forEach(function(m) {
        if (gIdx === -1 || m.year !== selectedYear || m.groupIdx !== gIdx || !el.data[m.traceIdx]) return;
        var d = el.data[m.traceIdx];
        spuren.push({ x: d.x, y: glaetten(d.y), type: 'scatter', mode: 'lines', connectgaps: true, line: { color: farben[region], width: 2, shape: 'spline', smoothing: 0.8 }, hoverinfo: 'skip' });
      });
    });
    var std = el.data[standardKurveTraceIdx];
    if (std) spuren.push({ x: std.x, y: std.y, type: 'scatter', mode: 'lines', line: { color: '#E24B4A', width: 1.5, dash: 'dot' }, hoverinfo: 'skip' });
    var schrift = getComputedStyle(seiteEl || document.body).fontFamily;
    Plotly.react(plotEl, spuren, {
      height: teaserPlotHoehe(), margin: { l: 30, r: 6, t: 4, b: 20 }, showlegend: false,
      xaxis: { range: [1, 52], fixedrange: true, tickvals: [10, 20, 30, 40, 50], tickfont: { size: 10 }, zeroline: false, showgrid: false },
      yaxis: { fixedrange: true, rangemode: 'tozero', tickfont: { size: 10 }, zeroline: false, gridcolor: '#eee', nticks: 4 },
      shapes: strich, font: { family: schrift }, paper_bgcolor: 'rgba(0,0,0,0)', plot_bgcolor: 'rgba(0,0,0,0)'
    }, { staticPlot: true, responsive: true, displayModeBar: false });
  }

  // Detailansicht (Handy): die grosse Kurve als Vollbild-Ebene, im
  // Querformat ohne Bedienelemente. Zurueck-Geste des Browsers schliesst sie.
  var detailOffen = false;
  function oeffneDetail() {
    if (!istMobil()) { zeigeKurve(); return; }
    if (detailOffen) return;
    detailOffen = true;
    document.body.classList.add('gw-kurve-detail');
    document.documentElement.classList.add('gw-detail-offen');
    if (detailTitelEl) detailTitelEl.textContent = auswahlText() + ' · ' + selectedYear;
    try { history.pushState({ gwDetail: true }, ''); } catch (e) {}
    passeKurvenHoeheAn();
  }
  function schliesseDetail(ausVerlauf) {
    if (!detailOffen) return;
    detailOffen = false;
    document.body.classList.remove('gw-kurve-detail');
    document.documentElement.classList.remove('gw-detail-offen');
    if (document.fullscreenElement && document.exitFullscreen) document.exitFullscreen().catch(function() {});
    try { if (screen.orientation && screen.orientation.unlock) screen.orientation.unlock(); } catch (e) {}
    if (!ausVerlauf && history.state && history.state.gwDetail) history.back();
    passeKurvenHoeheAn();
  }
  window.addEventListener('popstate', function() { if (detailOffen) schliesseDetail(true); });
  var detailResizeTimer = null;
  function detailNachfuehren() {
    if (!detailOffen) return;
    clearTimeout(detailResizeTimer);
    detailResizeTimer = setTimeout(passeKurvenHoeheAn, 200);
  }
  window.addEventListener('orientationchange', detailNachfuehren);
  window.addEventListener('resize', detailNachfuehren);
  function zeigeStandortBlatt(ort, siteIdx, kennzahlen) {
    var t = (kennzahlen || '').split('|');
    var inhalt = document.createElement('div');
    var zeile = function(l, w) {
      var z = document.createElement('div'); z.className = 'gw-blatt-zeile';
      var a = document.createElement('span'); a.textContent = l;
      var b = document.createElement('b'); b.textContent = w;
      z.appendChild(a); z.appendChild(b); inhalt.appendChild(z);
    };
    zeile('Graswachstum', (t[2] || '–') + ' kg TS/ha/Tag');
    zeile('DGV', t[3] ? t[3] + ' kg TS/ha' : 'keine Angabe');
    var fi = afcFensterJeWoche[selectedYear + ' ' + selectedWeek];
    var v = fi ? afcVerlaeufe[fi - 1] : null;
    if (v) zeile('Ziel-DGV dieser Woche', v.low + '–' + v.high + ' kg TS/ha');
    var verlauf = standortVerlaeufe[ort] && standortVerlaeufe[ort][selectedYear];
    if (verlauf && verlauf.length > 1) {
      var titelKurve = document.createElement('div');
      titelKurve.className = 'gw-blatt-unter';
      titelKurve.style.marginTop = '8px';
      titelKurve.textContent = 'Graswachstum ' + selectedYear + ' (Strich = gewaehlte Woche)';
      inhalt.appendChild(titelKurve);
      var montag = new Date(Date.UTC(selectedYear, 0, 4));
      montag.setUTCDate(montag.getUTCDate() - ((montag.getUTCDay() + 6) % 7) + (selectedWeek - 1) * 7);
      var doy = Math.round((montag - Date.UTC(selectedYear, 0, 1)) / 86400000) + 1;
      inhalt.appendChild(miniKurve(verlauf, doy));
    }
    var knopf = document.createElement('button');
    knopf.type = 'button'; knopf.className = 'gw-blatt-knopf';
    knopf.textContent = 'Ganze Graswachstumskurve anzeigen';
    knopf.addEventListener('click', function() { schliesseBlatt(); zeigeKurve(); });
    inhalt.appendChild(knopf);
    var alter = t[5] === '0' ? 'heute' : (t[5] === '1' ? 'gestern' : 'vor ' + t[5] + ' Tagen');
    var unter = (t[1] ? t[1] + ' m ü. M. · ' : '') + (t[4] ? 'gemessen am ' + t[4] + ' (' + alter + ')' : '');
    zeigeBlatt(t[0] || ort, unter, inhalt);
  }

  // Bei Auswahl eines EINZELNEN Standorts (statt einer Gruppe) ist der
  // Vergleich mit dem Vorjahr besonders aussagekraeftig (nur eine Kurve statt
  // vieler ueberlagerter) - der Schalter wird deshalb automatisch aktiviert,
  // falls er noch aus war. Manuelles Wiederausschalten durch die Nutzerin
  // bleibt danach erhalten (wird NICHT bei jedem applyState() erneut erzwungen,
  // nur genau bei diesem Auswahlwechsel).
  var vorjahrAutomatisch = false;
  function aktiviereVorjahrFuerEinzelstandort() {
    if (vorjahrOn) return;
    vorjahrOn = true;
    vorjahrAutomatisch = true;
    if (vorjahrToggleWrap && vorjahrToggleWrap.checkbox) vorjahrToggleWrap.checkbox.checked = true;
  }

  var yearSelect = document.createElement('select');
  yearSelect.className = 'gw-year-select';
  alleJahre.forEach(function(jr) {
    var opt = document.createElement('option');
    opt.value = jr;
    opt.textContent = jr;
    if (jr === neuestesJahr) opt.selected = true;
    yearSelect.appendChild(opt);
  });
  yearSelect.addEventListener('change', function() {
    stoppePlay();
    selectedYear = yearSelect.value;
    var hatNiederschlag = jahreMitNiederschlag.indexOf(selectedYear) !== -1;
    precipCheckbox.disabled = !hatNiederschlag;
    if (!hatNiederschlag) { precipCheckbox.checked = false; precipOn = false; }
    aktualisiereLayerVerfuegbarkeit();
    applyXAxis();
    var maxW = maxWocheFuerJahr(selectedYear);
    if (selectedWeek > maxW) {
      selectedWeek = maxW;
      if (sliderInput) sliderInput.value = String(selectedWeek);
    }
    if (typeof aktualisiereZukunftMaske === 'function') aktualisiereZukunftMaske();
    applyState();
  });

  // Radiobuttons rechts neben der Wachstumskarte fuer die optionalen
  // Hintergrund-Ebenen - befuellen einen leeren Platzhalter-Container aus
  // dem HTML (analog zum Kalenderwochen-Schieberegler weiter unten).
  var mapControlsContainer = document.getElementById('datenexplorer-map-controls');
  var radioNiederschlag = null, radioBoden = null, radioTemperatur = null, radioBodentemperatur = null;
  var radioSonnenschein = null, radioEt0 = null, radioGdd = null, radioNiederschlagKum = null;
  var radioWachstumspotenzialRate = null, radioWachstumspotenzialKum = null;
  var experimentellerModus = new URLSearchParams(window.location.search).has('experimentell');
  var radioJeEbene = {};
  var toggleGraswachstum = null, toggleDgv = null, toggleStationen = null;
  function aktualisiereLayerVerfuegbarkeit() {
    if (radioNiederschlag) radioNiederschlag.disabled = !ebeneHatJahr('niederschlag', selectedYear);
    if (radioBoden) radioBoden.disabled = !ebeneHatJahr('boden', selectedYear);
    if (radioTemperatur) radioTemperatur.disabled = !ebeneHatJahr('temperatur', selectedYear);
    if (radioBodentemperatur) radioBodentemperatur.disabled = !ebeneHatJahr('bodentemperatur', selectedYear);
    if (radioSonnenschein) radioSonnenschein.disabled = !ebeneHatJahr('sonnenschein', selectedYear);
    if (radioEt0) radioEt0.disabled = !ebeneHatJahr('et0', selectedYear);
    if (radioGdd) radioGdd.disabled = !ebeneHatJahr('gdd', selectedYear);
    if (radioNiederschlagKum) radioNiederschlagKum.disabled = !ebeneHatJahr('niederschlag_kum', selectedYear);
    if (radioWachstumspotenzialRate) radioWachstumspotenzialRate.disabled = !ebeneHatJahr('wachstumspotenzial_rate', selectedYear);
    if (radioWachstumspotenzialKum) radioWachstumspotenzialKum.disabled = !ebeneHatJahr('wachstumspotenzial_kum', selectedYear);
    schnittRadios.forEach(function(r) { r.disabled = !ebeneHatJahr(r.value, selectedYear); });
    var schnittWeg = schnittRadios.some(function(r) { return hintergrundEbene === r.value && r.disabled; });
    if (schnittWeg || (hintergrundEbene === 'niederschlag' && radioNiederschlag && radioNiederschlag.disabled) ||
        (hintergrundEbene === 'boden' && radioBoden && radioBoden.disabled) ||
        (hintergrundEbene === 'temperatur' && radioTemperatur && radioTemperatur.disabled) ||
        (hintergrundEbene === 'bodentemperatur' && radioBodentemperatur && radioBodentemperatur.disabled) ||
        (hintergrundEbene === 'sonnenschein' && radioSonnenschein && radioSonnenschein.disabled) ||
        (hintergrundEbene === 'et0' && radioEt0 && radioEt0.disabled) ||
        (hintergrundEbene === 'gdd' && radioGdd && radioGdd.disabled) ||
        (hintergrundEbene === 'niederschlag_kum' && radioNiederschlagKum && radioNiederschlagKum.disabled) ||
        (hintergrundEbene === 'wachstumspotenzial_rate' && radioWachstumspotenzialRate && radioWachstumspotenzialRate.disabled) ||
        (hintergrundEbene === 'wachstumspotenzial_kum' && radioWachstumspotenzialKum && radioWachstumspotenzialKum.disabled)) {
      hintergrundEbene = 'keine';
      var radioKeineEl = mapControlsContainer && mapControlsContainer.querySelector('input[value=keine]');
      if (radioKeineEl) radioKeineEl.checked = true;
      aktualisiereLayerLegende();
    }
  }
  // Farbverlauf-Balken + Quellenangabe der aktuell gewaehlten Hintergrund-
  // Ebene, direkt aus layerLegenden (R: layer_legenden) - EINE Quelle fuer
  // Farben/Wertebereich/Quelle statt sie hier ein zweites Mal nachzubauen.
  // Bei Auswahl Keine bleibt der Bereich leer (kein Farbverlauf ohne aktive Ebene).
  var layerLegendeBox = null;
  var wertAnzeigeEl = null;
  var koordinatenEl = null;
  var ortschaftEl = null;
  var ladeHinweisEl = null;
  var afcLegendeBox = null;
  // Pfeil auf dem Farbverlaufs-Balken, der die Position des Werts am Cursor
  // zeigt (zeigeWertAmPunkt()) - legendeBereich ist der [min,max]-Wertebereich
  // der gerade aktiven Ebene, fuer die Prozent-Umrechnung.
  var legendePfeilEl = null;
  var legendeBereich = null;
  // Kompakte AFC-Legende im Ebenen-Kasten (zusaetzlich zur grossen Ring-
  // Legende auf der Karte selbst) - nur sichtbar, wenn der AFC-Schalter an
  // ist (Default), und mit dem jahreszeitlichen Zielbereich der GERADE
  // gewaehlten Kalenderwoche (kann je nach Woche wechseln, siehe
  // afc_optimum_windows/R).
  function aktualisiereAfcLegende() {
    if (!afcLegendeBox) return;
    if (!afcOn) { afcLegendeBox.style.display = 'none'; return; }
    var fensterIdx = afcFensterJeWoche[selectedYear + ' ' + selectedWeek];
    var verlauf = fensterIdx ? afcVerlaeufe[fensterIdx - 1] : null;
    if (!verlauf) { afcLegendeBox.style.display = 'none'; return; }
    afcLegendeBox.style.display = 'block';
    afcLegendeBox.innerHTML = '';
    // Ring wie der AFC-Ring auf der Karte: conic-gradient beginnt bei 12 Uhr
    // und laeuft im Uhrzeigersinn von 0 bis 1500 kg - 0 und 1500 liegen also
    // beide oben, dort markiert ein Strich den Nullpunkt (mit Beschriftung).
    // Der Zielbereich der Woche: duenne Striche an den Grenzen plus ein
    // gebogener Doppelpfeil aussen am Ring.
    var ringWrap = document.createElement('div');
    ringWrap.className = 'gw-afc-ring-wrap';
    var ring = document.createElement('div');
    ring.className = 'gw-afc-ring';
    ring.style.background = 'conic-gradient(' + verlauf.farben.join(',') + ')';
    ringWrap.appendChild(ring);
    var NS = 'http://www.w3.org/2000/svg';
    var svg = document.createElementNS(NS, 'svg');
    svg.setAttribute('class', 'gw-afc-ring-svg');
    svg.setAttribute('width', '80'); svg.setAttribute('height', '80'); svg.setAttribute('viewBox', '0 0 80 80');
    var el = function(name, attrs) { var e = document.createElementNS(NS, name); Object.keys(attrs).forEach(function(k) { e.setAttribute(k, attrs[k]); }); return e; };
    var defs = el('defs', {});
    var marker = el('marker', { id: 'gw-afc-pfeil', viewBox: '0 0 10 10', refX: '6', refY: '5', markerWidth: '5', markerHeight: '5', orient: 'auto-start-reverse' });
    marker.appendChild(el('path', { d: 'M0,0 L10,5 L0,10 z', fill: '#000' }));
    defs.appendChild(marker); svg.appendChild(defs);
    svg.appendChild(el('line', { x1: 40, y1: 9, x2: 40, y2: 24, stroke: '#000', 'stroke-width': 2 }));
    var punkt = function(wert, r) { var a = wert / 1500 * 2 * Math.PI; return [40 + r * Math.sin(a), 40 - r * Math.cos(a)]; };
    // Striche an Zielunter-/-obergrenze quer ueber das Farbband, wie auf den
    // AFC-Ringen der Karte - der Pfeil aussen verbindet sie.
    [verlauf.low, verlauf.high].forEach(function(wert) {
      var innen = punkt(wert, 16), aussen = punkt(wert, 30);
      svg.appendChild(el('line', { x1: innen[0], y1: innen[1], x2: aussen[0], y2: aussen[1], stroke: '#000', 'stroke-width': 1.5 }));
    });
    var p1 = punkt(verlauf.low, 34), p2 = punkt(verlauf.high, 34);
    var gross = (verlauf.high - verlauf.low) / 1500 > 0.5 ? 1 : 0;
    svg.appendChild(el('path', { d: 'M' + p1[0] + ',' + p1[1] + ' A34,34 0 ' + gross + ',1 ' + p2[0] + ',' + p2[1],
      fill: 'none', stroke: '#000', 'stroke-width': 1.5, 'marker-start': 'url(#gw-afc-pfeil)', 'marker-end': 'url(#gw-afc-pfeil)' }));
    // Schlank: nur die beiden Grenzen des Zielbereichs als kleine Zahlen an
    // den Pfeilenden - die ausfuehrliche Erklaerung steht in der Hilfe (DGV).
    [verlauf.low, verlauf.high].forEach(function(wert) {
      var p = punkt(wert, 43);
      var t = el('text', { x: p[0], y: p[1] + 3.5, 'font-size': 10, fill: '#444',
        'text-anchor': p[0] > 42 ? 'start' : (p[0] < 38 ? 'end' : 'middle') });
      t.textContent = String(wert);
      svg.appendChild(t);
    });
    ringWrap.appendChild(svg);
    ringWrap.title = 'DGV-Zielbereich dieser Woche: ' + verlauf.low + '–' + verlauf.high + ' kg TS/ha';
    afcLegendeBox.dataset.zielLow = verlauf.low;
    afcLegendeBox.dataset.zielHigh = verlauf.high;
    afcLegendeBox.appendChild(ringWrap);
  }

  // "Tage seit Messung"-Legende (Graufaerbung von Graswachstum-Kreis/AFC-Ring)
  // als kompakte Box im Ebenen-Kasten statt als Plotly-natives colorbar auf
  // der Karte selbst - Letzteres kollidierte dort mit dem (nur bei Hover
  // sichtbaren) Modebar-Bereich und war auf der Karte zudem schwer zu finden.
  // Dieselbe Sichtbarkeitsregel wie die Hover-Marker-Trace selbst
  // (graswachstumOn || afcOn, siehe applyMapState()) - die Faerbung gehoert
  // zu BEIDEN Ebenen gemeinsam, nicht nur zu AFC.
  var tageSeitMessungBox = null;
  function aktualisiereTageSeitMessungLegende() {
    if (!tageSeitMessungBox) return;
    if (!graswachstumOn && !afcOn) { tageSeitMessungBox.style.display = 'none'; return; }
    tageSeitMessungBox.style.display = 'flex';
  }
  // Kartentitel: fester Kopf "Graswachstum", darunter Kalenderwoche und
  // Grafikdatum (Erstellung der Seite). Ist eine Meteo-Ebene aktiv, folgt deren
  // Name mit dem tatsaechlichen Datenstand (werte.bis) - der kann, v.a. in der
  // neuesten Woche, wegen Publikationsverzoegerung hinterherhinken.
  function aktualisiereKartentitel() {
    var growthMapGd = document.querySelector('#datenexplorer-growthmap .js-plotly-plot');
    if (!growthMapGd) return;
    // Kurz (auch fuer Mobile): fester Titel, darunter Woche und Grafikdatum,
    // bei aktiver Meteo-Ebene eine dritte Zeile mit Ebene und Datenstand.
    // Erstellungsdatum allein wirkt aktueller als die Messdaten: neueste Messung des Jahres dazu
    var messDatum = messDatumJeJahr[selectedYear];
    var unterzeile = 'KW ' + selectedWeek + ' ' + selectedYear + (messDatum ? ' · letzte Messung ' + messDatum.slice(0, 6) + ' · aktualisiert ' + grafikDatum : ' · Grafik vom ' + grafikDatum);
    var ebenenZeile = '';
    var titel = '<b>Graswachstum</b><br><span style="font-size:12px">' + unterzeile + '</span>';
    var zeilen = 2;
    if (hintergrundEbene !== 'keine') {
      // Bodenwasserbilanz traegt ihr "(berechnet) <Datum>" bereits im
      // Radio-Label (siehe aktualisiereLayerLabels()) - hier deshalb die
      // Basis-Bezeichnung OHNE das Datum verwenden, das unten per "Stand
      // ..." ohnehin einmal dazukommt. Sonst stuende dasselbe Datum zweimal
      // im Titel.
      var radio = radioJeEbene[hintergrundEbene];
      var label = (hintergrundEbene === 'boden')
        ? (layerLegenden.boden.label + ' (berechnet)')
        : (hintergrundEbene === 'wachstumspotenzial_rate' || hintergrundEbene === 'wachstumspotenzial_kum')
        ? (layerLegenden[hintergrundEbene].label + ' (experimentell, Erholung ' + erholung + ' Tage)')
        : ((radio && radio.labelTextEl) ? radio.labelTextEl.textContent
           : (layerLegenden[hintergrundEbene] ? layerLegenden[hintergrundEbene].label : hintergrundEbene));
      var cacheEintrag = ebenenCache[ebeneDateiSchluessel(hintergrundEbene)];
      var werteEintrag = cacheEintrag && cacheEintrag.werte && cacheEintrag.werte[selectedYear + ' ' + selectedWeek];
      var stand = (werteEintrag && werteEintrag.bis) ? ('Stand ' + werteEintrag.bis) : 'lädt…';
      ebenenZeile = label + ', ' + stand;
      titel += '<br><span style="font-size:11px">' + ebenenZeile + '</span>';
      zeilen = 3;
    }
    if (mobilKopfUnterEl) mobilKopfUnterEl.textContent = unterzeile + (ebenenZeile ? ' · ' + ebenenZeile : '');
    if (istMobil() || istAppModus()) Plotly.relayout(growthMapGd, { 'title.text': '', 'margin.t': 6 });
    else Plotly.relayout(growthMapGd, { 'title.text': titel, 'margin.t': zeilen === 3 ? 74 : 58 });
  }

  function aktualisiereLayerLegende() {
    aktualisiereMeteoFensterSichtbarkeit();
    aktualisiereKartentitel();
    if (!layerLegendeBox) return;
    var info = layerLegenden[hintergrundEbene];
    aktualisiereKartenleiste(info);
    if (!info) { layerLegendeBox.style.display = 'none'; wertAnzeigeEl = null; koordinatenEl = null; ortschaftEl = null; legendePfeilEl = null; legendeBereich = null; return; }
    layerLegendeBox.style.display = 'block';
    layerLegendeBox.innerHTML = '';
    // Wertebereich bei Summen-Ebenen (info.fensterSkaliert) proportional zur
    // aktuellen Fenstergroesse hochskaliert (siehe R: baue_fenster_ebenen())
    // - bei Mittelwert-Ebenen bleibt der Bereich unveraendert.
    var bereich = info.fensterSkaliert ? [info.bereich[0], Math.round(info.bereich[1] * meteoFenster / 7)] : info.bereich;
    legendeBereich = bereich;
    var balkenWrap = document.createElement('div');
    balkenWrap.className = 'gw-layer-legende-balken-wrap';
    var balken = document.createElement('div');
    balken.className = 'gw-layer-legende-balken';
    balken.style.background = 'linear-gradient(to right, ' + info.farben.join(',') + ')';
    // Pfeil zeigt die Position des Werts am Cursor auf dem Farbverlauf -
    // Positionierung/Sichtbarkeit uebernimmt zeigeWertAmPunkt()/
    // versteckeWertAnzeige(), hier nur frisch angelegt und initial versteckt.
    legendePfeilEl = document.createElement('div');
    legendePfeilEl.className = 'gw-legende-pfeil';
    legendePfeilEl.style.display = 'none';
    balkenWrap.appendChild(balken);
    balkenWrap.appendChild(legendePfeilEl);
    var skala = document.createElement('div');
    skala.className = 'gw-layer-legende-skala';
    var minEl = document.createElement('span'); minEl.textContent = bereich[0] + ' ' + info.einheit;
    var maxEl = document.createElement('span'); maxEl.textContent = bereich[1] + ' ' + info.einheit;
    skala.appendChild(minEl);
    skala.appendChild(maxEl);
    var quelle = document.createElement('div');
    quelle.className = 'gw-layer-legende-quelle';
    quelle.textContent = 'Quelle: ' + info.quelle;
    wertAnzeigeEl = document.createElement('div');
    wertAnzeigeEl.className = 'gw-layer-wert-anzeige';
    setzeWertText('Wert am Cursor: –');
    koordinatenEl = document.createElement('div');
    koordinatenEl.className = 'gw-layer-wert-anzeige gw-layer-wert-zusatz';
    koordinatenEl.textContent = 'Koordinaten LV95: –';
    ortschaftEl = document.createElement('div');
    ortschaftEl.className = 'gw-layer-wert-anzeige gw-layer-wert-zusatz';
    ortschaftEl.textContent = 'Ort: –';
    // Ladehinweis (blinkende Punkte, wie beim Ort/PLZ-Nachschlagen) - nur
    // sichtbar, waehrend diese Ebene NOCH NICHT im ebenenCache liegt (siehe
    // ladeEbene()/aktualisiereHintergrundEbene() oben): Bild und Werte-
    // Gitter treffen typischerweise erst nach einem kurzen fetch() ein,
    // ohne diesen Hinweis waere die Karte in der Zwischenzeit einfach leer
    // und nicht von keine Daten fuer diese Woche zu unterscheiden.
    ladeHinweisEl = document.createElement('div');
    ladeHinweisEl.className = 'gw-layer-wert-anzeige';
    ladeHinweisEl.innerHTML = 'Ebene wird geladen ' + LADE_PUNKTE_HTML;
    ladeHinweisEl.style.display = ebenenCache[ebeneDateiSchluessel(hintergrundEbene)] ? 'none' : 'block';
    // Von laedt noch (ladeHinweisEl) UNTERSCHEIDEN: die Ebene ist bereits
    // geladen, hat aber fuer die AKTUELL gewaehlte Woche keine Daten (z.B.
    // Sonnenschein nahe am aktuellen Datum, siehe ebeneHatWoche()) - sonst
    // zeigt die Karte einfach kommentarlos nichts.
    var keinDatenHinweisEl = document.createElement('div');
    keinDatenHinweisEl.className = 'gw-layer-wert-anzeige';
    keinDatenHinweisEl.textContent = 'Keine Daten für diese Woche';
    keinDatenHinweisEl.style.display = (ladeHinweisEl.style.display === 'none' && !ebeneHatWoche(hintergrundEbene, selectedYear, selectedWeek)) ? 'block' : 'none';
    layerLegendeBox.appendChild(balkenWrap);
    layerLegendeBox.appendChild(skala);
    layerLegendeBox.appendChild(quelle);
    layerLegendeBox.appendChild(ladeHinweisEl);
    layerLegendeBox.appendChild(keinDatenHinweisEl);
    layerLegendeBox.appendChild(wertAnzeigeEl);
    layerLegendeBox.appendChild(koordinatenEl);
    layerLegendeBox.appendChild(ortschaftEl);
  }

  // Anzeigefeld statt Plotly-Tooltip: die Hintergrund-Ebenen sind reine
  // PNGs (kein Hover moeglich) - beim Bewegen/Tippen ueber der Karte wird
  // per Pixel->Daten-Umrechnung (Plotlys eigene xaxis/yaxis.p2d()) die
  // naechstgelegene Zelle des mitgelieferten, groben Werte-Gitters (siehe
  // R: raster_zu_datauri(), dieselbe Aufloesung wie das jeweilige Bild)
  // nachgeschlagen - kein zusaetzlicher Server, keine Plotly-Trace noetig.
  // Liefert das Werte-Gitter der AKTUELL gewaehlten Ebene nur, wenn sie
  // bereits vollstaendig geladen ist (siehe ladeEbene()/ebenenCache oben) -
  // waehrend des ersten Ladens (kurzes Zeitfenster) liefert die Funktion
  // null, die Cursor-Wertabfrage zeigt dann keine Daten statt eines
  // veralteten/falschen Werts.
  function aktivesWerteGitter() {
    if (hintergrundEbene === 'keine') return null;
    var cache = ebenenCache[ebeneDateiSchluessel(hintergrundEbene)];
    return cache ? cache.werte : null;
  }
  // Naeherungsformel swisstopo (WGS84 -> LV95, Genauigkeit ca. 1-2m,
  // Approximate formulas for the transformation between Swiss projection
  // coordinates and WGS84 - rein clientseitig ohne Serveraufruf, fuer die
  // Koordinatenanzeige beim Cursor.
  function wgs84ZuLv95(lon, lat) {
    var phiSek = (lat * 3600 - 169028.66) / 10000;
    var lambdaSek = (lon * 3600 - 26782.5) / 10000;
    var e = 2600072.37
      + 211455.93 * lambdaSek
      - 10938.51 * lambdaSek * phiSek
      - 0.36 * lambdaSek * Math.pow(phiSek, 2)
      - 44.54 * Math.pow(lambdaSek, 3);
    var n = 1200147.07
      + 308807.95 * phiSek
      + 3745.25 * Math.pow(lambdaSek, 2)
      + 76.63 * Math.pow(phiSek, 2)
      - 194.56 * Math.pow(lambdaSek, 2) * phiSek
      + 119.79 * Math.pow(phiSek, 3);
    return { e: e, n: n };
  }
  // Ortschaft/PLZ per swisstopo-Identify-Abfrage (amtliches Ortschaften-
  // verzeichnis) - im Gegensatz zu Koordinaten/Rasterwert (rein lokal) ein
  // echter Serveraufruf, daher verzoegert (erst wenn der Cursor kurz
  // stillsteht) statt bei jedem mousemove, um die oeffentliche API nicht
  // unnoetig oft anzufragen. Bei JEDER Cursorbewegung wird der alte Ort
  // sofort ausgeblendet (blinkende Punkte statt eines veralteten Werts) -
  // ortschaftAnfrageId veraltet dabei auch eine evtl. noch laufende
  // vorherige Anfrage sofort (deren Antwort koennte sonst NACH einer
  // neueren eintreffen und den Ort faelschlich wieder zuruecksetzen).
  var ortschaftAbfrageTimer = null;
  var ortschaftAnfrageId = 0;
  var LADE_PUNKTE_HTML = '<span class=gw-lade-punkte><span></span><span></span><span></span></span>';
  function sucheOrtschaftPlz(e, n, meineId) {
    var url = 'https://api3.geo.admin.ch/rest/services/api/MapServer/identify' +
      '?geometry=' + e + ',' + n + '&geometryType=esriGeometryPoint&imageDisplay=1,1,1' +
      '&mapExtent=' + e + ',' + n + ',' + e + ',' + n + '&tolerance=50' +
      '&layers=all:ch.swisstopo-vd.ortschaftenverzeichnis_plz&returnGeometry=false&sr=2056';
    fetch(url).then(function(r) { return r.json(); }).then(function(daten) {
      if (meineId !== ortschaftAnfrageId) return;
      if (!ortschaftEl) ortschaftEl = document.createElement('div');
      var treffer = daten && daten.results && daten.results[0];
      var ort = treffer ? treffer.attributes.plz + ' ' + treffer.attributes.langtext : 'ausserhalb der Schweiz';
      ortschaftEl.textContent = 'Ort: ' + ort;
      setzeOrtUndKoordinaten(null, ort);
    }).catch(function() {
      if (ortschaftEl && meineId === ortschaftAnfrageId) ortschaftEl.textContent = 'Ort: nicht abrufbar (offline?)';
      if (meineId === ortschaftAnfrageId) setzeOrtUndKoordinaten(null, 'Ort nicht abrufbar');
    });
  }
  // Positioniert den Pfeil auf dem Farbverlaufs-Balken proportional zum Wert
  // innerhalb legendeBereich ([min,max] der aktiven Ebene) - versteckt ihn,
  // wenn kein gueltiger Zahlenwert vorliegt (keine Daten/ausserhalb der
  // Schweiz/keine Ebene aktiv).
  function aktualisierePfeilPosition(wert) {
    if (!legendePfeilEl) return;
    if (wert === null || wert === undefined || isNaN(wert) || !legendeBereich) {
      legendePfeilEl.style.display = 'none';
      return;
    }
    var anteil = (wert - legendeBereich[0]) / (legendeBereich[1] - legendeBereich[0]);
    anteil = Math.max(0, Math.min(1, anteil));
    legendePfeilEl.style.left = (anteil * 100) + '%';
    legendePfeilEl.style.display = 'block';
  }
  // erzwingen: auch ohne aktive Ebene (Tippen/Gedrückthalten auf dem Handy:
  // Koordinaten und Ort); beim Mauszeiger ohne Ebene keine Ortsabfragen
  function zeigeWertAmPunkt(lon, lat, erzwingen) {
    if (!wertAnzeigeEl && !erzwingen) return;
    var lv95 = wgs84ZuLv95(lon, lat);
    if (koordinatenEl) {
      koordinatenEl.textContent = 'Koordinaten LV95: ' + Math.round(lv95.e) + ' / ' + Math.round(lv95.n);
    }
    if (ortschaftEl) ortschaftEl.innerHTML = 'Ort: ' + LADE_PUNKTE_HTML;
    setzeOrtUndKoordinaten(Math.round(lv95.e) + ' / ' + Math.round(lv95.n), LADE_PUNKTE_HTML);
    ortschaftAnfrageId++;
    clearTimeout(ortschaftAbfrageTimer);
    ortschaftAbfrageTimer = setTimeout(function() {
      var meineId = ++ortschaftAnfrageId;
      sucheOrtschaftPlz(lv95.e, lv95.n, meineId);
    }, 350);
    var gitterJeWoche = aktivesWerteGitter();
    var info = layerLegenden[hintergrundEbene];
    var gitter = gitterJeWoche ? gitterJeWoche[selectedYear + ' ' + selectedWeek] : null;
    if (!info) setzeWertText('Wert am Cursor: keine Ebene gewählt');
    if (!gitter || !info) { aktualisierePfeilPosition(null); return; }
    var col = Math.floor((lon - gitter.x0) / (gitter.x1 - gitter.x0) * gitter.ncol);
    var row = Math.floor((gitter.y1 - lat) / (gitter.y1 - gitter.y0) * gitter.nrow);
    if (col < 0 || col >= gitter.ncol || row < 0 || row >= gitter.nrow) {
      setzeWertText('Wert am Cursor: ' + (info.ausserhalb || 'ausserhalb der Schweiz'));
      aktualisierePfeilPosition(null);
      return;
    }
    var wert = gitter.m[row][col];
    setzeWertText((wert === null || wert === undefined) ?
      'Wert am Cursor: keine Daten' : 'Wert am Cursor: ' + wert + ' ' + info.einheit);
    aktualisierePfeilPosition(wert);
  }
  function versteckeWertAnzeige() {
    setzeWertText('Wert am Cursor: –');
    if (koordinatenEl) koordinatenEl.textContent = 'Koordinaten LV95: –';
    if (ortschaftEl) ortschaftEl.textContent = 'Ort: –';
    setzeOrtUndKoordinaten('', '');
    aktualisierePfeilPosition(null);
    ortschaftAnfrageId++;
    clearTimeout(ortschaftAbfrageTimer);
  }
  function verarbeiteKartenZeiger(evt) {
    var growthMapGd = document.querySelector('#datenexplorer-growthmap .js-plotly-plot');
    if (!growthMapGd) return;
    var fl = growthMapGd._fullLayout;
    if (!fl || !fl.xaxis || !fl.yaxis) return;
    var punkt = (evt.touches && evt.touches.length > 0) ? evt.touches[0] : evt;
    var rect = growthMapGd.getBoundingClientRect();
    var xPixel = punkt.clientX - rect.left;
    var yPixel = punkt.clientY - rect.top;
    var lon = fl.xaxis.p2d(xPixel - fl.xaxis._offset);
    var lat = fl.yaxis.p2d(yPixel - fl.yaxis._offset);
    zeigeWertAmPunkt(lon, lat);
  }

  // Karte auf Touch-Geraeten wie eine Karten-App (eingebettete Google-Karte):
  // - zwei Finger: zoomen und verschieben (waehrend der Geste per CSS-
  //   Transform verschoben/skaliert, erst beim Loslassen neu gezeichnet)
  // - Doppeltipp: vergroessern
  // - ein Finger: die Seite scrollt wie gewohnt (Hinweis «zwei Finger»)
  // - kurz tippen: Standort oeffnen, aber nur, wenn ein Kreis getroffen ist
  //   (kein «magnetisches» Einrasten auf den naechsten Standort); daneben
  //   Wert der Ebene mit Fadenkreuz
  // - gedrueckt halten (auch mit Ziehen): Lupe ueber dem Finger mit Wert,
  //   Ort und Koordinaten; das Fadenkreuz bleibt danach einige Sekunden
  var LANG_DRUCK_MS = 450, TIPP_TOLERANZ_PX = 10;
  var kartenGestenEnde = 0, standortGetipptZeit = 0;
  var lupeEl = null, kreuzEl = null, hinweisEl = null, kreuzTimer = null, hinweisTimer = null;
  function gestenElemente() {
    if (lupeEl) return;
    lupeEl = document.createElement('div'); lupeEl.className = 'gw-lupe';
    kreuzEl = document.createElement('div'); kreuzEl.className = 'gw-fadenkreuz';
    hinweisEl = document.createElement('div'); hinweisEl.className = 'gw-karte-hinweis';
    hinweisEl.textContent = 'Zum Verschieben und Zoomen zwei Finger verwenden';
    document.body.appendChild(lupeEl); document.body.appendChild(kreuzEl);
  }
  function aktualisiereLupe() {
    if (!lupeEl || !lupeEl.classList.contains('sichtbar')) return;
    lupeEl.innerHTML = '<b>' + (wertKurzText && wertKurzText !== '–' ? wertKurzText : '') + '</b>' +
      (ortText && ortText !== wertKurzText ? '<span>' + ortText + '</span>' : '') + (koordText ? '<small>' + koordText + '</small>' : '');
  }
  function zeigeKreuz(clientX, clientY, mitLupe) {
    gestenElemente();
    clearTimeout(kreuzTimer);
    kreuzEl.style.left = clientX + 'px'; kreuzEl.style.top = clientY + 'px';
    kreuzEl.classList.add('sichtbar');
    if (mitLupe) {
      lupeEl.classList.add('sichtbar');
      lupeEl.style.left = Math.max(80, Math.min(window.innerWidth - 80, clientX)) + 'px';
      lupeEl.style.top = Math.max(8, clientY - 96) + 'px';
      aktualisiereLupe();
    }
  }
  // Fadenkreuz (und Lupe) nach einigen Sekunden ausblenden
  function kreuzSpaeterWeg(ms) {
    clearTimeout(kreuzTimer);
    kreuzTimer = setTimeout(versteckeKreuz, ms);
  }
  function versteckeKreuz() {
    clearTimeout(kreuzTimer);
    if (kreuzEl) kreuzEl.classList.remove('sichtbar');
    if (lupeEl) lupeEl.classList.remove('sichtbar');
  }
  window.addEventListener('scroll', function() { if (kreuzEl && kreuzEl.classList.contains('sichtbar')) versteckeKreuz(); }, { passive: true });
  function zeigeHinweis(gd) {
    gestenElemente();
    if (!hinweisEl.parentNode) document.body.appendChild(hinweisEl);
    var r = gd.getBoundingClientRect();
    hinweisEl.style.left = (r.left + r.width / 2) + 'px';
    hinweisEl.style.top = (Math.max(0, r.top) + Math.min(r.bottom, window.innerHeight)) / 2 + 'px';
    hinweisEl.classList.add('sichtbar');
    clearTimeout(hinweisTimer);
    hinweisTimer = setTimeout(function() { hinweisEl.classList.remove('sichtbar'); }, 1600);
  }
  function bindeKartenGesten(gd) {
    if (istTouch()) {
      // kein Rechteck-Zoom per Finger, Treffer nur am Kreis (Abstand zum
      // Kreisrand in px), Doppeltipp macht die Karte selbst
      Plotly.relayout(gd, { dragmode: false, hoverdistance: 4 });
      if (gd._context) gd._context.doubleClick = false;
    }
    var g = null, letzterTipp = null;
    var rel = function(t) { var r = gd.getBoundingClientRect(); return { x: t.clientX - r.left, y: t.clientY - r.top, cx: t.clientX, cy: t.clientY }; };
    var mitte = function(a, b) { return { x: (a.x + b.x) / 2, y: (a.y + b.y) / 2 }; };
    var abstand = function(a, b) { return Math.hypot(a.x - b.x, a.y - b.y); };
    var ebene = function() { return gd.querySelector('.svg-container'); };
    function datenPunkt(p) {
      var fl = gd._fullLayout;
      return { lon: fl.xaxis.p2d(p.x - fl.xaxis._offset), lat: fl.yaxis.p2d(p.y - fl.yaxis._offset) };
    }
    function werteAn(p, mitLupe) {
      var d = datenPunkt(p);
      zeigeWertAmPunkt(d.lon, d.lat, true);
      zeigeKreuz(p.cx, p.cy, mitLupe);
    }
    // Grenzen: nicht kleiner als die ganze Schweiz, hoechstens 40-fach gross
    function grenzen() {
      var ki = GWDatenexplorer.kartenInfo, xa = gd._fullLayout.xaxis;
      var jetzt = Math.abs(xa.range[1] - xa.range[0]), voll = ki ? ki.xMaxSchweiz - ki.xMin : jetzt;
      return { sMin: Math.min(1, jetzt / voll), sMax: jetzt / (voll / 40), ki: ki };
    }
    function transformiere() {
      var el = ebene(); if (!el) return;
      var tx = g.m.x - g.s * g.m0.x, ty = g.m.y - g.s * g.m0.y;
      el.style.transformOrigin = '0 0';
      el.style.transform = 'translate(' + tx + 'px,' + ty + 'px) scale(' + g.s + ')';
      g.raf = null;
    }
    function zoomeAuf(x0, x1, yUnten, yOben) {
      var ki = GWDatenexplorer.kartenInfo;
      if (ki) {
        // Mitte bleibt ueber der Schweiz (kein Wegschieben ins Leere)
        var mx = (x0 + x1) / 2, my = (yUnten + yOben) / 2;
        var dx = Math.max(ki.xMin, Math.min(ki.xMaxSchweiz, mx)) - mx;
        var dy = Math.max(ki.yMitte - ki.ySpanSchweiz / 2, Math.min(ki.yMitte + ki.ySpanSchweiz / 2, my)) - my;
        x0 += dx; x1 += dx; yUnten += dy; yOben += dy;
      }
      return Plotly.relayout(gd, { 'xaxis.range': [x0, x1], 'yaxis.range': [yUnten, yOben] });
    }
    function beendeZwei() {
      var el = ebene(), fl = gd._fullLayout, xa = fl.xaxis, ya = fl.yaxis;
      var s = g.s, tx = g.m.x - s * g.m0.x, ty = g.m.y - s * g.m0.y;
      if (g.raf) cancelAnimationFrame(g.raf);
      kartenGestenEnde = Date.now();
      var zurueck = function() { if (el) el.style.transform = ''; };
      if (Math.abs(s - 1) < 0.01 && Math.hypot(tx, ty) < 3) { zurueck(); return; }
      // Bildschirmpunkt q zeigte vor der Geste den Punkt (q - t) / s
      var alt = function(ax, q, t) { return ax.p2d((q - t) / s - ax._offset); };
      zoomeAuf(alt(xa, xa._offset, tx), alt(xa, xa._offset + xa._length, tx),
        alt(ya, ya._offset + ya._length, ty), alt(ya, ya._offset, ty)).then(zurueck, zurueck);
    }
    function zoomeUm(p, faktor) {
      var xa = gd._fullLayout.xaxis, ya = gd._fullLayout.yaxis, d = datenPunkt(p);
      var gr = grenzen(), f = Math.min(faktor, gr.sMax);
      if (f <= 1.01) return;
      zoomeAuf(d.lon - (d.lon - xa.range[0]) / f, d.lon + (xa.range[1] - d.lon) / f,
        d.lat - (d.lat - ya.range[0]) / f, d.lat + (ya.range[1] - d.lat) / f);
    }
    gd.addEventListener('touchstart', function(evt) {
      if (evt.touches.length >= 2) {
        // zwei Finger gehoeren der Karte (nicht Plotly, nicht der Seite)
        evt.preventDefault(); evt.stopPropagation();
        if (g && g.timer) clearTimeout(g.timer);
        versteckeKreuz();
        var a = rel(evt.touches[0]), b = rel(evt.touches[1]), gr = grenzen();
        g = { art: 'zwei', m0: mitte(a, b), d0: Math.max(10, abstand(a, b)), m: mitte(a, b), s: 1, sMin: gr.sMin, sMax: gr.sMax };
        gd.style.overflow = 'hidden';
        return;
      }
      if (g && g.art === 'zwei') return;
      var p = rel(evt.touches[0]);
      g = { art: 'eins', p0: p, p: p, t0: Date.now(), lang: false, bewegt: false };
      g.timer = setTimeout(function() {
        if (!g || g.art !== 'eins' || g.bewegt) return;
        g.lang = true;
        if (navigator.vibrate) { try { navigator.vibrate(12); } catch (e) {} }
        werteAn(g.p, true);
      }, LANG_DRUCK_MS);
    }, { capture: true, passive: false });
    gd.addEventListener('touchmove', function(evt) {
      if (!g) return;
      if (g.art === 'zwei') {
        evt.preventDefault(); evt.stopPropagation();
        if (evt.touches.length < 2) return;
        var a = rel(evt.touches[0]), b = rel(evt.touches[1]);
        g.m = mitte(a, b);
        g.s = Math.max(g.sMin, Math.min(g.sMax, abstand(a, b) / g.d0));
        if (!g.raf) g.raf = requestAnimationFrame(transformiere);
        return;
      }
      g.p = rel(evt.touches[0]);
      if (g.lang) { evt.preventDefault(); werteAn(g.p, true); return; }
      if (!g.bewegt && abstand(g.p, g.p0) > TIPP_TOLERANZ_PX) {
        g.bewegt = true; clearTimeout(g.timer);
        zeigeHinweis(gd);
      }
    }, { capture: true, passive: false });
    function ende(evt) {
      if (!g) return;
      if (g.art === 'zwei') {
        evt.preventDefault();
        if (evt.touches.length === 0) { beendeZwei(); g = null; }
        return;
      }
      clearTimeout(g.timer);
      if (g.lang) {
        evt.preventDefault();
        kartenGestenEnde = Date.now();
        kreuzSpaeterWeg(4000);
        g = null;
        return;
      }
      if (!g.bewegt && evt.type === 'touchend') {
        var p = g.p0, jetzt = Date.now();
        if (letzterTipp && jetzt - letzterTipp.t < 320 && abstand(p, letzterTipp.p) < 30) {
          evt.preventDefault();
          letzterTipp = null; kartenGestenEnde = jetzt;
          versteckeKreuz();
          zoomeUm(p, 2);
        } else {
          letzterTipp = { t: jetzt, p: p };
          var tipp = letzterTipp;
          // Kurz getippt: hat Plotly einen Standort getroffen (plotly_click),
          // oeffnet der sein Blatt; sonst Wert an dieser Stelle
          setTimeout(function() {
            if (letzterTipp !== tipp || standortGetipptZeit >= tipp.t) return;
            werteAn(p, true);
            kreuzSpaeterWeg(4000);
          }, 340);
        }
      }
      g = null;
    }
    gd.addEventListener('touchend', ende, { capture: true, passive: false });
    gd.addEventListener('touchcancel', function(evt) {
      if (g && g.art === 'zwei') { var el = ebene(); if (el) el.style.transform = ''; }
      if (g && g.timer) clearTimeout(g.timer);
      g = null;
    }, { capture: true });
  }
  if (mapControlsContainer) {
    var layerPanel = document.createElement('div');
    layerPanel.className = 'gw-layer-panel';
    var layerHeading = document.createElement('div');
    layerHeading.className = 'gw-layer-heading';
    layerHeading.textContent = 'Ebenen';
    var ebenenZuZeile = document.createElement('div');
    ebenenZuZeile.className = 'gw-ebenen-zu-zeile gw-mobil-only';
    var ebenenZu = document.createElement('button'); ebenenZu.type = 'button'; ebenenZu.className = 'gw-blatt-zu'; ebenenZu.textContent = '×';
    ebenenZu.setAttribute('aria-label', 'Ebenen schliessen');
    ebenenZu.addEventListener('click', function(evt) { evt.stopPropagation(); document.body.classList.remove('gw-ebenen-offen'); });
    ebenenZuZeile.appendChild(layerHeading); ebenenZuZeile.appendChild(ebenenZu);
    layerPanel.appendChild(ebenenZuZeile);
    var layerHeadingDesktop = layerHeading.cloneNode(true);
    layerHeadingDesktop.classList.add('gw-desktop-only');
    layerPanel.insertBefore(layerHeadingDesktop, ebenenZuZeile);

    // PLZ/Ort-Suche: swisstopo-SearchServer (dieselbe oeffentliche API wie
    // fuer die Cursor-Ortsabfrage) liefert Vorschlaege waehrend des Tippens
    // (origins=zipcode,gg25 deckt Postleitzahlen UND Gemeindenamen ab).
    // Bei Auswahl (Klick oder Enter) wird ein Fadenkreuz-Marker auf die
    // Karte gesetzt und per Plotly.Fx.hover() dessen Tooltip (Ortsname,
    // Ebenen-Wert an dieser Stelle, LV95-Koordinaten) sofort angezeigt -
    // zusaetzlich aktualisiert sich das normale Cursor-Anzeigefeld gleich
    // mit (zeigeWertAmPunkt()).
    var sucheWrap = document.createElement('div');
    sucheWrap.className = 'gw-combo';
    sucheWrap.style.marginBottom = '10px';
    sucheWrap.style.display = 'block';
    var sucheInput = document.createElement('input');
    sucheInput.type = 'text';
    sucheInput.placeholder = 'PLZ oder Ort suchen…';
    sucheInput.style.width = '100%';
    var sucheListe = document.createElement('div');
    sucheListe.className = 'gw-combo-list';
    sucheListe.style.display = 'none';
    sucheListe.style.width = '100%';
    sucheWrap.appendChild(sucheInput);
    sucheWrap.appendChild(sucheListe);
    layerPanel.appendChild(sucheWrap);

    var sucheTimer = null;
    var sucheErgebnisse = [];
    function ortsLabelKlartext(treffer) { return treffer.attrs.label.replace(new RegExp('</?b>', 'g'), ''); }
    function sucheOrteVorschlaege(text) {
      if (!text || text.length < 2) { sucheListe.style.display = 'none'; return; }
      var url = 'https://api3.geo.admin.ch/rest/services/api/SearchServer' +
        '?searchText=' + encodeURIComponent(text) + '&type=locations&origins=zipcode,gg25&limit=8&sr=2056';
      fetch(url).then(function(r) { return r.json(); }).then(function(daten) {
        sucheErgebnisse = (daten && daten.results) || [];
        sucheListe.innerHTML = '';
        sucheErgebnisse.forEach(function(treffer) {
          var item = document.createElement('div');
          item.className = 'gw-combo-item';
          item.textContent = ortsLabelKlartext(treffer);
          item.addEventListener('mousedown', function(evt) {
            evt.preventDefault();
            waehleSucheErgebnis(treffer);
          });
          sucheListe.appendChild(item);
        });
        sucheListe.style.display = sucheErgebnisse.length > 0 ? 'block' : 'none';
      }).catch(function() { sucheListe.style.display = 'none'; });
    }
    function waehleSucheErgebnis(treffer) {
      sucheInput.value = ortsLabelKlartext(treffer);
      sucheListe.style.display = 'none';
      platziereFadenkreuz(treffer.attrs.lon, treffer.attrs.lat, ortsLabelKlartext(treffer));
    }
    sucheInput.addEventListener('input', function() {
      clearTimeout(sucheTimer);
      var text = sucheInput.value;
      sucheTimer = setTimeout(function() { sucheOrteVorschlaege(text); }, 300);
    });
    sucheInput.addEventListener('keydown', function(evt) {
      if (evt.key === 'Enter' && sucheErgebnisse.length > 0) {
        evt.preventDefault();
        waehleSucheErgebnis(sucheErgebnisse[0]);
      }
    });
    sucheInput.addEventListener('blur', function() {
      setTimeout(function() { sucheListe.style.display = 'none'; }, 150);
    });

    // Setzt (bzw. verschiebt) den Fadenkreuz-Marker auf lon/lat und zeigt
    // per Plotly.Fx.hover() sofort dessen Tooltip - der Ortsname kommt
    // direkt aus dem Suchtreffer (keine erneute Ortsabfrage noetig, wir
    // haben ja gerade danach gesucht), Ebenen-Wert/Koordinaten wie beim
    // Cursor-Anzeigefeld berechnet.
    function platziereFadenkreuz(lon, lat, ortsName) {
      var growthMapGd = document.querySelector('#datenexplorer-growthmap .js-plotly-plot');
      if (!growthMapGd) return;
      var lv95 = wgs84ZuLv95(lon, lat);
      var zeilen = ['<b>' + ortsName + '</b>'];
      var gitterJeWoche = aktivesWerteGitter();
      var info = layerLegenden[hintergrundEbene];
      var gitter = gitterJeWoche ? gitterJeWoche[selectedYear + ' ' + selectedWeek] : null;
      if (gitter && info) {
        var col = Math.floor((lon - gitter.x0) / (gitter.x1 - gitter.x0) * gitter.ncol);
        var row = Math.floor((gitter.y1 - lat) / (gitter.y1 - gitter.y0) * gitter.nrow);
        if (col >= 0 && col < gitter.ncol && row >= 0 && row < gitter.nrow) {
          var wert = gitter.m[row][col];
          zeilen.push((wert === null || wert === undefined) ? 'Wert: keine Daten' : 'Wert: ' + wert + ' ' + info.einheit);
        } else {
          zeilen.push('Wert: ausserhalb der Schweiz');
        }
      }
      zeilen.push('Koordinaten LV95: ' + Math.round(lv95.e) + ' / ' + Math.round(lv95.n));
      var hoverText = zeilen.join('<br>');
      if (fadenkreuzTraceIdx === null) {
        Plotly.addTraces(growthMapGd, {
          x: [lon], y: [lat], type: 'scatter', mode: 'markers',
          marker: { symbol: 'cross-thin-open', size: 26, color: '#e6194b', line: { width: 2.5 } },
          hoverinfo: 'text', hovertext: [hoverText], showlegend: false, name: 'Suche'
        });
        fadenkreuzTraceIdx = growthMapGd.data.length - 1;
      } else {
        Plotly.restyle(growthMapGd, { x: [[lon]], y: [[lat]], hovertext: [[hoverText]], visible: [true] }, [fadenkreuzTraceIdx]);
      }
      Plotly.Fx.hover(growthMapGd, [{ curveNumber: fadenkreuzTraceIdx, pointNumber: 0 }]);
      zeigeWertAmPunkt(lon, lat);
    }

    // Kleiner i-Knopf mit Klapp-Popup fuer laengere Erklaerungstexte (die
    // Quellenangabe als nativer title-Tooltip reicht fuer eine ganze
    // Absatz-Erklaerung nicht) - per Klick statt nur Hover, damit es auch
    // auf Touch-Geraeten funktioniert; ein Klick ausserhalb schliesst das
    // Popup wieder.
    function macheInfoKnopf(text, zusatz, titel) {
      var eintrag = dokuFinde(titel) || dokuNeu(titel, text, zusatz);
      if (zusatz && !eintrag.legende) eintrag.legende = zusatz;
      var wrap = document.createElement('span');
      wrap.className = 'gw-info-wrap';
      var btn = document.createElement('button');
      btn.type = 'button';
      btn.className = 'gw-info-btn';
      btn.textContent = 'i';
      btn.title = 'Erklärung: ' + eintrag.titel;
      btn.setAttribute('aria-label', btn.title);
      btn.addEventListener('click', function(evt) {
        evt.preventDefault();
        evt.stopPropagation();
        oeffneDoku(eintrag.id, btn);
      });
      wrap.appendChild(btn);
      return wrap;
    }

    // makeToggle() setzt normalerweise Text VOR den Schalter (so in der
    // Kurven-Legende gewuenscht) - im Ebenen-Kasten sollen alle Schalter
    // wie die Radiobuttons darunter linksbuendig ausgerichtet sein
    // (Schalter/Radio links, Text rechts daneben), deshalb hier vertauscht.
    function schalterLinksbuendig(toggleWrap) {
      if (toggleWrap.children.length === 2) toggleWrap.insertBefore(toggleWrap.children[1], toggleWrap.children[0]);
      return toggleWrap;
    }

    // Wie makeLayerRadio() unten, nur fuer einen Umschalter (Toggle) statt
    // eines Radiobuttons - fuer Graswachstum/DGV/MeteoSchweiz-Stationen, die
    // (anders als die Hintergrund-Raster-Ebenen) unabhaengig VONEINANDER
    // ein-/ausblendbar sein sollen, nicht als Radiogruppe.
    function macheLayerToggle(labelText, checked, onChange, erklaerung, zusatzKlasse, zusatzInfo) {
      var zeile = document.createElement('div');
      zeile.className = 'gw-layer-option-zeile' + (zusatzKlasse ? ' ' + zusatzKlasse : '');
      var toggleWrap = schalterLinksbuendig(makeToggle(labelText, checked, onChange));
      zeile.appendChild(toggleWrap);
      if (erklaerung) zeile.appendChild(macheInfoKnopf(erklaerung, zusatzInfo, labelText));
      layerPanel.appendChild(zeile);
      return toggleWrap;
    }
    // Reihenfolge Graswachstum / DGV / MeteoSchweiz-Stationen: die beiden
    // Betriebs-Ebenen (Graswachstum-Kreis, DGV-Ring) zuerst, je eigener
    // Schalter - danach MeteoSchweiz-Stationen als reine Wetter-
    // Referenzebene. DGV = Durchschnittlicher GrasVorrat, der intern/in der
    // Erklaerung weiterhin als AFC (Average Farm Cover) referenzierte
    // Fachbegriff.
    toggleGraswachstum = macheLayerToggle('Graswachstum (kg TS/ha/Tag)', true, function(checked) { graswachstumOn = checked; applyState(); },
      'Die Zahl im Kreis zeigt das zuletzt gemessene Graswachstum in kg TS/ha/Tag (Trockensubstanz-Zuwachs pro Hektare und Tag). Die Graufaerbung des Kreises zeigt, wie lange die Messung zurueckliegt: weiss = frisch gemessen (0 Tage), dunkelgrau = bis zu 14 Tage alt. Standorte ohne Messung in den letzten 14 Tagen werden nicht mehr angezeigt.');
    toggleDgv = macheLayerToggle('DGV (kg TS/ha)', true, function(checked) { afcOn = checked; applyState(); },
      'DGV (Durchschnittlicher GrasVorrat, international AFC = Average Farm Cover) schaetzt den aktuellen Grasvorrat des Betriebs in kg Trockensubstanz pro Hektare (kg TS/ha). Der Ring zeigt diesen Vorrat als Fortschrittsbalken auf einer Skala von 0 bis 1500 kg TS/ha und faerbt ihn nach dem jahreszeitlichen Zielbereich: rot = deutlich zu wenig (unter 200 kg praktisch leer), gruen = im Zielbereich, blaugruen = deutlich mehr als noetig. Der Zielbereich verschiebt sich uebers Jahr, z.B. Fruehling ca. 500-700, Sommer ca. 700-800, Herbst ca. 900-1200 kg TS/ha.', null,
      function() { if (!afcLegendeBox || afcLegendeBox.style.display === 'none') return null; var c = afcLegendeBox.cloneNode(true); c.className = 'gw-layer-legende'; c.style.display = 'block'; c.style.marginBottom = '10px'; return c; });

    // Kompakte DGV-Legende (Ring) DIREKT nach dem DGV-Schalter - gehoert
    // inhaltlich dazu. aktualisiereAfcLegende() (siehe unten) blendet die
    // Box aus, sobald DGV ausgeschaltet ist.
    // DGV-Ring und Tage-seit-Messung nebeneinander in einer Zeile
    var messLegendeZeile = document.createElement('div');
    messLegendeZeile.className = 'gw-mess-legende';
    layerPanel.appendChild(messLegendeZeile);
    afcLegendeBox = document.createElement('div');
    afcLegendeBox.className = 'gw-afc-legende-box';
    afcLegendeBox.style.display = 'none';
    messLegendeZeile.appendChild(afcLegendeBox);

    // "Tage seit Messung" (Graufaerbung Graswachstum-Kreis/AFC-Ring) - statt
    // eines Plotly-nativen Colorbars auf der Karte (kollidierte dort mit dem
    // Hover-Modebar-Bereich) als kompakte, statische Box direkt hier neben
    // Graswachstum/DGV. Inhalt aendert sich nie (fixe Skala 0-14 Tage), daher
    // einmalig aufgebaut statt bei jedem Wochenwechsel neu gerendert.
    tageSeitMessungBox = document.createElement('div');
    tageSeitMessungBox.className = 'gw-tage-legende';
    tageSeitMessungBox.style.display = 'none';
    tageSeitMessungBox.title = 'Graufärbung von Kreis und Ring: Tage seit der letzten Messung';
    var tsmBalken = document.createElement('div');
    tsmBalken.className = 'gw-tage-balken';
    var tsmText = document.createElement('div');
    tsmText.className = 'gw-tage-text';
    ['0 Tage', 'seit Messung', '14 Tage'].forEach(function(t, i) {
      var sp = document.createElement('span'); sp.textContent = t;
      if (i === 1) sp.className = 'gw-tage-mitte';
      tsmText.appendChild(sp);
    });
    tageSeitMessungBox.appendChild(tsmBalken);
    tageSeitMessungBox.appendChild(tsmText);
    messLegendeZeile.appendChild(tageSeitMessungBox);

    toggleStationen = macheLayerToggle('MeteoSchweiz-Stationen', false, function(checked) { smnStationenOn = checked; aktualisiereSmnStationen(); },
      'Zeigt die oeffentlichen MeteoSchweiz-Automatikstationen (SwissMetNet) mit ihren aktuellsten Tageswerten (Lufttemperatur, Bodentemperatur, Niederschlag, Globalstrahlung, Sonnenscheindauer) als Diamant-Symbole. Reine Wetter-Referenzstationen, unabhaengig von der gewaehlten Kalenderwoche und NICHT Teil der AGFF-Grasmessungen. Bodentemperatur wird nur an einem Teil der rund 150 Stationen gemessen - dort steht im Tooltip entsprechend keine Daten.');

    // Eigene Orte (erfasst im Export unter «Karte für eine Veranstaltung»):
    // Schalter nur sichtbar, wenn die Liste Orte enthaelt
    var orteToggle = macheLayerToggle('Eigene Orte', orteAnzeigen, function(checked) { setzeOrteAnzeigen(checked); },
      'Zeigt die eigenen Orte (z.B. Betriebe) als Marker mit Namen. Erfassen, umbenennen und entfernen lassen sie sich unter «Exportieren» › «Karte für eine Veranstaltung»; dort entstehen auch Karten mit Werten für diese Orte als PDF, SVG oder HTML.');
    orteHoerer.push(function() {
      orteToggle.parentNode.style.display = eigeneOrte.length ? '' : 'none';
      var cb = orteToggle.querySelector('input[type=checkbox]');
      if (cb) cb.checked = orteAnzeigen;
    });
    orteToggle.parentNode.style.display = eigeneOrte.length ? '' : 'none';

    // Schieberegler fuer die Fenstergroesse (Tage) der gleitendes-Fenster-
    // Ebenen (meteoFensterEbenen, siehe oben) - wird OBERHALB der Legende
    // eingefuegt (layerPanel.appendChild() hier laeuft VOR dem der Legende
    // weiter unten) und ist nur sichtbar, waehrend eine Fenster-Ebene aktiv
    // ist (nicht bei keine Meteodaten, Bodenwasserbilanz, Wachstumsgrad-
    // tage - siehe aktualisiereMeteoFensterSichtbarkeit()).
    var meteoFensterWrap = null, meteoFensterInput = null, meteoFensterLabel = null;
    function macheMeteoFensterSchieberegler() {
      meteoFensterWrap = document.createElement('div');
      meteoFensterWrap.className = 'gw-meteo-fenster';
      meteoFensterLabel = document.createElement('div');
      meteoFensterLabel.className = 'gw-meteo-fenster-label';
      meteoFensterInput = document.createElement('input');
      meteoFensterInput.type = 'range';
      meteoFensterInput.min = '0';
      meteoFensterInput.max = String(meteoFensterStufen.length - 1);
      meteoFensterInput.step = '1';
      meteoFensterInput.addEventListener('input', function() {
        meteoFenster = meteoFensterStufen[parseInt(meteoFensterInput.value, 10)];
        aktualisiereMeteoFensterAnzeige();
        aktualisiereHintergrundEbene();
        aktualisiereLayerLegende();
      });
      meteoFensterWrap.appendChild(meteoFensterLabel);
      meteoFensterWrap.appendChild(meteoFensterInput);
      layerPanel.appendChild(meteoFensterWrap);
      aktualisiereMeteoFensterAnzeige();
      aktualisiereMeteoFensterSichtbarkeit();
    }
    function aktualisiereMeteoFensterAnzeige() {
      if (meteoFensterInput) meteoFensterInput.value = String(meteoFensterStufen.indexOf(meteoFenster));
      if (meteoFensterLabel) meteoFensterLabel.textContent = 'Zeitraum: ' + meteoFenster + ' Tage';
      aktualisiereLayerLabels();
    }
    function aktualisiereMeteoFensterSichtbarkeit() {
      if (meteoFensterWrap) meteoFensterWrap.style.display = istFensterEbene(hintergrundEbene) ? 'block' : 'none';
      if (erholungWrap) erholungWrap.style.display = istErholungsEbene(hintergrundEbene) ? 'block' : 'none';
    }
    var erholungWrap = null;
    function macheErholungsSchieberegler() {
      erholungWrap = document.createElement('div');
      erholungWrap.className = 'gw-meteo-fenster';
      erholungWrap.title = 'Eigene Erweiterung, nicht Teil von ModVege: Nach Trockenheit steigt der Wasserstress-Faktor fuers Wachstum hoechstens so schnell, dass die volle Erholung diese Anzahl Tage dauert. 0 = unveraendertes Modell (springt nach Regen sofort zurueck).';
      var label = document.createElement('div');
      label.className = 'gw-meteo-fenster-label';
      var input = document.createElement('input');
      input.type = 'range';
      input.min = '0';
      input.max = String(erholungStufen.length - 1);
      input.step = '1';
      input.value = String(erholungStufen.indexOf(erholung));
      function zeige() { label.textContent = 'Erholung nach Trockenheit: ' + erholung + ' Tage'; }
      input.addEventListener('input', function() {
        erholung = erholungStufen[parseInt(input.value, 10)];
        zeige();
        aktualisiereHintergrundEbene();
        aktualisiereLayerLegende();
      });
      erholungWrap.appendChild(label);
      erholungWrap.appendChild(input);
      layerPanel.appendChild(erholungWrap);
      zeige();
      aktualisiereMeteoFensterSichtbarkeit();
    }

    // title (nativer Browser-Tooltip) je Option mit der Quellenangabe, wie
    // einst als Untertitel bei den Export-Grafiken (siehe layerLegenden.quelle).
    // erklaerung (optional): zusaetzlicher i-Knopf mit laengerem Klartext.
    function makeLayerRadio(value, labelText, erklaerung, zusatzKlasse) {
      var zeile = document.createElement('div');
      zeile.className = 'gw-layer-option-zeile' + (zusatzKlasse ? ' ' + zusatzKlasse : '');
      var wrap = document.createElement('label');
      wrap.className = 'gw-layer-option';
      if (layerLegenden[value]) wrap.title = 'Quelle: ' + layerLegenden[value].quelle;
      var radio = document.createElement('input');
      radio.type = 'radio';
      radio.name = 'gw-layer';
      radio.value = value;
      radio.checked = (value === 'keine');
      radio.addEventListener('change', function() {
        if (!radio.checked) return;
        hintergrundEbene = value;
        // Schieberegler-Fenstergroesse springt bei JEDEM Ebenenwechsel auf
        // den fuer die neue Ebene hinterlegten Standard zurueck (siehe
        // meteoFensterStandard oben) - kein Merken eines individuellen
        // Werts je Ebene.
        if (istFensterEbene(value)) meteoFenster = meteoFensterStandard[value];
        aktualisiereMeteoFensterAnzeige();
        aktualisiereHintergrundEbene();
        aktualisiereLayerLegende();
      });
      var text = document.createElement('span');
      text.textContent = labelText;
      radio.labelTextEl = text;
      wrap.appendChild(radio);
      wrap.appendChild(text);
      zeile.appendChild(wrap);
      if (erklaerung) zeile.appendChild(macheInfoKnopf(erklaerung, null, labelText));
      layerPanel.appendChild(zeile);
      return radio;
    }
    // Ueberschrift statt Trennlinie: macht den Abschnittswechsel von den
    // Standort-/Stations-Schaltern oben zu den flaechendeckenden MeteoSchweiz-
    // Gitterdaten-Ebenen klar, ohne zusaetzlich eine Trennlinie zu brauchen.
    // "Gitterdatensatz" ist MeteoSchweiz' eigener Fachbegriff fuer diese
    // raeumlich interpolierten Produkte (RhiresD/TabsD/SrelD/...).
    var meteoGitterHeading = document.createElement('div');
    meteoGitterHeading.className = 'gw-layer-heading';
    meteoGitterHeading.style.marginTop = '10px';
    meteoGitterHeading.textContent = 'MeteoSchweiz-Gitterdaten';
    layerPanel.appendChild(meteoGitterHeading);
    makeLayerRadio('keine', 'keine Meteodaten');
    radioNiederschlag = makeLayerRadio('niederschlag', layerLegenden.niederschlag.label, 'siehe Hilfe');
    if (layerLegenden.niederschlag_kum) radioNiederschlagKum = makeLayerRadio('niederschlag_kum', layerLegenden.niederschlag_kum.label, 'siehe Hilfe');
    radioTemperatur = makeLayerRadio('temperatur', layerLegenden.temperatur.label,
      'Mittlere Lufttemperatur (2m) im oben gewaehlten Zeitraum vor dem Stichtag. Graswachstum beginnt erst ab einer Basistemperatur von ca. 5 Grad C spuerbar (darunter praktisch Wachstumsstillstand), das Optimum liegt bei ca. 15-20 Grad C. Ueber ca. 25 Grad C bremst Hitzestress das Wachstum trotz ausreichend Wasser wieder. Als Faustregel fuer den Wachstumsantrieb ueber mehrere Tage dient die Wachstumsgradtagsumme: Summe aus (Tagesmitteltemperatur minus 5 Grad C) an allen Tagen mit Werten darueber.');
    radioBodentemperatur = makeLayerRadio('bodentemperatur', layerLegenden.bodentemperatur.label,
      'ACHTUNG SCHAETZUNG, keine Feldmessung: MeteoSchweiz misst Bodentemperatur nur an einzelnen Stationen, nicht flaechendeckend als Karte. Gezeigt wird stattdessen der gleitende Mittelwert der Lufttemperatur (2m) im oben gewaehlten Zeitraum - eine grobe Naeherung an die traegere, gedaempfte oberste Bodenschicht (ca. 5-10cm); ein laengerer Zeitraum simuliert mehr Daempfung. Bodentemperatur ist u.a. fuer den Vegetationsbeginn im Fruehling und die Stickstoff-Mineralisierung im Boden relevant: beides kommt unter ca. 5-8 Grad C weitgehend zum Erliegen.');
    radioSonnenschein = makeLayerRadio('sonnenschein', layerLegenden.sonnenschein.label,
      'Sonnenscheindauer im oben gewaehlten Zeitraum vor dem Stichtag, relativ zur astronomisch maximal moeglichen Tagesdauer (0-100%, MeteoSchweiz SrelD). Mehr Sonne treibt die Photosynthese und damit das Wachstum an, erhoeht aber auch die Verdunstung (siehe ET0/Bodenwasserbilanz). Diese Daten werden erst mit 1-2 Monaten Verzoegerung aufbereitet - die allerneuesten Wochen sind deshalb oft noch nicht verfuegbar.');
    radioEt0 = makeLayerRadio('et0', layerLegenden.et0.label,
      'Potenzielle Verdunstung (Evapotranspiration) nach der Hargreaves-Formel (FAO-56), Summe im oben gewaehlten Zeitraum vor dem Stichtag - dieselbe Berechnung, die auch ins Bucket-Modell der Bodenwasserbilanz einfliesst. Zeigt, wie viel Wasser dem Boden allein durch Verdunstung entzogen wird: hohe Werte bei gleichzeitig wenig Niederschlag beguenstigen Trockenstress.');
    radioGdd = makeLayerRadio('gdd', layerLegenden.gdd.label,
      'Kumulierte Wachstumsgradtage seit Beginn der lokal vorhandenen Temperaturdaten: Summe aus (Tagesmitteltemperatur minus 5 Grad C) an allen Tagen mit Werten darueber, laufend aufaddiert (MeteoSchweiz TabsD). Eine in der Agronomie gebraeuchliche Faustregel fuer die pflanzenverfuegbare Waermesumme seit Vegetationsbeginn - hoehere Werte bedeuten mehr angesammelte Wachstumsbedingungen.');
    // Bodenwasserbilanz ganz am Schluss, mit Trennlinie abgesetzt: anders
    // als die anderen Ebenen (direkte MeteoSchweiz-Messwerte/-Aggregate) ist
    // dies eine SELBST BERECHNETE Groesse (Eimer-Modell aus Niederschlag +
    // ET0, siehe Erklaerung) - das Label macht das zusaetzlich explizit.
    radioBoden = makeLayerRadio('boden', layerLegenden.boden.label,
      'Der Boden wird vereinfacht wie ein Eimer betrachtet: Regen fuellt ihn, Verdunstung leert ihn. Ist der Eimer voll, laeuft der Ueberschuss ungenutzt ab. Wie viel taeglich verdunstet, wird aus den Temperaturen geschaetzt - ein feuchter Boden verdunstet mehr als ein bereits trockener. Der Wert zeigt den aktuellen Fuellstand: 130 mm = Boden gut mit Wasser versorgt, 0 mm = ausgetrocknet.',
      'gw-layer-vor-boden');
    // Vergleich mit dem Normal 1991-2020 (R: 28_klimanormale.R, nur wenn die Normale vorliegen)
    if (layerLegenden.niederschlag_kum_normal || layerLegenden.gdd_normal || layerLegenden.boden_normal) {
      var normalHeading = document.createElement('div');
      normalHeading.className = 'gw-layer-heading';
      normalHeading.style.marginTop = '10px';
      normalHeading.textContent = 'Vergleich mit 1991–2020';
      layerPanel.appendChild(normalHeading);
      if (layerLegenden.niederschlag_kum_normal) schnittRadios.push(makeLayerRadio('niederschlag_kum_normal', 'Niederschlag seit 1. Januar, % des Normals',
        'Niederschlag seit 1. Januar bis zum Stichtag, verglichen mit demselben Stichtag in den Jahren 1991 bis 2020 (Median = 100 %). Braun: trockener als üblich, blau: nasser. 70 % heisst: 30 % weniger Regen als in einem mittleren Jahr.'));
      if (layerLegenden.gdd_normal) schnittRadios.push(makeLayerRadio('gdd_normal', 'Wärmesumme: Tage voraus/zurück',
        'Wie weit die Vegetation wärmemässig voraus ist: Die Wachstumsgradtage seit 1. Januar (Tagesmittel minus 5 Grad C) werden mit dem mittleren Verlauf 1991 bis 2020 verglichen. +10 Tage heisst: So viel Wärme hatte es in einem mittleren Jahr erst 10 Tage später. Im Winter und in Hochlagen (zu wenig Wärme) bleibt die Karte leer.'));
      if (layerLegenden.boden_normal) schnittRadios.push(makeLayerRadio('boden_normal', 'Bodenwasser: mm mehr/weniger',
        'Bodenwasserbilanz (Eimer-Modell, 130 mm) am Stichtag, verglichen mit demselben Stichtag in den Jahren 1991 bis 2020 (gleiche Rechnung je Jahr). Braun: weniger Wasser im Boden als im Median, blau: mehr. Ab April, weil der Eimer am 1. April voll startet.'));
      if (experimentellerModus && layerLegenden.wachstumspotenzial_kum_normal) schnittRadios.push(makeLayerRadio('wachstumspotenzial_kum_normal', 'Potenzielles Wachstum, % des Normals',
        'EXPERIMENTELL. Potenzielles Graswachstum seit 1. Januar (ModVege, Erholungsverzögerung 14 Tage), verglichen mit derselben Rechnung für 1991 bis 2020 (Median = 100 %).'));
    }
    // Potenzielles Wachstum (ModVege/growR): experimentell, nur mit
    // ?experimentell in der URL sichtbar (siehe wachstumspotenzial_
    // freigeschaltet im R-Teil fuer die bekannten Schwaechen).
    if (experimentellerModus) {
      radioWachstumspotenzialRate = makeLayerRadio('wachstumspotenzial_rate', layerLegenden.wachstumspotenzial_rate.label,
        'EXPERIMENTELL. Zeigt, wie viel Graswachstum das Klima (Temperatur, Strahlung, Wasserhaushalt) diese Woche pro Pixel maximal zulassen wuerde - ohne Naehrstofflimitierung und ohne Schnitt/Beweidung. Berechnet mit ModVege (Jouven et al. 2006, R-Paket growR). Bekannte Schwaechen: nach Regen auf eine Trockenperiode springt das Modell sofort auf volles Potenzial zurueck (reale Wiesen brauchen dafuer Wochen), und Grundwasserboeden werden nicht abgebildet.');
      radioWachstumspotenzialKum = makeLayerRadio('wachstumspotenzial_kum', layerLegenden.wachstumspotenzial_kum.label,
        'EXPERIMENTELL. Wie Potenzielles Wachstum, aber seit 1. Januar aufsummiert - zeigt, wie viel sich uebers Jahr an klimatisch moeglichem (ungenutztem) Wachstum angesammelt hat. Gleiche bekannte Schwaechen bei Trockenheit.');
    }
    // Schnittanalyse Testgebiet (experimentell): Radios nur, wenn die Daten
    // vorliegen (siehe R: schnittanalyse_index). Beim Auswaehlen zoomt die
    // Karte auf das Testgebiet - auf der Schweizkarte waere es nur ein Punkt.
    if (experimentellerModus && schnittanalyseGebiet) {
      var saHeading = document.createElement('div');
      saHeading.className = 'gw-layer-heading';
      saHeading.style.marginTop = '10px';
      saHeading.textContent = 'Schnittanalyse ' + schnittanalyseGebiet.name;
      layerPanel.appendChild(saHeading);
      Object.keys(ebenenSchluessel).filter(istSchnittEbene).forEach(function(n) {
        var r = makeLayerRadio(n, layerLegenden[n].label, 'EXPERIMENTELL. ' + layerLegenden[n].quelle + ' Stand jeweils bis Montag der gewaehlten Woche.');
        r.addEventListener('change', function() {
          if (!r.checked) return;
          var gd = document.querySelector('#datenexplorer-growthmap .js-plotly-plot');
          var g = schnittanalyseGebiet, rand = 0.15;
          var dx = (g.lon1 - g.lon0) * rand, dy = (g.lat1 - g.lat0) * rand;
          if (gd) Plotly.relayout(gd, { 'xaxis.range': [g.lon0 - dx, g.lon1 + dx], 'yaxis.range': [g.lat0 - dy, g.lat1 + dy] });
        });
        schnittRadios.push(r);
      });
    }
    // Nachschlagetabelle Ebenenname -> Radio, fuer aktualisiereLayerLabels()
    // (haengt dort das Symbol/die Fenstergroesse an alle 5 Fenster-Ebenen).
    radioJeEbene = { niederschlag: radioNiederschlag, temperatur: radioTemperatur, bodentemperatur: radioBodentemperatur, sonnenschein: radioSonnenschein, et0: radioEt0 };
    macheMeteoFensterSchieberegler();
    if (experimentellerModus) macheErholungsSchieberegler();
    aktualisiereLayerLabels();
    layerLegendeBox = document.createElement('div');
    layerLegendeBox.className = 'gw-layer-legende';
    layerLegendeBox.style.display = 'none';
    // Legende (Farbverlauf, Wert am Cursor, Koordinaten, Ort) samt Zeitraum-Regler ganz oben, noch vor «Ebenen»
    var legendeOben = document.createElement('div');
    legendeOben.className = 'gw-legende-oben';
    [meteoFensterWrap, erholungWrap, layerLegendeBox].forEach(function(x) { if (x) legendeOben.appendChild(x); });
    layerPanel.insertBefore(legendeOben, layerPanel.firstChild);
    mapControlsContainer.appendChild(layerPanel);
    aktualisiereLayerVerfuegbarkeit();
    aktualisiereLayerLegende();
  }

  function makeToggle(labelText, checked, onChange) {
    var wrap = document.createElement('div');
    wrap.className = 'gw-toggle-wrap';
    var text = document.createElement('span');
    text.textContent = labelText;
    var label = document.createElement('label');
    label.className = 'gw-toggle';
    var checkbox = document.createElement('input');
    checkbox.type = 'checkbox';
    checkbox.checked = checked;
    var slider = document.createElement('span');
    slider.className = 'gw-toggle-slider';
    checkbox.addEventListener('change', function() { onChange(checkbox.checked); });
    label.appendChild(checkbox);
    label.appendChild(slider);
    wrap.appendChild(text);
    wrap.appendChild(label);
    wrap.checkbox = checkbox;
    return wrap;
  }

  var precipToggleWrap = makeToggle('Niederschlag', true, function(checked) { precipOn = checked; applyState(); });
  var precipCheckbox = precipToggleWrap.checkbox;
  var xAxisToggleWrap = makeToggle('Kalenderwochen', true, function(checked) { datumOn = !checked; applyXAxis(); });
  var vorjahrToggleWrap = makeToggle('Andere Jahre', false, function(checked) { vorjahrOn = checked; vorjahrAutomatisch = false; applyState(); });
  vorjahrToggleWrap.title = 'Kurven der anderen Jahre zum Vergleich in Grau einblenden (Vorjahr dunkler)';

  controls.appendChild(comboWrap);
  controls.appendChild(yearSelect);

  var titleEl = document.createElement('div');
  titleEl.className = 'gw-title';
  titleEl.textContent = 'Graswachstumskurve';

  var fillHost = document.createElement('div');
  fillHost.className = 'gw-kurvenbereich';
  fillHost.style.width = '100%';
  el.parentNode.insertBefore(fillHost, el);
  // App-Layout: gemeinsamer Griff zwischen Karte und Kurve - ziehen
  // verschiebt die Grenze, die Pfeile schieben sie ganz nach oben (Kurve
  // voll) oder ganz nach unten (Karte voll). Ausserhalb des App-Layouts
  // unsichtbar.
  var kurveGriff = document.createElement('div');
  kurveGriff.className = 'gw-kurve-griff';
  kurveGriff.setAttribute('role', 'separator');
  kurveGriff.setAttribute('aria-orientation', 'horizontal');
  kurveGriff.setAttribute('aria-label', 'Grösse von Karte und Kurve verschieben');
  kurveGriff.setAttribute('aria-valuemin', '0');
  kurveGriff.setAttribute('aria-valuemax', '100');
  kurveGriff.tabIndex = 0;
  kurveGriff.title = 'Ziehen: Grösse ändern · Klicken: Kurve ein- oder ausklappen';
  var kurveGriffPunkte = gwIcon('griff');
  kurveGriffPunkte.setAttribute('class', 'gw-icon gw-kurve-griff-punkte');
  kurveGriff.appendChild(kurveGriffPunkte);
  kurveGriff.appendChild(gwIcon('kurve'));
  var kurveGriffTitel = document.createElement('span');
  kurveGriffTitel.className = 'gw-kurve-griff-titel';
  kurveGriffTitel.textContent = 'Wachstumskurve';
  var kurveGriffInfo = document.createElement('span');
  kurveGriffInfo.className = 'gw-kurve-griff-info';
  kurveGriff.appendChild(kurveGriffTitel); kurveGriff.appendChild(kurveGriffInfo);
  function griffPfeil(icon, text, ziel) {
    var b = document.createElement('button');
    b.type = 'button'; b.className = 'gw-griff-pfeil';
    b.title = text; b.setAttribute('aria-label', text);
    b.appendChild(gwIcon(icon));
    b.addEventListener('pointerdown', function(evt) { evt.stopPropagation(); });
    b.addEventListener('click', function(evt) { evt.stopPropagation(); setzeKurvenHoehe(ziel() ); });
    kurveGriff.appendChild(b);
    return b;
  }
  griffPfeil('hoch', 'Kurve ganz nach oben', function() { return kurveMax(); });
  griffPfeil('runter', 'Kurve ganz nach unten', function() { return 0; });
  fillHost.appendChild(kurveGriff);

  var detailKopf = document.createElement('div');
  detailKopf.className = 'gw-detail-kopf';
  var detailZurueck = document.createElement('button');
  detailZurueck.type = 'button'; detailZurueck.className = 'gw-detail-zurueck';
  detailZurueck.setAttribute('aria-label', 'Zurück zur Karte');
  detailZurueck.appendChild(gwIcon('zurueck'));
  detailZurueck.addEventListener('click', function(evt) { evt.stopPropagation(); schliesseDetail(); });
  var detailTitelEl = document.createElement('span');
  detailTitelEl.className = 'gw-detail-titel';
  var querKnopf = document.createElement('button');
  querKnopf.type = 'button'; querKnopf.className = 'gw-werkzeug-knopf gw-quer-knopf';
  setzeKnopfInhalt(querKnopf, 'drehen', 'Quer ansehen');
  var drehHinweis = document.createElement('div');
  drehHinweis.className = 'gw-dreh-hinweis';
  drehHinweis.textContent = 'Bitte das Handy quer drehen – mit eingeschalteter automatischer Drehung passt sich die Grafik an.';
  querKnopf.addEventListener('click', function(evt) {
    evt.stopPropagation();
    var ziel = document.documentElement;
    var vb = ziel.requestFullscreen ? ziel.requestFullscreen({ navigationUI: 'hide' }) : Promise.reject(new Error('kein Vollbild'));
    vb.then(function() { return screen.orientation.lock('landscape'); }).catch(function() {
      drehHinweis.classList.add('sichtbar');
      setTimeout(function() { drehHinweis.classList.remove('sichtbar'); }, 5000);
    });
  });
  detailKopf.appendChild(detailZurueck); detailKopf.appendChild(detailTitelEl); detailKopf.appendChild(querKnopf);
  fillHost.insertBefore(detailKopf, fillHost.firstChild);
  fillHost.insertBefore(drehHinweis, detailKopf.nextSibling);
  var kurveInhalt = document.createElement('div');
  kurveInhalt.className = 'gw-kurve-inhalt';
  fillHost.appendChild(kurveInhalt);
  kurveInhalt.appendChild(titleEl);
  kurveInhalt.appendChild(controls);

  var chartRow = document.createElement('div');
  chartRow.className = 'gw-chart-row';

  var legendPanel = document.createElement('div');
  legendPanel.className = 'gw-legend-panel';
  var legendHeader = document.createElement('div');
  legendHeader.className = 'gw-legend-header';
  var legendTitle = document.createElement('span');
  legendTitle.textContent = 'Standort';
  var legendClose = document.createElement('button');
  legendClose.type = 'button';
  legendClose.className = 'gw-legend-close';
  legendClose.title = 'Legende ausblenden';
  legendClose.textContent = String.fromCharCode(215);
  legendHeader.appendChild(legendTitle);
  legendHeader.appendChild(legendClose);
  var legendOptions = document.createElement('div');
  legendOptions.className = 'gw-legend-options';
  legendOptions.appendChild(precipToggleWrap);
  legendOptions.appendChild(xAxisToggleWrap);
  legendOptions.appendChild(vorjahrToggleWrap);
  var legendList = document.createElement('div');
  legendPanel.appendChild(legendHeader);
  legendPanel.appendChild(legendOptions);
  legendPanel.appendChild(legendList);

  var legendEdge = document.createElement('div');
  legendEdge.className = 'gw-legend-edge';
  var edgeBtn = document.createElement('button');
  edgeBtn.type = 'button';
  edgeBtn.className = 'gw-edge-btn active';
  edgeBtn.title = 'Legende ein-/ausblenden';
  edgeBtn.textContent = String.fromCharCode(9776);
  legendEdge.appendChild(edgeBtn);

  function setLegendOpen(open) {
    legendPanel.classList.toggle('collapsed', !open);
    edgeBtn.classList.toggle('active', open);
    // Plotlys responsive-Modus reagiert zwar von selbst auf die Breiten-
    // aenderung (per ResizeObserver auf el), ein expliziter Resize-Aufruf
    // NACH Abschluss der CSS-Breiten-Transition (150ms) stellt aber
    // zuverlaessig sicher, dass die Kurve den frei werdenden Platz nutzt,
    // unabhaengig von Browser-spezifischen Details der ResizeObserver-Timing.
    setTimeout(function() { Plotly.Plots.resize(el); }, 200);
  }
  edgeBtn.addEventListener('click', function() { setLegendOpen(legendPanel.classList.contains('collapsed')); });
  legendClose.addEventListener('click', function() { setLegendOpen(false); });

  // siteIdx (optional): macht den Eintrag klickbar (Standort-Filter, siehe
  // waehleSiteViaKlick()) - fuer die nicht-standortbezogenen Eintraege
  // (Mittleres Wachstum, Durchschnitt Mittelland) wird kein siteIdx uebergeben.
  function addLegendItem(label, color, style, siteIdx) {
    var item = document.createElement('div');
    item.className = 'gw-legend-item';
    if (siteIdx !== undefined) {
      item.classList.add('gw-legend-item-clickable');
      item.title = 'Nur ' + label + ' anzeigen';
      item.addEventListener('click', function() { waehleSiteViaKlick(siteIdx); });
    }
    var swatch = document.createElement('span');
    swatch.className = 'gw-legend-swatch';
    swatch.style.borderTopColor = color;
    swatch.style.borderTopStyle = style;
    var text = document.createElement('span');
    text.textContent = label;
    item.appendChild(swatch);
    item.appendChild(text);
    legendList.appendChild(item);
  }
  function renderLegendItems() {
    legendList.innerHTML = '';
    if (selection.type === 'group') {
      for (var i = 0; i < siteNames.length; i++) {
        if (siteInAuswahl(i)) addLegendItem(siteNames[i], siteColors[i], 'solid', i);
      }
      var weitere = weitereStandorte();
      if (weitere > 0 || alleStandorteZeigen) {
        var mehr = document.createElement('button');
        mehr.type = 'button';
        mehr.className = 'gw-legend-mehr';
        mehr.textContent = alleStandorteZeigen ? 'Nur regelmässig messende Standorte' : '+ ' + weitere + ' weitere (selten gemessen)';
        mehr.addEventListener('click', function(evt) { evt.stopPropagation(); alleStandorteZeigen = !alleStandorteZeigen; applyState(); });
        legendList.appendChild(mehr);
      }
      addLegendItem('Mittleres Wachstum', 'black', 'dashed');
    } else if (selection.type === 'multi') {
      selection.sites.forEach(function(i) { addLegendItem(siteNames[i], siteFarbe(i), 'solid', i); });
      selection.groups.forEach(function(g) { addLegendItem('Mittel ' + gruppenName(g), gruppenFarbe(g), 'dashed'); });
    } else {
      addLegendItem(siteNames[selection.idx], siteColors[selection.idx], 'solid', selection.idx);
    }
    addLegendItem('Durchschnitt Mittelland', 'red', 'dotted');
  }

  el.style.flex = '1 1 auto';
  el.style.minWidth = '0';
  chartRow.appendChild(el);
  chartRow.appendChild(legendPanel);
  chartRow.appendChild(legendEdge);
  kurveInhalt.appendChild(chartRow);

  function aktualisiereKurvenGriff() {
    if (!kurveGriffInfo) return;
    kurveGriffInfo.textContent = auswahlText() + ' · ' + selectedYear;
  }
  // Hoehe des Kurvenbereichs im App-Layout (px); Rest bekommt die Karte.
  var kurveHoehe = null, kurveAnteil = null;
  function kurveMax() {
    if (!seiteEl) return 0;
    var belegt = kurveGriff.offsetHeight + 1;
    var kopf = seiteEl.querySelector('.gw-mobil-kopf');
    if (kopf) belegt += kopf.offsetHeight;
    var zeit = document.getElementById('datenexplorer-slider');
    if (zeit) belegt += zeit.offsetHeight;
    return Math.max(0, seiteEl.clientHeight - belegt);
  }
  // Start: flach (ein Drittel), im Hochformat die Haelfte
  function kurveStandard() {
    var max = kurveMax();
    var anteil = window.innerHeight > window.innerWidth * 1.1 ? 0.5 : 0.34;
    return Math.min(max, Math.max(210, Math.round(max * anteil)));
  }
  function setzeKurvenHoehe(h, ohnePlot) {
    if (!kurveInhalt || !istAppModus()) return;
    var max = kurveMax();
    kurveHoehe = Math.max(0, Math.min(max, Math.round(h)));
    if (max > 0) kurveAnteil = kurveHoehe / max;
    kurveInhalt.style.height = kurveHoehe + 'px';
    seiteEl.classList.toggle('gw-kurve-zu', kurveHoehe < 1);
    kurveGriff.setAttribute('aria-expanded', String(kurveHoehe >= 1));
    seiteEl.classList.toggle('gw-kurve-voll', max > 0 && kurveHoehe >= max - 1);
    kurveGriff.setAttribute('aria-valuenow', String(max ? Math.round(100 * kurveHoehe / max) : 0));
    if (appLeisteKurveEl) appLeisteKurveEl.classList.toggle('aktiv', kurveHoehe > 0);
    if (!ohnePlot) passeKurvenHoeheAn();
  }
  // Kompatibel zu Icon-Leiste und App-Moduswechsel
  function setzeKurveAuf(auf) {
    if (!istAppModus()) return;
    setzeKurvenHoehe(auf ? kurveStandard() : 0);
  }
  function aktualisiereKurvenGriff() {
    if (!kurveGriffInfo) return;
    var text = auswahlText() + ' · ' + selectedYear;
    if (selection.type === 'group' && !alleStandorteZeigen && regelListe().length > 0) {
      var n = 0;
      for (var i = 0; i < siteNames.length; i++) if (siteInAuswahl(i)) n++;
      text += ' · ' + n + ' regelmässig messende Standorte';
    }
    kurveGriffInfo.textContent = text;
    if (detailOffen && detailTitelEl) detailTitelEl.textContent = auswahlText() + ' · ' + selectedYear;
  }
  // Ziehen (Maus, Finger, Stift); ohne Bewegung = Klick: Kurve zuklappen bzw.
  // wieder auf die vorige Hoehe aufklappen (Enter/Leertaste ebenso).
  var kurveVorher = null;
  function kurveUmschalten() {
    if ((kurveHoehe || 0) >= 1) { kurveVorher = kurveHoehe; setzeKurvenHoehe(0); }
    else setzeKurvenHoehe(kurveVorher && kurveVorher >= 80 ? Math.min(kurveVorher, kurveMax()) : kurveStandard());
  }
  (function() {
    var startY = 0, startH = 0, bewegt = false, aktiv = false;
    kurveGriff.addEventListener('pointerdown', function(evt) {
      if (!istAppModus()) return;
      aktiv = true; bewegt = false; startY = evt.clientY; startH = kurveHoehe || 0;
      kurveGriff.setPointerCapture(evt.pointerId);
      seiteEl.classList.add('gw-griff-zieht');
    });
    kurveGriff.addEventListener('pointermove', function(evt) {
      if (!aktiv) return;
      var d = startY - evt.clientY;
      if (Math.abs(d) > 3) bewegt = true;
      if (bewegt) setzeKurvenHoehe(startH + d, true);
    });
    function ende() {
      if (!aktiv) return;
      aktiv = false;
      seiteEl.classList.remove('gw-griff-zieht');
      var max = kurveMax();
      if (!bewegt) { kurveUmschalten(); return; }
      // In die Extreme einrasten
      if (kurveHoehe < 70) setzeKurvenHoehe(0);
      else if (kurveHoehe > max - 70) setzeKurvenHoehe(max);
      else setzeKurvenHoehe(kurveHoehe);
    }
    kurveGriff.addEventListener('pointerup', ende);
    kurveGriff.addEventListener('pointercancel', ende);
    kurveGriff.addEventListener('keydown', function(evt) {
      if (evt.key === 'Enter' || evt.key === ' ') { evt.preventDefault(); kurveUmschalten(); return; }
      var h = kurveHoehe || 0, max = kurveMax();
      var neu = evt.key === 'ArrowUp' ? h + 40 : evt.key === 'ArrowDown' ? h - 40 : evt.key === 'Home' ? max : evt.key === 'End' ? 0 : null;
      if (neu === null) return;
      evt.preventDefault();
      setzeKurvenHoehe(neu);
    });
  })();
  window.addEventListener('resize', function() {
    if (istAppModus() && kurveHoehe !== null && kurveAnteil !== null) setzeKurvenHoehe(kurveAnteil * kurveMax());
  });
  // Plot-Hoehe: im App-Layout aus dem Kurvenbereich, in der Detailansicht
  // (Handy) aus dem Bildschirm, sonst die feste Hoehe aus R.
  // Kompakt im App-Layout: Titel und x-Achsenbeschriftung stehen dort schon
  // im Griff bzw. sind aus dem Zusammenhang klar - mehr Hoehe fuer die Kurve.
  var kurveLayoutOriginal = null;
  function kurveLayoutFuer(kompakt, mitAchsentitel) {
    if (!kurveLayoutOriginal) {
      kurveLayoutOriginal = { t: el.layout.margin.t, b: el.layout.margin.b, titel: el.layout.title ? el.layout.title.text : '' };
    }
    var achse = datumOn ? 'Datum (Montag der Woche)' : 'Kalenderwoche';
    if (kompakt && mitAchsentitel) return { 'title.text': '', 'margin.t': 10, 'margin.b': kurveLayoutOriginal.b, 'xaxis.title.text': achse };
    return kompakt
      ? { 'title.text': '', 'margin.t': 10, 'margin.b': 28, 'xaxis.title.text': '' }
      : { 'title.text': kurveLayoutOriginal.titel, 'margin.t': kurveLayoutOriginal.t, 'margin.b': kurveLayoutOriginal.b,
          'xaxis.title.text': datumOn ? 'Datum (Montag der Woche)' : 'Kalenderwoche' };
  }
  function passeKurvenHoeheAn() {
    if (!kurveInhalt || !chartRow) return;
    var h = null, kompakt = false;
    if (detailOffen && istMobil()) {
      var quer = window.matchMedia('(orientation: landscape)').matches;
      h = quer ? Math.max(200, window.innerHeight - detailKopf.offsetHeight - 6) : Math.round(Math.max(260, window.innerHeight * 0.58));
      chartRow.style.height = '';
      kompakt = true;
    } else if (istAppModus() && kurveHoehe !== null) {
      if (kurveHoehe < 80) return;
      h = Math.max(120, Math.round(kurveHoehe - controls.offsetHeight - 12));
      chartRow.style.height = h + 'px';
      kompakt = true;
    } else {
      chartRow.style.height = '';
      h = 520;
      if (el.layout && el.layout.height === 520 && !kurveLayoutOriginal) return;
    }
    var aenderung = kurveLayoutFuer(kompakt, detailOffen);
    aenderung.height = h;
    Plotly.relayout(el, aenderung).then(function() { return Plotly.Plots.resize(el); }).then(aktualisiereSkala);
  }
  window.addEventListener('resize', passeKurvenHoeheAn);

  // el wurde bereits von Plotly (responsive=TRUE) auf seine urspruengliche
  // volle Breite gerendert, BEVOR es hier in chartRow neben legendPanel
  // (210px) eingefuegt wurde. Der ResizeObserver, den responsive=TRUE
  // registriert, erkennt diese synchrone DOM-Umstrukturierung nicht
  // zuverlaessig sofort - die Kurve blieb bisher zu breit (Ueberlappung
  // mit der Sidebar) und korrigierte sich erst bei einem echten Browser-
  // Resize/Zoom, der einen Reflow ausloest. Ein expliziter Resize-Aufruf
  // nach dem naechsten Layout-Frame (wenn die neue Flex-Breite bereits
  // feststeht) erzwingt die korrekte Breite von Anfang an, analog zum
  // Resize-Aufruf in setLegendOpen() weiter oben.
  requestAnimationFrame(function() { Plotly.Plots.resize(el); });

  // Kalenderwochen-Schieberegler: eigener Container zwischen Kurve und
  // Karten (im HTML bereits als leeres <div id="datenexplorer-slider">
  // angelegt), wird hier befuellt - steuert beide Karten gemeinsam.
  var stoppePlay = function() {};
  var sliderContainer = document.getElementById('datenexplorer-slider');
  var weekLabel = null;
  var sliderInput = null;
  if (sliderContainer) {
    var sliderRow = document.createElement('div');
    sliderRow.className = 'gw-slider-row';

    // alignedBox wird per JS exakt auf die Breite/Position der x-Achse
    // (Zeichenflaeche) der Kurve darunter ausgerichtet (siehe
    // syncSliderZuAchse() weiter unten) - Wochenlabel, Pfeile und
    // Schieberegler liegen alle darin, Heute-Button bewusst ausserhalb
    // (rechts davon, siehe sliderRow.appendChild(todayBtn) unten).
    var alignedBox = document.createElement('div');
    alignedBox.className = 'gw-slider-aligned';
    alignedBox.style.position = 'relative';

    weekLabel = document.createElement('div');
    weekLabel.className = 'gw-slider-label';

    var trackRow = document.createElement('div');
    trackRow.className = 'gw-slider-track-row';

    sliderInput = document.createElement('input');
    sliderInput.type = 'range';
    sliderInput.min = '1';
    sliderInput.max = '52';
    sliderInput.step = '1';
    sliderInput.value = String(selectedWeek);

    // Schwebender Tooltip ueber dem Schieberegler-Griff, der WAEHREND des
    // Ziehens (nicht erst nach Loslassen) live die gewaehlte Woche zeigt -
    // die bisherige Anzeige (weekLabel) bleibt zusaetzlich bestehen.
    var sliderTooltip = document.createElement('div');
    sliderTooltip.className = 'gw-slider-tooltip';
    sliderTooltip.style.display = 'none';
    alignedBox.appendChild(sliderTooltip);

    var zukunftMaske = document.createElement('div');
    zukunftMaske.className = 'gw-zukunft-maske';
    zukunftMaske.style.display = 'none';
    alignedBox.appendChild(zukunftMaske);

    function aktualisiereZukunftMaske() {
      var maxW = maxWocheFuerJahr(selectedYear);
      if (maxW >= 52) { zukunftMaske.style.display = 'none'; return; }
      var min = parseFloat(sliderInput.min), max = parseFloat(sliderInput.max);
      var anteil = (maxW - min) / (max - min);
      var sliderRect = sliderInput.getBoundingClientRect();
      var boxRect = alignedBox.getBoundingClientRect();
      var maskLinks = (sliderRect.left - boxRect.left) + anteil * sliderRect.width;
      zukunftMaske.style.left = maskLinks + 'px';
      zukunftMaske.style.width = Math.max(0, (sliderRect.right - boxRect.left) - maskLinks) + 'px';
      zukunftMaske.style.top = (sliderRect.top - boxRect.top) + 'px';
      zukunftMaske.style.height = sliderRect.height + 'px';
      zukunftMaske.style.display = 'block';
    }

    function positioniereSliderTooltip() {
      var min = parseFloat(sliderInput.min), max = parseFloat(sliderInput.max);
      var anteil = (selectedWeek - min) / (max - min);
      var sliderRect = sliderInput.getBoundingClientRect();
      var boxRect = alignedBox.getBoundingClientRect();
      var thumbX = (sliderRect.left - boxRect.left) + anteil * sliderRect.width;
      sliderTooltip.style.left = thumbX + 'px';
      sliderTooltip.textContent = 'KW ' + selectedWeek;
    }
    function zeigeSliderTooltip() { sliderTooltip.style.display = 'block'; positioniereSliderTooltip(); }
    function verstecke_SliderTooltip() { sliderTooltip.style.display = 'none'; }
    sliderInput.addEventListener('mousedown', zeigeSliderTooltip);
    sliderInput.addEventListener('touchstart', zeigeSliderTooltip);
    window.addEventListener('mouseup', verstecke_SliderTooltip);
    window.addEventListener('touchend', verstecke_SliderTooltip);

    // 'input' feuert bei <input type=range> laufend WAEHREND des Ziehens
    // (anders als 'change', das erst beim Loslassen feuert) - Karten und
    // Tooltip aktualisieren sich daher schon live beim Verschieben.
    sliderInput.addEventListener('input', function() {
      stoppePlay();
      var w = parseInt(sliderInput.value, 10);
      var maxW = maxWocheFuerJahr(selectedYear);
      if (w > maxW) { w = maxW; sliderInput.value = String(w); }
      selectedWeek = w;
      applyMapState();
      positioniereSliderTooltip();
    });

    function springeZuWoche(w) {
      selectedWeek = Math.max(1, Math.min(maxWocheFuerJahr(selectedYear), w));
      sliderInput.value = String(selectedWeek);
      applyMapState();
    }

    var prevBtn = document.createElement('button');
    prevBtn.type = 'button';
    prevBtn.className = 'gw-step-btn';
    prevBtn.title = 'Eine Woche zurueck';
    prevBtn.textContent = String.fromCharCode(9664);
    prevBtn.addEventListener('click', function() { stoppePlay(); springeZuWoche(selectedWeek - 1); });

    var nextBtn = document.createElement('button');
    nextBtn.type = 'button';
    nextBtn.className = 'gw-step-btn';
    nextBtn.title = 'Eine Woche vor';
    nextBtn.textContent = String.fromCharCode(9654);
    nextBtn.addEventListener('click', function() { stoppePlay(); springeZuWoche(selectedWeek + 1); });

    var todayBtn = document.createElement('button');
    todayBtn.type = 'button';
    todayBtn.className = 'gw-step-btn gw-today-btn';
    todayBtn.title = 'Aktuelle Kalenderwoche (heutiges Jahr)';
    todayBtn.textContent = 'Heute';
    todayBtn.addEventListener('click', function() {
      stoppePlay();
      selectedYear = neuestesJahr;
      yearSelect.value = neuestesJahr;
      var hatNiederschlag = jahreMitNiederschlag.indexOf(selectedYear) !== -1;
      precipCheckbox.disabled = !hatNiederschlag;
      aktualisiereLayerVerfuegbarkeit();
      applyXAxis();
      aktualisiereZukunftMaske();
      springeZuWoche(heutigeWoche);
      applyState();
    });

    // Pfeile sitzen neben der Wochenbeschriftung, oberhalb des eigentlichen
    // Schiebereglers (nicht mehr links/rechts vom Regler selbst).
    var labelRow = document.createElement('div');
    labelRow.className = 'gw-slider-label-row';
    labelRow.appendChild(prevBtn);
    labelRow.appendChild(weekLabel);
    labelRow.appendChild(nextBtn);
    trackRow.appendChild(sliderInput);
    alignedBox.appendChild(labelRow);
    alignedBox.appendChild(trackRow);
    // Play: Woche fuer Woche bis zur letzten verfuegbaren; steht der Regler
    // schon dort, beginnt es bei der ersten Woche mit Messungen.
    var playBtn = document.createElement('button');
    playBtn.type = 'button';
    playBtn.className = 'gw-step-btn gw-play-btn';
    var playTimer = null;
    function setzePlayIcon(laeuft) {
      playBtn.innerHTML = '';
      playBtn.appendChild(gwIcon(laeuft ? 'pause' : 'play'));
      playBtn.title = laeuft ? 'Anhalten' : 'Wochen abspielen';
      playBtn.setAttribute('aria-label', playBtn.title);
      playBtn.classList.toggle('aktiv', laeuft);
    }
    function ersteWocheMitDaten(jahr) {
      var praefix = jahr + ' ', erste = null;
      Object.keys(graswachstumBilder).forEach(function(k) {
        if (k.indexOf(praefix) !== 0) return;
        var w = parseInt(k.slice(praefix.length), 10);
        if (erste === null || w < erste) erste = w;
      });
      return erste || 1;
    }
    stoppePlay = function() {
      if (playTimer) { clearInterval(playTimer); playTimer = null; }
      setzePlayIcon(false);
    };
    function startePlay() {
      if (selectedWeek >= maxWocheFuerJahr(selectedYear)) springeZuWoche(ersteWocheMitDaten(selectedYear));
      playTimer = setInterval(function() {
        if (selectedWeek >= maxWocheFuerJahr(selectedYear)) { stoppePlay(); return; }
        springeZuWoche(selectedWeek + 1);
      }, 900);
      setzePlayIcon(true);
    }
    setzePlayIcon(false);
    playBtn.addEventListener('click', function() { if (playTimer) stoppePlay(); else startePlay(); });

    sliderRow.appendChild(alignedBox);
    sliderRow.appendChild(playBtn);
    sliderRow.appendChild(todayBtn);
    sliderContainer.appendChild(sliderRow);

    // Der Zeitstrahl (Wochenlabel + Pfeile + Schieberegler, alles in
    // alignedBox) soll IMMER exakt gleich breit sein wie die x-Achse
    // (Zeichenflaeche) der Kurve darunter - Heute-Button bewusst ausserhalb
    // dieser Ausrichtung, ganz rechts. el._fullLayout._size liefert Plotlys
    // tatsaechlich berechnete Zeichenflaeche relativ zu el SELBST (l=linker
    // Rand fuer die y-Achsenbeschriftung, w=Breite der Zeichenflaeche) -
    // die absoluten Bildschirmpositionen von el und sliderRow werden hier
    // bewusst per getBoundingClientRect() verglichen statt Gleichheit
    // anzunehmen, da sliderRow ein eigenes Padding hat (.gw-slider-row),
    // das el nicht hat.
    function syncSliderZuAchse() {
      var fl = el._fullLayout;
      if (!fl || !fl._size) return;
      var curveRect = el.getBoundingClientRect();
      var rowRect = sliderRow.getBoundingClientRect();
      // margin wird ab dem Ende der PADDING-Box gemessen, nicht ab
      // rowRect.left selbst - .gw-slider-row hat ein eigenes Padding
      // (16px), das hier mit abgezogen werden muss, sonst landet
      // alignedBox um genau diesen Betrag zu weit rechts.
      var rowPaddingLinks = parseFloat(getComputedStyle(sliderRow).paddingLeft) || 0;
      var plotLinksAbs = curveRect.left + fl._size.l;
      var plotRechtsAbs = plotLinksAbs + fl._size.w;
      alignedBox.style.marginLeft = (plotLinksAbs - rowRect.left - rowPaddingLinks) + 'px';
      alignedBox.style.width = (plotRechtsAbs - plotLinksAbs) + 'px';
      positioniereSliderTooltip();
      aktualisiereZukunftMaske();
    }
    syncSliderZuAchse();
    window.addEventListener('resize', syncSliderZuAchse);
    // Feuert nach JEDEM Redraw der Kurve (Fenstergroesse, Sidebar auf/zu,
    // Legende ein/aus - alles, was die Zeichenflaechen-Breite aendern kann).
    el.on('plotly_afterplot', syncSliderZuAchse);
  }

  // Kalenderwoche zusaetzlich per Mausklick auf die x-Achse der Kurve
  // waehlbar - Plotlys eigenes 'plotly_click' feuert nur bei Klicks auf
  // Datenpunkte, nicht im Achsenbereich darunter. Die Woche wird daher
  // direkt aus der Klick-Pixelposition berechnet (ueber die interne
  // Pixel-zu-Daten-Umrechnung der x-Achse), aber NUR fuer Klicks unterhalb
  // der eigentlichen Zeichenflaeche (sonst wuerde ein Klick zum Zoomen in
  // der Grafik selbst versehentlich auch die Woche aendern).
  el.addEventListener('click', function(evt) {
    var fl = el._fullLayout;
    if (!fl || !fl.xaxis || !fl._size) return;
    var rect = el.getBoundingClientRect();
    var yPixel = evt.clientY - rect.top;
    var plotBottom = fl._size.t + fl._size.h;
    if (yPixel < plotBottom) return;
    var xPixel = evt.clientX - rect.left;
    var xData = fl.xaxis.p2d(xPixel - fl.xaxis._offset);
    var woche = Math.max(1, Math.min(maxWocheFuerJahr(selectedYear), Math.round(xData)));
    stoppePlay();
    selectedWeek = woche;
    if (sliderInput) sliderInput.value = String(woche);
    applyMapState();
  });

  // ---------- Eigene Orte (z.B. Betriebe fuer eine Veranstaltung) ----------
  // Liste von PLZ/Ortschaften (swisstopo-Suche mit Vorschlaegen): als Marker
  // auf der Karte (Schalter), im Direktlink (orte=Name~lon~lat,...; an=orte)
  // und in der Veranstaltungskarte (Export) mit den Werten der Ebenen. Die
  // Koordinaten stehen im Link, damit QR-Codes ohne erneute Suche und
  // eindeutig funktionieren (eine PLZ kann mehrere Ortschaften haben).
  function ortToken(o) { return o.name.replace(/[~,]/g, ' ').replace(/\s+/g, ' ').trim() + '~' + o.lon.toFixed(3) + '~' + o.lat.toFixed(3); }
  function ortAusToken(t) {
    var m = String(t).split('~');
    if (m.length !== 3) return null;
    var lon = parseFloat(m[1]), lat = parseFloat(m[2]);
    if (!m[0].trim() || !isFinite(lon) || !isFinite(lat)) return null;
    return { name: m[0].trim(), lon: lon, lat: lat };
  }
  function ortAusTreffer(t) {
    return { name: t.attrs.label.replace(/<\/?b>/g, '').replace(' - ', ' ').trim(), lon: t.attrs.lon, lat: t.attrs.lat };
  }
  function ortsSuche(text, limit, herkunft) {
    var url = 'https://api3.geo.admin.ch/rest/services/api/SearchServer?searchText=' + encodeURIComponent(text) +
      '&type=locations&origins=' + (herkunft || 'zipcode,gg25') + '&limit=' + (limit || 8) + '&sr=2056';
    return fetch(url).then(function(r) { return r.json(); }).then(function(d) { return (d && d.results) || []; });
  }
  // Ein Eintrag aus einer eingefuegten Liste: «3513 Bigenthal» ueber die PLZ
  // suchen und die Ortschaft mit dem aehnlichsten Namen nehmen (auch bei
  // Tippfehlern; eine PLZ kann mehrere Ortschaften haben), sonst freie Suche.
  function normName(t) { return String(t).toLowerCase().normalize('NFD').replace(/[\u0300-\u036f]/g, '').replace(/[^a-z0-9]/g, ''); }
  function namensAehnlichkeit(a, b) {
    a = normName(a); b = normName(b);
    if (!a || !b) return 0;
    if (a === b) return 100;
    if (a.indexOf(b) === 0 || b.indexOf(a) === 0) return 80;
    if (a.indexOf(b) !== -1 || b.indexOf(a) !== -1) return 60;
    var n = 0;
    while (n < a.length && n < b.length && a[n] === b[n]) n++;
    return n * 5;
  }
  function sucheListenEintrag(text) {
    var m = String(text).match(/^(\d{4})\s*(.*)$/);
    if (!m) return ortsSuche(text, 1).then(function(r) { return r[0] || null; });
    return ortsSuche(m[1], 20, 'zipcode').then(function(r) {
      r = r.filter(function(t) { return String(t.attrs.detail || t.attrs.label).indexOf(m[1]) !== -1; });
      if (!r.length) return ortsSuche(text, 1).then(function(x) { return x[0] || null; });
      if (!m[2]) return r[0];
      var name = function(t) { return t.attrs.label.replace(/<\/?b>/g, '').replace(/^\d{4}\s*-\s*/, ''); };
      return r.slice().sort(function(a, b) { return namensAehnlichkeit(name(b), m[2]) - namensAehnlichkeit(name(a), m[2]); })[0];
    });
  }
  function setzeEigeneOrte(liste, ausLink) {
    var gesehen = {};
    eigeneOrte = liste.filter(function(o) { var k = ortToken(o); if (gesehen[k]) return false; gesehen[k] = true; return true; });
    if (!ausLink) { try { localStorage.setItem(ORTE_SPEICHER, JSON.stringify(eigeneOrte)); } catch (e) {} }
    orteHoerer.forEach(function(f) { f(); });
    aktualisiereOrteSpur();
    if (GWDatenexplorer.schreibeUrlBald) GWDatenexplorer.schreibeUrlBald();
  }
  function setzeOrteAnzeigen(an) {
    orteAnzeigen = !!an;
    orteHoerer.forEach(function(f) { f(); });
    aktualisiereOrteSpur();
    if (GWDatenexplorer.schreibeUrlBald) GWDatenexplorer.schreibeUrlBald();
  }
  function aktualisiereOrteSpur() {
    var gd = document.querySelector('#datenexplorer-growthmap .js-plotly-plot');
    if (!gd || !gd.data) return;
    var sichtbar = orteAnzeigen && eigeneOrte.length > 0;
    var lon = eigeneOrte.map(function(o) { return o.lon; }), lat = eigeneOrte.map(function(o) { return o.lat; });
    var namen = eigeneOrte.map(function(o) { return o.name; });
    var hover = eigeneOrte.map(function(o, i) { return '<b>' + (i + 1) + '. ' + o.name.replace(/</g, '&lt;') + '</b>'; });
    if (orteTraceIdx === null) {
      if (!sichtbar) return;
      Plotly.addTraces(gd, {
        x: lon, y: lat, type: 'scatter', mode: 'markers+text', text: namen, textposition: 'top center',
        textfont: { size: 12, color: '#5a0b2e' }, hoverinfo: 'text', hovertext: hover, cliponaxis: false,
        marker: { symbol: 'circle', size: 12, color: ORTE_FARBE, line: { color: 'white', width: 2 } },
        showlegend: false, name: 'Eigene Orte'
      });
      orteTraceIdx = gd.data.length - 1;
      return;
    }
    Plotly.restyle(gd, { x: [lon], y: [lat], text: [namen], hovertext: [hover], visible: sichtbar }, [orteTraceIdx]);
  }

  // Eingabe mit Vorschlaegen und Kaertchen; mehrere Eintraege (Komma,
  // Semikolon, Zeilenumbruch) werden nacheinander gesucht - je erster Treffer.
  function macheOrteEditor(meldung) {
    var wrap = document.createElement('div');
    wrap.className = 'gw-orte';
    var liste = document.createElement('div');
    liste.className = 'gw-orte-liste';
    var combo = document.createElement('div');
    combo.className = 'gw-combo gw-orte-combo';
    var input = document.createElement('input');
    input.type = 'text';
    input.placeholder = 'PLZ oder Ortschaft hinzufügen …';
    input.setAttribute('aria-label', 'PLZ oder Ortschaft hinzufügen');
    var vorschlaege = document.createElement('div');
    vorschlaege.className = 'gw-combo-list';
    vorschlaege.style.display = 'none';
    combo.appendChild(input); combo.appendChild(vorschlaege);
    var aktionen = document.createElement('div');
    aktionen.className = 'gw-orte-aktionen';
    var leeren = document.createElement('button');
    leeren.type = 'button'; leeren.className = 'gw-orte-link'; leeren.textContent = 'Liste leeren';
    leeren.addEventListener('click', function() { if (!eigeneOrte.length || window.confirm('Alle ' + eigeneOrte.length + ' Orte aus der Liste entfernen?')) setzeEigeneOrte([]); });
    aktionen.appendChild(leeren);
    wrap.appendChild(combo); wrap.appendChild(liste); wrap.appendChild(aktionen);
    var treffer = [], timer = null, aktiv = -1;
    function zeichneListe() {
      liste.innerHTML = '';
      eigeneOrte.forEach(function(o, i) {
        var chip = document.createElement('span');
        chip.className = 'gw-ort-chip';
        var nr = document.createElement('span'); nr.className = 'gw-ort-nr'; nr.textContent = String(i + 1);
        var name = document.createElement('button'); name.type = 'button'; name.className = 'gw-ort-name'; name.textContent = o.name;
        name.title = 'Bezeichnung ändern (z.B. Name des Betriebs)';
        name.addEventListener('click', function() {
          var neu = window.prompt('Bezeichnung auf Karte und in der Tabelle:', o.name);
          if (neu && neu.trim()) { var kopie = eigeneOrte.slice(); kopie[i] = Object.assign({}, o, { name: neu.trim() }); setzeEigeneOrte(kopie); }
        });
        var weg = document.createElement('button'); weg.type = 'button'; weg.className = 'gw-chip-x'; weg.textContent = String.fromCharCode(215);
        weg.setAttribute('aria-label', o.name + ' entfernen');
        weg.addEventListener('click', function() { setzeEigeneOrte(eigeneOrte.filter(function(x, j) { return j !== i; })); });
        chip.appendChild(nr); chip.appendChild(name); chip.appendChild(weg);
        liste.appendChild(chip);
      });
      leeren.style.display = eigeneOrte.length ? '' : 'none';
    }
    orteHoerer.push(zeichneListe);
    zeichneListe();
    function waehle(t) {
      vorschlaege.style.display = 'none'; input.value = ''; treffer = [];
      setzeEigeneOrte(eigeneOrte.concat([ortAusTreffer(t)]));
    }
    function zeigeVorschlaege() {
      vorschlaege.innerHTML = '';
      treffer.forEach(function(t, i) {
        var item = document.createElement('div');
        item.className = 'gw-combo-item' + (i === aktiv ? ' active' : '');
        item.textContent = ortAusTreffer(t).name;
        item.addEventListener('mousedown', function(evt) { evt.preventDefault(); waehle(t); });
        vorschlaege.appendChild(item);
      });
      vorschlaege.style.display = treffer.length ? 'block' : 'none';
    }
    function mehrere(text) {
      var teile = text.split(/[,;\n\t]+/).map(function(x) { return x.trim(); }).filter(Boolean);
      if (!teile.length) return;
      input.value = ''; vorschlaege.style.display = 'none';
      var nichtGefunden = [], neu = [];
      if (meldung) meldung('Suche ' + teile.length + ' Orte …');
      var kette = Promise.resolve();
      teile.forEach(function(t) {
        kette = kette.then(function() {
          return sucheListenEintrag(t).then(function(r) { if (r) neu.push(ortAusTreffer(r)); else nichtGefunden.push(t); }, function() { nichtGefunden.push(t); });
        });
      });
      kette.then(function() {
        var vorher = eigeneOrte.length;
        setzeEigeneOrte(eigeneOrte.concat(neu));
        var hinzu = eigeneOrte.length - vorher, doppelt = neu.length - hinzu;
        if (meldung) meldung(hinzu + ' Orte hinzugefügt' + (doppelt ? ' (' + doppelt + ' doppelt)' : '') + (nichtGefunden.length ? ', nicht gefunden: ' + nichtGefunden.join(', ') : '.'), nichtGefunden.length > 0);
      });
    }
    input.addEventListener('input', function() {
      clearTimeout(timer);
      var text = input.value;
      if (/[,;\n\t]/.test(text)) return;
      if (text.trim().length < 2) { vorschlaege.style.display = 'none'; return; }
      timer = setTimeout(function() {
        ortsSuche(text.trim(), 8).then(function(r) { if (input.value !== text) return; treffer = r; aktiv = -1; zeigeVorschlaege(); }, function() {});
      }, 250);
    });
    input.addEventListener('paste', function(evt) {
      var text = (evt.clipboardData || window.clipboardData).getData('text');
      if (/[,;\n\t]/.test(text)) { evt.preventDefault(); mehrere(text); }
    });
    input.addEventListener('keydown', function(evt) {
      if (evt.key === 'ArrowDown' || evt.key === 'ArrowUp') {
        if (!treffer.length) return;
        evt.preventDefault();
        aktiv = (aktiv + (evt.key === 'ArrowDown' ? 1 : -1) + treffer.length) % treffer.length;
        zeigeVorschlaege();
      } else if (evt.key === 'Enter') {
        evt.preventDefault();
        if (/[,;]/.test(input.value)) mehrere(input.value);
        else if (treffer.length) waehle(treffer[Math.max(0, aktiv)]);
        else if (input.value.trim()) mehrere(input.value);
      } else if (evt.key === 'Escape') { vorschlaege.style.display = 'none'; }
    });
    input.addEventListener('blur', function() { setTimeout(function() { vorschlaege.style.display = 'none'; }, 150); });
    return wrap;
  }

  // ---------- Export: Bilder, Zeitserien, PDF ----------
  // Seitenleiste rechts. Karte und Kurve werden ausserhalb des Bildschirms
  // als Plotly-Figur gerendert (Plotly.toImage mit Figur-Objekt) - die
  // sichtbare Ansicht bleibt unberuehrt. jsPDF und JSZip (MIT) werden erst
  // beim ersten Gebrauch von cdnjs geladen.
  var EXPORT_BIBLIOTHEKEN = {
    jspdf: 'https://cdnjs.cloudflare.com/ajax/libs/jspdf/2.5.1/jspdf.umd.min.js',
    jszip: 'https://cdnjs.cloudflare.com/ajax/libs/jszip/3.10.1/jszip.min.js',
    qrcode: 'https://cdnjs.cloudflare.com/ajax/libs/qrcode-generator/1.4.4/qrcode.min.js'
  };
  var exportBibliotheken = {};
  function ladeBibliothek(name) {
    if (exportBibliotheken[name]) return exportBibliotheken[name];
    exportBibliotheken[name] = new Promise(function(ok, fehler) {
      var s = document.createElement('script');
      s.src = EXPORT_BIBLIOTHEKEN[name];
      s.onload = function() { ok(); };
      s.onerror = function() { delete exportBibliotheken[name]; fehler(new Error('Bibliothek ' + name + ' konnte nicht geladen werden (Internetverbindung?)')); };
      document.head.appendChild(s);
    });
    return exportBibliotheken[name];
  }
  function zweistellig(n) { return (n < 10 ? '0' : '') + n; }
  function exportName(teil, woche) { return 'graswachstum_' + selectedYear + '-KW' + zweistellig(woche || selectedWeek) + (teil ? '_' + teil : ''); }
  function herunterladen(inhalt, name) {
    var url = typeof inhalt === 'string' ? inhalt : URL.createObjectURL(inhalt);
    var a = document.createElement('a');
    a.href = url; a.download = name; a.style.display = 'none';
    document.body.appendChild(a); a.click();
    setTimeout(function() { a.remove(); if (typeof inhalt !== 'string') URL.revokeObjectURL(url); }, 1500);
  }
  // Plotly liefert PNG als base64-, SVG als URI-kodierte data:-URL
  function dataUrlInhalt(url) {
    var komma = url.indexOf(',');
    var kopf = url.slice(0, komma), rumpf = url.slice(komma + 1);
    if (kopf.indexOf(';base64') !== -1) {
      var bin = atob(rumpf), arr = new Uint8Array(bin.length);
      for (var i = 0; i < bin.length; i++) arr[i] = bin.charCodeAt(i);
      return arr;
    }
    return decodeURIComponent(rumpf);
  }
  function ladeEbeneP(schluessel) {
    return new Promise(function(ok, fehler) {
      var t = setTimeout(function() { fehler(new Error('Ebene ' + schluessel + ' lädt nicht')); }, 30000);
      ladeEbene(schluessel, function(d) { clearTimeout(t); ok(d); });
    });
  }
  function exportSchrift() { return getComputedStyle(seiteEl || document.body).fontFamily; }
  function wocheText(woche) {
    var b = wochenDatumBereichText(woche);
    return 'KW ' + woche + ' ' + selectedYear + (b ? ' (' + b + ')' : '');
  }
  function wochenDatumBereichText(woche) {
    try {
      var montag = new Date(Date.UTC(selectedYear, 0, 4));
      montag.setUTCDate(montag.getUTCDate() - ((montag.getUTCDay() + 6) % 7) + (woche - 1) * 7);
      var sonntag = new Date(montag.getTime() + 6 * 86400000);
      var f = function(d, mitJahr) { return zweistellig(d.getUTCDate()) + '.' + zweistellig(d.getUTCMonth() + 1) + '.' + (mitJahr ? d.getUTCFullYear() : ''); };
      return f(montag, false) + ' – ' + f(sonntag, true);
    } catch (e) { return ''; }
  }
  function ebenenTitel(name, fenster) {
    var info = layerLegenden[name];
    if (!info) return '';
    if (istFensterEbene(name)) return info.label + ' ' + (info.symbol === 'sum' ? 'Σ' : '⌀') + ' ' + fenster + ' Tage';
    if (name === 'boden') return info.label + ' (berechnet)';
    return info.label;
  }
  function ebenenTitelPdf(name, fenster) {
    var info = layerLegenden[name];
    if (!info) return '';
    if (istFensterEbene(name)) return info.label + ', ' + (info.symbol === 'sum' ? 'Summe' : 'Mittel') + ' ' + fenster + ' Tage';
    return ebenenTitel(name, fenster);
  }
  function ebenenBereich(name, fenster) {
    var info = layerLegenden[name];
    return info.fensterSkaliert ? [info.bereich[0], Math.round(info.bereich[1] * fenster / 7)] : info.bereich;
  }

  // Karte als Figur: ganze Schweiz ('voll') oder aktueller Ausschnitt
  function kartenFigur(opt) {
    var gd = document.querySelector('#datenexplorer-growthmap .js-plotly-plot');
    var info = GWDatenexplorer.kartenInfo;
    var xr, yr;
    // Aktueller Ausschnitt nur, wenn hineingezoomt - sonst die ganze Schweiz
    // ohne den (auf dem Bildschirm fuer Leerraum genutzten) Rand
    var gx = gd.layout.xaxis.range, gezoomt = gx && (gx[1] - gx[0]) < (info.xMaxSchweiz - info.xMin) * 0.97;
    if (opt.ansicht === 'aktuell' && gezoomt) {
      xr = gx.slice(); yr = gd.layout.yaxis.range.slice();
    } else {
      xr = [info.xMin, info.xMaxSchweiz];
      yr = [info.yMitte - info.ySpanSchweiz * 0.52, info.yMitte + info.ySpanSchweiz * 0.52];
    }
    var oben = opt.titel ? 54 : 6, breite = opt.breite || 1400;
    var hoehe = Math.round((breite - 12) * (yr[1] - yr[0]) * info.scaleratio / (xr[1] - xr[0])) + oben + 6;
    var layout = Object.assign({}, gd.layout, {
      width: breite, height: hoehe, images: opt.bilder,
      title: opt.titel ? { text: opt.titel, x: 0.01, xanchor: 'left', font: { size: 18 } } : { text: '' },
      margin: { l: 6, r: 6, t: oben, b: 6 }, paper_bgcolor: 'white', font: { family: exportSchrift() },
      xaxis: Object.assign({}, gd.layout.xaxis, { range: xr, autorange: false }),
      yaxis: Object.assign({}, gd.layout.yaxis, { range: yr, autorange: false })
    });
    return { data: opt.mitSpuren ? gd.data : [], layout: layout, config: { staticPlot: true } };
  }
  function kartenBilderFuer(woche, ebenenBild, mitStandorten) {
    var k = selectedYear + ' ' + woche;
    var bilder = [kartenbildHintergrund];
    if (ebenenBild) bilder.push(ebenenBild);
    if (mitStandorten) {
      if (afcOn && afcRingBilder[k]) bilder.push(afcRingBilder[k]);
      if (graswachstumOn && graswachstumBilder[k]) bilder.push(graswachstumBilder[k]);
    }
    return bilder;
  }
  function aktuelleEbeneBild(woche) {
    if (hintergrundEbene === 'keine') return Promise.resolve(null);
    return ladeEbeneP(ebeneDateiSchluessel(hintergrundEbene)).then(function(d) { return (d.bilder || {})[selectedYear + ' ' + woche] || null; });
  }
  function kartenTitel(woche) {
    return 'Graswachstum ' + wocheText(woche) + (hintergrundEbene !== 'keine' ? ' · ' + ebenenTitel(hintergrundEbene, meteoFenster) : '');
  }
  // Kurve als Figur: mit Titel, Achsenbeschriftung und Plotly-Legende
  function kurvenFigur(opt) {
    var gruppeJeTrace = {};
    groupGrowthMeta.forEach(function(m) { gruppeJeTrace[m.traceIdx] = m.groupIdx; });
    var daten = el.data.map(function(t, i) {
      var sichtbar = t.visible === true || t.visible === undefined;
      var name = (gruppeJeTrace[i] !== undefined && selection.type === 'multi') ? 'Mittel ' + gruppenName(gruppeJeTrace[i]) : t.name;
      return Object.assign({}, t, { name: name, showlegend: sichtbar && !!t.name });
    });
    var breite = opt.breite || 1400, hoehe = opt.hoehe || 760, rand = { l: 64, r: 64, t: 56, b: 120 };
    var strich = (el.layout.shapes || [])[0];
    if (strich && opt.woche) strich = Object.assign({}, strich, { x0: opt.woche, x1: opt.woche });
    var m = skalaAuto ? { shapes: [], annotations: [] } : skalaMarkierungen(werteUeberSkala(), hoehe - rand.t - rand.b, breite - rand.l - rand.r, el.layout.xaxis.range || [0, 52]);
    var shapes = [strich].filter(Boolean).concat(m.shapes);
    var annotations = (el.layout.annotations || []).filter(function(a) { return a.name !== 'gw-skala'; }).concat(m.annotations);
    var layout = Object.assign({}, el.layout, {
      width: breite, height: hoehe, shapes: shapes, annotations: annotations,
      title: { text: 'Graswachstumskurve – ' + auswahlText() + ' ' + selectedYear + (opt.woche ? ' (KW ' + opt.woche + ')' : ''), x: 0.01, xanchor: 'left', font: { size: 18 } },
      margin: rand, paper_bgcolor: 'white', font: { family: exportSchrift() },
      showlegend: true, legend: { orientation: 'h', x: 0, y: -0.2, font: { size: 11 } },
      xaxis: Object.assign({}, el.layout.xaxis, { title: { text: datumOn ? 'Datum (Montag der Woche)' : 'Kalenderwoche' } })
    });
    return { data: daten, layout: layout, config: { staticPlot: true } };
  }
  // SVG: eingebettete WebP-Bilder als PNG, damit auch Programme ohne
  // WebP-Unterstuetzung (aeltere Grafikprogramme) die Datei richtig zeigen
  function webpAlsPng(src) {
    if (typeof src !== 'string' || src.indexOf('data:image/webp') !== 0) return Promise.resolve(src);
    return new Promise(function(ok) {
      var img = new Image();
      img.onload = function() {
        var c = document.createElement('canvas');
        c.width = img.naturalWidth; c.height = img.naturalHeight;
        c.getContext('2d').drawImage(img, 0, 0);
        ok(c.toDataURL('image/png'));
      };
      img.onerror = function() { ok(src); };
      img.src = src;
    });
  }
  function figurBild(fig, format, faktor) {
    var bilder = (fig.layout && fig.layout.images) || [];
    var vorbereitet = format === 'svg'
      ? Promise.all(bilder.map(function(b) { return webpAlsPng(b.source).then(function(q) { return Object.assign({}, b, { source: q }); }); }))
          .then(function(neu) { fig.layout.images = neu; })
      : Promise.resolve();
    return vorbereitet.then(function() {
      return Plotly.toImage(fig, { format: format, width: fig.layout.width, height: fig.layout.height, scale: format === 'png' ? (faktor || 1) : 1 });
    });
  }
  // Fuer PDFs: PNG auf weissem Grund als JPEG (ein Bruchteil der Groesse)
  function alsJpeg(pngUrl) {
    return new Promise(function(ok, fehler) {
      var img = new Image();
      img.onload = function() {
        var c = document.createElement('canvas');
        c.width = img.naturalWidth; c.height = img.naturalHeight;
        var ctx = c.getContext('2d');
        ctx.fillStyle = 'white'; ctx.fillRect(0, 0, c.width, c.height);
        ctx.drawImage(img, 0, 0);
        ok(c.toDataURL('image/jpeg', 0.88));
      };
      img.onerror = function() { fehler(new Error('Bild konnte nicht umgewandelt werden')); };
      img.src = pngUrl;
    });
  }
  // Ausfuehrliche Legenden fuer PDFs (Canvas, 4-fache Aufloesung)
  function afcVerlaufFuer(woche) {
    var fi = afcFensterJeWoche[selectedYear + ' ' + woche];
    return fi ? afcVerlaeufe[fi - 1] : null;
  }
  function afcRingPng(verlauf) {
    var f = 4, g = 64, c = document.createElement('canvas');
    c.width = g * f; c.height = g * f;
    var ctx = c.getContext('2d');
    ctx.scale(f, f);
    var m = g / 2, ra = 20, ri = 13;
    if (ctx.createConicGradient) {
      var grad = ctx.createConicGradient(-Math.PI / 2, m, m);
      verlauf.farben.forEach(function(farbe, i) { grad.addColorStop(i / Math.max(1, verlauf.farben.length - 1), farbe); });
      ctx.fillStyle = grad;
    } else ctx.fillStyle = verlauf.farben[Math.floor(verlauf.farben.length / 2)];
    ctx.beginPath(); ctx.arc(m, m, ra, 0, 2 * Math.PI); ctx.arc(m, m, ri, 0, 2 * Math.PI, true); ctx.fill();
    var punkt = function(wert, rad) { var a = wert / 1500 * 2 * Math.PI; return [m + rad * Math.sin(a), m - rad * Math.cos(a)]; };
    ctx.strokeStyle = '#000'; ctx.lineWidth = 1.2;
    ctx.beginPath(); ctx.moveTo(m, m - ri + 1); ctx.lineTo(m, m - ra - 4); ctx.stroke();
    [verlauf.low, verlauf.high].forEach(function(w) {
      var a = punkt(w, ri - 1), b = punkt(w, ra + 3);
      ctx.beginPath(); ctx.moveTo(a[0], a[1]); ctx.lineTo(b[0], b[1]); ctx.stroke();
    });
    var a0 = verlauf.low / 1500 * 2 * Math.PI - Math.PI / 2, a1 = verlauf.high / 1500 * 2 * Math.PI - Math.PI / 2;
    ctx.lineWidth = 1;
    ctx.beginPath(); ctx.arc(m, m, ra + 5, a0, a1); ctx.stroke();
    [[a0, -1], [a1, 1]].forEach(function(v) {
      var x = m + (ra + 5) * Math.cos(v[0]), y = m + (ra + 5) * Math.sin(v[0]);
      var tx = -Math.sin(v[0]) * v[1], ty = Math.cos(v[0]) * v[1];
      ctx.beginPath(); ctx.moveTo(x + tx * 3, y + ty * 3);
      ctx.lineTo(x - ty * 1.8, y + tx * 1.8); ctx.lineTo(x + ty * 1.8, y - tx * 1.8); ctx.closePath();
      ctx.fillStyle = '#000'; ctx.fill();
    });
    return c.toDataURL('image/png');
  }
  // Legende DGV/Graswachstum ins PDF: Ring mit Beschriftung, Grau-Balken,
  // erklaerende Texte. Gibt die verbrauchte Hoehe (mm) zurueck.
  function pdfMessLegende(doc, x, y, breite, woche) {
    var verlauf = afcVerlaufFuer(woche), h = 0;
    doc.setTextColor(40);
    if (afcOn && verlauf) {
      var d = 26;
      doc.addImage(afcRingPng(verlauf), 'PNG', x, y, d, d);
      doc.setFontSize(6.5); doc.setTextColor(70);
      doc.text('0 / 1500', x + d / 2, y - 0.6, { align: 'center' });
      var pt = function(w, rad) { var a = w / 1500 * 2 * Math.PI; return [x + d / 2 + rad * Math.sin(a), y + d / 2 - rad * Math.cos(a)]; };
      [verlauf.low, verlauf.high].forEach(function(w) {
        var p = pt(w, d * 0.46);
        doc.text(String(w), p[0], p[1] + 1, { align: p[0] > x + d / 2 + 1 ? 'left' : (p[0] < x + d / 2 - 1 ? 'right' : 'center') });
      });
      var tx = x + d + 8, tb = breite - d - 8;
      doc.setFont('helvetica', 'bold'); doc.setFontSize(8.5); doc.setTextColor(30);
      doc.text('Ring: DGV (Durchschnittlicher Grasvorrat, kg TS/ha)', tx, y + 3);
      doc.setFont('helvetica', 'normal'); doc.setFontSize(7.8); doc.setTextColor(60);
      var t = doc.splitTextToSize('Der Ring füllt sich im Uhrzeigersinn ab 0 (oben) bis 1500 kg TS/ha. Farbe nach dem Zielbereich der Jahreszeit: rot = deutlich zu wenig (unter 200 kg praktisch leer), grün = im Zielbereich, blaugrün = mehr als nötig. Der Doppelpfeil markiert den Zielbereich dieser Woche: ' + verlauf.low + '–' + verlauf.high + ' kg TS/ha.', tb);
      doc.text(t, tx, y + 7);
      h = Math.max(d + 2, 7 + t.length * 3.2);
    }
    if (graswachstumOn || afcOn) {
      var y2 = y + h + (h ? 3 : 0);
      doc.addImage(farbBalkenPng(['#ffffff', '#757575'], 300, 10), 'PNG', x, y2, 26, 3);
      doc.setDrawColor(190); doc.rect(x, y2, 26, 3);
      doc.setFontSize(6.5); doc.setTextColor(70);
      doc.text('0', x, y2 + 6); doc.text('14 Tage', x + 26, y2 + 6, { align: 'right' });
      var tx2 = x + 34, tb2 = breite - 34;
      doc.setFont('helvetica', 'bold'); doc.setFontSize(8.5); doc.setTextColor(30);
      doc.text(graswachstumOn ? 'Kreis: Graswachstum (kg TS/ha/Tag)' : 'Graufärbung: Tage seit Messung', tx2, y2 + 2.5);
      doc.setFont('helvetica', 'normal'); doc.setFontSize(7.8); doc.setTextColor(60);
      var t2 = doc.splitTextToSize((graswachstumOn ? 'Die Zahl im Kreis ist das zuletzt gemessene Graswachstum. ' : '') + 'Die Graufärbung von Kreis und Ring zeigt die Tage seit der letzten Messung: weiss = am Messtag, dunkelgrau = 14 Tage. Ältere Messungen erscheinen nicht.', tb2);
      doc.text(t2, tx2, y2 + 6.2);
      h = (y2 - y) + Math.max(7, 6.2 + t2.length * 3.2);
    }
    return h;
  }
  // Farbskala einer Ebene mit Titel, Einheit und Zwischenwerten
  function pdfEbenenLegende(doc, name, fenster, x, y, breite, klein) {
    var info = layerLegenden[name];
    if (!info) return 0;
    var bereich = ebenenBereich(name, fenster), bh = klein ? 2.6 : 3.6;
    doc.addImage(farbBalkenPng(info.farben, 400, 12), 'PNG', x, y, breite, bh);
    doc.setDrawColor(190); doc.rect(x, y, breite, bh);
    doc.setFontSize(klein ? 6.3 : 7.5); doc.setTextColor(70);
    var schritte = klein ? 2 : 4;
    for (var i = 0; i <= schritte; i++) {
      var w = bereich[0] + (bereich[1] - bereich[0]) * i / schritte;
      var wt = Math.abs(w) >= 100 ? String(Math.round(w)) : String(Math.round(w * 10) / 10);
      doc.line(x + breite * i / schritte, y + bh, x + breite * i / schritte, y + bh + 1);
      doc.text(wt + (i === schritte ? ' ' + String(info.einheit).split(',')[0] : ''), x + breite * i / schritte, y + bh + (klein ? 3.6 : 4.4), { align: i === 0 ? 'left' : (i === schritte ? 'right' : 'center') });
    }
    return bh + (klein ? 4.5 : 6);
  }
  // QR-Code zur ausfuehrlichen Hilfe (Legenden) auf graswachstum.ch
  var HILFE_URL = 'https://graswachstum.ch/de/growth/help';
  function pdfQr(doc, x, y, groesse, text) {
    pdfQrVektor(doc, HILFE_URL, x, y, groesse, 'M');
    doc.setFontSize(6.5); doc.setTextColor(90);
    var z = doc.splitTextToSize(text, 40);
    doc.text(z, x + groesse + 2.5, y + 3);
    doc.setTextColor(40, 100, 90);
    doc.textWithLink('graswachstum.ch/de/growth/help', x + groesse + 2.5, y + 3 + z.length * 2.8 + 1, { url: HILFE_URL });
  }
  // Mini-Legende Messstandorte (Ebenen-PDF): kleiner Ring mit Zielwerten,
  // Grau-Balken und eine Zeile Text - Details per QR-Code in der Hilfe
  function pdfMiniMessLegende(doc, x, y, breite, woche) {
    var verlauf = afcVerlaufFuer(woche), d = 9;
    doc.setFontSize(6.3); doc.setTextColor(70);
    if (afcOn && verlauf) {
      doc.addImage(afcRingPng(verlauf), 'PNG', x, y, d, d);
      doc.text('Ring: DGV kg TS/ha, Ziel ' + verlauf.low + '–' + verlauf.high, x + d + 1.5, y + 3.2);
    }
    if (graswachstumOn || afcOn) {
      var xb = x + d + 1.5;
      doc.text(graswachstumOn ? 'Kreis: Graswachstum kg TS/ha/Tag' : '', xb, y + 6.4);
      doc.text('Tage seit Messung', x + breite - 11, y + 2.2, { align: 'center' });
      doc.addImage(farbBalkenPng(['#ffffff', '#757575'], 200, 8), 'PNG', x + breite - 22, y + 3.2, 22, 2.2);
      doc.setDrawColor(190); doc.rect(x + breite - 22, y + 3.2, 22, 2.2);
      doc.text('0', x + breite - 22, y + 7.8);
      doc.text('14', x + breite, y + 7.8, { align: 'right' });
    }
  }
  function farbBalkenPng(farben, breite, hoehe) {
    var c = document.createElement('canvas');
    c.width = breite; c.height = hoehe;
    var ctx = c.getContext('2d'), g = ctx.createLinearGradient(0, 0, breite, 0);
    farben.forEach(function(f, i) { g.addColorStop(farben.length > 1 ? i / (farben.length - 1) : 0, f); });
    ctx.fillStyle = g; ctx.fillRect(0, 0, breite, hoehe);
    return c.toDataURL('image/png');
  }

  // --- Aktionen ---
  function exportBilder(objekte, formate, faktor) {
    var auftraege = [];
    var ebeneP = aktuelleEbeneBild(selectedWeek);
    objekte.forEach(function(obj) {
      formate.forEach(function(fmt) {
        auftraege.push(ebeneP.then(function(bild) {
          var fig = obj === 'karte'
            ? kartenFigur({ ansicht: 'aktuell', bilder: kartenBilderFuer(selectedWeek, bild, true), titel: kartenTitel(selectedWeek), mitSpuren: true })
            : kurvenFigur({});
          return figurBild(fig, fmt, faktor).then(function(url) { return { name: exportName(obj) + '.' + fmt, url: url }; });
        }));
      });
    });
    return Promise.all(auftraege).then(function(dateien) {
      if (dateien.length === 1) { herunterladen(dateien[0].url, dateien[0].name); return; }
      return ladeBibliothek('jszip').then(function() {
        var zip = new JSZip();
        dateien.forEach(function(d) { zip.file(d.name, dataUrlInhalt(d.url)); });
        return zip.generateAsync({ type: 'blob' }).then(function(blob) { herunterladen(blob, exportName('bilder') + '.zip'); });
      });
    });
  }
  function exportZeitserie(von, bis, objekte, format, fortschritt) {
    var wochen = [];
    for (var w = von; w <= bis; w++) wochen.push(w);
    var ebeneDaten = hintergrundEbene === 'keine' ? Promise.resolve(null) : ladeEbeneP(ebeneDateiSchluessel(hintergrundEbene));
    return Promise.all([ladeBibliothek('jszip'), ebeneDaten]).then(function(r) {
      var daten = r[1], zip = new JSZip(), schritt = 0, gesamt = wochen.length * objekte.length;
      var kette = Promise.resolve();
      wochen.forEach(function(woche) {
        objekte.forEach(function(obj) {
          kette = kette.then(function() {
            var bild = daten ? (daten.bilder || {})[selectedYear + ' ' + woche] : null;
            var fig = obj === 'karte'
              ? kartenFigur({ ansicht: 'voll', bilder: kartenBilderFuer(woche, bild, true), titel: kartenTitel(woche) })
              : kurvenFigur({ woche: woche });
            return figurBild(fig, format, 1).then(function(url) {
              zip.file(exportName(obj, woche) + '.' + format, dataUrlInhalt(url));
              fortschritt(++schritt, gesamt);
            });
          });
        });
      });
      return kette.then(function() { return zip.generateAsync({ type: 'blob' }); }).then(function(blob) {
        herunterladen(blob, 'graswachstum_' + selectedYear + '_KW' + zweistellig(von) + '-KW' + zweistellig(bis) + '.zip');
      });
    });
  }
  function pdfKopf(doc, titel, unter) {
    doc.setFont('helvetica', 'bold'); doc.setFontSize(16); doc.setTextColor(30);
    doc.text(titel, 12, 16);
    doc.setFont('helvetica', 'normal'); doc.setFontSize(9); doc.setTextColor(100);
    doc.text(unter, 12, 22);
    doc.setDrawColor(210); doc.line(12, 25, 198, 25);
  }
  function pdfFuss(doc, quellen) {
    doc.setFontSize(7); doc.setTextColor(120);
    var zeilen = doc.splitTextToSize(quellen, 186);
    doc.text(zeilen, 12, 297 - 6 - (zeilen.length - 1) * 3);
  }
  function heuteText() { var d = new Date(); return zweistellig(d.getDate()) + '.' + zweistellig(d.getMonth() + 1) + '.' + d.getFullYear(); }
  function exportPdfBericht(fortschritt) {
    return Promise.all([ladeBibliothek('jspdf'), aktuelleEbeneBild(selectedWeek), ladeBibliothek('qrcode')]).then(function(r) {
      var doc = new window.jspdf.jsPDF({ unit: 'mm', format: 'a4', orientation: 'portrait' });
      pdfKopf(doc, 'Graswachstum ' + wocheText(selectedWeek), 'Bericht aus dem Datenexplorer · erstellt am ' + heuteText() + ' · graswachstum.ch');
      var kf = kartenFigur({ ansicht: 'aktuell', bilder: kartenBilderFuer(selectedWeek, r[1], true), breite: 1500, mitSpuren: true });
      return figurBild(kf, 'png', 1).then(alsJpeg).then(function(kUrl) {
        fortschritt(1, 3);
        var y = 29, b = 186, kH = Math.min(118, b * kf.layout.height / kf.layout.width), kB = kH * kf.layout.width / kf.layout.height;
        doc.addImage(kUrl, 'JPEG', 12 + (b - kB) / 2, y, kB, kH);
        y += kH + 4;
        var info = layerLegenden[hintergrundEbene];
        if (info) {
          doc.setFont('helvetica', 'bold'); doc.setFontSize(8.5); doc.setTextColor(30);
          doc.text('Hintergrund: ' + ebenenTitelPdf(hintergrundEbene, meteoFenster), 12, y + 3);
          y += 5;
          y += pdfEbenenLegende(doc, hintergrundEbene, meteoFenster, 12, y, 110, false) + 2;
        }
        y += pdfMessLegende(doc, 12, y + 3, 186, selectedWeek) + 3;
        var cf = kurvenFigur({ breite: 1500, hoehe: Math.max(520, Math.round(1500 * (297 - 40 - y) / 186)) });
        return figurBild(cf, 'png', 1).then(alsJpeg).then(function(cUrl) {
          fortschritt(2, 3);
          y += 3;
          if (y > 297 - 80) { doc.addPage(); y = 14; }
          var cH = Math.min(297 - 36 - y, b * cf.layout.height / cf.layout.width), cB = cH * cf.layout.width / cf.layout.height;
          doc.addImage(cUrl, 'JPEG', 12 + (b - cB) / 2, y, cB, cH);
          pdfQr(doc, 12, 297 - 32, 16, 'Mehr Erklärungen zu Karte, Kurve und Ebenen in der Hilfe des Datenexplorers:');
          pdfFuss(doc, 'Quellen: Messnetz Graswachstum der AGFF (graswachstum.ch)' + (info ? '; ' + info.quelle : '') + '. Erstellt mit dem Datenexplorer Graswachstum.');
          doc.save(exportName('bericht') + '.pdf');
          fortschritt(3, 3);
        });
      });
    });
  }
  var PDF_EBENEN = ['niederschlag', 'temperatur', 'bodentemperatur', 'sonnenschein', 'et0', 'gdd', 'boden'];
  function exportPdfEbenen(mitStandorten, fortschritt) {
    var zellen = [{ name: 'keine', titel: 'Graswachstum und DGV' }].concat(PDF_EBENEN.filter(function(n) { return layerLegenden[n]; }).map(function(n) {
      var fenster = istFensterEbene(n) ? meteoFensterStandard[n] : null;
      return { name: n, fenster: fenster, schluessel: fenster ? n + '_' + fenster : n, titel: ebenenTitelPdf(n, fenster) };
    }));
    return Promise.all([ladeBibliothek('jspdf'), ladeBibliothek('qrcode')]).then(function() {
      var doc = new window.jspdf.jsPDF({ unit: 'mm', format: 'a4', orientation: 'portrait' });
      pdfKopf(doc, 'Wetter-Ebenen ' + wocheText(selectedWeek), 'Stichtag jeweils Montag der Woche · erstellt am ' + heuteText() + ' · graswachstum.ch');
      var spalten = 2, zellB = 91, zellH = 56, x0 = 12, y0 = 29, abstand = 4;
      var kette = Promise.resolve(), schritt = 0;
      zellen.forEach(function(z, i) {
        kette = kette.then(function() {
          var daten = z.name === 'keine' ? Promise.resolve(null) : ladeEbeneP(z.schluessel).catch(function() { return null; });
          return daten.then(function(d) {
            var k = selectedYear + ' ' + selectedWeek;
            var bild = d ? (d.bilder || {})[k] : null;
            var stand = d && d.werte && d.werte[k] && d.werte[k].bis ? 'Stand ' + d.werte[k].bis : '';
            var fig = kartenFigur({ ansicht: 'voll', bilder: kartenBilderFuer(selectedWeek, bild, mitStandorten || z.name === 'keine'), breite: 900 });
            return figurBild(fig, 'png', 1).then(alsJpeg).then(function(url) {
              var x = x0 + (i % spalten) * (zellB + abstand), y = y0 + Math.floor(i / spalten) * zellH;
              doc.setFont('helvetica', 'bold'); doc.setFontSize(8.5); doc.setTextColor(30);
              doc.text(z.titel, x, y + 3);
              doc.setFont('helvetica', 'normal'); doc.setFontSize(7); doc.setTextColor(110);
              if (stand) doc.text(stand, x + zellB, y + 3, { align: 'right' });
              var mH = Math.min(zellH - 14, zellB * fig.layout.height / fig.layout.width), mB = mH * fig.layout.width / fig.layout.height;
              doc.addImage(url, 'JPEG', x + (zellB - mB) / 2, y + 5, mB, mH);
              var info = layerLegenden[z.name];
              if (info && bild) {
                pdfEbenenLegende(doc, z.name, z.fenster || 7, x + 8, y + mH + 6.5, zellB - 16, true);
              } else if (info) {
                doc.setFontSize(7); doc.setTextColor(150);
                doc.text('keine Daten für diese Woche', x + zellB / 2, y + mH + 10, { align: 'center' });
              } else {
                pdfMiniMessLegende(doc, x + 6, y + mH + 5.5, zellB - 12, selectedWeek);
              }
              fortschritt(++schritt, zellen.length);
            });
          });
        });
      });
      return kette.then(function() {
        var yQ = Math.min(297 - 36, y0 + Math.ceil(zellen.length / spalten) * zellH + 4);
        pdfQr(doc, 12, yQ, 18, 'Ausführliche Legenden und Erklärungen zu allen Ebenen in der Hilfe des Datenexplorers:');
        pdfFuss(doc, 'Quellen: Messnetz Graswachstum der AGFF (graswachstum.ch); MeteoSchweiz Open Data, 1-km-Gitterdaten (RhiresD/RprelimD Niederschlag, TabsD Temperatur, SrelD Sonnenschein). Bodentemperatur (Schätzung), Verdunstung ET0 (Hargreaves), Wachstumsgradtage (Basis 5 °C) und Bodenwasserbilanz sind daraus berechnet.');
        doc.save(exportName('wetter-ebenen') + '.pdf');
      });
    });
  }

  // --- Veranstaltungskarte: eigene Orte auf Themenkarten ---
  // Je Thema eine Seite (A4 quer): Kartenausschnitt mit nummerierten Orten,
  // Legende, Wertetabelle und QR-Code zur App (Link mit Jahr, Woche, Ebene,
  // Ausschnitt und den Orten). Ausgabe als PDF, SVG (eine Datei je Thema) und
  // als interaktive HTML-Datei (Themen umschalten, Werte je Ort). Die Marker
  // zeichnet der Export selbst (nicht Plotly), damit sie in allen Formaten
  // gleich aussehen und in HTML/SVG anklickbar bzw. bearbeitbar bleiben.
  var VERANSTALTUNG_THEMEN = [
    // art: 'summe' = aufsummiert seit 1.1. (Zeitraum = Differenz zweier Stichtage), 'zustand' = Stand je Woche (Zeitraum = Mittel/Tiefstwert)
    { name: 'niederschlag_kum', titel: 'Niederschlag seit 1. Januar', kurz: 'Niederschlag', art: 'summe' },
    { name: 'boden', titel: 'Bodenwasserbilanz', kurz: 'Bodenwasser', art: 'zustand', standard: [0, 130] },
    { name: 'wachstumspotenzial_kum', titel: 'Potenzielles Graswachstum seit 1. Januar', kurz: 'Potenzielles Graswachstum', art: 'summe', experimentell: true, faktor: 0.01, einheit: 'dt TS/ha', standard: [0, 150] }
  ];
  // Erklaerungen der Methode (kurz, einfach): erscheinen auf der Seite, wenn neben der Karte Platz ist
  var METHODIK = {
    niederschlag_kum: ['Niederschlag: Tagessummen der 1-km-Gitterdaten von MeteoSchweiz (Radar und Messstationen), seit 1. Januar bzw. über die letzten 28 Tage bis zum Stichtag addiert. 1 mm = 1 Liter pro m².',
      'Der Wert gilt für das 1-km-Feld des Ortes. Einzelne Gewitter können lokal deutlich abweichen.'],
    boden: ['Wasser im Boden: Der Boden wird als Speicher von 130 mm gerechnet (wie im Wachstumsmodell). Ab 1. April (Speicher voll) füllt der Niederschlag ihn auf, die aus der Temperatur berechnete Verdunstung leert ihn.',
      'Je tiefer der Wert, desto trockener. Bodenart, Gründigkeit, Hanglage und Bewässerung sind nicht berücksichtigt.'],
    wachstumspotenzial_kum: ['Das Modell ModVege/growR rechnet täglich aus Temperatur, Sonnenstrahlung und Bodenwasser, wie viel Gras unter guten Bedingungen wachsen könnte: ohne Nährstoffmangel, Schnitt und Beweidung.',
      'Es zeigt, was das Wetter hergibt, und ist keine Ertragsprognose. Echte Bestände wachsen meist weniger.']
  };
  // Text auf eine Breite umbrechen; messen(text) liefert die Breite in mm
  function umbrechen(text, breite, messen) {
    var zeilen = [], z = '';
    String(text).split(' ').forEach(function(w) {
      var t = z ? z + ' ' + w : w;
      if (z && messen(t) > breite) { zeilen.push(z); z = w; } else z = t;
    });
    if (z) zeilen.push(z);
    return zeilen;
  }
  // Absaetze, die in die Hoehe passen (ganze Absaetze); [] wenn schon der erste nicht passt
  function methodikAbsaetze(thema, breite, hoehe, schriftMm, messen) {
    var abs = METHODIK[thema.name] || [], zh = schriftMm * 1.4, h = 6.2, aus = [];
    for (var i = 0; i < abs.length; i++) {
      var z = umbrechen(abs[i], breite, messen), bedarf = z.length * zh + 1.6;
      if (h + bedarf > hoehe) break;
      h += bedarf; aus.push(z);
    }
    return { absaetze: aus, zeilenH: zh };
  }
  // --- Kurzlinks: QR-Codes mit langen Links (alle Orte stehen darin) sind nur gross scanbar ---
  // POST an <graswachstum.ch>/g (Plugin datenexplorer) liefert {kurz: 'https://graswachstum.ch/g/abc234'};
  // ohne Antwort (offline, Fehler) bleibt der lange Link.
  function kurzDienstUrl() {
    if (window.GW_KURZ_URL) return window.GW_KURZ_URL;
    var h = window.location.hostname;
    return /(^|\.)graswachstum\.ch$/.test(h) && h !== 'apps.graswachstum.ch' ? window.location.origin + '/g' : 'https://graswachstum.ch/g';
  }
  var kurzCache = {};
  function kurzLink(lang) {
    if (!kurzCache[lang]) {
      var ctl = typeof AbortController === 'function' ? new AbortController() : null;
      var t = ctl ? setTimeout(function() { ctl.abort(); }, 8000) : null;
      kurzCache[lang] = fetch(kurzDienstUrl(), { method: 'POST', headers: { 'Content-Type': 'text/plain;charset=utf-8' }, body: JSON.stringify({ url: lang }), signal: ctl ? ctl.signal : undefined })
        .then(function(r) { return r.ok ? r.json() : null; })
        .then(function(d) { return d && d.kurz ? d.kurz : null; })
        .catch(function() { return null; })
        .then(function(k) { if (t) clearTimeout(t); if (!k) delete kurzCache[lang]; return k; });
    }
    return kurzCache[lang];
  }
  function ohneProtokoll(u) { return String(u).replace(/^https?:\/\//, ''); }
  var SEITE = { b: 297, h: 210, rand: 12, karteB: 188, karteH: 163, spalteX: 207, spalteB: 78 };
  function appBasisUrl() {
    var h = window.location.hostname;
    if (/(^|\.)graswachstum\.ch$/.test(h) && h !== 'apps.graswachstum.ch') return window.location.origin + window.location.pathname;
    return 'https://graswachstum.ch/de/growth';
  }
  function veranstaltungsLink(thema, woche, aus) {
    var enc = function(v) { return encodeURIComponent(v).replace(/%2C/g, ',').replace(/%20/g, '+').replace(/%7E/gi, '~'); };
    var p = ['jahr=' + selectedYear, 'kw=' + woche, 'ebene=' + thema.name,
      'karte=' + [aus.xr[0], aus.xr[1], aus.yr[0], aus.yr[1]].map(function(v) { return v.toFixed(3); }).join(',')];
    if (eigeneOrte.length) { p.push('orte=' + enc(eigeneOrte.map(ortToken).join(','))); p.push('an=orte'); }
    return appBasisUrl() + (thema.experimentell ? '?experimentell' : '') + '#' + p.join('&');
  }
  // Link auf den Bericht selbst (ber=1): oeffnet die interaktive Ansicht mit den Orten, Themen und dem Ausschnitt
  function berichtLink(opt, aus) {
    var enc = function(v) { return encodeURIComponent(v).replace(/%2C/g, ',').replace(/%20/g, '+').replace(/%7E/gi, '~'); };
    var themen = opt.themen.filter(function(n) { return VERANSTALTUNG_THEMEN.some(function(t) { return t.name === n; }); });
    var exp = themen.some(function(n) { return VERANSTALTUNG_THEMEN.some(function(t) { return t.name === n && t.experimentell; }); });
    var p = ['ber=1', 'jahr=' + selectedYear, 'kw=' + selectedWeek, 'bt=' + themen.join(','),
      'karte=' + [aus.xr[0], aus.xr[1], aus.yr[0], aus.yr[1]].map(function(v) { return v.toFixed(3); }).join(','),
      'orte=' + enc(eigeneOrte.map(ortToken).join(','))];
    if (opt.titel) p.push('bti=' + enc(opt.titel));
    if (!opt.namen) p.push('bn=0');
    if (opt.messstandorte) p.push('bm=1');
    if (!opt.landeskarte) p.push('bk=0');
    if (opt.skala === 'explorer') p.push('bs=explorer');
    return appBasisUrl() + (exp ? '?experimentell' : '') + '#' + p.join('&');
  }
  // Geteilter Bericht: Vollbildansicht ueber dem Datenexplorer (HTML-Bericht wird im Browser aufgebaut)
  var berichtGezeigt = false, berichtHash = null;
  function berichtAnzeigen(q) {
    if (berichtGezeigt) return;
    berichtGezeigt = true;
    berichtHash = ['ber=1', 'bt=' + (q.bt || ''), 'karte=' + (q.karte || '')].concat(q.bs === 'explorer' ? ['bs=explorer'] : [], q.bti ? ['bti=' + encodeURIComponent(q.bti).replace(/%20/g, '+')] : [], q.bn === '0' ? ['bn=0'] : [], q.bm === '1' ? ['bm=1'] : [], q.bk === '0' ? ['bk=0'] : []);
    var k = (q.karte || '').split(',').map(parseFloat);
    var opt = {
      themen: (q.bt || 'niederschlag_kum,boden').split(',').filter(Boolean), formate: ['html'], ausschnitt: 'orte',
      namen: q.bn !== '0', messstandorte: q.bm === '1', titel: q.bti || '', landeskarte: q.bk !== '0', nurHtml: true, skala: q.bs === 'explorer' ? 'explorer' : 'ausschnitt',
      aus: k.length === 4 && k.every(isFinite) ? { xr: [k[0], k[1]], yr: [k[2], k[3]] } : null
    };
    var d = GWDatenexplorer.berichtLaden() || document.createElement('div');
    var laden = d.querySelector('.gw-bericht-laden');
    var leiste = document.createElement('div');
    leiste.style.cssText = 'display:flex;flex-wrap:wrap;gap:8px;align-items:center;padding:8px 12px;background:#fff;border-bottom:1px solid #d5ddd8;font-size:14px';
    var status = document.createElement('span');
    status.style.cssText = 'flex:1;min-width:160px;color:#52514e';
    status.textContent = 'Bericht wird aufgebaut … (Karten und Werte werden geladen)';
    var knopf = function(text, fn) {
      var b = document.createElement('button');
      b.type = 'button'; b.textContent = text;
      b.style.cssText = 'font:inherit;padding:6px 12px;border:1px solid #2f6f5e;border-radius:16px;background:#fff;color:#2f6f5e;cursor:pointer';
      b.addEventListener('click', fn);
      return b;
    };
    var rahmen = document.createElement('iframe');
    rahmen.title = 'Bericht';
    rahmen.setAttribute('sandbox', 'allow-scripts allow-popups allow-popups-to-escape-sandbox');   // Inhalt stammt aus dem Link: keine Rechte der Seite
    rahmen.style.cssText = 'flex:1;border:0;width:100%;background:#eef1ee';
    var html = null;
    leiste.appendChild(status);
    leiste.appendChild(knopf('Als Datei speichern', function() { if (html) herunterladen(new Blob([html], { type: 'text/html;charset=utf-8' }), 'graswachstum_bericht_' + selectedYear + '-KW' + zweistellig(selectedWeek) + '.html'); }));
    leiste.appendChild(knopf('Im Datenexplorer öffnen', function() { berichtHash = null; berichtGezeigt = false; d.remove(); schreibeUrlBald(); }));
    leiste.style.display = 'none'; rahmen.style.display = 'none';
    d.appendChild(leiste); d.appendChild(rahmen);
    if (!d.parentNode) document.body.appendChild(d);
    var fertig = function() { if (laden) laden.remove(); leiste.style.display = 'flex'; rahmen.style.display = 'block'; };
    Promise.all([ladeBibliothek('qrcode')]).then(function() { return exportVeranstaltung(opt, function() {}); }).then(function(h) {
      html = h;
      rahmen.addEventListener('load', fertig, { once: true });
      rahmen.srcdoc = h;
      status.textContent = (q.bti ? q.bti + ' · ' : '') + 'Bericht ' + wocheText(selectedWeek);
    }).catch(function(e) {
      console.error('Bericht:', e);
      status.textContent = 'Der Bericht konnte nicht aufgebaut werden: ' + (e && e.message ? e.message : e);
      status.style.color = '#b00020';
      fertig();
    });
  }
  // Ausschnitt mit Seitenverhaeltnis verhaeltnis (Breite/Hoehe in Pixeln)
  function veranstaltungsAusschnitt(art, verhaeltnis) {
    var info = GWDatenexplorer.kartenInfo, gd = kartenGd(), sr = info.scaleratio;
    var cx, cy, sx, sy;
    var gx = gd.layout.xaxis.range, gy = gd.layout.yaxis.range;
    if (art === 'orte' && eigeneOrte.length) {
      var lons = eigeneOrte.map(function(o) { return o.lon; }), lats = eigeneOrte.map(function(o) { return o.lat; });
      var x0 = Math.min.apply(null, lons), x1 = Math.max.apply(null, lons), y0 = Math.min.apply(null, lats), y1 = Math.max.apply(null, lats);
      cx = (x0 + x1) / 2; cy = (y0 + y1) / 2;
      sx = Math.max((x1 - x0) * 1.35, 0.3); sy = Math.max((y1 - y0) * 1.35, 0.3 / sr);
    } else if (art === 'aktuell' && gx && (gx[1] - gx[0]) < (info.xMaxSchweiz - info.xMin) * 0.97) {
      cx = (gx[0] + gx[1]) / 2; cy = (gy[0] + gy[1]) / 2; sx = gx[1] - gx[0]; sy = gy[1] - gy[0];
    } else {
      cx = (info.xMin + info.xMaxSchweiz) / 2; cy = info.yMitte; sx = info.xMaxSchweiz - info.xMin; sy = info.ySpanSchweiz * 1.04;
    }
    // auf das Seitenverhaeltnis der Kartenflaeche erweitern
    if (sx / (sy * sr) < verhaeltnis) sx = verhaeltnis * sy * sr; else sy = sx / (verhaeltnis * sr);
    return { xr: [cx - sx / 2, cx + sx / 2], yr: [cy - sy / 2, cy + sy / 2] };
  }
  // Kartenbild des Ausschnitts als Canvas: Ebene scharf (Gitterzellen ohne
  // Weichzeichnen), darueber die graue Landeskarte von swisstopo im Modus
  // «multiplizieren» (Ortschaften, Strassen, Relief bleiben lesbar). Ohne
  // Landeskarte (offline) die Kantons-/Seenkarte des Datenexplorers.
  function bildLaden(src, mitCors) {
    return new Promise(function(ok, fehler) {
      var img = new Image();
      if (mitCors) img.crossOrigin = 'anonymous';
      img.onload = function() { ok(img); };
      img.onerror = function() { fehler(new Error('Bild konnte nicht geladen werden')); };
      img.src = src;
    });
  }
  var landeskartenCache = {};
  function landeskarte(aus, breite, hoehe) {
    var url = 'https://wms.geo.admin.ch/?SERVICE=WMS&VERSION=1.3.0&REQUEST=GetMap&LAYERS=ch.swisstopo.pixelkarte-grau&STYLES=&CRS=EPSG:4326' +
      '&BBOX=' + [aus.yr[0], aus.xr[0], aus.yr[1], aus.xr[1]].map(function(v) { return v.toFixed(5); }).join(',') +
      '&WIDTH=' + breite + '&HEIGHT=' + hoehe + '&FORMAT=image/png';
    if (!landeskartenCache[url]) landeskartenCache[url] = bildLaden(url, true).catch(function() { delete landeskartenCache[url]; return null; });
    return landeskartenCache[url];
  }
  function zeichneGeoBild(ctx, img, b, aus, breite, hoehe) {
    var x0 = b.x - (b.xanchor === 'right' ? b.sizex : b.xanchor === 'center' ? b.sizex / 2 : 0);
    var y0 = b.y + (b.yanchor === 'bottom' ? b.sizey : b.yanchor === 'middle' ? b.sizey / 2 : 0);
    var px = function(lon) { return (lon - aus.xr[0]) / (aus.xr[1] - aus.xr[0]) * breite; };
    var py = function(lat) { return (aus.yr[1] - lat) / (aus.yr[1] - aus.yr[0]) * hoehe; };
    ctx.drawImage(img, px(x0), py(y0), px(x0 + b.sizex) - px(x0), py(y0 - b.sizey) - py(y0));
  }
  // --- Karten aus Rohwerten (statt fertiger Bilder): Ausschnitt, eigene Farbskala ---
  // Gitter der Ebenen (werte: x0..x1, y0..y1, ncol x nrow, m = Zeilen von Norden) auf den
  // Kartenausschnitt zuschneiden; faktor rechnet die Einheit um (kg -> dt: 0.01).
  function zuschnitt(gitter, aus, faktor) {
    if (!gitter || !gitter.m) return null;
    var nc = gitter.ncol, nr = gitter.nrow, dx = (gitter.x1 - gitter.x0) / nc, dy = (gitter.y1 - gitter.y0) / nr;
    var c0 = Math.max(0, Math.floor((aus.xr[0] - gitter.x0) / dx) - 1), c1 = Math.min(nc - 1, Math.ceil((aus.xr[1] - gitter.x0) / dx) + 1);
    var r0 = Math.max(0, Math.floor((gitter.y1 - aus.yr[1]) / dy) - 1), r1 = Math.min(nr - 1, Math.ceil((gitter.y1 - aus.yr[0]) / dy) + 1);
    if (c1 < c0 || r1 < r0) return null;
    var w = c1 - c0 + 1, h = r1 - r0 + 1, v = new Float32Array(w * h), f = faktor || 1;
    for (var r = 0; r < h; r++) {
      var zeile = gitter.m[r0 + r] || [];
      for (var c = 0; c < w; c++) { var x = zeile[c0 + c]; v[r * w + c] = (x === null || x === undefined || isNaN(x)) ? NaN : x * f; }
    }
    return { x0: gitter.x0 + c0 * dx, x1: gitter.x0 + (c1 + 1) * dx, y1: gitter.y1 - r0 * dy, y0: gitter.y1 - (r1 + 1) * dy, ncol: w, nrow: h, v: v };
  }
  function zWert(z, lon, lat) {
    if (!z) return null;
    var c = Math.floor((lon - z.x0) / (z.x1 - z.x0) * z.ncol), r = Math.floor((z.y1 - lat) / (z.y1 - z.y0) * z.nrow);
    if (c < 0 || c >= z.ncol || r < 0 || r >= z.nrow) return null;
    var w = z.v[r * z.ncol + c];
    return isNaN(w) ? null : w;
  }
  // Farbskala an den Ausschnitt angepasst: 5.–95. Perzentil der Werte im Ausschnitt, auf runde Werte
  function farbskalaAuto(z, aus, voll) {
    var l = [];
    if (z) {
      for (var r = 0; r < z.nrow; r++) for (var c = 0; c < z.ncol; c++) {
        var lon = z.x0 + (c + 0.5) * (z.x1 - z.x0) / z.ncol, lat = z.y1 - (r + 0.5) * (z.y1 - z.y0) / z.nrow, w = z.v[r * z.ncol + c];
        if (!isNaN(w) && lon >= aus.xr[0] && lon <= aus.xr[1] && lat >= aus.yr[0] && lat <= aus.yr[1]) l.push(w);
      }
    }
    if (l.length < 4) return voll.slice();
    l.sort(function(a, b) { return a - b; });
    var lo = l[Math.floor(l.length * 0.05)], hi = l[Math.ceil(l.length * 0.95) - 1];
    if (hi - lo < (voll[1] - voll[0]) * 0.04) { var m = (lo + hi) / 2, h = (voll[1] - voll[0]) * 0.02; lo = m - h; hi = m + h; }
    var schritt = netteSchrittweite((hi - lo) / 5);
    lo = Math.max(Math.min(0, voll[0]), Math.floor(lo / schritt) * schritt);
    hi = Math.ceil(hi / schritt) * schritt;
    if (hi <= lo) hi = lo + schritt;
    return [lo, hi];
  }
  // Beschriftung der Farbskala: Anfang, Ende und runde Zwischenwerte
  function skalaMarken(lo, hi) {
    var st = netteSchrittweite((hi - lo) / 4), l = [{ v: lo, f: 0 }];
    for (var v = Math.ceil((lo + st * 0.35) / st) * st; v < hi - st * 0.35; v += st) l.push({ v: v, f: (v - lo) / (hi - lo) });
    l.push({ v: hi, f: 1 });
    return l;
  }
  function hexRgb(h) { h = String(h).replace('#', ''); if (h.length === 3) h = h.replace(/./g, '$&$&'); return [parseInt(h.slice(0, 2), 16), parseInt(h.slice(2, 4), 16), parseInt(h.slice(4, 6), 16)]; }
  // Raster als Canvas (eine Zelle = ein Pixel), Farben linear zwischen den Stufen, Deckkraft 75 % wie im Datenexplorer
  function rasterCanvas(z, lo, hi, farben) {
    var c = document.createElement('canvas');
    c.width = z.ncol; c.height = z.nrow;
    var ctx = c.getContext('2d'), img = ctx.createImageData(z.ncol, z.nrow), rgb = farben.map(hexRgb), n = rgb.length - 1;
    for (var i = 0; i < z.v.length; i++) {
      var w = z.v[i];
      if (isNaN(w)) continue;
      var a = Math.max(0, Math.min(1, (w - lo) / ((hi - lo) || 1))) * n, k = Math.min(n - 1, Math.floor(a)), f = a - k, p = rgb[k], q = rgb[Math.min(n, k + 1)];
      img.data[i * 4] = p[0] + (q[0] - p[0]) * f; img.data[i * 4 + 1] = p[1] + (q[1] - p[1]) * f; img.data[i * 4 + 2] = p[2] + (q[2] - p[2]) * f; img.data[i * 4 + 3] = 191;
    }
    ctx.putImageData(img, 0, 0);
    return c;
  }
  function zeichneRaster(ctx, rc, z, aus, breite, hoehe) {
    var px = function(lon) { return (lon - aus.xr[0]) / (aus.xr[1] - aus.xr[0]) * breite; };
    var py = function(lat) { return (aus.yr[1] - lat) / (aus.yr[1] - aus.yr[0]) * hoehe; };
    ctx.imageSmoothingEnabled = true;
    ctx.imageSmoothingQuality = 'high';
    ctx.drawImage(rc, px(z.x0), py(z.y1), px(z.x1) - px(z.x0), py(z.y0) - py(z.y1));
  }
  // Fuer die HTML-Ausgabe: Gitter kompakt (Int16, Wert x10, fehlend = -32768) als base64
  function zKodieren(z) {
    if (!z) return null;
    var a = new Int16Array(z.v.length);
    for (var i = 0; i < a.length; i++) a[i] = isNaN(z.v[i]) ? -32768 : Math.max(-32767, Math.min(32767, Math.round(z.v[i] * 10)));
    var b = new Uint8Array(a.buffer), s = '';
    for (var k = 0; k < b.length; k += 8192) s += String.fromCharCode.apply(null, b.subarray(k, k + 8192));
    return { x0: z.x0, x1: z.x1, y0: z.y0, y1: z.y1, ncol: z.ncol, nrow: z.nrow, b: btoa(s) };
  }

  // Kartenbild des Ausschnitts: Ebene aus den Rohwerten (Farbskala t.skala), darueber die Landeskarte
  // («multiplizieren»), oben die Messstandorte. Liefert auch die Teile einzeln (HTML: Skala aenderbar).
  function veranstaltungsKarte(aus, t, opt, breite, hoehe) {
    var mess = opt.messstandorte ? kartenBilderFuer(t.woche, null, true).slice(1) : [];
    return Promise.all([
      // doppelte Aufloesung: swisstopo zeichnet dann die detailliertere Stufe
      // mit verhaeltnismaessig kleineren Ortsnamen
      opt.landeskarte ? landeskarte(aus, breite * 2, hoehe * 2) : Promise.resolve(null),
      Promise.all(mess.map(function(b) { return bildLaden(b.source).then(function(i) { return { b: b, img: i }; }); })),
      opt.landeskarte ? Promise.resolve(null) : bildLaden(kartenbildHintergrund.source).then(function(i) { return { b: kartenbildHintergrund, img: i }; })
    ]).then(function(r) {
      var neu = function() { var c = document.createElement('canvas'); c.width = breite; c.height = hoehe; return c; };
      var karte = r[0];
      // Grundkarte (weiss + Landeskarte, sonst Kantone/Seen)
      var basis = neu(), bctx = basis.getContext('2d');
      bctx.fillStyle = 'white'; bctx.fillRect(0, 0, breite, hoehe);
      if (karte) bctx.drawImage(karte, 0, 0, breite, hoehe);
      else if (r[2]) zeichneGeoBild(bctx, r[2].img, r[2].b, aus, breite, hoehe);
      // Messstandorte (durchsichtig)
      var oben = null;
      if (r[1].length) { oben = neu(); var octx = oben.getContext('2d'); r[1].forEach(function(e) { zeichneGeoBild(octx, e.img, e.b, aus, breite, hoehe); }); }
      // zusammengesetzt (PDF, SVG-Datei)
      var c = neu(), ctx = c.getContext('2d');
      ctx.fillStyle = 'white'; ctx.fillRect(0, 0, breite, hoehe);
      if (!karte) ctx.drawImage(basis, 0, 0);
      if (t.z) zeichneRaster(ctx, rasterCanvas(t.z, t.skala[0], t.skala[1], t.info.farben), t.z, aus, breite, hoehe);
      if (karte) { ctx.globalCompositeOperation = 'multiply'; ctx.drawImage(karte, 0, 0, breite, hoehe); ctx.globalCompositeOperation = 'source-over'; }
      if (oben) ctx.drawImage(oben, 0, 0);
      return { canvas: c, basis: basis, oben: oben, landeskarte: !!karte };
    });
  }
  // Beschriftungen ohne Ueberlappung: rechts, links, oben oder unten vom
  // Marker; passt nichts, bleibt nur die Nummer (Name steht in der Tabelle).
  // Masse in mm der Kartenflaeche (kb x kh), Schriftgroesse fs in mm.
  function beschriftungen(pos, kb, kh, fs, mitNamen) {
    var belegt = pos.map(function(p) { return p.x < 0 || p.x > 1 || p.y < 0 || p.y > 1 ? null : { x0: p.x * kb - 2.8, x1: p.x * kb + 2.8, y0: p.y * kh - 2.8, y1: p.y * kh + 2.8 }; });
    var frei = function(q) {
      if (q.x0 < 0.5 || q.x1 > kb - 0.5 || q.y0 < 0.5 || q.y1 > kh - 0.5) return false;
      return belegt.every(function(b) { return !b || q.x1 < b.x0 || q.x0 > b.x1 || q.y1 < b.y0 || q.y0 > b.y1; });
    };
    return pos.map(function(p, i) {
      if (!mitNamen || !belegt[i]) return null;
      var x = p.x * kb, y = p.y * kh, w = eigeneOrte[i].name.length * fs * 0.53 + 0.6, h = fs * 1.05;
      var kandidaten = [
        { anker: 'start', tx: x + 3.4, ty: y + fs * 0.36, q: { x0: x + 3.1, x1: x + 3.4 + w, y0: y - h / 2, y1: y + h / 2 } },
        { anker: 'end', tx: x - 3.4, ty: y + fs * 0.36, q: { x0: x - 3.4 - w, x1: x - 3.1, y0: y - h / 2, y1: y + h / 2 } },
        { anker: 'middle', tx: x, ty: y - 3.3, q: { x0: x - w / 2, x1: x + w / 2, y0: y - 3.3 - h * 0.8, y1: y - 3.0 + h * 0.2 } },
        { anker: 'middle', tx: x, ty: y + 3.3 + fs * 0.8, q: { x0: x - w / 2, x1: x + w / 2, y0: y + 3.0, y1: y + 3.3 + h } }
      ];
      for (var k = 0; k < kandidaten.length; k++) {
        if (frei(kandidaten[k].q)) { belegt.push(kandidaten[k].q); return kandidaten[k]; }
      }
      return null;
    });
  }
  function gitterWert(gitter, lon, lat) {
    if (!gitter || !gitter.m) return null;
    var col = Math.floor((lon - gitter.x0) / (gitter.x1 - gitter.x0) * gitter.ncol);
    var row = Math.floor((gitter.y1 - lat) / (gitter.y1 - gitter.y0) * gitter.nrow);
    if (col < 0 || col >= gitter.ncol || row < 0 || row >= gitter.nrow) return null;
    var w = gitter.m[row][col];
    return (w === null || w === undefined || isNaN(w)) ? null : w;
  }
  function tausender(n) { return String(n).replace(/\B(?=(\d{3})+(?!\d))/g, '’'); }
  function wertText(w, info) {
    if (w === null) return '–';
    var z = Math.abs(w) >= 100 ? tausender(Math.round(Math.abs(w) >= 2000 ? w / 10 : w) * (Math.abs(w) >= 2000 ? 10 : 1)) : String(Math.round(w * 10) / 10);
    return z + ' ' + String(info.einheit).split(',')[0];
  }
  // Daten je Thema: Rohwerte der Woche (sonst der letzten Woche davor mit Daten) im Ausschnitt,
  // Werte an den Orten, Farbskala (an den Ausschnitt angepasst oder Vorgabe); Niederschlag zusaetzlich
  // 28 Tage und alle Wochen des Jahres (HTML: freier Zeitraum)
  function veranstaltungsThema(thema, woche, aus, opt) {
    opt = opt || {};
    return ladeEbeneP(ebeneDateiSchluessel(thema.name)).then(function(d) {
      var info0 = layerLegenden[thema.name], f = thema.faktor || 1, alleWerte = d.werte || {}, w = woche;
      var info = thema.einheit ? Object.assign({}, info0, { einheit: thema.einheit, bereich: [info0.bereich[0] * f, info0.bereich[1] * f] }) : info0;
      while (w > 0 && !alleWerte[selectedYear + ' ' + w]) w--;
      if (w === 0) throw new Error('Für «' + thema.titel + '» gibt es ' + selectedYear + ' noch keine Daten.');
      var k = selectedYear + ' ' + w, gitter = alleWerte[k];
      var stand = (gitter && gitter.bis) || (d.datum && d.datum[k]) || wochenDatumBereichText(w).split(' – ')[0] + selectedYear;
      var z = zuschnitt(gitter, aus, f), zmax = -Infinity;
      if (z) for (var zi = 0; zi < z.v.length; zi++) if (z.v[zi] > zmax) zmax = z.v[zi];
      var voll = [Math.min(0, info.bereich[0]), Math.max(info.bereich[1], isFinite(zmax) ? Math.ceil(zmax / netteSchrittweite(zmax / 5)) * netteSchrittweite(zmax / 5) : 0)];
      var auto = thema.standard ? thema.standard.slice() : farbskalaAuto(z, aus, voll);
      voll[1] = Math.max(voll[1], auto[1]);
      var erg = {
        thema: thema, info: info, woche: w, stand: stand, z: z, auto: auto, voll: voll,
        skala: opt.skala === 'explorer' ? voll.slice() : auto,
        werte: eigeneOrte.map(function(o) { var x = gitterWert(gitter, o.lon, o.lat); return x === null ? null : x * f; })
      };
      // Alle Wochen des Jahres bis zum Stichtag: im HTML-Bericht laesst sich der Zeitraum waehlen
      // (Summen: Differenz zweier Stichtage; Zustand: Mittel bzw. Tiefstwert der Wochen)
      erg.wochen = {}; erg.wochenStand = {};
      Object.keys(alleWerte).forEach(function(kk) {
        var p = kk.split(' ');
        if (p[0] !== String(selectedYear) || +p[1] > w || !alleWerte[kk].m) return;
        erg.wochen[p[1]] = zuschnitt(alleWerte[kk], aus, f);
        erg.wochenStand[p[1]] = alleWerte[kk].bis || (d.datum && d.datum[kk]) || wochenDatumBereichText(+p[1]).split(' – ')[1] || '';
      });
      if (thema.name !== 'niederschlag_kum') return erg;
      return ladeEbeneP('niederschlag_28').then(function(d2) {
        var g2 = (d2.werte || {})[k];
        if (g2 && g2.m) {
          erg.z28 = zuschnitt(g2, aus, 1);
          erg.werte2 = { kopf: '28 Tage', titel: 'Niederschlag letzte 28 Tage', info: layerLegenden.niederschlag || info,
            werte: eigeneOrte.map(function(o) { return gitterWert(g2, o.lon, o.lat); }) };
        }
        return erg;
      }, function() { return erg; });
    });
  }
  // Fehlerkorrektur L: die Links mit allen Orten sind lang - so bleiben die
  // Punkte gross genug zum Scannen ab Papier
  function qrModule(text, stufe) {
    var qr = window.qrcode(0, stufe || 'L');
    qr.addData(text); qr.make();
    return qr;
  }
  // QR-Code als Vektor ins PDF (Rechtecke je Zeile zusammengefasst) - als
  // PNG legt jsPDF ihn unkomprimiert ab (mehrere MB je Seite)
  function pdfQrVektor(doc, text, x, y, groesse, stufe) {
    var qr = qrModule(text, stufe), n = qr.getModuleCount(), m = groesse / (n + 4);
    doc.setFillColor(255, 255, 255); doc.rect(x, y, groesse, groesse, 'F');
    doc.setFillColor(0, 0, 0);
    for (var r = 0; r < n; r++) {
      for (var k = 0; k < n; k++) {
        if (!qr.isDark(r, k)) continue;
        var anfang = k;
        while (k + 1 < n && qr.isDark(r, k + 1)) k++;
        doc.rect(x + (anfang + 2) * m, y + (r + 2) * m, (k - anfang + 1) * m + 0.01, m + 0.01, 'F');
      }
    }
  }
  function qrSvgPfad(text, x, y, groesse, stufe) {
    var qr = qrModule(text, stufe), n = qr.getModuleCount(), m = groesse / (n + 4), d = '';
    for (var r = 0; r < n; r++) for (var k = 0; k < n; k++) {
      if (qr.isDark(r, k)) d += 'M' + (x + (k + 2) * m).toFixed(2) + ' ' + (y + (r + 2) * m).toFixed(2) + 'h' + m.toFixed(2) + 'v' + m.toFixed(2) + 'h-' + m.toFixed(2) + 'z';
    }
    return '<rect x="' + x + '" y="' + y + '" width="' + groesse + '" height="' + groesse + '" fill="#fff"/><path d="' + d + '" fill="#000"/>';
  }
  // Marker-Positionen in der Kartenflaeche (0..1)
  function ortePositionen(aus) {
    return eigeneOrte.map(function(o) {
      return { x: (o.lon - aus.xr[0]) / (aus.xr[1] - aus.xr[0]), y: (aus.yr[1] - o.lat) / (aus.yr[1] - aus.yr[0]) };
    });
  }
  function esc(t) { return String(t).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;'); }
  function veranstaltungsQuelle(t, mitLandeskarte) {
    return 'Quelle: ' + t.info.quelle + ' Kartengrundlage: ' + (mitLandeskarte ? 'Landeskarte grau © swisstopo' : 'Kantone und Seen (swisstopo)') + '. Erstellt mit dem Datenexplorer Graswachstum der AGFF.';
  }
  function themaUntertitel(t, opt) {
    return (opt.titel ? opt.titel + ' · ' : '') + 'Stand ' + t.stand + ' · erstellt am ' + heuteText() + ' · graswachstum.ch';
  }

  // Eine Seite als SVG (mm-Koordinaten), Grundlage fuer SVG- und HTML-Ausgabe
  // karte: zusammengesetztes JPEG (Text) oder {jpg, basis, oben, landeskarte}; interaktiv (HTML): Ebene getrennt,
  // Farbskala mit Pfeilen unter dem Zahlenstrahl verschiebbar
  function veranstaltungSeiteSvg(t, karte, aus, opt, link, interaktiv) {
    var kartenBild = typeof karte === 'string' ? karte : karte.jpg;
    var kurz = link.length < 80;
    var S = SEITE, pos = ortePositionen(aus), teile = [];
    var kx = S.rand, ky = 29, kb = S.karteB, kh = S.karteH;
    teile.push('<rect width="' + S.b + '" height="' + S.h + '" fill="#fff"/>');
    teile.push('<text class="gw-v-titel" x="' + S.rand + '" y="16" font-size="6.2" font-weight="700" fill="#1e1e1e">' + esc(t.thema.titel + ' ' + selectedYear) + '</text>');
    teile.push('<text x="' + S.rand + '" y="22" font-size="3.3" fill="#646464">' + esc(themaUntertitel(t, opt)) + '</text>');
    teile.push('<line x1="' + S.rand + '" y1="25" x2="' + (S.b - S.rand) + '" y2="25" stroke="#d2d2d2" stroke-width="0.3"/>');
    if (interaktiv && typeof karte !== 'string' && t.z) {
      var clip = 'gw-v-clip-' + t.thema.name, z = t.z;
      var zx = kx + (z.x0 - aus.xr[0]) / (aus.xr[1] - aus.xr[0]) * kb, zy = ky + (aus.yr[1] - z.y1) / (aus.yr[1] - aus.yr[0]) * kh;
      var zb = (z.x1 - z.x0) / (aus.xr[1] - aus.xr[0]) * kb, zh = (z.y1 - z.y0) / (aus.yr[1] - aus.yr[0]) * kh;
      teile.push('<defs><clipPath id="' + clip + '"><rect x="' + kx + '" y="' + ky + '" width="' + kb + '" height="' + kh + '"/></clipPath></defs><g clip-path="url(#' + clip + ')">');
      if (!karte.landeskarte) teile.push('<image x="' + kx + '" y="' + ky + '" width="' + kb + '" height="' + kh + '" preserveAspectRatio="none" xlink:href="' + karte.basis + '"/>');
      else teile.push('<rect x="' + kx + '" y="' + ky + '" width="' + kb + '" height="' + kh + '" fill="#fff"/>');
      teile.push('<image class="gw-v-ebene" x="' + zx.toFixed(2) + '" y="' + zy.toFixed(2) + '" width="' + zb.toFixed(2) + '" height="' + zh.toFixed(2) + '" preserveAspectRatio="none" xlink:href=""/>');
      if (karte.landeskarte) teile.push('<image x="' + kx + '" y="' + ky + '" width="' + kb + '" height="' + kh + '" preserveAspectRatio="none" style="mix-blend-mode:multiply" xlink:href="' + karte.basis + '"/>');
      if (karte.oben) teile.push('<image x="' + kx + '" y="' + ky + '" width="' + kb + '" height="' + kh + '" preserveAspectRatio="none" xlink:href="' + karte.oben + '"/>');
      teile.push('</g>');
    } else {
      teile.push('<image class="gw-v-karte" x="' + kx + '" y="' + ky + '" width="' + kb + '" height="' + kh + '" preserveAspectRatio="none" xlink:href="' + kartenBild + '"/>');
    }
    teile.push('<rect x="' + kx + '" y="' + ky + '" width="' + kb + '" height="' + kh + '" fill="none" stroke="#bdbdbd" stroke-width="0.3"/>');
    // Marker
    teile.push('<g class="gw-v-orte">');
    var labels = beschriftungen(pos, kb, kh, 3.1, opt.namen);
    pos.forEach(function(p, i) {
      if (p.x < 0 || p.x > 1 || p.y < 0 || p.y > 1) return;
      var x = kx + p.x * kb, y = ky + p.y * kh, lb = labels[i];
      teile.push('<g class="gw-v-ort" data-i="' + i + '">');
      if (lb) teile.push('<text x="' + (kx + lb.tx).toFixed(2) + '" y="' + (ky + lb.ty).toFixed(2) + '" font-size="3.1" text-anchor="' + lb.anker + '" fill="#3a0a20" stroke="#fff" stroke-width="0.9" paint-order="stroke" stroke-linejoin="round">' + esc(eigeneOrte[i].name) + '</text>');
      teile.push('<circle cx="' + x.toFixed(2) + '" cy="' + y.toFixed(2) + '" r="2.5" fill="' + ORTE_FARBE + '" stroke="#fff" stroke-width="0.6"/>');
      teile.push('<text x="' + x.toFixed(2) + '" y="' + (y + 1.05).toFixed(2) + '" font-size="2.9" font-weight="700" fill="#fff" text-anchor="middle">' + (i + 1) + '</text>');
      teile.push('</g>');
    });
    teile.push('</g>');
    // Spalte rechts: Legende
    var x = S.spalteX, b = S.spalteB, y = 33;
    teile.push('<text x="' + x + '" y="' + y + '" font-size="3.6" font-weight="700" fill="#1e1e1e">Legende <tspan font-weight="400" fill="#52514e">(' + esc(String(t.info.einheit).split(',')[0]) + ')</tspan></text>');
    y += 4;
    var grad = 'gw-v-grad-' + t.thema.name, sk = t.skala, stops = t.info.farben.map(function(f, i) { return { o: t.info.farben.length > 1 ? i / (t.info.farben.length - 1) : 0, f: f }; });
    if (interaktiv) {
      // Zahlenstrahl von 0 (bzw. Minimum) bis Maximum; Farbverlauf zwischen den beiden Pfeilen (Min/Max der Farbskala)
      var voll = t.voll, an = function(v) { return x + b * (v - voll[0]) / (voll[1] - voll[0]); };
      var o0 = (sk[0] - voll[0]) / (voll[1] - voll[0]), o1 = (sk[1] - voll[0]) / (voll[1] - voll[0]);
      teile.push('<defs><linearGradient id="' + grad + '" class="gw-v-grad">' + stops.map(function(st) { return '<stop offset="' + (o0 + (o1 - o0) * st.o).toFixed(4) + '" stop-color="' + st.f + '" data-o="' + st.o + '"/>'; }).join('') + '</linearGradient></defs>');
      teile.push('<rect class="gw-v-balken" x="' + x + '" y="' + y + '" width="' + b + '" height="3.6" fill="url(#' + grad + ')" stroke="#bebebe" stroke-width="0.2"/>');
      teile.push('<text x="' + x + '" y="' + (y - 0.6) + '" font-size="2.2" fill="#8a8a8a" class="gw-v-ende">' + tausender(Math.round(voll[0])) + '</text><text x="' + (x + b) + '" y="' + (y - 0.6) + '" font-size="2.2" fill="#8a8a8a" text-anchor="end" class="gw-v-ende">' + tausender(Math.round(voll[1])) + '</text>');
      ['lo', 'hi'].forEach(function(g, i) {
        var xg = an(sk[i]);
        teile.push('<g class="gw-v-griff" data-g="' + g + '" tabindex="0" role="slider" aria-label="' + (i ? 'Höchstwert' : 'Tiefstwert') + ' der Farbskala" aria-valuenow="' + sk[i] + '" transform="translate(' + xg.toFixed(2) + ' 0)" style="cursor:ew-resize">'
          + '<rect x="-3" y="' + (y + 3.2) + '" width="6" height="6" fill="transparent"/><path d="M0 ' + (y + 3.8) + ' l-2 3.2 h4 z" fill="#2f6f5e"/>'
          + '<text x="0" y="' + (y + 10.2) + '" font-size="2.7" fill="#1e1e1e" font-weight="700" text-anchor="middle" class="gw-v-gwert">' + tausender(Math.round(sk[i])) + '</text></g>');
      });
      teile.push('<text x="' + x + '" y="' + (y + 13.6) + '" font-size="2.3" fill="#52514e">Pfeile verschieben: Farbskala anpassen · <tspan class="gw-v-auto" fill="#2f6f5e" text-decoration="underline" style="cursor:pointer">zurücksetzen</tspan></text>');
      y += 4;
    } else {
      teile.push('<defs><linearGradient id="' + grad + '">' + stops.map(function(st) { return '<stop offset="' + st.o + '" stop-color="' + st.f + '"/>'; }).join('') + '</linearGradient></defs>');
      teile.push('<rect x="' + x + '" y="' + y + '" width="' + b + '" height="3.6" fill="url(#' + grad + ')" stroke="#bebebe" stroke-width="0.2"/>');
      skalaMarken(sk[0], sk[1]).forEach(function(mk, i, l) {
        var xt = x + b * mk.f, letzte = i === l.length - 1;
        teile.push('<line x1="' + xt + '" y1="' + (y + 3.6) + '" x2="' + xt + '" y2="' + (y + 4.6) + '" stroke="#999" stroke-width="0.2"/>');
        teile.push('<text x="' + xt + '" y="' + (y + 7.6) + '" font-size="2.7" fill="#464646" text-anchor="' + (i === 0 ? 'start' : letzte ? 'end' : 'middle') + '">' + (i === 0 && sk[0] > t.voll[0] ? '≤' : letzte && sk[1] < t.voll[1] ? '≥' : '') + tausender(Math.round(mk.v)) + '</text>');
      });
    }
    y += 12;
    if (t.thema.experimentell) {
      teile.push('<text x="' + x + '" y="' + y + '" font-size="2.7" fill="#b23b00">Modellrechnung (ModVege), experimentell –</text>');
      teile.push('<text x="' + x + '" y="' + (y + 3.4) + '" font-size="2.7" fill="#b23b00">ohne Nährstoffe, Schnitt und Beweidung.</text>');
      y += 8;
    }
    // Tabelle
    var n = eigeneOrte.length, zeileH = Math.max(2.6, Math.min(4.6, (210 - 50 - y) / Math.max(1, n + 1)));
    var platz = Math.floor((210 - 50 - y) / zeileH) - 1, zeigen = n > platz ? platz - 1 : n;
    var fs = Math.min(3.1, zeileH * 0.72), zwei = !!t.werte2, nameMax = zwei ? 22 : 30, ohneEinheit = function(w, i) { return wertText(w, i).replace(/\s\S+$/, ''); };
    teile.push('<text x="' + x + '" y="' + (y + 3) + '" font-size="' + fs + '" font-weight="700" fill="#1e1e1e">Nr. Ort</text>');
    if (zwei) {
      teile.push('<text x="' + (x + b - 16) + '" y="' + (y + 3) + '" font-size="' + fs + '" font-weight="700" fill="#1e1e1e" text-anchor="end">seit 1.1.</text>');
      teile.push('<text class="gw-v-k2" x="' + (x + b) + '" y="' + (y + 3) + '" font-size="' + fs + '" font-weight="700" fill="#1e1e1e" text-anchor="end">28 Tage</text>');
    } else {
      teile.push('<text class="gw-v-k1" x="' + (x + b) + '" y="' + (y + 3) + '" font-size="' + fs + '" font-weight="700" fill="#1e1e1e" text-anchor="end">Wert</text>');
    }
    y += zeileH;
    var tabEnde = y + zeigen * zeileH + (zeigen < n ? zeileH : 0) + 3;
    if (zeigen < n) teile.push('<text x="' + x + '" y="' + (y + zeigen * zeileH + zeileH * 0.78) + '" font-size="' + fs + '" fill="#52514e">… und ' + (n - zeigen) + ' weitere (Nummern auf der Karte)</text>');
    eigeneOrte.slice(0, zeigen).forEach(function(o, i) {
      var yz = y + i * zeileH;
      if (i % 2 === 0) teile.push('<rect x="' + (x - 1) + '" y="' + (yz + 0.4) + '" width="' + (b + 2) + '" height="' + zeileH + '" fill="#f6f6f6"/>');
      teile.push('<g class="gw-v-zeile" data-i="' + i + '"><text x="' + x + '" y="' + (yz + zeileH * 0.78) + '" font-size="' + fs + '" fill="#1e1e1e">' + (i + 1) + '. ' + esc(o.name.length > nameMax ? o.name.slice(0, nameMax - 1) + '…' : o.name) + '</text>');
      if (zwei) {
        teile.push('<text x="' + (x + b - 16) + '" y="' + (yz + zeileH * 0.78) + '" font-size="' + fs + '" fill="#1e1e1e" text-anchor="end">' + esc(ohneEinheit(t.werte[i], t.info)) + '</text>');
        teile.push('<text class="gw-v-w2" data-i="' + i + '" x="' + (x + b) + '" y="' + (yz + zeileH * 0.78) + '" font-size="' + fs + '" fill="#1e1e1e" text-anchor="end">' + esc(ohneEinheit(t.werte2.werte[i], t.werte2.info)) + '</text></g>');
      } else {
        teile.push('<text class="gw-v-w1" data-i="' + i + '" x="' + (x + b) + '" y="' + (yz + zeileH * 0.78) + '" font-size="' + fs + '" fill="#1e1e1e" text-anchor="end">' + esc(wertText(t.werte[i], t.info)) + '</text></g>');
      }
    });
    // QR-Code (Kurzlink: wenige Module, gut scanbar)
    var q = 26, qy = S.h - S.rand - q - 6;
    // Methodik: wo neben der Karte Platz bleibt
    var mfs = 2.6, mm = methodikAbsaetze(t.thema, b, qy - 4 - tabEnde, mfs, function(z) { return z.length * mfs * 0.5; });
    if (mm.absaetze.length) {
      var my = tabEnde + 3.5;
      teile.push('<text x="' + x + '" y="' + my + '" font-size="3" font-weight="700" fill="#1e1e1e">So entstehen die Werte</text>');
      my += 3.2;
      mm.absaetze.forEach(function(z) {
        z.forEach(function(zeile) { my += mm.zeilenH; teile.push('<text x="' + x + '" y="' + my.toFixed(2) + '" font-size="' + mfs + '" fill="#464646">' + esc(zeile) + '</text>'); });
        my += 1.6;
      });
    }
    teile.push(qrSvgPfad(link, x, qy, q, kurz ? 'M' : 'L'));
    teile.push('<text x="' + (x + q + 2.5) + '" y="' + (qy + 5) + '" font-size="2.8" fill="#464646"><tspan x="' + (x + q + 2.5) + '">Online vertiefen:</tspan><tspan x="' + (x + q + 2.5) + '" dy="3.6">diese Karte mit allen</tspan><tspan x="' + (x + q + 2.5) + '" dy="3.6">Orten im Datenexplorer</tspan></text>');
    teile.push('<a href="' + esc(link) + '"><text x="' + (x + q + 2.5) + '" y="' + (qy + 18) + '" font-size="2.8" fill="#28645a" text-decoration="underline">' + esc(kurz ? ohneProtokoll(link) : 'graswachstum.ch/growth') + '</text></a>');
    // Quelle
    teile.push('<text x="' + S.rand + '" y="' + (S.h - 6) + '" font-size="2.4" fill="#787878">' + esc(veranstaltungsQuelle(t, opt.mitLandeskarte)) + '</text>');
    return '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="297mm" height="210mm" viewBox="0 0 297 210" font-family="Helvetica, Arial, sans-serif">' + teile.join('') + '</svg>';
  }

  function veranstaltungPdfSeite(doc, t, kartenJpeg, aus, opt, link) {
    var S = SEITE, pos = ortePositionen(aus), kx = S.rand, ky = 29, kb = S.karteB, kh = S.karteH;
    doc.setFont('helvetica', 'bold'); doc.setFontSize(17); doc.setTextColor(30);
    doc.text(t.thema.titel + ' ' + selectedYear, S.rand, 16);
    doc.setFont('helvetica', 'normal'); doc.setFontSize(9); doc.setTextColor(100);
    doc.text(themaUntertitel(t, opt), S.rand, 22);
    doc.setDrawColor(210); doc.setLineWidth(0.3); doc.line(S.rand, 25, S.b - S.rand, 25);
    doc.addImage(kartenJpeg, 'JPEG', kx, ky, kb, kh);
    doc.setDrawColor(190); doc.rect(kx, ky, kb, kh);
    var labels = beschriftungen(pos, kb, kh, 3.1, opt.namen);
    pos.forEach(function(p, i) {
      if (p.x < 0 || p.x > 1 || p.y < 0 || p.y > 1) return;
      var x = kx + p.x * kb, y = ky + p.y * kh, lb = labels[i];
      if (lb) {
        doc.setFontSize(8.5); doc.setFont('helvetica', 'normal');
        var bt = doc.getTextWidth(eigeneOrte[i].name), tx = kx + lb.tx, ty = ky + lb.ty;
        var x0 = lb.anker === 'start' ? tx : lb.anker === 'end' ? tx - bt : tx - bt / 2;
        doc.setGState(new doc.GState({ opacity: 0.8 })); doc.setFillColor(255, 255, 255);
        doc.roundedRect(x0 - 0.6, ty - 3.0, bt + 1.2, 3.9, 0.8, 0.8, 'F');
        doc.setGState(new doc.GState({ opacity: 1 }));
        doc.setTextColor(58, 10, 32); doc.text(eigeneOrte[i].name, tx, ty, { align: lb.anker === 'start' ? 'left' : lb.anker === 'end' ? 'right' : 'center' });
      }
      doc.setFillColor(194, 24, 91); doc.setDrawColor(255, 255, 255); doc.setLineWidth(0.6);
      doc.circle(x, y, 2.5, 'FD');
      doc.setFont('helvetica', 'bold'); doc.setFontSize(7.5); doc.setTextColor(255, 255, 255);
      doc.text(String(i + 1), x, y + 0.95, { align: 'center' });
    });
    var x = S.spalteX, b = S.spalteB, y = 33;
    doc.setFont('helvetica', 'bold'); doc.setFontSize(10); doc.setTextColor(30);
    doc.text('Legende', x, y);
    doc.setFont('helvetica', 'normal'); doc.setTextColor(82);
    doc.text('(' + String(t.info.einheit).split(',')[0] + ')', x + doc.getTextWidth('Legende ') + 1, y);
    y += 4;
    doc.addImage(farbBalkenPng(t.info.farben, 400, 12), 'PNG', x, y, b, 3.6);
    doc.setDrawColor(190); doc.setLineWidth(0.2); doc.rect(x, y, b, 3.6);
    doc.setFontSize(7.5); doc.setTextColor(70);
    skalaMarken(t.skala[0], t.skala[1]).forEach(function(mk, li, l) {
      var letzte = li === l.length - 1;
      doc.line(x + b * mk.f, y + 3.6, x + b * mk.f, y + 4.6);
      doc.text((li === 0 && t.skala[0] > t.voll[0] ? '<' : letzte && t.skala[1] < t.voll[1] ? '>' : '') + tausender(Math.round(mk.v)), x + b * mk.f, y + 8, { align: li === 0 ? 'left' : letzte ? 'right' : 'center' });
    });
    y += 12;
    if (t.thema.experimentell) {
      doc.setFont('helvetica', 'normal'); doc.setFontSize(7.5); doc.setTextColor(178, 59, 0);
      doc.text(doc.splitTextToSize('Modellrechnung (ModVege), experimentell – ohne Nährstoffe, Schnitt und Beweidung.', b), x, y + 2);
      y += 9;
    }
    var n = eigeneOrte.length, zeileH = Math.max(2.6, Math.min(4.6, (210 - 50 - y) / Math.max(1, n + 1)));
    var platz = Math.floor((210 - 50 - y) / zeileH) - 1, zeigen = n > platz ? platz - 1 : n;
    var fs = Math.min(8.8, zeileH * 2.05), zwei = !!t.werte2, nameMax = zwei ? 22 : 30, ohneEinheit = function(w, i) { return wertText(w, i).replace(/\s\S+$/, ''); };
    doc.setFont('helvetica', 'bold'); doc.setFontSize(fs); doc.setTextColor(30);
    doc.text('Nr. Ort', x, y + 3);
    if (zwei) { doc.text('seit 1.1.', x + b - 16, y + 3, { align: 'right' }); doc.text('28 Tage', x + b, y + 3, { align: 'right' }); }
    else doc.text('Wert', x + b, y + 3, { align: 'right' });
    y += zeileH;
    var tabEnde = y + zeigen * zeileH + (zeigen < n ? zeileH : 0) + 3;
    doc.setFont('helvetica', 'normal');
    if (zeigen < n) { doc.setTextColor(82); doc.text('… und ' + (n - zeigen) + ' weitere (Nummern auf der Karte)', x, y + zeigen * zeileH + zeileH * 0.78); }
    eigeneOrte.slice(0, zeigen).forEach(function(o, i) {
      var yz = y + i * zeileH;
      if (i % 2 === 0) { doc.setFillColor(246, 246, 246); doc.rect(x - 1, yz + 0.4, b + 2, zeileH, 'F'); }
      doc.setTextColor(30);
      doc.text((i + 1) + '. ' + (o.name.length > nameMax ? o.name.slice(0, nameMax - 1) + '…' : o.name), x, yz + zeileH * 0.78);
      if (zwei) {
        doc.text(ohneEinheit(t.werte[i], t.info), x + b - 16, yz + zeileH * 0.78, { align: 'right' });
        doc.text(ohneEinheit(t.werte2.werte[i], t.werte2.info), x + b, yz + zeileH * 0.78, { align: 'right' });
      } else {
        doc.text(wertText(t.werte[i], t.info), x + b, yz + zeileH * 0.78, { align: 'right' });
      }
    });
    var q = 26, qy = S.h - S.rand - q - 6, kurz = link.length < 80;
    // Methodik: wo neben der Karte Platz bleibt
    doc.setFont('helvetica', 'normal'); doc.setFontSize(7.4);
    var mfs = 7.4 * 0.3528, mm = methodikAbsaetze(t.thema, b, qy - 4 - tabEnde, mfs, function(z) { return doc.getTextWidth(z); });
    if (mm.absaetze.length) {
      var my = tabEnde + 3.5;
      doc.setFont('helvetica', 'bold'); doc.setFontSize(8.4); doc.setTextColor(30);
      doc.text('So entstehen die Werte', x, my);
      my += 3.4;
      doc.setFont('helvetica', 'normal'); doc.setFontSize(7.4); doc.setTextColor(70);
      mm.absaetze.forEach(function(z) {
        z.forEach(function(zeile) { my += mm.zeilenH; doc.text(zeile, x, my); });
        my += 1.6;
      });
    }
    pdfQrVektor(doc, link, x, qy, q, kurz ? 'M' : 'L');
    doc.setFont('helvetica', 'normal'); doc.setFontSize(8); doc.setTextColor(70);
    doc.text(['Online vertiefen:', 'diese Karte mit allen', 'Orten im Datenexplorer'], x + q + 2.5, qy + 5);
    doc.setTextColor(40, 100, 90);
    doc.textWithLink(kurz ? ohneProtokoll(link) : 'graswachstum.ch/growth', x + q + 2.5, qy + 18, { url: link });
    doc.setFontSize(7); doc.setTextColor(120);
    doc.text(doc.splitTextToSize(veranstaltungsQuelle(t, opt.mitLandeskarte), S.b - 2 * S.rand), S.rand, S.h - 6);
  }

  // --- Jahresrueckblick (HTML-Ausgabe): eigene Orte im Vergleich zur Schweiz ---
  // Je Kalenderwoche: Mittel der eigenen Orte und Mittel aller Gitterzellen der
  // Schweiz fuer Niederschlag seit 1. Januar, Bodenwasserbilanz und potenzielles
  // Wachstum; dazu das gemessene Graswachstum des naechsten Messstandorts.
  // Farben geprueft (dataviz-Validator): Gebiet #c2185b = Markerfarbe,
  // Schweiz #2a78d6, Referenz #eda100 gestrichelt mit sichtbarer Beschriftung.
  var RB_FARBEN = { gebiet: '#c2185b', schweiz: '#2a78d6', referenz: '#eda100' };
  function mittel(werte) {
    var s = 0, n = 0;
    werte.forEach(function(v) { if (v !== null && v !== undefined && !isNaN(v)) { s += v; n++; } });
    return n ? s / n : null;
  }
  function gitterMittel(gitter) {
    var s = 0, n = 0;
    (gitter.m || []).forEach(function(zeile) { (zeile || []).forEach(function(v) { if (v !== null && v !== undefined && !isNaN(v)) { s += v; n++; } }); });
    return n ? s / n : null;
  }
  // Koordinaten der Messstandorte aus den Kartenpunkten (je Snapshot-Trace)
  function messstandortKoordinaten() {
    var gd = kartenGd(), k = {};
    mapWochen.forEach(function(m, i) {
      var t = gd && gd.data[i], namen = mapPointOrts[i];
      if (!t || !namen) return;
      namen.forEach(function(n, j) { if (!k[n] && isFinite(t.x[j]) && isFinite(t.y[j])) k[n] = { lon: t.x[j], lat: t.y[j] }; });
    });
    return k;
  }
  function km(a, b) {
    var r = Math.PI / 180, dx = (a.lon - b.lon) * Math.cos((a.lat + b.lat) / 2 * r) * 111.32, dy = (a.lat - b.lat) * 110.57;
    return Math.sqrt(dx * dx + dy * dy);
  }
  // Normalwerte 1991-2020 fuer den Bericht (R: 28_klimanormale.R, grob je Woche): fehlt die Datei, ohne Band
  var normaleP = null;
  function ladeNormale() {
    if (!normaleP) normaleP = fetch(gwBasis + 'ebenen/normale.json').then(function(r) { return r.ok ? r.json() : null; }).catch(function() { return null; });
    return normaleP;
  }
  // Perzentile P10, P30, P50, P70, P90 im Mittel der eigenen Orte fuer eine Woche
  var NORMAL_P = [10, 30, 50, 70, 90];
  function normalAnOrten(N, groesse, w) {
    var g = N && N[groesse];
    if (!g || !g.werte || !g.werte.p50 || !g.werte.p50[w - 1]) return null;
    var d = [];
    for (var pi = 0; pi < NORMAL_P.length; pi++) {
      var gitter = { x0: g.x0, x1: g.x1, y0: g.y0, y1: g.y1, ncol: g.ncol, nrow: g.nrow, m: (g.werte['p' + NORMAL_P[pi]] || [])[w - 1] };
      var v = mittel(eigeneOrte.map(function(o) { return gitterWert(gitter, o.lon, o.lat); }));
      if (v === null) return null;
      d.push(v);
    }
    return { d: d, periode: g.periode };
  }
  // Anteil der Normjahre mit weniger als v (0..1), linear zwischen den Perzentilen
  function rangImNormal(v, d) {
    if (v === null || !d) return null;
    var n = d.length;
    if (v <= d[0]) return v < d[0] ? 0.05 : 0.1;
    if (v >= d[n - 1]) return v > d[n - 1] ? 0.95 : 0.9;
    for (var i = 1; i < n; i++) if (v <= d[i]) return (NORMAL_P[i - 1] + (NORMAL_P[i] - NORMAL_P[i - 1]) * (v - d[i - 1]) / ((d[i] - d[i - 1]) || 1)) / 100;
    return 0.5;
  }
  // «trockener als 8 von 10 Jahren»; worte: [weniger, mehr] z.B. ['trockener', 'nasser']
  function rangText(anteil, worte) {
    if (anteil === null) return '';
    if (anteil > 0.35 && anteil < 0.65) return 'im üblichen Bereich (wie die Hälfte der Jahre)';
    return anteil >= 0.65 ? worte[1] + ' als ' + Math.min(9, Math.round(anteil * 10)) + ' von 10 Jahren' : worte[0] + ' als ' + Math.min(9, Math.round((1 - anteil) * 10)) + ' von 10 Jahren';
  }
  function jahresrueckblickDaten(wocheBis) {
    var themen = [];
    VERANSTALTUNG_THEMEN.filter(function(t) { return layerLegenden[t.name]; }).forEach(function(t) {
      // Normal 1991-2020 je Thema (R: 28_klimanormale.R), Worte fuer den Rang: [weniger, mehr]
      var NORMAL_JE_THEMA = { niederschlag_kum: ['trockener', 'nasser'], boden: ['trockener', 'feuchter'], wachstumspotenzial_kum: ['tiefer', 'höher'] };
      themen.push(Object.assign({ normal: NORMAL_JE_THEMA[t.name] ? t.name : null, worte: NORMAL_JE_THEMA[t.name] }, t));
      if (t.name === 'niederschlag_kum' && layerLegenden.niederschlag) themen.push({ name: 'niederschlag_28', titel: 'Niederschlag letzte 28 Tage', datei: 'niederschlag_28', info: 'niederschlag', normal: 'niederschlag_28', worte: ['trockener', 'nasser'] });
    });
    if (layerLegenden.gdd) themen.push({ name: 'gdd', titel: 'Wärmesumme seit 1. Januar', datei: 'gdd', normal: 'gdd', nurMitNormal: true, worte: ['kühler', 'wärmer'] });
    return ladeNormale().then(function(N) { return Promise.all(themen.filter(function(th) { return !th.nurMitNormal || (N && N[th.normal]); }).map(function(th) {
      return ladeEbeneP(th.datei || ebeneDateiSchluessel(th.name)).then(function(d) {
        var punkte = [];
        for (var w = 1; w <= wocheBis; w++) {
          var g = (d.werte || {})[selectedYear + ' ' + w];
          if (!g || !g.m) continue;
          var f = th.faktor || 1, gm = mittel(eigeneOrte.map(function(o) { return gitterWert(g, o.lon, o.lat); })), sm = gitterMittel(g);
          punkte.push({ w: w, gebiet: gm === null ? null : gm * f, schweiz: sm === null ? null : sm * f });
        }
        var info = layerLegenden[th.info || th.name];
        if (th.einheit) info = Object.assign({}, info, { einheit: th.einheit });
        // Normalband 1991-2020 ueber das ganze Jahr (Mittel der eigenen Orte)
        var normal = null;
        if (th.normal && N && N[th.normal]) {
          normal = { periode: N[th.normal].periode, wochen: [] };
          for (var nw = 1; nw <= 52; nw++) { var nv = normalAnOrten(N, th.normal, nw); if (nv) normal.wochen.push({ w: nw, d: nv.d.map(function(x) { return x * (th.faktor || 1); }) }); }
          if (!normal.wochen.length) normal = null;
        }
        return { thema: th, info: info, punkte: punkte, normal: normal };
      }, function() { return null; });
    }).concat([
      // Wasserbilanz je Woche: Niederschlag 7 Tage minus Verdunstung ET0 7 Tage
      Promise.all([ladeEbeneP('niederschlag_7'), ladeEbeneP('et0_7')]).then(function(r) {
        var punkte = [];
        for (var w = 1; w <= wocheBis; w++) {
          var k = selectedYear + ' ' + w, n = (r[0].werte || {})[k], e = (r[1].werte || {})[k];
          if (!n || !e || !n.m || !e.m) continue;
          var gb = mittel(eigeneOrte.map(function(o) { var a = gitterWert(n, o.lon, o.lat), b = gitterWert(e, o.lon, o.lat); return a === null || b === null ? null : a - b; }));
          var sn = gitterMittel(n), se = gitterMittel(e);
          punkte.push({ w: w, gebiet: gb, schweiz: sn === null || se === null ? null : sn - se });
        }
        return { bilanz: punkte };
      }, function() { return null; })
    ])); }).then(function(liste) {
      var bilanz = (liste.pop() || {}).bilanz || [];
      // Gemessenes Graswachstum: naechster Messstandort mit Daten im Jahr
      var koord = messstandortKoordinaten(), mitte = { lon: mittel(eigeneOrte.map(function(o) { return o.lon; })), lat: mittel(eigeneOrte.map(function(o) { return o.lat; })) };
      var best = null;
      siteGrowthMeta.forEach(function(m) {
        if (m.year !== selectedYear) return;
        var n = siteNames[m.siteIdx], c = koord[n];
        if (!c) return;
        var d = km(mitte, c);
        if (!best || d < best.km) best = { name: n, km: d, traceIdx: m.traceIdx, siteIdx: m.siteIdx };
      });
      var spur = function(idx, bisW) {
        var t = el.data[idx];
        return t ? t.x.map(function(x, i) { return [x, t.y[i]]; }).filter(function(p) { return p[1] !== null && isFinite(p[1]) && p[0] <= (bisW || wocheBis); })
          .map(function(p) { return [p[0], Math.round(p[1] * 10) / 10]; }) : [];
      };
      var alle = null;
      groupGrowthMeta.forEach(function(m) { if (m.year === selectedYear && m.groupIdx === 0) alle = m.traceIdx; });
      var messung = {
        standort: best, nah: best ? spur(best.traceIdx) : [], schweiz: alle !== null ? spur(alle) : [],
        referenz: spur(standardKurveTraceIdx).filter(function(p) { return p[0] <= 52; })
      };
      // Mehrjahresvergleich: je Jahr die Kurve des naechsten Standorts und das Mittel aller Standorte
      // (gewaehltes Jahr bis zum Stichtag, die anderen ganz)
      var jahresSpur = function(meta, passt) {
        var j = {};
        meta.forEach(function(m) { if (passt(m)) { var p = spur(m.traceIdx, m.year === selectedYear ? wocheBis : 53); if (p.length > 1) j[m.year] = p; } });
        return j;
      };
      var bezuege = [];
      if (best) {
        var nahJ = jahresSpur(siteGrowthMeta, function(m) { return m.siteIdx === best.siteIdx; });
        if (Object.keys(nahJ).length > 1) bezuege.push({ name: best.name, lang: 'Messstandort ' + best.name + ' (' + Math.round(best.km) + ' km)', jahre: nahJ });
      }
      var alleJ = jahresSpur(groupGrowthMeta, function(m) { return m.groupIdx === 0; });
      if (Object.keys(alleJ).length > 1) bezuege.push({ name: 'Alle Standorte', lang: 'Mittel aller Messstandorte', jahre: alleJ });
      var mehrjahr = bezuege.length ? { aktuell: String(selectedYear), wocheBis: wocheBis, bezuege: bezuege, referenz: messung.referenz.length ? spur(standardKurveTraceIdx, 52) : [] } : null;
      return { themen: liste.filter(Boolean), messung: messung, bilanz: bilanz, mehrjahr: mehrjahr };
    });
  }
  function netteSchrittweite(roh) {
    var e = Math.pow(10, Math.floor(Math.log10(Math.max(roh, 1e-9)))), f = roh / e;
    return (f <= 1 ? 1 : f <= 2 ? 2 : f <= 2.5 ? 2.5 : f <= 5 ? 5 : 10) * e;
  }
  // Liniendiagramm als SVG; serien: [{ name, farbe, strich, punkte: [[woche, wert], ...] }]
  // band (optional): { name, punkte: [[woche, unten, oben], ...] } - grau hinter den Linien (Normal 1991-2020)
  function rueckblickDiagramm(id, serien, einheit, band) {
    var B = 640, H = 250, l = 52, r = 78, o = 26, u = 34, pb = B - l - r, ph = H - o - u, xMax = 52;
    var yMax = 0;
    serien.forEach(function(s) { s.punkte.forEach(function(p) { if (p[1] > yMax) yMax = p[1]; }); });
    if (band) band.punkte.forEach(function(p) { if (p[2] > yMax) yMax = p[2]; });
    var schritt = netteSchrittweite((yMax || 1) / 4);
    yMax = Math.max(schritt, Math.ceil(yMax / schritt) * schritt);
    var X = function(w) { return l + (w - 1) / (xMax - 1) * pb; }, Y = function(v) { return o + ph - v / yMax * ph; };
    var t = ['<svg class="gw-rb-svg" viewBox="0 0 ' + B + ' ' + H + '" role="img" aria-label="Diagramm ' + esc(serien.map(function(s) { return s.name; }).join(', ')) + '">'];
    for (var v = 0; v <= yMax + 1e-9; v += schritt) {
      t.push('<line x1="' + l + '" x2="' + (l + pb) + '" y1="' + Y(v).toFixed(1) + '" y2="' + Y(v).toFixed(1) + '" stroke="' + (v === 0 ? '#9a9a9a' : '#e6e6e6') + '" stroke-width="1"/>');
      t.push('<text x="' + (l - 6) + '" y="' + (Y(v) + 4).toFixed(1) + '" font-size="11" fill="#52514e" text-anchor="end">' + tausender(Math.round(v)) + '</text>');
    }
    t.push('<text x="' + (l - 44) + '" y="11" font-size="11" fill="#52514e">' + esc(einheit) + '</text>');
    for (var w = 1; w <= 52; w += 4) {
      t.push('<text x="' + X(w).toFixed(1) + '" y="' + (o + ph + 16) + '" font-size="11" fill="#52514e" text-anchor="middle">' + w + '</text>');
    }
    t.push('<text x="' + (l + pb / 2) + '" y="' + (H - 2) + '" font-size="11" fill="#52514e" text-anchor="middle">Kalenderwoche</text>');
    if (band && band.punkte.length > 1) {
      var oben = band.punkte.map(function(p) { return X(p[0]).toFixed(1) + ' ' + Y(p[2]).toFixed(1); });
      var unten = band.punkte.slice().reverse().map(function(p) { return X(p[0]).toFixed(1) + ' ' + Y(p[1]).toFixed(1); });
      t.push('<path d="M' + oben.join('L') + 'L' + unten.join('L') + 'Z" fill="#b4b2a9" fill-opacity="0.35" stroke="none"/>');
    }
    serien.forEach(function(s) {
      var d = '', vorher = null;
      s.punkte.forEach(function(p) {
        if (p[1] === null) { vorher = null; return; }
        d += (vorher === null || p[0] - vorher > 2 ? 'M' : 'L') + X(p[0]).toFixed(1) + ' ' + Y(p[1]).toFixed(1);
        vorher = p[0];
      });
      t.push('<path d="' + d + '" fill="none" stroke="' + s.farbe + '" stroke-width="2" stroke-linejoin="round" stroke-linecap="round"' + (s.strich === 'dash' ? ' stroke-dasharray="6 4"' : '') + '/>');
    });
    // Direktbeschriftung am Linienende, Text in Textfarbe; Ueberlappungen verschieben
    var enden = serien.map(function(s) {
      var p = s.punkte.filter(function(q) { return q[1] !== null; }).pop();
      return p ? { name: s.kurz || s.name, farbe: s.farbe, x: X(p[0]), y: Y(p[1]) } : null;
    }).filter(Boolean).sort(function(a, b) { return a.y - b.y; });
    enden.forEach(function(e, i) { if (i > 0 && e.y - enden[i - 1].y < 13) e.y = enden[i - 1].y + 13; });
    enden.forEach(function(e) {
      t.push('<circle cx="' + e.x.toFixed(1) + '" cy="' + e.y.toFixed(1) + '" r="3" fill="' + e.farbe + '" stroke="#fff" stroke-width="2"/>');
      t.push('<text x="' + (Math.min(e.x, l + pb) + 7).toFixed(1) + '" y="' + (e.y + 4).toFixed(1) + '" font-size="11" fill="#1e1e1e">' + esc(e.name) + '</text>');
    });
    t.push('<line class="gw-rb-kreuz" x1="0" x2="0" y1="' + o + '" y2="' + (o + ph) + '" stroke="#52514e" stroke-width="1" visibility="hidden"/>');
    t.push('<rect class="gw-rb-flaeche" x="' + l + '" y="' + o + '" width="' + pb + '" height="' + ph + '" fill="transparent"/>');
    t.push('</svg>');
    var daten = { l: l, pb: pb, o: o, ph: ph, yMax: yMax, B: B, H: H, einheit: einheit,
      serien: serien.map(function(s) { var m = {}; s.punkte.forEach(function(p) { if (p[1] !== null) m[p[0]] = Math.round(p[1] * 10) / 10; }); return { name: s.name, farbe: s.farbe, werte: m }; }) };
    var legende = '<div class="gw-rb-legende">' + serien.map(function(s) {
      return '<span><i style="border-top:2px ' + (s.strich === 'dash' ? 'dashed' : 'solid') + ' ' + s.farbe + '"></i>' + esc(s.name) + '</span>';
    }).join('') + (band ? '<span><i style="border-top:10px solid rgba(180,178,169,.55);width:14px"></i>' + esc(band.name) + '</span>' : '') + '</div>';
    var wochen = [];
    serien.forEach(function(s) { s.punkte.forEach(function(p) { if (p[1] !== null && wochen.indexOf(p[0]) === -1) wochen.push(p[0]); }); });
    wochen.sort(function(a, b) { return a - b; });
    var tabelle = '<details class="gw-rb-tabelle"><summary>Tabelle</summary><table><thead><tr><th>KW</th>' + serien.map(function(s) { return '<th>' + esc(s.name) + '</th>'; }).join('') + '</tr></thead><tbody>' +
      wochen.map(function(w) { return '<tr><td>' + w + '</td>' + daten.serien.map(function(s) { return '<td>' + (s.werte[w] !== undefined ? tausender(Math.round(s.werte[w])) : '–') + '</td>'; }).join('') + '</tr>'; }).join('') + '</tbody></table></details>';
    return { html: legende + '<div class="gw-rb-fig" data-id="' + id + '">' + t.join('') + '</div>' + tabelle, daten: daten };
  }
  function prozentText(a, b) {
    if (a === null || b === null || !b) return '';
    var p = Math.round((a / b - 1) * 100);
    return p === 0 ? 'wie das Schweizer Mittel' : (Math.abs(p) + ' % ' + (p > 0 ? 'über' : 'unter') + ' dem Schweizer Mittel');
  }
  var VOLLBILD = '<button type="button" class="gw-rb-gross" aria-label="Diagramm im Vollbild zeigen" title="Vollbild">⤢</button>';
  // Wasserbilanz je Woche (Niederschlag minus Verdunstung ET0, je 7 Tage): Balken fuer Ihre Orte
  // (blau Ueberschuss, orange Defizit), gestrichelt das Mittel der Schweiz
  function rueckblickBalken(id, punkte) {
    var B = 640, H = 250, l = 52, r = 78, o = 26, u = 34, pb = B - l - r, ph = H - o - u;
    var m = 1;
    punkte.forEach(function(p) { [p.gebiet, p.schweiz].forEach(function(v) { if (v !== null && Math.abs(v) > m) m = Math.abs(v); }); });
    var st = netteSchrittweite(m / 2); m = Math.ceil(m / st) * st;
    var X = function(w) { return l + (w - 1) / 51 * pb; }, Y = function(v) { return o + ph / 2 - v / m * ph / 2; }, bw = pb / 52 * 0.7;
    var t = ['<svg class="gw-rb-svg" viewBox="0 0 ' + B + ' ' + H + '" role="img" aria-label="Wasserbilanz je Woche: Niederschlag minus Verdunstung">'];
    for (var v = -m; v <= m + 1e-9; v += st) {
      t.push('<line x1="' + l + '" x2="' + (l + pb) + '" y1="' + Y(v).toFixed(1) + '" y2="' + Y(v).toFixed(1) + '" stroke="' + (Math.abs(v) < 1e-9 ? '#7a7a7a' : '#e6e6e6') + '" stroke-width="1"/>');
      t.push('<text x="' + (l - 6) + '" y="' + (Y(v) + 4).toFixed(1) + '" font-size="11" fill="#52514e" text-anchor="end">' + (v > 0 ? '+' : '') + tausender(Math.round(v)) + '</text>');
    }
    t.push('<text x="' + (l - 44) + '" y="11" font-size="11" fill="#52514e">mm/Woche</text>');
    for (var w = 1; w <= 52; w += 4) t.push('<text x="' + X(w).toFixed(1) + '" y="' + (o + ph + 16) + '" font-size="11" fill="#52514e" text-anchor="middle">' + w + '</text>');
    t.push('<text x="' + (l + pb / 2) + '" y="' + (H - 2) + '" font-size="11" fill="#52514e" text-anchor="middle">Kalenderwoche</text>');
    punkte.forEach(function(p) {
      if (p.gebiet === null) return;
      var y0 = Y(0), y1 = Y(p.gebiet), pos = p.gebiet >= 0;
      t.push('<rect x="' + (X(p.w) - bw / 2).toFixed(1) + '" y="' + Math.min(y0, y1).toFixed(1) + '" width="' + bw.toFixed(1) + '" height="' + Math.max(1, Math.abs(y1 - y0)).toFixed(1) + '" rx="2" fill="' + (pos ? '#2a78d6' : '#d97706') + '"><title>KW ' + p.w + ': ' + (pos ? '+' : '') + Math.round(p.gebiet) + ' mm (Ihre Orte)' + (p.schweiz !== null ? ', Schweiz ' + (p.schweiz >= 0 ? '+' : '') + Math.round(p.schweiz) + ' mm' : '') + '</title></rect>');
    });
    var d = '', vorher = null;
    punkte.forEach(function(p) { if (p.schweiz === null) { vorher = null; return; } d += (vorher === null ? 'M' : 'L') + X(p.w).toFixed(1) + ' ' + Y(p.schweiz).toFixed(1); vorher = p.w; });
    t.push('<path d="' + d + '" fill="none" stroke="#52514e" stroke-width="1.5" stroke-dasharray="5 4"/>');
    t.push('</svg>');
    var legende = '<div class="gw-rb-legende"><span><i style="border-top:8px solid #2a78d6;width:12px"></i>Ihre Orte: Überschuss</span><span><i style="border-top:8px solid #d97706;width:12px"></i>Ihre Orte: Defizit</span><span><i style="border-top:2px dashed #52514e"></i>Schweiz (Mittel)</span></div>';
    var tabelle = '<details class="gw-rb-tabelle"><summary>Tabelle</summary><table><thead><tr><th>KW</th><th>Ihre Orte (mm)</th><th>Schweiz (mm)</th></tr></thead><tbody>' +
      punkte.map(function(p) { return '<tr><td>' + p.w + '</td><td>' + (p.gebiet === null ? '–' : Math.round(p.gebiet)) + '</td><td>' + (p.schweiz === null ? '–' : Math.round(p.schweiz)) + '</td></tr>'; }).join('') + '</tbody></table></details>';
    return legende + '<div class="gw-rb-fig" data-id="' + id + '">' + t.join('') + '</div>' + tabelle;
  }
  function jahresrueckblickHtml(rb) {
    var diagramme = [], kacheln = [], daten = {};
    rb.themen.forEach(function(t, i) {
      var ein = String(t.info.einheit).split(',')[0];
      var serien = [
        { name: 'Ihre Orte (Mittel)', kurz: 'Ihre Orte', farbe: RB_FARBEN.gebiet, punkte: t.punkte.map(function(p) { return [p.w, p.gebiet]; }) },
        { name: 'Schweiz (Mittel)', kurz: 'Schweiz', farbe: RB_FARBEN.schweiz, punkte: t.punkte.map(function(p) { return [p.w, p.schweiz]; }) }
      ];
      var band = null;
      if (t.normal) {
        serien.push({ name: 'Ihre Orte, Normal ' + t.normal.periode + ' (Median)', kurz: 'Normal', farbe: '#6b6a65', strich: 'dash', punkte: t.normal.wochen.map(function(x) { return [x.w, x.d[2]]; }) });
        band = { name: 'Ihre Orte: 8 von 10 Jahren ' + t.normal.periode, punkte: t.normal.wochen.map(function(x) { return [x.w, x.d[0], x.d[4]]; }) };
      }
      var d = rueckblickDiagramm('rb' + i, serien, ein, band);
      daten['rb' + i] = d.daten;
      diagramme.push('<figure class="gw-rb-karte"><figcaption><span>' + esc(t.thema.titel) + (t.thema.experimentell ? ' <small>(Modellrechnung, experimentell)</small>' : '') + '</span>' + VOLLBILD + '</figcaption>' + d.html + '</figure>');
      var letzte = t.punkte.filter(function(p) { return p.gebiet !== null; }).pop();
      if (!letzte) return;
      var zusatz = '';
      if (t.thema.name === 'boden') {
        var trocken = t.punkte.filter(function(p) { return p.gebiet !== null && p.gebiet < 40; });
        var tief = t.punkte.reduce(function(a, p) { return p.gebiet !== null && (!a || p.gebiet < a.gebiet) ? p : a; }, null);
        zusatz = trocken.length ? trocken.length + ' Wochen unter 40 mm, am trockensten KW ' + tief.w + ' (' + Math.round(tief.gebiet) + ' mm)' : 'keine Woche unter 40 mm';
        var nb = t.normal && t.normal.wochen.filter(function(x) { return x.w === letzte.w; })[0];
        var bodenNormal = nb ? (nb.d[0] === nb.d[4] ? 'wie in den meisten Jahren ' + t.normal.periode : rangText(rangImNormal(letzte.gebiet, nb.d), t.thema.worte) + ' ' + t.normal.periode) + ' (Median ' + Math.round(nb.d[2]) + ' mm)' : '';
        kacheln.push('<div class="gw-rb-kachel"><span>' + esc(t.thema.titel) + ', KW ' + letzte.w + '</span><b>' + Math.round(letzte.gebiet) + ' mm</b>' + (bodenNormal ? '<em class="gw-rb-normal">' + esc(bodenNormal) + '</em>' : '') + '<em>' + esc(zusatz) + '</em></div>');
      } else {
        var imNormal = '';
        var nw = t.normal && t.normal.wochen.filter(function(x) { return x.w === letzte.w; })[0];
        if (nw) {
          var gdd = t.thema.name === 'gdd';
          imNormal = rangText(rangImNormal(letzte.gebiet, nw.d), t.thema.worte || ['tiefer', 'höher']) + ' ' + t.normal.periode + ' (Median ' + tausender(Math.round(nw.d[2])) + ' ' + ein + ')';
          if (gdd) {
            // Vorsprung: wann erreicht der mittlere Verlauf die heutige Waermesumme?
            var med = t.normal.wochen, ziel = letzte.gebiet, tag = null;
            for (var k = 1; k < med.length; k++) if (med[k].d[2] >= ziel && med[k - 1].d[2] <= ziel) { tag = (med[k - 1].w - 1) * 7 + 7 * (ziel - med[k - 1].d[2]) / ((med[k].d[2] - med[k - 1].d[2]) || 1); break; }
            if (tag !== null && nw.d[2] >= 30) {
              var vor = Math.round(tag - (letzte.w - 1) * 7);
              imNormal += ' · ' + (Math.abs(vor) < 2 ? 'Vegetation im Zeitplan' : 'Vegetation ' + Math.abs(vor) + ' Tage ' + (vor > 0 ? 'voraus' : 'im Rückstand'));
            }
          }
        }
        kacheln.push('<div class="gw-rb-kachel"><span>' + esc(t.thema.titel) + ' (bis KW ' + letzte.w + ')</span><b>' + tausender(Math.round(letzte.gebiet)) + ' ' + esc(ein) + '</b>' + (imNormal ? '<em class="gw-rb-normal">' + esc(imNormal) + '</em>' : '') + '<em>' +
          esc(prozentText(letzte.gebiet, letzte.schweiz)) + (letzte.schweiz !== null ? ' (' + tausender(Math.round(letzte.schweiz)) + ' ' + esc(ein) + ')' : '') + '</em></div>');
      }
    });
    if (rb.bilanz && rb.bilanz.length) {
      diagramme.push('<figure class="gw-rb-karte"><figcaption><span>Wasserbilanz je Woche: Niederschlag minus Verdunstung</span>' + VOLLBILD + '</figcaption>' + rueckblickBalken('rbw', rb.bilanz) + '</figure>');
      var ab = rb.bilanz.filter(function(p) { return p.w >= 14 && p.gebiet !== null; }), summe = ab.reduce(function(a, p) { return a + p.gebiet; }, 0);
      var defizit = ab.filter(function(p) { return p.gebiet < 0; }).length;
      if (ab.length) kacheln.push('<div class="gw-rb-kachel"><span>Wasserbilanz seit April (KW 14–' + ab[ab.length - 1].w + ')</span><b>' + (summe >= 0 ? '+' : '–') + tausender(Math.round(Math.abs(summe))) + ' mm</b><em>' + defizit + ' von ' + ab.length + ' Wochen mit mehr Verdunstung als Regen</em></div>');
    }
    var m = rb.messung;
    if (m.standort && m.nah.length) {
      var serien = [
        { name: 'Gemessen in ' + m.standort.name + ' (' + Math.round(m.standort.km) + ' km)', kurz: m.standort.name, farbe: RB_FARBEN.gebiet, punkte: m.nah },
        { name: 'Alle Standorte (Mittel)', kurz: 'Alle Standorte', farbe: RB_FARBEN.schweiz, punkte: m.schweiz },
        { name: 'Durchschnitt Mittelland, langjährig', kurz: 'Mittelland', farbe: RB_FARBEN.referenz, strich: 'dash', punkte: m.referenz }
      ];
      var d = rueckblickDiagramm('rbm', serien, 'kg TS/ha/Tag');
      daten.rbm = d.daten;
      diagramme.push('<figure class="gw-rb-karte"><figcaption><span>Gemessenes Graswachstum ' + selectedYear + '</span>' + VOLLBILD + '</figcaption>' + d.html + '</figure>');
      var spitze = m.nah.reduce(function(a, p) { return !a || p[1] > a[1] ? p : a; }, null);
      kacheln.push('<div class="gw-rb-kachel"><span>Nächster Messstandort</span><b>' + esc(m.standort.name) + '</b><em>' + Math.round(m.standort.km) + ' km vom Mittelpunkt Ihrer Orte · Spitze ' + Math.round(spitze[1]) + ' kg TS/ha/Tag in KW ' + spitze[0] + '</em></div>');
    }
    // Mehrjahresvergleich: zeichnet die Laufzeit (berichtLaufzeit, GWB.mj) - Jahr hervorheben, aufsummiert/je Tag, Bezug
    if (rb.mehrjahr) {
      diagramme.unshift('<figure class="gw-rb-karte gw-mj"><figcaption><span>Graswachstum im Jahresvergleich</span>' + VOLLBILD + '</figcaption><div class="gw-mj-inhalt"></div></figure>');
    }
    return {
      html: '<section data-t="rb" hidden class="gw-rb"><div class="gw-rb-kacheln">' + kacheln.join('') + '</div><div class="gw-rb-gitter">' + diagramme.join('') + '</div>' +
        '<p class="gw-rb-hinweis">«Ihre Orte» = Mittel der Werte an den erfassten Orten; «Schweiz» = Mittel aller 1-km-Gitterzellen der Schweiz. Gemessenes Graswachstum: Messnetz der AGFF.' +
          (rb.themen.some(function(t) { return t.normal; }) ? ' «Normal»: dieselbe Rechnung für jedes Jahr 1991–2020 (MeteoSchweiz-Archiv, ca. 5-km-Raster); der graue Bereich zeigt, wo 8 von 10 Jahren lagen.' : '') + '</p></section>',
      daten: daten
    };
  }


  // Laufzeit im HTML-Bericht: Farbskala per Pfeil verschieben, Niederschlag-Zeitraum umschalten,
  // Diagramme im Vollbild. Wird als Quelltext in den Bericht kopiert (berichtLaufzeit.toString()).
  function berichtLaufzeit() {
    var G = window.GWB, NS = 'http://www.w3.org/1999/xlink';
    function dek(e) {
      if (!e) return null;
      var bin = atob(e.b), u = new Uint8Array(bin.length);
      for (var i = 0; i < bin.length; i++) u[i] = bin.charCodeAt(i);
      var a = new Int16Array(u.buffer), v = new Float32Array(a.length);
      for (var k = 0; k < a.length; k++) v[k] = a[k] === -32768 ? NaN : a[k] / 10;
      return { x0: e.x0, x1: e.x1, y0: e.y0, y1: e.y1, ncol: e.ncol, nrow: e.nrow, v: v };
    }
    function wert(z, lon, lat) {
      var c = Math.floor((lon - z.x0) / (z.x1 - z.x0) * z.ncol), r = Math.floor((z.y1 - lat) / (z.y1 - z.y0) * z.nrow);
      if (c < 0 || c >= z.ncol || r < 0 || r >= z.nrow) return null;
      var w = z.v[r * z.ncol + c]; return isNaN(w) ? null : w;
    }
    function schritt(roh) { var e = Math.pow(10, Math.floor(Math.log10(Math.max(roh, 1e-9)))), f = roh / e; return (f <= 1 ? 1 : f <= 2 ? 2 : f <= 2.5 ? 2.5 : f <= 5 ? 5 : 10) * e; }
    function auto(z, aus, voll) {
      var l = [];
      for (var r = 0; r < z.nrow; r++) for (var c = 0; c < z.ncol; c++) {
        var lon = z.x0 + (c + 0.5) * (z.x1 - z.x0) / z.ncol, lat = z.y1 - (r + 0.5) * (z.y1 - z.y0) / z.nrow, w = z.v[r * z.ncol + c];
        if (!isNaN(w) && lon >= aus.xr[0] && lon <= aus.xr[1] && lat >= aus.yr[0] && lat <= aus.yr[1]) l.push(w);
      }
      if (l.length < 4) return voll.slice();
      l.sort(function(a, b) { return a - b; });
      var lo = l[Math.floor(l.length * 0.05)], hi = l[Math.ceil(l.length * 0.95) - 1], st = schritt(Math.max(hi - lo, (voll[1] - voll[0]) * 0.04) / 5);
      lo = Math.max(Math.min(0, voll[0]), Math.floor(lo / st) * st); hi = Math.ceil(hi / st) * st;
      return [lo, hi > lo ? hi : lo + st];
    }
    function rgb(h) { h = h.replace('#', ''); return [parseInt(h.slice(0, 2), 16), parseInt(h.slice(2, 4), 16), parseInt(h.slice(4, 6), 16)]; }
    function bild(z, lo, hi, farben) {
      var c = document.createElement('canvas'); c.width = z.ncol; c.height = z.nrow;
      var ctx = c.getContext('2d'), img = ctx.createImageData(z.ncol, z.nrow), f = farben.map(rgb), n = f.length - 1;
      for (var i = 0; i < z.v.length; i++) {
        var w = z.v[i]; if (isNaN(w)) continue;
        var a = Math.max(0, Math.min(1, (w - lo) / ((hi - lo) || 1))) * n, k = Math.min(n - 1, Math.floor(a)), t = a - k, p = f[k], q = f[Math.min(n, k + 1)];
        img.data[i * 4] = p[0] + (q[0] - p[0]) * t; img.data[i * 4 + 1] = p[1] + (q[1] - p[1]) * t; img.data[i * 4 + 2] = p[2] + (q[2] - p[2]) * t; img.data[i * 4 + 3] = 191;
      }
      ctx.putImageData(img, 0, 0); return c.toDataURL('image/png');
    }
    function zahl(v) { var r = Math.abs(v) >= 100 ? Math.round(v) : Math.round(v * 10) / 10; return String(r).replace(/\B(?=(\d{3})+(?!\d))/g, '’'); }
    G.seiten.forEach(function(S, i) {
      var sec = document.querySelector('main section[data-t="' + i + '"]'); if (!sec || !S.z) return;
      var svg = sec.querySelector('svg'), img = svg.querySelector('.gw-v-ebene'), balken = svg.querySelector('.gw-v-balken');
      var bx = +balken.getAttribute('x'), bw = +balken.getAttribute('width');
      var z = dek(S.z), lage = { sk: S.skala.slice(), auto: S.auto.slice() }, rf = 0;
      function zeichnen() {
        if (rf) return;
        rf = setTimeout(function() {
          rf = 0;
          var u = bild(z, lage.sk[0], lage.sk[1], S.farben); img.setAttribute('href', u); img.setAttributeNS(NS, 'xlink:href', u);
          var o0 = (lage.sk[0] - S.voll[0]) / (S.voll[1] - S.voll[0]), o1 = (lage.sk[1] - S.voll[0]) / (S.voll[1] - S.voll[0]);
          svg.querySelectorAll('.gw-v-grad stop').forEach(function(st) { st.setAttribute('offset', (o0 + (o1 - o0) * +st.getAttribute('data-o')).toFixed(4)); });
          ['lo', 'hi'].forEach(function(g, k) {
            var el = svg.querySelector('.gw-v-griff[data-g="' + g + '"]');
            el.setAttribute('transform', 'translate(' + (bx + bw * (lage.sk[k] - S.voll[0]) / (S.voll[1] - S.voll[0])).toFixed(2) + ' 0)');
            el.setAttribute('aria-valuenow', lage.sk[k]); el.querySelector('.gw-v-gwert').textContent = zahl(lage.sk[k]);
          });
        }, 30);
      }
      var st = schritt((S.voll[1] - S.voll[0]) / 100);
      function setzen(k, v) {
        v = Math.round(v / st) * st;
        if (k === 0) lage.sk[0] = Math.max(S.voll[0], Math.min(v, lage.sk[1] - st)); else lage.sk[1] = Math.min(S.voll[1], Math.max(v, lage.sk[0] + st));
        zeichnen();
      }
      svg.querySelectorAll('.gw-v-griff').forEach(function(el) {
        var k = el.getAttribute('data-g') === 'lo' ? 0 : 1;
        el.addEventListener('pointerdown', function(e) {
          e.preventDefault(); el.setPointerCapture(e.pointerId);
          function mv(ev) { var p = svg.createSVGPoint(); p.x = ev.clientX; p.y = ev.clientY; p = p.matrixTransform(svg.getScreenCTM().inverse()); setzen(k, S.voll[0] + (p.x - bx) / bw * (S.voll[1] - S.voll[0])); }
          function ende() { el.removeEventListener('pointermove', mv); el.removeEventListener('pointerup', ende); el.removeEventListener('pointercancel', ende); }
          el.addEventListener('pointermove', mv); el.addEventListener('pointerup', ende); el.addEventListener('pointercancel', ende);
        });
        el.addEventListener('keydown', function(e) {
          var d = e.key === 'ArrowLeft' || e.key === 'ArrowDown' ? -1 : e.key === 'ArrowRight' || e.key === 'ArrowUp' ? 1 : 0;
          if (d) { e.preventDefault(); setzen(k, lage.sk[k] + d * st * (e.shiftKey ? 10 : 2)); }
        });
      });
      var zur = svg.querySelector('.gw-v-auto');
      if (zur) zur.addEventListener('click', function() { lage.sk = lage.auto.slice(); zeichnen(); });
      zeichnen();
      // Zeitraum: Summen (Niederschlag, Wachstumspotenzial) als Differenz zweier Stichtage,
      // Zustand (Bodenwasser) als Mittel, Tiefst- oder Hoechstwert der Wochen
      var steuer = sec.querySelector('.gw-v-steuer'); if (!steuer || !S.wochen) return;
      var kum = z, zustand = S.art === 'zustand', titel = svg.querySelector('.gw-v-titel'), k1 = svg.querySelector('.gw-v-k1'), k2 = svg.querySelector('.gw-v-k2');
      var wl = Object.keys(S.wochen).map(Number).sort(function(a, b) { return a - b; }), wc = {};
      function woche(w) { return wc[w] || (wc[w] = dek(S.wochen[w])); }
      var von = steuer.querySelector('[name=von]'), bis = steuer.querySelector('[name=bis]'), wie = steuer.querySelector('[name=wie]');
      var tipI = k2 ? S.tip : S.tip1, tipTitel = null;
      function leer() { var d = woche(wl[wl.length - 1]); return { x0: d.x0, x1: d.x1, y0: d.y0, y1: d.y1, ncol: d.ncol, nrow: d.nrow, v: new Float32Array(d.v.length) }; }
      function setzeZ(nz, t, kopf, werte) {
        z = nz;
        if (nz === kum) { lage.auto = S.auto.slice(); lage.sk = S.skala.slice(); } else if (!zustand) { lage.auto = auto(z, G.aus, S.voll); lage.sk = lage.auto.slice(); }
        zeichnen();
        titel.textContent = t;
        var kk = k2 || k1;
        if (kk) kk.textContent = kopf;
        svg.querySelectorAll(k2 ? '.gw-v-w2' : '.gw-v-w1').forEach(function(el) { var v = werte[+el.getAttribute('data-i')]; el.textContent = v === null ? '–' : zahl(v) + (k2 ? '' : ' ' + S.einheit); });
        if (window.D && tipI > -1 && D.themen[tipI]) {
          if (tipTitel === null) tipTitel = D.themen[tipI].titel;
          D.themen[tipI].titel = t === S.titel ? tipTitel : t;
          D.themen[tipI].werte = werte.map(function(v) { return v === null ? '–' : zahl(v) + ' ' + S.einheit; });
        }
      }
      function anOrten(d) { return G.orte.map(function(o) { return wert(d, o.lon, o.lat); }); }
      function differenz(a, b) {
        var zb = woche(b), za = a > 0 ? woche(a) : null, d = leer();
        for (var i = 0; i < d.v.length; i++) d.v[i] = za ? zb.v[i] - za.v[i] : zb.v[i];
        return d;
      }
      function zusammen(liste, art) {
        var d = leer(), n = new Float32Array(d.v.length);
        d.v.fill(art === 'mittel' ? 0 : NaN);
        liste.forEach(function(w) {
          var q = woche(w);
          for (var i = 0; i < d.v.length; i++) {
            var x = q.v[i]; if (isNaN(x)) continue;
            if (art === 'mittel') { d.v[i] += x; n[i]++; }
            else if (isNaN(d.v[i]) || (art === 'min' ? x < d.v[i] : x > d.v[i])) d.v[i] = x;
          }
        });
        if (art === 'mittel') for (var i = 0; i < d.v.length; i++) d.v[i] = n[i] ? d.v[i] / n[i] : NaN;
        return d;
      }
      var letzte = wl[wl.length - 1], ART = { mittel: 'Mittel', min: 'tiefster Wert', max: 'höchster Wert' }, ARTK = { mittel: 'Ø', min: 'min.', max: 'max.' };
      function anwenden() {
        var art = steuer.querySelector('[aria-pressed=true]').getAttribute('data-z');
        steuer.querySelector('.gw-v-frei').hidden = art !== 'frei';
        if (art === 'kum') setzeZ(kum, S.titel, k2 ? '28 Tage' : 'Wert', k2 ? S.werte28 : S.werte);
        else if (art === '28') {
          if (zustand) {
            var vier = wl.filter(function(w) { return w > letzte - 4; }), d4 = zusammen(vier, 'mittel');
            setzeZ(d4, S.kurz + ', Mittel KW ' + vier[0] + '–' + letzte + ' (bis ' + S.stand + ')', 'Ø 4 Wochen', anOrten(d4));
          } else if (S.z28) setzeZ(dek(S.z28), S.kurz + ' letzte 28 Tage (bis ' + S.stand + ')', '28 Tage', S.werte28);
          else {
            var a4 = wl.filter(function(w) { return w <= letzte - 4; }).pop() || 0, d28 = differenz(a4, letzte);
            setzeZ(d28, S.kurz + ' letzte 28 Tage (bis ' + S.stand + ')', '28 Tage', anOrten(d28));
          }
        } else {
          var a = +von.value, b = +bis.value, d, lbl, kopf;
          if (zustand) {
            if (b < a) { var t = a; a = b; b = t; von.value = String(a); bis.value = String(b); }
            var w = wie ? wie.value : 'mittel', liste = wl.filter(function(x) { return x >= a && x <= b; });
            d = zusammen(liste, w);
            lbl = a === b ? S.kurz + ' KW ' + a + ' (Stand ' + S.wochenStand[a] + ')' : S.kurz + ', ' + ART[w] + ' KW ' + a + '–' + b;
            kopf = a === b ? 'KW ' + a : ARTK[w] + ' KW ' + a + '–' + b;
          } else {
            if (b <= a) { var nb = wl.filter(function(x) { return x > a; })[0]; if (!nb) return; bis.value = String(nb); b = nb; }
            d = differenz(a, b);
            lbl = S.kurz + ' ' + (a > 0 ? S.wochenStand[a] : '1.1.') + '–' + S.wochenStand[b];
            kopf = (a > 0 ? 'KW ' + a : '1.1.') + '–' + b;
          }
          setzeZ(d, lbl, kopf, anOrten(d));
        }
      }
      steuer.querySelectorAll('[data-z]').forEach(function(b) {
        b.addEventListener('click', function() { steuer.querySelectorAll('[data-z]').forEach(function(x) { x.setAttribute('aria-pressed', String(x === b)); }); anwenden(); });
      });
      [von, bis, wie].forEach(function(s) { if (s) s.addEventListener('change', anwenden); });
    });
    // Mehrjahresvergleich Graswachstum: gewaehltes Jahr kraeftig, die anderen grau, Mittelland gestrichelt;
    // Satz oben vergleicht den Zuwachs im gleichen Zeitraum
    (function(M) {
      var fig = M && document.querySelector('.gw-mj'), box = fig && fig.querySelector('.gw-mj-inhalt');
      if (!box) return;
      var lage = { b: 0, jahr: M.aktuell, art: 'kum' }, SVGNS = 'http://www.w3.org/2000/svg';
      function ganz(v) { return String(Math.round(v)).replace(/\B(?=(\d{3})+(?!\d))/g, '’'); }
      // Linear verbundene Messwerte; Integral der Tageswerte (kg TS/ha/Tag) zwischen zwei Wochen in dt TS/ha
      function an(p, w) {
        if (!p.length || w < p[0][0] || w > p[p.length - 1][0]) return null;
        for (var i = 1; i < p.length; i++) if (w <= p[i][0]) { var a = p[i - 1], b = p[i]; return a[1] + (b[1] - a[1]) * (w - a[0]) / ((b[0] - a[0]) || 1); }
        return p[0][1];
      }
      function integral(p, von, bis) {
        var s = 0;
        for (var i = 1; i < p.length; i++) {
          var a = Math.max(von, p[i - 1][0]), b = Math.min(bis, p[i][0]);
          if (b > a) s += (an(p, a) + an(p, b)) / 2 * (b - a) * 7;
        }
        return s / 100;
      }
      function kumuliert(p) { return p.map(function(q) { return [q[0], integral(p, p[0][0], q[0])]; }); }
      function reihe(p) { return lage.art === 'kum' ? kumuliert(p) : p; }
      function zeichnen() {
        var B = M.bezuege[lage.b], jahre = Object.keys(B.jahre).sort();
        if (!B.jahre[lage.jahr]) lage.jahr = B.jahre[M.aktuell] ? M.aktuell : jahre[jahre.length - 1];
        var hj = B.jahre[lage.jahr], ende = Math.min(hj[hj.length - 1][0], lage.jahr === M.aktuell ? M.wocheBis : 52);
        // Satz: Zuwachs im gleichen Zeitraum (ab der spaetesten ersten Messung der verglichenen Jahre)
        var start = hj[0][0];
        var eigen = integral(hj, start, ende);
        var vergleich = jahre.filter(function(j) { return j !== lage.jahr; }).map(function(j) {
          // gemeinsamer Zeitraum beider Jahre
          var p = B.jahre[j], s = Math.max(start, p[0][0]), e = Math.min(ende, p[p.length - 1][0]);
          if (e - s < 4) return null;
          return { name: j, wert: integral(p, s, e), eigen: integral(hj, s, e), s: s, e: e };
        }).filter(Boolean);
        if (M.referenz.length) vergleich.push({ name: 'Mittelland langjährig', wert: integral(M.referenz, start, ende), eigen: eigen, s: start, e: ende });
        var satz = '<p class="gw-mj-satz"><b>' + lage.jahr + ': ' + ganz(eigen) + ' dt TS/ha</b> gewachsen von KW ' + start + ' bis ' + ende + ' <span>(' + B.lang + ')</span></p>';
        if (vergleich.length) satz += '<ul class="gw-mj-vgl">' + vergleich.map(function(v) {
          var pr = v.wert ? Math.round((v.eigen / v.wert - 1) * 100) : 0;
          return '<li>' + (v.name === 'Mittelland langjährig' ? 'gegenüber Mittelland (langjährig): ' : 'gegenüber ' + v.name + ': ') + '<b>' + (pr === 0 ? 'gleich viel' : Math.abs(pr) + ' % ' + (pr > 0 ? 'mehr' : 'weniger')) + '</b>'
            + ' <span>(' + ganz(v.eigen) + ' gegen ' + ganz(v.wert) + ' dt' + (v.s !== start || v.e !== ende ? ', verglichen KW ' + v.s + '–' + v.e : '') + ')</span></li>';
        }).join('') + '</ul>';
        // Diagramm
        var W = 640, H = 270, l = 52, r = 64, o = 16, u = 34, pb = W - l - r, ph = H - o - u, yMax = 1;
        var serien = jahre.map(function(j) { return { name: j, p: reihe(B.jahre[j]), an: j === lage.jahr }; });
        var ref = M.referenz.length ? { name: 'Mittelland', p: lage.art === 'kum' ? kumuliert(M.referenz.filter(function(q) { return q[0] >= serien.filter(function(s) { return s.an; })[0].p[0][0]; })) : M.referenz, ref: true } : null;
        serien.concat(ref ? [ref] : []).forEach(function(s) { s.p.forEach(function(q) { if (q[1] > yMax) yMax = q[1]; }); });
        var st = schritt(yMax / 4); yMax = Math.ceil(yMax / st) * st;
        var X = function(w) { return l + (w - 1) / 51 * pb; }, Y = function(v) { return o + ph - v / yMax * ph; };
        var t = ['<svg class="gw-rb-svg" viewBox="0 0 ' + W + ' ' + H + '" role="img" aria-label="Graswachstum je Jahr, ' + lage.jahr + ' hervorgehoben">'];
        for (var v = 0; v <= yMax + 1e-9; v += st) {
          t.push('<line x1="' + l + '" x2="' + (l + pb) + '" y1="' + Y(v).toFixed(1) + '" y2="' + Y(v).toFixed(1) + '" stroke="' + (v === 0 ? '#9a9a9a' : '#e6e6e6') + '"/>');
          t.push('<text x="' + (l - 6) + '" y="' + (Y(v) + 4).toFixed(1) + '" font-size="11" fill="#52514e" text-anchor="end">' + ganz(v) + '</text>');
        }
        t.push('<text x="' + (l - 44) + '" y="11" font-size="11" fill="#52514e">' + (lage.art === 'kum' ? 'dt TS/ha' : 'kg TS/ha/Tag') + '</text>');
        for (var w = 1; w <= 52; w += 4) t.push('<text x="' + X(w).toFixed(1) + '" y="' + (o + ph + 16) + '" font-size="11" fill="#52514e" text-anchor="middle">' + w + '</text>');
        t.push('<text x="' + (l + pb / 2) + '" y="' + (H - 2) + '" font-size="11" fill="#52514e" text-anchor="middle">Kalenderwoche</text>');
        function pfad(p) { var d = '', vor = null; p.forEach(function(q) { d += (vor === null || (lage.art !== 'kum' && q[0] - vor > 3) ? 'M' : 'L') + X(q[0]).toFixed(1) + ' ' + Y(q[1]).toFixed(1); vor = q[0]; }); return d; }
        if (ref) t.push('<path d="' + pfad(ref.p) + '" fill="none" stroke="#eda100" stroke-width="2" stroke-dasharray="6 4"/>');
        serien.filter(function(s) { return !s.an; }).forEach(function(s) { t.push('<path d="' + pfad(s.p) + '" fill="none" stroke="#a8a69e" stroke-width="1.6" stroke-linejoin="round"/>'); });
        serien.filter(function(s) { return s.an; }).forEach(function(s) { t.push('<path d="' + pfad(s.p) + '" fill="none" stroke="#fff" stroke-width="6" stroke-linejoin="round"/><path d="' + pfad(s.p) + '" fill="none" stroke="#2a78d6" stroke-width="3" stroke-linejoin="round" stroke-linecap="round"/>'); });
        var enden = serien.concat(ref ? [ref] : []).map(function(s) { var q = s.p[s.p.length - 1]; return q ? { s: s, x: X(q[0]), y: Y(q[1]), yr: Y(q[1]) } : null; }).filter(Boolean).sort(function(a, b) { return a.y - b.y; });
        enden.forEach(function(e, i) { if (i > 0 && e.y - enden[i - 1].y < 13) e.y = enden[i - 1].y + 13; });
        // unten nicht ueber die Achse hinaus: von unten her wieder nach oben schieben
        for (var ei = enden.length - 1; ei >= 0; ei--) { var grenze = ei === enden.length - 1 ? o + ph - 4 : enden[ei + 1].y - 13; if (enden[ei].y > grenze) enden[ei].y = grenze; }
        enden.forEach(function(e) {
          var f = e.s.ref ? '#eda100' : e.s.an ? '#2a78d6' : '#a8a69e';
          t.push('<circle cx="' + e.x.toFixed(1) + '" cy="' + e.yr.toFixed(1) + '" r="' + (e.s.an ? 4 : 3) + '" fill="' + f + '" stroke="#fff" stroke-width="2"/>');
          t.push('<text x="' + (Math.min(e.x, l + pb) + 7).toFixed(1) + '" y="' + (e.y + 4).toFixed(1) + '" font-size="11"' + (e.s.an ? ' font-weight="700"' : '') + ' fill="#1e1e1e">' + e.s.name + '</text>');
        });
        t.push('<line class="gw-rb-kreuz" x1="0" x2="0" y1="' + o + '" y2="' + (o + ph) + '" stroke="#52514e" visibility="hidden"/><rect x="' + l + '" y="' + o + '" width="' + pb + '" height="' + ph + '" fill="transparent"/></svg>');
        var knopf = function(gruppe, wert, text, an) { return '<button type="button" data-' + gruppe + '="' + wert + '" aria-pressed="' + an + '">' + text + '</button>'; };
        var leiste = '<div class="gw-mj-leiste"><div role="group" aria-label="Jahr hervorheben">' + jahre.map(function(j) { return knopf('jahr', j, j, j === lage.jahr); }).join('') + '</div>'
          + '<div role="group" aria-label="Darstellung">' + knopf('art', 'kum', 'aufsummiert', lage.art === 'kum') + knopf('art', 'tag', 'je Tag', lage.art === 'tag') + '</div>'
          + (M.bezuege.length > 1 ? '<div role="group" aria-label="Messung">' + M.bezuege.map(function(b, i) { return knopf('b', i, b.name, i === lage.b); }).join('') + '</div>' : '') + '</div>';
        var legende = '<div class="gw-rb-legende"><span><i style="border-top:3px solid #2a78d6"></i>' + lage.jahr + '</span><span><i style="border-top:2px solid #a8a69e"></i>andere Jahre</span>' + (ref ? '<span><i style="border-top:2px dashed #eda100"></i>Durchschnitt Mittelland, langjährig</span>' : '') + '</div>';
        var wochen = {}; serien.forEach(function(s) { s.p.forEach(function(q) { wochen[q[0]] = 1; }); });
        var tabelle = '<details class="gw-rb-tabelle"><summary>Tabelle</summary><table><thead><tr><th>KW</th>' + jahre.map(function(j) { return '<th>' + j + '</th>'; }).join('') + '</tr></thead><tbody>'
          + Object.keys(wochen).map(Number).sort(function(a, b) { return a - b; }).map(function(w) { return '<tr><td>' + w + '</td>' + serien.map(function(s) { var q = s.p.filter(function(x) { return x[0] === w; })[0]; return '<td>' + (q ? ganz(q[1]) : '–') + '</td>'; }).join('') + '</tr>'; }).join('') + '</tbody></table></details>';
        box.innerHTML = leiste + satz + legende + '<div class="gw-rb-fig">' + t.join('') + '</div>' + tabelle
          + '<p class="gw-rb-hinweis">Aufsummiert: Tageszuwachs zwischen den Messungen linear verbunden, ab der ersten Messung des Jahres. Vergleiche gelten für denselben Zeitraum. «Alle Standorte»: das Messnetz ist nicht jedes Jahr gleich.</p>';
        [['jahr', String], ['art', String], ['b', Number]].forEach(function(g) {
          box.querySelectorAll('[data-' + g[0] + ']').forEach(function(k) {
            k.addEventListener('click', function() {
              var wert = k.getAttribute('data-' + g[0]); lage[g[0]] = g[1](wert); zeichnen();
              var neu = box.querySelector('[data-' + g[0] + '="' + wert + '"]'); if (neu) neu.focus();
            });
          });
        });
        // Fadenkreuz: Werte aller Jahre in der Woche
        var svg = box.querySelector('svg'), kreuz = svg.querySelector('.gw-rb-kreuz'), tip = document.getElementById('tip');
        function weg() { kreuz.setAttribute('visibility', 'hidden'); if (tip) tip.hidden = true; }
        svg.addEventListener('pointerleave', weg);
        svg.addEventListener('pointermove', function(e) {
          var p = svg.createSVGPoint(); p.x = e.clientX; p.y = e.clientY; p = p.matrixTransform(svg.getScreenCTM().inverse());
          var w = Math.round((p.x - l) / pb * 51 + 1); if (w < 1 || w > 52 || !tip) { weg(); return; }
          var zeilen = serien.concat(ref ? [ref] : []).map(function(s) { var v = an(s.p, w); return v === null ? '' : '<br><i style="display:inline-block;width:10px;border-top:2px ' + (s.ref ? 'dashed #eda100' : 'solid ' + (s.an ? '#2a78d6' : '#a8a69e')) + ';vertical-align:middle;margin-right:5px"></i>' + (s.an ? '<b>' : '') + s.name + ': ' + ganz(v) + (s.an ? '</b>' : '') + (lage.art === 'kum' ? ' dt' : ' kg'); }).join('');
          if (!zeilen) { weg(); return; }
          var x = l + (w - 1) / 51 * pb; kreuz.setAttribute('x1', x); kreuz.setAttribute('x2', x); kreuz.setAttribute('visibility', 'visible');
          tip.innerHTML = '<b>KW ' + w + '</b>' + zeilen; tip.hidden = false;
          tip.style.left = Math.min(e.clientX + 14, innerWidth - tip.offsetWidth - 8) + 'px'; tip.style.top = Math.max(8, e.clientY - tip.offsetHeight - 14) + 'px';
        });
      }
      zeichnen();
    })(G.mj);
    // Vollbild fuer die Diagramme des Jahresrueckblicks
    var voll = null, platz = null;
    function zu() { if (!voll) return; platz.parentNode.replaceChild(voll.querySelector('figure'), platz); voll.remove(); voll = null; document.body.style.overflow = ''; }
    document.querySelectorAll('.gw-rb-gross').forEach(function(b) {
      b.addEventListener('click', function() {
        var fig = b.closest('figure'); platz = document.createComment('vollbild');
        fig.parentNode.replaceChild(platz, fig);
        voll = document.createElement('div'); voll.className = 'gw-voll'; voll.setAttribute('role', 'dialog'); voll.setAttribute('aria-modal', 'true'); voll.setAttribute('aria-label', 'Diagramm im Vollbild');
        voll.innerHTML = '<button type="button" class="gw-voll-zu" aria-label="Vollbild schliessen">×</button>';
        voll.appendChild(fig); document.body.appendChild(voll); document.body.style.overflow = 'hidden';
        voll.querySelector('.gw-voll-zu').addEventListener('click', zu); voll.querySelector('.gw-voll-zu').focus();
      });
    });
    document.addEventListener('keydown', function(e) { if (e.key === 'Escape') zu(); });
  }

  function veranstaltungHtml(seiten, opt, rueckblick) {
    var rb = rueckblick ? jahresrueckblickHtml(rueckblick) : null;
    var daten = {
      orte: eigeneOrte.map(function(o) { return o.name; }),
      themen: seiten.reduce(function(l, s) {
        l.push({ titel: s.t.thema.titel, einheit: String(s.t.info.einheit).split(',')[0], werte: s.t.werte.map(function(w) { return wertText(w, s.t.info); }) });
        if (s.t.werte2) l.push({ titel: s.t.werte2.titel, einheit: 'mm', werte: s.t.werte2.werte.map(function(w) { return wertText(w, s.t.werte2.info); }) });
        return l;
      }, [])
    };
    var tipIdx = 0, gwb = { orte: eigeneOrte.map(function(o) { return { lon: o.lon, lat: o.lat }; }), aus: seiten[0] && seiten[0].aus, seiten: [], mj: rueckblick && rueckblick.mehrjahr || null };
    seiten.forEach(function(s) {
      var t = s.t, e = { z: zKodieren(t.z), skala: t.skala, auto: t.auto, voll: t.voll, farben: t.info.farben, titel: t.thema.titel + ' ' + selectedYear, tip: -1,
        art: t.thema.art, kurz: t.thema.kurz, einheit: String(t.info.einheit).split(',')[0], woche: t.woche, stand: t.stand, werte: t.werte, tip1: tipIdx };
      tipIdx++;
      if (t.werte2) { e.tip = tipIdx; tipIdx++; e.werte28 = t.werte2.werte; e.z28 = zKodieren(t.z28); }
      if (t.wochen && Object.keys(t.wochen).length > 1) {
        e.wochen = {}; e.wochenStand = t.wochenStand;
        Object.keys(t.wochen).forEach(function(w) { e.wochen[w] = zKodieren(t.wochen[w]); });
      }
      gwb.seiten.push(e);
    });
    var tabs = seiten.map(function(s, i) { return '<button type="button" data-t="' + i + '"' + (i === 0 ? ' class="an"' : '') + '>' + esc(s.t.thema.titel) + '</button>'; }).join('') +
      (rb ? '<button type="button" data-t="rb">Jahresrückblick ' + selectedYear + '</button>' : '');
    var bloecke = seiten.map(function(s, i) {
      var steuer = '', wl = s.t.wochen ? Object.keys(s.t.wochen).map(Number).sort(function(a, b) { return a - b; }) : [];
      if (wl.length > 1 && s.t.thema.art) {
        var zustand = s.t.thema.art === 'zustand';
        var opts = function(sel) { return wl.map(function(w) { return '<option value="' + w + '"' + (w === sel ? ' selected' : '') + '>KW ' + w + ' (' + esc(s.t.wochenStand[w] || '') + ')</option>'; }).join(''); };
        steuer = '<div class="gw-v-steuer" role="group" aria-label="Zeitraum"><span>Zeitraum:</span>'
          + (zustand ? '<button type="button" data-z="kum" aria-pressed="true">Stichtag</button><button type="button" data-z="28" aria-pressed="false">Mittel letzte 4 Wochen</button>'
            : '<button type="button" data-z="kum" aria-pressed="true">seit 1. Januar</button><button type="button" data-z="28" aria-pressed="false">letzte 28 Tage</button>')
          + '<button type="button" data-z="frei" aria-pressed="false">frei wählen</button>'
          + '<span class="gw-v-frei" hidden><label>von <select name="von">' + (zustand ? '' : '<option value="0">1. Januar</option>') + opts(wl[Math.max(0, wl.length - (zustand ? 5 : 9))]) + '</select></label> <label>bis <select name="bis">' + opts(wl[wl.length - 1]) + '</select></label>'
          + (zustand ? ' <label>Wert <select name="wie"><option value="mittel">Mittel</option><option value="min">tiefster</option><option value="max">höchster</option></select></label>' : '') + '</span></div>';
      }
      return '<section data-t="' + i + '"' + (i === 0 ? '' : ' hidden') + '>' + steuer + s.svg + '<p class="link"><a href="' + esc(s.link) + '" target="_blank" rel="noopener">Diese Karte im Datenexplorer öffnen</a></p></section>';
    }).join('');
    var skript = 'var GWB=' + JSON.stringify(gwb).replace(/</g, '\\u003c') + ';window.GWB=GWB;(' + berichtLaufzeit.toString() + ')();' + 'var D=' + JSON.stringify(daten).replace(/</g, '\\u003c') + ';var R=' + JSON.stringify(rb ? rb.daten : {}).replace(/</g, '\\u003c') + ';' +
      'var tip=document.getElementById("tip");' +
      'document.querySelectorAll("nav button").forEach(function(b){b.addEventListener("click",function(){' +
      'document.querySelectorAll("nav button").forEach(function(x){x.classList.toggle("an",x===b)});' +
      'document.querySelectorAll("main section").forEach(function(s){s.hidden=s.getAttribute("data-t")!==b.getAttribute("data-t")});tip.hidden=true;})});' +
      'function zeige(i,e){var h="<b>"+(i+1)+". "+D.orte[i].replace(/</g,"&lt;")+"</b>";D.themen.forEach(function(t){h+="<br>"+t.titel+": <b>"+t.werte[i]+"</b>"});' +
      'tip.innerHTML=h;tip.hidden=false;var x=e.clientX,y=e.clientY;tip.style.left=Math.min(x+12,innerWidth-tip.offsetWidth-8)+"px";tip.style.top=Math.max(8,y-tip.offsetHeight-12)+"px";' +
      'document.querySelectorAll(".gw-v-ort,.gw-v-zeile").forEach(function(g){g.classList.toggle("hell",+g.getAttribute("data-i")===i)})}' +
      'document.querySelectorAll(".gw-v-ort,.gw-v-zeile").forEach(function(g){var i=+g.getAttribute("data-i");' +
      'g.addEventListener("mouseenter",function(e){zeige(i,e)});g.addEventListener("click",function(e){e.stopPropagation();zeige(i,e)});' +
      'g.addEventListener("mouseleave",function(){tip.hidden=true;document.querySelectorAll(".hell").forEach(function(x){x.classList.remove("hell")})})});' +
      'document.addEventListener("click",function(){tip.hidden=true});' +
      // Jahresrueckblick: Fadenkreuz und Tooltip je Diagramm
      'document.querySelectorAll(".gw-rb-fig").forEach(function(f){var d=R[f.getAttribute("data-id")];if(!d)return;var svg=f.querySelector("svg"),kreuz=svg.querySelector(".gw-rb-kreuz"),pk=[];' +
      'd.serien.forEach(function(s){var c=document.createElementNS("http://www.w3.org/2000/svg","circle");c.setAttribute("r","4");c.setAttribute("fill",s.farbe);c.setAttribute("stroke","#fff");c.setAttribute("stroke-width","2");c.setAttribute("visibility","hidden");svg.appendChild(c);pk.push(c)});' +
      'function weg(){kreuz.setAttribute("visibility","hidden");pk.forEach(function(c){c.setAttribute("visibility","hidden")});tip.hidden=true}' +
      'svg.addEventListener("pointerleave",weg);svg.addEventListener("pointermove",function(e){var p=svg.createSVGPoint();p.x=e.clientX;p.y=e.clientY;p=p.matrixTransform(svg.getScreenCTM().inverse());' +
      'var w=Math.round((p.x-d.l)/d.pb*51+1);if(w<1||w>52){weg();return}var x=d.l+(w-1)/51*d.pb,h="<b>KW "+w+"</b>",irgend=false;' +
      'd.serien.forEach(function(s,i){var v=s.werte[w];if(v===undefined){pk[i].setAttribute("visibility","hidden");return}irgend=true;var y=d.o+d.ph-v/d.yMax*d.ph;pk[i].setAttribute("cx",x);pk[i].setAttribute("cy",y);pk[i].setAttribute("visibility","visible");' +
      'h+="<br><i style=\\"display:inline-block;width:10px;border-top:2px solid "+s.farbe+";vertical-align:middle;margin-right:5px\\"></i>"+s.name+": <b>"+String(Math.round(v)).replace(/\\B(?=(\\d{3})+(?!\\d))/g,"\u2019")+" "+d.einheit+"</b>"});' +
      'if(!irgend){weg();return}kreuz.setAttribute("x1",x);kreuz.setAttribute("x2",x);kreuz.setAttribute("visibility","visible");tip.innerHTML=h;tip.hidden=false;' +
      'tip.style.left=Math.min(e.clientX+14,innerWidth-tip.offsetWidth-8)+"px";tip.style.top=Math.max(8,e.clientY-tip.offsetHeight-14)+"px"})});';
    var titel = opt.titel || ('Graswachstum ' + selectedYear + ' – Karten für eigene Orte');
    return '<!doctype html><html lang="de"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">' +
      '<title>' + esc(titel) + '</title><style>' +
      'body{margin:0;font-family:system-ui,-apple-system,"Segoe UI",Roboto,sans-serif;background:#eef1ee;color:#1e1e1e}' +
      'header{padding:14px 18px 6px}header h1{margin:0;font-size:20px}header p{margin:4px 0 0;color:#555;font-size:13px}' +
      'nav{display:flex;flex-wrap:wrap;gap:6px;padding:8px 18px}nav button{font:inherit;font-size:14px;padding:7px 12px;border:1px solid #9bb5a8;border-radius:16px;background:#fff;cursor:pointer}' +
      'nav button.an{background:#2f6f5e;color:#fff;border-color:#2f6f5e}' +
      'main{padding:6px 18px 24px}section svg{width:100%;height:auto;max-height:calc(100vh - 150px);background:#fff;box-shadow:0 1px 6px rgba(0,0,0,.15);border-radius:4px;display:block;margin:0 auto}' +
      '.gw-v-ort,.gw-v-zeile{cursor:pointer}.gw-v-ort.hell circle{stroke:#ffd54f;stroke-width:1.2}.gw-v-zeile.hell text{font-weight:700}' +
      '.link{text-align:center;font-size:14px}.link a{color:#2f6f5e}' +
      '#tip{position:fixed;z-index:9;background:#fff;border:1px solid #bbb;border-radius:8px;box-shadow:0 3px 12px rgba(0,0,0,.2);padding:7px 10px;font-size:13px;line-height:1.45;max-width:300px;pointer-events:none}' +
      '.gw-rb{max-width:1200px;margin:0 auto}.gw-rb-kacheln{display:grid;grid-template-columns:repeat(auto-fit,minmax(220px,1fr));gap:10px;margin-bottom:12px}' +
      '.gw-rb-kachel{background:#fff;border-radius:8px;padding:10px 12px;box-shadow:0 1px 4px rgba(0,0,0,.1)}.gw-rb-kachel span{display:block;font-size:12.5px;color:#52514e}' +
      '.gw-rb-kachel b{display:block;font-size:24px;margin:2px 0}.gw-rb-kachel em{display:block;font-style:normal;font-size:12.5px;color:#52514e}' +
      '.gw-rb-gitter{display:grid;grid-template-columns:repeat(auto-fit,minmax(min(100%,520px),1fr));gap:12px}' +
      '.gw-rb-karte{margin:0;background:#fff;border-radius:8px;padding:10px 12px;box-shadow:0 1px 4px rgba(0,0,0,.1)}.gw-rb-karte figcaption{font-weight:700;font-size:15px;margin-bottom:4px}.gw-rb-karte small{font-weight:400;color:#b23b00}' +
      '.gw-rb-legende{display:flex;flex-wrap:wrap;gap:4px 14px;font-size:12.5px;color:#1e1e1e;margin:2px 0 4px}.gw-rb-legende i{display:inline-block;width:18px;vertical-align:middle;margin-right:5px}' +
      '.gw-rb-svg{width:100%;height:auto;display:block;touch-action:pan-y}.gw-rb-tabelle summary{cursor:pointer;font-size:12.5px;color:#2f6f5e;margin-top:4px}' +
      '.gw-rb-tabelle table{border-collapse:collapse;font-size:12px;margin-top:6px}.gw-rb-tabelle th,.gw-rb-tabelle td{padding:2px 8px;text-align:right;border-bottom:1px solid #eee}' +
      '.gw-rb-hinweis{font-size:12px;color:#52514e;margin-top:10px}.gw-rb-kachel em.gw-rb-normal{color:#1e1e1e;font-weight:600;margin-bottom:2px}' +
      '.gw-mj{grid-column:1/-1}.gw-mj-leiste{display:flex;flex-wrap:wrap;gap:8px 16px;margin:4px 0 8px}.gw-mj-leiste div{display:flex;flex-wrap:wrap;gap:4px}' +
      '.gw-mj-leiste button{font:inherit;font-size:14px;padding:6px 12px;min-height:36px;border:1px solid #9bb5a8;border-radius:16px;background:#fff;cursor:pointer}.gw-mj-leiste button[aria-pressed=true]{background:#2a78d6;border-color:#2a78d6;color:#fff;font-weight:700}' +
      '.gw-mj-leiste div:nth-child(n+2) button[aria-pressed=true]{background:#2f6f5e;border-color:#2f6f5e}' +
      '.gw-mj-satz{margin:2px 0;font-size:15px}.gw-mj-satz b{font-size:20px}.gw-mj-satz span,.gw-mj-vgl span{color:#52514e;font-size:13px}.gw-mj-vgl{margin:2px 0 6px;padding-left:18px;font-size:14px;line-height:1.5}' +
      '.gw-v-steuer{display:flex;flex-wrap:wrap;gap:6px;align-items:center;justify-content:center;margin:0 auto 8px;font-size:14px}.gw-v-steuer button{font:inherit;padding:7px 12px;border:1px solid #9bb5a8;border-radius:16px;background:#fff;cursor:pointer;min-height:36px}' +
      '.gw-v-steuer button[aria-pressed=true]{background:#2f6f5e;color:#fff;border-color:#2f6f5e}.gw-v-steuer select{font:inherit;padding:5px 6px;border-radius:6px;border:1px solid #9bb5a8;background:#fff}' +
      '.gw-v-griff:focus{outline:none}.gw-v-griff:focus path{fill:#c2185b}' +
      '.gw-rb-karte figcaption{display:flex;align-items:center;gap:8px}.gw-rb-gross{margin-left:auto;font:inherit;font-size:18px;line-height:1;width:34px;height:34px;border:1px solid #c8d3cd;border-radius:8px;background:#fff;cursor:pointer}' +
      '.gw-voll{position:fixed;inset:0;z-index:50;background:#fff;padding:16px 16px 24px;overflow:auto;display:flex;flex-direction:column;box-sizing:border-box}.gw-voll *{box-sizing:border-box}.gw-voll figure{margin:0 auto;width:100%;max-width:1400px;box-shadow:none}.gw-voll .gw-rb-gross{display:none}' +
      '.gw-voll .gw-rb-svg{max-height:calc(100vh - 140px)}.gw-voll-zu{position:fixed;top:10px;right:12px;z-index:51;width:44px;height:44px;font-size:28px;line-height:1;border:1px solid #c8d3cd;border-radius:50%;background:#fff;cursor:pointer}' +
      '@media (max-width:600px){header,nav,main{padding-left:10px;padding-right:10px}.gw-rb-fig{overflow-x:auto}.gw-rb-svg{min-width:560px}}' +
      '@media print{nav,.link,#tip{display:none}main section{display:block!important;page-break-after:always}section svg{box-shadow:none;max-height:none}}' +
      '</style></head><body><header><h1>' + esc(titel) + '</h1><p>Stichtag ' + esc(wocheText(seiten[0].t.woche)) + ' · erstellt am ' + heuteText() + ' · Orte antippen für die Werte aller Themen</p></header>' +
      '<nav>' + tabs + '</nav><main>' + bloecke + (rb ? rb.html : '') + '</main><div id="tip" hidden></div><script>' + skript + '</script></body></html>';
  }

  function exportVeranstaltung(opt, fortschritt) {
    if (!eigeneOrte.length) throw new Error('Bitte zuerst Orte erfassen.');
    var themen = VERANSTALTUNG_THEMEN.filter(function(t) { return opt.themen.indexOf(t.name) !== -1 && layerLegenden[t.name]; });
    if (!themen.length) throw new Error('Bitte mindestens ein Thema wählen.');
    if (!opt.formate.length) throw new Error('Bitte mindestens ein Format wählen.');
    var pxB = 1500, pxH = Math.round(pxB * SEITE.karteH / SEITE.karteB);
    opt.pxB = pxB; opt.pxH = pxH;
    var aus = opt.aus || veranstaltungsAusschnitt(opt.ausschnitt, pxB / pxH);
    var schritte = themen.length * opt.formate.length + 1, schritt = 0;
    var weiter = function() { fortschritt(++schritt, schritte); };
    return Promise.all([ladeBibliothek('qrcode'), opt.formate.indexOf('pdf') !== -1 ? ladeBibliothek('jspdf') : null,
      opt.formate.length > 1 || (opt.formate.indexOf('svg') !== -1 && themen.length > 1) ? ladeBibliothek('jszip') : null]).then(function() {
      return Promise.all([Promise.all(themen.map(function(th) { return veranstaltungsThema(th, selectedWeek, aus, opt); })),
        opt.formate.indexOf('html') !== -1 ? jahresrueckblickDaten(selectedWeek).catch(function(e) { console.warn('Jahresrückblick:', e); return null; }) : null]);
    }).then(function(ergebnis) {
      var daten = ergebnis[0], rueckblick = ergebnis[1];
      weiter();
      var dateien = [], doc = null, seitenHtml = [];
      var kette = Promise.resolve();
      daten.forEach(function(t) {
        var lang = veranstaltungsLink(t.thema, t.woche, aus);
        var datei = 'graswachstum_' + selectedYear + '-KW' + zweistellig(t.woche) + '_' + t.thema.name;
        kette = kette.then(function() {
          return Promise.all([kurzLink(lang), veranstaltungsKarte(aus, t, opt, pxB, pxH)]).then(function(r) {
            var link = r[0] || lang, k = r[1];
            opt.mitLandeskarte = k.landeskarte;
            var jpg = k.canvas.toDataURL('image/jpeg', 0.8);
            if (opt.formate.indexOf('pdf') !== -1) {
              if (!doc) doc = new window.jspdf.jsPDF({ unit: 'mm', format: 'a4', orientation: 'landscape', compress: true });
              else doc.addPage('a4', 'landscape');
              veranstaltungPdfSeite(doc, t, jpg, aus, opt, link);
              weiter();
            }
            if (opt.formate.indexOf('svg') !== -1) {
              dateien.push({ name: datei + '.svg', inhalt: '<?xml version="1.0" encoding="UTF-8"?>\n' + veranstaltungSeiteSvg(t, jpg, aus, opt, link) });
              weiter();
            }
            if (opt.formate.indexOf('html') !== -1) {
              var teile = { jpg: jpg, basis: k.basis.toDataURL('image/jpeg', 0.82), oben: k.oben ? k.oben.toDataURL('image/png') : null, landeskarte: k.landeskarte };
              seitenHtml.push({ t: t, link: link, aus: aus, svg: veranstaltungSeiteSvg(t, teile, aus, opt, link, true).replace(/ width="297mm" height="210mm"/, '') });
              weiter();
            }
          });
        });
      });
      return kette.then(function() {
        if (opt.nurHtml) return veranstaltungHtml(seitenHtml, opt, rueckblick);
        var basis = 'graswachstum_' + selectedYear + '-KW' + zweistellig(daten[0].woche) + '_orte';
        if (doc) dateien.unshift({ name: basis + '.pdf', inhalt: doc.output('blob') });
        if (seitenHtml.length) dateien.push({ name: basis + '.html', inhalt: new Blob([veranstaltungHtml(seitenHtml, opt, rueckblick)], { type: 'text/html;charset=utf-8' }) });
        if (dateien.length === 1) {
          var d = dateien[0];
          herunterladen(d.inhalt instanceof Blob ? d.inhalt : new Blob([d.inhalt], { type: 'image/svg+xml;charset=utf-8' }), d.name);
          return;
        }
        var zip = new JSZip();
        dateien.forEach(function(d) { zip.file(d.name, d.inhalt); });
        return zip.generateAsync({ type: 'blob' }).then(function(blob) { herunterladen(blob, basis + '.zip'); });
      });
    });
  }

  // --- Seitenleiste ---
  var exportEl = null, exportStatusEl = null, exportOpener = null;
  function exportFeld(tag, klasse, text) { var e = document.createElement(tag); if (klasse) e.className = klasse; if (text !== undefined) e.textContent = text; return e; }
  function exportWahl(typ, name, wert, text, an) {
    var l = exportFeld('label', 'gw-export-wahl');
    var i = document.createElement('input'); i.type = typ; i.name = name; i.value = wert; i.checked = !!an;
    l.appendChild(i); l.appendChild(document.createTextNode(' ' + text));
    return l;
  }
  function gewaehlt(name) {
    return Array.prototype.slice.call(exportEl.querySelectorAll('input[name=' + name + ']:checked')).map(function(i) { return i.value; });
  }
  function exportStatus(text, fehler) {
    exportStatusEl.textContent = text || '';
    exportStatusEl.classList.toggle('fehler', !!fehler);
  }
  function exportAusfuehren(knopf, arbeit) {
    if (knopf.disabled) return;
    var alle = exportEl.querySelectorAll('.gw-export-los');
    Array.prototype.forEach.call(alle, function(k) { k.disabled = true; });
    exportStatus('Wird erstellt …');
    var fortschritt = function(n, gesamt) { exportStatus('Wird erstellt … ' + n + ' von ' + gesamt); };
    Promise.resolve().then(function() { return arbeit(fortschritt); }).then(function(meldung) {
      exportStatus(typeof meldung === 'string' ? meldung : 'Fertig – die Datei wird heruntergeladen.');
    }).catch(function(err) {
      console.error('Export:', err);
      exportStatus('Das hat nicht geklappt: ' + (err && err.message ? err.message : err), true);
    }).then(function() {
      Array.prototype.forEach.call(alle, function(k) { k.disabled = false; });
    });
  }
  function baueExport() {
    if (exportEl) return;
    exportEl = exportFeld('aside', 'gw-export');
    exportEl.setAttribute('aria-label', 'Exportieren');
    exportEl.addEventListener('click', function(evt) { evt.stopPropagation(); });
    exportEl.addEventListener('keydown', function(evt) { if (evt.key === 'Escape') { evt.stopPropagation(); schliesseExport(); } });
    var kopf = exportFeld('div', 'gw-export-kopf');
    kopf.appendChild(exportFeld('h2', 'gw-export-h', 'Exportieren'));
    var zu = exportFeld('button', 'gw-doku-zu', String.fromCharCode(215));
    zu.type = 'button'; zu.setAttribute('aria-label', 'Export schliessen');
    zu.addEventListener('click', schliesseExport);
    kopf.appendChild(zu);
    exportEl.appendChild(kopf);
    var rumpf = exportFeld('div', 'gw-export-rumpf');
    exportEl.appendChild(rumpf);
    var abschnitt = function(titel, text) {
      var a = exportFeld('section', 'gw-export-abschnitt');
      a.appendChild(exportFeld('h3', 'gw-export-titel', titel));
      if (text) a.appendChild(exportFeld('p', 'gw-export-text', text));
      rumpf.appendChild(a);
      return a;
    };
    var zeile = function(eltern, kinder, beschriftung) {
      var z = exportFeld('div', 'gw-export-zeile');
      if (beschriftung) z.appendChild(exportFeld('span', 'gw-export-beschr', beschriftung));
      kinder.forEach(function(k) { z.appendChild(k); });
      eltern.appendChild(z);
      return z;
    };
    var losKnopf = function(eltern, text, arbeit) {
      var b = exportFeld('button', 'gw-export-los', text);
      b.type = 'button';
      b.addEventListener('click', function() { exportAusfuehren(b, arbeit); });
      eltern.appendChild(b);
      return b;
    };
    // 0. Direktlink
    var a0 = abschnitt('Link zu dieser Ansicht', 'Die Adresse im Browser zeigt immer die aktuelle Ansicht (Jahr, Woche, Auswahl, Ebene, Kartenausschnitt). Wer den Link öffnet, sieht dasselbe.');
    var linkKnopf = exportFeld('button', 'gw-export-los gw-export-link', 'Link kopieren');
    linkKnopf.type = 'button';
    linkKnopf.addEventListener('click', function() {
      schreibeUrl();
      var url = window.location.href;
      var fertig = function() { exportStatus('Link kopiert.'); };
      if (navigator.clipboard && navigator.clipboard.writeText) navigator.clipboard.writeText(url).then(fertig, function() { window.prompt('Link kopieren:', url); });
      else window.prompt('Link kopieren:', url);
    });
    a0.appendChild(linkKnopf);
    // Veranstaltungskarte: eigene Orte auf Themenkarten
    var aV = abschnitt('Karte für eine Veranstaltung', 'Eigene Orte (z.B. die Betriebe der Teilnehmenden) auf Karten zu Niederschlag, Wasserbilanz und potenziellem Graswachstum, mit Wertetabelle und QR-Code zur App. Stichtag ist die Woche der Zeitleiste.');
    aV.classList.add('gw-export-veranstaltung');
    aV.appendChild(macheOrteEditor(exportStatus));
    aV.appendChild(exportFeld('p', 'gw-export-hinweis', 'Mehrere PLZ auf einmal: mit Komma oder Zeilenumbruch einfügen. Auf einen Namen tippen, um ihn zu ändern (z.B. Betriebsname).'));
    var orteSchalter = exportWahl('checkbox', 'gw-ex-v-zeigen', 'ja', 'Orte auf der Karte zeigen', orteAnzeigen);
    orteSchalter.querySelector('input').addEventListener('change', function(evt) { setzeOrteAnzeigen(evt.target.checked); });
    orteHoerer.push(function() { orteSchalter.querySelector('input').checked = orteAnzeigen; });
    zeile(aV, [orteSchalter]);
    var titelFeld = document.createElement('input');
    titelFeld.type = 'text'; titelFeld.className = 'gw-export-text-feld'; titelFeld.placeholder = 'z.B. Weidetagung Muri, 20.10.2026';
    titelFeld.setAttribute('aria-label', 'Titel der Veranstaltung (optional)');
    zeile(aV, [titelFeld], 'Titel');
    zeile(aV, VERANSTALTUNG_THEMEN.filter(function(t) { return layerLegenden[t.name]; }).map(function(t) {
      return exportWahl('checkbox', 'gw-ex-v-thema', t.name, t.titel + (t.experimentell ? ' (experimentell)' : ''), true);
    }), 'Themen');
    zeile(aV, [exportWahl('radio', 'gw-ex-v-aus', 'orte', 'um die Orte', true), exportWahl('radio', 'gw-ex-v-aus', 'aktuell', 'aktueller Ausschnitt', false),
      exportWahl('radio', 'gw-ex-v-aus', 'voll', 'ganze Schweiz', false)], 'Ausschnitt');
    zeile(aV, [exportWahl('radio', 'gw-ex-v-skala', 'ausschnitt', 'an den Ausschnitt angepasst', true), exportWahl('radio', 'gw-ex-v-skala', 'explorer', 'wie im Datenexplorer', false)], 'Farbskala');
    zeile(aV, [exportWahl('checkbox', 'gw-ex-v-lk', 'ja', 'Landeskarte (swisstopo)', true), exportWahl('checkbox', 'gw-ex-v-namen', 'ja', 'Namen an der Karte', true),
      exportWahl('checkbox', 'gw-ex-v-mess', 'ja', 'Messstandorte', false)], 'Karte');
    zeile(aV, [exportWahl('checkbox', 'gw-ex-v-fmt', 'pdf', 'PDF', true), exportWahl('checkbox', 'gw-ex-v-fmt', 'svg', 'SVG', false),
      exportWahl('checkbox', 'gw-ex-v-fmt', 'html', 'HTML (interaktiv)', true)], 'Format');
    var veranstaltungsOpt = function() {
      return {
        themen: gewaehlt('gw-ex-v-thema'), formate: gewaehlt('gw-ex-v-fmt'), ausschnitt: gewaehlt('gw-ex-v-aus')[0] || 'orte',
        namen: gewaehlt('gw-ex-v-namen').length > 0, messstandorte: gewaehlt('gw-ex-v-mess').length > 0, titel: titelFeld.value.trim(),
        landeskarte: gewaehlt('gw-ex-v-lk').length > 0, skala: gewaehlt('gw-ex-v-skala')[0] || 'ausschnitt'
      };
    };
    losKnopf(aV, 'Karten erstellen', function(fortschritt) { return exportVeranstaltung(veranstaltungsOpt(), fortschritt); });
    aV.appendChild(exportFeld('p', 'gw-export-hinweis', 'Oder den Bericht als Link teilen: Wer ihn öffnet, sieht die interaktive Ansicht mit allen Orten sofort, ohne etwas herunterzuladen (immer mit den aktuellen Daten).'));
    losKnopf(aV, 'Bericht als Link teilen', function() {
      var opt = veranstaltungsOpt();
      if (!eigeneOrte.length) throw new Error('Bitte zuerst Orte erfassen.');
      if (!opt.themen.length) throw new Error('Bitte mindestens ein Thema wählen.');
      var aus = veranstaltungsAusschnitt(opt.ausschnitt, SEITE.karteB / SEITE.karteH);
      var lang = berichtLink(opt, aus);
      return kurzLink(lang).then(function(kurz) {
        var url = kurz || lang;
        var meldung = kurz ? 'Kurzer Link kopiert: ' + ohneProtokoll(kurz) : 'Link kopiert (der Kurzlink-Dienst war nicht erreichbar, daher der lange Link).';
        if (navigator.clipboard && navigator.clipboard.writeText) return navigator.clipboard.writeText(url).then(function() { return meldung; }, function() { window.prompt('Link kopieren:', url); return meldung; });
        window.prompt('Link kopieren:', url);
        return meldung;
      });
    });
    // 1. Einzelbilder
    var a1 = abschnitt('Bilder', 'Karte und Kurve der aktuellen Ansicht. Mehrere Dateien kommen als ZIP.');
    zeile(a1, [exportWahl('checkbox', 'gw-ex-obj', 'karte', 'Karte', true), exportWahl('checkbox', 'gw-ex-obj', 'kurve', 'Kurve', true)], 'Objekte');
    zeile(a1, [exportWahl('checkbox', 'gw-ex-fmt', 'png', 'PNG', true), exportWahl('checkbox', 'gw-ex-fmt', 'svg', 'SVG', false)], 'Format');
    zeile(a1, [exportWahl('radio', 'gw-ex-gr', '1', 'Bildschirm', true), exportWahl('radio', 'gw-ex-gr', '2', 'Druck (2×)', false)], 'PNG-Grösse');
    losKnopf(a1, 'Bilder herunterladen', function() {
      var obj = gewaehlt('gw-ex-obj'), fmt = gewaehlt('gw-ex-fmt');
      if (!obj.length || !fmt.length) throw new Error('Bitte mindestens ein Objekt und ein Format wählen.');
      return exportBilder(obj, fmt, parseInt(gewaehlt('gw-ex-gr')[0] || '1', 10));
    });
    // 2. Zeitserie
    var a2 = abschnitt('Zeitserie (ZIP)', 'Ein Bild je Kalenderwoche im gewählten Jahr, mit der aktiven Ebene.');
    var von = document.createElement('input'), bis = document.createElement('input');
    [von, bis].forEach(function(f) { f.type = 'number'; f.min = '1'; f.max = '53'; f.step = '1'; f.className = 'gw-export-zahl'; });
    von.setAttribute('aria-label', 'von Kalenderwoche'); bis.setAttribute('aria-label', 'bis Kalenderwoche');
    exportEl.vonEl = von; exportEl.bisEl = bis;
    zeile(a2, [exportFeld('span', '', 'KW'), von, exportFeld('span', '', 'bis KW'), bis], 'Wochen');
    zeile(a2, [exportWahl('checkbox', 'gw-ex-zs-obj', 'karte', 'Karte', true), exportWahl('checkbox', 'gw-ex-zs-obj', 'kurve', 'Kurve', false)], 'Objekte');
    zeile(a2, [exportWahl('radio', 'gw-ex-zs-fmt', 'png', 'PNG', true), exportWahl('radio', 'gw-ex-zs-fmt', 'svg', 'SVG', false)], 'Format');
    losKnopf(a2, 'ZIP erstellen', function(fortschritt) {
      var v = parseInt(von.value, 10), b = parseInt(bis.value, 10), maxW = maxWocheFuerJahr(selectedYear);
      if (!(v >= 1) || !(b >= v)) throw new Error('Bitte gültige Wochen wählen (von ≤ bis).');
      b = Math.min(b, maxW);
      var obj = gewaehlt('gw-ex-zs-obj');
      if (!obj.length) throw new Error('Bitte mindestens ein Objekt wählen.');
      return exportZeitserie(v, b, obj, gewaehlt('gw-ex-zs-fmt')[0] || 'png', fortschritt);
    });
    // 3. PDF-Bericht
    var a3 = abschnitt('PDF-Bericht', 'Karte, Legende und Kurve der aktuellen Ansicht auf einer A4-Seite.');
    losKnopf(a3, 'PDF-Bericht erstellen', function(fortschritt) { return exportPdfBericht(fortschritt); });
    // 4. PDF Wetter-Ebenen
    var a4 = abschnitt('PDF Wetter-Ebenen', 'Alle Wetter-Ebenen der gewählten Woche als kleine Karten (etwa A7) auf einer A4-Seite.');
    zeile(a4, [exportWahl('checkbox', 'gw-ex-ws', 'ja', 'Messstandorte einzeichnen', false)]);
    losKnopf(a4, 'PDF erstellen', function(fortschritt) { return exportPdfEbenen(gewaehlt('gw-ex-ws').length > 0, fortschritt); });
    exportStatusEl = exportFeld('p', 'gw-export-status');
    exportStatusEl.setAttribute('role', 'status');
    exportStatusEl.setAttribute('aria-live', 'polite');
    exportEl.appendChild(exportStatusEl);
    document.body.appendChild(exportEl);
  }
  function oeffneExport(opener) {
    baueExport();
    exportOpener = opener || null;
    var erste = 1;
    Object.keys(graswachstumBilder).forEach(function(k) {
      var t = k.split(' ');
      if (t[0] === String(selectedYear)) { var w = parseInt(t[1], 10); if (erste === 1 || w < erste) erste = w; }
    });
    exportEl.vonEl.value = String(erste);
    exportEl.bisEl.value = String(selectedWeek);
    exportStatus('');
    exportEl.classList.add('offen');
    document.documentElement.classList.add('gw-export-offen');
    setTimeout(function() { var f = exportEl.querySelector('.gw-doku-zu'); if (f) f.focus(); }, 0);
  }
  function schliesseExport() {
    if (!exportEl) return;
    exportEl.classList.remove('offen');
    document.documentElement.classList.remove('gw-export-offen');
    if (exportOpener && exportOpener.focus) exportOpener.focus();
  }
  function istExportOffen() { return !!(exportEl && exportEl.classList.contains('offen')); }

  // ---------- Direktlinks: Ansicht in der Adresse (#...) ----------
  // Die Adresse spiegelt laufend Jahr, Woche, Auswahl, Ebene, Schalter,
  // Kartenausschnitt und offenes Hilfethema; beim Oeffnen eines Links wird die
  // Ansicht wiederhergestellt. Standardansicht = Adresse ohne #. Nur
  // replaceState (kein neuer Verlaufseintrag je Klick). Alte Links
  // #hilfe bzw. #hilfe-<thema> und .../growth/help funktionieren weiter.
  var urlBereit = false, urlTimer = null, urlWirdGesetzt = false;
  function gruppenKurz(g) { return g === 0 ? 'alle' : String(groupLabels[g]).split(': ').pop(); }
  function gruppeAusKurz(k) {
    for (var g = 0; g < groupLabels.length; g++) if (gruppenKurz(g).toLowerCase() === String(k).toLowerCase()) return g;
    return -1;
  }
  function kartenGd() { return document.querySelector('#datenexplorer-growthmap .js-plotly-plot'); }
  function zustandAlsHash() {
    var p = [];
    var setze = function(k, v) { p.push(k + '=' + encodeURIComponent(v).replace(/%2C/g, ',').replace(/%20/g, '+')); };
    var wocheGeaendert = selectedYear !== neuestesJahr || selectedWeek !== daten.START_WOCHE;
    if (wocheGeaendert) { setze('jahr', selectedYear); setze('kw', selectedWeek); }
    var a = auswahlListen();
    if (!(selection.type === 'group' && selection.idx === 0)) {
      if (a.groups.length) setze('gruppe', a.groups.map(gruppenKurz).join(','));
      if (a.sites.length) setze('standort', a.sites.map(function(i) { return siteNames[i]; }).join(','));
    }
    if (hintergrundEbene !== 'keine') {
      setze('ebene', hintergrundEbene);
      if (istFensterEbene(hintergrundEbene) && meteoFenster !== meteoFensterStandard[hintergrundEbene]) setze('tage', meteoFenster);
    }
    var an = [], aus = [];
    if (!graswachstumOn) aus.push('wachstum');
    if (!afcOn) aus.push('dgv');
    if (smnStationenOn) an.push('stationen');
    if (!precipOn && jahreMitNiederschlag.indexOf(selectedYear) !== -1) aus.push('niederschlag');
    if (datumOn) an.push('datum');
    if (vorjahrOn && !vorjahrAutomatisch) an.push('vorjahr');
    if (!vorjahrOn && vorjahrAutomatisch === false && selection.type === 'site') aus.push('vorjahr');
    if (alleStandorteZeigen) an.push('alle-standorte');
    if (skalaAuto) an.push('skala');
    if (orteAnzeigen && eigeneOrte.length) an.push('orte');
    if (eigeneOrte.length && (orteAnzeigen || ortLinkAusHash)) setze('orte', eigeneOrte.map(ortToken).join(','));
    if (an.length) setze('an', an.join(','));
    if (aus.length) setze('aus', aus.join(','));
    var gd = kartenGd(), info = GWDatenexplorer.kartenInfo;
    if (gd && info && gd.layout.xaxis.range) {
      var xr = gd.layout.xaxis.range, yr = gd.layout.yaxis.range;
      if ((xr[1] - xr[0]) < (info.xMaxSchweiz - info.xMin) * 0.9) setze('karte', [xr[0], xr[1], yr[0], yr[1]].map(function(v) { return v.toFixed(3); }).join(','));
    }
    if (istDokuOffen() && dokuAktiv) setze('hilfe', dokuAktiv);
    if (berichtHash) p = berichtHash.concat(p.filter(function(x) { return !/^(jahr|kw|karte|ebene|an)=/.test(x); }), ['jahr=' + selectedYear, 'kw=' + selectedWeek]);   // geteilter Bericht bleibt beim Neuladen offen
    return p.length ? '#' + p.join('&') : '';
  }
  function schreibeUrl() {
    if (!urlBereit || urlWirdGesetzt) return;
    var hash = zustandAlsHash();
    if (hash === (window.location.hash || '')) return;
    var basis = window.location.pathname + window.location.search;
    try { history.replaceState(history.state, '', basis + hash); } catch (e) {}
  }
  function schreibeUrlBald() {
    if (!urlBereit) return;
    clearTimeout(urlTimer);
    urlTimer = setTimeout(schreibeUrl, 250);
  }
  GWDatenexplorer.schreibeUrlBald = schreibeUrlBald;
  function leseHash() {
    var h = (window.location.hash || '').replace(/^#/, ''), q = {};
    var alt = h.match(/^hilfe(?:-([a-z0-9-]+))?$/);
    if (alt) { q.hilfe = alt[1] || ''; return q; }
    h.split('&').forEach(function(teil) {
      if (!teil) return;
      var i = teil.indexOf('=');
      var k = i === -1 ? teil : teil.slice(0, i), v = i === -1 ? '' : teil.slice(i + 1);
      try { q[decodeURIComponent(k)] = decodeURIComponent(v.replace(/\+/g, ' ')); } catch (e) {}
    });
    return q;
  }
  function setzeSchalter(wrap, an) {
    var cb = wrap && wrap.checkbox ? wrap.checkbox : (wrap && wrap.querySelector ? wrap.querySelector('input[type=checkbox]') : null);
    if (!cb || cb.disabled || cb.checked === an) return;
    cb.checked = an;
    cb.dispatchEvent(new Event('change'));
  }
  function stelleZustandHer(q) {
    urlWirdGesetzt = true;
    try {
      var liste = function(k) { return q[k] ? q[k].split(',').map(function(x) { return x.trim(); }).filter(Boolean) : []; };
      if (q.jahr && alleJahre.indexOf(q.jahr) !== -1 && q.jahr !== selectedYear) {
        yearSelect.value = q.jahr;
        yearSelect.dispatchEvent(new Event('change'));
      }
      if (q.kw) {
        var w = parseInt(q.kw, 10);
        if (w >= 1 && w <= 53 && sliderInput) {
          sliderInput.value = String(Math.min(w, maxWocheFuerJahr(selectedYear)));
          sliderInput.dispatchEvent(new Event('input'));
        }
      }
      var gruppenIdx = liste('gruppe').map(gruppeAusKurz).filter(function(g) { return g !== -1; });
      var sitesIdx = liste('standort').map(function(n) { return siteNames.indexOf(n); }).filter(function(i) { return i !== -1; });
      if (gruppenIdx.length || sitesIdx.length) {
        setzeAuswahl(gruppenIdx, sitesIdx);
        aktualisiereAuswahlChips();
      }
      var an = liste('an'), aus = liste('aus');
      setzeSchalter(toggleGraswachstum, aus.indexOf('wachstum') === -1);
      setzeSchalter(toggleDgv, aus.indexOf('dgv') === -1);
      setzeSchalter(toggleStationen, an.indexOf('stationen') !== -1);
      if (aus.indexOf('niederschlag') !== -1) setzeSchalter(precipToggleWrap, false);
      if (an.indexOf('datum') !== -1) setzeSchalter(xAxisToggleWrap, false);
      if (an.indexOf('vorjahr') !== -1) setzeSchalter(vorjahrToggleWrap, true);
      if (aus.indexOf('vorjahr') !== -1) setzeSchalter(vorjahrToggleWrap, false);
      if (an.indexOf('alle-standorte') !== -1) alleStandorteZeigen = true;
      skalaAuto = an.indexOf('skala') !== -1;
      if (q.orte) {
        var ausLink = liste('orte').map(ortAusToken).filter(Boolean);
        if (ausLink.length) { ortLinkAusHash = true; setzeEigeneOrte(ausLink, true); }
      }
      if (an.indexOf('orte') !== -1 || q.orte) setzeOrteAnzeigen(an.indexOf('orte') !== -1);
      if (q.ber === '1' && eigeneOrte.length) setTimeout(function() { berichtAnzeigen(q); }, 1500);
      if (q.ebene) {
        var radio = document.querySelector('input[name=gw-layer][value="' + q.ebene.replace(/[^a-z0-9_]/gi, '') + '"]');
        if (radio && !radio.disabled) { radio.checked = true; radio.dispatchEvent(new Event('change')); }
        if (q.tage && istFensterEbene(q.ebene) && meteoFensterInput) {
          var stufe = meteoFensterStufen.indexOf(parseInt(q.tage, 10));
          if (stufe !== -1) { meteoFensterInput.value = String(stufe); meteoFensterInput.dispatchEvent(new Event('input')); }
        }
      }
      applyState();
      if (q.karte) {
        var k = q.karte.split(',').map(parseFloat);
        // erst nach den Groessenanpassungen beim Start (die setzen die ganze Schweiz)
        if (k.length === 4 && k.every(isFinite)) setTimeout(function() { var gd = kartenGd(); if (gd) Plotly.relayout(gd, { 'xaxis.range': [k[0], k[1]], 'yaxis.range': [k[2], k[3]] }); }, 1300);
      }
      if (q.hilfe !== undefined) {
        var thema = doku.some(function(e) { return e.id === q.hilfe; }) ? q.hilfe : null;
        setTimeout(function() { if (seiteEl && eingebettet) seiteEl.scrollIntoView({ block: 'start' }); oeffneDoku(thema); }, 400);
      }
    } finally {
      setTimeout(function() { urlWirdGesetzt = false; }, 500);
    }
  }
  setTimeout(function() {
    var q = leseHash();
    if (/\/(help|hilfe)\/?$/.test(window.location.pathname) && q.hilfe === undefined) q.hilfe = '';
    if (Object.keys(q).length) {
      stelleZustandHer(q);
      // Seitenanfang: bei einem Link mit Ansicht direkt zum Datenexplorer
      if (eingebettet && seiteEl && q.hilfe === undefined) setTimeout(function() { seiteEl.scrollIntoView({ block: 'start' }); }, 600);
    }
    setTimeout(function() { urlBereit = true; schreibeUrl(); }, 1900);
    var kgd = kartenGd();
    if (kgd && kgd.on) kgd.on('plotly_relayout', schreibeUrlBald);
  }, 200);
  window.addEventListener('hashchange', function() {
    if (urlWirdGesetzt) return;
    var q = leseHash();
    if (Object.keys(q).length) stelleZustandHer(q);
  });

  bestimmeAppModus();
  applyState();
  // Erneuter Aufruf leicht verzoegert: beim allerersten applyState() oben
  // (synchron waehrend des Bindens DIESES Widgets) sind die beiden
  // Karten-Widgets moeglicherweise noch nicht gebunden (siehe Kommentar in
  // applyMapState()), wodurch applyMapState() dort ins Leere laeuft. Nach
  // 100ms sind alle drei Widgets garantiert initialisiert.
  setTimeout(applyState, 100);
  setTimeout(function() { window.dispatchEvent(new Event('resize')); }, 150);
};
