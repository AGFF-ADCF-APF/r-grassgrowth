/*
 * Datenexplorer Graswachstum - Frontend.
 * Wird von 27_plot_datenexplorer.R als htmltools-Abhaengigkeit an beide
 * Plotly-Widgets gehaengt (lib/gw-datenexplorer-<version>/). R liefert nur
 * Daten: die onRender-Hooks rufen GWDatenexplorer.karte(el, x, d) bzw.
 * GWDatenexplorer.kurve(el, x, daten) mit einem Datenobjekt auf.
 */
window.GWDatenexplorer = window.GWDatenexplorer || {};

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
    vollX = voll.x; vollY = voll.y;
    Plotly.relayout(el, { width: breite, height: hoehe, 'xaxis.range': voll.x, 'yaxis.range': voll.y });
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
  function siteInAuswahl(siteIdx) {
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
      vis[m.traceIdx] = m.year === selectedYear && selection.type === 'group' && m.groupIdx === selection.idx;
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
    if (alleJahre.indexOf(vorjahr) === -1) return;

    var grau = 'rgba(140,140,140,0.7)';
    var traceIdxListe = [];
    siteGrowthMeta.forEach(function(m) {
      if (m.year !== vorjahr) return;
      if (!siteInAuswahl(m.siteIdx)) return;
      traceIdxListe.push(m.traceIdx);
      vorjahrStyledTraceIdx.push({ traceIdx: m.traceIdx, color: siteColors[m.siteIdx] });
    });
    groupGrowthMeta.forEach(function(m) {
      if (m.year !== vorjahr || selection.type !== 'group' || m.groupIdx !== selection.idx) return;
      traceIdxListe.push(m.traceIdx);
      vorjahrStyledTraceIdx.push({ traceIdx: m.traceIdx, color: 'black' });
    });
    if (traceIdxListe.length > 0) Plotly.restyle(el, { visible: true, 'line.color': grau, 'marker.color': grau }, traceIdxListe);

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

  function applyXAxis() {
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
      var inSel = selection.type === 'group' ? (oi >= 0 && siteVisible[selection.idx][oi]) : (selection.type === 'site' && siteNames[selection.idx] === o);
      return inSel ? 4 : 1;
    });
  }

  function applyMapState() {
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
    if (growthMapGd) Plotly.restyle(growthMapGd, { visible: vis });
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
      growthMapGd.addEventListener('touchstart', verarbeiteKartenZeiger, { passive: true });
      growthMapGd.addEventListener('touchmove', verarbeiteKartenZeiger, { passive: true });
      growthMapGd.addEventListener('mouseleave', versteckeWertAnzeige);
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
    hilfe: ['M12 12m-9 0a9 9 0 1 0 18 0a9 9 0 1 0 -18 0', 'M12 17l0 .01', 'M12 13.5a1.5 1.5 0 0 1 1 -1.5a2.6 2.6 0 1 0 -3 -4'],
    runter: ['M6 9l6 6l6 -6'],
    zurueck: ['M5 12l14 0', 'M5 12l6 6', 'M5 12l6 -6'],
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
    stichworte: 'zoom vergroessern verschieben standort antippen klicken plz ort suche fadenkreuz wert cursor standortblatt',
    text: ['Vergrössern mit dem Mausrad oder mit zwei Fingern, verschieben durch Ziehen. Über die ganze Schweiz hinaus lässt sich nicht verkleinern; das Haus-Symbol der Werkzeugleiste zeigt wieder die ganze Schweiz.',
      'Ein Klick auf einen Standort öffnet das Standortblatt mit dem letzten Messwert, dem DGV, dem Zielbereich der Woche und einer kleinen Saisonkurve. Von dort führt «Ganze Graswachstumskurve anzeigen» zur grossen Kurve dieses Standorts.',
      'Das Feld «PLZ oder Ort suchen» bei den Ebenen setzt ein Fadenkreuz auf den Ort. Ist eine Hintergrund-Ebene aktiv, zeigt der Datenexplorer deren Wert an dieser Stelle (auf dem Desktop auch laufend unter dem Mauszeiger).'] });
  dokuEintrag({ id: 'zeitleiste', gruppe: 'Erste Schritte', titel: 'Zeitleiste und Abspielen',
    alias: ['Zeitleiste'], stichworte: 'kalenderwoche kw woche schieberegler heute pfeile play abspielen animation zukunft',
    text: ['Die Zeitleiste wählt die Kalenderwoche für Karte und Kurve: mit dem Schieberegler, den Pfeilen (eine Woche zurück oder vor) oder «Heute» für die aktuelle Woche. Der grau hinterlegte Teil liegt in der Zukunft.',
      'Der Abspielen-Knopf zeigt Woche für Woche bis zur letzten verfügbaren Woche. Steht der Regler schon am Ende, beginnt er bei der ersten Woche mit Messungen. Jede andere Bedienung der Zeitleiste hält das Abspielen an.',
      'In der Wachstumskurve wählt auch ein Klick auf die Achse unter der Grafik die Woche.'] });
  dokuEintrag({ id: 'kurve', gruppe: 'Erste Schritte', titel: 'Wachstumskurve',
    alias: ['Wachstumskurve', 'Graswachstumskurve'], stichworte: 'kurve linie mittel durchschnitt mittelland vorjahr niederschlag balken gruppe region hoehenlage jahr legende',
    legende: function() { return dokuLinien([['Standort (je eigene Farbe)', '#1D9E75', 'solid'], ['Mittleres Wachstum der Auswahl', 'black', 'dashed'], ['Durchschnitt Mittelland, langjährig', 'red', 'dotted'], ['Vorjahr zum Vergleich', 'rgba(140,140,140,0.9)', 'solid'], ['Niederschlag pro Woche', 'steelblue', 'balken']]); },
    text: ['Die Kurve zeigt das gemessene Graswachstum in kg TS/ha/Tag über die Saison. Oben links wählen Sie eine Gruppe (alle Standorte, eine Region West/Mitte/Ost oder eine Höhenlage) oder tippen einen Standort ins Suchfeld, daneben das Jahr.',
      'Rechts steht die Legende. Ein Klick auf einen Standort zeigt nur noch diesen. Die Schalter darüber blenden Niederschlag (Balken, mm pro Woche) und die Kurven des Vorjahres ein, oder stellen die Achse von Kalenderwochen auf Datum um.',
      'Beim Start erscheinen nur Standorte, die regelmässig messen (siehe dort). Die übrigen lassen sich in der Legende dazuschalten.'] });
  dokuEintrag({ id: 'regelmaessig', gruppe: 'Erste Schritte', titel: 'Regelmässig messende Standorte',
    stichworte: 'startansicht filter weitere selten gemessen standorte acht wochen',
    text: ['Damit die Kurve beim Start übersichtlich bleibt, zeigt sie in der Gruppenansicht nur Standorte mit mindestens 8 Kalenderwochen mit Messung im gewählten Jahr. Das Mittel der Gruppe (schwarz gestrichelt) wird weiterhin aus allen Standorten berechnet.',
      'In der Legende blendet «+ … weitere (selten gemessen)» die übrigen Standorte mit Daten im Jahr ein, «Nur regelmässig messende Standorte» blendet sie wieder aus.'] });
  dokuEintrag({ id: 'ansicht', gruppe: 'Erste Schritte', titel: 'Ansicht anpassen (Desktop)',
    alias: ['Grösse von Karte und Kurve verschieben'], stichworte: 'griff ziehen schieben pfeile gross klein vollbild ebenen einklappen layout tastatur',
    text: ['Zwischen Karte und Kurve liegt ein Griff: Ziehen verschiebt die Grenze, der Pfeil nach oben macht die Kurve ganz gross, der Pfeil nach unten die Karte. Ein Doppelklick auf den Griff stellt die Standardgrösse wieder her. Mit der Tastatur: Griff anwählen, dann Pfeiltasten, Pos1 oder Ende.',
      'Die Icon-Leiste links blendet die Ebenen und die Kurve ein und aus. Auf der Website vergrössert «Vollbild» den Datenexplorer auf den ganzen Bildschirm; Escape oder der Knopf beenden das Vollbild.'] });
  dokuEintrag({ id: 'handy', gruppe: 'Erste Schritte', titel: 'Auf dem Handy',
    stichworte: 'mobile smartphone teaser detail querformat quer drehen zurueck geste regionen',
    legende: function() { return dokuLinien([['Region West', '#378ADD', 'solid'], ['Region Mitte', '#1D9E75', 'solid'], ['Region Ost', '#BA7517', 'solid'], ['Langjähriges Mittel', '#E24B4A', 'dotted']]); },
    text: ['Unter der Zeitleiste zeigt eine flache Kurve das Wachstum der drei Regionen, über drei Wochen geglättet, und das langjährige Mittel. Antippen, «Alle Kurven» oder «Standort wählen …» öffnet die grosse Kurve als eigene Ansicht; die Zurück-Geste des Handys schliesst sie wieder.',
      'Im Hochformat blinkt «Quer ansehen». Wo das Handy es erlaubt, dreht der Knopf die Ansicht; sonst bitte das Handy quer halten. Bei eingeschalteter automatischer Drehung erscheint die Grafik im Querformat über den ganzen Bildschirm.'] });

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
  dokuEintrag({ id: 'gdd', gruppe: 'Wetter-Ebenen', titel: 'Wachstumsgradtage',
    alias: ['Wachstumsgradtage'], stichworte: 'gdd gradtage waermesumme temperatursumme vegetation',
    legende: dokuFarbskala('gdd'), quelle: dokuQuelle('gdd'),
    text: ['Aufsummierte Wärme seit Jahresbeginn: an jedem Tag die Tagesmitteltemperatur minus 5 °C, sofern positiv. Eine verbreitete Faustregel für die pflanzenverfügbare Wärme seit Vegetationsbeginn; höhere Werte bedeuten mehr angesammelte Wachstumsbedingungen.'] });

  dokuEintrag({ id: 'boden', gruppe: 'Berechnete Ebenen', titel: 'Bodenwasserbilanz',
    alias: ['Bodenwasserbilanz'], stichworte: 'bodenwasser wasser eimer bucket trocken trockenheit duerre fuellstand speicher',
    legende: dokuFarbskala('boden'), quelle: dokuQuelle('boden'),
    text: ['Der Boden wird vereinfacht als Eimer betrachtet: Regen füllt ihn, Verdunstung leert ihn, ist er voll, läuft der Überschuss ab. Ein feuchter Boden verdunstet mehr als ein bereits trockener.',
      'Der Wert zeigt den Füllstand am Stichtag: 100 mm = gut mit Wasser versorgt, 0 mm = ausgetrocknet. Die Ebene ist selbst berechnet und keine Messung.'] });
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
    if (evt.key !== 'Escape' || !istVollbild() || istDokuOffen()) return;
    var offen = blattEl.style.display !== 'none' || document.body.classList.contains('gw-ebenen-offen') ||
      (seiteEl.classList.contains('gw-app-schmal') && !seiteEl.classList.contains('gw-ebenen-zu'));
    if (!offen) setzeVollbild(false);
  }, true);

  // Mobile-Elemente rund um die Karte (auf dem Desktop per CSS ausgeblendet)
  var kartenzeileEl = document.getElementById('gw-kartenzeile');
  var mobilKopfUnterEl = null, kartenleisteLegendeEl = null, kartenleisteWertEl = null, kurveKnopfEl = null;
  var teaserEl = null, teaserTitelEl = null, teaserJahr = null, teaserKoerper = null, teaserKopfEl = null;
  if (kartenzeileEl) {
    var mobilKopf = document.createElement('div');
    mobilKopf.className = 'gw-mobil-only gw-mobil-kopf';
    var mkTitel = document.createElement('div'); mkTitel.className = 'gw-mobil-kopf-titel'; mkTitel.textContent = 'Graswachstum';
    mobilKopfUnterEl = document.createElement('div'); mobilKopfUnterEl.className = 'gw-mobil-kopf-unter';
    mobilKopf.appendChild(mkTitel); mobilKopf.appendChild(mobilKopfUnterEl);
    kartenzeileEl.parentNode.insertBefore(mobilKopf, kartenzeileEl);
    var kartenleiste = document.createElement('div');
    kartenleiste.className = 'gw-mobil-only gw-kartenleiste';
    kartenleisteLegendeEl = document.createElement('div');
    kartenleisteLegendeEl.className = 'gw-kartenleiste-legende';
    kartenleisteLegendeEl.style.display = 'none';
    kartenleisteWertEl = document.createElement('div');
    kartenleisteWertEl.className = 'gw-kartenleiste-wert';
    kartenleisteWertEl.style.display = 'none';
    kartenleiste.appendChild(kartenleisteLegendeEl); kartenleiste.appendChild(kartenleisteWertEl);
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
    if (kartenleisteWertEl) kartenleisteWertEl.textContent = text.replace('Wert am Cursor: ', '').replace('–', 'Auf die Karte tippen für den Wert');
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

  var options = groupLabels.map(function(label, i) { return { type: 'group', idx: i, label: label }; });
  var siteOptions = siteNames.map(function(name, i) { return { type: 'site', idx: i, label: name }; });

  var controls = document.createElement('div');
  controls.className = 'gw-controls';

  var comboWrap = document.createElement('div');
  comboWrap.className = 'gw-combo';
  var input = document.createElement('input');
  input.type = 'text';
  input.placeholder = 'Gruppe oder Standort…';
  input.value = groupLabels[0];
  var list = document.createElement('div');
  list.className = 'gw-combo-list';
  list.style.display = 'none';

  function renderList(query) {
    query = (query || '').toLowerCase();
    list.innerHTML = '';
    var matchedGroups = options.filter(function(o) { return o.label.toLowerCase().indexOf(query) !== -1; });
    var matchedSites = siteOptions.filter(function(o) { return o.label.toLowerCase().indexOf(query) !== -1; });
    matchedGroups.forEach(function(o) { list.appendChild(makeItem(o)); });
    if (matchedSites.length > 0) {
      var sep = document.createElement('div');
      sep.className = 'gw-combo-sep';
      sep.textContent = 'Einzelstandorte';
      list.appendChild(sep);
      matchedSites.forEach(function(o) { list.appendChild(makeItem(o)); });
    }
    list.style.display = (matchedGroups.length + matchedSites.length > 0) ? 'block' : 'none';
  }

  function makeItem(o) {
    var item = document.createElement('div');
    item.className = 'gw-combo-item';
    item.textContent = o.label;
    item.addEventListener('mousedown', function(e) {
      e.preventDefault();
      selection = { type: o.type, idx: o.idx };
      if (o.type === 'site') aktiviereVorjahrFuerEinzelstandort();
      input.value = o.label;
      list.style.display = 'none';
      applyState();
    });
    return item;
  }

  input.addEventListener('focus', function() { renderList(''); });
  input.addEventListener('input', function() { renderList(input.value); });
  input.addEventListener('blur', function() { setTimeout(function() { list.style.display = 'none'; }, 150); });
  comboWrap.appendChild(input);
  comboWrap.appendChild(list);

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
      vorherigeSelection = { type: selection.type, idx: selection.idx };
      selection = { type: 'site', idx: siteIdx };
      aktiviereVorjahrFuerEinzelstandort();
    }
    input.value = selection.type === 'group' ? groupLabels[selection.idx] : siteNames[selection.idx];
    applyState();
  }

  // Kartenklick: Standort waehlen OHNE Umschalten (ein zweiter Klick auf
  // denselben Standort oeffnet nur wieder das Blatt).
  function waehleSite(siteIdx) {
    if (selection.type === 'site' && selection.idx === siteIdx) return;
    vorherigeSelection = { type: selection.type, idx: selection.idx };
    selection = { type: 'site', idx: siteIdx };
    aktiviereVorjahrFuerEinzelstandort();
    input.value = siteNames[siteIdx];
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
    if (teaserTitelEl) teaserTitelEl.textContent = 'Wachstumskurve ' + selectedYear;
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
    if (detailTitelEl) detailTitelEl.textContent = (input.value || 'Alle Standorte') + ' · ' + selectedYear;
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
  function aktiviereVorjahrFuerEinzelstandort() {
    if (vorjahrOn) return;
    vorjahrOn = true;
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
  var radioSonnenschein = null, radioEt0 = null, radioGdd = null;
  var radioWachstumspotenzialRate = null, radioWachstumspotenzialKum = null;
  var experimentellerModus = new URLSearchParams(window.location.search).has('experimentell');
  var radioJeEbene = {};
  function aktualisiereLayerVerfuegbarkeit() {
    if (radioNiederschlag) radioNiederschlag.disabled = !ebeneHatJahr('niederschlag', selectedYear);
    if (radioBoden) radioBoden.disabled = !ebeneHatJahr('boden', selectedYear);
    if (radioTemperatur) radioTemperatur.disabled = !ebeneHatJahr('temperatur', selectedYear);
    if (radioBodentemperatur) radioBodentemperatur.disabled = !ebeneHatJahr('bodentemperatur', selectedYear);
    if (radioSonnenschein) radioSonnenschein.disabled = !ebeneHatJahr('sonnenschein', selectedYear);
    if (radioEt0) radioEt0.disabled = !ebeneHatJahr('et0', selectedYear);
    if (radioGdd) radioGdd.disabled = !ebeneHatJahr('gdd', selectedYear);
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
    var unterzeile = 'KW ' + selectedWeek + ' ' + selectedYear + ' · Grafik vom ' + grafikDatum;
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
    koordinatenEl.textContent = 'Koordinaten: –';
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
      if (!ortschaftEl || meineId !== ortschaftAnfrageId) return;
      var treffer = daten && daten.results && daten.results[0];
      ortschaftEl.textContent = treffer ?
        ('Ort: ' + treffer.attributes.plz + ' ' + treffer.attributes.langtext) :
        'Ort: ausserhalb der Schweiz';
    }).catch(function() {
      if (ortschaftEl && meineId === ortschaftAnfrageId) ortschaftEl.textContent = 'Ort: nicht abrufbar (offline?)';
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
  function zeigeWertAmPunkt(lon, lat) {
    if (!wertAnzeigeEl) return;
    var lv95 = wgs84ZuLv95(lon, lat);
    if (koordinatenEl) {
      koordinatenEl.textContent = 'Koordinaten: ' + Math.round(lv95.e) + ' / ' + Math.round(lv95.n) + ' (LV95)';
    }
    if (ortschaftEl) ortschaftEl.innerHTML = 'Ort: ' + LADE_PUNKTE_HTML;
    ortschaftAnfrageId++;
    clearTimeout(ortschaftAbfrageTimer);
    ortschaftAbfrageTimer = setTimeout(function() {
      var meineId = ++ortschaftAnfrageId;
      sucheOrtschaftPlz(lv95.e, lv95.n, meineId);
    }, 350);
    var gitterJeWoche = aktivesWerteGitter();
    var info = layerLegenden[hintergrundEbene];
    var gitter = gitterJeWoche ? gitterJeWoche[selectedYear + ' ' + selectedWeek] : null;
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
    if (koordinatenEl) koordinatenEl.textContent = 'Koordinaten: –';
    if (ortschaftEl) ortschaftEl.textContent = 'Ort: –';
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
      zeilen.push('Koordinaten: ' + Math.round(lv95.e) + ' / ' + Math.round(lv95.n) + ' (LV95)');
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
    macheLayerToggle('Graswachstum (kg TS/ha/Tag)', true, function(checked) { graswachstumOn = checked; applyState(); },
      'Die Zahl im Kreis zeigt das zuletzt gemessene Graswachstum in kg TS/ha/Tag (Trockensubstanz-Zuwachs pro Hektare und Tag). Die Graufaerbung des Kreises zeigt, wie lange die Messung zurueckliegt: weiss = frisch gemessen (0 Tage), dunkelgrau = bis zu 14 Tage alt. Standorte ohne Messung in den letzten 14 Tagen werden nicht mehr angezeigt.');
    macheLayerToggle('DGV (kg TS/ha)', true, function(checked) { afcOn = checked; applyState(); },
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

    macheLayerToggle('MeteoSchweiz-Stationen', false, function(checked) { smnStationenOn = checked; aktualisiereSmnStationen(); },
      'Zeigt die oeffentlichen MeteoSchweiz-Automatikstationen (SwissMetNet) mit ihren aktuellsten Tageswerten (Lufttemperatur, Bodentemperatur, Niederschlag, Globalstrahlung, Sonnenscheindauer) als Diamant-Symbole. Reine Wetter-Referenzstationen, unabhaengig von der gewaehlten Kalenderwoche und NICHT Teil der AGFF-Grasmessungen. Bodentemperatur wird nur an einem Teil der rund 150 Stationen gemessen - dort steht im Tooltip entsprechend keine Daten.');

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
      'Der Boden wird vereinfacht wie ein Eimer betrachtet: Regen fuellt ihn, Verdunstung leert ihn. Ist der Eimer voll, laeuft der Ueberschuss ungenutzt ab. Wie viel taeglich verdunstet, wird aus den Temperaturen geschaetzt - ein feuchter Boden verdunstet mehr als ein bereits trockener. Der Wert zeigt den aktuellen Fuellstand: 100 mm = Boden gut mit Wasser versorgt, 0 mm = ausgetrocknet.',
      'gw-layer-vor-boden');
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
    layerPanel.appendChild(layerLegendeBox);
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
  var vorjahrToggleWrap = makeToggle('Vorjahresdaten', false, function(checked) { vorjahrOn = checked; applyState(); });
  vorjahrToggleWrap.title = 'Kurve(n) des Vorjahres zum Vergleich in Grau einblenden';

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
    kurveGriffInfo.textContent = (input.value || 'Alle Standorte') + ' · ' + selectedYear;
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
    var text = (input.value || 'Alle Standorte') + ' · ' + selectedYear;
    if (selection.type === 'group' && !alleStandorteZeigen && regelListe().length > 0) {
      var n = 0;
      for (var i = 0; i < siteNames.length; i++) if (siteInAuswahl(i)) n++;
      text += ' · ' + n + ' regelmässig messende Standorte';
    }
    kurveGriffInfo.textContent = text;
  }
  // Ziehen (Maus, Finger, Stift); ohne Bewegung = Klick: aus einem Extrem
  // zurueck zur Standardhoehe. Doppelklick: Standardhoehe.
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
      if (!bewegt) {
        if (kurveHoehe < 1 || kurveHoehe >= max - 1) setzeKurvenHoehe(kurveStandard());
        return;
      }
      // In die Extreme einrasten
      if (kurveHoehe < 70) setzeKurvenHoehe(0);
      else if (kurveHoehe > max - 70) setzeKurvenHoehe(max);
      else setzeKurvenHoehe(kurveHoehe);
    }
    kurveGriff.addEventListener('pointerup', ende);
    kurveGriff.addEventListener('pointercancel', ende);
    kurveGriff.addEventListener('dblclick', function() { setzeKurvenHoehe(kurveStandard()); });
    kurveGriff.addEventListener('keydown', function(evt) {
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
    Plotly.relayout(el, aenderung).then(function() { Plotly.Plots.resize(el); });
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
