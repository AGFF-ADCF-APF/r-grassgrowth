/*
 * Startansicht des Datenexplorers (Grav-Plugin datenexplorer, Konfiguration
 * «kurzansicht»): schlichte Karte und Kurve des neuesten Jahres als SVG, ohne
 * Plotly. Daten aus outputs/Datenexplorer_kurz.json (27_plot_datenexplorer.R).
 * Der volle Datenexplorer laedt erst auf Klick (opt.voll(hash)), beim Zeigen
 * auf einen Knopf beginnt das Laden schon im Hintergrund (opt.vorladen()).
 *
 *   GWKurzansicht.zeige(el, daten, { basis, sprache, voll, vorladen })
 */
(function () {
  'use strict';

  var TEXTE = {
    de: {
      titel: 'Graswachstum', kw: 'KW', letzte: 'letzte Messung', aktualisiert: 'aktualisiert',
      erkunden: 'Interaktiv erkunden', einheit: 'kg TS/ha/Tag',
      zahl: 'Zahl = Graswachstum in kg TS/ha/Tag', grau: 'grauer Kreis = Messung einige Tage alt',
      frage: '? = diese Woche keine Messung', gemessen: 'gemessen', vor: 'vor', tagen: 'Tagen', heute: 'heute', gestern: 'gestern',
      keine: 'diese Woche keine Messung', zuletzt: 'zuletzt', zeigen: 'Klicken: im Datenexplorer zeigen', antippen: 'Nochmals antippen: im Datenexplorer zeigen',
      mittel: 'Mittel aller Standorte', referenz: 'Durchschnitt Mittelland (langjährig)', woche: 'Woche ab',
      mehr_titel: 'Mehr entdecken im Datenexplorer',
      mehr_text: 'Jede Kalenderwoche seit 2024, alle Standorte, Wetter- und Bodenkarten, Vergleiche mit anderen Jahren und Berichte für Ihren Ort.',
      laden: 'lädt beim ersten Öffnen rund 3 MB', wird_geladen: 'Datenexplorer wird geladen …',
      wege: [
        ['', 'Alle Wochen und Standorte', 'Karte und Kurve Woche für Woche'],
        ['an=vorjahr', 'Mit anderen Jahren vergleichen', 'Liegt das Wachstum über oder unter dem Vorjahr?'],
        ['ebene=niederschlag_kum', 'Niederschlag seit 1. Januar', 'Karte der ganzen Schweiz'],
        ['ebene=boden', 'Bodenwasser', 'Wie viel Wasser hat der Boden noch?']
      ],
      liste: 'Werte der Karte als Liste', karte_label: 'Karte der Schweiz mit dem Graswachstum an den Messstandorten',
      kurve_label: 'Graswachstumskurve', monate: ['Jan', 'Feb', 'Mär', 'Apr', 'Mai', 'Jun', 'Jul', 'Aug', 'Sep', 'Okt', 'Nov', 'Dez']
    },
    fr: {
      titel: 'Croissance de l’herbe', kw: 'sem.', letzte: 'dernière mesure', aktualisiert: 'mis à jour',
      erkunden: 'Explorer en interactif', einheit: 'kg MS/ha/jour',
      zahl: 'Chiffre = croissance en kg MS/ha/jour', grau: 'cercle gris = mesure de quelques jours',
      frage: '? = pas de mesure cette semaine', gemessen: 'mesuré le', vor: 'il y a', tagen: 'jours', heute: 'aujourd’hui', gestern: 'hier',
      keine: 'pas de mesure cette semaine', zuletzt: 'dernière', zeigen: 'Cliquer : afficher dans l’explorateur', antippen: 'Toucher encore : afficher dans l’explorateur',
      mittel: 'Moyenne de tous les sites', referenz: 'Moyenne Plateau (pluriannuelle)', woche: 'semaine du',
      mehr_titel: 'Découvrir plus dans l’explorateur de données',
      mehr_text: 'Chaque semaine depuis 2024, tous les sites, cartes météo et sol, comparaisons avec d’autres années et rapports pour votre lieu.',
      laden: 'environ 3 Mo au premier chargement', wird_geladen: 'Chargement de l’explorateur …',
      wege: [
        ['', 'Toutes les semaines et sites', 'Carte et courbe semaine par semaine'],
        ['an=vorjahr', 'Comparer avec d’autres années', 'Croissance au-dessus ou au-dessous de l’an passé ?'],
        ['ebene=niederschlag_kum', 'Précipitations depuis le 1er janvier', 'Carte de toute la Suisse'],
        ['ebene=boden', 'Eau du sol', 'Combien d’eau reste-t-il dans le sol ?']
      ],
      liste: 'Valeurs de la carte en liste', karte_label: 'Carte de la Suisse avec la croissance de l’herbe aux sites de mesure',
      kurve_label: 'Courbe de croissance', monate: ['jan', 'fév', 'mar', 'avr', 'mai', 'juin', 'juil', 'août', 'sep', 'oct', 'nov', 'déc']
    }
  };

  var SVGNS = 'http://www.w3.org/2000/svg';
  function s(tag, attr, eltern) {
    var e = document.createElementNS(SVGNS, tag);
    for (var k in attr) if (attr[k] !== undefined && attr[k] !== null) e.setAttribute(k, attr[k]);
    if (eltern) eltern.appendChild(e);
    return e;
  }
  function h(tag, klasse, text, eltern) {
    var e = document.createElement(tag);
    if (klasse) e.className = klasse;
    if (text !== undefined && text !== null) e.textContent = text;
    if (eltern) eltern.appendChild(e);
    return e;
  }
  // Grau je Alter der Messung wie im Datenexplorer (weiss bis #757575 nach 14 Tagen)
  function grau(tage) {
    var t = Math.max(0, Math.min(1, (tage || 0) / 14)), v = Math.round(255 - t * (255 - 117));
    return 'rgb(' + v + ',' + v + ',' + v + ')';
  }

  function zeige(el, d, opt) {
    opt = opt || {};
    var T = TEXTE[opt.sprache] || TEXTE.de;
    var basis = opt.basis || '';
    var voll = opt.voll || function () {};
    var vorladen = opt.vorladen || function () {};
    var farbe = {}, namenEls = [], hervorOrt = null;
    d.kurven.forEach(function (k) { farbe[k.ort] = k.farbe; });

    el.innerHTML = '';
    var wurzel = h('div', 'gwk', null, el);

    // ---------- Kopf ----------
    var kopf = h('div', 'gwk-kopf', null, wurzel);
    var kt = h('div', 'gwk-kopftext', null, kopf);
    var titel = h('p', 'gwk-titel', null, kt);
    titel.appendChild(document.createTextNode(T.titel + ' '));
    h('b', null, T.kw + ' ' + d.kw, titel);
    titel.appendChild(document.createTextNode(' · ' + d.von.slice(0, 6) + '–' + d.bis));
    h('p', 'gwk-unter', T.letzte + ' ' + d.letzte_messung + ' · ' + T.aktualisiert + ' ' + d.erstellt, kt);

    var knoepfe = [];
    function vollKnopf(knopf, hash) {
      knoepfe.push(knopf);
      knopf.addEventListener('pointerenter', vorladen);
      knopf.addEventListener('focus', vorladen);
      knopf.addEventListener('click', function () { oeffnen(hash, knopf); });
    }
    function oeffnen(hash, knopf) {
      if (wurzel.classList.contains('gwk--laedt')) return;
      wurzel.classList.add('gwk--laedt');
      if (knopf) knopf.setAttribute('aria-busy', 'true');
      status.hidden = false;
      Promise.resolve(voll(hash ? '#' + hash : '')).catch(function () {}).then(function () {
        wurzel.classList.remove('gwk--laedt');
        status.hidden = true;
        if (knopf) knopf.removeAttribute('aria-busy');
      });
    }
    var haupt = h('button', 'gwk-knopf gwk-knopf--haupt', null, kopf);
    haupt.type = 'button';
    haupt.innerHTML = '<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M3 17l5-5 4 4 8-8M15 8h5v5" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/></svg>';
    h('span', null, T.erkunden, haupt);
    vollKnopf(haupt, '');
    var status = h('p', 'gwk-status', T.wird_geladen, wurzel);
    status.setAttribute('role', 'status');
    status.hidden = true;

    // ---------- Karte ----------
    var k = d.karte, lon0 = k.lon[0], lonB = k.lon[1] - k.lon[0], lat1 = k.lat[1], latB = k.lat[1] - k.lat[0];
    var W = 1000, H = Math.round(W * latB * k.verhaeltnis / lonB);
    var px = function (lon) { return (lon - lon0) / lonB * W; };
    var py = function (lat) { return (lat1 - lat) / latB * H; };
    var karteFig = h('figure', 'gwk-karte', null, wurzel);
    var karte = s('svg', { viewBox: '0 0 ' + W + ' ' + H, role: 'group', 'aria-label': T.karte_label }, karteFig);
    s('image', { href: basis + k.bild, x: 0, y: 0, width: W, height: H, preserveAspectRatio: 'none' }, karte);
    var punkteG = s('g', {}, karte);
    var tipp = h('div', 'gwk-tipp', null, karteFig);
    tipp.hidden = true;

    // «?» zuerst zeichnen, Kreise mit Zahl darueber
    var punkte = d.punkte.slice().sort(function (a, b) { return (b.wert === null) - (a.wert === null); });
    var punktEls = [];
    punkte.forEach(function (p) {
      var g = s('g', { class: 'gwk-punkt' + (p.wert === null ? ' gwk-punkt--frage' : ''), tabindex: 0, role: 'button' }, punkteG);
      var aria = p.ort + ': ' + (p.wert === null ? T.keine : p.wert + ' ' + T.einheit);
      g.setAttribute('aria-label', aria);
      var kreis = s('circle', { cx: px(p.lon), cy: py(p.lat), fill: p.wert === null ? '#fff' : grau(p.tage) }, g);
      var text = s('text', { x: px(p.lon), y: py(p.lat), 'text-anchor': 'middle', 'dominant-baseline': 'central' }, g);
      text.textContent = p.wert === null ? '?' : String(p.wert);
      punktEls.push({ p: p, g: g, kreis: kreis, text: text });
    });
    function punktGroesse() {
      var breite = karte.getBoundingClientRect().width || 600;
      var r = Math.max(10, Math.min(17, breite / 45)) * W / breite;
      punktEls.forEach(function (x) {
        x.kreis.setAttribute('r', r);
        x.kreis.setAttribute('stroke-width', r * 0.12);
        x.text.setAttribute('font-size', r * (x.p.wert !== null && x.p.wert >= 100 ? 0.85 : 1.0));
      });
    }

    var tippOrt = null, letzterZeiger = 'mouse';
    function zeigeTipp(x) {
      var p = x.p, z = [];
      tipp.innerHTML = '';
      h('b', null, p.ort, tipp);
      if (p.wert !== null) {
        z.push(p.wert + ' ' + T.einheit);
        // Alter ab heute (p.tage zaehlt ab Ende der Kartenwoche und bestimmt nur das Grau)
        var dt = p.letzte_datum.split('.'), n = Math.max(0, Math.round((new Date().setHours(0, 0, 0, 0) - new Date(+dt[2], dt[1] - 1, +dt[0])) / 86400000));
        var alt = n === 0 ? T.heute : n === 1 ? T.gestern : T.vor + ' ' + n + ' ' + T.tagen;
        z.push(T.gemessen + ' ' + p.letzte_datum + ' (' + alt + ')');
        if (p.afc !== null) z.push('AFC ' + p.afc + ' kg TS/ha');
      } else {
        z.push(T.keine);
        if (p.letzte_datum) z.push(T.zuletzt + ' ' + p.letzte_datum + ': ' + p.letzte_wert + ' ' + T.einheit);
      }
      z.push(letzterZeiger === 'touch' ? T.antippen : T.zeigen);
      z.forEach(function (t, i) { h('span', i === z.length - 1 ? 'gwk-tipp-hinweis' : null, t, tipp); });
      tipp.hidden = false;
      var fr = karteFig.getBoundingClientRect(), kr = x.kreis.getBoundingClientRect();
      var links = kr.left - fr.left + kr.width / 2, oben = kr.top - fr.top;
      tipp.style.left = Math.max(4, Math.min(fr.width - tipp.offsetWidth - 4, links - tipp.offsetWidth / 2)) + 'px';
      tipp.style.top = (oben - tipp.offsetHeight - 8 < 0 ? oben + kr.height + 8 : oben - tipp.offsetHeight - 8) + 'px';
      tippOrt = p.ort;
      hervorheben(p.ort);
    }
    function versteckeTipp() { tipp.hidden = true; tippOrt = null; hervorheben(null); }
    punktEls.forEach(function (x) {
      x.g.addEventListener('pointerdown', function (e) { letzterZeiger = e.pointerType; });
      x.g.addEventListener('pointerenter', function (e) { if (e.pointerType === 'mouse') { letzterZeiger = 'mouse'; zeigeTipp(x); vorladen(); } });
      x.g.addEventListener('pointerleave', function (e) { if (e.pointerType === 'mouse') versteckeTipp(); });
      x.g.addEventListener('focus', function () { zeigeTipp(x); });
      x.g.addEventListener('blur', versteckeTipp);
      x.g.addEventListener('click', function () {
        // Touch: erster Tipp zeigt die Angaben, der zweite oeffnet den Datenexplorer
        if (letzterZeiger !== 'mouse' && tippOrt !== x.p.ort) { zeigeTipp(x); vorladen(); return; }
        oeffnen('standort=' + encodeURIComponent(x.p.ort));
      });
      x.g.addEventListener('keydown', function (e) {
        if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); oeffnen('standort=' + encodeURIComponent(x.p.ort)); }
        if (e.key === 'Escape') versteckeTipp();
      });
    });
    document.addEventListener('pointerdown', function (e) { if (!tipp.hidden && !karteFig.contains(e.target)) versteckeTipp(); });

    // Legende der Karte
    var leg = h('p', 'gwk-legende', null, wurzel);
    [['42', '#fff', T.zahl], ['', grau(10), T.grau], ['?', '#fff', T.frage]].forEach(function (e, i) {
      var sp = h('span', null, null, leg);
      var mini = s('svg', { viewBox: '0 0 20 20', width: 18, height: 18, 'aria-hidden': 'true', class: i === 2 ? 'gwk-punkt--frage' : '' }, sp);
      s('circle', { cx: 10, cy: 10, r: 8.5, fill: e[1], stroke: i === 2 ? '#8a8a8a' : '#000', 'stroke-width': 1.3, 'stroke-dasharray': i === 2 ? '3 2' : null }, mini);
      var t = s('text', { x: 10, y: 10.5, 'text-anchor': 'middle', 'dominant-baseline': 'central', 'font-size': 9, 'font-weight': 700, fill: i === 2 ? '#666' : '#000' }, mini);
      t.textContent = e[0];
      sp.appendChild(document.createTextNode(e[2]));
    });

    // ---------- Kurve ----------
    var kurveFig = h('figure', 'gwk-kurve', null, wurzel);
    h('p', 'gwk-achse', T.einheit, kurveFig);
    var kurve = s('svg', { role: 'img', 'aria-label': T.kurve_label + ' ' + d.jahr }, kurveFig);
    var kurveTipp = h('div', 'gwk-tipp gwk-tipp--kurve', null, kurveFig);
    kurveTipp.hidden = true;

    var alleWerte = [], wochen = [];
    d.kurven.forEach(function (k) { k.punkte.forEach(function (p) { alleWerte.push(p[1]); wochen.push(p[0]); }); });
    d.mittel.forEach(function (p) { alleWerte.push(p[1]); });
    var xMin = Math.min(10, Math.min.apply(null, wochen.concat([52])) - 1), xMax = Math.max(48, Math.max.apply(null, wochen.concat([0])) + 1);
    var yMax = Math.max(100, Math.ceil(Math.max.apply(null, alleWerte.concat([0])) / 25) * 25);
    var linien = {};

    function zeichneKurve() {
      var B = kurveFig.clientWidth || 600, Hk = B < 560 ? 230 : 290;
      var m = { l: 34, r: 10, t: 8, b: 24 };
      kurve.setAttribute('viewBox', '0 0 ' + B + ' ' + Hk);
      kurve.setAttribute('width', B);
      kurve.setAttribute('height', Hk);
      kurve.innerHTML = '';
      var X = function (w) { return m.l + (w - xMin) / (xMax - xMin) * (B - m.l - m.r); };
      var Y = function (v) { return Hk - m.b - v / yMax * (Hk - m.t - m.b); };
      var schritt = yMax > 150 ? 50 : 25;
      var gitter = s('g', { class: 'gwk-gitter' }, kurve);
      for (var v = 0; v <= yMax; v += schritt) {
        s('line', { x1: m.l, x2: B - m.r, y1: Y(v), y2: Y(v) }, gitter);
        s('text', { x: m.l - 6, y: Y(v), 'text-anchor': 'end', 'dominant-baseline': 'central' }, gitter).textContent = v;
      }
      d.monate.forEach(function (mo) {
        var mitte = mo[0] + 2.2;
        if (mitte < xMin + 0.5 || mitte > xMax - 0.5) return;
        if (mo[0] > xMin && mo[0] < xMax) s('line', { x1: X(mo[0]), x2: X(mo[0]), y1: Hk - m.b, y2: Hk - m.b + 4, class: 'gwk-tick' }, gitter);
        s('text', { x: X(mitte), y: Hk - m.b + 15, 'text-anchor': 'middle' }, gitter).textContent = T.monate[parseInt(mo[1], 10) - 1];
      });
      // aktuelle Woche
      var akt = s('g', { class: 'gwk-aktuell' }, kurve);
      s('line', { x1: X(d.kw), x2: X(d.kw), y1: m.t, y2: Hk - m.b }, akt);
      s('text', { x: X(d.kw) - 4, y: m.t + 9, 'text-anchor': 'end' }, akt).textContent = T.kw + ' ' + d.kw;

      // Linie bei einer Messluecke von mehr als 4 Wochen unterbrechen
      var pfad = function (pts) { return pts.map(function (p, i) { return (i && p[0] - pts[i - 1][0] <= 4 ? 'L' : 'M') + X(p[0]).toFixed(1) + ' ' + Y(p[1]).toFixed(1); }).join(''); };
      s('path', { d: pfad(d.referenz.filter(function (p) { return p[0] >= xMin && p[0] <= xMax; })), class: 'gwk-referenz' }, kurve);
      linien = {};
      d.kurven.forEach(function (k) {
        if (!k.punkte.length) return;
        var g = s('g', { class: 'gwk-linie', stroke: k.farbe, fill: k.farbe }, kurve);
        s('path', { d: pfad(k.punkte), fill: 'none' }, g);
        k.punkte.forEach(function (p) { s('circle', { cx: X(p[0]), cy: Y(p[1]), r: 2.4 }, g); });
        linien[k.ort] = g;
      });
      s('path', { d: pfad(d.mittel), class: 'gwk-mittel' }, kurve);
      var fadenkreuz = s('line', { class: 'gwk-faden', y1: m.t, y2: Hk - m.b, visibility: 'hidden' }, kurve);
      kurve._achsen = { X: X, m: m, B: B, faden: fadenkreuz };
      if (hervorOrt) hervorheben(hervorOrt);
    }

    // Werte einer Woche fuer den Kurven-Tooltip
    var jeWoche = {};
    d.kurven.forEach(function (k) { k.punkte.forEach(function (p) { (jeWoche[p[0]] = jeWoche[p[0]] || []).push([k.ort, p[1], p[2]]); }); });
    var mittelJe = {}, refJe = {};
    d.mittel.forEach(function (p) { mittelJe[p[0]] = p; });
    d.referenz.forEach(function (p) { refJe[p[0]] = p[1]; });
    function kurveZeiger(e) {
      var a = kurve._achsen; if (!a) return;
      var r = kurve.getBoundingClientRect(), x = e.clientX - r.left;
      var w = Math.round(xMin + (x - a.m.l) / (a.B - a.m.l - a.m.r) * (xMax - xMin));
      if (w < Math.max(1, xMin) || w > Math.min(53, xMax)) { kurveWeg(); return; }
      a.faden.setAttribute('x1', a.X(w)); a.faden.setAttribute('x2', a.X(w)); a.faden.setAttribute('visibility', 'visible');
      kurveTipp.innerHTML = '';
      h('b', null, T.kw + ' ' + w, kurveTipp);
      var werte = (jeWoche[w] || []).slice().sort(function (p, q) { return q[1] - p[1]; });
      if (mittelJe[w]) zeile('gwk-tipp-mittel', T.mittel, mittelJe[w][1]);
      werte.forEach(function (v) { zeile(null, v[0], v[1], farbe[v[0]]); });
      if (refJe[w] !== undefined) zeile('gwk-tipp-ref', T.referenz, refJe[w]);
      kurveTipp.hidden = false;
      var fr = kurveFig.getBoundingClientRect(), links = e.clientX - fr.left;
      kurveTipp.style.left = (links + 14 + kurveTipp.offsetWidth > fr.width ? links - 14 - kurveTipp.offsetWidth : links + 14) + 'px';
      kurveTipp.style.top = '24px';
    }
    function zeile(klasse, name, wert, f) {
      var z = h('span', 'gwk-tipp-zeile' + (klasse ? ' ' + klasse : ''), null, kurveTipp);
      var punkt = h('i', null, null, z);
      if (f) punkt.style.background = f;
      h('span', null, name, z);
      h('b', null, String(wert), z);
    }
    function kurveWeg() {
      kurveTipp.hidden = true;
      if (kurve._achsen) kurve._achsen.faden.setAttribute('visibility', 'hidden');
    }
    kurve.addEventListener('pointermove', kurveZeiger);
    kurve.addEventListener('pointerdown', kurveZeiger);
    kurve.addEventListener('pointerleave', function (e) { if (e.pointerType === 'mouse') kurveWeg(); });
    document.addEventListener('pointerdown', function (e) { if (!kurveFig.contains(e.target)) kurveWeg(); });

    function hervorheben(ort) {
      hervorOrt = ort;
      Object.keys(linien).forEach(function (o) {
        linien[o].classList.toggle('gwk-blass', !!ort && o !== ort);
        linien[o].classList.toggle('gwk-stark', o === ort);
      });
      punktEls.forEach(function (x) { x.g.classList.toggle('gwk-blass', !!ort && x.p.ort !== ort); });
      namenEls.forEach(function (b) { b.classList.toggle('gwk-an', b.dataset.ort === ort); });
    }

    // Namen unter der Kurve: Zeigen hebt hervor, Klick oeffnet den Standort im Datenexplorer
    var namen = h('ul', 'gwk-namen', null, wurzel);
    d.kurven.forEach(function (k) {
      if (!k.punkte.length) return;
      var li = h('li', null, null, namen), b = h('button', 'gwk-name', null, li);
      b.type = 'button';
      b.dataset.ort = k.ort;
      h('i', null, null, b).style.background = k.farbe;
      b.appendChild(document.createTextNode(k.ort));
      b.addEventListener('pointerenter', function () { hervorheben(k.ort); vorladen(); });
      b.addEventListener('pointerleave', function () { hervorheben(null); });
      b.addEventListener('focus', function () { hervorheben(k.ort); });
      b.addEventListener('blur', function () { hervorheben(null); });
      b.addEventListener('click', function () { oeffnen('standort=' + encodeURIComponent(k.ort), b); });
      namenEls.push(b);
    });
    [['gwk-name--mittel', T.mittel], ['gwk-name--ref', T.referenz]].forEach(function (e) {
      var li = h('li', 'gwk-name gwk-name--fest ' + e[0], null, namen);
      h('i', null, null, li);
      li.appendChild(document.createTextNode(e[1]));
    });

    // ---------- Wege in den vollen Datenexplorer ----------
    var mehr = h('section', 'gwk-mehr', null, wurzel);
    h('h3', null, T.mehr_titel, mehr);
    h('p', null, T.mehr_text, mehr);
    var wege = h('div', 'gwk-wege', null, mehr);
    T.wege.forEach(function (w, i) {
      var b = h('button', 'gwk-weg' + (i === 0 ? ' gwk-weg--haupt' : ''), null, wege);
      b.type = 'button';
      h('b', null, w[1], b);
      h('span', null, w[2], b);
      vollKnopf(b, w[0] || ('kw=' + d.kw));
    });
    h('p', 'gwk-klein', T.laden, mehr);

    // Werte als Liste (Screenreader, Ausdruck)
    var det = h('details', 'gwk-liste', null, wurzel);
    h('summary', null, T.liste, det);
    var ul = h('ul', null, null, det);
    d.punkte.forEach(function (p) {
      h('li', null, p.ort + ': ' + (p.wert !== null ? p.wert + ' ' + T.einheit + ' (' + p.letzte_datum + ')'
        : T.keine + (p.letzte_datum ? ' – ' + T.zuletzt + ' ' + p.letzte_datum + ': ' + p.letzte_wert : '')), ul);
    });

    punktGroesse();
    zeichneKurve();
    var breiteAlt = 0;
    var neu = function () {
      var b = wurzel.clientWidth;
      if (b === breiteAlt) return;
      breiteAlt = b;
      punktGroesse();
      zeichneKurve();
    };
    if (window.ResizeObserver) new ResizeObserver(neu).observe(wurzel);
    else window.addEventListener('resize', neu);
  }

  window.GWKurzansicht = { zeige: zeige };
})();
