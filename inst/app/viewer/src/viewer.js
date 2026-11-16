// Bundled with deck.gl 9.4.1 and Viv 0.23.0; see build.mjs for the entry point.
import './help.js';
import './grid-controls.js';
import {loadGeometry} from './geometry.js';
import {pointSpacing, pointRadius} from './point-size.js';
import {Deck, OrthographicView, COORDINATE_SYSTEM} from '@deck.gl/core';
import {ScatterplotLayer, PathLayer, PolygonLayer, BitmapLayer} from '@deck.gl/layers';
import {DataFilterExtension} from '@deck.gl/extensions';
import {MultiscaleImageLayer, loadOmeZarr, loadOmeTiff, getImageSize, isInterleaved} from '@hms-dbmi/viv';
import {Matrix4} from '@math.gl/core';

const panels = ['sc', 'sd', 'ec', 'ed'];
const filterExtension = new DataFilterExtension({filterSize: 1});
const array = x => x == null ? [] : Array.isArray(x) ? x : [x];
const json = async url => { const r = await fetch(url); if (!r.ok) throw Error(`Cannot load spatial data (${r.status}).`); return r.json(); };
const inside = (x, y, poly) => {
  let hit = false;
  for (let i = 0, j = poly.length - 1; i < poly.length; j = i++) {
    const a = poly[i], b = poly[j];
    if ((a[1] > y) !== (b[1] > y) && x < (b[0] - a[0]) * (y - a[1]) / (b[1] - a[1]) + a[0]) hit = !hit;
  }
  return hit;
};

class SpatialViewer {
  constructor(el) {
    this.el = el; this.tool = 'pan'; this.selected = new Set();
    this.states = {}; this.fitted = {}; this.dataCache = new Map(); this.imageCache = new Map();
    this.spacingCache = new Map();
    this.ticket = 0; this.selectedIndices = []; this.selectedPaths = [];
    this.error = el.querySelector('.sl-viewer-error');
    this.status = document.querySelector('.sl-viewer-status');
    this.svg = el.querySelector('svg');
    this.labels = Object.fromEntries(panels.map(id => {
      const label = document.createElement('div'); label.className = 'sl-viewer-label';
      const text = document.createElement('span'); label.append(text);
      const button = document.createElement('button'); button.type = 'button';
      button.className = 'sl-viewer-expand'; button.textContent = 'Expand';
      button.setAttribute('aria-label', `Expand ${id.startsWith('s') ? 'spatial' : 'embedding'} ${id.endsWith('c') ? 'continuous' : 'discrete'} panel`);
      button.onclick = () => { this.expanded = this.expanded === id ? null : id; this.resize(); };
      for (const [symbol, delta, name] of [['−',-1,'out'],['+',1,'in']]) {
        const zoom = document.createElement('button'); zoom.type = 'button'; zoom.className = 'sl-viewer-zoom';
        zoom.textContent = symbol; zoom.setAttribute('aria-label', `Zoom ${name} ${id === 'sc' ? 'spatial continuous' : id === 'sd' ? 'spatial discrete' : id === 'ec' ? 'embedding continuous' : 'embedding discrete'}`);
        zoom.onclick = () => {
          const pair = id[0] === 's' ? ['sc','sd'] : ['ec','ed'];
          for (const p of pair) this.states[p] = this.constrain(p, {...this.states[p], zoom: this.states[p].zoom + delta});
          this.draw();
        };
        label.append(zoom);
      }
      label.append(button); el.append(label); return [id, label];
    }));
    try {
      this.deck = new Deck({parent: el.querySelector('.sl-viewer-canvas'),
        views: [], layers: [], useDevicePixels: Math.min(window.devicePixelRatio || 1, 2),
        getCursor: ({isDragging}) => this.tool === 'pan' ? (isDragging ? 'grabbing' : 'grab') : 'crosshair',
        layerFilter: ({layer, viewport}) => layer.id.includes(`${viewport.id}:`),
        onViewStateChange: ({viewId, viewState}) => {
          const pair = viewId[0] === 's' ? ['sc', 'sd'] : ['ec', 'ed'];
          for (const id of pair) this.states[id] = this.constrain(id, viewState);
          this.draw();
        },
        getTooltip: info => this.tooltip(info),
        onClick: (info, event) => this.click(info, event),
        onError: error => this.fail(error.message || String(error))
      });
    } catch (e) { this.fail(`Unable to start the spatial viewer: ${e.message}`); }
    new ResizeObserver(() => this.resize()).observe(el);
    for (const button of document.querySelectorAll('[data-viewer-tool]')) button.onclick = () => {
      this.tool = button.dataset.viewerTool;
      for (const b of document.querySelectorAll('[data-viewer-tool]')) b.classList.toggle('active', b === button);
      this.svg.style.pointerEvents = this.tool === 'pan' ? 'none' : 'all'; this.resize();
    };
    for (const button of document.querySelectorAll('[data-viewer-action]')) button.onclick = () => {
      if (button.dataset.viewerAction === 'clear') this.select([]);
      else { this.fit(); this.draw(); }
    };
    this.svg.addEventListener('pointerdown', e => this.startLasso(e));
    this.svg.addEventListener('pointermove', e => this.moveLasso(e));
    this.svg.addEventListener('pointerup', e => this.finishLasso(e));
    this.svg.addEventListener('pointercancel', () => { this.lasso = null; this.svg.replaceChildren(); });
    el.addEventListener('keydown', e => {
      if (e.key === 'Escape') { this.expanded = null; this.select([]); this.resize(); }
    });
    window.Shiny.setInputValue('viewer_ready', true, {priority: 'event'});
  }
  fail(message) { this.error.textContent = message; this.error.style.display = 'block'; }
  clearError() { this.error.textContent = ''; this.error.style.display = 'none'; }
  async apply(message) {
    const ticket = ++this.ticket; this.lastMessage = message;
    try {
      this.clearError(); this.status.textContent = 'Loading spatial data…';
      const meta = message.dataset;
      // Download boundaries alongside coordinates/features, not after their
      // first frame. Share an in-flight request across initial feature updates.
      if (meta.geometry && this.geometryLoad?.url !== meta.geometry) {
        this.geometryLoad = {url: meta.geometry, promise: loadGeometry(meta.geometry, message.total)
          .then(geometry => ({geometry}), error => ({error}))};
      } else if (!meta.geometry) this.geometryLoad = null;
      const geometryPromise = this.geometryLoad?.promise;
      if (!this.dataCache.has(meta.id)) this.dataCache.set(meta.id, json(meta.url));
      const featurePromise = fetch(message.features).then(r => { if (!r.ok) throw Error('Cannot load feature values.'); return r.arrayBuffer(); });
      const [data, bytes] = await Promise.all([this.dataCache.get(meta.id), featurePromise]);
      if (ticket !== this.ticket) return;
      const old = this.meta, changed = old?.id !== meta.id;
      this.meta = meta;
      this.message = {...message, ...(this.appearance?.sample === meta.sample ? this.appearance : {})};
      if (changed) {
        this.data = data; this.data.keys = array(data.keys);
        this.indices = Array.from({length: this.data.keys.length}, (_, i) => i);
        this.positions = this.indices.map(i => [array(data.x)[i], array(data.y)[i]]);
        this.embeddings = Array.isArray(data.embedding) ? data.embedding : [];
        const spatialKey = `spatial:${meta.sample}`, embeddingKey = `embedding:${meta.id}`;
        if (!this.spacingCache.has(spatialKey)) this.spacingCache.set(spatialKey, pointSpacing(this.positions));
        if (meta.hasEmbedding && !this.spacingCache.has(embeddingKey)) this.spacingCache.set(embeddingKey, pointSpacing(this.embeddings));
        this.spatialSpacing = this.spacingCache.get(spatialKey);
        this.embeddingSpacing = this.spacingCache.get(embeddingKey);
        if (old?.geometry !== meta.geometry) this.geometry = null;
        if (old?.sample !== meta.sample) this.selected.clear();
      }
      const n = this.indices.length;
      if (bytes.byteLength !== 17 * n) throw Error('Feature values do not match the spatial units.');
      this.values = new Float32Array(bytes, 0, n);
      this.labelIndices = new Uint32Array(bytes, 4 * n, n);
      this.continuous = new Uint8Array(bytes, 8 * n, 4 * n);
      this.discrete = new Uint8Array(bytes, 12 * n, 4 * n);
      this.eligible = new Uint8Array(bytes, 16 * n, n);
      for (const i of this.selected) if (!this.eligible[i]) this.selected.delete(i);
      this.rebuildSelection();
      if (!old || old.sample !== meta.sample) { this.image = null; this.imageKey = null; this.states = {}; this.fit(); }
      else if (old.embeddingName !== meta.embeddingName) this.fit(false, true);
      this.renderLegends(); this.resize(); this.emitSelection();
      if (changed && meta.geometry && !this.geometry) geometryPromise.then(({geometry, error}) => {
        if (this.meta?.id !== meta.id) return;
        if (error) throw error;
        this.geometry = geometry; this.rebuildSelection(); this.draw();
      }).catch(e => { if (this.meta?.id === meta.id) this.fail(`Unable to load cell boundaries: ${e.message}`); });
      this.refreshImage(old?.sample === meta.sample);
      if (this.dataCache.size > 2) this.dataCache.delete(this.dataCache.keys().next().value);
    } catch (e) { this.dataCache.delete(message.dataset.id); if (ticket === this.ticket) this.fail(e.message); }
  }
  applyAppearance(message) {
    this.appearance = message;
    if (!this.message || this.meta?.sample !== message.sample) return;
    this.message = {...this.message, ...message};
    this.refreshImage(true); this.draw();
  }
  refreshImage(preserve) {
    const source = this.message.image ? {...this.message.image,
      yDirection: this.message.image.yDirection || (this.meta.yDown ? 'down' : 'up')} : null;
    const key = `${this.meta.sample}:${JSON.stringify(source)}`;
    if (key === this.imageKey) return;
    this.imageKey = key; this.image = null;
    this.fit(true, false, preserve); this.draw();
    if (!source) return;
    // Opening image metadata is independent of recoloring and appearance
    // changes. Stale image completions must not replace a newer source.
    if (!this.imageCache.has(source.url)) this.imageCache.set(source.url, this.openImage(source));
    this.imageCache.get(source.url).then(im => {
      if (key !== this.imageKey) return;
      this.image = {...im, source}; this.fit(true, false, true); this.draw();
    }).catch(e => {
      this.imageCache.delete(source.url);
      if (key === this.imageKey) this.fail(`Unable to load histology: ${e.message}`);
    });
  }
  async openImage(source) {
    if (source.type === 'image') return {bitmap: true};
    const url = new URL(source.url, document.baseURI).href;
    const loaded = source.type === 'ome-zarr' ? await loadOmeZarr(url, {type: 'multiscales'}) : await loadOmeTiff(url);
    const loader = loaded.data;
    if (!loader?.length) throw Error('No image pyramid found.');
    const size = getImageSize(loader[0]);
    if (loader.length === 1 && Math.max(size.width,size.height) > 4096) throw Error('Prepare a multiresolution image pyramid before viewing a large full-resolution image.');
    const shape = loader[0].shape;
    const interleaved = isInterleaved(shape);
    const labels = loader[0].labels.map(l => typeof l === 'string' ? l : l.name);
    const c = labels.indexOf('c');
    const channels = interleaved ? 1 : (c >= 0 ? Math.min(3, shape[c]) : 1);
    const selections = Array.from({length: channels}, (_, channel) => Object.fromEntries(
      labels.filter(l => !['x','y','s'].includes(l)).map(l => [l, l === 'c' ? channel : 0])));
    const {width, height} = getImageSize(loader[0]);
    // Viv's loaders expose pixel coordinates; physical NGFF scales are metadata,
    // not an extra transform already applied by the loader. Use the registered
    // scene extent once, avoiding a second micron-to-pixel conversion.
    const extent = source.extent || [0, 0, width, height];
    const down = source.yDirection ? source.yDirection === 'down' : this.meta.yDown;
    const matrix = new Matrix4().translate([extent[0], down ? extent[1] : extent[3], 0])
      .scale([(extent[2] - extent[0]) / width, (extent[3] - extent[1]) / height * (down ? 1 : -1), 1]);
    return {loader, selections, extent, matrix, channels};
  }
  rects() {
    const ids = this.expanded ? [this.expanded] : panels.filter(id => id[0] === 's' || this.meta?.hasEmbedding);
    const w = this.el.clientWidth, h = this.el.clientHeight;
    const columns = this.expanded || w < 620 ? 1 : 2;
    const rows = Math.ceil(ids.length / columns);
    return ids.map((id, i) => ({id, x: (i % columns) * w / columns,
      y: Math.floor(i / columns) * h / rows + 30, width: w / columns, height: h / rows - 30}));
  }
  resize() {
    if (!this.deck || !this.meta || !this.el.clientWidth) return;
    const rects = this.rects();
    for (const [id, label] of Object.entries(this.labels)) {
      const r = rects.find(v => v.id === id); label.style.display = r ? 'flex' : 'none';
      if (r) {
        Object.assign(label.style, {left: `${r.x}px`, top: `${r.y - 30}px`, width: `${r.width}px`});
        label.firstChild.textContent = `${id[0] === 's' ? 'Spatial' : this.meta.embeddingName} · ${id[1] === 'c' ? this.message.continuousTitle : this.message.discreteTitle}`;
        label.lastChild.textContent = this.expanded === id ? 'Restore' : 'Expand';
      }
    }
    this.deck.setProps({width: this.el.clientWidth, height: this.el.clientHeight,
      views: rects.map(r => new OrthographicView({...r, flipY: r.id[0] === 's' ? this.meta.yDown : false,
        controller: {dragPan: this.tool === 'pan', scrollZoom: true, doubleClickZoom: this.tool === 'pan', keyboard: true}}))});
    this.fit(true, true, true);
    this.draw();
  }
  constrain(id, state) {
    const full = this.fitted[id];
    if (!full) return {...state};
    if (state.zoom <= full.zoom + 1e-6) return {...state, ...full};
    return {...state, minZoom: full.zoom};
  }
  fit(spatial = true, embedding = true, preserve = false) {
    if (!this.positions || !this.el.clientWidth || !this.el.clientHeight) return;
    const bounds = points => {
      let x0 = Infinity, y0 = Infinity, x1 = -Infinity, y1 = -Infinity;
      for (const p of points) if (p && p.every(Number.isFinite)) { x0 = Math.min(x0, p[0]); x1 = Math.max(x1, p[0]); y0 = Math.min(y0, p[1]); y1 = Math.max(y1, p[1]); }
      return Number.isFinite(x0) ? [x0, y0, x1, y1] : [0, 0, 1, 1];
    };
    const set = (ids, b) => {
      const rect = this.rects().find(r => ids.includes(r.id)) || this.rects()[0] || {width: 500, height: 400};
      const zoom = Math.log2(Math.min(rect.width / Math.max(1, b[2]-b[0]), rect.height / Math.max(1, b[3]-b[1]))) - 0.1;
      const view = {target: [(b[0] + b[2]) / 2, (b[1] + b[3]) / 2, 0],
        zoom, minZoom: zoom, maxZoom: 12};
      for (const id of ids) {
        const old = this.states[id];
        const atFit = old && old.zoom <= old.minZoom + 1e-6;
        this.fitted[id] = view;
        this.states[id] = preserve && old && !atFit ? this.constrain(id, old) : {...view};
      }
    };
    if (spatial) set(['sc','sd'], this.message?.image ? this.message.image.extent || this.image?.extent || bounds(this.positions) : bounds(this.positions));
    if (embedding) set(['ec','ed'], bounds(this.embeddings));
  }
  draw() {
    if (!this.deck || !this.message || !this.states.sc || !this.el.clientWidth) return;
    const m = this.message, layers = [];
    for (const id of panels) {
      const out = this.labels[id].querySelector('.sl-viewer-zoom');
      out.disabled = !this.states[id] || this.states[id].zoom <= this.states[id].minZoom + 1e-6;
    }
    for (const id of panels.filter(id => id[0] === 's' || this.meta.hasEmbedding)) {
      const spatial = id[0] === 's', continuous = id[1] === 'c';
      const positions = spatial ? this.positions : this.embeddings;
      const radius = pointRadius(spatial ? this.spatialSpacing : this.embeddingSpacing,
        this.fitted[id]?.zoom, m.pointSizeScale);
      const rgba = continuous ? this.continuous : this.discrete;
      const color = i => rgba.subarray(i * 4, i * 4 + 4);
      const eligible = i => this.eligible[i] && positions[i]?.every(Number.isFinite) ? 1 : 0;
      const common = {coordinateSystem: COORDINATE_SYSTEM.CARTESIAN,
        pickable: true, opacity: m.opacity, parameters: {depthCompare: 'always', depthWriteEnabled: false}};
      if (spatial && this.image && m.imageOpacity > 0) {
        const im = this.image;
        if (im.bitmap) {
          const b = im.source.extent;
          if (b) layers.push(new BitmapLayer({id: `${id}:image:bitmap`, image: im.source.url,
            bounds: this.meta.yDown ? [b[0],b[3],b[2],b[1]] : b,
            coordinateSystem: COORDINATE_SYSTEM.CARTESIAN, opacity: m.imageOpacity,
            parameters: common.parameters}));
        } else layers.push(new MultiscaleImageLayer({id: `${id}:image:pyramid`, viewportId: id,
          loader: im.loader, selections: im.selections, modelMatrix: im.matrix,
          contrastLimits: im.selections.map(() => [0,255]),
          channelsVisible: im.selections.map(() => true),
          colors: im.channels === 1 ? [[255,255,255]] : [[255,0,0],[0,255,0],[0,0,255]],
          opacity: m.imageOpacity, pickable: false, maxCacheSize: 48,
          onTileError: e => this.fail(`Image tile could not load: ${e.message}`), parameters: common.parameters}));
      }
      const boundaries = spatial && this.geometry && (this.states[id]?.zoom ?? -20) > -2.5;
      // Keep both representations mounted. Removing a layer at the zoom cutoff
      // discards its GPU buffers and repeats geometry preparation on the next crossing.
      if (spatial && this.geometry) {
        layers.push(new PathLayer({...common, id: `${id}:units:outlines`, data: this.geometry.paths,
          visible: Boolean(boundaries && m.boundaryStyle !== 'fill'),
          positionFormat: 'XY', getColor: (_, {index}) => color(this.geometry.ringUnits[index]),
          widthUnits: 'pixels', getWidth: 1,
          extensions: [filterExtension], filterRange: [1,1],
          getFilterValue: (_, {index}) => eligible(this.geometry.ringUnits[index]),
          updateTriggers: {getColor: m.revision, getFilterValue: m.revision}}));
        if (m.boundaryStyle === 'fill') layers.push(new PolygonLayer({...common,
          id: `${id}:units:polygons`, data: this.geometry.polygons(), visible: Boolean(boundaries),
          filled: true, stroked: false, positionFormat: 'XY', getPolygon: f => f, getFillColor: f => color(f.properties.unit_index),
          extensions: [filterExtension], filterRange: [1,1],
          getFilterValue: f => eligible(f.properties.unit_index),
          updateTriggers: {getFillColor: m.revision, getFilterValue: m.revision}}));
        if (this.selectedPaths.length) layers.push(new PathLayer({...common, id: `${id}:selected`,
          data: this.selectedPaths, visible: Boolean(boundaries),
          getPath: r => this.geometry.path(r), positionFormat: 'XY',
          getColor: [255,255,255,255], widthUnits: 'pixels', getWidth: 2, pickable: false}));
      }
      layers.push(new ScatterplotLayer({...common, id: `${id}:units:points`, data: this.indices,
        visible: !boundaries, getPosition: i => positions[i] || [0,0], getRadius: radius,
        radiusUnits: 'pixels', getFillColor: color,
        extensions: [filterExtension], filterRange: [1,1], getFilterValue: eligible,
        updateTriggers: {getFillColor: m.revision, getFilterValue: m.revision}}));
      if (this.selectedIndices.length) layers.push(new ScatterplotLayer({...common, id: `${id}:selection`,
        data: this.selectedIndices, getPosition: i => positions[i] || [0,0],
        getRadius: radius + 2, radiusUnits: 'pixels', filled: false,
        stroked: true, getLineColor: [255,255,255,255], lineWidthUnits: 'pixels', getLineWidth: 2, pickable: false,
        extensions: [filterExtension], filterRange: [1,1], getFilterValue: eligible,
        updateTriggers: {getFilterValue: m.revision}}));
    }
    this.deck.setProps({layers, viewState: {...this.states}});
    this.status.textContent = `${Number(m.shown).toLocaleString()} / ${Number(m.total).toLocaleString()} shown · ${this.selected.size.toLocaleString()} selected`;
  }
  index(info) {
    if (info.layer?.id.includes(':units:outlines')) return this.geometry?.ringUnits[info.index];
    return typeof info.object === 'number' ? info.object : info.object?.properties?.unit_index;
  }
  tooltip(info) {
    const i = this.index(info);
    if (i == null || !this.eligible?.[i]) return null;
    const panel = info.viewport?.id || info.layer?.id.split(':')[0];
    const discrete = panel === 'sd' || panel === 'ed';
    const title = discrete ? this.message.discreteTitle : this.message.continuousTitle;
    const value = discrete ? array(this.message.discreteLegend)[this.labelIndices[i]]?.label : this.values[i].toPrecision(5);
    return {text: `${this.data.keys[i]}\n${title}: ${value}`};
  }
  click(info, event) {
    if (this.tool === 'lasso') return;
    const i = this.index(info); if (i == null || !this.eligible[i]) return;
    this.select([i], event.srcEvent?.shiftKey ? 'add' : event.srcEvent?.altKey ? 'remove' : 'replace');
    window.Shiny.setInputValue('viewer_click', {keys: [this.data.keys[i]], sample: this.meta.sample, revision: this.message.revision, nonce: Date.now()}, {priority:'event'});
  }
  rebuildSelection() {
    this.selectedIndices = [...this.selected];
    this.selectedPaths = [];
    if (this.geometry && this.selected.size) {
      for (let r = 0; r < this.geometry.ringUnits.length; r++) if (this.selected.has(this.geometry.ringUnits[r])) this.selectedPaths.push(r);
    }
  }
  select(indices, operation = 'replace') {
    if (operation === 'replace') this.selected.clear();
    for (const i of indices) {
      if (!this.eligible?.[i]) continue;
      if (operation === 'remove') this.selected.delete(i); else this.selected.add(i);
    }
    this.rebuildSelection(); this.emitSelection(); this.draw();
  }
  emitSelection() {
    if (!this.meta) return;
    window.Shiny.setInputValue('viewer_selection', {keys: [...this.selected].map(i => this.data.keys[i]),
      sample: this.meta.sample, revision: this.message.revision}, {priority:'event'});
  }
  point(event) { const b = this.el.getBoundingClientRect(); return [event.clientX-b.left, event.clientY-b.top]; }
  startLasso(e) {
    if (!this.meta || this.tool === 'pan') return;
    const p = this.point(e), rect = this.rects().find(r => p[0] >= r.x && p[0] < r.x+r.width && p[1] >= r.y && p[1] < r.y+r.height);
    if (!rect) return;
    this.svg.setPointerCapture(e.pointerId); e.preventDefault();
    this.lasso = {points: [p], rect, start: p, mode: this.tool};
  }
  moveLasso(e) {
    if (!this.lasso) return;
    const p = this.point(e), last = this.lasso.points.at(-1);
    if (Math.hypot(p[0]-last[0],p[1]-last[1]) < 3) return;
    if (this.lasso.mode === 'select') {
      const a = this.lasso.start; this.lasso.points = [a,[p[0],a[1]],p,[a[0],p[1]]];
    } else this.lasso.points.push(p);
    const polygon = document.createElementNS('http://www.w3.org/2000/svg','polygon');
    polygon.setAttribute('points', this.lasso.points.map(p => p.join(',')).join(' '));
    this.svg.replaceChildren(polygon);
  }
  finishLasso(e) {
    if (!this.lasso) return;
    const {points, rect} = this.lasso; this.lasso = null; this.svg.replaceChildren();
    if (points.length < 3) {
      if (this.tool === 'select') this.click(this.deck.pickObject({x:points[0][0],y:points[0][1],radius:4}) || {}, {srcEvent:e});
      return;
    }
    const viewport = this.deck.getViewports().find(v => v.id === rect.id);
    const polygon = points.map(p => viewport.unproject([p[0]-rect.x,p[1]-rect.y]));
    const xs = polygon.map(p => p[0]), ys = polygon.map(p => p[1]);
    const b = [Math.min(...xs),Math.min(...ys),Math.max(...xs),Math.max(...ys)];
    const coords = rect.id[0] === 's' ? this.positions : this.embeddings, selected = [];
    for (const i of this.indices) {
      if (!this.eligible[i]) continue;
      const p = coords[i];
      if (p && p[0] >= b[0] && p[0] <= b[2] && p[1] >= b[1] && p[1] <= b[3] && inside(p[0],p[1],polygon)) selected.push(i);
    }
    this.select(selected, e.shiftKey ? 'add' : e.altKey ? 'remove' : 'replace');
  }
  renderLegends() {
    const root = document.querySelector('.sl-viewer-legends'); root.replaceChildren();
    const m = this.message, continuous = document.createElement('div');
    const title = document.createElement('strong'); title.textContent = m.continuousTitle; continuous.append(title);
    const ramp = document.createElement('div'); ramp.className = 'sl-color-ramp';
    ramp.style.background = `linear-gradient(to right, ${array(m.palette).join(',')})`; continuous.append(ramp);
    const range = document.createElement('div'); range.className = 'sl-color-range';
    range.textContent = array(m.domain).map(v => Number(v).toPrecision(4)).join(' — '); continuous.append(range); root.append(continuous);
    const discrete = document.createElement('div'); discrete.className = 'sl-discrete-legend';
    for (const item of array(m.discreteLegend)) {
      const entry = document.createElement('span'), swatch = document.createElement('i'); swatch.style.backgroundColor = item.color;
      entry.append(swatch, document.createTextNode(item.label)); discrete.append(entry);
    }
    root.append(discrete);
  }
}

function mount() {
  const element = document.getElementById('spot_viewer');
  if (!element || element.spatialViewer || !window.Shiny) return;
  element.spatialViewer = new SpatialViewer(element);
}
function register() {
  if (typeof window.Shiny?.setInputValue !== 'function' || typeof window.Shiny?.addCustomMessageHandler !== 'function' || window.__spatialLIBDRegistered) return;
  window.__spatialLIBDRegistered = true;
  window.Shiny.addCustomMessageHandler('spatiallibd:viewer', message => { mount(); document.getElementById('spot_viewer')?.spatialViewer.apply(message); });
  window.Shiny.addCustomMessageHandler('spatiallibd:appearance', message => { mount(); document.getElementById('spot_viewer')?.spatialViewer.applyAppearance(message); });
  window.Shiny.addCustomMessageHandler('spatiallibd:error', message => { mount(); document.getElementById('spot_viewer')?.spatialViewer.fail(message.message); });
  mount();
}
if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', register); else register();
if (window.jQuery) window.jQuery(document).on('shiny:connected', register);
