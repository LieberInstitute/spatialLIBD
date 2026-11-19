// Private counterpart of app_spot_write_geometry(). Coordinates stay float64;
// typed-array views share the response buffer instead of creating vertex objects.
export function decodeGeometry(buffer, units) {
  const invalid = () => { throw Error('Invalid spatial boundary data.'); };
  if (buffer.byteLength < 32) invalid();
  const header = new DataView(buffer);
  const word = i => header.getUint32(i * 4, true);
  if (word(0) !== 1397506628 || word(1) !== 1 || word(5) !== 8 || word(6) !== units) invalid();
  const rings = word(2), vertices = word(3), polygons = word(4);
  const indexWords = 8 + rings + 1 + polygons + 1 + polygons + rings;
  const coordinateOffset = Math.ceil(indexWords / 2) * 8;
  if (!rings || !polygons || coordinateOffset + vertices * 16 !== buffer.byteLength) invalid();
  const ringOffsets = new Uint32Array(buffer, 32, rings + 1);
  const polygonOffsets = new Uint32Array(buffer, 32 + (rings + 1) * 4, polygons + 1);
  const polygonUnits = new Uint32Array(buffer, 32 + (rings + polygons + 2) * 4, polygons);
  const ringUnits = new Uint32Array(buffer, 32 + (rings + polygons * 2 + 2) * 4, rings);
  const xy = new Float64Array(buffer, coordinateOffset, vertices * 2);
  if (ringOffsets[0] !== 0 || ringOffsets[rings] !== vertices || polygonOffsets[0] !== 0 || polygonOffsets[polygons] !== rings) invalid();
  for (let r = 0; r < rings; r++) if (ringOffsets[r + 1] - ringOffsets[r] < 4 || ringOffsets[r + 1] > vertices || ringUnits[r] >= units) invalid();
  for (let p = 0; p < polygons; p++) if (polygonOffsets[p + 1] <= polygonOffsets[p] || polygonOffsets[p + 1] > rings || polygonUnits[p] >= units) invalid();
  const path = r => xy.subarray(ringOffsets[r] * 2, ringOffsets[r + 1] * 2);
  const pick = polygonPicker(xy, ringOffsets, polygonOffsets, polygonUnits);
  let fillData;
  return {
    ringUnits, path, pick,
    paths: {length: rings, startIndices: ringOffsets, attributes: {getPath: {value: xy, size: 2}}},
    // Triangulated shapes are only needed when selected in Appearance. Keep holes and
    // multipolygon parts together without expanding coordinates into JS arrays.
    polygons: () => fillData ||= Array.from({length: polygons}, (_, p) => {
      const first = polygonOffsets[p], end = polygonOffsets[p + 1];
      return {positions: xy.subarray(ringOffsets[first] * 2, ringOffsets[end] * 2),
        holeIndices: Array.from({length: end - first - 1}, (_, h) => (ringOffsets[first + h + 1] - ringOffsets[first]) * 2),
        properties: {unit_index: polygonUnits[p]}};
    })
  };
}

export async function loadGeometry(url, units) {
  const response = await fetch(url);
  if (!response.ok) throw Error(`Cannot load cell boundaries (${response.status}).`);
  return decodeGeometry(await response.arrayBuffer(), units);
}

// Index polygon bounding boxes once. Outline picking tests only nearby polygon
// interiors, avoiding triangulation/GPU fill buffers for the entire sample.
function polygonPicker(xy, rings, polygons, units) {
  const n = units.length, bounds = new Float64Array(4 * n);
  let xmin = Infinity, ymin = Infinity, xmax = -Infinity, ymax = -Infinity;
  for (let p = 0; p < n; p++) {
    let x0 = Infinity, y0 = Infinity, x1 = -Infinity, y1 = -Infinity;
    const r = polygons[p];
    for (let i = rings[r] * 2; i < rings[r + 1] * 2; i += 2) {
      x0 = Math.min(x0, xy[i]); x1 = Math.max(x1, xy[i]);
      y0 = Math.min(y0, xy[i + 1]); y1 = Math.max(y1, xy[i + 1]);
    }
    bounds.set([x0,y0,x1,y1], p * 4);
    xmin = Math.min(xmin,x0); ymin = Math.min(ymin,y0);
    xmax = Math.max(xmax,x1); ymax = Math.max(ymax,y1);
  }
  const width = Math.max(1, xmax-xmin), height = Math.max(1, ymax-ymin);
  const cell = Math.sqrt(width * height / n) * 4;
  const nx = Math.max(1, Math.min(512, Math.ceil(width/cell)));
  const ny = Math.max(1, Math.min(512, Math.ceil(height/cell)));
  const ix = x => Math.max(0, Math.min(nx-1, Math.floor((x-xmin)/width*nx)));
  const iy = y => Math.max(0, Math.min(ny-1, Math.floor((y-ymin)/height*ny)));
  const bins = new Array(nx*ny), large = [];
  for (let p = 0; p < n; p++) {
    const b = p*4, x0 = ix(bounds[b]), x1 = ix(bounds[b+2]);
    const y0 = iy(bounds[b+1]), y1 = iy(bounds[b+3]);
    // Avoid duplicating a large polygon into thousands of bins.
    if ((x1-x0+1)*(y1-y0+1) > 64) { large.push(p); continue; }
    for (let y = y0; y <= y1; y++) for (let x = x0; x <= x1; x++) {
      (bins[y*nx+x] ||= []).push(p);
    }
  }
  const inRing = (x,y,r) => {
    let hit = false;
    const start = rings[r]*2, end = rings[r+1]*2;
    for (let i = start, j = end-2; i < end; j = i, i += 2) {
      const ax = xy[i], ay = xy[i+1], bx = xy[j], by = xy[j+1];
      if ((ay > y) !== (by > y) && x < (bx-ax)*(y-ay)/(by-ay)+ax) hit = !hit;
    }
    return hit;
  };
  return (x,y,eligible) => {
    if (!Number.isFinite(x) || !Number.isFinite(y) || x < xmin || x > xmax || y < ymin || y > ymax) return null;
    let found = -1;
    for (const candidates of [bins[iy(y)*nx+ix(x)] || [], large]) {
      for (let i = candidates.length-1; i >= 0; i--) {
        const p = candidates[i], b = p*4;
        if (p <= found || !eligible(units[p]) || x < bounds[b] || x > bounds[b+2] || y < bounds[b+1] || y > bounds[b+3]) continue;
        const first = polygons[p], end = polygons[p+1];
        if (!inRing(x,y,first)) continue;
        let hole = false;
        for (let r = first+1; r < end && !hole; r++) hole = inRing(x,y,r);
        if (!hole) { found = p; break; }
      }
    }
    return found < 0 ? null : units[found];
  };
}
