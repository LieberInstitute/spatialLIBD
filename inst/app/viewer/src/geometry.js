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
  let fillData;
  return {
    ringUnits, path,
    paths: {length: rings, startIndices: ringOffsets, attributes: {getPath: {value: xy, size: 2}}},
    // Filled shapes are only needed when selected in Appearance. Keep holes and
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
