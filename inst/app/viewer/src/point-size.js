// Estimate spacing with exact nearest-neighbor queries against all finite points.
// Only query a deterministic, evenly spaced subset; discard the index afterward.
// The viewer caches this scalar, not another copy of the sample's coordinates.
export function pointSpacing(points, maxQueries = 2048) {
  const ids = [];
  for (let i = 0; i < points.length; i++) {
    const p = points[i];
    if (p && Number.isFinite(p[0]) && Number.isFinite(p[1])) ids.push(i);
  }
  if (ids.length < 2) return null;
  // Balanced implicit kd-tree: median partitioning avoids sorting every branch.
  const partition = (left, right, k, axis) => {
    while (left < right) {
      const pivot = points[ids[(left + right) >> 1]][axis];
      let i = left, j = right;
      while (i <= j) {
        while (points[ids[i]][axis] < pivot) i++;
        while (points[ids[j]][axis] > pivot) j--;
        if (i <= j) { const temp = ids[i]; ids[i++] = ids[j]; ids[j--] = temp; }
      }
      if (k <= j) right = j;
      else if (k >= i) left = i;
      else break;
    }
  };
  const build = (left, right, axis) => {
    if (left >= right) return;
    const mid = (left + right) >> 1;
    partition(left, right, mid, axis);
    build(left, mid - 1, 1 - axis); build(mid + 1, right, 1 - axis);
  };
  build(0, ids.length - 1, 0);
  const nearest = query => {
    const p = points[query];
    let best = Infinity;
    const visit = (left, right, axis) => {
      if (left > right || best === 0) return;
      const mid = (left + right) >> 1, id = ids[mid], q = points[id];
      const dx = p[0] - q[0], dy = p[1] - q[1];
      if (id !== query) best = Math.min(best, dx * dx + dy * dy);
      const delta = p[axis] - q[axis];
      if (delta <= 0) {
        visit(left, mid - 1, 1 - axis);
        if (delta * delta < best) visit(mid + 1, right, 1 - axis);
      } else {
        visit(mid + 1, right, 1 - axis);
        if (delta * delta < best) visit(left, mid - 1, 1 - axis);
      }
    };
    visit(0, ids.length - 1, 0);
    return Math.sqrt(best);
  };
  // Sample original coordinate order rather than the tree's spatial ordering.
  const queries = Math.min(maxQueries, ids.length), sample = [];
  let seen = 0, next = 0;
  for (let i = 0; i < points.length && sample.length < queries; i++) {
    const p = points[i];
    if (!p || !Number.isFinite(p[0]) || !Number.isFinite(p[1])) continue;
    if (seen++ === next) {
      sample.push(nearest(i));
      next = Math.floor(sample.length * ids.length / queries);
    }
  }
  sample.sort((a, b) => a - b);
  return sample[Math.floor(0.3 * (sample.length - 1))];
}

export function pointRadius(spacing, fitZoom, adjustment = 1) {
  if (!Number.isFinite(adjustment) || adjustment <= 0) adjustment = 1;
  if (spacing == null || !Number.isFinite(fitZoom)) return 2.8 * adjustment;
  // At the fitted view, diameter equals the 30th-percentile spacing. Keep a
  // visible minimum for dense data and a restrained maximum for sparse data.
  return Math.max(0.6, Math.min(3, spacing * 2 ** fitZoom / 2)) * adjustment;
}
