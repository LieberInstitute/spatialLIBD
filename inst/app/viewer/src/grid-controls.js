// step=1 controls spinner increments but still permits typed/pasted fractions.
// Normalize before Shiny's input listener reads the value, and reject decimal
// keystrokes. Leave an empty field editable; the server validates it on Update.
const gridDimension = target => target instanceof HTMLInputElement &&
  ['grid_nrow', 'grid_ncol'].includes(target.id);
const normalize = event => {
  const input = event.target;
  if (!gridDimension(input) || input.value === '') return;
  const value = Number(input.value);
  if (Number.isFinite(value)) input.value = String(Math.max(1, Math.round(value)));
};
document.addEventListener('input', normalize, true);
document.addEventListener('change', normalize, true);
document.addEventListener('keydown', event => {
  if (gridDimension(event.target) && !event.ctrlKey && !event.metaKey &&
      ['.', ',', 'e', 'E', '+', '-'].includes(event.key)) event.preventDefault();
});
