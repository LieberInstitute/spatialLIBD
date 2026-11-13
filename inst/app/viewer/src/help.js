// Shiny already supplies Bootstrap's popover plugin. Delegate events so these
// controls also work in dynamically rendered UI, independently of viewer state.
function hideHelp(except = null) {
  const $ = window.jQuery;
  if (!$?.fn.popover) return;
  for (const button of document.querySelectorAll('.sl-control-info')) {
    if (button !== except && $(button).data('bs.popover')) $(button).popover('hide');
  }
}

document.addEventListener('click', event => {
  if (event.target.closest('.sl-control-help')) return;
  const button = event.target.closest('.sl-control-info');
  hideHelp(button);
  if (!button) return;
  event.preventDefault(); // Keep the associated dropdown closed.
  const $ = window.jQuery;
  if (!$?.fn.popover) return;
  const trigger = $(button);
  if (!trigger.data('bs.popover')) {
    trigger.popover({container: 'body', placement: 'auto bottom', trigger: 'manual', html: true, animation: false,
      viewport: {selector: 'body', padding: 8},
      title: () => button.dataset.helpTitle,
      content: () => document.getElementById(button.dataset.helpId)?.innerHTML || '',
      template: '<div class="popover sl-control-help" role="tooltip"><div class="arrow"></div><h3 class="popover-title"></h3><div class="popover-content"></div></div>'});
    trigger.on('shown.bs.popover', () => button.setAttribute('aria-expanded', 'true'));
    trigger.on('hidden.bs.popover', () => button.setAttribute('aria-expanded', 'false'));
  }
  trigger.popover('toggle');
});

document.addEventListener('keydown', event => {
  if (event.key !== 'Escape') return;
  const open = document.querySelector('.sl-control-info[aria-expanded="true"]');
  hideHelp();
  if (open && event.target.closest('.sl-control-help')) open.focus();
});
window.addEventListener('resize', () => hideHelp());
