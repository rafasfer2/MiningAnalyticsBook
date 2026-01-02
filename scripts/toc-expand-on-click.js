document.addEventListener('DOMContentLoaded', function () {
  try {
    const toc = document.querySelector('nav.toc-active[role="doc-toc"]');
    if (!toc) return;

    const links = toc.querySelectorAll('a[data-scroll-target]');
    links.forEach((link) => {
      link.addEventListener('click', function (e) {
        // find immediate child UL (level below this link)
        const parentLi = link.parentElement;
        if (!parentLi) return;
        const childUl = parentLi.querySelector(':scope > ul.collapse');
        if (childUl) {
          // toggle collapse class (Quarto expects 'collapse' to hide)
          if (childUl.classList.contains('collapse')) {
            childUl.classList.remove('collapse');
          } else {
            childUl.classList.add('collapse');
          }

          // force Quarto's walk() to recompute via scroll event
          setTimeout(() => window.dispatchEvent(new Event('scroll')), 50);
        }
      });
    });
  } catch (err) {
    // fail silently
    console.error('toc-expand-on-click error', err);
  }
});
