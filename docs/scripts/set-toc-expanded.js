// Define o valor padrão de expansão do TOC para manter até nível 2 aberto
// (mantendo `toc-depth: 3` em `_quarto.yml` para incluir h3 nos itens,
// mas deixando h3 fechados por padrão — serão abertos apenas quando ativos).
(function(){
  try {
    const navs = document.querySelectorAll('nav.toc-active[role="doc-toc"]');
    navs.forEach((nav) => {
      // Se já houver um valor explícito, não sobrescreve
      if (!nav.hasAttribute('data-toc-expanded')) {
        nav.setAttribute('data-toc-expanded', '2');
      }
    });
  } catch (e) {
    // silencioso — falhas aqui não interrompem o carregamento da página
    console.error('set-toc-expanded error', e);
  }
})();
