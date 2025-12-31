document.addEventListener('DOMContentLoaded', function() {
  // Formata legendas de figuras/tabelas
  var sel = document.querySelectorAll('.quarto-float-caption, figcaption, caption');
  var re = /^((Figura|Tabela)\s*\d+(?:\.\d+)*)(?:\s*[:\-]?\s*)([\s\S]*)$/i;
  sel.forEach(function(el){
    if (el.innerHTML.indexOf('<strong>') !== -1) return;
    var text = el.textContent.trim();
    var m = text.match(re);
    if (m) {
      var rest = m[3] ? m[3].trim() : '';
      el.innerHTML = '<strong>' + m[1] + ' -</strong> ' + rest;
    }
  });

  // Formata títulos de exemplos gerados pelo Quarto
  // Padrão observado: "Exemplo 1.1 (Título do Exemplo)" — queremos "Exemplo 1.1 - Título do Exemplo"
  var exTitles = document.querySelectorAll('.theorem-title strong, .theorem-title');
  var exRe = /^(Exemplo\s*\d+(?:\.\d+)*)(?:\s*)\((.*)\)$/i;
  exTitles.forEach(function(el){
    try {
      var txt = el.textContent.trim();
      var m = txt.match(exRe);
      if (m) {
        var prefix = m[1].trim();
        var title = m[2].trim();
        el.textContent = prefix + ' - ' + title;
      }
    } catch (e) {
      // ignore
    }
  });
});
document.addEventListener('DOMContentLoaded', function() {
  // Formata legendas de figuras/tabelas
  var sel = document.querySelectorAll('.quarto-float-caption, figcaption, caption');
  var re = /^((Figura|Tabela)\s*\d+(?:\.\d+)*)(?:\s*[:\-]?\s*)([\s\S]*)$/i;
  sel.forEach(function(el){
    if (el.innerHTML.indexOf('<strong>') !== -1) return;
    var text = el.textContent.trim();
    var m = text.match(re);
    if (m) {
      var rest = m[3] ? m[3].trim() : '';
      el.innerHTML = '<strong>' + m[1] + ' -</strong> ' + rest;
    }
  });

  // Formata títulos de exemplos gerados pelo Quarto
  // Padrão observado: "Exemplo 1.1 (Título do Exemplo)" — queremos "Exemplo 1.1 - Título do Exemplo"
  var exTitles = document.querySelectorAll('.theorem-title strong, .theorem-title');
  var exRe = /^(Exemplo\s*\d+(?:\.\d+)*)(?:\s*)\((.*)\)$/i;
  exTitles.forEach(function(el){
    try {
      var txt = el.textContent.trim();
      var m = txt.match(exRe);
      if (m) {
        var prefix = m[1].trim();
        var title = m[2].trim();
        el.textContent = prefix + ' - ' + title;
      }
    } catch (e) {
      // ignore
    }
  });
});
