document.addEventListener('DOMContentLoaded', function() {
  var sel = document.querySelectorAll('.quarto-float-caption, figcaption, caption');
  var re = /^((Figura|Tabela)\s*\d+(?:\.\d+)*)(?:\s*[:\-]?\s*)([\s\S]*)$/i;
  sel.forEach(function(el){
    // skip if already processed
    if (el.innerHTML.indexOf('<strong>') !== -1) return;
    var text = el.textContent.trim();
    var m = text.match(re);
    if (m) {
      var rest = m[3] ? m[3].trim() : '';
      // preserve existing inline HTML is not handled; this targets plain-text captions
      // include the hyphen inside the bold prefix: "<strong>Figura X.Y -</strong> rest"
      el.innerHTML = '<strong>' + m[1] + ' -</strong> ' + rest;
    }
  });
});
