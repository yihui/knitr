// I'm not taking comments from the website any more
setTimeout(function() {
  var b = document.getElementsByTagName('button');
  if (b.length == 0) return;
  for (i = 0; i < b.length; i++) {
    var x = b[i];
    if (!x.disabled) {
      x.onclick = function(e) {
        if (window.confirm('Sorry, no more questions here please; see FAQ 2: ' +
        'http://bit.ly/knitr-faq')) {
          window.open('http://bit.ly/knitr-faq');
        }
        x.disabled = true;
        return false;
      }
    }
  }
}, 3000);
