// I'm not taking comments from the website any more
setTimeout(function() {
  var b = document.getElementsByTagName('button');
  if (b.length == 0) return;
  for (i = 0; i < b.length; i++) {
    var x = b[i];
    if (x.clicked == undefined) {
      x.onclick = function(e) {
        if (x.clicked) return true;
        if (window.confirm('Sorry, no more questions here please; see FAQ 2: http://bit.ly/knitr-faq' +
        ' If you have a comment instead of a question, hit Cancel and resubmit')) {
          window.open('http://bit.ly/knitr-faq');
        } else x.clicked = true;
        return false;
      }
    }
  }
}, 3000);
