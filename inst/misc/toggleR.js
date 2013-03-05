// toggle visibility of R source blocks in R Markdown output
function toggle_R() {
  var x = document.getElementsByClassName('r');
  if (x.length == 0) return;
  function toggle_vis(o) {
    var d = o.style.display;
    o.style.display = (d == 'block' || d == '') ? 'none':'block';
  }

  for (i = 0; i < x.length; i++) {
    var y = x[i];
    switch (y.tagName.toLowerCase()) {
      case 'pre':
        toggle_vis(y)
        break;
      case 'code':
        var z = y.parentNode;
        // pandoc uses the class 'sourceCode r' on both pre and code
        if (z.tagName.toLowerCase() == 'pre' && z.className != 'sourceCode r') {
          toggle_vis(z);
        }
        break;
    }
  }
}

document.write('<button type="button" onclick="toggle_R();" style="position: absolute; top: 0; right: 0;">Hide/Show Source</button>')
