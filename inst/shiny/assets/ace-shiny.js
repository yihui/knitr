var editor = ace.edit("notebook");
editor.setTheme("ace/theme/tomorrow");
editor.getSession().setMode('ace/mode/markdown');
editor.getSession().setUseWrapMode(true);
editor.getSession().setTabSize(2);
editor.getSession().setFoldStyle('markbegin');

editor.getSession().on('change', function(e) {
  $('#nbSrc').val(editor.getValue()).change();
});
editor.getSession().selection.on('changeSelection', function(e) {
  var s = editor.session.getTextRange(editor.getSelectionRange());
  if (s == '') s = editor.getValue();
  $('#nbSrc').val(s).change();
});
editor.commands.addCommand({
  name: 'insertChunk',
  bindKey: 'Ctrl-Alt-I',
  exec: function(editor) {
    editor.insert('```{r}\n\n```\n');
    editor.navigateUp(2);
  }
});
editor.commands.addCommand({
  name: 'compileNotebook',
  bindKey: 'F4|Ctrl-Shift-H',
  exec: function(editor) {
    $('#proxy button').trigger('click');
  }
});

$(document).ready(function() {
  var w = Math.max($(window).width()/2, 300);
  $('#notebook').width(w - 10);
  $('#nbOut').css('left', w + 10 + 'px');
  var h = window.location.hash;
  if (h) {
    // pass a url after # in the url
    h = h.replace('#', '');
    $.get(h, function(data) {
      var str = data.content;
      if (data.encoding == 'base64') {
        str = str.replace(/\n/g, '');
        str = decodeURIComponent(escape(window.atob( str )));
      }
      if (str) editor.setValue(str);
    })
    .error(function() {
      alert('unable to read ' + h);
    });
  }
  $('#nbSrc').val(editor.getValue());
})
