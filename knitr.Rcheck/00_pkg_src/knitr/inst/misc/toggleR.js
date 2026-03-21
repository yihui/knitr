// toggle visibility of R source blocks in R Markdown output
(d => {
  d.insertAdjacentHTML('beforeend','<button style="position:absolute;top:0;right:0;z-index:2;">Toggle Source</button>');
  d.lastElementChild.onclick = (e) => {
    d.querySelectorAll('pre.r').forEach(el => {
      el.style.display = (el.style.display === 'none') ? 'block' : 'none';
    });
  };
})(document.body);
