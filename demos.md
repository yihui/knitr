---
layout: default
title: Demos
subtitle: Source and output of demos
---

<div id="home">
  <p>Note there is a larger collection of examples in the <a href="https://github.com/yihui/knitr-examples">knitr-examples</a> repository on Github. This page is more for the documentation purpose. You can take a look at the <a href="/knitr/demo/showcase/">collection of knitr applications</a> by other users as well.</p>
  <h2>Archive ({{ site.posts | size }} demos)</h2>
  <ul class="posts">
    {% for post in site.posts reversed %}
      <li><span>{{ post.date | date_to_string }}</span> &raquo; <a href="{{ site.url }}{{ post.url }}">{{ post.title }}</a> <span>({{ post.subtitle }})</span></li>
    {% endfor %}
  </ul>
</div>
