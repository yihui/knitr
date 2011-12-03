---
layout: default
title: Demos
---

<div id="home">
  <h1>Archive ({{ site.posts | size }} demos)</h1>
  <ul class="posts">
    {% for post in site.posts %}
      <li><span>{{ post.date | date_to_string }}</span> &raquo; <a href="/knitr{{ post.url }}">{{ post.title }}</a> ({{ post.subtitle }})</li>
    {% endfor %}
  </ul>
</div>
