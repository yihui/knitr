---
layout: default
title: WordPress
subtitle: Publish blog posts from R + knitr to WordPress
category: demo
---

The **RWordPress** package allows one to publish blog posts from R to WordPress (see the `newPost()` function in the package). A blog post is essentially an HTML fragment, and **knitr** can create such a fragment from R Markdown with the **markdown** package. Below is how to do this with the function `knit2wp()` in **knitr**:

{% highlight r %}
if (!require('RWordPress'))
  install.packages('RWordPress', repos = 'http://www.omegahat.org/R', type = 'source')
library(RWordPress)
options(WordpressLogin = c(user = 'password'),
        WordpressURL = 'http://user.wordpress.com/xmlrpc.php')
library(knitr)
knit2wp('yourfile.Rmd', title = 'Your post title')
{% endhighlight %}

Basically you set your login name and password as well as the url of the WordPress server (path to `xmlrpc.php`), so that **RWordPress** can send the post to the server. If you are a WordPress.com user, you may want to use the `shortcode` option, i.e. `knit2wp(..., shortcode = TRUE)`. See `?knit2wp` for details.

How to upload images? There are a few possibilities. One is to [upload images to Imgur](/knitr/demo/upload/), and another is to save images to your Dropbox folder, e.g.

{% highlight r %}
opts_knit$set(base.url = 'https://dl.dropbox.com/u/15335397/wp/',
              base.dir = 'path/to/Dropbox/Public/wp/')
{% endhighlight %}

I created a folder named `wp` under the `Public` folder, and the above code tells **knitr** to add a prefix to the image URL's, and generate images in the Dropbox folder. To get the URL of a file in the Public folder, just right-click on it and copy the link from the menu.

Note you can write and preview the draft in RStudio until you are comfortable to publish it. Once it is published, it is not straightforward to modify it (although you can), and that is why you, as a cool hacker, should blog with Jekyll instead of WordPress. It is always easy to deal with plain text files. Once you have got PHP, MySQL, password, plugins, ... things get complicated quickly.

If you have your own server, I recommend you not to use the `shortcode` option, and you should consider much nicer alternative options for syntax highlighting such as [highlight.js](http://softwaremaniacs.org/soft/highlight/en/), e.g. you can add this to your WordPress template:

{% highlight html %}
<link rel="stylesheet" href="http://yandex.st/highlightjs/7.3/styles/default.min.css">
<script src="http://yandex.st/highlightjs/7.3/highlight.min.js"></script>
<script src="http://yandex.st/highlightjs/7.3/languages/r.min.js"></script>
<script>hljs.initHighlightingOnLoad();</script>
{% endhighlight %}

I thank William K. Morris for his early experiment with **knitr** and WordPress. The `knit2wp()` function was based on [his blog post](http://wkmor1.wordpress.com/2012/07/01/rchievement-of-the-day-3-bloggin-from-r-14/). I also tested it [here](http://yihui.wordpress.com/).
