## Frequently Asked Questions

The FAQ's are compiled from the [issues](https://github.com/yihui/knitr/issues) and messages I received from blog posts and emails, etc.

1. **knitr** does not work...
  - Please first update all your R packages (use `update.packages()`) and probably R itself ([what is the current R version?](http://cran.r-project.org/)), then see if it works; if not, file an [issue](https://github.com/yihui/knitr/issues) to me (email is fine, too).
1. What is the best place to ask questions when the [package website](http://yihui.name/knitr) is not helpful?
  - Depending what you want to ask, you may use these tools (I keep track of the first three more frequently):
  - [issues page](https://github.com/yihui/knitr/issues): bug reports and feature requests
  - [knitr mailing list](https://groups.google.com/group/knitr) or R-help list: general questions and feedback via email in public; you can also email me in private (see `packageDescription('knitr', fields = 'Maintainer')`)
  - [StackOverflow](http://stackoverflow.com/questions/tagged/knitr): general questions (more public than emails)
  - Twitter ([`@xieyihui`](http://twitter.com/xieyihui)) or Google+ ([`+Yihui Xie`](https://plus.google.com/u/0/109653178371807724268/posts))
1. Oh, the tons of arguments like `Sweave(..., prefix.string=abc, keep.source=FALSE, foo=bar)` are really flexible; why `knit()` only has so few arguments?
  - Because I believe putting these arguments in `knit()` breaks the principle of reproducibility, and so does using environmental variables (see [#19](https://github.com/yihui/knitr/issues/19) for details).
1. I love RStudio and Sweave is sweet there; is **knitr** going to work with RStudio?
  - I love RStudio too, and yes, **knitr** is going to be supported in RStudio thanks to RStudio developers; currently you can try the [development version](http://www.rstudio.org/download/daily/) as long as you understand what a development version means. By the way, I also recommend LyX if you want to use Sweave or **knitr**.
1. You mentioned LyX so many times, so what the heck is LyX?
  - It is an intelligent wrapper for LaTeX; see http://www.lyx.org for details. I would like to define it as a software package that can both increase the productivity of an _experienced_ LaTeX user by 300%, and decrease it by 500% for a LaTeX novice. Don't use it simply because its GUI is so tempting; it is not MS Word. I have added support for **knitr** in LyX; see [the lyx demo page](http://yihui.name/knitr/demo/lyx/).
1. Where are those prompt characters `>` and `+`? I feel uncomfortable reading R output without them.
  - They are removed by default, because I believe they make no sense. This is the reason why I dislike books on R which used `>` and `+`; they twist my mind and make my eyes bleed when I read the R code in the books. For those who really want to read R code like `> 1+1` instead of `1 + 1`, you have the [chunk option](http://yihui.name/knitr/options) `prompt`.
1. What is the working directory? Can I change my working directory in my code chunks?
  - You'd better not do this. Your working directory is always `getwd()` (all output files will be written here), but the code chunks are evaluated under the directory where your input document comes from. Changing working directories while running R code is a bad practice in general. See [#38](https://github.com/yihui/knitr/issues/38) for a discussion. You should also try to avoid absolute directories whenever possible (use relative directories instead), because it makes things less reproducible.
1. The gray (shading) box is too narrow for my output.
  - No, it is not because the box is too narrow (the box uses the current line width); it is because your output is too wide. Use a smaller `width` option to avoid output exceeding the page margin, e.g. `options(width = 60)`; see [#44](https://github.com/yihui/knitr/issues/44).
1. LaTeX told me there was an error related to `\end{kframe}`.
  - `kframe` is a LaTeX environment that I defined for the chunk output, and it does not work well with certain other LaTeX environments, e.g. the `center` environment; when you want to center your plots, you should use chunk option `fig.align='center'` instead of putting the whole chunk inside `\begin{center}` and `\end{center}`.
1. How to comment out inline R code like in `\Sexpr{code}`?
  - see issue [#110](https://github.com/yihui/knitr/issues/110): `%\%Sexpr{code}` or `\Sexpr{#code}`
1. I have done something cool with **knitr**; could you add a link in your website?
  - Sure! I'd love to; just let me know.
1. What can I do for you?
  - Many things, e.g. donate me zillions of money (well, I'm kidding), buy me a book from my [Amazon wishlist](http://amzn.com/w/2S7M0GLEC32SB), [tweet](https://twitter.com/xieyihui) my [links](http://yihui.name/knitr), mention **knitr** on [Google+](https://plus.google.com/u/0/109653178371807724268/posts) or Facebook, or fork this repository and contribute code, or just say hello to me somewhere
