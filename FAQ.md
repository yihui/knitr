## Frequently Asked Questions

This FAQ is compiled from the [issues](https://github.com/yihui/knitr/issues) and messages I received from blog posts and emails, etc.

1. **knitr** does not work...
  - Please first update all your R packages (use `update.packages()`) and probably R itself ([what is the current R version?](http://cran.r-project.org/)), then see if it works; if not, please file an [issue](https://github.com/yihui/knitr/issues) to me.
1. What is the best place to ask questions when the [package website](http://yihui.name/knitr) is not helpful?
  - Depending what you want to ask, you may use these tools (I keep track of the first three more frequently):
  - (Recommended) [StackOverflow](http://stackoverflow.com/questions/tagged/knitr): general questions (more experts and quicker answers there).
  - [Github issues](https://github.com/yihui/knitr/issues): bug reports and feature requests.
  - [knitr mailing list](https://groups.google.com/group/knitr) or [R-help](http://www.r-project.org/mail.html) list: general questions and feedback via email in public.
  - Private email is the least recommended way unless there are really private issues.
  - Twitter ([`@xieyihui`](http://twitter.com/xieyihui)) or Google+ ([`+Yihui Xie`](https://plus.google.com/u/0/109653178371807724268/posts)).
1. How should I get started?
  - Watch the [5-min video](http://www.screenr.com/qcv8) on the [homepage](http://yihui.name/knitr).
1. What is the best editor for writing **knitr** source documents?
  - For beginners, perhaps [RStudio](http://www.rstudio.com/ide/); **knitr** is also supported in [LyX](http://yihui.name/knitr/demo/lyx/), [Emacs/ESS](http://ess.r-project.org/), WinEdt, Tinn-R and a variety of [editors](http://yihui.name/knitr/demo/editors/).
1. Where are those prompt characters `>` and `+`? I feel uncomfortable reading R output without them.
  - They are removed by default, because [I believe they make no sense](http://yihui.name/en/2013/01/code-pollution-with-command-prompts/). This is the reason why I dislike books on R which used `>` and `+`; they twist my mind and make my eyes bleed when I read the R code in the books. For those who really want to read R code like `> 1+1` instead of `1 + 1`, you have the [chunk option](http://yihui.name/knitr/options) `prompt`.
1. What is the working directory? Can I change my working directory in my code chunks?
  - You'd better not do this. Your working directory is always `getwd()` (all output files will be written here), but the code chunks are evaluated under the directory where your input document comes from. Changing working directories while running R code is a bad practice in general. See [#38](https://github.com/yihui/knitr/issues/38) for a discussion. You should also try to avoid absolute directories whenever possible (use relative directories instead), because it makes things less reproducible.
1. The gray (shading) box is too narrow for my output.
  - No, it is not because the box is too narrow (the box uses the current line width); it is because your output is too wide. Use a smaller `width` option to avoid text output exceeding the page margin, e.g. `options(width = 60)` (see [#44](https://github.com/yihui/knitr/issues/44)).
1. How to comment out inline R code like in `\Sexpr{code}`?
  - see issue [#110](https://github.com/yihui/knitr/issues/110): you can destroy `\Sexpr` by `%\%Sexpr{code}` or comment out R code like `\Sexpr{#code}`, or just comment out the whole paragraph with `%` in the beginning of lines.
1. How can I write a literal code chunk? i.e. write a code chunk which is not parsed and useful for tutorials?
  - You need to destroy the chunk header, e.g. add an empty string before the chunk header: `\Sexpr{''}<<label, what=ever>>=`, or ```` `r ''` ```{r label, what=ever} ```` ([#443](https://github.com/yihui/knitr/issues/443)); see [example 062](https://github.com/yihui/knitr-examples)
1. I have done something cool with **knitr**; could you add a link in your website?
  - Sure! I'd love to; just let me know.
1. What can I do for you?
  - Many things, e.g. [donate zillions to me](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=ZRJDEBSEJEUJY&lc=US&item_name=Donate%20to%20Yihui%20Xie&currency_code=USD&bn=PP%2dDonationsBF%3abtn_donateCC_LG%2egif%3aNonHosted) (well, I'm kidding), buy me a book from my [Amazon wishlist](http://amzn.com/w/2S7M0GLEC32SB), [tweet](https://twitter.com/xieyihui) my [links](http://yihui.name/knitr), mention **knitr** on [Google+](https://plus.google.com/u/0/109653178371807724268/posts) or Facebook, or fork this repository and contribute code, or just say hello to me somewhere.
  - Cite the package and the [knitr book](http://www.crcpress.com/product/isbn/9781482203530); see `citation('knitr')` in R.
