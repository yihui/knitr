For general questions, please ask them on StackOverflow first, using the tags `r` and `knitr`: http://stackoverflow.com/questions/ask Please come back here only if nobody answers your question there, and let me know the URL of your StackOverflow post. It is a better idea to use the wisdom of the community than the limited time of a single person.

For bug reports, please provide a minimal, self-contained, and reproducible example by reducing your example as much as possible right before the problem goes away. By doing this, you may be able to figure out what the problem really is before reporting to me. You can attach your example as a zip file here along with `devtools::session_info('knitr')`, and screenshots are often very helpful to illustrate your issues.

To include a verbatim chunk of arbitrary text, wrap it in a pair of three backticks. When any line of your text contains N backticks (N >= 3), use N + 1 backticks to wrap the text, e.g. use four backticks to wrap three:

````
A sample document.

```{r}
1 + 1  # a line of code
```

Another paragraph.
````

If it is just a chunk of R code (or other languages) and you want syntax highlighting, you may use three backticks to format it, e.g.

```r
rnorm(10)
```

Usually your issue will be closed after it is fixed, but sometimes it is closed only because I'm not able to offer any help. It does not mean your issue is not real or bad. You can propose a fix by yourself through a pull request. Your constructive feedback is always appreciated.
