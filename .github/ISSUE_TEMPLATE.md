For bug reports, please provide a minimal, self-contained, and reproducible example by reducing your example as much as possible right before the problem goes away. By doing this, you may be able to figure out what the problem really is before reporting to me. You can attach your example as a zip file here along with `devtools::session_info('knitr')`, and screenshots are often very helpful to illustrate your issues.

To include a verbatim chunk of arbitrary text, indent it by four spaces (this can be done in RStudio by selecting the text and pressing the `Tab` key once or twice, depending on your settings), especially when it contains backticks, e.g.

    A sample document.

    ```{r}
    1 + 1  # a line of code
    ```

    Another paragraph.

If it is just a chunk of R code (or other languages) and you want syntax highlighting, you may use three backticks to format it, e.g.

```r
rnorm(10)
```
