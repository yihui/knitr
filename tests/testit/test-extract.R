library(testit)

assert(
  'html_to_r() extracts R code within ```R decorator',
  identical(
    {
      rmarkdown_html <- "<html><body>Intro
      ```R\n1 * 1\n```\nmore text\n</body></html>\n"

      html_to_r(rmarkdown_html)

    },

    "1 * 1")
)



assert(
  'html_to_r() extracts R code within ```r decorator',
  identical(
    {
      rmarkdown_html <- "<html><body>more text\n```r\n2 * 2\n```
      some more text\n</body></html>\n"

      html_to_r(rmarkdown_html)

    },

    "2 * 2")
)



assert(
  'html_to_r() extracts R code within ``` decorator',
  identical(
    {
      rmarkdown_html <- "<html><body>some more text\n```\n3 * 3\n```\nThe End.
      </body></html>\n"

      html_to_r(rmarkdown_html)

    },

    "3 * 3")
)



assert(
  'html_to_r() extracts R code from R Markdown HTML file containing a varity of decorators',
  identical(
    {
      rmarkdown_html <- "<html><body>Intro
      ```R\n1 * 1\n```\nmore text\n```r\n2 * 2\n```
      some more text\n```\n3 * 3\n```\nThe End.
      </body></html>\n"

      html_to_r(rmarkdown_html)

    },

    "1 * 1\n\n2 * 2\n\n3 * 3")
)




