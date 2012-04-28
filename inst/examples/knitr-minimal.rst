A reST document for knitr
=========================

This is a reStructuredText document. The input filename extension is ``Rrst``
and the output filename will be ``rst``. Here is how we write R code in
**knitr**:



::

    options(width = 75)
    render_rst(strict = TRUE)
    opts_knit$set(upload.fun = imgur_upload)  # upload images
    opts_chunk$set(cache = TRUE, cache.path = "cache/rst-", fig.path = "figure/rst-", 
        fig.width = 5, fig.height = 5)  # global chunk options




More examples
-------------

A code chunk begins with ``.. {r label, options}``, and ends with ``.. ..``
(note the space in between). Optionally you can precede all R code with two
dots, e.g.



::

    1 + 1



::

    ## [1] 2



::

    rnorm(10)



::

    ##  [1] -0.1835 -0.4287  1.1259 -0.1039 -0.6829  0.4316  1.1405 -0.2924
    ##  [9] -0.4319 -0.5229



::

    warning("do not forget the space after ..!")



::

    ## Warning message: do not forget the space after ..!




Here is a plot:



::

    library(ggplot2)
    qplot(hp, mpg, data = mtcars) + geom_smooth()



::

    ## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.


.. figure:: http://i.imgur.com/5trZl.png
    :alt: A ggplot2 example
    :width: 360px


    


Inline R code is like this: the value of pi is ``3.1416``.

