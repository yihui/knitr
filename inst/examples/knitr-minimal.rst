A reST document for knitr
=========================

This is a reStructuredText document. The input filename extension is ``Rrst``
and the output filename will be ``rst``. Here is how we write R code in
**knitr**:



::

    options(width = 75)
    render_rst(strict = TRUE)  # do not use the sourcecode directive
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

    ##  [1] -1.2721 -0.7205 -0.5392  2.0504  0.2321 -0.8445 -0.1221 -0.6043
    ##  [9] -0.7840  1.1898



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


.. figure:: http://i.imgur.com/wvTsc.png
    :alt: A ggplot2 example
    :width: 360px


    A ggplot2 example



Inline R code is like this: the value of pi is 3.1416.

