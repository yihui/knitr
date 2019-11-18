# knitr

[![Build Status](https://travis-ci.org/yihui/knitr.svg)](https://travis-ci.org/yihui/knitr)
[![Coverage Status](https://codecov.io/gh/yihui/knitr/branch/master/graph/badge.svg)](https://codecov.io/github/yihui/knitr?branch=master)
[![Downloads from the RStudio CRAN mirror](http://cranlogs.r-pkg.org/badges/knitr)](https://cran.r-project.org/package=knitr)

El paquete de R **knitr** es un motor de programación literaria de uso genérico, con una API ligera diseñada para dar a los usuarios control total de la salida sin un trabajo de codificación pesado. Combina muchas características dentro de un paquete con leves ajustes motivados de mi uso diario de Sweave. Observe la [página principal](https://yihui.name/knitr/) del paquete para más detalles y ejemplos. Vea las [FAQ](https://yihui.name/knitr/faq/) para una lista de preguntas frecuentes (que incluye dónde hacer preguntas).


## Instalación

Puede instalar la versión estable en [CRAN](https://cran.r-project.org/package=knitr):

```r
install.packages('knitr')
```

También puede instalar la versión en desarrollo desde [XRAN](https://xran.yihui.org), la cual provee construcciones diarias de **knitr**:

```r
# primero actualice todos los paquetes existentes
update.packages(ask = FALSE, repos = 'http://cran.r-project.org')
install.packages('knitr', repos = c('https://xran.yihui.org', 'http://cran.r-project.org'))
```

O usar **devtools** para instalar la versión en desarrollo desde GitHub:

```r
devtools::install_github('yihui/knitr', build_vignettes = TRUE)
```


## Motivación

Mientras Sweave y paquetes adicionales relacionados como [**cacheSweave**](https://cran.r-project.org/package=cacheSweave) y [**pgfSweave**](https://cran.r-project.org/package=pgfSweave) son motores bastante buenos en programación literaria en R, sin embargo frecuentemente siento que mis manos se cansan, por ejemplo:

- comencé con el código fuente de Sweave y deseé cientos de veces *si solo pudiera insertar fácilmente*  `[width=.8\textwidth]` *entre* `\includegraphics` *y* `{my-plot.pdf}` (la manera oficial en Sweave es `\setkeys{Gin}` pero se ajusta con una amplitud global, la cual no es realista dado que frecuentemente tenemos que fijar la amplitud individualmente; sí, puede usar `\setkeys{Gin}` muchas veces, pero ¿por qué no proporcionar una sola opción para cada trozo de código?)
- deseé muchas veces *si tan solo pudiera usar dispositivos gráficos distintos a PDF y postscript*; ahora el sueño se ha vuelto realidad en el R oficial, pero lo que estaba esperando era un opción tan simple como `dev = 'png'` o `dev  = 'CairoJPEG'`
- deseé que pudiera grabarse múltiples gráficos en un trozo de código en lugar de solo el último de ellos
- deseé que hubiera una forma de redondear los números en `\Sexpr{}` diferente a escribir expresiones como `\Sexpr{round(x, 3)}` para *cada* `\Sexpr{}`
- deseé que no tuviera que imprimir (`print()`) gráficos desde [**ggplot2**](https://cran.r-project.org/package=ggplot2) y que un simple `qplot(x, y)` me devolviera un gráfico en Sweave
- deseé que los usuarios no necesitasen instrucciones sobre `Sweave.sty` o se encontraran con problemas debido al hecho de que LaTeX no encuentra `Sweave.sty`
- deseé que **cacheSweave** pudiera imprimir los resultados de un trozo de código incluso si este estaba almacenado en caché
- deseé que [**brew**](https://cran.r-project.org/package=brew) soportase gráficos
- deseé que [**R2HTML**](https://cran.r-project.org/package=R2HTML) soportase el resaltado de sintaxis de código R
- ...

[<img src="http://i.imgur.com/yYw46aF.jpg" align="right" alt="The book Dynamic Documents with R and knitr" />](http://amzn.com/1498716962)

El paquete **knitr** se diseñó para dar al usuario acceso a todas las partes del proceso de lidiar con un documento de programación literaria, de forma que no haya necesidad de modificar cualquiera de los componentes centrales si quiere mayor libertad. He estudiado el código fuente de **pgfSweave** y **cacheSweave** un par de veces, y frecuentemente me siento incómodo con la gran cantidad de código copiado del R oficial, especialmente cuando R tiene una nueva versión liberada (empezaré a preocuparme si los paquetes adicionales todavía están actualizados respecto al Sweave ocifial).


## Uso 

```r
library(knitr)
?knit
knit(input)
```

If options are not explicitly specified, **knitr** will try to guess
reasonable default settings. A few manuals are available such as the [main
manual](https://yihui.name/knitr/demo/manual/), and the
[graphics
manual](https://yihui.name/knitr/demo/graphics/). For a
more organized reference, see the [knitr book](http://amzn.com/1498716962).

Si no se especifican opciones explícitamente, **knitr** tratará de suponer una configuración por defecto razonable. Hay disponibles unos pocos manuales, tales como el [manual principal](https://yihui.name/knitr/demo/manual/) y el [manual de gráficos](https://yihui.name/knitr/demo/graphics/). Para una referencia más organizada, vea el libro de [knitr](http://amzn.com/1498716962).


## Licencia

Este paquete es software libre y de código abierto, licenciado bajo GPL.
