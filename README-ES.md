# knitr

[![Build Status](https://travis-ci.org/yihui/knitr.svg)](https://travis-ci.org/yihui/knitr)
[![Coverage Status](https://coveralls.io/repos/yihui/knitr/badge.svg?branch=master&service=github)](https://coveralls.io/github/yihui/knitr?branch=master)
[![Downloads from the RStudio CRAN mirror](http://cranlogs.r-pkg.org/badges/knitr)](https://cran.r-project.org/package=knitr)

The paquete R **knitr** es un motor de programación literaria de proposito general, 
con un diseño de API ligero, que da a los usuarios control total de la salida
sin un trabajo de codificación pesado. Combina muchas características dentro de un paquete 
con leves ajustes motivados de mi uso diario de Sweave. Ve el paquete en [homepage](http://yihui.name/knitr) para detalles y ejemplos. Mira [FAQ's](https://github.com/yihui/knitr/blob/master/FAQ.md) para una lista de 
preguntas frecuentes (incluyendo donde hacer las preguntas).

##Instalación

Puedes instalar la versión estable en 
[CRAN](https://cran.r-project.org/package=knitr):

```r
install.packages('knitr', dependencies = TRUE)
```

También puedes la versión de desarrollo de [XRAN](http://yihui.name/xran/), la cual provee construcciones diarias de 
**knitr**:

```r
# update all existing packages first
update.packages(ask = FALSE, repos = 'http://cran.rstudio.org')
install.packages('knitr', repos = c('http://yihui.name/xran', 'http://cran.rstudio.org'))
```

O usar **devtools** para instalar la versión de desarrollo desde GitHUb:

```r
devtools::install_github('yihui/knitr', build_vignettes = TRUE)
```

##Motivación

Mientras Sweave y paquetes relacionados como [**cacheSweave**](https://cran.r-project.org/package=cacheSweave) y [**pgfSweave**](https://cran.r-project.org/package=pgfSweave) son bastante buenos motores para la programación literaria en R, sin embargo frecuentemente siento que mis manos se cansan,
por ejemplo:

- Comencé con el código fuente de Sweave y deseaba cientos de veces
  que *si sólo pudiera insertar fácilmente*  `[width=.8\textwidth]` *entre* `\includegraphics` *y* `{my-plot.pdf}` (la manera oficial en Sweave es
  `\setkeys{Gin}` pero se coloca una amplitud global la cual no es realista.
  por ello debemos colocar las anchuras individualmente; si, puedes usar `\setkeys{Gin}` muchas veces, pero ¿Por qué no sólo proporcionar una opción para cada pedazo?)
- Me hubiera gustado muchas veces que *si tan solo pudiera usar otros dispositivos gráficos
  como PDF y postscript*; ahora los sueños se vuelven realidad en el R oficial,
  pero lo que estaba esperando era un opción simple como `dev = 'png'` or `dev
  = 'CairoJPEG'`
- Me hubiera gustado múltiples -------plots--------- en un código ---chunk---- que pudieran grabarse en vez de     
  sólo el último
- Me hubiera gustado que hubiera una manera de redondear los números en `\Sexpr{}` diferente
  escribiendo expresiones como `\Sexpr{round(x, 3)}` para *cada* 
  `\Sexpr{}`
- Me hubiera gustado no tener que `print()` gráficas desde
  [**ggplot2**](https://cran.r-project.org/package=ggplot2) y un simple 
  `qplot(x, y)` me diera  una gráfica en Sweave
- Deseaba que los usuarios nunca necesitaran instrucciones en `Sweave.sty` o encontrarse
  problemas debido al hecho de que LaTex no pueden encontrar `Sweave.sty`
- Deseaba **cacheSweave** pudiera imprimir los resultados de un código chunk incluso
  si este era almacenado en caché
- Deseaba que [**brew**](https://cran.r-project.org/package=brew) pudiera soportar
  gráficas
- Deseaba que [**R2HTML**](https://cran.r-project.org/package=R2HTML)
  pudiera soportar sintaxis destacada de código R
- ...


[<img src="http://i.imgur.com/yYw46aF.jpg" align="right" alt="The book Dynamic Documents with R and knitr" />](http://amzn.com/1498716962)

El paquete **knitr*+ fue diseñado para dar al usuario acceso a todas las partes del proceso a tratar con un documento de literate programming, para que no haya
necesidad de cortar cualquiera de los componentes básicos si quieres más libertad. He pasado el código fuente de **pgfSweave** y **cacheSweave** un par de veces, y frecuentemente me siento inconforme con la gran cantidad de código copiado del programa oficial R, especialmente cuando R tiene una nueva versión se libera (Empezaré a preocuparme si los paquetes add-on permanecen actualizado con el Sweave oficial).

## Uso 

```r
library(knitr)
?knit
knit(input)
```

Si los paquetes no están especificados explicitamente, **knitr** tratará de hacer las configuraciones por default razonables. Pocos manuales están disponibles, como el [main manual](http://yihui.name/knitr/demo/manual/), y el [graphics manual](http://yihui.name/knitr/demo/graphics/). Para una referencia más organizada, ve el [knitr book](http://amzn.com/1498716962).

##Licensia

This package is free and open source software, licensed under GPL.




