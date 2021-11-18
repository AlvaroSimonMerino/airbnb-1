De ahora en adelante, hasta que preguntemos en clase, la manera de trabajar será importando ficheros .Rmd

De momento AnalisisInicialAirbnb.Rmd es el fichero principal, desde el cual llamaremos a aquellos en los que trabajamos. Esto nos permitirá trabajar a la vez y en la rama main sin generar conflictos de código. Más adelante le daremos forma parecida a la Memoria que subió Álvaro.

Para empezar a trabajar con los datos ya en ficheros separados, hay que añadir a nuestro código:

library(knitr)
source(knitr::purl("AnalisisInicialAirBnb.Rmd")

Con esto tendremos los datos cargados y en su formato correspondiente.


Happy EDA!
