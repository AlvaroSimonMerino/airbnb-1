De ahora en adelante, hasta que preguntemos en clase, la manera de trabajar será importando ficheros .Rmd

De momento AnalisisInicialAirbnb.Rmd es el fichero principal, desde el cual llamaremos a aquellos en los que trabajamos. Esto nos permitirá trabajar a la vez y en la rama main sin generar conflictos de código. Más adelante le daremos forma parecida a la Memoria que subió Álvaro.

Para empezar a trabajar con los datos ya en ficheros separados, lo único que hay que hacer es abrir AnalisisInicialAirBnb.Rmd, ejecutar todo o hacer knit, y con esto tendremos los datos cargados (variable 'data) y en su formato correspondiente en el espacio de trabajo. Si no deseáis hacer esto más de una vez, guardad el .RData


Intentemos no modificar más 'data'. Cuando hagamos Train/Test o imputemos valores, la renombraremos.

Tareas por hacer:
1.- Realizar una descripción precisa de los objetivos de esta práctica (en curso)
2.- Encontrar outliers, sobre todo en $price$.
3.- Decidir qué se hace con los valores missing.
4.- Hacer una función que divida los sets en Train/Test, en un 70%/30% para todos los id de cada neighbouhood_group_cleansed.
	MUCHO OJO: esta función se puede programar ya, pero no se implementará para analizar hasta que los outliers y missings hayan sido tratados.


Happy EDA!
