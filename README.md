# Predicción de precios de AirBnbs mediante Regresión Lineal Multivariante

## Práctica Final de la asignatura "Programación Orientada a Ciencia de Datos" del Máster en Data Science de la Universidad Rey Juan Carlos

## Autores: Alvaro Simón Merino, Katsiaryna Zaitsava, Antonio Fernández Cáceres

### Entregada el 14 de Enero de 2022


Para la realización de esta práctica se ha seleccionado el dataset *listings.csv* (Airbnb Madrid, 2021-09-10), un conjunto de datos obtenido de [insideairbnb.com](http://insideairbnb.com/).
Puede ser descargado a través de este [enlace](http://data.insideairbnb.com/spain/comunidad-de-madrid/madrid/2021-09-10/data/listings.csv.gz).

El objetivo general de esta práctica es proponer un modelo de regresión lineal multivariante para predecir el precio por noche de un espacio ofertado en la plataforma *AirBnb* y situado en Madrid.
<br/>

Dicho objetivo, a su vez se dividirá en los siguientes pasos:

1.  Selección preliminar de variables.
2.  Separación del conjunto de datos en dos grupos: Training (70% de los datos) y Test (30% de los datos)
3.  Realización de un análisis exploratorio univariante de los datos.
4.  Estudio e imputación de datos faltantes.
5.  Realización de un análisis exploratorio multivariante de los datos.
6.  Transformaciones necesarias a cada una de las variables para poder ser utilizadas en la regresión.
7.  Ajuste, aplicación y evaluación de un modelo de regresión lineal múltiple con las variables seleccionadas para la predicción de la variable *price*.


El código en R se encuentra en el fichero *AirBNB-Regresion_Lineal.Rmd*, y el análisis está exportado en *AirBNB-Regresion_Lineal.html*.


Calificación obtenida: 9/10