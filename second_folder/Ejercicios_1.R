#ESTRUCTURAS DE DATOS EN R Ejercicio 1

# Ejercicio 1

# a) Genere la siguiente secuencia en R: 123123123123 

rep(c(1,2,3),4) 

# [1] 1 2 3 1 2 3 1 2 3 1 2 3

# b) Genere la siguiente secuencia en R: 10.00000 10.04545 10.09091 10.13636 
# 10.18182 10.22727 10.27273 10.31818 10.36364 10.40909 10.45455 10.50000

seq(10.0,10.5,length=12)

# [1] 10.00000 10.04545 10.09091 10.13636 10.18182 10.22727 10.27273 10.31818
# [9] 10.36364 10.40909 10.45455 10.50000


# c) Genere la siguiente secuencia en R: "1" "2" "3" "banana" "1" "2" "3" "banana"

rep(c(1,2,3, 'banana'),2)

# [1] "1"      "2"      "3"      "banana" "1"      "2"      "3"      "banana"

# Ejercicio 2

airquality
head(airquality)
dim(airquality) 

# Dataframe airquality tiene 6 variables (Ozone, Solar.R, Wind, 
# Temp, Month, Day) con 153 observaciones.

summary(airquality)

# Dataframe tiene 44 observaciones con elementos nulos (NA): 
# 37 en Ozone y 7 en Solar.R.

# Meses de observaciones son Mayo-Septiembre (5-9)


max(airquality$Temp[airquality$Month == 5]) # 81

# Temperatura máxima del viento en el mes de mayo estaba 81.



airquality$Ozone[is.na(airquality$Ozone)] <- 0 # NA transformados a 0.

mean(airquality$Ozone[airquality$Month == 7])


# Con NA transformados a 0 media del ozono en el mes de Julio estaba 49,58.

# Pero sin transformar NA a 0 (eliminar estos observaciones) - 59,12:
# mean(airquality$Ozone[airquality$Month == 7], na.rm = TRUE)


airquality$Month[airquality$Temp == max(airquality$Temp)] # 8 

# En el dataframe la temperatura fue mayor en el mes Agosto.

length(airquality[airquality$Temp > 90 & airquality$Ozone < 100, 5])

# Hubo un total de 13 observaciones donde la temperatura fue > 90 
# y el ozono < 100.

airquality$Solar.R[is.na(airquality$Solar.R)] <- 0


library(dplyr)
airquality %>% 
  group_by(Month) %>% 
  summarize(Mean_Ozone = mean(Ozone), Mean_Solar.R = mean(Solar.R), Mean_Wind = mean(Wind), Mean_Temp = mean(Temp) )

# Podemos concluir que en meses con mas altas tempaturas viento 
# es mas bajo (relación inversa). No hay una dependencia clara entre
# las variables Ozono, Temperatura y Radiación Solar. Solo se puede ver
# que en dos meses con mas alta temperatura media (Agosto, Septiembre) también 
# fueron los mayores exponentes medias de la variable Ozone.


# Ejercicio 3

# La función de Fibonacci, que permite introducir el valor máximo 1000.

fib <- function(n) {
  if (n <= 1000) {
  if (n < 2)
    n
  else
    fib(n - 1) + fib(n - 2) }
}

fib(23) # 28657

fib(1001) # como 1001 > 1000, no calcula la funcion




