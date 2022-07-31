# Taller Final 


## punto 1 
calificacion <- 10
if (calificacion < 10) {
  print('�Hay mucho por mejorar!')
} else if (calificacion >= 10 & calificacion < 20){
  print("�Bien! Pero podr�a ser excelente.")
} else {
  print("�Excelente Servicio!")
  
}

### Punto 1.2:
### se le indican que hubo un cambio en el sistema de calificaciones, ya que permite desagregar la calificaci�n del restaurante 
### en 3 elementos: calidad de la comida, decoraci�n y servicio, cada uno en escala de 0 a 30.

### es necesario que se mantenga un indicador de calificaci�n conjunto. Y para ello,se  implementa una funci�n en R que reciba 3 
### argumentos -calidad, decoracion, servici.


### Calidad: 10
### Decoraci�n: 5
### Servicio: 15

calificacion<- function(calidad = 10, decoracion = 5, servicio = 15){
  total <- calidad + decoracion + servicio
  return(total)
}

calificacion(3,10,12)

#### PUNTO 2. Tidyverse ####

## Punto 2.1 Cargar Datos

install.packages("tidyverse")
library(tidyverse)

pr <- read_csv("price_ratings.csv")
rl <- read_csv("restaurant_locations.csv")

str(pr)
head(pr, 8)
str(rl)
head(rl, 8)

## ¿Ambos dataframes cumplen con que cada fila es una observación y cada columna es una variable?
##
## - Si, se puede observar al lado derecho de los dataframes en el espacio de Data, que cada una cuenta
##   con una cantidad distinta de observaciones ("pr - 672" y "rl - 168") pero con una misma cantidad
##   de variables ("3"). Sin embargo, para saber si son filas y columnas respectivamente, 
##   usamos el comando str(), que describe la estructura de nuestro dataframe, asi corroboramos que las 
##   las filas son observaciones, y que las columnas efectivamente son variables debido a su tamaño.
##   
##   Con str() tambien notamos que ambos dataframes tienen dos variables tipo num y una tipo chr
##
##   Al imprimir las 8 primeras filas de cada dataframe obtenemos que:
##
##    pr "price_ratings"      /             rl "restaurant_locations"     
##                            /      
##   Id   Variable   Valor    /   Id_restaurant    Restaurant           East
##    1   Price       43      /       1           Daniella Ristorante     0
##    1   Food        22      /       2           Tello's Ristorante      0
##    1   Decor       18      /       3           Biricchino              0
##    1   Service     20      /       4           Bottino                 0
##    2   Price       32      /       5           Da Umberto              0
##    2   Food        20      /       6           Le Madri                0
##    2   Decor       19      /       7           Le Zie                  0
##    2   Service     19      /       8           Pasticcio               1

## Punto 2.2 Pivot

pr <- pr %>%
  spread(key = Variable, value = Valor)

## Punto 2.3 Joins

DATA <- pr %>%
  inner_join(rl, by = c("Id" = "Id_restaurant"))

## Punto 2.4 Select y arrange

dataframe <- DATA %>%
  select(c(Restaurant, Price, Service)) %>%
  arrange(Price)

head(dataframe, 1)
tail(dataframe, 1)

## ¿Cuál es el restaurante más caro?

## - El restaurante más caro es San Domenico con un precio de 65 dólares

## ¿Cuál es el más barato?

## - El restaurante más barato es Lamarca con un precio de 19 dólares

## Punto 2.5 Group_by y Summarise

dataframe_2 <- DATA %>%
  group_by(East) %>%
  summarise(precio_prom = mean(Food))

dataframe_2

## ¿La comida es más cara en el lado Este o el lado Oeste de Manhattan?

## - la comida es más cara en el lado Este (1) de Manhattan, ya que en promedio
##   el valor de la comida es de 20.9 dólares, mientras que en el lado Oeste (0) es
##   de 20.1 aproximadamente.

#### PUNTO 3. Análisis Estadístico ####

## Punto 3.1 Regresión Lineal

regresion <- lm(Price ~ Food +
     Decor +
     Service +
     East, data = DATA)

summary(regresion)

## ¿Todas las variables son estadísticamente significaticas?

##  - No todas las variables son significativas, observamos a partir
##    del P-valor que la variable "Service", no es significativa a un
##    nivel de significancia de 5%, ya que su P-valor es de 0.9945.
##
##    Aunque ya dependiendo del nivel de rigurosidad, si tomamos una significancia
##    de 0.01, la variable "East" pasaría a ser no significativa al igual que la
##    variable "Service", ya que el P-valor de dichas variables supera el nivel de
##    significacncia (0.0304 y 0.9945 respectivamente).

## ¿Qué variable parece influir más en el precio de la comida?

##  - A un nivel de significancia de 0.05, la variable que parece influir más
##    en el precio de la comida, basandonos en el valor de los betas es "East".
##    Recordemos que es una variable que indica si el restaurante se ubica al 
##    este o al oeste de la Quinta Avenida.
##
##    Teniendo esto en cuenta, se puede deducir que dependiendo si el restaurante se encuentra
##    en el este de la quinta avenida o no, el precio puede llegar aumentar en 2.068050 unidades.
##    Averiguando al respecto, esto se puede atribuir a que en muchas ocasiones, 
##    el este de la quinta avenida de Manhattan está más desarrollado, por ende su precio es mayor.

#### PUNTO 4. Visualización de Datos ####

## Punto 4.1 Gráfico de Densidad

ggplot(DATA, aes(Service, linetype = as.factor(East))) +
  geom_density() +
  labs(
    title = "Gráfico de Densidad",
    x = "Servicio",
    y = "Densidad",
    linetype = "Este"
  )

## ¿Cuál zona de Manhattan tiende a tener mejor calificación de servicio?

## - Al ver el comportamiento de ambas zonas en el gráfico, podemos ver que la densidad de la zona Este
##   tiene un sesgo a la derecha, donde su pico es más alto en comparación con el Oeste. Por lo tanto,
##   la zona de Manhattan que tiende a tener una mejor calificación de servicio es el Este.

## Punto 4.2 Gráfico de Dispersión

ggplot(DATA, aes(x = Price, y = Decor, color = as.factor(East))) +
  geom_point() +
  scale_color_manual(
    values = c("blue", "green"),
    labels = c("Oeste", "Este")
  )

## Según el gráfico, ¿A qué zona de Manhattan pertenece el restaurante con menor precio?

## - El punto azul en la parte inferior izquierda da a entender que el restaurante con
##   menor precio se encuentra en la zona Oeste de Manhattan.











