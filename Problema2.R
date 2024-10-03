#Codigo para problema 2 RECTA DE REGRESIÓN POR MÍNIMOS CUADRADOS

mis_dades <- iris
mis_dades

# en la sesión anterior vimos como trabajar con una variable sola
# (una columna de la tabla de datos, ahora trabajaremos con dos columnas
# (dos variables del experimento)

x <- mis_dades$Petal.Length
y <- mis_dades$Sepal.Length

# queremos ver qué le pasa al sépalo a medida que aumentamos la longitud
# del pétalo

plot(x,y)

# según los apuntes guardados en el ipad:

x_bar <- mean(x)
y_bar <- mean(y)

m <- sum((x-x_bar)*(y-y_bar))/sum((x-x_bar)^2)
b <- y_bar - m*x_bar

# la PREDICCIÓN de un valor de Petal.Length es el valor de y de la recta que he
# creado que mejor se ajusta a los datos que tengo

pred_1.5 <- m*1.5+b
pred_1.5

# vamos a pintar la recta de regresión en una misma gráfica que los
# puntos reales

x_pred <- 0:10
y_pred <- m*x_pred + b

plot(x,y) # para hacerlo con puntos
lines(x_pred, y_pred) # para hacer la recta que une los puntos

# ahora vamos a calcular el COEFICIENTE DE DETERMINACIÓN, que es una medida
# entre 0 y 1 que nos dice qué tan buena es la aproximación de la regresión
# lineal por mínimos cuadrados

x_pred2 <- x # para coger todos los datos y no solo el vector [0:10]
y_pred2 <- m*x_pred2 + b

Rsq <- sum((y_pred-y_bar)^2)/sum((y-y_bar)^2) # COEFICIENTE DE DETERMINACIÓN
cor <- sqrt(rSQ) # COEFICIENTE DE CORRELACIÓN

mod <- lm(y~x) # para crear la regresión lineal por mínimos
# cuadrados DIRECTAMENTE, te da b y m en ese orden

summary(mod) # Multiple R-squared es el R^2
cor.test(x,y) # para ver el coeficiente de correlación

predict(mod, data.frame(x=x)) # que prediga los valores de y en cada uno de
# los puntos observados

