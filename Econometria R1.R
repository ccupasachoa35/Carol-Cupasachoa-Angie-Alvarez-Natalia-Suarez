#c3.1
##i) ¿Cual es el signo más probable para b2?
###el signo mas probable para b2 seria positivo, debido a que el aumento de un nivel de ingresos mejoraria el peso del bebe al nacer


##ii) ¿Cree que cigs y faminc estén correlacionados? Explique por qué la correlación puede ser positiva o negativa.
cor(bwght$faminc,bwght$cigs)
###cigs y faminc tienen una correlacion negativa, ya que por lo general las madres con un mayor
###nivel de ingresos tienen estilos de vida mas saludables por lo cual tienden a no fumar en el embarazo.


##iii) Ahora, calcule la ecuación con y sin faminc utilizando los datos del archivo BWGHT.
###RAW. Dé los resultados en forma de ecuación incluyendo el tamaño de la muestra y la Rcuadrada. 
###Explique sus resultados enfocándose en si el añadir faminc modifica de manera  sustancial el efecto esperado de cigs sobre bwght.


library(wooldridge)
data("bwght")
#Faminc = nivel ingreso de la madre
#cigs = promedio cigarrillos al dia en el embarazo
#bwght= peso del bebé al nacer libras

#Con Faminc
c3.1 <- lm(bwght ~ cigs +faminc, data=bwght)
summary(c3.1)

#sin Faminc
c3.11 <- lm(bwght ~ cigs, data=bwght)
summary(c3.11)

###RTA: En la ecuación con faminc se evidencia que por cada aumento en un cigarrillo al día durante el embarazo, se espera que el 
###peso del bebé al nacer disminuya en 0.46341 libras, teniendo constante el nivel de ingreso de la madre. Respecto al coeficiente 
###de faminc, este indica que por cada aumento en el nivel de ingreso de la madre, se espera que el peso al nacer del bebé aumente
###en 0.09276 libras, con el número constante de cigarrillos al día durante el embarazo.
###En el modelo sin faminc se espera el mismo efecto de disminución del peso del bebé al nacer, uqe en este caso disminuyo aún más 
###a 0.51377 libras. 

###En cuanto a la significacncia vemos que el aumento de cigarrillos y del nivel de ingresos son significativos en el peso del bebé 
###al nacer porque el valor de P es menor que el 0.05 establecido.

###Por último, con el coeficiente de determinación se ve que el modelo con el Faminc, R2 ajustado es 0.0284, mientras que en el 
###modelo sin faminc es 0.02202. lo que indica que el modelo que incluye faminc explica un poco más de la variabilidad en el peso 
###al nacer que el modelo sin esta variable.



#c3.2
##Utilice los datos del archivo HPRICE1.RAW para estimar el modelo
data("hprice1")
summary(hprice1)

c3.2 <- lm(price ~ sqrft + bdrms, data = hprice1)
summary(c3.2)


#i) Escriba los resultados en forma de ecuación.
### price= -19.31500 + 0.12844 sqrft + 15.19819 bdrms 


#ii) ¿Cual es el incremento en precio estimado para una casa con una habitación (bdrms) más, 
#manteniendo constante la superficie en pies cuadrados (sqrft)?
### si la variacion de la superficie en pies cuadrados (sqrft) se mantiene constante, quiere decir
### que solo tiene efecto la variacion de las habitaciones, por ende, por cada habitacion adicional en la casa
### el precio de la casa aumentara en 15.198 dolares.


#iii) ¿Cual es el incremento en precio estimado para una casa con una habitación adicional de 
#140 pies cuadrados? Compare esto con su respuesta al inciso (ii).

price.hmas = (0.128436*140) + (15.19819*1)
view(price.hmas)

### Una habiatacion adicional de 140 pies cuadrados tiene un incremento en el precio de 33.179 dolares. 
## Con respecto al punto anterior, en este caso el precio es un poco más del doble considerando que el tamaño de la casa tambien 
##aumento, mientras que en el caso anterior se aumentaba una habitacion mas por un menor aumento en el precio, sin embargo el 
##tamaño de la casa seguía siendo el mismo.


#iv) ¿Qué porcentaje de la variación en el precio se explica por la extensión en pies cuadrados 
#y el número de habitaciones?

summary(c3.2)
## A partir del coeficiente de determinacion R^2 se puede observar que el porcentaje de la variacion 
## del precio es de 63,19% 


#v) La primera casa en la muestra tiene sqrft= 2,438 y bdrms = 4. Determine el precio de 
#venta estimado para esta casa con la línea de regresión de MCO.

#MCOprice = (-19.31500 + 0.128436sqrft + 15.19819bdrms)
MCOprice = (-19.31500 + (0.128436 * 2438) + (15.19819 * 4))
view(MCOprice)
###El precio estimado para la casa con la linea de regresion MCO es de $354.604 dolares


#vi) El precio de venta de la primera casa en la muestra fue $300,000 (así que price = 300). 
#Determine el residual para esta casa. ¿Sugiere esto que el comprador pagó de más o de 
#menos por la casa?

Residual= (300-MCOprice)
view(Residual)

## El comprador de la primera casa pago $54,604.728 menos de lo que se esperaba, debido a que el precio 
## de la casa estaba sobreevaluado.
