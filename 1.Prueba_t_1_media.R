#Prueba de t para una media con una muestra de n observaciones independientes

#A continuación se introducen los datos correspondientes al peso de 10 ratas 
#macho de 3 meses, seleccionadas aleatoriamente de una población.  
#Como hipótesis nula  se tiene mu población= 400 g, que es un valor fijo que 
#define al peso sano de las ratas macho sanas de esa edad.

peso_ratas<-c(394, 398, 393, 398, 412, 394, 413, 389, 393, 397)

#La prueba de t se establece de la siguiente manera, indicando "two.sided" 
#por ser una prueba bilateral

t.test(peso_ratas, mu=400, alternative="two.sided")

#El valor t calculado, el cual tiene signo negativo, se contrasta contra
#el valor t crítico con signo negativo para alfa/2 = 0.025

qt(0.025,df=9, lower.tail=TRUE)

#El valor p es mostrado en la salida de t.test. Este se puede conocer con la función pt
pt(-0.74531, df=9, lower.tail=TRUE) # el área desde -t hacia -infinito
pt(0.74531, df=9, lower.tail=FALSE) #el área desde +t hacia +infinito
#Se suman las dos áreas, teniendo así al valor p de la prueba, el cual coincide
#con el mostrado por la salida t.test
p_value<-pt(-0.74531, df=9, lower.tail=TRUE)+ pt(0.74531, df=9, lower.tail=FALSE)
p_value
#Ya que p_value es > alfa (0.05), no debe rechazarse la hipótesis nula.