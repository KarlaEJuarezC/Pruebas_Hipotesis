#Ejemplo de datos pareados
sin_instrucciones<-c(5,9,5,9,6,11,8,11,7,9,8,16,10,12,7,9,21,14,12,13)
con_instrucciones<-c(20,24,20,18,22,19,20,19,17,21,17,20,20,16,24,22,25,21,19,23)
median(sin_instrucciones); median(con_instrucciones) #Calcular medianas
calificaciones<-c(sin_instrucciones,con_instrucciones)
instruccion<-c(rep("sin",20), rep("con",20)) #Generar vector con sin y con repetida 20 veces cada una
datos2<-data.frame("Calificaciones"=calificaciones, "Instruccion"=instruccion) #Guardar data frame
write.csv(datos2, "datos2.csv") #Exportar a un .csv

#Prueba de t 
t.test(con_instrucciones,sin_instrucciones, alternative="greater", paired=T )
#Para conocer los l?mites del intervalo de confianza para la media de las diferencias
t.test(con_instrucciones,sin_instrucciones, alternative="two.sided", paired=T )

Media_Diferencias<-10.25
Limite_superior<-12.14577 # Este valor es obtenido en la salida de t.test
Limite_inferior<-8.35423 # Este valor es obtenido en la salida de t.test
MargenError<-(Limite_superior-Limite_inferior)/2  #Para calcular el margen de error

#Se genera una gr?fica en donde se coloca como un punto al valor estimado para la media de las diferencias
#Como l?mites del eje y se establecen valores dentro de los que quepan los l?mites del intervalo de confianza
plot(1,Media_Diferencias, axes=F, pch=16, ylim=c(-1,13), ylab="Diferencia en Calificaciones", xlab="Estudiantes del grupo x", font.lab=2)
axis(2, at=seq(-1,13, by=1)) #se genera una secuencia en el eje y que va de 1 en 1
abline(h=0, lty=2, col="red")# lty = 2 discontinua
abline(h=-1, lty=1, col="black") #lty=1 continua

segments(1,Media_Diferencias-MargenError,1,Media_Diferencias+MargenError) #para trazar linea horizontal que va desde el l?mite inferior al superior del intervalo de confianza
segments(1-0.03,Limite_inferior, 1+0.03, Limite_inferior) #Para trazar los bigotes inferiores del intervalo La anchura hacia cada lado de la linea horizontal es 0.03
segments(1-0.03,Limite_superior, 1+0.03, Limite_superior)#Para trazar los bigotes superiores del intervalo. La anchura hacia cada lado de la linea horizontal es 0.03
#El intervalo no contiene a 0, lo cual es consistente con el rechazo de H0
#Los estudiantes obtienen una mayor calificaci?n al recibir instrucciones que cuando no las reciben

