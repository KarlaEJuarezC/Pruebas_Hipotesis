#--------------------Ejemplo de datos independientes--------
install.packages("ggplot2")
install.packages("plyr")
library(ggplot2)
library(plyr) 
getwd()
#En la siguiente instrucción tienen que colocar la dirección en la que quieren trabajar en su computadora 
#y en donde tienen guardados los datos
setwd("C:/Users/.../Pruebas_Hipotesis")
datos<-read.csv("Absorcion_Oxigeno.csv") #para leer los datos
#Para generar una base de datos con unicamente los valores de absocion de oxigeno y temperaturas
AbsorcionO2<-c(datos$absorcion25, datos$absorcion33)
AbsorcionO2
AbsorcionO2<-na.omit(AbsorcionO2)  #para omitir los na que aparecieron porque hay un valor menos en la temperatura de 25
Temperaturas<-c(rep(25,11), rep(33,12))  #generar un vector con los valores de temperaturas
datos2<-data.frame("Temperaturas"=Temperaturas,"AbsorcionO2"= AbsorcionO2)  #para generar un data frame
datos
datos2
str(datos2)  #obtener estructura del data frame recién creado
datos2$Temperaturas<-as.factor(datos2$Temperaturas)  #especificar que temperaturas debe ser tratado como un factor ( y no como n?mero)
str(datos2)  #obtener estructura del data frame con Temperatura como Factor

#Prueba para evaluar igualdad de varianzas
var.test(datos$absorcion25, datos$absorcion33) #Forma 1 de solicitar prueba especificando cada conjunto de datos
var.test(datos2$AbsorcionO2~datos2$Temperaturas) #Forma 2 de solicitar prueba colocando la variable respuesta en funci?n del factor

#Prueba de t para 2 muestras con n observaciones independientes de cola izquierda
t.test(datos$absorcion25, datos$absorcion33, alternative="less", var.equal = TRUE) 
t.test(datos2$AbsorcionO2~datos2$Temperaturas, alternative="less", var.equal = TRUE)  
#Para conocer los valores del intervalo de confianza para la diferencia de medias
t.test(datos$absorcion25, datos$absorcion33, alternative="two.sided") 

#A continuacion se solicitan los estadisticos desctiptivos de las dos muestras
media1<-mean(datos$absorcion25, na.rm=TRUE)
media2<-mean(datos$absorcion33, na.rm=TRUE)
sd1<-sd(datos$absorcion25, na.rm=TRUE)
sd2<-sd(datos$absorcion33, na.rm=TRUE)
# Se programa una funcion para calcular error estandar
se <- function(a)
{
  return(sd(a, na.rm=TRUE)/sqrt(length(a)))
}
se(datos$absorcion33) 
sd(datos$absorcion33)/sqrt(length(datos$absorcion33))

#Para graficar el intervalo de confianza para diferencia de medias
Diferencia_medias<-media1-media2
Limite_superior<--0.2672832 # Este valor es obtenido en la salida de t.test
Limite_inferior<- -23.7175653 # Este valor es obtenido en la salida de t.test
MargenError<-(Limite_superior-Limite_inferior)/2  #Para calcular el margen de error

#Se genera una grafica en donde se coloca como un punto al valor estimado para la diferencia de medias
#Como limites del eje y se establecen valores dentro de los que quepan los limites del intervalo de confianza
plot(1,Diferencia_medias, axes=F, pch=16, ylim=c(-25,1), ylab="Absorcion O2", xlab="Temperatura 25-Temperatura 33", font.lab=2)
axis(2, at=seq(-25,1, by=1)) #se genera una secuencia en el eje y que va de 1 en 1
abline(h=0, lty=2, col="red")# lty = 2 discontinua a la altura de 0
abline(h=-25, lty=1, col="black") #lty=1 continua a la altura de 25 (base del grafico)
segments(1,Diferencia_medias-MargenError,1,Diferencia_medias+MargenError) 
segments(1-0.03,Limite_inferior, 1+0.03, Limite_inferior) #Para trazar los bigotes inferiores del intervalo La anchura hacia cada lado de la linea horizontal es 0.03
segments(1-0.03,Limite_superior, 1+0.03, Limite_superior)#Para trazar los bigotes superiores del intervalo. La anchura hacia cada lado de la linea horizontal es 0.03
#El intervalo no contiene a 0, lo cual es consistente con el rechazo de H0
#La media a 25°C es menor que la media a 33°C

#Grafica de columnas (medias) con desviacion estandar
#FORMA 1
#Para generar una gráfica de columnas en donde las barras de error corresponden a desviaci�n est�ndar
medias <- c(media1, media2)
#Primero se generan las columnas con las medias. Se eligen los colores verde(3) y azul (4)
mp <- barplot(medias, axes=FALSE, ylim=c(0, 60),col=c(3,4),
              main="Gráfica de medias con desviación estándar", xlab="Temperaturas (°C)", 
              ylab="Absorción de Oxígeno")

#Se agregan nombres en eje x
axis(1, labels=c("Absorción O2 a 25°C", "Absorción O2 a 33°C"), at = mp)
#Se agregan los valores del eje y (secuencia del 0 al 60 de 5 en 5)
axis(2, at=seq(0,60, by=5))
stDevs <- matrix(c(sd1, sd2), 2) #se genera una matriz 1x2 con las desviaciones est?ndar
segments(mp, medias - stDevs, mp, medias + stDevs, lwd=5) #Se adicionan las lineas verticales de las barras de error
segments(mp - 0.1, medias - stDevs, mp + 0.1, medias - stDevs, lwd=2) #se adicionan los remates a las barras de error
segments(mp - 0.1 , medias + stDevs, mp + 0.1 , medias + stDevs, lwd=2) #se adicionan los remates a las barras de error
#FORMA 2
#se requieren paqueterias ggplot2 y plyr

#Se programa una funcion para calcular media, desviación y error estándar a partir de data frame
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE), se = se(x[[col]]))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

#Se genera una base de datos con las medias, desviaci�n y error est�ndar para la absorci�n de Ox�geno en funci�n de las temperaturas
datos3<- data_summary(datos2, varname="AbsorcionO2",  groupnames=c("Temperaturas"))
datos3
#Para generar el gráfico de medias con desviación estándar
GRAFICA1<-ggplot(datos3, aes(x=Temperaturas, y=AbsorcionO2, fill=Temperaturas)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=AbsorcionO2-sd, ymax=AbsorcionO2+sd), width=.2,
                position=position_dodge(.9))+
  scale_fill_manual(values = c('#855C75', '#D9AF6B'))+
  theme_classic() 

GRAFICA1
ggsave( "grafica1.jpg", GRAFICA1)

#Para generar el gráfico de medias con error estandar
ggplot(datos3, aes(x=Temperaturas, y=AbsorcionO2, fill=Temperaturas)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=AbsorcionO2-se, ymax=AbsorcionO2+se), width=.2,
                position=position_dodge(.9))+
  scale_fill_manual(values=c("wheat",8))+
  labs( title="Columnas (medias)   ", 
        subtitle="+/- Error estándar", caption="Elaboración por Karla JC en R") +
  annotate("text", label = "*", x = 1, y = 42, size = 3, colour = "black", fontface = "bold")+
  theme_minimal()

