#Prueba de t para 1 muestra de n observaciones independientes
peso<-c(310,311,390,368,420, 376, 303, 410, 365, 350)
shapiro.test(peso)
qqnorm(peso) 
qqline(peso) 
#Para graficar el histograma con la curva normal
media<- mean(peso) #Calcular la media excluyendo los NA(missing values)
desvest<-sd(peso) #Calcular la desviacion estandar 
Minimo<-min(peso) 
Maximo<-max(peso) 
x<-seq(Minimo,Maximo,.001) #Generar secuencia de 0.001 en 0.001 unidades
hist(peso, density=20, prob=T, xlab="peso", main="Histograma con curva normal", ylab="Densidad de probabilidad", xlim=c(235,485))
abline(h=0) #adicionar l?nea horizontal a la altura de 0
curve(dnorm(x, mean=media, sd=desvest), col="#b04e80", lwd=2, add=TRUE, yaxt="n") #adicionar curva que corresponde a una distribuci?n normal con los mismos valores de media y desviaci?n est?ndar
palette()

#Para realizar prueba de t
#La prueba sera de dos colas y como valor contra 
#el que se contrastara se usara 400
t.test(peso, mu=400, alternative="two.sided")

#Gr?fica del Intervalo de confianza para la media mu con el valor de mu0= 400
MuEstimada<-360.3
Limite_superior<-390.0771
Limite_inferior<-330.5229
MargenError<-360.3-330.5229
Valor_prueba<-400
plot(1,MuEstimada, axes=F, pch=16, ylim=c(320,410), ylab="Peso (g)", xlab="Ratas del Bioterio", font.lab=2)
axis(2, at=seq(320,410, by=10)) #se genera una secuencia en el eje y que va de 10 en 10
abline(h=Valor_prueba, lty=2, col="red")# lty = 2 discontinua
abline(h=320, lty=1, col="black") #lty=1 continua
segments(1,MuEstimada-MargenError,1,MuEstimada+MargenError) 
segments(1-0.03,L?mite_inferior, 1+0.03, L?mite_inferior) 
segments(1-0.03,L?mite_superior, 1+0.03, L?mite_superior)
  
