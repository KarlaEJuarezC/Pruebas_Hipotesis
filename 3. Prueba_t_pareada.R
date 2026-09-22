#Ejemplo de datos pareados
#Forma 1 de instriducir los datos
Estacion<-c("ATI","BJU", "CAM", "CUT", "FAC", "IZT", "PED", "SAG", "SFE", "TLA", "UIZ", "VIF")
pm10_20_Marzo<-c(49, 50, 50, 57, 48, 54, 40, 63, 35, 57, 68, 60)
pm10_20_Abril<-c(58, 60, 58, 78, 52, 63, 61, 94, 55, 68, 81, 78)
datos1<-data.frame(Estacion,pm10_20_Marzo,pm10_20_Abril)

#Forma 2 de introducir los datos
Mes<-c(rep("Marzo",12), rep("Abril",12))
pm10<-c(pm10_20_Marzo, pm10_20_Abril)
datos2<-data.frame(Mes, pm10)
datos2$Mes<-factor(datos2$Mes, levels=c("Abril", "Marzo"))

#Prueba de t (Forma 1 de realizarla)
t.test(datos1$pm10_20_Abril,datos1$pm10_20_Marzo, alternative="greater", paired=T)

#Prueba de t (Forma 2 de realizarla)
t.test(datos2$pm10~datos2$Mes, alternative="greater", paired=T)

