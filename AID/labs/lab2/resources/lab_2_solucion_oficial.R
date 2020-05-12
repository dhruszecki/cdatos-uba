#archivo de alumnos
cat("\014")
rm(list = ls())

library(readxl)# llama a la librería
library(ggplot2)

#cambiar direccion a la que corresponda
dir_root = "C:/Users/feder/Documents/PERSONAL ACTIVO/Docencia/AID/Clase sabado 25/"

#1 levanto con read_xlsx
datos_crudos = read_xlsx(paste0(dir_root,"Datos de alumnos de AID 2020.xlsx"))

#2 me quedo con las columnas necesarias
datos = datos_crudos[c(1,5,6,7,8)]

#3 histograma y bloxplot de la edad de todos los alumnos
hist(datos$Edad)
boxplot(datos$Edad)

#4a ¿cual es la carrera mas frecuente en la maestria?
freq_titulo = as.data.frame(table(datos$Titulo2))
titulo_mas_frecuente = freq_titulo[which.max(freq_titulo$Freq),1]

#4b ¿cual es la universidad mas frecuente en la maestria?
freq_universidad = as.data.frame(table(datos$Universidad2))
universidad_mas_frecuente = freq_universidad[which.max(freq_universidad$Freq),1]

#5 ordena y se queda con las primeras 4 
freq_titulo_ordenado = freq_titulo[order(-freq_titulo$Freq),]
freq_titulo_ordenado_truncado = freq_titulo_ordenado[1:4,]
#grafica un barplot ordenado 
x =barplot(freq_titulo_ordenado_truncado[,2],ylab =("Cantidad"),xaxt='n')
labs <- freq_titulo_ordenado_truncado[,1]
text(cex=1, x=x-.25, y=-2.25, labs, xpd=TRUE, srt=90)

#6 se queda con todos los alumnos de las 4 carreras mas frecuentes
datos_filtrados = datos[datos$Titulo2 %in% labs,]
#tabla de frecuencias de dos variables y luego pasado a dataframe
datos_titulo_genero = data.frame(table(datos_filtrados[c(4,2)]))

#7 grafico de barras con distingo en barras apiladas
ggplot(data=datos_titulo_genero,aes(x=Titulo2,y=Freq, fill=Genero) ) +
  geom_bar ( stat ="identity", colour="blue")+scale_fill_brewer(palette="Paired") +
  xlab("Categoría de peso") + ylab ("")

#8 obtener los alumnos que estudian "Ing informatica" y "Sistemas" 
#de la comision "C1" y de 30 años o mas
datos_filtrados_2 = datos[datos$Titulo2 %in% c("Ing Informatica", "Sistemas") &
                          datos$Comision  %in% "C1" &
                            datos$Edad>=30,]

#9 histograma y bloxplot de la edad de todos los alumnos
par(mfrow = c(2,1))
hist(datos$Edad,xlim=c(20,60))
boxplot(datos$Edad,horizontal = TRUE,ylim=c(20,60))