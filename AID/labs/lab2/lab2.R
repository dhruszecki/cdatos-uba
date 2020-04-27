rm(list = ls()) 

library(readxl)
alumnos_AID_2020 <- read_excel("labs/lab2/ds/Datos de alumnos de AID 2020.xlsx")
alumniDF <- data.frame(
  alumnos_AID_2020[, "Comision"],
  alumnos_AID_2020[, "Titulo2"],
  alumnos_AID_2020[, "Universidad2"],
  alumnos_AID_2020[, "Genero"],
  alumnos_AID_2020[, "Edad"],
  stringsAsFactors = TRUE
)

plot(x = alumniDF$Edad)
hist(x= alumniDF$Edad)
boxplot(x= alumniDF$Edad)

# 4.1
freq_titulo <- as.data.frame(table(alumniDF$Titulo2))
titulo_mas_frecuente <- freq_titulo[which.max(freq_titulo$Freq),1]


# 4.2
freq_univerdidad <- as.data.frame(table(alumniDF$Universidad2))
universidad_mas_frecuente <- freq_univerdidad[which.max(freq_univerdidad$Freq),1]

# 5
titulos_top4 = freq_titulo[order(freq_titulo$Freq, decreasing=TRUE),][1:4,]
barplot(titulos_top4$Freq, names.arg = titulos_top4$Var1)

# 6
freq_titulo_genero <- as.data.frame(table(alumniDF$Genero, alumniDF$Titulo2))
library(ggplot2)
ggplot(data=freq_titulo_genero , aes( x=Var2 , y=Freq , fill =Var1 ) ) +
geom_bar (stat = "identity" , colour = "blue" ) +
scale_fill_brewer (palette = "Paired" ) +
xlab ( "Título" ) +
ylab ( "" )
