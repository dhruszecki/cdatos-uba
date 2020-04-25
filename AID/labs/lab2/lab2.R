rm(list = ls()) 

library(readxl)
alumnos_AID_2020 <- read_excel("Datos de alumnos de AID 2020.xlsx")
View(alumnos_AID_2020)                                                                                                 

alumniDF <- data.frame(
  alumnos_AID_2020[, "Comision"],
  alumnos_AID_2020[, "Titulo2"],
  alumnos_AID_2020[, "Universidad2"],
  alumnos_AID_2020[, "Genero"],
  alumnos_AID_2020[, "Edad"],
  stringsAsFactors = TRUE
)

#View(alumnos_AID_2020)                                                                                                 

p <- plot(x = alumniDF$Edad)
hist(x= alumniDF$Edad)
boxplot(x= alumniDF$Edad)

# 4.1
freq_titulo <- as.data.frame(table(alumniDF$Titulo2))
titulo_mas_frecuente <- freq_titulo[witch.max(freq_titulo$Freq),1]

# 4.2
freq_univerdidad <- as.data.frame(table(alumniDF$Universidad2))
universidad_mas_frecuente <- freq_univerdidad[witch.max(freq_univerdidad$Freq),1]

# 5


