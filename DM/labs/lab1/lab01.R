# importamos el dataset
MPI_subnational <- read.csv("~/maestria/DM/labs/lab1/MPI_subnational.csv") # Tipo dataframe

# factor: dato categórico


MPI_subnational[-c(1,2)] # me da una dataframe
MPI_subnational$Headcount.Ratio.Regional # me da un vector
MPI_subnational$Headcount.Ratio.Regiona[1:10]


mask = MPI_subnational$Headcount.Ratio.Regiona > 50

MPI_subnational[mask, ]

MPI_subnational[(MPI_subnational$Headcount.Ratio.Regiona > 50) &
                  (MPI_subnational$Intensity.of.deprivation.Regional < 80), ]


MPI_subnational[c(F,T),] # selecciono los pares (plot)

hist(MPI_subnational$MPI.National, main = "Histograma MPI Nacional", xlab = "MPI", ylab = "Frecuencia")

mean(MPI_subnational$Intensity.of.deprivation.Regional, na.rm = TRUE)

mean(MPI_subnational$Intensity.of.deprivation.Regional[!is.na(MPI_subnational$Intensity.of.deprivation.Regional)])



?hist
