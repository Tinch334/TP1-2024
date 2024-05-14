install.packages("readxl")
install.packages("openxlsx")

library(readxl)
library(openxlsx)


rangoLectura <- "B3:DN1125"
datosBarrios <- read_excel("Datos_LP.xlsx", 
                           range=rangoLectura)