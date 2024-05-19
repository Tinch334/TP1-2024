install.packages("dplyr")

library(dplyr)


coalesce_columnas <- function(frame, indice_inicial, cantidad) {
  #Primero creamos un vector con todas las columnas a "juntar".
  columnas <- c()
  
  for (i in indice_inicial:(indice_inicial + cantidad - 1)) {
    columnas <- append(columnas, frame[i])
  }
  
  #Hacemos un coalesce, es decir juntamos las columnas. Los signos de exclamacion
  #son para que R evalue la lista antes de llamar a la funcion con ese argumento
  #evitando un error(Creo).
  frame[indice_inicial] <- coalesce(!!!columnas)
  
  return(frame)
}

suma_no_nula<- function(frame, indice_resultado, indice_inicial_suma, cantidad) {
  #Voy por las columnas especificadas, si una celda es nula la transformo en cero,
  #de lo contrario en 1. Seria mejor hacer esto con lapply, no pude hacerlo funcionar.
  for (i in indice_inicial_suma:(indice_inicial_suma + cantidad - 1)) {
    frame[i] <- sapply(frame[i], function(x) ifelse(is.na(x), 0, 1))
  }

  frame[indice_resultado] <-
    rowSums(frame[, indice_inicial_suma:(indice_inicial_suma + cantidad - 1)])
    
  return(frame)
}


#Renombramos otras columnas que pierden el nombre por tener la misma "cabeza", o que se piereden al leer el archivo
names(datosBarrios)[1] = "PROVINCIA"
names(datosBarrios)[2] = "BARRIO"

#Colapsamos las respuestas ubicadas en multiples columnas a solo 1.

# datosBarrios$`¿Qué fuentes de energía utilizan para cocinar en su vivienda?` <-
#   coalesce_columnas(datosBarrios, 37, 5)

datosBarrios <- coalesce_columnas(datosBarrios, 37, 5)

datosBarrios <- coalesce_columnas(datosBarrios, 42, 6)

datosBarrios <- suma_no_nula(datosBarrios, 72, 72, 4)

datosBarrios <- suma_no_nula(datosBarrios, 78, 78, 4)

# Ver estructura del dataset (como planilla)
View(datosBarrios)
