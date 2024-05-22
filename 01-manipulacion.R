install.packages("dplyr")

library(dplyr)
# para utilizar funciones con manejo de strings
library(stringr)


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


# Renombramos otras columnas que pierden el nombre por tener la misma "cabeza", o que se piereden al leer el archivo
names(datosBarrios)[1] = "PROVINCIA"
names(datosBarrios)[2] = "BARRIO"

# renombrados para posterior uso
names(datosBarrios)[5] = "num_integrantes_vivienda"
names(datosBarrios)[10] = "cant_menores_de_edad_del_hogar"
names(datosBarrios)[12] = "num_ambientes_usados_como_dormitorio"
names(datosBarrios)[13] = "num_max_pers_duermen_por_dormitorio"
names(datosBarrios)[92] = "cucas"
names(datosBarrios)[93] = "mosquitos"
names(datosBarrios)[94] = "ratas"
names(datosBarrios)[116] = "frec_recoleccion_basura_municipal"


# Crear una función para combinar las plagas en una única columna
unificar_plagas <- function(row) {
  plagas <- c()
  
  if (row["cucas"] != "" && !is.na(row["cucas"])) {
    plagas <- c(plagas, "Cucarachas")
  }
  if (row["mosquitos"] != "" && !is.na(row["mosquitos"])) {
    plagas <- c(plagas, "Mosquitos")
  }
  if (row["ratas"] != "" && !is.na(row["ratas"])) {
    plagas <- c(plagas, "Ratas")
  }
  
  if (length(plagas) == 0) {
    return("No tiene plagas")
  } else {
    return(paste(plagas, collapse = "/"))
  }
}

# aplicar funcion a cada columna del df
datosBarrios$presencia_plagas <- apply(datosBarrios, 1, unificar_plagas)
 
 
contar_plagas <- function(plaga, datos) {
  ocurrencias <- str_count(datos$presencia_plagas, plaga)
  return(sum(ocurrencias))
}

# Contar las ocurrencias de cada plaga
cucarachas <- contar_plagas("Cucarachas", datosBarrios)
mosquitos <- contar_plagas("Mosquitos", datosBarrios)
ratas <- contar_plagas("Ratas", datosBarrios)

# Crear un dataframe con los resultados
plagas_acumuladas <- data.frame(
  Plaga = c("Cucarachas", "Mosquitos", "Ratas"),
  Cantidad = c(cucarachas, mosquitos, ratas)
)

# Mostrar el dataframe
View(plagas_acumuladas)

# (para corroborar que se cuentan correctamente)
#cantidad_palabra <- sum(grepl("Cucarachas", datosBarrios$cucas))
#print(cantidad_palabra)

apariciones_cadena <- function(df, columna, cadena) {
  # Contar la cantidad de veces que aparece la palabra en la columna
  cantidad <- sum(df[[columna]] == cadena)
  return(cantidad)
}

familias_sin_rec_muni <- apariciones_cadena(datosBarrios, "frec_recoleccion_basura_municipal", "No hay servicio de recolección municipal")
familias_con_rec_muni <- sum(!is.na(datosBarrios["frec_recoleccion_basura_municipal"])) - familias_sin_rec_muni


print(familias_sin_rec_muni )
print(familias_con_rec_muni )
# una vez teniendo los datos, armar el dataframe

familia_posee_recoleccion_muni <- data.frame(
  Categoria = c("Familias que poseen", "Familias que no poseen"),
  Cantidad = c(familias_con_rec_muni, familias_sin_rec_muni)
)
View(familia_posee_recoleccion_muni)

# Promedio de personas por habitacion
# somos cuidadosos de no dividir celdas con NA o con 0 en el denominador
datosBarrios$promedio_personas_por_habitacion <- ifelse(!is.na(datosBarrios$num_integrantes_vivienda) &
                                                        !is.na(datosBarrios$num_ambientes_usados_como_dormitorio) &
                                                        datosBarrios$num_ambientes_usados_como_dormitorio != 0,
                                                        datosBarrios$num_integrantes_vivienda / datosBarrios$num_ambientes_usados_como_dormitorio,
                                                        NA
                                                        )



# promedio <= 2, sin hacinamiento
# 2 < promedio <= 4.99, hacinamiento moderado
# sino, hacinamiento critico

datosBarrios$tipo_hacinamiento <- cut(datosBarrios$promedio_personas_por_habitacion,
                                 breaks = c(0, 2, 4.99, Inf),
                                 labels = c("no posee", "moderado", "crítico"),
                                 right = TRUE)

View(datosBarrios)


#Colapsamos las respuestas ubicadas en multiples columnas a solo 1.

# datosBarrios$`¿Qué fuentes de energía utilizan para cocinar en su vivienda?` <-
#   coalesce_columnas(datosBarrios, 37, 5)

datosBarrios <- coalesce_columnas(datosBarrios, 37, 5)

datosBarrios <- coalesce_columnas(datosBarrios, 42, 6)

datosBarrios <- suma_no_nula(datosBarrios, 72, 72, 4)

datosBarrios <- suma_no_nula(datosBarrios, 78, 78, 4)

# Ver estructura del dataset (como planilla)
View(datosBarrios)
