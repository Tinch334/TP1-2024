install.packages("dplyr")

library(dplyr)
# para utilizar funciones con manejo de strings
library(stringr)

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
names(datosBarrios)[113] = "basural_cerca"
names(datosBarrios)[115] = "forma_eliminacion_residuos"
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

# Crear una función para combinar si una familia tiene plagas o no
unificar_plagas_numerico <- function(row) {
  if (row["presencia_plagas"] == "No tiene plagas") {
    return(0)
  }
  else {
    return(str_count(row["presencia_plagas"], "/") + 1)
  }
}

unificar_plagas_simplificado <- function(row) {
  if (row["presencia_plagas"] == "No tiene plagas") {
    return("No tiene plagas")
  }
  else {
    return("Tiene plagas")
  }
}

unificar_basural <- function(row) {
  if (row["basural_cerca"] != "No") {
    return("No")
  }
  else {
    return("Si")
  }
}

freq_recoleccion_a_dias <- function(row) {
  if (row["frec_recoleccion_basura_municipal"] == "Una vez a la semana") {
    return(1)
  }
  else if (row["frec_recoleccion_basura_municipal"] == "Entre 2 y 4 veces a la semana") {
    return(floor(runif(n = 1, min =2, max = 5)))    
  }
  else if (row["frec_recoleccion_basura_municipal"] == "Al menos 5 veces a la semana") {
    return(floor(runif(n = 1, min = 5, max = 8)))
  }
  else {
    return(0)
  }
}


# aplicar funcion a cada columna del df
datosBarrios$presencia_plagas <- apply(datosBarrios, 1, unificar_plagas)

datosBarrios$presencia_plagas_simplificado <- apply(datosBarrios, 1, unificar_plagas_simplificado)

datosBarrios$cerca_basural <- apply(datosBarrios, 1, unificar_basural)

contar_plagas <- function(plaga, datos) {
  ocurrencias <- str_count(datos$presencia_plagas, plaga)
  return(sum(ocurrencias))
}

# Contar las ocurrencias de cada plaga
cucarachas_cant <- contar_plagas("Cucarachas", datosBarrios)
mosquitos_cant <- contar_plagas("Mosquitos", datosBarrios)
ratas_cant <- contar_plagas("Ratas", datosBarrios)

# Crear un dataframe con los resultados
plagas_acumuladas <- data.frame(
  Plaga = c("Cucarachas", "Mosquitos", "Ratas"),
  Cantidad = c(cucarachas_cant, mosquitos_cant, ratas_cant)
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


eliminacion_residuos_transporte_propio <-
  apariciones_cadena(datosBarrios, "forma_eliminacion_residuos", "El traslado de la basura al exterior del barrio queda a cuenta de cada vecine y/o se realiza por lxs propixs vecinxs")
eliminacion_residuos_quema <-
  apariciones_cadena(datosBarrios, "forma_eliminacion_residuos", "Quemamos basura en mi domicilio")
eliminacion_residuos_basural <-
  apariciones_cadena(datosBarrios, "forma_eliminacion_residuos", "Basural a cielo abierto dentro del mismo barrio (o adyacente)")

# una vez teniendo los datos, armar el dataframe
familia_posee_recoleccion_muni <- data.frame(
  Categoria = c("Familias que poseen", "Familias que no poseen"),
  Cantidad = c(familias_con_rec_muni, familias_sin_rec_muni)
)

View(familia_posee_recoleccion_muni)

eliminacion_residuos_no_recoleccion_muni <- data.frame(
  Categoria = c("Traslado propio", "Quema en el domicilio", "Basural a cielo abierto"),
  Cantidad = c(eliminacion_residuos_transporte_propio, eliminacion_residuos_quema, eliminacion_residuos_basural)
)

View(eliminacion_residuos_no_recoleccion_muni)

datosBarrios$recoleccion_residuos_en_dias <- apply(datosBarrios, 1, freq_recoleccion_a_dias)
datosBarrios$presencia_plagas_numerico <- apply(datosBarrios, 1, unificar_plagas_numerico)


plagas_y_recoleccion <- data.frame(
  CantidadPlagas = datosBarrios$presencia_plagas_numerico,
  FreqRecoleccion = datosBarrios$recoleccion_residuos_en_dias
)
# plagas_y_recoleccion <- data.frame(
#   CantidadPlagas = c(0, 0, 0, 0, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3),
#   FreqRecoleccion = c(1, 3, 3, 3, 3, 6, 6, 1, 3, 1, 1, 3, 3, 1, 6, 6, 3, 1, 1)
# )

View(plagas_y_recoleccion)

plagas_y_recoleccion <-
  plagas_y_recoleccion[plagas_y_recoleccion$FreqRecoleccion != 0,]

View(plagas_y_recoleccion)

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
