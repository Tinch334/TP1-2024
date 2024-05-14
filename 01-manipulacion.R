install.packages("dplyr")

library(dplyr)


#Renombramos otras columnas que pierden el nombre por tener la misma "cabeza", o que se piereden al leer el archivo
names(datosBarrios)[1] = "PROVINCIA"
names(datosBarrios)[2] = "BARRIO"

#Estos nombres son para conveniencia a futuro
names(datosBarrios)[38] = "FCOCINA1"
names(datosBarrios)[39] = "FCOCINA2"
names(datosBarrios)[40] = "FCOCINA3"
names(datosBarrios)[41] = "FCOCINA4"

names(datosBarrios)[43] = "FCALEFACCION1"
names(datosBarrios)[44] = "FCALEFACCION2"
names(datosBarrios)[45] = "FCALEFACCION3"
names(datosBarrios)[46] = "FCALEFACCION4"
names(datosBarrios)[47] = "FCALEFACCION5"

names(datosBarrios)[73] = "PHUMEDAD1"
names(datosBarrios)[74] = "PHUMEDAD2"
names(datosBarrios)[75] = "PHUMEDAD3"
names(datosBarrios)[76] = "PHUMEDAD4"
names(datosBarrios)[77] = "PHUMEDAD5"

names(datosBarrios)[79] = "PESTRUCTURALES1"
names(datosBarrios)[80] = "PESTRUCTURALES2"
names(datosBarrios)[81] = "PESTRUCTURALES3"
names(datosBarrios)[82] = "PESTRUCTURALES4"
names(datosBarrios)[83] = "PESTRUCTURALES5"


#Colapsamos las respuestas ubicadas en multiples columnas a solo 1.
datosBarrios$`¿Qué fuentes de energía utilizan para cocinar en su vivienda?` <- coalesce(
  datosBarrios$`¿Qué fuentes de energía utilizan para cocinar en su vivienda?`,
  datosBarrios$FCOCINA1,
  datosBarrios$FCOCINA2,
  datosBarrios$FCOCINA3,
  datosBarrios$FCOCINA4
)

datosBarrios$`¿Cuál es la principal fuente de energía que utiliza para calefaccionar la vivienda?` <- coalesce(
  datosBarrios$`¿Cuál es la principal fuente de energía que utiliza para calefaccionar la vivienda?`,
  datosBarrios$FCALEFACCION1,
  datosBarrios$FCALEFACCION2,
  datosBarrios$FCALEFACCION3,
  datosBarrios$FCALEFACCION4,
  datosBarrios$FCALEFACCION5,
)

#Su vivienda, ¿posee problemas de humedad graves y/o filtraciones?



#Eliminamos las columnas adicionales donde estaban las respuestas del punto anterior.
datosBarrios$FCOCINA1 <- NULL
datosBarrios$FCOCINA2 <- NULL
datosBarrios$FCOCINA3 <- NULL
datosBarrios$FCOCINA4 <- NULL

datosBarrios$FCALEFACCION1 <- NULL
datosBarrios$FCALEFACCION2 <- NULL
datosBarrios$FCALEFACCION3 <- NULL
datosBarrios$FCALEFACCION4 <- NULL
datosBarrios$FCALEFACCION5 <- NULL

# Ver estructura del dataset (como planilla)
View(datosBarrios)