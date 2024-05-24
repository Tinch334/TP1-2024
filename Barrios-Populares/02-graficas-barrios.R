# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("farver")

library(dplyr)
library(ggplot2)
library(farver)

# Fijo el dataset
attach(datosBarrios)


#####################
# Gráfico de barras #
#####################

print(plagas_acumuladas)

graficaPlagasFrecuentes <-
  ggplot(plagas_acumuladas, aes(x = Plaga, y = Cantidad)) +
  geom_bar(stat = "identity", fill = "bisque", width=0.5, color="black") +
  labs(title = "Plagas mas frecuentes",
       x = "Plagas",
       y = "Cantidad de familias que poseen la plaga") +
  # barras horizontales para mejor interpretacion en la lectura
  coord_flip() +
  scale_y_continuous(limits = c(0, 625), breaks = seq(0, 625, by = 100))+
  theme_bw()

graficaPlagasFrecuentes


graficaDistrTiposHacinamiento <-
  ggplot(datosBarrios, aes(x = tipo_hacinamiento)) +
  geom_bar(fill = "#c9c0a9", color="black") +
  labs(title = "Distribución de Hacinamiento",
       x = "Tipo de hacinamiento",
       y = "Cantidad de Familias") +
  theme_bw() +
  coord_flip() +
  scale_y_continuous(limits = c(0, 750), breaks = seq(0, 750, by = 100))

graficaDistrTiposHacinamiento


#?
graficaCantPlagasSegunRec <- ggplot(plagas_y_recoleccion, aes(x = FreqRecoleccion, fill = factor(CantidadPlagas))) +
  scale_fill_manual(values = c("#7f6544", "#e0dacb", "#9d947c", "#2d2d2d")) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Cantidad de Plagas por Frecuencia de Recolecta de Basura",
       x = "Frecuencia de Recolecta",
       y = "Cantidad de Hogares",
       fill = "Numero de Plagas") +
  theme_minimal()



graficaCantPlagasSegunRec

###############################
# Gráfico de barras agrupadas #
###############################

graficaRelPlagasCercaniaBasural <- 
  ggplot(datosBarrios, aes(x = cerca_basural, fill = presencia_plagas_simplificado)) +
  geom_bar(color="black") +
  scale_fill_manual(values=c("#e0dacb", "#2d2d2d")) +
  labs(title = "Relacion entre plagas y cercania a un basural",
       x = "Distancia al basural más cercano (km)",
       y = "Numero de familias",
       fill = "Posee plagas") +
  theme_bw()

graficaRelPlagasCercaniaBasural

#######################
# Gráfico de bastones #
#######################

# tratar los datos de la columna como valores discretos
datosBarrios$cant_menores_de_edad_del_hogar <- as.factor(datosBarrios$cant_menores_de_edad_del_hogar)

# agrega un margen despues de la altura maxima a la que puede llegar una barra
max_y_value <- max(table(datosBarrios$cant_menores_de_edad_del_hogar)) * 1.1


graficaMenoresPorVivienda <- 
  ggplot(datosBarrios) +
  ggtitle("Frecuencia de menores de edad por vivienda") +
  aes(x = cant_menores_de_edad_del_hogar) + 
  geom_bar(width = 0.2, fill = "bisque", color="black") +
  scale_y_continuous(limits = c(0, max_y_value)) +
  labs(y = "Número de familias", 
       x = "Número de menores de edad") +
  theme_bw()

graficaMenoresPorVivienda



#######################
# Gráfico de SECTORES #
#######################

graficaFamiliaPoseeRMuni <- 
  ggplot(familia_posee_recoleccion_muni, aes(x = "", y = Cantidad, fill = Categoria)) +
  geom_bar(stat = "identity", width = 1, color="black") +
  coord_polar(theta = "y") +
  labs(title = "Disposición de familias de recoleccion de residuos municipales",
       fill = "",
       x = "",
       y = "") +
  scale_fill_manual(values = c("#2d2d2d", "#e0dacb")) +
  theme_bw()

graficaFamiliaPoseeRMuni


graficaEliminacionPropiaResiduos <- 
  ggplot(eliminacion_residuos_no_recoleccion_muni, aes(x = "", y = Cantidad, fill = Categoria)) +
  geom_bar(stat = "identity", width = 1, color="black") +
  coord_polar(theta = "y") +
  labs(title = "Eliminacion de residuos por cuenta propia",
       fill = "",
       x = "",
       y = "") +
  scale_fill_manual(values = c("#7f6544", "#2d2d2d", "#e0dacb")) +
  theme_bw()

graficaEliminacionPropiaResiduos


###################
# Gráfico Boxplot #
###################

print(plagas_y_recoleccion)

graficaPlagasYRecoleccion <-
  ggplot(plagas_y_recoleccion, aes(x = CantidadPlagas, y = FreqRecoleccion)) + 
  geom_boxplot() +
  geom_jitter()

graficaPlagasYRecoleccion


graficaPrecioAlquilerSegunCantPlagas <- 
  ggplot(datos_alquiler, aes(x = factor(presencia_plagas_numerico), y = precio_alquiler)) +
  geom_boxplot(fill = "bisque", color = "black") +
  labs(title = "Relación entre precio del alquiler y numero de plagas",
       x = "Número de Plagas",
       y = "Precio del Alquiler ($ARS)") +
  theme_minimal()

graficaPrecioAlquilerSegunCantPlagas
