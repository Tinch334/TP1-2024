# Instalo los paquetes necesarios (si aún no los tengo instalados)
#install.packages("tidyverse")
install.packages("ggplot2")

install.packages("farver")


library(dplyr)

library(ggplot2)

library(farver)

# Fijo el dataset
attach(datosBarrios)


#####################
# Gráfico de barras #
#####################

graficoPlagasFrecuentes <-
  ggplot(plagas_acumuladas, aes(x = reorder(Plaga, -Cantidad), y = Cantidad)) +
  geom_bar(stat = "identity", fill = "tan2", col="black", width=0.5) +
  labs(title = "Plagas mas frecuentes",
       x = "Plagas",
       y = "Cantidad") +
  # barras horizontales para mejor interpretacion en la lectura
  coord_flip() +
  scale_y_continuous(limits = c(0, 625), breaks = seq(0, 625, by = 100))+
  theme_bw()

graficoPlagasFrecuentes


graficoDistrTiposHacinamiento <-
  ggplot(datosBarrios, aes(x = tipo_hacinamiento)) +
  geom_bar(fill = "deepskyblue", color = "black") +
  labs(title = "Distribución de Hacinamiento",
       x = "Tipo de hacinamiento",
       y = "Cantidad de Familias") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 750), breaks = seq(0, 750, by = 100))

graficoDistrTiposHacinamiento


###############################
# Gráfico de barras agrupadas #
###############################

graficoRelPlagasCieloAbierto <- 
  ggplot(datosBarrios, aes(x = cerca_basural, fill = presencia_plagas_simplificado)) +
  geom_bar() +
  labs(title = "Relacion entre plagas y cercania a un basural",
       x = "Esta a menos de 2km de un basural",
       y = "Frecuencia",
       fill = "Posee plagas") +
  theme_bw()

graficoRelPlagasCieloAbierto
  
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
  geom_bar(width = 0.16, fill = "lightblue4") +
  scale_y_continuous(limits = c(0, max_y_value)) +
  labs(y = "Número de familias", 
       x = "Número de menores de edad") +
  theme_bw()

graficaMenoresPorVivienda



#######################
# Gráfico de SECTORES #
#######################

# Crear el gráfico de torta
graficaFamiliaPoseeRMuni <- 
  ggplot(familia_posee_recoleccion_muni, aes(x = "", y = Cantidad, fill = Categoria)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Disposición de familias de recoleccion de residuos municipales",
       fill = "",
       x = "",
       y = "") +
  scale_fill_manual(values = c("cadetblue", "dodgerblue3")) +
  theme_bw()

graficaFamiliaPoseeRMuni


graficaEliminacionPropiaResiduos <- 
  ggplot(eliminacion_residuos_no_recoleccion_muni, aes(x = "", y = Cantidad, fill = Categoria)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Eliminacion de resiudos por cuenta propia",
       fill = "",
       x = "",
       y = "") +
  scale_fill_manual(values = c("cadetblue", "dodgerblue3", "navyblue")) +
  theme_bw()

graficaEliminacionPropiaResiduos


###################
# Gráfico Boxplot #
###################
plagas_y_recoleccion$CantidadPlagas <-
  factor(plagas_y_recoleccion$CantidadPlagas)

View(plagas_y_recoleccion)

graficaPlagasYRecoleccion <-
  ggplot(plagas_y_recoleccion, aes(x = CantidadPlagas, y = FreqRecoleccion)) + 
  geom_boxplot() +
  geom_jitter()

graficaPlagasYRecoleccion

## Descarga de graficas (capaz poner esto en un archivo aparte(?))
## Formato png
ggsave("graficos/graficoPlagasFrecuentes.png", graficoPlagasFrecuentes)
ggsave("graficos/graficoDistrTiposHacinamiento.png", graficoDistrTiposHacinamiento)
ggsave("graficos/graficoMenoresVivienda.png", graficaMenoresPorVivienda)
ggsave("graficos/graficaFamiliaPoseeRMuni.png", graficaFamiliaPoseeRMuni)