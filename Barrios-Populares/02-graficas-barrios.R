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

graficaPlagasFrecuentes <-
  ggplot(plagas_acumuladas, aes(x = reorder(Plaga, -Cantidad), y = Cantidad)) +
  geom_bar(stat = "identity", fill = "bisque", width=0.5, color="black") +
  labs(title = "Plagas mas frecuentes",
       x = "Plagas",
       y = "Cantidad") +
  # barras horizontales para mejor interpretacion en la lectura
  coord_flip() +
  scale_y_continuous(limits = c(0, 625), breaks = seq(0, 625, by = 100))+
  theme_bw()

graficaPlagasFrecuentes


graficaDistrTiposHacinamiento <-
  ggplot(datosBarrios, aes(x = tipo_hacinamiento)) +
  geom_bar(fill = "bisque", color="black") +
  labs(title = "Distribución de Hacinamiento",
       x = "Tipo de hacinamiento",
       y = "Cantidad de Familias") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 750), breaks = seq(0, 750, by = 100))

graficaDistrTiposHacinamiento


###############################
# Gráfico de barras agrupadas #
###############################

graficaRelPlagasCieloAbierto <- 
  ggplot(datosBarrios, aes(x = cerca_basural, fill = presencia_plagas_simplificado)) +
  geom_bar(color="black") +
  scale_fill_manual(values=c("bisque", "coral3")) +
  labs(title = "Relacion entre plagas y cercania a un basural",
       x = "Esta a menos de 2km de un basural",
       y = "Frecuencia",
       fill = "Posee plagas") +
  theme_bw()

graficaRelPlagasCieloAbierto
  
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

# Crear el gráfico de torta
graficaFamiliaPoseeRMuni <- 
  ggplot(familia_posee_recoleccion_muni, aes(x = "", y = Cantidad, fill = Categoria)) +
  geom_bar(stat = "identity", width = 1, color="black") +
  coord_polar(theta = "y") +
  labs(title = "Disposición de familias de recoleccion de residuos municipales",
       fill = "",
       x = "",
       y = "") +
  scale_fill_manual(values = c("bisque", "coral3")) +
  theme_bw()

graficaFamiliaPoseeRMuni


graficaEliminacionPropiaResiduos <- 
  ggplot(eliminacion_residuos_no_recoleccion_muni, aes(x = "", y = Cantidad, fill = Categoria)) +
  geom_bar(stat = "identity", width = 1, color="black") +
  coord_polar(theta = "y") +
  labs(title = "Eliminacion de resiudos por cuenta propia",
       fill = "",
       x = "",
       y = "") +
  scale_fill_manual(values = c("bisque", "coral3", "darksalmon")) +
  theme_bw()

graficaEliminacionPropiaResiduos


###################
# Gráfico Boxplot #
###################

View(plagas_y_recoleccion)
print(plagas_y_recoleccion)

graficaPlagasYRecoleccion <-
  ggplot(plagas_y_recoleccion, 
         aes(x = plagas_y_recoleccion$FreqRecoleccion,
        y = factor(plagas_y_recoleccion$CantidadPlagas))) + 
  geom_boxplot(fill = "bisque") +
  #geom_jitter() +
  labs(title = "Relacion entre la cantidad de plagas y la recoleccion de resiudos",
       fill = "",
       x = "Frequencia de recoleccion",
       y = "Cantidad de plagas") +
  scale_fill_manual(values = c("bisque"))

graficaPlagasYRecoleccion
