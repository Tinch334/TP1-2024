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
       y = "Cantidad de familias que poseen la plaga",
       caption = "Observatorio Villero, La Poderosa (2022)") +
# invertir ejes para mejor interpretacion en la lectura
  coord_flip() +
  scale_y_continuous(limits = c(0, 625), breaks = seq(0, 625, by = 100))+
  theme_bw()

graficaPlagasFrecuentes


graficaDistrTiposHacinamiento <-
  ggplot(datosBarrios, aes(x = tipo_hacinamiento)) +
  geom_bar(fill = "#c9c0a9", color="black") +
  labs(title = "Distribución de Hacinamiento",
       x = "Tipo de hacinamiento",
       y = "Cantidad de Familias",
       caption = "Observatorio Villero, La Poderosa (2022)") +
  theme_bw() +
  coord_flip() +
  scale_y_continuous(limits = c(0, 750), breaks = seq(0, 750, by = 100))

graficaDistrTiposHacinamiento


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
       fill = "Posee plagas",
       caption = "Observatorio Villero, La Poderosa (2022)") +
  theme_bw()

graficaRelPlagasCercaniaBasural


graficaRelPlagasMatPared <- ggplot(datosBarrios, aes(x = Tipo_Pared, fill = presencia_plagas_simplificado)) +
  geom_bar() +
  labs(title = "Relación entre el Tipo de Pared y la Presencia de Plagas",
       x = "Material de Paredes Exteriores",
       y = "Número de Familias",
       fill = "Posee plagas",
       caption = "Observatorio Villero, La Poderosa (2022)") +
  scale_fill_manual(values = c("No tiene plagas" = "#b0a18e", "Tiene plagas" = "#a37457")) +
  scale_y_continuous(limits = c(0, 950), breaks = seq(0, 950, by = 100))+
  theme_bw()

graficaRelPlagasMatPared

graficaRelPlagasMatPiso <- ggplot(datosBarrios, aes(x = datosBarrios$"¿De qué material está hecho el piso (revestimiento) de su vivienda?", fill = presencia_plagas_simplificado)) +
  geom_bar() +
  labs(title = "Relación entre el Tipo de material del Piso y la Presencia de Plagas",
       x = "Material del revestimiento del Piso",
       y = "Número de Familias",
       fill = "Posee plagas",
       caption = "Observatorio Villero, La Poderosa (2022)") +
  scale_fill_manual(values = c("No tiene plagas" = "#b0a18e", "Tiene plagas" = "#a37457")) +
  scale_y_continuous(limits = c(0, 600), breaks = seq(0, 600, by = 50))+
  theme_bw()

graficaRelPlagasMatPiso


graficaRelRecoleccionPlagas <- ggplot(plagas_y_recoleccion, aes(x = FreqRecoleccion, fill = CantidadPlagas)) +
  geom_bar() +
  labs(title = "Relación entre frecuencia recoleccion y presencia plagas",
       x = "Frecuencia de recolección semanal",
       y = "Número de Familias",
       fill = "Posee plagas",
       caption = "Observatorio Villero, La Poderosa (2022)") +
  scale_fill_manual(values = c("No tiene plagas" = "#b0a18e", "Tiene plagas" = "#a37457")) +
  scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, by = 50))+
  theme_bw()

graficaRelRecoleccionPlagas

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
       x = "Número de menores de edad",
       caption = "Observatorio Villero, La Poderosa (2022)") +
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
       y = "",
       caption = "Observatorio Villero, La Poderosa (2022)") +
  scale_fill_manual(values = c("#2d2d2d", "#e0dacb")) +
  theme_bw()

graficaFamiliaPoseeRMuni


graficaEliminacionPropiaResiduos <- 
  ggplot(eliminacion_residuos_no_recoleccion_muni, aes(x = "", y = Cantidad, fill = Categoria)) +
  geom_bar(stat = "identity", width = 1, color="black") +
  coord_polar(theta = "y") +
  labs(title = "Métodos de eliminacion de residuos de vecinos sin recoleccion municipal",
       fill = "",
       x = "",
       y = "",
       caption = "Observatorio Villero, La Poderosa (2022)") +
  scale_fill_manual(values = c("#7f6544", "#2d2d2d", "#e0dacb")) +
  theme_bw()

graficaEliminacionPropiaResiduos


###################
# Gráfico Boxplot #
###################
graficaPrecioAlquilerSegunCantPlagas <- 
  ggplot(datos_alquiler, aes(x = factor(presencia_plagas_numerico), y = precio_alquiler)) +
  geom_boxplot(fill = "bisque", color = "black") +
  labs(title = "Relación entre precio del alquiler y número de plagas",
       x = "Número de Plagas",
       y = "Precio del Alquiler ($ARS)",
       caption = "Observatorio Villero, La Poderosa (2022)") +
  #geom_jitter()+
  scale_y_continuous(limits = c(0, 30100), breaks = seq(0, 30100, by = 2500))+
  theme_bw()

graficaPrecioAlquilerSegunCantPlagas


graficaPrecioAlquilerSegunCantPlagas_Valores <- 
  ggplot(datos_alquiler, aes(x = factor(presencia_plagas_numerico), y = precio_alquiler)) +
  geom_boxplot(fill = "bisque", color = "black") +
  stat_summary(fun = min, geom = "text",
               aes(label = round(..y.., 2)),
               vjust = 1.5, size = 4, color = "black",
               position = position_dodge(width = 0.75)) +  # minimo
  stat_summary(fun = max, geom = "text",
               aes(label =round(..y.., 2)),
               vjust = -0.5, size = 4, color = "black",
               position = position_dodge(width = 0.75)) +  # mediana
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)),
               vjust = -1, size = 4, color = "black",
               position = position_dodge(width = 0.75)) +  # maximo
  labs(title = "Relación entre precio del alquiler y número de plagas",
       x = "Número de Plagas",
       y = "Precio del Alquiler ($ARS)",
       caption = "Observatorio Villero, La Poderosa (2022)") +
  
  scale_y_continuous(limits = c(0, 30100), breaks = seq(0, 30100, by = 2500))+
  theme_bw()
graficaPrecioAlquilerSegunCantPlagas_Valores