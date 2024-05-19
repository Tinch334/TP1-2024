# Instalo los paquetes necesarios (si aún no los tengo instalados)
 install.packages("tidyverse")
 install.packages("ggplot2")

# Cargo los paquetes que voy a usar
library(tidyverse)
library(ggplot2)

# Fijo el dataset
attach(datos_limpios)
attach(datosBarrios)

View(datos_limpios)
 
#####################
# Gráfico de barras #
#####################

datos %>%
	mutate( tiempo = factor(tiempo,
													levels = 1:5,
													labels = c("Menos de 2 años", "Entre 2 y 5 años",
																		 "Entre 5 y 10 años", "Entre 10 y 20 años",
																		 "20 años o más"))) %>%
	ggplot() + 
	
	#aes(x = tiempo) + # Frecuencias absolutas
	aes(x = reorder(tiempo, tiempo, function(x) -length(x))) + # Ordenar según frecuencia
	#aes(x = tiempo, y = ..count.. / sum(..count..)) + # Porcentajes
	# aes(x = reorder(tiempo, tiempo, function(x) -length(x)), 
	#		y = ..count.. / sum(..count..)) +  # Porcentajes ordenados según frecuencia
	#scale_y_continuous(labels = scales::percent) +    # Eje para porcentajes
	
	geom_bar(width = 0.75,   # Ancho de barras
					 fill = '#7ed021',  # Color de relleno 
					 col = "black",  # Color de línea
					 alpha = 0.6) +  # Transparencia
	
	labs(y = "Cantidad de árboles", x = "Tiempo desde la plantación") + # Nombres de ejes
	
	ggtitle("Antigüedad de plantación de los árboles") +
	
	coord_flip() + # Barras horizontales o verticales

	theme_classic() # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/
	

# Crear el gráfico de barras horizontales (ordenado segun frecuencia de plagas)


ggplot(plagas_acumuladas, aes(x = reorder(Plaga, -Cantidad), y = Cantidad)) +
  geom_bar(stat = "identity", fill = "tan2", col="black", width=0.5) +
  labs(title = "Plagas mas frecuentes",
       x = "Plagas",
       y = "Cantidad") +
  theme_classic() +
  # barras horizontales para mejor interpretacion en la lectura
  coord_flip() +
  scale_y_continuous(limits = c(0, 625), breaks = seq(0, 625, by = 100))


  ggplot(datosBarrios, aes(x = tipo_hacinamiento)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribución de Hacinamiento",
       x = "Tipo de hacinamiento",
       y = "Cantidad de Familias") +
  theme_classic() +
  scale_y_continuous(limits = c(0, 750), breaks = seq(0, 750, by = 100))

 ###########################################
# Gráfico de barras a partir de una tabla #
###########################################

datos_limpios %>%
	mutate(
		alguna = atracnosis + roya + manchas + ampollas,
		ninguna = ifelse(alguna == 0, 1, 0)
	) %>%
	summarize(atracnosis = sum(atracnosis),
						roya = sum(roya),
						manchas = sum(manchas),
						ampollas = sum(ampollas),
						ninguna = sum(ninguna)) %>%
	pivot_longer(cols = c(atracnosis, roya, manchas, ampollas, ninguna),
							 names_to = "plaga",
							 values_to = "cant") %>%



 ggplot(aes(x = plaga,
            y = cant)) + 
   geom_bar(stat = "identity", # Argumento necesario si partimo de una tabla
            width = 0.75) +
   labs(y = "Cantidad de árboles", x = "Presencia de plagas") +
   ggtitle("Antigüedad de plantación de los árboles") +
   coord_flip() +
   theme_classic() # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/
 
 
 
 
#######################
# Gráfico de bastones #
#######################

# tratar los datos de la columna como valores discretos
datosBarrios$cant_menores_de_edad_del_hogar <- as.factor(datosBarrios$cant_menores_de_edad_del_hogar)

# agrega un margen despues de la altura maxima a la que puede llegar una barra
max_y_value <- max(table(datosBarrios$cant_menores_de_edad_del_hogar)) * 1.1
 
 
ggplot(datosBarrios) +
   ggtitle("Frecuencia de menores de edad por vivienda") +
   aes(x = cant_menores_de_edad_del_hogar) + 
   geom_bar(width = 0.1, fill = "darkblue") +
   scale_y_continuous(limits = c(0, max_y_value)) +
   labs(y = "Número de familias", 
        x = "Número de menores de edad") +
   theme_classic()


#######################
# Gráfico de SECTORES #
#######################

# Crear el gráfico de torta
ggplot(familia_posee_recoleccion_muni, aes(x = "", y = Cantidad, fill = Categoria)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Disposición de familias de recoleccion de residuos municipales",
       fill = "",
       x = "",
       y = "") +
  scale_fill_manual(values = c("bisque4", "chartreuse3"))
  theme_classic()


###########
# Boxplot #
###########

datos_limpios %>% 
	
	# Puedo filtrar en el mismo paso que construyo el gráfico
	filter(especie == "Eucalipto") %>%
	
	ggplot() +
	aes(x = diametro, y = "") +
	geom_boxplot(width = 0.75, fill = "lightgray", outlier.size = 1) +
	theme(axis.ticks.y = element_blank()) +
	labs(y = "", x = "Diámetro (cm)") +
	scale_x_continuous(breaks = seq(0, 250, 50))




##############
# Histograma #
##############

# Frecuencias absolutas
ggplot(datos_limpios) +
	aes(x = diametro) +
	geom_histogram(fill = "lightgray", col = "black", 
								 breaks = seq(0, 250, 20)) +
	scale_x_continuous(breaks = seq(0, 250, 20)) +
	labs(x = "Diámetro (cm)", y = "Cantidad de árboles")

# Frecuencias relativas
ggplot(datos_limpios) +
	aes(x = diametro, y = ..count../sum(..count..)) +
	geom_histogram(fill = "lightgray", col = "black", 
								 breaks = seq(0, 250, 20)) +
	scale_x_continuous(breaks = seq(0, 250, 20)) +
	scale_y_continuous(labels = scales::percent) +
	labs(x = "Diámetro (cm)", y = "Cantidad de árboles")



##############
# Densidades #
##############

ggplot(datos_limpios) +
	aes(x = altura) +
	stat_density(bw = 3, # Nivel de suavizado
							         # El nivel por defecto puede conocerse con bw.nrd0(datos_limpios$altura)
							 fill = "lightgray", col = "black") +
	labs(x = "Altura (m)", y = "Densidad")

