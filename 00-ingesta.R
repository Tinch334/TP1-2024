# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("googledrive")
# install.packages("readxl")
# install.packages("openxlsx")
library(readxl)
library(googledrive)
library(openxlsx)


# Descargo el archivo mediante su id de google drive
# El link de los archivos de drive tiene esta forma:
# https://docs.google.com/spreadsheets/d/16_zhdrZIW72I45SHIsVkGv-KYQw1oeup
# El id de esta hoja de cálculo es "16_zhdrZIW72I45SHIsVkGv-KYQw1oeup"
googledrive::drive_download(as_id("16_zhdrZIW72I45SHIsVkGv-KYQw1oeup"), 
														overwrite = T)

# Cargo el archivo como .xlsx
# se saltea las primeras 3 filas
datosArbol <- readxl::read_excel("arbol.xlsx", 
														col_names = FALSE, 
														skip = 3)

# Veo la estructura del dataset
View(datosArbol)


## Link de google drive a la planilla de barrios populares:
## https://docs.google.com/spreadsheets/d/1IRhvzOQkvuspQF3TAsBCI-68i8ya0_hy/edit#gid=1908972294

# Lo descargamos
googledrive::drive_download(as_id("1IRhvzOQkvuspQF3TAsBCI-68i8ya0_hy"), 
                            overwrite = T)

rangoLectura <- "B3:DN1125"
datosBarrios <- read_excel("Datos_LP.xlsx", 
                           range=rangoLectura)
# asigna las categorias de las columnas 1 y 2, que se pierden al leerlo
names(datosBarrios)[1] = "PROVINCIA"
names(datosBarrios)[2] = "BARRIO"

# Ver estructura del dataset (como planilla)
View(datosBarrios)
