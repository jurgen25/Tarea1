##############################################################################
#                   CURSO: ANALISIS EXPLORATORIO DE DATOS EN R               #
#                       PROFESOR: MICHAEL ROMERO NAVARRO                     #
##############################################################################

#instalacion de paquetes
install.packages("ggplot2", dependencies = T)

#Llamado de librer?as
#Visualizaciones
library("ggplot2")
#Ordenamiento de elementos en data frames
library("forcats")
#Manejo de fechas
library("lubridate")
#Transformación de data frame
library(tidyr)
#Facilidades para trabajar sobre elementos con multiples observaciones
library(plyr)
#Definicion de escalas para graficos
library("scales")

#Consultamos la tabla de datos Iris
iris
str(iris)
datos <- iris

#Iniciamos nuestra visualización
str(datos)
#Capa de datos
ggplot(data = datos)

#Capa de datos y capa estetica
ggplot(data = datos, mapping = aes(x = Sepal.Length, y = Petal.Length, color = Species))

#Capa de datos, capa estetica y capa de geometrías
ggplot(data = datos, mapping = aes(x = Sepal.Length, y = Petal.Length, color = Species))+
  geom_point()

#Diferentes tipos de gráficos para los mismos datos
datos <- mpg
str(mpg)
ggplot(data = datos, mapping = aes(x = cty, y = hwy)) +
  geom_point() +
  labs(title = " Grafico geom_point()")

ggplot(data = datos, mapping = aes(x = manufacturer)) +
  geom_bar()  +
  coord_flip()+
  labs(title = "geom_bar")

ggplot(data = datos, mapping = aes(x = hwy))  +
  geom_histogram() +
  labs(title = "geom_histogram()")

ggplot(data = datos, mapping = aes(x = manufacturer, y = hwy)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "geom_boxplot()")

ggplot(data = datos, mapping = aes(x = hwy))  +
  geom_density() +
  labs(title = "geom_density()")

#Ejemplo 1: Gráfico de Barras, Color, Orden y Limpieza
casos_cancer <- read.csv('cancer_hombres.csv', header=TRUE, sep=';',dec='.', check.names = F)
str(casos_cancer)

ggplot(data = casos_cancer, mapping = aes(x = localizacion, y = total)) +
  geom_col() 

ggplot(data = casos_cancer, mapping = aes(x = localizacion, y = total)) +
  geom_col() +
  coord_flip()

ggplot(data = casos_cancer, 
       mapping = aes(x = fct_reorder(localizacion, total), y = total)) +
  geom_col() +
  coord_flip()

ggplot(data = casos_cancer, 
       mapping = aes(x = fct_reorder(localizacion, total), y = total)) +
  geom_col() +
  coord_flip() +
  labs(y = "Cantidad de casos",
       x = "Localización del tumor",
       title = " Mortalidad por tumores malignos más frecuentes en la población masculina según localización anatomica, Costa Rica 2015",
       caption = "Fuente de datos: Caja Costarricense del Seguro Social.") +
  theme(plot.title = element_text(hjust = .5),
        plot.caption = element_text(hjust = 0))

ggplot(data = casos_cancer, 
       mapping = aes(x = fct_reorder(localizacion, total), y = total)) +
  geom_col() +
  geom_text(mapping = aes(label = total)) + # agregamos textos
  coord_flip() +
  labs(y = "Cantidad de casos",
       x = "Localización del tumor",
       title = " Mortalidad por tumores malignos más frecuentes en la población nmasculina según localización anatomica, Costa Rica 2015",
       caption = "Fuente de datos: Caja Costarricense del Seguro Social.") +
  theme(plot.title = element_text(hjust = .5),
        plot.caption = element_text(hjust = 0))

ggplot(data = casos_cancer, 
       mapping = aes(x = fct_reorder(localizacion, total), y = total)) +
  geom_col() +
  geom_text(mapping = aes(label = total), hjust = -.3) + # agregamos textos
  labs(y = "Cantidad de casos",
       x = "Localización del tumor",
       title = " Mortalidad por tumores malignos más frecuentes en la población nmasculina según localización anatomica, Costa Rica 2015",
       caption = "Fuente de datos: Caja Costarricense del Seguro Social.") +
  coord_flip() + 
  scale_y_continuous(limits = c(0,800), expand = c(0,0)) +
  theme(plot.title = element_text(hjust = .5),# colocamos el titulo en el centro
        panel.grid = element_blank(), # eliminamos las líneas del fondo
        axis.text.x = element_blank(), # eliminamos el texto del eje x
        plot.caption = element_text(hjust = 0)) # colocamos la fuente a la izquierda


#Ejemplo 2: Grafico de lineas, etiquetas
nacimientos <- read.csv('nacimientoscr.csv', header=TRUE, sep=';',dec='.', check.names = F)
nacimientos
str(nacimientos)

#Transformamos algunas variables
nacimientos$fecha <- as.factor(nacimientos$fecha)
#Utilizando la librería lubridate convertimos la fecha
nacimientos$fecha <- dmy(nacimientos$fecha)

#Gráfico sencillo
ggplot(data = nacimientos, mapping = aes(x = fecha, y = nacimientos)) +
  geom_line()

#Agregamos titulos y nombre a ejes
ggplot(data = nacimientos, mapping = aes(x = fecha, y = nacimientos)) +
  geom_line() +
  labs(title = "Cantidad de nacimientos por año en Costa Rica de 1965 a 2019",
       x = "Año",
       y = "Cantidad de nacimientos",
       caption = "Fuente de datos: Caja Costarricense del Seguro Social") +
  scale_y_continuous(labels = scales::comma)

# Se agrupa el grafico por sexo 
ggplot(data = nacimientos, mapping = aes(x = fecha, y = nacimientos, color = sexo)) +
  geom_line() +
  labs(title = "Cantidad de nacimientos por año en Costa Rica de 1965 a 2019",
       x = "Año",
       y = "Cantidad de nacimientos",
       caption = "Fuente de datos: Caja Costarricense del Seguro Social") +
  scale_color_manual(values = c("#32EA55","#E438B7"))+
  theme(legend.position = "bottom")+
  scale_y_continuous(labels = scales::comma)



#Ejemplo #3: gráfico comparativo transversal
#Se cargan los datos
Datos <- read.csv('nacimientoscr2.csv', header=TRUE, sep=';',dec='.', check.names = F)
str(Datos)
#Transformación de variables
Datos$fecha <- as.factor(Datos$fecha)
Datos$fecha <- dmy(Datos$fecha)

#definición del canva
ggplot(data = Datos, 
       mapping = aes(x = nacimientos_Totales, 
                     y = factor(year(fecha)))) 

#Se agrega la geometría
ggplot(data = Datos, 
       mapping = aes(x = nacimientos_Totales, 
                     y = factor(year(fecha)))) +
  geom_point(mapping = aes(x = Nacimientos_hombres, color = "Masculino"),
             size = 3) +
  geom_point(mapping = aes(x = Nacimientos_mujeres, color = "Femenino"),
             size = 3)


#Se agrega geometría para definición visualizar pares de elementos
ggplot(data = Datos, 
       mapping = aes(x = nacimientos_Totales, 
                     y = factor(year(fecha)))) +
  geom_segment(mapping = aes(x = Nacimientos_mujeres, xend = Nacimientos_hombres,
                             y = factor(year(fecha)),
                             yend = factor(year(fecha))), 
               size = 1.2,
               color = "gray80") +
  geom_point(mapping = aes(x = Nacimientos_hombres, color = "Masculino"),
             size = 3) +
  geom_point(mapping = aes(x = Nacimientos_mujeres, color = "Femenino"),
             size = 3)

#Se agrega texto a cada uno de los elementos con geometría de texto
ggplot(data = Datos, 
       mapping = aes(x = nacimientos_Totales, 
                     y = factor(year(fecha)))) +
  geom_segment(mapping = aes(x = Nacimientos_mujeres, xend = Nacimientos_hombres,
                             y = factor(year(fecha)),
                             yend = factor(year(fecha))), 
               size = 1.2,
               color = "gray80") +
  geom_point(mapping = aes(x = Nacimientos_hombres, color = "Masculino"),
             size = 3) +
  geom_point(mapping = aes(x = Nacimientos_mujeres, color = "Femenino"),
             size = 3) +
  geom_text(mapping = aes(x = Nacimientos_mujeres,
                          label = Nacimientos_mujeres),
            hjust = 1.7) +
  geom_text(mapping = aes(x = Nacimientos_hombres,
                          label = Nacimientos_hombres),
            hjust = -.4)


# Se genera la estetica 
ggplot(data = Datos, 
       mapping = aes(x = nacimientos_Totales, 
                     y = factor(year(fecha)))) +
  geom_segment(mapping = aes(x = Nacimientos_mujeres, xend = Nacimientos_hombres,
                             y = factor(year(fecha)),
                             yend = factor(year(fecha))), 
               size = 1.2,
               color = "gray80") +
  geom_point(mapping = aes(x = Nacimientos_hombres, color = "Masculino"),
             size = 3) +
  geom_point(mapping = aes(x = Nacimientos_mujeres, color = "Femenino"),
             size = 3) +
  geom_text(mapping = aes(x = Nacimientos_mujeres,
                          label = Nacimientos_mujeres),
            hjust = 1.7) +
  geom_text(mapping = aes(x = Nacimientos_hombres,
                          label = Nacimientos_hombres),
            hjust = -.4)+
  labs(title = "Nacimientos según sexo, Costa Rica de 1950 al 2013",
       x = "",
       y = "Año",
       color = "Sexo",
       caption = "Fuente de datos: Caja Costarricense del Seguro Social") +
  scale_color_manual(values = c("#e35d6a","#0392cf"))+
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text.x = element_blank())

#Ejemplo #4: grafico de calor

#Se cargan los datos
temperaturas <- read.csv2("temperaturasEuropa.csv", fileEncoding="latin1", dec = ".")
str(temperaturas)
colnames(temperaturas)[c(1,14:18)] <- c("ciudad","media","amplitud","latitud","longitud","region")
temperaturas

#Utilizamos la función gather de la librería tidyr para crear un data frame a partir de otro
temperaturas <- gather(data = temperaturas, 
                       key = "mes", 
                       value = "temperatura",
                       # variables a ignorar
                       -c(ciudad, media,amplitud,latitud,longitud,region))
head(temperaturas)

#Graficamos las temperaturas por mes para cada ciudad
ggplot(data = temperaturas, 
       mapping = aes(x = mes, y = temperatura, color = ciudad, group = ciudad)) +
  geom_line()

#transformamos la variable mes para que la misma sea categoria ordenada
temperaturas$mes <- factor(temperaturas$mes,levels = unique(temperaturas$mes),ordered = T)

#Volvemos a graficar con el mes como categorico 
ggplot(data = temperaturas, 
       mapping = aes(x = mes, y = temperatura, color = ciudad, group = ciudad)) +
  geom_line()


#Se modifica la presentacion 
ggplot(data = temperaturas, 
       mapping = aes(x = mes, y = temperatura, color = ciudad, group = ciudad)) +
  geom_line() +
  labs(title = "Temperatura promedio mensual para 35 cuidades europeas",
       x = "Mes",
       y = "Temperatura (grados centigrados)") +
  guides(color = guide_legend(title = "Ciudades", nrow = 3)) +
  theme(legend.position = "top")


#Se modifica la presentacion del grafico para mejor visualizacion
ggplot(data = temperaturas, 
       mapping = aes(x = mes, y = ciudad, fill = temperatura)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "B") 

# Se ordenan los valores por mes
ggplot(data = temperaturas, 
       mapping = aes(x = mes,
                     y = fct_reorder(ciudad, temperatura,.fun = median), 
                     fill = temperatura)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "B", 
                       guide = guide_colorbar(barwidth =  unit(20,"cm"),
                                              title = "Temperatura",
                                              title.vjust = 1,
                                              title.position = "top"),
                       breaks = seq(-10,30,5), # Cortes cada 5 grados
                       limits = c(-10,30)) + # Las temperaturas van de -10 a 30
  scale_x_discrete(expand = c(0,0)) + # Eliminar espacios en el eje x
  scale_y_discrete(expand = c(0,0)) + # Eliminar espacios en el eje y
  labs(title = "Temperatura de 35 ciudades europeas",
       y = "Ciudad",
       x = "Mes") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 18, hjust = .5) # centrar el titulo y aumentar el tamaño
  )


#Ejemplo#5 - PIB

#Se cargan los datos
oceania <- read.csv('Oceania.csv', header=TRUE, sep=';',dec=',', check.names = F)
str(oceania)

#Se gr?fica sencillo ordenando las observaciones por la variable GDP
ggplot(data = oceania, 
       mapping = aes(x = fct_reorder(country, GDP), y = GDP)) +
  geom_col() +
  coord_flip() +
  labs(title = "Pa?ses de Ocean?a ordenados por su Producto Interno Bruto",
       y = "PIB",
       x = "Pa?s",
       caption = "Fuente de datos: Gapminder Foundation")

#Dado que el gr?fico es un poco confuso, modificamos la presentacion 
ggplot(data = oceania, 
       mapping = aes(x = fct_reorder(country, -GDP), y = log10(GDP))) +
  geom_col() +
  coord_flip() +
  labs(title = "Pa?ses de Ocean?a ordenados por su Producto Interno Bruto",
       y = "PIB",
       x = "Pa?s",
       caption = "Fuente de datos: Gapminder Foundation")

#Delimitamos ejes y aspectos de presentacion
ggplot(data = oceania, 
       mapping = aes(x = fct_reorder(country, -GDP), y = log10(GDP))) +
  geom_col() +
  # Delimitamos el eje y
  coord_flip(ylim = log10(c(3.1e8, 9.9e11))) +
  # Cambiamos los textos de las etiquetas
  scale_y_continuous(breaks = log10(c(3.1e8, 1e9, 3.e9, 1e10,
                                      3.e10, 1e11, 3.e11, 1e12)),
                     labels = c("0.3", "1.0", "3.0", "10", "30",
                                "100", "300", "1000")) +
  labs(title = "Pa?ses de Ocean?a ordenados por su Producto Interno Bruto",
       y = "PIB",
       x = "Pa?s",
       caption = "Fuente de datos: Gapminder Foundation")


#en lugar de puntos cambiamos la geometr?a por puntos
ggplot(oceania, aes(x = reorder(country, -GDP), y = log10(GDP))) + 
  geom_point(size = 3.5) +
  scale_y_continuous(breaks = log10(c(3e8, 1e9, 3.e9, 1e10,
                                      3.e10, 1e11, 3.e11, 1e12)),
                     labels = c("0.3", "1.0", "3.0", "10",
                                "30", "100", "300", "1000"),
                     name = "              PIB (billones USD)",
                     limits = log10(c(3e7, 9.9e11)),
                     expand = c(0, 0)) +
  coord_flip() +
  theme(plot.margin = margin(12, 6, 3, 0))

#Se agregan detalles de presentacion
ggplot(oceania, aes(x = reorder(country, -GDP), y = log10(GDP))) + 
  geom_point(size = 3.5,
             color = "steelblue") +
  geom_text(aes(label = country,
                y = log10(GDP) - .08),
            hjust = 1,
            size = 4,
            alpha = 1) +
  scale_y_continuous(breaks = log10(c(3e8, 1e9, 3.e9, 1e10,
                                      3.e10, 1e11, 3.e11, 1e12)),
                     labels = c("0.3", "1.0", "3.0", "10",
                                "30", "100", "300", "1000"),
                     name = "              PIB (billones USD)",
                     limits = log10(c(3e7, 9.9e11)),
                     expand = c(0, 0)) +
  scale_x_discrete(name = NULL,
                   breaks = NULL) +
  coord_flip() +
  labs(title = "Pa?ses de Ocean?a ordenados por su Producto Interno Bruto",
       y = "PIB",
       x = "Pa?s",
       caption = "Fuente de datos: Gapminder Foundation") +
  theme(plot.margin = margin(12, 6, 3, 0))
