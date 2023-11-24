#--------------------------------------------
#       Visualización de Datos en R
#       R Day 2023 
#       Osnamir Bru Cordero 
# -------------------------------------------

#############################################
### Agenda:                                ##
#############################################

### 1. Tipos de variables y gráficos       ##
###    para tipos de variable              ##  
### 2. Exploración de datos categóricos    ##
### 3. Gráficos para UNA variable          ##
###    categórica                          ##
### 4. Gráficos para DOS variables         ##
###    categóricas 
##  5 . Gráficos de series de tiempo
#############################################
  

#############################################

# Activar paquetes

library(tidyverse)

##############################################
### Fijar directorio de trabajo ##############
##############################################

getwd()

# (SE RECOMIENDA HACER CADA VEZ QUE SE INICIA R)

# Control + Shift + H

# setwd("") cambiar el directorio manualmente

getwd()

##############################################
### Importar datos ###########################
##############################################
### 1. Prueba Saber11 2019-2

saber11_2019 <- read_csv("saber11_2019.csv")

### 2. Datos Covid - 19 Colombia
covid_colombia <- read_csv("https://www.datos.gov.co/api/views/gt2j-8ykr/rows.csv?accessType=DOWNLOAD")

# --------------------------------------------
#  Variables y tipos de gráficos 
#---------------------------------------------

# ------------------------------------------------
# Variables categóricas:                      
#   Ej. sexo, afiliación política,            
#     nivel educativo, estrato socioeconómico.
#   Tipos de gráficos:                        
#     Gráfico de barras y relacionados.       
#    Gráfico de pastel y relacionados (No recomendado). 
#    Variables cuantitativas:                    
#   Ej. Edad, salario, ingreso, puntajes,     
#     peso, talla, etc.                       
#   Tipos de gráficos:                        
#     Histograma                              
#     Polígono de frecuencias                 
#     Densidad                              
#--------------------------------------------------------


### Recurso muy recomendado:
# https://www.data-to-viz.com 


##############################################
### Explorar los datos #######################
##############################################

head(saber11_2019)
tail(saber11_2019)
dim(saber11_2019)
ncol(saber11_2019)
nrow(saber11_2019)
names(saber11_2019)
attach(saber11_2019)

# datos covid 
head(covid_colombia)
tail(covid_colombia)
dim(covid_colombia)
ncol(covid_colombia)
nrow(covid_colombia)
names(covid_colombia)

##############################################
### Exploración de variables categóricas #####
##############################################
table(saber11_2019$COLE_NATURALEZA)
prop.table(table(saber11_2019$COLE_NATURALEZA))
round(prop.table(table(saber11_2019$COLE_NATURALEZA)),2)

table(saber11_2019$FAMI_ESTRATOVIVIENDA)
prop.table(table(saber11_2019$FAMI_ESTRATOVIVIENDA))
round(prop.table(table(saber11_2019$FAMI_ESTRATOVIVIENDA)),3)

# Para covid 
table(covid_colombia$Estado)
prop.table(table(covid_colombia$Estado))
round(prop.table(table(covid_colombia$Estado)),3)

# ---------------------------------------------
# Geoms (capa geometrica)
# ---------------------------------------------

# Geoms en ggplot son los objetos geométricos 
# usados para representar los datos.

# Ej. geom_point sirve para realizar 
#  diagramas de dispersión.

## https://github.com/rstudio/cheatsheets/raw/master/data-visualization-2.1.pdf

############################################
## Gráficos para una variable categórica: ##
############################################

## Gráfico de barras

# Variable: COLE_NATURALEZA

ggplot(data = saber11_2019, 
       mapping = aes(x = COLE_NATURALEZA)) + 
  geom_bar()

# ¿Qué atributos estéticos (i.e. aesthetics) de la 
# gráfica se pueden modificar?
?geom_bar

# Modifiquemos el relleno y el color de las lineas
ggplot(data = saber11_2019, 
       mapping = aes(x = COLE_NATURALEZA)) + 
  geom_bar(fill="Darkgreen", color = "Black")

## http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
colours()
# demo("colors")

# Cambiemos el nombre de los ejes:

ggplot(data = saber11_2019, 
       mapping = aes(x = COLE_NATURALEZA)) + 
  geom_bar(fill="Darkgreen", color = "Black") +
  labs(y = "Número de estudiantes", 
       x = "Tipo de colegio")

# Y cambiemos el aspecto general de la gráfica

ggplot(data = saber11_2019, 
       mapping = aes(x = COLE_NATURALEZA)) + 
  geom_bar(fill="Darkgreen", color = "Black") +
  labs(y = "Número de estudiantes", 
       x = "Tipo de colegio") +
 theme_bw()

# ¿Cómo cambiar el eje y para que muestre
# la frecuencia relativa?

# https://d33wubrfki0l68.cloudfront.net/70a3b18a1128c785d8676a48c005ee9b6a23cc00/7283c/images/visualization-stat-bar.png

ggplot(data = saber11_2019, 
       mapping = aes(x = COLE_NATURALEZA, y = stat(prop), group = 1)) + 
  geom_bar(fill="Darkgreen", color = "Black") +
  labs(y = "Número de estudiantes", 
       x = "Tipo de colegio") +
  theme_bw()

# Una forma alternativa y equivalente

ggplot(data = saber11_2019, 
       mapping = aes(x = COLE_NATURALEZA)) + 
  geom_bar(aes(y = ..count../sum(..count..)), 
           fill="Darkgreen", color = "Black") +
  labs(y = "Número de estudiantes", 
       x = "Tipo de colegio") +
  theme_bw()

# Gráfico de pastel

# NO LOS USEN: https://www.data-to-viz.com/caveat/pie.html




##############################################
## Gráficos para DOS variables categóricas: ##
##############################################

ggplot(data = saber11_2019, 
       mapping = aes(x = COLE_AREA_UBICACION, 
                     fill = DESEMP_INGLES)) +
 geom_bar()

# Hay tres opciones relevantes para la manera como hace
# el relleno este tipo de gráficas. 
# stack es la opción por defecto
# Las otras dos son dodge y fill:

ggplot(data = saber11_2019, 
       mapping = aes(x = COLE_AREA_UBICACION, 
                     fill = DESEMP_INGLES)) +
  geom_bar(position = "dodge")

ggplot(data = saber11_2019, 
       mapping = aes(x = COLE_AREA_UBICACION, 
                     fill = DESEMP_INGLES)) +
  geom_bar(position = "fill")

# Por último, si quiero cambiar la orientación de la gráfica
# Puedo usar la capa coord_flip

ggplot(data = saber11_2019, 
       mapping = aes(x = COLE_AREA_UBICACION, 
                     fill = DESEMP_INGLES)) +
  geom_bar(position = "fill") + 
  coord_flip()

## vignette("ggplot2-specs")

###############################################################################
### Gráficas para ver la distribución de una variable cuantitativa/numérica ###
###############################################################################

## Histograma

ggplot(data = saber11_2019, mapping = aes(x = PUNT_GLOBAL)) + 
  geom_histogram()

# Por defecto, ggplot hace los histogramas con 30 categorías. 
# Esto se puede cambiar con las opciones bins y binwidth:

ggplot(data = saber11_2019, mapping = aes(x = PUNT_GLOBAL)) + 
  geom_histogram(bins = 15)

ggplot(data = saber11_2019, mapping = aes(x = PUNT_GLOBAL)) + 
  geom_histogram(binwidth = 30)

# Una gráfica alternativa o complementaria al histograma es el polígono de 
# frecuencias:

ggplot(data = saber11_2019, mapping = aes(x = PUNT_GLOBAL)) + 
  geom_histogram() + 
  geom_freqpoly()

ggplot(data = saber11_2019, mapping = aes(x = PUNT_GLOBAL)) + 
  geom_histogram(bins = 15, fill = "lightsteelblue", color = "black") + 
  geom_freqpoly(bins = 15)

#  mirar  
#  ggplot(data = saber11_2019, mapping = aes(x = PUNT_GLOBAL)) + 
#  geom_histogram(bins = 15, fill = "lightsteelblue", color = "black") + 
#  geom_smooth()
  
ggplot(data = saber11_2019, mapping = aes(x = PUNT_GLOBAL)) + 
  geom_freqpoly(bins = 15)

# Una alternativa al histograma y al polígono de frecuencias es la densidad de kernel, 
# una versión suavizada del histograma:

ggplot(data = saber11_2019, mapping = aes(x = PUNT_GLOBAL)) + 
  geom_density()

# Algunas opciones que puede ser interesante modificar son el ancho de banda y 
# el tipo de kernel con el que se estima la densidad 
?geom_density

# Las gráficas anteriores podrían complementarse con una línea vertical 
# para señalar algún valor  en particular en la distribución.
# Por ejemplo, se podría querer agregar una línea vertical en la media de la
# variable
# ¿Cómo se haría esto?

round(mean(saber11_2019$PUNT_GLOBAL, na.rm = TRUE),2)


ggplot(data = saber11_2019, mapping = aes(x = PUNT_GLOBAL)) + 
  geom_density() + 
  geom_vline(xintercept = 246.59)

ggplot(data = saber11_2019, mapping = aes(x = PUNT_GLOBAL)) + 
  geom_density() + 
  geom_vline(xintercept = mean(saber11_2019$PUNT_GLOBAL, na.rm = TRUE))

ggplot(data = saber11_2019, mapping = aes(x = PUNT_GLOBAL)) + 
  geom_density() + 
  geom_vline(xintercept = mean(saber11_2019$PUNT_GLOBAL, na.rm = TRUE), 
             color = "Red")


###############################################################################
### Gráficas para ver la distribución de una variable cuantitativa/numérica ###
### y una categórica                                                        ###
###############################################################################

# Diagrama de cajas:

ggplot(data = saber11_2019, mapping = aes(x = COLE_AREA_UBICACION, 
                                          y = PUNT_GLOBAL)) +
  geom_boxplot()

ggplot(data = saber11_2019, mapping = aes(x = COLE_AREA_UBICACION, 
                                          y = PUNT_GLOBAL)) +
  geom_boxplot() + 
  coord_flip()

###############################################################################
### Paréntesis                                                              ###
### Diagrama de cajas para UNA VARIABLE                                     ###
###############################################################################
ggplot(data = saber11_2019, mapping = aes(x = "", 
                                          y = PUNT_GLOBAL)) +
  geom_boxplot() + 
  coord_flip()


###############################################################################
### Cierra Paréntesis                                                       ###
###############################################################################
#----------------------------------------------
# Stop 
# Como colocar los valores de las barras 
dat2 <- data.frame(
  Genero = factor(c("Mujer","Mujer","Mujer","Mujer","Mujer","Hombre","Hombre","Hombre","Hombre", "Hombre")),
  Niveles = factor(c("Muy baja","Baja","Moderada","Alta", "Muy Alta", "Muy baja","Baja","Moderada","Alta", "Muy Alta"), levels=c("Muy baja","Baja","Moderada","Alta", "Muy Alta")),
  Puntaje = c(17.6, 24.6, 28.9, 23.9, 4.9, 15.8, 30, 35.8, 16.7, 1.7)
)
dat2

ggplot(data=dat2, aes(x=Niveles, y=Puntaje, fill=Genero)) +
  geom_bar(stat="identity", position=position_dodge())+ 
  scale_fill_manual(values=c("#56B4E9", "#F0E442"))+ 
  geom_text(aes(label = Puntaje),vjust = 1.5, colour = "black", position = position_dodge(.9))





# ------------------------------------------
# Comparación densidades:

ggplot(data = saber11_2019, mapping = aes(x = PUNT_GLOBAL, 
                                          color = COLE_AREA_UBICACION)) +
  geom_density()

ggplot(data = saber11_2019, mapping = aes(x = PUNT_GLOBAL, 
                                          fill = COLE_AREA_UBICACION)) +
  geom_density()

ggplot(data = saber11_2019, mapping = aes(x = PUNT_GLOBAL, 
                                          fill = COLE_AREA_UBICACION, 
                                          alpha = COLE_AREA_UBICACION)) +
  geom_density()

ggplot(data = saber11_2019, mapping = aes(x = PUNT_GLOBAL, 
                                          fill = COLE_AREA_UBICACION, 
                                          alpha = COLE_AREA_UBICACION, 
                                          color = COLE_AREA_UBICACION)) +
  geom_density()

#---------------------------------------------
# Gráficas para visualizar series de tiempo                               ###
#---------------------------------------------
# Libraries
library(ggplot2)

# create data
xValue <- 1:10
yValue <- cumsum(rnorm(10))
data <- data.frame(xValue,yValue)

# Plot
ggplot(data, aes(x=xValue, y=yValue)) +
  geom_line()

#---
# Libraries
library(ggplot2)
library(hrbrthemes)

# create data
xValue <- 1:10
yValue <- cumsum(rnorm(10))
data <- data.frame(xValue,yValue)

# Plot
ggplot(data, aes(x=xValue, y=yValue)) +
  geom_line( color="#69b3a2", size=2, alpha=0.9, linetype=1) +
  theme_ipsum() +
  ggtitle("Evolution of something")


# ------
# Libraries
library(ggplot2)
library(dplyr)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/3_TwoNumOrdered.csv", header=T)
data$date <- as.Date(data$date)

# Plot
data %>%
  tail(10) %>%
  ggplot( aes(x=date, y=value)) +
  geom_line() +
  geom_point()

# ---------

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/3_TwoNumOrdered.csv", header=T)
data$date <- as.Date(data$date)

# Plot
data %>%
  tail(10) %>%
  ggplot( aes(x=date, y=value)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  theme_ipsum() +
  ggtitle("Evolución del precio del Bitcoin")
#----------

# Libraries
library(ggplot2)
library(dplyr)
library(babynames)
library(ggrepel)
library(tidyr)

# data
data <- babynames %>% 
  filter(name %in% c("Ashley", "Amanda")) %>%
  filter(sex=="F") %>%
  filter(year>1970) %>%
  select(year, name, n) %>%
  spread(key = name, value=n, -1)

# plot
data %>% 
  ggplot(aes(x=Amanda, y=Ashley, label=year)) +
  geom_point() +
  geom_segment(aes(
    xend=c(tail(Amanda, n=-1), NA), 
    yend=c(tail(Ashley, n=-1), NA)
  )
  ) 
#-----------
# Libraries
library(ggplot2)
library(dplyr)
library(babynames)
library(ggrepel)
library(tidyr)

# data
data <- babynames %>% 
  filter(name %in% c("Ashley", "Amanda")) %>%
  filter(sex=="F") %>%
  filter(year>1970) %>%
  select(year, name, n) %>%
  spread(key = name, value=n, -1)

# Select a few date to label the chart
tmp_date <- data %>% sample_frac(0.3)

# plot 
data %>% 
  ggplot(aes(x=Amanda, y=Ashley, label=year)) +
  geom_point(color="#69b3a2") +
  geom_text_repel(data=tmp_date) +
  geom_segment(color="#69b3a2", 
               aes(
                 xend=c(tail(Amanda, n=-1), NA), 
                 yend=c(tail(Ashley, n=-1), NA)
               ),
               arrow=arrow(length=unit(0.3,"cm"))
  ) +
  theme_ipsum()

#-------------

library(lubridate)
trm <- read_csv("https://www.datos.gov.co/api/views/mcec-87by/rows.csv?accessType=DOWNLOAD")

trm <- trm %>% 
  mutate(fecha = dmy(VIGENCIAHASTA))

ggplot(data = trm, 
       mapping = aes(x = fecha, y = VALOR)) + 
  geom_line()

ggplot(data = trm, 
       mapping = aes(x = fecha, y = VALOR)) + 
  geom_area()

# otra 
# Libraries
library(ggplot2)
library(babynames) # provide the dataset: a dataframe 
library(dplyr)

# Keep only 3 names
don <- babynames %>% 
  filter(name %in% c("Ashley", "Patricia", "Helen")) %>%
  filter(sex=="F")

# Plot
don %>%
  ggplot( aes(x=year, y=n, group=name, color=name)) +
  geom_line()







