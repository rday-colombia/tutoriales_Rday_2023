#----------------------------------------------
#---- Visualización de Datos en R -------------
#     Graficos interactivos 
#----------------------------------------------

###############################################################
### Agenda:   #################################################
### 1. Animaciones.                   #########################
### 2. Gráficas interactivas.        ##########################
###############################################################
###############################################################

###############################################################

#############################################

# Activar paquetes
# (SE DEBE HACER CADA VEZ QUE SE INICIA R)
library(tidyverse)
library(lubridate)
library(scales)
library(RColorBrewer)
# install.packages
# install.packages("av")
# install.packages("gifski")
library(gganimate)
# install.packages("plotly")
library(plotly)
# install.packages("htmlwidgets")
library(htmlwidgets)
library(gapminder)
##############################################
### Fijar directorio de trabajo ##############
##############################################

getwd()

# (SE RECOMIENDA HACER CADA VEZ QUE SE INICIA R)

# Control + Shift + H

# Limpiar espacio de trabajo

rm(list = ls())

##############################################
### Importar datos ###########################
##############################################

### 1. Prueba Saber11 2019-2

saber11_2019 <- read_csv("saber11_2019.csv")

### 2. Datos Covid - 19 Colombia
covid_colombia <- read_csv("https://www.datos.gov.co/api/views/gt2j-8ykr/rows.csv?accessType=DOWNLOAD")

### 3. Datos de la trm en Colombia:
trm <- read_csv("https://www.datos.gov.co/api/views/mcec-87by/rows.csv?accessType=DOWNLOAD")


###############################################################################
##### 1. Animaciones                                                 ##########
###############################################################################

## Hacer animaciones con gráficos de ggplot

# Partimos del siguiente gráfico:
# ESTU_INSE_INDIVIDUAL; índice socioeconomico del evaluado
# PUNT_GLOBAL; puntaje total obtenido.

g <- ggplot(data = saber11_2019, 
            mapping = aes(x = ESTU_INSE_INDIVIDUAL, 
                          y = PUNT_GLOBAL)) + 
  geom_point()

g

# chequear 

table(saber11_2019$FAMI_ESTRATOVIVIENDA)

# Comenzamos con una animación sencilla:

animacion <- g + 
  transition_states(FAMI_ESTRATOVIVIENDA,
                    transition_length = 2,
                    state_length = 1)

animacion


# La función transition_states divide los datos de la gráfica de acuerdo con una
# variable discreta y hace la animación entre los diferentes valores de esta 
# variable. Pueden buscar otras manera de hacer transiciones buscando las funciones 
# `transition_*`. 




# Para cambiar la manera como se hace el entrelazado entre los estados (definidos 
# por la variable discreta), se usa la función `ease_aes()`. 
# Por defecto el entrelazado se hace de manera lineal. Para cambiar este comportamiento:

animacion + ease_aes('cubic-in-out')

# Lo anterior se podría complementar con un título que indique cuál estrato se está mostrando:

animacion + 
  labs(title = "Ahora mostrando {closest_state}",
       subtitle = "Recuadro {frame} de {nframes}")


# La anterior animación sugeriría que un estudiante estaría cambiando de estrato. 
# Para hacer más claro lo que está pasando, se podrían cambiar los colores de la 
# gráfica de acuerdo con los categorías de la variable de estrato:

ggplot(data = saber11_2019, 
       mapping = aes(x = ESTU_INSE_INDIVIDUAL, 
                     y = PUNT_GLOBAL, 
                     color = FAMI_ESTRATOVIVIENDA)) + 
  geom_point() + 
  scale_color_discrete(na.translate = FALSE) +
  transition_states(FAMI_ESTRATOVIVIENDA,
                    transition_length = 2,
                    state_length = 1) + 
  labs(title = "Ahora mostrando {closest_state}",
       subtitle = "Recuadro {frame} de {nframes}")



# La otra opción es indicar que son grupos distintos:

ggplot(data = saber11_2019, 
       mapping = aes(x = ESTU_INSE_INDIVIDUAL, 
                     y = PUNT_GLOBAL, 
                     group = seq_along(FAMI_ESTRATOVIVIENDA), 
                     color = FAMI_ESTRATOVIVIENDA)) + 
  geom_point() + 
  scale_color_discrete(na.translate = FALSE) + 
  transition_states(FAMI_ESTRATOVIVIENDA,
                    transition_length = 2,
                    state_length = 1)


# La animación anterior quedó un poco estática. 
# Simplemente aparecen y desaparecen los tres grupos. 
# Para hacerla un poco más interactiva se podría usar las funciones `enter_*` y `exit_*`.

ggplot(data = saber11_2019, 
       mapping = aes(x = ESTU_INSE_INDIVIDUAL, 
                     y = PUNT_GLOBAL, 
                     color = FAMI_ESTRATOVIVIENDA)) + 
  geom_point() + 
  scale_color_discrete(na.translate = FALSE) +
  transition_states(FAMI_ESTRATOVIVIENDA,
                    transition_length = 2,
                    state_length = 1) +
  enter_fade() + 
  exit_shrink()

## Para exportar la anterior animación se puede usar la función anim_save

anim_save("animacion_estratos.gif")

# Para hacer la animación en todos los casos anteriores, 
# "detras de cámaras" se está llamando la función `animate()`.
# Esta función tienen varios argumentos, como el número de recuadros que se usan
# en la animación y la manera como se hace la animación. 
# Por defecto la animación se hace en gif. Si se quisiera otro tipo de animación, 
# por ejemplo en video, tendría que cambiarse las opciones por defecto de la función `animate`.

library(av)

animacion2 <- ggplot(data = saber11_2019, 
                     mapping = aes(x = ESTU_INSE_INDIVIDUAL, 
                                   y = PUNT_GLOBAL, 
                                   color = FAMI_ESTRATOVIVIENDA)) + 
  geom_point() + 
  scale_color_discrete(na.translate = FALSE) +
  transition_states(FAMI_ESTRATOVIVIENDA,
                    transition_length = 2,
                    state_length = 1) +
  enter_fade() + 
  exit_shrink()

animate(animacion2, renderer = av_renderer())

## Recursos recomendados:

# https://gganimate.com/articles/gganimate.html#your-first-animation

# https://www.r-graph-gallery.com/animation.html


# Otro Ejemplo (2)

# Librerias 
library(ggplot2)
library(gganimate)
library(babynames)
library(hrbrthemes)
options(scipen = 999)

# 
don <- babynames %>% 
  filter(name %in% c("Ashley", "Patricia", "Helen")) %>%
  filter(sex=="F")

# Plot
don %>%
  ggplot( aes(x=year, y=n, group=name, color=name)) +
  geom_line() +
  geom_point() +
  scale_color_discrete(na.translate = FALSE) +
  ggtitle("Popularidad de nombres estadounidenses en los últimos 30 años") +
  theme_ipsum() +
  ylab("Número de bebes nacidas") +
  transition_reveal(year)

# Save at gif:
anim_save("287-smooth-animation-with-tweenr.gif")

# Otro ejemplo(3)  

# Obtener los datos
library(gapminder)

# Cargar librerías 
library(ggplot2)
library(gganimate)
options(scipen = 999) # notación c
# Make a ggplot, but add frame=year: one image per year
ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  # gganimate bits específicos:
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'Esperanza de vida') +
  transition_time(year) +
  ease_aes('linear')

# Save at gif:
anim_save("271-ggplot2-animated-gif-chart-with-gganimate1.gif")

# Otro ejemplo (4)

# Get data:
library(gapminder)

# Charge libraries:
library(ggplot2)
library(gganimate)
options(scipen = 999)
# Make a ggplot, but add frame=year: one image per year
ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'Esperanza de vida') +
  transition_time(year) +
  ease_aes('linear')

# Save at gif:
anim_save("271-ggplot2-animated-gif-chart-with-gganimate2.gif")

# Otro ejemplo (5)

teachingApps::teachingApp(app_name = "distribution_normal_functions")


###############################################################################
##### 2. Gráficos interactivos                                       ##########
###############################################################################

# La función ggplotly del paquete plotly permite convertir cualquier gráfico 
# creado con ggplot en un gráfico interactivo. 

# Ejemplo 1

# Véamos un ejemplo con datos del paquete gapminder:

# Cargo los datos de gapminder
data(gapminder)

# Filtro solamente los registros del año 2007
gap_2007 <- gapminder %>%
  filter(year==2007) 

# Creo la gráfica y la asigno al objeto llamado grafica:
grafica <- ggplot(data = gap_2007, mapping = aes(x = gdpPercap, 
                                                 y = lifeExp, 
                                                 size = pop, 
                                                 color=continent)) +
  geom_point() +
  theme_bw()

# Vuelvo la gráfica interactiva
ggplotly(grafica)

# Para exportarla como html:
# mirar 
saveWidget(grafica, file="grafica.html")

# Ejemplo 2

# Convertir VIGENCIAHASTA a fecha:

trm <- trm %>% mutate(fecha = dmy(VIGENCIAHASTA))

# Crear gráfica
gr_trm <- ggplot(data = trm, mapping = aes(x = fecha, y = VALOR)) + 
  geom_area(fill = "rosybrown2") + 
  scale_y_continuous(label = label_dollar(prefix = "COP $", 
                                          big.mark = ",", 
                                          decimal.mark = "."))  +
  labs(y = "TRM", 
       x = "Fecha")
gr_trm

# Convertir a gráfica interactiva
ggplotly(gr_trm)

# Ejemplo Covid
casos_diarios_x_depto <- covid_colombia %>% 
  group_by(`fecha reporte web`, `Nombre departamento`) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  group_by(`Nombre departamento`) %>% 
  mutate(acumulados = cumsum(n)) %>% 
  arrange(`Nombre departamento`, `fecha reporte web`)



# Actividad 
View(covid_colombia)
library(lubridate)

casos_diarios_x_depto <- covid_colombia %>% 
  group_by(`fecha reporte web`, `Nombre departamento`) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  group_by(`Nombre departamento`) %>% 
  mutate(acumulados = cumsum(n)) %>% 
  arrange(`Nombre departamento`, `fecha reporte web`)

fecha <- casos_diarios_x_depto$`fecha reporte web`
FECHA <- dmy_hms(fecha)


grafica_deptos <- ggplot(data = casos_diarios_x_depto, 
                         mapping = aes(x = FECHA , 
                                       y = acumulados)) + 
  geom_line()  +
  theme(legend.position = "none", 
        text = element_text(family = "serif"), 
        axis.title.x = element_text(face = "bold"), 
        panel.grid = element_blank(), 
        axis.line = element_line(colour = "Black"), 
        axis.text.x = element_text(angle = 90)) + 
  scale_y_log10(label = label_comma(big.mark = ".",
                                    decimal.mark = ",")) + 
  scale_x_datetime(date_breaks = "2 month", 
                   date_labels = "%d %b") + 
  facet_wrap(vars(`Nombre departamento`))

grafica_deptos

ggplotly(grafica_deptos)

# Recurso recomendado: https://plotly.com/ggplot2/


# Otra data 

enlace<-'https://n9.cl/h5vdu'
DatosApt<- read.table(file=enlace, header=TRUE)
attach(DatosApt)

ggplot(data = DatosApt, 
       mapping = aes(x = mt2, 
                     y = precio, 
                     color = estrato)) + 
  geom_point() + 
  transition_states(estrato,
                    transition_length = 2,
                    state_length = 1) +
  enter_fade() + 
  exit_shrink()+
  # gganimate bits específicos:
  labs(title = 'Estrato: {frame_time}', x = 'mt2', y = 'Precio') +
  transition_time(estrato) +
  ease_aes('linear')

# Adicionales 

library(tidyverse)
library(rmarkdown)
library(socviz) # funciones utiles y conjunto de datos para visualización de datos 


# 4. Datos educación Colombia ---------------------------------------------

educacion <- read_csv("https://www.datos.gov.co/api/views/nudc-7mev/rows.csv?accessType=DOWNLOAD")
head(educacion)
names(educacion)
View(educacion)
#### Bucles para generar gráficas de manera "automatizada" sin repetir código
### Hacer gráficas para varias variables:
table(educacion$AÑO)
# se toma un año 
educacion_2019 <- educacion %>% filter(AÑO == 2019)
View(educacion_2019)
# se seleccionan las variables a graficar 
variables <- c("REPITENCIA", "DESERCIÓN", "APROBACIÓN")

for (i in variables){
  grafica <- ggplot(data = educacion_2019, mapping = aes_string(x =i)) + 
    geom_histogram() + labs(title = paste("Distribución de", str_to_title(i)))
  ggsave(paste0(i, ".png"))
  assign(i, grafica)
  
  print(grafica)
}
facet_wrap(vars(expo2))
transition_states( Municipios,
                   transition_length = 2,
                   state_length = 1)+
  exit_shrink( )
# Hacer gráficas para varios años -----------------------------------------

for (i in 2011:2019){
  datos <- educacion %>% filter(AÑO == i)
  grafica <- ggplot(data = datos, mapping = aes(x = DESERCIÓN)) + 
    geom_density() + labs(title = paste("Distribución de Deserción en ", i))
  ggsave(paste0("Deserción_", i, ".png"))
  print(grafica)
  assign(paste0("Deserción_", i), grafica)
}


# Hacer gráficas para varios departamentos --------------------------------

departamentos <- c("Antioquia", "Magdalena", "Atlántico")

for (i in departamentos){
  datos <- educacion_2019 %>% filter(DEPARTAMENTO == i)
  grafica <- ggplot(data = datos, mapping = aes(x = DESERCIÓN)) + 
    geom_density() + labs(title = paste("Distribución de Deserción 2019 en ", i))
  ggsave(paste0("Deserción_", i, ".png"))
  print(grafica)
  assign(paste0("Deserción_", i), grafica)
}


# Hacer gráficas si se cumple una condición: ------------------------------


mun_x_depto <- educacion_2019 %>% group_by(DEPARTAMENTO) %>% count(sort = TRUE)

departamentos_todos <- unique(educacion_2019$DEPARTAMENTO)
departamentos_todos
for (i in departamentos_todos){
  depto <- educacion_2019 %>% filter(DEPARTAMENTO == i)
  n <- nrow(depto)
  if (n >50) {
    grafica <- ggplot(data = depto, mapping = aes(x = DESERCIÓN)) + 
      geom_density() + labs(title = paste("Distribución de Deserción en ", i))
    assign(paste0("Deserción2_", i), grafica)
  } else {
    print(paste0("El departamento de ", i, " tiene menos de 50 municipios"))
  }
}



# Otro ejemplo  -----------------------------------------------------------

head(saber11_2019)
names(saber11_2019)

### Hacer gráficas para varias variables:

saber11_femenino <- saber11_2019 %>% filter(ESTU_GENERO == "F")
View(saber11_femenino)
dim(saber11_femenino)

Variables_f <- c("PUNT_LECTURA_CRITICA", "PUNT_MATEMATICAS", "PUNT_INGLES")

for (i in Variables_f){
  grafica <- ggplot(data = saber11_femenino, mapping = aes_string(x =i)) + 
    geom_density() + labs(title = paste("Distribución de Femenino", str_to_title(i)))
  ggsave(paste0(i, ".png"))
  assign(i, grafica)
  print(grafica)
}


# Masculino  --------------------------------------------------------------

saber11_masculino <- saber11_2019 %>% filter(ESTU_GENERO == "M")
View(saber11_masculino)
dim(saber11_masculino)

Variables_m <- c("PUNT_LECTURA_CRITICA", "PUNT_MATEMATICAS", "PUNT_INGLES")

for (i in Variables_m){
  grafica <- ggplot(data = saber11_femenino, mapping = aes_string(x =i)) + 
    geom_density() + labs(title = paste("Distribución de Masculino", str_to_title(i)))
  ggsave(paste0(i, ".png"))
  assign(i, grafica)
  print(grafica)
}

