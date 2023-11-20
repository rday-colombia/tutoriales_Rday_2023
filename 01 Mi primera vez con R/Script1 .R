##### Mi primera vez con R-mi primer Script 
#### Clase 1- Primeros pasos con Rstudio

####visualización de ventanas R Studio


### Ejemplo de sintaxis 1:
demo(graphics)### Este demo ejecuta gráficos


#### Ejemplo 2: Creación de un vector de variables explicativas

variables <- c("nombre","genero", "edad", "profesion", "estatura")

#class(variables) ## Esto es un vector de caracter (nombres)


######### Ejemplo 3: Datos estudiantes

### vector 1: Nombre

Nombre <- c("Stael","Luis","Diomira","Nilsa","Daniela",
            "Ruth","Elmie","Jorge","Eddyson","Gabriela",
            "Arturo","Juliana")### Vector de caracter

Genero<- c("F","M","F","F","F","F","F","M","M","F","M","F") 
#### vector de caracter


Edad<- c(33, 36, 29, 34, 26, 35, 30, 29, 33, 27, 29, 36) #Vector numerico

Profesión<- c("Psicóloga", "Bióloga", "Enfermera", "Médico", "Microbióloga", "Médico", "Médico", "Gestor", "Médico", "Microbióloga", "Médico", "Veterinaria" ) ##vector de caracter

Estatura<- c(1.55, 1.70, 1.60, 1.64, 1.67, 1.51, 1.65, 1.84, 1.72, 1.63, 1.74, 1.60) ###vector numerico

#### Crear una base de datos con los vectores anteriores

datos1<-data.frame(cbind(Nombre,Genero,Edad,Profesión,Estatura))


####################### Parte 2

class(Nombre) ## Sirve para saber el tipo de variable u objeto

class(Genero)

class(datos1)

### Indexación de un vector de caracter:

#### Ejemplo: del vector Nombre, filtrar los primeros 5 nombres

Nombre[1:5]

Nombre[1] ## Selecciona el primer nombre
Nombre [12] # Selecciona el último

Nombre[c(1,3,5)] 
#Seleccionar las personas en las posiciones 1, 3 y 5

###### Ejemplo: Indexación lógica de un vector

### del vector Género, filtrar los de género masculino

#### Solución 1: usar la posición de los hombres

Genero[c(2,8,9,11)]

#### Solución2: Comparación lógica

which(Genero=="M")### Cuáles elementos de Genero son masculino

length(which(Genero=="M")) ## Cuántos hombres hay

table(Genero) 
### Sirve para obtener la frecuencia (cantidad) de la variable

### Ejercicio: Filtrar de la base de datos, los estudiantes
###menores a 30 años

length(which(Edad<30))
length(which(Edad>=30))

#### Quienes tienen edades < 30?

which(Edad<30) ## Averiguo las posiciones

Nombre[which(Edad<30)]

### Quienes tienen entre 25 y 30 años?

which(Edad>=25 & Edad<=30)

Nombre[which(Edad>=25 & Edad<=30)]

############## Indexación de una base de datos(dataframe)

############# Filtro tipo1: por filas

## Obtener los datos de los primeros 3 estudiantes 
##de la base de datos

datos1[1:3,]

datos1[c(1,3,5),] 
## Filtra los estudiantes de los estudiantes 
##en posiciones 1, 3 y 5


### Fltro tipo 2: Por columnas

datos1[,1:3] ## Filtrar las primeras 3 columnas

## Otra forma: indicar los nombres de la variables

datos1[,c("Nombre","Genero","Profesion")]

### Función auxliar:

names(datos1) ### Muestra los nombres del objeto datos1

dim(datos1) ### Muestra la dimensión (# de filas y # de columnas)

str(datos1) ## Muestra un resumen de la base de datos

######### Corregir el tipo de variables (Edad y Estatura)

attach(datos1) ## Sirve para fijar los nombres de las variables sin que estén 
## creados como vectores individuales en el enviroment

class(Edad)

class(Estatura)

### Convertir edad en numérica:

datos1$Edad<-as.numeric(Edad)

#### Convertir estatura en numérica:

datos1$Estatura<-as.numeric(Estatura)

class(Edad)

class(Estatura)

detach(datos1)

attach(datos1)


############ Tipo 3: Filtro por filas y columnas

### Ejercicio: Filtrar de los tres primeros estudiantes
### las variables edad y estatura.

datos1[1:3,c("Edad","Estatura")]

datos1[1:3,c(4,6)]

#Instrucción instalaciòn de Paquetes

#Instrucción cargar librerias

#Importar bases de datos - Calidad de Vida.




