# Titulo: Demo sobre conexión a BD desde R y realizar consultas
# Fecha: 24/11/2023
# Autor: Yeison Yovany Ocampo Naranjo


# Librerias requeridas ----------------------------------------------------


# Instalar los paquetes si aún no lo has hecho
if (!requireNamespace("DBI", quietly = TRUE)) {
  install.packages("DBI")
}

if (!requireNamespace("odbc", quietly = TRUE)) {
  install.packages("odbc")
}

# Cargar las bibliotecas
library(DBI)
library(odbc)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)


# Conecxion BD ------------------------------------------------------------

# verifiquemos antes de ejecutar que tenemos nuestra DNS creada en las ODBC
con <- dbConnect(odbc::odbc(), "ODBC_RDay_2023", timeout = 10)




# Creacion de consultas o llamados a la BD para traer datos ---------------


datos <- dbGetQuery(con,"SELECT CustomerID FROM Customers")

datos2 <- dbGetQuery(con, "select * from Orders")



# Consultas con parámetros desde R ----------------------------------------


fecha = "1997-01-01"
query = "Select * from Orders where OrderDate > ?"
datos3 <- dbGetQuery(conn = con,statement = query,params = list(fecha))



# Consultas con más de un parámetro ---------------------------------------


fecha = "1997-01-01"
country = 'France'
query = "Select * from Orders where OrderDate > ? and ShipCountry = ?"
datos4 <- dbGetQuery(conn = con,statement = query,params = list(fecha,country))



# Consultas con más de una tabla ------------------------------------------

datos5 <- dbGetQuery(conn = con,statement = "select O.*,OD.*,C.* from Orders AS O
                                              inner join Customers AS C on C.CustomerID = O.CustomerID
                                              inner join [Order Details] OD on OD.OrderID = O.OrderID")
