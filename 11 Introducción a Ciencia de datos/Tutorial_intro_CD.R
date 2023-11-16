#################################
#Econometria
#Tutorial_intro_CD -Introduccion a la ciencia de datos: Clustering######
#Profesora: Nini Johana Marin-Rodriguez
#Universidad de Medellin
#Fuente original: https://uc-r.github.io/kmeans_clustering
#################################

## #Librerias --------------------------------------------------------------
library(datasets)
library(data.table) # organizacion datos
library(ggplot2)    # visualizacion
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization


##Cargo datos disponibles en la libreria datasets --------------------------------------------------------------
df <- USArrests
df <- na.omit(df)
df <- scale(df)
head(df)

#for visualizing a distance matrix
distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
#dev.off()

##k-means --------------------------------------------------------------
k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)

#Si imprimimos los resultados veremos que nuestras agrupaciones dieron
#lugar a 2 tamaños de conglomerados de 30 y 20. Vemos los centros de 
#los conglomerados (medias) para los dos grupos en las cuatro variables
#(Asesinato, Asalto, UrbanPop, Violación). También obtenemos la asignación
#de conglomerados para cada observación (es decir, Alabama se asignó al
#conglomerado 2, Arkansas se asignó al conglomerado 1, etc.).
k2

#Grafico los clusters
fviz_cluster(k2, data = df)

#Alternativamente, puede utilizar gráficos de dispersión por pares 
#estándar para ilustrar los conglomerados en comparación con las 
#variables originales
df %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         state = row.names(USArrests)) %>%
  ggplot(aes(UrbanPop, Murder, color = factor(cluster), label = state)) +
  geom_text()

#Podemos ejecutar el mismo proceso para 3, 4 y 5 clusters, y los 
#resultados se muestran en la figura:

k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")


n1 <- fviz_cluster(k2, data = df)
n2 <- fviz_cluster(k3, data = df)
n3 <- fviz_cluster(k4, data = df)
n4 <- fviz_cluster(k5, data = df)

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)
grid.arrange(n1, n2, n3, n4, nrow = 2)

##Determinar los clústeres óptimos --------------------------------------------------------------
###1. Método del codo (Elbow method) --------------------------------------------------------------
set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#Otra forma más sencilla
set.seed(123)
fviz_nbclust(df, kmeans, method = "wss")

###2. Método de la silueta (Silhouette method) --------------------------------------------------------------

# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(df, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

#Otra forma más sencilla
fviz_nbclust(df, kmeans, method = "silhouette")

###3. Método Estadístico de Gap (Gap statistic method) --------------------------------------------------------------

# compute gap statistic
set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")

#Otra forma más sencilla
fviz_gap_stat(gap_stat)

#Resultados finales
# Compute k-means clustering with k = 4
set.seed(123)
final <- kmeans(df, 4, nstart = 25)
print(final)

#Visualización de los resultados finales
fviz_cluster(final, data = df)

#Y podemos extraer los conglomerados y añadirlos a nuestros datos iniciales para
#hacer algunas estadísticas descriptivas a nivel de conglomerado:

USArrests %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

#Otros datos para realizar aplicaciones --------------------------------------------------------------
# Otros datos disponibles en la libreria datasets y ggplot2
# para hacer analisis de cluster

data(iris)
data.table(iris)
data(diamonds)
data.table(diamonds)