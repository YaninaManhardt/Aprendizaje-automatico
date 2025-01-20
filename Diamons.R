
library(ggplot2)
data(diamonds)

summary(diamonds)


diamond_df <- data.frame(diamonds)
diamond_data <- diamonds[,-c(2,3,4,7)]
cut <- diamonds[,2]

install.packages("dplyr")
library(dplyr)

diamond_di=data.frame(diamond_df) %>%
  mutate(cut=dplyr::recode(cut,
                           Ideal="Ideal",
                           Premium="Premium",
                           VeryGood="Muy Buena",
                           Good="Buena",
                           Fair="Justa"))

options(repr.plot.width=20, repr.plot.height=15)

# Crear un gráfico de pares con la mitad superior
install.packages("GGally")
library(GGally)

ggpairs(diamond_df, lower = list(continuous = "points"), aes(color = cut)) +
  theme(axis.text = element_text(size = 12),  # Tamaño del texto en los ejes
        axis.title = element_text(size = 14))  # Tamaño de los títulos de los ejes

# Crear una tabla con estadísticas descriptivas
summary_table <- diamond_df %>%
  group_by(cut) %>%
  summarise(
    Media_Longitud_Carat = mean(carat),
    Media_Anchura_Depth = mean(depth),
    SD_Longitud_Carat = sd(carat),
    SD_Anchura_Depth = sd(depth),
    Media_Longitud_Table = mean(table),
    Media_Anchura_Table = mean(x),
    SD_Longitud_Table = sd(table),
    SD_Anchura_Table = sd(x),
    Media_Longitud_Table = mean(y),
    Media_Anchura_Table = mean(z),
    SD_Longitud_Table = sd(y),
    SD_Anchura_Table = sd(z)
  )

# Mostrar la tabla con estadísticas descriptivas
print(summary_table)

#VOLVEMOS AL TAMAÑO HABITUAL

options(repr.plot.width=10, repr.plot.height=6)  # Ajusta el tamaño según tus preferencias

# Definir una paleta de colores personalizada
custom_colors <- c("darkblue", "darkgreen", "pink")

#Escalar los datos


diamonds_data_scale<-scale(diamond_data)
head(diamond_data)
head(diamonds_data_scale)

# Realizamos el análisis de componentes principales
pca_result <- prcomp(diamonds_data_scale, center = TRUE, scale. = TRUE)

# Resumen de los resultados
summary(pca_result)

#Resultados: Criterio de ordenamiento de cada componente
pca_result

# Obtén las desviaciones estándar de cada componente principal
std_devs <- sqrt(pca_result$sdev^2)

# Calcula la proporción acumulada de varianza explicada
cumulative_var <- cumsum(pca_result$sdev^2) / sum(pca_result$sdev^2)

# Crea un DataFrame con los nombres de PC, desviaciones estándar y proporción acumulada
pca_data <- data.frame(PC = paste0("PC", 4:6), Std_Dev = std_devs, Cumulative_Variance = cumulative_var)

# Crear el gráfico
if (!require("ggtext")) {
  install.packages("ggtext")
}

library(ggplot2)
library(ggtext)


diamonds_pca <- ggplot(pca_data, aes(x = PC, group = 1)) +
  geom_line(aes(y = Std_Dev, color = "Desviación Estándar"), size = 1) +
  geom_line(aes(y = Cumulative_Variance, color = "Proporción Acumulada"), size = 1, linetype = "dashed") +
  geom_point(aes(y = Std_Dev, color = "Desviación Estándar"), size = 3) +
  geom_point(aes(y = Cumulative_Variance, color = "Proporción Acumulada"), size = 3) +
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~ . * 100,
                        breaks = seq(0, 2, 0.2) * 100,
                        labels = scales::percent(seq(0, 2, 0.2)),name = "Proporción Acumulada"
    ),
    breaks = seq(0, 2, 0.2),
    labels = seq(0, 2, 0.2)
  ) +
  scale_color_manual(values = c("Desviación Estándar" = "skyblue", "Proporción Acumulada" = "red")) +
  labs(x = "Componente Principal", y = "Desviación Estándar") +
  labs(color = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14)
  )


# Imprimir el gráfico
print(diamonds_pca)


options(repr.plot.width=15, repr.plot.height=8)  # Ajusta el tamaño según tus preferencias

biplot(pca_result)


install_github('vqv/ggbiplot')
library(ggbiplot)


#Personalizado con ggbiplot
ggbiplot(pca_result) +
  theme_minimal() +  # Establecer un tema
  scale_color_discrete(name = 'Caracteristicas') +
  geom_point(aes(color = diamonds$carat), size = 3) +  # Personalizar los puntos con colores
  guides(col = guide_legend(override.aes = list(size = 5))) +  # Ajustar el tamaño de los puntos en la leyenda
  theme_minimal() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14)
  )

----------------------------------
  # Obtener los loadings para los componentes principales (PCA)
  loadings <- pca_result$rotation


# Crear un DataFrame con los loadings (2D)
loadings_data <- data.frame(
  Variable = colnames(loadings),
  PC1 = loadings[, 1],
  PC2 = loadings[, 2]
)

# Crear un DataFrame con los loadings (2D)
loadings_data3d <- data.frame(
  Variable = colnames(loadings),
  PC1 = loadings[, 1],
  PC2 = loadings[, 2],
  PC3 = loadings[, 3]
)

loadings_data
loadings_data3d



biplot <- ggplot(loadings_data, aes(x = PC1, y = PC2, color = Variable)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_text(data = subset(loadings_data, !is.na(PC1) & !is.na(PC2)), aes(label = Variable), hjust = 1, vjust = 1, size = 6) +
  geom_segment(data = subset(loadings_data, !is.na(PC1) & !is.na(PC2)), aes(xend = 0, yend = 0, color = Variable), size = 2) +
  #geom_segment(data = subset(loadings_data, !is.na(PC1) & !is.na(PC2)), aes(xend = 0.1 * sign(PC1), yend = 0.1 * sign(PC2), color = Variable), arrow = arrow(type = "closed", length = unit(0.1, "inches")), size = 1) +
  labs(x = "Componente Principal 1", y = "Componente Principal 2") +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_continuous(limits = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text = element_text(size = 16),  # Ajusta el tamaño del texto de los ejes
        axis.title = element_text(size = 18),  # Ajusta el tamaño de los títulos de los ejes
        legend.text = element_text(size = 14)
  )

print(biplot)

#Cluster Jerárquico
#Distancia Euclidiana

set.seed(123)

#Se calculan las distancias
dist_matrix <- dist(diamonds[,6:7], method = "euclidean")

#Luego, aplica el algoritmo de clustering jerárquico utilizando la función hclust:
clustering_h <- hclust(dist_matrix, method = "ward.D2")
help(hclus)
#GRÁFICO BÁSICO
plot(clustering_h)

-------------------------------------------------------
#Acortando los datos
#dendograma denso

# Trabajar con un subconjunto más pequeño de los datos
subset_data <- diamonds[sample(nrow(diamonds), 1000), c(6, 7)]

# Calcular distancias en el subconjunto
dist_matrix <- dist(subset_data, method = "euclidean")

# Aplicar el algoritmo de clustering jerárquico al subconjunto
clustering_h <- hclust(dist_matrix, method = "ward.D2")

# Graficar el dendrograma
plot(clustering_h)


------------------------------------------------------------------------
install.packages("data.table")
library(data.table)

data("diamonds")
diamond_data <- diamonds

# Convertir un data frame a data.table
setDT(diamond_data)

# Ejemplo de manipulación
diamond_data[, .(Mean_Price = mean(price), Max_Carat = max(carat)), by = cut]


#GRÁFICO PERSONALIZADO:

set.seed(123)
sample_indices <- sample(1:nrow(diamonds), 60)

# Crear una paleta de colores
colores <- rainbow(10)

# Calcular la matriz de distancias
dist_matrix <- dist(diamonds[sample_indices, 6:7], method = "euclidean")

# Realizar el clustering jerárquico
clustering_h <- hclust(dist_matrix, method = "ward.D2")

# Obtener los clusters usando cutree
cut_tree <- cutree(clustering_h, k = 10)

# Asignar colores a los clusters
colores_asignados <- colores[cut_tree]

# Crear una paleta de colores para los clusters
paleta_clusters <- colorRampPalette(colores)

# Graficar el dendrograma con colores
plot(clustering_h, main = "Dendrograma Jerárquico", col.dendrogram = paleta_clusters(20), hang = -1)

# Agregar etiquetas a los clusters
rect.hclust(clustering_h, k = 30, border = colores_asignados)
-------------------------------------
#Datos escalados

install.packages("gridExtra")
library(gridExtra)

set.seed(123)

diamond_hc_2<-
  fviz_dend(x = hc_euclidean_complete, k = 2, cex = 0.6) +
  geom_hline(yintercept = 6, linetype = "dashed") +
  labs(title = "Clustering Jerárquico",
       subtitle = "Distancia Euclidiana, Enlace Completo, K=2") +
  theme(axis.text = element_text(size = 16),  # Aumenta el tamaño de letra de los ejes
        axis.title = element_text(size = 18),  # Aumenta el tamaño de letra de los títulos
        legend.text = element_text(size = 16))  # Aumenta el tamaño de letra de la leyenda



diamond_hc_5<-
  fviz_dend(x = hc_euclidean_complete, k = 5, cex = 0.6) +
  geom_hline(yintercept = 1.8, linetype = "dashed") +
  labs(title = "Clustering Jerárquico",
       subtitle = "Distancia Euclidiana, Enlace Completo, K=5") +
  theme(axis.text = element_text(size = 16),  # Aumenta el tamaño de letra de los ejes
        axis.title = element_text(size = 18),  # Aumenta el tamaño de letra de los títulos
        legend.text = element_text(size = 16))  # Aumenta el tamaño de letra de la leyenda

#Organizar los gráficos en una fila
options(repr.plot.width=20, repr.plot.height=6)  # Ajusta el tamaño según tus preferencias
grid.arrange(diamond_hc_2, diamond_hc_5, ncol = 2)

options(repr.plot.width=22, repr.plot.height=10)  # Ajusta el tamaño según tus preferencias

----------------------
install.packages("gridExtra")
library(gridExtra)

set.seed(123)
# Cálculo de las distancias
dist_matrix <- dist(diamonds[, 4:6], method = "euclidean")

# Aplicar el algoritmo de clustering jerárquico utilizando la función hclust
clustering_h <- hclust(dist_matrix, method = "ward.D2")

# Trazar el dendrograma
dend <- as.dendrogram(clustering_h)
dend <- dend %>%
  set("branches_k_color", k = 3)  # Definir el número de grupos

# Graficar el dendrograma
plot(dend, main = "Dendrograma del Clustering Jerárquico", labels=rownames(diamonds$cut),cex=0.7)
rect.hclust(clustering_h, k = 3, border = "blue")

# Definir el valor de cut_height
cut_height <- 10

# Agregar una línea horizontal en el punto de corte
abline(h = cut_height, col = "red", lty = 5)

-------------------------------------------------
#K-MEANS

#install.packages("cluster")
library(cluster)

# install.packages("pander")
library(pander)

set.seed(123)

#Seleccionar las columnas relevantes para el clustering (en este caso, sin la etiqueta "Species")
diamond_data <- diamonds[, -c(2, 3, 4, 7)]

# Especificar el número de clusters (K)
K <- 3

# Paso 3: Ejecutar el algoritmo K-Means
kmeans_result <- kmeans(diamond_data, centers = K, nstart = 25)
#help(kmeans)

# Calcular la inercia intra y extra grupo
inertia_intra_group <- kmeans_result$tot.withinss
inertia_inter_group <- kmeans_result$totss - inertia_intra_group

cat("Resultados del Clustering K-Means:\n")
cat("Número de Clusters (K):", K, "\n")
cat("Inercia Intra-Grupo:", inertia_intra_group, "\n")
cat("Inercia Inter-Grupo:", inertia_inter_group, "\n")

#Mostrar los resultados
cat("\nCentroides de Clusters:\n")
print(data.frame(Cluster = 1:K, kmeans_result$centers))

# Tabla con recuento de observaciones en cada cluster
cluster_counts <- table(kmeans_result$cluster)
cat("\nRecuento de Observaciones en Cada Cluster:\n")
#print(cluster_counts)

# Crear una tabla formateada
cluster_table <- as.data.frame(cluster_counts)
colnames(cluster_table) <- c("Cluster", "Observaciones")

# Imprimir la tabla
print(pander(cluster_table, caption = "Recuento de Observaciones en Cada Cluster"))#install.packages("cluster")

library(ggplot2)
set.seed(123)
#GRÁFICO 2D
palette <- scales::brewer_pal(palette="Dark2")(K) #Podrían ser otras paletas: Dark2, Set1, Set2, Set3 Paired


# Crear un gráfico de dispersión 2D con colores por cluster y tamaños de texto personalizados
ggplot(diamonds, aes(carat, price , color = as.factor(kmeans_result$cluster))) +
  geom_point(size = 3) +
  labs(x = "Quilates", y = "Precio") +
  scale_color_manual(values = palette, name = "Cluster") +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14))

install.packages("gridExtra")
library(gridExtra)
set.seed(123)

#GRÁFICOS 2D para cada K

#K=2---------------------------------------------------------------------------
kmeans_result_k2 <- kmeans(diamond_data, centers = 2, nstart = 25)
palette_k2 <- scales::hue_pal()(2)

plot_k2<-ggplot(diamonds, aes(carat, price, color = factor(kmeans_result_k2$cluster))) +
  geom_point(size = 3) +
  labs(x = "Quilates", y = "Precio") +
  scale_color_manual(values = palette_k2, name="Cluster") +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14)) +
  ggtitle("K-Means Clustering (K = 2)")

#K=3--------------------------------------------------------------------------
kmeans_result_k3 <- kmeans(diamond_data, centers = 3, nstart = 25)
palette_k3 <- scales::hue_pal()(3)

plot_k3<-ggplot(diamonds, aes(carat, price, color = factor(kmeans_result_k3$cluster))) +
  geom_point(size = 3) +
  labs(x = "Quilates", y = "Precio") +
  scale_color_manual(values = palette_k3,name="Cluster") +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14)) +
  ggtitle("K-Means Clustering (K = 3)")

#K=4--------------------------------------------------------------------------
kmeans_result_k4 <- kmeans(diamond_data, centers = 4, nstart = 25)
palette_k4 <- scales::hue_pal()(4)

plot_k4<-ggplot(diamonds, aes(carat, price, color = factor(kmeans_result_k4$cluster))) +
  geom_point(size = 3) +
  labs(x = "Quilates", y = "Precio") +
  scale_color_manual(values = palette_k4,name="Cluster") +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14)) +
  ggtitle("K-Means Clustering (K = 4)")

print(plot_k4)

# Organizar los gráficos en una fila
options(repr.plot.width=20, repr.plot.height=6)  # Ajusta el tamaño según tus preferencias
grid.arrange(plot_k2, plot_k3, plot_k4, ncol = 3)
--------------------

  #install.packages("cluster")
  library(cluster)

# install.packages("pander")
library(pander)

set.seed(123)

#Seleccionar las columnas relevantes para el clustering (en este caso, sin la etiqueta "Species")
diamond_data <- diamonds[, -c(2, 3, 4, 7)]

# Especificar el número de clusters (K)
K <- 4

# Paso 3: Ejecutar el algoritmo K-Means
kmeans_result <- kmeans(diamond_data, centers = K, nstart = 25)
#help(kmeans)

# Calcular la inercia intra y extra grupo
inertia_intra_group <- kmeans_result$tot.withinss
inertia_inter_group <- kmeans_result$totss - inertia_intra_group

cat("Resultados del Clustering K-Means:\n")
cat("Número de Clusters (K):", K, "\n")
cat("Inercia Intra-Grupo:", inertia_intra_group, "\n")
cat("Inercia Inter-Grupo:", inertia_inter_group, "\n")

#Mostrar los resultados
cat("\nCentroides de Clusters:\n")
print(data.frame(Cluster = 1:K, kmeans_result$centers))

# Tabla con recuento de observaciones en cada cluster
cluster_counts <- table(kmeans_result$cluster)
cat("\nRecuento de Observaciones en Cada Cluster:\n")
#print(cluster_counts)

# Crear una tabla formateada
cluster_table <- as.data.frame(cluster_counts)
colnames(cluster_table) <- c("Cluster", "Observaciones")

# Imprimir la tabla
print(pander(cluster_table, caption = "Recuento de Observaciones en Cada Cluster"))#install.packages("cluster")
----------------------------------------------------
set.seed(123)
# Crear un dataframe con las etiquetas abreviadas
etiquetas_df <- data.frame(cut = unique(diamonds$cut), Etiqueta = c("gd", "vg", "pre", "ide", "fai"))

# Unir el dataframe de etiquetas con el dataframe iris
diamonds_etiquetas <- merge(diamonds, etiquetas_df, by.x = "cut", by.y = "cut")
diamonds_etiquetas

palette_k2 <- scales::hue_pal()(2)
palette_k3 <- scales::hue_pal()(3)
palette_k4 <- scales::hue_pal()(4)

# K = 2
plot_k2<-ggplot(diamonds_etiquetas, aes(carat, price, color = factor(kmeans_result_k2$cluster))) +
  geom_label(aes(label = Etiqueta), size = 4) +
  labs(x = "Quilates", y = "Precio") +
  scale_color_manual(values = palette_k2, name = "Cluster") +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14)) +
  ggtitle("K-Means Clustering (K = 2)")

# K = 3
plot_k3<-ggplot(diamonds_etiquetas, aes(carat, price, color = factor(kmeans_result_k3$cluster))) +
  geom_label(aes(label = Etiqueta), size = 4) +
  labs(x = "Quilates", y = "Precio") +
  scale_color_manual(values = palette_k3, name = "Cluster") +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14)) +
  ggtitle("K-Means Clustering (K = 3)")

# K = 4
plot_k4<-ggplot(diamonds_etiquetas, aes(carat, price, color = factor(kmeans_result_k4$cluster))) +
  geom_label(aes(label = Etiqueta), size = 4) +
  labs(x = "Quilates", y = "Precio") +
  scale_color_manual(values = palette_k4, name = "Cluster") +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14)) +
  ggtitle("K-Means Clustering (K = 4)")

# Organizar los gráficos en una fila
options(repr.plot.width=20, repr.plot.height=6)  # Ajusta el tamaño según tus preferencias
grid.arrange(plot_k2, plot_k3, plot_k4, ncol = 3)
