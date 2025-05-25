# Cargamos las librerías necesarias para análisis y visualización
library(MASS)        # Contiene datasets y funciones estadísticas
library(mgcv)        # Para modelos GAM (no se usa aquí pero puede ser útil)
library(GGally)      # Para pares gráficos mejorados
library(ggdendro)    # Para graficar dendrogramas con ggplot2
library(gridExtra)   # Para organizar múltiples gráficos

# Cargamos el dataset iris, clásico en clasificación y clustering
data(iris)

# Resumen estadístico de las primeras 4 variables numéricas (medidas florales)
summary(iris[,1:4])

#----------------------------------------------------------
# 1) Clustering K-medias (K-means) con k=3 clusters (sabemos que iris tiene 3 especies)
#----------------------------------------------------------
res.km3 = kmeans(iris[,1:4], centers=3)  # Ejecutamos K-means con 3 grupos

# Tabla de contingencia: clusters encontrados vs especies reales
table(res.km3$cluster, iris$Species)

# Visualización de pares de variables coloreadas por especie y forma según cluster K-means
ggpairs(iris, columns=1:4, aes(colour=Species, shape=factor(res.km3$cluster)))

# También realizamos clustering con 2 y 4 clusters para comparar
res.km2 = kmeans(iris[,1:4], centers=2)
res.km4 = kmeans(iris[,1:4], centers=4)

# Calculamos matriz de distancias euclidianas entre observaciones (para índices de calidad)
D = dist(iris[,1:4])

# Calculamos métricas de calidad de clustering usando la librería fpc
aa2 = fpc::cluster.stats(D, res.km2$cluster)
aa3 = fpc::cluster.stats(D, res.km3$cluster)
aa4 = fpc::cluster.stats(D, res.km4$cluster)

# Extraemos índices: Dunn, Dunn2, Calinski-Harabasz (CH), Silhouette Index (SI)
IDX = rbind(
  c(aa2$dunn, aa2$dunn2, aa2$ch, aa2$avg.silwidth),
  c(aa3$dunn, aa3$dunn2, aa3$ch, aa3$avg.silwidth),
  c(aa4$dunn, aa4$dunn2, aa4$ch, aa4$avg.silwidth)
)
colnames(IDX) = c("Dunn", "Dunn2", "CH", "SI")
rownames(IDX) = c("K=2", "K=3", "K=4")

# Mostramos los índices para comparar la calidad de los diferentes K
IDX

#----------------------------------------------------------
# 2) Clustering Jerárquico usando método de enlace completo
#----------------------------------------------------------
res.hc = hclust(D, method = "complete")  # Clustering jerárquico aglomerativo

# Cortamos el dendrograma en 2, 3 y 4 clusters para comparar
hc2 = cutree(res.hc, 2)
hc3 = cutree(res.hc, 3)
hc4 = cutree(res.hc, 4)

# Calculamos métricas de calidad para estos clusters jerárquicos
aa2 = fpc::cluster.stats(D, hc2)
aa3 = fpc::cluster.stats(D, hc3)
aa4 = fpc::cluster.stats(D, hc4)

# Guardamos las métricas en una matriz para comparación
IDX = rbind(
  c(aa2$dunn, aa2$dunn2, aa2$ch, aa2$avg.silwidth),
  c(aa3$dunn, aa3$dunn2, aa3$ch, aa3$avg.silwidth),
  c(aa4$dunn, aa4$dunn2, aa4$ch, aa4$avg.silwidth)
)
colnames(IDX) = c("Dunn", "Dunn2", "CH", "SI")
rownames(IDX) = c("K=2", "K=3", "K=4")

# Mostrar resultados para jerárquico
IDX

# Visualización dendrograma con ggdendro y ggplot2
ddata = dendro_data(res.hc, type = "rectangle")  # Prepara datos para ggplot
ggplot(segment(ddata)) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
  coord_flip() + scale_y_reverse() +  # Ajustes visuales para mejor presentación
  labs(title = "Dendrograma - Clustering Jerárquico")

#----------------------------------------------------------
# 3) Método Mean-Shift (sin librería, con función propia)
#----------------------------------------------------------
X2d = iris[,1:4]  # Datos (4 dimensiones)

# Función para calcular el nuevo punto (media ponderada) en Mean-Shift
fg = function(x0, datos, h = 0.15) {
  dd = sweep(datos, 2, x0, "-")           # Diferencia vectorial de cada punto con x0
  dd = apply(dd, 1, function(u) sqrt(sum(u^2)))  # Distancias euclidianas
  wd = dnorm(dd / quantile(dd, probs = h, na.rm = TRUE))  # Pesos con kernel normal
  wd = wd / sum(wd)                        # Normalizamos pesos
  fx = apply(sweep(datos, 1, wd, "*"), 2, sum)  # Media ponderada (mean-shift)
  return(fx)
}

Xnew = as.matrix(X2d)  # Datos como matriz para manipulación

plot(Xnew[,1:2], type = "n", main = "Ejemplo Mean-Shift con Iris (2 primeras dimensiones)")
for (i in 1:nrow(Xnew)) {
  ffold = rep(0, ncol(X2d))
  ff = fg(as.vector(Xnew[i,]), datos = X2d, h = 0.15)
  arrows(Xnew[i,1], Xnew[i,2], ff[1], ff[2], length = 0.05, col = 2)  # Flecha de movimiento inicial
  while (sum((ffold - ff)^2) > 1e-5) {  # Criterio de convergencia para Mean-Shift
    ffold = ff
    ff = fg(as.vector(ffold), datos = X2d, h = 0.15)
    arrows(ffold[1], ffold[2], ff[1], ff[2], length = 0.05, col = 2)  # Flechas del recorrido
  }
  points(ff[1], ff[2], pch = 19, cex = 2, col = 4)  # Punto de convergencia
}
points(X2d[,1], X2d[,2], pch = 19)  # Puntos originales

#----------------------------------------------------------
# 4) Función DBSCAN (implementación personalizada)
#----------------------------------------------------------
DBSCAN = function(fobj, eps, minPts = 5, metric = dist, par.metric = list(method = "euclidean")) {
  # Preparamos los parámetros para la función de distancia
  aaf = formals(metric)[-1]
  if (length(par.metric) > 0) {
    nam = names(par.metric)
    for (i in 1:length(par.metric)) {
      aaf[[nam[i]]] = par.metric[[i]]
    }
  }
  # Calculamos la matriz de distancias
  D = as.matrix(do.call(metric, c(alist(fobj), aaf)))
  diag(D) = NA
  n = nrow(D)

  vis = rep(0, n)       # Vector para marcar visitados (no usado explícitamente)
  cl = 0                # Contador de clusters
  cluster = rep(0, n)   # Vector para guardar asignación de cluster por punto
  mark = rep(NA, n)     # Marca tipo de punto: núcleo (C), borde (A), ruido (N)

  if (is.null(eps)) eps = quantile(D, probs = 0.05, na.rm = TRUE)

  # Contamos vecinos para cada punto dentro de eps
  vecinos = apply(D, 1, function(x, e) { sum(x <= e, na.rm = TRUE) }, e = eps)

  # Marcamos puntos según vecinos y minPts
  mark[vecinos < 1] = "N"                 # Ruido (sin vecinos)
  mark[vecinos >= minPts] = "C"           # Núcleo
  mark[vecinos < minPts & vecinos > 0] = "A"  # Borde

  # Buscamos puntos núcleo sin asignar cluster y los procesamos
  nl = sum(mark == "C" & cluster == 0, na.rm = TRUE)
  if (nl == 0) stop("No center points. Consider modify minPts or eps")

  while (nl > 0) {
    lori = which(mark == "C" & cluster == 0)[1]  # Primer núcleo sin cluster
    cl = cl + 1
    cluster[lori] = cl

    nc0 = sum(cluster == 0)
    if (nc0 > 0) {
      mDgr = min(apply(D[which(cluster == cl & mark == "C"), which(cluster == 0), drop = FALSE], 2, min, na.rm = TRUE))
    } else {
      mDgr = 2 * eps
      break
    }

    while (mDgr < eps) {
      nc1 = sum(cluster == cl)
      if (nc1 == 1) {
        lvec = which(D[which(cluster == cl & mark == "C"), ] < eps)
      } else {
        lvec = which(apply(D[which(cluster == cl & mark == "C"), , drop = FALSE], 2, min, na.rm = TRUE) < eps)
      }
      cluster[lvec] = cl

      nc0 = sum(cluster == 0)
      if (nc0 > 0) {
        mDgr = min(apply(D[which(cluster == cl & mark == "C"), which(cluster == 0), drop = FALSE], 2, min, na.rm = TRUE))
      } else {
        mDgr = 2 * eps
        break
      }
    }
    nl = sum(mark == "C" & cluster == 0)
  }

  # Marcamos puntos borde como ruido o borde según vecinos núcleo
  lA = which(mark == "A")
  for (i in lA) {
    if (sum(D[i, ] <= eps & mark == "C") > 0) mark[i] = "A" else mark[i] = "N"
  }

  # Devolvemos resultado: cluster asignado y marca de punto
  return(list(cluster = cluster, mark = mark))
}

#----------------------------------------------------------
# 5) Uso de DBSCAN con los datos iris y visualización
#----------------------------------------------------------
eps = quantile(D, prob = 0.09)  # Elegimos epsilon como un cuantil de la distribución de distancias

res.dbscan = DBSCAN(X2d, eps = eps)

# Convertimos clusters y marcas a factores para facilitar visualización
fac = factor(res.dbscan$cluster,
             levels = c(0, 1:max(res.dbscan$cluster)),
             labels = c("Ruido", paste0("Gr.:", 1:max(res.dbscan$cluster))))
marca = factor(res.dbscan$mark,
               levels = c("C", "A", "N"),
               labels = c("Núcleo", "Borde", "Ruido"))

# Tabla comparativa: especies reales vs clusters DBSCAN
table(iris$Species, fac)

# Gráfico con ggplot2, mostrando clusters y tipo de punto (núcleo, borde, ruido)
library(ggplot2)
ggplot(data = iris) +
  geom_point(mapping = aes(x = Sepal.Length, y = Sepal.Width, col = fac, shape = marca), size = 2) +
  labs(x = "Longitud Sépalo", y = "Anchura Sépalo", color = "Cluster DBSCAN", shape = "Tipo Punto") +
  ggtitle("Clustering DBSCAN en Iris: Sepal Length vs Sepal Width")
