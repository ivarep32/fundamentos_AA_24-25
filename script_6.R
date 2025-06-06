# =============================================
# 1. APALANCAMIENTOS EN REGRESIÓN SIMPLE
# =============================================

# ---------------------------------------------
# 1.1. Generación de datos y modelo base
# ---------------------------------------------

# Fijamos semilla para reproducibilidad
set.seed(123456)

# Generamos 20 observaciones
n <- 20
x <- 1:n  # Valores de 1 a 20

# Modelo verdadero: Y = 1 + 0.5x + ε, con ε ~ N(0,1)
y <- 1 + 0.5 * x + rnorm(n)

# Ajustamos el modelo lineal
mod <- lm(y ~ x)

# Visualización básica
plot(y ~ x, xlab = "Variable X", ylab = "Variable Y",
     main = "Regresión lineal simple", asp = 1)
abline(mod, col = "blue")

# Resumen del modelo
summary(mod)

# ---------------------------------------------
# 1.2. Análisis de apalancamientos
# ---------------------------------------------

# TEORÍA:
# El apalancamiento h_ii mide cuánto influye cada observación en su propia predicción
# Fórmula: h_ii = 1/n + (x_i - x̄)^2 / (n*S_x^2)
# Valores altos indican mayor capacidad de influencia
# Criterio: h_ii > 4/n es preocupante

# Calculamos los apalancamientos
leverages <- hatvalues(mod)
print(round(leverages, 3))

# Umbral de preocupación
umbral <- 4/n
print(paste("Umbral de apalancamiento:", round(umbral, 3)))

# Identificamos observaciones con alto apalancamiento
which(leverages > umbral)
# ninguna observacion supera el umbral por tanto
# no hay ningun punto con una influencia potencial
#excesiva

# ---------------------------------------------
# 1.3. Añadir punto influyente y comparar
# ---------------------------------------------

# Añadimos un punto extremo (x=30, y desplazado)
x_ampliado <- c(x, 30)
y_ampliado <- c(y, 1 + 0.5*30 - 8)  # Desplazamos 8 unidades hacia abajo
round(hat(model.matrix(mod_b2)),3)

# Ajustamos nuevo modelo
mod_b2 <- lm(y_ampliado ~ x_ampliado)

# Calculamos apalancamientos del nuevo modelo
leverages_b2 <- hatvalues(mod_b2)
print(round(leverages_b2, 3))

# Nuevo umbral (ahora n=21)
umbral_b2 <- 4/length(x_ampliado)
print(paste("Nuevo umbral:", round(umbral_b2, 3)))

# Observación problemática
which(leverages_b2 > umbral_b2)  # La observación 21
# vemos que el nuevo punto si tiene mucha influencia

# =============================================
# 2. DIAGNOSIS DE RESIDUOS
# =============================================

# ---------------------------------------------
# 2.1. Residuos estandarizados
# ---------------------------------------------

# TEORÍA:
# Residuos estandarizados: r_i = e_i / (σ̂√(1-h_ii))
# Corrigen la heterocedasticidad inducida por los apalancamientos
# Criterio: |r_i| > 2 sugiere observación atípica
residuals(mod_b2)
rstandard(mod_b2)
which(abs(rstandard(mod_b2)) > 2) #vemos que se declara la observacion añadida como atipica

# ---------------------------------------------
# 2.2. Residuos estudentizados
# ---------------------------------------------

# TEORÍA:
# Residuos estudentizados: t_i = e_i / (σ̂_(i)√(1-h_ii))
# Donde σ̂_(i) se calcula excluyendo la i-ésima observación
# Son más sensibles para detectar valores atípicos
rstudent(mod_b2)
which(abs(rstudent(mod_b2))>2) #mismo resultado

# ---------------------------------------------
# 2.3. Test de normalidad
# ---------------------------------------------

# TEORÍA:
# El test de Shapiro-Wilk contrasta la normalidad de los residuos
# p-valor < 0.05 sugiere no normalidad

shapiro.test(rstandard(mod_b2))

# Interpretación:
# p-value = 0.01425 < 0.05 ⇒ Rechazamos normalidad
# Esto puede deberse a la presencia de valores atípicos

# ---------------------------------------------
# 2.4. Distancia de Cook
# ---------------------------------------------

# TEORÍA:
# Distancia de Cook mide el cambio en las predicciones al eliminar cada punto
# Fórmula: D_i = (1/p) * r_i^2 * (h_ii/(1-h_ii))
# Criterio: D_i > 0.5 o D_i > 1 indica influencia preocupante
cooks.distance(mod_b2)
# Identificamos puntos influyentes
which(cooks.distance(mod_b2) > 0.5)  # Solo la observación 21

# =============================================
# 3. GRÁFICOS DE DIAGNOSIS
# =============================================

# TEORÍA:
# R produce 4 gráficos de diagnóstico automáticos:
# 1. Residuos vs Ajustados: Para detectar no linealidad, heterocedasticidad
# 2. QQ-plot: Para evaluar normalidad de residuos
# 3. Scale-Location: Para detectar heterocedasticidad
# 4. Residuos vs Apalancamiento: Para identificar puntos influyentes
par(mfrow = c(2, 2))
plot(mod_b2)

# =============================================
# 4. PAUTAS DE ACTUACIÓN
# =============================================

# TEORÍA:
# Ante observaciones atípicas/influyentes:
# 1. Verificar si son errores de medición/digitación
# 2. Considerar transformaciones de variables
# 3. Evaluar modelos alternativos (robustos)
# 4. Si se justifica, eliminar las observaciones problemáticas

# En nuestro caso, la observación 21 es claramente influyente:
# - Alto apalancamiento (0.383)
# - Residuo estudentizado extremo (-5.97)
# - Distancia de Cook muy alta (3.92)

# Opción 1: Eliminar la observación influyente
mod_clean <- lm(y ~ x)  # Volvemos al modelo original

# Opción 2: Usar regresión robusta (se reduce la influencia pero sigue estando)
library(MASS)
mod_robusto <- rlm(y_ampliado ~ x_ampliado)

# Comparación de coeficientes
coef(mod_b2)  # Con el punto influyente
coef(mod_clean)  # Sin el punto
coef(mod_robusto)  # Modelo robusto

# CONCLUSIÓN:
# El punto añadido en x=30 afecta significativamente el modelo
# La solución más simple en este caso es eliminarlo
# En casos reales, habría que investigar la causa de la anomalía