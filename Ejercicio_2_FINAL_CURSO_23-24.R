# Importamos la librería
library(faraway)
# Cargamos los datos
data(seatpos)

# Selección de las variables de interés
hipcenter <- seatpos$hipcenter; hipcenter
Age <- seatpos$Age; age
Weight <- seatpos$Weight; weight
Ht <- seatpos$Ht; Ht
HtShoes <- seatpos$HtShoes; HtShoes
Seated <- seatpos$Seated; Seated
Arm <- seatpos$Arm; Arm
Thigh <- seatpos$Thigh; Thigh
Leg <- seatpos$Leg; Leg

# Quitar los valores negativos de hipcenter calculando el valor absoluto
seatpos$hipcenter <- abs(seatpos$hipcenter)
hipcenter <- seatpos$hipcenter; hipcenter

# Seleccionamos las variables numéricas
datos <- seatpos[, c("hipcenter", "Age", "Weight", "Ht", "HtShoes", "Seated", "Arm", "Thigh", "Leg")]; datos

# Realizar el Pca
pca <- prcomp(seatpos, scale. = TRUE); pca

# Resumen del pca
summary(pca)

# Generamos el biplot
biplot(pca, scale = 0)

# Ajustamos el modelo de regresión lineal
modelo <- lm(hipcenter ~ Age + Weight + Ht + HtShoes + Seated + Arm + Thigh + Leg, data = datos)

# Resumen del modelo
summary(modelo)