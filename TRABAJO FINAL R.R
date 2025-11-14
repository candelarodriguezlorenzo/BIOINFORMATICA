# NombreApellido_Trabajo2.R
#candela rodriguez lorenzo
# Trabajo final Bioinformática - Curso 25/26
# Análisis de parámetros biomédicos por tratamiento

# 1. Cargar librerías (si necesarias) y datos del archivo "datos_biomed.csv". (0.5 pts)
datos <- read.csv("/Users/candelarodriguezlorenzo/Desktop/datos_biomed.csv", stringsAsFactors = FALSE)

# 2. Exploración inicial con las funciones head(), summary(), dim() y str(). ¿Cuántas variables hay? ¿Cuántos tratamientos? (0.5 pts)
# Mostrar las primeras filas
head(datos)
# Resumen estadístico general
summary(datos)
# Dimensiones del data frame (número de filas y columnas)
dim(datos)
# Estructura del conjunto de datos
str(datos)
# Contar cuántas variables hay
num_variables <- ncol(datos)
cat("Número de variables:", num_variables, "\n")
# Contar cuántos tratamientos distintos hay
num_tratamientos <- length(unique(datos$Tratamiento))
cat("Número de tratamientos:", num_tratamientos, "\n")

# 3. Una gráfica que incluya todos los boxplots por tratamiento. (1 pt)
valores <- c(datos$Glucosa, datos$Presion, datos$Colesterol)
tratamientos <- rep(datos$Tratamiento, 3)
variables <- rep(c("Glucosa", "Presion", "Colesterol"), each = nrow(datos))
# Crear etiquetas combinadas para el eje X
etiquetas <- paste(tratamientos, variables, sep = "-")
# Crear el boxplot
boxplot(valores ~ etiquetas,
        main = "Boxplots por Tratamiento y Variable",
        las = 2, col = c("lightblue", "lightgreen", "lightpink"))

# 4. Realiza un violin plot (investiga qué es). (1 pt)
# Función para dibujar un violín
violin_plot <- function(x, at, ancho = 0.4, col = "lightblue") {
  dens <- density(x)  # calcular densidad
  dens$y <- dens$y / max(dens$y) * ancho
  polygon(c(at - dens$y, at + rev(dens$y)),
          c(dens$x, rev(dens$x)),
          col = col, border = "black")
  # Añadir mediana
  med <- median(x)
  segments(at - ancho, med, at + ancho, med, col = "red", lwd = 2)
}
# Preparar gráfico vacío
tratamientos <- unique(datos$Tratamiento)
plot(1:length(tratamientos), type = "n",
     xlim = c(0.5, length(tratamientos) + 0.5),
     ylim = range(datos$Glucosa),
     xaxt = "n", xlab = "Tratamiento", ylab = "Glucosa",
     main = "Violin Plot (Glucosa por Tratamiento)")

axis(1, at = 1:length(tratamientos), labels = tratamientos)
# Dibujar violines
colores <- c("lightblue", "lightgreen", "lightpink")
for (i in 1:length(tratamientos)) {
  grupo <- datos$Glucosa[datos$Tratamiento == tratamientos[i]]
  violin_plot(grupo, at = i, col = colores[i])
}

# 5. Realiza un gráfico de dispersión "Glucosa vs Presión". Emplea legend() para incluir una leyenda en la parte inferior derecha. (1 pt)
# Asignar colores según el tratamiento
colores <- c("red", "blue", "green")
nombres_trat <- unique(datos$Tratamiento)
# Crear un vector de colores
col_puntos <- colores[as.numeric(factor(datos$Tratamiento))]
# Crear el gráfico de dispersión
plot(datos$Glucosa, datos$Presion,
     col = col_puntos,
     pch = 19,
     main = "Relación entre Glucosa y Presión",
     xlab = "Glucosa",
     ylab = "Presión")
# Agregar leyenda
legend("bottomright",
       legend = nombres_trat,
       col = colores,
       pch = 19,
       title = "Tratamiento")

# 6. Realiza un facet Grid (investiga qué es): Colesterol vs Presión por tratamiento. (1 pt)
tratamientos <- unique(datos$Tratamiento)
# Configurar la ventana gráfica con una fila por cada tratamiento
par(mfrow = c(1, length(tratamientos)))
# Crear un gráfico por tratamiento
for (t in tratamientos) {
  subset_datos <- datos[datos$Tratamiento == t, ]
  
  plot(subset_datos$Presion, subset_datos$Colesterol,
       main = paste("Tratamiento:", t),
       xlab = "Presión",
       ylab = "Colesterol",
       col = "darkorange",
       pch = 19)
}
par(mfrow = c(1, 1))

# 7. Realiza un histogramas para cada variable. (0.5 pts)
# Configurar ventana gráfica: 1 fila y 3 columnas
par(mfrow = c(1, 3))
# Histograma de Glucosa
hist(datos$Glucosa,
     main = "Histograma de Glucosa",
     xlab = "Glucosa",
     col = "lightblue",
     border = "white")
# Histograma de Presión
hist(datos$Presion,
     main = "Histograma de Presión",
     xlab = "Presión",
     col = "lightgreen",
     border = "white")
# Histograma de Colesterol
hist(datos$Colesterol,
     main = "Histograma de Colesterol",
     xlab = "Colesterol",
     col = "lightpink",
     border = "white")

# 8. Crea un factor a partir del tratamiento. Investifa factor(). (1 pt)
datos$Tratamiento <- factor(datos$Tratamiento)
str(datos)
levels(datos$Tratamiento)

# 9. Obtén la media y desviación estándar de los niveles de glucosa por tratamiento. Emplea aggregate() o apply(). (0.5 pts)
# Calcular la media de glucosa por tratamiento
media_glucosa <- aggregate(Glucosa ~ Tratamiento, data = datos, FUN = mean)
# Calcular la desviación estándar de glucosa por tratamiento
sd_glucosa <- aggregate(Glucosa ~ Tratamiento, data = datos, FUN = sd)
print(media_glucosa)
print(sd_glucosa)

# 10. Extrae los datos para cada tratamiento y almacenalos en una variable. Ejemplo todos los datos de Placebo en una variable llamada placebo. (1 pt)
# Extraer los datos según el tratamiento
placebo  <- datos[datos$Tratamiento == "Placebo", ]
farmacoA <- datos[datos$Tratamiento == "FarmacoA", ]
farmacoB <- datos[datos$Tratamiento == "FarmacoB", ]
head(placebo)
head(farmacoA)
head(farmacoB)

# 11. Evalúa si los datos siguen una distribución normal y realiza una comparativa de medias acorde. (1 pt)
cat("\nPRUEBA DE NORMALIDAD (Shapiro-Wilk) por tratamiento:\n")
shapiro_results <- by(datos$Glucosa, datos$Tratamiento, shapiro.test)
print(shapiro_results)
# Si los datos son normales → usar ANOVA
cat("\nANOVA: Glucosa ~ Tratamiento\n")
anova_model <- aov(Glucosa ~ Tratamiento, data = datos)
print(summary(anova_model))
# Si los datos NO son normales → usar Kruskal-Wallis
cat("\nKruskal-Wallis (por si los datos no son normales):\n")
kruskal <- kruskal.test(Glucosa ~ Tratamiento, data = datos)
print(kruskal)

# 12. Realiza un ANOVA sobre la glucosa para cada tratamiento. (1 pt)
cat("\n------------------------------------\n")
cat("ANÁLISIS DE VARIANZA (ANOVA)\n")
cat("------------------------------------\n")
# Crear el modelo ANOVA
anova_model <- aov(Glucosa ~ Tratamiento, data = datos)
cat("\nResultados del ANOVA:\n")
print(summary(anova_model))


