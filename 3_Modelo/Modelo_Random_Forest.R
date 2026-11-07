# ========================================== #
# TRABAJO PRÁCTICO - RANDOM FOREST
# ========================================== #

# 0. Instalar y cargar las librerías que necesitamos
paquetes <- c("readxl", "ranger", "ggplot2", "DiagrammeR")
paquetes_faltantes <- paquetes[!paquetes %in% installed.packages()[, "Package"]]
if (length(paquetes_faltantes) > 0) install.packages(paquetes_faltantes)

library(readxl)
library(ranger)
library(ggplot2)
library(DiagrammeR)

# 1. Leer y preparar nuestro dataset
archivo <- file.choose() #Seleccionamos el archivo Excel con nuestra base de datos
df <- read_excel(archivo, sheet = "Base madre") #Leemos nuestro dataset

df_clean <- df[!is.na(df$demanda_electrica_neta_media_MW), ] #Eliminamos las filas con valores faltantes en la demanda

features <- c( #Indicamos cuales son las features que queremos que tome el modelo
  "temperatura_media_C_san_fernando",
  "humedad_relativa_media_pct_san_fernando",
  "temperatura_media_C_ezeiza",
  "humedad_relativa_media_pct_ezeiza",
  "temperatura_media_C_buenos_aires_obs",
  "temperatura_media_C_la_plata",
  "humedad_relativa_media_pct_la_plata",
  "mes_del_anio"
)

variable_objetivo <- "demanda_electrica_neta_media_MW"

#Nos quedamos con las variables que vamos a utilizar y eliminamos filas incompletas
df_modelo <- df_clean[, c(variable_objetivo, features)]
df_modelo <- na.omit(df_modelo)

# 2. Dividir los datos en entrenamiento y prueba
set.seed(42) #Fijamos una semilla para poder reproducir la division
indices_train <- sample(seq_len(nrow(df_modelo)), size = floor(0.8 * nrow(df_modelo)))
train <- df_modelo[indices_train, ]
test <- df_modelo[-indices_train, ]


# 3. Configurar y entrenar el Random Forest Regressor
formula_rf <- reformulate(features, response = variable_objetivo)

rf_model <- ranger(
  formula = formula_rf,
  data = train,
  num.trees = 100,
  max.depth = 10,
  seed = 42,
  importance = "impurity"
) #Le pasamos los parámetros y entrenamos el modelo

# 4. Obtener las predicciones en el set de prueba
y_real_MW <- test[[variable_objetivo]]
y_pred_rf <- predict(rf_model, data = test)$predictions

# 5. Calcular métricas para Random Forest
errores <- y_real_MW - y_pred_rf
errores_absolutos <- abs(errores)

mae_rf <- mean(errores_absolutos)
rmse_rf <- sqrt(mean(errores^2))

#Identificamos los picos usando el mismo percentil 90
limite_alto <- as.numeric(quantile(y_real_MW, 0.90))
indices_picos <- which(y_real_MW >= limite_alto)
picos_reales <- y_real_MW[indices_picos]
picos_predichos_rf <- y_pred_rf[indices_picos]

mae_picos_rf <- mean(abs(picos_reales - picos_predichos_rf))
subestimacion_rf <- mean(picos_reales) - mean(picos_predichos_rf)

#Calculamos la dispersion de los errores absolutos
desvio_estandar_rf <- sd(errores_absolutos)
coef_variacion_rf <- (desvio_estandar_rf / mean(errores_absolutos)) * 100

# 6. Imprimir tabla de métricas para Random Forest
metricas <- data.frame(
  MÉTRICA = c(
    "MAE Global (MW)",
    "RMSE Global (MW)",
    "MAE en Picos (Top 10% MW)",
    "Subestimación promedio en picos (MW)",
    "Desviación estándar del error absoluto (MW)",
    "Coeficiente de variación del error absoluto (%)"
  ),
  `RANDOM FOREST` = c(
    mae_rf,
    rmse_rf,
    mae_picos_rf,
    subestimacion_rf,
    desvio_estandar_rf,
    coef_variacion_rf
  ),
  check.names = FALSE
)

cat("\n")
cat("====================================================================\n")
cat(sprintf("%-52s | %18s\n", "MÉTRICA", "RANDOM FOREST"))
cat("====================================================================\n")
for (i in seq_len(nrow(metricas))) {
  sufijo <- if (grepl("%", metricas$MÉTRICA[i])) "%" else ""
  cat(sprintf("%-52s | %15.2f %s\n",
              metricas$MÉTRICA[i],
              metricas[["RANDOM FOREST"]][i],
              sufijo))
}
cat("Cantidad de muestras de picos analizadas:", length(picos_reales), "\n")
cat(sprintf("Promedio Real de los Picos: %.2f MW\n", mean(picos_reales)))
cat(sprintf("Promedio Predicho de los Picos (RF): %.2f MW\n", mean(picos_predichos_rf)))
cat("====================================================================\n\n")



#======== A partir de acá son gráficos ======0=====


#GRÁFICO 1: COMPARATIVA TEMPORAL
cantidad_muestras <- min(100, length(y_real_MW))
datos_temporales <- data.frame(
  Muestra = rep(seq_len(cantidad_muestras), 2),
  Demanda = c(y_real_MW[seq_len(cantidad_muestras)],
              y_pred_rf[seq_len(cantidad_muestras)]),
  Tipo = rep(c("Valor Real (MW)", "Predicción Random Forest"), each = cantidad_muestras)
)

grafico_temporal <- ggplot(datos_temporales, aes(x = Muestra, y = Demanda, color = Tipo)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5) +
  labs(
    title = "Comparativa con Random Forest: Real vs Predicción (Muestra Temporal)",
    x = "Índice de Muestra",
    y = "Demanda Eléctrica (MW)",
    color = ""
  ) +
  theme_minimal()

print(grafico_temporal)

#GRÁFICO 2: DIAGRAMA DE DISPERSIÓN DE PICOS
datos_dispersion <- data.frame(
  Real = y_real_MW,
  Predicho = y_pred_rf,
  Es_pico = seq_along(y_real_MW) %in% indices_picos
)

grafico_picos <- ggplot(datos_dispersion, aes(x = Real, y = Predicho)) +
  geom_point(aes(color = Es_pico), alpha = 0.65) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  scale_color_manual(
    values = c("FALSE" = "mediumpurple", "TRUE" = "orange"),
    labels = c("FALSE" = "Resto de los datos", "TRUE" = "Picos (Top 10%)"),
    name = ""
  ) +
  labs(
    title = "Diagrama de Dispersión Random Forest: Comportamiento Real en Picos",
    x = "Valores Reales (MW)",
    y = "Predicciones (MW)"
  ) +
  theme_minimal()

print(grafico_picos)

#GRÁFICO 3: RELACIÓN CON VARIABLES CLIMATOLÓGICAS
datos_variables <- data.frame(
  Temperatura_BA = test$temperatura_media_C_buenos_aires_obs,
  Humedad_LP = test$humedad_relativa_media_pct_la_plata,
  Mes = test$mes_del_anio,
  Real = y_real_MW,
  Predicho = y_pred_rf
)

par(mfrow = c(1, 3))

plot(datos_variables$Humedad_LP, datos_variables$Real,
     col = rgb(0, 0, 1, 0.45), pch = 16,
     xlab = "Humedad (%)", ylab = "Demanda Eléctrica (MW)",
     main = "Demanda vs Humedad")
points(datos_variables$Humedad_LP, datos_variables$Predicho,
       col = rgb(0.5, 0, 0.7, 0.45), pch = 16)
legend("topleft", legend = c("Real", "Predicho (RF)"),
       col = c("blue", "purple"), pch = 16, cex = 0.7)
grid()

plot(datos_variables$Temperatura_BA, datos_variables$Real,
     col = rgb(0, 0, 1, 0.45), pch = 16,
     xlab = "Temperatura (°C)", ylab = "Demanda Eléctrica (MW)",
     main = "Demanda vs Temperatura")
points(datos_variables$Temperatura_BA, datos_variables$Predicho,
       col = rgb(0.5, 0, 0.7, 0.45), pch = 16)
legend("topleft", legend = c("Real", "Predicho (RF)"),
       col = c("blue", "purple"), pch = 16, cex = 0.7)
grid()

plot(datos_variables$Mes, datos_variables$Real,
     col = rgb(0, 0, 1, 0.45), pch = 16,
     xlab = "Mes", ylab = "Demanda Eléctrica (MW)",
     main = "Demanda vs Mes del Año")
points(datos_variables$Mes, datos_variables$Predicho,
       col = rgb(0.5, 0, 0.7, 0.45), pch = 16)
legend("topleft", legend = c("Real", "Predicho (RF)"),
       col = c("blue", "purple"), pch = 16, cex = 0.7)
grid()

par(mfrow = c(1, 1))

#Importancia de las variables
importancia <- data.frame(
  Variable = names(rf_model$variable.importance),
  Importancia = as.numeric(rf_model$variable.importance)
)

grafico_importancia <- ggplot(importancia,
                              aes(x = reorder(Variable, Importancia), y = Importancia)) +
  geom_col(fill = "mediumpurple") +
  coord_flip() +
  labs(
    title = "Importancia de las variables en el Random Forest",
    x = "Variable",
    y = "Importancia"
  ) +
  theme_minimal()

print(grafico_importancia)

#Ploteo de la estructura de uno de los árboles del Random Forest
arbol_ejemplo <- ranger::treeInfo(rf_model, tree = 1) #Seleccionamos el primer árbol del modelo

#Armamos el diagrama hasta una profundidad de 3 para poder leer las decisiones
nodos <- arbol_ejemplo
dot <- c(
  "digraph arbol {",
  "graph [rankdir = TB]",
  "node [shape = box, style = rounded, fontname = Helvetica, fontsize = 10]"
)

agregar_nodo <- function(id, profundidad) {
  fila <- nodos[nodos$nodeID == id, ]

  if (nrow(fila) == 0) return(invisible(NULL))

  if (isTRUE(fila$terminal) || profundidad >= 3) {
    if (isTRUE(fila$terminal)) {
      etiqueta <- paste0("Predicción = ", round(fila$prediction, 2))
    } else {
      etiqueta <- "..."
    }
    dot <<- c(dot, sprintf('n%s [label="%s"];', id, etiqueta))
    return(invisible(NULL))
  }

  etiqueta <- paste0(fila$splitvarName, " <= ", round(fila$splitval, 2))
  dot <<- c(dot, sprintf('n%s [label="%s"];', id, etiqueta))

  izquierdo <- fila$leftChild
  derecho <- fila$rightChild

  dot <<- c(dot, sprintf('n%s -> n%s [label="Sí"];', id, izquierdo))
  dot <<- c(dot, sprintf('n%s -> n%s [label="No"];', id, derecho))

  agregar_nodo(izquierdo, profundidad + 1)
  agregar_nodo(derecho, profundidad + 1)
}

agregar_nodo(0, 0)
dot <- c(dot, "}")

diagrama_arbol <- DiagrammeR::grViz(paste(dot, collapse = "\n"))
print(diagrama_arbol)

