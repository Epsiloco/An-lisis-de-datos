
datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")
datos_limpios <- datos %>% tidyr::drop_na(all_of(c(variables_y, var_x1, var_x2)))

View(datos)

# ==============================================================================
# 0. CARGA DE LIBRERÍAS
# ==============================================================================
install.packages("randomForest")
library(ggplot2)
library(mgcv)          # Para modelos GAM
library(randomForest)  # Para Random Forest
library(e1071)         # Para SVM
library(rpart)         # Para Árboles de decisión
library(lmtest)        # Para pruebas de homocedasticidad e independencia
library(car)           # Para QQ-plots y diagnósticos
library(splines)       # Para Splines
library(dplyr)         # Para manipulación de datos

names(datos)
names(datos_limpios)

# Asumiendo que tu dataframe ya se llama 'datos' y contiene TAH_Rel e Hidro_100
datos <- na.omit(datos[, c("TAH_PRZ_Rel", "Hidro_100")]) # Recomendado limpiar NAs
str(datos)

# ==============================================================================
# 1. VERIFICACIÓN DE SUPUESTOS (Numérica y Gráfica)
# ==============================================================================
# Los supuestos se evalúan sobre los residuos de un modelo base. 
# Usaremos el modelo cuadrático como base teórica de optimización.
mod_base <- lm(TAH_PRZ_Rel ~ poly(Hidro_100, 2, raw = TRUE), data = datos)
residuos <- residuals(mod_base)

cat("--- VERIFICACIÓN DE SUPUESTOS ---\n")

# A. Normalidad
# Numérica: Prueba de Shapiro-Wilk (si N > 5000, usar nortest::ad.test)
shapiro_test <- shapiro.test(residuos)
cat("Normalidad (Shapiro-Wilk): p-valor =", shapiro_test$p.value, "\n")

# B. Homocedasticidad
# Numérica: Prueba de Breusch-Pagan
bp_test <- bptest(mod_base)
cat("Homocedasticidad (Breusch-Pagan): p-valor =", bp_test$p.value, "\n")

# C. Independencia
# Numérica: Prueba de Durbin-Watson
dw_test <- dwtest(mod_base)
cat("Independencia (Durbin-Watson): p-valor =", dw_test$p.value, "\n")

# Gráficas de supuestos
par(mfrow = c(2, 2))
# 1. Normalidad (QQ Plot)
qqPlot(residuos, main = "Q-Q Plot de Residuos", ylab = "Residuos Estudentizados")
# 2. Homocedasticidad (Residuos vs Ajustados)
plot(fitted(mod_base), residuos, main = "Residuos vs Ajustados", 
     xlab = "Valores Ajustados (TAH_Rel)", ylab = "Residuos")
abline(h = 0, col = "red", lty = 2)
# 3. Distribución de TAH_Rel original
hist(datos$TAH_PRZ_Rel, main = "Histograma de TAH_Rel", xlab = "TAH_PRZ_Rel", col = "lightblue")
# 4. Relación directa
plot(datos$Hidro_100, datos$TAH_PZA_Rel, main = "TAH_Rel vs Hidro_100", 
     xlab = "Hidro_100", ylab = "TAH_PRZ_Rel", pch = 16, col = rgb(0,0,0,0.5))
par(mfrow = c(1, 1))

# ==============================================================================
# 2. AJUSTE DE MODELOS
# ==============================================================================
# Ajustaremos 10 modelos diferentes
m1_lin   <- lm(TAH_PRZ_Rel ~ Hidro_100, data = datos)
m2_quad  <- lm(TAH_PRZ_Rel ~ poly(Hidro_100, 2, raw=TRUE), data = datos)
m3_cub   <- lm(TAH_PRZ_Rel ~ poly(Hidro_100, 3, raw=TRUE), data = datos)
m4_poly4 <- lm(TAH_PRZ_Rel ~ poly(Hidro_100, 4, raw=TRUE), data = datos)
m5_splin <- lm(TAH_PRZ_Rel ~ ns(Hidro_100, df = 4), data = datos)
m6_gam   <- gam(TAH_PRZ_Rel ~ s(Hidro_100), data = datos)
m7_loess <- loess(TAH_PRZ_Rel ~ Hidro_100, data = datos, control = loess.control(surface = "direct"))
set.seed(123)
m8_rf    <- randomForest(TAH_PRZ_Rel ~ Hidro_100, data = datos, ntree = 500)
m9_svm   <- svm(TAH_PRZ_Rel ~ Hidro_100, data = datos)
m10_tree <- rpart(TAH_PRZ_Rel ~ Hidro_100, data = datos)

lista_modelos <- list(
  "Lineal" = m1_lin, "Cuadratico" = m2_quad, "Cubico" = m3_cub, 
  "Polinomio_G4" = m4_poly4, "Spline_Natural" = m5_splin, "GAM" = m6_gam, 
  "Loess" = m7_loess, "Random_Forest" = m8_rf, "SVM" = m9_svm, 
  "Arbol_Decision" = m10_tree
)

# ==============================================================================
# 3. VALIDACIÓN INTERNA Y COMPARACIÓN ENTRE MODELOS
# ==============================================================================
# Función para extraer métricas de validación de cada modelo
extraer_metricas <- function(modelo, nombre, df_datos) {
  y_real <- df_datos$TAH_PRZ_Rel
  
  # Predicciones
  if(nombre == "Loess") {
    y_pred <- predict(modelo, newdata = df_datos)
  } else {
    y_pred <- predict(modelo, df_datos)
  }
  
  # RMSE y MAE
  rmse <- sqrt(mean((y_real - y_pred)^2, na.rm = TRUE))
  mae <- mean(abs(y_real - y_pred), na.rm = TRUE)
  
  # R2 y Adj R2
  sse <- sum((y_real - y_pred)^2, na.rm = TRUE)
  sst <- sum((y_real - mean(y_real, na.rm = TRUE))^2, na.rm = TRUE)
  r2 <- 1 - (sse / sst)
  
  n <- nrow(df_datos)
  # Aproximación de parámetros para Adj R2
  p <- if(inherits(modelo, "lm") || inherits(modelo, "gam")) length(coef(modelo)) else 3 
  adj_r2 <- 1 - ((1 - r2) * (n - 1) / (n - p - 1))
  
  # AIC, BIC y P-valor (Solo aplicable estrictamente para modelos paramétricos/verosimilitud)
  aic_val <- tryCatch(AIC(modelo), error = function(e) NA)
  bic_val <- tryCatch(BIC(modelo), error = function(e) NA)
  
  # Extracción de p-valor del modelo (F-statistic) para lm, aproximado para gam
  pval <- NA
  if(inherits(modelo, "lm")) {
    f_stat <- summary(modelo)$fstatistic
    if(!is.null(f_stat)) {
      pval <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
    }
  } else if(inherits(modelo, "gam")) {
    pval <- summary(modelo)$s.table[1, 4] # p-valor del smooth term
  }
  
  data.frame(Modelo = nombre, R2 = round(r2, 4), Adj_R2 = round(adj_r2, 4), 
             RMSE = round(rmse, 4), MAE = round(mae, 4), 
             AIC = round(aic_val, 2), BIC = round(bic_val, 2), 
             P_Valor = signif(pval, 4))
}

# Generar tabla comparativa
tabla_validacion <- do.call(rbind, lapply(names(lista_modelos), function(nom) {
  extraer_metricas(lista_modelos[[nom]], nom, datos)
}))

# Ordenar por el mejor Ajuste (Menor RMSE)
tabla_validacion <- tabla_validacion %>% arrange(RMSE)
print("--- TABLA DE VALIDACIÓN ENTRE MODELOS ---")
print(tabla_validacion)

# ==============================================================================
# 4. DETERMINACIÓN DE LA HUMEDAD ÓPTIMA
# ==============================================================================
# Crear secuencia densa de humedad para predecir y buscar el máximo
rango_humedad <- seq(min(datos$Hidro_100, na.rm = TRUE), 
                     max(datos$Hidro_100, na.rm = TRUE), 
                     length.out = 2000)
df_simulacion <- data.frame(Hidro_100 = rango_humedad)

# Encontrar el óptimo para cada modelo
optimos <- lapply(names(lista_modelos), function(nom) {
  modelo <- lista_modelos[[nom]]
  
  if(nom == "Loess") {
    preds <- predict(modelo, newdata = df_simulacion)
  } else {
    preds <- predict(modelo, df_simulacion)
  }
  
  # Índice del valor máximo predicho de TAH_Rel
  idx_max <- which.max(preds)
  
  data.frame(
    Modelo = nom,
    Hidro_100_Optima = round(df_simulacion$Hidro_100[idx_max], 2),
    TAH_Rel_Max_Proyectado = round(preds[idx_max], 4)
  )
})

tabla_optimos <- do.call(rbind, optimos)

# Unir con la tabla de validación para tener el panorama completo
resultado_final <- merge(tabla_validacion, tabla_optimos, by = "Modelo") %>%
  arrange(RMSE)

cat("\n--- VALORES ÓPTIMOS DE HUMEDAD POR MODELO ---\n")
print(resultado_final[, c("Modelo", "RMSE", "R2", "Hidro_100_Optima", "TAH_Rel_Max_Proyectado")])

# ==============================================================================
# 5. VISUALIZACIÓN DE LA CURVA DEL MEJOR MODELO
# ==============================================================================
mejor_modelo_nom <- "GAM"
mejor_optimo <- tabla_optimos$Hidro_100_Optima[tabla_optimos$Modelo == mejor_modelo_nom]
max_tah <- tabla_optimos$TAH_Rel_Max_Proyectado[tabla_optimos$Modelo == mejor_modelo_nom]

df_simulacion$Pred_TAH <- predict(lista_modelos[[mejor_modelo_nom]], df_simulacion)

# Calcular límites amplios para ver el óptimo
# 1) Rango de Hidro_100: desde el mínimo de los datos o de la simulación, 
#    hasta el máximo incluyendo el óptimo con margen
x_min <- min(c(datos$Hidro_100, df_simulacion$Hidro_100))
x_max <- max(c(datos$Hidro_100, df_simulacion$Hidro_100, mejor_optimo)) * 1.05  # 5% de margen

# 2) Rango de TAH: desde el mínimo observado, hasta el máximo proyectado con margen
y_min <- min(datos$TAH_PRZ_Rel) * 0.95
y_max <- max(c(datos$TAH_PRZ_Rel, df_simulacion$Pred_TAH, max_tah)) * 1.05

# Gráfico con límites ajustados
ggplot(datos, aes(x = Hidro_100, y = TAH_PRZ_Rel)) +
  geom_point(alpha = 0.5, color = "gray40") +
  geom_line(data = df_simulacion, aes(x = Hidro_100, y = Pred_TAH), 
            color = "blue", size = 1.2) +
  geom_vline(xintercept = mejor_optimo, linetype = "dashed", color = "red", size = 1) +
  geom_hline(yintercept = max_tah, linetype = "dashed", color = "darkgreen") +
  annotate("text", 
           x = mejor_optimo, 
           y = min(datos$TAH_PRZ_Rel) * 0.95,   # colocar texto cerca del mínimo
           label = paste("Óptimo:", mejor_optimo), 
           color = "red", hjust = -0.1, vjust = 0) +
  labs(title = paste("Curva de Optimización para TAH_Rel usando", mejor_modelo_nom),
       subtitle = "Punto sugerido para inicio de aplicación del madurante",
       x = "Humedad (Hidro_100)", 
       y = "TAH Relativo") +
  coord_cartesian(xlim = c(x_min, x_max), 
                  ylim = c(y_min, y_max)) +   # <-- Aquí controlas el zoom
  theme_minimal()


# ==============================================================================
# 0. LIBRERÍAS NECESARIAS
# ==============================================================================
library(mgcv)
library(randomForest)
library(e1071)
library(rpart)
library(splines)
library(dplyr)
library(caret) # Útil para crear pliegues estratificados si es necesario

# Asumiendo que 'datos' ya está cargado en tu entorno
# datos <- na.omit(datos[, c("TAH_Rel", "Hidro_100")])

# Nombres de los modelos a evaluar
nombres_modelos <- c("Lineal", "Cuadratico", "Cubico", "Polinomio_G4", 
                     "Spline_Natural", "GAM", "Loess", "Random_Forest", 
                     "SVM", "Arbol_Decision")

# Función auxiliar para entrenar dinámicamente cualquier modelo
entrenar_modelo <- function(nom, train_data) {
  if(nom == "Lineal") return(lm(TAH_PRZ_Rel ~ Hidro_100, data = train_data))
  if(nom == "Cuadratico") return(lm(TAH_PRZ_Rel ~ poly(Hidro_100, 2, raw=TRUE), data = train_data))
  if(nom == "Cubico") return(lm(TAH_PRZ_Rel ~ poly(Hidro_100, 3, raw=TRUE), data = train_data))
  if(nom == "Polinomio_G4") return(lm(TAH_PRZ_Rel ~ poly(Hidro_100, 4, raw=TRUE), data = train_data))
  if(nom == "Spline_Natural") return(lm(TAH_PRZ_Rel ~ ns(Hidro_100, df=4), data = train_data))
  if(nom == "GAM") return(gam(TAH_PRZ_Rel ~ s(Hidro_100), data = train_data))
  if(nom == "Loess") return(loess(TAH_PRZ_Rel ~ Hidro_100, data = train_data, control = loess.control(surface = "direct")))
  if(nom == "Random_Forest") {
    set.seed(123)
    return(randomForest(TAH_PRZ_Rel ~ Hidro_100, data = train_data, ntree = 500))
  }
  if(nom == "SVM") return(svm(TAH_PRZ_Rel ~ Hidro_100, data = train_data))
  if(nom == "Arbol_Decision") return(rpart(TAH_PRZ_Rel ~ Hidro_100, data = train_data))
}

# ==============================================================================
# 1. K-FOLD CROSS-VALIDATION (K = 10)
# ==============================================================================
cat("Iniciando K-Fold Cross-Validation (K=10)...\n")
set.seed(42)
k <- 10
# Crear índices para los pliegues (folds)
pliegues <- createFolds(datos$TAH_PRZ_Rel, k = k, list = TRUE, returnTrain = FALSE)

# Matriz para guardar resultados de K-Fold
resultados_kfold <- data.frame(Modelo = character(), RMSE_CV = numeric(), R2_CV = numeric())

for(nom in nombres_modelos) {
  rmse_pliegues <- c()
  r2_pliegues <- c()
  
  for(i in 1:k) {
    idx_test <- pliegues[[i]]
    train_data <- datos[-idx_test, ]
    test_data <- datos[idx_test, ]
    
    # Entrenar modelo con el 90%
    mod <- tryCatch(entrenar_modelo(nom, train_data), error = function(e) NULL)
    
    if(!is.null(mod)) {
      # Predecir sobre el 10% oculto
      if(nom == "Cuadratico") {
        preds <- predict(mod, newdata = test_data)
      } else {
        preds <- predict(mod, test_data)
      }
      
      # Calcular métricas del pliegue (evitando NAs en Loess en bordes extremos)
      valid_idx <- !is.na(preds)
      if(sum(valid_idx) > 1) {
        y_real <- test_data$TAH_PRZ_Rel[valid_idx]
        y_pred <- preds[valid_idx]
        
        rmse <- sqrt(mean((y_real - y_pred)^2))
        r2 <- cor(y_real, y_pred)^2 # R2 aproximado de predicción
        
        rmse_pliegues <- c(rmse_pliegues, rmse)
        r2_pliegues <- c(r2_pliegues, r2)
      }
    }
  }
  
  # Promediar métricas del modelo
  resultados_kfold <- rbind(resultados_kfold, data.frame(
    Modelo = nom,
    RMSE_CV = round(mean(rmse_pliegues, na.rm = TRUE), 4),
    R2_CV = round(mean(r2_pliegues, na.rm = TRUE), 4)
  ))
}

print(resultados_kfold %>% arrange(RMSE_CV))

# ==============================================================================
# 2. PRUEBA BOOTSTRAP (B = 100 iteraciones)
# ==============================================================================
cat("\nIniciando Validación por Bootstrap (B=100)...\n")
set.seed(84)
B <- 100
n_filas <- nrow(datos)

resultados_boot <- data.frame(Modelo = character(), RMSE_Boot = numeric(), R2_Boot = numeric())

for(nom in nombres_modelos) {
  rmse_b <- c()
  r2_b <- c()
  
  for(i in 1:B) {
    # Muestreo con reemplazo
    idx_boot <- sample(1:n_filas, size = n_filas, replace = TRUE)
    train_boot <- datos[idx_boot, ]
    
    # Datos Out-of-Bag (OOB) para validación (los que no entraron en el remuestreo)
    test_oob <- datos[-unique(idx_boot), ]
    
    mod <- tryCatch(entrenar_modelo(nom, train_boot), error = function(e) NULL)
    
    if(!is.null(mod) && nrow(test_oob) > 0) {
      if(nom == "Loess") {
        preds <- predict(mod, newdata = test_oob)
      } else {
        preds <- predict(mod, test_oob)
      }
      
      valid_idx <- !is.na(preds)
      if(sum(valid_idx) > 1) {
        y_real <- test_oob$TAH_PRZ_Rel[valid_idx]
        y_pred <- preds[valid_idx]
        
        rmse <- sqrt(mean((y_real - y_pred)^2))
        r2 <- cor(y_real, y_pred)^2
        
        rmse_b <- c(rmse_b, rmse)
        r2_b <- c(r2_b, r2)
      }
    }
  }
  
  resultados_boot <- rbind(resultados_boot, data.frame(
    Modelo = nom,
    RMSE_Boot = round(mean(rmse_b, na.rm = TRUE), 4),
    R2_Boot = round(mean(r2_b, na.rm = TRUE), 4)
  ))
}

print(resultados_boot %>% arrange(RMSE_Boot))

# ==============================================================================
# 3. TABLA CONSOLIDADA
# ==============================================================================
# Unimos las dos validaciones para tomar la decisión final
tabla_final_validacion <- merge(resultados_kfold, resultados_boot, by = "Modelo") %>%
  arrange(RMSE_CV)

cat("\n--- RESUMEN FINAL DE VALIDACIÓN EXTERNA ---\n")
print(tabla_final_validacion)



# ==============================================================================
# 8. MÓDULO MULTIVARIADO: OPTIMIZACIÓN DE DOSIS (CCTon) EN 4 RANGOS DE HUMEDAD
# ==============================================================================
cat("\n--- INICIANDO ZONIFICACIÓN DE HUMEDAD (Análogo a image_0bbeae.jpg) ---\n")

# 8.1 Ajuste de Modelos Multivariados (Humedad + Dosis)
mv_lin   <- lm(TAH_Rel ~ Hidro_100 * CCTon, data = datos)
mv_quad  <- lm(TAH_Rel ~ poly(Hidro_100, 2, raw=TRUE) * poly(CCTon, 2, raw=TRUE), data = datos)
mv_gam   <- gam(TAH_Rel ~ te(Hidro_100, CCTon), data = datos) 
mv_loess <- loess(TAH_Rel ~ Hidro_100 * CCTon, data = datos, control = loess.control(surface = "direct"))
set.seed(123)
mv_rf    <- randomForest(TAH_Rel ~ Hidro_100 + CCTon, data = datos, ntree = 500)
mv_svm   <- svm(TAH_Rel ~ Hidro_100 + CCTon, data = datos)

lista_modelos_mv <- list(
  "MV_Lineal" = mv_lin, "MV_Cuadratico" = mv_quad, "MV_GAM" = mv_gam,
  "MV_Loess" = mv_loess, "MV_Random_Forest" = mv_rf, "MV_SVM" = mv_svm
)

# 8.2 Definir los 4 rangos de humedad usando cuartiles (Zonas dinámicas)
# Esto detecta los cortes estadísticos de tus propios datos (25%, 50%, 75%)
cortes_hidro <- quantile(datos$Hidro_100, probs = c(0, 0.25, 0.50, 0.75, 1), na.rm = TRUE)

nombres_zonas <- c(
  paste("Zona 1: <", round(cortes_hidro[2], 1), "%"),
  paste("Zona 2:", round(cortes_hidro[2], 1), "a", round(cortes_hidro[3], 1), "%"),
  paste("Zona 3:", round(cortes_hidro[3], 1), "a", round(cortes_hidro[4], 1), "%"),
  paste("Zona 4: >=", round(cortes_hidro[4], 1), "%")
)

# 8.3 Crear una cuadrícula densa para simular la superficie 3D
grid_hidro <- seq(min(datos$Hidro_100, na.rm = TRUE), max(datos$Hidro_100, na.rm = TRUE), length.out = 200)
grid_ccton <- seq(min(datos$CCTon, na.rm = TRUE), max(datos$CCTon, na.rm = TRUE), length.out = 200)
df_superficie <- expand.grid(Hidro_100 = grid_hidro, CCTon = grid_ccton)

# Asignar a qué zona pertenece cada fila simulada
df_superficie$Zona_Humedad <- cut(df_superficie$Hidro_100, 
                                  breaks = cortes_hidro, 
                                  include.lowest = TRUE, 
                                  labels = nombres_zonas)

# ==============================================================================
# 8.4 Encontrar la Dosis Óptima por Zona y Modelo (CORREGIDO)
# ==============================================================================
optimos_zonas_lista <- lapply(names(lista_modelos_mv), function(nom) {
  modelo <- lista_modelos_mv[[nom]]
  
  # Forzamos explícitamente 'newdata' y convertimos a vector numérico
  # para evitar que modelos como LOESS devuelvan matrices
  preds <- as.numeric(predict(modelo, newdata = df_superficie))
  
  df_temp <- df_superficie
  df_temp$Pred_TAH_Rel <- preds
  df_temp$Modelo <- nom
  
  # Buscar la dosis exacta que maximiza el TAH_Rel dentro de cada zona
  df_optimos <- df_temp %>%
    filter(!is.na(Pred_TAH_Rel)) %>% # Ignorar los NA de bordes
    group_by(Modelo, Zona_Humedad) %>%
    slice(which.max(Pred_TAH_Rel)) %>%
    ungroup() %>%
    select(Modelo, Zona_Humedad, Dosis_Optima_ccTon = CCTon, 
           Humedad_Exacta_Pico = Hidro_100, TAH_Rel_Maximo = Pred_TAH_Rel)
  
  return(df_optimos)
})

tabla_zonas_optimas <- do.call(rbind, optimos_zonas_lista)

cat("\n--- RECOMENDACIÓN DE DOSIS POR RANGO DE HUMEDAD ---\n")
print(as.data.frame(tabla_zonas_optimas %>% arrange(Modelo, Zona_Humedad)))

# ==============================================================================
# 9. VISUALIZACIÓN DE SUPERFICIE DE RESPUESTA CON ZONAS DE DECISIÓN
# ==============================================================================
# Requerirá el paquete metR o ggplot2 básico. Usaremos geom_contour_filled de ggplot2
# Cambia 'mejor_mv' por el modelo que validó mejor en tu Bootstrap (ej. MV_GAM o MV_Cuadratico)
mejor_mv <- "MV_Loess" 

df_graf_sup <- df_superficie
df_graf_sup$Pred_TAH_Rel <- predict(lista_modelos_mv[[mejor_mv]], df_graf_sup)
recs_graf <- tabla_zonas_optimas %>% filter(Modelo == mejor_mv)

# Coordenadas X centradas para las etiquetas de texto
centro_x <- max(grid_ccton) * 0.45 

ggplot(df_graf_sup, aes(x = CCTon, y = Hidro_100)) +
  # 1. Gradiente de colores simulando la superficie de respuesta
  geom_contour_filled(aes(z = Pred_TAH_Rel), alpha = 0.85, bins = 10) +
  scale_fill_viridis_d(option = "turbo", name = "TAH_Rel Predicho") + # Colores similares a tu imagen
  
  # 2. Puntos reales del dataset
  geom_point(data = datos, aes(x = CCTon, y = Hidro_100), color = "black", alpha = 0.4, size = 1) +
  
  # 3. Líneas de corte de las 4 Zonas Operativas
  geom_hline(yintercept = cortes_hidro[2], linetype = "dashed", color = "black", size = 0.8) +
  geom_hline(yintercept = cortes_hidro[3], linetype = "dashed", color = "black", size = 0.8) +
  geom_hline(yintercept = cortes_hidro[4], linetype = "dashed", color = "black", size = 0.8) +
  
  # 4. Textos con las recomendaciones automáticas de Dosis (CCTon)
  annotate("text", x = centro_x, y = mean(c(cortes_hidro[1], cortes_hidro[2])), 
           label = paste(nombres_zonas[1], "• APLICAR", round(recs_graf$Dosis_Optima_ccTon[1], 1), "cc/ton"), 
           color = "white", fontface = "bold", size = 4) +
  annotate("text", x = centro_x, y = mean(c(cortes_hidro[2], cortes_hidro[3])), 
           label = paste(nombres_zonas[2], "• APLICAR", round(recs_graf$Dosis_Optima_ccTon[2], 1), "cc/ton"), 
           color = "white", fontface = "bold", size = 4) +
  annotate("text", x = centro_x, y = mean(c(cortes_hidro[3], cortes_hidro[4])), 
           label = paste(nombres_zonas[3], "• APLICAR", round(recs_graf$Dosis_Optima_ccTon[3], 1), "cc/ton"), 
           color = "white", fontface = "bold", size = 4) +
  annotate("text", x = centro_x, y = mean(c(cortes_hidro[4], max(grid_hidro))), 
           label = paste(nombres_zonas[4], "• APLICAR", round(recs_graf$Dosis_Optima_ccTon[4], 1), "cc/ton"), 
           color = "white", fontface = "bold", size = 4) +
  
  # 5. Estética y títulos
  labs(title = paste("Zonas Operativas Auto-Detectadas - Modelo:", mejor_mv),
       subtitle = "Validación de Dosis Óptima (Eje X) según franja de Humedad (Eje Y)",
       x = "Dosis Aplicada (CCTon)", 
       y = "Humedad (Hidro_100)") +
  theme_minimal() +
  theme(legend.position = "right")


########### Para las 4 zonas de la SR





# ==============================================================================
# 8. MÓDULO MULTIVARIADO: OPTIMIZACIÓN DE DOSIS (CCTon) EN 4 RANGOS DE HUMEDAD
# ==============================================================================
cat("\n--- INICIANDO ZONIFICACIÓN DE HUMEDAD (Análogo a image_0bbeae.jpg) ---\n")

# 8.1 Ajuste de Modelos Multivariados (Humedad + Dosis)
mv_lin   <- lm(TAH_Rel ~ Hidro_100 * CCTon, data = datos)
mv_quad  <- lm(TAH_Rel ~ poly(Hidro_100, 2, raw=TRUE) * poly(CCTon, 2, raw=TRUE), data = datos)
mv_gam   <- gam(TAH_Rel ~ te(Hidro_100, CCTon), data = datos) 
mv_loess <- loess(TAH_Rel ~ Hidro_100 * CCTon, data = datos, control = loess.control(surface = "direct"))
set.seed(123)
mv_rf    <- randomForest(TAH_Rel ~ Hidro_100 + CCTon, data = datos, ntree = 500)
mv_svm   <- svm(TAH_Rel ~ Hidro_100 + CCTon, data = datos)

lista_modelos_mv <- list(
  "MV_Lineal" = mv_lin, "MV_Cuadratico" = mv_quad, "MV_GAM" = mv_gam,
  "MV_Loess" = mv_loess, "MV_Random_Forest" = mv_rf, "MV_SVM" = mv_svm
)

# ==============================================================================
# 8.2 Definir los 4 rangos de humedad empíricos (Análogo a superficie original)
# ==============================================================================
# Usamos tus cortes agronómicos exactos: 64.5, 72.5 y 81.5
min_hidro <- min(datos$Hidro_100, na.rm = TRUE)
max_hidro <- max(datos$Hidro_100, na.rm = TRUE)

# Creamos el vector de cortes asegurando que atrape los valores mínimos y máximos reales de tu DF
cortes_hidro <- c(floor(min_hidro) - 1, 64.5, 72.5, 81.5, ceiling(max_hidro) + 1)

# Nombramos las zonas exactamente como en tu imagen
nombres_zonas <- c(
  "Zona 1: < 64.5 %",
  "Zona 2: 64.5 a 72.5 %",
  "Zona 3: 72.5 a 81.5 %",
  "Zona 4: >= 81.5 %"
)

# 8.3 Crear una cuadrícula densa para simular la superficie 3D
grid_hidro <- seq(min(datos$Hidro_100, na.rm = TRUE), max(datos$Hidro_100, na.rm = TRUE), length.out = 200)
grid_ccton <- seq(min(datos$CCTon, na.rm = TRUE), max(datos$CCTon, na.rm = TRUE), length.out = 200)
df_superficie <- expand.grid(Hidro_100 = grid_hidro, CCTon = grid_ccton)

# Asignar a qué zona pertenece cada fila simulada
df_superficie$Zona_Humedad <- cut(df_superficie$Hidro_100, 
                                  breaks = cortes_hidro, 
                                  include.lowest = TRUE, 
                                  labels = nombres_zonas)

# 8.4 Encontrar la Dosis Óptima por Zona y Modelo (Usando as.numeric para evitar errores)
optimos_zonas_lista <- lapply(names(lista_modelos_mv), function(nom) {
  modelo <- lista_modelos_mv[[nom]]
  
  preds <- as.numeric(predict(modelo, newdata = df_superficie))
  
  df_temp <- df_superficie
  df_temp$Pred_TAH_Rel <- preds
  df_temp$Modelo <- nom
  
  df_optimos <- df_temp %>%
    filter(!is.na(Pred_TAH_Rel)) %>%
    group_by(Modelo, Zona_Humedad) %>%
    slice(which.max(Pred_TAH_Rel)) %>%
    ungroup() %>%
    select(Modelo, Zona_Humedad, Dosis_Optima_ccTon = CCTon, 
           Humedad_Exacta_Pico = Hidro_100, TAH_Rel_Maximo = Pred_TAH_Rel)
  
  return(df_optimos)
})

tabla_zonas_optimas <- do.call(rbind, optimos_zonas_lista)

cat("\n--- RECOMENDACIÓN DE DOSIS POR RANGO DE HUMEDAD (CORTES FIJOS) ---\n")
print(as.data.frame(tabla_zonas_optimas %>% arrange(Modelo, Zona_Humedad)))

# ==============================================================================
# 9. VISUALIZACIÓN DE SUPERFICIE DE RESPUESTA CON ZONAS DE DECISIÓN
# ==============================================================================
# Requerirá el paquete metR o ggplot2 básico. Usaremos geom_contour_filled de ggplot2
# Cambia 'mejor_mv' por el modelo que validó mejor en tu Bootstrap (ej. MV_GAM o MV_Cuadratico)
mejor_mv <- "MV_GAM" 

df_graf_sup <- df_superficie
df_graf_sup$Pred_TAH_Rel <- predict(lista_modelos_mv[[mejor_mv]], df_graf_sup)
recs_graf <- tabla_zonas_optimas %>% filter(Modelo == mejor_mv)

# Coordenadas X centradas para las etiquetas de texto
centro_x <- max(grid_ccton) * 0.45 

ggplot(df_graf_sup, aes(x = CCTon, y = Hidro_100)) +
  # 1. Gradiente de colores simulando la superficie de respuesta
  geom_contour_filled(aes(z = Pred_TAH_Rel), alpha = 0.85, bins = 10) +
  scale_fill_viridis_d(option = "turbo", name = "TAH_Rel Predicho") + # Colores similares a tu imagen
  
  # 2. Puntos reales del dataset
  geom_point(data = datos, aes(x = CCTon, y = Hidro_100), color = "black", alpha = 0.4, size = 1) +
  
  # 3. Líneas de corte de las 4 Zonas Operativas
  geom_hline(yintercept = cortes_hidro[2], linetype = "dashed", color = "black", size = 0.8) +
  geom_hline(yintercept = cortes_hidro[3], linetype = "dashed", color = "black", size = 0.8) +
  geom_hline(yintercept = cortes_hidro[4], linetype = "dashed", color = "black", size = 0.8) +
  
  # 4. Textos con las recomendaciones automáticas de Dosis (CCTon)
  annotate("text", x = centro_x, y = mean(c(cortes_hidro[1], cortes_hidro[2])), 
           label = paste(nombres_zonas[1], "• APLICAR", round(recs_graf$Dosis_Optima_ccTon[1], 1), "cc/ton"), 
           color = "white", fontface = "bold", size = 4) +
  annotate("text", x = centro_x, y = mean(c(cortes_hidro[2], cortes_hidro[3])), 
           label = paste(nombres_zonas[2], "• APLICAR", round(recs_graf$Dosis_Optima_ccTon[2], 1), "cc/ton"), 
           color = "white", fontface = "bold", size = 4) +
  annotate("text", x = centro_x, y = mean(c(cortes_hidro[3], cortes_hidro[4])), 
           label = paste(nombres_zonas[3], "• APLICAR", round(recs_graf$Dosis_Optima_ccTon[3], 1), "cc/ton"), 
           color = "white", fontface = "bold", size = 4) +
  annotate("text", x = centro_x, y = mean(c(cortes_hidro[4], max(grid_hidro))), 
           label = paste(nombres_zonas[4], "• APLICAR", round(recs_graf$Dosis_Optima_ccTon[4], 1), "cc/ton"), 
           color = "white", fontface = "bold", size = 4) +
  
  # 5. Estética y títulos
  labs(title = paste("Zonas Operativas Auto-Detectadas - Modelo:", mejor_mv),
       subtitle = "Validación de Dosis Óptima (Eje X) según franja de Humedad (Eje Y)",
       x = "Dosis Aplicada (CCTon)", 
       y = "Humedad (Hidro_100)") +
  theme_minimal() +
  theme(legend.position = "right")

