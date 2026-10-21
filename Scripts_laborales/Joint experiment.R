# ==============================================================================
# SCRIPT GENÉRICO DE SELECCIÓN MULTICRITERIO CON CLUSTERING
# ==============================================================================
# - Agrupamiento jerárquico (Ward) de tratamientos según medias de respuesta
# - Índice de desempeño (min-max) con ponderación por:
#     * Variables "mayor es mejor"
#     * Variables "menor es mejor"
#     * Estabilidad (promedio de CVs, temporal si se indica col_tiempo)
# - Determinación automática del número óptimo de clústeres (con gráficos)
# - Reordenamiento de clusters por índice promedio (Cluster 1 = mejor)
# - Comparación contra testigo mediante modelos mixtos (efectos aleatorios:
#   vars_aleatorias y opcionalmente col_tiempo) o ANOVA, con α = 0.15
# - Dendrograma con testigo resaltado
# - Boxplots de las variables originales por clúster
# - Gráfico de burbujas (tamaño = diferencia absoluta vs testigo)
# - Reporte de tratamientos élite (Cluster 1 y que superan al testigo)
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. CARGAR LIBRERÍAS
# ------------------------------------------------------------------------------
paquetes <- c("dplyr", "tidyr", "cluster", "factoextra", "ggplot2", "ggrepel", 
              "dendextend", "ggtext", "lme4", "emmeans", "multcomp", "gridExtra")
nuevos_paquetes <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]
if(length(nuevos_paquetes)) install.packages(nuevos_paquetes)

library(dplyr)
library(tidyr)
library(cluster)
library(factoextra)
library(ggplot2)
library(ggrepel)
library(dendextend)
library(ggtext)
library(lme4)
library(emmeans)
library(multcomp)
library(gridExtra)

# ------------------------------------------------------------------------------
# 1. PARÁMETROS DE ENTRADA (EL USUARIO DEBE MODIFICAR ESTOS)
# ------------------------------------------------------------------------------
var_tratamiento <- "PRODUCTO"        # Nombre de la columna con los tratamientos
nombre_testigo  <- "TESTIGO"         # Valor exacto que identifica al testigo

# Variables de respuesta según su naturaleza
vars_mayor_es_mejor <- c()           # Ej: c("Rendimiento", "Peso")
vars_menor_es_mejor <- c("total.vivos")  # Ej: c("Plagas", "Enfermedad")
variables_respuesta <- c(vars_mayor_es_mejor, vars_menor_es_mejor)

# Variables aleatorias para Modelos Mixtos (Fincas, Fechas, Ensayos, Repeticiones)
# Déjalo vacío c() si es un diseño simple sin bloques
vars_aleatorias <- c() # Ej: c("Finca", "Fecha") 

# COLUMNA DE TIEMPO (OPCIONAL): si tus datos tienen información temporal (años, zafras, etc.)
# indícala aquí para usarla en el cálculo de estabilidad temporal y como efecto aleatorio.
# Si no hay, déjalo como NULL.
col_tiempo <- NULL   # Ej: "ZAFRA" o "Año"

alpha_superacion <- 0.15   # Nivel de significancia para superar al testigo

# Parámetros de clustering (opcionales)
k_optimo_usuario <- 3      # Si es 0, se calcula automáticamente (por silueta)
k_max <- 6                 # Máximo número de clústeres a evaluar
metodo_distancia <- "euclidean"   # "euclidean", "manhattan", etc.
metodo_cluster <- "ward.D2"       # "ward.D2", "complete", "average", etc.

# ------------------------------------------------------------------------------
# 2. VALIDACIONES PREVIAS
# ------------------------------------------------------------------------------
if (!(nombre_testigo %in% unique(datos[[var_tratamiento]]))) {
  stop(paste("El testigo", nombre_testigo, "no se encuentra en la columna", var_tratamiento))
}

n_trat <- length(unique(datos[[var_tratamiento]]))
if (n_trat < 4) {
  stop("Se necesitan al menos 4 tratamientos para realizar clustering.")
}

# Verificar que las variables de respuesta existen en datos
vars_faltantes <- variables_respuesta[!(variables_respuesta %in% names(datos))]
if (length(vars_faltantes) > 0) {
  stop(paste("Las siguientes variables no están en 'datos':", 
             paste(vars_faltantes, collapse = ", ")))
}

# Si se indicó col_tiempo, verificar que exista
if (!is.null(col_tiempo) && !(col_tiempo %in% names(datos))) {
  stop(paste("La columna", col_tiempo, "no se encuentra en 'datos'."))
}

# ------------------------------------------------------------------------------
# 3. ESTADÍSTICA DESCRIPTIVA Y MATRIZ DE CLUSTER
# ------------------------------------------------------------------------------
resumen_datos <- datos %>%
  group_by(!!sym(var_tratamiento)) %>%
  summarise(across(all_of(variables_respuesta), 
                   list(media = ~mean(., na.rm = TRUE),
                        cv = ~sd(., na.rm = TRUE) / mean(., na.rm = TRUE) * 100),
                   .names = "{.col}_{.fn}")) %>%
  ungroup()

# Si hay col_tiempo, calculamos también CV temporal (a través del tiempo)
if (!is.null(col_tiempo)) {
  estabilidad_temporal <- datos %>%
    group_by(!!sym(var_tratamiento), !!sym(col_tiempo)) %>%
    summarise(across(all_of(variables_respuesta), ~mean(., na.rm = TRUE)),
              .groups = "drop") %>%
    group_by(!!sym(var_tratamiento)) %>%
    summarise(across(all_of(variables_respuesta),
                     list(cv_temporal = ~sd(., na.rm = TRUE) / mean(., na.rm = TRUE) * 100),
                     .names = "{.col}_cv_temp")) %>%
    ungroup()
  
  # Unir al resumen (sobreescribimos el CV normal con el temporal o añadimos ambos)
  resumen_datos <- resumen_datos %>%
    left_join(estabilidad_temporal, by = var_tratamiento)
  
  # Usaremos las columnas "_cv_temp" para el índice de estabilidad
  cols_cv <- paste0(variables_respuesta, "_cv_temp")
  cat("Usando CV temporal (a través de", col_tiempo, ") para el índice de estabilidad.\n")
} else {
  # Si no hay col_tiempo, usamos el CV entre repeticiones
  cols_cv <- paste0(variables_respuesta, "_cv")
  cat("Usando CV entre repeticiones para el índice de estabilidad.\n")
}

# Preparar matriz para clustering (solo columnas numéricas de medias)
cols_medias <- paste0(variables_respuesta, "_media")
datos_cluster <- resumen_datos[, cols_medias, drop = FALSE]
rownames(datos_cluster) <- resumen_datos[[var_tratamiento]]

# ------------------------------------------------------------------------------
# 4. ÍNDICE MULTICRITERIO MIN-MAX (0 a 1)
# ------------------------------------------------------------------------------
resumen_datos$Indice_Desempeno <- 0

# Número de componentes: variables respuesta + 1 (estabilidad)
n_componentes <- length(variables_respuesta) + 1
peso_por_var <- 1 / n_componentes

# 4.1 Variables "Mayor es Mejor"
for(v in vars_mayor_es_mejor) {
  col <- paste0(v, "_media")
  min_v <- min(resumen_datos[[col]], na.rm = TRUE)
  max_v <- max(resumen_datos[[col]], na.rm = TRUE)
  if (max_v > min_v) {
    score <- (resumen_datos[[col]] - min_v) / (max_v - min_v)
  } else {
    score <- 0
  }
  resumen_datos$Indice_Desempeno <- resumen_datos$Indice_Desempeno + (score * peso_por_var)
}

# 4.2 Variables "Menor es Mejor" (Invertidas)
for(v in vars_menor_es_mejor) {
  col <- paste0(v, "_media")
  min_v <- min(resumen_datos[[col]], na.rm = TRUE)
  max_v <- max(resumen_datos[[col]], na.rm = TRUE)
  if (max_v > min_v) {
    score <- 1 - ((resumen_datos[[col]] - min_v) / (max_v - min_v))
  } else {
    score <- 0
  }
  resumen_datos$Indice_Desempeno <- resumen_datos$Indice_Desempeno + (score * peso_por_var)
}

# 4.3 Estabilidad (Menor Promedio de CVs es Mejor)
# Verificar que las columnas de CV existan (si no, se omite)
cols_cv_existentes <- cols_cv[cols_cv %in% names(resumen_datos)]
if (length(cols_cv_existentes) > 0) {
  promedio_cv <- rowMeans(resumen_datos[, cols_cv_existentes, drop = FALSE], na.rm = TRUE)
  min_cv <- min(promedio_cv, na.rm = TRUE)
  max_cv <- max(promedio_cv, na.rm = TRUE)
  if (max_cv > min_cv) {
    score_cv <- 1 - ((promedio_cv - min_cv) / (max_cv - min_cv))
  } else {
    score_cv <- 0
  }
  resumen_datos$Indice_Desempeno <- resumen_datos$Indice_Desempeno + (score_cv * peso_por_var)
} else {
  warning("No se encontraron columnas de CV. La estabilidad no se incluirá en el índice.")
}

# ------------------------------------------------------------------------------
# 5. ANÁLISIS CLUSTER CON DETERMINACIÓN DEL NÚMERO ÓPTIMO (GRÁFICOS)
# ------------------------------------------------------------------------------
datos_escalados <- scale(datos_cluster)
matriz_distancia <- dist(datos_escalados, method = metodo_distancia)

# Si el usuario no especificó k, calculamos automáticamente con varios métodos
if (k_optimo_usuario == 0) {
  cat("\n=== Determinando número óptimo de clústeres ===\n")
  
  # Usamos kmeans para la evaluación (más estable que hcut)
  p_sil <- fviz_nbclust(datos_escalados, FUN = kmeans, method = "silhouette", 
                        k.max = min(k_max, n_trat - 1)) +
    ggtitle("Método de la Silueta (kmeans)")
  
  p_wss <- fviz_nbclust(datos_escalados, FUN = kmeans, method = "wss", 
                        k.max = min(k_max, n_trat - 1)) +
    ggtitle("Método del Codo (WSS)")
  
  p_gap <- fviz_nbclust(datos_escalados, FUN = kmeans, method = "gap_stat", 
                        k.max = min(k_max, n_trat - 1), nboot = 50) +
    ggtitle("Estadística Gap")
  
  # Mostrar los tres gráficos juntos
  grid.arrange(p_sil, p_wss, p_gap, ncol = 3,
               top = "Métodos para determinar el número óptimo de clústeres")
  
  # Extraer el valor óptimo según silueta (criterio por defecto)
  datos_sil <- p_sil$data
  k_optimo <- as.numeric(datos_sil[which.max(datos_sil$y), "clusters"])
  if (is.na(k_optimo) || k_optimo < 2) k_optimo <- 3
} else {
  k_optimo <- k_optimo_usuario
}

cat("\n*** Número óptimo de clústeres: k =", k_optimo, "***\n")

# Clustering jerárquico (usando el método elegido)
hc <- hclust(matriz_distancia, method = metodo_cluster)
grupos_temp <- cutree(hc, k = k_optimo)
resumen_datos$Cluster_Original <- as.factor(grupos_temp)

# REORDENAMIENTO: El Cluster 1 siempre será el de mejor índice promedio
orden_clusters <- resumen_datos %>%
  dplyr::group_by(Cluster_Original) %>%
  dplyr::summarise(Promedio_Idx = mean(Indice_Desempeno, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(desc(Promedio_Idx)) %>%
  dplyr::mutate(Cluster = 1:dplyr::n()) # Usamos 1:n() para mayor robustez

# Limpiamos si por error ya existía una columna 'Cluster' de una ejecución anterior
if("Cluster" %in% names(resumen_datos)) {
  resumen_datos <- resumen_datos %>% dplyr::select(-Cluster)
}

resumen_datos <- resumen_datos %>%
  dplyr::left_join(orden_clusters %>% dplyr::select(Cluster_Original, Cluster), by = "Cluster_Original") %>%
  dplyr::mutate(Cluster = as.factor(Cluster)) %>%
  dplyr::select(-Cluster_Original)

# Integrar cluster a datos crudos (usamos merge para evitar conflictos)
datos <- merge(datos, resumen_datos[, c(var_tratamiento, "Cluster")], by = var_tratamiento, all.x = TRUE)

# ------------------------------------------------------------------------------
# 5b. BOXPLOTS DE LAS VARIABLES ORIGINALES POR CLÚSTER
# ------------------------------------------------------------------------------
datos_long <- datos %>%
  dplyr::select(!!sym(var_tratamiento), Cluster, all_of(variables_respuesta)) %>%
  pivot_longer(cols = all_of(variables_respuesta), names_to = "Variable", values_to = "Valor")

boxplot_graf <- ggplot(datos_long, aes(x = Cluster, y = Valor, fill = Cluster)) +
  geom_boxplot(outlier.alpha = 0.3, show.legend = FALSE) +
  stat_summary(fun = mean, geom = "point", shape = 18, color = "red", size = 2) +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(x = "Cluster (ordenado por índice)", y = "Valor de la variable") +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
print(boxplot_graf)

# ------------------------------------------------------------------------------
# 6. MODELOS MIXTOS (COMPARACIÓN CONTRA EL TESTIGO α = 0.15)
# ------------------------------------------------------------------------------
cat("\n=== EVALUACIÓN ESTADÍSTICA vs", nombre_testigo, "===\n")

resumen_datos$Supera_Testigo <- "No"
resumen_datos$P_Valor_Testigo <- NA
resumen_datos$Diferencia_Media <- NA

if(length(variables_respuesta) > 0) {
  var_eval <- variables_respuesta[1]
  
  # Construir efectos aleatorios: incluir vars_aleatorias y col_tiempo si existe
  efectos_aleatorios <- vars_aleatorias
  if (!is.null(col_tiempo) && !(col_tiempo %in% efectos_aleatorios)) {
    efectos_aleatorios <- c(efectos_aleatorios, col_tiempo)
  }
  
  if(length(efectos_aleatorios) > 0) {
    efectos_str <- paste(paste0("(1|", efectos_aleatorios, ")"), collapse = " + ")
    form <- as.formula(paste(var_eval, "~", var_tratamiento, "+", efectos_str))
    modelo <- try(lmer(form, data = datos), silent = TRUE)
    if(!inherits(modelo, "try-error")) {
      emm <- emmeans(modelo, specs = as.formula(paste("~", var_tratamiento)))
      contrastes <- as.data.frame(contrast(emm, "trt.vs.ctrl", 
                                           ref = which(levels(as.factor(datos[[var_tratamiento]])) == nombre_testigo)))
    } else {
      cat("  El modelo mixto no convergió. Se usará ANOVA simple.\n")
      modelo <- NULL
    }
  } else {
    # Modelo lineal simple sin efectos aleatorios
    form <- as.formula(paste(var_eval, "~", var_tratamiento))
    modelo <- lm(form, data = datos)
    if (!inherits(modelo, "try-error")) {
      emm <- emmeans(modelo, specs = as.formula(paste("~", var_tratamiento)))
      contrastes <- as.data.frame(contrast(emm, "trt.vs.ctrl", 
                                           ref = which(levels(as.factor(datos[[var_tratamiento]])) == nombre_testigo)))
    } else {
      contrastes <- NULL
    }
  }
  
  if (exists("contrastes") && !is.null(contrastes) && nrow(contrastes) > 0) {
    for(i in 1:nrow(contrastes)) {
      trat_actual <- gsub(paste0(" - ", nombre_testigo), "", contrastes$contrast[i])
      p_val <- contrastes$p.value[i]
      estimacion <- contrastes$estimate[i]
      
      supera <- FALSE
      if (var_eval %in% vars_mayor_es_mejor & estimacion > 0 & p_val < alpha_superacion) supera <- TRUE
      if (var_eval %in% vars_menor_es_mejor & estimacion < 0 & p_val < alpha_superacion) supera <- TRUE
      
      idx <- which(resumen_datos[[var_tratamiento]] == trat_actual)
      if (length(idx) == 1) {
        resumen_datos$P_Valor_Testigo[idx] <- p_val
        resumen_datos$Diferencia_Media[idx] <- estimacion
        if(supera) resumen_datos$Supera_Testigo[idx] <- "Sí"
      }
    }
  } else {
    cat("  No se pudieron obtener contrastes. Verifique los datos.\n")
  }
} else {
  cat("  No hay variables respuesta definidas. Se omite comparación.\n")
}
resumen_datos$Supera_Testigo[resumen_datos[[var_tratamiento]] == nombre_testigo] <- "Es Testigo"

# ------------------------------------------------------------------------------
# 7. VISUALIZACIONES AVANZADAS
# ------------------------------------------------------------------------------

# A. Dendrograma (Testigo en Rojo)
dend <- as.dendrogram(hc)
dend <- set(dend, "labels_cex", 0.8)
labels_colors(dend) <- rep("black", length(labels(dend)))
idx_testigo <- which(labels(dend) == nombre_testigo)
if (length(idx_testigo) > 0) labels_colors(dend)[idx_testigo] <- "red"

par(mar = c(3, 1, 3, 7))
plot(dend, horiz = TRUE, main = paste("Dendrograma de Similitud\n(Testigo en Rojo, k=", k_optimo, ")"))
rect.dendrogram(dend, k = k_optimo, horiz = TRUE, border = "steelblue", lty = 2)

# B. Scatterplot de Desempeño vs Estabilidad (Burbujas)
if (length(variables_respuesta) >= 2) {
  var_x <- paste0(variables_respuesta[1], "_media")
  var_y <- paste0(variables_respuesta[2], "_media")
  y_label <- paste("Promedio:", variables_respuesta[2])
} else if (length(variables_respuesta) == 1) {
  var_x <- paste0(variables_respuesta[1], "_media")
  # Usamos el primer CV disponible (el que se haya calculado)
  cv_col <- grep(paste0(variables_respuesta[1], "_cv"), names(resumen_datos), value = TRUE)[1]
  if (!is.na(cv_col)) {
    var_y <- cv_col
    y_label <- paste("CV de", variables_respuesta[1], "(%)")
  } else {
    var_y <- NULL
    y_label <- NULL
  }
} else {
  var_x <- NULL
  var_y <- NULL
}

if (!is.null(var_x) && !is.null(var_y) && var_y %in% names(resumen_datos)) {
  val_testigo <- resumen_datos[[var_x]][resumen_datos[[var_tratamiento]] == nombre_testigo]
  if (length(val_testigo) == 1) {
    resumen_datos$Diff_Testigo <- abs(resumen_datos[[var_x]] - val_testigo)
  } else {
    resumen_datos$Diff_Testigo <- 1
  }
  
  grafico_burbujas <- ggplot(resumen_datos, aes(x = !!sym(var_x), y = !!sym(var_y))) +
    geom_point(aes(color = Supera_Testigo, size = Diff_Testigo), alpha = 0.8) +
    geom_text_repel(aes(label = !!sym(var_tratamiento)), size = 3.5, max.overlaps = 15) +
    scale_color_manual(values = c("Es Testigo" = "red", "Sí" = "darkgreen", "No" = "gray50")) +
    scale_size_continuous(range = c(2, 8), name = paste("Dif. Absoluta vs", nombre_testigo)) +
    theme_minimal() +
    labs(title = "Análisis de Desempeño Multicriterio",
         subtitle = paste("Basado en agrupamiento (Cluster 1 = mejor). α =", alpha_superacion),
         x = paste("Promedio:", variables_respuesta[1]), 
         y = y_label)
  print(grafico_burbujas)
} else {
  cat("No se pudo generar el gráfico de burbujas: faltan datos.\n")
}

# ------------------------------------------------------------------------------
# 8. REPORTE FINAL DE CANDIDATOS ÉLITE
# ------------------------------------------------------------------------------
elite <- resumen_datos %>%
  filter(Cluster == 1, !!sym(var_tratamiento) != nombre_testigo) %>%
  arrange(desc(Indice_Desempeno))

cat("\n=== TRATAMIENTOS ÉLITE (Cluster 1) ===\n")
if (nrow(elite) > 0) {
  print(elite %>% dplyr::select(!!sym(var_tratamiento), all_of(var_x), Indice_Desempeno, 
                                Supera_Testigo, P_Valor_Testigo, Diferencia_Media))
} else {
  cat("No hay tratamientos en el Cluster 1 además del testigo.\n")
}

cat("\n=== RESUMEN DE SUPERACIÓN AL TESTIGO ===\n")
tabla_supera <- resumen_datos %>%
  dplyr::select(!!sym(var_tratamiento), Cluster, Indice_Desempeno, Supera_Testigo, P_Valor_Testigo) %>%
  arrange(Cluster, desc(Indice_Desempeno))
print(tabla_supera)

# ------------------------------------------------------------------------------
# Fin del script
# ------------------------------------------------------------------------------