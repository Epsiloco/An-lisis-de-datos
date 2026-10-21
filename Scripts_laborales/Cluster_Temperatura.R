

#################




# ==============================================================================
# SCRIPT INTEGRADO: CLUSTERING DE TEMPERATURAS + ANÁLISIS DE IDEOTIPO (MTSI)
# Excluye testigo, dendrograma con colores correctos (verde = mejor, rojo = peor),
# análisis dentro del cluster verde, y heatmap de perfil multi‑atributo.
#
# CORRECCIÓN: los colores del dendrograma (ramas, etiquetas y rectángulos) ahora
# se calculan de forma explícita a partir de Cluster_Orden (ranking por
# desempeño), en vez de depender de la numeración interna de color_branches()/
# order.dendrogram(), que no coincidía con el ranking real.
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. CARGAR LIBRERÍAS
# ------------------------------------------------------------------------------
paquetes <- c("dplyr", "tidyr", "cluster", "factoextra", "ggplot2", "ggrepel", 
              "dendextend", "ggtext", "lme4", "lmerTest", "glmmTMB", "emmeans", 
              "multcomp", "multcompView", "gridExtra", "car", "MASS")
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
library(lmerTest) 
library(glmmTMB) 
library(emmeans)
library(multcomp)
library(multcompView)
library(gridExtra)
library(car)
library(MASS)   # se carga al final para evitar conflicto con select

# ------------------------------------------------------------------------------
# 1. LECTURA DE DATOS Y PARÁMETROS
# ------------------------------------------------------------------------------
datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")
cat("\n--- Variables detectadas ---\n")
print(names(datos))
View(datos)

datos$Humedad.relativa.Inicial<- as.numeric(datos$Humedad.relativa.Inicial)
class(datos$Humedad.relativa.Inicial)

# ==================== PARÁMETROS CONFIGURABLES ====================
var_temperatura <- "Humedad.relativa.Inicial"
nombre_testigo <- "0"          # valor numérico del testigo
vars_mayor_es_mejor <- c("TCH","Rendimiento", "TAH", "Pureza")
vars_menor_es_mejor <- c()
variables_respuesta <- c(vars_mayor_es_mejor, vars_menor_es_mejor)
vars_aleatorias <- c("Replica")   # ajustar según diseño (si no existe, dejar c())
distribucion_modelo <- "gaussian"
alpha_superacion <- 0.15
k_optimo_usuario <- 2
k_max <- 6
metodo_distancia <- "euclidean"
metodo_cluster <- "ward.D2"
generar_graficos <- TRUE

# Variables para el análisis de ideotipo (pueden ser las mismas o un subconjunto)
vars_mtsi <- variables_respuesta   # o c("TCH", "Rendimiento", "TAH", "Pureza", "Pol")

# ==================== VALIDACIONES ====================
if (!(var_temperatura %in% names(datos))) stop("La variable de temperatura no existe en los datos")
if (length(variables_respuesta) == 0) stop("No hay variables de respuesta definidas")
testigo_num <- as.numeric(as.character(nombre_testigo))
if (is.na(testigo_num)) stop("El testigo debe ser un valor numérico")

# ------------------------------------------------------------------------------
# 2. EXCLUIR TESTIGO Y CLUSTERING
# ------------------------------------------------------------------------------
cat("\n===== PASO 1: EXCLUIR TESTIGO Y CLUSTERING =====\n")
datos_sin_testigo <- datos %>% filter(!!sym(var_temperatura) != testigo_num)
if (nrow(datos_sin_testigo) == 0) stop("No hay datos sin el testigo.")

resumen_temp <- datos_sin_testigo %>%
  group_by(!!sym(var_temperatura)) %>%
  summarise(across(all_of(variables_respuesta), 
                   list(media = ~mean(., na.rm = TRUE),
                        cv = ~sd(., na.rm = TRUE) / mean(., na.rm = TRUE) * 100),
                   .names = "{.col}_{.fn}")) %>%
  ungroup()
resumen_temp <- resumen_temp %>% mutate(across(ends_with("_cv"), ~ifelse(is.nan(.), 0, .)))

# Índice de desempeño (mayor es mejor, penaliza CV alto)
resumen_temp$Indice_Desempeno <- 0
peso_por_var <- 1 / (length(variables_respuesta) + 1)
for(v in vars_mayor_es_mejor) {
  col <- paste0(v, "_media")
  min_v <- min(resumen_temp[[col]], na.rm = TRUE); max_v <- max(resumen_temp[[col]], na.rm = TRUE)
  score <- ifelse(max_v > min_v, (resumen_temp[[col]] - min_v) / (max_v - min_v), 0)
  resumen_temp$Indice_Desempeno <- resumen_temp$Indice_Desempeno + (score * peso_por_var)
}
for(v in vars_menor_es_mejor) {
  col <- paste0(v, "_media")
  min_v <- min(resumen_temp[[col]], na.rm = TRUE); max_v <- max(resumen_temp[[col]], na.rm = TRUE)
  score <- ifelse(max_v > min_v, 1 - ((resumen_temp[[col]] - min_v) / (max_v - min_v)), 0)
  resumen_temp$Indice_Desempeno <- resumen_temp$Indice_Desempeno + (score * peso_por_var)
}
cols_cv <- paste0(variables_respuesta, "_cv")
promedio_cv <- rowMeans(resumen_temp[, cols_cv, drop = FALSE], na.rm = TRUE)
min_cv <- min(promedio_cv, na.rm = TRUE); max_cv <- max(promedio_cv, na.rm = TRUE)
score_cv <- ifelse(max_cv > min_cv, 1 - ((promedio_cv - min_cv) / (max_cv - min_cv)), 0)
resumen_temp$Indice_Desempeno <- resumen_temp$Indice_Desempeno + (score_cv * peso_por_var)

# Matriz para clustering
datos_cluster_temp <- resumen_temp[, paste0(variables_respuesta, "_media"), drop = FALSE]
rownames(datos_cluster_temp) <- resumen_temp[[var_temperatura]]
datos_escalados_temp <- scale(datos_cluster_temp)
matriz_dist_temp <- dist(datos_escalados_temp, method = metodo_distancia)

# Número de clusters
if(k_optimo_usuario == 0) {
  if(nrow(datos_escalados_temp) < 3) {
    k_optimo <- 1
  } else {
    sil <- fviz_nbclust(datos_escalados_temp, FUN = kmeans, method = "silhouette",
                        k.max = min(k_max, nrow(datos_escalados_temp)-1))
    sil_data <- sil$data
    if(!is.null(sil_data) && nrow(sil_data) > 0) {
      max_sil <- which.max(sil_data$y)
      k_optimo <- as.numeric(sil_data[max_sil, "clusters"])
    } else {
      k_optimo <- 2
    }
    if(is.na(k_optimo) || k_optimo < 2) k_optimo <- 2
    print(sil)
  }
} else {
  k_optimo <- k_optimo_usuario
}
k_optimo <- min(k_optimo, nrow(datos_escalados_temp))

hc_temp <- hclust(matriz_dist_temp, method = metodo_cluster)
resumen_temp$Cluster_Original <- as.factor(cutree(hc_temp, k = k_optimo))
orden_clusters_temp <- resumen_temp %>%
  group_by(Cluster_Original) %>%
  summarise(Promedio_Idx = mean(Indice_Desempeno, na.rm = TRUE)) %>%
  arrange(desc(Promedio_Idx)) %>%
  mutate(Cluster_Orden = as.factor(1:n()))
resumen_temp <- resumen_temp %>%
  left_join(orden_clusters_temp, by = "Cluster_Original") %>%
  dplyr::select(-one_of("Cluster_Original"))

paleta_base <- c("#4DAF4A", "#377EB8", "#E41A1C")
colores_clusters <- colorRampPalette(paleta_base)(length(unique(resumen_temp$Cluster_Orden)))
names(colores_clusters) <- levels(resumen_temp$Cluster_Orden)

# ------------------------------------------------------------------------------
# 3. DENDROGRAMA CON COLORES CORRECTOS (VERDE ARRIBA)
# ------------------------------------------------------------------------------
cat("\n=== GENERANDO DENDROGRAMA CON COLORES CORRECTOS ===\n")

# Crear dendrograma base
dend <- as.dendrogram(hc_temp)

# Reordenar las hojas PRIMERO (mejor cluster al final => queda arriba en horizontal)
# Ordenamos de peor a mejor: primero cluster más alto (peor), luego el 1 (mejor) al final
orden_hojas <- resumen_temp %>%
  mutate(Temp_char = as.character(!!sym(var_temperatura))) %>%
  arrange(desc(Cluster_Orden), desc(Indice_Desempeno)) %>%
  pull(Temp_char)


dend_ordenado <- rotate(dend, orden_hojas)

# --- CORRECCIÓN CLAVE ---
# color_branches(dend, k=k, col=colores_clusters) numera los clusters según el
# orden en que aparecen dentro del árbol (izquierda a derecha), NO según
# Cluster_Orden (que numera por desempeño: 1=mejor/verde ... k=peor/rojo).
# Por eso los colores no correspondían con los grupos reales.
#
# La solución es calcular, para cada hoja, su Cluster_Orden real, EN EL MISMO
# ORDEN en que las hojas ya quedaron dispuestas en el dendrograma rotado
# (labels(dend_ordenado)), y pasar ese vector explícitamente a color_branches()
# mediante el argumento `clusters`, en vez de dejar que lo calcule por su cuenta.
etiquetas_dend <- labels(dend_ordenado)
cluster_orden_hoja <- resumen_temp$Cluster_Orden[match(etiquetas_dend, resumen_temp[[var_temperatura]])]
clusters_para_color <- as.integer(as.character(cluster_orden_hoja))

# Colorear las ramas usando esos clusters explícitos -> ahora sí coinciden 100%
# con Cluster_Orden (1 = verde/mejor desempeño ... k = rojo/peor desempeño)
dend_ordenado <- color_branches(dend_ordenado, clusters = clusters_para_color,
                                col = colores_clusters[as.character(sort(unique(clusters_para_color)))])

# Colorear también las etiquetas de las hojas con el mismo criterio
labels_colors(dend_ordenado) <- colores_clusters[as.character(clusters_para_color)]

# Graficar (mejores arriba en horizontal)
par(mar = c(3,1,3,7))
plot(dend_ordenado, horiz = TRUE, main = "Dendrograma de Temperaturas (sin testigo)\nVerde = mejor desempeño")

# Dibujar rectángulos con los colores correctos, en el orden en que aparecen
# visualmente en el dendrograma ya rotado (clusters_para_color ya está alineado
# a ese orden, así que no hace falta -ni conviene- volver a reindexar con
# order.dendrogram(), que usa un sistema de índices distinto y desordenaba los colores)
colores_rect <- colores_clusters[as.character(unique(clusters_para_color))]
rect.dendrogram(dend_ordenado, k = k_optimo, horiz = TRUE, 
                border = colores_rect, lty = 2, lwd = 2)
Sys.sleep(2)

cat("\n--- Resumen de temperaturas ordenadas por desempeño ---\n")
print(resumen_temp %>% arrange(desc(Indice_Desempeno)))

# ------------------------------------------------------------------------------
# 4. SELECCIÓN DEL MEJOR CLUSTER (VERDE)
# ------------------------------------------------------------------------------
mejor_cluster <- orden_clusters_temp$Cluster_Original[1]
temperaturas_verde <- resumen_temp[[var_temperatura]][resumen_temp$Cluster_Orden == 1]
cat("\n===== MEJOR CLUSTER (Verde) =====\n")
cat("Temperaturas en el cluster de mejor desempeño:\n")
print(temperaturas_verde)

if (length(temperaturas_verde) < 2) {
  stop("El mejor cluster tiene menos de 2 temperaturas. No se puede hacer MANOVA.")
}
rango_verde <- range(as.numeric(as.character(temperaturas_verde)))
cat("Rango de temperaturas en el cluster verde:", rango_verde[1], "-", rango_verde[2], "\n")
datos_verde <- datos %>% filter(!!sym(var_temperatura) %in% temperaturas_verde)

# ------------------------------------------------------------------------------
# 5. ANÁLISIS DENTRO DEL CLUSTER VERDE: ANOVAS Y MANOVA
# ------------------------------------------------------------------------------
cat("\n===== PASO 2: ANÁLISIS DENTRO DEL CLUSTER VERDE =====\n")

# 5.1 ANOVAs univariados (temperatura como factor)
cat("\n--- ANOVAs univariados (factor Temperatura) ---\n")
resultados_anova_verde <- list()
for (v in variables_respuesta) {
  datos_var <- datos_verde %>% filter(!is.na(!!sym(v)))
  if (nrow(datos_var) < 2) {
    cat("  Variable", v, ": insuficientes datos. Omitida.\n")
    next
  }
  modelo <- lm(as.formula(paste(v, "~ as.factor(", var_temperatura, ")")), data = datos_var)
  anova_res <- anova(modelo)
  cat("\n--- Variable:", v, "---\n")
  print(anova_res)
  resultados_anova_verde[[v]] <- anova_res
}

# 5.2 MANOVA (temperatura como factor)
cat("\n--- MANOVA (temperaturas del cluster verde) ---\n")
datos_manova_verde <- datos_verde %>%
  dplyr::select(any_of(c(variables_respuesta, var_temperatura))) %>%
  na.omit()
n_completos <- nrow(datos_manova_verde)
n_vars <- length(variables_respuesta)
niveles_temp <- unique(datos_manova_verde[[var_temperatura]])
n_niveles <- length(niveles_temp)

cat("  Filas completas:", n_completos, "\n")
cat("  Niveles de temperatura en el cluster:", paste(niveles_temp, collapse = ", "), "\n")
cat("  Número de niveles:", n_niveles, "\n")

if (n_completos > n_vars && n_niveles >= 2) {
  manova_model <- manova(as.matrix(datos_manova_verde[, variables_respuesta]) ~ 
                           as.factor(datos_manova_verde[[var_temperatura]]), 
                         data = datos_manova_verde)
  cat("\n--- Resultados MANOVA (Wilks) ---\n")
  print(summary(manova_model, test = "Wilks"))
  cat("\n--- Otros tests ---\n")
  print(summary(manova_model, test = "Pillai"))
  print(summary(manova_model, test = "Hotelling-Lawley"))
  print(summary(manova_model, test = "Roy"))
  
  # Comparaciones post-hoc (Tukey) para cada variable
  cat("\n--- Comparaciones post-hoc (Tukey) para cada variable ---\n")
  for (v in variables_respuesta) {
    datos_var <- datos_verde %>% filter(!is.na(!!sym(v)))
    if (nrow(datos_var) < 2) next
    modelo_aov <- aov(as.formula(paste(v, "~ as.factor(", var_temperatura, ")")), data = datos_var)
    tukey <- TukeyHSD(modelo_aov, ordered = TRUE)
    cat("\nVariable:", v, "\n")
    print(tukey)
  }
} else {
  cat("\nNo se puede realizar MANOVA porque:\n")
  if (n_completos <= n_vars) cat("  - Filas completas (", n_completos, ") <= número de variables (", n_vars, ").\n")
  if (n_niveles < 2) cat("  - Solo hay", n_niveles, "nivel(es) de temperatura.\n")
  cat("  Se recomienda usar ANOVAs univariados (ya mostrados).\n")
}

# 5.3 Ranking de estabilidad dentro del cluster verde
cat("\n--- Temperaturas más equilibradas dentro del cluster verde ---\n")
ranking_verde <- resumen_temp %>%
  filter(!!sym(var_temperatura) %in% temperaturas_verde) %>%
  arrange(desc(Indice_Desempeno)) %>%
  dplyr::select(!!sym(var_temperatura), Indice_Desempeno, 
                ends_with("_media"), ends_with("_cv"))
print(ranking_verde)
mejor_temp <- ranking_verde %>% slice(1) %>% pull(!!sym(var_temperatura))
cat("\nTemperatura con mejor equilibrio (mayor índice):", mejor_temp, "\n")

# ------------------------------------------------------------------------------
# 6. ANÁLISIS DE IDEOTIPO (MTSI) PARA LAS TEMPERATURAS DEL CLUSTER VERDE
# ------------------------------------------------------------------------------
cat("\n===== PASO 3: ANÁLISIS DE IDEOTIPO (MTSI) EN EL CLUSTER VERDE =====\n")

# 6.1 Configuración para MTSI
vars_y <- vars_mtsi   # variables a incluir en el ideotipo
# Todas las direcciones son "max" porque son mayor es mejor
direcciones <- rep("max", length(vars_y))
names(direcciones) <- vars_y

# 6.2 Promediar por temperatura (sin agrupar por Réplica) para obtener una fila por temperatura
df_means <- datos_verde %>%
  group_by(!!sym(var_temperatura)) %>%
  summarise(across(all_of(vars_y), mean, na.rm = TRUE)) %>%
  ungroup() %>%
  drop_na(all_of(vars_y))

# Si no hay datos, mostrar mensaje y continuar
if (nrow(df_means) == 0) {
  cat("No hay datos completos para el análisis MTSI en el cluster verde. Se omite.\n")
} else {
  # Renombrar columna de temperatura como "UE" para compatibilidad
  df_means <- df_means %>% rename(UE = !!sym(var_temperatura))
  
  # 6.3 Estandarización 0-100 (normalización min-max con dirección "max")
  df_norm <- df_means
  df_real <- df_means
  
  for (v in vars_y) {
    dir <- direcciones[v]
    mi <- min(df_norm[[v]], na.rm = TRUE)
    ma <- max(df_norm[[v]], na.rm = TRUE)
    if (ma == mi) {
      df_norm[[v]] <- 50
    } else {
      if (dir == "min") {
        df_norm[[v]] <- ((ma - df_norm[[v]]) / (ma - mi)) * 100
      } else {
        df_norm[[v]] <- ((df_norm[[v]] - mi) / (ma - mi)) * 100
      }
    }
  }
  
  # 6.4 Distancia euclidiana al ideotipo (100)
  df_res <- df_norm %>% dplyr::select(UE)
  df_res$Distancia_MTSI <- 0
  ideotipo_perfecto <- 100
  for (v in vars_y) {
    df_res$Distancia_MTSI <- df_res$Distancia_MTSI + (df_norm[[v]] - ideotipo_perfecto)^2
  }
  df_res$Distancia_MTSI <- sqrt(df_res$Distancia_MTSI)
  
  # 6.5 Score de similitud (0-100) y ranking
  max_dist <- max(df_res$Distancia_MTSI, na.rm = TRUE)
  min_dist <- min(df_res$Distancia_MTSI, na.rm = TRUE)
  if (max_dist == min_dist) {
    df_res$Score_Similitud <- 50
  } else {
    df_res$Score_Similitud <- ((max_dist - df_res$Distancia_MTSI) / (max_dist - min_dist)) * 100
  }
  df_res <- df_res %>%
    arrange(Distancia_MTSI) %>%
    mutate(Ranking_Global = row_number())
  
  # Unir con valores reales
  df_final <- df_res %>%
    left_join(df_real, by = "UE")
  
  # 6.6 Preparar datos para el mapa de calor
  df_melt_norm <- df_norm %>%
    pivot_longer(cols = all_of(vars_y), names_to = "Variable", values_to = "Puntuacion_0_100")
  df_melt_real <- df_real %>%
    pivot_longer(cols = all_of(vars_y), names_to = "Variable", values_to = "Valor_Real")
  df_heatmap <- left_join(df_melt_norm, df_melt_real, by = c("UE", "Variable"))
  # Ordenar las UE (temperaturas) según el ranking (mejor arriba en el heatmap)
  df_heatmap$UE <- factor(df_heatmap$UE, levels = rev(df_final$UE))
  
  # 6.7 Mapa de calor
  p_heat <- ggplot(df_heatmap, aes(x = Variable, y = UE, fill = Puntuacion_0_100)) +
    geom_tile(color = "white", linewidth = 0.5) +
    scale_fill_gradient2(low = "#e74c3c", mid = "#f1c40f", high = "#27ae60", 
                         midpoint = 50, name = "Aptitud\n(100=Ideal)") +
    geom_text(aes(label = round(Valor_Real, 2)), color = "black", size = 3.5, fontface = "bold") +
    theme_minimal(base_size = 14) +
    labs(
      x = "Variables Evaluadas", 
      y = "Temperatura (°C) - Ordenadas de mejor a peor según MTSI", 
      title = "Perfil Multi‑Atributo (MTSI) para el Cluster Verde",
      subtitle = "Distancia al Ideotipo y Score de Similitud"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
      axis.text.y = element_text(face = "bold", size = 8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  print(p_heat)
  
  # Mostrar tabla de resultados MTSI
  cat("\n--- Resultados del análisis de ideotipo (MTSI) para el cluster verde ---\n")
  print(df_final)
}

# ------------------------------------------------------------------------------
# 7. COMPARACIÓN CON EL TESTIGO
# ------------------------------------------------------------------------------
cat("\n===== PASO 4: COMPARACIÓN CON EL TESTIGO (", testigo_num, ") =====\n")
datos_testigo <- datos %>% filter(!!sym(var_temperatura) == testigo_num)

if (nrow(datos_testigo) > 0) {
  medias_testigo <- datos_testigo %>%
    summarise(across(all_of(variables_respuesta), mean, na.rm = TRUE))
  cat("Medias del testigo:\n")
  print(medias_testigo)
  
  cat("\n--- Comparación de cada temperatura del cluster vs testigo (ANOVA con contrastes) ---\n")
  datos_comp <- datos %>% filter(!!sym(var_temperatura) %in% c(temperaturas_verde, testigo_num))
  
  for (v in variables_respuesta) {
    datos_var <- datos_comp %>% filter(!is.na(!!sym(v)))
    if (nrow(datos_var) < 2) next
    modelo <- lm(as.formula(paste(v, "~ as.factor(", var_temperatura, ")")), data = datos_var)
    emm <- emmeans(modelo, specs = ~ as.factor(get(var_temperatura)))
    niveles <- levels(as.factor(datos_var[[var_temperatura]]))
    idx_testigo <- which(niveles == as.character(testigo_num))
    if (length(idx_testigo) == 1) {
      cont <- contrast(emm, "trt.vs.ctrl", ref = idx_testigo)
      cat("\nVariable:", v, "\n")
      print(cont)
    } else {
      cat("\nVariable:", v, " - testigo no encontrado en los niveles.\n")
    }
  }
} else {
  cat("No hay datos del testigo en el conjunto de datos.\n")
}

# ------------------------------------------------------------------------------
# 8. GRÁFICOS ADICIONALES DE PERFIL Y CV (cluster verde)
# ------------------------------------------------------------------------------
if (generar_graficos) {
  # Perfil de medias
  medias_verde <- datos_verde %>%
    group_by(!!sym(var_temperatura)) %>%
    summarise(across(all_of(variables_respuesta), mean, na.rm = TRUE)) %>%
    pivot_longer(cols = all_of(variables_respuesta), names_to = "Variable", values_to = "Media")
  
  p_perfil <- ggplot(medias_verde, aes(x = as.factor(!!sym(var_temperatura)), y = Media, 
                                       group = Variable, color = Variable)) +
    geom_line(size = 1.2) + geom_point(size = 3) +
    labs(title = "Perfil de medias en el cluster verde",
         x = "Temperatura", y = "Media") +
    theme_bw()
  print(p_perfil)
  
  # Coeficiente de variación
  cv_verde <- resumen_temp %>%
    filter(!!sym(var_temperatura) %in% temperaturas_verde) %>%
    dplyr::select(!!sym(var_temperatura), Indice_Desempeno, ends_with("_cv")) %>%
    pivot_longer(cols = ends_with("_cv"), names_to = "Variable", values_to = "CV") %>%
    mutate(Variable = gsub("_cv", "", Variable))
  
  p_cv <- ggplot(cv_verde, aes(x = as.factor(!!sym(var_temperatura)), y = CV, fill = Variable)) +
    geom_col(position = "dodge") +
    labs(title = "Coeficiente de variación por temperatura (cluster verde)",
         x = "Temperatura", y = "CV (%)") +
    theme_bw()
  print(p_cv)
}

# ------------------------------------------------------------------------------
# 9. REPORTE FINAL
# ------------------------------------------------------------------------------
cat("\n===== RESUMEN FINAL =====\n")

cat("Temperaturas del cluster verde (mejor desempeño):", 
    paste(temperaturas_verde, collapse = ", "), "\n")
cat("Rango del cluster verde:", rango_verde[1], "-", rango_verde[2], "\n")
cat("Mejor temperatura según índice de desempeño:", mejor_temp, "\n")
if (exists("df_final") && nrow(df_final) > 0) {
  cat("Mejor temperatura según MTSI (menor distancia al ideotipo):", 
      df_final$UE[which.min(df_final$Distancia_MTSI)], "\n")
}
cat("Variables analizadas:", paste(variables_respuesta, collapse = ", "), "\n")
cat("Testigo excluido del clustering:", testigo_num, "\n")
cat("\n¡Análisis completado!\n")




######################## Humedad

# ==============================================================================
# SCRIPT INTEGRADO: CLUSTERING DE TEMPERATURAS + ANÁLISIS DE IDEOTIPO (MTSI)
# Excluye testigo, dendrograma con colores correctos (verde = mejor, rojo = peor),
# análisis dentro del cluster verde, y heatmap de perfil multi‑atributo.
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. CARGAR LIBRERÍAS
# ------------------------------------------------------------------------------
paquetes <- c("dplyr", "tidyr", "cluster", "factoextra", "ggplot2", "ggrepel", 
              "dendextend", "ggtext", "lme4", "lmerTest", "glmmTMB", "emmeans", 
              "multcomp", "multcompView", "gridExtra", "car", "MASS")
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
library(lmerTest) 
library(glmmTMB) 
library(emmeans)
library(multcomp)
library(multcompView)
library(gridExtra)
library(car)
library(MASS)   # se carga al final para evitar conflicto con select

# ------------------------------------------------------------------------------
# 1. LECTURA DE DATOS Y PARÁMETROS
# ------------------------------------------------------------------------------
datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")
cat("\n--- Variables detectadas ---\n")
print(names(datos))
View(datos)

datos$Humedad.relativa.Inicial <- as.numeric(datos$Humedad.relativa.Inicial)
class(datos$Humedad.relativa.Inicial)

# ==================== PARÁMETROS CONFIGURABLES ====================
var_temperatura <- "Humedad.relativa.Inicial"
nombre_testigo <- "0"          # valor numérico del testigo
vars_mayor_es_mejor <- c("TCH","Rendimiento", "TAH", "Pureza")
vars_menor_es_mejor <- c()
variables_respuesta <- c(vars_mayor_es_mejor, vars_menor_es_mejor)
vars_aleatorias <- c("Replica")   # ajustar según diseño (si no existe, dejar c())
distribucion_modelo <- "gaussian"
alpha_superacion <- 0.15
k_optimo_usuario <- 3
k_max <- 6
metodo_distancia <- "euclidean"
metodo_cluster <- "ward.D2"
generar_graficos <- TRUE

# Variables para el análisis de ideotipo
vars_mtsi <- variables_respuesta   

# ==================== VALIDACIONES ====================
if (!(var_temperatura %in% names(datos))) stop("La variable de temperatura no existe en los datos")
if (length(variables_respuesta) == 0) stop("No hay variables de respuesta definidas")
testigo_num <- as.numeric(as.character(nombre_testigo))
if (is.na(testigo_num)) stop("El testigo debe ser un valor numérico")

# ------------------------------------------------------------------------------
# 2. EXCLUIR TESTIGO Y CLUSTERING
# ------------------------------------------------------------------------------
cat("\n===== PASO 1: EXCLUIR TESTIGO Y CLUSTERING =====\n")
datos_sin_testigo <- datos %>% filter(!!sym(var_temperatura) != testigo_num)
if (nrow(datos_sin_testigo) == 0) stop("No hay datos sin el testigo.")

resumen_temp <- datos_sin_testigo %>%
  group_by(!!sym(var_temperatura)) %>%
  summarise(across(all_of(variables_respuesta), 
                   list(media = ~mean(., na.rm = TRUE),
                        cv = ~sd(., na.rm = TRUE) / mean(., na.rm = TRUE) * 100),
                   .names = "{.col}_{.fn}")) %>%
  ungroup()
resumen_temp <- resumen_temp %>% mutate(across(ends_with("_cv"), ~ifelse(is.nan(.), 0, .)))

# Índice de desempeño (mayor es mejor, penaliza CV alto)
resumen_temp$Indice_Desempeno <- 0
peso_por_var <- 1 / (length(variables_respuesta) + 1)
for(v in vars_mayor_es_mejor) {
  col <- paste5 <- paste0(v, "_media")
  min_v <- min(resumen_temp[[col]], na.rm = TRUE); max_v <- max(resumen_temp[[col]], na.rm = TRUE)
  score <- ifelse(max_v > min_v, (resumen_temp[[col]] - min_v) / (max_v - min_v), 0)
  resumen_temp$Indice_Desempeno <- resumen_temp$Indice_Desempeno + (score * peso_por_var)
}
for(v in vars_menor_es_mejor) {
  col <- paste0(v, "_media")
  min_v <- min(resumen_temp[[col]], na.rm = TRUE); max_v <- max(resumen_temp[[col]], na.rm = TRUE)
  score <- ifelse(max_v > min_v, 1 - ((resumen_temp[[col]] - min_v) / (max_v - min_v)), 0)
  resumen_temp$Indice_Desempeno <- resumen_temp$Indice_Desempeno + (score * peso_por_var)
}
cols_cv <- paste0(variables_respuesta, "_cv")
promedio_cv <- rowMeans(resumen_temp[, cols_cv, drop = FALSE], na.rm = TRUE)
min_cv <- min(promedio_cv, na.rm = TRUE); max_cv <- max(promedio_cv, na.rm = TRUE)
score_cv <- ifelse(max_cv > min_cv, 1 - ((promedio_cv - min_cv) / (max_cv - min_cv)), 0)
resumen_temp$Indice_Desempeno <- resumen_temp$Indice_Desempeno + (score_cv * peso_por_var)

# Matriz para clustering
datos_cluster_temp <- resumen_temp[, paste0(variables_respuesta, "_media"), drop = FALSE]
rownames(datos_cluster_temp) <- resumen_temp[[var_temperatura]]
datos_escalados_temp <- scale(datos_cluster_temp)
matriz_dist_temp <- dist(datos_escalados_temp, method = metodo_distancia)

# Número de clusters (Cálculo directo seguro sin fviz_nbclust)
if(k_optimo_usuario == 0) {
  if(nrow(datos_escalados_temp) < 3) {
    k_optimo <- 1
  } else {
    max_k_val <- min(k_max, nrow(datos_escalados_temp) - 1)
    if(max_k_val >= 2) {
      sil_widths <- numeric(max_k_val - 1)
      for(k in 2:max_k_val) {
        set.seed(123)
        km <- kmeans(datos_escalados_temp, centers = k, nstart = 25)
        ss <- cluster::silhouette(km$cluster, matriz_dist_temp)
        sil_widths[k - 1] <- mean(ss[, 3])
      }
      k_optimo <- which.max(sil_widths) + 1
    } else {
      k_optimo <- 2
    }
    if(is.na(k_optimo) || k_optimo < 2) k_optimo <- 2
  }
} else {
  k_optimo <- k_optimo_usuario
}
k_optimo <- min(k_optimo, nrow(datos_escalados_temp))

hc_temp <- hclust(matriz_dist_temp, method = metodo_cluster)
resumen_temp$Cluster_Original <- as.factor(cutree(hc_temp, k = k_optimo))
orden_clusters_temp <- resumen_temp %>%
  group_by(Cluster_Original) %>%
  summarise(Promedio_Idx = mean(Indice_Desempeno, na.rm = TRUE)) %>%
  arrange(desc(Promedio_Idx)) %>%
  mutate(Cluster_Orden = as.factor(1:n()))
resumen_temp <- resumen_temp %>%
  left_join(orden_clusters_temp, by = "Cluster_Original") %>%
  dplyr::select(-one_of("Cluster_Original"))

paleta_base <- c("#4DAF4A", "#377EB8", "#E41A1C")
colores_clusters <- colorRampPalette(paleta_base)(length(unique(resumen_temp$Cluster_Orden)))
names(colores_clusters) <- levels(resumen_temp$Cluster_Orden)

# ------------------------------------------------------------------------------
# 3. DENDROGRAMA CON COLORES CORRECTOS (VERDE ARRIBA)
# ------------------------------------------------------------------------------
cat("\n=== GENERANDO DENDROGRAMA CON COLORES CORRECTOS ===\n")

dend <- as.dendrogram(hc_temp)

orden_hojas <- resumen_temp %>%
  mutate(Temp_char = as.character(!!sym(var_temperatura))) %>%
  arrange(desc(Cluster_Orden), desc(Indice_Desempeno)) %>%
  pull(Temp_char)

dend_ordenado <- rotate(dend, orden_hojas)

etiquetas_dend <- labels(dend_ordenado)
cluster_orden_hoja <- resumen_temp$Cluster_Orden[match(etiquetas_dend, resumen_temp[[var_temperatura]])]
clusters_para_color <- as.integer(as.character(cluster_orden_hoja))

dend_ordenado <- color_branches(dend_ordenado, clusters = clusters_para_color,
                                col = colores_clusters[as.character(sort(unique(clusters_para_color)))])

labels_colors(dend_ordenado) <- colores_clusters[as.character(clusters_para_color)]

par(mar = c(3,1,3,7))
plot(dend_ordenado, horiz = TRUE, main = "Dendrograma de Temperaturas (sin testigo)\nVerde = mejor desempeño")

colores_rect <- colores_clusters[as.character(unique(clusters_para_color))]
rect.dendrogram(dend_ordenado, k = k_optimo, horiz = TRUE, 
                border = colores_rect, lty = 2, lwd = 2)
Sys.sleep(2)

cat("\n--- Resumen de temperaturas ordenadas por desempeño ---\n")
print(resumen_temp %>% arrange(desc(Indice_Desempeno)))

# ------------------------------------------------------------------------------
# 4. SELECCIÓN DEL MEJOR CLUSTER (VERDE)
# ------------------------------------------------------------------------------
mejor_cluster <- orden_clusters_temp$Cluster_Original[1]
temperaturas_verde <- resumen_temp[[var_temperatura]][resumen_temp$Cluster_Orden == 1]
cat("\n===== MEJOR CLUSTER (Verde) =====\n")
cat("Temperaturas en el cluster de mejor desempeño:\n")
print(temperaturas_verde)

if (length(temperaturas_verde) < 2) {
  stop("El mejor cluster tiene menos de 2 temperaturas. No se puede hacer MANOVA.")
}
rango_verde <- range(as.numeric(as.character(temperaturas_verde)))
cat("Rango de temperaturas en el cluster verde:", rango_verde[1], "-", rango_verde[2], "\n")
datos_verde <- datos %>% filter(!!sym(var_temperatura) %in% temperaturas_verde)

# ------------------------------------------------------------------------------
# 5. ANÁLISIS DENTRO DEL CLUSTER VERDE: ANOVAS Y MANOVA
# ------------------------------------------------------------------------------
cat("\n===== PASO 2: ANÁLISIS DENTRO DEL CLUSTER VERDE =====\n")

cat("\n--- ANOVAs univariados (factor Temperatura) ---\n")
resultados_anova_verde <- list()
for (v in variables_respuesta) {
  datos_var <- datos_verde %>% filter(!is.na(!!sym(v)))
  if (nrow(datos_var) < 2) {
    cat("  Variable", v, ": insuficientes datos. Omitida.\n")
    next
  }
  modelo <- lm(as.formula(paste(v, "~ as.factor(", var_temperatura, ")")), data = datos_var)
  anova_res <- anova(modelo)
  cat("\n--- Variable:", v, "---\n")
  print(anova_res)
  resultados_anova_verde[[v]] <- anova_res
}

cat("\n--- MANOVA (temperaturas del cluster verde) ---\n")
datos_manova_verde <- datos_verde %>%
  dplyr::select(any_of(c(variables_respuesta, var_temperatura))) %>%
  na.omit()
n_completos <- nrow(datos_manova_verde)
n_vars <- length(variables_respuesta)
niveles_temp <- unique(datos_manova_verde[[var_temperatura]])
n_niveles <- length(niveles_temp)

cat("  Filas completas:", n_completos, "\n")
cat("  Niveles de temperatura en el cluster:", paste(niveles_temp, collapse = ", "), "\n")
cat("  Número de niveles:", n_niveles, "\n")

if (n_completos > n_vars && n_niveles >= 2) {
  manova_model <- manova(as.matrix(datos_manova_verde[, variables_respuesta]) ~ 
                           as.factor(datos_manova_verde[[var_temperatura]]), 
                         data = datos_manova_verde)
  cat("\n--- Resultados MANOVA (Wilks) ---\n")
  print(summary(manova_model, test = "Wilks"))
  cat("\n--- Otros tests ---\n")
  print(summary(manova_model, test = "Pillai"))
  print(summary(manova_model, test = "Hotelling-Lawley"))
  print(summary(manova_model, test = "Roy"))
  
  cat("\n--- Comparaciones post-hoc (Tukey) para cada variable ---\n")
  for (v in variables_respuesta) {
    datos_var <- datos_verde %>% filter(!is.na(!!sym(v)))
    if (nrow(datos_var) < 2) next
    modelo_aov <- aov(as.formula(paste(v, "~ as.factor(", var_temperatura, ")")), data = datos_var)
    tukey <- TukeyHSD(modelo_aov, ordered = TRUE)
    cat("\nVariable:", v, "\n")
    print(tukey)
  }
} else {
  cat("\nNo se puede realizar MANOVA porque:\n")
  if (n_completos <= n_vars) cat("  - Filas completas (", n_completos, ") <= número de variables (", n_vars, ").\n")
  if (n_niveles < 2) cat("  - Solo hay", n_niveles, "nivel(es) de temperatura.\n")
  cat("  Se recomienda usar ANOVAs univariados (ya mostrados).\n")
}

cat("\n--- Temperaturas más equilibradas dentro del cluster verde ---\n")
ranking_verde <- resumen_temp %>%
  filter(!!sym(var_temperatura) %in% temperaturas_verde) %>%
  arrange(desc(Indice_Desempeno)) %>%
  dplyr::select(!!sym(var_temperatura), Indice_Desempeno, 
                ends_with("_media"), ends_with("_cv"))
print(ranking_verde)
mejor_temp <- ranking_verde %>% slice(1) %>% pull(!!sym(var_temperatura))
cat("\nTemperatura con mejor equilibrio (mayor índice):", mejor_temp, "\n")

# ------------------------------------------------------------------------------
# 6. ANÁLISIS DE IDEOTIPO (MTSI) PARA LAS TEMPERATURAS DEL CLUSTER VERDE
# ------------------------------------------------------------------------------
cat("\n===== PASO 3: ANÁLISIS DE IDEOTIPO (MTSI) EN EL CLUSTER VERDE =====\n")

vars_y <- vars_mtsi   
direcciones <- rep("max", length(vars_y))
names(direcciones) <- vars_y

df_means <- datos_verde %>%
  group_by(!!sym(var_temperatura)) %>%
  summarise(across(all_of(vars_y), mean, na.rm = TRUE)) %>%
  ungroup() %>%
  drop_na(all_of(vars_y))

if (nrow(df_means) == 0) {
  cat("No hay datos completos para el análisis MTSI en el cluster verde. Se omite.\n")
} else {
  df_means <- df_means %>% rename(UE = !!sym(var_temperatura))
  
  df_norm <- df_means
  df_real <- df_means
  
  for (v in vars_y) {
    dir <- direcciones[v]
    mi <- min(df_norm[[v]], na.rm = TRUE)
    ma <- max(df_norm[[v]], na.rm = TRUE)
    if (ma == mi) {
      df_norm[[v]] <- 50
    } else {
      if (dir == "min") {
        df_norm[[v]] <- ((ma - df_norm[[v]]) / (ma - mi)) * 100
      } else {
        df_norm[[v]] <- ((df_norm[[v]] - mi) / (ma - mi)) * 100
      }
    }
  }
  
  df_res <- df_norm %>% dplyr::select(UE)
  df_res$Distancia_MTSI <- 0
  ideotipo_perfecto <- 100
  for (v in vars_y) {
    df_res$Distancia_MTSI <- df_res$Distancia_MTSI + (df_norm[[v]] - ideotipo_perfecto)^2
  }
  df_res$Distancia_MTSI <- sqrt(df_res$Distancia_MTSI)
  
  max_dist <- max(df_res$Distancia_MTSI, na.rm = TRUE)
  min_dist <- min(df_res$Distancia_MTSI, na.rm = TRUE)
  if (max_dist == min_dist) {
    df_res$Score_Similitud <- 50
  } else {
    df_res$Score_Similitud <- ((max_dist - df_res$Distancia_MTSI) / (max_dist - min_dist)) * 100
  }
  df_res <- df_res %>%
    arrange(Distancia_MTSI) %>%
    mutate(Ranking_Global = row_number())
  
  df_final <- df_res %>%
    left_join(df_real, by = "UE")
  
  df_melt_norm <- df_norm %>%
    pivot_longer(cols = all_of(vars_y), names_to = "Variable", values_to = "Puntuacion_0_100")
  df_melt_real <- df_real %>%
    pivot_longer(cols = all_of(vars_y), names_to = "Variable", values_to = "Valor_Real")
  df_heatmap <- left_join(df_melt_norm, df_melt_real, by = c("UE", "Variable"))
  df_heatmap$UE <- factor(df_heatmap$UE, levels = rev(df_final$UE))
  
  p_heat <- ggplot(df_heatmap, aes(x = Variable, y = UE, fill = Puntuacion_0_100)) +
    geom_tile(color = "white", linewidth = 0.5) +
    scale_fill_gradient2(low = "#e74c3c", mid = "#f1c40f", high = "#27ae60", 
                         midpoint = 50, name = "Aptitud\n(100=Ideal)") +
    geom_text(aes(label = round(Valor_Real, 2)), color = "black", size = 3.5, fontface = "bold") +
    theme_minimal(base_size = 14) +
    labs(
      x = "Variables Evaluadas", 
      y = "Temperatura (°C) - Ordenadas de mejor a peor según MTSI", 
      title = "Perfil Multi‑Atributo (MTSI) para el Cluster Verde",
      subtitle = "Distancia al Ideotipo y Score de Similitud"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
      axis.text.y = element_text(face = "bold", size = 8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  print(p_heat)
  
  cat("\n--- Resultados del análisis de ideotipo (MTSI) para el cluster verde ---\n")
  print(df_final)
}

# ------------------------------------------------------------------------------
# 7. COMPARACIÓN CON EL TESTIGO
# ------------------------------------------------------------------------------
cat("\n===== PASO 4: COMPARACIÓN CON EL TESTIGO (", testigo_num, ") =====\n")
datos_testigo <- datos %>% filter(!!sym(var_temperatura) == testigo_num)

if (nrow(datos_testigo) > 0) {
  medias_testigo <- datos_testigo %>%
    summarise(across(all_of(variables_respuesta), mean, na.rm = TRUE))
  cat("Medias del testigo:\n")
  print(medias_testigo)
  
  cat("\n--- Comparación de cada temperatura del cluster vs testigo (ANOVA con contrastes) ---\n")
  datos_comp <- datos %>% filter(!!sym(var_temperatura) %in% c(temperaturas_verde, testigo_num))
  
  for (v in variables_respuesta) {
    datos_var <- datos_comp %>% filter(!is.na(!!sym(v)))
    if (nrow(datos_var) < 2) next
    modelo <- lm(as.formula(paste(v, "~ as.factor(", var_temperatura, ")")), data = datos_var)
    emm <- emmeans(modelo, specs = ~ as.factor(get(var_temperatura)))
    niveles <- levels(as.factor(datos_var[[var_temperatura]]))
    idx_testigo <- which(niveles == as.character(testigo_num))
    if (length(idx_testigo) == 1) {
      cont <- contrast(emm, "trt.vs.ctrl", ref = idx_testigo)
      cat("\nVariable:", v, "\n")
      print(cont)
    } else {
      cat("\nVariable:", v, " - testigo no encontrado en los niveles.\n")
    }
  }
} else {
  cat("No hay datos del testigo en el conjunto de datos.\n")
}

# ------------------------------------------------------------------------------
# 8. GRÁFICOS ADICIONALES DE PERFIL Y CV (cluster verde)
# ------------------------------------------------------------------------------
if (generar_graficos) {
  medias_verde <- datos_verde %>%
    group_by(!!sym(var_temperatura)) %>%
    summarise(across(all_of(variables_respuesta), mean, na.rm = TRUE)) %>%
    pivot_longer(cols = all_of(variables_respuesta), names_to = "Variable", values_to = "Media")
  
  p_perfil <- ggplot(medias_verde, aes(x = as.factor(!!sym(var_temperatura)), y = Media, 
                                       group = Variable, color = Variable)) +
    geom_line(size = 1.2) + geom_point(size = 3) +
    labs(title = "Perfil de medias en el cluster verde",
         x = "Temperatura", y = "Media") +
    theme_bw()
  print(p_perfil)
  
  cv_verde <- resumen_temp %>%
    filter(!!sym(var_temperatura) %in% temperaturas_verde) %>%
    dplyr::select(!!sym(var_temperatura), Indice_Desempeno, ends_with("_cv")) %>%
    pivot_longer(cols = ends_with("_cv"), names_to = "Variable", values_to = "CV") %>%
    mutate(Variable = gsub("_cv", "", Variable))
  
  p_cv <- ggplot(cv_verde, aes(x = as.factor(!!sym(var_temperatura)), y = CV, fill = Variable)) +
    geom_col(position = "dodge") +
    labs(title = "Coeficiente de variación por temperatura (cluster verde)",
         x = "Temperatura", y = "CV (%)") +
    theme_bw()
  print(p_cv)
}

# ------------------------------------------------------------------------------
# 9. REPORTE FINAL
# ------------------------------------------------------------------------------
cat("\n===== RESUMEN FINAL =====\n")

cat("Temperaturas del cluster verde (mejor desempeño):", 
    paste(temperaturas_verde, collapse = ", "), "\n")
cat("Rango del cluster verde:", rango_verde[1], "-", rango_verde[2], "\n")
cat("Mejor temperatura según índice de desempeño:", mejor_temp, "\n")
if (exists("df_final") && nrow(df_final) > 0) {
  cat("Mejor temperatura según MTSI (menor distancia al ideotipo):", 
      df_final$UE[which.min(df_final$Distancia_MTSI)], "\n")
}
cat("Variables analizadas:", paste(variables_respuesta, collapse = ", "), "\n")
cat("Testigo excluido del clustering:", testigo_num, "\n")
cat("\n¡Análisis completado!\n")
