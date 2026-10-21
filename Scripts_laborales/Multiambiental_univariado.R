# ==============================================================================
# SCRIPT GENÉRICO DE SELECCIÓN Y RANKING ESTADÍSTICO (INCLUYE GLMM Y ZINB)
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. CARGAR LIBRERÍAS
# ------------------------------------------------------------------------------
paquetes <- c("dplyr", "tidyr", "cluster", "factoextra", "ggplot2", "ggrepel", 
              "dendextend", "ggtext", "lme4", "lmerTest", "glmmTMB", "emmeans", 
              "multcomp", "multcompView", "gridExtra", "car")
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

# ¡IMPORTANTE! Lectura de datos desde el portapapeles
datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")
cat("\n--- Variables detectadas ---\n")
print(names(datos))

# ------------------------------------------------------------------------------
# 1. PARÁMETROS DE ENTRADA
# ------------------------------------------------------------------------------
var_tratamiento <- "Descripción"        
nombre_testigo  <- "Testigo"         

vars_mayor_es_mejor <- c("Total_indiv")           
vars_menor_es_mejor <- c()  
variables_respuesta <- c(vars_mayor_es_mejor, vars_menor_es_mejor)

# Para ANIDAMIENTOS usa la barra invertida. Ej: "INGENIO/ENSAYO"
# Si dejas c() hará un modelo simple (sin efectos aleatorios)
vars_aleatorias <- c("REPLICA") 
col_tiempo <- "FECHA.DE.MUESTREO"                   

alpha_superacion <- 0.15             

# --- DISTRIBUCIÓN ESTADÍSTICA PARA EL MODELO ---

# "gaussian"  -> ANOVA simple o Modelos Mixtos Normales.

# "poisson"   -> Conteos sin sobredispersión.

# "nbinomial" -> Conteos biológicos con sobredispersión (plagas).

# "zinb"      -> Conteos con exceso de ceros (Zero-Inflated).

# "binomial"  -> Presencia/ausencia (0/1).

# "beta"      -> Porcentajes (0 a 1).


distribucion_modelo <- "poisson" 

# Parámetros de clustering
k_optimo_usuario <- 4              
k_max <- 6                           
metodo_distancia <- "euclidean"      
metodo_cluster <- "ward.D2"          

# ------------------------------------------------------------------------------
# 2. VALIDACIONES PREVIAS
# ------------------------------------------------------------------------------
if (!(nombre_testigo %in% unique(datos[[var_tratamiento]]))) {
  stop(paste("El testigo", nombre_testigo, "no se encuentra en la columna", var_tratamiento))
}

# ------------------------------------------------------------------------------
# 3. ESTADÍSTICA DESCRIPTIVA Y MATRIZ DE CLUSTER
# ------------------------------------------------------------------------------
resumen_datos <- datos %>%
  dplyr::group_by(!!sym(var_tratamiento)) %>%
  dplyr::summarise(dplyr::across(dplyr::all_of(variables_respuesta), 
                                 list(media = ~mean(., na.rm = TRUE),
                                      cv = ~sd(., na.rm = TRUE) / mean(., na.rm = TRUE) * 100),
                                 .names = "{.col}_{.fn}")) %>%
  dplyr::ungroup()

resumen_datos <- resumen_datos %>% mutate(across(ends_with("_cv"), ~ifelse(is.nan(.), 0, .)))

if (!is.null(col_tiempo) && col_tiempo %in% names(datos)) {
  estabilidad_temporal <- datos %>%
    dplyr::group_by(!!sym(var_tratamiento), !!sym(col_tiempo)) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(variables_respuesta), ~mean(., na.rm = TRUE)), .groups = "drop") %>%
    dplyr::group_by(!!sym(var_tratamiento)) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(variables_respuesta),
                                   list(cv_temporal = ~sd(., na.rm = TRUE) / mean(., na.rm = TRUE) * 100),
                                   .names = "{.col}_cv_temp")) %>%
    dplyr::ungroup()
  resumen_datos <- resumen_datos %>% dplyr::left_join(estabilidad_temporal, by = var_tratamiento)
  cols_cv <- paste0(variables_respuesta, "_cv_temp")
} else {
  cols_cv <- paste0(variables_respuesta, "_cv")
}

datos_cluster <- resumen_datos[, paste0(variables_respuesta, "_media"), drop = FALSE]
rownames(datos_cluster) <- resumen_datos[[var_tratamiento]]

# ------------------------------------------------------------------------------
# 4. ÍNDICE MULTICRITERIO Y CLUSTERING
# ------------------------------------------------------------------------------
resumen_datos$Indice_Desempeno <- 0
peso_por_var <- 1 / (length(variables_respuesta) + 1)

for(v in vars_mayor_es_mejor) {
  col <- paste0(v, "_media")
  min_v <- min(resumen_datos[[col]], na.rm = TRUE); max_v <- max(resumen_datos[[col]], na.rm = TRUE)
  score <- ifelse(max_v > min_v, (resumen_datos[[col]] - min_v) / (max_v - min_v), 0)
  resumen_datos$Indice_Desempeno <- resumen_datos$Indice_Desempeno + (score * peso_por_var)
}

for(v in vars_menor_es_mejor) {
  col <- paste0(v, "_media")
  min_v <- min(resumen_datos[[col]], na.rm = TRUE); max_v <- max(resumen_datos[[col]], na.rm = TRUE)
  score <- ifelse(max_v > min_v, 1 - ((resumen_datos[[col]] - min_v) / (max_v - min_v)), 0)
  resumen_datos$Indice_Desempeno <- resumen_datos$Indice_Desempeno + (score * peso_por_var)
}

if (any(cols_cv %in% names(resumen_datos))) {
  promedio_cv <- rowMeans(resumen_datos[, cols_cv[cols_cv %in% names(resumen_datos)], drop = FALSE], na.rm = TRUE)
  min_cv <- min(promedio_cv, na.rm = TRUE); max_cv <- max(promedio_cv, na.rm = TRUE)
  score_cv <- ifelse(max_cv > min_cv, 1 - ((promedio_cv - min_cv) / (max_cv - min_cv)), 0)
  resumen_datos$Indice_Desempeno <- resumen_datos$Indice_Desempeno + (score_cv * peso_por_var)
}

cat("\n=== GENERANDO GRÁFICO 1: MÉTODOS K-ÓPTIMO ===\n")
datos_escalados <- scale(datos_cluster)
matriz_distancia <- dist(datos_escalados, method = metodo_distancia)

if (k_optimo_usuario == 0) {
  p_sil <- fviz_nbclust(datos_escalados, FUN = kmeans, method = "silhouette", k.max = min(k_max, nrow(datos_escalados) - 1)) + ggtitle("Silueta")
  p_wss <- fviz_nbclust(datos_escalados, FUN = kmeans, method = "wss", k.max = min(k_max, nrow(datos_escalados) - 1)) + ggtitle("Codo")
  p_gap <- fviz_nbclust(datos_escalados, FUN = kmeans, method = "gap_stat", k.max = min(k_max, nrow(datos_escalados) - 1), nboot = 50) + ggtitle("Gap")
  print(grid.arrange(p_sil, p_wss, p_gap, ncol = 3, top = "Métodos para determinar el número óptimo de clústeres"))
  Sys.sleep(2) 
  
  k_optimo <- as.numeric(p_sil$data[which.max(p_sil$data$y), "clusters"])
  if (is.na(k_optimo) || k_optimo < 2) k_optimo <- 3
} else {
  k_optimo <- k_optimo_usuario
}

hc <- hclust(matriz_distancia, method = metodo_cluster)
resumen_datos$Cluster_Original <- as.factor(cutree(hc, k = k_optimo))

# Ordenamos del mejor al peor basado en el Índice de Desempeño
orden_clusters <- resumen_datos %>%
  dplyr::group_by(Cluster_Original) %>%
  dplyr::summarise(Promedio_Idx = mean(Indice_Desempeno, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(Promedio_Idx)) %>%
  dplyr::mutate(Cluster = as.factor(1:dplyr::n()))

resumen_datos <- resumen_datos %>%
  dplyr::left_join(orden_clusters %>% dplyr::select(Cluster_Original, Cluster), by = "Cluster_Original") %>%
  dplyr::select(-Cluster_Original)

# ==============================================================================
# NUEVO: DEFINIR PALETA DE COLORES (Verde: Mejor, Azul: Intermedio, Rojo: Peor)
# ==============================================================================
paleta_base <- c("#4DAF4A", "#377EB8", "#E41A1C") # Verde, Azul, Rojo
colores_clusters <- colorRampPalette(paleta_base)(k_optimo)
names(colores_clusters) <- as.character(1:k_optimo)
# ==============================================================================

cat("\n=== GENERANDO GRÁFICO 2: DENDROGRAMA ===\n")
dend <- as.dendrogram(hc)

# 1. Colorear las ramas del árbol por clúster (por orden de estructura, se mantiene estándar)
dend <- color_branches(dend, k = k_optimo)
dend <- set(dend, "labels_cex", 0.8)

# Mantener al testigo en color rojo
labels_col <- rep("black", length(labels(dend)))
idx_testigo <- which(labels(dend) == nombre_testigo)
if (length(idx_testigo) > 0) labels_col[idx_testigo] <- "red"
labels_colors(dend) <- labels_col

par(mar = c(3, 1, 3, 7))
plot(dend, horiz = TRUE, main = paste("Dendrograma de Similitud\n(Testigo en Rojo, k=", k_optimo, ")"))

# 2. Sincronizar colores de recuadros con nuestra paleta Verde/Azul/Rojo
hojas_izq_a_der <- labels(dend)
clusters_izq_a_der <- resumen_datos$Cluster[match(hojas_izq_a_der, resumen_datos[[var_tratamiento]])]
orden_k_dend <- rle(as.character(clusters_izq_a_der))$values
colores_recuadros <- colores_clusters[orden_k_dend]

# Dibujar recuadros identificando los grupos por el color de desempeño
rect.dendrogram(dend, k = k_optimo, horiz = TRUE, border = colores_recuadros, lty = 2, lwd = 2) 
Sys.sleep(2) 

# ------------------------------------------------------------------------------
# 5. MODELOS Y CONTRASTES
# ------------------------------------------------------------------------------
cat("\n========================================================")
cat("\n--> TIPO DE DISTRIBUCIÓN ESTADÍSTICA ASIGNADA:", toupper(distribucion_modelo), "<--")
cat("\n========================================================\n")

resumen_datos$Letra_Estadistica <- "" 
resumen_datos$P_Valor_vs_Testigo <- NA  
resumen_datos$Supera_Testigo <- "No"

if(length(variables_respuesta) > 0) {
  var_eval <- variables_respuesta[1] 
  
  efectos_aleatorios <- vars_aleatorias
  if (!is.null(col_tiempo) && !(col_tiempo %in% efectos_aleatorios)) efectos_aleatorios <- c(efectos_aleatorios, col_tiempo)
  
  if(length(efectos_aleatorios) > 0) {
    efectos_str <- paste(paste0("(1|", efectos_aleatorios, ")"), collapse = " + ")
    form_str <- paste(var_eval, "~", var_tratamiento, "+", efectos_str)
  } else {
    form_str <- paste(var_eval, "~", var_tratamiento)
  }
  
  if(distribucion_modelo == "gaussian") {
    modelo <- try(lmerTest::lmer(as.formula(form_str), data = datos), silent = TRUE)
  } else {
    fam <- switch(distribucion_modelo,
                  "poisson" = poisson(),
                  "nbinomial" = nbinom2(),
                  "zinb" = nbinom2(),
                  "binomial" = binomial(),
                  "beta" = glmmTMB::beta_family())
    
    if(distribucion_modelo == "zinb") {
      modelo <- try(glmmTMB(as.formula(form_str), ziformula = ~1, family = fam, data = datos), silent = TRUE)
    } else {
      modelo <- try(glmmTMB(as.formula(form_str), family = fam, data = datos), silent = TRUE)
    }
  }
  
  if(inherits(modelo, "try-error") || is.null(modelo)) {
    cat("  Aviso: El modelo mixto falló o no tiene variables aleatorias. Ajustando modelo simple (ANOVA/GLM).\n")
    if(distribucion_modelo == "gaussian") {
      modelo <- lm(as.formula(paste(var_eval, "~", var_tratamiento)), data = datos)
    } else {
      modelo <- glm(as.formula(paste(var_eval, "~", var_tratamiento)), family = switch(distribucion_modelo, "nbinomial"=MASS::glm.nb, "zinb"=MASS::glm.nb, poisson()), data = datos)
    }
  }
  
  cat("\n=== RESULTADO GLOBAL DEL MODELO (ANÁLISIS DE DEVIANZA/VARIANZA) ===\n")
  tryCatch({
    print(car::Anova(modelo, type = "II"))
  }, error = function(e) {
    cat("  No se pudo usar car::Anova. Imprimiendo tabla base:\n")
    print(stats::anova(modelo))
  })
  cat("===================================================================\n")
  
  emm <- emmeans(modelo, specs = as.formula(paste("~", var_tratamiento)))
  revertir_letras <- var_eval %in% vars_mayor_es_mejor
  
  tryCatch({
    suppressMessages(res_cld <- as.data.frame(multcomp::cld(emm, Letters = letters, adjust = "none", reversed = revertir_letras)))
    col_letras <- grep("group|class", names(res_cld), value = TRUE)[1]
    
    if(!is.na(col_letras)) {
      letras_df <- res_cld %>% 
        dplyr::select(!!sym(var_tratamiento), Letras_Temp = !!sym(col_letras)) %>%
        dplyr::mutate(Letras_Temp = trimws(Letras_Temp))
      
      resumen_datos <- resumen_datos %>% 
        dplyr::select(-Letra_Estadistica) %>% 
        dplyr::left_join(letras_df, by = var_tratamiento) %>%
        dplyr::rename(Letra_Estadistica = Letras_Temp)
    }
  }, error = function(e) { cat("\n  [!] ADVERTENCIA: No se pudieron asignar Letras de Tukey.\n") })
  
  contrastes <- try(as.data.frame(contrast(emm, "trt.vs.ctrl", ref = which(levels(as.factor(datos[[var_tratamiento]])) == nombre_testigo))), silent = TRUE)
  if(!inherits(contrastes, "try-error")) {
    
    col_estim <- grep("estimate|ratio", names(contrastes), ignore.case = TRUE, value = TRUE)[1]
    col_pval <- grep("p.value", names(contrastes), ignore.case = TRUE, value = TRUE)[1]
    
    for(i in 1:nrow(contrastes)) {
      trat_actual <- gsub(paste0(" - ", nombre_testigo), "", contrastes$contrast[i])
      estim <- contrastes[[col_estim]][i]
      pval <- contrastes[[col_pval]][i]
      
      if(length(estim) == 0 || is.na(estim) || length(pval) == 0 || is.na(pval)) next
      
      idx <- which(resumen_datos[[var_tratamiento]] == trat_actual)
      resumen_datos$P_Valor_vs_Testigo[idx] <- round(pval, 4) 
      
      supera <- FALSE
      es_ratio <- grepl("ratio", col_estim, ignore.case = TRUE)
      valor_nulo <- ifelse(es_ratio, 1, 0)
      
      if (var_eval %in% vars_mayor_es_mejor & estim > valor_nulo & pval < alpha_superacion) supera <- TRUE
      if (var_eval %in% vars_menor_es_mejor & estim < valor_nulo & pval < alpha_superacion) supera <- TRUE
      
      if(supera) resumen_datos$Supera_Testigo[idx] <- "Sí"
    }
  }
}

resumen_datos$Supera_Testigo[resumen_datos[[var_tratamiento]] == nombre_testigo] <- "Es Testigo"

# ------------------------------------------------------------------------------
# 6. VISUALIZACIONES: BURBUJAS Y RANKING DE BARRAS
# ------------------------------------------------------------------------------
cat("\n=== GENERANDO GRÁFICO 3: BURBUJAS ===\n")
var_media <- paste0(var_eval, "_media")
var_cv <- cols_cv[1]

val_testigo <- resumen_datos[[var_media]][resumen_datos[[var_tratamiento]] == nombre_testigo]
if (length(val_testigo) == 1) {
  resumen_datos$Diff_Testigo <- abs(resumen_datos[[var_media]] - val_testigo)
} else {
  resumen_datos$Diff_Testigo <- 1
}

grafico_burbujas <- ggplot(resumen_datos, aes(x = !!sym(var_media), y = !!sym(var_cv))) +
  geom_point(aes(color = Cluster, size = Diff_Testigo), alpha = 0.8) +
  geom_text_repel(aes(label = !!sym(var_tratamiento)), size = 4, max.overlaps = 15) +
  scale_size_continuous(range = c(3, 10), name = paste("Dif. Absoluta vs", nombre_testigo)) +
  scale_color_manual(values = colores_clusters) + # Aplicar la paleta personalizada
  theme_bw(base_size = 12) +
  labs(title = paste("Relación Desempeño vs Inestabilidad (", var_eval, ")", sep=""),
       subtitle = "Tamaño de burbuja: Diferencia absoluta frente al testigo",
       x = paste("Promedio de", var_eval), 
       y = paste("Coeficiente de Variación (%)"))
print(grafico_burbujas)
Sys.sleep(2)

cat("\n=== GENERANDO GRÁFICO 4: RANKING DE BARRAS ===\n")
if (var_eval %in% vars_menor_es_mejor) {
  resumen_datos[[var_tratamiento]] <- factor(resumen_datos[[var_tratamiento]], 
                                             levels = resumen_datos[[var_tratamiento]][order(resumen_datos[[var_media]], decreasing = TRUE)])
} else {
  resumen_datos[[var_tratamiento]] <- factor(resumen_datos[[var_tratamiento]], 
                                             levels = resumen_datos[[var_tratamiento]][order(resumen_datos[[var_media]], decreasing = FALSE)])
}

grafico_ranking <- ggplot(resumen_datos, aes(x = !!sym(var_tratamiento), y = !!sym(var_media), fill = Cluster)) +
  geom_col(color = "black", alpha = 0.85, width = 0.7) +
  geom_text(aes(label = Letra_Estadistica, y = !!sym(var_media) + (max(!!sym(var_media), na.rm=T) * 0.05)), 
            size = 5, fontface = "bold", color = "darkred") +
  coord_flip() + 
  scale_fill_manual(values = colores_clusters) + # Aplicar la paleta personalizada
  labs(title = paste("Ranking Final de Tratamientos:", var_eval),
       subtitle = paste("Modelo:", toupper(distribucion_modelo), "| Orden biológico real"),
       x = "Tratamientos (Mejor arriba)",
       y = paste("Promedio Biológico de", var_eval),
       fill = "Clúster") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"), axis.text.y = element_text(face = "bold"))
print(grafico_ranking)

  # ------------------------------------------------------------------------------
# 7. REPORTE EN CONSOLA (TABLA FINAL COMPLETA)
# ------------------------------------------------------------------------------
cat("\n=== RESUMEN FINAL DE TRATAMIENTOS (ORDENADO POR EFECTIVIDAD) ===\n")
tabla_final <- resumen_datos %>%
  dplyr::select(!!sym(var_tratamiento), Cluster, !!sym(var_media), !!sym(var_cv), 
                Letra_Estadistica, P_Valor_vs_Testigo, Supera_Testigo) %>%
  dplyr::arrange(if(var_eval %in% vars_menor_es_mejor) !!sym(var_media) else desc(!!sym(var_media)))
print(tabla_final)

