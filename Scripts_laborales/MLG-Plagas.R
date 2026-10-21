# =============================================================================
# SCRIPT AVANZADO DE ANÁLISIS ESTADÍSTICO MULTIVARIABLE (GLMM / GLM)
# CON DIAGNÓSTICO AUTOMÁTICO DE SUPUESTOS E INTERPRETACIÓN EN CONSOLA
# =============================================================================

# =============================================================================
# 0. CONFIGURACIÓN INICIAL AUTOMATIZADA
# =============================================================================
var_tratamiento    <- "Descripción"     
nombre_testigo     <- "Testigo" 
incluir_testigo_anl <- F               
incluir_replica     <- T               
var_replica         <- "REPLICA"          

# --- FACTORES DE AGRUPACIÓN / ANIDAMIENTO EXTRA (Opcional) ---
# Ingresa uno o varios factores jerárquicos en orden correlativo, ej: c("ANIO", "ZAFRA", "FINCA")
# Si no tienes factores de agrupación extras, déjalo vacío: c()
factores_agrupacion <- c() 

# --- NUEVO: LIBERTAD DE MODELADO (EFECTOS FIJOS Y ALEATORIOS) ---
# Efectos fijos extra: Agrega interacciones o covariables. Ej: "* fecha" o "+ bloque"
# Déjalo vacío ("") si solo quieres evaluar el tratamiento.
efectos_fijos_extra <- "" 

# Efectos aleatorios personalizados. Ej: "(1|Zafra/Finca/Replica) + (1|Zafra/Finca)"
# Si lo dejas en NULL, el script usará la configuración por defecto de var_replica y factores_agrupacion.
efectos_aleatorios_custom <- "(1|REPLICA)" 
# ----------------------------------------------------------------

variables_conteo     <- c("Total_indiv") 
variables_porcentaje <- c()

# --- SELECCIÓN DEL TIPO DE MODELO ---

# -----------------------------------------------------------------------------
# DATOS DE CONTEO
# -----------------------------------------------------------------------------
# "POISSON"       -> Ideal para conteos estándar donde la Varianza es IGUAL a la Media.
#                    Rara vez se cumple en biología (suele haber sub o sobredispersión).
# "QUASIPOISSON" -> Para datos con sobredispersión lineal (Varianza = Phi * Media).
#                    En glmmTMB se ejecuta como 'nbinom1', el equivalente moderno de 
#                    Máxima Verosimilitud que permite incluir bloques y anidamientos.
# "NB"           -> Binomial Negativa estándar (nbinom2). Para sobredispersión cuadrática
#                    (la varianza aumenta fuertemente con la media). Es el más robusto 
#                    para poblaciones de insectos agregadas o agrupadas en parches.
# "ZIP"          -> Poisson con Inflación de Ceros. Se usa cuando tienes muchos ceros 
#                    debido a fallas en el muestreo o zonas donde la plaga no habita, 
#                    pero los datos que no son cero siguen una dispersión normal Poisson.
# "ZINB"         -> Binomial Negativa con Inflación de Ceros. El modelo más completo para 
#                    plagas de campo: corrige el exceso de ceros y la sobredispersión 
#                    de los conteos positivos al mismo tiempo.

modelo_elegido_conteo <- "ZINB"


# -----------------------------------------------------------------------------
# PORCENTAJES
# -----------------------------------------------------------------------------
# "GAUSSIAN"      -> Modelo Lineal Normal tradicional. Exige que los porcentajes se 
#                    distribuyan de forma simétrica (idealmente valores centrales entre 30% y 70%).
# "BINOMIAL"      -> Para proporciones puras derivadas de conteos dicotómicos (ej: Número de 
#                    hojas enfermas / Total de hojas muestreadas). Exige datos de n y éxitos.
# "QUASIBINOMIAL" -> Variante de la Binomial que corrige la sobredispersión de los bloques.
# "BETA"         -> El estándar de oro para datos porcentuales continuos enteros (0-100%) o 
#                    tasas (0-1) que son asimétricos o se acumulan cerca de los extremos (0% o 100%).
#                    Modela de forma elegante varianzas heterogéneas sin transformar datos.
# "ZI_BETA"       -> Distribución Beta con Inflación de Ceros. Ideal para eficiencias de control 
#                    o severidad de daño donde muchas parcelas registran exactamente 0% de daño.

modelo_elegido_porc   <- "BETA"

# --- CONFIGURACIÓN DE LETRAS Y RELATIVOS ---

# ORDEN DE LAS LETRAS (Prueba de medias Fisher/LSD)
# TRUE  = Letra 'a' para la media MÁS ALTA (Ideal en Rendimiento o Mortalidad).
# FALSE = Letra 'a' para la media MÁS BAJA (Ideal en Conteos de plagas o % de Daño).

letra_a_en_mayor_media <- T  

# DIRECCIÓN DEL EFECTO PARA LA EFICIENCIA (Base 0)
# TRUE  = Menos es mejor (Plagas). Fórmula: 1 - (Tratamiento / Testigo)
# FALSE = Más es mejor (Rendimiento). Fórmula: (Tratamiento / Testigo) - 1

menor_es_mejor         <- F


# =============================================================================
# 1. CARGA DE LIBRERÍAS Y DATOS
# =============================================================================
library(glmmTMB)   
library(emmeans)   
library(multcomp)  
library(dplyr)     
library(ggplot2)   
library(car)       
library(DHARMa)    
df <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")
cat("\n--- Variables detectadas ---\n")
print(names(df))
View(df)

# =============================================================================
# 1.5 DIAGNÓSTICO PREVIO PARA SELECCIÓN DE MODELOS
# =============================================================================
cat("\n===================================================================\n")
cat(">>> DIAGNÓSTICO SUGERIDO PARA SELECCIÓN DE MODELOS ESTADÍSTICOS <<<\n")
cat("===================================================================\n")

# Diagnóstico para Conteos
if (length(variables_conteo) > 0) {
  cat("\n[ ANÁLISIS DE DATOS DE CONTEO ]\n")
  for (v in variables_conteo) {
    if (v %in% names(df)) {
      dat <- na.omit(df[[v]])
      media_v <- mean(dat)
      var_v <- var(dat)
      ratio_disp <- ifelse(media_v == 0, 0, var_v / media_v)
      pct_ceros <- (sum(dat == 0) / length(dat)) * 100
      
      cat(sprintf("Variable: %s\n", v))
      cat(sprintf("  - Media: %.2f | Varianza: %.2f | Ratio Var/Media: %.2f\n", media_v, var_v, ratio_disp))
      cat(sprintf("  - Frecuencia de ceros: %.1f%%\n", pct_ceros))
      
      cat("  -> MODELO SUGERIDO: ")
      if (pct_ceros > 15 && ratio_disp > 1.5) {
        cat("ZINB (Exceso de ceros y fuerte sobredispersión detectada)\n")
      } else if (pct_ceros > 15 && ratio_disp <= 1.5) {
        cat("ZIP (Inflación de ceros sin sobredispersión severa)\n")
      } else if (ratio_disp > 1.5) {
        cat("NB o QUASIPOISSON (Sobredispersión detectada sin exceso de ceros)\n")
      } else {
        cat("POISSON (Dispersión ideal, varianza similar a la media)\n")
      }
    }
  }
}

# Diagnóstico para Porcentajes
if (length(variables_porcentaje) > 0) {
  cat("\n[ ANÁLISIS DE DATOS DE PORCENTAJE ]\n")
  for (v in variables_porcentaje) {
    if (v %in% names(df)) {
      dat <- na.omit(df[[v]])
      # Se asume que si el maximo es > 1, está en escala 0-100
      if(max(dat) > 1) dat <- dat / 100 
      pct_ceros <- (sum(dat == 0) / length(dat)) * 100
      pct_unos <- (sum(dat == 1) / length(dat)) * 100
      
      cat(sprintf("Variable: %s\n", v))
      cat(sprintf("  - Mínimo: %.2f | Máximo: %.2f | Ceros: %.1f%% | Cienes: %.1f%%\n", min(dat), max(dat), pct_ceros, pct_unos))
      
      cat("  -> MODELO SUGERIDO: ")
      if (pct_ceros > 15) {
        cat("ZI_BETA (Alta cantidad de valores en 0 absoluto)\n")
      } else if (pct_ceros > 0 || pct_unos > 0 || max(dat) > 0.8 || min(dat) < 0.2) {
        cat("BETA o QUASIBINOMIAL (Datos asimétricos o con valores extremos)\n")
      } else {
        cat("GAUSSIAN o BETA (Distribución simétrica lejos de los extremos 0 y 1)\n")
      }
    }
  }
}
cat("===================================================================\n")


# =============================================================================
# 2. PREPARACIÓN AUTOMÁTICA DE FACTORES
# =============================================================================
df[[var_tratamiento]] <- as.factor(df[[var_tratamiento]])

if(incluir_replica && (var_replica %in% names(df))) df[[var_replica]] <- as.factor(df[[var_replica]])

for (factor_col in factores_agrupacion) {
  if (factor_col %in% names(df)) df[[factor_col]] <- as.factor(df[[factor_col]])
}

todas_las_variables <- c(variables_conteo, variables_porcentaje)
for (v in todas_las_variables) {
  if (v %in% names(df)) df[[v]][is.na(df[[v]])] <- 0
}

formula_random <- ""
componentes_random <- c()
if(incluir_replica && (var_replica %in% names(df))) componentes_random <- c(componentes_random, paste("(1 |", var_replica, ")"))
if(length(factores_agrupacion) > 0) componentes_random <- c(componentes_random, paste("(1 |", paste(factores_agrupacion, collapse = "/"), ")"))

if(length(componentes_random) > 0) {
  formula_random <- paste("+", paste(componentes_random, collapse = " + "))
  mensaje_estructura <- paste("Estructura Mixta Default:", paste(componentes_random, collapse = " y "))
} else {
  mensaje_estructura <- "Modelo de Efectos Fijos"
}

if(incluir_testigo_anl) {
  df_analisis <- df; titulo_extra <- ""
} else {
  df_analisis <- df %>% dplyr::filter(.data[[var_tratamiento]] != nombre_testigo) %>% droplevels()
  titulo_extra <- "(Excluyendo Testigo)"
}

formatear_pval <- function(p) { if(is.na(p)) return("NA"); if(p < 0.001) return("< 0.001"); return(as.character(round(p, 4))) }

# =============================================================================
# 3. BUCLE DE ANÁLISIS, SUPUESTOS Y GRÁFICOS
# =============================================================================
tabla_reporte_final <- df %>% dplyr::group_by(.data[[var_tratamiento]]) %>% dplyr::summarise(Obs = dplyr::n(), .groups = 'drop') %>% dplyr::rename(Tratamiento = !!sym(var_tratamiento))
fila_pvalores <- data.frame(Tratamiento = "P-valor (Trt)", Obs = NA)

config_variables <- data.frame(
  Variable = c(variables_conteo, variables_porcentaje),
  Tipo = c(rep("Conteo", length(variables_conteo)), rep("Porcentaje", length(variables_porcentaje))), stringsAsFactors = FALSE
)

for(i in seq_len(nrow(config_variables))) {
  v_activa <- config_variables$Variable[i]
  t_activa <- config_variables$Tipo[i]
  if (!(v_activa %in% names(df))) next
  
  cat(paste("\n======================================================\nANALIZANDO:", v_activa, "\n======================================================\n"))
  
  medias_reales <- df %>% dplyr::group_by(.data[[var_tratamiento]]) %>% dplyr::summarise(MediaReal = mean(.data[[v_activa]], na.rm = TRUE), .groups = 'drop')
  media_testigo_ref <- medias_reales$MediaReal[medias_reales[[var_tratamiento]] == nombre_testigo]
  if(length(media_testigo_ref) == 0 || is.na(media_testigo_ref)) media_testigo_ref <- NA
  
  df_modelo_input <- df_analisis
  fam_elegida <- gaussian
  zi_formula  <- ~ 0
  
  if(t_activa == "Porcentaje") {
    if(max(df_modelo_input[[v_activa]], na.rm=TRUE) > 1) df_modelo_input[[v_activa]] <- df_modelo_input[[v_activa]] / 100
    
    if (modelo_elegido_porc %in% c("BETA", "ZI_BETA")) {
      df_modelo_input[[v_activa]] <- (df_modelo_input[[v_activa]] * (nrow(df_modelo_input) - 1) + 0.5) / nrow(df_modelo_input)
      fam_elegida <- beta_family
      if(modelo_elegido_porc == "ZI_BETA") zi_formula <- ~ 1
      nombre_modelo_print <- "GLMM Distribución Beta"
    } else if (modelo_elegido_porc %in% c("BINOMIAL", "QUASIBINOMIAL")) {
      fam_elegida <- binomial; nombre_modelo_print <- "GLMM Binomial"
    } else { fam_elegida <- gaussian; nombre_modelo_print <- "GLMM Gaussiano" }
  } else {
    if (modelo_elegido_conteo == "POISSON")      { fam_elegida <- poisson; nombre_modelo_print <- "GLMM Poisson" }
    if (modelo_elegido_conteo == "QUASIPOISSON") { fam_elegida <- nbinom1; nombre_modelo_print <- "GLMM nbinom1 (Equivalente Cuasipoisson)" }
    if (modelo_elegido_conteo == "NB")           { fam_elegida <- nbinom2; nombre_modelo_print <- "GLMM Binomial Negativa" }
    if (modelo_elegido_conteo == "ZIP")          { fam_elegida <- poisson; zi_formula <- ~ 1; nombre_modelo_print <- "GLMM ZIP" }
    if (modelo_elegido_conteo == "ZINB")         { fam_elegida <- nbinom2; zi_formula <- ~ 1; nombre_modelo_print <- "GLMM ZINB" }
  }
  
  # --- CONSTRUCCIÓN DINÁMICA DE LA FÓRMULA ---
  if (!is.null(efectos_fijos_extra) && efectos_fijos_extra != "") {
    formula_fija <- paste(v_activa, "~", var_tratamiento, efectos_fijos_extra)
  } else {
    formula_fija <- paste(v_activa, "~", var_tratamiento)
  }
  
  if (!is.null(efectos_aleatorios_custom)) {
    formula_final <- paste(formula_fija, "+", efectos_aleatorios_custom)
    cat(paste("Estructura Mixta (Custom):", efectos_aleatorios_custom, "\n"))
  } else {
    formula_final <- paste(formula_fija, formula_random)
    cat(paste(mensaje_estructura, "\n"))
  }
  
  modelo_fit <- glmmTMB(as.formula(formula_final), ziformula = zi_formula, family = fam_elegida, data = df_modelo_input)
  
  # --- 3.1 VERIFICACIÓN DE SUPUESTOS CON INTERPRETACIÓN EN CONSOLA (ACTUALIZADO) ---
  cat("\n>>> VERIFICACIÓN DE SUPUESTOS METODOLÓGICOS (DHARMa) <<<\n")
  residuos_dharma <- simulateResiduals(fittedModel = modelo_fit, plot = FALSE)
  plot(residuos_dharma, quantreg = TRUE) 
  
  # A. Evaluación del Supuesto de Dispersión
  test_disp <- testDispersion(residuos_dharma, plot = FALSE)
  p_disp <- test_disp$p.value
  cat(paste("  [*] Test de Dispersión (p =", round(p_disp, 4), "): "))
  if(p_disp < 0.05) {
    cat("NO SE CUMPLE (Sobredispersión/Subdispersión detectada).\n")
    cat("      Significado: La variabilidad real de la plaga rompe los límites teóricos del modelo.\n")
  } else {
    cat("SE CUMPLE (Dispersión controlada dentro del rango esperado).\n")
    cat("      Significado: La varianza se comporta de forma ideal conforme cambia la media.\n")
  }
  
  # B. Evaluación del Supuesto de Exceso de Ceros (Solo para variables de conteo)
  p_zeros <- NA
  if (t_activa == "Conteo") {
    test_zeros <- testZeroInflation(residuos_dharma, plot = FALSE)
    p_zeros <- test_zeros$p.value
    cat(paste("  [*] Test de Exceso de Ceros (p =", round(p_zeros, 4), "): "))
    if(p_zeros < 0.05) {
      cat("NO SE CUMPLE (Inflación de ceros significativa).\n")
      cat("      Significado: Tienes más parcelas o plantas con 'cero' plagas de lo estadísticamente normal.\n")
    } else {
      cat("SE CUMPLE (Sin inflación de ceros anómala).\n")
      cat("      Significado: La cantidad de ceros en el ensayo se explica perfectamente por el azar biológico.\n")
    }
  }
  
  # C. Dictamen Final de Confiabilidad Estadística
  cat("  [>] DICTAMEN DE CONFIABILIDAD DE LAS LETRAS DE MEDIAS:\n")
  if(p_disp >= 0.05 && (is.na(p_zeros) || p_zeros >= 0.05)) {
    cat("      --> ¡PRUEBA TOTALMENTE CONFIABLE! Tus letras y grupos son 100% válidos para publicación científica.\n")
  } else {
    # Si el modelo elegido ya era robusto (NB, ZINB, Beta, etc.) y aún así DHARMa protesta levemente:
    if((t_activa == "Conteo" && modelo_elegido_conteo %in% c("NB", "ZINB", "QUASIPOISSON")) || 
       (t_activa == "Porcentaje" && modelo_elegido_porc %in% c("BETA", "ZI_BETA"))) {
      cat("      --> PRUEBA TOLERABLE/ACEPTABLE. El modelo ya es robusto y absorbió la mayor parte de la dispersión.\n")
    } else {
      cat("      --> ¡ALERTA! POCO CONFIABLE. El error estándar está sesgado y tus letras podrían ser falsas.\n")
      if(t_activa == "Conteo" && p_disp < 0.05) cat("          RECOMENDACIÓN: Cambia el modelo de conteo de 'POISSON' a 'NB' o 'QUASIPOISSON'.\n")
      if(t_activa == "Conteo" && p_zeros < 0.05) cat("          RECOMENDACIÓN: Cambia el modelo de conteo a 'ZINB' o 'ZIP' para manejar los ceros.\n")
      if(t_activa == "Porcentaje" && p_disp < 0.05) cat("          RECOMENDACIÓN: Cambia la familia de porcentaje a 'BETA' o 'ZI_BETA'.\n")
    }
  }
  cat("-----------------------------------------------------------------------------\n\n")
  
  # --- 3.2 ANOVA Y MEDIAS ---
  anova_res <- Anova(modelo_fit, type = "II")
  pval_txt <- formatear_pval(anova_res[1, grep("Pr", names(anova_res))])
  
  # NOTA: emmeans está configurado para extraer las medias marginales del tratamiento principal
  # para mantener la consistencia de tu tabla final y los gráficos.
  medias_ajustadas <- emmeans(modelo_fit, as.formula(paste("~", var_tratamiento)), type = "response")
  medias_ajustadas_df <- as.data.frame(medias_ajustadas)
  
  nombres_media <- c("emmean", "response", "prob", "rate", "prop")
  col_media <- names(medias_ajustadas_df)[names(medias_ajustadas_df) %in% nombres_media][1]
  
  colnames(medias_ajustadas_df)[names(medias_ajustadas_df) == col_media] <- "emmean"
  colnames(medias_ajustadas_df)[colnames(medias_ajustadas_df) %in% c("lower.CL", "asymp.LCL")] <- "LCL"
  colnames(medias_ajustadas_df)[colnames(medias_ajustadas_df) %in% c("upper.CL", "asymp.UCL")] <- "UCL"
  
  if(t_activa == "Porcentaje" && max(df[[v_activa]], na.rm=TRUE) > 1) {
    medias_ajustadas_df <- medias_ajustadas_df %>% 
      dplyr::mutate(emmean = emmean * 100, LCL = LCL * 100, UCL = UCL * 100)
  }
  
  letras_test <- cld(medias_ajustadas, alpha = 0.15, Letters = letters, adjust = "none", reversed = letra_a_en_mayor_media)
  letras_test$.group <- trimws(letras_test$.group)
  
  # --- 3.3 ENSAMBLE Y RELATIVOS VECTORIALES ---
  datos_combinados <- merge(medias_reales, letras_test %>% dplyr::select(!!sym(var_tratamiento), .group), by = var_tratamiento, all.x = TRUE)
  
  if (!is.na(media_testigo_ref) && media_testigo_ref != 0) {
    datos_combinados <- datos_combinados %>% 
      dplyr::mutate(
        Rel_Base1  = round(MediaReal / media_testigo_ref, 4),
        Efic_Base0 = if(menor_es_mejor) round(1 - (MediaReal / media_testigo_ref), 4) else round((MediaReal / media_testigo_ref) - 1, 4)
      )
  } else {
    datos_combinados <- datos_combinados %>% dplyr::mutate(Rel_Base1 = NA, Efic_Base0 = NA)
  }
  
  nom_med  <- paste0(v_activa, "_Media")
  nom_rel1 <- paste0(v_activa, "_Rel(Base1)")
  nom_rel0 <- paste0(v_activa, "_Efic(Base0)")
  
  datos_combinados <- datos_combinados %>% 
    dplyr::mutate(Media_L = ifelse(is.na(.group), as.character(round(MediaReal, 2)), paste0(round(MediaReal, 2), " ", .group)))
  
  datos_combinados <- datos_combinados[, c(var_tratamiento, "Media_L", "Rel_Base1", "Efic_Base0")]
  colnames(datos_combinados) <- c("Tratamiento", nom_med, nom_rel1, nom_rel0)
  
  tabla_reporte_final <- dplyr::left_join(tabla_reporte_final, datos_combinados, by = "Tratamiento")
  fila_pvalores[[nom_med]]  <- pval_txt
  fila_pvalores[[nom_rel1]] <- ""
  fila_pvalores[[nom_rel0]] <- ""
  
  # --- 3.4 GRÁFICOS ---
  datos_graf <- merge(medias_ajustadas_df, medias_reales, by = var_tratamiento)
  datos_graf <- merge(datos_graf, letras_test %>% dplyr::select(!!sym(var_tratamiento), .group), by = var_tratamiento)
  
  plot_out <- ggplot(datos_graf, aes(x = reorder(.data[[var_tratamiento]], MediaReal))) +
    geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.25, color = "black", size = 0.9) +
    geom_point(aes(y = emmean), color = "blue", size = 2.5, shape = 18) +
    geom_point(aes(y = MediaReal), color = "black", size = 3.5, shape = 16) +
    geom_text(aes(y = MediaReal, label = paste0(round(MediaReal, 2), " ", .group)), vjust = -0.8, fontface = "bold", size = 3.5) +
    coord_flip() + theme_bw() +
    labs(title = paste("Efectos sobre:", v_activa), subtitle = paste(nombre_modelo_print, "| p-valor =", pval_txt), x = "Tratamientos", y = paste("Valores de", v_activa))
  if(!incluir_testigo_anl && !is.na(media_testigo_ref)) plot_out <- plot_out + geom_hline(yintercept = media_testigo_ref, linetype = "dashed", color = "red")
  
  print(plot_out)
}

# =============================================================================
# 4. TABLA FINAL
# =============================================================================
tabla_reporte_final <- tabla_reporte_final %>% dplyr::mutate_all(as.character)
fila_pvalores       <- fila_pvalores %>% dplyr::mutate_all(as.character)
fila_pvalores$Obs   <- ""
fila_pvalores       <- fila_pvalores[, colnames(tabla_reporte_final)]
tabla_con_valores_p <- rbind(tabla_reporte_final, fila_pvalores)
tabla_con_valores_p[is.na(tabla_con_valores_p)] <- "-"

cat("\n--- TABLA FINAL DE RESULTADOS CON SUPUESTOS Y RELATIVOS CORREGIDOS ---\n")
print(as.data.frame(tabla_con_valores_p), row.names = FALSE)




# =============================================================================
# 5. MÓDULO ADICIONAL: CURVA DE DENSIDAD POBLACIONAL Y ÁREA BAJO LA CURVA (AUC)
# =============================================================================

names(df)

cat("\n===================================================================\n")
cat(">>> MÓDULO DE ÁREA BAJO LA CURVA (AUC) Y DENSIDAD POBLACIONAL <<<\n")
cat("===================================================================\n")

# Si no tienes plotly cargado previamente, lo cargamos aquí para el módulo
if (!require(plotly)) { install.packages("plotly"); library(plotly) }

# --- CONFIGURACIÓN ESPECÍFICA DEL MÓDULO ---
var_tiempo           <- "FECHA.DE.MUESTREO" # Nombre exacto de tu columna temporal

# >>> NUEVO INTERRUPTOR DE AGRUPACIÓN <<<
curva_general        <- FALSE             # TRUE = Una sola curva global, FALSE = Varias curvas separadas
var_agrupacion_curva <- var_tratamiento   # Columna para agrupar si curva_general es FALSE (ej. var_tratamiento, "ENSAYO", "INGENIO")

# >>> INTERRUPTOR DE POBLACIÓN INICIAL <<<
integrar_pob_inicial <- F              # TRUE = Añadir un "Día 0" a la curva, FALSE = Usar solo muestreos regulares


# ----- NUEVO: Control de fechas iniciales por grupo -----
fecha_inicial_por_grupo <- T   # TRUE = usa el primer muestreo de cada grupo como Día 0
# FALSE = usa la fecha fija que indiques en 'fecha_inicial'
# (Si pones FALSE, puedes seguir usando 'fecha_inicial' como antes)


# DATOS DE POBLACIÓN INICIAL (Solo se usan si integrar_pob_inicial es TRUE)
names(df)
columna_pob_inicial  <- "Población.inicial" # Nombre de la columna con el conteo inicial (ej: "pob_0")
fecha_inicial        <- "10/06/2026"                # Escribe la fecha inicial (Procura usar tu mismo formato)

# ESTÉTICA Y LÍNEAS DE CORTE
suavizar_curvas      <- F  # TRUE = Curvas suavizadas, FALSE = Líneas rectas conectando puntos
umbral_dano          <- NULL  # Umbral de acción / daño (ej: 10)
nivel_dano_economico <- NULL  # Nivel de daño económico (NDE) (ej: 20)

if (var_tiempo %in% names(df) && length(variables_conteo) > 0) 
  
  v_conteo_auc <- variables_conteo[1]
  
# ===========================================================================
# 5.0 INTEGRACIÓN DEL PUNTO DE INICIO (Día 0) POR GRUPO
# ===========================================================================
df_work <- df

if (integrar_pob_inicial) {
  # Verificar que la columna de población inicial existe
  if (!is.null(columna_pob_inicial) && columna_pob_inicial %in% names(df_work)) {
    
    # ---- Si se pide fecha inicial por grupo ----
    if (fecha_inicial_por_grupo && !curva_general) {
      
      # Agrupar por la variable de agrupación y obtener la fecha mínima (primer muestreo)
      # También calculamos la población inicial media por grupo (por si hay varios registros)
      df_primeras_fechas <- df_work %>%
        dplyr::group_by(.data[[var_agrupacion_curva]]) %>%
        dplyr::summarise(
          Fecha_Inicio = min(as.Date(as.character(.data[[var_tiempo]]), 
                                     tryFormats = c("%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d", "%d-%m-%Y")), na.rm = TRUE),
          Pob_Inicial = mean(.data[[columna_pob_inicial]], na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        dplyr::filter(!is.na(Fecha_Inicio))
      
      # Crear una fila inicial para cada grupo con su fecha y población inicial
      df_init <- df_primeras_fechas %>%
        dplyr::mutate(
          !!var_tiempo := format(Fecha_Inicio, "%d/%m/%Y"),   # Usamos el mismo formato que tus datos
          !!v_conteo_auc := Pob_Inicial
        ) %>%
        dplyr::select(-Fecha_Inicio, -Pob_Inicial)  # Quitamos columnas auxiliares
      
      # Añadir las columnas adicionales que pueda tener df_work (se rellenan con NA)
      # Para simplificar, solo conservamos las columnas que existen en df_work y en df_init
      columnas_faltantes <- setdiff(names(df_work), names(df_init))
      for (col in columnas_faltantes) {
        df_init[[col]] <- NA
      }
      
      # Unir con los datos originales
      df_work <- dplyr::bind_rows(df_init, df_work)
      
      cat(sprintf("[✓] Se integró el punto de inicio (Día 0) para cada grupo de '%s' usando la primera fecha de muestreo.\n", var_agrupacion_curva))
      
    } else {
      # ---- NUEVO: fecha fija con TODOS los grupos ----
      cat(sprintf("[✓] Se integró el punto de inicio (Día 0) con fecha fija: '%s' para TODOS los tratamientos.\n", fecha_inicial))
      
      # Crear una fila de inicio para CADA tratamiento, manteniendo la estructura de columnas
      df_init <- df_work %>%
        dplyr::group_by(.data[[var_agrupacion_curva]]) %>%
        dplyr::slice(1) %>%                    # Toma la primera fila de cada grupo (para heredar todas las columnas y factores)
        dplyr::ungroup() %>%
        dplyr::mutate(
          !!var_tiempo := as.character(fecha_inicial),             # Obliga a que la fecha de todos sea 10/06/2026
          !!v_conteo_auc := mean(.data[[columna_pob_inicial]], na.rm = TRUE) # Asigna la población inicial de ese grupo
        )
      
      # Unir las nuevas filas de inicio con los datos originales
      df_work <- dplyr::bind_rows(df_init, df_work)
    }
    
  } else {
    cat(sprintf("[!] Advertencia: integrar_pob_inicial es TRUE, pero no se encontró la columna '%s'.\n", columna_pob_inicial))
  }
}

# --- Conversión a fecha y cálculo de tiempo numérico (como ya tenías) ---
df_work$Fecha_Plot <- as.Date(as.character(df_work[[var_tiempo]]), 
                              tryFormats = c("%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d", "%d-%m-%Y"))

if(any(is.na(df_work$Fecha_Plot))) {
  warning("Algunas fechas no pudieron ser reconocidas. Revisa que el formato sea dd/mm/yyyy o yyyy-mm-dd.")
}

# Calcular tiempo numérico en días respecto a la fecha mínima GLOBAL (si quieres que el cero sea el mínimo global)
# Pero si usamos fechas por grupo, el tiempo numérico se calculará sobre el total y el gráfico mostrará fechas reales.
# Si prefieres que el eje X muestre días desde el inicio de cada grupo, tendrías que calcular el tiempo por grupo.
# Para simplificar, dejamos el cálculo global (así las curvas se superponen en el mismo eje temporal)
df_work$tiempo_num <- as.numeric(df_work$Fecha_Plot) - min(as.numeric(df_work$Fecha_Plot), na.rm = TRUE)
  
  # ===========================================================================
  # 5.1 GRÁFICO INTERACTIVO DE CURVA DE DENSIDAD POBLACIONAL (PLOTLY)
  # ===========================================================================
  if (curva_general) {
    # Opción A: Una sola curva general
    df_curve <- df_work %>%
      dplyr::filter(!is.na(Fecha_Plot)) %>% 
      dplyr::group_by(Fecha_Plot, tiempo_num) %>%
      dplyr::summarise(Media_Conteo = mean(.data[[v_conteo_auc]], na.rm = TRUE), .groups = 'drop') %>%
      dplyr::arrange(Fecha_Plot)
    
    plot_curva_base <- ggplot(df_curve, aes(x = Fecha_Plot, y = Media_Conteo, group = 1)) +
      geom_point(size = 3, alpha = 0.9, color = "#2980B9")
    
  } else {
    # Opción B: Curvas agrupadas (por tratamiento, finca, etc.)
    df_curve <- df_work %>%
      dplyr::filter(!is.na(Fecha_Plot)) %>% 
      dplyr::group_by(.data[[var_agrupacion_curva]], Fecha_Plot, tiempo_num) %>%
      dplyr::summarise(Media_Conteo = mean(.data[[v_conteo_auc]], na.rm = TRUE), .groups = 'drop') %>%
      dplyr::arrange(Fecha_Plot) 
    
    plot_curva_base <- ggplot(df_curve, aes(x = Fecha_Plot, y = Media_Conteo, color = .data[[var_agrupacion_curva]], group = .data[[var_agrupacion_curva]])) +
      geom_point(size = 3, alpha = 0.9)
  }
  
  # Estética base compartida
  plot_curva_base <- plot_curva_base +
    scale_x_date(date_labels = "%d/%m/%Y", breaks = unique(df_curve$Fecha_Plot)) + 
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(face = "bold", size = 15, color = "#333333"),
      axis.title = element_text(face = "bold", color = "#555555"),
      axis.text.x = element_text(angle = 45, hjust = 1, color = "#333333"),
      legend.title = element_text(face = "bold")
    ) +
    labs(title = paste("Dinámica Poblacional -", v_conteo_auc),
         x = "Tiempo / Fecha de Evaluación",
         y = paste("Promedio de", v_conteo_auc))
  
  if (!curva_general) {
    plot_curva_base <- plot_curva_base + labs(color = var_agrupacion_curva)
  }
  
  if (suavizar_curvas) {
    plot_curva_base <- plot_curva_base + geom_smooth(se = FALSE, method = "loess", span = 0.7, linewidth = 1.2)
  } else {
    if (curva_general) {
      plot_curva_base <- plot_curva_base + geom_line(linewidth = 1.2, alpha = 0.8, color = "#2980B9")
    } else {
      plot_curva_base <- plot_curva_base + geom_line(linewidth = 1.2, alpha = 0.8)
    }
  }
  
  if (!is.null(umbral_dano)) {
    fecha_min <- min(df_curve$Fecha_Plot, na.rm = TRUE)
    plot_curva_base <- plot_curva_base + 
      geom_hline(yintercept = umbral_dano, linetype = "dashed", color = "#E67E22", linewidth = 0.9) +
      annotate("text", x = fecha_min, y = umbral_dano, label = "UDE=0.20", 
               nudge_y = 0.5, color = "#E67E22", size = 4.5, fontface = "bold")
  }
  
  if (!is.null(nivel_dano_economico)) {
    fecha_min <- min(df_curve$Fecha_Plot, na.rm = TRUE)
    plot_curva_base <- plot_curva_base + 
      geom_hline(yintercept = nivel_dano_economico, linetype = "dashed", color = "#E74C3C", linewidth = 0.9) +
      annotate("text", x = fecha_min, y = nivel_dano_economico, label = "NDE", vjust = -0.5, color = "#E74C3C", size = 3.5, fontface = "bold")
  }
  
  grafico_interactivo <- ggplotly(plot_curva_base, tooltip = c("x", "y", "color")) %>%
    layout(legend = list(orientation = "h", x = 0, y = -0.25),
           hovermode = "x unified")
  
  print(grafico_interactivo)
  
  # ===========================================================================
  # 5.2 CÁLCULO DEL ÁREA BAJO LA CURVA (AUC)
  # ===========================================================================
  calc_auc <- function(x, y) {
    df_temp <- na.omit(data.frame(x=x, y=y))
    if(nrow(df_temp) < 2) return(NA)
    df_temp <- df_temp[order(df_temp$x), ]
    sum(diff(df_temp$x) * (df_temp$y[-1] + df_temp$y[-nrow(df_temp)]) / 2, na.rm = TRUE)
  }
  
  if (curva_general) {
    cat("\n--- Calculando AUC General (Sin agrupaciones) ---\n")
    auc_total <- calc_auc(df_curve$tiempo_num, df_curve$Media_Conteo)
    cat(sprintf("[>] Área Bajo la Curva Total (Impacto acumulado global): %.2f\n", auc_total))
    cat("\n(Al seleccionar 'curva_general = TRUE', se omite el análisis estadístico comparativo ANOVA).\n")
    
  } else 
    vars_agrupacion_auc <- c(var_agrupacion_curva)
    if (incluir_replica && var_replica %in% names(df_work)) vars_agrupacion_auc <- c(vars_agrupacion_auc, var_replica)
    for (f in factores_agrupacion) if (f %in% names(df_work)) vars_agrupacion_auc <- c(vars_agrupacion_auc, f)
    
    df_auc <- df_work %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(vars_agrupacion_auc))) %>%
      dplyr::summarise(AUC = calc_auc(tiempo_num, .data[[v_conteo_auc]]), .groups = 'drop') %>%
      dplyr::filter(!is.na(AUC))
    
    # ===========================================================================
    # 5.3 MODELADO ESTADÍSTICO DEL AUC CON LA MISMA ESTRUCTURA QUE EL MODELO PRINCIPAL
    # ===========================================================================
    cat("\n--- Analizando el Área Bajo la Curva (AUC) con estructura consistente ---\n")
    
    # --- CONSTRUCCIÓN DE LA PARTE ALEATORIA (IDÉNTICA A LA DEL MÓDULO 5) ---
    
    # Si hay efectos aleatorios personalizados, los usamos; si no, los construimos automáticamente.
    if (!is.null(efectos_aleatorios_custom) && efectos_aleatorios_custom != "") {
      # Opción personalizada
      formula_random_auc <- efectos_aleatorios_custom
      mensaje_estructura_auc <- paste("Estructura Mixta (Custom):", efectos_aleatorios_custom)
    } else {
      # Construcción automática basada en réplica y factores de agrupación
      componentes_random_auc <- c()
      
      # 1. Añadir réplica si está activa y existe en df_auc
      if(incluir_replica && (var_replica %in% names(df_auc))) {
        componentes_random_auc <- c(componentes_random_auc, paste("(1 |", var_replica, ")"))
      }
      
      # 2. Añadir factores de agrupación anidados (ej: INGENIO/ENSAYO)
      if(length(factores_agrupacion) > 0) {
        # Solo aquellos que realmente existen en df_auc
        factores_existentes <- factores_agrupacion[factores_agrupacion %in% names(df_auc)]
        if(length(factores_existentes) > 0) {
          nested_str <- paste(factores_existentes, collapse = "/")
          componentes_random_auc <- c(componentes_random_auc, paste("(1 |", nested_str, ")"))
        }
      }
      
      if(length(componentes_random_auc) > 0) {
        formula_random_auc <- paste(componentes_random_auc, collapse = " + ")
        mensaje_estructura_auc <- paste("Estructura Mixta Default:", paste(componentes_random_auc, collapse = " y "))
      } else {
        formula_random_auc <- "1"  # Solo intercepto (modelo de efectos fijos)
        mensaje_estructura_auc <- "Modelo de Efectos Fijos (sin parte aleatoria)"
      }
    }
    
    # Construir la fórmula completa: efectos fijos (tratamiento) + efectos aleatorios (si los hay)
    if (formula_random_auc != "1") {
      formula_auc_completa <- as.formula(paste("AUC ~", var_agrupacion_curva, "+", formula_random_auc))
    } else {
      # Si no hay efectos aleatorios, usamos un modelo lineal simple (lm)
      formula_auc_completa <- as.formula(paste("AUC ~", var_agrupacion_curva))
    }
    
    cat("\n[>] Estructura del modelo para AUC:\n")
    cat("    ", mensaje_estructura_auc, "\n")
    cat("    Fórmula completa:", deparse(formula_auc_completa), "\n")
    
    # --- AJUSTE DEL MODELO (USANDO glmmTMB CON FAMILIA GAUSSIANA PARA CONSISTENCIA) ---
    # Si tiene efectos aleatorios, usamos glmmTMB (que ya está cargado) con familia gaussian.
    # Si no, usamos lm (que es más rápido y da los mismos resultados)
    if (formula_random_auc != "1") {
      modelo_auc <- glmmTMB(formula_auc_completa, family = gaussian, data = df_auc)
      # Para ANOVA y medias, usamos car::Anova y emmeans (compatible con glmmTMB)
      anova_auc <- Anova(modelo_auc, type = "II")
      # Extraer el p-valor del tratamiento principal (fila correspondiente)
      pval_auc <- anova_auc[rownames(anova_auc) == var_agrupacion_curva, "Pr(>Chisq)"]
      if(is.na(pval_auc) || is.null(pval_auc)) {
        # Si no lo encuentra, intentamos con el primer término
        pval_auc <- anova_auc[1, "Pr(>Chisq)"]
      }
    } else {
      modelo_auc <- lm(formula_auc_completa, data = df_auc)
      anova_auc <- Anova(modelo_auc, type = "II")
      pval_auc <- anova_auc[rownames(anova_auc) == var_agrupacion_curva, "Pr(>F)"]
      if(is.na(pval_auc) || is.null(pval_auc)) {
        pval_auc <- anova_auc[1, "Pr(>F)"]
      }
    }
    
    # --- OBTENER MEDIAS AJUSTADAS Y LETRAS (con emmeans) ---
    medias_auc <- emmeans(modelo_auc, as.formula(paste("~", var_agrupacion_curva)), type = "response")
    letras_auc <- cld(medias_auc, alpha = 0.15, Letters = letters, adjust = "tukey", reversed = letra_a_en_mayor_media)
    letras_auc$.group <- trimws(letras_auc$.group)
    
    # Preparar tabla resumen
    df_medias_auc <- as.data.frame(letras_auc) %>%
      dplyr::select(!!sym(var_agrupacion_curva), emmean, SE, .group) %>%
      dplyr::rename(Categoria = !!sym(var_agrupacion_curva), Media_AUC = emmean, Error_Estandar = SE, Grupo = .group) %>%
      dplyr::mutate(Media_AUC = round(Media_AUC, 2), Error_Estandar = round(Error_Estandar, 2)) %>%
      dplyr::arrange(Media_AUC)
    
    cat("\n--- Tabla Resumen de AUC (Con Grupos de Significancia) ---\n")
    print(df_medias_auc, row.names = FALSE)
    
    # ===========================================================================
    # 5.4 GRÁFICO DE BARRAS PARA AUC CON LETRAS
    # ===========================================================================
    plot_auc_barras <- ggplot(df_medias_auc, aes(x = reorder(Categoria, Media_AUC), y = Media_AUC, fill = Categoria)) +
      geom_col(color = "white", alpha = 0.85, width = 0.6) +
      geom_errorbar(aes(ymin = Media_AUC - Error_Estandar, ymax = Media_AUC + Error_Estandar), width = 0.15, linewidth = 0.7, color = "#444444") +
      geom_text(aes(y = Media_AUC + Error_Estandar, label = Grupo), hjust = -0.5, fontface = "bold", size = 4) +
      coord_flip() + 
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 15),
        legend.position = "none"
      ) +
      labs(title = paste("Área Bajo la Curva (AUC) -", v_conteo_auc),
           subtitle = paste("Modelo consistente con estructura definida | p-valor =", formatear_pval(pval_auc)),  # formatear_pval(pval_auc)
           x = var_agrupacion_curva,
           y = "Unidades de AUC (Insectos x Días)")
    
    print(plot_auc_barras)
    
    # ===========================================================================
    # 5.5 INTERPRETACIÓN AUTOMÁTICA
    # ===========================================================================
    cat("\n[>] INTERPRETACIÓN DEL ÁREA BAJO LA CURVA (AUC):\n")
    cat("El Área Bajo la Curva (AUC) cuantifica la presión poblacional acumulada.\n")
    cat("   - Estructura utilizada:", mensaje_estructura_auc, "\n")
    if (!is.na(pval_auc) && pval_auc < 0.15) { 
      cat(sprintf("\n-> RESULTADO: Diferencias ESTADÍSTICAMENTE SIGNIFICATIVAS (p = %.4f) en el impacto total acumulado.\n", pval_auc))
    } else {
      cat(sprintf("\n-> RESULTADO: NO se encontraron diferencias significativas (p = %.4f) en el AUC acumulado.\n", pval_auc))
    }

    
# =============================================================================
# 6. MÓDULO DE GRÁFICOS GRUPALES (RENDIMIENTO VS CONSISTENCIA)
# =============================================================================
names(df)

cat("\n===================================================================\n")
cat(">>> MÓDULO DE GRÁFICOS GRUPALES: ESTABILIDAD Y EFICACIA <<<\n")
cat("===================================================================\n")

if (!require(ggrepel)) { install.packages("ggrepel"); library(ggrepel) }
if (!require(stringr)) { install.packages("stringr"); library(stringr) }
if (!require(gridExtra)) { install.packages("gridExtra"); library(gridExtra) }

# --- INTERRUPTORES ESPECÍFICOS DEL MÓDULO ---
generar_graficos_cuadrantes <- TRUE  # TRUE = Generar gráficos, FALSE = Omitir módulo

# Columnas de agrupación para separar los gráficos (Ej: INGENIO, ENSAYO, FINCA)
var_graf_grupo1 <- "INGENIO" 
var_graf_grupo2 <- "ENSAYO"  # Puede ser NULL si solo quieres agrupar por 1 nivel
var_graf_ia     <- "Ingrediente.activo" # Nombre de tu columna de Ingredientes Activos

# --- CONEXIÓN AUTOMÁTICA CON EL SCRIPT PRINCIPAL ---
# Hereda el tratamiento y la variable de evaluación directamente de tus configuraciones iniciales
var_graf_prod   <- var_tratamiento      
var_graf_eval   <- variables_conteo[1]  
# ---------------------------------------------------

if(generar_graficos_cuadrantes && var_graf_eval %in% names(df)) 
  
  cat("========== PREPARANDO DATOS PARA GRÁFICOS ==========\n")
  
  # Usamos una copia limpia para no alterar los datos del modelo estadístico
  df_clean <- df
  
  # Estandarización de nombres de productos y evaluación
  df_clean[[var_graf_prod]] <- trimws(toupper(as.character(df_clean[[var_graf_prod]])))
  df_clean[[var_graf_eval]] <- suppressWarnings(as.numeric(as.character(df_clean[[var_graf_eval]])))
  
  # Preparación y limpieza de Ingredientes Activos
  if (var_graf_ia %in% names(df_clean)) {
    df_clean[[var_graf_ia]] <- trimws(toupper(as.character(df_clean[[var_graf_ia]])))
    df_clean[[var_graf_ia]] <- ifelse(is.na(df_clean[[var_graf_ia]]) | df_clean[[var_graf_ia]] == "", 
                                      "NO ESPECIFICADO", df_clean[[var_graf_ia]])
    # Reemplaza el IA del testigo basándose en el interruptor principal
    if(!is.null(nombre_testigo)) {
      df_clean[[var_graf_ia]] <- ifelse(df_clean[[var_graf_prod]] == toupper(nombre_testigo), "SIN APLICACION", df_clean[[var_graf_ia]])
    }
  } else {
    df_clean[[var_graf_ia]] <- "NO ESPECIFICADO"
  }
  
  # Filtros de limpieza (Quitar NAs en la evaluación y tratamientos vacíos)
  df_clean <- df_clean %>%
    dplyr::filter(!is.na(.data[[var_graf_eval]]), 
                  !is.na(.data[[var_graf_prod]]), 
                  .data[[var_graf_prod]] != "")
  
  # Crear variable de COMBINACION dinámica respetando los interruptores de agrupación
  if(!is.null(var_graf_grupo1) && var_graf_grupo1 %in% names(df_clean)) {
    df_clean <- df_clean %>% dplyr::filter(!is.na(.data[[var_graf_grupo1]]), .data[[var_graf_grupo1]] != "")
    comb_str <- as.character(df_clean[[var_graf_grupo1]])
    
    if(!is.null(var_graf_grupo2) && var_graf_grupo2 %in% names(df_clean)) {
      df_clean <- df_clean %>% dplyr::filter(!is.na(.data[[var_graf_grupo2]]), .data[[var_graf_grupo2]] != "")
      comb_str <- paste(comb_str, as.character(df_clean[[var_graf_grupo2]]), sep = " - ")
    }
    df_clean$COMBINACION <- comb_str
  } else {
    df_clean$COMBINACION <- "GLOBAL"
  }
  
  # <--- CAMBIO: Definir el ingrediente activo como factor con niveles globales
  df_clean[[var_graf_ia]] <- factor(df_clean[[var_graf_ia]], 
                                    levels = unique(df_clean[[var_graf_ia]]))
  
  combinaciones <- unique(df_clean$COMBINACION)
  cat("\nCombinaciones disponibles para graficar:\n")
  print(combinaciones)
  
  # ===========================================================================
  # FUNCIÓN CENTRALIZADA PARA CREAR GRÁFICOS DE CUADRANTES
  # ===========================================================================
  # <--- CAMBIO: Añadido parámetro legend_pos para controlar la leyenda
  crear_grafico_cuadrante <- function(datos, combinacion_actual, var_etiqueta, titulo_base, 
                                      color_col = var_etiqueta, legend_pos = "right") {
    df_filt <- datos %>% dplyr::filter(COMBINACION == combinacion_actual)
    if (nrow(df_filt) == 0) return(NULL)
    
    df_rank <- df_filt %>%
      dplyr::group_by(Etiqueta = .data[[var_etiqueta]]) %>%
      dplyr::summarise(
        Media_Eval = mean(.data[[var_graf_eval]], na.rm = TRUE),
        SD_Eval = sd(.data[[var_graf_eval]], na.rm = TRUE),
        N = n(),
        .groups = 'drop'
      ) %>%
      dplyr::mutate(
        SE_Eval = ifelse(is.na(SD_Eval) | N < 2, 0, SD_Eval / sqrt(N)),
        Consistencia = ifelse(SE_Eval == 0 | is.na(SE_Eval), 1, 1 / (SE_Eval + 0.001))
      )
    
    # Unimos el color asignado a cada etiqueta
    color_data <- df_filt %>%
      dplyr::select(Etiqueta = .data[[var_etiqueta]], Color = .data[[color_col]]) %>%
      dplyr::distinct()
    
    df_rank <- df_rank %>%
      dplyr::left_join(color_data, by = "Etiqueta")
    
    corte_x <- mean(df_rank$Media_Eval, na.rm = TRUE)
    corte_y <- mean(df_rank$SE_Eval, na.rm = TRUE)
    
    p <- ggplot(df_rank, aes(x = Media_Eval, y = SE_Eval, color = Color)) +
      geom_vline(xintercept = corte_x, linetype = "dashed", color = "red", alpha = 0.6) +
      geom_hline(yintercept = corte_y, linetype = "dashed", color = "red", alpha = 0.6) +
      geom_point(aes(size = Consistencia), alpha = 0.8) +
      geom_text_repel(
        aes(label = stringr::str_wrap(Etiqueta, width = 25)),
        size = 4, fontface = "bold", show.legend = FALSE,
        max.overlaps = 30, box.padding = 0.5, point.padding = 0.3
      ) +
      scale_color_discrete(name = var_graf_ia) + 
      theme_bw(base_size = 13) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        legend.position = legend_pos  # <--- CAMBIO: usado el parámetro
      ) +
      labs(
        title = paste(titulo_base, "-", combinacion_actual),
        subtitle = paste0("Cuadrante Inf. Izq = Mejores y Más Estables | n = ", nrow(df_filt)),
        x = paste("Media de", var_graf_eval),
        y = "Error Estándar (Inestabilidad)"
      )
    return(p)
  }
  
  # ===========================================================================
  # GENERACIÓN DE GRÁFICOS
  # ===========================================================================
  cat("\n========== GENERANDO GRÁFICOS SEPARADOS ==========\n")
  
  graficos_productos <- list()
  graficos_ia <- list()
  
  for (combo in combinaciones) {
    cat("\nGenerando gráficos para:", combo)
    
    # 1. Gráfico por Tratamiento Comercial (con leyenda, por defecto "right")
    p_prod <- crear_grafico_cuadrante(df_clean, combo, var_graf_prod, "PRODUCTOS COMERCIALES", 
                                      color_col = var_graf_ia, legend_pos = "none")  # Cambio: leyenda a la derecha "left", "right", "none" para desactivar la leyenda usar none
    if (!is.null(p_prod)) {
      graficos_productos[[combo]] <- p_prod
      dev.new(width = 10, height = 8)
      print(p_prod)
      cat(" [Prod OK]")
    }
    
    # 2. Gráfico por Ingrediente Activo (sin leyenda)
    if (var_graf_ia %in% names(df_clean)) {
      p_ia <- crear_grafico_cuadrante(df_clean, combo, var_graf_ia, "INGREDIENTES ACTIVOS", 
                                      color_col = var_graf_ia, legend_pos = "none")  # <--- CAMBIO: "left", "right", "none"
      if (!is.null(p_ia)) {
        graficos_ia[[combo]] <- p_ia
        dev.new(width = 10, height = 8)
        print(p_ia)
        cat(" [IA OK]")
      }
    }
  }
  
  cat("\n\n========== GRÁFICOS GENERADOS ==========\n")
  cat("Total de gráficos de productos:", length(graficos_productos), "\n")
  cat("Total de gráficos de I.A.:", length(graficos_ia), "\n")
  
 
# =============================================================================
# 7. MÓDULO OBSERVACIONAL: CURVAS Y AUC (SIN DISEÑO EXPERIMENTAL)
# =============================================================================
cat("\n===================================================================\n")
cat(">>> MÓDULO DE CURVAS Y AUC PARA DATOS OBSERVACIONALES <<<\n")
cat("===================================================================\n")

if (!require(plotly)) { install.packages("plotly"); library(plotly) }
if (!require(dplyr)) { install.packages("dplyr"); library(dplyr) }
if (!require(ggplot2)) { install.packages("ggplot2"); library(ggplot2) }

# Aseguramos el uso del dataframe correcto según tu entorno
df_base <- if(exists("datos")) datos else df 
names(df)

# --- CONFIGURACIÓN DEL MÓDULO OBSERVACIONAL ---
# 1. Variable temporal: Puede ser fecha ("FECHA.MUESTREO") o número correlativo ("Numero.muestreo")
var_tiempo_obs     <- "fecha.muestreo" 
es_fecha           <- TRUE  # TRUE = Usa formato fecha (dd/mm/yyyy). FALSE = Usa números simples.

# >>> NUEVO INTERRUPTOR: CURVA GENERAL VS AGRUPADA <<<
curva_general_obs  <- FALSE   # TRUE = Una sola curva global. FALSE = Curvas separadas por grupo.

# 2. Variable de agrupación (Ej. Finca, Lote, Variedad) - Se usará si curva_general_obs es FALSE
var_grupo_obs      <- "INGENIO" 

# 3. Facetado (Para separar gráficas por algún macro-factor, ej: INGENIO)
usar_facetado      <- FALSE       # Cambiar a FALSE para apagar las mini-ventanas.
var_facetado       <- "INGENIO"  # Factor para facetar (Solo funciona si usar_facetado es TRUE).

# 4. Variable a graficar (Hereda la principal automáticamente)
v_conteo_obs       <- variables_conteo[1] 

if (v_conteo_obs %in% names(df_base) && var_tiempo_obs %in% names(df_base)) {
  
  cat("========== PROCESANDO DATOS OBSERVACIONALES ==========\n")
  
  # Filtramos NAs de la variable a evaluar
  df_obs <- df_base %>% dplyr::filter(!is.na(.data[[v_conteo_obs]]))
  
  # ===========================================================================
  # 7.1 PROCESAMIENTO DINÁMICO DEL TIEMPO
  # ===========================================================================
  if (es_fecha) {
    df_obs$Tiempo_Num <- as.Date(as.character(df_obs[[var_tiempo_obs]]), 
                                 tryFormats = c("%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d", "%d-%m-%Y"))
    if(any(is.na(df_obs$Tiempo_Num))) warning("Algunas fechas no se reconocieron correctamente.")
    df_obs$Tiempo_Calc <- as.numeric(df_obs$Tiempo_Num) - min(as.numeric(df_obs$Tiempo_Num), na.rm = TRUE)
  } else {
    df_obs$Tiempo_Num <- suppressWarnings(as.numeric(as.character(df_obs[[var_tiempo_obs]])))
    df_obs <- df_obs %>% dplyr::filter(!is.na(Tiempo_Num))
    df_obs$Tiempo_Calc <- df_obs$Tiempo_Num
  }
  
  # ===========================================================================
  # 7.2 PREPARACIÓN DE DATOS Y GRÁFICO DE CURVAS
  # ===========================================================================
  if (curva_general_obs) {
    # OPCIÓN A: CURVA GENERAL (Licúa todas las fincas)
    vars_agrupacion <- c("Tiempo_Num", "Tiempo_Calc")
    if (usar_facetado && var_facetado %in% names(df_obs)) vars_agrupacion <- c(var_facetado, vars_agrupacion)
    
    df_curve_obs <- df_obs %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(vars_agrupacion))) %>%
      dplyr::summarise(Media_Conteo = mean(.data[[v_conteo_obs]], na.rm = TRUE), .groups = 'drop') %>%
      dplyr::arrange(Tiempo_Num)
    
    plot_obs <- ggplot(df_curve_obs, aes(x = Tiempo_Num, y = Media_Conteo)) +
      geom_point(size = 3, alpha = 0.9, color = "#2980B9")
    
    # Trazar línea solo si hay suficientes puntos
    if (usar_facetado && var_facetado %in% names(df_obs)) {
      df_lines_only <- df_curve_obs %>% dplyr::group_by(.data[[var_facetado]]) %>% dplyr::filter(dplyr::n() > 1)
      if (nrow(df_lines_only) > 0) plot_obs <- plot_obs + geom_line(data = df_lines_only, aes(group = .data[[var_facetado]]), color = "#2980B9", linewidth = 1.2, alpha = 0.8)
    } else {
      if (nrow(df_curve_obs) > 1) plot_obs <- plot_obs + geom_line(group = 1, color = "#2980B9", linewidth = 1.2, alpha = 0.8)
    }
    
  } else {
    # OPCIÓN B: CURVAS SEPARADAS POR GRUPO
    vars_agrupacion <- c(var_grupo_obs, "Tiempo_Num", "Tiempo_Calc")
    if (usar_facetado && var_facetado %in% names(df_obs)) vars_agrupacion <- c(var_facetado, vars_agrupacion)
    
    df_curve_obs <- df_obs %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(vars_agrupacion))) %>%
      dplyr::summarise(Media_Conteo = mean(.data[[v_conteo_obs]], na.rm = TRUE), .groups = 'drop') %>%
      dplyr::arrange(Tiempo_Num)
    
    df_lines_only <- df_curve_obs %>% dplyr::group_by(.data[[var_grupo_obs]]) %>% dplyr::filter(dplyr::n() > 1) %>% dplyr::ungroup()
    
    plot_obs <- ggplot(df_curve_obs, aes(x = Tiempo_Num, y = Media_Conteo, color = .data[[var_grupo_obs]])) +
      geom_point(size = 3, alpha = 0.9)
    
    if (nrow(df_lines_only) > 0) {
      plot_obs <- plot_obs + geom_line(data = df_lines_only, aes(group = .data[[var_grupo_obs]]), linewidth = 1.2, alpha = 0.8)
    }
  }
  
  # Estética base compartida
  plot_obs <- plot_obs +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
      plot.title = element_text(face = "bold", size = 15, color = "#333333"),
      axis.title = element_text(face = "bold", color = "#555555"),
      axis.text.x = element_text(angle = 45, hjust = 1, color = "#333333"),
      legend.title = element_text(face = "bold")
    ) +
    labs(title = paste("Dinámica Poblacional Observacional -", v_conteo_obs),
         x = ifelse(es_fecha, "Fecha de Evaluación", "Número de Muestreo / Evaluación"),
         y = paste("Promedio de", v_conteo_obs))
  
  if (!curva_general_obs) plot_obs <- plot_obs + labs(color = var_grupo_obs)
  
  if (es_fecha) {
    plot_obs <- plot_obs + scale_x_date(date_labels = "%d/%m/%Y", breaks = unique(df_curve_obs$Tiempo_Num))
  } else {
    plot_obs <- plot_obs + scale_x_continuous(breaks = unique(df_curve_obs$Tiempo_Num))
  }
  
  if (usar_facetado && var_facetado %in% names(df_obs)) {
    plot_obs <- plot_obs + facet_wrap(as.formula(paste("~", var_facetado)), scales = "free_y")
  }
  
  print(ggplotly(plot_obs, tooltip = c("x", "y", "color")) %>%
          layout(legend = list(orientation = "h", x = 0, y = -0.25)))
  
  # ===========================================================================
  # 7.3 CÁLCULO Y GRÁFICO DEL ÁREA BAJO LA CURVA (AUC) DIRECTO
  # ===========================================================================
  cat("\n--- Cálculo del Área Bajo la Curva (AUC) Observacional ---\n")
  
  calc_auc_obs <- function(x, y) {
    dt <- na.omit(data.frame(x=x, y=y))
    if(nrow(dt) < 2) return(NA)
    dt <- dt[order(dt$x), ]
    sum(diff(dt$x) * (dt$y[-1] + dt$y[-nrow(dt)]) / 2, na.rm = TRUE)
  }
  
  if (curva_general_obs) {
    vars_auc <- c()
    if (usar_facetado && var_facetado %in% names(df_obs)) vars_auc <- c(var_facetado)
    
    if (length(vars_auc) > 0) {
      df_auc_obs <- df_curve_obs %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(vars_auc))) %>%
        dplyr::summarise(AUC = calc_auc_obs(Tiempo_Calc, Media_Conteo), .groups = 'drop') %>%
        dplyr::filter(!is.na(AUC))
    } else {
      auc_total <- calc_auc_obs(df_curve_obs$Tiempo_Calc, df_curve_obs$Media_Conteo)
      df_auc_obs <- data.frame(Global = "Total General", AUC = auc_total) %>% dplyr::filter(!is.na(AUC))
    }
  } else {
    vars_auc <- c(var_grupo_obs)
    if (usar_facetado && var_facetado %in% names(df_obs)) vars_auc <- c(var_facetado, vars_auc)
    
    df_auc_obs <- df_curve_obs %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(vars_auc))) %>%
      dplyr::summarise(AUC = calc_auc_obs(Tiempo_Calc, Media_Conteo), .groups = 'drop') %>%
      dplyr::filter(!is.na(AUC))
  }
  
  if (nrow(df_auc_obs) > 0) {
    df_auc_obs <- df_auc_obs %>% dplyr::arrange(desc(AUC))
    df_auc_obs$AUC <- round(df_auc_obs$AUC, 2)
    
    cat("\n[>] Resultados del AUC (Ordenados de mayor a menor impacto):\n")
    print(as.data.frame(df_auc_obs))
    
    # Preparamos la variable del eje Y según el caso
    eje_y_auc <- if (curva_general_obs && length(vars_auc) == 0) "Global" else if (curva_general_obs) var_facetado else var_grupo_obs
    
    plot_auc_obs <- ggplot(df_auc_obs, aes(x = reorder(.data[[eje_y_auc]], AUC), y = AUC, fill = .data[[eje_y_auc]])) +
      geom_col(color = "white", alpha = 0.85, width = 0.6) +
      geom_text(aes(label = AUC), hjust = -0.2, fontface = "bold", size = 4) +
      coord_flip() +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 15),
        legend.position = "none"
      ) +
      labs(title = paste("Área Bajo la Curva (AUC) -", v_conteo_obs),
           subtitle = "Impacto acumulado real (Sin comparaciones estadísticas)",
           x = eje_y_auc, 
           y = ifelse(es_fecha, "Unidades de AUC (Plaga x Días)", "Unidades de AUC (Plaga x Intervalos de Muestreo)"))
    
    if (!curva_general_obs && usar_facetado && var_facetado %in% names(df_obs)) {
      plot_auc_obs <- plot_auc_obs + facet_wrap(as.formula(paste("~", var_facetado)))
    }
    
    print(plot_auc_obs)
  } else {
    cat("\n[!] No se pudo calcular el AUC. Hacen falta más evaluaciones temporales.\n")
  }
  
} else {
  cat("\n[!] ERROR: No se encontró la variable de tiempo '", var_tiempo_obs, "' o no hay variables de conteo definidas.\n")
}
cat("===================================================================\n")



# =============================================================================
# 8. MÓDULO DE DISTRIBUCIÓN ESPACIAL Y PATRÓN DE AGREGACIÓN (SIN COORDENADAS)
# =============================================================================
cat("\n===================================================================\n")
cat(">>> MÓDULO DE PATRÓN DE DISTRIBUCIÓN Y HACINAMIENTO DE LA PLAGA <<<\n")
cat("===================================================================\n")

if (!require(dplyr)) { install.packages("dplyr"); library(dplyr) }
if (!require(ggplot2)) { install.packages("ggplot2"); library(ggplot2) }
if (!require(ggrepel)) { install.packages("ggrepel"); library(ggrepel) }

# Aseguramos el uso del dataframe de análisis

df_base <- df

# --- CONFIGURACIÓN DEL MÓDULO DE DISTRIBUCIÓN ---
var_eval_dist       <- variables_conteo[1]  
var_agrupacion_dist <- var_tratamiento      

# >>> INTERRUPTOR DE MÉTODO GRÁFICO DE ÍNDICES <<<
# Opciones: "VMR", "LLOYD", "MORISITA", "GREEN", "K_BINNEG" o "TODOS"
metodo_grafico      <- "TODOS" 

# FORZAR CONVERSIÓN NUMÉRICA
if (var_eval_dist %in% names(df_base)) {
  df_base[[var_eval_dist]] <- suppressWarnings(as.numeric(as.character(df_base[[var_eval_dist]])))
  df_dist <- df_base %>% dplyr::filter(!is.na(.data[[var_eval_dist]]))
  
  # ===========================================================================
  # 8.1 FUNCIÓN MATEMÁTICA MULTI-ÍNDICE (ECOLÓGICA COMPLETA)
  # ===========================================================================
  calc_dispersion <- function(x) {
    n <- length(x)
    sum_x <- sum(x, na.rm = TRUE)
    sum_x2 <- sum(x^2, na.rm = TRUE)
    
    if (n < 2 || sum_x == 0) {
      return(data.frame(N = n, Media = mean(x, na.rm=TRUE), Varianza = var(x, na.rm=TRUE), 
                        VMR = NA, Lloyd_LIP = NA, Morisita = NA, Green = NA, K_BinNeg = NA,
                        Distribucion = "Ausencia / Insuficiente"))
    }
    
    m <- mean(x, na.rm = TRUE)
    v <- var(x, na.rm = TRUE)
    
    # 1. VMR (Índice de Dispersión) y Chi-Cuadrado
    vmr <- v / m
    chi_val <- (n - 1) * vmr
    p_val <- 2 * min(pchisq(chi_val, df = n - 1, lower.tail = FALSE), pchisq(chi_val, df = n - 1, lower.tail = TRUE))
    
    # 2. Lloyd (Índice de Parches / Hacinamiento)
    lip <- (m + (vmr - 1)) / m
    
    # 3. Morisita (Independiente de densidad)
    morisita <- if (sum_x > 1) n * (sum_x2 - sum_x) / ((sum_x^2) - sum_x) else NA
    
    # 4. Coeficiente de Green (Rango normalizado de 0 a 1 para agregación)
    green <- if (sum_x > 1) (vmr - 1) / (sum_x - 1) else NA
    
    # 5. Parámetro K de la Binomial Negativa (Menor = Más agregado)
    k_est <- if (v > m && m > 0) (m^2) / (v - m) else NA
    
    # Clasificación Ecológica Estadística
    if (p_val >= 0.05) {
      tipo <- "Aleatoria (Aislados)"
    } else if (vmr > 1) {
      tipo <- "Focos (Agregada)"
    } else {
      tipo <- "Uniforme (Regular)"
    }
    
    return(data.frame(N = n, 
                      Media = round(m, 2), 
                      Varianza = round(v, 2),
                      VMR = round(vmr, 2),
                      Lloyd_LIP = round(lip, 2),
                      Morisita = round(morisita, 2),
                      Green = round(green, 4),
                      K_BinNeg = round(k_est, 2),
                      Distribucion = tipo))
  }
  
  # ===========================================================================
  # 8.2 ANÁLISIS GLOBAL DEL ENSAYO
  # ===========================================================================
  cat("\n--- 1. COMPORTAMIENTO ECOLÓGICO GLOBAL (TODO EL ENSAYO) ---\n")
  res_glob <- calc_dispersion(df_dist[[var_eval_dist]])
  print(res_glob)
  
  # ===========================================================================
  # 8.3 ANÁLISIS DESGLOSADO POR TRATAMIENTO
  # ===========================================================================
  cat("\n--- 2. CUADRO MULTIVARIADO POR TRATAMIENTO ---\n")
  
  res_dist_trt <- df_dist %>%
    dplyr::group_by(Tratamiento = .data[[var_agrupacion_dist]]) %>%
    dplyr::do(calc_dispersion(.[[var_eval_dist]])) %>%
    as.data.frame()
  
  # Lógica de ordenamiento para la tabla
  col_orden <- "Morisita" 
  if (metodo_grafico == "VMR") col_orden <- "VMR"
  if (metodo_grafico == "LLOYD") col_orden <- "Lloyd_LIP"
  if (metodo_grafico == "GREEN") col_orden <- "Green"
  if (metodo_grafico == "K_BINNEG") col_orden <- "K_BinNeg"
  
  # K_BinNeg se ordena ascendente (menor es peor foco), los demás descendentes
  if (col_orden == "K_BinNeg") {
    res_dist_trt <- res_dist_trt %>% dplyr::arrange(.data[[col_orden]])
  } else {
    res_dist_trt <- res_dist_trt %>% dplyr::arrange(desc(.data[[col_orden]]))
  }
  
  print(res_dist_trt)
  
  # ===========================================================================
  # 8.4 GRÁFICO FUNDAMENTAL 1: PERFIL FÍSICO DEL FOCO (VIOLÍN + JITTER)
  # ===========================================================================
  plot_violin <- ggplot(df_dist, aes(x = reorder(.data[[var_agrupacion_dist]], .data[[var_eval_dist]], FUN = mean), 
                                     y = .data[[var_eval_dist]], fill = .data[[var_agrupacion_dist]])) +
    geom_violin(alpha = 0.5, trim = FALSE, color = "darkgray") +
    geom_jitter(width = 0.2, alpha = 0.6, color = "#2C3E50", size = 2) +
    coord_flip() +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(linetype = "dashed"),
      plot.title = element_text(face = "bold", size = 15),
      legend.position = "none"
    ) +
    labs(title = paste("Morfología Espacial de la Plaga -", var_eval_dist),
         subtitle = "Puntos dispersos hacia arriba demuestran plantas foco (sobre-infestadas).",
         x = "Tratamientos", 
         y = paste("Conteo Real de", var_eval_dist))
  
  print(plot_violin)
  
  # ===========================================================================
  # 8.5 GRÁFICO FUNDAMENTAL 2: RELACIÓN VARIANZA VS MEDIA (BASE DE LOS MÉTODOS)
  # ===========================================================================
  res_dist_graf_var <- res_dist_trt %>% dplyr::filter(!is.na(VMR))
  
  plot_var_mean <- ggplot(res_dist_graf_var, aes(x = Media, y = Varianza, color = Distribucion, label = Tratamiento)) +
    geom_point(size = 4, alpha = 0.8) +
    # Línea 1:1 (Límite de distribución Aleatoria / Poisson)
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 1.2) +
    geom_text_repel(size = 3.5, fontface = "bold", show.legend = FALSE, box.padding = 0.5, max.overlaps = 20) +
    scale_color_manual(values = c("Focos (Agregada)" = "#E74C3C", 
                                  "Aleatoria (Aislados)" = "#2ECC71", 
                                  "Uniforme (Regular)" = "#3498DB",
                                  "Ausencia / Insuficiente" = "#95A5A6")) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
      plot.title = element_text(face = "bold", size = 15),
      legend.position = "bottom",
      legend.title = element_blank()
    ) +
    labs(title = paste("Relación Varianza vs Media -", var_eval_dist),
         subtitle = "Tratamientos por encima de la línea roja (1:1) indican infestación focalizada.",
         x = "Media (Densidad poblacional)", 
         y = "Varianza (Dispersión)")
  
  print(plot_var_mean)
  
  # ===========================================================================
  # 8.6 GENERADOR DINÁMICO DE GRÁFICOS DE ÍNDICES ESPECÍFICOS
  # ===========================================================================
  generar_grafico_indice <- function(datos, variable_y, titulo_principal, subtitulo, linea_corte, invertir_orden = FALSE) {
    df_graf <- datos %>% dplyr::filter(!is.na(.data[[variable_y]]))
    
    if(invertir_orden) {
      df_graf$Tratamiento <- factor(df_graf$Tratamiento, levels = df_graf$Tratamiento[order(df_graf[[variable_y]], decreasing = TRUE)])
    } else {
      df_graf$Tratamiento <- factor(df_graf$Tratamiento, levels = df_graf$Tratamiento[order(df_graf[[variable_y]])])
    }
    
    p <- ggplot(df_graf, aes(x = Tratamiento, y = .data[[variable_y]], fill = Distribucion)) +
      geom_col(color = "black", alpha = 0.85, width = 0.6) +
      geom_hline(yintercept = linea_corte, linetype = "dashed", color = "red", linewidth = 1.2) +
      geom_text(aes(label = .data[[variable_y]]), hjust = -0.3, fontface = "bold", size = 4) +
      coord_flip() +
      scale_fill_manual(values = c("Focos (Agregada)" = "#E74C3C", 
                                   "Aleatoria (Aislados)" = "#2ECC71", 
                                   "Uniforme (Regular)" = "#3498DB",
                                   "Ausencia / Insuficiente" = "#95A5A6")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 15),
        legend.position = "bottom"
      ) +
      labs(title = titulo_principal, subtitle = subtitulo, x = "Tratamientos", y = "Valor del Índice", fill = "Comportamiento Estadístico")
    return(p)
  }
  
  # Lógica del Interruptor de Gráficos
  metodos_a_graficar <- if(metodo_grafico == "TODOS") c("VMR", "LLOYD", "MORISITA", "GREEN", "K_BINNEG") else c(metodo_grafico)
  
  for (met in metodos_a_graficar) {
    if (met == "VMR") print(generar_grafico_indice(res_dist_trt, "VMR", paste("Dispersión Clásica (VMR) -", var_eval_dist), "Límite aleatorio = 1.0. Barras mayores indican agregación.", 1))
    if (met == "LLOYD") print(generar_grafico_indice(res_dist_trt, "Lloyd_LIP", paste("Índice de Parches (Lloyd) -", var_eval_dist), "Límite aleatorio = 1.0. Mide hacinamiento.", 1))
    if (met == "MORISITA") print(generar_grafico_indice(res_dist_trt, "Morisita", paste("Índice de Morisita -", var_eval_dist), "Límite aleatorio = 1.0. Independiente de tamaño de muestra.", 1))
    if (met == "GREEN") print(generar_grafico_indice(res_dist_trt, "Green", paste("Coeficiente de Green -", var_eval_dist), "Límite aleatorio = 0. Acercarse a 1.0 es máxima agregación.", 0))
    if (met == "K_BINNEG") print(generar_grafico_indice(res_dist_trt, "K_BinNeg", paste("Parámetro K (Binomial Negativa) -", var_eval_dist), "Línea en 8.0. ATENCIÓN: Valores MENORES a 8 indican FOCOS FUERTES.", 8, invertir_orden = TRUE))
  }
  
  # ===========================================================================
  # 8.7 GUÍA DE INTERPRETACIÓN DE LOS NUEVOS ÍNDICES
  # ===========================================================================
  cat("\n[>] GUÍA ECOLÓGICA E INTERPRETACIÓN FINAL:\n")
  cat("  - GRÁFICO VARIANZA VS MEDIA: La base visual de todos los métodos. Todo punto sobre la línea roja es un foco.\n")
  cat("  - MORISITA: El más confiable. Si baja a 1.0, el tratamiento eliminó la dinámica de foco.\n")
  cat("  - GREEN (Cx): Normalizado. 0 = Aleatorio, 1 = Foco perfecto. Ideal para comparar tratamientos dispares.\n")
  cat("  - K (Binomial Negativa): ¡Se lee al revés! \n")
  cat("      * K < 2: Plaga extremadamente focalizada (muy difícil de muestrear).\n")
  cat("      * K entre 2 y 8: Agregación moderada.\n")
  cat("      * K > 8: Plaga esparcida al azar.\n")
  
} else {
  cat("\n[!] ERROR: No se encontró la variable de conteo '", var_eval_dist, "' en la base de datos.\n")
}
cat("===================================================================\n")



# =============================================================================
# 9. MÓDULO DE IDENTIFICACIÓN ROBUSTA DE DATOS ATÍPICOS (OUTLIERS)
# =============================================================================
cat("\n===================================================================\n")
cat(">>> MÓDULO DE DETECCIÓN DE ATÍPICOS PARA CONTEOS DE PLAGAS <<<\n")
cat("===================================================================\n")

# --- CONFIGURACIÓN ESPECÍFICA DEL MÓDULO ---
# Hereda automáticamente la variable de plaga y el tratamiento principal
v_conteo_out  <- variables_conteo[1]
var_agrup_out <- var_tratamiento 

# Parámetro de rigor: 3 es el estándar para MAD, 3.5 es más conservador
umbral_mad    <- 3               

if (v_conteo_out %in% names(df) && var_agrup_out %in% names(df)) {
  
  cat(paste("Analizando valores atípicos para la variable:", v_conteo_out, "\n"))
  cat(paste("Agrupación base para cálculo de dispersión:", var_agrup_out, "\n"))
  
  # ===========================================================================
  # 9.1 CÁLCULO DE LÍMITES Y EXTRACCIÓN DE ATÍPICOS
  # ===========================================================================
  df_outliers <- df %>%
    dplyr::filter(!is.na(.data[[v_conteo_out]])) %>%
    dplyr::group_by(.data[[var_agrup_out]]) %>%
    dplyr::mutate(
      # MÉTODO A: MAD (Desviación Absoluta de la Mediana por Tratamiento)
      Mediana_Grp = median(.data[[v_conteo_out]], na.rm = TRUE),
      MAD_Grp     = mad(.data[[v_conteo_out]], constant = 1.4826, na.rm = TRUE),
      # Evitar división por cero si todos los datos de un grupo son idénticos
      Score_MAD   = ifelse(MAD_Grp == 0, 0, abs(.data[[v_conteo_out]] - Mediana_Grp) / MAD_Grp),
      Atipico_MAD = ifelse(Score_MAD > umbral_mad, "Sí", "No"),
      
      # MÉTODO B: IQR LOGARÍTMICO (Para conteos biológicos asimétricos)
      Log_Conteo  = log(.data[[v_conteo_out]] + 1),
      Q1_log      = quantile(Log_Conteo, 0.25, na.rm = TRUE),
      Q3_log      = quantile(Log_Conteo, 0.75, na.rm = TRUE),
      IQR_log     = Q3_log - Q1_log,
      Limite_Sup_Log = Q3_log + 1.5 * IQR_log,
      Limite_Sup_Real = exp(Limite_Sup_Log) - 1,
      Atipico_Log = ifelse(.data[[v_conteo_out]] > Limite_Sup_Real, "Sí", "No")
    ) %>%
    dplyr::ungroup()
  
  # Extraer y mostrar solo las filas marcadas como atípicas
  resumen_outliers <- df_outliers %>%
    dplyr::filter(Atipico_MAD == "Sí" | Atipico_Log == "Sí") %>%
    dplyr::select(all_of(c(var_agrup_out, v_conteo_out, "Score_MAD", "Atipico_MAD", "Atipico_Log"))) %>%
    dplyr::arrange(desc(Score_MAD))
  
  if (nrow(resumen_outliers) > 0) {
    cat(sprintf("\n[!] Se detectaron %d valores atípicos que exceden los límites biológicos normales:\n", nrow(resumen_outliers)))
    print(as.data.frame(resumen_outliers))
    cat("\nNota: Revisa si estos valores son focos reales de la plaga o errores de digitación antes de aplicar el GLMM.\n")
  } else {
    cat("\n[✓] No se detectaron valores atípicos extremos con los parámetros actuales.\n")
  }
  
  # ===========================================================================
  # 9.2 GRÁFICO DIAGNÓSTICO (BOXPLOT + JITTER)
  # ===========================================================================
  plot_outliers <- ggplot(df_outliers, aes(x = .data[[var_agrup_out]], y = .data[[v_conteo_out]])) +
    # Boxplot base transparente sin dibujar los outliers por defecto
    geom_boxplot(outlier.shape = NA, fill = "#E0E0E0", color = "#555555", width = 0.5, alpha = 0.7) +
    # Jitter para esparcir los puntos y destacar los atípicos según el Score MAD
    geom_jitter(aes(color = Atipico_MAD, size = Atipico_MAD), width = 0.2, alpha = 0.8) +
    scale_color_manual(values = c("No" = "#2980B9", "Sí" = "#E74C3C")) +
    scale_size_manual(values = c("No" = 1.5, "Sí" = 3.5)) +
    coord_flip() +
    theme_bw(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.y = element_text(face = "bold", color = "#333333"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    ) +
    labs(title = paste("Diagnóstico de Datos Atípicos (MAD) -", v_conteo_out),
         subtitle = paste("Umbral MAD =", umbral_mad, "| Comparación interna por", var_agrup_out),
         x = "Tratamientos",
         y = paste("Conteo absoluto de", v_conteo_out),
         color = "¿Es Atípico extremo?",
         size = "¿Es Atípico extremo?")
  
  try(print(plot_outliers), silent = TRUE)
  
} else {
  cat("\n[!] ERROR: La variable", v_conteo_out, "o", var_agrup_out, "no se encuentra en el dataframe.\n")
}
cat("===================================================================\n")



################################################### Otros módulos de análisis ########################


# Solución robusta para el error del dispositivo PNG en R
if(!require(Cairo)) { install.packages("Cairo"); library(Cairo) }
if(capabilities("cairo")) {
  options(bitmapType = "cairo")
}

# --- Cargar todas las librerías necesarias para los módulos ---
paquetes <- c("minpack.lm", "survival", "survminer", "viridis", "tidyr", 
              "dplyr", "ggplot2", "emmeans", "multcomp", "multcompView", "ggrepel", "boot")

for (p in paquetes) {
  if(!require(p, character.only = TRUE)) { 
    install.packages(p)
    library(p, character.only = TRUE) 
  }
}

# --- Reutilización de variables globales del script principal ---
names(datos)

var_tiempo_crec    <- var_tiempo       
es_fecha_crec      <- TRUE             
umbral_min_datos   <- 3                
modelo_crec        <- c("logistic", "gompertz") 


# =============================================================================
# MÓDULO 3.1: CURVAS DE CRECIMIENTO Y MODELOS DINÁMICOS CON LETRAS DE SIGNIFICANCIA
# =============================================================================
cat("\n===================================================================\n")
cat(">>> MÓDULO DE MODELOS DE CRECIMIENTO POBLACIONAL (Logístico y Gompertz) <<<\n")
cat("===================================================================\n")

# --- Preparación de datos ---
if(!exists("df_obs")) {
  df_obs_crec <- df %>% dplyr::filter(!is.na(.data[[var_tiempo_crec]]))
} else {
  df_obs_crec <- df_obs
}

if (!(var_tratamiento %in% names(df_obs_crec))) {
  stop(paste("La columna", var_tratamiento, "no existe en df_obs_crec."))
}

vars_id <- c(var_tratamiento)
if(exists("incluir_replica") && incluir_replica && exists("var_replica") && var_replica %in% names(df_obs_crec)) {
  vars_id <- c(vars_id, var_replica)
}
if(exists("factores_agrupacion") && length(factores_agrupacion) > 0) {
  vars_id <- c(vars_id, intersect(factores_agrupacion, names(df_obs_crec)))
}
df_obs_crec <- df_obs_crec %>%
  tidyr::unite("Unidad", dplyr::all_of(vars_id), remove = FALSE)

if(es_fecha_crec) {
  df_obs_crec$Fecha_Num <- as.Date(as.character(df_obs_crec[[var_tiempo_crec]]),
                                   tryFormats = c("%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d", "%d-%m-%Y"))
  df_obs_crec <- df_obs_crec %>% dplyr::filter(!is.na(Fecha_Num))
  df_obs_crec$t <- as.numeric(df_obs_crec$Fecha_Num - min(df_obs_crec$Fecha_Num, na.rm = TRUE))
} else {
  df_obs_crec$t <- as.numeric(as.character(df_obs_crec[[var_tiempo_crec]]))
}

v_conteo_crec <- variables_conteo[1]
if (!(v_conteo_crec %in% names(df_obs_crec))) {
  stop(paste("La variable", v_conteo_crec, "no existe en los datos."))
}

df_crec <- df_obs_crec %>%
  dplyr::select(Unidad, t, all_of(v_conteo_crec)) %>%
  na.omit()

# --- Función para ajustar modelos ---
ajustar_crecimiento <- function(datos_unidad) {
  if(nrow(datos_unidad) < umbral_min_datos) return(NULL)
  t <- datos_unidad$t
  y <- datos_unidad[[v_conteo_crec]]
  
  K_start <- max(y, na.rm = TRUE) * 1.2
  r_start <- 0.1
  tmid_start <- median(t, na.rm = TRUE)
  
  fit_log <- tryCatch(
    nlsLM(y ~ K/(1 + exp(-r*(t - tmid))),
          start = list(K = K_start, r = r_start, tmid = tmid_start),
          control = nls.lm.control(maxiter = 100)),
    error = function(e) NULL)
  
  fit_gomp <- tryCatch(
    nlsLM(y ~ K * exp(-exp(-r*(t - tmid))),
          start = list(K = K_start, r = r_start, tmid = tmid_start),
          control = nls.lm.control(maxiter = 100)),
    error = function(e) NULL)
  
  list(logistic = fit_log, gompertz = fit_gomp)
}

# --- Ajuste por unidad ---
unidades <- unique(df_crec$Unidad)
resultados_par <- data.frame()

cat("\nAjustando modelos por unidad experimental...\n")
for(u in unidades) {
  dat_u <- df_crec %>% dplyr::filter(Unidad == u) %>% dplyr::arrange(t)
  ajustes <- ajustar_crecimiento(dat_u)
  if(is.null(ajustes)) next
  
  for(mod_name in names(ajustes)) {
    fit <- ajustes[[mod_name]]
    if(!is.null(fit)) {
      coefs <- coef(fit)
      resultados_par <- rbind(resultados_par, data.frame(
        Unidad = u,
        Modelo = mod_name,
        K = coefs["K"],
        r = coefs["r"],
        tmid = coefs["tmid"],
        stringsAsFactors = FALSE
      ))
    }
  }
}

if(nrow(resultados_par) == 0) {
  cat("No se pudo ajustar ningún modelo. Verifica los datos temporales.\n")
} else {
  cols_meta <- c("Unidad", var_tratamiento)
  if(exists("incluir_replica") && incluir_replica && exists("var_replica") && var_replica %in% names(df_obs_crec)) {
    cols_meta <- c(cols_meta, var_replica)
  }
  if(exists("factores_agrupacion") && length(factores_agrupacion) > 0) {
    cols_meta <- c(cols_meta, intersect(factores_agrupacion, names(df_obs_crec)))
  }
  cols_meta <- cols_meta[cols_meta %in% names(df_obs_crec)]
  
  meta <- df_obs_crec %>%
    dplyr::select(dplyr::all_of(cols_meta)) %>%
    distinct()
  
  resultados_par <- dplyr::left_join(resultados_par, meta, by = "Unidad")
  
  cat("\nResumen de parámetros por tratamiento:\n")
  print(resultados_par %>%
          dplyr::group_by(Modelo, !!sym(var_tratamiento)) %>%
          dplyr::summarise(
            across(c(K, r, tmid),
                   list(media = ~ mean(.x, na.rm = TRUE),
                        sd    = ~ sd(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}"),
            .groups = 'drop'
          ))
  
  # --- Comparación estadística y obtención de letras ---
  cat("\nANOVA para K (capacidad de carga) - Modelo logístico:\n")
  sub_log_K <- resultados_par %>% dplyr::filter(Modelo == "logistic")
  letras_df <- NULL  
  
  if(nrow(sub_log_K) > 0) {
    formula_k <- as.formula(paste("K ~", var_tratamiento))
    modelo_K <- tryCatch(lm(formula_k, data = sub_log_K), error = function(e) NULL)
    
    if(!is.null(modelo_K)) {
      print(anova(modelo_K))
      emm_K <- emmeans(modelo_K, as.formula(paste("~", var_tratamiento)))
      print(emm_K)
      
      # TryCatch para evitar fallos si no hay varianza entre grupos
      tryCatch({
        letras_K <- cld(emm_K, alpha = 0.15, Letters = letters, adjust = "none")
        letras_df <- as.data.frame(letras_K)[, c(var_tratamiento, ".group")]
        colnames(letras_df) <- c("Tratamiento", "Letra")
        letras_df$Letra <- trimws(letras_df$Letra)
        cat("\nLetras de significancia para K:\n")
        print(letras_df)
      }, error = function(e) {
        cat("No se pudieron generar las letras de significancia (posible falta de varianza).\n")
      })
    }
  }
  
  # --- Gráficos del Módulo 3.1 ---
  resultados_long <- resultados_par %>%
    tidyr::pivot_longer(cols = c(K, r, tmid),
                        names_to = "Parametro",
                        values_to = "Valor")
  
  plot_params <- ggplot(resultados_long,
                        aes(x = !!sym(var_tratamiento), y = Valor,
                            fill = !!sym(var_tratamiento))) +
    geom_boxplot(alpha = 0.6, outlier.shape = NA) +
    geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
    facet_grid(Parametro ~ Modelo, scales = "free_y") +
    theme_bw(base_size = 13) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") +
    labs(title = "Parámetros de crecimiento por tratamiento",
         subtitle = "Comparación de modelos Logístico y Gompertz",
         x = var_tratamiento, y = "Valor del parámetro")
  
  try(print(plot_params), silent = TRUE)
  
  # --- Curvas ajustadas ---
  params_trt <- resultados_par %>%
    dplyr::filter(Modelo == "logistic") %>%
    dplyr::group_by(!!sym(var_tratamiento)) %>%
    dplyr::summarise(
      K_mean = mean(K, na.rm = TRUE),
      r_mean = mean(r, na.rm = TRUE),
      tmid_mean = mean(tmid, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    dplyr::rename(Tratamiento = !!sym(var_tratamiento))
  
  if(!is.null(letras_df)) {
    params_trt <- dplyr::left_join(params_trt, letras_df, by = "Tratamiento")
  } else {
    params_trt$Letra <- ""
  }
  
  # Protección contra rangos infinitos
  t_min <- min(df_crec$t, na.rm = TRUE)
  t_max <- max(df_crec$t, na.rm = TRUE)
  if(is.infinite(t_min)) t_min <- 0
  if(is.infinite(t_max)) t_max <- 100
  t_seq <- seq(t_min, t_max, length.out = 100)
  
  curvas_pred <- data.frame()
  for(i in 1:nrow(params_trt)) {
    trt <- params_trt$Tratamiento[i]
    K <- params_trt$K_mean[i]
    r <- params_trt$r_mean[i]
    tmid <- params_trt$tmid_mean[i]
    letra <- params_trt$Letra[i]
    if(!is.na(K) && !is.na(r) && !is.na(tmid)) {
      y_pred <- K / (1 + exp(-r * (t_seq - tmid)))
      curvas_pred <- rbind(curvas_pred, data.frame(Tratamiento = trt, t = t_seq, y = y_pred, Letra = letra))
    }
  }
  
  if(nrow(curvas_pred) > 0) {
    plot_curvas <- ggplot() +
      geom_point(data = df_crec, aes(x = t, y = .data[[v_conteo_crec]], color = "Datos observados"),
                 alpha = 0.3, size = 1, show.legend = FALSE) +
      geom_line(data = curvas_pred, aes(x = t, y = y, color = Tratamiento, group = Tratamiento), linewidth = 1.2) +
      geom_text_repel(data = curvas_pred %>% dplyr::group_by(Tratamiento) %>% dplyr::filter(t == max(t)),
                      aes(x = t, y = y, label = Letra, color = Tratamiento),
                      nudge_x = 0.5, fontface = "bold", size = 5, show.legend = FALSE) +
      scale_color_viridis_d(name = "Tratamiento") +
      theme_bw(base_size = 13) +
      labs(title = paste("Curvas de crecimiento (Logístico) -", v_conteo_crec), x = "Días", y = v_conteo_crec) +
      theme(legend.position = "right")
    
    try(print(plot_curvas), silent = TRUE)
  }
  cat("\n[✓] Módulo de curvas de crecimiento finalizado.\n")
}


# =============================================================================
# MÓDULO 3.2: MAPAS DE CALOR TEMPORALES
# =============================================================================
cat("\n===================================================================\n")
cat(">>> MÓDULO DE MAPA DE CALOR DE LA DINÁMICA POBLACIONAL <<<\n")
cat("===================================================================\n")

var_tiempo_heat    <- var_tiempo       
var_agrup_heat     <- var_tratamiento   
tipo_agregacion    <- "media"           
escala_color       <- "D"               

df_heat <- df %>% dplyr::filter(!is.na(.data[[var_tiempo_heat]]), !is.na(.data[[variables_conteo[1]]]))

temp_raw <- as.character(df_heat[[var_tiempo_heat]])
temp_fecha <- as.Date(temp_raw, tryFormats = c("%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d", "%d-%m-%Y"))
proporcion_fechas <- mean(!is.na(temp_fecha))

if (proporcion_fechas > 0.5) {
  df_heat$Tiempo_Plot <- temp_fecha
  df_heat <- df_heat %>% dplyr::filter(!is.na(Tiempo_Plot))
  etiqueta_tiempo <- "Fecha"
} else {
  df_heat$Tiempo_Plot <- as.numeric(temp_raw)
  df_heat <- df_heat %>% dplyr::filter(!is.na(Tiempo_Plot))
  etiqueta_tiempo <- "Muestreo"
}

v_conteo <- variables_conteo[1]
df_agg <- df_heat %>%
  dplyr::group_by(Trat = .data[[var_agrup_heat]], Tiempo = Tiempo_Plot) %>%
  dplyr::summarise(Valor = if(tipo_agregacion == "media") mean(.data[[v_conteo]], na.rm=TRUE) else sum(.data[[v_conteo]], na.rm=TRUE),
                   .groups = 'drop')

heatmap <- ggplot(df_agg, aes(x = Tiempo, y = Trat, fill = Valor)) +
  geom_tile(color = "white") +
  scale_fill_viridis(option = escala_color, name = paste(tipo_agregacion, "de", v_conteo)) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank()) +
  labs(title = paste("Mapa de calor temporal -", v_conteo),
       subtitle = paste("Agregación por", var_agrup_heat), x = etiqueta_tiempo, y = var_agrup_heat)

try(print(heatmap), silent = TRUE)


# Con mejores colores:

# --- CAMBIO 1: ESCALA DE COLORES (Verde -> Amarillo -> Naranja -> Rojo) ---
escala_fill <- scale_fill_gradientn(
  colours = c("#4CAF50", "#FFEB3B", "#FF9800", "#F44336"), 
  name = paste(tipo_agregacion, "de", v_conteo)
)

# --- CAMBIO 2: ESCALA DEL EJE X (FORZAR TODAS LAS FECHAS) ---
if (etiqueta_tiempo == "Fecha") {
  # Extraemos todas las fechas únicas y ordenadas de los datos agregados
  fechas_unicas <- sort(unique(df_agg$Tiempo))
  
  # Forzamos a que ggplot use EXACTAMENTE esas fechas como etiquetas
  escala_x <- scale_x_date(breaks = fechas_unicas, 
                           date_labels = "%d/%m/%Y")
} else {
  # Si es un número de muestreo, mantenemos el eje continuo
  escala_x <- scale_x_continuous()
}

# --- CREACIÓN DEL MAPA DE CALOR ---
heatmap <- ggplot(df_agg, aes(x = Tiempo, y = Trat, fill = Valor)) +
  geom_tile(color = "white") +
  escala_fill +              
  escala_x +                 
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), # Rotación para que no se encimen
        panel.grid = element_blank()) +
  labs(title = paste("Mapa de calor temporal -", v_conteo),
       subtitle = paste("Agregación por", var_agrup_heat), x = etiqueta_tiempo, y = var_agrup_heat)

try(print(heatmap), silent = TRUE)


# =============================================================================
# MÓDULO 3.3: ANÁLISIS DE SUPERVIVENCIA (TIEMPO HASTA UMBRAL)
# =============================================================================
cat("\n===================================================================\n")
cat(">>> MÓDULO DE ANÁLISIS DE SUPERVIVENCIA HASTA UMBRAL DE PLAGA <<<\n")
cat("===================================================================\n")

var_tiempo_surv    <- var_tiempo        
umbral_plaga       <- 5                 
tiempo_max         <- NULL              

df_surv <- df %>% dplyr::filter(!is.na(.data[[var_tiempo_surv]]))

temp_raw <- as.character(df_surv[[var_tiempo_surv]])
temp_fecha <- as.Date(temp_raw, tryFormats = c("%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d", "%d-%m-%Y"))
proporcion_fechas <- mean(!is.na(temp_fecha))

if (proporcion_fechas > 0.5) {
  df_surv$Fecha_Num <- temp_fecha
  df_surv <- df_surv %>% dplyr::filter(!is.na(Fecha_Num))
  df_surv$t_num <- as.numeric(df_surv$Fecha_Num - min(df_surv$Fecha_Num, na.rm = TRUE))
} else {
  df_surv$t_num <- as.numeric(temp_raw)
}

v_conteo_surv <- variables_conteo[1]
vars_id <- c(var_tratamiento)
if(exists("incluir_replica") && incluir_replica && exists("var_replica") && var_replica %in% names(df_surv)) vars_id <- c(vars_id, var_replica)
if(exists("factores_agrupacion") && length(factores_agrupacion) > 0) vars_id <- c(vars_id, intersect(factores_agrupacion, names(df_surv)))
df_surv <- df_surv %>% tidyr::unite("Unidad", dplyr::all_of(vars_id), remove = FALSE)

eventos <- df_surv %>%
  dplyr::group_by(Unidad) %>%
  dplyr::filter(.data[[v_conteo_surv]] >= umbral_plaga) %>%
  dplyr::summarise(t_evento = min(t_num, na.rm = TRUE), .groups = 'drop')

if(is.null(tiempo_max)) tiempo_max <- max(df_surv$t_num, na.rm = TRUE)

ultimo_tiempo <- df_surv %>%
  dplyr::group_by(Unidad) %>%
  dplyr::summarise(t_max = max(t_num, na.rm = TRUE), .groups = 'drop')

datos_surv <- dplyr::full_join(ultimo_tiempo, eventos, by = "Unidad") %>%
  dplyr::mutate(tiempo = ifelse(is.na(t_evento), t_max, t_evento),
                evento = ifelse(is.na(t_evento), 0, 1))

meta_surv <- df_surv %>% dplyr::select(Unidad, all_of(var_tratamiento)) %>% distinct()
datos_surv <- dplyr::left_join(datos_surv, meta_surv, by = "Unidad")

surv_obj <- Surv(time = datos_surv$tiempo, event = datos_surv$evento)
km_fit <- survfit(surv_obj ~ datos_surv[[var_tratamiento]], data = datos_surv)

cat("\n--- Test de Log-Rank para comparar curvas de supervivencia ---\n")
tryCatch({ print(survdiff(surv_obj ~ datos_surv[[var_tratamiento]], data = datos_surv)) }, error=function(e) cat("Error en Log-Rank.\n"))

# Manejo de error específico de ggsurvplot en entornos no interactivos
tryCatch({
  ggsurv <- ggsurvplot(km_fit, data = datos_surv, pval = TRUE, conf.int = TRUE, risk.table = TRUE,
                       xlab = "Días", ylab = "Prob. bajo umbral", title = paste("Supervivencia -", v_conteo_surv),
                       legend.title = var_tratamiento, ggtheme = theme_bw())
  print(ggsurv)
}, error = function(e) {
  cat("\n[Advertencia] No se pudo imprimir el gráfico de supervivencia. Verifique el dispositivo gráfico.\n")
})

cat("\nMedianas de tiempo hasta alcanzar el umbral:\n")
print(surv_median(km_fit))


# =============================================================================
# MÓDULO 3.6: BOOTSTRAP DE ÍNDICES DE DISPERSIÓN (Corregido y blindado)
# =============================================================================
cat("\n===================================================================\n")
cat(">>> MÓDULO DE BOOTSTRAP PARA ÍNDICES DE AGREGACIÓN <<<\n")
cat("===================================================================\n")

n_boot         <- 1000   
conf_level     <- 0.95   
var_eval_boot  <- variables_conteo[1]
var_agrup_boot <- var_tratamiento

# Función robusta para evitar vectores de diferentes longitudes o condicionales fallidos
calc_dispersion <- function(x) {
  x <- na.omit(x)
  n <- length(x)
  if (n < 2 || sum(x) == 0) return(as.numeric(c(NA, NA, NA, NA, NA)))
  
  m <- mean(x)
  v <- var(x)
  sum_x <- sum(x)
  sum_x2 <- sum(x^2)
  
  vmr <- v / m
  lip <- (m + (vmr - 1)) / m
  morisita <- ifelse(sum_x > 1, n * (sum_x2 - sum_x) / ((sum_x^2) - sum_x), NA)
  green <- ifelse(sum_x > 1, (vmr - 1) / (sum_x - 1), NA)
  k_est <- ifelse(!is.na(v) && !is.na(m) && v > m && m > 0, (m^2) / (v - m), NA)
  
  return(as.numeric(c(vmr, lip, morisita, green, k_est)))
}

df_boot <- df %>% dplyr::filter(!is.na(.data[[var_eval_boot]]))
lista_datos <- split(df_boot[[var_eval_boot]], df_boot[[var_agrup_boot]])

boot_indices <- function(datos, indices) calc_dispersion(datos[indices])
resultados_boot <- list()

for(trt in names(lista_datos)) {
  datos_trt <- lista_datos[[trt]]
  
  if(length(na.omit(datos_trt)) < 3) {
    cat(sprintf("Datos insuficientes para el tratamiento: %s\n", trt))
    next
  }
  
  set.seed(123) 
  boot_res <- tryCatch({ boot(datos_trt, boot_indices, R = n_boot) }, error = function(e) NULL)
  if (is.null(boot_res)) next
  
  ci_list <- lapply(1:5, function(i) {
    if(all(is.na(boot_res$t[,i]))) return(c(NA, NA))
    tryCatch({ boot.ci(boot_res, type = "perc", index = i)$percent[4:5] }, error = function(e) c(NA, NA))
  })
  
  ci_matrix <- do.call(rbind, ci_list)
  colnames(ci_matrix) <- c("LI", "LS")
  ind_names <- c("VMR","Lloyd_LIP","Morisita","Green","K_BinNeg")
  
  resultados_boot[[trt]] <- data.frame(
    Tratamiento = trt, Indice = ind_names, Observado = calc_dispersion(datos_trt),
    LI = ci_matrix[,"LI"], LS = ci_matrix[,"LS"], stringsAsFactors = FALSE
  )
}

if(length(resultados_boot) > 0) {
  tabla_boot <- do.call(rbind, resultados_boot)
  cat("\nTabla de índices con intervalos de confianza bootstrap:\n")
  print(tabla_boot, row.names = FALSE)
  
  for(ind in c("VMR","Lloyd_LIP","Morisita","Green","K_BinNeg")) {
    sub_tab <- tabla_boot %>% dplyr::filter(Indice == ind, !is.na(Observado))
    if(nrow(sub_tab) == 0) next
    
    p <- ggplot(sub_tab, aes(x = reorder(Tratamiento, Observado), y = Observado, color = Tratamiento)) +
      geom_point(size = 3) + geom_errorbar(aes(ymin = LI, ymax = LS), width = 0.2) + coord_flip() +
      labs(title = paste("Índice", ind, "con IC bootstrap"), x = "", y = ind) +
      theme_bw(base_size = 13) + theme(legend.position = "none")
    
    if(ind %in% c("VMR","Lloyd_LIP","Morisita")) p <- p + geom_hline(yintercept = 1, linetype = "dashed", color = "red")
    if(ind == "Green") p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "red")
    if(ind == "K_BinNeg") p <- p + geom_hline(yintercept = 8, linetype = "dashed", color = "blue") + annotate("text", x = 1, y = 8, label = "K=8", vjust = -0.5, color = "blue")
    
    try(print(p), silent = TRUE)
  }
} else {
  cat("\nNo se generaron resultados de bootstrap suficientes.\n")
}
