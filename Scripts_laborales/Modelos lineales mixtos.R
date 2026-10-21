# ==============================================================================
# SCRIPT MAESTRO: INTEGRACIÓN TOTAL (DIAGNÓSTICO ORIGINAL + AUTOMATIZACIÓN)
# ==============================================================================
library(lme4)
library(lmerTest)
library(nlme)
library(emmeans)
library(multcomp)
library(ggplot2)
library(dplyr)
library(tidyr)

# ------------------------------------------------------------------------------
# SECCIÓN 1: CONFIGURACIÓN GENERAL (Tu Zona de Control)
# ------------------------------------------------------------------------------

# 1. Carga de datos
datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "", stringsAsFactors = TRUE)
names(datos)
View(datos)

# 2. Selector de Motor Estadístico
MOTOR <- "lme4"  # "lme4" (permite sumas aleatorias) o "nlme" (tu diagnóstico varIdent)

# 3. Múltiples Variables y Efectos Fijos
VARS_RESPUESTA <- c("TCH_REAL")            # Ej: c("Rendimiento", "TCH", "TAH")
EFECTOS_FIJOS  <- "ROTACION.ZP"                     # Sumas (+) e interacciones (*) permitidas

# 4. Fórmulas de Efectos Aleatorios
ALEATORIOS_LME4 <- "(1|cuadrante/TIPO.SIEMBRA/Tipo.de.corte/Variedad..Agrícola.)"  # Para lme4
ALEATORIOS_NLME <- "~ 1 | Replica"# Para nlme (Anidamiento estricto)

# 5. Configuración del Reporte y PMM
FACTOR_EVALUAR    <- "ROTACION.ZP"   # Factor principal para sacar las letras y el gráfico
TESTIGO           <- "ROTACION"       # Nombre exacto de tu testigo (Para Relativo = 1.0)
AJUSTE_PMM        <- "Tukey"   # Método PMM (ej. "Tukey" o "none" para Fisher)
ALPHA_VAL         <- 0.15      # Significancia (α)
LETRAS_ORDEN_DESC <- TRUE      # TRUE: Letra 'a' para la media más ALTA. FALSE: 'a' para la más BAJA.

# 6. Estructuras de Varianza (Solo si MOTOR == "nlme")
VAR_EST_1 <- "Replica"
VAR_EST_2 <- FACTOR_EVALUAR
VAR_EST_3 <- paste(FACTOR_EVALUAR, "* Replica")

# ------------------------------------------------------------------------------
# INICIALIZACIÓN Y CORRECCIÓN DE EJES (Solución al Zig-Zag)
# ------------------------------------------------------------------------------
es_numerico <- suppressWarnings(all(!is.na(as.numeric(as.character(datos[[FACTOR_EVALUAR]])))))

if(es_numerico) {
  niveles_ordenados <- sort(unique(as.numeric(as.character(datos[[FACTOR_EVALUAR]]))))
  datos[[FACTOR_EVALUAR]] <- factor(datos[[FACTOR_EVALUAR]], levels = niveles_ordenados)
} else {
  datos[[FACTOR_EVALUAR]] <- as.factor(as.character(datos[[FACTOR_EVALUAR]]))
}

tabla_maestra <- datos %>% 
  group_by(.data[[FACTOR_EVALUAR]]) %>% 
  summarise(n = n(), .groups = "drop") %>%
  mutate(across(all_of(FACTOR_EVALUAR), as.character))

p_valores_lista <- list()

# ==============================================================================
# SECCIÓN 2: BUCLE MULTI-VARIABLE (Modelado y Diagnóstico)
# ==============================================================================

for(Y_VAR in VARS_RESPUESTA) {
  cat(paste0("\n==================================================================\n",
             "  EVALUANDO VARIABLE: ", toupper(Y_VAR), 
             "\n==================================================================\n"))
  
  datos_proc <- datos %>% filter(!is.na(.data[[Y_VAR]]))
  datos_proc[[Y_VAR]] <- as.numeric(as.character(datos_proc[[Y_VAR]]))
  
  modelo_optimo <- NULL; nombre_optimo <- ""
  validos <- c(TRUE) # Para control de la tabla resumen de nlme
  
  # --------------------------------------------------------------------------
  # 2.A RUTA LME4
  # --------------------------------------------------------------------------
  if (MOTOR == "lme4") {
    form_lme4 <- paste(Y_VAR, "~", EFECTOS_FIJOS, "+", ALEATORIOS_LME4)
    modelo_optimo <- tryCatch(lmer(as.formula(form_lme4), data = datos_proc), error=function(e) NULL)
    if(is.null(modelo_optimo)) { cat("[!] Fallo LME4 para", Y_VAR, "\n"); next }
    nombre_optimo <- "Modelo lme4 (Homocedástico)"
    
    cat("\n> Resumen General del Modelo (lme4):\n")
    print(summary(modelo_optimo), correlation = FALSE) # correlation=FALSE limpia la consola
    
    # --------------------------------------------------------------------------
    # 2.B RUTA NLME (TUS GRÁFICOS Y MODELOS ORIGINALES)
    # --------------------------------------------------------------------------
  } else {
    cat("\n--- COMIENZO: DIAGNÓSTICO DE MODELOS (nlme) ---\n")
    form_fija <- as.formula(paste(Y_VAR, "~", EFECTOS_FIJOS))
    form_rand <- as.formula(ALEATORIOS_NLME)
    
    modelo_rep   <- tryCatch(lme(fixed=form_fija, random=form_rand, weights=varIdent(form=as.formula(paste("~1|", VAR_EST_1))), data=datos_proc, na.action=na.omit, control=lmeControl(msMaxIter=100, opt="optim")), error=function(e) NULL)
    modelo_trat  <- tryCatch(lme(fixed=form_fija, random=form_rand, weights=varIdent(form=as.formula(paste("~1|", VAR_EST_2))), data=datos_proc, na.action=na.omit, control=lmeControl(msMaxIter=100, opt="optim")), error=function(e) NULL)
    modelo_inter <- tryCatch(lme(fixed=form_fija, random=form_rand, weights=varIdent(form=as.formula(paste("~1|", VAR_EST_3))), data=datos_proc, na.action=na.omit, control=lmeControl(msMaxIter=100, opt="optim")), error=function(e) NULL)
    
    lista_mod <- list(modelo_rep, modelo_trat, modelo_inter)
    nombres_mod <- c(paste("1. VarIdent (", VAR_EST_1, ")", sep=""), paste("2. VarIdent (", VAR_EST_2, ")", sep=""), paste("3. VarIdent (", VAR_EST_3, ")", sep=""))
    validos <- !sapply(lista_mod, is.null)
    
    if(sum(validos) == 0) { cat("[!] Modelos nlme fallaron para", Y_VAR, "\n"); next }
    
    comp_modelos <- do.call(anova, lista_mod[validos])
    cat("\n> Comparativa de Modelos (Selecciona el de menor AIC/BIC):\n"); print(comp_modelos)
    
    mejor_idx <- which.min(comp_modelos$AIC)
    modelo_optimo <- lista_mod[validos][[mejor_idx]]
    nombre_optimo <- nombres_mod[validos][mejor_idx]
    
    cat(paste0("\n[INFO]: El modelo seleccionado automáticamente por mejor AIC es: ", nombre_optimo, "\n"))
    
    cat("\n--- COMIENZO: GENERACIÓN DE GRÁFICOS DE DIAGNÓSTICO ---\n")
    
    # TU GRÁFICO 1: Residuos vs Predichos
    plot(modelo_optimo, resid(., type="pearson") ~ fitted(.), 
         main = paste("Residuos Estandarizados vs Predichos\nModelo:", nombre_optimo),
         xlab = "Valores Ajustados", ylab = "Residuos de Pearson")
    
    # TU GRÁFICO 2: Boxplot Comparativo Global (Solo si los 3 se ajustaron)
    if(sum(validos) == 3) {
      res_rep   <- resid(modelo_rep, type = "pearson")
      res_trat  <- resid(modelo_trat, type = "pearson")
      res_inter <- resid(modelo_inter, type = "pearson")
      
      tabla_residuos <- data.frame(
        Residuo = c(res_rep, res_trat, res_inter),
        Modelo = rep(nombres_mod, each = length(res_rep))
      )
      
      g_residuos <- ggplot(tabla_residuos, aes(x = Modelo, y = Residuo, fill = Modelo)) +
        geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.shape = 16, outlier.size = 2.5) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "darkblue", linewidth = 1) +
        labs(title = paste("Comparación de Residuos por Estructura de Varianza -", Y_VAR), subtitle = "Evaluación de homocedasticidad entre los 3 modelos", x = "Estructura del Modelo", y = "Residuos de Pearson") +
        scale_fill_manual(values = c("#A3E4D7", "#F9E79F", "#F5CBA7")) + theme_minimal() +
        theme(legend.position = "none", axis.text.x = element_text(face = "bold", size = 10), axis.title = element_text(face = "bold"))
      print(g_residuos)
      
      # TU GRÁFICO 3: Boxplots de Residuos por Tratamiento
      par(mfrow = c(3, 1)) 
      boxplot(res_rep ~ datos_proc[[FACTOR_EVALUAR]], main = nombres_mod[1], ylab = "Residuos", xlab = FACTOR_EVALUAR, col = "#A3E4D7"); abline(h = 0, col = "red", lty = 2, lwd = 1.5)
      boxplot(res_trat ~ datos_proc[[FACTOR_EVALUAR]], main = nombres_mod[2], ylab = "Residuos", xlab = FACTOR_EVALUAR, col = "#F9E79F"); abline(h = 0, col = "red", lty = 2, lwd = 1.5)
      boxplot(res_inter ~ datos_proc[[FACTOR_EVALUAR]], main = nombres_mod[3], ylab = "Residuos", xlab = FACTOR_EVALUAR, col = "#F5CBA7"); abline(h = 0, col = "red", lty = 2, lwd = 1.5)
      par(mfrow = c(1, 1))
    }
    
    cat("\n> Resumen General del Modelo (Estadísticos y coeficientes):\n")
    print(summary(modelo_optimo), correlation = FALSE) # Suprime los números extensos de correlación
  }
  
  # --------------------------------------------------------------------------
  # 2.C VERIFICACIÓN FORMAL DE SUPUESTOS
  # --------------------------------------------------------------------------
  cat("\n> VERIFICACIÓN FORMAL DE SUPUESTOS:\n")
  res_p <- resid(modelo_optimo, type = "pearson")
  
  p_shap <- tryCatch(shapiro.test(res_p)$p.value, error=function(e) NA)
  cat(sprintf("  - Shapiro-Wilk (Normalidad): P-valor = %.4f %s\n", p_shap, ifelse(!is.na(p_shap) && p_shap < 0.05, "[ALERTA: No Normal]", "[OK]")))
  
  p_bart <- tryCatch(bartlett.test(res_p ~ datos_proc[[FACTOR_EVALUAR]])$p.value, error=function(e) NA)
  cat(sprintf("  - Bartlett (Homocedasticidad): P-valor = %.4f %s\n", p_bart, ifelse(!is.na(p_bart) && p_bart < 0.05, "[ALERTA: Varianzas distintas]", "[OK]")))
  
  # --------------------------------------------------------------------------
  # 2.D ANDEVA Y P-VALORES
  # --------------------------------------------------------------------------
  cat("\n--- COMIENZO: ANÁLISIS FINAL DEL MODELO ÓPTIMO ---\n")
  anova_res <- anova(modelo_optimo)
  cat("\n> Tabla de ANOVA (Efectos Fijos):\n"); print(anova_res)
  
  pval <- NA
  tryCatch({
    if (MOTOR == "lme4") pval <- anova_res[FACTOR_EVALUAR, "Pr(>F)"] else pval <- anova_res[FACTOR_EVALUAR, "p-value"]
  }, error=function(e) pval <- NA)
  p_valores_lista[[Y_VAR]] <- ifelse(is.na(pval), "-", ifelse(pval < 0.001, "<0.001", sprintf("%.4f", pval)))
  
  # --------------------------------------------------------------------------
  # 2.E PRUEBA DE MEDIAS Y TUS TABLAS DE RESULTADOS
  # --------------------------------------------------------------------------
  form_emm <- as.formula(paste("~", FACTOR_EVALUAR))
  
  # SuppressMessages evita el texto extra de emmeans sobre grados de libertad
  medias_ajustadas <- suppressMessages(emmeans::emmeans(modelo_optimo, form_emm))
  letras_medias <- suppressMessages(cld(medias_ajustadas, alpha = ALPHA_VAL, Letters = letters, adjust = AJUSTE_PMM, decreasing = LETRAS_ORDEN_DESC))
  letras_medias$.group <- trimws(letras_medias$.group)
  
  cat("\n> VERSIÓN A: Tabla de Medias AJUSTADAS (Modelo Óptimo):\n")
  print(as.data.frame(letras_medias))
  
  # TU GRÁFICO 4: Medias Ajustadas (Individual)
  g_final_ajustadas <- ggplot(as.data.frame(letras_medias), aes_string(y = paste0("reorder(", FACTOR_EVALUAR, ", emmean)"), x = "emmean")) +
    geom_errorbarh(aes(xmin = emmean - SE, xmax = emmean + SE), height = 0.2, color = "#2C3E50", linewidth = 0.8) +
    geom_point(color = "black", size = 3) +
    geom_text(aes(label = round(emmean, 2)), vjust = -1.2, size = 3.5, color = "#2C3E50") +
    geom_text(aes(x = emmean + SE, label = .group), hjust = -0.5, size = 4.5, fontface = "bold", color = "black") +
    labs(title = paste("Prueba de Medias AJUSTADAS para", Y_VAR), subtitle = paste("Basado en:", nombre_optimo), x = paste(Y_VAR, "(Media Estimada ± SE)"), y = FACTOR_EVALUAR) +
    theme_minimal() + theme(axis.text.y = element_text(face = "bold", size = 11), panel.grid.major.y = element_blank())
  print(g_final_ajustadas)
  
  # Medias Reales (Aritméticas)
  medias_reales <- datos_proc %>% group_by(.data[[FACTOR_EVALUAR]]) %>% summarise(media_real = mean(.data[[Y_VAR]], na.rm=TRUE), SE_real = sd(.data[[Y_VAR]], na.rm=TRUE) / sqrt(n()), .groups = "drop")
  
  letras_medias_reales <- as.data.frame(letras_medias) %>% dplyr::select(all_of(FACTOR_EVALUAR), .group) %>% left_join(medias_reales, by = FACTOR_EVALUAR)
  
  cat("\n> VERSIÓN B: Tabla de Medias REALES (Aritméticas directas):\n")
  print(letras_medias_reales)
  
  # TU GRÁFICO 5: Medias Reales (Individual)
  g_final_reales <- ggplot(letras_medias_reales, aes_string(y = paste0("reorder(", FACTOR_EVALUAR, ", media_real)"), x = "media_real")) +
    geom_errorbarh(aes(xmin = media_real - SE_real, xmax = media_real + SE_real), height = 0.2, color = "black", linewidth = 0.8) +
    geom_point(color = "black", size = 3) +
    geom_text(aes(label = round(media_real, 2)), vjust = -1.2, size = 3.5, color = "black") +
    geom_text(aes(x = media_real + SE_real, label = .group), hjust = -0.5, size = 4.5, fontface = "bold", color = "black") +
    labs(title = paste("Prueba de Medias REALES para", Y_VAR), subtitle = "Medias muestrales directas con letras del LMM", x = paste(Y_VAR, "(Media Real ± SE Real)"), y = FACTOR_EVALUAR) +
    theme_minimal() + theme(axis.text.y = element_text(face = "bold", size = 11), panel.grid.major.y = element_blank())
  print(g_final_reales)
  
  # TU TABLA COMPARATIVA DE LOS 3 MODELOS (Solo para nlme)
  if (MOTOR == "nlme" && sum(validos) == 3) {
    medias_rep_df   <- as.data.frame(suppressMessages(emmeans(modelo_rep, form_emm))) %>% dplyr::select(all_of(FACTOR_EVALUAR), Rep = emmean) %>% mutate(across(1, as.character))
    medias_trat_df  <- as.data.frame(suppressMessages(emmeans(modelo_trat, form_emm))) %>% dplyr::select(all_of(FACTOR_EVALUAR), Trat = emmean) %>% mutate(across(1, as.character))
    medias_inter_df <- as.data.frame(suppressMessages(emmeans(modelo_inter, form_emm))) %>% dplyr::select(all_of(FACTOR_EVALUAR), Inter = emmean) %>% mutate(across(1, as.character))
    
    resumen_modelos <- medias_rep_df %>% left_join(medias_trat_df, by = FACTOR_EVALUAR) %>% left_join(medias_inter_df, by = FACTOR_EVALUAR) %>%
      left_join(medias_reales %>% dplyr::select(all_of(FACTOR_EVALUAR), Real = media_real) %>% mutate(across(1, as.character)), by = FACTOR_EVALUAR)
    
    cat("\n> Resumen comparativo de medias estimadas y reales por cada modelo:\n"); print(resumen_modelos)
  }
  
  # --------------------------------------------------------------------------
  # 2.F GRÁFICOS DE PENDIENTES ALEATORIAS (Spaghetti Plot Individual)
  # --------------------------------------------------------------------------
  x_var_fija <- all.vars(as.formula(paste("~", EFECTOS_FIJOS)))[1]
  grupos_aleatorios <- if(MOTOR == "lme4") names(getME(modelo_optimo, "flist")) else names(modelo_optimo$groups)[1]
  
  datos_proc$Pred_Fixed <- if(MOTOR == "lme4") predict(modelo_optimo, re.form = NA) else predict(modelo_optimo, level = 0)
  datos_proc$Pred_Mixed <- predict(modelo_optimo)
  
  for(grupo in grupos_aleatorios) {
    col_graf <- gsub(":", "_", grupo) 
    if(grepl(":", grupo)) {
      partes <- unlist(strsplit(grupo, ":"))
      datos_proc[[col_graf]] <- apply(datos_proc[, partes, drop=FALSE], 1, paste, collapse="_")
    } else { col_graf <- grupo }
    
    p_ran <- ggplot(datos_proc, aes(x = .data[[x_var_fija]], y = .data[[Y_VAR]], color = as.factor(.data[[col_graf]]))) +
      geom_point(alpha = 0.5, size = 2) +
      geom_line(aes(y = Pred_Mixed, group = .data[[col_graf]]), linewidth = 1.2, alpha = 0.8) +
      geom_line(aes(y = Pred_Fixed, group = 1), color = "black", linewidth = 2.5) +
      scale_color_viridis_d(option = "turbo", name = grupo) +
      labs(title = paste("Pendientes Aleatorias:", Y_VAR), subtitle = paste("Agrupación:", grupo), x = x_var_fija, y = Y_VAR) + theme_bw(base_size = 14) + theme(legend.position = "right")
    
    if(is.numeric(datos_proc[[x_var_fija]])) { p_ran <- p_ran + geom_smooth(aes(group=1), method="lm", formula=y~x, color="black", linewidth=2.5, se=FALSE, linetype="dashed") }
    
    print(p_ran) # Se imprime SOLO
  }
  
  # --------------------------------------------------------------------------
  # 2.G EXTRACCIÓN DE DATOS PARA LA TABLA MAESTRA
  # --------------------------------------------------------------------------
  df_var_res <- letras_medias_reales %>% mutate(across(all_of(FACTOR_EVALUAR), as.character))
  val_testigo <- df_var_res$media_real[df_var_res[[FACTOR_EVALUAR]] == TESTIGO]
  if(length(val_testigo) == 0 || is.na(val_testigo)) val_testigo <- mean(df_var_res$media_real, na.rm=TRUE)
  
  df_var_res <- df_var_res %>%
    mutate(Relativo = media_real / val_testigo[1], Letra_Limpia = ifelse(is.na(.group), "", .group), Media_Combinada = trimws(sprintf("%.2f %s", media_real, Letra_Limpia)))
  
  df_unir <- df_var_res %>% dplyr::select(all_of(FACTOR_EVALUAR), Media_Combinada, Relativo) %>%
    rename(!!paste0(Y_VAR, "_Media") := Media_Combinada, !!paste0(Y_VAR, "_Relativo") := Relativo)
  
  tabla_maestra <- tabla_maestra %>% left_join(df_unir, by = FACTOR_EVALUAR)
}

# ==============================================================================
# SECCIÓN 3: TABLA MAESTRA CONSOLIDADA Y REORDENADA
# ==============================================================================
cat("\n=================================================================================\n")
cat("                  TABLA GERENCIAL CONSOLIDADA Y REORDENADA\n")
cat("=================================================================================\n")

nombres_cols <- names(tabla_maestra)
cols_medias <- grep("_Media$", nombres_cols, value = TRUE)
cols_relativos <- grep("_Relativo$", nombres_cols, value = TRUE)

tabla_imprimir <- tabla_maestra %>%
  dplyr::select(all_of(FACTOR_EVALUAR), n, all_of(cols_medias), all_of(cols_relativos)) %>%
  mutate(across(all_of(cols_relativos), ~ sprintf("%.3f", .))) %>%
  arrange(desc(.[[3]])) # Ordenar de mayor a menor

fila_pval <- data.frame(matrix(ncol = ncol(tabla_imprimir), nrow = 1))
colnames(fila_pval) <- colnames(tabla_imprimir)
fila_pval[[FACTOR_EVALUAR]] <- "p-valor"
fila_pval$n <- "-"

for(Y_VAR in VARS_RESPUESTA) {
  col_m <- paste0(Y_VAR, "_Media")
  col_r <- paste0(Y_VAR, "_Relativo")
  if(col_m %in% names(fila_pval)) fila_pval[[col_m]] <- p_valores_lista[[Y_VAR]]
  if(col_r %in% names(fila_pval)) fila_pval[[col_r]] <- "-"
}

tabla_final <- bind_rows(tabla_imprimir %>% mutate(across(everything(), as.character)), fila_pval %>% mutate(across(everything(), as.character)))

print(as.data.frame(tabla_final), row.names = FALSE)
