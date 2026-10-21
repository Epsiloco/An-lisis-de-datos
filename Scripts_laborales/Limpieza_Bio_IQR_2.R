# ==============================================================================
# SCRIPT UNIFICADO AVANZADO: AUDITORÍA DE DATOS (AZÚCAR Y TCH)
# VERSIÓN BLINDADA: SECCIONES 100% INDEPENDIENTES (AUTO-DETECT + HOVER + STATS)
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. PAQUETES DE R
# ------------------------------------------------------------------------------
paquetes <- c("openxlsx", "dplyr", "tidyr", "ggplot2", "plotly", "htmlwidgets")
no_instalados <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]
if(length(no_instalados)) install.packages(no_instalados)
lapply(paquetes, library, character.only = TRUE)


# ==============================================================================
# ==============================================================================
# SECCIÓN 1: AZÚCAR
# ==============================================================================
# ==============================================================================
message(">>> POR FAVOR, COPIA LOS DATOS DE AZÚCAR AL PORTAPAPELES AHORA <<<")
invisible(readline(prompt="Presiona [Enter] cuando hayas copiado los datos..."))

datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")

# FUNCIÓN TOLERANTE A ERRORES (Definida aquí para que la sección sea independiente)
buscar_columna <- function(nombres_existentes, patrones_busqueda, nombre_defecto) {
  for (patron in patrones_busqueda) {
    coincidencia <- grep(patron, nombres_existentes, ignore.case = TRUE, value = TRUE)
    if (length(coincidencia) > 0) return(coincidencia[1])
  }
  return(nombre_defecto)
}

# --- DETECCIÓN AUTOMÁTICA INTELIGENTE DE COLUMNAS (AZÚCAR) ---
nombres_azucar <- names(datos)
c_rend  <- buscar_columna(nombres_azucar, c("rendimiento", "kg.ton", "rend", "kg"), "Rendimiento.Kg.Ton")
c_pur   <- buscar_columna(nombres_azucar, c("pureza", "pur", "pure"), "Pureza")
c_trat  <- buscar_columna(nombres_azucar, c("tratamiento", "trat", "trt"), "Tratamiento")
c_rep   <- buscar_columna(nombres_azucar, c("replica", "rep", "rep."), "Replica")
c_sub   <- buscar_columna(nombres_azucar, c("etiqueta", "submuestra", "sub", "id"), "Etiqueta")

message(paste0("\n[Azúcar] Columnas detectadas de forma automática:\n",
               "  - Rendimiento -> '", c_rend, "'\n", "  - Pureza -> '", c_pur, "'\n",
               "  - Tratamiento -> '", c_trat, "'\n", "  - Réplica -> '", c_rep, "'\n",
               "  - Submuestra -> '", c_sub, "'\n"))

# Estandarización de nombres interna para blindar el proceso
datos_estandar <- datos %>%
  rename(Rend_Orig = !!sym(c_rend), Pur_Orig = !!sym(c_pur), Trat_Orig = !!sym(c_trat), Rep_Orig = !!sym(c_rep), Sub_Orig = !!sym(c_sub))

# --- PANEL DE CONTROL MANUAL ---
manual_corte  <- 1 
manual_tercio <- 3 
manual_edad <- 11.83
ensayo_madurante <- TRUE 
max_cv             <- 8    
aplicar_imputacion   <- TRUE   
imputar_solo_rojos   <- TRUE   

# --- MATRIZ DE PARÁMETROS BIOLÓGICOS ---
parametros_bio <- expand.grid(tercio_zafra = c("Tercio 1", "Tercio 2", "Tercio 3", "Global"),
                              corte_cat = c("Plantilla", "Soca 1", "Soca 2", "Soca 3", "Global")) %>%
  mutate(LI_Bio_Rend = case_when(corte_cat == "Plantilla" & tercio_zafra == "Tercio 2" ~ 95,
                                 corte_cat == "Plantilla" ~ 80,
                                 tercio_zafra == "Tercio 2" ~ 100,
                                 TRUE ~ 85),
         LS_Bio_Rend = ifelse(ensayo_madurante, 175, 145),
         LI_Bio_Pur = ifelse(tercio_zafra == "Tercio 2", 82, 80), LS_Bio_Pur = 98)

corte_ensayo <- case_when(manual_corte==0~"Plantilla", manual_corte==1~"Soca 1", manual_corte==2~"Soca 2", TRUE~"Soca 3")
tercio_ensayo <- case_when(manual_tercio==1~"Tercio 1", manual_tercio==2~"Tercio 2", TRUE~"Tercio 3")

# --- PROCESAMIENTO ---
datos_analizados <- datos_estandar %>%
  mutate(Rend_Eval = as.numeric(as.character(Rend_Orig)), 
         Pur_Eval = as.numeric(as.character(Pur_Orig)),
         Trat = as.factor(Trat_Orig), Rep = as.character(Rep_Orig), Sub = as.character(Sub_Orig),
         corte_cat = corte_ensayo, tercio_zafra = tercio_ensayo, Edad_Meses = manual_edad) %>%
  left_join(parametros_bio, by = c("tercio_zafra", "corte_cat")) %>%
  
  group_by(Trat, Rep) %>%
  mutate(
    sd_rend_rep = sd(Rend_Eval, na.rm=T),
    cv_rend_rep = ifelse(mean(Rend_Eval,na.rm=T)==0, 0, (sd_rend_rep/mean(Rend_Eval, na.rm=T))*100),
    sd_pur_rep  = sd(Pur_Eval, na.rm=T),
    cv_pur_rep  = ifelse(mean(Pur_Eval,na.rm=T)==0, 0, (sd_pur_rep/mean(Pur_Eval, na.rm=T))*100),
    LI_IQR_Rep_Rend = round(quantile(Rend_Eval, 0.25, na.rm=T) - 1.5*IQR(Rend_Eval, na.rm=T), 2),
    LS_IQR_Rep_Rend = round(quantile(Rend_Eval, 0.75, na.rm=T) + 1.5*IQR(Rend_Eval, na.rm=T), 2),
    LI_IQR_Rep_Pur = round(quantile(Pur_Eval, 0.25, na.rm=T) - 1.5*IQR(Pur_Eval, na.rm=T), 2),
    LS_IQR_Rep_Pur = round(quantile(Pur_Eval, 0.75, na.rm=T) + 1.5*IQR(Pur_Eval, na.rm=T), 2),
    dev_r = abs(Rend_Eval - median(Rend_Eval, na.rm=T)),
    culprit_r = (dev_r == max(dev_r, na.rm=T)) & (dev_r > 0) & (cv_rend_rep > max_cv),
    dev_p = abs(Pur_Eval - median(Pur_Eval, na.rm=T)),
    culprit_p = (dev_p == max(dev_p, na.rm=T)) & (dev_p > 0) & (cv_pur_rep > max_cv)
  ) %>%
  ungroup() %>%
  
  group_by(Trat) %>%
  mutate(LI_IQR_Trat_Rend = round(quantile(Rend_Eval, 0.25, na.rm=T) - 1.5*IQR(Rend_Eval, na.rm=T), 2),
         LS_IQR_Trat_Rend = round(quantile(Rend_Eval, 0.75, na.rm=T) + 1.5*IQR(Rend_Eval, na.rm=T), 2),
         LI_IQR_Trat_Pur  = round(quantile(Pur_Eval, 0.25, na.rm=T) - 1.5*IQR(Pur_Eval, na.rm=T), 2),
         LS_IQR_Trat_Pur  = round(quantile(Pur_Eval, 0.75, na.rm=T) + 1.5*IQR(Pur_Eval, na.rm=T), 2)) %>%
  ungroup() %>%
  
  mutate(
    f_cv_r = (cv_rend_rep > max_cv), f_cv_p = (cv_pur_rep > max_cv),
    f_iqr_rep_r = (Rend_Eval < LI_IQR_Rep_Rend | Rend_Eval > LS_IQR_Rep_Rend),
    f_iqr_rep_p = (Pur_Eval < LI_IQR_Rep_Pur | Pur_Eval > LS_IQR_Rep_Pur),
    f_iqr_trat_r = (Rend_Eval < LI_IQR_Trat_Rend | Rend_Eval > LS_IQR_Trat_Rend),
    f_iqr_trat_p = (Pur_Eval < LI_IQR_Trat_Pur | Pur_Eval > LS_IQR_Trat_Pur),
    f_bio_r = (Rend_Eval < LI_Bio_Rend | Rend_Eval > LS_Bio_Rend),
    f_bio_p = (Pur_Eval < LI_Bio_Pur | Pur_Eval > LS_Bio_Pur),
    
    across(starts_with("f_"), ~replace_na(., FALSE)),
    across(starts_with("culprit_"), ~replace_na(., FALSE)),
    
    falla_roja_rend = culprit_r | f_iqr_rep_r,
    falla_roja_pur  = culprit_p | f_iqr_rep_p,
    falla_roja      = falla_roja_rend | falla_roja_pur,
    
    falla_verde_rend = !falla_roja_rend & f_iqr_trat_r,
    falla_verde_pur  = !falla_roja_pur & f_iqr_trat_p,
    falla_verde      = falla_verde_rend | falla_verde_pur,
    dato_sospechoso = falla_roja | falla_verde,
    
    Motivo_Atipico = paste0(
      ifelse(f_cv_r, "CV Rend Alto; ", ""), ifelse(f_cv_p, "CV Pur Alto; ", ""),
      ifelse(f_iqr_rep_r, "Fuera IQR Réplica Rend; ", ""), ifelse(f_iqr_rep_p, "Fuera IQR Réplica Pur; ", ""),
      ifelse(f_iqr_trat_r, "Fuera IQR Trat Rend; ", ""), ifelse(f_iqr_trat_p, "Fuera IQR Trat Pur; ", ""),
      ifelse(f_bio_r, "Fuera Rango Bio Rend; ", ""), ifelse(f_bio_p, "Fuera Rango Bio Pur; ", "")
    ),
    Motivo_Atipico = gsub("; $", "", Motivo_Atipico),
    Motivo_Atipico = ifelse(Motivo_Atipico == "", "Normal", Motivo_Atipico),
    
    Estado_Plot = case_when(falla_roja ~ "🔴 Falla Proceso", falla_verde ~ "🟢 Atípico Tratamiento", TRUE ~ "✅ Normal")
  )

# --- IMPUTACIÓN PARA ESTADÍSTICAS ---
datos_finales_stats <- datos_analizados 
if (aplicar_imputacion) {
  datos_imputados <- datos_analizados %>%
    group_by(Trat, Rep) %>%
    mutate(mean_val_rend = mean(Rend_Eval[!falla_roja_rend], na.rm=TRUE), mean_val_pur  = mean(Pur_Eval[!falla_roja_pur], na.rm=TRUE)) %>%
    ungroup() %>% group_by(Trat) %>%
    mutate(med_trat_rend = median(Rend_Eval[!dato_sospechoso], na.rm=TRUE), med_trat_pur  = median(Pur_Eval[!dato_sospechoso], na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(
      mean_val_rend = ifelse(is.na(mean_val_rend) | is.nan(mean_val_rend), med_trat_rend, mean_val_rend),
      mean_val_pur  = ifelse(is.na(mean_val_pur) | is.nan(mean_val_pur), med_trat_pur, mean_val_pur),
      Rend_Final = case_when(falla_roja_rend ~ mean_val_rend, !imputar_solo_rojos & falla_verde_rend ~ med_trat_rend, TRUE ~ Rend_Eval),
      Pur_Final = case_when(falla_roja_pur ~ mean_val_pur, !imputar_solo_rojos & falla_verde_pur ~ med_trat_pur, TRUE ~ Pur_Eval)
    )
  datos_finales_stats <- datos_imputados 
} else { datos_finales_stats <- datos_finales_stats %>% mutate(Rend_Final = Rend_Eval, Pur_Final = Pur_Eval) }

# --- VISUALIZACIÓN INTERACTIVA (AZÚCAR CON MEDIAS DINÁMICAS HOVER Y CV) ---
colores_estado <- c("✅ Normal" = "#4A4A4A", "🔴 Falla Proceso" = "#E91E63", "🟢 Atípico Tratamiento" = "#4CAF50")

# Pre-cálculo de tablas de medias para hacerlas interactivas en Plotly y generar Resumen
med_r_trat <- datos_analizados %>% group_by(Trat) %>% summarise(Rend_Eval = mean(Rend_Eval, na.rm=TRUE), .groups="drop")
med_r_rep  <- datos_analizados %>% group_by(Trat, Rep) %>% summarise(Rend_Eval = mean(Rend_Eval, na.rm=TRUE), SD_Rend=sd(Rend_Eval, na.rm=TRUE), CV_Rend=(SD_Rend/Rend_Eval)*100, .groups="drop")
med_p_trat <- datos_analizados %>% group_by(Trat) %>% summarise(Pur_Eval = mean(Pur_Eval, na.rm=TRUE), .groups="drop")
med_p_rep  <- datos_analizados %>% group_by(Trat, Rep) %>% summarise(Pur_Eval = mean(Pur_Eval, na.rm=TRUE), SD_Pur=sd(Pur_Eval, na.rm=TRUE), CV_Pur=(SD_Pur/Pur_Eval)*100, .groups="drop")

# IMPRIMIR RESUMEN DE AZÚCAR EN CONSOLA
message("\n> Resumen Estadístico por Réplica (Azúcar):")
resumen_azucar <- med_r_rep %>%
  left_join(med_p_rep, by = c("Trat", "Rep")) %>%
  mutate(across(where(is.numeric), ~round(., 2)))
print(as.data.frame(resumen_azucar))

suppressWarnings({
  # Rendimiento - Tratamiento
  p_rend_trat <- ggplot(datos_analizados, aes(x = Trat, y = Rend_Eval)) +
    geom_boxplot(outlier.shape = NA, fill = "#f0f0f0", color = "#555555", alpha = 0.7) +
    geom_jitter(aes(color = Estado_Plot, text = paste("Submuestra:", Sub, "<br>Réplica:", Rep, "<br>Rendimiento:", Rend_Eval, "<br>Motivo:", Motivo_Atipico)), width = 0.2, size = 2.5, alpha = 0.8) +
    geom_point(data = med_r_trat, aes(x = Trat, y = Rend_Eval, text = paste("Media General:", round(Rend_Eval, 2))), shape = 18, size = 3.5, color = "blue", alpha = 0.9) +
    scale_color_manual(values = colores_estado) + theme_minimal() + labs(title = "Rendimiento por Tratamiento", x = "Tratamientos", y = "Kg/Ton")
  
  # Rendimiento - Réplicas
  p_rend_rep <- ggplot(datos_analizados, aes(x = Rep, y = Rend_Eval)) +
    geom_boxplot(outlier.shape = NA, fill = "#e6f2ff", color = "#336699", alpha = 0.6) +
    geom_jitter(aes(color = Estado_Plot, text = paste("Submuestra:", Sub, "<br>Rendimiento:", Rend_Eval, "<br>Motivo:", Motivo_Atipico)), width = 0.15, size = 2.5, alpha = 0.8) +
    geom_point(data = med_r_rep, aes(x = Rep, y = Rend_Eval, text = paste("Media Réplica:", round(Rend_Eval, 2), "<br>SD:", round(SD_Rend, 2), "<br>CV:", round(CV_Rend, 2), "%")), shape = 18, size = 3.5, color = "blue", alpha = 0.9) +
    facet_wrap(~Trat, scales = "free_x") + scale_color_manual(values = colores_estado) + theme_minimal() + labs(title = "Variabilidad de Rendimiento Intra-Réplica", x = "Réplicas", y = "Kg/Ton")
  
  # Pureza - Tratamiento
  p_pur_trat <- ggplot(datos_analizados, aes(x = Trat, y = Pur_Eval)) +
    geom_boxplot(outlier.shape = NA, fill = "#f0f0f0", color = "#555555", alpha = 0.7) +
    geom_jitter(aes(color = Estado_Plot, text = paste("Submuestra:", Sub, "<br>Réplica:", Rep, "<br>Pureza:", Pur_Eval, "%<br>Motivo:", Motivo_Atipico)), width = 0.2, size = 2.5, alpha = 0.8) +
    geom_point(data = med_p_trat, aes(x = Trat, y = Pur_Eval, text = paste("Media General:", round(Pur_Eval, 2), "%")), shape = 18, size = 3.5, color = "blue", alpha = 0.9) +
    scale_color_manual(values = colores_estado) + theme_minimal() + labs(title = "Pureza por Tratamiento", x = "Tratamientos", y = "Pureza (%)")
  
  # Pureza - Réplicas
  p_pur_rep <- ggplot(datos_analizados, aes(x = Rep, y = Pur_Eval)) +
    geom_boxplot(outlier.shape = NA, fill = "#fff2e6", color = "#cc6600", alpha = 0.6) +
    geom_jitter(aes(color = Estado_Plot, text = paste("Submuestra:", Sub, "<br>Pureza:", Pur_Eval, "%<br>Motivo:", Motivo_Atipico)), width = 0.15, size = 2.5, alpha = 0.8) +
    geom_point(data = med_p_rep, aes(x = Rep, y = Pur_Eval, text = paste("Media Réplica:", round(Pur_Eval, 2), "%<br>SD:", round(SD_Pur, 2), "<br>CV:", round(CV_Pur, 2), "%")), shape = 18, size = 3.5, color = "blue", alpha = 0.9) +
    facet_wrap(~Trat, scales = "free_x") + scale_color_manual(values = colores_estado) + theme_minimal() + labs(title = "Variabilidad de Pureza Intra-Réplica", x = "Réplicas", y = "Pureza (%)")
})

message("\n> Generando Gráficos Interactivos de Azúcar...")
print(ggplotly(p_rend_trat, tooltip = "text"))
print(ggplotly(p_rend_rep,  tooltip = "text"))
print(ggplotly(p_pur_trat,  tooltip = "text"))
print(ggplotly(p_pur_rep,   tooltip = "text"))


# ==============================================================================
# ==============================================================================
# SECCIÓN 2: TCH
# ==============================================================================
# ==============================================================================
message("\n---------------------------------------------------------")
message(">>> POR FAVOR, COPIA LOS DATOS DE TCH AL PORTAPAPELES AHORA <<<")
invisible(readline(prompt="Presiona [Enter] cuando hayas copiado los datos..."))

datos_TCH <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")

# FUNCIÓN TOLERANTE A ERRORES (Definida también aquí para evitar fallos si solo corres esta sección)
buscar_columna <- function(nombres_existentes, patrones_busqueda, nombre_defecto) {
  for (patron in patrones_busqueda) {
    coincidencia <- grep(patron, nombres_existentes, ignore.case = TRUE, value = TRUE)
    if (length(coincidencia) > 0) return(coincidencia[1])
  }
  return(nombre_defecto)
}

# --- DETECCIÓN AUTOMÁTICA INTELIGENTE DE COLUMNAS (TCH) ---
nombres_tch <- names(datos_TCH)
c_tch  <- buscar_columna(nombres_tch, c("^tch$", "tch_calc", "tch"), "TCH")
c_peso <- buscar_columna(nombres_tch, c("peso.neto", "neto", "peso", "pesoneto"), "Peso.Neto")
c_mts  <- buscar_columna(nombres_tch, c("metros.lineales", "metros", "lineales", "mts"), "Metros.lineales")
c_trat_tch <- buscar_columna(nombres_tch, c("tratamientos", "tratamiento", "trat", "trt"), "Tratamientos")
c_rep_tch  <- buscar_columna(nombres_tch, c("replica", "rep", "rep."), "Replica")
c_carreta  <- buscar_columna(nombres_tch, c("carreta", "no.carreta", "carro", "id"), "No.carreta")

message(paste0("\n[TCH] Columnas detectadas de forma automática:\n",
               "  - TCH -> '", c_tch, "'\n", "  - Peso Neto -> '", c_peso, "'\n",
               "  - Metros Lineales -> '", c_mts, "'\n", "  - Tratamiento -> '", c_trat_tch, "'\n",
               "  - Réplica -> '", c_rep_tch, "'\n", "  - Carreta -> '", c_carreta, "'\n"))

# Estandarización de nombres interna para TCH
datos_tch_estandar <- datos_TCH %>%
  rename(TCH_Orig = !!sym(c_tch), Peso_Orig = !!sym(c_peso), Mts_Orig = !!sym(c_mts), 
         Trat_Orig = !!sym(c_trat_tch), Rep_Orig = !!sym(c_rep_tch), Car_Orig = !!sym(c_carreta))

# --- CONFIGURACIÓN DE PARÁMETROS ---
manual_corte  <- 2     
manual_tercio <- 3     
max_cv_permitido <- 12  
var_audit <- c("TCH") # Audita las 3 para tener los CV y SD guardados

# --- MATRIZ BIOLÓGICA (TCH) ---
parametros_bio_tch <- expand.grid(tercio_zafra = c("Tercio 1", "Tercio 2", "Tercio 3"),
                                  corte_cat = c("Plantilla", "Soca 1", "Soca 2", "Soca 3")) %>%
  mutate(LI_Bio = case_when(corte_cat == "Plantilla" ~ 100, corte_cat == "Soca 1" ~ 90, TRUE ~ 75),
         LS_Bio = case_when(corte_cat == "Plantilla" ~ 185, corte_cat == "Soca 1" ~ 165, TRUE ~ 140))

corte_ensayo <- case_when(manual_corte==0~"Plantilla", manual_corte==1~"Soca 1", manual_corte==2~"Soca 2", TRUE~"Soca 3")
tercio_ensayo <- case_when(manual_tercio==1~"Tercio 1", manual_tercio==2~"Tercio 2", TRUE~"Tercio 3")

# --- PROCESAMIENTO DINÁMICO ---
datos_analizados_tch <- datos_tch_estandar %>%
  mutate(eval_TCH = as.numeric(as.character(TCH_Orig)),
         eval_Peso.Neto = as.numeric(as.character(Peso_Orig)),
         eval_Metros.lineales = as.numeric(as.character(Mts_Orig)),
         Trat = as.factor(Trat_Orig), Rep = as.character(Rep_Orig), Sub = as.character(Car_Orig)) %>%
  left_join(parametros_bio_tch %>% filter(corte_cat == corte_ensayo & tercio_zafra == tercio_ensayo), by = character())

evaluar_variable <- function(data, col_name) {
  val_col <- paste0("eval_", col_name)
  data %>%
    group_by(Trat, Rep) %>%
    mutate(
      !!paste0("sd_", col_name, "_rep") := sd(.data[[val_col]], na.rm=T),
      !!paste0("cv_", col_name, "_rep") := (sd(.data[[val_col]], na.rm=T)/mean(.data[[val_col]], na.rm=T))*100,
      cv_rep = (sd(.data[[val_col]], na.rm=T)/mean(.data[[val_col]], na.rm=T))*100,
      li_iqr_rep = quantile(.data[[val_col]], 0.25, na.rm=T) - 1.5*IQR(.data[[val_col]], na.rm=T),
      ls_iqr_rep = quantile(.data[[val_col]], 0.75, na.rm=T) + 1.5*IQR(.data[[val_col]], na.rm=T)) %>%
    ungroup() %>% group_by(Trat) %>%
    mutate(li_iqr_trat = quantile(.data[[val_col]], 0.25, na.rm=T) - 1.5*IQR(.data[[val_col]], na.rm=T),
           ls_iqr_trat = quantile(.data[[val_col]], 0.75, na.rm=T) + 1.5*IQR(.data[[val_col]], na.rm=T)) %>%
    ungroup() %>%
    mutate(rojo = (.data[[val_col]] < li_iqr_rep | .data[[val_col]] > ls_iqr_rep) | (cv_rep > max_cv_permitido),
           fuera_bio = if(col_name == "TCH") (.data[[val_col]] < LI_Bio | .data[[val_col]] > LS_Bio) else FALSE,
           fuera_iqr_trat = (.data[[val_col]] < li_iqr_trat | .data[[val_col]] > ls_iqr_trat),
           verde = !rojo & (fuera_bio | fuera_iqr_trat),
           !!paste0("Estado_", col_name) := case_when(rojo ~ "🔴 Falla Rep", verde ~ "🟢 Atípico", TRUE ~ "✅ Normal")) %>%
    dplyr::select(-cv_rep, -li_iqr_rep, -ls_iqr_rep, -li_iqr_trat, -ls_iqr_trat, -rojo, -verde, -fuera_bio, -fuera_iqr_trat)
}

for(v in var_audit) { datos_analizados_tch <- evaluar_variable(datos_analizados_tch, v) }

# --- VISUALIZACIÓN INTERACTIVA (TCH CON MEDIAS DINÁMICAS HOVER Y CV) ---
colores_estado_tch <- c("✅ Normal" = "#4A4A4A", "🔴 Falla Rep" = "#E91E63", "🟢 Atípico" = "#4CAF50")

# Pre-cálculo de tablas de medias específicas para TCH y Resúmenes
m_tch_tr  <- datos_analizados_tch %>% group_by(Trat) %>% summarise(eval_TCH = mean(eval_TCH, na.rm=T), .groups="drop")
m_tch_rp  <- datos_analizados_tch %>% group_by(Trat, Rep) %>% summarise(eval_TCH = mean(eval_TCH, na.rm=T), SD_TCH=sd(eval_TCH, na.rm=T), CV_TCH=(SD_TCH/eval_TCH)*100, .groups="drop")

m_peso_tr <- datos_analizados_tch %>% group_by(Trat) %>% summarise(eval_Peso.Neto = mean(eval_Peso.Neto, na.rm=T), .groups="drop")
m_peso_rp <- datos_analizados_tch %>% group_by(Trat, Rep) %>% summarise(eval_Peso.Neto = mean(eval_Peso.Neto, na.rm=T), SD_Peso=sd(eval_Peso.Neto, na.rm=T), CV_Peso=(SD_Peso/eval_Peso.Neto)*100, .groups="drop")

m_mts_tr  <- datos_analizados_tch %>% group_by(Trat) %>% summarise(eval_Metros.lineales = mean(eval_Metros.lineales, na.rm=T), .groups="drop")
m_mts_rp  <- datos_analizados_tch %>% group_by(Trat, Rep) %>% summarise(eval_Metros.lineales = mean(eval_Metros.lineales, na.rm=T), SD_Mts=sd(eval_Metros.lineales, na.rm=T), CV_Mts=(SD_Mts/eval_Metros.lineales)*100, .groups="drop")

# IMPRIMIR RESUMEN DE TCH EN CONSOLA
message("\n> Resumen Estadístico por Réplica (TCH, Peso y Metros):")
resumen_tch <- m_tch_rp %>%
  left_join(m_peso_rp, by = c("Trat", "Rep")) %>%
  left_join(m_mts_rp, by = c("Trat", "Rep")) %>%
  mutate(across(where(is.numeric), ~round(., 2)))
print(as.data.frame(resumen_tch))

suppressWarnings({
  # TCH - Tratamiento
  p_tch_trat <- ggplot(datos_analizados_tch, aes(x = Trat, y = eval_TCH)) +
    geom_boxplot(outlier.shape = NA, fill = "#e8f5e9", color = "#2e7d32", alpha = 0.6) +
    geom_jitter(aes(color = Estado_TCH, text = paste("Carreta:", Sub, "<br>Réplica:", Rep, "<br>TCH:", round(eval_TCH, 2), "<br>Diagnóstico:", Estado_TCH)), width = 0.2, size = 2.5, alpha = 0.8) +
    geom_point(data = m_tch_tr, aes(x = Trat, y = eval_TCH, text = paste("Media General:", round(eval_TCH, 2))), shape = 18, size = 3.5, color = "blue", alpha = 0.9) +
    scale_color_manual(values = colores_estado_tch) + theme_minimal() + labs(title = "TCH por Tratamiento", x = "Tratamientos", y = "TCH")
  
  # TCH - Réplica
  p_tch_rep <- ggplot(datos_analizados_tch, aes(x = Rep, y = eval_TCH)) +
    geom_boxplot(outlier.shape = NA, fill = "#e8f5e9", color = "#2e7d32", alpha = 0.1) +
    geom_jitter(aes(color = Estado_TCH, text = paste("Carreta:", Sub, "<br>TCH:", round(eval_TCH, 2), "<br>Diagnóstico:", Estado_TCH)), width = 0.15, size = 2.5, alpha = 0.8) +
    geom_point(data = m_tch_rp, aes(x = Rep, y = eval_TCH, text = paste("Media Réplica:", round(eval_TCH, 2), "<br>SD:", round(SD_TCH, 2), "<br>CV:", round(CV_TCH, 2), "%")), shape = 18, size = 3.5, color = "blue", alpha = 0.9) +
    facet_wrap(~Trat, scales = "free_x") + scale_color_manual(values = colores_estado_tch) + theme_minimal() + labs(title = "Variabilidad de TCH Intra-Réplica", x = "Réplicas", y = "TCH")
  
  # PESO NETO - Tratamiento
  p_peso_trat <- ggplot(datos_analizados_tch, aes(x = Trat, y = eval_Peso.Neto)) +
    geom_boxplot(outlier.shape = NA, fill = "#e3f2fd", color = "#1565c0", alpha = 0.6) +
    geom_jitter(aes(color = Estado_Peso.Neto, text = paste("Carreta:", Sub, "<br>Réplica:", Rep, "<br>Peso Neto:", round(eval_Peso.Neto, 2), "<br>Diagnóstico:", Estado_Peso.Neto)), width = 0.2, size = 2.5, alpha = 0.8) +
    geom_point(data = m_peso_tr, aes(x = Trat, y = eval_Peso.Neto, text = paste("Media General:", round(eval_Peso.Neto, 2))), shape = 18, size = 3.5, color = "blue", alpha = 0.9) +
    scale_color_manual(values = colores_estado_tch) + theme_minimal() + labs(title = "Peso Neto por Tratamiento", x = "Tratamientos", y = "Peso Neto")
  
  # PESO NETO - Réplica
  p_peso_rep <- ggplot(datos_analizados_tch, aes(x = Rep, y = eval_Peso.Neto)) +
    geom_boxplot(outlier.shape = NA, fill = "#e3f2fd", color = "#1565c0", alpha = 0.4) +
    geom_jitter(aes(color = Estado_Peso.Neto, text = paste("Carreta:", Sub, "<br>Peso Neto:", round(eval_Peso.Neto, 2), "<br>Diagnóstico:", Estado_Peso.Neto)), width = 0.15, size = 2.5, alpha = 0.8) +
    geom_point(data = m_peso_rp, aes(x = Rep, y = eval_Peso.Neto, text = paste("Media Réplica:", round(eval_Peso.Neto, 2), "<br>SD:", round(SD_Peso, 2), "<br>CV:", round(CV_Peso, 2), "%")), shape = 18, size = 3.5, color = "blue", alpha = 0.9) +
    facet_wrap(~Trat, scales = "free_x") + scale_color_manual(values = colores_estado_tch) + theme_minimal() + labs(title = "Variabilidad de Peso Neto Intra-Réplica", x = "Réplicas", y = "Peso Neto")
  
  # METROS LINEALES - Tratamiento
  p_mts_trat <- ggplot(datos_analizados_tch, aes(x = Trat, y = eval_Metros.lineales)) +
    geom_boxplot(outlier.shape = NA, fill = "#fff3e0", color = "#e65100", alpha = 0.6) +
    geom_jitter(aes(color = Estado_Metros.lineales, text = paste("Carreta:", Sub, "<br>Réplica:", Rep, "<br>Metros:", round(eval_Metros.lineales, 2), "<br>Diagnóstico:", Estado_Metros.lineales)), width = 0.2, size = 2.5, alpha = 0.8) +
    geom_point(data = m_mts_tr, aes(x = Trat, y = eval_Metros.lineales, text = paste("Media General:", round(eval_Metros.lineales, 2))), shape = 18, size = 3.5, color = "blue", alpha = 0.9) +
    scale_color_manual(values = colores_estado_tch) + theme_minimal() + labs(title = "Metros Lineales por Tratamiento", x = "Tratamientos", y = "Metros Lineales")
  
  # METROS LINEALES - Réplica
  p_mts_rep <- ggplot(datos_analizados_tch, aes(x = Rep, y = eval_Metros.lineales)) +
    geom_boxplot(outlier.shape = NA, fill = "#fff3e0", color = "#e65100", alpha = 0.4) +
    geom_jitter(aes(color = Estado_Metros.lineales, text = paste("Carreta:", Sub, "<br>Metros:", round(eval_Metros.lineales, 2), "<br>Diagnóstico:", Estado_Metros.lineales)), width = 0.15, size = 2.5, alpha = 0.8) +
    geom_point(data = m_mts_rp, aes(x = Rep, y = eval_Metros.lineales, text = paste("Media Réplica:", round(eval_Metros.lineales, 2), "<br>SD:", round(SD_Mts, 2), "<br>CV:", round(CV_Mts, 2), "%")), shape = 18, size = 3.5, color = "blue", alpha = 0.9) +
    facet_wrap(~Trat, scales = "free_x") + scale_color_manual(values = colores_estado_tch) + theme_minimal() + labs(title = "Variabilidad de Metros Lineales Intra-Réplica", x = "Réplicas", y = "Metros Lineales")
})

message("\n> Generando Gráficos Interactivos de TCH...")
print(ggplotly(p_tch_trat, tooltip = "text"))
print(ggplotly(p_tch_rep,  tooltip = "text"))
print(ggplotly(p_peso_trat, tooltip = "text"))
print(ggplotly(p_peso_rep,  tooltip = "text"))
print(ggplotly(p_mts_trat, tooltip = "text"))
print(ggplotly(p_mts_rep,  tooltip = "text"))

message("\n*** PROCESO FINALIZADO CON ÉXITO ***")
