paquetes <- c(
  "openxlsx", "fBasics", "psych", "modeest", "ggrepel", "GGally", 
  "mice", "corrplot", "readxl", "doebioresearch", "performance", "dplyr", 
  "ScottKnott", "agricolae", "car", "broom", "data.table", "emmeans", 
  "ggplot2", "tidyverse", "lattice", "nlme", "lme4", "lmerTest", "multcomp", 
  "rstatix", "ggpubr", "see", "MASS", "lsmeans", "scales", "lmtest", "multcompView", 
  "googlesheets4", "googledrive", "clipr", "FactoMineR", "factoextra", 
  "glmmTMB", "DHARMa", "MuMIn", "hnp", "effects", "sjstats", "ExpDes", "sf", "tmap", "terra",
  "RVAideMemoire", "RColorBrewer", "DiagrammeR", "esquisse", "dlookr")

# Verifica cuáles no están instalados
no_instalados <- paquetes[!(paquetes %in% installed.packages()[,"Package"])];no_instalados

# Instala los que faltan
if(length(no_instalados)) {
  install.packages(no_instalados)
} else {
  message("Todos los paquetes ya están instalados.")
}

# Carga todos los paquetes (opcional)
lapply(paquetes, library, character.only = TRUE)



datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")

attach(datos)
library(dplyr)
library(tidyr)
library(agricolae)
library(purrr)
names(datos)
View(datos)

# Conversión de variables

  # "TCH", "Rendimiento", "TAH", "Pureza", "TCH_Rel", "Rend_Rel", "TAH_Rel", "P_Rel"

  # "TCH_", "Rendimiento.Kg.Ton", "TAH_", "Pureza_", "TCH_relativo", "Rend_relativo", "TAH_relativo", "Pureza_relativa


cols_a_numerica <- intersect(names(datos), c("TCH", "TCH_Rel"))

vr<- c("TCH", "TCH_Rel")

cols_a_factor <- intersect(names(datos), c("Tratamiento", "Replica", "Microorganismos", "Biosustrato", "Biochar","Dosis_Biochar"))


datos <- datos %>%
  mutate(across(all_of(cols_a_numerica), ~as.numeric(as.character(.)))) %>%
  mutate(across(all_of(cols_a_factor), as.factor))

sapply(datos, class)


# =========================================================================
# CONFIGURACIÓN
# =========================================================================
names(datos)

tipo_experimento     <- "factorial" # simple o factorial 
factores             <- c("Biochar" ,"Dosis_Biochar", "Microorganismos") 
bloque               <- "Replica" 
variables_resp       <- c("TCH", "TCH_Rel")

# Indica aquí las variables a las que NO quieres que se les agreguen las letras
variables_sin_letras <- c("TCH_Rel") # Ejemplo: Solo mostrará la media para Rendimiento

prueba_medias        <- "Tukey" 
modo_letras          <- "auto" # auto, interaccion, Biosustrato o cualquier factor individual
fincas_por_tabla     <- 1
columna_finca        <- "Zafra"
nivel_sig            <- 0.15

# =========================================================================
# PROCESAMIENTO (CORREGIDO PARA EVITAR DETENCIONES POR ERRORES)
# =========================================================================

fincas_unicas <- unique(datos[[columna_finca]])
resultados_totales <- list()

for (finca_actual in fincas_unicas) {
  df_finca <- datos %>% dplyr::filter(!!sym(columna_finca) == finca_actual) %>% droplevels()
  if (nrow(df_finca) == 0) next
  
  factores_validos <- purrr::keep(factores, ~ length(unique(na.omit(df_finca[[.x]]))) >= 2)
  if (length(unique(na.omit(df_finca[[bloque]]))) < 2 || length(factores_validos) == 0) next
  if (tipo_experimento != "factorial") factores_validos <- factores_validos[1]
  
  df_finca[[bloque]] <- as.factor(df_finca[[bloque]])
  for (fac in factores_validos) df_finca[[fac]] <- as.factor(df_finca[[fac]])
  
  grid_tratamientos <- df_finca %>% 
    dplyr::select(dplyr::all_of(factores_validos)) %>% 
    dplyr::distinct() %>% 
    dplyr::arrange(dplyr::across(dplyr::all_of(factores_validos)))
  
  tabla_finca <- grid_tratamientos
  p_valores_lista <- list()
  nombres_p_guardados <- c()
  
  for (var in variables_resp) {
    # Usamos tryCatch para que si esta variable falla, el script siga con la siguiente
    resultado_variable <- tryCatch({
      
      # 1. Verificación de varianza (evita el error de perfect fit antes de que ocurra)
      if (var(df_finca[[var]], na.rm = TRUE) == 0) stop("Varianza cero")
      
      termino_factores <- paste(factores_validos, collapse = " * ")
      formula_mod <- as.formula(paste(var, "~", bloque, "+", termino_factores))
      
      modelo <- aov(formula_mod, data = df_finca)
      anova_res <- anova(modelo)
      
      # Extracción de P-valores
      terminos_anova <- rownames(anova_res)[!rownames(anova_res) %in% c(bloque, "Residuals")]
      p_vals_fmt <- c()
      for (term in terminos_anova) {
        p_val <- anova_res[term, "Pr(>F)"]
        nombre_p <- paste0("P-valor (", term, ")")
        if (!nombre_p %in% nombres_p_guardados) nombres_p_guardados <- c(nombres_p_guardados, nombre_p)
        p_vals_fmt[nombre_p] <- ifelse(!is.na(p_val) && p_val < 0.01, "<0.01*", 
                                       ifelse(!is.na(p_val) && p_val <= nivel_sig, paste0(sprintf("%.2f", p_val), "*"), 
                                              ifelse(!is.na(p_val), sprintf("%.2f", p_val), "NA")))
      }
      
      # 4. SELECCIÓN DE TRATAMIENTO PARA PRUEBA DE MEDIAS (Tu lógica original)
      interaccion_max <- paste(factores_validos, collapse = ":")
      if (modo_letras == "auto") {
        p_int_max <- if(interaccion_max %in% rownames(anova_res)) anova_res[interaccion_max, "Pr(>F)"] else 1
        if (!is.na(p_int_max) && p_int_max <= nivel_sig) {
          trt_prueba <- factores_validos
        } else {
          p_principales <- anova_res[factores_validos, "Pr(>F)"]
          trt_prueba <- if (any(p_principales <= nivel_sig, na.rm = TRUE)) factores_validos[which.min(p_principales)] else factores_validos
        }
      } else {
        trt_prueba <- if(modo_letras == "interaccion") factores_validos else unlist(strsplit(modo_letras, ":"))
      }
      
      # 5. PRUEBA DE MEDIAS
      if (prueba_medias == "Tukey") {
        prueba <- HSD.test(modelo, trt_prueba, alpha = nivel_sig, group = TRUE)
      } else {
        prueba <- LSD.test(modelo, trt_prueba, alpha = nivel_sig, group = TRUE)
      }
      
      grupos <- prueba$groups
      grupos <- grupos[order(grupos[[var]], decreasing = TRUE), ]
      grupos$Letras <- toupper(trimws(as.character(grupos$groups)))
      
      incluir_letras <- !(var %in% variables_sin_letras)
      
      # 6. UNIÓN DE RESULTADOS
      if (length(trt_prueba) > 1) {
        grupos_sep <- grupos %>%
          tibble::rownames_to_column("Tratamiento") %>%
          tidyr::separate(Tratamiento, into = trt_prueba, sep = ":") %>%
          dplyr::mutate(dplyr::across(dplyr::all_of(trt_prueba), as.factor))
        grupos_sep$Media <- round(grupos_sep[[var]], 2)
        res_var <- grid_tratamientos %>% dplyr::left_join(grupos_sep, by = trt_prueba)
        val_final <- if (incluir_letras) paste(res_var$Media, res_var$Letras) else as.character(res_var$Media)
      } else {
        grupos_sep <- grupos %>%
          tibble::rownames_to_column(trt_prueba) %>%
          dplyr::mutate(!!sym(trt_prueba) := as.factor(!!sym(trt_prueba)))
        res_var <- aggregate(as.formula(paste(var, "~", paste(factores_validos, collapse = "+"))), data = df_finca, FUN = mean)
        res_var[[var]] <- round(res_var[[var]], 2)
        res_var <- res_var %>% dplyr::left_join(grupos_sep %>% dplyr::select(dplyr::all_of(trt_prueba), Letras), by = trt_prueba)
        val_final <- if (incluir_letras) paste(res_var[[var]], res_var$Letras) else as.character(res_var[[var]])
      }
      
      # Retornar lista con p-valores y el vector de resultados
      list(p_vals = p_vals_fmt, datos_col = val_final)
      
    }, error = function(e) {
      # Si hay error (perfect fit, NaNs, etc), devolvemos valores vacíos para que el script no se detenga
      list(p_vals = setNames(rep("Error", length(factores_validos)), paste0("P-valor (", factores_validos, ")")), 
           datos_col = rep("N.A", nrow(grid_tratamientos)))
    })
    
    # Integrar los resultados obtenidos (o el error) a la tabla de la finca
    p_valores_lista[[var]] <- resultado_variable$p_vals
    tabla_finca[[var]] <- resultado_variable$datos_col
  }
  
  # Filas de P-valores
  if(length(nombres_p_guardados) > 0) {
    filas_p <- data.frame(matrix("", ncol = length(factores_validos), nrow = length(nombres_p_guardados)))
    colnames(filas_p) <- factores_validos
    filas_p[[factores_validos[1]]] <- nombres_p_guardados
    for (var in variables_resp) { 
      for(np in nombres_p_guardados) {
        val_p <- p_valores_lista[[var]][np]
        filas_p[filas_p[[factores_validos[1]]] == np, var] <- ifelse(is.null(val_p) || is.na(val_p), "NA", val_p)
      }
    }
    tabla_finca <- dplyr::bind_rows(tabla_finca %>% dplyr::mutate(across(everything(), as.character)), 
                                    filas_p %>% dplyr::mutate(across(everything(), as.character)))
  }
  
  tabla_finca <- tabla_finca %>% 
    dplyr::mutate(Finca = ifelse(dplyr::row_number() == 1, finca_actual, "")) %>% 
    dplyr::select(Finca, dplyr::everything())
  
  resultados_totales[[finca_actual]] <- tabla_finca
}

# =========================================================================
# RESULTADOS Y VISUALIZACIÓN
# =========================================================================

fincas_procesadas <- names(resultados_totales)

if (length(fincas_procesadas) > 0) {
  n_chunks <- ceiling(length(fincas_procesadas) / fincas_por_tabla)
  chunks_fincas <- split(fincas_procesadas, cut(seq_along(fincas_procesadas), n_chunks, labels = FALSE))
  
  for (i in seq_along(chunks_fincas)) {
    fincas_en_chunk <- chunks_fincas[[i]]
    tabla_final <- as.data.frame(dplyr::bind_rows(resultados_totales[fincas_en_chunk]))
    
    nombre_tabla <- paste0("Tabla_Resultados_", i)
    assign(nombre_tabla, tabla_final)
    
    View(get(nombre_tabla), title = nombre_tabla)
  }
}

