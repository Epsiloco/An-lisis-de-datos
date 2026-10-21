# ==============================================================================
# BLOQUE B: CÁLCULOS PARA PIVOT CON BASE 1.0 FORZADA (1, 2 o 3 FACTORES)
# ==============================================================================

calcular_relativos_sueltos <- function(factor1_col, factor1_testigo, 
                                       factor2_col = NULL, factor2_testigo = NULL, 
                                       factor3_col = NULL, factor3_testigo = NULL,
                                       cols_valores) {
  
  # 1. Leer datos
  datos_pegados <- read_clip_tbl(header = TRUE)
  columnas_copiadas <- names(datos_pegados)
  
  if (!(factor1_col %in% columnas_copiadas)) {
    stop("Error: Falta la columna del Factor 1 en tu selección.")
  }
  
  c_f1 <- sym(factor1_col)
  datos_resultado <- datos_pegados
  
  # 2. Cálculo de FWC
  if (all(c("Pureza", "Rendimiento", "TAH") %in% columnas_copiadas)) {
    datos_resultado <- datos_resultado %>%
      mutate(
        Pureza = if_else(as.numeric(Pureza) <= 0, NA_real_, as.numeric(Pureza)),
        FWC = 1.4 - (40 / Pureza),
        FWC_Rend = as.numeric(Rendimiento) * FWC,
        FWC_TAH = as.numeric(TAH) * FWC
      )
    cols_valores <- c(cols_valores, "FWC_Rend", "FWC_TAH")
  }
  
  # Asegurar numéricos
  datos_resultado <- datos_resultado %>% mutate(across(any_of(cols_valores), as.numeric))
  
  # 3. CÁLCULOS SEGÚN NÚMERO DE FACTORES
  if (!is.null(factor3_col) && !is.null(factor3_testigo)) {
    # --- ESCENARIO C: TRIFACTORIAL (3 Factores) ---
    if (!(factor2_col %in% columnas_copiadas) | !(factor3_col %in% columnas_copiadas)) {
      stop("Error: Faltan las columnas del Factor 2 o Factor 3.")
    }
    
    c_f2 <- sym(factor2_col)
    c_f3 <- sym(factor3_col)
    
    datos_resultado <- datos_resultado %>%
      mutate(across(any_of(cols_valores), 
                    ~ .x / mean(.x[!!c_f1 == factor1_testigo & !!c_f2 == factor2_testigo & !!c_f3 == factor3_testigo], na.rm = TRUE), 
                    .names = "{.col}_Rel")) %>%
      mutate(across(any_of(cols_valores), 
                    ~ .x / mean(.x[!!c_f1 == factor1_testigo], na.rm = TRUE), 
                    .names = "{.col}_Rel2")) %>%
      mutate(across(any_of(cols_valores), 
                    ~ .x / mean(.x[!!c_f2 == factor2_testigo], na.rm = TRUE), 
                    .names = "{.col}_Rel3")) %>%
      mutate(across(any_of(cols_valores), 
                    ~ .x / mean(.x[!!c_f3 == factor3_testigo], na.rm = TRUE), 
                    .names = "{.col}_Rel4")) %>%
      # Forzar bases a 1.0 exacto
      mutate(
        across(ends_with("_Rel"),  ~ if_else(!!c_f1 == factor1_testigo & !!c_f2 == factor2_testigo & !!c_f3 == factor3_testigo, 1.0, .x)),
        across(ends_with("_Rel2"), ~ if_else(!!c_f1 == factor1_testigo, 1.0, .x)),
        across(ends_with("_Rel3"), ~ if_else(!!c_f2 == factor2_testigo, 1.0, .x)),
        across(ends_with("_Rel4"), ~ if_else(!!c_f3 == factor3_testigo, 1.0, .x))
      )
    
  } else if (!is.null(factor2_col) && !is.null(factor2_testigo)) {
    # --- ESCENARIO B: BIFACTORIAL (2 Factores) ---
    if (!(factor2_col %in% columnas_copiadas)) stop("Error: Falta la columna del Factor 2.")
    c_f2 <- sym(factor2_col)
    
    datos_resultado <- datos_resultado %>%
      mutate(across(any_of(cols_valores), 
                    ~ .x / mean(.x[!!c_f1 == factor1_testigo & !!c_f2 == factor2_testigo], na.rm = TRUE), 
                    .names = "{.col}_Rel")) %>%
      mutate(across(any_of(cols_valores), 
                    ~ .x / mean(.x[!!c_f1 == factor1_testigo], na.rm = TRUE), 
                    .names = "{.col}_Rel2")) %>%
      mutate(across(any_of(cols_valores), 
                    ~ .x / mean(.x[!!c_f2 == factor2_testigo], na.rm = TRUE), 
                    .names = "{.col}_Rel3")) %>%
      # Forzar bases a 1.0 exacto
      mutate(
        across(ends_with("_Rel"),  ~ if_else(!!c_f1 == factor1_testigo & !!c_f2 == factor2_testigo, 1.0, .x)),
        across(ends_with("_Rel2"), ~ if_else(!!c_f1 == factor1_testigo, 1.0, .x)),
        across(ends_with("_Rel3"), ~ if_else(!!c_f2 == factor2_testigo, 1.0, .x))
      )
    
  } else {
    # --- ESCENARIO A: MONOFACTORIAL (1 Factor) ---
    datos_resultado <- datos_resultado %>%
      mutate(across(any_of(cols_valores), 
                    ~ .x / mean(.x[!!c_f1 == factor1_testigo], na.rm = TRUE), 
                    .names = "{.col}_Rel")) %>%
      mutate(across(any_of(cols_valores), 
                    ~ .x / mean(.x[!!c_f1 == factor1_testigo], na.rm = TRUE), 
                    .names = "{.col}_Rel2")) %>%
      # Forzar bases a 1.0 exacto
      mutate(
        across(ends_with("_Rel"),  ~ if_else(!!c_f1 == factor1_testigo, 1.0, .x)),
        across(ends_with("_Rel2"), ~ if_else(!!c_f1 == factor1_testigo, 1.0, .x))
      )
  }
  
  # 4. Devolver al portapapeles
  write_clip(datos_resultado)
  cat("\n¡Listo! Divisiones calculadas y testigos forzados a 1.0. Presiona Ctrl + V.\n")
  
  return(datos_resultado)
}


# 2 factores
variables_para_portapapeles <- c("TCH", "Rendimiento", "TAH", "Pureza")

resultados_sueltos <- calcular_relativos_sueltos(
  factor1_col     = "Bacteria",      
  factor1_testigo = "Sin Bacteria",                
  factor2_col     = "Sustrato",        # Reemplaza con tu segundo factor    
  factor2_testigo = "Sin SB",             # Reemplaza con tu segundo testigo      
  cols_valores    = variables_para_portapapeles
)





# 3 factores

variables_para_portapapeles <- c("TCH", "Rendimiento", "TAH", "Pureza")

resultados_sueltos <- calcular_relativos_sueltos(
  factor1_col     = "Bacteria",      
  factor1_testigo = "Sin Bacteria",                
  factor2_col     = "Sustrato",        # Reemplaza con tu segundo factor    
  factor2_testigo = "Sin SB",             # Reemplaza con tu segundo testigo      
  factor3_col     = "Nitrogeno",           
  factor3_testigo = "0 %N",              
  cols_valores    = variables_para_portapapeles
)









library(clipr)
str(read_clip_tbl(header = TRUE))
