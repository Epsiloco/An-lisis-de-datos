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
###########################################################################

datos<- read_clip_tbl()
View(datos)
attach(datos)
names(datos)

##################################################################################



# ==============================================================================
# FUNCIÓN AUTOMATIZADA MULTI-VARIABLE: MAGNITUD DE EFECTO (COHEN)
# ==============================================================================

# Cargar librerías necesarias
library(dplyr)
library(ggplot2)
library(knitr)

generar_reporte_multivariable <- function(df, vars_respuesta, var_tratamiento, var_bloque) {
  
  # 0. Asegurar que los agrupadores sean factores
  df[[var_tratamiento]] <- as.factor(df[[var_tratamiento]])
  df[[var_bloque]] <- as.factor(df[[var_bloque]])
  
  # BUCLE PARA EVALUAR CADA VARIABLE SOLICITADA
  for (var in vars_respuesta) {
    
    cat("\n\n######################################################################\n")
    cat(sprintf("        ANALIZANDO VARIABLE: %s        \n", toupper(var)))
    cat("######################################################################\n")
    
    # 1. ANDEVA (Análisis de Varianza Dinámico)
    formula_aov <- as.formula(paste(var, "~", var_tratamiento, "+", var_bloque))
    modelo_anova <- aov(formula_aov, data = df)
    
    # 2. EXTRACCIÓN DE ESTADÍSTICOS (El Error)
    CME <- summary(modelo_anova)[[1]]["Residuals", "Mean Sq"]
    DE_ensayo <- sqrt(CME)
    media_general <- mean(df[[var]], na.rm = TRUE)
    CV_ensayo <- (DE_ensayo / media_general) * 100
    
    cat("\n--- ESTADÍSTICOS DE CALIDAD DEL ENSAYO ---\n")
    cat(sprintf("Media General    : %.2f\n", media_general))
    cat(sprintf("Desviación (DE)  : %.2f\n", DE_ensayo))
    cat(sprintf("Coef. Variación  : %.2f %%\n\n", CV_ensayo))
    
    # 3. CONSTRUCCIÓN DE LA TABLA (d de Cohen)
    resumen_tratamientos <- df %>%
      group_by(Tratamiento = .data[[var_tratamiento]]) %>%
      summarise(Media_Variable = round(mean(.data[[var]], na.rm = TRUE), 2)) %>%
      arrange(desc(Media_Variable)) 
    
    # Detectar automáticamente el tratamiento ganador
    valor_referencia <- resumen_tratamientos$Media_Variable[1]
    nombre_referencia <- as.character(resumen_tratamientos$Tratamiento[1])
    
    tabla_cohen <- resumen_tratamientos %>%
      mutate(
        Diferencia_vs_Top = ifelse(Tratamiento == nombre_referencia, "-", 
                                   sprintf("%.2f", abs(valor_referencia - Media_Variable))),
        
        valor_d = ifelse(Tratamiento == nombre_referencia, NA, 
                         abs(valor_referencia - Media_Variable) / DE_ensayo),
        
        Etiqueta_d = case_when(
          is.na(valor_d) ~ "-",
          valor_d >= 0.8 ~ paste0(sprintf("%.2f", valor_d), " (Grande)"),
          valor_d >= 0.5 ~ paste0(sprintf("%.2f", valor_d), " (Medio)"),
          TRUE           ~ paste0(sprintf("%.2f", valor_d), " (Pequeño)")
        )
      ) %>%
      select(Tratamiento, Media_Variable, Diferencia_vs_Top, Etiqueta_d)
    
    # Renombrar dinámicamente las columnas
    nombres_columnas <- c(var_tratamiento, var, 
                          paste("Diferencia vs Top"), 
                          "Magnitud Efecto (Cohen)")
    names(tabla_cohen) <- nombres_columnas
    
    # Imprimir tabla
    print(kable(tabla_cohen, align = "c"))
    cat("\n")
    
    # 4. GRÁFICO GERENCIAL
    n_replicas <- length(unique(df[[var_bloque]]))
    error_estandar <- DE_ensayo / sqrt(n_replicas)
    
    grafico <- ggplot(resumen_tratamientos, 
                      aes(x = reorder(Tratamiento, Media_Variable), 
                          y = Media_Variable, 
                          fill = Tratamiento == nombre_referencia)) +
      
      geom_bar(stat = "identity", width = 0.65, color = "black", alpha = 0.9) +
      geom_errorbar(aes(ymin = Media_Variable - error_estandar, 
                        ymax = Media_Variable + error_estandar), 
                    width = 0.15, color = "black", linewidth = 0.8) +
      geom_text(aes(label = sprintf("%.2f", Media_Variable)), 
                vjust = -2.5, size = 5.5, fontface = "bold") +
      
      scale_fill_manual(values = c("TRUE" = "#D9534F", "FALSE" = "#5BC0DE")) + 
      
      labs(
        title = paste("Impacto Agronómico:", toupper(var)),
        subtitle = sprintf("Calidad del ensayo: CV = %.2f%% | Referencia: %s", 
                           CV_ensayo, substr(nombre_referencia, 1, 25)),
        x = "",
        y = var
      ) +
      
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(color = "grey40", size = 12, hjust = 0.5, margin = margin(b = 15)),
        axis.text.x = element_text(angle = 15, hjust = 1, face = "bold", color = "black"),
        axis.title.y = element_text(face = "bold", margin = margin(r = 15)),
        panel.grid.major.x = element_blank()
      ) +
      
      coord_cartesian(ylim = c(min(resumen_tratamientos$Media_Variable) - (error_estandar * 3), 
                               max(resumen_tratamientos$Media_Variable) + (error_estandar * 4)))
    
    # Imprimir el gráfico (necesario dentro de un bucle for en R)
    print(grafico)
    
    # Pausa ligera para que R Studio renderice bien los gráficos en el panel
    Sys.sleep(0.5) 
  }
}



# Ejemplo: Evaluar TCH, Peso Neto y Surcos Cosechados todo de una vez
generar_reporte_multivariable(df = datos, 
                              vars_respuesta = c("TCH"), 
                              var_tratamiento = "Tratamientos", 
                              var_bloque = "Replica")
