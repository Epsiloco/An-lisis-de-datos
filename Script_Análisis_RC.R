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
#######################################--Carga y preparación de datos--#############################################
excel_sheets("Velocidades de cosechadoras - Obraje 10114-213.xlsx")
datos<- read_excel("Velocidades de cosechadoras - Obraje 10114-213.xlsx", sheet = "R")


# Copiando y pegando los datos directamente desde Excel

datos <- read_clip_tbl()  

attach(datos)
view(datos)
head(datos)
tail(datos)
names(datos)

# Para cambiar nombres de columnas por si hay algún error

names(datos)[names(datos)=="Rendimiento.Kg.Ton"]<- "Rendimiento"


# ChatGPT
install.packages("chattr")
Sys.setenv(OPENAI_API_KEY = "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")
library(chattr)
chattr::chattr_app() # abrir



# Estandarización del nombre de columnas
estandarizar_nombres_columnas <- function(df, excluir = c("Tratamiento", "Replica")) {
 
  nombres_actuales <- names(df)
  
  limpiar_nombre <- function(nombre) {
    
    if (nombre %in% excluir) {
      return(nombre)
    }
    
    nombre <- tolower(nombre)

    nombre <- gsub("%", "porc", nombre)
    nombre <- gsub("/", "_", nombre)
    nombre <- gsub("\\s+", "_", nombre)  
    nombre <- gsub("[^a-zA-Z0-9_-]", "", nombre)  
    nombre <- gsub("-+", "_", nombre)  
    nombre <- gsub("^-|-$", "", nombre)  
    
    return(nombre)
  }
  
  nombres_nuevos <- sapply(nombres_actuales, limpiar_nombre)
  
  names(df) <- nombres_nuevos
  
  cambios <- data.frame(
    Original = nombres_actuales,
    Estandarizado = nombres_nuevos,
    stringsAsFactors = FALSE
  )
  
  print(cambios)
  return(df)
}

datos <- estandarizar_nombres_columnas(datos)
print(names(datos))
print(str(datos))

attach(datos)
colnames(datos)


# Conversión de variables
  #  "tch", "rendimiento", "tah", "pureza"
  # "brix", "pol", "humedad", "pureza"
  # "diametro", "altura", "poblacion", "peso", "tml", "tallo_ha", "tch"

names(datos)

cols_a_numerica <- intersect(names(datos), c("tch", "rendimiento", "tah", "pureza"))
cols_a_factor <- intersect(names(datos), c("Tratamiento", "Replica"))

datos <- datos %>%
  mutate(across(all_of(cols_a_numerica), ~as.numeric(as.character(.)))) %>%
  mutate(across(all_of(cols_a_factor), as.factor))

sapply(datos, class)


# Relativización de variables de respuesta 

colnames(datos)
levels(datos$Tratamiento)

nombre_testigo <- "Testigo" 
vars_a_relativizar <- c("tch", "rendimiento", "tah", "pureza") 

# "replica" para usar al testigo como base o "general" para calcular segun la media general del testigo
modo_relativo <- "replica" 

# Promedio general por si se elige el modo "general"
promedios_testigo <- datos %>%
  filter(Tratamiento == nombre_testigo) %>%
  summarise(across(all_of(vars_a_relativizar), \(x) mean(x, na.rm = TRUE)))

datos <- datos %>%
  group_by(Replica) %>% 
  mutate(across(all_of(vars_a_relativizar), 
                .fns = list(rel = ~ {
                  if (modo_relativo == "replica") {
                    
                    val_base <- .x[Tratamiento == nombre_testigo]
                    if(length(val_base) == 1) .x / val_base else NA_real_
                    
                  } else {
                    
                    val_gen <- as.numeric(promedios_testigo[[cur_column()]])
                    .x / val_gen
                  }
                }),
                .names = "{.col}_rel")) %>%
  ungroup()

head(datos)
view(datos)

#######################################--Limpieza--#################################################################

create_report(datos)
diagnose(datos)
plot_na_pareto(datos) #grafica de datos faltantes: si no los hay, tira error


#------------------------------------------
# Análisis de datos atípicos
#------------------------------------------

names(datos)
# Errores de digitación

  # Variables:
variables_revisar <- c("altura", "diametro", "poblacion")   


for (var in variables_revisar) {
  if (var %in% names(datos)) {
    print(sort(datos[[var]], na.last = TRUE))
  } else {
    warning(paste("La variable", var, "no existe en 'datos'."))
  }
}

# Identificación con gráficos
colnames(datos)
vars_num <- c("Población", "Tallos.con.Flor", "Porcentaje", "Altura.Total", 
              "Peso.Total", "Altura.sin.Corcho", "Peso.sin.Corcho", 
              "Entrenudos.totales.por.tallo", "Grado.de.corcho") 
var_group <- "No.Trata"                        

# Histogramas
  # Según frecuencia
for (v in vars_num) {
  hist(
    datos[[v]],
    main = paste("Distribución de", v),
    xlab = v,
    col = "lightblue",
    breaks = 20
  )
}

  # Según densidad
for (v in vars_num) {
  
  dens <- density(datos[[v]], na.rm = TRUE)
  
  hist(
    datos[[v]],
    main = paste("Distribución de", v),
    xlab = v,
    col = "lightblue",
    breaks = 20,
    freq = FALSE   
  )
  
  lines(dens, lwd = 2, col = "red")
  
  abline(v = mean(datos[[v]], na.rm = TRUE), col = "darkgreen", lwd = 2, lty = 2)
}

# Boxplots
for (v in vars_num) {
  boxplot(
    datos[[v]] ~ datos[[var_group]],
    main = paste("Boxplot de", v, "por", var_group),
    xlab = var_group,
    ylab = v,
    col = "lightgreen"
  )
}


# Boxplots elegantes y ligeramente vivaces
for (v in vars_num) {
  

  colores <- c("white", "white", "white", "white", "white", "white")
  n_grupos <- length(unique(datos[[var_group]]))
  colores <- colores[1:n_grupos]
  

  boxplot(
    datos[[v]] ~ datos[[var_group]],
    main = paste("Boxplot de", v, "por", var_group),
    xlab = var_group,
    ylab = v,
    col = colores,
    border = "black",
    notch = FALSE,
    las = 1,
    cex.main = 1.8,
    cex.lab = 1.5,
    cex.axis = 1.3,
    outline = TRUE
  )
  
  medias <- tapply(datos[[v]], datos[[var_group]], mean, na.rm = TRUE)
  points(1:length(medias), medias, col = "red", pch = 19, cex = 1.3)
}


# Boxplots con réplicas
vars_num <- c("tch", "rendimiento", "tah", "pureza")
var_group <- "Tratamiento"
var_anidada <- "Replica" 

for (v in vars_num) {
  
  grafico_boxplot_final <- datos %>%
    ggplot(aes(x = as.factor(.data[[var_anidada]]), 
               y = .data[[v]], 
               fill = as.factor(.data[[var_anidada]]))) + 
    
    
    geom_boxplot(
      width = 0.9, 
      outlier.shape = 1, 
      outlier.size = 3,
      
      linewidth = 0.3 
    ) +
    
    
    facet_wrap(~ .data[[var_group]], 
               scales = "free_x", 
               ncol = 4) + 
    
    
    labs(
      title = paste("Boxplot de", v, "por Réplica dentro de cada Tratamiento"),
      x = "Réplica",
      y = v,
      fill = "Réplica"
    ) +
    
    
    theme_bw(base_size = 18) + 
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 22), 
      legend.position = "right",
      axis.title.x = element_blank(), 
      
      strip.text = element_text(size = 16, face = "bold"), 
      
      panel.spacing = unit(0.5, "lines") 
    ) +
    
    scale_fill_brewer(palette = "Dark2") 
  
  print(grafico_boxplot_final)
}

# Captura de valores de boxplot
outliers_list <- list()

for (v in vars_num) {
  bp <- boxplot(
    datos[[v]] ~ datos[[var_group]],
    plot = FALSE
  )
  outliers_list[[v]] <- bp$stats
}

outliers_list

# Datos atípicos por tratamiento
outliers_by_group <- list()

for (v in vars_num) {
  outliers_by_group[[v]] <- tapply(datos[[v]], datos[[var_group]], boxplot.stats)
}

outliers_by_group



# Eliminar datos atípicos
  # Método 1
names(datos)
numericas <- c("COSECHA(BRIX)", "Pre Cosecha(BRIX)")
grupo <- "PERIODOS"   


for (v in numericas) {
  
  out <- boxplot(datos[[v]] ~ datos[[grupo]], plot = FALSE)$out
  
  datos[[v]][datos[[v]] %in% out] <- NA
}

colSums(is.na(datos[numericas]))

plot_na_pareto(datos) #grafica de datos faltantes: si no los hay, tira error



# Cargar librería necesaria para Excel
if (!require("openxlsx")) install.packages("openxlsx")
library(openxlsx)



# Método 2: con el IQR
# Elige: "imputar" (pone NA) o "eliminar" (borra la fila)
accion <- "NA" 

numericas <- c("rendimiento")
reporte_eliminados <- list()

for (v in numericas) {
  if (v %in% names(datos)) {
    
   
    Q1 <- quantile(datos[[v]], 0.25, na.rm = TRUE)
    Q3 <- quantile(datos[[v]], 0.75, na.rm = TRUE)
    IQR_val <- Q3 - Q1
    
    lower <- Q1 - 1.5 * IQR_val
    upper <- Q3 + 1.5 * IQR_val
    
   
    outliers_mask <- (datos[[v]] < lower | datos[[v]] > upper) & !is.na(datos[[v]])
    
  
    indices_filas <- which(outliers_mask)
    valores_outliers <- datos[[v]][outliers_mask]
    
    reporte_eliminados[[v]] <- data.frame(
      Fila_Original = indices_filas,
      Valor_Detectado = valores_outliers
    )
    
    cantidad <- length(indices_filas)
    
    if (accion == "eliminar") {
      datos <- datos[!outliers_mask, ]
      cat("Variable:", v, "| Filas eliminadas:", cantidad, "\n")
    } else {
      datos[[v]][outliers_mask] <- NA
      cat("Variable:", v, "| Valores marcados como NA:", cantidad, "\n")
    }
    
  } else {
    warning(paste("La variable", v, "no existe en el dataset."))
  }
}

print(reporte_eliminados)

# --- Guardado en Excel ---
nombre_archivo <- "datos_limpios_IQR.xlsx"

wb <- createWorkbook()
addWorksheet(wb, "Datos_Limpios")
addWorksheet(wb, "Reporte_Outliers")

writeData(wb, "Datos_Limpios", datos)

df_reporte <- do.call(rbind, reporte_eliminados)
writeData(wb, "Reporte_Outliers", df_reporte)

saveWorkbook(wb, nombre_archivo, overwrite = TRUE)

cat("\nProceso finalizado. Archivo guardado como:", nombre_archivo, "\n")
cat("Dimensiones finales:", dim(datos)[1], "filas y", dim(datos)[2], "columnas\n")


#------------------------------------------
# Datos faltantes
#------------------------------------------

# Imputación con media
colnames(datos)
numericas <- c("tch", "rendimiento_kg_ton", "tah", "pureza")
colSums(is.na(datos[numericas]))

for (v in numericas) {
  datos[[v]][is.na(datos[[v]])] <- mean(datos[[v]], na.rm = TRUE)
}

colSums(is.na(datos[numericas]))
attach(datos)
view(datos)


##############################
##################Otros métodos de imputación más potentes

if (!require("pacman")) install.packages("pacman")
pacman::p_load(mice, missForest, VIM, dplyr, tidyr, ggplot2)

# MICE con PMM (Predictive Mean Matching)
imp_mice_obj <- mice(datos, m=5, method='pmm', seed=123, printFlag=FALSE)
datos_mice <- complete(imp_mice_obj, 1)
view(datos_mice)

# MissForest (Random Forest) - El más potente en Machine Learning
set.seed(123)
imp_forest_obj <- missForest(datos)
datos_forest <- imp_forest_obj$ximp
view(datos_forest)

# KNN Imputation (k-Vecinos más cercanos)
datos_knn <- VIM::kNN(datos, k=5, imp_var = FALSE) 
view(datos_knn)

# CART (Classification and Regression Trees) - Alta Precisión
imp_cart_obj <- mice(datos, m=5, method='cart', seed=123, printFlag=FALSE)
datos_cart <- complete(imp_cart_obj, 1)
view(datos_cart)

# Diagnostico
imp_forest_obj$OOBerror[1] # Error NRMSE
VIM::marginplot(datos[, c("tch", "rendimiento")]) # < 0.3 es imputación confiable

# Comparación de métodos
# Identificación de negativos por método
negativos_resumen <- data.frame(
  PMM    = sum(datos_mice[sapply(datos_mice, is.numeric)] < 0),
  Forest = sum(datos_forest[sapply(datos_forest, is.numeric)] < 0),
  KNN    = sum(datos_knn[sapply(datos_knn, is.numeric)] < 0),
  CART   = sum(datos_cart[sapply(datos_cart, is.numeric)] < 0)
)
View(negativos_resumen)

# Comparativo de Medias 
target_var <- "tch" 
resumen_medias <- data.frame(
  Metodo = c("Original", "PMM", "Forest", "KNN", "CART"),
  Media  = c(mean(datos[[target_var]], na.rm = TRUE),
             mean(datos_mice[[target_var]]),
             mean(datos_forest[[target_var]]),
             mean(datos_knn[[target_var]]),
             mean(datos_cart[[target_var]]))
)
View(resumen_medias)

# Gráfico de Fidelidad de Distribución
df_comp <- data.frame(
  Original = datos[[target_var]],
  PMM = datos_mice[[target_var]],
  Forest = datos_forest[[target_var]],
  KNN = datos_knn[[target_var]],
  CART = datos_cart[[target_var]]
) %>% pivot_longer(cols = everything(), names_to = "Metodo", values_to = "Valor")

ggplot(df_comp, aes(x = Valor, color = Metodo)) +
  geom_density(linewidth = 1) +
  theme_minimal() +
  labs(title = paste("Distribución Imputada vs Original:", target_var),
       x = "Valor", y = "Densidad")

# Matriz de Correlación 
cor_original <- cor(datos$tch, datos$rendimiento, use="complete.obs")
cor_forest   <- cor(datos_forest$tch, datos_forest$rendimiento)
cor_cart     <- cor(datos_cart$tch, datos_cart$rendimiento)
print(c(cor_original, cor_forest, cor_cart))

# Medias por tratamiento
medias_por_tratamiento <- df_comp %>%
  mutate(Tratamiento = rep(datos$Tratamiento, 5)) %>%
  group_by(Metodo, Tratamiento) %>%
  summarise(Media = mean(Valor, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Metodo, values_from = Media)

View(medias_por_tratamiento)

# Gráfico de consistencia por replica (Jitter plot)

ggplot(df_comp %>% mutate(Tratamiento = rep(datos$Tratamiento, 5)), 
       aes(x = Tratamiento, y = Valor, color = Metodo)) +
  geom_jitter(position = position_jitter(0.2), size = 2, alpha = 0.6) +
  facet_wrap(~Metodo) +
  theme_bw() +
  labs(title = paste("Dispersión de Datos por Tratamiento:", target_var),
       subtitle = "Compare si la nube de puntos imputada imita la dispersión original",
       x = "Tratamiento", y = "Valor") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###############################################--Análisis descriptivo--#######################################################
# Filtrar 

library(dplyr)

# Ejecuta esta línea para respaldar por primera vez o para reiniciar 'datos'
colnames(datos)

datos_original <- datos 
datos <- datos_original # Reiniciar


# Filtrar por filas
filtrar_por_tratamiento <- function(factor = "Dosis",
                                    incluir = NULL,
                                    excluir = "Testigo") {
  if (!factor %in% names(datos)) {
    stop(paste("La columna", factor, "no existe en el objeto 'datos'."))
  }
  
  df <- datos
  if (!is.null(incluir)) {
    df <- df %>% dplyr::filter(.data[[factor]] %in% incluir)
  }
  if (!is.null(excluir)) {
    df <- df %>% dplyr::filter(!(.data[[factor]] %in% excluir))
  }
  assign("datos", df, envir = .GlobalEnv)
}

filtrar_por_tratamiento() 

# Agregación por promedios
# Esto colapsa las réplicas y deja solo el promedio por Tratamiento y dda
datos <- datos %>%
  group_by(fertilidad) %>%
  summarise(across(where(is.numeric), ~mean(., na.rm = TRUE)), .groups = "drop")

# Tabla final
colnames(datos)
datos <- datos %>% 
  select(fertilidad, tch, rendimiento_kg_ton, tah, pureza)
view(datos)

##############################################################################################
attach(datos)
# Medidas de resumen
names(datos)

factores <- c("Tratamiento", "volumen")

estadisticas_por_variable <- list(
  rendimiento_kg_ton = "media",
  tch = "media",
  tah = "media",
  pureza = "media"
)

calc_stat <- function(x, stat) {
  if(length(x) == 0 || all(is.na(x))) return(NA)
  
  n <- sum(!is.na(x))
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  
  switch(stat,
         "media"   = m,
         "total"   = sum(x, na.rm = TRUE),
         "sd"      = s,
         "var"     = var(x, na.rm = TRUE),
         "CV"      = if(m != 0) (s/m)*100 else NA,
         "min"     = min(x, na.rm = TRUE),
         "max"     = max(x, na.rm = TRUE),
         "mediana" = median(x, na.rm = TRUE),
         "SE"      = s / sqrt(n),
         "LS"      = m + (s / sqrt(n)),
         "LI"      = m - (s / sqrt(n)),
         stop("Estadística no reconocida: ", stat)
  )
}

variables_existentes <- names(estadisticas_por_variable)[names(estadisticas_por_variable) %in% names(datos)]

if(length(variables_existentes) == 0) {
  stop("Error: Ninguna de las variables especificadas existe en los datos. ",
       "Variables especificadas: ", paste(names(estadisticas_por_variable), collapse = ", "),
       "\nVariables disponibles: ", paste(names(datos), collapse = ", "))
}


nombres_columnas <- character(length(variables_existentes))
for(i in seq_along(variables_existentes)) {
  var <- variables_existentes[i]
  stat <- estadisticas_por_variable[[var]]
  nombres_columnas[i] <- paste0(var, "_", stat)
}

resumen <- datos %>%
  group_by(across(all_of(factores))) %>%
  summarise(
    across(
      all_of(variables_existentes),
      ~ {
        var_name <- cur_column()
        calc_stat(.x, estadisticas_por_variable[[var_name]])
      }
    ),
    .groups = "drop"
  )


names(resumen)[(length(factores) + 1):ncol(resumen)] <- nombres_columnas


print("Resumen estadístico:")
print(resumen)                      # Mostrar resultados


cat("\nVariables procesadas:", paste(variables_existentes, collapse = ", "), "\n")
cat("Factores de agrupación:", paste(factores, collapse = ", "), "\n")


################################################################
# Tablas de doble entrada (contingencia)
names(datos)

fila <- "dosis"
columna <- "Replica" 
variable <- "rendimiento"
estadistica <- "media"  # Opciones: "media", "mediana", "suma", "min", "max", 
# "sd", "cv", "ee", "ls", "li"


tabla <- datos %>%
  group_by(across(all_of(c(fila, columna)))) %>%
  summarise(Valor = case_when(
    estadistica == "media" ~ mean(.data[[variable]], na.rm = TRUE),
    estadistica == "mediana" ~ median(.data[[variable]], na.rm = TRUE),
    estadistica == "suma" ~ sum(.data[[variable]], na.rm = TRUE),
    estadistica == "min" ~ min(.data[[variable]], na.rm = TRUE),
    estadistica == "max" ~ max(.data[[variable]], na.rm = TRUE),
    estadistica == "sd" ~ sd(.data[[variable]], na.rm = TRUE),
    estadistica == "cv" ~ (sd(.data[[variable]], na.rm = TRUE) / mean(.data[[variable]], na.rm = TRUE)) * 100,
    estadistica == "ee" ~ sd(.data[[variable]], na.rm = TRUE) / sqrt(length(na.omit(.data[[variable]]))),
    estadistica == "ls" ~ mean(.data[[variable]], na.rm = TRUE) + (sd(.data[[variable]], na.rm = TRUE) / sqrt(length(na.omit(.data[[variable]])))),
    estadistica == "li" ~ mean(.data[[variable]], na.rm = TRUE) - (sd(.data[[variable]], na.rm = TRUE) / sqrt(length(na.omit(.data[[variable]]))))
  ), .groups = 'drop') %>%
  pivot_wider(
    names_from = all_of(columna), 
    values_from = Valor
  )

print(tabla)
############################################################
# Tabla con IC y otras medidas de resumen
names(datos)

variables_numericas <- c("Población", "Tallos.con.Flor", "Porcentaje", "Altura.Total", 
                         "Peso.Total", "Altura.sin.Corcho", "Peso.sin.Corcho", 
                         "Entrenudos.totales.por.tallo", "Grado.de.corcho")       
factores <- c("Tratamiento")                       
medidas_estadisticas <- c("media", "sd", "max", "min", "cv")


calcular_estadisticas <- function(datos, variable, factor_grupo, medidas) {
  tryCatch({
    if (!variable %in% names(datos)) stop(paste("Variable", variable, "no encontrada"))
    
    datos_temp <- datos
    datos_temp[[variable]] <- as.numeric(datos_temp[[variable]])
    
    resumen <- datos_temp %>%
      group_by(across(all_of(factor_grupo))) %>%
      summarise(
        n = n(),
        media = mean(.data[[variable]], na.rm = TRUE),
        total = sum(.data[[variable]], na.rm = TRUE),
        sd = sd(.data[[variable]], na.rm = TRUE),
        max = max(.data[[variable]], na.rm = TRUE),
        min = min(.data[[variable]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        se = sd / sqrt(n),
        li = media - 1.96 * se,
        ls = media + 1.96 * se,
        cv = ifelse(media != 0, (sd / media) * 100, NA)
      ) %>%
      mutate(across(where(is.numeric), ~ round(.x, 4)))
    
    
    columnas_disponibles <- c(factor_grupo, medidas)
    columnas_disponibles <- columnas_disponibles[columnas_disponibles %in% names(resumen)]
    
    resumen <- resumen %>%
      select(all_of(columnas_disponibles)) %>%
      mutate(Variable = variable) %>%
      select(Variable, everything())
    
    return(resumen)
  }, error = function(e) {
    warning(paste("Error procesando variable", variable, ":", e$message))
    return(NULL)
  })
}


verificar_datos <- function(datos) {
}

verificar_datos(datos)


variables_existentes <- variables_numericas[variables_numericas %in% names(datos)]
factores_existentes <- factores[factores %in% names(datos)]
medidas_disponibles <- c("media", "total", "sd", "se", "li", "ls", "cv", "n", "max", "min")
medidas_solicitadas <- medidas_estadisticas[medidas_estadisticas %in% medidas_disponibles]


if(length(variables_existentes) > 0 && length(factores_existentes) > 0 && length(medidas_solicitadas) > 0) {
  
  cuadros_individuales <- list()
  
  # Ajuste de opciones para la impresión de decimales y ancho de la consola
  options(pillar.sigfig = 4, width = 200) 
  
  for(variable_actual in variables_existentes) {
    cat("Procesando variable:", variable_actual, "...\n")
    resultado <- calcular_estadisticas(datos, variable_actual, factores_existentes, medidas_solicitadas)
    if(!is.null(resultado)) {
      cuadros_individuales[[variable_actual]] <- resultado
      cat("Cuadro generado para", variable_actual, "\n")
      # El data frame 'resultado' ya tiene 4 decimales redondeados
      print(resultado) 
      cat("\n")
    }
  }
  
  # Restaurar las opciones predeterminadas de R (Opcional, pero buena práctica)
  options(pillar.sigfig = 7, width = 80)
  
  if(length(cuadros_individuales) > 0) {
    cuadro_general <- bind_rows(cuadros_individuales)
    assign("cuadro_general_estadisticas", cuadro_general, envir = .GlobalEnv)
    
  }
  
} else {
  cat("Configuración inválida. Verifica que las variables y factores existan y que las medidas solicitadas sean válidas.\n")
}



######################################################
# Gráficos



datos <- read_clip_tbl() 
attach(datos)
names(datos)
# Gráfico de líneas para observar tendencias

factores_interes <- c("Tratamiento") 
variables_analizar <- c("Población", "Tallos.con.Flor", "Porcentaje", "Altura.Total", "Peso.Total",
                        "Altura.sin.Corcho", "Peso.sin.Corcho", "Entrenudos.totales.por.tallo", 
                        "Entrenudos.con.corcho.por.tallo", "Grado.de.corcho") # Las que quieres observar


library(ggplot2)
library(tidyr)
library(dplyr)


df_plot <- datos %>%
  select(all_of(factores_interes), all_of(variables_analizar)) %>%
  pivot_longer(cols = all_of(variables_analizar), 
               names_to = "Variable", 
               values_to = "Valor") %>%
  
  mutate(across(all_of(factores_interes), as.factor))


ggplot(df_plot, aes(x = .data[[factores_interes[1]]], y = Valor, group = 1)) +
  
  stat_summary(fun = mean, geom = "line", color = "steelblue", size = 1) +
  stat_summary(fun = mean, geom = "point", color = "darkblue", size = 2) +
  
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, color = "gray40") +
  
  facet_wrap(~ Variable, scales = "free_y", ncol = 2) + 
  theme_minimal() +
  labs(title = "Análisis de Tendencias por Tratamiento",
       subtitle = paste("Factores analizados:", paste(factores_interes, collapse = ", ")),
       x = "Niveles de Tratamiento",
       y = "Respuesta (Media ± SE)") +
  theme(strip.text = element_text(face = "bold", size = 10),
        panel.spacing = unit(1, "lines"))


# Con titulos más visibles
ggplot(df_plot, aes(x = .data[[factores_interes[1]]], y = Valor, group = 1)) +
  stat_summary(fun = mean, geom = "line", color = "steelblue", size = 1) +
  stat_summary(fun = mean, geom = "point", color = "darkblue", size = 2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, color = "gray40") +
  facet_wrap(~ Variable, scales = "free_y", ncol = 2) + 
  theme_minimal() +
  labs(title = "Análisis de Tendencias por Tratamiento",
       x = "Niveles de Tratamiento",
       y = "Respuesta (Media ± SE)") +
  theme(
    strip.text = element_text(face = "bold", size = 9),
    panel.spacing = unit(1.5, "lines"),

    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),

    plot.margin = margin(t = 10, r = 10, b = 20, l = 10)
  ) +

  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10))












# Gráfico de barras
names(datos)
numericas <- c("Cobertura.Graminea")
factor_group <- "Tratamiento"

resumen <- datos %>%
  group_by(across(all_of(factor_group))) %>%
  summarise(across(all_of(numericas), mean, na.rm = TRUE), .groups = "drop")

for (v in numericas) {
  resumen[[paste0(v, "_label")]] <- sprintf("%.2f", resumen[[v]])
}

for (v in numericas) {
  label_col <- paste0(v, "_label")
  
  p <- ggplot(resumen, aes_string(x = factor_group, y = v)) +
    geom_bar(stat = "identity", fill = "lightblue", color = "black", width = 0.6) +  # solo delineado
    geom_text(aes_string(label = label_col), vjust = -0.5, color = "black") +
    labs(title = paste("Gráfico de barras - medias de", v),
         x = factor_group,
         y = v) +
    theme_minimal(base_size = 14)
  
  print(p)
}


# Gráfico de líneas y puntos
names(datos)
numericas <- c("altura")  
factor_group <- "Tratamiento"                     

resumen <- datos %>%
  group_by(across(all_of(factor_group))) %>%
  summarise(across(all_of(numericas), mean, na.rm = TRUE), .groups = "drop")

for (v in numericas) {
  resumen[[paste0(v, "_label")]] <- sprintf("%.2f", resumen[[v]])
}

for (v in numericas) {
  
  label_col <- paste0(v, "_label")
  
  p <- ggplot(resumen, aes_string(x = factor_group, y = v, group = 1)) +
    
    geom_line(color = "#2C3E50", size = 1.2) +
    
    geom_point(color = "#E74C3C", size = 3) +
    
    geom_text(aes_string(label = label_col), vjust = -1, color = "#34495E", size = 4) +
    
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      axis.title = element_text(face = "bold"),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_blank()
    ) +
    labs(title = paste("Promedio de", v, "por", factor_group),
         x = factor_group,
         y = v)
  
  print(p)
}



##################################################
# Gráficos de barras con factores de partición
names(datos)


config <- list(
  datos = datos,                   
  variable_y = "porc_infestacion", 
  factor_principal = "Tratamiento", 
  factores_adicionales = c("muestreo"),
  orden_niveles = list(            
    muestreo = c("Pre_Muestreo", "1er_Muestreo", "2do_Muestreo")
  ),
  tipo_resumen = "media"           # "media" o "total"
)


graficos_barras_flexibles <- function(config) {
  library(dplyr)
  library(ggplot2)
  
  df <- config$datos
  
  todos_factores <- c(config$factor_principal, config$factores_adicionales)
  for(fac in todos_factores) {
    if(!is.null(config$orden_niveles[[fac]])) {
      df[[fac]] <- factor(df[[fac]], levels = config$orden_niveles[[fac]])
    }
  }
  
  resultados <- list()
  

  if(length(config$factores_adicionales) == 0) config$factores_adicionales <- ""
  
  for(factor_actual in config$factores_adicionales) {
    

    agrupacion <- if(factor_actual != "") c(config$factor_principal, factor_actual) else config$factor_principal
    

    if(config$tipo_resumen == "media") {
      df_resumen <- df %>%
        group_by_at(agrupacion) %>%
        summarise(
          valor = mean(get(config$variable_y), na.rm=TRUE),
          se = sd(get(config$variable_y), na.rm=TRUE)/sqrt(n()),
          .groups="drop"
        )
    } else if(config$tipo_resumen == "total") {
      df_resumen <- df %>%
        group_by_at(agrupacion) %>%
        summarise(
          valor = sum(get(config$variable_y), na.rm=TRUE),
          se = NA, # no tiene sentido error estándar para total
          .groups="drop"
        )
    } else {
      stop("tipo_resumen debe ser 'media' o 'total'")
    }
    
  
    if(factor_actual != "") {
      g <- ggplot(df_resumen, aes_string(x = config$factor_principal,
                                         y = "valor",
                                         fill = factor_actual))
    } else {
      g <- ggplot(df_resumen, aes_string(x = config$factor_principal,
                                         y = "valor"))
    }
    
    g <- g +
      geom_bar(stat="identity", position=position_dodge(0.8), width=0.7, alpha=0.8) +
      # Solo agregar error si se calcula
      {if(config$tipo_resumen == "media") geom_errorbar(aes(ymin = valor - se, ymax = valor + se),
                                                        position=position_dodge(0.8), width=0.2, size=0.5) } +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face="bold", size=14, hjust=0.5),
        axis.title = element_text(face="bold", size=12),
        legend.title = element_text(face="bold")
      ) +
      labs(
        x = config$factor_principal,
        y = if(config$tipo_resumen=="media") paste("Media de", config$variable_y) else paste("Total de", config$variable_y),
        fill = factor_actual,
        title = paste("Gráfico de barras de", config$variable_y)
      )
    
    print(g)
    resultados[[factor_actual]] <- g
  }
  
  return(resultados)
}

graficos <- graficos_barras_flexibles(config) # Gráficar


##################################################################
# Barras de error exploratorias
names(datos)

config <- list(
  datos = datos,                    
  variable_y = "Cobertura.Graminea",  
  factor = "Tratamiento",           
  factor_adicional = "",            
  orden_niveles = list(             
    Tratamiento = c(),
    muestreo = c()
  ),
  tipo_error = "IC",                # "SE", "SD" o "IC"
  mostrar_linea = T,            # TRUE = dibuja línea entre medias, FALSE = solo puntos
  color_punto = "red",             # color de los puntos si no hay factor adicional
  color_error = "black"             # color de las barras de error si no hay factor adicional
)


graficos_puntos_error <- function(config) {
  library(dplyr)
  library(ggplot2)
  
  df <- config$datos
  
 
  for(fac in c(config$factor, config$factor_adicional)) {
    if(!is.null(fac) && fac != "" && !is.null(config$orden_niveles[[fac]])) {
      df[[fac]] <- factor(df[[fac]], levels = config$orden_niveles[[fac]])
    }
  }
  
  
  if(!is.null(config$factor_adicional) && config$factor_adicional != "") {
    agrupacion <- c(config$factor, config$factor_adicional)
    aes_map <- aes_string(
      x = config$factor_adicional,
      y = "media",
      group = config$factor,
      color = config$factor
    )
    color_point <- NULL   # color automático según factor
    color_error <- NULL   # color automático según factor
  } else {
    agrupacion <- config$factor
    aes_map <- aes_string(
      x = config$factor,
      y = "media",
      group = 1
    )
    color_point <- config$color_punto
    color_error <- config$color_error
  }
  

  df_resumen <- df %>%
    group_by_at(agrupacion) %>%
    summarise(
      media = mean(get(config$variable_y), na.rm=TRUE),
      sd_val = sd(get(config$variable_y), na.rm=TRUE),
      n = n(),
      .groups="drop"
    ) %>%
    mutate(
      error = case_when(
        config$tipo_error == "SE" ~ sd_val/sqrt(n),
        config$tipo_error == "SD" ~ sd_val,
        config$tipo_error == "IC" ~ 1.96*sd_val/sqrt(n),
        TRUE ~ NA_real_
      )
    )
  
  # Crear gráfico
  g <- ggplot(df_resumen, aes_map)
  
  # Línea opcional
  if(isTRUE(config$mostrar_linea)) {
    g <- g + geom_line(linewidth = 1, color = "black") # color_point para mismo color de pto
  }
  
  # Puntos
  g <- g + geom_point(size = 4, color = color_point)
  
  # Barras de error
  g <- g + geom_errorbar(aes(ymin = media - error, ymax = media + error),
                         width = 0.2, size = 0.6, color = color_error)
  
  # Tema profesional
  g <- g +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face="bold", size=14, hjust=0.5),
      axis.title = element_text(face="bold", size=12),
      legend.title = element_text(face="bold")
    ) +
    labs(
      x = if(!is.null(config$factor_adicional) && config$factor_adicional != "") config$factor_adicional else config$factor,
      y = paste("Media de", config$variable_y),
      color = if(!is.null(config$factor_adicional) && config$factor_adicional != "") config$factor else NULL,
      title = paste("Gráfico de puntos con error:", config$variable_y)
    )
  
  print(g)
  return(g)
}

grafico <- graficos_puntos_error(config)


#################################################################

#                        Curvas de maduración
################################################################

names(datos)

library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)

# Calcular medias de pol según dda y Tratamiento y redondear a 2 decimales
datos_medias <- datos %>%
  group_by(Tratamiento, dda) %>%
  summarise(pol_media = round(mean(pol, na.rm = TRUE), 2)) %>%
  ungroup()

# Gráfico con estética mejorada
grafico <- ggplot(datos_medias, aes(x = dda, y = pol_media, color = Tratamiento)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  labs(
    x = "Días después de la aplicación (DDA)",
    y = "Pol (%)",
    color = "Tratamiento",
    title = "Curva de maduración por Tratamiento"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_line(linetype = "dashed", color = "grey80"),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12)
  )

# Crear tabla resumen para mostrar debajo del gráfico
tabla_resumen <- datos_medias %>%
  pivot_wider(names_from = Tratamiento, values_from = pol_media)

tabla_grob <- ggpubr::ggtexttable(tabla_resumen, 
                                  rows = NULL, 
                                  theme = ttheme(base_size = 12))

# Combinar gráfico y tabla
ggpubr::ggarrange(grafico, tabla_grob, 
                  ncol = 1, nrow = 2, 
                  heights = c(3, 1))



######################
###### Curva de maduración con otra estética:

colnames(datos)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)

# 1. Preparación de medias (mantenemos tu lógica)
datos_medias <- datos %>%
  group_by(Tratamiento, dda) %>%
  summarise(pol_media = round(mean(average_of_rendimiento_kg_ton, na.rm = TRUE), 2), .groups = "drop")

# 2. Gráfico con aspecto profesional y claro
grafico <- ggplot(datos_medias, aes(x = dda, y = pol_media, color = Tratamiento, group = Tratamiento)) +
  geom_line(linewidth = 1.2) + # Líneas más visibles
  geom_point(size = 2.5, stroke = 1.5) + # Puntos grandes con borde
  scale_color_brewer(palette = "Set1") + # Colores de alto contraste
  labs(
    x = "Días después de la aplicación (DDA)",
    y = "Rendimiento (Kg/ton)",
    color = "Tratamiento",
    title = "Curva de maduración por Tratamiento"
  ) +
  theme_classic(base_size = 14) + # Estética de artículo: fondo blanco y ejes negros
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    legend.position = "right",
    panel.grid.major.y = element_line(color = "grey90", linetype = "dashed") # Solo líneas horizontales
  )

# 3. Tabla resumen (estilo minimalista para no saturar)
tabla_resumen <- datos_medias %>%
  pivot_wider(names_from = Tratamiento, values_from = pol_media)

tabla_grob <- ggpubr::ggtexttable(tabla_resumen, 
                                  rows = NULL, 
                                  theme = ttheme("minimal", base_size = 13))

# 4. Combinación final
ggpubr::ggarrange(grafico, tabla_grob, 
                  ncol = 1, nrow = 2, 
                  heights = c(3, 1))




##################################################################
#                  Gráfico de curvas en general
##################################################################

names(datos)

library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)

# Calcular medias de pol según dda y Tratamiento y redondear a 2 decimales
datos_medias <- datos %>%
  group_by(Tratamiento, numero_de_muestra) %>%
  summarise(pol_media = round(mean(ind_m2, na.rm = TRUE), 2)) %>%
  ungroup()

# Gráfico con estética mejorada
grafico <- ggplot(datos_medias, aes(x = numero_de_muestra, y = pol_media, color = Tratamiento)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  labs(
    x = "Número de muestra",
    y = "Individuos/m2",
    color = "Tratamiento",
    title = "Curva de densidad poblacional"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_line(linetype = "dashed", color = "grey80"),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12)
  )

# Crear tabla resumen para mostrar debajo del gráfico
tabla_resumen <- datos_medias %>%
  pivot_wider(names_from = Tratamiento, values_from = pol_media)

tabla_grob <- ggpubr::ggtexttable(tabla_resumen, 
                                  rows = NULL, 
                                  theme = ttheme(base_size = 12))

# Combinar gráfico y tabla
ggpubr::ggarrange(grafico, tabla_grob, 
                  ncol = 1, nrow = 2, 
                  heights = c(3, 1))




##############################
######### Curva para densidad poblacional de plagas con UDE


library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)

# 1. Preparación de medias
datos_medias <- datos %>%
  group_by(Tratamiento, dda) %>%
  summarise(pol_media = round(mean(intensidad_infestacion, na.rm = TRUE), 2), .groups = "drop")

# 2. Gráfico con Línea de Referencia UDE
grafico <- ggplot(datos_medias, aes(x = dda, y = pol_media, color = Tratamiento, group = Tratamiento)) +
  # Línea de referencia del UDE (2%)
  geom_hline(yintercept = 2, linetype = "dashed", color = "firebrick", linewidth = 1) +
  # Etiqueta para el UDE
  annotate("text", x = max(datos_medias$dda), y = 7.5, label = "UDE: 2%", 
           color = "firebrick", fontface = "bold", hjust = 1) +
  
  geom_line(linewidth = 1.2) + 
  geom_point(size = 2.5, stroke = 1.5) + 
  scale_color_brewer(palette = "Set1") + 
  labs(
    x = "Días después de la aplicación (DDA)",
    y = "% Intensidad de infestación",
    color = "Tratamiento",
    title = "Curva de densidad poblacional de Diatraea por tratamiento",
    subtitle = "La línea roja discontinua indica el Umbral de Daño Económico (UDE) de Diatraea spp."
  ) +
  theme_classic(base_size = 14) + 
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    legend.position = "right",
    panel.grid.major.y = element_line(color = "grey90", linetype = "dotted")
  ) +
  # Asegurar que el eje Y siempre empiece en 0 y llegue al menos al UDE
  expand_limits(y = c(0, 10))

# 3. Tabla resumen
tabla_resumen <- datos_medias %>%
  pivot_wider(names_from = Tratamiento, values_from = pol_media)

tabla_grob <- ggpubr::ggtexttable(tabla_resumen, 
                                  rows = NULL, 
                                  theme = ttheme("minimal", base_size = 13))

# 4. Combinación final
ggpubr::ggarrange(grafico, tabla_grob, 
                  ncol = 1, nrow = 2, 
                  heights = c(3, 1))



###########################
######### Curva para comparar estadíos

eje_x <- "Tratamiento"
prefijos_plaga <- c(GC = "gc", GA = "ga") 
estadios <- c("larva", "adulto")
# -----------------------------

for (i in seq_along(prefijos_plaga)) {
  
  nombre_comun <- names(prefijos_plaga)[i]
  prefijo <- prefijos_plaga[i]
  
  columnas_plaga <- paste(prefijo, estadios, sep = "_")
  
  datos_plot <- datos %>%
    select(all_of(eje_x), all_of(columnas_plaga)) %>%
    pivot_longer(
      cols = starts_with(prefijo),
      names_to = "Estadio",
      values_to = "Conteo"
    ) %>%
    mutate(
      Estadio = factor(gsub(paste0(prefijo, "_"), "", Estadio), levels = estadios)
    )
  
  resumen <- datos_plot %>%
    group_by(.data[[eje_x]], Estadio) %>%
    summarise(
      media = mean(Conteo, na.rm = TRUE),
      se = sd(Conteo, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  gg <- ggplot(resumen, aes(x = .data[[eje_x]], y = media, fill = .data[[eje_x]])) +
    geom_bar(stat = "identity", color = "black", width = 0.7) +
    geom_errorbar(aes(ymin = media, ymax = media + se), width = 0.2) +
    
    # Agregar valor de la media sobre la barra
    geom_text(aes(label = sprintf("%.2f", media), y = media + se), 
              vjust = -0.5, 
              size = 3.5, 
              fontface = "bold") +
    
    facet_wrap(~ Estadio, scales = "free_y") +
    
    scale_fill_viridis_d(option = "mako", begin = 0.2, end = 0.8) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + # Espacio para el texto
    
    labs(
      title = paste("Población de", nombre_comun, "por Tratamiento"),
      subtitle = "Conteo promedio por muestra (Media ± SE)",
      x = eje_x,
      y = "Conteo promedio"
    ) +
    theme_bw() +
    theme(
      legend.position = "none",
      strip.background = element_rect(fill = "gray20"),
      strip.text = element_text(color = "white", face = "bold"),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  print(gg)
}






########################################################
# Gráfico alométrico para ensayos de madurantes

#########################################################
colnames(datos)
library(ggplot2)
library(dplyr)


# Variable para el Eje Principal (Violines)
vars_principal <- c("tch") 

# Variable para el Eje Secundario (Puntos y Línea)
vars_secundario <- c("tah") 

# Variable para el Color de los puntos (Degradado)
var_color_puntos <- "pureza"

# Factores de agrupación
factor_x <- "Tratamiento"   # El que va en el eje X
factor_fill <- "Tratamiento" # El que da color a los violines



for(v_pri in vars_principal){
  for(v_sec in vars_secundario){
    
    cat("\nCreando gráfico alométrico:", v_pri, "vs", v_sec, "\n")
    
    # --- A. Calcular factor de escala dinámico ---
    # Esto asegura que el eje secundario siempre sea proporcional al principal
    ay_val <- max(datos[[v_pri]], na.rm = TRUE) / max(datos[[v_sec]], na.rm = TRUE)
    
    # --- B. Crear resumen para la línea de tendencia ---
    df_resumen <- datos %>%
      group_by(.data[[factor_x]]) %>%
      summarise(mean_sec = mean(.data[[v_sec]], na.rm = TRUE), .groups = "drop")
    
    # --- C. Construcción del Gráfico Profesional ---
    p_alo <- ggplot(datos, aes(x = .data[[factor_x]])) +
      # CAPA 1: TCH (Eje Principal) - Violín y Boxplot
      geom_violin(aes(y = .data[[v_pri]], fill = .data[[factor_fill]]), 
                  trim = FALSE, alpha = 0.6, color = "white") +
      geom_boxplot(aes(y = .data[[v_pri]]), 
                   width = 0.1, color = "black", alpha = 0.3, outlier.shape = NA) +
      
      # CAPA 2: Línea de tendencia (Eje Secundario)
      geom_line(data = df_resumen, aes(y = mean_sec * ay_val, group = 1), 
                color = "firebrick", size = 1, linetype = "solid") +
      
      # CAPA 3: Puntos Individuales (Eje Secundario) con degradado
      geom_point(aes(y = .data[[v_sec]] * ay_val, color = .data[[var_color_puntos]]), 
                 size = 3, alpha = 0.8) +
      
      # ESCALAS Y DOBLE EJE
      scale_y_continuous(
        name = paste(v_pri, "(Eje Principal)"),
        sec.axis = sec_axis(~./ay_val, name = paste(v_sec, "(Eje Secundario)"))
      ) +
      scale_color_gradientn(colors = c("yellow", "orange", "darkgreen")) +
      scale_fill_brewer(palette = "Pastel1") +
      
      # ESTÉTICA PROFESIONAL (MARCO Y TÍTULOS)
      labs(title = paste("Relación Alométrica:", v_pri, "vs", v_sec),
           subtitle = paste("Puntos y línea roja representados en eje secundario (Factor:", round(ay_val, 2), ")"),
           caption = paste("Color de puntos basado en:", var_color_puntos),
           x = "Tratamiento / Descripción",
           fill = "Tratamiento",
           color = paste(var_color_puntos, "(%)")) +
      theme_bw() +
      theme(
        panel.border = element_rect(color = "black", fill = NA, size = 1.3), # Marco profesional
        axis.title.y.right = element_text(color = "firebrick", face = "bold"),
        axis.title.y.left = element_text(face = "bold"),
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        panel.grid.minor = element_blank()
      )
    
    print(p_alo)
  }
}


#####################################
######################################### Gráfico alométrico de variables para NIR, biometria e IDD

# Gráfico de líneas y puntos
colnames(datos)
vars_a_graficar <- c("altura") 
factor_eje_x    <- "Tratamiento"
factor_color    <- "Tratamiento"

# --- Procesamiento y Gráfico ---
datos %>%
  # 1. Seleccionamos columnas únicas para evitar el error de duplicados
  select(all_of(unique(c(factor_eje_x, factor_color, vars_a_graficar)))) %>%
  
  # 2. Agrupamos usando across() y unique() por la misma razón
  group_by(across(all_of(unique(c(factor_eje_x, factor_color))))) %>%
  summarise(across(all_of(vars_a_graficar), ~mean(., na.rm = TRUE)), .groups = "drop") %>%
  
  # 3. Formato largo para que ggplot pueda facetar por 'Variable'
  pivot_longer(cols = all_of(vars_a_graficar), 
               names_to = "Variable", 
               values_to = "Media") %>%
  
  # 4. Asegurar que el eje X sea factor
  mutate(across(all_of(factor_eje_x), as.factor)) %>%
  
  # --- Gráfico ---
  ggplot(aes(x = .data[[factor_eje_x]], 
             y = Media, 
             color = .data[[factor_color]], 
             group = .data[[factor_color]])) +
  
  geom_line(linewidth = 1) +
  geom_point(size = 2.5, shape = 16) + 
  
  # Facetas con escalas libres para apreciar la variación de cada variable
  facet_wrap(~Variable, scales = "free_y") +
  
  # Estética profesional
  scale_color_brewer(palette = "Set1") + 
  theme_classic(base_size = 14) + 
  labs(title = "Análisis Alométrico: Evolución de Medias",
       subtitle = "Escalas ajustadas individualmente por variable",
       x = factor_eje_x,
       y = "Valor Promedio",
       color = factor_color) +
  theme(
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(face = "bold", size = 12),
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.y = element_line(color = "gray95"),
    panel.spacing = unit(1, "lines") # Espacio entre cuadros
  )




########### Gráfico de líneas y puntos para varias variables

colnames(datos)

eje_x <- "Tratamiento"  
vars_a_graficar <- c("tallos_ha", "altura", "diametro", "peso") 

datos_lineas <- tabla_completa %>%
  filter(Variable %in% vars_a_graficar) %>%
  mutate(Variable = factor(Variable, levels = vars_a_graficar))

gg_lineas <- ggplot(datos_lineas, aes(x = .data[[eje_x]], y = emmean, group = 1)) +
  geom_line(aes(color = Variable), size = 1.2) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                width = 0.1, color = "black", alpha = 0.5) +
  geom_point(aes(fill = Variable), size = 2, shape = 21, color = "black", stroke = 1.2) +
  geom_text(aes(label = sprintf("%.2f", emmean)), 
            vjust = -2,           
            size = 4,             
            fontface = "bold",    
            color = "black") +    
  facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
  scale_color_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
  scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
  scale_y_continuous(expand = expansion(mult = c(0.15, 0.35))) +
  labs(title = "Evolución de Variables por Tratamiento",
       subtitle = "Valores promedio por tratamiento",
       x = eje_x, 
       y = "Media Estimada") +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "gray20"),
    strip.text = element_text(color = "white", face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), 
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(face = "bold")
  )

print(gg_lineas)
###############################################--ANDEVA--#########################################################

############### DCA
colnames(datos)

tratamiento <- "Tratamiento"
respuestas <- c("tch", "rendimiento", "tah", "pureza") 
f_principal <- tratamiento 
tabla_completa <- data.frame() 
alfa <- 0.15

for (var in respuestas) {
  cat("\n====================================\n")
  cat("Variable:", var, "\n")
  cat("====================================\n")
  
  formula <- as.formula(paste0("`", var, "` ~ `", tratamiento, "`"))
  modelo <- aov(formula, data = datos)
  
  print(summary(modelo))
  
  cat("\n--- Verificación de supuestos ---\n")
  shapiro <- shapiro.test(residuals(modelo))
  cat("Shapiro-Wilk p-value:", shapiro$p.value, "\n")
  
  specs <- as.formula(paste0("~ `", tratamiento, "`"))
  emm <- emmeans(modelo, specs)
  # alpha ajusta el nivel de significancia para las letras
  cld_res <- cld(emm, Letters = letters, adjust = "tukey", decreasing = TRUE, alpha = alfa)
  
  temp_df <- as.data.frame(cld_res)
  temp_df$Variable <- var
  names(temp_df)[names(temp_df) == "emmean"] <- "emmean"
  tabla_completa <- rbind(tabla_completa, temp_df)
}

orden_vars <- unique(tabla_completa$Variable)
grupos_graficos <- split(orden_vars, ceiling(seq_along(orden_vars) / 4))

for (i in seq_along(grupos_graficos)) {
  
  datos_plot <- tabla_completa %>%
    filter(Variable %in% grupos_graficos[[i]]) %>%
    mutate(Variable = factor(Variable, levels = grupos_graficos[[i]]))
  
  gg <- ggplot(datos_plot, aes(y = reorder(!!sym(f_principal), emmean), x = emmean)) +
    
    geom_errorbarh(aes(xmin = emmean - SE, xmax = emmean + SE), 
                   height = 0.1, color = "black", size = 0.5) +
    
    geom_point(size = 1.5, color = "black") + 
    
    geom_text(aes(label = sprintf("%.2f %s", emmean, trimws(.group))),
              hjust = -0.2,  
              vjust = -1.0,  
              size = 3.5, 
              fontface = "bold") +
    
    facet_wrap(~ Variable, scales = "free_x", ncol = 2) + 
    
    scale_x_continuous(expand = expansion(mult = c(0.1, 0.4))) + 
    
    labs(title = paste("Comparación de Medias (Tukey) - Panel", i),
         subtitle = paste("Letra 'a' indica la media mayor (alpha =", alfa, ")"),
         x = "Media Estimada ± SE", y = "Tratamiento") +
    
    theme_bw() + 
    theme(
      legend.position = "none",
      strip.background = element_rect(fill = "gray90"),
      strip.text = element_text(face = "bold", size = 10),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 10, color = "black"),
      panel.spacing = unit(2, "lines") 
    )
  
  print(gg)
}


############## DBCA

names(datos)


alpha <- 0.15  

tratamiento <- "Tratamiento"
respuestas <- c("tch", "rendimiento", "tah", "pureza")

for (var in respuestas) {
  formula <- as.formula(paste0("`", var, "` ~ factor(", tratamiento, ") + factor(Replica)")) # Replica
  modelo <- aov(formula, data = datos)
  
  cat("\n====================================\n")
  cat("Variable:", var, "\n")
  cat("====================================\n")
  
  print(anova(modelo))   # uso anova() en lugar de summary() para que coincida con el manual
}


# Verificación de supuestos
for (var in respuestas) {
  formula <- as.formula(paste0("`", var, "` ~ factor(", tratamiento, ") + factor(Replica)"))
  modelo <- aov(formula, data = datos)
  
  cat("\n====================================\n")
  cat("Variable:", var, "\n")
  cat("====================================\n")
  
  cat("\n--- Verificación de supuestos ---\n")
  
  # 1. QQ-plot
  qqPlot(modelo$residuals, main=paste("QQ-plot:", var), col="red", lwd=2, pch=16)
  
  # 2. Residuos vs Ajustados
  plot(modelo$fitted.values, modelo$residuals,
       main=paste("Residuos vs Ajustados:", var),
       xlab="Valores Ajustados", ylab="Residuos", pch=16, col="blue")
  abline(h=0, col="red", lwd=2)
  
  # Shapiro-Wilk
  if(length(unique(modelo$residuals)) > 1){
    SW <- shapiro.test(modelo$residuals)
    cat("\nShapiro-Wilk para", var, ":\n")
    print(SW)
  } else {
    cat("\nShapiro-Wilk no se puede calcular: todos los residuos son idénticos.\n")
  }
  
  # Bartlett y Levene con manejo de errores
  if(length(unique(datos[[var]])) > 1){
    
    # Verificar que todos los grupos tengan al menos 2 observaciones para Bartlett
    conteo_por_grupo <- table(datos[[tratamiento]])
    grupos_validos <- all(conteo_por_grupo >= 2)
  
    if(grupos_validos){
      cat("\nPrueba de Bartlett para", var, ":\n")
      bartlett_result <- tryCatch({
        bartlett.test(as.formula(paste0("`", var, "` ~ factor(", tratamiento, ")")), data=datos)
      }, error = function(e) {
        return(paste("Error en Bartlett:", e$message))
      })
      print(bartlett_result)
    } else {
      cat("\nPrueba de Bartlett no se puede calcular: algunos tratamientos tienen menos de 2 observaciones.\n")
    }
    
    # Levene es más robusto, lo intentamos igual
    cat("\nPrueba de Levene para", var, ":\n")
    levene_result <- tryCatch({
      leveneTest(as.formula(paste0("`", var, "` ~ factor(", tratamiento, ")")), 
                 data=datos, center="median")
    }, error = function(e) {
      return(paste("Error en Levene:", e$message))
    })
    print(levene_result)
    
  } else {
    cat("Bartlett y Levene no se pueden calcular: variable constante.\n")
  }
  
  cat("\n------------------------------------\n")
}


# Prueba Múltiple de Medias (PMM):

for (var in respuestas) {
  
  # Crear un nombre seguro para la variable
  nombre_var <- paste0("var_", gsub("[^[:alnum:]_]", "_", var))
  
  # Ignorar nombres inválidos
  if(grepl("^var_[_]*$", nombre_var)){
    cat("\nVariable", var, "omitida: nombre no válido.\n")
    next
  }
  
  # Crear dataframe temporal y renombrar variable
  datos_temp <- datos
  names(datos_temp)[names(datos_temp) == var] <- nombre_var
  
  # Asegurar que Tratamiento y Replica sean factores
  datos_temp$Tratamiento <- factor(datos_temp$Tratamiento) # cambiar a Tratamiento
  datos_temp$Replica <- factor(datos_temp$Replica)
  
  # Modelo ANDEVA
  formula <- as.formula(paste0("`", nombre_var, "` ~ Tratamiento + Replica"))
  modelo <- aov(formula, data = datos_temp)
  
  cat("\n====================================\n")
  cat("Variable:", var, "\n")
  cat("====================================\n")
  
  # Verificar que hay más de un valor y más de un nivel con datos
  niveles_con_datos <- sum(tapply(datos_temp[[nombre_var]], datos_temp$Tratamiento, #cambiar a Tratamiento
                                  function(x) length(unique(x)) > 0))
  
  if(length(unique(datos_temp[[nombre_var]])) > 1 & niveles_con_datos > 1){
    
    # -------------------------
    # Tukey HSD
    # -------------------------
    cat("\n--- Tukey HSD (alpha =", alpha, ") ---\n")
    print(HSD.test(modelo, trt = "Tratamiento", alpha = alpha, console = TRUE))
    
    # -------------------------
    # TukeyHSD (stats) - BLOQUE AGREGADO
    # -------------------------
    cat("\n--- TukeyHSD (función base R) ---\n")
    tukey_base <- TukeyHSD(modelo, which = "Tratamiento", conf.level = 1 - alpha)
    print(tukey_base)
    
    # -------------------------
    # Scott-Knott
    # -------------------------
    cat("\n--- Scott-Knott (alpha =", alpha, ") ---\n")
    sk <- SK(modelo, which = "Tratamiento", dispersion = "se", sig.level = alpha)
    print(summary(sk))
    
  } else {
    cat("\nPMM no se puede calcular: variable constante o menos de 2 niveles de tratamiento con datos.\n")
  }
  
  cat("\n------------------------------------\n")
}


# Barras de error:

alpha <- 0.15   


for (var in respuestas) {
  
  datos_temp <- datos
  names(datos_temp)[names(datos_temp) == var] <- "valor"
  
  datos_temp$Tratamiento <- factor(datos_temp$Tratamiento)
  datos_temp$Replica <- factor(datos_temp$Replica)
  
  modelo <- aov(valor ~ Tratamiento + Replica, data = datos_temp)
  
  
  tukey <- HSD.test(modelo, "Tratamiento", group = TRUE, alpha = alpha)
  
  letras <- tukey$groups %>%
    mutate(Tratamiento = rownames(tukey$groups)) %>%
    select(Tratamiento, letra = groups)
  
  
  resumen <- datos_temp %>%
    group_by(Tratamiento) %>%
    summarise(
      media = mean(valor, na.rm = TRUE),
      sd = sd(valor, na.rm = TRUE),
      n = n(),
      se = sd / sqrt(n),
      
      t_value = qt(1 - alpha/2, df = n - 1),
      
      IC_inf = media - t_value * se,
      IC_sup = media + t_value * se,
      .groups = "drop"
    ) %>%
    left_join(letras, by = "Tratamiento") %>%
    arrange(media) %>%
    mutate(Trat_ordenado = factor(Tratamiento, levels = Tratamiento))
  
  colores <- "red"
  
  print(
    ggplot(resumen, aes(x = Trat_ordenado, y = media)) +
      geom_point(size = 3, color = colores) +
      geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup), width = 0.15) +
      geom_text(aes(label = letra, y = IC_sup + 0.05 * max(media)),
                size = 7, vjust = 0) +
      labs(title = paste("Medias ± IC (1 - α) con letras Tukey -", var),
           subtitle = paste("α =", alpha),
           y = var, x = "Tratamiento") +
      theme_minimal(base_size = 17) +
      theme(plot.title = element_text(hjust = 0.5))
  )
}



# Gráfico de lineas de error elegante:

# Asegúrate de que los paquetes necesarios estén cargados
library(dplyr)
library(ggplot2)
# library(agricolae) # Recuerda cargar este paquete para HSD.test

alpha <- 0.15  

# Definición de colores
COLOR_PUNTO <- "red"
COLOR_TEXTO <- "black"

# MAPEO ACTUALIZADO DE VARIABLES para títulos descriptivos
titulos_y <- c(
  "tch" = "TCH (toneladas/ha)", 
  "rendimiento" = "Rendimiento (kg/tonelada)", 
  "tah" = "TAH (toneladas/ha)", # Asumiendo Toneladas de Azúcar por Hectárea
  "pureza" = "Pureza (%)"
)


for (var in respuestas) {
  
  datos_temp <- datos
  names(datos_temp)[names(datos_temp) == var] <- "valor"
  
  datos_temp$Tratamiento <- factor(datos_temp$Tratamiento)
  datos_temp$Replica <- factor(datos_temp$Replica)
  
  # --- CÁLCULOS ESTADÍSTICOS ---
  modelo <- aov(valor ~ Tratamiento + Replica, data = datos_temp)
  
  # PRUEBA DE TUKEY
  tukey <- HSD.test(modelo, "Tratamiento", group = TRUE, alpha = alpha)
  
  letras <- tukey$groups %>%
    mutate(Tratamiento = rownames(tukey$groups)) %>%
    select(Tratamiento, letra = groups) %>%
    # CONVERTIMOS LETRAS A MAYÚSCULAS
    mutate(letra = toupper(letra)) 
  
  resumen <- datos_temp %>%
    group_by(Tratamiento) %>%
    summarise(
      media = mean(valor, na.rm = TRUE),
      sd = sd(valor, na.rm = TRUE),
      n = n(),
      se = sd / sqrt(n),
      t_value = qt(1 - alpha/2, df = n - 1),
      IC_inf = media - t_value * se,
      IC_sup = media + t_value * se,
      .groups = "drop"
    ) %>%
    left_join(letras, by = "Tratamiento") %>%
    
    # ORDENACIÓN CLAVE: DE MAYOR A MENOR MEDIA (Descendente)
    arrange(desc(media)) %>% 
    
    # Establecer el orden de los niveles del factor para el eje X
    mutate(Trat_ordenado = factor(Tratamiento, levels = Tratamiento))
  
  # Determinamos el título del eje Y
  y_titulo <- ifelse(var %in% names(titulos_y), titulos_y[var], var)
  
  # ------------------------------------------------------------------
  # GENERACIÓN DEL GRÁFICO CON ESTÉTICA MEJORADA
  # ------------------------------------------------------------------
  
  print(
    ggplot(resumen, aes(x = Trat_ordenado, y = media)) +
      
      # 1. Barras de error (Intervalo de Confianza)
      geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup), 
                    width = 0.15, 
                    linewidth = 1, 
                    color = "gray30") + 
      
      # 2. Puntos (Medias) - Rojo
      geom_point(size = 3, color = COLOR_PUNTO) + 
      
      # 3. Letras de Tukey (MAYÚSCULAS y Negras)
      geom_text(aes(label = letra, 
                    y = IC_sup + 0.05 * (max(resumen$media) - min(resumen$media))), 
                size = 6,              
                color = COLOR_TEXTO,   
                fontface = "bold",     
                vjust = 0) +
      
      # 4. Leyendas y Título
      labs(title = paste("Medias de", y_titulo, "± IC (", round(100 * (1 - alpha), 0), "%)"),
           subtitle = paste("Prueba de Tukey (α =", alpha, "). Ordenado por media descendente."),
           y = y_titulo, 
           x = "Tratamiento") +
      
      # 5. Tema Profesional (Blanco y Negro)
      theme_bw(base_size = 17) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", color = COLOR_TEXTO, size = 18),
        plot.subtitle = element_text(hjust = 0.5, color = "gray50"),
        
        axis.title = element_text(face = "bold", color = COLOR_TEXTO, size = 16),
        # Gira etiquetas X para mejor lectura si los nombres son largos
        axis.text.x = element_text(color = COLOR_TEXTO, size = 14, angle = 45, hjust = 1), 
        axis.text.y = element_text(color = COLOR_TEXTO, size = 14), 
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = COLOR_TEXTO, fill = NA, linewidth = 1)
      )
  )
}



# Gráfico de barras de error según la prueba de Fisher LSD: PARA BLOQUES AL AZAR

library(dplyr)
library(ggplot2)
library(agricolae)

alpha <- 0.15  
COLOR_PUNTO <- "red"
COLOR_TEXTO <- "black"

for (var in respuestas) {
  
  datos_temp <- datos
  names(datos_temp)[names(datos_temp) == var] <- "valor"
  datos_temp$Tratamiento <- factor(datos_temp$Tratamiento)
  datos_temp$Replica <- factor(datos_temp$Replica)
  

  modelo <- aov(valor ~ Tratamiento + Replica, data = datos_temp)
  

  lsd_test <- LSD.test(modelo, "Tratamiento", group = TRUE, alpha = alpha)
  
  letras <- lsd_test$groups %>%
    mutate(Tratamiento = rownames(lsd_test$groups)) %>%
    select(Tratamiento, letra = groups) %>%
    mutate(letra = toupper(letra)) 
  

  mse <- anova(modelo)["Residuals", "Mean Sq"]
  n_reps <- length(unique(datos_temp$Replica))
  
 
  se_modelo <- sqrt(mse / n_reps)
  

  t_critico <- qt(1 - alpha/2, df = modelo$df.residual)
  
  resumen <- datos_temp %>%
    group_by(Tratamiento) %>%
    summarise(
      media = mean(valor, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(letras, by = "Tratamiento") %>%
    mutate(
      se = se_modelo,
      IC_inf = media - t_critico * se,
      IC_sup = media + t_critico * se
    ) %>%
    arrange(desc(media)) %>% 
    mutate(Trat_ordenado = factor(Tratamiento, levels = Tratamiento))
  
  # Gráfico
  print(
    ggplot(resumen, aes(x = Trat_ordenado, y = media)) +
      geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup), 
                    width = 0.1, linewidth = 0.8, color = "gray40") + 
      geom_point(size = 4, color = COLOR_PUNTO) + 
      geom_text(aes(label = letra, 
                    y = IC_sup + (max(media)*0.02)), 
                size = 5, fontface = "bold", color = COLOR_TEXTO, vjust = 0) +
      labs(title = paste("Medias de", var, "± IC (", round(100*(1-alpha)), "%)"),
           subtitle = paste("Prueba LSD de Fisher (α =", alpha, ") | ANOVA p =", 
                            round(summary(modelo)[[1]][["Pr(>F)"]][1], 3)),
           y = var, x = "Tratamiento") +
      theme_bw(base_size = 15) +
      theme(panel.grid.major = element_blank(), 
            axis.text.x = element_text(angle = 45, hjust = 1))
  )
}




# Gráfico de lineas de error segun la prueba de Scott Knott para DBCA (colocar las letras manual)

library(dplyr)
library(ggplot2)
library(ScottKnott)

alpha <- 0.15  
COLOR_PUNTO <- "red"

for (var in respuestas) {
  
  
  datos_temp <- datos
  names(datos_temp)[names(datos_temp) == var] <- "valor"
  
  datos_temp$Tratamiento <- factor(datos_temp$Tratamiento)
  datos_temp$Replica <- factor(datos_temp$Replica)
  
  
  modelo <- aov(valor ~ Tratamiento + Replica, data = datos_temp)
  
  
  sk_res <- SK(modelo, which = "Tratamiento", sig.level = alpha)
  
  
  medias <- datos_temp %>%
    group_by(Tratamiento) %>%
    summarise(media = mean(valor, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(media))
  
  
  grupos_sk <- sk_res$groups
  
  
  tratamientos_ordenados <- medias$Tratamiento
  
  
  letras_asignadas <- character(length(tratamientos_ordenados))
  
  
  for (i in seq_along(grupos_sk)) {
    tratamientos_en_grupo <- names(grupos_sk[[i]])
    
    for (trat in tratamientos_en_grupo) {
      idx <- which(tratamientos_ordenados == trat)
      if (length(idx) > 0) {
        letras_asignadas[idx] <- LETTERS[i]}}}
  
  mse <- anova(modelo)["Residuals", "Mean Sq"]
  n_reps <- length(unique(datos_temp$Replica))
  se_modelo <- sqrt(mse / n_reps)
  t_critico <- qt(1 - alpha/2, df = modelo$df.residual)
  
  
  resumen_grafico <- datos_temp %>%
    group_by(Tratamiento) %>%
    summarise(media = mean(valor, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      IC_inf = media - (t_critico * se_modelo),
      IC_sup = media + (t_critico * se_modelo)
    ) %>%
    arrange(desc(media)) %>% 
    mutate(Trat_ordenado = factor(Tratamiento, levels = Tratamiento)) %>%
    
    left_join(
      data.frame(
        Tratamiento = tratamientos_ordenados,
        letra_sk = letras_asignadas
      ),
      by = "Tratamiento"
    )
  
  print(
    ggplot(resumen_grafico, aes(x = Trat_ordenado, y = media)) +
      geom_line(aes(group = 1), color = "gray90", linetype = "dashed") +
      geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup), 
                    width = 0.1, color = "gray30", linewidth = 0.8) + 
      geom_point(size = 4, color = COLOR_PUNTO) + 
      
  
      geom_text(aes(label = letra_sk, y = IC_sup), 
                size = 6, 
                fontface = "bold", 
                vjust = -0.6,  
                color = "darkblue") +
      
      
      geom_text(aes(label = letra_sk, y = media),
                size = 5, fontface = "bold",
                vjust = -1.5, color = "darkred") +
      
      labs(title = paste("Variable:", var),
           subtitle = paste("Grupos Scott-Knott (α =", alpha, ")"),
           y = var, x = "Tratamiento") +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) + # Ajustado
      theme_classic(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 35, hjust = 1, color = "black", face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)
      )
  )
}


### Obtención de resultados en tablas:

library(knitr); library(kableExtra); library(agricolae); library(dplyr)

for (var in respuestas) {

  datos_temp <- datos %>% mutate(valor = .[[var]], Trat = factor(Tratamiento), Rep = factor(Replica))
  mod <- aov(valor ~ Trat + Rep, data = datos_temp)
  
  formato_tabla <- function(df, titulo) {
    print(kable(df, digits = 4, format = "html", row.names = FALSE, caption = paste("<b>", titulo, "</b>")) %>%
            kable_styling(bootstrap_options = c("striped", "bordered", "condensed"), full_width = F) %>%
            row_spec(0, bold = TRUE, color = "white", background = "#666666")) # Gris estándar
  }
  
  df_anova <- as.data.frame(anova(mod))
  df_anova <- cbind(Fuente_Variacion = rownames(df_anova), df_anova)
  formato_tabla(df_anova, paste("ANDEVA:", var))
  
  tk <- HSD.test(mod, "Trat", group = TRUE, alpha = alpha)$groups
  df_tk <- data.frame(Tratamiento = rownames(tk), Media = tk[,1], Letras = tk[,2])
  formato_tabla(df_tk, paste("Tukey (alpha=", alpha, "):", var))
  
  lsd <- LSD.test(mod, "Trat", group = TRUE, alpha = alpha)$groups
  df_lsd <- data.frame(Tratamiento = rownames(lsd), Media = lsd[,1], Letras = lsd[,2])
  formato_tabla(df_lsd, paste("Fisher LSD (alpha=", alpha, "):", var))
  
  cat("<br><hr style='border: 1px solid #ddd;'><br>")
}


###### Contrastes ortogonales

names(datos) <- trimws(names(datos))
names(datos)

mis_variables  <- trimws(c("tch", "tah")) # Limpiamos espacios aquí también
mi_factor      <- "Tratamiento"
alfa           <- 0.15 

# "none" para FISHER LSD | "tukey" para TUKEY
metodo_ajuste  <- "none" 

# 3. Definición de contrastes (asegúrate que el orden coincida con levels(datos$Tratamiento))
levels(datos$Tratamiento)
mis_contrastes <- list(
  "Todos_vs_Madurez Natural"  = c(1,1,1,1,-5), 
  "Etephon48_vs_Etephon+Ferteco" = c(0,-1,1,0,0),
  "Etephon Manvert_vs_Etephon 48" = c(-1,1,0,0,0),
  "Etephon Manvert_vs_Etephon sulf" = c(-1,0,0,1,0)
)

# =========================================================
# BUCLE DE ANÁLISIS
# =========================================================

res_finales <- lapply(mis_variables, function(v) {
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("ANÁLISIS PARA:", toupper(v), "\n")
  cat("MÉTODO AJUSTE:", ifelse(metodo_ajuste == "none", "FISHER LSD", "TUKEY"), "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Verificación de existencia
  if(!v %in% names(datos)) {
    cat("ERROR: La variable '", v, "' no existe en el dataset. Saltando...\n")
    return(NULL)
  }
  
  # CONSTRUCCIÓN SEGURA DE LA FÓRMULA (Evita el error de objeto ' tah ' no encontrado)
  # Si usas bloques, añade "Replica" dentro del c()
  formula_v <- reformulate(termlabels = mi_factor, response = v)
  
  modelo <- lm(formula_v, data = datos)
  emm <- emmeans(modelo, as.formula(paste("~", mi_factor)))
  
  # --- [1] PRUEBA DE CONTRASTES ORTOGONALES ---
  cat("\n[1] PRUEBA DE CONTRASTES:\n")
  # Aplicamos el ajuste seleccionado también a los contrastes
  tab_con <- as.data.frame(test(contrast(emm, method = mis_contrastes), adjust = metodo_ajuste))
  print(tab_con[, c("contrast", "estimate", "t.ratio", "p.value")])
  
  # --- [2] CLASIFICACIÓN DE MEDIAS (Letras) ---
  nombre_prueba <- ifelse(metodo_ajuste == "none", "Fisher LSD", "Tukey")
  cat("\n[2] CLASIFICACIÓN DE MEDIAS (", nombre_prueba, "):\n")
  
  letras <- cld(emm, alpha = alfa, Letters = letters, reversed = TRUE, adjust = metodo_ajuste)
  print(letras)
  
  # --- [3] INTERPRETACIÓN RÁPIDA ---
  cat("\n[3] INTERPRETACIÓN RÁPIDA:\n")
  ganador <- letras[1, mi_factor] 
  cat("El tratamiento con mejor desempeño promedio es:", as.character(ganador), "\n")
  
  return(list(contrastes = tab_con, letras = letras))
})

names(res_finales) <- mis_variables



###### Contrastes no ortogonales y prueba de Dunett

library(emmeans)
library(multcomp)
library(multcompView)

names(datos) <- trimws(names(datos))
mis_variables  <- trimws(c("tch","tah", "rendimiento", "pureza"))
mi_factor      <- "Tratamiento"
nombre_testigo <- "Madurez Natural" 
alfa           <- 0.15 
metodo_ajuste  <- "tukey" # "none" para FISHER, "tukey" para TUKEY

levels(datos$Tratamiento)
mis_comparaciones_man <- list(
  "todos_vs_MN"  = c("Glifosato Potasico 36.3 SL (100%)", "Glifosato Potasico 36.3 SL + Nanopro 0.30 L/Ha (100%)",
                     "Glifosato Potasico 36.3 SL + Nanopro 0.30 L/Ha (50%)", 
                     "Glifosato Potasico 36.3 SL + Nanopro 0.30 L/Ha (75%)", "Madurez Natural"),
  "GP100_vs_GPB100"       = c("Glifosato Potasico 36.3 SL (100%)", "Glifosato Potasico 36.3 SL + Nanopro 0.30 L/Ha (100%)"),
  "GPB100_vs_GPB75"      = c("Glifosato Potasico 36.3 SL + Nanopro 0.30 L/Ha (100%)", "Glifosato Potasico 36.3 SL + Nanopro 0.30 L/Ha (75%)"),
  "GPB75_vs_GPB50" = c("Glifosato Potasico 36.3 SL + Nanopro 0.30 L/Ha (75%)", "Glifosato Potasico 36.3 SL + Nanopro 0.30 L/Ha (100%)")
)

# --- BUCLE DE ANÁLISIS ---
analisis_auto <- lapply(mis_variables, function(v) {
  
  cat("\n", paste(rep("█", 40), collapse = ""), "\n")
  cat(" ANALIZANDO VARIABLE:", toupper(v), "\n")
  
  formula_v <- reformulate(termlabels = mi_factor, response = v)
  modelo <- lm(formula_v, data = datos)
  emm <- emmeans(modelo, as.formula(paste("~", mi_factor)))
  
  # 1. DUNNETT
  cat("\n[1] DUNNETT (vs", nombre_testigo, "):\n")
  pos_ref <- which(levels(datos[[mi_factor]]) == nombre_testigo)
  if(length(pos_ref) > 0) {
    print(summary(contrast(emm, method = "dunnett", ref = pos_ref), infer = TRUE))
  }
  
  # 2. CONTRASTES MANUALES
  cat("\n[2] CONTRASTES MANUALES (Ajuste:", metodo_ajuste, "):\n")
  generar_vector <- function(nivel_pos, nivel_neg, todos_niveles) {
    vec <- rep(0, length(todos_niveles))
    vec[which(todos_niveles == nivel_pos)] <- 1
    vec[which(todos_niveles == nivel_neg)] <- -1
    return(vec)
  }
  
  niveles_reales <- levels(datos[[mi_factor]])
  lista_vectores <- lapply(mis_comparaciones_man, function(x) {
    generar_vector(x[1], x[2], niveles_reales)
  })
  
  tab_man <- test(contrast(emm, method = lista_vectores), adjust = metodo_ajuste)
  print(tab_man)
  
  # 3. LETRAS (CLD)
  cat("\n[3] CLASIFICACIÓN DE MEDIAS:\n")
  res_letras <- cld(emm, alpha = alfa, Letters = letters, reversed = TRUE, adjust = metodo_ajuste)
  print(res_letras)
  
  return(list(dunnett = tab_man, letras = res_letras))
})


################ DBCA con muestreo ##################

pacman::p_load(readxl, lme4, lmerTest, MASS, DHARMa, car, multcomp, hnp, dplyr, tidyr,
               ggplot2, extrafont, gridExtra, ggiraphExtra, tinytex, reshape2, ggpubr, 
               emmeans, lsmeans, multcompView, effects, vegan, tidyverse, knitr, 
               kableExtra, ggrepel, patchwork)

vars_analisis <- c("tch") 
f_principal   <- "Tratamiento"
f_bloque      <- "Replica"
alpha_val     <- 0.15
datos         <- datos 

for(v in vars_analisis) {
  
  cat("\n==============================================================\n")
  cat("ANÁLISIS PARA:", v, "\n")
  cat("==============================================================\n")
  
  # --- Verificar significancia del Error de Muestreo ---
  f_comp <- as.formula(paste0("`", v, "` ~ ", f_principal, " * ", f_bloque))
  moDBA <- aov(f_comp, data = datos)
  
  cat("\n[1] Verificación del Error de Muestreo:\n")
  res_tab <- as.data.frame(summary(moDBA)[[1]])
  print(res_tab)
  
  # Extraer p-valor de la interacción de forma segura
  row_interacc <- grep(":", rownames(res_tab))
  p_ee <- res_tab[row_interacc, ncol(res_tab)] 
  
  # --- PASO 2: Selección de Modelo ---
  if (!is.na(p_ee) && p_ee > 0.05) {
    cat("\n[INFO] EM NO significativo (p =", round(p_ee, 4), "). Usando moDBA1.\n")
    f_final <- as.formula(paste0("`", v, "` ~ ", f_principal, " + ", f_bloque))
    modelo_final <- aov(f_final, data = datos)
    print(summary(modelo_final))
  } else {
    cat("\n[INFO] EM significativo (p =", round(p_ee, 4), "). Usando moDBA2.\n")
    f_final <- as.formula(paste0("`", v, "` ~ ", f_principal, " + ", f_bloque, " + Error(", f_bloque, "/", f_principal, ")"))
    modelo_final <- aov(f_final, data = datos)
    print(summary(modelo_final))
  }
  
  # --- PASO 3: Obtención de letras (Post-hoc) ---
  res_means <- emmeans(modelo_final, as.formula(paste("~", f_principal))) %>%
    multcomp::cld(Letters = letters, sort = TRUE, reverse = TRUE, alpha = alpha_val) %>%
    as.data.frame()
  
  # --- PASO 4: Cálculo de Medias Reales ---
  medias_reales <- datos %>%
    group_by(across(all_of(f_principal))) %>%
    summarise(media_r = mean(!!sym(v), na.rm = TRUE),
              se_r = sd(!!sym(v), na.rm = TRUE) / sqrt(n()), .groups = "drop")
  
  # --- PASO 5: Unión de datos ---
  datos_plot <- res_means %>%
    select(!!sym(f_principal), .group) %>%
    left_join(medias_reales, by = f_principal) %>%
    mutate(Variable = v)
  
  # --- PASO 6: Gráfico con Apariencia Idéntica ---
  gg <- ggplot(datos_plot, aes(y = reorder(!!sym(f_principal), media_r), x = media_r)) +
    geom_errorbarh(aes(xmin = media_r - se_r, xmax = media_r + se_r), height = 0.2) +
    geom_point(size = 3, color = "black") +
    geom_text(aes(label = sprintf("%.2f %s", media_r, trimws(.group))),
              hjust = -0.1, vjust = -1, size = 4.5, family = "Arial") +
    
    facet_wrap(~ Variable, scales = "free_x") + 
    scale_x_continuous(expand = expansion(mult = c(0.1, 0.5))) +
    
    labs(x = "Media Aritmética Real ± SE", 
         y = "Tratamiento",
         title = paste("Comparación de Medias Reales -", v)) +
    
    theme_minimal(base_family = "Arial") +
    theme(strip.text = element_text(size = 13, face = "bold"),
          text = element_text(size = 12)) +
    theme_classic2()
  
  print(gg)
}





colnames(datos)

moDBA <- aov(tch~Tratamiento+Replica+Tratamiento*Replica) #Verificar si el em es significativo
summary(moDBA)


moDBA1 <- aov(vnm1~trat+blo) #En este caso no es significativo el error de muestreo y se utiliza el modelo:
summary(moDBA1)

moDBA2 <- aov(tch~Tratamiento+Replica+Error(Replica/Tratamiento)) #si el em es significativo
summary(moDBA2)



# Modelo: Respuesta ~ Efectos Fijos + Error(Parcela / Punto)
mo_anidado2 <- aov(pol ~ Tratamiento + Replica + Error(Replica:Tratamiento/viaje), data = datos)

summary(mo_anidado2)






library(lme4)
library(lmerTest)

# El modelo correcto para doble anidamiento:
# (1|Bloque:Trim) es el error de la Unidad Experimental (Parcela)
# (1|Bloque:Trim:Punto) es el error del Punto de Muestreo
modelo_anidado <- lmer(tch ~ Tratamiento + Replica + 
                         (1|Replica:Tratamiento) + 
                         (1|Replica:Tratamiento:viaje), 
                       data = datos)

# Ver el ANDEVA para el Tratamiento (Trim)
anova(modelo_anidado)


######################### Polinomios ortogonales (DBCA, DCA) #########################

respuestas <- c("tch", "rendimiento", "tah") 
factor_dosis <- "Tratamiento"   
replica_name <- "Replica"  

alfa <- 0.95


for (var_y in respuestas) {
  
  Y   <- as.numeric(datos[[var_y]])
  
  TR1 <- as.numeric(as.character(datos[[factor_dosis]])) 
  TR  <- as.factor(datos[[factor_dosis]])
  
  t <- length(unique(TR))
  grado_max <- t - 1
  n <- length(Y)
  
  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  cat(" ANÁLISIS DE POLINOMIOS ORTOGONALES PARA:", toupper(var_y), "\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  
  
  if (!is.null(replica_name)) {
    REP <- as.factor(datos[[replica_name]])
    mod_base <- lm(Y ~ REP + TR)
  } else {
    mod_base <- lm(Y ~ TR)
  }
  
  sum_mod <- summary(mod_base)
  cv <- (sqrt(deviance(mod_base)/df.residual(mod_base)) / mean(Y)) * 100
  
  cat("\nAnálisis de la varianza\n")
  cat("\nVariable    N      R²      R² Aj     CV")
  cat(sprintf("\n%-10s %-5d  %.3f   %.3f     %.2f\n", 
              var_y, n, sum_mod$r.squared, sum_mod$adj.r.squared, cv))
  
  
  cat("\nCuadro de Análisis de la Varianza (SC tipo III)\n")
  contrasts(TR) <- contr.poly(t)
  mod_aov <- if(!is.null(replica_name)) aov(Y ~ REP + TR) else aov(Y ~ TR)
  
  nombres_grados <- c("lineal", "cuadratico", "cubico", "cuartico", "quintico", "sextico", "septico")
  lista_split <- setNames(as.list(1:grado_max), nombres_grados[1:grado_max])
  print(summary(mod_aov, split = list(TR = lista_split)))
  
  
  cat("\n", paste(rep("-", 45), collapse = ""), "\n")
  cat(" REPORTES DETALLADOS POR GRADO\n")
  cat(paste(rep("-", 45), collapse = ""), "\n")
  
  tabla_seleccion <- data.frame()
  
  for (g in 1:grado_max) {
    nom_g <- toupper(nombres_grados[g])
    
    if (g == 1) {
      formula_manual <- as.formula("Y ~ TR1")
    } else {
      terminos <- c("TR1", paste0("I(TR1^", 2:g, ")"))
      formula_manual <- as.formula(paste("Y ~", paste(terminos, collapse = " + ")))
    }
    
    m_reg <- lm(formula_manual)
    s_reg <- summary(m_reg)
    
    v_vif <- "N/A"
    if (g > 1) {
      v_res <- try(vif(m_reg), silent = TRUE)
      v_vif <- if(inherits(v_res, "try-error")) "Alta" else paste(round(v_res, 2), collapse = ", ")
    }
    
    tabla_seleccion <- rbind(tabla_seleccion, data.frame(Modelo = nom_g, R2 = s_reg$r.squared, AIC = AIC(m_reg)))
    
    cat(sprintf("\n>>> MODELO %s <<<\n", nom_g))
    print(coef(s_reg))
    cat(sprintf("R2: %.4f | AIC: %.2f | VIF: %s\n", s_reg$r.squared, AIC(m_reg), v_vif))
    
    

    par(mar=c(5, 5, 4, 3), las=1)
    promedios_y <- aggregate(Y ~ TR1, FUN = mean)
    
    # Aumentamos el margen al 25% para alejar la cámara significativamente
    rango_x <- range(TR1)
    rango_y <- range(Y)
    ext_x <- diff(rango_x) * 0.25
    ext_y <- diff(rango_y) * 0.25
    
    # Definimos límites fijos
    lim_x <- c(rango_x[1] - ext_x, rango_x[2] + ext_x)
    lim_y <- c(rango_y[1] - ext_y, rango_y[2] + ext_y)
    
    plot(promedios_y$TR1, promedios_y$Y, pch=21, bg="#3498db80", col="navy", cex=1.8,
         main=paste("Modelo", nom_g, "-", var_y), 
         xlab=paste(factor_dosis, "(Dosis)"), ylab=paste("Promedio", var_y),
         xlim = lim_x, ylim = lim_y, 
         panel.first = grid(col="gray90", lty=1))
    
    # Curva extendida para llenar el nuevo espacio
    d_seq <- seq(lim_x[1], lim_x[2], length.out=200)
    pred <- predict(m_reg, data.frame(TR1=d_seq), interval="confidence")
    
    # Dibujar banda e intervalos
    polygon(c(d_seq, rev(d_seq)), c(pred[,2], rev(pred[,3])), col=rgb(0.1, 0.8, 0.3, 0.1), border=NA)
    lines(d_seq, pred[,1], col="darkgreen", lwd=3)
    # --------------------------------------------------------
  }
  
  cat("\n", paste(rep("-", 45), collapse = ""), "\n")
  cat(" RESUMEN DE SELECCIÓN DE MODELOS\n")
  cat(paste(rep("-", 45), collapse = ""), "\n")
  print(tabla_seleccion)
}




#####################################################################################
# Arreglo factorial combinatorio

names(datos)

factores <- c("distanciamiento", "dosis")
bloque <- "Replica"

# Variables de respuesta
respuestas <- c("altura_m", "diametro_cm", "tallo_ha", "tolete_ha")

# Convertir factores y respuestas
for(f in factores) datos[[f]] <- as.factor(datos[[f]])
datos[[bloque]] <- as.factor(datos[[bloque]])

for(v in respuestas) datos[[v]] <- as.numeric(datos[[v]])


# ANDEVA
for(var in respuestas){
  cat("\n============================================\n")
  cat("ANDEVA para:", var, "\n")
  cat("============================================\n")
  
  # ANDEVA
  formula_anidada <- paste(factores, collapse="*")
  formula <- as.formula(paste0("`", var, "` ~ ", bloque, " + ", formula_anidada))
  modelo <- aov(formula, data=datos)
  print(summary(modelo))
  
  # Pruebas de Tukey para factores individuales con α=0.15
  tukey_results <- list()
  for(f in factores){
    modelo_factor <- aov(as.formula(paste0("`", var, "` ~ ", f)), data=datos)
    tukey_factor <- HSD.test(modelo_factor, f, alpha=0.15, console=FALSE)
    tukey_results[[f]] <- tukey_factor
  }
  
  # Prueba de Tukey para interacción con α=0.15
  datos$interaccion_completa <- interaction(datos[,factores], sep="_")
  modelo_interaccion <- aov(as.formula(paste0("`", var, "` ~ interaccion_completa")), data=datos)
  tukey_interaccion <- HSD.test(modelo_interaccion, "interaccion_completa", alpha=0.15, console=FALSE)
  
  # -----------------------------
  # GRÁFICOS DE PUNTOS CON SE Y LETRAS DE TUKEY
  # -----------------------------
  
  # 1. Gráficos para factores individuales
  for(f in factores){
    # Calcular medias y SE
    resumen <- datos %>%
      group_by(.data[[f]]) %>%
      summarise(media = mean(.data[[var]], na.rm=TRUE),
                se = sd(.data[[var]], na.rm=TRUE)/sqrt(n()),
                .groups="drop")
    
    # Agregar letras de Tukey
    resumen <- resumen %>%
      left_join(
        data.frame(
          nivel = rownames(tukey_results[[f]]$groups),
          grupo = tukey_results[[f]]$groups[,2]
        ), 
        by = c(setNames("nivel", f))
      )
    
    # Gráfico de puntos con SE y letras de Tukey
    p_point <- ggplot(resumen, aes(x = .data[[f]], y = media)) +
      geom_point(size = 4, color = "steelblue") +
      geom_errorbar(aes(ymin = media - se, ymax = media + se), 
                    width = 0.1, color = "steelblue", size = 0.8) +
      geom_text(aes(label = grupo, y = media + se + max(media)*0.02), 
                size = 5, fontface = "bold") +
      labs(title = paste(var, "-", f),
           subtitle = "Letras diferentes indican diferencias significativas (Tukey, α=0.15)",
           x = f, y = var) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10))
    
    print(p_point)
  }
  
  # 2. Gráfico para interacción - CORREGIDO
  if(length(factores) > 1){
    f1 <- factores[1]
    f2 <- factores[2]
    
    # Calcular medias y SE para interacción
    resumen_inter <- datos %>%
      group_by(.data[[f1]], .data[[f2]]) %>%
      summarise(media = mean(.data[[var]], na.rm = TRUE),
                se = sd(.data[[var]], na.rm = TRUE) / sqrt(n()),
                .groups = "drop") %>%
      mutate(Interaccion = interaction(.data[[f1]], .data[[f2]], sep = " : "))
    
    # Agregar letras de Tukey para interacción - CORREGIDO
    tukey_groups <- data.frame(
      Interaccion = rownames(tukey_interaccion$groups),
      grupo = tukey_interaccion$groups[,2]
    )
    
    # Asegurar que los nombres coincidan
    tukey_groups$Interaccion <- gsub("_", " : ", tukey_groups$Interaccion)
    
    resumen_inter <- resumen_inter %>%
      left_join(tukey_groups, by = "Interaccion")
    
    # VERIFICAR SI LAS LETRAS SE UNIERON CORRECTAMENTE
    cat("\nVerificación letras Tukey para interacción -", var, ":\n")
    print(resumen_inter[, c(f1, f2, "media", "grupo")])
    
    # Gráfico de puntos para interacción - CORREGIDO
    p_inter <- ggplot(resumen_inter, aes(x = .data[[f1]], y = media, 
                                         color = .data[[f2]], group = .data[[f2]])) +
      geom_point(size = 4, position = position_dodge(0.3)) +
      geom_errorbar(aes(ymin = media - se, ymax = media + se), 
                    width = 0.15, position = position_dodge(0.3), size = 0.8) +
      geom_text(aes(label = grupo, y = media + se + max(media)*0.03),
                position = position_dodge(0.3), size = 4, fontface = "bold", 
                show.legend = FALSE) +
      labs(title = paste(var, "- Interacción", f1, "×", f2),
           subtitle = "Letras diferentes indican diferencias significativas (Tukey, α=0.15)",
           x = f1, y = var, color = f2) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            legend.position = "bottom")
    
    print(p_inter)
    
    # Gráfico alternativo de interacción - más claro
    p_inter_alt <- ggplot(resumen_inter, aes(x = interaction(.data[[f1]], .data[[f2]]), y = media)) +
      geom_point(size = 4, color = "steelblue") +
      geom_errorbar(aes(ymin = media - se, ymax = media + se), 
                    width = 0.1, color = "steelblue", size = 0.8) +
      geom_text(aes(label = grupo, y = media + se + max(media)*0.02),
                size = 4, fontface = "bold") +
      labs(title = paste(var, "- Interacción", f1, "×", f2),
           subtitle = "Letras diferentes indican diferencias significativas (Tukey, α=0.15)",
           x = paste("Interacción", f1, "×", f2), y = var) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(size = 12),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
    
    print(p_inter_alt)
  }
  
  # Limpiar variable temporal
  datos$interaccion_completa <- NULL
}



# Mostrar tablas de resultados

library(knitr); library(kableExtra); library(agricolae); library(dplyr)

for (var in respuestas) {

  form <- as.formula(paste0("`", var, "` ~ ", bloque, " + ", paste(factores, collapse="*")))
  mod  <- aov(form, data = datos)
  
  render_tabla <- function(df, tit) {
    print(kable(df, digits = 3, format = "html", row.names = F, caption = paste("<b>", tit, "</b>")) %>%
            kable_styling(bootstrap_options = c("striped", "bordered", "condensed"), full_width = F) %>%
            row_spec(0, bold = T, color = "white", background = "#666666"))
  }
  
  df_anova <- as.data.frame(summary(mod)[[1]])
  df_anova <- cbind(Fuente_Variacion = rownames(df_anova), df_anova)
  render_tabla(df_anova, paste("ANDEVA:", var))
  
  efectos <- c(as.list(factores), list(factores)) 
  
  for (efe in efectos) {
    nom_efe <- paste(efe, collapse = ":")
    

    tk <- HSD.test(mod, efe, alpha = 0.15)$groups
    render_tabla(data.frame(Nivel = rownames(tk), Media = tk[,1], Tukey = tk[,2]), 
                 paste("TUKEY -", nom_efe, "(", var, ")"))
    

    lsd <- LSD.test(mod, efe, alpha = 0.15)$groups
    render_tabla(data.frame(Nivel = rownames(lsd), Media = lsd[,1], LSD = lsd[,2]), 
                 paste("LSD -", nom_efe, "(", var, ")"))
  }
  cat("<br><hr style='border: 2px solid #000;'><br>")
}



# Gráficos PEC

library(ggplot2)
library(dplyr)
library(patchwork)
library(agricolae)
library(ggpubr) # Necesario para theme_classic2()

# Elige: "separados", "panel" o "ambos"
opcion_visualizacion <- "panel" 
alpha_sig <- 0.15 

for (var in respuestas) {
  cat("\nGenerando gráficos para:", var, "\n")
  
  f1 <- factores[1]
  f2 <- factores[2]
  
  #--- Función de puntos con estética unificada ---
  generar_plot_punto_horizontal <- function(factor_nom, titulo) {
    res_tk <- HSD.test(modelo, factor_nom, alpha = alpha_sig)$groups
    res_tk[[factor_nom]] <- rownames(res_tk)
    
    df_p <- datos %>%
      group_by(.data[[factor_nom]]) %>%
      summarise(media = mean(.data[[var]], na.rm = TRUE),
                se = sd(.data[[var]], na.rm = TRUE) / sqrt(n()), .groups = "drop") %>%
      left_join(res_tk, by = factor_nom)
    
    ggplot(df_p, aes(y = reorder(.data[[factor_nom]], media), x = media)) +
      # Estética unificada: height = 0.2
      geom_errorbarh(aes(xmin = media - se, xmax = media + se), height = 0.2) +
      # Estética unificada: size = 3, negro
      geom_point(size = 3, color = "black") +
      # Estética unificada: size = 4.5, Arial, vjust = -1
      geom_text(aes(label = sprintf("%.2f %s", media, trimws(groups))), 
                hjust = -0.1, vjust = -1, size = 4.5, family = "Arial") +
      scale_x_continuous(expand = expansion(mult = c(0.1, 0.5))) +
      labs(title = titulo, x = "Media relativa estimada ± SE", y = factor_nom) +
      # Temas unificados
      theme_minimal(base_family = "Arial") +
      theme(strip.text = element_text(size = 13), text = element_text(size = 12)) +
      theme_classic2()
  }
  
  #--- Interacción (Puntos) con estética unificada ---
  datos$int_temp <- interaction(datos[[f1]], datos[[f2]], sep = ":")
  mod_int <- aov(as.formula(paste0("`", var, "` ~ int_temp")), data = datos)
  tk_int <- HSD.test(mod_int, "int_temp", alpha = alpha_sig)$groups
  tk_int$combinacion <- rownames(tk_int)
  
  df_int <- datos %>%
    group_by(.data[[f1]], .data[[f2]]) %>%
    summarise(media = mean(.data[[var]], na.rm = TRUE),
              se = sd(.data[[var]], na.rm = TRUE) / sqrt(n()), .groups = "drop") %>%
    mutate(combinacion = paste(.data[[f1]], .data[[f2]], sep = ":")) %>%
    left_join(tk_int[, c("combinacion", "groups")], by = "combinacion")
  
  p_int_puntos <- ggplot(df_int, aes(y = reorder(.data[[f2]], media), x = media)) +
    geom_errorbarh(aes(xmin = media - se, xmax = media + se), height = 0.2) +
    geom_point(size = 3, color = "black") +
    geom_text(aes(label = sprintf("%.2f %s", media, trimws(groups))), 
              hjust = -0.1, vjust = -1, size = 4.5, family = "Arial") +
    facet_wrap(as.formula(paste("~", f1)), scales = "free_y") +
    scale_x_continuous(expand = expansion(mult = c(0.1, 0.5))) +
    labs(title = paste("Interacción:", f1, "x", f2), x = "Media relativa estimada ± SE", y = f2) +
    theme_minimal(base_family = "Arial") +
    theme(strip.text = element_text(size = 13, face="bold"), text = element_text(size = 12)) +
    theme_classic2()
  
  #--- Interacción (Barras) con estética mejorada ---
  p_int_barras <- ggplot(df_int, aes(x = .data[[f1]], y = media, fill = .data[[f2]])) +
    geom_bar(stat = "identity", position = position_dodge(0.9), color = "black") +
    geom_errorbar(aes(ymin = media - se, ymax = media + se), 
                  width = 0.2, position = position_dodge(0.9)) +
    geom_text(aes(label = trimws(groups), y = media + se), 
              position = position_dodge(0.9), vjust = -0.5, size = 4.5, family="Arial") +
    scale_fill_grey(start = 0.4, end = 0.9) + 
    labs(title = paste("Comparación de Medias (Barras):", f1, "x", f2), 
         x = f1, y = var, fill = f2) +
    theme_classic2() +
    theme(legend.position = "top", plot.title = element_text(size=13, face="bold"))
  
  # Generar efectos principales
  p1 <- generar_plot_punto_horizontal(f1, paste("Efecto:", f1))
  p2 <- generar_plot_punto_horizontal(f2, paste("Efecto:", f2))
  
  if (opcion_visualizacion %in% c("separados", "ambos")) {
    print(p1); print(p2); print(p_int_puntos); print(p_int_barras)
  }
  
  if (opcion_visualizacion %in% c("panel", "ambos")) {
    panel_final <- (p1 | p2) / p_int_barras + 
      plot_annotation(title = paste("ANÁLISIS FACTORIAL COMBINATORIO:", toupper(var)),
                      theme = theme(plot.title = element_text(size = 16, face = "bold", family="Arial")))
    print(panel_final)
    
    ggsave(paste0("Panel_Factorial_", var, ".png"), panel_final, width = 400, height = 300, dpi = 600, units = "mm")
  }
  
  datos$int_temp <- NULL
}



# Gráfico de barras
names(datos)
library(ggplot2)
library(scales)

# Identificar factores del bucle
f1_name <- factores[1]
f2_name <- factores[2]

# 1. Resumen estadístico (Media y Desviación Estándar)
df_resumen_barras <- datos %>%
  group_by(.data[[f1_name]], .data[[f2_name]]) %>%
  summarise(
    MEDIA = mean(.data[[var]], na.rm = TRUE),
    SD = sd(.data[[var]], na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Crear llave para unir las letras de Tukey
  mutate(interaccion_completa = interaction(.data[[f1_name]], .data[[f2_name]], sep = "_"))

# 2. Preparar las letras de Tukey (usando el objeto tukey_interaccion ya existente)
letras_data <- as.data.frame(tukey_interaccion$groups)
letras_data$interaccion_completa <- rownames(letras_data)

# Unir letras al resumen del gráfico
df_resumen_barras <- df_resumen_barras %>%
  left_join(letras_data %>% select(interaccion_completa, groups), by = "interaccion_completa")

# 3. Calcular espacio para que las letras no choquen (15% del máximo)
offset_letras <- max(df_resumen_barras$MEDIA + df_resumen_barras$SD, na.rm = TRUE) * 0.15

# 4. Construcción del Gráfico
p_paper <- ggplot(df_resumen_barras, aes(x = .data[[f1_name]], y = MEDIA, fill = .data[[f2_name]])) +
  # Barras con borde negro fino
  geom_bar(stat = "identity", position = position_dodge(0.9), color = "black", size = 0.3) +
  # Barras de Error (Desviación Estándar)
  geom_errorbar(aes(ymin = MEDIA - SD, ymax = MEDIA + SD),
                width = 0.2, position = position_dodge(0.9), color = "black") +
  # Letras de significancia sobre las barras
  geom_text(aes(label = groups, y = MEDIA + SD + (offset_letras/4)), 
            position = position_dodge(0.9), vjust = 0, size = 4, fontface = "bold") +
  # Facetado para mayor claridad (opcional, puedes comentarlo si prefieres todo en un eje)
  facet_wrap(as.formula(paste("~", f2_name)), labeller = label_both) +
  # Escalas y Colores profesionales
  scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.2))) +
  scale_fill_brewer(palette = "Paired") + 
  labs(title = paste("Efecto de la Interacción en:", toupper(var)),
       subtitle = "Medias ± Desviación Estándar. Letras distintas indican diferencias (Tukey α=0.15)",
       x = f1_name, y = var, fill = f2_name) +
  theme_classic() + # Estilo limpio sin cuadrícula
  theme(
    strip.background = element_rect(fill = "gray90", color = "black"),
    strip.text = element_text(face = "bold"),
    legend.position = "top",
    axis.text = element_text(color = "black", size = 10),
    plot.title = element_text(face = "bold", size = 14)
  )

# Visualizar
print(p_paper)

#######################################################
################## Arreglo factorial combinatorio con tratamiento adicional


library(emmeans)
library(dplyr)
library(multcomp)
library(multcompView)
library(ggplot2)
library(patchwork)
library(agricolae)

respuestas <- c("rendimiento_kg_ton", "tch", "tah", "pureza")
factores <- c("volumen", "dosis") 
bloq <- "Replica"
col_trata <- "Tratamiento"      
n_testigo <- "Testigo"
alpha <- 0.15                   
opcion_visualizacion <- "panel" # "separados", "panel" o "ambos"

# --- Coeficientes de contrastes ortogonales----
coefs_manuales <- c(9, -1, -1, -1, -1, -1, -1, -1, -1, -1)

for (var in respuestas) {
  message(paste("Procesando análisis y gráficos para:", var))
  
  f_global <- as.formula(paste0("`", var, "` ~ `", col_trata, "` + `", bloq, "`"))
  mod_global <- aov(f_global, data = datos)
  an_g <- summary(mod_global)[[1]]
  

  emm_g_df <- as.data.frame(emmeans(mod_global, col_trata))
  r_val <- mean(datos %>% group_by(.data[[col_trata]]) %>% summarise(n=n()) %>% pull(n))
  sc_contraste <- (sum(coefs_manuales * emm_g_df$emmean)^2) / sum((coefs_manuales^2) / r_val)
  
  
  datos_fact <- datos %>% filter(.data[[col_trata]] != n_testigo)
  f_fact <- as.formula(paste0("`", var, "` ~ ", paste(factores, collapse="*"), " + `", bloq, "`"))
  mod_aux <- aov(f_fact, data = datos_fact)
  an_f <- summary(mod_aux)[[1]]
  
  # ANOVA final
  fv_f_reales <- setdiff(rownames(an_f), c(bloq, "Residuals"))
  tabla_unificada <- data.frame(
    FV = c(bloq, fv_f_reales, "Testigo vs Resto", "Error", "Total"),
    GL = c(an_g[bloq, "Df"], an_f[fv_f_reales, "Df"], 1, an_g["Residuals", "Df"], sum(an_g$Df)),
    SC = c(an_g[bloq, "Sum Sq"], an_f[fv_f_reales, "Sum Sq"], sc_contraste, an_g["Residuals", "Sum Sq"], sum(an_g$`Sum Sq`))
  ) %>% mutate(CM = SC/GL, F_calc = CM/(an_g["Residuals", "Sum Sq"]/an_g["Residuals", "Df"]), 
               p_val = pf(F_calc, GL, an_g["Residuals", "Df"], lower.tail = FALSE))
  print(tabla_unificada)
  
  # Gráficos PEC

  f1 <- factores[1]
  f2 <- factores[2]
  
  # 1. Función para Efectos Principales con estética unificada
  generar_plot_efecto <- function(factor_nom, titulo) {
    res <- HSD.test(mod_aux, factor_nom, alpha = alpha)$groups
    res[[factor_nom]] <- rownames(res)
    
    df_p <- datos_fact %>%
      group_by(.data[[factor_nom]]) %>%
      summarise(emmean = mean(.data[[var]], na.rm = TRUE),
                SE = sd(.data[[var]], na.rm = TRUE) / sqrt(n()), .groups = "drop") %>%
      left_join(res, by = factor_nom)
    
    ggplot(df_p, aes(y = reorder(.data[[factor_nom]], emmean), x = emmean)) +
      # Estética unificada: height = 0.2
      geom_errorbarh(aes(xmin = emmean - SE, xmax = emmean + SE), height = 0.2, color = "black") +
      # Estética unificada: puntos negros tamaño 3
      geom_point(size = 3, color = "black") +
      # Estética unificada: size = 4.5, Arial, vjust = -1
      geom_text(aes(label = sprintf("%.2f %s", emmean, trimws(groups))), 
                hjust = -0.1, vjust = -1, size = 4.5, family = "Arial") +
      # Expansión para evitar cortes de texto
      scale_x_continuous(expand = expansion(mult = c(0.1, 0.5))) +
      labs(title = titulo, x = "Media relativa estimada ± SE", y = factor_nom) +
      # Temas unificados
      theme_minimal(base_family = "Arial") +
      theme(strip.text = element_text(size = 13), text = element_text(size = 12)) +
      theme_classic2()
  }
  
  # 2. Función para Interacción con estética unificada
  generar_plot_interaccion <- function() {
    tk_int <- HSD.test(mod_aux, c(f1, f2), alpha = alpha)$groups
    tk_int$combinacion <- rownames(tk_int)
    
    df_int <- datos_fact %>%
      group_by(.data[[f1]], .data[[f2]]) %>%
      summarise(emmean = mean(.data[[var]], na.rm = TRUE),
                SE = sd(.data[[var]], na.rm = TRUE) / sqrt(n()), .groups = "drop") %>%
      mutate(combinacion = paste(.data[[f1]], .data[[f2]], sep = ":")) %>%
      left_join(tk_int[, c("combinacion", "groups")], by = "combinacion")
    
    ggplot(df_int, aes(y = reorder(.data[[f2]], emmean), x = emmean)) +
      # Estética unificada
      geom_errorbarh(aes(xmin = emmean - SE, xmax = emmean + SE), height = 0.2, color = "black") +
      geom_point(size = 3, color = "black") +
      geom_text(aes(label = sprintf("%.2f %s", emmean, trimws(groups))), 
                hjust = -0.1, vjust = -1, size = 4.5, family = "Arial") +
      facet_wrap(as.formula(paste("~", f1)), scales = "free_y") +
      scale_x_continuous(expand = expansion(mult = c(0.1, 0.5))) +
      labs(title = paste("Interacción:", f1, "x", f2), 
           subtitle = paste("Variable:", var), 
           x = "Media relativa estimada ± SE", y = f2) +
      # Temas unificados
      theme_minimal(base_family = "Arial") +
      theme(strip.background = element_rect(fill = "grey95", color = "white"), 
            strip.text = element_text(color = "black", face = "bold", size = 13),
            text = element_text(size = 12)) +
      theme_classic2()
  }
  
  # 3. EJECUCIÓN, PANEL Y GUARDADO
  p1 <- generar_plot_efecto(f1, paste("Efecto Principal:", f1))
  p2 <- generar_plot_efecto(f2, paste("Efecto Principal:", f2))
  p_int <- generar_plot_interaccion()
  
  if (opcion_visualizacion %in% c("separados", "ambos")) {
    print(p1); print(p2); print(p_int)
  }
  
  if (opcion_visualizacion %in% c("panel", "ambos")) {
    panel_completo <- (p1 | p2) / p_int + 
      plot_annotation(title = paste("ANÁLISIS DE ARREGLO FACTORIAL + TESTIGO:", toupper(var)),
                      theme = theme(plot.title = element_text(size = 16, face = "bold", family = "Arial")))
    
    print(panel_completo)
    
    # Guardado en alta resolución
    ggsave(paste0("Panel_Factorial_Testigo_", var, ".png"), panel_completo, 
           width = 400, height = 300, dpi = 900, units = "mm")
  }  }
  
#######################
############################################ Parcelas divididas

colnames(datos)
factores <- c("coctel", "biosustrato")   # factores principales
bloq <- "Replica"                 # bloque
respuestas <- c("tch")

# Asegurar que los factores y bloque sean factores
for(f in c(factores, bloq)){
  datos[[f]] <- as.factor(datos[[f]])
}

########################### ANDEVA ###########################
for(var in respuestas){
  cat("\n====================================\n")
  cat("ANDEVA para la variable:", var, "\n")
  cat("====================================\n")
  
  # Modelo con bloques y parcelas divididas
  formula_anidada <- paste0("`", var, "` ~ ", factores[1], " + ", factores[2], " + ", bloq,
                            " + ", factores[1], ":", factores[2], " + Error(", bloq, "/", factores[1], ")")
  
  modelo <- aov(as.formula(formula_anidada), data = datos)
  print(summary(modelo))
}

########################### SUPUESTOS ###########################
for(var in respuestas){
  cat("\n====================================\n")
  cat("Supuestos para la variable:", var, "\n")
  cat("====================================\n")
  
  # Modelo ANDEVA con error (normalidad y homogeneidad)
  formula_aov <- paste0("`", var, "` ~ ", factores[1], "*", factores[2], " + ", bloq, "/", factores[1])
  modelo_aov <- aov(as.formula(formula_aov), data=datos)
  
  # Normalidad de residuos
  qqPlot(modelo_aov$residuals, main=paste("QQ-plot:", var), col="red", lwd=2, pch=16)
  if(length(unique(modelo_aov$residuals)) > 1){
    cat("\nShapiro-Wilk:\n")
    print(shapiro.test(modelo_aov$residuals))
  }
  
  # Residuos vs Ajustados
  plot(modelo_aov$fitted.values, modelo_aov$residuals, col="blue", pch=16,
       xlab="Valores Ajustados", ylab="Residuos",
       main=paste("Residuos vs Ajustados:", var))
  abline(h=0, col="red", lwd=2)
  
  # Homogeneidad (Bartlett y Levene)
  formula_interaccion <- as.formula(paste0("`", var, "` ~ interaction(", factores[1], ",", factores[2], ")"))
  if(length(unique(datos[[var]])) > 1){
    cat("\nBartlett test:\n")
    print(bartlett.test(formula_interaccion, data=datos))
    cat("\nLevene test (mediana):\n")
    print(leveneTest(formula_interaccion, data=datos, center="median"))
  }
  
  # Independencia de errores
  cat("\nIndependencia de errores (ACF de residuos):\n")
  acf(modelo_aov$residuals, main=paste("ACF de residuos:", var))
}


########################### PMM ###########################
# Definir el nivel de significancia global
alpha <- 0.15   # <--- Cambiar el nivel de significancia 

for(var in respuestas){
  cat("\n====================================\n")
  cat("PMM para la variable:", var, "\n")
  cat("====================================\n")
  
  # Interacción completa
  datos$interaccion_completa <- interaction(datos[, factores], sep="_")
  modelo_interaccion <- aov(as.formula(paste0("`", var, "` ~ interaccion_completa")), data=datos)
  
  if(length(unique(datos[[var]])) > 1){
    cat("\n--- Tukey HSD para interacción ---\n")
    HSD.test(modelo_interaccion, "interaccion_completa", group=TRUE, console=TRUE, alpha=alpha)
    
    cat("\n--- Scott-Knott para interacción ---\n")
    try({
      sk <- SK(modelo_interaccion, which="interaccion_completa", dispersion="se", sig.level=alpha)
      print(summary(sk))
    })
  }
  
  # Factores individuales
  for(f in factores){
    modelo_factor <- aov(as.formula(paste0("`", var, "` ~ ", f)), data=datos)
    
    cat("\n--- Tukey HSD para factor:", f, "---\n")
    HSD.test(modelo_factor, f, group=TRUE, console=TRUE, alpha=alpha)
    
    cat("\n--- Scott-Knott para factor:", f, "---\n")
    try({
      sk_f <- SK(modelo_factor, which=f, dispersion="se", sig.level=alpha)
      print(summary(sk_f))
    })
  }
  
  datos$interaccion_completa <- NULL
}
########################### GRÁFICOS ###########################
library(ggplot2)
library(dplyr)
library(agricolae)

# ==========================================================
# CONFIGURACIÓN INICIAL
# ==========================================================
colnames(datos)
alpha_global <- 0.15   
factores <- c("fertilidad", "formula_bacterias") 
bloq <- "Replica"
respuestas <- c("tch", "rendimiento_kg_ton", "tah", "pureza")

# Asegurar formato de factores
for(f in c(factores, bloq)){
  datos[[f]] <- as.factor(datos[[f]])
}


for(var in respuestas){
  cat("\n====================================\n")
  cat("ANALIZANDO VARIABLE:", toupper(var), " (Alpha =", alpha_global, ")\n")
  cat("====================================\n")
  
  # --- 1. GRÁFICOS DE FACTORES INDIVIDUALES ---
  for(f in factores){
    # Cálculo estadístico
    mod_f <- aov(as.formula(paste0("`", var, "` ~ ", f)), data = datos)
    tukey_f <- HSD.test(mod_f, f, group = TRUE, alpha = alpha_global)
    
    # Preparar letras en mayúscula y negro
    letras_f <- tukey_f$groups
    letras_f[[f]] <- rownames(letras_f)
    letras_f$groups <- toupper(as.character(letras_f$groups))
    
    # Resumen para el gráfico
    resumen_f <- datos %>%
      group_by(.data[[f]]) %>%
      summarise(media = mean(.data[[var]], na.rm = TRUE),
                se = sd(.data[[var]], na.rm = TRUE) / sqrt(n()),
                n_val = n(), .groups = "drop") %>%
      left_join(letras_f[, c(f, "groups")], by = f)
    
    # Gráfico Profesional Factor Individual
    p1 <- ggplot(resumen_f, aes(x = .data[[f]], y = media)) +
      # Capas de datos
      geom_errorbar(aes(ymin = media - se, ymax = media + se), width = 0.1, size = 0.8, color = "black") +
      geom_point(shape = 21, fill = "red", color = "black", size = 4, stroke = 1.2) +
      geom_text(aes(label = groups, y = media + se), vjust = -1.2, fontface = "bold", size = 5, color = "black") +
      # Estética de Marco y Títulos
      labs(title = paste("Efecto Principal:", toupper(f)),
           subtitle = paste("Variable:", var, "| Media ± Error Estándar | n =", unique(resumen_f$n_val)),
           x = paste("Niveles de", f), y = paste("Promedio de", var)) +
      theme_bw() +
      theme(panel.border = element_rect(colour = "black", fill=NA, size=1.5), # Marco grueso
            panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold", size = 14),
            axis.title = element_text(face = "bold")) +
      expand_limits(y = max(resumen_f$media + resumen_f$se) * 1.2)
    
    print(p1)
  }
  
  # --- 2. GRÁFICO DE INTERACCIÓN (Puntos con Dodge y Marco) ---
  if(length(factores) > 1){
    f1 <- factores[1]; f2 <- factores[2]
    
    # Modelo para interacción
    datos$int_label <- interaction(datos[[f1]], datos[[f2]], sep = ":")
    mod_int <- aov(as.formula(paste0("`", var, "` ~ int_label")), data = datos)
    tukey_int <- HSD.test(mod_int, "int_label", group = TRUE, alpha = alpha_global)
    
    letras_int <- tukey_int$groups
    letras_int$int_label <- rownames(letras_int)
    letras_int$groups <- toupper(as.character(letras_int$groups))
    
    resumen_int <- datos %>%
      group_by(.data[[f1]], .data[[f2]]) %>%
      summarise(media = mean(.data[[var]], na.rm = TRUE),
                se = sd(.data[[var]], na.rm = TRUE) / sqrt(n()), .groups = "drop") %>%
      mutate(int_label = interaction(.data[[f1]], .data[[f2]], sep = ":")) %>%
      left_join(letras_int[, c("int_label", "groups")], by = "int_label")
    
    # Gráfico de Interacción Profesional
    p_int <- ggplot(resumen_int, aes(x = .data[[f1]], y = media, fill = .data[[f2]])) +
      # Capas con dodge para evitar solapamiento
      geom_errorbar(aes(ymin = media - se, ymax = media + se), 
                    width = 0.2, size = 0.8, position = position_dodge(0.4), color = "black") +
      geom_point(aes(shape = .data[[f2]]), size = 3, stroke = 1.2, 
                 position = position_dodge(0.4), fill = "red", color = "black") +
      geom_text(aes(label = groups, y = media + se), 
                position = position_dodge(0.4), vjust = -1.2, 
                fontface = "bold", size = 4.5, color = "black") +
      # Configuración de formas (hasta 5 niveles en f2)
      scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
      # Marco y Estética
      labs(title = paste("Interacción:", f1, "x", f2),
           subtitle = paste("Comparación de medias mediante prueba de Tukey (α =", alpha_global, ")"),
           x = f1, y = var, shape = f2, fill = f2) +
      theme_bw() +
      theme(panel.border = element_rect(colour = "black", fill=NA, size=1.5), # Marco grueso
            legend.position = "bottom",
            legend.box.background = element_rect(colour = "black"),
            plot.title = element_text(face = "bold", size = 15)) +
      expand_limits(y = max(resumen_int$media + resumen_int$se) * 1.2)
    
    print(p_int)
    datos$int_label <- NULL # Limpiar
  }
}


# Obtención de resultados en tablas:

library(knitr); library(kableExtra); library(agricolae); library(dplyr)

for (var in respuestas) {

  form_split <- as.formula(paste0("`", var, "` ~ ", factores[1], " * ", factores[2], 
                                  " + Error(", bloq, "/", factores[1], ")"))
  mod_split <- aov(form_split, data = datos)
  
  render_tabla_gris <- function(df, tit) {
    print(kable(df, digits = 4, format = "html", row.names = F, caption = paste("<b>", tit, "</b>")) %>%
            kable_styling(bootstrap_options = c("striped", "bordered", "condensed"), full_width = F) %>%
            row_spec(0, bold = T, color = "white", background = "#666666"))
  }
  
  sum_mod <- summary(mod_split)
 
  df_anova <- do.call(rbind, lapply(sum_mod, function(x) as.data.frame(x[[1]])))
  df_anova <- cbind(Fuente_Variacion = rownames(df_anova), df_anova)
  render_tabla_gris(df_anova, paste("ANDEVA PARCELAS DIVIDIDAS:", var))
  
  mod_aux <- aov(as.formula(paste0("`", var, "` ~ ", bloq, " + ", factores[1], " * ", factores[2])), data = datos)
  
  efectos_split <- c(as.list(factores), list(factores))
  
  for (efe in efectos_split) {
    nom_efe <- paste(efe, collapse = ":")
    
  
    tk <- HSD.test(mod_aux, efe, alpha = alpha)$groups
    render_tabla_gris(data.frame(Nivel = rownames(tk), Media = tk[,1], Tukey = tk[,2]), 
                      paste("TUKEY -", nom_efe, "(", var, ")"))
    
    lsd <- LSD.test(mod_aux, efe, alpha = alpha)$groups
    render_tabla_gris(data.frame(Nivel = rownames(lsd), Media = lsd[,1], LSD = lsd[,2]), 
                      paste("LSD -", nom_efe, "(", var, ")"))
  }
  cat("<br><hr style='border: 2px solid #000;'><br>")
}



# Barras de error (PEC)

# Elige: "separados", "panel" o "ambos"
opcion_visualizacion <- "panel" 

for (var in respuestas) {
  message(paste("Procesando gráficos para:", var))
  
  f1 <- factores[1]
  f2 <- factores[2]
  mod_aux <- aov(as.formula(paste0("`", var, "` ~ ", bloq, " + ", f1, " * ", f2)), data = datos)
  
  # --- Función Efecto Principal ---
  generar_plot_efecto <- function(factor_nom, titulo) {
    res <- HSD.test(mod_aux, factor_nom, alpha = alpha)$groups
    res[[factor_nom]] <- rownames(res)
    
    df_p <- datos %>%
      group_by(.data[[factor_nom]]) %>%
      summarise(emmean = mean(.data[[var]], na.rm = TRUE),
                SE = sd(.data[[var]], na.rm = TRUE) / sqrt(n()), .groups = "drop") %>%
      left_join(res, by = factor_nom)
    
    ggplot(df_p, aes(y = reorder(.data[[factor_nom]], emmean), x = emmean)) +
      # Estética unificada: height = 0.2
      geom_errorbarh(aes(xmin = emmean - SE, xmax = emmean + SE), height = 0.2, color = "black") +
      # Estética unificada: puntos negros
      geom_point(size = 3, color = "black") + 
      # Estética unificada: size = 4.5, family Arial
      geom_text(aes(label = sprintf("%.2f %s", emmean, trimws(groups))), 
                hjust = -0.1, vjust = -1, size = 4.5, family = "Arial") +
      scale_x_continuous(expand = expansion(mult = c(0.1, 0.5))) +
      labs(title = titulo, x = "Media relativa estimada ± SE", y = factor_nom) +
      # Cambio a theme_classic2
      theme_minimal(base_family = "Arial") +
      theme(plot.title = element_text(face = "bold", size = 13),
            strip.text = element_text(size = 13),
            text = element_text(size = 12)) +
      theme_classic2()
  }
  
  # --- Función Interacción ---
  generar_plot_interaccion <- function() {
    tk_int <- HSD.test(mod_aux, c(f1, f2), alpha = alpha)$groups
    tk_int$combinacion <- rownames(tk_int)
    
    df_int <- datos %>%
      group_by(.data[[f1]], .data[[f2]]) %>%
      summarise(emmean = mean(.data[[var]], na.rm = TRUE),
                SE = sd(.data[[var]], na.rm = TRUE) / sqrt(n()), .groups = "drop") %>%
      mutate(combinacion = paste(.data[[f1]], .data[[f2]], sep = ":")) %>%
      left_join(tk_int[, c("combinacion", "groups")], by = "combinacion")
    
    ggplot(df_int, aes(y = reorder(.data[[f2]], emmean), x = emmean)) +
      geom_errorbarh(aes(xmin = emmean - SE, xmax = emmean + SE), height = 0.2, color = "black") +
      geom_point(size = 3, color = "black") + 
      geom_text(aes(label = sprintf("%.2f %s", emmean, trimws(groups))), 
                hjust = -0.1, vjust = -1, size = 4.5, family = "Arial") +
      facet_wrap(as.formula(paste("~", f1)), scales = "free_y") +
      scale_x_continuous(expand = expansion(mult = c(0.1, 0.5))) +
      labs(title = paste("Interacción:", f1, "x", f2), 
           subtitle = paste("Variable:", var), x = "Media relativa estimada ± SE", y = f2) +
      theme_minimal(base_family = "Arial") +
      theme(legend.position = "none", 
            strip.background = element_rect(fill = "grey95", color = "white"), 
            strip.text = element_text(color = "black", face = "bold", size = 13),
            text = element_text(size = 12)) +
      theme_classic2()
  }
  
  p1 <- generar_plot_efecto(f1, paste("Efecto Principal:", f1))
  p2 <- generar_plot_efecto(f2, paste("Efecto Principal:", f2))
  p_int <- generar_plot_interaccion()
  
  # --- Guardado y Visualización ---
  if (opcion_visualizacion %in% c("separados", "ambos")) {
    ggsave(paste0("Efecto_", f1, "_", var, ".png"), p1, width = 400, height = 200, dpi = 600, units = "mm")
    ggsave(paste0("Efecto_", f2, "_", var, ".png"), p2, width = 400, height = 200, dpi = 600, units = "mm")
    ggsave(paste0("Interaccion_", var, ".png"), p_int, width = 400, height = 250, dpi = 600, units = "mm")
    print(p1); print(p2); print(p_int)
  }
  
  if (opcion_visualizacion %in% c("panel", "ambos")) {
    panel_completo <- (p1 | p2) / p_int + 
      plot_annotation(title = paste("ANÁLISIS DE PARCELAS DIVIDIDAS:", toupper(var)),
                      theme = theme(plot.title = element_text(size = 16, face = "bold", family = "Arial")))
    print(panel_completo)
    ggsave(paste0("Panel_Negro_SplitPlot_", var, ".png"), panel_completo, width = 400, height = 350, dpi = 600, units = "mm")
  }
}


# Gráfico con anidamiento

pacman::p_load(dplyr, tidyr, ggplot2, ggpubr, emmeans, multcomp, multcompView, patchwork)

# --- 1. CONFIGURACIÓN DE USUARIO ---
colnames(datos)
vars_analisis <- c("tch") 
f_principal   <- "coctel"
f_sub         <- "biosustrato"
f_bloque      <- "Replica"
alpha_val     <- 0.15 # Nivel de significancia para las letras

# --- 2. PROCESAMIENTO ---
lista_graficos <- list()

for(v in vars_analisis) {
  message(paste("Analizando variable:", v))
  
  # Modelo para obtener letras
  formula_mod <- as.formula(paste0("`", v, "` ~ `", f_bloque, "` + `", f_principal, "` * `", f_sub, "`"))
  modelo <- aov(formula_mod, data = datos)
  
  # Obtener letras de significancia
  # reversed = TRUE hace que la media mayor sea "a"
  letras <- emmeans(modelo, as.formula(paste("~", f_sub, "|", f_principal))) %>%
    multcomp::cld(Letters = letters, alpha = alpha_val, sort = TRUE, reversed = TRUE) %>%
    as.data.frame() %>%
    mutate(Variable = v)
  
  # Cálculo de Medias Reales y SE
  medias_reales <- datos %>%
    group_by(!!sym(f_principal), !!sym(f_sub)) %>%
    summarise(media_r = mean(.data[[v]], na.rm = TRUE),
              se_r = sd(.data[[v]], na.rm = TRUE) / sqrt(n()), .groups = "drop")
  
  # Unión de datos para el gráfico
  df_plot <- letras %>%
    left_join(medias_reales, by = c(f_principal, f_sub))
  
  # --- 3. CREACIÓN DEL GRÁFICO ---
  # Reorder por media_r para que el gráfico sea scannable
  gg <- ggplot(df_plot, aes(y = reorder(!!sym(f_sub), media_r), x = media_r)) +
    geom_errorbarh(aes(xmin = media_r - se_r, xmax = media_r + se_r), height = 0.2) +
    geom_point(size = 3, color = "black") +
    # Etiqueta: Media + Letra (Limpia espacios con trimws)
    geom_text(aes(label = sprintf("%.2f %s", media_r, trimws(.group))),
              hjust = -0.1, vjust = -1.1, size = 4.2) +
    
    facet_wrap(as.formula(paste("~", f_principal)), scales = "free_x") + 
    
    scale_x_continuous(expand = expansion(mult = c(0.1, 0.5))) +
    labs(x = "Media Aritmética Real ± SE", 
         y = f_sub,
         title = paste("VARIABLE:", toupper(v)),
         subtitle = paste("Letra 'a' para la media más alta (alpha =", alpha_val, ")")) +
    
    theme_classic() +
    theme(
      strip.text = element_text(size = 13, face = "bold"),
      strip.background = element_blank(),
      panel.spacing = unit(1.5, "lines"),
      plot.title = element_text(face = "bold", size = 14)
    )
  
  lista_graficos[[v]] <- gg
}

print(lista_graficos[[1]])

############################## 
#################################### Parcelas divididas con testigo adicional

names(datos)
library(emmeans)
library(dplyr)
library(ggplot2)
library(patchwork)
library(agricolae)
library(car)


respuestas <- c("rendimiento", "tch", "tah", "pureza")
f_principal <- "fertilidad"  
f_sub <- "formula_bacterias"          
bloq <- "Replica"
col_trata <- "Tratamiento" 
n_testigo <- "100%NO APLICAR"
alpha <- 0.15 

# Coeficientes de los contrastes ortogonales (Testigo vs todos)
coefs_manuales <- c(-1,	-1,	-1,	-1,	-1,	-1,	6)

for (var in respuestas) {
  message(paste("\n>>> ANALIZANDO VARIABLE:", var))
  
  f_global <- as.formula(paste0("`", var, "` ~ `", col_trata, "` + `", bloq, "`"))
  mod_global <- aov(f_global, data = datos)
  an_g <- summary(mod_global)[[1]]
  
  emm_g_df <- as.data.frame(emmeans(mod_global, col_trata))
  r_val <- mean(datos %>% group_by(.data[[col_trata]]) %>% summarise(n=n()) %>% pull(n))
  sc_contraste <- (sum(coefs_manuales * emm_g_df$emmean)^2) / sum((coefs_manuales^2) / r_val)
  
  datos_fact <- datos %>% filter(.data[[col_trata]] != n_testigo)
  f_split <- as.formula(paste0("`", var, "` ~ `", bloq, "` + `", f_principal, "` * `", f_sub, "` + Error(`", bloq, "`/`", f_principal, "`)"))
  mod_split <- aov(f_split, data = datos_fact)
  an_s <- summary(mod_split)
  
  err_a <- an_s[[2]][[1]]["Residuals", ]
  err_b <- an_s[[3]][[1]]["Residuals", ]
  
  tabla_unificada <- data.frame(
    FV = c(bloq, f_principal, "Error (a)", f_sub, paste0(f_principal, ":", f_sub), "Testigo vs Resto", "Error (b)"),
    GL = c(an_s[[2]][[1]][bloq, "Df"], 
           an_s[[2]][[1]][f_principal, "Df"], 
           err_a$Df, 
           an_s[[3]][[1]][f_sub, "Df"], 
           an_s[[3]][[1]][paste0(f_principal, ":", f_sub), "Df"], 
           1, 
           err_b$Df),
    SC = c(an_s[[2]][[1]][bloq, "Sum Sq"], 
           an_s[[2]][[1]][f_principal, "Sum Sq"], 
           err_a$`Sum Sq`, 
           an_s[[3]][[1]][f_sub, "Sum Sq"], 
           an_s[[3]][[1]][paste0(f_principal, ":", f_sub), "Sum Sq"], 
           sc_contraste, 
           err_b$`Sum Sq`)
  ) %>% mutate(CM = SC/GL)
  
  tabla_unificada <- tabla_unificada %>% mutate(
    F_calc = case_when(
      FV == f_principal ~ CM / (err_a$`Sum Sq`/err_a$Df),
      FV %in% c(f_sub, paste0(f_principal, ":", f_sub), "Testigo vs Resto") ~ CM / (err_b$`Sum Sq`/err_b$Df),
      TRUE ~ NA_real_
    ),
    p_val = case_when(
      FV == f_principal ~ pf(F_calc, GL, err_a$Df, lower.tail = FALSE),
      FV %in% c(f_sub, paste0(f_principal, ":", f_sub), "Testigo vs Resto") ~ pf(F_calc, GL, err_b$Df, lower.tail = FALSE),
      TRUE ~ NA_real_
    )
  )
  
  print(tabla_unificada)
  
  
  message("--- Pruebas de Tukey ---")
  tukey_a <- HSD.test(datos_fact[[var]], datos_fact[[f_principal]], 
                      DFerror = err_a$Df, MSerror = err_a$`Mean Sq`, alpha = alpha)
  
  tukey_b <- HSD.test(datos_fact[[var]], datos_fact[[f_sub]], 
                      DFerror = err_b$Df, MSerror = err_b$`Mean Sq`, alpha = alpha)
  
  tukey_int <- HSD.test(datos_fact[[var]], interaction(datos_fact[[f_principal]], datos_fact[[f_sub]]), 
                        DFerror = err_b$Df, MSerror = err_b$`Mean Sq`, alpha = alpha)
  
  # 5. GRÁFICOS (Reutilizando tu lógica de visualización)
  # [Aquí puedes insertar las funciones de generación de plot de tu código original 
  # usando los objetos tukey_a, tukey_b y tukey_int creados arriba]
}



######################## Prueba de contrastes ortogonales para percelas divididas ####################


library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)

variables_y   <- c("tch") 
factor_A      <- "coctel"      
factor_B      <- "biosustrato" 
bloque        <- "Replica"
alfa          <- 0.15 

# Seguir el orden de los tratamientos 
levels(datos$coctel)                 # Ingresar los factores correctos
levels(datos$biosustrato)
# Contrastes Factor A
lista_contrastes_A <- list(
  "Bio_vs_Testigos"    = c(2, 2, 2, -3, -3),
  "Inorg_vs_Absoluto"  = c(0, 0, 0, -1, 1),
  "Mezclas_vs_Estres"  = c(1, 1, -2, 0, 0),
  "Lab_vs_Campo"       = c(1, -1, 0, 0, 0)
)

# Contrastes Factor B
lista_contrastes_B <- list(
  "Bio_vs_Control"    = c(-2, 1, 1), 
  "Dosis3_vs_Dosis10" = c(0, -1, 1)
)


for (v in variables_y) {
  cat("\n>>> VARIABLE:", v, "\n")
  
  # Modelo Mixto
  f_mod <- as.formula(paste(v, "~", factor_A, "*", factor_B, "+ (1|", bloque, "/", factor_A, ")"))
  modelo <- lmer(f_mod, data = datos)
  
  cat("- Tabla ANOVA:\n")
  anova_res <- anova(modelo)
  print(anova_res)
  
  cat("\n- Contrastes", factor_A, ":\n")
  emm_a <- emmeans(modelo, as.formula(paste("~", factor_A)))
  print(test(contrast(emm_a, method = lista_contrastes_A), level = 1 - alfa))
  
  cat("\n- Contrastes", factor_B, ":\n")
  emm_b <- emmeans(modelo, as.formula(paste("~", factor_B)))
  print(test(contrast(emm_b, method = lista_contrastes_B), level = 1 - alfa))
  
  # Lógica de Interacción
  p_int <- anova_res[paste0(factor_A, ":", factor_B), "Pr(>F)"]
  
  if (!is.na(p_int) && p_int < alfa) {
    cat("\n- INTERACCIÓN SIGNIFICATIVA (p =", round(p_int, 4), "). Contrastes de B en cada A:\n")
    emm_int <- emmeans(modelo, as.formula(paste("~", factor_B, "|", factor_A)))
    print(test(contrast(emm_int, method = lista_contrastes_B), level = 1 - alfa))
    
    cat("\n- Clasificación por letras (Interacción):\n")
    print(cld(emm_int, alpha = alfa, Letters = letters, adjust = "tukey"))
  } else {
    cat("\n- Clasificación por letras (Tukey,", factor_B, "):\n")
    print(cld(emm_b, alpha = alfa, Letters = letters, adjust = "tukey"))
  }
  cat(paste(rep("-", 40), collapse = ""), "\n")
}






################# Análisis y gráficos M. PEC #####################################################

# --- Librerías ---
pacman::p_load(readxl, lme4, lmerTest, MASS, DHARMa, car, multcomp, hnp, dplyr, tidyr,
               ggplot2, extrafont, gridExtra, ggiraphExtra, tinytex, reshape2, ggpubr, 
               emmeans, lsmeans, multcompView, effects, vegan, tidyverse, knitr, 
               kableExtra, ggrepel, patchwork)


# --- 1. Configuración (Aquí eliges tus variables) ---


# "TCH", "TCH..relativo", "Rendimiento", "Rendimiento..relativo", "TAH", "TAH..relativo", "Pureza", "Pureza..relativa."

# "TCH", "TCH_Rel", "Rendimiento", "Rend_Rel", "TAH", "TAH_Rel","TAH_PRZ", "TAH_PRZ_Rel", "Pureza", "Pureza_Rel"

names(datos)
vars_analisis <- c("tch", "rendimiento", "pureza") # Puedes agregar más: c("brix", "tch", "tah")
f_principal   <- "Tratamiento"
f_bloque      <- "Replica" #Replica
alpha_val     <- 0.15

# --- 2. ANOVA y Post-hoc (Para obtener las letras) ---
lista_posthoc <- list()

for(v in vars_analisis) {
  formula_lm <- as.formula(paste0("`", v, "` ~ ", f_principal, " + ", f_bloque)) #agregar: " + ", f_bloque
  modelo <- lm(formula_lm, data = datos)
  
  # Obtener letras de significancia
  res_means <- emmeans(modelo, as.formula(paste("~", f_principal))) %>%
    multcomp::cld(Letters = letters, sort = TRUE, reverse = TRUE, alpha = alpha_val) %>%
    as.data.frame() %>%
    mutate(Variable = v)
  
  lista_posthoc[[v]] <- res_means
}

tabla_posthoc <- bind_rows(lista_posthoc)

# --- 3. Cálculo de Medias Reales ---
# Calculamos media y SE para CUALQUIER variable en vars_analisis
medias_reales <- datos %>%
  group_by(across(all_of(f_principal))) %>%
  summarise(across(all_of(vars_analisis), 
                   list(media_r = ~mean(.x, na.rm = TRUE),
                        se_r = ~sd(.x, na.rm = TRUE) / sqrt(n()))),
            .groups = "drop") %>%
  pivot_longer(cols = -!!sym(f_principal), 
               names_to = c("Variable", ".value"), 
               names_pattern = "(.*)_(media_r|se_r)")

# --- 4. Unión de datos ---
# Aquí conectamos las letras del modelo con las medias reales
datos_final_grafico <- tabla_posthoc %>%
  select(!!sym(f_principal), Variable, .group) %>%
  left_join(medias_reales, by = c(f_principal, "Variable"))

# --- 5. Gráfico Combinado con Estética Unificada ---
grupos_graficos <- split(vars_analisis, ceiling(seq_along(vars_analisis) / 4))

for (i in seq_along(grupos_graficos)) {
  
  datos_plot <- datos_final_grafico %>%
    filter(Variable %in% grupos_graficos[[i]]) %>%
    mutate(Variable = factor(Variable, levels = grupos_graficos[[i]]))
  
  if(nrow(datos_plot) == 0) next
  
  gg <- ggplot(datos_plot, aes(y = reorder(!!sym(f_principal), media_r), x = media_r)) +
    # Estética unificada
    geom_errorbarh(aes(xmin = media_r - se_r, xmax = media_r + se_r), height = 0.2) +
    geom_point(size = 3, color = "black") +
    geom_text(aes(label = sprintf("%.2f %s", media_r, trimws(.group))),
              hjust = -0.1, vjust = -1, size = 4.5, family = "Arial") +
    
    facet_wrap(~ Variable, scales = "free_x") + 
    scale_x_continuous(expand = expansion(mult = c(0.1, 0.5))) +
    
    labs(x = "Media Aritmética Real ± SE", 
         y = "Tratamiento",
         title = paste("Comparación de Medias Reales - Panel", i)) +
    
    theme_minimal(base_family = "Arial") +
    theme(strip.text = element_text(size = 13, face = "bold"),
          text = element_text(size = 12)) +
    theme_classic2()
  
  print(gg)
}


################### Scott Knott a mano

pacman::p_load(tidyverse, ggpubr)

# 1. DICCIONARIO DE LETRAS (El orden aquí define el orden de los gráficos)
mapping_letras <- list(
  tch         = c("a", "b", "b", "c"),
  rendimiento = c("b", "a", "a", "a"),
  tah         = c("b", "a", "a", "b"),
  pureza      = c("b", "a", "a", "a")
)

# 2. AUTOMATIZACIÓN DE TABLAS
dosis_niveles <- c("0_%", "50_%", "75_%", "100_%") 

letras_df <- enframe(mapping_letras, name = "Variable", value = "Letra") %>%
  unnest(Letra) %>%
  group_by(Variable) %>%
  mutate(dosis = dosis_niveles) %>% 
  ungroup()

# 3. PROCESAMIENTO
medias_reales <- datos %>%
  drop_na(all_of(names(mapping_letras))) %>%
  group_by(dosis) %>%
  summarise(across(all_of(names(mapping_letras)), 
                   list(media = ~mean(.x, na.rm = TRUE),
                        se = ~sd(.x, na.rm = TRUE) / sqrt(n()))),
            .groups = "drop") %>%
  pivot_longer(cols = -dosis, 
               names_to = c("Variable", ".value"), 
               names_pattern = "(.*)_(media|se)") %>%
  left_join(letras_df, by = c("dosis", "Variable")) %>%
  # Ordenar facetas según la lista original
  mutate(Variable = factor(Variable, levels = names(mapping_letras)))

# 4. GRÁFICO FINAL
ggplot(medias_reales, aes(y = reorder(dosis, media), x = media)) +
  
  geom_errorbarh(aes(xmin = media - se, xmax = media + se), 
                 height = 0.2, color = "black") +
  
  geom_point(size = 3.5, color = "black") +
  
  # Etiqueta combinada: Media + Letra (Sin negritas)
  geom_text(aes(label = paste(sprintf("%.2f", media), Letra)), 
            hjust = -0.1,    
            vjust = -1.3,    
            size = 4,      # Tamaño sutilmente ajustado
            family = "Arial") + # Se eliminó fontface = "bold"
  
  facet_wrap(~ Variable, scales = "free_x") + 
  
  # Espacio lateral para evitar cortes
  scale_x_continuous(expand = expansion(mult = c(0.1, 0.7))) +
  
  theme_classic(base_family = "Arial") +
  theme(strip.text = element_text(size = 13, face = "bold"), # Mantiene negrita solo en título de panel
        text = element_text(size = 12),
        axis.text = element_text(color = "black"),
        panel.spacing = unit(2, "lines")) + 
  
  labs(x = "Media Aritmética Real ± SE", 
       y = "Dosis", 
       title = "Comparación de Medias Reales (Letras: Scott-Knott)")


####################################

#--- Fisher LSD (Ajustado para Medias Reales) ---

lista_posthoc <- list()

for(v in vars_analisis) {
  message(paste("Procesando:", v))
  
  datos_temp <- datos %>% filter(!is.na(!!sym(v)))
  
  # 1. Calculamos las MEDIAS REALES (Aritméticas)
  real_means <- datos_temp %>%
    group_by(!!sym(f_principal)) %>%
    summarise(mean_real = mean(!!sym(v), na.rm = TRUE), .groups = "drop")
  
  # 2. Modelo y ANOVA
  formula_lm <- as.formula(paste(v, "~", f_principal, "+", f_bloque))
  modelo <- lm(formula_lm, data = datos_temp)
  
  print(kable(Anova(modelo, type = 2), caption = paste("ANOVA para", v)) %>% 
          kable_styling(bootstrap_options = c("striped", "hover")))
  
  # 3. Post-hoc: FISHER LSD (Obtenemos las letras)
  res_means <- emmeans(modelo, as.formula(paste("~", f_principal))) %>%
    multcomp::cld(Letters = letters, 
                  sort = TRUE, 
                  reverse = TRUE, 
                  alpha = 0.15, 
                  adjust = "none") %>% 
    as.data.frame() %>%
    mutate(Variable = v)
  
  # 4. UNIR MEDIAS REALES a los resultados del Post-hoc
  res_means <- res_means %>%
    left_join(real_means, by = f_principal) %>%
    mutate(.group = trimws(.group))
  
  lista_posthoc[[v]] <- res_means
}

tabla_completa <- bind_rows(lista_posthoc)

# --- Graficación con Medias Reales ---

for (i in seq_along(grupos_graficos)) {
  
  datos_plot <- tabla_completa %>%
    filter(Variable %in% grupos_graficos[[i]]) %>%
    mutate(Variable = factor(Variable, levels = grupos_graficos[[i]]))
  
  # Usamos mean_real en lugar de emmean para el eje X y las etiquetas
  gg <- ggplot(datos_plot, aes(y = reorder(!!sym(f_principal), mean_real), x = mean_real)) +
    
    # 1. Barras de Error (Mantenemos SE del modelo, es lo correcto estadísticamente)
    geom_errorbarh(aes(xmin = mean_real - SE, xmax = mean_real + SE), height = 0.2) +
    
    # 2. Puntos (Media Real)
    geom_point(size = 3, color = "black") +
    
    # 3. Etiquetas (Media Real + Letras)
    geom_text(aes(label = sprintf("%.2f %s", mean_real, .group)),
              hjust = -0.1, 
              vjust = -1, 
              size = 4.5, 
              family = "Arial") +
    
    facet_wrap(~ Variable, scales = "free_x", ncol = 2) + 
    scale_x_continuous(expand = expansion(mult = c(0.1, 0.5))) + 
    labs(x = "Media aritmética real ± SE (modelo)", 
         y = "Tratamiento",
         title = "Prueba de Fisher LSD - Comparación de Medias Reales",
         subtitle = "Letras de significancia basadas en modelo lineal (alpha = 0.15)") +
    
    theme_minimal(base_family = "Arial") +
    theme(strip.text = element_text(size = 13),
          text = element_text(size = 12)) +
    ggpubr::theme_classic2() # Aseguré el llamado a la librería
  
  print(gg)
  
  ggsave(paste0("Panel_LSD_Real_Means_", i, ".png"), gg, 
         width = 400, height = 200, dpi = 900, units = "mm")
}



################### Desdoblamientos y contrastes #######################################################
library(car)      
library(emmeans)  
library(agricolae) 
library(dplyr)
library(ggplot2)    
library(patchwork)  

colnames(datos)

VARIABLES_RESPUESTA <- c("altura_m", "tallos_ha")

FACTORES_PRINCIPALES <- c("coctel", "biosustrato") 

ALPHA_TUKEY <- 0.15 
DATOS_FUENTE <- datos


NUM_VARIABLES <- length(VARIABLES_RESPUESTA)

FACTORES_INTERACCION <- rep(list(FACTORES_PRINCIPALES), NUM_VARIABLES)


ejecutar_analisis_robusto <- function(y_var, factor_A, factor_B, data_obj, alpha_val) {
  
  data_temp <- data_obj
  
  data_temp[[factor_A]] <- as.factor(data_temp[[factor_A]])
  data_temp[[factor_B]] <- as.factor(data_temp[[factor_B]])
  
  
  cat("\n======================================================\n")
  cat(paste("ANALIZANDO:", y_var, "vs", factor_A, "*", factor_B, "\n"))
  cat("======================================================\n")
  
  formula_interaccion <- as.formula(paste(y_var, "~", factor_A, "*", factor_B))
  m0 <- lm(formula_interaccion, data = data_temp)
  
  cat("\n--- Medias Ajustadas (emmeans) y Desdoblamiento ---\n")

  emm_AB <- emmeans(m0, as.formula(paste("~", factor_A, "|", factor_B))) 
  desdoblamiento_cld <- cld(emm_AB, type="response", 
                            alpha = alpha_val, Letters = letters, 
                            adjust = "tukey", reversed=T)
  
  print(desdoblamiento_cld)
  
  cat("\n--- Contraste Tukey HSD para Factores Simples ---\n")
  
  formula_A <- as.formula(paste(y_var, "~", factor_A))
  modelo_A <- aov(formula_A, data = data_temp)
  
  cat(paste("\nResultado de Tukey HSD para:", factor_A, "\n"))
  tryCatch(
    print(TukeyHSD(modelo_A, conf.level = 1 - alpha_val)),
    error = function(e) {
      cat(paste("ERROR al calcular Tukey HSD para", factor_A, ":", conditionMessage(e), "\n"))
    }
  )
  
  formula_B <- as.formula(paste(y_var, "~", factor_B))
  modelo_B <- aov(formula_B, data = data_temp)
  
  cat(paste("\nResultado de Tukey HSD para:", factor_B, "\n"))
  tryCatch(
    print(TukeyHSD(modelo_B, conf.level = 1 - alpha_val)),
    error = function(e) {
      cat(paste("ERROR al calcular Tukey HSD para", factor_B, ":", conditionMessage(e), "\n"))
    }
  )
  
  plot_efecto <- function(f_nom) {
    res_tk <- HSD.test(aov(as.formula(paste(y_var, "~", f_nom)), data = data_temp), f_nom, alpha = alpha_val)$groups
    res_tk[[f_nom]] <- rownames(res_tk)
    
    df_p <- data_temp %>%
      group_by(.data[[f_nom]]) %>%
      summarise(m = mean(.data[[y_var]], na.rm = TRUE),
                se = sd(.data[[y_var]], na.rm = TRUE) / sqrt(n()), .groups = "drop") %>%
      left_join(res_tk, by = f_nom)
    
    ggplot(df_p, aes(y = reorder(.data[[f_nom]], m), x = m)) +
      geom_errorbarh(aes(xmin = m - se, xmax = m + se), height = 0.2) +
      geom_point(size = 3) +
      geom_text(aes(label = sprintf("%.2f %s", m, trimws(groups))), hjust = -0.2, vjust = -1, size = 4) +
      scale_x_continuous(expand = expansion(mult = c(0.1, 0.4))) +
      labs(title = paste("Efecto:", f_nom), x = "Media ± SE", y = NULL) +
      theme_classic()
  }
  
  # Gráfico Anidado (Factor A dentro de Factor B)
  df_ani <- as.data.frame(desdoblamiento_cld)
  
  p_ani <- ggplot(df_ani, aes(y = reorder(.data[[factor_A]], emmean), x = emmean)) +
    geom_errorbarh(aes(xmin = emmean - SE, xmax = emmean + SE), height = 0.2) +
    geom_point(size = 3) +
    geom_text(aes(label = sprintf("%.2f %s", emmean, trimws(.group))), hjust = -0.2, vjust = -1, size = 4) +
    facet_wrap(as.formula(paste("~", factor_B)), scales = "free_x") +
    scale_x_continuous(expand = expansion(mult = c(0.1, 0.4))) +
    labs(title = paste("Anidamiento:", y_var), subtitle = paste(factor_A, "dentro de", factor_B), x = "Media ± SE", y = NULL) +
    theme_classic() +
    theme(strip.text = element_text(face = "bold"))
  
  # Mostrar Panel
  p1 <- plot_efecto(factor_A)
  p2 <- plot_efecto(factor_B)
  print((p1 | p2) / p_ani)
}


if (length(FACTORES_PRINCIPALES) != 2) {
  stop("ERROR: Por favor, defina exactamente dos factores en FACTORES_PRINCIPALES.")
}

for (i in 1:NUM_VARIABLES) {
  
  y <- VARIABLES_RESPUESTA[i]
  factores <- FACTORES_INTERACCION[[i]]
  f1 <- factores[1]
  f2 <- factores[2]
  
  ejecutar_analisis_robusto(y_var = y, 
                            factor_A = f1, 
                            factor_B = f2, 
                            data_obj = DATOS_FUENTE, 
                            alpha_val = ALPHA_TUKEY)
}


############################### Aporte de variabilidad ################################

library(ggplot2)
library(dplyr)
library(tidyr)

variables_num <- c("tch", "rendimiento", "tah", "pureza")
mi_formula_base <- "Tratamiento" 

df_resultados_global <- data.frame()
df_desglose_indiv <- data.frame()

for (var in variables_num) {
  formula_full <- as.formula(paste(var, "~", mi_formula_base))
  fit <- aov(formula_full, data = datos)
  resumen <- summary(fit)[[1]]
  
  tabla_anova <- as.data.frame(resumen)
  tabla_anova$Fuente <- trimws(rownames(tabla_anova))
  tabla_anova$Fuente[tabla_anova$Fuente == "Residuals"] <- "Error Exp."
  
  ss_total <- sum(tabla_anova$`Sum Sq`)
  tabla_anova$Contribucion_SS <- (tabla_anova$`Sum Sq` / ss_total) * 100
  tabla_anova$Variable <- var
  
  df_resultados_global <- rbind(df_resultados_global, 
                                tabla_anova %>% select(Variable, Fuente, CM = `Mean Sq`, Contribucion_SS))
  
  media_global <- mean(datos[[var]], na.rm = TRUE)
  df_trata_names <- as.character(unique(datos[[mi_formula_base]]))
  
  desglose_temp <- datos %>%
    group_by(Nivel = get(mi_formula_base)) %>%
    summarise(
      Varianza_CM = (mean(get(var), na.rm = TRUE) - media_global)^2 / (length(df_trata_names) - 1)
    ) %>%
    mutate(Variable = var)
  
  error_cm <- data.frame(Nivel = "Error Exp.", 
                         Varianza_CM = tabla_anova$`Mean Sq`[tabla_anova$Fuente == "Error Exp."], 
                         Variable = var)
  
  df_desglose_indiv <- rbind(df_desglose_indiv, desglose_temp, error_cm)
}

ggplot(df_resultados_global, aes(x = Variable, y = Contribucion_SS, fill = Fuente)) +
  geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.2) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title = "1. Contribución a la Variabilidad Total (SS%)", y = "% Suma de Cuadrados") +
  geom_text(aes(label = ifelse(Contribucion_SS > 3, paste0(round(Contribucion_SS, 1), "%"), "")), 
            position = position_stack(vjust = 0.5), size = 3, color = "white")

ggplot(df_resultados_global, aes(x = Fuente, y = CM, fill = Fuente)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Variable, scales = "free_y") +
  theme_bw() +
  labs(title = "2. Comparación de Magnitud de Varianza (CM)", y = "Cuadrado Medio") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

df_desglose_indiv <- df_desglose_indiv %>%
  group_by(Variable) %>%
  mutate(Porcentaje = (Varianza_CM / sum(Varianza_CM)) * 100)

ggplot(df_desglose_indiv, aes(x = Variable, y = Porcentaje, fill = Nivel)) +
  geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.2) +
  scale_fill_viridis_d(option = "plasma") +
  theme_minimal() +
  labs(title = "3. Desglose de Varianza: Niveles vs Error", y = "Proporción de Varianza (%)") +
  geom_text(aes(label = ifelse(Porcentaje > 3, paste0(round(Porcentaje, 1), "%"), "")), 
            position = position_stack(vjust = 0.5), size = 3, color = "red")

ggplot(df_desglose_indiv, aes(x = Nivel, y = Varianza_CM, fill = Nivel)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Variable, scales = "free_y") +
  theme_light() +
  labs(title = "4. Magnitud de Varianza por Nivel", y = "Varianza (CM)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")


###############################--Regresión lineal simple--#########################################

names(datos)
modelo1<-lm(tch ~ velocidad, data=datos)
summary(modelo1)
print(modelo1)
#Modelo: y=Bo+B1x-----y=7.357+0.489x
plot(velocidad, tch, main="Gráfico de dispersion (TCH vs velocidad)", xlab="Velocidad (km/h)", ylab="TCH", pch=19, col="blue")
abline(modelo1, col="red", linetype="solid", lwd=2)




# En bucle

respuestas_reg <- c("tch")
f_reg_x <- "fertilidad"
x_lims <- c(40, 110) 

for (var in respuestas_reg) {
  if (!var %in% colnames(datos)) next
  
  # 2. LIMPIEZA DE DATOS
  df_temp <- datos %>%
    mutate(
      x_num = as.numeric(gsub("[^0-9.]", "", as.character(get(f_reg_x)))),
      y_clean = gsub(",", ".", as.character(.data[[var]])),
      y_num = as.numeric(gsub("[^0-9.]", "", y_clean))
    ) %>%
    filter(!is.na(x_num), !is.na(y_num))
  
  # 3. MODELO Y ESTADÍSTICOS
  modelo <- lm(y_num ~ x_num, data = df_temp)
  aic_val <- round(AIC(modelo), 2)
  bic_val <- round(BIC(modelo), 2)
  r2_val  <- round(summary(modelo)$r.squared, 3)
  p_val   <- summary(modelo)$coefficients[2,4]
  
  # Impresión en consola para diagnóstico
  cat("\nVariable:", toupper(var), "| AIC:", aic_val, "| BIC:", bic_val, "| R2:", r2_val, "\n")
  
  # 4. CÁLCULO DE BANDAS (Suavizado)
  x_range <- seq(x_lims[1], x_lims[2], length.out = 200)
  preds <- data.frame(x_num = x_range)
  
  conf <- predict(modelo, newdata = preds, interval = "confidence")
  pred <- predict(modelo, newdata = preds, interval = "prediction")
  df_preds <- cbind(preds, as.data.frame(conf), as.data.frame(pred[,-1]))
  colnames(df_preds) <- c("x_num", "fit", "lwr_conf", "upr_conf", "lwr_pred", "upr_pred")
  
  # 5. GRÁFICO PROFESIONAL
  g <- ggplot(df_temp, aes(x = x_num, y = y_num)) +
    # Banda de Predicción (Área de incertidumbre de datos individuales)
    geom_ribbon(data = df_preds, aes(y = fit, ymin = lwr_pred, ymax = upr_pred), 
                fill = "grey92", alpha = 0.5) +
    # Banda de Confianza (Precisión de la recta de regresión)
    geom_ribbon(data = df_preds, aes(y = fit, ymin = lwr_conf, ymax = upr_conf), 
                fill = "steelblue", alpha = 0.2) +
    # Línea de tendencia elegante
    geom_line(data = df_preds, aes(y = fit), color = "#2C3E50", linewidth = 1.1) +
    # Puntos con borde (Estética clásica)
    geom_point(size = 3, color = "white", fill = "#2C3E50", shape = 21, stroke = 1) +
    
    # Anotaciones matemáticas con mejor formato
    annotate("label", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2,
             label = paste0("Model: y = ", round(coef(modelo)[1],2), " + ", round(coef(modelo)[2],2), "x",
                            "\nR² = ", r2_val, " (p < ", format.pval(p_val, digits = 2), ")",
                            "\nAIC = ", aic_val),
             family = "Arial", size = 3.5, fill = "white", label.size = NA, alpha = 0.8) +
    
    # Escalas y visualización
    scale_x_continuous(breaks = c(50, 75, 100)) +
    scale_y_continuous(n.breaks = 6) +
    coord_cartesian(xlim = x_lims) +
    
    labs(title = paste("Análisis de Tendencia Lineal:", toupper(var)),
         subtitle = "Diseño con intervalos de confianza (95%) y predicción",
         x = "Dosis (%)", 
         y = paste("Respuesta:", var)) +
    
    # Tema profesional
    theme_pubr() + # Basado en ggpubr para estilo de revista científica
    theme(
      aspect.ratio = 3/4, # Estira el gráfico para que la pendiente sea más clara
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, color = "grey30"),
      axis.title = element_text(face = "bold"),
      panel.grid.major.y = element_line(color = "grey95"), # Líneas horizontales sutiles
      axis.line = element_line(color = "black")
    )
  
  print(g)
}


############################--Regresión lineal múltiple--#######################################
datos$Replica<- as.numeric(datos$Replica)
class(datos$Replica)

names(datos)
modelo3<-lm(tch ~ velocidad + Replica)
summary(modelo3)
print(modelo3)

names(datos)
modelo4<-lm(rendimiento_kg_ton ~ velocidad + Replica)
summary(modelo3)
print(modelo3)

names(datos)
modelo5<-lm(tah ~ velocidad + Replica)
summary(modelo3)
print(modelo3)

names(datos)
modelo6<-lm(pureza ~ velocidad + Replica)
summary(modelo3)
print(modelo3)


############# En bucle

datos$Replica <- as.numeric(datos$Replica)

resp_multiple <- "tch"
modelos_a_probar <- list(
  modelo1 = c("velocidad"),
  modelo2 = c("velocidad", "Replica") 
)


for (i in seq_along(modelos_a_probar)) {
  vars_x <- modelos_a_probar[[i]]
  
  formula_m <- as.formula(paste(resp_multiple, "~", paste(vars_x, collapse = " + ")))
  modelo_m <- lm(formula_m, data = datos, na.action = na.exclude)
  
 
  coefs <- round(coef(modelo_m), 3)
  intercepto <- coefs[1]
  

  ecuacion_texto <- paste0("italic(Y) == ", intercepto)
  
  for (j in 2:length(coefs)) {
    valor <- coefs[j]
    nombre_var <- names(coefs)[j]

    signo <- if(valor >= 0) " + " else " " 
    ecuacion_texto <- paste0(ecuacion_texto, signo, valor, "*italic(", nombre_var, ")")
  }
  
  datos_grafico <- data.frame(
    Observado = datos[[resp_multiple]],
    Predicho = predict(modelo_m, datos)
  )
  
  r2_val <- round(summary(modelo_m)$adj.r.squared, 3)
  p_val <- format.pval(summary(modelo_m)$coefficients[2,4], digits = 2)
  
  p_m <- ggplot(datos_grafico, aes(x = Predicho, y = Observado)) +
    geom_point(color = "black", fill = "gray70", shape = 21, size = 3, alpha = 0.8) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", size = 0.7) +
    geom_smooth(method = "lm", color = "firebrick", fill = "firebrick", alpha = 0.1, size = 1) +
    labs(
      title = paste("Validación del Modelo:", names(modelos_a_probar)[i]),
      subtitle = bquote(R[adj]^2 == .(r2_val) ~~~ p < .(p_val)),
      x = "Predichos",
      y = "Valores observados"
    ) +
    theme_bw(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      axis.title = element_text(face = "bold")
    ) +
  
    annotate("text", 
             x = -Inf, y = Inf, # Esquina superior izquierda
             label = ecuacion_texto, 
             parse = TRUE, 
             hjust = -0.1, vjust = 1.5, 
             size = 4, color = "darkblue")
  
  print(p_m)
}


# Gráficos de dispersión por variable regresora

for (i in seq_along(modelos_a_probar)) {
  vars_x <- modelos_a_probar[[i]]
  
  for (x_var in vars_x) {

    fit <- lm(as.formula(paste(resp_multiple, "~", x_var)), data = datos)
    r2_simple <- round(summary(fit)$r.squared, 3)
    
    p_disp <- ggplot(datos, aes_string(x = x_var, y = resp_multiple)) +
      geom_point(color = "black", fill = "steelblue", shape = 21, size = 3, alpha = 0.6) +
      geom_smooth(method = "lm", color = "darkred", fill = "gray80", size = 1) +
      labs(
        title = paste("Relación Individual:", x_var, "vs", resp_multiple),
        subtitle = bquote(R^2 == .(r2_simple)),
        x = paste(x_var, "(Km/h)"),
        y = paste(resp_multiple)
      ) +
      theme_bw(base_size = 12) +
      theme(panel.grid.minor = element_blank())
    
    print(p_disp)
  }
}






library(ggplot2)

# Asegurar que Replica sea numérica
datos$Replica <- as.numeric(as.character(datos$Replica))

resp_multiple <- "tch"
modelos_a_probar <- list(
  modelo1 = c("velocidad"),
  modelo2 = c("velocidad", "Replica") 
)

# 1. BUCLE DE VALIDACIÓN (Observado vs Predicho)
# ---------------------------------------------------------
for (i in seq_along(modelos_a_probar)) {
  vars_x <- modelos_a_probar[[i]]
  formula_m <- as.formula(paste(resp_multiple, "~", paste(vars_x, collapse = " + ")))
  modelo_m <- lm(formula_m, data = datos, na.action = na.exclude)
  
  coefs <- round(coef(modelo_m), 3)
  ecuacion_texto <- paste0("italic(Y) == ", coefs[1])
  for (j in 2:length(coefs)) {
    valor <- coefs[j]; nombre_var <- names(coefs)[j]
    signo <- if(valor >= 0) " + " else " " 
    ecuacion_texto <- paste0(ecuacion_texto, signo, valor, "*italic(", nombre_var, ")")
  }
  
  datos_grafico <- data.frame(
    Observado = datos[[resp_multiple]],
    Predicho = predict(modelo_m, datos)
  )
  
  r2_val <- round(summary(modelo_m)$adj.r.squared, 3)
  p_val <- format.pval(summary(modelo_m)$coefficients[2,4], digits = 2)
  
  p_m <- ggplot(datos_grafico, aes(x = Predicho, y = Observado)) +
    geom_point(color = "black", fill = "gray70", shape = 21, size = 3, alpha = 0.8) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
    geom_smooth(method = "lm", color = "firebrick", fill = "firebrick", alpha = 0.1) +
    # ASPECT.RATIO = 1 hace que el gráfico sea cuadrado sin importar las unidades
    theme_bw(base_size = 14) +
    theme(aspect.ratio = 1, plot.title = element_text(face = "bold")) +
    labs(
      title = paste("Validación del Modelo:", names(modelos_a_probar)[i]),
      subtitle = bquote(R[adj]^2 == .(r2_val) ~~~ p < .(p_val)),
      x = "Valores Predichos", y = "Valores Observados"
    ) +
    annotate("text", x = -Inf, y = Inf, label = ecuacion_texto, parse = TRUE, 
             hjust = -0.05, vjust = 1.5, size = 4, color = "darkblue")
  
  print(p_m)
}

# 2. BUCLE DE DISPERSIÓN INDIVIDUAL (X vs Y)
# ---------------------------------------------------------
for (i in seq_along(modelos_a_probar)) {
  vars_x <- modelos_a_probar[[i]]
  
  for (x_var in vars_x) {
    fit_s <- lm(as.formula(paste(resp_multiple, "~", x_var)), data = datos, na.action = na.exclude)
    cf <- round(coef(fit_s), 3); sgn <- if(cf[2] >= 0) " + " else " "
    ecuacion_s <- paste0("italic(Y) == ", cf[1], sgn, cf[2], "*italic(", x_var, ")")
    r2_simple <- round(summary(fit_s)$r.squared, 3)
    
    p_disp <- ggplot(datos, aes_string(x = x_var, y = resp_multiple)) +
      geom_jitter(width = 0.05, color = "black", fill = "steelblue", shape = 21, size = 3, alpha = 0.6) +
      geom_smooth(method = "lm", color = "darkred", fill = "gray80") +
      # Aquí también forzamos un marco cuadrado para que la línea se vea inclinada
      theme_bw(base_size = 12) +
      theme(aspect.ratio = 1, panel.grid.minor = element_blank()) +
      labs(
        title = paste("Relación:", x_var, "vs", resp_multiple),
        subtitle = bquote(R^2 == .(r2_simple)),
        x = paste(x_var, "(km/h)"), y = resp_multiple
      ) +
      annotate("text", x = -Inf, y = Inf, label = ecuacion_s, parse = TRUE, 
               hjust = -0.05, vjust = 1.5, size = 4, color = "darkred")
    
    print(p_disp)
  }
}


###############################--MLM--###############################################################
colnames(datos)

respuestas <- c("TCH")  # agregar todas tus variables

#-----------------------------
# Definir efectos
#-----------------------------
efectos_fijos <- c("Tratamiento")
efectos_aleatorios <- c("(1 | Replica)")

# Convertir factores
factores <- c("Tratamiento", "Replica")
for(f in factores){
  if(f %in% colnames(datos)) datos[[f]] <- factor(datos[[f]])
}

#-----------------------------
# Bucle completo para todas las variables
#-----------------------------
for(var in respuestas){
  cat("\n====================================\n")
  cat("Variable:", var, "\n")
  cat("====================================\n")
  
  # Construir fórmula
  fixed_str <- paste(efectos_fijos, collapse = " + ")
  random_str <- paste(efectos_aleatorios, collapse = " + ")
  formula <- as.formula(paste0(var, " ~ ", fixed_str, " + ", random_str))
  
  # Ajustar modelo
  modelo <- lmer(formula, data = datos, REML = TRUE)
  
  # Resultados
  print(summary(modelo))
  print(anova(modelo))
  
  #-----------------------------
  # Verificación de supuestos
  #-----------------------------
  
  # Simulación de residuos DHARMa
  sim_res <- simulateResiduals(modelo)
  
  # Boxplot por factor, evitando problema de longitud
  trat <- model.frame(modelo)$Tratamiento
  plotResiduals(sim_res, form = trat)
  
  # Gráficos generales de DHARMa
  plot(sim_res)
  
  # Shapiro-Wilk
  cat("\nShapiro-Wilk de residuos simulados:\n")
  print(shapiro.test(residuals(sim_res)))
  
  # Residuos vs ajustados
  fitted_vals <- fitted(modelo)
  resids <- resid(modelo)
  p1 <- ggplot(data.frame(Fitted=fitted_vals, Resid=resids), aes(x=Fitted, y=Resid)) +
    geom_point() + 
    geom_hline(yintercept=0, linetype="dashed", color="red") +
    labs(title=paste("Residuos vs Ajustados:", var))
  print(p1)
  
  # Residuos vs orden
  p2 <- ggplot(data.frame(Order=1:length(resids), Resid=resids), aes(x=Order, y=Resid)) +
    geom_point() + 
    geom_hline(yintercept=0, linetype="dashed", color="red") +
    labs(title=paste("Residuos vs Orden:", var))
  print(p2)
  
  # ACF de residuos
  cat("\nACF de residuos:\n")
  acf(resids, main=paste("ACF de residuos:", var))
}



# Prueba de Tukey
respuestas <- c("infestacion", "intensidad_infestacion")
nivel_significancia <- 0.15  # 15%

#-----------------------------
# Tukey por pares con nivel personalizado
#-----------------------------
cat("\n=== Tukey HSD (α =", nivel_significancia, ") ===\n")

for(var in respuestas){
  cat("\n", strrep("=", 50))
  cat("\nVariable:", var, "\n")
  cat(strrep("=", 50), "\n")
  
  modelo <- lmer(paste(var, "~ Tratamiento + (1 | Replica)"), data = datos)
  
  # Tukey con nivel personalizado
  emm <- emmeans(modelo, ~ Tratamiento)
  contrastes <- pairs(emm, adjust = "tukey")
  
  cat("Comparaciones por pares (Tukey):\n")
  print(summary(contrastes, level = 1 - nivel_significancia))
}

#-----------------------------
# Gráficos con letras de Tukey (15%)
#-----------------------------
for(var in respuestas){
  cat("\n", strrep("=", 50))
  cat("\nVariable:", var, "\n")
  cat(strrep("=", 50), "\n")
  
  modelo <- lmer(paste(var, "~ Tratamiento + (1 | Replica)"), data = datos)
  
  # Letras de Tukey con alpha = 0.15
  letras <- cld(emmeans(modelo, ~ Tratamiento), 
                Letters = letters, 
                adjust = "tukey", 
                alpha = nivel_significancia)
  
  resumen <- as.data.frame(letras)
  resumen <- resumen[order(-resumen$emmean), ]
  
  print(resumen)
  
  # Gráfico principal
  p <- ggplot(resumen, aes(x = reorder(Tratamiento, -emmean), y = emmean)) +
    geom_col(fill = "lightblue", alpha = 0.7) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), 
                  width = 0.2, color = "black", linewidth = 0.8) +
    geom_text(aes(label = .group, y = upper.CL * 1.08), 
              size = 5, fontface = "bold") +
    labs(title = paste(var, "- α =", nivel_significancia),
         x = "Tratamiento",
         y = "Media ajustada",
         caption = paste("Letras diferentes: p <", nivel_significancia)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}

#-----------------------------
# Versión con puntos (alternativa)
#-----------------------------
for(var in respuestas){
  modelo <- lmer(paste(var, "~ Tratamiento + (1 | Replica)"), data = datos)
  
  letras <- cld(emmeans(modelo, ~ Tratamiento), 
                Letters = letters, 
                adjust = "tukey", 
                alpha = nivel_significancia)
  
  resumen <- as.data.frame(letras)
  resumen <- resumen[order(-resumen$emmean), ]
  
  p_puntos <- ggplot(resumen, aes(x = reorder(Tratamiento, -emmean), y = emmean)) +
    geom_point(size = 3, color = "darkblue") +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), 
                  width = 0.15, color = "red", linewidth = 1) +
    geom_text(aes(label = .group, y = upper.CL * 1.1), 
              size = 4, fontface = "bold", color = "darkgreen") +
    labs(title = paste(var, "- α =", nivel_significancia),
         x = "Tratamiento", 
         y = var) +
    theme_classic()
  
  print(p_puntos)
}



# Resultados en tablas


library(knitr); library(kableExtra); library(dplyr); library(emmeans); library(multcomp)

render_gris <- function(df, tit) {
  print(kable(df, digits = 4, format = "html", row.names = FALSE, 
              caption = paste("<b>", tit, "</b>")) %>%
          kable_styling(bootstrap_options = c("striped", "bordered", "condensed"), full_width = F) %>%
          row_spec(0, bold = TRUE, color = "white", background = "#666666"))
}

for(var in respuestas) {
  
  formula_lmm <- as.formula(paste0(var, " ~ Tratamiento + (1 | Replica)"))
  modelo <- lmer(formula_lmm, data = datos)
  
  df_anova <- as.data.frame(anova(modelo))
  df_anova <- cbind(Fuente_Variacion = rownames(df_anova), df_anova)
  render_gris(df_anova, paste("ANDEVA (Tipo III) - Variable:", var))
  
  emm <- emmeans(modelo, ~ Tratamiento)
  letras <- cld(emm, Letters = letters, adjust = "tukey", alpha = nivel_significancia)
  
  resumen_medias <- as.data.frame(letras) %>%
    dplyr::select(Tratamiento, emmean, SE, lower.CL, upper.CL, .group) %>%
    rename(Media_Ajustada = emmean, Letras = .group) %>%
    arrange(desc(Media_Ajustada))
  
  render_gris(resumen_medias, paste("PRUEBA DE MEDIAS (Tukey α =", nivel_significancia, ") -", var))
  
  df_pairs <- as.data.frame(pairs(emm, adjust = "tukey"))
  render_gris(df_pairs, paste("COMPARACIONES POR PARES -", var))
  
  cat("<br><hr style='border: 1px solid #333;'><br>")
}

# --- DESACTIVAR FUNCIÓN DE TABLAS ---
rm(render_gris)



#####################################################################################################
##########################
######## Análisis de correlación 

colnames(datos)
vars_correlacion <- c("TCH", "Rendimiento", "TAH", "Pureza", "Población", "Tallos.con.Flor", "Porcentaje", "Altura.Total", 
                      "Peso.Total", "Altura.sin.Corcho", "Peso.sin.Corcho", "Entrenudos.totales.por.tallo", "Entrenudos.con.corcho.por.tallo",
                      "Grado.de.corcho") 

# Selección del método: "pearson", "spearman" o "kendall"
metodo <- "pearson"


# Filtrar solo las variables elegidas y eliminar NAs
df_sub <- datos %>% select(all_of(vars_correlacion)) %>% drop_na()

cat("\n--- MATRIZ DE CORRELACIÓN (", toupper(metodo), ") ---\n")

# A. Calcular Matriz de Correlación
matriz_cor <- cor(df_sub, method = metodo)
print(round(matriz_cor, 3))

# B. Calcular Matriz de Valores p (Significancia)
# Función interna para obtener p-values de toda la matriz
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

p_matriz <- cor.mtest(df_sub, method = metodo); p_matriz

# Correlograma
par(mfrow=c(1,1))

corrplot(matriz_cor, 
         method = "color",           # Círculos de color
         type = "upper",             # Solo la parte superior para evitar redundancia
         order = "hclust",           # Agrupa variables similares
         addCoef.col = "red",      # Muestra el coeficiente de correlación
         tl.col = NULL,           # Color de las etiquetas
         tl.srt = 45,                # Rotación de etiquetas
         p.mat = p_matriz,           # Matriz de significancia
         sig.level = 0.05,           # Nivel de corte (alpha)
         insig = NULL,        # Marca con asteriscos los significativos
         pch.col = NULL,            # Color de asteriscos
         col = COL2('RdBu', 10),     # Paleta Rojo (Negativo) a Azul (Positivo)
         title = paste("\nCorrelograma de", toupper(metodo)),
         mar = c(0,0,2,0))






#############################################-MLG-##################################################

############## Diagnóstico inicial:
names(datos)

check_overdispersion <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type = "pearson")
  dispersion <- sum(rp^2) / rdf
  return(dispersion)
}


is_count_variable <- function(x) {
  return(is.numeric(x) && all(x == floor(x), na.rm = TRUE) && all(x >= 0, na.rm = TRUE))
}


is_proportion_variable <- function(x) {
  return(is.numeric(x) &&
           all(x >= 0, na.rm = TRUE) &&
           all(x <= 1 | x <= 100, na.rm = TRUE))
}



diagnose_variables <- function(df) {
  
  for (var in names(df)) {
    x <- df[[var]]
    
 
    if (!is.numeric(x)) next
    
    cat("\n=============================\n")
    cat("Variable:", var, "\n")
    cat("=============================\n")
    
 
    
    if (is_count_variable(x)) {
      cat("Tipo de variable detectado: CONTEO\n")
      
      media <- mean(x, na.rm = TRUE)
      varianza <- var(x, na.rm = TRUE)
      
      cat("Media:", media, "\n")
      cat("Varianza:", varianza, "\n\n")
      

      if (varianza > media * 1.5) {
        cat("→ Evidencia de SOBREDISPERSIÓN (Var > Media).\n")
      } else {
        cat("→ No hay evidencia fuerte de sobredispersión por media-varianza.\n")
      }
      

      try({
        m_pois <- glm(x ~ 1, family = poisson)
        disp <- check_overdispersion(m_pois)
        cat("\nFactor de dispersión (Poisson):", disp, "\n")
        
        if (disp > 1.5) {
          cat("→ El modelo Poisson está sobredisperso.\n")
          cat("Sugerencia: Binomial Negativa o Quasi-Poisson.\n")
        } else {
          cat("→ Poisson parece adecuado.\n")
        }
      }, silent = TRUE)
      
      next
    }
    
    
    if (is_proportion_variable(x)) {
      cat("Tipo de variable detectado: PROPORCIÓN / PORCENTAJE\n")
      
  
      if (max(x, na.rm = TRUE) > 1) {
        xp <- x / 100
        cat("→ Detectado porcentaje. Convertido automáticamente a proporción 0–1.\n")
      } else {
        xp <- x
      }
      

      if (any(xp == 0 | xp == 1, na.rm = TRUE)) {
        cat("→ La variable contiene valores 0 o 1 exactos.\n")
        cat("Sugerencia: Modelo Beta inflado (Zero-One Inflated Beta) o Quasi-binomial.\n")
      } else {
        cat("→ No hay valores extremos exactos.\n")
        cat("Sugerencia: Beta Regression es adecuada.\n")
      }
      

      try({
        m_qb <- glm(xp ~ 1, family = quasibinomial)
        disp <- check_overdispersion(m_qb)
        
        cat("\nFactor de dispersión (Quasi-binomial):", disp, "\n")
        
        if (disp > 1.5) {
          cat("→ Sobredispersión importante.\n")
          cat("Recomendación: Quasi-binomial o Beta regression.\n")
        } else {
          cat("→ Variabilidad moderada, modelos binomiales pueden funcionar.\n")
        }
      }, silent = TRUE)
      
      next
    }
    
    
    cat("Tipo de variable: Continua (no conteo, no proporción)\n")
    cat("→ No se analiza distribución especial.\n")
  }
}


diagnose_variables(datos) # resultados de diagnóstico 


############# Variables de conteo ########################

# Distribución binomial negativa y quassi poisson

names(datos)
attach(datos)
view(datos)

factores <- c("Tratamiento", "Replica")


variables_respuesta <- c("Población")


terminos_modelo <- c("Tratamiento", "Replica")


familias_modelos <- c("binomial_negativa")  # binomial_negativa, quasipoisson


if (!require(MASS)) install.packages("MASS"); library(MASS)
if (!require(car)) install.packages("car"); library(car)
if (!require(dplyr)) install.packages("dplyr"); library(dplyr)
if (!require(purrr)) install.packages("purrr"); library(purrr)


crear_formula <- function(respuesta, terminos) {
  formula_str <- paste0(respuesta, " ~ ", paste(terminos, collapse = " + "))
  as.formula(formula_str)
}


ajustar_modelo <- function(formula, datos, familia) {
  tryCatch({
    switch(familia,
           "binomial_negativa" = glm.nb(formula, data = datos),
           "quasipoisson" = glm(formula, data = datos, family = quasipoisson()),
           stop("Familia no reconocida: ", familia)
    )
  }, error = function(e) {
    warning("Error ajustando modelo: ", e$message)
    return(NULL)
  })
}


extraer_anova <- function(modelo, familia) {
  if (is.null(modelo)) return(NULL)
  
  tryCatch({

    anova_result <- Anova(modelo, type = "II", test = "LR")
    anova_df <- as.data.frame(anova_result)
    anova_df$Termino <- rownames(anova_df)
    rownames(anova_df) <- NULL
    

    names(anova_df) <- c("LR_Chisq", "Df", "Pr_Chisq", "Termino")
    
    return(anova_df)
  }, error = function(e) {
    warning("Error en ANOVA: ", e$message)
    return(NULL)
  })
}


variables_existentes <- variables_respuesta[variables_respuesta %in% names(datos)]

if (length(variables_existentes) == 0) {
  stop("Error: Ninguna variable de respuesta existe en los datos")
}


resultados_mlg <- list()
resultados_anova <- list()


for (variable in variables_existentes) {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("Modelando variable:", variable, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
 
  formula_actual <- crear_formula(variable, terminos_modelo)
  cat("Fórmula:", deparse(formula_actual), "\n\n")
  

  for (familia in familias_modelos) {
    cat("--- Ajustando modelo", familia, "---\n")
    
    modelo <- ajustar_modelo(formula_actual, datos, familia)
    
    if (!is.null(modelo)) {
   
      nombre_modelo <- paste0(variable, "_", familia)
      resultados_mlg[[nombre_modelo]] <- modelo
      
 
      anova_result <- extraer_anova(modelo, familia)
      if (!is.null(anova_result)) {
        anova_result$Variable <- variable
        anova_result$Familia <- familia
        resultados_anova[[nombre_modelo]] <- anova_result
      }
      
    
      cat("✓ Modelo ajustado exitosamente\n")
      cat("  AIC:", ifelse(!is.null(modelo$aic), round(modelo$aic, 2), "NA"), "\n")
      if (familia == "binomial_negativa") {
        cat("  Theta:", round(modelo$theta, 3), "\n")
      }
      cat("  Dispersión:", round(summary(modelo)$dispersion, 3), "\n")
      
      
      if (!is.null(anova_result)) {
        cat("\n  ANOVA (Type II LR tests):\n")
        print(anova_result[, c("Termino", "LR_Chisq", "Df", "Pr_Chisq")])
      }
      
    } else {
      cat("✗ Error ajustando modelo\n")
    }
    cat("\n")
  }
}


if (length(resultados_anova) > 0) {
 
  anova_completa <- do.call(rbind, resultados_anova)
  rownames(anova_completa) <- NULL
  
  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  cat("TABLA RESUMEN COMPLETA - ANÁLISIS DE DEVIANZA (TYPE II TESTS)\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  

  for (variable in variables_existentes) {
    for (familia in familias_modelos) {
      nombre_modelo <- paste0(variable, "_", familia)
      if (nombre_modelo %in% names(resultados_anova)) {
        cat("\nResponse:", variable, "- Familia:", familia, "\n")
        anova_var <- resultados_anova[[nombre_modelo]]
 
        anova_display <- anova_var[, c("Termino", "LR_Chisq", "Df", "Pr_Chisq")]
        anova_display$LR_Chisq <- round(anova_display$LR_Chisq, 4)
        anova_display$Pr_Chisq <- round(anova_display$Pr_Chisq, 4)
      
        anova_display$Significancia <- ifelse(anova_display$Pr_Chisq < 0.001, "***",
                                              ifelse(anova_display$Pr_Chisq < 0.01, "**",
                                                     ifelse(anova_display$Pr_Chisq < 0.05, "*", 
                                                            ifelse(anova_display$Pr_Chisq < 0.1, ".", ""))))
        print(anova_display)
        cat("\n")
      }
    }
  }
  

  assign("modelos_mlg", resultados_mlg, envir = .GlobalEnv)
  assign("tabla_anova", anova_completa, envir = .GlobalEnv)
  assign("resultados_anova_detallados", resultados_anova, envir = .GlobalEnv)
  
  cat("✓ Modelos guardados en 'modelos_mlg'\n")
  cat("✓ Tabla ANOVA completa en 'tabla_anova'\n")
  cat("✓ Resultados ANOVA detallados en 'resultados_anova_detallados'\n")
  
} else {
  cat("No se pudo generar ningún análisis ANOVA\n")
}


if (length(resultados_anova) > 0) {
  cat("\n", paste(rep("#", 70), collapse = ""), "\n")
  cat("RESUMEN EJECUTIVO - TÉRMINOS SIGNIFICATIVOS (p < 0.05)\n")
  cat(paste(rep("#", 70), collapse = ""), "\n")
  
  significativos_encontrados <- FALSE
  
  for (variable in variables_existentes) {
    for (familia in familias_modelos) {
      nombre_modelo <- paste0(variable, "_", familia)
      if (nombre_modelo %in% names(resultados_anova)) {
        anova_var <- resultados_anova[[nombre_modelo]]
        significativos <- anova_var[anova_var$Pr_Chisq < 0.05 & !is.na(anova_var$Pr_Chisq), ]
        
        if (nrow(significativos) > 0) {
          significativos_encontrados <- TRUE
          cat("\n", variable, "(", familia, "):\n")
          for (i in 1:nrow(significativos)) {
            cat("  • ", significativos$Termino[i], 
                " (LR Chisq = ", round(significativos$LR_Chisq[i], 3),
                ", p = ", round(significativos$Pr_Chisq[i], 4), ")\n", sep = "")
          }
        }
      }
    }
  }
  
  if (!significativos_encontrados) {
    cat("\nNo se encontraron términos significativos al nivel p < 0.05\n")
  }
}


# Verificación de supuestos 

if (!require(DHARMa)) install.packages("DHARMa"); library(DHARMa)
if (!require(performance)) install.packages("performance"); library(performance)

verificar_supuestos <- function(modelo, familia, variable_nombre) {
  if (is.null(modelo)) return(NULL)
  
  cat("\n", paste(rep("-", 60), collapse = ""), "\n")
  cat("VERIFICACIÓN DE SUPUESTOS:", variable_nombre, "-", familia, "\n")
  cat(paste(rep("-", 60), collapse = ""), "\n")
  
  resultados_supuestos <- list()
  
  tryCatch({
    
    cat("\n1. ANÁLISIS DE RESIDUOS (DHARMa):\n")
    simulationOutput <- simulateResiduals(fittedModel = modelo, plot = TRUE)
    
    
    test_unif <- testUniformity(simulationOutput, plot = FALSE)
    resultados_supuestos$test_uniformidad <- test_unif$p.value
    cat("   Test de uniformidad: p =", round(test_unif$p.value, 4), 
        ifelse(test_unif$p.value < 0.05, "✗ PROBLEMA", "✓ OK"), "\n")
    
    
    test_disp <- testDispersion(simulationOutput, plot = FALSE)
    resultados_supuestos$test_dispersion <- test_disp$p.value
    cat("   Test de dispersión: p =", round(test_disp$p.value, 4),
        ifelse(test_disp$p.value < 0.05, "✗ PROBLEMA DE DISPERSIÓN", "✓ OK"), "\n")
    

    test_out <- testOutliers(simulationOutput, plot = FALSE)
    resultados_supuestos$test_outliers <- test_out$p.value
    cat("   Test de outliers: p =", round(test_out$p.value, 4),
        ifelse(test_out$p.value < 0.05, "✗ OUTLIERS DETECTADOS", "✓ OK"), "\n")
    
  }, error = function(e) {
    cat("   Error en DHARMa:", e$message, "\n")
  })
  
  tryCatch({
    
    cat("\n2. ANÁLISIS DE DISPERSIÓN:\n")
    dispersion_ratio <- summary(modelo)$dispersion
    resultados_supuestos$dispersion_ratio <- dispersion_ratio
    cat("   Ratio de dispersión:", round(dispersion_ratio, 3), "\n")
    
    if (familia == "quasipoisson") {
      if (dispersion_ratio > 1.5) {
        cat("   ⚠️  Posible overdispersion (ratio > 1.5)\n")
      } else if (dispersion_ratio < 0.5) {
        cat("   ⚠️  Posible underdispersion (ratio < 0.5)\n")
      } else {
        cat("   ✓ Dispersión adecuada para quasipoisson\n")
      }
    }
    
    if (familia == "binomial_negativa") {
      theta <- modelo$theta
      resultados_supuestos$theta <- theta
      cat("   Theta (binomial negativa):", round(theta, 3), "\n")
      if (theta < 1) {
        cat("   ⚠️  Theta bajo - puede indicar alta variabilidad\n")
      } else {
        cat("   ✓ Theta adecuado\n")
      }
    }
    
  }, error = function(e) {
    cat("   Error en análisis de dispersión:", e$message, "\n")
  })
  
  tryCatch({
   
    cat("\n3. GRÁFICO DE RESIDUOS vs AJUSTADOS:\n")
    plot(fitted(modelo), residuals(modelo, type = "deviance"),
         main = paste("Residuos vs Ajustados -", variable_nombre, "-", familia),
         xlab = "Valores Ajustados", ylab = "Residuos Deviance")
    abline(h = 0, col = "red", lty = 2)
    
  
    correlation_test <- cor.test(fitted(modelo), residuals(modelo, type = "deviance"))
    resultados_supuestos$correlacion_residuos <- correlation_test$p.value
    cat("   Correlación residuos-ajustados: p =", round(correlation_test$p.value, 4),
        ifelse(correlation_test$p.value < 0.05, "✗ PATRÓN NO ALEATORIO", "✓ OK"), "\n")
    
  }, error = function(e) {
    cat("   Error en gráfico residuos:", e$message, "\n")
  })
  
  tryCatch({
  
    cat("\n4. MULTICOLINEALIDAD (VIF):\n")
    if (length(coef(modelo)) > 2) {
      vif_values <- tryCatch({
        vif(modelo)
      }, error = function(e) {
        return(NA)
      })
      
      if (!any(is.na(vif_values))) {
        resultados_supuestos$vif <- vif_values
        cat("   Valores VIF:\n")
        for (i in 1:length(vif_values)) {
          cat("     ", names(vif_values)[i], ":", round(vif_values[i], 2), 
              ifelse(vif_values[i] > 5, "⚠️", ""), 
              ifelse(vif_values[i] > 10, "✗", ""), "\n")
        }
        
        if (any(vif_values > 10)) {
          cat("   ✗ PROBLEMA: Multicolinealidad severa (VIF > 10)\n")
        } else if (any(vif_values > 5)) {
          cat("   ⚠️  Posible multicolinealidad (VIF > 5)\n")
        } else {
          cat("   ✓ Sin problemas de multicolinealidad\n")
        }
      } else {
        cat("   No se pudo calcular VIF para este modelo\n")
      }
    } else {
      cat("   Modelo muy simple para cálculo de VIF\n")
    }
    
  }, error = function(e) {
    cat("   Error en VIF:", e$message, "\n")
  })
  
  return(resultados_supuestos)
}



cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("INICIANDO VERIFICACIÓN DE SUPUESTOS PARA TODOS LOS MODELOS\n")
cat(paste(rep("=", 80), collapse = ""), "\n")


resultados_supuestos <- list()


for (variable in variables_existentes) {
  for (familia in familias_modelos) {
    nombre_modelo <- paste0(variable, "_", familia)
    if (nombre_modelo %in% names(resultados_mlg)) {
      modelo <- resultados_mlg[[nombre_modelo]]
      supuestos <- verificar_supuestos(modelo, familia, variable)
      resultados_supuestos[[nombre_modelo]] <- supuestos
      
     
      cat("\n")
      Sys.sleep(1)
    }
  }
}



cat("\n", paste(rep("#", 80), collapse = ""), "\n")
cat("RESUMEN FINAL - VERIFICACIÓN DE SUPUESTOS\n")
cat(paste(rep("#", 80), collapse = ""), "\n")

problemas_detectados <- FALSE

for (variable in variables_existentes) {
  for (familia in familias_modelos) {
    nombre_modelo <- paste0(variable, "_", familia)
    if (nombre_modelo %in% names(resultados_supuestos)) {
      supuestos <- resultados_supuestos[[nombre_modelo]]
      
    
      problemas <- c()
      
      if (!is.null(supuestos$test_uniformidad) && supuestos$test_uniformidad < 0.05) {
        problemas <- c(problemas, "Residuos no uniformes")
      }
      if (!is.null(supuestos$test_dispersion) && supuestos$test_dispersion < 0.05) {
        problemas <- c(problemas, "Problemas de dispersión")
      }
      if (!is.null(supuestos$test_outliers) && supuestos$test_outliers < 0.05) {
        problemas <- c(problemas, "Outliers significativos")
      }
      if (!is.null(supuestos$correlacion_residuos) && supuestos$correlacion_residuos < 0.05) {
        problemas <- c(problemas, "Patrón en residuos")
      }
      
      if (length(problemas) > 0) {
        problemas_detectados <- TRUE
        cat("\n⚠️  ", variable, "(", familia, "):\n")
        cat("    Problemas:", paste(problemas, collapse = ", "), "\n")
      } else {
        cat("\n✓  ", variable, "(", familia, "): Supuestos cumplidos adecuadamente\n")
      }
    }
  }
}

if (!problemas_detectados) {
  cat("\n✅ TODOS LOS MODELOS CUMPLEN ADECUADAMENTE CON LOS SUPUESTOS\n")
} else {
  cat("\n🔴 ALGUNOS MODELOS PRESENTAN PROBLEMAS EN LOS SUPUESTOS - INTERPRETAR CON CAUTELA\n")
}


assign("resultados_supuestos", resultados_supuestos, envir = .GlobalEnv)
cat("\n✓ Resultados de supuestos guardados en 'resultados_supuestos'\n")


cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("RECOMENDACIONES:\n")
cat("• Modelos con problemas de uniformidad: considerar transformaciones\n")        # Recomendaciones generales
cat("• Modelos con overdispersion: binomial negativa suele ser mejor opción\n")
cat("• Modelos con underdispersion: Poisson puede ser adecuado\n")
cat("• Presencia de outliers: verificar datos y considerar modelos robustos\n")
cat("• Multicolinealidad: simplificar el modelo o remover predictores correlacionados\n")
cat(paste(rep("=", 80), collapse = ""), "\n")


# Prueba múltiple de medias y gráfico de líneas de error

if (!require(emmeans)) install.packages("emmeans"); library(emmeans)
if (!require(multcomp)) install.packages("multcomp"); library(multcomp)
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if (!require(multcompView)) install.packages("multcompView"); library(multcompView)


nivel_significancia <- 0.15 # ajustar


realizar_prueba_medias <- function(modelo, variable_nombre, familia, termino, alpha = 0.15) {
  if (is.null(modelo)) return(NULL)
  
  cat("\n", paste(rep("-", 60), collapse = ""), "\n")
  cat("PRUEBA MÚLTIPLE DE MEDIAS:", variable_nombre, "-", familia, "\n")
  cat("Término:", termino, "- Nivel de significancia:", alpha, "\n")
  cat(paste(rep("-", 60), collapse = ""), "\n")
  
  tryCatch({
    
    emm <- emmeans(modelo, specs = termino, type = "response")
    
    
    comparaciones <- pairs(emm, adjust = "tukey")
    
    
    letras <- cld(emm, alpha = alpha, Letters = letters, adjust = "tukey")
    
    
    cat("\nMedias marginales estimadas:\n")
    print(emm)
    
    cat("\nComparaciones múltiples (Tukey):\n")
    print(comparaciones)
    
    cat("\nLetras de significancia (alpha =", alpha, "):\n")
    print(letras)
    
    return(list(emm = emm, comparaciones = comparaciones, letras = letras))
    
  }, error = function(e) {
    cat("Error en prueba de medias:", e$message, "\n")
    return(NULL)
  })
}


crear_grafico_lineas_error <- function(resultados_medias, variable_nombre, familia, termino) {
  if (is.null(resultados_medias)) return(NULL)
  
  letras <- resultados_medias$letras
  emm <- resultados_medias$emm
  
  
  plot_data <- as.data.frame(emm)
  
  # **CORRECCIÓN 1: Identificación de nombres de columnas de IC**
  ic_names <- names(plot_data)[grep("\\.LCL$|\\.UCL$", names(plot_data))]
  LCL_name <- ic_names[grep("\\.LCL$", ic_names)]
  UCL_name <- ic_names[grep("\\.UCL$", ic_names)]
  
  # Determinar el nombre de la columna de la media estimada (response o emmean)
  mean_name <- ifelse("response" %in% names(plot_data), "response", "emmean")
  
  
  plot_data$.group <- letras$.group
  
  
  p <- ggplot(plot_data, aes(x = .data[[names(plot_data)[1]]], y = .data[[mean_name]])) +
    geom_point(size = 3, color = "blue") +
    geom_line(aes(group = 1), linetype = "dashed", alpha = 0.5, color = "blue") +
    geom_errorbar(aes(ymin = .data[[LCL_name]], ymax = .data[[UCL_name]]), # <--- CORRECCIÓN aplicada
                  width = 0.2, linewidth = 0.8, color = "blue") +
    geom_text(aes(label = .group, y = .data[[UCL_name]]), # <--- CORRECCIÓN aplicada
              vjust = -0.8, size = 4, fontface = "bold", color = "red") +
    labs(title = paste("Medias de", variable_nombre, "-", familia),
         subtitle = paste("Término:", termino),
         x = termino,
         y = paste("Media Estimada de", variable_nombre), # Cambio para ser más preciso
         caption = paste("Letras diferentes indican diferencias significativas (α =", nivel_significancia, ")")) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
  return(p)
}


crear_grafico_lineas_error_alt <- function(resultados_medias, variable_nombre, familia, termino) {
  if (is.null(resultados_medias)) return(NULL)
  
  letras <- resultados_medias$letras
  
  
  plot_data <- as.data.frame(letras)
  
  
  names(plot_data) <- tolower(names(plot_data))
  
  # **CORRECCIÓN 2: Uso de nombres 'response' / 'asymp.lcl'**
  # La media es 'response' o 'emmean', el IC es 'asymp.lcl' / 'asymp.ucl'
  
  mean_name <- ifelse("response" %in% names(plot_data), "response", "emmean")
  LCL_name <- ifelse("asymp.lcl" %in% names(plot_data), "asymp.lcl", "lower.cl")
  UCL_name <- ifelse("asymp.ucl" %in% names(plot_data), "asymp.ucl", "upper.cl")
  
  
  if (!LCL_name %in% names(plot_data)) { # Si aún no encuentra los nombres, usar SE (menos preciso)
    
    plot_data$temp_lcl <- plot_data$emmean - plot_data$se * 1.96
    plot_data$temp_ucl <- plot_data$emmean + plot_data$se * 1.96
    LCL_name <- "temp_lcl"
    UCL_name <- "temp_ucl"
    warning("Usando IC basado en SE * 1.96, no los límites asintóticos reales.")
  }
  
  # **Aplicar CORRECCIÓN 2 en el gráfico**
  p <- ggplot(plot_data, aes(x = .data[[names(plot_data)[1]]], y = .data[[mean_name]])) +
    geom_point(size = 4, color = "red") +
    geom_line(aes(group = 1), linetype = "dashed", alpha = 0.5, color = "black") +
    geom_errorbar(aes(ymin = .data[[LCL_name]], ymax = .data[[UCL_name]]), # <--- CORRECCIÓN aplicada
                  width = 0.2, linewidth = 0.8, color = "black") +
    geom_text(aes(label = .group, y = .data[[UCL_name]]), # <--- CORRECCIÓN aplicada
              vjust = -0.8, size = 5, fontface = "bold", color = "red") +
    labs(title = paste("Medias de", variable_nombre, "-", familia),
         subtitle = paste("Término:", termino),
         x = termino,
         y = paste("Media Estimada de", variable_nombre), # Cambio para ser más preciso
         caption = paste("Letras diferentes indican diferencias significativas (α =", nivel_significancia, ")")) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 16),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
  return(p)
}


crear_grafico_bigotes <- function(datos, variable_nombre, termino) {
  
  if (!termino %in% names(datos)) {
    cat("El término", termino, "no existe en los datos\n")
    return(NULL)
  }
  
  
  p <- ggplot(datos, aes(x = .data[[termino]], y = .data[[variable_nombre]])) +
    geom_boxplot(width = 0.3, alpha = 0.7, outlier.shape = NA) +
    geom_jitter(width = 0.1, alpha = 0.6, size = 1.5, color = "blue") +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
    labs(title = paste("Distribución de", variable_nombre),
         subtitle = paste("Por niveles de", termino),
         x = termino,
         y = variable_nombre) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
  return(p)
}


cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("INICIANDO PRUEBAS MÚLTIPLES DE MEDIAS Y GRÁFICOS\n")
cat("Nivel de significancia:", nivel_significancia, "\n")
cat(paste(rep("=", 80), collapse = ""), "\n")


resultados_medias <- list()
graficos_medias <- list()
graficos_bigotes <- list()


terminos_para_medias <- c("Tratamiento") # Ajustar según las FV

# Bucle principal
for (variable in variables_existentes) {
  for (familia in familias_modelos) {
    nombre_modelo <- paste0(variable, "_", familia)
    
    if (nombre_modelo %in% names(resultados_mlg)) {
      modelo <- resultados_mlg[[nombre_modelo]]
      
      cat("\n", paste(rep("*", 70), collapse = ""), "\n")
      cat("PROCESANDO:", variable, "-", familia, "\n")
      cat(paste(rep("*", 70), collapse = ""), "\n")
      
      
      for (termino in terminos_para_medias) {
        if (termino %in% all.vars(formula(modelo))) {
          
          resultados <- realizar_prueba_medias(modelo, variable, familia, termino, nivel_significancia)
          
          if (!is.null(resultados)) {
            
            key <- paste0(variable, "_", familia, "_", termino)
            resultados_medias[[key]] <- resultados
            
            
            cat("\nGenerando gráfico de líneas de error...\n")
            grafico <- crear_grafico_lineas_error_alt(resultados, variable, familia, termino)
            graficos_medias[[key]] <- grafico
            
            
            cat("Generando gráfico de bigotes...\n")
            grafico_bigotes <- crear_grafico_bigotes(datos, variable, termino)
            graficos_bigotes[[key]] <- grafico_bigotes
            
            
            Sys.sleep(1)
          }
        }
      }
    }
  }
}


######### Distribución de Poisson: distribución aleatoria

pacman::p_load(readxl, lme4, lmerTest, MASS, DHARMa, car, multcomp, hnp, dplyr, tidyr,
               ggplot2, extrafont, gridExtra, ggiraphExtra, tinytex, reshape2, ggpubr, 
               emmeans, lsmeans, multcompView, effects, vegan, tidyverse, knitr, 
               kableExtra, ggrepel, patchwork)

colnames(datos)

vars_analisis <- c("Hoja.Ancha", "Graminea", "Cipereacea") 
f_principal   <- "Tratamiento"
f_bloque      <- "Replica"

# Conversión a factor
datos <- datos %>%
  mutate(across(all_of(c(f_principal, f_bloque)), as.factor))

# --- Bucle de Análisis MLG Poisson y Verificación ---
lista_posthoc <- list()

for(v in vars_analisis) {
  message(paste("Analizando conteos para:", v))
  
  # 1. Ajuste del Modelo Poisson
  formula_glm <- as.formula(paste(v, "~", f_principal, "+", f_bloque))
  modelo_pois <- glm(formula_glm, family = poisson(link = "log"), data = datos)
  
  # 2. Verificación de Supuestos con DHARMa
  # (Importante para verificar si realmente no hay sobredispersión)
  residuos <- simulateResiduals(modelo_pois)
  plot(residuos) # Genera gráficos de diagnóstico
  testDispersion(residuos) 
  
  # 3. ANOVA (Wald Chi-square test para GLM)
  print(kable(Anova(modelo_pois, type = 2), caption = paste("Análisis de Desvianza (Poisson) para", v)) %>% 
          kable_styling(bootstrap_options = c("striped", "hover")))
  
  # 4. Post-hoc: Comparación de medias (Escala de respuesta)
  # type = "response" convierte el log(lambda) a conteos reales
  res_means <- emmeans(modelo_pois, as.formula(paste("~", f_principal)), type = "response") %>%
    multcomp::cld(Letters = letters, sort = TRUE, reverse = TRUE, alpha = 0.05) %>%
    as.data.frame() %>%
    mutate(Variable = v)
  
  # Renombrar columnas de emmeans para que coincidan con el código de gráfico (rate o prob -> emmean)
  colnames(res_means)[which(colnames(res_means) %in% c("rate", "prob", "response"))] <- "emmean"
  res_means$.group <- trimws(res_means$.group)
  
  lista_posthoc[[v]] <- res_means
}

tabla_completa <- bind_rows(lista_posthoc)

# --- Gráfico Combinado Estilo Panel ---

orden_vars      <- vars_analisis
grupos_graficos <- split(orden_vars, ceiling(seq_along(orden_vars) / 4))

for (i in seq_along(grupos_graficos)) {
  
  datos_plot <- tabla_completa %>%
    filter(Variable %in% grupos_graficos[[i]]) %>%
    mutate(Variable = factor(Variable, levels = grupos_graficos[[i]]))
  
  gg <- ggplot(datos_plot, aes(y = reorder(!!sym(f_principal), emmean), x = emmean)) +
    # Barras de Error (Basadas en el SE calculado por emmeans en escala response)
    geom_errorbarh(aes(xmin = emmean - SE, xmax = emmean + SE), 
                   height = 0.1, color = "black", size = 0.5) +
    # Puntos de la media
    geom_point(size = 3, color = "forestgreen") + # Color verde para insectos/agro
    # Etiquetas: Conteo medio + Letras de significancia
    geom_text(aes(label = sprintf("%.2f %s", emmean, .group)),
              hjust = -0.2, 
              vjust = -1.0, 
              size = 3.8, 
              family = "Arial", 
              fontface = "bold") +
    # Facetado
    facet_wrap(~ Variable, scales = "free_x", ncol = 2) + 
    # Estética
    scale_x_continuous(expand = expansion(mult = c(0.1, 0.4))) + 
    labs(title = paste("Análisis de Conteos (Poisson) - Panel", i),
         subtitle = "Medias estimadas con error estándar. Letras distintas indican diferencias significativas (p < 0.05)",
         x = "Conteo Promedio (Escala Original)", 
         y = "Tratamiento") +
    theme_bw() + 
    theme(
      legend.position = "none",
      text = element_text(family = "Arial", color = "black"),
      strip.background = element_rect(fill = "grey95"),
      strip.text = element_text(face = "bold", size = 11),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 10, color = "black"),
      panel.spacing = unit(2, "lines") 
    )
  
  print(gg)}




############# Porcentajes ########################
names(datos)

factores_prop <- c("Tratamiento", "Replica")
variables_proporciones <- c("Eficiencia.Hoja.Ancha", "Eficiencia.Graminea", "Eficiencia.Cipereacea")
terminos_modelo_prop <- c("Tratamiento", "Replica")
familias_modelos_prop <- c("beta", "quasibinomial")  # se ajustan ambos; pruebas/plots se hacen para quasibinomial
escala_variables <- list(
  Porc_captura = "porcentaje",
  Proporcion_exito = "proporcion" #proporcion
)


detectar_escala <- function(valores) {
  if (all(valores >= 0 & valores <= 1, na.rm = TRUE)) return("proporcion")
  if (all(valores >= 0 & valores <= 100, na.rm = TRUE)) return("porcentaje")
  return("desconocida")
}

transformar_a_proporcion <- function(datos, variables, escalas) {
  datos2 <- datos
  for (v in variables) {
    if (!v %in% names(datos2)) next
    escala <- escalas[[v]]
    vals <- datos2[[v]]
    if (is.null(escala)) {
      escala <- detectar_escala(vals)
      message("Variable ", v, " detectada como: ", escala)
    }
    if (escala == "porcentaje") {
      datos2[[v]] <- vals / 100
      message("Variable ", v, " transformada de porcentaje a proporción")
    } else if (escala == "desconocida") {
      warning("Escala desconocida para ", v, ". Valores entre ",
              min(vals, na.rm = TRUE), " y ", max(vals, na.rm = TRUE))
    }
  }
  datos2
}

crear_formula_prop <- function(respuesta, terminos) {
  as.formula(paste0(respuesta, " ~ ", paste(terminos, collapse = " + ")))
}

ajustar_modelo_prop <- function(formula, datos, familia, variable) {
  tryCatch({
    if (familia == "beta") {
      # transformacion recomendada para beta (evitar 0/1)
      y <- model.response(model.frame(formula, datos))
      n <- length(y)
      y_transf <- (y * (n - 1) + 0.5) / n
      datos_temp <- datos
      datos_temp[[variable]] <- y_transf
      betareg::betareg(formula, data = datos_temp)
    } else {
      glm(formula, data = datos, family = quasibinomial())
    }
  }, error = function(e) {
    warning("Error ajustando modelo (", familia, "): ", e$message)
    NULL
  })
}

extraer_anova_prop <- function(modelo, familia, terminos_completos) {
  if (is.null(modelo)) return(NULL)
  tryCatch({
    if (familia == "quasibinomial") {
      a <- car::Anova(modelo, type = "II", test = "F")
      adf <- as.data.frame(a)
      adf$Termino <- rownames(adf)
      rownames(adf) <- NULL
      names(adf)[1:4] <- c("F_value", "Df", "Df_res", "Pr_F")
      adf[, c("Termino", "F_value", "Df", "Df_res", "Pr_F")]
    } else if (familia == "beta") {
      # Likelihood ratio tests por término (beta)
      resultados <- data.frame()
      formula_completa <- stats::formula(modelo)
      for (termino in terminos_completos) {
        tryCatch({
          terminos_sin <- terminos_completos[terminos_completos != termino]
          if (length(terminos_sin) == 0) next
          fr <- crear_formula_prop(as.character(formula_completa)[2], terminos_sin)
          mod_red <- betareg::betareg(fr, data = modelo$model)
          test_lr <- lmtest::lrtest(mod_red, modelo)
          p_valor <- as.numeric(test_lr[2, "Pr(>Chisq)"])
          lr_chisq <- 2 * (logLik(modelo) - logLik(mod_red))
          resultados <- rbind(resultados,
                              data.frame(Termino = termino,
                                         Chisq = round(as.numeric(lr_chisq), 4),
                                         Df = NA,
                                         Pr_Chisq = round(p_valor, 4),
                                         stringsAsFactors = FALSE))
        }, error = function(e) {
          warning("Error test LR en termino ", termino, ": ", e$message)
        })
      }
      resultados
    }
  }, error = function(e) {
    warning("Error en ANOVA de proporciones: ", e$message)
    NULL
  })
}

extraer_resumen_beta <- function(modelo) {
  if (is.null(modelo)) return(NULL)
  tryCatch({
    s <- summary(modelo)
    coefm <- s$coefficients$mean
    data.frame(Parametro = rownames(coefm),
               Coeficiente = coefm[, "Estimate"],
               Error_Std = coefm[, "Std. Error"],
               z_value = coefm[, "z value"],
               Pr_z = coefm[, "Pr(>|z|)"],
               stringsAsFactors = FALSE)
  }, error = function(e) {
    warning("Error resumen beta: ", e$message)
    NULL
  })
}

# ----------------------------------------------------------
# Inicio del análisis: transformación y comprobaciones
# ----------------------------------------------------------
datos_prop <- transformar_a_proporcion(datos, variables_proporciones, escala_variables)

variables_existentes_prop <- intersect(variables_proporciones, names(datos_prop))
if (length(variables_existentes_prop) == 0) stop("No se encontró ninguna variable de proporción en los datos")

# Preparar contenedores
resultados_prop <- list()
resultados_anova_prop <- list()
resultados_coeficientes_prop <- list()


for (variable in variables_existentes_prop) {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("Modelando:", variable, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  formula_actual <- crear_formula_prop(variable, terminos_modelo_prop)
  cat("Fórmula:", deparse(formula_actual), "\n")
  
  for (familia in familias_modelos_prop) {
    cat(" Ajustando familia:", familia, "...\n")
    modelo <- ajustar_modelo_prop(formula_actual, datos_prop, familia, variable)
    if (is.null(modelo)) {
      cat("  ✗ Error: modelo no ajustado\n")
      next
    }
    nombre_modelo <- paste0(variable, "_", familia)
    resultados_prop[[nombre_modelo]] <- modelo
    cat("  ✓ Modelo ajustado:", nombre_modelo, "\n")
    
    if (familia == "quasibinomial") {
      # información adicional
      cat("   Dispersión:", round(summary(modelo)$dispersion, 3), "\n")
      cat("   AIC:", round(AIC(modelo), 2), "\n")
      anova_res <- extraer_anova_prop(modelo, familia, terminos_modelo_prop)
      if (!is.null(anova_res)) {
        anova_res$Variable <- variable
        anova_res$Familia <- familia
        resultados_anova_prop[[nombre_modelo]] <- anova_res
        cat("   ANOVA (Type II F-tests):\n"); print(anova_res[, c("Termino","F_value","Df","Pr_F")])
      }
    } else if (familia == "beta") {
      cat("   Pseudo R2:", round(summary(modelo)$pseudo.r.squared, 3), "\n")
      cat("   LogLik:", round(logLik(modelo), 2), "\n")
      coef_res <- extraer_resumen_beta(modelo)
      if (!is.null(coef_res)) {
        coef_res$Variable <- variable
        coef_res$Familia <- familia
        resultados_coeficientes_prop[[nombre_modelo]] <- coef_res
        cat("   COEFICIENTES (beta):\n"); print(coef_res[, c("Parametro","Coeficiente","Pr_z")])
      }
      # intentar ANOVA para beta
      anova_alt <- extraer_anova_prop(modelo, familia, terminos_modelo_prop)
      if (!is.null(anova_alt)) {
        anova_alt$Variable <- variable
        anova_alt$Familia <- familia
        resultados_anova_prop[[nombre_modelo]] <- anova_alt
        cat("   ANOVA (LR tests):\n"); print(anova_alt)
      }
    }
    cat("\n")
  }
}


###################################################################
#Prueba múltiple de medias

config_grafico <- list(
  factor_principal = "Tratamiento",
  
  alpha = 0.15,             # Significancia
  mostrar_linea = FALSE,     # Línea que une medias
  
  color_linea = "black",
  grosor_linea = 1,
  
  color_punto = "red",
  tamaño_punto = 4,
  
  grosor_error = 1,
  ancho_error = 0.15
)


for (nombre_modelo in names(resultados_prop)) {
  
  modelo_act <- resultados_prop[[nombre_modelo]]
  
  split_name <- unlist(strsplit(nombre_modelo, "_"))
  variable <- paste(split_name[1:(length(split_name)-1)], collapse = "_")
  familia <- split_name[length(split_name)]
  
  cat("\n------------------------------------------------------------\n")
  cat("Variable:", variable, "- Familia:", familia, "\n")
  cat("Solo comparaciones por TRATAMIENTO\n")
  cat("------------------------------------------------------------\n")
  
  factor_usado <- config_grafico$factor_principal
  
  if (!(factor_usado %in% names(modelo_act$model))) {
    cat("El factor configurado no está en el modelo. Saltando...\n")
    next
  }
  

  emms <- tryCatch({
    emmeans(modelo_act, specs = factor_usado)
  }, error = function(e) {
    cat("Error en EMMs:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(emms)) next
  

  comparaciones <- tryCatch({
    pairs(emms, adjust = "tukey")
  }, error = function(e) {
    cat("Error en comparaciones múltiples:", e$message, "\n")
    return(NULL)
  })
  
  print(comparaciones)
  

  letras <- tryCatch({
    multcomp::cld(emms, adjust = "tukey", Letters = letters)
  }, error = function(e) {
    cat("Error generando letras:", e$message, "\n")
    return(NULL)
  })
  
  df_plot <- as.data.frame(letras)
  df_plot <- df_plot %>% mutate(y_pos = emmean + SE * 1.3)
  

  cat("\nTabla de medias marginales (solo por tratamiento):\n")
  print(df_plot[, c(factor_usado, "emmean", "SE", ".group")])
  
  # -------------- GRÁFICO DINÁMICO -------------------
  
  g <- ggplot(df_plot, aes_string(x = factor_usado, y = "emmean")) +
    
    # Punto
    geom_point(size = config_grafico$tamaño_punto,
               color = config_grafico$color_punto) +
    
    # Línea opcional
    { if (config_grafico$mostrar_linea)
      geom_line(group = 1,
                linewidth = config_grafico$grosor_linea,
                color = config_grafico$color_linea)
    } +
    
    # Barras de error
    geom_errorbar(
      aes(ymin = emmean - SE, ymax = emmean + SE),
      width = config_grafico$ancho_error,
      linewidth = config_grafico$grosor_error
    ) +
    
    # Letras
    geom_text(aes(label = .group, y = y_pos),
              vjust = 0, size = 5) +
    
    labs(
      title = paste("Medias marginales por tratamiento -", variable, "(", familia, ")"),
      x = config_grafico$factor_principal,
      y = paste("Media marginal de", variable)
    ) +
    theme_bw(base_size = 14)
  
  print(g)
}

############### Otro enfoque de MLG para porcentajes (incluye ajuste de 0s y 1s exactos)
names(datos)
library(ggplot2)
library(dplyr)
library(emmeans)
library(multcomp)
library(multcompView)
library(car)
library(betareg)


config <- list(
  variables    = c("Eficiencia.Hoja.Ancha", "Eficiencia.Graminea", "Eficiencia.Cipereacea"),
  terminos     = c("Tratamiento", "Replica"),
  familias     = c("beta", "quasibinomial"),
  
  # Cambia a "Chisq": menos conservador o "F"
  test_quasi   = "F", 
  
  # Para evitar error de tener valores exactos de 0 y 100 en beta regresion
  ajuste_beta  = T
)

datos_prop <- datos
for (v in config$variables) {
  if (max(datos_prop[[v]], na.rm = TRUE) > 1) {
    datos_prop[[v]] <- datos_prop[[v]] / 100
  }
}

resultados_modelos <- list()

for (variable in config$variables) {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat(" ANALIZANDO VARIABLE:", variable, "\n")
  
  for (familia in config$familias) {
    cat("\n>>> Modelo:", familia, "\n")
    
    formula_f <- as.formula(paste(variable, "~", paste(config$terminos, collapse = " + ")))
    
    modelo <- tryCatch({
      if (familia == "beta") {
        y <- datos_prop[[variable]]
        if (config$ajuste_beta) {
          n <- length(y)
          # Transformación a proporción
          y_mod <- (y * (n - 1) + 0.5) / n
          d_temp <- datos_prop; d_temp[[variable]] <- y_mod
          betareg(formula_f, data = d_temp)
        } else {
          betareg(formula_f, data = datos_prop)
        }
      } else {
        glm(formula_f, data = datos_prop, family = quasibinomial(link = "logit"))
      }
    }, error = function(e) {
      cat("  Error al ajustar el modelo:", e$message, "\n")
      return(NULL)
    })
    
    if (is.null(modelo)) next
    
    nombre_mod <- paste0(variable, "_", familia)
    resultados_modelos[[nombre_mod]] <- modelo
    
    # Diagnósticos
    ratio <- if(familia == "quasibinomial") summary(modelo)$dispersion else (1/mean(predict(modelo, type="precision")))
    em_res <- as.data.frame(emmeans(modelo, ~ Tratamiento))
    
    cat("  Ratio Dispersión:", round(ratio, 5), "\n")
    cat("  SE promedio:", round(mean(em_res$SE), 6), "\n")
    
    if (familia == "quasibinomial") {
      print(Anova(modelo, type = "II", test = config$test_quasi))
    } else {
      print(Anova(modelo, type = "II", test = "Chisq"))
    }
  }
}

# Post-ANDEVA

for (nombre in names(resultados_modelos)) {
  m <- resultados_modelos[[nombre]]
  info <- unlist(strsplit(nombre, "_"))
  v_name <- info[1]; f_name <- info[length(info)]
  
  em <- emmeans(m, ~ Tratamiento, type = "response")
  
  letras_df <- tryCatch({
    cld(em, Letters = letters, adjust = "tukey", reversed = TRUE) %>% as.data.frame()
  }, error = function(e) return(NULL))
  
  if (is.null(letras_df)) next
  
  col_y   <- intersect(names(letras_df), c("response", "prob", "emmean"))[1]
  col_inf <- intersect(names(letras_df), c("asymp.LCL", "lower.CL"))[1]
  col_sup <- intersect(names(letras_df), c("asymp.UCL", "upper.CL"))[1]
  
  df_plot <- letras_df %>%
    rename(media = !!col_y, lcl = !!col_inf, ucl = !!col_sup, letras = .group) %>%
    mutate(letras = trimws(letras))
  
  p <- ggplot(df_plot, aes(x = Tratamiento, y = media, group = 1)) +
    geom_line(color = "grey70", linetype = "dashed") +
    geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.1) +
    geom_point(size = 4, color = ifelse(f_name == "beta", "blue4", "red4")) +
    geom_text(aes(label = letras, y = ucl), vjust = -1, fontface = "bold", size = 5) +
    scale_y_continuous(labels = scales::percent_format(), 
                       limits = c(0, max(df_plot$ucl) * 1.3)) +
    labs(title = paste("Variable:", v_name),
         subtitle = paste("Modelo:", f_name, "| Test Quasi:", 
                          if(f_name=="quasibinomial") config$test_quasi else "Chisq"),
         y = "Cobertura Estimada (%)", x = "Tratamiento") +
    theme_classic(base_size = 12)
  
  print(p)
}




################### con MLM con distribución beta regresión ###################

library(glmmTMB)
library(car)
library(emmeans)
library(ggplot2)
library(dplyr)
library(multcomp)
library(multcompView)
library(DHARMa)


config <- list(
  variables = c("Eficiencia.Hoja.Ancha", "Eficiencia.Graminea", "Eficiencia.Ciperaceas"),
  fijo      = "Tratamiento",
  aleatorio = "(1|Replica)", 
  epsilon   = 0.0001 
)


datos_mlm <- datos
resultados_mlm <- list()

for (v in config$variables) {
  cat("\n", paste(rep("=", 70), collapse = ""), "\n")
  cat(" ANALIZANDO VARIABLE (MLM):", v, "\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  
  y <- datos_mlm[[v]]
  if(max(y, na.rm=T) > 1) y <- y / 100
  
  y[y >= 1] <- 1 - config$epsilon
  y[y <= 0] <- 0 + config$epsilon
  v_prop_name <- paste0(v, "_prop")
  datos_mlm[[v_prop_name]] <- y
  
  formula_mlm <- as.formula(paste0(v_prop_name, " ~ ", config$fijo, " + ", config$aleatorio))
  
  modelo <- tryCatch({
    glmmTMB(formula_mlm, data = datos_mlm, family = beta_family(link = "logit"))
  }, error = function(e) {
    cat("  Error al ajustar MLM:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(modelo)) next
  resultados_mlm[[v]] <- modelo
  
  cat("\n>>> DIAGNÓSTICO DE RESIDUOS (DHARMa):\n")
  residuos <- simulateResiduals(fittedModel = modelo, n = 250)
  
  print(testResiduals(residuos))
  
  
  plot(residuos)
  
  cat("\n>>> ANOVA (Type II Wald Chi-square):\n")
  print(Anova(modelo, type = "II"))
  
  em_res <- emmeans(modelo, ~ Tratamiento, type = "response")
  letras_df <- cld(em_res, Letters = letters, adjust = "tukey", reversed = TRUE) %>% 
    as.data.frame()
  
  col_media <- intersect(names(letras_df), c("response", "prob", "emmean"))[1]
  col_inf   <- intersect(names(letras_df), c("lower.CL", "asymp.LCL", "lower"))[1]
  col_sup   <- intersect(names(letras_df), c("upper.CL", "asymp.UCL", "upper"))[1]
  
  letras_df <- letras_df %>%
    rename(media = !!col_media, lcl = !!col_inf, ucl = !!col_sup) %>%
    mutate(.group = trimws(.group))
  
  cat("\n>>> ESTIMACIONES Y PRECISIÓN:\n")
  print(letras_df %>% select(Tratamiento, media, SE, lcl, ucl))
  cat("\n  * SE Promedio:", round(mean(letras_df$SE), 6), "\n")
  
  p <- ggplot(letras_df, aes(x = Tratamiento, y = media)) +
    geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.1, color = "red", linewidth = 0.7) +
    geom_point(size = 4, color = "red") +
    geom_text(aes(label = .group, y = ucl), vjust = -1.2, fontface = "bold", size = 5) +
    scale_y_continuous(labels = scales::percent_format(), 
                       limits = c(0, min(1.1, max(letras_df$ucl) * 1.3))) +
    labs(title = paste("Efecto en:", v),
         subtitle = "Modelo Mixto (GLMM Beta) | Réplica como Efecto Aleatorio",
         caption = "Diferencias según prueba de Tukey (p < 0.05)",
         y = "Control Estimado (%)", x = "Tratamiento") +
    theme_minimal(base_size = 14)
  
  print(p)
}

############################ Análisis de datos porcentuales con transformación arcoseno y box-cox 
# DBCA

library(MASS)
library(agricolae)
library(ggplot2)
library(dplyr)

# --- 1. CONFIGURACIÓN INICIAL ---
var_dep      <- "Porcentaje"  
f_trat       <- "Tratamiento" 
f_bloque     <- "Replica"     

metodo_trans <- "boxcox"    
prueba_post  <- "LSD"         # "Tukey" o "LSD"
letras_altas <- FALSE          # TRUE: 'a' al mayor | FALSE: 'a' al menor

vars_analisis <- c(var_dep) 

# --- 2. TRANSFORMACIÓN DE DATOS ---
datos[[f_trat]]   <- as.factor(datos[[f_trat]])
datos[[f_bloque]] <- as.factor(datos[[f_bloque]])

if (metodo_trans == "arcoseno") {
  datos$y_trans <- asin(sqrt(datos[[var_dep]] / 100))
  message("Transformación Arcoseno aplicada.")
} else {
  y_temp <- datos[[var_dep]]
  if(any(y_temp <= 0)) y_temp <- y_temp + 0.5
  bc <- boxcox(y_temp ~ datos[[f_bloque]] + datos[[f_trat]], plotit = FALSE)
  lambda <- bc$x[which.max(bc$y)]
  datos$y_trans <- if(lambda == 0) log(y_temp) else (y_temp^lambda - 1) / lambda
  message(paste("Transformación Box-Cox aplicada. Lambda:", round(lambda, 4)))
}

# --- 3. ANDEVA Y PRUEBA DE MEDIAS (MOSTRAR EN CONSOLA) ---
formula_mod   <- as.formula(paste("y_trans ~", f_bloque, "+", f_trat))
modelo        <- aov(formula_mod, data = datos)
titulo_prueba <- ifelse(prueba_post == "Tukey", "Prueba de Tukey (HSD)", "Prueba de Fisher (LSD)")

cat("\n==============================================================\n")
cat("                RESULTADOS DEL ANÁLISIS (ANDEVA)              \n")
cat("==============================================================\n")
print(summary(modelo)) # <--- MUESTRA EL ANDEVA EN CONSOLA

cat("\n==============================================================\n")
cat("             ", titulo_prueba, "             \n")
cat("==============================================================\n")

if (prueba_post == "Tukey") {
  test_post <- HSD.test(modelo, f_trat, group = TRUE, console = TRUE, decreasing = letras_altas)
} else {
  # Para LSD, el orden de letras se maneja internamente en agricolae
  # Si letras_altas es FALSE, invertimos el orden de las medias para que 'a' sea el menor
  test_post <- LSD.test(modelo, f_trat, group = TRUE, console = TRUE)
  
  if (!letras_altas) {
    # Re-ejecutamos con lógica de orden inverso si se solicita
    test_post <- LSD.test(modelo, f_trat, group = TRUE, console = FALSE)
    # Reordenamos manualmente para que 'a' sea el valor más bajo
    test_post$groups <- test_post$groups[order(test_post$groups$y_trans), ]
  }
}

# --- 4. DIAGNÓSTICO DE SUPUESTOS ---
cat("\n--- DIAGNÓSTICO DE RESIDUOS ---\n")
print(shapiro.test(residuals(modelo)))
par(mfrow = c(1, 2))
plot(modelo, which = 1) # Homocedasticidad
plot(modelo, which = 2) # Normalidad

# --- 5. PREPARACIÓN PARA EL GRÁFICO (DATOS REALES) ---
df_resumen <- datos %>%
  group_by(!!sym(f_trat)) %>%
  summarise(
    media_r = mean(!!sym(var_dep), na.rm = TRUE),
    sd_r    = sd(!!sym(var_dep), na.rm = TRUE),
    n       = n()
  ) %>%
  mutate(se_r = sd_r / sqrt(n), Variable = var_dep)

labels_df <- test_post$groups
labels_df[[f_trat]] <- rownames(labels_df)
datos_final_grafico <- df_resumen %>% 
  left_join(labels_df[, c(f_trat, "groups")], by = f_trat)

# --- 6. GRÁFICO CON ESTÉTICA UNIFICADA ---



grupos_graficos <- split(vars_analisis, ceiling(seq_along(vars_analisis) / 4))

for (i in seq_along(grupos_graficos)) {
  datos_plot <- datos_final_grafico %>%
    filter(Variable %in% grupos_graficos[[i]]) %>%
    mutate(Variable = factor(Variable, levels = grupos_graficos[[i]]))
  
  if(nrow(datos_plot) == 0) next
  
  gg <- ggplot(datos_plot, aes(y = reorder(!!sym(f_trat), media_r), x = media_r)) +
    geom_errorbarh(aes(xmin = media_r - se_r, xmax = media_r + se_r), height = 0.2) +
    geom_point(size = 3, color = "black") +
    geom_text(aes(label = sprintf("%.2f %s", media_r, trimws(groups))),
              hjust = -0.2, vjust = -1, size = 4.5) +
    
    facet_wrap(~ Variable, scales = "free_x") + 
    scale_x_continuous(expand = expansion(mult = c(0.1, 0.5))) +
    labs(x = "Media Aritmética Real ± SE", 
         y = "Tratamiento",
         title = paste("Comparación de Medias (", titulo_prueba, ") - Panel", i)) +
    theme_classic() +
    theme(strip.text = element_text(size = 13, face = "bold"),
          text = element_text(size = 12))
  
  print(gg)
}



####################### Análisis de componentes principales -ACP-#################


vars_acp <- c("TCH", "Rendimiento", "TAH", "Pureza", "Población", "Tallos.con.Flor", "Porcentaje", "Altura.Total", 
              "Peso.Total", "Altura.sin.Corcho", "Peso.sin.Corcho", "Entrenudos.totales.por.tallo", "Entrenudos.con.corcho.por.tallo",
              "Grado.de.corcho")

factor_grupo <- "Tratamiento" 
mostrar_puntos_fondo <- TRUE  # TRUE: muestra observaciones | FALSE: solo centroides y flechas


library(FactoMineR)
library(factoextra)
library(dplyr)
library(stringr)

# --- PREPARACIÓN DE DATOS ---
df_acp <- datos %>%
  select(all_of(factor_grupo), all_of(vars_acp)) %>%
  na.omit()

# --- EJECUCIÓN DEL ACP ---
# Usamos el factor como variable suplementaria (index 1)
res.pca <- PCA(df_acp, quali.sup = 1, graph = FALSE)

# --- GENERACIÓN DEL BIPLOT POR CUADRANTES ---
fviz_pca_biplot(res.pca, 
                # 1. Configuración de Individuos (Fondo)
                geom.ind = if(mostrar_puntos_fondo) "point" else "none",
                col.ind = "gray85",      # Color neutro para no distraer
                alpha.ind = 0.4,         # Transparencia
                pointshape = 19, 
                
                # 2. Configuración de Tratamientos (Centroides)
                habillage = 1,           # Colorear por Tratamiento
                addEllipses = TRUE,      # Elipses para ver agrupamiento por cuadrante
                ellipse.level = 0.95,
                mean.point = TRUE,       # Resalta el punto medio del tratamiento
                mean.point.size = 5,
                
                # 3. Configuración de Variables (Flechas)
                col.var = "firebrick3",  # Color contrastante para las flechas
                arrowsize = 0.7,
                
                # 4. Ajustes de Texto y Repelado
                repel = TRUE,            # CRUCIAL: evita que nombres de variables y tratamientos choquen
                labelsize = 4) + 
  
  # Estética de Cuadrantes
  geom_vline(xintercept = 0, linetype = "dotted", color = "black", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", alpha = 0.5) +
  
  theme_minimal() +
  labs(title = "Análisis Biplot: Distribución de Tratamientos y Variables",
       subtitle = "Ubicación por cuadrantes según componentes principales",
       x = paste0("PC1 (", round(res.pca$eig[1,2], 1), "%)"),
       y = paste0("PC2 (", round(res.pca$eig[2,2], 1), "%)"),
       colour = "Tratamiento", fill = "Tratamiento") +
  theme(legend.position = "bottom",
        text = element_text(size = 12)) +
  # Opcional: Acorta nombres de tratamientos en la leyenda si son muy largos
  guides(colour = guide_legend(ncol = 3))





######################################################################################################

# Barneon
# Análisis de las 25 macollas

usar_tercer_factor <- T      # TRUE: muestra facetas por variedad | FALSE: gráfico único
tipo_etiqueta      <- "porcentaje" # "porcentaje": muestra % | "valor": muestra cantidad n
# ==========================================

analisis_tallos <- datos %>%
  mutate(rango_altura = cut(altura, 
                            breaks = c(-Inf, 0.4, 0.8, Inf), 
                            labels = c("Baja", "Mediana", "Alta"))) %>%

  group_by(across(all_of(c(if(usar_tercer_factor) "variedad", "Tratamiento", "rango_altura")))) %>%
  summarise(cantidad = n(), .groups = "drop_last") %>%
  mutate(porcentaje = (cantidad / sum(cantidad)) * 100) %>%
  ungroup()
print(analisis_tallos, n = Inf)

analisis_tallos <- analisis_tallos %>%
  mutate(etiqueta_display = case_when(
    tipo_etiqueta == "porcentaje" ~ paste0(round(porcentaje, 1), "%"),
    tipo_etiqueta == "valor"      ~ as.character(cantidad),
    TRUE ~ ""
  ))

p <- ggplot(analisis_tallos, aes(x = Tratamiento, y = porcentaje, fill = rango_altura)) +
  geom_bar(stat = "identity", position = "fill", color = "black", size = 0.2) + 
  
  geom_text(aes(label = ifelse(porcentaje > 5, etiqueta_display, "")), 
            position = position_fill(vjust = 0.5), 
            size = 3.5, fontface = "bold", color = "black") +
  
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "YlGnBu") + 
  labs(title = "Distribución de altura de tallos",
       subtitle = paste("Visualizando:", tipo_etiqueta, 
                        if(usar_tercer_factor) "| Separado por Variedad" else ""),
       x = "Tratamiento", 
       y = "Proporción Acumulada", 
       fill = "Rango de Altura") +
  theme_bw() + 
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

if(usar_tercer_factor) {
  p <- p + facet_wrap(~ variedad, scales = "free_x") +
    theme(strip.background = element_rect(fill = "grey20"),
          strip.text = element_text(color = "white", face = "bold"))
}

print(p)




# ==========================================================
# SCRIPT COMPLETO: ANDEVA + TUKEY + GRÁFICOS CON MEDIAS
# ==========================================================

library(ggplot2)
library(dplyr)
library(agricolae)
library(scales)
library(patchwork)

# Factores y Variables
factores <- c("distanciamiento", "dosis")
bloque <- "Replica"
respuestas <- c("altura_m", "diametro_cm", "tallo_ha", "tolete_ha")

# Asegurar formatos
for(f in factores) datos[[f]] <- as.factor(datos[[f]])
datos[[bloque]] <- as.factor(datos[[bloque]])
for(v in respuestas) datos[[v]] <- as.numeric(datos[[v]])

# BUCLE PRINCIPAL
for(var in respuestas){
  cat("\nProcesando variable:", var, "\n")
  
  # 1. ANDEVA
  formula_anidada <- paste(factores, collapse="*")
  formula <- as.formula(paste0("`", var, "` ~ ", bloque, " + ", formula_anidada))
  modelo <- aov(formula, data=datos)
  
  # 2. Tukey para Interacción (Necesario para las letras del gráfico)
  datos$int_temp <- interaction(datos[,factores], sep="_")
  mod_int <- aov(as.formula(paste0("`", var, "` ~ int_temp")), data=datos)
  # group=TRUE asegura que 'a' sea la media más alta
  tukey_int <- HSD.test(mod_int, "int_temp", alpha=0.15, group=TRUE)
  
  # 3. Preparar datos para el gráfico de barras
  letras_data <- as.data.frame(tukey_int$groups)
  letras_data$interaccion_completa <- rownames(letras_data)
  
  df_resumen_barras <- datos %>%
    group_by(.data[[factores[1]]], .data[[factores[2]]]) %>%
    summarise(
      MEDIA = mean(.data[[var]], na.rm = TRUE),
      SD = sd(.data[[var]], na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(interaccion_completa = interaction(.data[[factores[1]]], .data[[factores[2]]], sep = "_")) %>%
    left_join(letras_data %>% select(interaccion_completa, groups), by = "interaccion_completa")
  
  # 4. Formato de decimales (0 para ha, 2 para m/cm)
  dec_p <- if(grepl("_ha", var)) 0 else 2
  
  # Espacio para que las letras no choquen
  offset <- max(df_resumen_barras$MEDIA + df_resumen_barras$SD, na.rm = TRUE) * 0.10
  
  # 5. CONSTRUCCIÓN DEL GRÁFICO (Estilo Paper con Medias)
  p_paper <- ggplot(df_resumen_barras, aes(x = .data[[factores[1]]], y = MEDIA, fill = .data[[factores[2]]])) +
    geom_bar(stat = "identity", position = position_dodge(0.9), color = "black", linewidth = 0.4) +
    geom_errorbar(aes(ymin = MEDIA - SD, ymax = MEDIA + SD),
                  width = 0.2, position = position_dodge(0.9), color = "black") +
    
    # ETIQUETA: MEDIA + LETRA
    geom_text(aes(label = paste0(format(round(MEDIA, dec_p), big.mark = ".", decimal.mark = ","), 
                                 "\n", trimws(groups)), 
                  y = MEDIA + SD + offset/2), 
              position = position_dodge(0.9), 
              vjust = 0, 
              size = 3.5, 
              fontface = "bold",
              lineheight = 0.8) +
    
    # Facetamos por la segunda variable para que se vea como pides
    facet_wrap(as.formula(paste("~", factores[2])), labeller = label_both) +
    
    scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ","),
                       expand = expansion(mult = c(0, 0.4))) + # Espacio extra arriba
    scale_fill_brewer(palette = "Paired") + 
    
    labs(title = paste("Efecto en:", toupper(var)),
         subtitle = "Valores sobre barras: Media + Letra Tukey (p < 0.15)",
         x = factores[1], y = var, fill = factores[2]) +
    
    theme_classic() +
    coord_cartesian(clip = "off") + 
    theme(
      strip.background = element_rect(fill = "gray90"),
      strip.text = element_text(face = "bold"),
      legend.position = "top",
      plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
    )
  
  print(p_paper)
  

}


