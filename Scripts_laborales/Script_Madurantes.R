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
################################################################################################


datos <- read_clip_tbl()  

attach(datos)
view(datos)
head(datos)
tail(datos)
names(datos)

# Para cambiar nombres de columnas por si hay algún error

names(datos)[names(datos)=="Rendimiento.Kg.Ton"]<- "Rendimiento"



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
# "altura", "peso", "diametro", "tml"

names(datos)

cols_a_numerica <- intersect(names(datos), c("TCH", "Rndimiento.Kg.Ton", "TAH", "Pureza"))
cols_a_factor <- intersect(names(datos), c("Tratamiento", "Replica", "Descripcion"))

datos <- datos %>%
  mutate(across(all_of(cols_a_numerica), ~as.numeric(as.character(.)))) %>%
  mutate(across(all_of(cols_a_factor), as.factor))

sapply(datos, class)



# Relativización de variables de respuesta 

colnames(datos)
levels(datos$Tratamiento)

nombre_testigo <- "Testigo" 
vars_a_relativizar <- c("TCH", "Rendimiento.Kg.Ton", "TAH", "Pureza") 

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



######################################### ANDEVA ##############################

library(agricolae)
library(car)
library(dplyr)
library(ggplot2)
library(ggpubr)

names(datos)
levels(datos$Tratamiento)

alpha <- 0.15  
tratamiento_col <- "Tratamiento"
nombre_testigo <- "Testigo" 
respuestas_base <- c("TCH", "Rendimiento.Kg.Ton", "TAH", "Pureza")

# Cálculo de la variable solicitada
datos <- datos %>% mutate(`TAH PZA` = TAH * (Pureza / 100))
respuestas_total <- c(respuestas_base, "TAH PZA")

lista_medias <- list()
p_valores <- list() 

# --- 2. BUCLE DE ANÁLISIS Y TABULACIÓN ---
for (var in respuestas_total) {
  datos_clean <- datos %>% filter(!is.na(!!sym(var)))
  
  # Modelo ANDEVA DBCA
  formula_str <- paste0("`", var, "` ~ factor(", tratamiento_col, ") + factor(Replica)")
  modelo <- aov(as.formula(formula_str), data = datos_clean)
  
  # Extraer P-valor
  anova_res <- anova(modelo)
  fila_trat <- grep(tratamiento_col, rownames(anova_res))
  p_valores[[var]] <- anova_res[fila_trat, "Pr(>F)"]
  
  # Medias y Relativos (Proporción, no %)
  df_m <- datos_clean %>%
    group_by(!!sym(tratamiento_col)) %>%
    summarise(mean_val = mean(!!sym(var)), .groups = 'drop') %>%
    rename(Tratamiento = 1)
  
  val_testigo <- df_m$mean_val[df_m$Tratamiento == nombre_testigo]
  df_m <- df_m %>% mutate(relativo = if(length(val_testigo) > 0) mean_val / val_testigo else NA)
  
  colnames(df_m)[2:3] <- c(var, paste0(var, "_rel"))
  lista_medias[[var]] <- df_m
}

# --- 3. GENERACIÓN DE TABLA DE RESUMEN FINAL ---
tabla_unida <- Reduce(function(x, y) merge(x, y, by = "Tratamiento", all = TRUE), lista_medias)
cols_normales <- respuestas_total
cols_relativos <- paste0(respuestas_total, "_rel")

# Reordenar columnas: Normales primero, Relativos después
tabla_resumen <- tabla_unida %>% select(Tratamiento, all_of(cols_normales), all_of(cols_relativos))

# Fila de P-valores
fila_p <- data.frame(Tratamiento = "P-valor (ANDEVA)")
for(v in cols_normales) fila_p[[v]] <- round(p_valores[[v]], 5)
for(r in cols_relativos) fila_p[[r]] <- NA
tabla_final <- rbind(tabla_resumen, fila_p)

# >>> OPCIÓN PARA COPIAR A EXCEL DIRECTAMENTE <<<
# Para Windows:
write.table(tabla_final, "clipboard", sep="\t", row.names=FALSE, dec=",") 
# Nota: Si usas Mac, cambia "clipboard" por pipe("pbcopy")

cat("\n==========================================================\n")
cat("TABLA COPIADA AL PORTAPAPELES. VE A EXCEL Y PEGA (CTRL+V).\n")
cat("==========================================================\n")
print(tabla_final, row.names = FALSE)

# --- 4. GRÁFICO COMBINADO CON ESTÉTICA UNIFICADA ---

# CONFIGURACIÓN DEL GRÁFICO
vars_analisis <- respuestas_total # Aquí puedes filtrar si quieres menos variables
tipo_prueba <- "Fisher"           # Cambia a "Tukey" si prefieres
f_principal <- tratamiento_col

datos_final_grafico <- list()

for (v in vars_analisis) {
  m <- aov(as.formula(paste0("`", v, "` ~ factor(", tratamiento_col, ") + factor(Replica)")), data = datos)
  prueba <- if(tipo_prueba == "Tukey") HSD.test(m, "factor(Tratamiento)", alpha = alpha) else LSD.test(m, "factor(Tratamiento)", alpha = alpha)
  
  df_gr <- prueba$groups
  df_gr[[f_principal]] <- rownames(df_gr)
  
  df_means_se <- data.frame(Tratamiento = rownames(prueba$means), 
                            media_r = prueba$means[,1], 
                            se_r = prueba$means[,2] / sqrt(max(as.numeric(datos$Replica), na.rm=T)))
  
  df_plot_v <- merge(df_means_se, df_gr[, c(f_principal, "groups")], by = f_principal)
  df_plot_v$Variable <- v
  datos_final_grafico[[v]] <- df_plot_v
}

datos_final_grafico <- bind_rows(datos_final_grafico)
colnames(datos_final_grafico)[which(names(datos_final_grafico) == "groups")] <- ".group"

# RENDERIZADO POR PANELES
grupos_graficos <- split(vars_analisis, ceiling(seq_along(vars_analisis) / 4))

for (i in seq_along(grupos_graficos)) {
  datos_plot <- datos_final_grafico %>%
    filter(Variable %in% grupos_graficos[[i]]) %>%
    mutate(Variable = factor(Variable, levels = grupos_graficos[[i]]))
  
  if(nrow(datos_plot) == 0) next
  
  gg <- ggplot(datos_plot, aes(y = reorder(!!sym(f_principal), media_r), x = media_r)) +
    geom_errorbarh(aes(xmin = media_r - se_r, xmax = media_r + se_r), height = 0.2) +
    geom_point(size = 3, color = "black") +
    geom_text(aes(label = sprintf("%.2f %s", media_r, trimws(.group))),
              hjust = -0.1, vjust = -1, size = 4.5) +
    facet_wrap(~ Variable, scales = "free_x") + 
    scale_x_continuous(expand = expansion(mult = c(0.1, 0.5))) +
    labs(x = "Media Aritmética Real ± SE", y = "Tratamiento",
         title = paste("Comparación de Medias Reales (", tipo_prueba, ") - Panel", i)) +
    theme_minimal() +
    theme(strip.text = element_text(size = 13, face = "bold"),
          text = element_text(size = 12)) +
    theme_classic2()
  
  print(gg)
}




#### NIR


alpha <- 0.15  
tratamiento_col <- "Tratamiento"
replica_col     <- "Replica"
nombre_testigo  <- "0 TON"  # Ajustado según tu indicación

# Variables industriales solicitadas
respuestas_base <- c("brix", "pol", "humedad", "pureza")
respuestas_total <- respuestas_base

lista_medias <- list()
p_valores <- list() 

# --- 2. BUCLE DE ANÁLISIS Y TABULACIÓN ---
for (var in respuestas_total) {
  # Limpieza de NAs
  datos_clean <- datos %>% filter(!is.na(!!sym(var)))
  
  # Modelo ANDEVA DBCA (Tratamiento + Bloque/Replica)
  formula_str <- paste0("`", var, "` ~ factor(", tratamiento_col, ") + factor(", replica_col, ")")
  modelo <- aov(as.formula(formula_str), data = datos_clean)
  
  # Extraer P-valor
  anova_res <- anova(modelo)
  fila_trat <- grep(tratamiento_col, rownames(anova_res))
  p_valores[[var]] <- anova_res[fila_trat, "Pr(>F)"]
  
  # Medias y Relativos
  df_m <- datos_clean %>%
    group_by(!!sym(tratamiento_col)) %>%
    summarise(mean_val = mean(!!sym(var)), .groups = 'drop') %>%
    rename(Tratamiento = 1)
  
  val_testigo <- df_m$mean_val[df_m$Tratamiento == nombre_testigo]
  df_m <- df_m %>% mutate(relativo = if(length(val_testigo) > 0) mean_val / val_testigo else NA)
  
  colnames(df_m)[2:3] <- c(var, paste0(var, "_rel"))
  lista_medias[[var]] <- df_m
}

# --- 3. GENERACIÓN DE TABLA DE RESUMEN FINAL ---
tabla_unida <- Reduce(function(x, y) merge(x, y, by = "Tratamiento", all = TRUE), lista_medias)
cols_normales <- respuestas_total
cols_relativos <- paste0(respuestas_total, "_rel")

# Reordenar columnas: Normales primero, Relativos después
tabla_resumen <- tabla_unida %>% select(Tratamiento, all_of(cols_normales), all_of(cols_relativos))

# Fila de P-valores
fila_p <- data.frame(Tratamiento = "P-valor (ANDEVA)")
for(v in cols_normales) fila_p[[v]] <- round(p_valores[[v]], 5)
for(r in cols_relativos) fila_p[[r]] <- NA
tabla_final <- rbind(tabla_resumen, fila_p)

# COPIAR A EXCEL DIRECTAMENTE
write.table(tabla_final, "clipboard", sep="\t", row.names=FALSE, dec=",") 

cat("\n==========================================================\n")
cat("TABLA COPIADA AL PORTAPAPELES (Variables Industriales).\n")
cat("==========================================================\n")
print(tabla_final, row.names = FALSE)

# --- 4. GRÁFICO CON FACETAS POR RÉPLICA ---

# Preparación de datos para el gráfico (Letras de significancia)
datos_final_grafico <- list()

for (v in respuestas_total) {
  m <- aov(as.formula(paste0("`", v, "` ~ factor(", tratamiento_col, ") + factor(", replica_col, ")")), data = datos)
  prueba <- LSD.test(m, paste0("factor(", tratamiento_col, ")"), alpha = alpha)
  
  df_gr <- prueba$groups
  df_gr[[tratamiento_col]] <- rownames(df_gr)
  
  # Medias por Tratamiento y Réplica para las facetas
  df_means_facet <- datos %>%
    group_by(!!sym(tratamiento_col), !!sym(replica_col)) %>%
    summarise(media_r = mean(!!sym(v), na.rm = TRUE),
              se_r = sd(!!sym(v), na.rm = TRUE) / sqrt(n()), .groups = "drop") %>%
    mutate(Variable = v) %>%
    left_join(df_gr[, c(tratamiento_col, "groups")], by = tratamiento_col)
  
  datos_final_grafico[[v]] <- df_means_facet
}

datos_plot_total <- bind_rows(datos_final_grafico)
colnames(datos_plot_total)[which(names(datos_plot_total) == "groups")] <- ".group"

# Renderizado del gráfico facetado
gg <- ggplot(datos_plot_total, aes(y = reorder(!!sym(tratamiento_col), media_r), x = media_r)) +
  geom_errorbarh(aes(xmin = media_r - se_r, xmax = media_r + se_r), height = 0.2) +
  geom_point(aes(color = !!sym(replica_col)), size = 3) +
  geom_text(aes(label = sprintf("%.2f %s", media_r, trimws(.group))),
            hjust = -0.1, vjust = -1, size = 3.5) +
  
  # Facetas: Columnas por Variable, Filas por Réplica
  facet_grid(vars(!!sym(replica_col)), vars(Variable), scales = "free_x") + 
  
  scale_x_continuous(expand = expansion(mult = c(0.1, 0.6))) +
  labs(x = "Media Aritmética Real ± SE", 
       y = "Tratamiento",
       title = "Análisis Industrial por Réplica y Tratamiento",
       color = "Réplica") +
  theme_bw() +
  theme_classic2() +
  theme(strip.text = element_text(size = 10, face = "bold"),
        panel.spacing = unit(1, "lines"),
        legend.position = "bottom")

print(gg)








######### IDD

alpha <- 0.15  
tratamiento_col <- "Tratamiento"
replica_col     <- "Replica"
nombre_testigo  <- "0 TON"  # Asegúrate que este nombre coincida con tu Excel

# Variables morfométricas solicitadas
respuestas_morf <- c("altura", "peso", "diametro", "tml")
respuestas_total <- respuestas_morf

lista_medias <- list()
p_valores <- list() 

# --- 2. BUCLE DE ANÁLISIS (ANOVA + MEDIAS + RELATIVOS) ---
for (var in respuestas_total) {
  if (!var %in% names(datos)) next # Salta si la variable no existe
  
  datos_clean <- datos %>% filter(!is.na(!!sym(var)))
  
  # Modelo ANDEVA DBCA
  formula_str <- paste0("`", var, "` ~ factor(", tratamiento_col, ") + factor(", replica_col, ")")
  modelo <- aov(as.formula(formula_str), data = datos_clean)
  
  # P-valor
  anova_res <- anova(modelo)
  fila_trat <- grep(tratamiento_col, rownames(anova_res))
  p_valores[[var]] <- anova_res[fila_trat, "Pr(>F)"]
  
  # Medias y Cálculo de Relativo respecto al Testigo
  df_m <- datos_clean %>%
    group_by(!!sym(tratamiento_col)) %>%
    summarise(mean_val = mean(!!sym(var)), .groups = 'drop') %>%
    rename(Tratamiento = 1)
  
  val_testigo <- df_m$mean_val[df_m$Tratamiento == nombre_testigo]
  df_m <- df_m %>% mutate(relativo = if(length(val_testigo) > 0) mean_val / val_testigo else NA)
  
  colnames(df_m)[2:3] <- c(var, paste0(var, "_rel"))
  lista_medias[[var]] <- df_m
}

# --- 3. TABLA DE RESUMEN PARA EXCEL ---
tabla_unida <- Reduce(function(x, y) merge(x, y, by = "Tratamiento", all = TRUE), lista_medias)
cols_normales <- respuestas_total
cols_relativos <- paste0(respuestas_total, "_rel")

tabla_resumen <- tabla_unida %>% select(Tratamiento, all_of(cols_normales), all_of(cols_relativos))

# Fila de P-valores al final
fila_p <- data.frame(Tratamiento = "P-valor (ANDEVA)")
for(v in cols_normales) fila_p[[v]] <- round(p_valores[[v]], 5)
for(r in cols_relativos) fila_p[[r]] <- NA
tabla_final_morf <- rbind(tabla_resumen, fila_p)

# Copiar al portapapeles
write.table(tabla_final_morf, "clipboard", sep="\t", row.names=FALSE, dec=",") 

cat("\n--- TABLA MORFOMÉTRICA COPIADA AL PORTAPAPELES ---\n")
print(tabla_final_morf, row.names = FALSE)


# --- CONFIGURACIÓN DEL GRÁFICO ---
vars_analisis <- c("altura", "peso", "diametro", "tml") # Variables morfométricas
tipo_prueba <- "Fisher"           # Opciones: "Fisher" o "Tukey"
f_principal <- tratamiento_col    # "Tratamiento"
facetar_por_replica <- TRUE      # CAMBIA A FALSE PARA VOLVER AL GRÁFICO ORIGINAL

datos_final_grafico <- list()

for (v in vars_analisis) {
  if (!v %in% names(datos)) next
  
  # 1. Modelo y Prueba de Medias (Se mantiene el diseño DBCA)
  m <- aov(as.formula(paste0("`", v, "` ~ factor(", tratamiento_col, ") + factor(Replica)")), data = datos)
  prueba <- if(tipo_prueba == "Tukey") HSD.test(m, "factor(Tratamiento)", alpha = alpha) else LSD.test(m, "factor(Tratamiento)", alpha = alpha)
  
  df_gr <- prueba$groups
  df_gr[[f_principal]] <- rownames(df_gr)
  
  # 2. Lógica de Medias: ¿General o por Réplica?
  if (facetar_por_replica) {
    # Media por cada tratamiento en cada réplica
    df_plot_v <- datos %>%
      group_by(!!sym(f_principal), Replica) %>%
      summarise(media_r = mean(!!sym(v), na.rm = TRUE),
                se_r = sd(!!sym(v), na.rm = TRUE) / sqrt(n()), .groups = "drop") %>%
      left_join(df_gr[, c(f_principal, "groups")], by = f_principal)
  } else {
    # Media general (tu código original)
    df_means_se <- data.frame(Tratamiento = rownames(prueba$means), 
                              media_r = prueba$means[,1], 
                              se_r = prueba$means[,2] / sqrt(max(as.numeric(datos$Replica), na.rm=T)))
    df_plot_v <- merge(df_means_se, df_gr[, c(f_principal, "groups")], by = f_principal)
  }
  
  df_plot_v$Variable <- v
  datos_final_grafico[[v]] <- df_plot_v
}

datos_final_grafico <- bind_rows(datos_final_grafico)
colnames(datos_final_grafico)[which(names(datos_final_grafico) == "groups")] <- ".group"

# --- RENDERIZADO POR PANELES ---
grupos_graficos <- split(vars_analisis, ceiling(seq_along(vars_analisis) / 4))

for (i in seq_along(grupos_graficos)) {
  datos_plot <- datos_final_grafico %>%
    filter(Variable %in% grupos_graficos[[i]]) %>%
    mutate(Variable = factor(Variable, levels = grupos_graficos[[i]]))
  
  if(nrow(datos_plot) == 0) next
  
  gg <- ggplot(datos_plot, aes(y = reorder(!!sym(f_principal), media_r), x = media_r)) +
    geom_errorbarh(aes(xmin = media_r - se_r, xmax = media_r + se_r), height = 0.2) +
    geom_point(size = 3, color = "black") +
    geom_text(aes(label = sprintf("%.2f %s", media_r, trimws(.group))),
              hjust = -0.1, vjust = -1, size = 3.8) +
    
    # Lógica de Facetado Dinámico
    { if(facetar_por_replica) 
      facet_grid(Replica ~ Variable, scales = "free_x") 
      else 
        facet_wrap(~ Variable, scales = "free_x") 
    } +
    
    scale_x_continuous(expand = expansion(mult = c(0.1, 0.6))) +
    labs(x = "Media Aritmética Real ± SE", y = "Tratamiento",
         title = paste("Comparación de Medias Reales (", tipo_prueba, ")"),
         subtitle = if(facetar_por_replica) "Desglosado por Réplica" else "Promedio General") +
    theme_minimal() +
    theme(strip.text = element_text(size = 11, face = "bold"),
          text = element_text(size = 12)) +
    theme_classic2()
  
  print(gg)
}
