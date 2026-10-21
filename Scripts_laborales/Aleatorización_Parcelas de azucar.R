datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")
cat("\n--- Variables detectadas ---\n")
print(names(datos))

datos2 <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")
cat("\n--- Variables detectadas ---\n")
print(names(datos2))

datos3 <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")
cat("\n--- Variables detectadas ---\n")
print(names(datos3))


###################################################




# ==============================================================================
# 1. CARGA DE LIBRERÍAS
# ==============================================================================
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(readr)
library(gridExtra)
library(grid)
library(ggnewscale)

# ==============================================================================
# 2. FUNCIÓN GENERAL CON BLOQUEO POR EQUILIBRIO (DBCA)
# ==============================================================================
generar_ensayo <- function(datos, nombre_finca, tratamientos_vector, 
                           orientacion = "NS", legend_labels = NULL, seed = 123) {
  set.seed(seed)
  nombres <- names(datos)
  
  # --- Detección automática de columnas ---
  col_altura <- case_when(
    "Promed.Altura" %in% nombres ~ "Promed.Altura",
    "Altura" %in% nombres ~ "Altura",
    "ALTURA" %in% nombres ~ "ALTURA",
    TRUE ~ NA_character_
  )
  col_diametro <- case_when(
    "Promed.Diametro" %in% nombres ~ "Promed.Diametro",
    "Diametro" %in% nombres ~ "Diametro",
    "DIAMETRO" %in% nombres ~ "DIAMETRO",
    TRUE ~ NA_character_
  )
  if (is.na(col_altura) | is.na(col_diametro)) stop(paste("Columnas no encontradas en", nombre_finca))
  
  cat("\n>>> Procesando:", nombre_finca, "| Orientación:", orientacion, "\n")
  
  # --- Limpieza ---
  datos_limpios <- datos %>%
    mutate(
      No.surco = as.numeric(as.character(No.surco)),
      Altura_Num = parse_number(as.character(.data[[col_altura]])),
      Diametro_Num = parse_number(as.character(.data[[col_diametro]]))
    ) %>%
    filter(!is.na(No.surco), !is.na(Altura_Num), !is.na(Diametro_Num))
  
  # --- PROMEDIOS POR NÚMERO DE SURCO ---
  datos_resumen <- datos_limpios %>%
    group_by(No.surco) %>%
    summarise(Altura = mean(Altura_Num, na.rm = TRUE),
              Diametro = mean(Diametro_Num, na.rm = TRUE),
              .groups = 'drop') %>%
    arrange(No.surco) %>%
    mutate(
      Z_Altura = as.numeric(scale(Altura)),
      Z_Diametro = as.numeric(scale(Diametro)),
      Indice_Equilibrio = Z_Altura + Z_Diametro
    )
  
  # ==========================================================================
  # BLOQUES POR TERTILES DE EQUILIBRIO (Alto, Medio, Bajo)
  # ==========================================================================
  n_surcos <- nrow(datos_resumen)
  n_alto  <- ceiling(n_surcos / 3)
  n_medio <- ceiling((n_surcos - n_alto) / 2)
  n_bajo  <- n_surcos - n_alto - n_medio
  
  datos_resumen <- datos_resumen %>%
    arrange(desc(Indice_Equilibrio)) %>%
    mutate(Bloque = c(rep("Alto", n_alto), rep("Medio", n_medio), rep("Bajo", n_bajo))) %>%
    arrange(No.surco) %>%
    mutate(Bloque = factor(Bloque, levels = c("Alto", "Medio", "Bajo")))
  
  # ==========================================================================
  # ASIGNACIÓN DE TRATAMIENTOS POR BLOQUE
  # ==========================================================================
  trat_base <- setdiff(unique(tratamientos_vector), "T1")
  n_T1 <- sum(tratamientos_vector == "T1")
  
  bloque_sizes <- datos_resumen %>%
    group_by(Bloque) %>%
    summarise(n_surcos = n(), n_units = n() * 2, .groups = 'drop') %>%
    arrange(Bloque)
  
  largest_block <- as.character(bloque_sizes$Bloque[which.max(bloque_sizes$n_units)])
  
  asignaciones <- list()
  for (i in 1:nrow(bloque_sizes)) {
    b <- as.character(bloque_sizes$Bloque[i])
    n_units <- bloque_sizes$n_units[i]
    
    pool <- trat_base
    n_extra <- n_units - length(pool)
    
    if (n_extra > 0) {
      if (b == largest_block && n_extra <= n_T1) {
        pool <- c(pool, rep("T1", n_extra))
      } else {
        pool <- c(pool, sample(trat_base, n_extra, replace = TRUE))
      }
    }
    asignaciones[[b]] <- sample(pool)
  }
  
  # Asignar tratamientos a cada unidad, manteniendo orden 1-10
  diseno_list <- list()
  for (i in 1:nrow(datos_resumen)) {
    surco <- datos_resumen$No.surco[i]
    b <- as.character(datos_resumen$Bloque[i])
    pool_b <- asignaciones[[b]]
    trat_1 <- pool_b[1]
    trat_2 <- pool_b[2]
    asignaciones[[b]] <- pool_b[-(1:2)]
    
    diseno_list[[i]] <- data.frame(
      No.surco = surco,
      Bloque = b,
      Pos_temp = c("A", "B"),
      Tratamiento = c(trat_1, trat_2),
      Altura = datos_resumen$Altura[i],
      Diametro = datos_resumen$Diametro[i],
      Indice_Equilibrio = datos_resumen$Indice_Equilibrio[i]
    )
  }
  diseno_largo <- bind_rows(diseno_list)
  
  if (orientacion == "NS") {
    diseno_largo <- diseno_largo %>%
      mutate(Posicion = ifelse(Pos_temp == "A", "Sur", "Norte")) %>%
      select(-Pos_temp)
  } else {
    diseno_largo <- diseno_largo %>%
      mutate(Posicion = ifelse(Pos_temp == "A", "Oeste", "Este")) %>%
      select(-Pos_temp)
  }
  
  diseno_largo <- diseno_largo %>%
    mutate(Etiqueta_Valores = paste0("Alt: ", round(Altura, 2), "\nDiam: ", round(Diametro, 2)))
  
  # ==========================================================================
  # TABLA DE VALORES
  # ==========================================================================
  tabla_final <- diseno_largo %>%
    select(No.surco, Bloque, Altura, Diametro, Posicion, Tratamiento) %>%
    arrange(No.surco, Posicion)
  
  cat("\n--- TABLA:", toupper(nombre_finca), "---\n")
  print(as.data.frame(tabla_final), row.names = FALSE)
  
  # ==========================================================================
  # COORDENADAS  (NS: S1 a la DERECHA, S10 a la IZQUIERDA)
  # ==========================================================================
  if (orientacion == "NS") {
    # x_plot: posición gráfica. S1 -> 10 (derecha), S10 -> 1 (izquierda)
    croquis_data <- diseno_largo %>%
      mutate(
        x_plot = 11 - No.surco,
        xmin = x_plot - 0.5, xmax = x_plot + 0.5,
        ymin = ifelse(Posicion == "Sur", 0, 1),
        ymax = ifelse(Posicion == "Sur", 1, 2),
        x_centro = x_plot,
        y_centro = ifelse(Posicion == "Sur", 0.5, 1.5)
      )
    # Etiquetas del eje X: de izquierda a derecha van S10, S9, ..., S1
    etiquetas_x <- paste0("S", 10:1)
  } else {
    # EW: S1 arriba, S10 abajo (sin cambios)
    croquis_data <- diseno_largo %>%
      mutate(
        xmin = ifelse(Posicion == "Oeste", 0, 1),
        xmax = ifelse(Posicion == "Oeste", 1, 2),
        ymin = 10 - No.surco,
        ymax = 11 - No.surco,
        x_centro = ifelse(Posicion == "Oeste", 0.5, 1.5),
        y_centro = 10.5 - No.surco
      )
  }
  
  colores_base <- c("T1" = "#E41A1C", "T2" = "#377EB8", "T3" = "#4DAF4A", 
                    "T4" = "#984EA3", "T5" = "#FF7F00", "T6" = "#FFFF33",
                    "T7" = "#00CED1", "Pendiente" = "#CCCCCC")
  colores_bloque <- c("Alto" = "#FDDBC7", "Medio" = "#D1E5F0", "Bajo" = "#F4A582")
  
  trat_names <- unique(tratamientos_vector)
  
  # ==========================================================================
  # CROQUIS 1: MAPA DE TRATAMIENTOS
  # ==========================================================================
  if (orientacion == "NS") {
    mapa_campo <- ggplot(croquis_data) +
      # Barra de bloques inferior (usa x_plot)
      geom_rect(aes(xmin = x_plot - 0.5, xmax = x_plot + 0.5, 
                    ymin = -0.9, ymax = -0.3, fill = Bloque),
                alpha = 0.85, color = "white", size = 0.5, show.legend = FALSE) +
      geom_text(aes(x = x_plot, y = -0.6, label = Bloque), 
                size = 3.5, fontface = "bold", color = "gray10") +
      # Celdas de tratamientos
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Tratamiento),
                color = "white", size = 1.2) +
      geom_text(aes(x = x_centro, y = y_centro, label = Tratamiento), fontface = "bold", size = 7) +
      # Títulos
      annotate("text", x = 5.5, y = 3.4, label = toupper(nombre_finca), size = 12, fontface = "bold", color = "#1B4F72") +
      annotate("text", x = 5.5, y = 2.85, label = "PARCELA", size = 11, fontface = "bold") +
      annotate("text", x = 5.5, y = -1.7, label = "CASCO", size = 11, fontface = "bold") +
      annotate("text", x = 5.5, y = -2.3, label = "Bloques por Equilibrio: Alto / Medio / Bajo", 
               size = 5, fontface = "italic", color = "gray30") +
      annotate("text", x = -0.8, y = 1.5, label = "Norte", size = 7, fontface = "bold") +
      annotate("text", x = -0.8, y = 0.5, label = "Sur", size = 7, fontface = "bold") +
      annotate("text", x = -1.8, y = 1.0, label = "Dirección", size = 7, angle = 90, fontface = "bold") +
      annotate("segment", x = 11.2, xend = 11.2, y = 1.5, yend = 2.7, 
               arrow = arrow(length = unit(0.4, "cm")), size = 1.4) +
      annotate("text", x = 11.2, y = 2.95, label = "N", size = 8, fontface = "bold") +
      scale_x_continuous(breaks = 1:10, labels = etiquetas_x, position = "bottom") +
      scale_fill_manual(values = c(colores_base, colores_bloque),
                        breaks = trat_names, labels = legend_labels) +
      theme_void() +
      theme(legend.position = "right",
            legend.title = element_text(face = "bold", size = 13),
            legend.text = element_text(size = 10),
            legend.key.size = unit(0.9, "cm"),
            plot.margin = margin(1, 2, 1, 2, "cm"),
            axis.text.x = element_text(size = 11, face = "bold", margin = margin(t = 5))) +
      labs(fill = "Tratamientos")
  } else {
    bloques_orden <- datos_resumen %>% arrange(No.surco) %>% pull(Bloque)
    mapa_campo <- ggplot(croquis_data) +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Tratamiento),
                color = "white", size = 1.2) +
      geom_text(aes(x = x_centro, y = y_centro, label = Tratamiento), fontface = "bold", size = 7) +
      annotate("text", x = -0.7, y = 10.5 - (1:10), 
               label = as.character(bloques_orden), 
               size = 3.5, fontface = "bold", color = "gray10", hjust = 1) +
      annotate("text", x = 1, y = 12.4, label = toupper(nombre_finca), size = 12, fontface = "bold", color = "#1B4F72") +
      annotate("text", x = 1, y = 11.85, label = "PARCELA", size = 11, fontface = "bold") +
      annotate("text", x = 1, y = -1.4, label = "CASCO", size = 11, fontface = "bold") +
      annotate("text", x = 0.5, y = -0.85, label = "Oeste", size = 7, fontface = "bold") +
      annotate("text", x = 1.5, y = -0.85, label = "Este",  size = 7, fontface = "bold") +
      annotate("segment", x = 3.6, xend = 3.6, y = 10, yend = 11.5, 
               arrow = arrow(length = unit(0.4, "cm")), size = 1.4) +
      annotate("text", x = 3.6, y = 11.8, label = "N", size = 8, fontface = "bold") +
      scale_y_continuous(breaks = 0.5:9.5, labels = paste0("S", 10:1), position = "left") +
      scale_x_continuous(breaks = c(0.5, 1.5), labels = c("Oeste", "Este"), position = "top") +
      scale_fill_manual(values = c(colores_base, colores_bloque),
                        breaks = trat_names, labels = legend_labels) +
      theme_void() +
      theme(legend.position = "right",
            legend.title = element_text(face = "bold", size = 13),
            legend.text = element_text(size = 10),
            legend.key.size = unit(0.9, "cm"),
            plot.margin = margin(1, 2, 1, 2, "cm"),
            axis.text.y = element_text(size = 10, face = "bold", margin = margin(r = 5)),
            axis.text.x.top = element_text(size = 10, face = "bold", margin = margin(b = 5))) +
      labs(fill = "Tratamientos")
  }
  
  # ==========================================================================
  # CROQUIS 2: MAPA DE CALOR (con ggnewscale)
  # ==========================================================================
  if (orientacion == "NS") {
    mapa_calor <- ggplot(croquis_data) +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                    fill = Indice_Equilibrio),
                color = "white", size = 1.5) +
      geom_text(aes(x = x_centro, y = y_centro, label = Etiqueta_Valores),
                fontface = "bold", size = 4.5, color = "white", lineheight = 1.1) +
      scale_fill_viridis_c(option = "plasma", direction = -1, 
                           name = "Índice de\nEquilibrio", na.value = "gray50") +
      new_scale_fill() +
      geom_rect(aes(xmin = x_plot - 0.5, xmax = x_plot + 0.5, 
                    ymin = -0.9, ymax = -0.3, fill = Bloque),
                alpha = 0.85, color = "white", size = 0.5, show.legend = FALSE) +
      geom_text(aes(x = x_plot, y = -0.6, label = Bloque), 
                size = 3.5, fontface = "bold", color = "gray10") +
      scale_fill_manual(values = colores_bloque) +
      annotate("text", x = 5.5, y = 3.4, label = toupper(nombre_finca), 
               size = 12, fontface = "bold", color = "#1B4F72") +
      annotate("text", x = 5.5, y = 2.85, label = "PARCELA", size = 11, fontface = "bold") +
      annotate("text", x = 5.5, y = -1.7, label = "CASCO", size = 11, fontface = "bold") +
      annotate("text", x = 5.5, y = -2.3, 
               label = "Bloques por Equilibrio: Alto / Medio / Bajo", 
               size = 5, fontface = "italic", color = "gray30") +
      annotate("text", x = -0.8, y = 1.5, label = "Norte", size = 7, fontface = "bold") +
      annotate("text", x = -0.8, y = 0.5, label = "Sur", size = 7, fontface = "bold") +
      annotate("text", x = -1.8, y = 1.0, label = "Dirección", size = 7, 
               angle = 90, fontface = "bold") +
      annotate("segment", x = 11.2, xend = 11.2, y = 1.5, yend = 2.7, 
               arrow = arrow(length = unit(0.4, "cm")), size = 1.4) +
      annotate("text", x = 11.2, y = 2.95, label = "N", size = 8, fontface = "bold") +
      scale_x_continuous(breaks = 1:10, labels = etiquetas_x, position = "bottom") +
      theme_void() +
      theme(legend.position = "right",
            legend.title = element_text(face = "bold", size = 13),
            legend.text = element_text(size = 11),
            legend.key.size = unit(0.9, "cm"),
            plot.margin = margin(1, 2, 1, 2, "cm"),
            axis.text.x = element_text(size = 11, face = "bold", margin = margin(t = 5)))
  } else {
    mapa_calor <- ggplot(croquis_data) +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                    fill = Indice_Equilibrio),
                color = "white", size = 1.5) +
      geom_text(aes(x = x_centro, y = y_centro, label = Etiqueta_Valores),
                fontface = "bold", size = 4.5, color = "white", lineheight = 1.1) +
      scale_fill_viridis_c(option = "plasma", direction = -1, 
                           name = "Índice de\nEquilibrio", na.value = "gray50") +
      new_scale_fill() +
      geom_rect(aes(xmin = -1.6, xmax = -1.1, ymin = 10 - No.surco, 
                    ymax = 11 - No.surco, fill = Bloque),
                alpha = 0.85, color = "white", size = 0.5, show.legend = FALSE) +
      geom_text(aes(x = -1.35, y = 10.5 - No.surco, label = Bloque), 
                size = 3.2, fontface = "bold", color = "gray10", angle = 90) +
      scale_fill_manual(values = colores_bloque) +
      annotate("text", x = 1, y = 12.4, label = toupper(nombre_finca), 
               size = 12, fontface = "bold", color = "#1B4F72") +
      annotate("text", x = 1, y = 11.85, label = "PARCELA", size = 11, fontface = "bold") +
      annotate("text", x = 1, y = -1.4, label = "CASCO", size = 11, fontface = "bold") +
      annotate("text", x = 0.5, y = -0.85, label = "Oeste", size = 7, fontface = "bold") +
      annotate("text", x = 1.5, y = -0.85, label = "Este",  size = 7, fontface = "bold") +
      annotate("segment", x = 3.6, xend = 3.6, y = 10, yend = 11.5, 
               arrow = arrow(length = unit(0.4, "cm")), size = 1.4) +
      annotate("text", x = 3.6, y = 11.8, label = "N", size = 8, fontface = "bold") +
      scale_y_continuous(breaks = 0.5:9.5, labels = paste0("S", 10:1), position = "left") +
      scale_x_continuous(breaks = c(0.5, 1.5), labels = c("Oeste", "Este"), position = "top") +
      theme_void() +
      theme(legend.position = "right",
            legend.title = element_text(face = "bold", size = 13),
            legend.text = element_text(size = 11),
            legend.key.size = unit(0.9, "cm"),
            plot.margin = margin(1, 2, 1, 2, "cm"),
            axis.text.y = element_text(size = 10, face = "bold", margin = margin(r = 5)),
            axis.text.x.top = element_text(size = 10, face = "bold", margin = margin(b = 5)))
  }
  
  return(list(tabla = tabla_final, mapa_campo = mapa_campo, mapa_calor = mapa_calor))
}

# ==============================================================================
# 3. VECTORES DE TRATAMIENTOS (T1 = Testigo absoluto)
# ==============================================================================
trat_zulia   <- c(rep("T1", 2), rep("T2", 3), rep("T3", 3), rep("T4", 3), 
                  rep("T5", 3), rep("T6", 3), rep("T7", 3))
trat_balsamo <- c(rep("T1", 2), rep("T2", 3), rep("T3", 3), rep("T4", 3), 
                  rep("T5", 3), rep("T6", 3), rep("T7", 3))
trat_limones <- c(rep("T1", 3), rep("T2", 3), rep("T3", 3), 
                  rep("T4", 3), rep("T5", 3), rep("T6", 3))

# ==============================================================================
# 4. ETIQUETAS DE LEYENDA
# ==============================================================================
labels_zulia <- c(
  "T1" = "T1 = Testigo absoluto",
  "T2" = "T2 = Premadurante 2019  -Glifosato+Boro 0 0",
  "T3" = "T3 = Premadurante 2019  +Glifosato+Boro 9 8",
  "T4" = "T4 = Premadurante 2019  +Glifosato+Boro 8 8",
  "T5" = "T5 = Premadurante 2019  +Glifosato+Boro 7 8",
  "T6" = "T6 = Premadurante 2019  +Glifosato+Boro 6 8",
  "T7" = "T7 = Premadurante 2019  +Glifosato+Boro 5 8"
)
labels_balsamo <- c(
  "T1" = "T1 = Testigo absoluto",
  "T2" = "T2 = Premadurante 2019  -Glifosato+Boro 0 0",
  "T3" = "T3 = Premadurante 2019  +Glifosato+Boro 9 8",
  "T4" = "T4 = Premadurante 2019  +Glifosato+Boro 8 8",
  "T5" = "T5 = Premadurante 2019  +Glifosato+Boro 7 8",
  "T6" = "T6 = Premadurante 2019  +Glifosato+Boro 6 8",
  "T7" = "T7 = Premadurante 2019  +Glifosato+Boro 5 11"
)
labels_limones <- c(
  "T1" = "T1 = Testigo absoluto",
  "T2" = "T2 = Premadurante 50%  -Glifosato 0 0",
  "T3" = "T3 = Premadurante 100%  -Glifosato 0 0",
  "T4" = "T4 = Premadurante 50%  +Glifosato 9 8",
  "T5" = "T5 = Premadurante 100%  +Glifosato 9 11",
  "T6" = "T6 = Sin premadurante  +Glifosato 7 8"
)

# ==============================================================================
# 5. EJECUCIÓN
# ==============================================================================
res_zulia   <- generar_ensayo(datos,  "Finca Zulia 201",   trat_zulia,   
                              orientacion = "NS", legend_labels = labels_zulia,   seed = 123)
res_balsamo <- generar_ensayo(datos2, "Finca Bálsamo 301", trat_balsamo, 
                              orientacion = "EW", legend_labels = labels_balsamo, seed = 456)
res_limones <- generar_ensayo(datos3, "Finca Limones 110", trat_limones, 
                              orientacion = "NS", legend_labels = labels_limones, seed = 789)

# ==============================================================================
# 6. PDF DE ALTA CALIDAD
# ==============================================================================
pdf("Croquis_Fincas.pdf", width = 22, height = 14, onefile = TRUE)

imprimir_pagina <- function(plot, titulo) {
  grid.newpage()
  grid.text(titulo, x = 0.5, y = 0.98, gp = gpar(fontsize = 18, fontface = "bold"))
  print(plot, vp = viewport(x = 0.5, y = 0.48, width = 0.98, height = 0.9))
}

imprimir_pagina(res_zulia$mapa_campo,   "Finca Zulia 201 - Croquis de Tratamientos")
imprimir_pagina(res_zulia$mapa_calor,   "Finca Zulia 201 - Mapa de Calor")
imprimir_pagina(res_balsamo$mapa_campo, "Finca Bálsamo 301 - Croquis de Tratamientos (Este-Oeste)")
imprimir_pagina(res_balsamo$mapa_calor, "Finca Bálsamo 301 - Mapa de Calor (Este-Oeste)")
imprimir_pagina(res_limones$mapa_campo, "Finca Limones 110 - Croquis de Tratamientos")
imprimir_pagina(res_limones$mapa_calor, "Finca Limones 110 - Mapa de Calor")

dibujar_tabla <- function(tabla, titulo) {
  grid.newpage()
  grid.text(titulo, x = 0.5, y = 0.96, gp = gpar(fontsize = 18, fontface = "bold"))
  tg <- tableGrob(tabla, rows = NULL, 
                  theme = ttheme_minimal(base_size = 12,
                                         core = list(fg_params = list(fontface = "bold"))))
  grid.draw(tg)
}
dibujar_tabla(res_zulia$tabla,   "Tabla de Valores - Finca Zulia 201")
dibujar_tabla(res_balsamo$tabla, "Tabla de Valores - Finca Bálsamo 301")
dibujar_tabla(res_limones$tabla, "Tabla de Valores - Finca Limones 110")

dev.off()
cat("\n[OK] PDF generado: Croquis_Fincas.pdf\n")























####### Correcto: 






# ==============================================================================
# 1. CARGA DE LIBRERÍAS
# ==============================================================================
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(readr)
library(gridExtra)
library(grid)
library(ggnewscale)

# ==============================================================================
# 2. FUNCIÓN GENERAL
# ==============================================================================
generar_ensayo <- function(datos, nombre_finca, tratamientos_vector, 
                           orientacion = "NS", legend_labels = NULL, seed = 123) {
  set.seed(seed)
  nombres <- names(datos)
  
  # --- Detección de columnas (case-insensitive para Lado) ---
  col_altura <- case_when(
    "Promed.Altura" %in% nombres ~ "Promed.Altura",
    "Altura" %in% nombres ~ "Altura",
    "ALTURA" %in% nombres ~ "ALTURA",
    TRUE ~ NA_character_)
  col_diametro <- case_when(
    "Promed.Diametro" %in% nombres ~ "Promed.Diametro",
    "Diametro" %in% nombres ~ "Diametro",
    "DIAMETRO" %in% nombres ~ "DIAMETRO",
    TRUE ~ NA_character_)
  col_lado <- nombres[tolower(nombres) == "lado"][1]
  
  if (is.na(col_altura) | is.na(col_diametro)) stop(paste("Columnas no encontradas en", nombre_finca))
  if (is.na(col_lado)) stop(paste("No se encontró columna 'Lado' en", nombre_finca))
  
  cat("\n>>> Procesando:", nombre_finca, "| Orientación:", orientacion, "\n")
  cat("    Columnas -> Altura:", col_altura, "| Diámetro:", col_diametro, "| Lado:", col_lado, "\n")
  
  # --- Limpieza ---
  datos_limpios <- datos %>%
    mutate(
      No.surco = as.numeric(as.character(No.surco)),
      Lado = as.character(.data[[col_lado]]),
      Altura_Num = parse_number(as.character(.data[[col_altura]])),
      Diametro_Num = parse_number(as.character(.data[[col_diametro]]))
    ) %>%
    filter(!is.na(No.surco), !is.na(Altura_Num), !is.na(Diametro_Num))
  
  # ==========================================================================
  # AVERAGES POR (No.surco, Lado) -> 20 UNIDADES EXPERIMENTALES
  # ==========================================================================
  datos_unidades <- datos_limpios %>%
    group_by(No.surco, Lado) %>%
    summarise(Altura = mean(Altura_Num, na.rm = TRUE),
              Diametro = mean(Diametro_Num, na.rm = TRUE),
              .groups = 'drop') %>%
    mutate(
      Z_Altura = as.numeric(scale(Altura)),
      Z_Diametro = as.numeric(scale(Diametro)),
      Indice_Equilibrio = Z_Altura + Z_Diametro
    )
  
  cat("    Unidades experimentales detectadas:", nrow(datos_unidades), "\n")
  
  # ==========================================================================
  # BLOQUES POR TERTILES SOBRE LAS 20 UNIDADES
  # ==========================================================================
  n_units <- nrow(datos_unidades)
  n_alto  <- ceiling(n_units / 3)
  n_medio <- ceiling((n_units - n_alto) / 2)
  n_bajo  <- n_units - n_alto - n_medio
  
  datos_unidades <- datos_unidades %>%
    arrange(desc(Indice_Equilibrio)) %>%
    mutate(
      Bloque = c(rep("Alto", n_alto), rep("Medio", n_medio), rep("Bajo", n_bajo)),
      Bloque = factor(Bloque, levels = c("Alto", "Medio", "Bajo"))
    ) %>%
    arrange(No.surco, Lado)
  
  # ==========================================================================
  # ASIGNACIÓN DE TRATAMIENTOS POR BLOQUE
  # (1 réplica de cada tratamiento base por bloque, extras en los bloques más grandes)
  # ==========================================================================
  trat_counts <- table(tratamientos_vector)
  base_treats <- names(trat_counts[trat_counts >= 3])   # tienen al menos 3 reps
  extra_treats <- unlist(lapply(names(trat_counts), function(t) {
    n_extra <- trat_counts[t] - 3
    if (n_extra > 0) rep(t, n_extra) else character(0)
  }))
  # Añadir Pendientes si faltan unidades
  n_total <- n_units
  n_empty <- n_total - length(tratamientos_vector)
  if (n_empty > 0) extra_treats <- c(extra_treats, rep("Pendiente", n_empty))
  
  bloque_sizes <- datos_unidades %>%
    count(Bloque) %>%
    arrange(Bloque) %>%
    mutate(n_extra = n - length(base_treats))
  
  extra_pool <- sample(extra_treats)
  asignaciones <- list()
  for (i in 1:nrow(bloque_sizes)) {
    b <- as.character(bloque_sizes$Bloque[i])
    n_extra <- bloque_sizes$n_extra[i]
    pool <- sample(base_treats)
    if (n_extra > 0) {
      extras <- extra_pool[1:n_extra]
      extra_pool <- extra_pool[-(1:n_extra)]
      pool <- c(pool, extras)
    }
    asignaciones[[b]] <- sample(pool)
  }
  
  # Asignar a cada unidad dentro del bloque (aleatoriamente)
  datos_unidades$Tratamiento <- NA_character_
  for (b in names(asignaciones)) {
    idx <- which(datos_unidades$Bloque == b)
    idx_shuffled <- sample(idx)
    datos_unidades$Tratamiento[idx_shuffled] <- asignaciones[[b]]
  }
  
  datos_unidades <- datos_unidades %>%
    mutate(Etiqueta_Valores = paste0("Alt: ", round(Altura, 2), "\nDiam: ", round(Diametro, 2)))
  
  # ==========================================================================
  # TABLA FINAL
  # ==========================================================================
  tabla_final <- datos_unidades %>%
    select(No.surco, Lado, Bloque, Altura, Diametro, Tratamiento) %>%
    arrange(No.surco, Lado)
  
  cat("\n--- TABLA:", toupper(nombre_finca), "---\n")
  print(as.data.frame(tabla_final), row.names = FALSE)
  
  # ==========================================================================
  # COORDENADAS (S1 a la DERECHA en NS)
  # ==========================================================================
  if (orientacion == "NS") {
    croquis_data <- datos_unidades %>%
      mutate(
        x_plot = 11 - No.surco,
        xmin = x_plot - 0.5, xmax = x_plot + 0.5,
        is_norte = tolower(Lado) == "norte",
        ymin = ifelse(is_norte, 1, 0),
        ymax = ifelse(is_norte, 2, 1),
        x_centro = x_plot,
        y_centro = ifelse(is_norte, 1.5, 0.5)
      )
    etiquetas_x <- paste0("S", 10:1)
  } else {
    croquis_data <- datos_unidades %>%
      mutate(
        is_este = tolower(Lado) == "este",
        xmin = ifelse(is_este, 1, 0),
        xmax = ifelse(is_este, 2, 1),
        x_centro = ifelse(is_este, 1.5, 0.5),
        ymin = 10 - No.surco,
        ymax = 11 - No.surco,
        y_centro = 10.5 - No.surco
      )
  }
  
  colores_base <- c("T1" = "#E41A1C", "T2" = "#377EB8", "T3" = "#4DAF4A", 
                    "T4" = "#984EA3", "T5" = "#FF7F00", "T6" = "#FFFF33",
                    "T7" = "#00CED1", "Pendiente" = "#CCCCCC")
  colores_bloque <- c("Alto" = "#B2182B", "Medio" = "#F4A582", "Bajo" = "#92C5DE")
  
  trat_names <- unique(tratamientos_vector)
  
  # ==========================================================================
  # CROQUIS 1: MAPA DE TRATAMIENTOS
  # ==========================================================================
  if (orientacion == "NS") {
    mapa_campo <- ggplot(croquis_data) +
      # Celdas de tratamientos
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Tratamiento),
                color = "white", size = 1.5) +
      # Pequeña franja lateral con el bloque
      geom_rect(aes(xmin = ifelse(is_norte, xmin, xmax - 0.1), 
                    xmax = ifelse(is_norte, xmin + 0.1, xmax),
                    ymin = ymin, ymax = ymax, fill = Bloque),
                color = NA, show.legend = FALSE) +
      scale_fill_manual(values = c(colores_base, colores_bloque)) +
      new_scale_fill() +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Tratamiento),
                color = "white", size = 1.5) +
      geom_text(aes(x = x_centro, y = y_centro, label = Tratamiento), 
                fontface = "bold", size = 7) +
      geom_text(aes(x = x_centro, y = y_centro - 0.35, label = Bloque), 
                size = 2.8, fontface = "italic", color = "gray20") +
      # Títulos
      annotate("text", x = 5.5, y = 3.2, label = toupper(nombre_finca), 
               size = 12, fontface = "bold", color = "#1B4F72") +
      annotate("text", x = 5.5, y = 2.7, label = "PARCELA", size = 11, fontface = "bold") +
      annotate("text", x = 5.5, y = -1.5, label = "CASCO", size = 11, fontface = "bold") +
      annotate("text", x = -0.8, y = 1.5, label = "Norte", size = 7, fontface = "bold") +
      annotate("text", x = -0.8, y = 0.5, label = "Sur", size = 7, fontface = "bold") +
      annotate("text", x = -1.8, y = 1.0, label = "Dirección", size = 7, angle = 90, fontface = "bold") +
      annotate("segment", x = 11.2, xend = 11.2, y = 1.5, yend = 2.7, 
               arrow = arrow(length = unit(0.4, "cm")), size = 1.4) +
      annotate("text", x = 11.2, y = 2.95, label = "N", size = 8, fontface = "bold") +
      scale_x_continuous(breaks = 1:10, labels = etiquetas_x, position = "bottom") +
      scale_fill_manual(values = c(colores_base, colores_bloque),
                        breaks = trat_names, labels = legend_labels) +
      theme_void() +
      theme(legend.position = "right",
            legend.title = element_text(face = "bold", size = 13),
            legend.text = element_text(size = 10),
            legend.key.size = unit(0.9, "cm"),
            plot.margin = margin(1, 2, 1, 2, "cm"),
            axis.text.x = element_text(size = 11, face = "bold", margin = margin(t = 5))) +
      labs(fill = "Tratamientos")
  } else {
    mapa_campo <- ggplot(croquis_data) +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Tratamiento),
                color = "white", size = 1.5) +
      geom_text(aes(x = x_centro, y = y_centro, label = Tratamiento), fontface = "bold", size = 7) +
      geom_text(aes(x = x_centro, y = y_centro - 0.3, label = Bloque), 
                size = 2.8, fontface = "italic", color = "gray20") +
      annotate("text", x = 1, y = 12.2, label = toupper(nombre_finca), 
               size = 12, fontface = "bold", color = "#1B4F72") +
      annotate("text", x = 1, y = 11.7, label = "PARCELA", size = 11, fontface = "bold") +
      annotate("text", x = 1, y = -1.4, label = "CASCO", size = 11, fontface = "bold") +
      annotate("text", x = 0.5, y = -0.85, label = "Oeste", size = 7, fontface = "bold") +
      annotate("text", x = 1.5, y = -0.85, label = "Este",  size = 7, fontface = "bold") +
      annotate("segment", x = 3.6, xend = 3.6, y = 10, yend = 11.5, 
               arrow = arrow(length = unit(0.4, "cm")), size = 1.4) +
      annotate("text", x = 3.6, y = 11.8, label = "N", size = 8, fontface = "bold") +
      scale_y_continuous(breaks = 0.5:9.5, labels = paste0("S", 1:10), position = "left") +
      scale_x_continuous(breaks = c(0.5, 1.5), labels = c("Oeste", "Este"), position = "top") +
      scale_fill_manual(values = c(colores_base, colores_bloque),
                        breaks = trat_names, labels = legend_labels) +
      theme_void() +
      theme(legend.position = "right",
            legend.title = element_text(face = "bold", size = 13),
            legend.text = element_text(size = 10),
            legend.key.size = unit(0.9, "cm"),
            plot.margin = margin(1, 2, 1, 2, "cm"),
            axis.text.y = element_text(size = 10, face = "bold", margin = margin(r = 5)),
            axis.text.x.top = element_text(size = 10, face = "bold", margin = margin(b = 5))) +
      labs(fill = "Tratamientos")
  }
  
  # ==========================================================================
  # CROQUIS 2: MAPA DE CALOR (valores por unidad)
  # ==========================================================================
  if (orientacion == "NS") {
    mapa_calor <- ggplot(croquis_data) +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                    fill = Indice_Equilibrio),
                color = "white", size = 1.5) +
      geom_text(aes(x = x_centro, y = y_centro, label = Etiqueta_Valores),
                fontface = "bold", size = 4.5, color = "white", lineheight = 1.1) +
      geom_text(aes(x = x_centro, y = y_centro - 0.35, label = Bloque),
                size = 2.8, fontface = "italic", color = "white") +
      scale_fill_viridis_c(option = "plasma", direction = -1, 
                           name = "Índice de\nEquilibrio", na.value = "gray50") +
      annotate("text", x = 5.5, y = 3.2, label = toupper(nombre_finca), 
               size = 12, fontface = "bold", color = "#1B4F72") +
      annotate("text", x = 5.5, y = 2.7, label = "PARCELA", size = 11, fontface = "bold") +
      annotate("text", x = 5.5, y = -1.5, label = "CASCO", size = 11, fontface = "bold") +
      annotate("text", x = -0.8, y = 1.5, label = "Norte", size = 7, fontface = "bold") +
      annotate("text", x = -0.8, y = 0.5, label = "Sur", size = 7, fontface = "bold") +
      annotate("text", x = -1.8, y = 1.0, label = "Dirección", size = 7, angle = 90, fontface = "bold") +
      annotate("segment", x = 11.2, xend = 11.2, y = 1.5, yend = 2.7, 
               arrow = arrow(length = unit(0.4, "cm")), size = 1.4) +
      annotate("text", x = 11.2, y = 2.95, label = "N", size = 8, fontface = "bold") +
      scale_x_continuous(breaks = 1:10, labels = etiquetas_x, position = "bottom") +
      theme_void() +
      theme(legend.position = "right",
            legend.title = element_text(face = "bold", size = 13),
            legend.text = element_text(size = 11),
            legend.key.size = unit(0.9, "cm"),
            plot.margin = margin(1, 2, 1, 2, "cm"),
            axis.text.x = element_text(size = 11, face = "bold", margin = margin(t = 5)))
  } else {
    mapa_calor <- ggplot(croquis_data) +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                    fill = Indice_Equilibrio),
                color = "white", size = 1.5) +
      geom_text(aes(x = x_centro, y = y_centro, label = Etiqueta_Valores),
                fontface = "bold", size = 4.5, color = "white", lineheight = 1.1) +
      geom_text(aes(x = x_centro, y = y_centro - 0.3, label = Bloque),
                size = 2.8, fontface = "italic", color = "white") +
      scale_fill_viridis_c(option = "plasma", direction = -1, 
                           name = "Índice de\nEquilibrio", na.value = "gray50") +
      annotate("text", x = 1, y = 12.2, label = toupper(nombre_finca), 
               size = 12, fontface = "bold", color = "#1B4F72") +
      annotate("text", x = 1, y = 11.7, label = "PARCELA", size = 11, fontface = "bold") +
      annotate("text", x = 1, y = -1.4, label = "CASCO", size = 11, fontface = "bold") +
      annotate("text", x = 0.5, y = -0.85, label = "Oeste", size = 7, fontface = "bold") +
      annotate("text", x = 1.5, y = -0.85, label = "Este",  size = 7, fontface = "bold") +
      annotate("segment", x = 3.6, xend = 3.6, y = 10, yend = 11.5, 
               arrow = arrow(length = unit(0.4, "cm")), size = 1.4) +
      annotate("text", x = 3.6, y = 11.8, label = "N", size = 8, fontface = "bold") +
      scale_y_continuous(breaks = 0.5:9.5, labels = paste0("S", 1:10), position = "left") +
      scale_x_continuous(breaks = c(0.5, 1.5), labels = c("Oeste", "Este"), position = "top") +
      theme_void() +
      theme(legend.position = "right",
            legend.title = element_text(face = "bold", size = 13),
            legend.text = element_text(size = 11),
            legend.key.size = unit(0.9, "cm"),
            plot.margin = margin(1, 2, 1, 2, "cm"),
            axis.text.y = element_text(size = 10, face = "bold", margin = margin(r = 5)),
            axis.text.x.top = element_text(size = 10, face = "bold", margin = margin(b = 5)))
  }
  
  return(list(tabla = tabla_final, mapa_campo = mapa_campo, mapa_calor = mapa_calor))
}

# ==============================================================================
# 3. TRATAMIENTOS
# ==============================================================================
trat_zulia   <- c(rep("T1", 2), rep("T2", 3), rep("T3", 3), rep("T4", 3), 
                  rep("T5", 3), rep("T6", 3), rep("T7", 3))
trat_balsamo <- c(rep("T1", 2), rep("T2", 3), rep("T3", 3), rep("T4", 3), 
                  rep("T5", 3), rep("T6", 3), rep("T7", 3))
trat_limones <- c(rep("T1", 3), rep("T2", 3), rep("T3", 3), 
                  rep("T4", 3), rep("T5", 3), rep("T6", 3))

# ==============================================================================
# 4. ETIQUETAS
# ==============================================================================
labels_zulia <- c(
  "T1" = "T1 = Testigo absoluto",
  "T2" = "T2 = Premadurante 2019  -Glifosato+Boro 0 0",
  "T3" = "T3 = Premadurante 2019  +Glifosato+Boro 9 8",
  "T4" = "T4 = Premadurante 2019  +Glifosato+Boro 8 8",
  "T5" = "T5 = Premadurante 2019  +Glifosato+Boro 7 8",
  "T6" = "T6 = Premadurante 2019  +Glifosato+Boro 6 8",
  "T7" = "T7 = Premadurante 2019  +Glifosato+Boro 5 8"
)
labels_balsamo <- c(
  "T1" = "T1 = Testigo absoluto",
  "T2" = "T2 = Premadurante 2019  -Glifosato+Boro 0 0",
  "T3" = "T3 = Premadurante 2019  +Glifosato+Boro 9 8",
  "T4" = "T4 = Premadurante 2019  +Glifosato+Boro 8 8",
  "T5" = "T5 = Premadurante 2019  +Glifosato+Boro 7 8",
  "T6" = "T6 = Premadurante 2019  +Glifosato+Boro 6 8",
  "T7" = "T7 = Premadurante 2019  +Glifosato+Boro 5 11"
)
labels_limones <- c(
  "T1" = "T1 = Testigo absoluto",
  "T2" = "T2 = Premadurante 50%  -Glifosato 0 0",
  "T3" = "T3 = Premadurante 100%  -Glifosato 0 0",
  "T4" = "T4 = Premadurante 50%  +Glifosato 9 8",
  "T5" = "T5 = Premadurante 100%  +Glifosato 9 11",
  "T6" = "T6 = Sin premadurante  +Glifosato 7 8"
)

# ==============================================================================
# 5. EJECUCIÓN
# ==============================================================================
res_zulia   <- generar_ensayo(datos,  "Finca Zulia 201",   trat_zulia,   
                              orientacion = "NS", legend_labels = labels_zulia,   seed = 123)
res_balsamo <- generar_ensayo(datos2, "Finca Bálsamo 301", trat_balsamo, 
                              orientacion = "EW", legend_labels = labels_balsamo, seed = 456)
res_limones <- generar_ensayo(datos3, "Finca Limones 110", trat_limones, 
                              orientacion = "NS", legend_labels = labels_limones, seed = 789)

# ==============================================================================
# 6. PDF
# ==============================================================================
pdf("Croquis_Fincas.pdf", width = 22, height = 14, onefile = TRUE)

imprimir_pagina <- function(plot, titulo) {
  grid.newpage()
  grid.text(titulo, x = 0.5, y = 0.98, gp = gpar(fontsize = 18, fontface = "bold"))
  print(plot, vp = viewport(x = 0.5, y = 0.48, width = 0.98, height = 0.9))
}

imprimir_pagina(res_zulia$mapa_campo,   "Finca Zulia 201 - Croquis de Tratamientos")
imprimir_pagina(res_zulia$mapa_calor,   "Finca Zulia 201 - Mapa de Calor")
imprimir_pagina(res_balsamo$mapa_campo, "Finca Bálsamo 301 - Croquis de Tratamientos (Este-Oeste)")
imprimir_pagina(res_balsamo$mapa_calor, "Finca Bálsamo 301 - Mapa de Calor (Este-Oeste)")
imprimir_pagina(res_limones$mapa_campo, "Finca Limones 110 - Croquis de Tratamientos")
imprimir_pagina(res_limones$mapa_calor, "Finca Limones 110 - Mapa de Calor")

dibujar_tabla <- function(tabla, titulo) {
  grid.newpage()
  grid.text(titulo, x = 0.5, y = 0.96, gp = gpar(fontsize = 18, fontface = "bold"))
  tg <- tableGrob(tabla, rows = NULL, 
                  theme = ttheme_minimal(base_size = 12,
                                         core = list(fg_params = list(fontface = "bold"))))
  grid.draw(tg)
}
dibujar_tabla(res_zulia$tabla,   "Tabla de Valores - Finca Zulia 201")
dibujar_tabla(res_balsamo$tabla, "Tabla de Valores - Finca Bálsamo 301")
dibujar_tabla(res_limones$tabla, "Tabla de Valores - Finca Limones 110")

dev.off()
cat("\n[OK] PDF generado: Croquis_Fincas.pdf\n")
