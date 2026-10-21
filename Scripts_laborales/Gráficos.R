############## Facetado por replica - Matriz 2x2 con Paginación

datos<- read_clip_tbl()
view(datos)
names(datos)
attach(datos)



############## Facetado por replica - VERSIÓN REPARADA

# ================================
# 1. CONFIGURACIÓN
# ================================
vars_analisis <- c("Tolete_dañado") 
f_tratamiento <- "Distanciamiento"
f_replica     <- "REGION"
alpha_val     <- 0.15

tipo_error      <- "IC" 
nivel_confianza <- 0.95
graficos_por_panel <- 7 # Cambia este número para agrupar (2, 4, etc.)

# Limpieza de espacios invisibles
datos[[f_replica]] <- trimws(as.character(datos[[f_replica]]))
datos[[f_tratamiento]] <- trimws(as.character(datos[[f_tratamiento]]))

# ================================
# 2. ANOVA y Post-hoc
# ================================
lista_posthoc <- list()
for(v in vars_analisis) {
  datos_limpios <- datos[!is.na(datos[[f_replica]]), ]
  formula_lm <- as.formula(paste0("`", v, "` ~ ", f_tratamiento, " + ", f_replica)) 
  modelo <- lm(formula_lm, data = datos_limpios)
  
  res_means <- emmeans(modelo, as.formula(paste("~", f_tratamiento))) %>%
    multcomp::cld(Letters = letters, sort = TRUE, reverse = TRUE, alpha = alpha_val) %>%
    as.data.frame() %>%
    mutate(Variable = v,
           ic_modelo = SE * qt(1 - (1 - nivel_confianza)/2, df = df),
           !!sym(f_tratamiento) := trimws(as.character(!!sym(f_tratamiento))))
  
  lista_posthoc[[v]] <- res_means
}
tabla_posthoc <- bind_rows(lista_posthoc)

# ================================
# 3. Medias Reales y Paginación
# ================================
medias_reales <- datos %>%
  filter(!is.na(!!sym(f_replica))) %>%
  group_by(across(all_of(c(f_tratamiento, f_replica)))) %>%
  summarise(across(all_of(vars_analisis), list(media_r = ~mean(.x, na.rm = TRUE))), .groups = "drop") %>%
  pivot_longer(cols = -c(!!sym(f_tratamiento), !!sym(f_replica)), 
               names_to = c("Variable", ".value"), 
               names_pattern = "(.*)_(media_r)")

frentes_unicos <- unique(medias_reales[[f_replica]])
tabla_paginas <- data.frame(
  id_rep = frentes_unicos,
  Grupo_Pagina = ceiling(seq_along(frentes_unicos) / graficos_por_panel)
)
colnames(tabla_paginas)[1] <- f_replica

# ================================
# 4. Unión Final (Solución al error de mutate)
# ================================
datos_final_grafico <- tabla_posthoc %>%
  select(!!sym(f_tratamiento), Variable, .group, SE, ic_modelo) %>% 
  left_join(medias_reales, by = c(f_tratamiento, "Variable")) %>%
  left_join(tabla_paginas, by = f_replica)

# Definimos el error fuera para evitar el conflicto de tamaños de if_else
if(tipo_error == "IC") {
  datos_final_grafico$error <- datos_final_grafico$ic_modelo
} else {
  datos_final_grafico$error <- datos_final_grafico$SE
}

# ================================
# 5. Bucle de Impresión
# ================================
n_cols <- ifelse(graficos_por_panel <= 1, 1, 2)

for (v in vars_analisis) {
  temp_var <- datos_final_grafico %>% filter(Variable == v)
  paginas_v <- sort(unique(na.omit(temp_var$Grupo_Pagina)))
  
  for (p in paginas_v) {
    datos_plot <- temp_var %>% filter(Grupo_Pagina == p)
    
    # Forzamos factor para el orden visual
    datos_plot$Trat_F <- reorder(datos_plot[[f_tratamiento]], datos_plot$media_r)
    
    gg <- ggplot(datos_plot, aes(y = Trat_F, x = media_r)) +
      geom_errorbarh(aes(xmin = media_r - error, xmax = media_r + error), height = 0.2) +
      geom_point(size = 3, color = "black") +
      geom_text(aes(label = sprintf("%.2f %s", media_r, trimws(.group))),
                hjust = -0.1, vjust = -1.2, size = 3.6) +
      
      facet_wrap(as.formula(paste("~", f_replica)), ncol = n_cols, scales = "free_x") +
      
      scale_x_continuous(expand = expansion(mult = c(0.1, 0.6))) +
      labs(x = paste("Media Real ±", tipo_error), y = "Tratamiento",
           title = paste("Variable:", v), subtitle = paste("Página", p)) +
      
      theme_classic2()
    
    print(gg)
  }
}



######################################################


############## Facetado por replica - Con conteo de N

# 1. CARGA DE DATOS (Asegúrate de copiar de Excel antes)
datos <- clipr::read_clip_tbl()
attach(datos)
view(datos)
names(datos)
levels(datos$Distanciamiento)

# ================================
# 1. CONFIGURACIÓN
# ================================
vars_analisis <- c("Tolete_dañado") 
f_tratamiento <- "Distanciamiento"
f_replica     <- "Tercio"
alpha_val     <- 0.15

tipo_error      <- "IC" 
nivel_confianza <- 0.95
graficos_por_panel <- 2 

# Limpieza
datos[[f_replica]] <- trimws(as.character(datos[[f_replica]]))
datos[[f_tratamiento]] <- trimws(as.character(datos[[f_tratamiento]]))

# ================================
# 2. ANOVA y Post-hoc
# ================================
lista_posthoc <- list()
for(v in vars_analisis) {
  datos_limpios <- datos[!is.na(datos[[f_replica]]), ]
  formula_lm <- as.formula(paste0("`", v, "` ~ ", f_tratamiento, " + ", f_replica)) 
  modelo <- lm(formula_lm, data = datos_limpios)
  
  res_means <- emmeans(modelo, as.formula(paste("~", f_tratamiento))) %>%
    multcomp::cld(Letters = letters, sort = TRUE, reverse = TRUE, alpha = alpha_val) %>%
    as.data.frame() %>%
    mutate(Variable = v,
           ic_modelo = SE * qt(1 - (1 - nivel_confianza)/2, df = df),
           !!sym(f_tratamiento) := trimws(as.character(!!sym(f_tratamiento))))
  
  lista_posthoc[[v]] <- res_means
}
tabla_posthoc <- bind_rows(lista_posthoc)

# ================================
# 3. Medias Reales, N y Paginación
# ================================
# Calculamos la N total de la variable analizada
n_total <- nrow(datos[!is.na(datos[[vars_analisis[1]]]), ])

medias_reales <- datos %>%
  filter(!is.na(!!sym(f_replica))) %>%
  group_by(across(all_of(c(f_tratamiento, f_replica)))) %>%
  # Calculamos media y también el n por grupo por si quieres ver n específico
  summarise(
    across(all_of(vars_analisis), list(
      media_r = ~mean(.x, na.rm = TRUE),
      n_obs = ~sum(!is.na(.x))
    )), .groups = "drop"
  ) %>%
  pivot_longer(cols = -c(!!sym(f_tratamiento), !!sym(f_replica)), 
               names_to = c("Variable", ".value"), 
               names_pattern = "(.*)_(media_r|n_obs)")

frentes_unicos <- unique(medias_reales[[f_replica]])
tabla_paginas <- data.frame(
  id_rep = frentes_unicos,
  Grupo_Pagina = ceiling(seq_along(frentes_unicos) / graficos_por_panel)
)
colnames(tabla_paginas)[1] <- f_replica

# ================================
# 4. Unión Final
# ================================
datos_final_grafico <- tabla_posthoc %>%
  select(!!sym(f_tratamiento), Variable, .group, SE, ic_modelo) %>% 
  left_join(medias_reales, by = c(f_tratamiento, "Variable")) %>%
  left_join(tabla_paginas, by = f_replica)

if(tipo_error == "IC") {
  datos_final_grafico$error <- datos_final_grafico$ic_modelo
} else {
  datos_final_grafico$error <- datos_final_grafico$SE
}

# ================================
# 5. Bucle de Impresión
# ================================
n_cols <- ifelse(graficos_por_panel <= 1, 1, 2)

for (v in vars_analisis) {
  temp_var <- datos_final_grafico %>% filter(Variable == v)
  paginas_v <- sort(unique(na.omit(temp_var$Grupo_Pagina)))
  
  # Calculamos N para el título (suma de observaciones en esta variable)
  n_variable <- sum(temp_var$n_obs, na.rm = TRUE)
  
  for (p in paginas_v) {
    datos_plot <- temp_var %>% filter(Grupo_Pagina == p)
    datos_plot$Trat_F <- reorder(datos_plot[[f_tratamiento]], datos_plot$media_r)
    
    gg <- ggplot(datos_plot, aes(y = Trat_F, x = media_r)) +
      geom_errorbarh(aes(xmin = media_r - error, xmax = media_r + error), height = 0.2) +
      geom_point(size = 3, color = "black") +
      geom_text(aes(label = sprintf("%.2f %s", media_r, trimws(.group))),
                hjust = -0.1, vjust = -1.2, size = 3.6) +
      
      facet_wrap(as.formula(paste("~", f_replica)), ncol = n_cols, scales = "free_x") +
      
      scale_x_continuous(expand = expansion(mult = c(0.1, 0.6))) +
      labs(
        x = paste("Media Real ±", tipo_error), 
        y = "Tratamiento",
        # AQUÍ SE INCLUYE LA N EN EL TÍTULO
        title = paste("Variable:", v, "(n =", n_variable, ")"), 
        subtitle = paste("Página", p, "| Grupo de", f_replica)
      ) +
      theme_classic2()
    
    print(gg)
  }
}
