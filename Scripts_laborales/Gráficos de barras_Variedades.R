library(readxl)
datos <- read_excel("c:/users/105216/OneDrive - Pantaleon. S.A/Investigacion_Agricola/Investigaciones activas/01 VARIEDADES/Variedades 25-26/Data/BASE DE DATOS 25-26 (1).xlsx", 
                    sheet = "Análisis")
names(datos)
View(datos)
attach(datos)

library(dplyr)
library(ggplot2)
library(tidyr)

# =====================================================================
# 0. CONFIGURACIÓN DEL TESTIGO PARA LA LÍNEA DE CORTE
# =====================================================================
variedad_testigo <- "CG02-163" 

# =====================================================================
# 1. PREPARACIÓN Y LIMPIEZA DE DATOS
# =====================================================================
datos_proc <- datos %>%
  mutate(
    TCH.Real = as.numeric(TCH.Real),
    Rend. = as.numeric(Rend.),
    TAH = as.numeric(TAH),
    PUREZA = as.numeric(PUREZA),
    # Aseguramos que la columna combinada esté en formato numérico
    TAH_Pureza = as.numeric(TAH_Pureza), 
    Finca = factor(trimws(Finca), levels = c("Santa nina", "La Conquista", "Olomega")), 
    Variedad = as.character(Variedad), 
    Zafra.Cosecha = as.factor(Zafra.Cosecha)
  ) %>%
  drop_na(TAH_Pureza)

# =====================================================================
# 2. FILTRO ÉLITE (PROMEDIO HISTÓRICO >= 0.95 EN SANTA NINA Y SIN TESTIGO)
# =====================================================================
# 1. Calculamos el promedio de TAH_Pureza en Santa Nina
resumen_santa_nina <- datos_proc %>%
  filter(Finca == "Santa nina") %>%
  group_by(Variedad) %>%
  summarise(
    Media_TAH_Pureza = mean(TAH_Pureza, na.rm = TRUE)
  )

# 2. Filtramos: >= 0.95 y OMITIMOS al testigo
variedades_elite_sn <- resumen_santa_nina %>%
  filter(Media_TAH_Pureza >= 0.95) %>%
  filter(Variedad != variedad_testigo) %>%
  pull(Variedad) %>%
  unique()

cat("Variedades seleccionadas (TAH_Pureza >= 0.95 en Santa Nina, excluyendo testigo):\n")
print(variedades_elite_sn)

# 3. Extraemos todos los datos para las variedades seleccionadas
datos_filtrados <- datos_proc %>%
  filter(Variedad %in% variedades_elite_sn) %>%
  drop_na(TCH.Real, Rend., TAH, PUREZA)

# =====================================================================
# 3. CÁLCULO DE PROMEDIOS GLOBALES
# =====================================================================
df_largo <- datos_filtrados %>%
  dplyr::select(Finca, Variedad, Zafra.Cosecha, TCH.Real, Rend., TAH, PUREZA) %>%
  pivot_longer(
    cols = c(TCH.Real, Rend., TAH, PUREZA),
    names_to = "Variable",
    values_to = "Valor"
  ) %>%
  mutate(Variable = factor(Variable, 
                           levels = c("TCH.Real", "Rend.", "TAH", "PUREZA"),
                           labels = c("TCH Real", "Rendimiento", "TAH", "Pureza")))

df_zafras <- df_largo %>%
  group_by(Finca, Variedad, Zafra.Cosecha, Variable) %>%
  summarise(Media = mean(Valor, na.rm = TRUE), .groups = "drop")

df_promedio <- df_largo %>%
  group_by(Finca, Variedad, Variable) %>%
  summarise(Media = mean(Valor, na.rm = TRUE), .groups = "drop") %>%
  mutate(Zafra.Cosecha = "Promedio General (3 Años)")

df_final <- bind_rows(df_zafras, df_promedio)
niveles_zafra <- c(levels(datos_filtrados$Zafra.Cosecha), "Promedio General (3 Años)")
df_final$Zafra.Cosecha <- factor(df_final$Zafra.Cosecha, levels = niveles_zafra)

# =====================================================================
# 4. GENERACIÓN DE LOS GRÁFICOS (VERTICAL AGRUPADO)
# =====================================================================
variables_graficar <- levels(df_final$Variable)
unidades_ejes <- c("TCH Real" = "Ton/Ha", "Rendimiento" = "Kg/Ton", "TAH" = "Ton/Ha", "Pureza" = "%")

colores_fincas <- c("Santa nina" = "#4daf4a", "La Conquista" = "#377eb8", "Olomega" = "#e41a1c")

for (var_actual in variables_graficar) {
  
  df_plot <- df_final %>% filter(Variable == var_actual)
  if(nrow(df_plot) == 0) next
  
  # --- OBTENER EL VALOR DEL TESTIGO PARA LA LÍNEA DE CORTE ---
  # Lo extraemos de los datos originales (datos_proc) porque lo sacamos de df_plot
  val_testigo <- datos_proc %>%
    filter(Variedad == variedad_testigo & Finca == "Santa nina") %>%
    summarise(Media_T = mean(case_when(
      var_actual == "TCH Real" ~ TCH.Real,
      var_actual == "Rendimiento" ~ Rend.,
      var_actual == "TAH" ~ TAH,
      var_actual == "Pureza" ~ PUREZA
    ), na.rm = TRUE)) %>%
    pull(Media_T)
  
  corte_testigo <- ifelse(length(val_testigo) > 0 && !is.na(val_testigo[1]), val_testigo[1], NA)
  df_hline <- data.frame(Zafra.Cosecha = "Promedio General (3 Años)", Corte = corte_testigo)
  
  p <- ggplot(df_plot, aes(x = Variedad, y = Media, fill = Finca)) +
    
    geom_bar(stat = "identity", position = position_dodge(preserve = "single", width = 0.85), 
             color = "black", linewidth = 0.4, width = 0.8) +
    
    geom_text(aes(label = sprintf("%.2f", Media)), 
              position = position_dodge(preserve = "single", width = 0.85), 
              vjust = -0.5, size = 2.8, fontface = "bold", color = "black") +
    
    facet_wrap(~ Zafra.Cosecha, ncol = 1, scales = "free_y") +
    
    scale_fill_manual(values = colores_fincas) + 
    theme_bw(base_size = 12) + 
    
    labs(
      title = paste("Desempeño Multi-Ambiental:", var_actual),
      subtitle = paste("Filtro: TAH_Pureza \u2265 0.95 | Línea Roja =", variedad_testigo),
      x = "Variedades Superiores (Testigo Excluido)", 
      y = paste(var_actual, "(", unidades_ejes[var_actual], ")"),
      fill = "Localidad:"
    ) +
    
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 9, color = "gray30"),
      
      axis.title.x = element_text(size = 10, face = "bold"), 
      axis.title.y = element_text(size = 10, face = "bold"),
      axis.text.x = element_text(angle = 30, hjust = 1, face = "bold", size = 9), 
      axis.text.y = element_text(size = 9),
      
      strip.background = element_rect(fill = "gray90", color = "black", linewidth = 1),
      strip.text = element_text(color = "black", face = "bold", size = 10), 
      panel.grid.major.x = element_blank(),
      
      legend.position = "top",
      legend.title = element_text(face = "bold", size = 9),
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.4, "cm"), 
      legend.margin = margin(b = -5) 
    ) + 
    
    coord_cartesian(ylim = c(0, max(df_plot$Media, na.rm = TRUE) * 1.20))
  
  if(!is.na(corte_testigo)) {
    p <- p + geom_hline(data = df_hline, aes(yintercept = Corte), 
                        color = "red", linetype = "dashed", linewidth = 0.8)
  }
  
  if(interactive()) dev.new(noRStudioGD = TRUE, width = 12, height = 9)
  print(p)
  
  Sys.sleep(1)
}





