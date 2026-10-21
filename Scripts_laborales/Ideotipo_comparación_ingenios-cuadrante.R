# =====================================================================
# ANÁLISIS DE IDEOTIPO (MTSI) POR CUADRANTE
# Interruptor para elegir entre solo dos ingenios o todos
# Variables: TCH, Rendimiento, TAH, Pureza, Pol
# Gráfico facetado por Cuadrante
# =====================================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# Cargar datos (ajusta la fuente si es necesario)
datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")
names(datos)
View(datos)

# ================= INTERRUPTOR =================
usar_solo_interes <- FALSE   # TRUE = solo Pantaleon y La Union; FALSE = todos los ingenios
# ===============================================

# 1. Filtrar según el interruptor
if (usar_solo_interes) {
  ingenios_interes <- c("PANTALEON", "LA UNION")
  datos_filtrados <- datos %>% filter(Ingenio %in% ingenios_interes)
  subtitulo <- "Comparación Pantaleon vs La Union"
} else {
  datos_filtrados <- datos   # todos los ingenios
  subtitulo <- "Todos los ingenios"
}

# 2. Variables respuesta y dirección (todas "max")
vars_y <- c("TCH", "TAH", "Pureza", "Pol")
direcciones <- setNames(rep("max", length(vars_y)), vars_y)

# 3. Promediar por combinación Cuadrante-Ingenio
df_means <- datos_filtrados %>%
  group_by(Cuadrante, Ingenio) %>%
  summarise(across(all_of(vars_y), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  drop_na(all_of(vars_y))

# 4. Determinar cuadrantes válidos
if (usar_solo_interes) {
  # Solo cuadrantes donde estén presentes ambos ingenios
  cuadrantes_validos <- df_means %>%
    group_by(Cuadrante) %>%
    summarise(n_ing = n_distinct(Ingenio), .groups = "drop") %>%
    filter(n_ing == 2) %>%
    pull(Cuadrante)
} else {
  # Cuadrantes con al menos 2 ingenios (para comparar)
  cuadrantes_validos <- df_means %>%
    group_by(Cuadrante) %>%
    summarise(n_ing = n_distinct(Ingenio), .groups = "drop") %>%
    filter(n_ing >= 2) %>%
    pull(Cuadrante)
}

df_means <- df_means %>% filter(Cuadrante %in% cuadrantes_validos)

if (nrow(df_means) == 0) {
  stop("No hay cuadrantes con datos suficientes para el análisis.")
}

# 5. Función para estandarizar y calcular distancia MTSI dentro de un cuadrante
mtsi_cuadrante <- function(df_cuad, vars, dirs) {
  df_norm <- df_cuad
  df_real <- df_cuad
  
  # Estandarización (0-100) usando solo los datos de este cuadrante
  for (v in vars) {
    mi <- min(df_norm[[v]], na.rm = TRUE)
    ma <- max(df_norm[[v]], na.rm = TRUE)
    dir <- dirs[[v]]
    
    if (ma == mi) {
      df_norm[[v]] <- 50
    } else {
      if (dir == "min") {
        df_norm[[v]] <- ((ma - df_norm[[v]]) / (ma - mi)) * 100
      } else {
        df_norm[[v]] <- ((df_norm[[v]] - mi) / (ma - mi)) * 100
      }
    }
  }
  
  # Distancia euclidiana al ideotipo (100)
  ideotipo <- 100
  dist <- rep(0, nrow(df_norm))
  for (v in vars) {
    dist <- dist + (df_norm[[v]] - ideotipo)^2
  }
  df_norm$Distancia_MTSI <- sqrt(dist)
  
  # Score de similitud (mayor = mejor)
  max_dist <- max(df_norm$Distancia_MTSI, na.rm = TRUE)
  min_dist <- min(df_norm$Distancia_MTSI, na.rm = TRUE)
  if (max_dist == min_dist) {
    df_norm$Score_Similitud <- 50
  } else {
    df_norm$Score_Similitud <- ((max_dist - df_norm$Distancia_MTSI) / (max_dist - min_dist)) * 100
  }
  
  # Ranking dentro del cuadrante (menor distancia = mejor)
  df_norm <- df_norm %>%
    arrange(Distancia_MTSI) %>%
    mutate(Ranking_Cuadrante = row_number())
  
  # Unir con valores reales
  df_final_cuad <- df_norm %>%
    left_join(df_real, by = c("Cuadrante", "Ingenio"), suffix = c("_norm", ""))
  
  return(df_final_cuad)
}

# 6. Aplicar la función a cada cuadrante
df_resultado <- df_means %>%
  split(.$Cuadrante) %>%
  lapply(function(df) mtsi_cuadrante(df, vars_y, direcciones)) %>%
  bind_rows()

# 7. Preparar datos para el heatmap facetado
df_melt_norm <- df_resultado %>%
  select(Cuadrante, Ingenio, all_of(vars_y)) %>%
  pivot_longer(cols = all_of(vars_y), names_to = "Variable", values_to = "Puntuacion_0_100")

df_melt_real <- df_means %>%
  pivot_longer(cols = all_of(vars_y), names_to = "Variable", values_to = "Valor_Real")

df_heatmap <- left_join(df_melt_norm, df_melt_real, 
                        by = c("Cuadrante", "Ingenio", "Variable"))

# 8. Heatmap facetado por Cuadrante
p_heat <- ggplot(df_heatmap, aes(x = Variable, y = Ingenio, fill = Puntuacion_0_100)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(low = "#e74c3c", mid = "#f1c40f", high = "#27ae60", 
                       midpoint = 50, name = "Aptitud\n(100=Ideal)") +
  geom_text(aes(label = round(Valor_Real, 2)), color = "black", size = 3.5, fontface = "bold") +
  facet_wrap(~ Cuadrante, scales = "free_y") +
  theme_minimal(base_size = 14) +
  labs(
    x = "Variables Evaluadas",
    y = "Ingenio",
    title = "Perfil Multi-Atributo por Cuadrante",
    subtitle = subtitulo
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold", size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

print(p_heat)

# 9. Mostrar tabla final
print(df_resultado)