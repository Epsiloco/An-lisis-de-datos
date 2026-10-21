datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")
names(datos)
attach(datos)
View(datos)

# =====================================================================
# SCRIPT: ANÁLISIS DE IDEOTIPO (MTSI) POR UNIDAD EXPERIMENTAL (UE)
# =====================================================================

library(dplyr)
library(tidyr)
library(ggplot2)

#1. Configuración de Variables
vars_y <- c("TCH", "Rendimiento", "TAH", "Pureza", "Pol")

direcciones <- list(
  Altura = "max",
  Diametro = "max",
  Población = "max",
  Metros.Lineales = "max", 
  TML = "max",
  Tallos.Ha = "max"
)

# 2. Promediar réplicas (Estaciones) por Unidad Experimental (UE)
df_means <- datos %>%
  group_by(Cuadrante, Ingenio) %>%
  summarise(across(all_of(vars_y), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  drop_na(all_of(vars_y))

# 3. Estandarización (Escala 0 a 100)
df_norm <- df_means
df_real <- df_means

for (v in vars_y) {
  dir <- direcciones[[v]]
  mi <- min(df_norm[[v]], na.rm = TRUE)
  ma <- max(df_norm[[v]], na.rm = TRUE)
  
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

# 4. Cálculo de la Distancia Euclidiana al Ideotipo (100)
df_res <- df_norm %>% select(UE, Tratamiento)
df_res$Distancia_MTSI <- 0
ideotipo_perfecto <- 100

for (v in vars_y) {
  df_res$Distancia_MTSI <- df_res$Distancia_MTSI + (df_norm[[v]] - ideotipo_perfecto)^2
}
df_res$Distancia_MTSI <- sqrt(df_res$Distancia_MTSI)

# 5. Score de Similitud y Ranking Global (CORREGIDO)
max_dist <- max(df_res$Distancia_MTSI, na.rm = TRUE)
min_dist <- min(df_res$Distancia_MTSI, na.rm = TRUE)

if (max_dist == min_dist) {
  df_res$Score_Similitud <- 50
} else {
  df_res$Score_Similitud <- ((max_dist - df_res$Distancia_MTSI) / (max_dist - min_dist)) * 100
}

df_res <- df_res %>%
  arrange(Distancia_MTSI) %>%
  mutate(Ranking_Global = row_number())

df_final <- df_res %>%
  left_join(df_real, by = c("UE", "Tratamiento"))

# 6. Preparación de datos para el Mapa de Calor
df_melt_norm <- df_norm %>%
  pivot_longer(cols = all_of(vars_y), names_to = "Variable", values_to = "Puntuacion_0_100")

df_melt_real <- df_real %>%
  pivot_longer(cols = all_of(vars_y), names_to = "Variable", values_to = "Valor_Real")

df_heatmap <- left_join(df_melt_norm, df_melt_real, by = c("UE", "Tratamiento", "Variable"))

# Ordenar las UEs en el eje Y según el Ranking Global
df_heatmap$UE <- factor(df_heatmap$UE, levels = rev(df_final$UE))

# 7. Generación del Mapa de Calor
p_heat_ue <- ggplot(df_heatmap, aes(x = Variable, y = UE, fill = Puntuacion_0_100)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(low = "#e74c3c", mid = "#f1c40f", high = "#27ae60", 
                       midpoint = 50, name = "Aptitud\n(100=Ideal)") +
  geom_text(aes(label = round(Valor_Real, 2)), color = "black", size = 3.5, fontface = "bold") +
  theme_minimal(base_size = 14) +
  labs(
    x = "Variables Evaluadas", 
    y = "Unidades Experimentales (Ordenadas de Peor a Mejor)", 
    title = "Perfil Agronómico Multi-Atributo por Parcela",
    subtitle = "Distancia al Ideotipo: Valores promediados a partir de las estaciones"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold", size = 8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

print(p_heat_ue)

print(df_final)
