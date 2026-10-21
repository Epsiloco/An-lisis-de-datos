datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")
View(datos)
attach(datos)
names(datos)



################################################################################



# =====================================================================
# SISTEMA AUTOMATIZADO MAESTRO: MANOVA Y RANKING GLOBAL CON LETRAS
# =====================================================================

# 1. Cargar librerías (Añadimos multcomp y multcompView para las letras)
pacman::p_load(car, emmeans, ggplot2, dplyr, tidyr, multcomp, multcompView)

# 2. CARGA DE DATOS 
# datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")

# =====================================================================
# 3. CONFIGURACIÓN INICIAL
# =====================================================================
variables_a_evaluar <- c("Rendimiento_Kg_Ton", "TAH", "Pureza")
columna_bloque <- "Replica"
alpha <- 0.15 

# =====================================================================
# 4. MOTOR DE AUTO-DETECCIÓN DE TRATAMIENTOS
# =====================================================================
niveles_originales <- unique(as.character(datos$Descripcion))
N <- length(niveles_originales)

idx_test <- grep("Madurez|Testigo|Natural", niveles_originales, ignore.case = TRUE)[1]
idx_100s <- grep("100%", niveles_originales, ignore.case = TRUE)
idx_solo <- idx_100s[!idx_100s %in% grep("Nano", niveles_originales, ignore.case = TRUE)][1]
idx_n100 <- grep("Nano.*100%|100%.*Nano", niveles_originales, ignore.case = TRUE)[1]
idx_n75  <- grep("75%", niveles_originales, ignore.case = TRUE)[1]
idx_n50  <- grep("50%", niveles_originales, ignore.case = TRUE)[1]

nom_test <- niveles_originales[idx_test]
nom_solo <- niveles_originales[idx_solo]
nom_n100 <- niveles_originales[idx_n100]
nom_n75  <- niveles_originales[idx_n75]
nom_n50  <- niveles_originales[idx_n50]

# Construcción matemática automática de contrastes
c1_testigo_vs_resto <- rep(1, N)
c1_testigo_vs_resto[idx_test] <- -(N - 1)

c2_solo_vs_nano <- rep(0, N)
c2_solo_vs_nano[idx_solo] <- -1
c2_solo_vs_nano[idx_n100] <- 1

c3_lineal <- rep(0, N)
c3_lineal[idx_n100] <- 1
c3_lineal[idx_n50]  <- -1

c4_cuad <- rep(0, N)
c4_cuad[idx_n100] <- 1
c4_cuad[idx_n75]  <- -2
c4_cuad[idx_n50]  <- 1

matriz_contrastes <- cbind(
  "C1_Testigo" = c1_testigo_vs_resto, 
  "C2_Solo"    = c2_solo_vs_nano, 
  "C3_Lineal"  = c3_lineal, 
  "C4_Cuad"    = c4_cuad
)
datos$Descripcion <- factor(datos$Descripcion, levels = niveles_originales)
contrasts(datos$Descripcion) <- matriz_contrastes

# =====================================================================
# 5. AJUSTE DINÁMICO DEL MANOVA Y TABLA DE CONTRASTES
# =====================================================================
y_vars <- paste(variables_a_evaluar, collapse = ", ")
formula_str <- paste0("cbind(", y_vars, ") ~ Descripcion + ", columna_bloque)
formula_mlm <- as.formula(formula_str)
modelo_mlm <- lm(formula_mlm, data = datos)

extraer_p_wilks <- function(modelo, hipotesis) {
  salida <- capture.output(linearHypothesis(modelo, hipotesis, test = "Wilks"))
  numeros <- suppressWarnings(as.numeric(strsplit(trimws(grep("Wilks", salida, value = TRUE)), "\\s+")[[1]]))
  return(round(tail(numeros[!is.na(numeros)], 1), 4))
}

manova_base <- summary(manova(formula_mlm, data = datos), test = "Wilks")
p_global <- round(manova_base$stats["Descripcion", "Pr(>F)"], 4)

c0_name <- paste("MANOVA GLOBAL (Efecto conjunto en", length(variables_a_evaluar), "variables)")
c1_name <- paste("CONTRASTE 1: [", nom_test, "] VS. [Resto de Tratamientos Químicos]")
c2_name <- paste("CONTRASTE 2: [", nom_solo, "] VS. [", nom_n100, "]")
c3_name <- paste("CONTRASTE 3 (Lineal): [", nom_n100, "] VS. [", nom_n50, "]")
c4_name <- paste("CONTRASTE 4 (Cuadrático): [", nom_n75, "] VS. Promedio de Extremos (100% y 50%)")

tabla_gerencial <- data.frame(
  Comparacion_Exacta = c(c0_name, c1_name, c2_name, c3_name, c4_name),
  P_Value = c(
    p_global,
    extraer_p_wilks(modelo_mlm, "DescripcionC1_Testigo = 0"),
    extraer_p_wilks(modelo_mlm, "DescripcionC2_Solo = 0"),
    extraer_p_wilks(modelo_mlm, "DescripcionC3_Lineal = 0"),
    extraer_p_wilks(modelo_mlm, "DescripcionC4_Cuad = 0")
  )
)

tabla_gerencial <- tabla_gerencial %>%
  mutate(
    Decisión = ifelse(P_Value < alpha, "SIGNIFICATIVO", "NO SIGNIFICATIVO"),
    Conclusión = case_when(
      P_Value < alpha & row_number() == 1 ~ "El manejo altera significativamente el perfil de las variables.",
      P_Value < alpha & row_number() == 2 ~ "Madurar químicamente supera estadísticamente al testigo natural.",
      P_Value >= alpha & row_number() == 3 ~ "Agregar el coadyuvante al 100% no generó diferencia estadística global.",
      P_Value < alpha & row_number() == 4 ~ "Pérdida lineal de calidad conjunta al bajar la dosis a la mitad.",
      P_Value < alpha & row_number() == 5 ~ "La dosis del 75% es el punto de máxima optimización técnica.",
      TRUE ~ "Empate estadístico multivariado."
    )
  )

cat("\n========================================================================================================\n")
cat("                       TABLA DE DECISIONES: MANOVA Y CONTRASTES ORTOGONALES                             \n")
cat("========================================================================================================\n")
print(tabla_gerencial, row.names = FALSE, right = FALSE, width = 1000)
cat("========================================================================================================\n\n")

# =====================================================================
# 6. CREACIÓN DEL ÍNDICE GLOBAL (PCA) Y PRUEBA DE MEDIAS (LETRAS)
# =====================================================================
# Aislamos las variables y creamos el componente principal (PCA)
datos_pca <- datos[, variables_a_evaluar]
pca_res <- prcomp(datos_pca, center = TRUE, scale. = TRUE)

# Extraemos el Componente 1 (Resume la mayor cantidad de información de las 3 variables)
PC1 <- pca_res$x[, 1]

# Aseguramos que valores altos del Índice signifiquen "Mejor Rendimiento/Pureza"
# Si la correlación es negativa, invertimos el signo
if(cor(PC1, datos[[variables_a_evaluar[1]]]) < 0) {
  PC1 <- PC1 * -1
}

datos$Indice_Global <- PC1

# ANOVA univariado sobre el Índice Global
modelo_indice <- lm(as.formula(paste("Indice_Global ~ Descripcion +", columna_bloque)), data = datos)

# Extraer medias y letras (Usando Fisher LSD sin ajuste, per tu preferencia de alpha=0.15)
medias_global <- emmeans(modelo_indice, ~ Descripcion)
letras_cld <- cld(medias_global, alpha = alpha, Letters = letters, adjust = "none")

# Limpiar dataframe para graficar
letras_cld$.group <- trimws(letras_cld$.group) # Quitar espacios de las letras
orden_grafico <- c(nom_test, nom_solo, nom_n100, nom_n75, nom_n50)
nombres_cortos <- c("Testigo", "Glifosato 100%", "NanoPro 100%", "NanoPro 75%", "NanoPro 50%")

letras_cld$Descripcion <- factor(letras_cld$Descripcion, levels = orden_grafico)
letras_cld$Tratamiento_Corto <- factor(letras_cld$Descripcion, levels = orden_grafico, labels = nombres_cortos)

# Calcular el límite superior del eje Y para que las letras no se corten
max_y <- max(letras_cld$upper.CL)
rango_y <- max_y - min(letras_cld$lower.CL)

# =====================================================================
# 7. GRÁFICO GERENCIAL DEL RANKING GLOBAL
# =====================================================================
grafico_ranking <- ggplot(letras_cld, aes(x = Tratamiento_Corto, y = emmean, fill = Tratamiento_Corto)) +
  geom_col(color = "black", alpha = 0.85, width = 0.6) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15, linewidth = 0.8) +
  # Añadir las letras de significancia arriba de las barras
  geom_text(aes(label = .group, y = upper.CL + (rango_y * 0.08)), size = 6, fontface = "bold", color = "black") +
  scale_fill_manual(values = c("#A6A6A6", "#2E75B6", "#548235", "#70AD47", "#A9D18E")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(face = "bold", size = 12, color = "black"),
    axis.title.y = element_text(face = "bold", margin = margin(t = 0, r = 15, b = 0, l = 0)),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#555555", margin = margin(b = 15))
  ) +
  labs(
    title = "Ranking de Desempeño Global de Madurantes",
    subtitle = "Basado en un Índice Multivariado (Rendimiento + TAH + Pureza). Letras distintas indican diferencia significativa.",
    x = "",
    y = "Índice de Calidad Global"
  )

print(grafico_ranking)
