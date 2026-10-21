# ==============================================================================
# SCRIPT AUTOMATIZADO: MODELOS LINEALES MIXTOS Y ANÁLISIS DE VARIANZA
# ==============================================================================

# ------------------------------------------------------------------------------
# SECCIÓN 1: SELECCIÓN DE VARIABLES Y FACTORES (Modifica solo aquí)
# ------------------------------------------------------------------------------

# 1. Carga de datos desde el portapapeles
datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")
View(datos)
names(datos)

# 2. Edatos# 2. Especifica los nombres exactos de tus columnas en el dataset (entre comillas)
VAR_RESPUESTA <- "Rendimiento"          # Variable dependiente (ej. TCH o TAH)
FACTOR_TRAT   <- "Tratamiento"  # Factor de estudio principal
FACTOR_BLOCK  <- "Replica"      # Factor de bloqueo / efecto aleatorio

# ------------------------------------------------------------------------------
# CONFIGURACIÓN AUTOMÁTICA (No modificar a partir de aquí)
# ------------------------------------------------------------------------------
library(nlme)
library(emmeans)
library(multcomp)
library(ggplot2)
library(dplyr)

# Crear variables internas estandarizadas para evitar el error de nombres duplicados
datos_proc <- datos %>%
  mutate(
    Y_resp      = as.numeric(.data[[VAR_RESPUESTA]]),
    Trat_factor = as.factor(.data[[FACTOR_TRAT]]),
    Rep_factor  = as.factor(.data[[FACTOR_BLOCK]])
  )

# ==============================================================================
# SECCIÓN 2: DIAGNÓSTICO (Evaluación e identificación del mejor modelo)
# ==============================================================================
cat("\n--- COMIENZO: DIAGNÓSTICO DE MODELOS ---\n")

# 1. Ajuste de los tres modelos con diferentes estructuras de varianza (varIdent)
modelo_rep <- lme(Y_resp ~ Trat_factor, 
                  random = ~ 1 | Rep_factor,
                  weights = varIdent(form = ~ 1 | Rep_factor),
                  data = datos_proc, na.action = na.omit,
                  control = lmeControl(msMaxIter = 100, opt = "optim"))

modelo_trat <- lme(Y_resp ~ Trat_factor, 
                   random = ~ 1 | Rep_factor,
                   weights = varIdent(form = ~ 1 | Trat_factor),
                   data = datos_proc, na.action = na.omit,
                   control = lmeControl(msMaxIter = 100, opt = "optim"))

modelo_inter <- lme(Y_resp ~ Trat_factor, 
                    random = ~ 1 | Rep_factor,
                    weights = varIdent(form = ~ 1 | Trat_factor * Rep_factor),
                    data = datos_proc, na.action = na.omit,
                    control = lmeControl(msMaxIter = 100, opt = "optim"))

# 2. Comparación de criterios de información (AIC / BIC)
cat("\n> Comparativa de Modelos (Selecciona el de menor AIC/BIC):\n")
comp_modelos <- anova(modelo_rep, modelo_trat, modelo_inter)
print(comp_modelos)

# SELECCIÓN AUTOMÁTICA DEL MEJOR MODELO BASADO EN AIC
lista_modelos <- list(modelo_rep, modelo_trat, modelo_inter)
mejor_idx <- which.min(comp_modelos$AIC)
modelo_optimo <- lista_modelos[[mejor_idx]]
nombres_mod <- c("VarIdent (Réplica)", "VarIdent (Tratamiento)", "VarIdent (Interacción)")

cat(paste0("\n[INFO]: El modelo seleccionado automáticamente por mejor AIC es: ", nombres_mod[mejor_idx], "\n"))


# ==============================================================================
# SECCIÓN 3: TODOS LOS GRÁFICOS (Visualizaciones de Diagnóstico)
# ==============================================================================
cat("\n--- COMIENZO: GENERACIÓN DE GRÁFICOS DE DIAGNÓSTICO ---\n")

# --- GRÁFICO 1: Residuos Estandarizados vs Predichos (Modelo Óptimo)
plot(modelo_optimo, resid(., type="pearson") ~ fitted(.), 
     main = paste("Residuos Estandarizados vs Predichos\nModelo:", nombres_mod[mejor_idx]),
     xlab = "Valores Ajustados", ylab = "Residuos de Pearson")

# --- GRÁFICO 2: Boxplot Comparativo Global de Residuos
res_rep <- resid(modelo_rep, type = "pearson")
res_trat <- resid(modelo_trat, type = "pearson")
res_inter <- resid(modelo_inter, type = "pearson")

tabla_residuos <- data.frame(
  Residuo = c(res_rep, res_trat, res_inter),
  Modelo = rep(c("1. VarIdent (Réplica)", 
                 "2. VarIdent (Tratamiento)", 
                 "3. VarIdent (Interacción)"), 
               each = length(res_rep))
)

g_residuos <- ggplot(tabla_residuos, aes(x = Modelo, y = Residuo, fill = Modelo)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.shape = 16, outlier.size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkblue", linewidth = 1) +
  labs(title = "Comparación de Residuos por Estructura de Varianza",
       subtitle = "Evaluación de homocedasticidad entre los 3 modelos",
       x = "Estructura del Modelo", y = "Residuos de Pearson") +
  scale_fill_manual(values = c("#A3E4D7", "#F9E79F", "#F5CBA7")) +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold"))
print(g_residuos)

# --- GRÁFICO 3: Boxplots de Residuos por Tratamiento (Los 3 Modelos)
par(mfrow = c(3, 1)) 

boxplot(resid(modelo_rep, type = "pearson") ~ datos_proc$Trat_factor, 
        main = "Modelo 1: Varianza por Réplica (Monitorear heterocedasticidad)",
        ylab = "Residuos", xlab = "Tratamiento", col = "#A3E4D7")
abline(h = 0, col = "red", lty = 2, lwd = 1.5)

boxplot(resid(modelo_trat, type = "pearson") ~ datos_proc$Trat_factor, 
        main = "Modelo 2: Varianza por Tratamiento (Cajas deberían ser homogéneas)",
        ylab = "Residuos", xlab = "Tratamiento", col = "#F9E79F")
abline(h = 0, col = "red", lty = 2, lwd = 1.5)

boxplot(resid(modelo_inter, type = "pearson") ~ datos_proc$Trat_factor, 
        main = "Modelo 3: Varianza por Interacción (Cajas deberían ser homogéneas)",
        ylab = "Residuos", xlab = "Tratamiento", col = "#F5CBA7")
abline(h = 0, col = "red", lty = 2, lwd = 1.5)

par(mfrow = c(1, 1)) # Restaurar ventana gráfica


# ==============================================================================
# SECCIÓN 4: ANÁLISIS FINAL (Resultados del Modelo Óptimo y Pruebas de Medias)
# ==============================================================================
cat("\n--- COMIENZO: ANÁLISIS FINAL DEL MODELO ÓPTIMO ---\n")

# 1. ANOVA del modelo seleccionado
cat("\n> Tabla de ANOVA (Efectos Fijos):\n")
print(anova(modelo_optimo))

# 2. Summary del modelo seleccionado
cat("\n> Resumen General del Modelo (Estadísticos y coeficientes):\n")
print(summary(modelo_optimo))


# 3. VERSIÓN A: MEDIAS MARGINALES AJUSTADAS (Predichas por el Modelo mixto)
medias_ajustadas <- emmeans(modelo_optimo, ~ Trat_factor)

letras_medias <- cld(medias_ajustadas, 
                     alpha = 0.15, 
                     Letters = letters, 
                     adjust = "Tukey",
                     decreasing = TRUE)
letras_medias$.group <- trimws(letras_medias$.group)

cat("\n> VERSIÓN A: Tabla de Medias AJUSTADAS (Modelo Óptimo - Tukey, alpha=0.15):\n")
print(letras_medias)

# --- GRÁFICO 4: Gráfico de resultados con Medias Ajustadas (EMMeans)
g_final_ajustadas <- ggplot(letras_medias, aes(y = reorder(Trat_factor, emmean), x = emmean)) +
  geom_errorbarh(aes(xmin = emmean - SE, xmax = emmean + SE), 
                 height = 0.2, color = "#2C3E50", linewidth = 0.8) +
  geom_point(color = "black", size = 3) +
  geom_text(aes(label = round(emmean, 2)), vjust = -1.2, size = 3.5, color = "#2C3E50") +
  geom_text(aes(x = emmean + SE, label = .group), 
            hjust = -0.5, size = 4.5, fontface = "bold", color = "black") +
  labs(title = paste("Prueba de Medias AJUSTADAS para", VAR_RESPUESTA),
       subtitle = paste("Basado en el modelo ajustado:", nombres_mod[mejor_idx]),
       x = paste(VAR_RESPUESTA, "(Media Estimada ± Error Estándar)"), y = FACTOR_TRAT) +
  theme_minimal() +
  theme(axis.text.y = element_text(face = "bold", size = 11),
        panel.grid.major.y = element_blank())
print(g_final_ajustadas)


# 4. VERSIÓN B: MEDIAS REALES (Aritméticas directas de tus datos muestrales)
medias_reales <- datos_proc %>%
  filter(!is.na(Y_resp)) %>%
  group_by(Trat_factor) %>%
  summarise(
    media_real = mean(Y_resp),
    SE_real = sd(Y_resp) / sqrt(n()),
    .groups = "drop"
  )

# Unimos las letras de significancia estadística del modelo a los valores numéricos reales
letras_medias_reales <- letras_medias %>%
  as.data.frame() %>%
  dplyr::select(Trat_factor, .group) %>%
  left_join(medias_reales, by = "Trat_factor")

cat("\n> VERSIÓN B: Tabla de Medias REALES (Aritméticas directas):\n")
print(letras_medias_reales)

# --- GRÁFICO 5 (Nuevo): Gráfico de resultados con Medias REALES
g_final_reales <- ggplot(letras_medias_reales, aes(y = reorder(Trat_factor, media_real), x = media_real)) +
  geom_errorbarh(aes(xmin = media_real - SE_real, xmax = media_real + SE_real), 
                 height = 0.2, color = "black", linewidth = 0.8) +
  geom_point(color = "black", size = 3) +
  geom_text(aes(label = round(media_real, 2)), vjust = -1.2, size = 3.5, color = "black") +
  geom_text(aes(x = media_real + SE_real, label = .group), 
            hjust = -0.5, size = 4.5, fontface = "bold", color = "black") +
  labs(title = paste("Prueba de Medias REALES (Observadas) para", VAR_RESPUESTA),
       subtitle = "Medias muestrales directas con letras de significancia del LMM",
       x = paste(VAR_RESPUESTA, "(Media Real ± Error Estándar Real)"), y = FACTOR_TRAT) +
  theme_minimal() +
  theme(axis.text.y = element_text(face = "bold", size = 11),
        panel.grid.major.y = element_blank())
print(g_final_reales)


# 5. Generación de tabla comparativa de valores predichos frente a los reales
medias_rep_df   <- as.data.frame(emmeans(modelo_rep, ~ Trat_factor)) %>% dplyr::select(Trat_factor, Rep = emmean)
medias_trat_df  <- as.data.frame(emmeans(modelo_trat, ~ Trat_factor)) %>% dplyr::select(Trat_factor, Trat = emmean)
medias_inter_df <- as.data.frame(emmeans(modelo_inter, ~ Trat_factor)) %>% dplyr::select(Trat_factor, Inter = emmean)

resumen_modelos <- medias_rep_df %>% 
  left_join(medias_trat_df, by = "Trat_factor") %>% 
  left_join(medias_inter_df, by = "Trat_factor") %>%
  left_join(medias_reales %>% dplyr::select(Trat_factor, Real = media_real), by = "Trat_factor")

# Devolver el nombre original de la columna para la presentación final
colnames(resumen_modelos)[1] <- FACTOR_TRAT

cat("\n> Resumen comparativo de medias estimadas y reales por cada modelo:\n")
print(resumen_modelos)
View(resumen_modelos)

cat("\n--- ¡PROCESO FINALIZADO CON ÉXITO! ---\n")
|