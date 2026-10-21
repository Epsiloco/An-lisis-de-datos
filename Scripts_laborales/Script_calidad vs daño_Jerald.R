

# ==============================================================================
# ANÁLISIS DE NIVEL DE DAÑO VS CALIDAD DE JUGO - COCHINILLA -
# ==============================================================================

# --- 0. PAQUETES Y DEPENDENCIAS ---
paquetes <- c("dplyr", "lubridate", "openxlsx", "tidyverse", "corrplot", 
              "ggpubr", "vegan", "FactoMineR", "factoextra", "segmented", 
              "patchwork", "scales", "broom", "agricolae", "ggrepel", "car")

for (p in paquetes) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}

# ==============================================================================
# BLOQUE 1: LECTURA, LIMPIEZA E INTEGRACIÓN DE DATOS
# ==============================================================================

# --- 1. Lectura desde portapapeles ---
PC_2027       <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "") # Actualizada
RP_MRSA       <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")
BD_cochinilla <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "") # Actualizada

# --- 2. Renombrar columnas en PC_2027 ---
PC_2027 <- PC_2027 %>%
  dplyr::rename(
    Pureza    = `Pureza...`,
    Rdto_kg_t = `Rdto...kg.t`,
    N_Muestra = `N...Muestra`
  )

# --- 3. Extraer y limpiar variables de BD_cochinilla ---
BD_limpio <- BD_cochinilla %>%
  dplyr::select(
    Terreno,
    ND         = ND,
    Incidencia = `X..de.Incidencia`
  ) %>%
  dplyr::filter(!is.na(Terreno))

limpiar_pct <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("\u00A0", "", x)
  x <- gsub("%", "", x)
  x <- gsub(",", ".", x)
  x[x == ""] <- NA
  suppressWarnings(as.numeric(x))
}

BD_limpio$Incidencia <- limpiar_pct(BD_limpio$Incidencia) / 100

moda <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

BD_incid <- BD_limpio %>%
  dplyr::group_by(Terreno) %>%
  dplyr::summarise(
    Incidencia = mean(Incidencia, na.rm = TRUE),
    ND         = moda(ND),
    .groups    = "drop"
  )

cat("\nValores únicos de ND (originales):\n")
print(unique(BD_incid$ND))

# --- 4. Formateo de Fechas y Limpieza ---
PC_2027$Fecha          <- as.Date(PC_2027$Fecha, format = "%d/%m/%Y")
RP_MRSA$Fecha.de.corte <- as.Date(RP_MRSA$Fecha.de.corte, format = "%d/%m/%Y")

PC_2027 <- PC_2027 %>%
  dplyr::mutate(
    Pureza    = limpiar_pct(Pureza),
    Rdto_kg_t = limpiar_pct(Rdto_kg_t)
  )

RP_fechas <- RP_MRSA %>%
  dplyr::select(Terreno, Fecha.de.corte) %>%
  dplyr::filter(!is.na(Terreno), !is.na(Fecha.de.corte)) %>%
  dplyr::group_by(Terreno) %>%
  dplyr::summarise(
    Fecha_corte = min(Fecha.de.corte, na.rm = TRUE),
    .groups     = "drop"
  )

# --- 5. Integración Final ---
PC_2027_full <- PC_2027 %>%
  dplyr::left_join(RP_fechas, by = "Terreno") %>%
  dplyr::left_join(BD_incid,  by = "Terreno") %>%
  dplyr::mutate(Edad_Actual = as.numeric(Fecha - Fecha_corte))

PC_2027_res <- PC_2027_full %>%
  dplyr::select(
    Terreno, N_Muestra, Fecha, Edad_Actual,
    Pureza, Rdto_kg_t, Incidencia, ND
  )

cat("\n>>> Vista previa PC_2027_res:\n")
print(head(PC_2027_res))

# --- 6. Data Resumida por Terreno ---
Resumen <- PC_2027_res %>%
  dplyr::group_by(Terreno) %>%
  dplyr::summarise(
    n_muestreos = n(),
    Edad_prom   = mean(Edad_Actual, na.rm = TRUE),
    Pureza_prom = mean(Pureza,      na.rm = TRUE),
    Rdto_prom   = mean(Rdto_kg_t,   na.rm = TRUE),
    Incid_prom  = mean(Incidencia,  na.rm = TRUE),
    ND          = moda(ND),
    .groups     = "drop"
  ) %>%
  dplyr::arrange(Terreno)

cat("\n>>> Vista previa Resumen:\n")
print(head(Resumen))

cat("\n>>> Distribución de ND por Terreno (Resumen):\n")
print(table(Resumen$ND, useNA = "ifany"))

cat("\n>>> Terrenos sin ND asignado:\n")
print(Resumen %>% dplyr::filter(is.na(ND)) %>% dplyr::pull(Terreno))


# ==============================================================================
# BLOQUE 2: EXPORTACIÓN A EXCEL
# ==============================================================================

fecha_hoy <- format(Sys.Date(), "%Y%m%d")
archivo   <- paste0("PC_2027_integrado_2", fecha_hoy, ".xlsx")

wb <- createWorkbook()

addWorksheet(wb, "PC_2027_res")
writeData(wb, "PC_2027_res", PC_2027_res)

addWorksheet(wb, "Resumen")
writeData(wb, "Resumen", Resumen)

estilo_cab <- createStyle(
  textDecoration = "bold",
  fgFill         = "#D9E1F2",
  border         = "TopBottom",
  halign         = "center"
)

addStyle(wb, "PC_2027_res", estilo_cab, rows = 1, cols = 1:ncol(PC_2027_res), gridExpand = TRUE)
addStyle(wb, "Resumen",     estilo_cab, rows = 1, cols = 1:ncol(Resumen),     gridExpand = TRUE)

setColWidths(wb, "PC_2027_res", cols = 1:ncol(PC_2027_res), widths = "auto")
setColWidths(wb, "Resumen",     cols = 1:ncol(Resumen),     widths = "auto")

freezePane(wb, "PC_2027_res", firstRow = TRUE)
freezePane(wb, "Resumen",     firstRow = TRUE)

saveWorkbook(wb, archivo, overwrite = TRUE)
cat("\n>>> Excel guardado exitosamente en:", normalizePath(archivo), "\n")


# ==============================================================================
# BLOQUE 3: ANÁLISIS ESTADÍSTICO Y MODELACIÓN
# ==============================================================================

# ------------------------------------------------------------------------------
# CONFIGURACIÓN INICIAL - MODIFICAR AQUÍ
# ------------------------------------------------------------------------------
tipo_data_analisis <- "PC"                    # "PC" o "Resumen" ---Recomendable trabajarlo con "PC" para mejor ajuste de modelos
var_predictor      <- "ND"                    # "ND", "Incidencia", etc.
tipo_predictor     <- "categorica"            # "categorica" o "numerica"
tipo_prueba        <- "kruskal"                 # "anova" (paramétrica) o "kruskal" (no paramétrica)
nombre_predictor   <- "N. daño"               # Nombre para etiquetas
n_grupos_numerico  <- 4                       # Cuartiles para heatmap numérico (3, 4, 5, etc.)
alpha_test         <- 0.15                    # Nivel de significancia global

# Orden personalizado (solo si tipo_predictor == "categorica")
orden_categorias <- c("Sin presencia", "Bajo (5%)", "Medio (15%)", "Alto (25%)", "Muy alto (>25%)")

# Etiqueta descriptiva de la prueba seleccionada
etiqueta_prueba <- ifelse(
  tipo_prueba == "anova",
  paste0("ANOVA + Fisher LSD α = ", alpha_test * 100, "%"),
  paste0("Kruskal-Wallis + LSD en rangos α = ", alpha_test * 100, "%")
)

cat("\n========================================================\n")
cat("EJECUTANDO ANÁLISIS:\n")
cat("  - Data:", tipo_data_analisis, "\n")
cat("  - Predictor:", var_predictor, "(", tipo_predictor, ")\n")
cat("  - Prueba:", etiqueta_prueba, "\n")
cat("========================================================\n")

# ------------------------------------------------------------------------------
# SELECCIÓN Y PREPARACIÓN DE DATA
# ------------------------------------------------------------------------------
if (tipo_data_analisis == "PC") {
  datos_raw <- PC_2027_res
} else if (tipo_data_analisis == "Resumen") {
  datos_raw <- Resumen %>%
    dplyr::rename(
      Pureza    = Pureza_prom,
      Rdto_kg_t = Rdto_prom,
      Incidencia = Incid_prom
    )
} else {
  stop("Opción no válida. Seleccione 'PC' o 'Resumen'.")
}

if (!var_predictor %in% names(datos_raw)) {
  stop(paste0("La variable '", var_predictor, "' no existe. ",
              "Columnas disponibles: ", paste(names(datos_raw), collapse = ", ")))
}

vars_calidad <- c("Pureza", "Rdto_kg_t")

datos <- datos_raw %>%
  dplyr::rename(PRED = all_of(var_predictor)) %>%
  dplyr::filter(!is.na(PRED), !is.na(Pureza), !is.na(Rdto_kg_t))

if (tipo_predictor == "categorica") {
  orden_final <- orden_categorias[orden_categorias %in% unique(datos$PRED)]
  datos$PRED  <- factor(datos$PRED, levels = orden_final, ordered = TRUE)
  datos$PRED_num <- as.numeric(datos$PRED)
  niveles_pred   <- levels(datos$PRED)
  ref_level      <- niveles_pred[1]
} else {
  datos$PRED     <- as.numeric(datos$PRED)
  datos$PRED_num <- datos$PRED
  niveles_pred   <- NULL
  ref_level      <- min(datos$PRED, na.rm = TRUE)
}

datos_limpios <- datos[, c("PRED", "PRED_num", vars_calidad)] %>% na.omit()

cat("Dimensiones analizadas:", dim(datos_limpios), "\n")
if (tipo_predictor == "categorica") {
  cat("Niveles del predictor:", paste(niveles_pred, collapse = " | "), "\n")
  cat("Frecuencia (n) por nivel:\n"); print(table(datos_limpios$PRED))
  cat("Nivel de referencia:", ref_level, "\n")
} else {
  cat("Rango del predictor:", range(datos_limpios$PRED), "\n")
  cat("Valor de referencia (mínimo):", ref_level, "\n")
}

# ------------------------------------------------------------------------------
# A. CORRELOGRAMA PEARSON
# ------------------------------------------------------------------------------
vars_cor <- c("PRED_num", vars_calidad)
matriz_pearson <- cor(datos_limpios[, vars_cor], method = "pearson")
matriz_pval    <- cor.mtest(datos_limpios[, vars_cor], method = "pearson")$p

corrplot(
  matriz_pearson, method = "circle", type = "lower", order = "original",
  tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.9,
  p.mat = matriz_pval, sig.level = alpha_test, insig = "pch",
  pch.col = "black", pch.cex = 1.4,
  title = paste0("Correlograma Pearson (Data: ", tipo_data_analisis,
                 " | Predictor: ", nombre_predictor, ")"),
  mar = c(0, 0, 3, 0)
)

for (v in vars_calidad) {
  cat("\n--- Pearson:", var_predictor, "vs", v, "---\n")
  print(cor.test(datos_limpios$PRED_num, datos_limpios[[v]], method = "pearson"))
}

# ------------------------------------------------------------------------------
# B. CORRELOGRAMA SPEARMAN (variables categóricas / ordinales)
# ------------------------------------------------------------------------------
datos_spearman <- datos_limpios
for (v in vars_calidad) {
  if (is.factor(datos_spearman[[v]]) || is.character(datos_spearman[[v]])) {
    datos_spearman[[v]] <- as.numeric(factor(datos_spearman[[v]],
                                             levels = unique(datos_spearman[[v]]),
                                             ordered = TRUE))
  }
}

vars_cor_sp <- c("PRED_num", vars_calidad)

matriz_spearman <- cor(datos_spearman[, vars_cor_sp],
                       method = "spearman", use = "pairwise.complete.obs")
matriz_pval_sp  <- cor.mtest(datos_spearman[, vars_cor_sp],
                             method = "spearman")$p

corrplot(
  matriz_spearman, method = "circle", type = "lower", order = "original",
  tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.9,
  p.mat = matriz_pval_sp, sig.level = alpha_test,
  insig = "pch", pch.col = "black", pch.cex = 1.4,
  title = paste0("Correlograma Spearman (Data: ", tipo_data_analisis,
                 " | Predictor: ", nombre_predictor, ")"),
  mar = c(0, 0, 3, 0)
)

for (v in vars_calidad) {
  cat("\n--- Spearman:", var_predictor, "vs", v, "---\n")
  print(cor.test(datos_spearman$PRED_num, datos_spearman[[v]],
                 method = "spearman", exact = FALSE))
}

# ------------------------------------------------------------------------------
# B.2  ALTERNATIVA: V de Cramér (asociación entre categóricas nominales)
# ------------------------------------------------------------------------------
if (!requireNamespace("vcd", quietly = TRUE)) install.packages("vcd")
library(vcd)

cramer_v <- function(x, y) {
  tbl <- table(x, y)
  chi <- suppressWarnings(chisq.test(tbl, correct = FALSE)$statistic)
  n   <- sum(tbl)
  k   <- min(nrow(tbl), ncol(tbl))
  sqrt(as.numeric(chi) / (n * (k - 1)))
}

vars_cat <- intersect(vars_calidad, names(datos_limpios))
matriz_cramer <- matrix(NA, length(vars_cat), length(vars_cat),
                        dimnames = list(vars_cat, vars_cat))
matriz_pval_cramer <- matriz_cramer

for (i in seq_along(vars_cat)) {
  for (j in seq_along(vars_cat)) {
    if (i == j) {
      matriz_cramer[i, j]     <- 1
      matriz_pval_cramer[i, j] <- 0
    } else if (j < i) {
      tbl <- table(datos_limpios[[vars_cat[i]]], datos_limpios[[vars_cat[j]]])
      matriz_cramer[i, j]     <- cramer_v(datos_limpios[[vars_cat[i]]],
                                          datos_limpios[[vars_cat[j]]])
      matriz_pval_cramer[i, j] <- suppressWarnings(
        chisq.test(tbl, correct = FALSE)$p.value)
      matriz_cramer[j, i]     <- matriz_cramer[i, j]
      matriz_pval_cramer[j, i] <- matriz_pval_cramer[i, j]
    }
  }
}

corrplot(
  matriz_cramer, method = "circle", type = "lower", order = "original",
  tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.9,
  p.mat = matriz_pval_cramer, sig.level = alpha_test,
  insig = "pch", pch.col = "black", pch.cex = 1.4,
  is.corr = FALSE,
  title = paste0("Correlograma V de Cramér (categóricas | Data: ",
                 tipo_data_analisis, ")"),
  mar = c(0, 0, 3, 0)
)

# ==============================================================================
# B.3  VERIFICACIÓN DE SUPUESTOS (para elegir ANOVA vs Kruskal-Wallis)
# ==============================================================================
# Comprueba: normalidad de residuos (Shapiro por grupo y global) y
# homocedasticidad (Levene). Genera QQ-plot y residuos vs predichos.
# Solo aplica si el predictor es categórico.

if (tipo_predictor == "categorica") {
  
  cat("\n========================================================\n")
  cat("VERIFICACIÓN DE SUPUESTOS (ANOVA)\n")
  cat("========================================================\n")
  
  diagnosticos_supuestos <- list()
  
  for (v in vars_calidad) {
    
    form <- as.formula(paste(v, "~ PRED"))
    mod  <- aov(form, data = datos_limpios)
    
    res     <- residuals(mod)
    fitted_ <- fitted(mod)
    grupos  <- datos_limpios$PRED
    
    # --- 1. Shapiro-Wilk global sobre residuos ---
    sw_global <- tryCatch(shapiro.test(res), error = function(e) NULL)
    
    # --- 2. Shapiro-Wilk por grupo ---
    sw_grupo <- datos_limpios %>%
      dplyr::group_by(PRED) %>%
      dplyr::summarise(
        n       = dplyr::n(),
        SW_W    = tryCatch(shapiro.test(.data[[v]])$statistic, error = function(e) NA),
        SW_p    = tryCatch(shapiro.test(.data[[v]])$p.value,   error = function(e) NA),
        .groups = "drop"
      ) %>%
      dplyr::mutate(Variable = v)
    
    # --- 3. Levene (homocedasticidad) ---
    levene_res <- tryCatch(car::leveneTest(form, data = datos_limpios),
                           error = function(e) NULL)
    
    # --- 4. Bartlett (solo si hay normalidad razonable) ---
    bartlett_res <- tryCatch(bartlett.test(form, data = datos_limpios),
                             error = function(e) NULL)
    
    diagnosticos_supuestos[[v]] <- list(
      mod = mod, res = res, fitted = fitted_,
      sw_global = sw_global, sw_grupo = sw_grupo,
      levene = levene_res, bartlett = bartlett_res
    )
    
    # --- Reporte en consola ---
    cat("\n>>>>>>>>>> Variable:", v, "<<<<<<<<<<\n")
    cat("\n  [Shapiro-Wilk GLOBAL sobre residuos]\n")
    if (!is.null(sw_global)) {
      cat("    W =", round(sw_global$statistic, 4),
          "| p =", format.pval(sw_global$p.value, digits = 4), "\n")
      cat("    →", ifelse(sw_global$p.value < alpha_test,
                          "NO normal (rechaza H0)",
                          "Compatible con normalidad"), "\n")
    } else cat("    No calculable\n")
    
    cat("\n  [Shapiro-Wilk POR GRUPO]\n")
    print(as.data.frame(sw_grupo %>%
                          dplyr::mutate(dplyr::across(where(is.numeric),
                                                      ~round(., 4)))), row.names = FALSE)
    
    cat("\n  [Levene - homocedasticidad]\n")
    if (!is.null(levene_res)) {
      p_lev <- levene_res[["Pr(>F)"]][1]
      cat("    F =", round(levene_res[["F value"]][1], 4),
          "| p =", format.pval(p_lev, digits = 4), "\n")
      cat("    →", ifelse(p_lev < alpha_test,
                          "Varianzas HETEROGÉNEAS (rechaza H0)",
                          "Varianzas homogéneas"), "\n")
    } else cat("    No calculable\n")
    
    cat("\n  [Bartlett - homocedasticidad (asume normalidad)]\n")
    if (!is.null(bartlett_res)) {
      cat("    K² =", round(bartlett_res$statistic, 4),
          "| p =", format.pval(bartlett_res$p.value, digits = 4), "\n")
    } else cat("    No calculable\n")
  }
  
  # ---------------------------------------------------------------------------
  # GRÁFICOS DIAGNÓSTICOS: QQ-plot + Residuos vs Predichos
  # ---------------------------------------------------------------------------
  for (v in vars_calidad) {
    
    d     <- diagnosticos_supuestos[[v]]
    res   <- d$res
    fit   <- d$fitted
    
    # Estandarizar residuos para QQ plot
    res_std <- scale(res)[, 1]
    
    # --- QQ plot base ---
    qq_df <- data.frame(
      teorico   = qqnorm(res_std, plot.it = FALSE)$x,
      muestral  = qqnorm(res_std, plot.it = FALSE)$y
    )
    
    p_qq <- ggplot(qq_df, aes(x = teorico, y = muestral)) +
      geom_abline(slope = 1, intercept = 0, color = "red",
                  linetype = "dashed", size = 0.8) +
      geom_point(color = "steelblue", alpha = 0.7, size = 2) +
      labs(
        title    = paste0("QQ-plot de residuos estandarizados: ", v),
        subtitle = paste0("Shapiro-Wilk global p = ",
                          format.pval(d$sw_global$p.value, digits = 4)),
        x = "Cuantiles teóricos (Normal)",
        y = "Cuantiles muestrales"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title    = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, color = "gray40"))
    
    # --- Residuos vs Predichos ---
    rv_df <- data.frame(
      Fitted  = fit,
      Resid   = res,
      Grupo   = datos_limpios$PRED
    )
    
    p_rv <- ggplot(rv_df, aes(x = Fitted, y = Resid)) +
      geom_hline(yintercept = 0, color = "red",
                 linetype = "dashed", size = 0.8) +
      geom_point(aes(color = Grupo), alpha = 0.75, size = 2.2) +
      geom_smooth(method = "loess", se = TRUE, color = "black",
                  fill = "gray70", alpha = 0.2, size = 0.9) +
      labs(
        title    = paste0("Residuos vs Predichos: ", v),
        subtitle = "Referencia: residuo = 0 (rojo) | Línea negra = tendencia LOESS",
        x = "Valores predichos",
        y = "Residuos"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title    = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
            legend.position = "top")
    
    print(p_qq)
    print(p_rv)
    
    # --- QQ por grupo (facet) ---
    qq_grupo <- datos_limpios %>%
      dplyr::select(PRED, all_of(v)) %>%
      dplyr::group_by(PRED) %>%
      dplyr::mutate(
        res_grupo = scale(.data[[v]])[, 1],
        teorico   = qqnorm(res_grupo, plot.it = FALSE)$x,
        muestral  = qqnorm(res_grupo, plot.it = FALSE)$y
      ) %>%
      dplyr::ungroup()
    
    p_qq_grupo <- ggplot(qq_grupo, aes(x = teorico, y = muestral)) +
      geom_abline(slope = 1, intercept = 0, color = "red",
                  linetype = "dashed", size = 0.7) +
      geom_point(color = "darkgreen", alpha = 0.7, size = 1.8) +
      facet_wrap(~ PRED, scales = "free") +
      labs(
        title    = paste0("QQ-plot por grupo (dentro de nivel): ", v),
        subtitle = "Normalidad por grupo tras estandarización intra-grupo",
        x = "Cuantiles teóricos",
        y = "Cuantiles muestrales"
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title    = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
            strip.text    = element_text(face = "bold"))
    
    print(p_qq_grupo)
  }
  
  # ---------------------------------------------------------------------------
  # RECOMENDACIÓN AUTOMÁTICA
  # ---------------------------------------------------------------------------
  cat("\n========================================================\n")
  cat("RECOMENDACIÓN AUTOMÁTICA SEGÚN SUPUESTOS\n")
  cat("========================================================\n")
  
  recomendacion <- sapply(vars_calidad, function(v) {
    d <- diagnosticos_supuestos[[v]]
    p_sw <- d$sw_global$p.value
    p_lev <- if (!is.null(d$levene)) d$levene[["Pr(>F)"]][1] else NA
    normal_ok  <- !is.na(p_sw)  && p_sw  >= alpha_test
    homoced_ok <- !is.na(p_lev) && p_lev >= alpha_test
    if (normal_ok && homoced_ok) "anova" else "kruskal"
  })
  
  print(data.frame(Variable = names(recomendacion),
                   Prueba_sugerida = recomendacion,
                   row.names = NULL))
  
  if (all(recomendacion == "kruskal") && tipo_prueba == "anova") {
    cat("\n⚠  Todos los supuestos sugieren KRUSKAL-WALLIS.",
        "\n   Considera cambiar tipo_prueba <- 'kruskal'.\n")
  } else if (all(recomendacion == "anova") && tipo_prueba == "kruskal") {
    cat("\n✔  Supuestos compatibles con ANOVA.",
        "\n   Puedes usar tipo_prueba <- 'anova'.\n")
  } else if (any(recomendacion == "kruskal") && tipo_prueba == "anova") {
    cat("\n⚠  Al menos una variable sugiere KRUSKAL-WALLIS.",
        "\n   Revisa los gráficos y considera cambiar de prueba.\n")
  }
  
  cat("\n[Prueba actualmente configurada]:", etiqueta_prueba, "\n")
  cat("========================================================\n")
  
} else {
  cat("\n>>> Predictor numérico: verificación de supuestos se realiza sobre el modelo lineal.\n")
  
  # Para predictor numérico: diagnóstico de residuos del modelo lineal
  for (v in vars_calidad) {
    mod  <- lm(as.formula(paste(v, "~ PRED_num")), data = datos_limpios)
    res  <- residuals(mod)
    fit  <- fitted(mod)
    
    sw <- tryCatch(shapiro.test(res), error = function(e) NULL)
    
    qq_df <- data.frame(
      teorico  = qqnorm(scale(res)[, 1], plot.it = FALSE)$x,
      muestral = qqnorm(scale(res)[, 1], plot.it = FALSE)$y
    )
    
    p_qq <- ggplot(qq_df, aes(x = teorico, y = muestral)) +
      geom_abline(slope = 1, intercept = 0, color = "red",
                  linetype = "dashed", size = 0.8) +
      geom_point(color = "steelblue", alpha = 0.7, size = 2) +
      labs(title = paste0("QQ-plot de residuos: ", v),
           subtitle = ifelse(!is.null(sw),
                             paste0("Shapiro-Wilk p = ",
                                    format.pval(sw$p.value, digits = 4)), ""),
           x = "Cuantiles teóricos", y = "Cuantiles muestrales") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, color = "gray40"))
    
    p_rv <- ggplot(data.frame(Fitted = fit, Resid = res),
                   aes(x = Fitted, y = Resid)) +
      geom_hline(yintercept = 0, color = "red",
                 linetype = "dashed", size = 0.8) +
      geom_point(color = "steelblue", alpha = 0.7, size = 2) +
      geom_smooth(method = "loess", se = TRUE, color = "black",
                  fill = "gray70", alpha = 0.2, size = 0.9) +
      labs(title = paste0("Residuos vs Predichos: ", v),
           subtitle = "Referencia: residuo = 0 (rojo)",
           x = "Valores predichos", y = "Residuos") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, color = "gray40"))
    
    print(p_qq); print(p_rv)
  }
}

# ------------------------------------------------------------------------------
# C. PRUEBA DE GRUPOS: ANOVA + Fisher LSD  ó  Kruskal-Wallis + LSD en rangos
# ------------------------------------------------------------------------------
obtener_letras <- function(data, var_name, alpha = 0.15, tipo_prueba = "kruskal") {
  
  if (tipo_prueba == "anova") {
    form   <- as.formula(paste(var_name, "~ PRED"))
    modelo <- aov(form, data = data)
    lsd    <- agricolae::LSD.test(modelo, "PRED", alpha = alpha, console = FALSE)
    g      <- lsd$groups
    g$PRED <- rownames(g)
    colnames(g)[which(colnames(g) == "groups")] <- "Letra"
    list(letras = g[, c("PRED", "Letra")], modelo = modelo, test = "anova")
    
  } else if (tipo_prueba == "kruskal") {
    kw <- agricolae::kruskal(data[[var_name]], data$PRED,
                             alpha = alpha, group = TRUE, console = FALSE)
    g      <- kw$groups
    g$PRED <- rownames(g)
    colnames(g)[which(colnames(g) == "groups")] <- "Letra"
    list(letras = g[, c("PRED", "Letra")], modelo = kw, test = "kruskal")
    
  } else stop("tipo_prueba debe ser 'anova' o 'kruskal'")
}

if (tipo_predictor == "categorica") {
  
  prueba_res <- list()
  letras_lsd <- list()
  
  for (v in vars_calidad) {
    r <- obtener_letras(datos_limpios, v,
                        alpha = alpha_test, tipo_prueba = tipo_prueba)
    prueba_res[[v]] <- r$modelo
    letras_lsd[[v]] <- r$letras %>% dplyr::mutate(Variable = v)
  }
  
  # --- Reporte por consola ---
  if (tipo_prueba == "anova") {
    cat("\n========== ANOVA POR VARIABLE ==========\n")
    for (v in vars_calidad) {
      cat("\n>>> Variable:", v, "\n")
      print(as.data.frame(summary(prueba_res[[v]])[[1]]), row.names = FALSE)
    }
    cat("\n========== LETRAS FISHER LSD (α = 15%) ==========\n")
  } else {
    cat("\n========== KRUSKAL-WALLIS POR VARIABLE ==========\n")
    for (v in vars_calidad) {
      cat("\n>>> Variable:", v, "\n")
      st <- prueba_res[[v]]$statistics
      cat("Chi-cuadrado =", round(st[[1]], 3),
          "| gl =", prueba_res[[v]]$parameters$df,
          "| p-valor =", format.pval(st[[2]], digits = 4), "\n")
    }
    cat("\n========== LETRAS LSD SOBRE RANGOS (α = 15%) ==========\n")
  }
  
  letras_lsd <- dplyr::bind_rows(letras_lsd) %>%
    dplyr::mutate(PRED = factor(PRED, levels = niveles_pred, ordered = TRUE))
  
  print(as.data.frame(letras_lsd), row.names = FALSE)
  
} else {
  cat("\n>>> Predictor numérico: la prueba por grupos se calcula dentro del heatmap por cuantiles\n")
  letras_lsd <- NULL
}

# ------------------------------------------------------------------------------
# D. GRÁFICOS DESCRIPTIVOS
# ------------------------------------------------------------------------------
if (tipo_predictor == "categorica") {
  
  stats_grupo <- datos_limpios %>%
    tidyr::pivot_longer(cols = all_of(vars_calidad), names_to = "Variable", values_to = "Valor") %>%
    dplyr::group_by(Variable, PRED) %>%
    dplyr::summarise(
      Media   = mean(Valor, na.rm = TRUE),
      SE      = sd(Valor, na.rm = TRUE) / sqrt(dplyr::n()),
      .groups = "drop"
    ) %>%
    dplyr::left_join(letras_lsd, by = c("Variable", "PRED")) %>%
    dplyr::group_by(Variable) %>%
    dplyr::mutate(y_letra = Media + SE + 0.05 * diff(range(Media, na.rm = TRUE))) %>%
    dplyr::ungroup()
  
  grafico_desc <- ggplot(stats_grupo, aes(x = PRED, y = Media, group = 1)) +
    geom_line(color = "steelblue", size = 1) +
    geom_errorbar(aes(ymin = Media - SE, ymax = Media + SE),
                  width = 0.15, color = "steelblue", size = 0.8) +
    geom_point(size = 3.5, color = "steelblue") +
    geom_text(aes(label = Letra, y = y_letra), size = 4.5,
              fontface = "bold", color = "darkred") +
    facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
    labs(
      title    = paste0("Curvas Descriptivas por ", nombre_predictor,
                        " (Data: ", tipo_data_analisis, ")"),
      subtitle = paste0("Media ± EE | ", etiqueta_prueba),
      x = nombre_predictor, y = "Media ± EE"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
      strip.text    = element_text(face = "bold"),
      axis.text.x   = element_text(angle = 20, hjust = 1)
    )
  print(grafico_desc)
  
} else {
  
  datos_long <- datos_limpios %>%
    tidyr::pivot_longer(cols = all_of(vars_calidad),
                        names_to = "Variable", values_to = "Valor")
  
  grafico_desc <- ggplot(datos_long, aes(x = PRED, y = Valor)) +
    geom_point(color = "steelblue", alpha = 0.5, size = 2) +
    geom_smooth(method = "lm", color = "darkred", fill = "red", alpha = 0.15) +
    facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
    labs(
      title    = paste0("Relación ", nombre_predictor, " vs Calidad"),
      subtitle = "Ajuste lineal con banda de confianza al 95%",
      x = nombre_predictor, y = "Valor"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
      strip.text    = element_text(face = "bold")
    )
  print(grafico_desc)
}

# ------------------------------------------------------------------------------
# E. MODELOS LINEALES
# ------------------------------------------------------------------------------
mod_lin_pureza <- lm(Pureza    ~ PRED_num, data = datos_limpios)
mod_lin_rdto   <- lm(Rdto_kg_t ~ PRED_num, data = datos_limpios)

cat("\n=== Pureza ~ PRED_num (Lineal) ===\n"); print(summary(mod_lin_pureza))
cat("\n=== Rdto ~ PRED_num (Lineal) ===\n"); print(summary(mod_lin_rdto))

if (tipo_predictor == "categorica") {
  mod_fac_pureza <- lm(Pureza    ~ PRED, data = datos_limpios)
  mod_fac_rdto   <- lm(Rdto_kg_t ~ PRED, data = datos_limpios)
} else {
  mod_fac_pureza <- mod_lin_pureza
  mod_fac_rdto   <- mod_lin_rdto
}

# ------------------------------------------------------------------------------
# F. MODELOS CUADRÁTICOS
# ------------------------------------------------------------------------------
mod_cuad_pureza <- lm(Pureza    ~ PRED_num + I(PRED_num^2), data = datos_limpios)
mod_cuad_rdto   <- lm(Rdto_kg_t ~ PRED_num + I(PRED_num^2), data = datos_limpios)

cat("\n--- ANOVA: Lineal vs Cuadrático (Pureza) ---\n")
print(as.data.frame(anova(mod_lin_pureza, mod_cuad_pureza)))
cat("\n--- ANOVA: Lineal vs Cuadrático (Rdto) ---\n")
print(as.data.frame(anova(mod_lin_rdto, mod_cuad_rdto)))

info_cuad <- function(mod) {
  cc   <- summary(mod)$coefficients
  b    <- cc["PRED_num", "Estimate"]
  c    <- cc["I(PRED_num^2)", "Estimate"]
  p_c  <- cc["I(PRED_num^2)", "Pr(>|t|)"]
  vert <- if (c != 0) -b / (2 * c) else NA_real_
  tipo <- if (c < 0) "Máximo" else if (c > 0) "Mínimo" else "Lineal"
  list(b = b, c = c, p_cuad = p_c, vertice = vert, tipo = tipo,
       r2 = summary(mod)$r.squared)
}
ic_pureza <- info_cuad(mod_cuad_pureza)
ic_rdto   <- info_cuad(mod_cuad_rdto)

# ------------------------------------------------------------------------------
# G. REGRESIÓN SEGMENTADA
# ------------------------------------------------------------------------------
psi_init <- median(datos_limpios$PRED_num)

mod_seg_pureza <- tryCatch(
  segmented::segmented(mod_lin_pureza, seg.Z = ~PRED_num, psi = psi_init),
  error = function(e) NULL)
mod_seg_rdto <- tryCatch(
  segmented::segmented(mod_lin_rdto,   seg.Z = ~PRED_num, psi = psi_init),
  error = function(e) NULL)

psi_pureza <- if (!is.null(mod_seg_pureza)) mod_seg_pureza$psi[, "Est."] else NA
psi_rdto   <- if (!is.null(mod_seg_rdto))   mod_seg_rdto$psi[, "Est."]   else NA

# ------------------------------------------------------------------------------
# H. GRÁFICO CUADRÁTICO
# ------------------------------------------------------------------------------
rango_pred <- range(datos_limpios$PRED_num, na.rm = TRUE)

grafico_cuad <- function(var_y, mod, titulo, info) {
  pred_data <- data.frame(PRED_num = seq(rango_pred[1], rango_pred[2], length.out = 300))
  pred      <- predict(mod, newdata = pred_data, interval = "confidence")
  pred_df   <- data.frame(PRED_num = pred_data$PRED_num,
                          Fit = pred[, "fit"],
                          Lower = pred[, "lwr"], Upper = pred[, "upr"])
  
  lab_p <- dplyr::case_when(
    info$p_cuad < 0.001 ~ "Curvatura: p < 0.001 ***",
    info$p_cuad < 0.01  ~ paste0("Curvatura: p = ", sprintf("%.3f", info$p_cuad), " **"),
    info$p_cuad < 0.05  ~ paste0("Curvatura: p = ", sprintf("%.3f", info$p_cuad), " *"),
    info$p_cuad < 0.15  ~ paste0("Curvatura: p = ", sprintf("%.3f", info$p_cuad), " ."),
    TRUE                ~ paste0("Curvatura: p = ", sprintf("%.3f", info$p_cuad), " ns")
  )
  
  curvatura_ok <- info$p_cuad < 0.15
  y_v          <- predict(mod, newdata = data.frame(PRED_num = info$vertice))
  
  g <- ggplot() +
    geom_ribbon(data = pred_df, aes(x = PRED_num, ymin = Lower, ymax = Upper),
                fill = "darkgreen", alpha = 0.15) +
    geom_line(data = pred_df, aes(x = PRED_num, y = Fit),
              color = "darkgreen", size = 1.3) +
    geom_point(data = datos_limpios, aes(x = PRED_num, y = .data[[var_y]]),
               color = "#D73027", alpha = 0.6, size = 2.3)
  
  if (curvatura_ok && !is.na(info$vertice)) {
    g <- g +
      geom_vline(xintercept = info$vertice, color = "red",
                 linetype = "dashed", size = 1) +
      geom_point(aes(x = info$vertice, y = y_v), shape = 8, size = 4.5,
                 color = "red", stroke = 1.2) +
      annotate("text", x = info$vertice, y = y_v,
               label = sprintf("Vértice (%s)\n%s = %.2f",
                               info$tipo, nombre_predictor, info$vertice),
               hjust = -0.08, vjust = 1.6, size = 3.5,
               color = "red", fontface = "bold.italic")
  }
  
  g +
    annotate("text", x = rango_pred[1], y = -Inf, label = lab_p,
             hjust = 0, vjust = -1.5, size = 3.4,
             fontface = "italic", color = "darkgreen") +
    annotate("text", x = rango_pred[2], y = -Inf,
             label = sprintf("R² = %.3f", info$r2),
             hjust = 1, vjust = -1.5, size = 3.4,
             fontface = "italic", color = "gray40") +
    labs(title = titulo,
         subtitle = "Ajuste Cuadrático | Vértice si p_curvatura < 0.15",
         x = nombre_predictor, y = var_y) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40")
    )
}

p_cuad_pureza <- grafico_cuad("Pureza",    mod_cuad_pureza,
                              "Ajuste Cuadrático: Pureza", ic_pureza)
p_cuad_rdto   <- grafico_cuad("Rdto_kg_t", mod_cuad_rdto,
                              "Ajuste Cuadrático: Rdto",   ic_rdto)
print(p_cuad_pureza); print(p_cuad_rdto)

# ------------------------------------------------------------------------------
# I. PANEL COMPARATIVO DE MODELOS
# ------------------------------------------------------------------------------
grafico_comparativo <- function(var_y, mod_lin, mod_cuad, mod_seg, info_cuad, titulo) {
  pred_data <- data.frame(PRED_num = seq(rango_pred[1], rango_pred[2], length.out = 300))
  pred_lin  <- predict(mod_lin,  newdata = pred_data)
  pred_cuad <- predict(mod_cuad, newdata = pred_data)
  pred_seg  <- if (!is.null(mod_seg)) predict(mod_seg, newdata = pred_data) else NA
  
  df_all <- data.frame(PRED_num = pred_data$PRED_num, Lineal = pred_lin,
                       Cuadratico = pred_cuad, Segmentado = pred_seg)
  psi    <- if (!is.null(mod_seg)) mod_seg$psi[, "Est."] else NA
  v_vert <- info_cuad$vertice
  
  ggplot(df_all, aes(x = PRED_num)) +
    geom_point(data = datos_limpios, aes(x = PRED_num, y = .data[[var_y]]),
               color = "#D73027", alpha = 0.35, size = 1.8, inherit.aes = FALSE) +
    geom_line(aes(y = Lineal, color = "Lineal"), size = 1.1) +
    geom_line(aes(y = Cuadratico, color = "Cuadrático"), size = 1.1) +
    { if (!is.null(mod_seg)) geom_line(aes(y = Segmentado, color = "Segmentado"), size = 1.1) } +
    { if (!is.na(v_vert) && info_cuad$p_cuad < 0.15)
      geom_vline(xintercept = v_vert, linetype = "dotted",
                 color = "darkgreen", size = 0.9) } +
    { if (!is.na(psi))
      geom_vline(xintercept = psi, linetype = "dashed",
                 color = "red", size = 0.9) } +
    scale_color_manual(
      values = c(Lineal = "steelblue", Cuadrático = "darkgreen", Segmentado = "red"),
      name = "Modelo") +
    labs(
      title = titulo,
      subtitle = sprintf("Vértice cuadrático = %.2f | Punto quiebre segmented = %.2f",
                         v_vert, psi),
      x = nombre_predictor, y = var_y) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
          legend.position = "top")
}

p_comp_pureza <- grafico_comparativo("Pureza",    mod_lin_pureza, mod_cuad_pureza,
                                     mod_seg_pureza, ic_pureza,
                                     "Comparación de Modelos: Pureza")
p_comp_rdto   <- grafico_comparativo("Rdto_kg_t", mod_lin_rdto, mod_cuad_rdto,
                                     mod_seg_rdto, ic_rdto,
                                     "Comparación de Modelos: Rdto")
print(p_comp_pureza); print(p_comp_rdto)

# ------------------------------------------------------------------------------
# J. IMPACTO RELATIVO (%)
# ------------------------------------------------------------------------------
if (tipo_predictor == "categorica") {
  
  pred_pct_fac <- function(mod, var_name, nd_ref_media) {
    grid  <- niveles_pred
    preds <- predict(mod, newdata = data.frame(PRED = factor(grid, levels = niveles_pred)),
                     interval = "confidence")
    data.frame(
      Variable   = var_name,
      PRED       = factor(grid, levels = niveles_pred, ordered = TRUE),
      PRED_num   = as.numeric(factor(grid, levels = niveles_pred)),
      Fit        = preds[, "fit"],
      Pct_Cambio = (preds[, "fit"] - nd_ref_media) / nd_ref_media * 100,
      Pct_Lower  = (preds[, "lwr"] - nd_ref_media) / nd_ref_media * 100,
      Pct_Upper  = (preds[, "upr"] - nd_ref_media) / nd_ref_media * 100
    )
  }
  
  ref_medias <- datos_limpios %>%
    dplyr::filter(PRED == ref_level) %>%
    dplyr::summarise(
      Pureza_ref = mean(Pureza, na.rm = TRUE),
      Rdto_ref   = mean(Rdto_kg_t, na.rm = TRUE)
    )
  
  resultados_pct <- bind_rows(
    pred_pct_fac(mod_fac_pureza, "Pureza",    ref_medias$Pureza_ref),
    pred_pct_fac(mod_fac_rdto,   "Rdto_kg_t", ref_medias$Rdto_ref)
  )
  
  grafico_pct <- ggplot(resultados_pct,
                        aes(x = PRED, y = Pct_Cambio, color = Variable, group = Variable)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(aes(ymin = Pct_Lower, ymax = Pct_Upper),
                  width = 0.15, size = 0.8, position = position_dodge(0.1)) +
    geom_line(size = 1.1, position = position_dodge(0.1)) +
    geom_point(size = 3.5, position = position_dodge(0.1)) +
    scale_color_manual(values = c(Pureza = "#2E75B6", Rdto_kg_t = "#C00000")) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
          legend.position = "top",
          axis.text.x = element_text(angle = 20, hjust = 1)) +
    labs(
      title = paste0("Impacto Relativo de ", nombre_predictor,
                     " en la Calidad (Data: ", tipo_data_analisis, ")"),
      subtitle = sprintf("Cambio porcentual respecto a '%s' (referencia = 0%%)", ref_level),
      x = nombre_predictor, y = "Cambio porcentual (%)"
    )
  
} else {
  
  ref_medias <- datos_limpios %>%
    dplyr::filter(PRED_num == min(PRED_num, na.rm = TRUE)) %>%
    dplyr::summarise(
      Pureza_ref = mean(Pureza, na.rm = TRUE),
      Rdto_ref   = mean(Rdto_kg_t, na.rm = TRUE)
    )
  
  grid_num <- seq(min(datos_limpios$PRED_num), max(datos_limpios$PRED_num), length.out = 100)
  
  pred_pct_num <- function(mod, var_name, ref_media) {
    preds <- predict(mod, newdata = data.frame(PRED_num = grid_num),
                     interval = "confidence")
    data.frame(
      Variable   = var_name,
      PRED_num   = grid_num,
      Fit        = preds[, "fit"],
      Pct_Cambio = (preds[, "fit"] - ref_media) / ref_media * 100,
      Pct_Lower  = (preds[, "lwr"] - ref_media) / ref_media * 100,
      Pct_Upper  = (preds[, "upr"] - ref_media) / ref_media * 100
    )
  }
  
  resultados_pct <- bind_rows(
    pred_pct_num(mod_lin_pureza, "Pureza",    ref_medias$Pureza_ref),
    pred_pct_num(mod_lin_rdto,   "Rdto_kg_t", ref_medias$Rdto_ref)
  )
  
  grafico_pct <- ggplot(resultados_pct,
                        aes(x = PRED_num, y = Pct_Cambio, color = Variable, fill = Variable)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_ribbon(aes(ymin = Pct_Lower, ymax = Pct_Upper), alpha = 0.15, color = NA) +
    geom_line(size = 1.3) +
    scale_color_manual(values = c(Pureza = "#2E75B6", Rdto_kg_t = "#C00000")) +
    scale_fill_manual(values  = c(Pureza = "#2E75B6", Rdto_kg_t = "#C00000")) +
    scale_x_continuous(breaks = pretty(datos_limpios$PRED_num, n = 6)) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
          legend.position = "top",
          axis.text.x = element_text(angle = 20, hjust = 1)) +
    labs(
      title = paste0("Impacto Relativo de ", nombre_predictor,
                     " en la Calidad (Data: ", tipo_data_analisis, ")"),
      subtitle = sprintf("Cambio porcentual respecto al valor mínimo de referencia (%s = %.2f)",
                         nombre_predictor, ref_level),
      x = nombre_predictor, y = "Cambio porcentual (%)"
    )
}

cat("\n========== IMPACTO RELATIVO (%) ==========\n")
print(as.data.frame(resultados_pct), row.names = FALSE)
print(grafico_pct)

# ------------------------------------------------------------------------------
# K. PERMANOVA Y PCA
# ------------------------------------------------------------------------------
cat("\n--- PERMANOVA (Adonis2) ---\n")
permanova_res <- vegan::adonis2(
  datos_limpios[, vars_calidad] ~ PRED,
  data = datos_limpios,
  method = "euclidean", permutations = 999)
print(permanova_res)

cat("\n--- PCA ---\n")
pca_res <- FactoMineR::PCA(datos_limpios[, vars_calidad], graph = FALSE)

if (tipo_predictor == "categorica") {
  
  print(factoextra::fviz_pca_ind(
    pca_res, geom.ind = "point",
    col.ind = datos_limpios$PRED,
    palette = c("#00AFBB", "#E7B800", "#FC4E07", "#8B0000", "#6A0DAD"),
    addEllipses = TRUE,
    legend.title = nombre_predictor,
    title = paste0("PCA: Perfil de Calidad según ", nombre_predictor,
                   " (Data: ", tipo_data_analisis, ")")
  ))
  
} else {
  
  print(factoextra::fviz_pca_ind(
    pca_res, geom.ind = "point",
    col.ind = datos_limpios$PRED_num,
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    legend.title = nombre_predictor,
    title = paste0("PCA: Perfil de Calidad según ", nombre_predictor,
                   " (Data: ", tipo_data_analisis, ")")
  ))
}

# ------------------------------------------------------------------------------
# L. ÍNDICE DE CALIDAD GLOBAL (ICG)
# ------------------------------------------------------------------------------
ref_medias_icg <- datos_limpios %>%
  dplyr::filter(if (tipo_predictor == "categorica") PRED == ref_level
                else PRED_num == min(PRED_num, na.rm = TRUE)) %>%
  dplyr::summarise(dplyr::across(all_of(vars_calidad), mean, na.rm = TRUE))

cambio_muestra <- datos_limpios %>%
  dplyr::mutate(dplyr::across(all_of(vars_calidad),
                              ~ (.x - ref_medias_icg[[dplyr::cur_column()]]) /
                                ref_medias_icg[[dplyr::cur_column()]])) %>%
  dplyr::mutate(ICG = rowMeans(dplyr::across(all_of(vars_calidad)), na.rm = TRUE))

if (tipo_predictor == "categorica") {
  icg_stats <- cambio_muestra %>%
    dplyr::group_by(PRED) %>%
    dplyr::summarise(
      ICG_mean  = mean(ICG),
      ICG_se    = sd(ICG) / sqrt(dplyr::n()),
      ICG_lower = ICG_mean - 1.96 * ICG_se,
      ICG_upper = ICG_mean + 1.96 * ICG_se,
      .groups   = "drop"
    )
} else {
  icg_stats <- cambio_muestra %>%
    dplyr::mutate(PRED_bin = cut(PRED_num, breaks = 5)) %>%
    dplyr::group_by(PRED_bin) %>%
    dplyr::summarise(
      ICG_mean  = mean(ICG),
      ICG_se    = sd(ICG) / sqrt(dplyr::n()),
      ICG_lower = ICG_mean - 1.96 * ICG_se,
      ICG_upper = ICG_mean + 1.96 * ICG_se,
      .groups   = "drop"
    ) %>%
    dplyr::rename(PRED = PRED_bin)
}

mod_icg <- lm(ICG ~ PRED_num,
              data = cambio_muestra %>% dplyr::mutate(PRED_num = as.numeric(PRED_num)))
cat("\n--- Modelo ICG ~ PRED_num ---\n"); print(summary(mod_icg))

grafico_icg <- ggplot(icg_stats, aes(x = PRED, y = ICG_mean, group = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  { if (!is.na(psi_rdto))
    geom_vline(xintercept = psi_rdto, color = "red", linetype = "dashed", size = 1) } +
  geom_ribbon(aes(ymin = ICG_lower, ymax = ICG_upper), fill = "steelblue", alpha = 0.2) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 3.5) +
  labs(
    title = paste0("Índice de Calidad Global (ICG) por ", nombre_predictor,
                   " (Data: ", tipo_data_analisis, ")"),
    subtitle = sprintf("Referencia = %s | Intervalo de confianza 95%%",
                       ifelse(tipo_predictor == "categorica",
                              ref_level,
                              sprintf("%.2f", ref_level))),
    x = nombre_predictor, y = "ICG Promedio"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.x   = element_text(angle = 20, hjust = 1)
  )

print(grafico_icg)

# ==============================================================================
# L.2  ICG ESCALADO - Análisis alternativo respecto al cuartil bajo (Q1)
# ==============================================================================
cat("\n========================================================\n")
cat("ICG ESCALADO (referencia = cuartil bajo Q1)\n")
cat("========================================================\n")

# --- Eje X dinámico ---
if ("Incidencia" %in% names(datos_raw)) {
  eje_x_var   <- "Incidencia"
  eje_x_label <- "Incidencia (proporción)"
} else {
  eje_x_var   <- "PRED_num"
  eje_x_label <- nombre_predictor
}

datos_icg_esc <- datos_raw %>%
  dplyr::filter(!is.na(Pureza), !is.na(Rdto_kg_t),
                !is.na(.data[[eje_x_var]])) %>%
  dplyr::select(all_of(c(eje_x_var, vars_calidad))) %>%
  na.omit()

# --- Cuartiles del eje X ---
q_cuts <- quantile(datos_icg_esc[[eje_x_var]],
                   probs = c(0, 0.25, 0.5, 0.75, 1),
                   na.rm = TRUE)

datos_icg_esc <- datos_icg_esc %>%
  dplyr::mutate(Q_grupo = cut(.data[[eje_x_var]],
                              breaks = q_cuts,
                              include.lowest = TRUE,
                              labels = c("Q1", "Q2", "Q3", "Q4")))

# --- Medias del cuartil bajo Q1 ---
ref_q1 <- datos_icg_esc %>%
  dplyr::filter(Q_grupo == "Q1") %>%
  dplyr::summarise(dplyr::across(all_of(vars_calidad), mean, na.rm = TRUE))

# --- ICG escalado como PROPORCIÓN (0.20 = 20%) ---
# Se calcula en escala 0–1 para que scale=1 de percent_format lo lea directo
datos_icg_esc <- datos_icg_esc %>%
  dplyr::mutate(
    d_Pureza = (Pureza    - ref_q1$Pureza)    / ref_q1$Pureza,      # proporción
    d_Rdto   = (Rdto_kg_t - ref_q1$Rdto_kg_t) / ref_q1$Rdto_kg_t,   # proporción
    ICG_esc  = rowMeans(cbind(d_Pureza, d_Rdto), na.rm = TRUE)      # proporción
  )

# --- Umbral crítico ---
umbral_pct   <- 0.80
umbral_valor <- quantile(datos_icg_esc[[eje_x_var]],
                         probs = umbral_pct, na.rm = TRUE)

# --- Modelo lineal (pendiente en proporción por unidad) ---
form_icg_esc <- reformulate(termlabels = eje_x_var, response = "ICG_esc")
mod_icg_esc  <- lm(form_icg_esc, data = datos_icg_esc)

slope_esc   <- coef(mod_icg_esc)[2]                    # ya es proporción/unidad
p_slope_esc <- summary(mod_icg_esc)$coefficients[2, 4]

# --- Reporte en consola, todo en % ---
cat(sprintf("Pendiente lineal ICG escalado ~ %s : %.4f%% por unidad (p = %.4f)\n",
            eje_x_var, slope_esc * 100, p_slope_esc))
cat(sprintf("Umbral crítico (%d%%): %.4f\n", umbral_pct * 100, umbral_valor))
cat(sprintf("Referencia Q1 → Pureza = %.3f | Rdto = %.3f\n",
            ref_q1$Pureza, ref_q1$Rdto_kg_t))

# ==============================================================================
# GRÁFICO 1: ICG Escalado global — TODO EN %
# ==============================================================================
grafico_icg_esc <- ggplot(datos_icg_esc,
                          aes(x = .data[[eje_x_var]], y = ICG_esc)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "gray30", linewidth = 0.7) +
  geom_vline(xintercept = umbral_valor, color = "red",
             linetype = "dashed", linewidth = 0.9) +
  geom_point(color = "#5B9BD5", alpha = 0.35, size = 1.8) +
  geom_smooth(method = "loess", se = TRUE,
              color = "#2E75B6", fill = "#5B9BD5",
              alpha = 0.25, linewidth = 1.2) +
  # --- EJE Y EN % ---
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),  # 0.20 → "20%"
    breaks = scales::pretty_breaks(n = 6)
  ) +
  labs(
    title    = "Índice de Calidad Global (ICG) Escalado",
    subtitle = sprintf(
      "Referencia: media del cuartil bajo Q1 | Pendiente lineal: %.4f%% por unidad de %s (p = %.3f)",
      slope_esc * 100, eje_x_var, p_slope_esc),
    x = eje_x_label,
    y = "Δ respecto al cuartil bajo (Q1)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", hjust = 0.5, size = 15),
    plot.subtitle    = element_text(hjust = 0.5, color = "gray40", size = 10),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(color = "gray25")
  )

print(grafico_icg_esc)

# ==============================================================================
# GRÁFICO 2: ICG Escalado facetado — TODO EN %
# ==============================================================================
datos_icg_long <- datos_icg_esc %>%
  dplyr::select(all_of(eje_x_var), d_Pureza, d_Rdto) %>%
  tidyr::pivot_longer(cols = c(d_Pureza, d_Rdto),
                      names_to = "Variable", values_to = "Delta") %>%
  dplyr::mutate(Variable = dplyr::recode(Variable,
                                         d_Pureza = "Pureza",
                                         d_Rdto   = "Rdto_kg_t"))

grafico_icg_esc_facet <- ggplot(datos_icg_long,
                                aes(x = .data[[eje_x_var]], y = Delta)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "gray30", linewidth = 0.7) +
  geom_vline(xintercept = umbral_valor, color = "red",
             linetype = "dashed", linewidth = 0.9) +
  geom_point(color = "#5B9BD5", alpha = 0.30, size = 1.5) +
  geom_smooth(method = "loess", se = TRUE,
              color = "#2E75B6", fill = "#5B9BD5",
              alpha = 0.25, linewidth = 1.1) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
  # --- EJE Y EN % ---
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1),
    breaks = scales::pretty_breaks(n = 5)
  ) +
  labs(
    title    = "ICG Escalado por variable (referencia = Q1)",
    subtitle = sprintf("Umbral crítico (%d%%): %.4f",
                       umbral_pct * 100, umbral_valor),
    x = eje_x_label,
    y = "Δ respecto al cuartil bajo (Q1)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", hjust = 0.5),
    plot.subtitle    = element_text(hjust = 0.5, color = "gray40"),
    strip.text       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(grafico_icg_esc_facet)

# --- MAPA DE CALOR: son las tablas de resumen ---

grafico_heatmap <- ggplot(heatmap_data,
                          aes(x = Grupo, y = Variable, fill = Pct_Cambio)) +
  geom_tile(color = "white", size = 1.5) +
  geom_text(aes(label = Label), color = "black", size = 4.2,
            fontface = "bold", lineheight = 1.05) +
  scale_fill_gradient2(
    low      = "#C00000",
    mid      = "#FFF2CC",
    high     = "#70AD47",
    midpoint = 0,
    limits   = c(min(heatmap_data$Pct_Cambio, na.rm = TRUE) - 0.5,
                 max(heatmap_data$Pct_Cambio, na.rm = TRUE) + 0.5),
    name     = "Cambio %"
  ) +
  labs(
    title    = titulo_heatmap,
    subtitle = subtitulo_heatmap,
    x        = x_label,
    y        = NULL,
    caption  = paste0(
      "Letra: agrupación ", etiqueta_prueba,
      " (letras distintas = diferencias significativas entre grupos).\n",
      "Sig. 'sig' = la variable presenta al menos una diferencia significativa entre grupos; ",
      "'ns' = ninguna diferencia significativa."
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid      = element_blank(),
    axis.text.x     = element_text(face = "bold", size = 10, angle = 20, hjust = 1),
    axis.text.y     = element_text(face = "bold", size = 12),
    legend.position = "right",
    legend.title    = element_text(face = "bold", size = 11),
    plot.title      = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle   = element_text(hjust = 0.5, color = "gray40", size = 11),
    plot.caption    = element_text(size = 9, color = "gray50", hjust = 0),
    plot.margin     = margin(15, 20, 15, 15)
  )

print(grafico_heatmap)

cat("\n========== Datos del mapa de calor ==========\n")
tabla_heatmap <- heatmap_data %>%
  dplyr::select(Variable, Grupo, Pct_Cambio, Letra, Sig) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 3))) %>%
  dplyr::arrange(Variable, Grupo)
print(as.data.frame(tabla_heatmap), row.names = FALSE)
