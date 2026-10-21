# =============================================================================
# ANÁLISIS DE RESPUESTA A NITRÓGENO (NT) EN CAÑA DE AZÚCAR
# Variables: TCH, Rendimiento (kg/t), TAH, Pureza
# =============================================================================

# ---- 0. Paquetes ------------------------------------------------------------
pkgs <- c("ggplot2", "dplyr", "segmented", "patchwork", "multcompView")
faltan <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(faltan)) install.packages(faltan)
invisible(lapply(pkgs, library, character.only = TRUE))



datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")
cat("\n--- Variables detectadas ---\n")
print(names(datos))


# ---- 1. Configuración -------------------------------------------------------
vars <- c(TCH          = "TCH",
          Rendimiento  = "Rendimiento.Kg.Ton",
          TAH          = "TAH",
          Pureza       = "Pureza")

etiquetas <- c(TCH         = "TCH (t caña/ha)",
               Rendimiento = "Rendimiento (kg azúcar/t caña)",
               TAH         = "TAH (t azúcar/ha)",
               Pureza      = "Pureza (%)")

etiqueta_N <- "Nitrógeno total, NT (kg N/ha)"

ratio_econ <- c(TCH = NA, Rendimiento = NA, TAH = NA, Pureza = NA)

alfa    <- 0.15
n_boot  <- 1000
carpeta <- "resultados_N"
dir.create(carpeta, showWarnings = FALSE)

col_opt <- "#1B9E77"
col_qui <- "#D62728"
col_cur <- "#1F4E79"

# ---- 2. Función de análisis por variable -----------------------------------
analizar <- function(v) {
  
  col <- vars[[v]]
  d <- datos |>
    transmute(NT = as.numeric(NT), y = as.numeric(.data[[col]]),
              Replica = factor(Replica)) |>
    filter(!is.na(NT), !is.na(y))
  
  x      <- d$NT
  rango  <- range(x)
  grid   <- data.frame(NT = seq(rango[1], rango[2], length.out = 300))
  medias <- d |> group_by(NT) |>
    summarise(m = mean(y), se = sd(y) / sqrt(n()), .groups = "drop")
  
  # ---- 2.1 Modelos lineal y cuadrático ------------------------------------
  m1 <- lm(y ~ NT, data = d)
  m2 <- lm(y ~ NT + I(NT^2), data = d)
  b1 <- unname(coef(m2)["NT"])
  b2 <- unname(coef(m2)["I(NT^2)"])
  r2 <- summary(m2)$r.squared
  
  p_F  <- anova(m1, m2)$`Pr(>F)`[2]
  t_b2 <- summary(m2)$coefficients["I(NT^2)", "t value"]
  p_b2 <- pt(t_b2, df.residual(m2))
  
  d$NTf      <- factor(d$NT)
  nl         <- nlevels(d$NTf)
  usa_bloque <- nlevels(d$Replica) > 1
  bl         <- if (usa_bloque) "Replica + " else ""
  fa     <- aov(as.formula(paste("y ~", bl, "NTf")), data = d)
  p_anova <- summary(fa)[[1]][["Pr(>F)"]][if (usa_bloque) 2 else 1]
  
  gr <- min(4, nl - 1)
  P  <- poly(d$NT, gr)
  for (j in 1:gr) d[[paste0("P", j)]] <- P[, j]
  an_pol <- anova(lm(as.formula(paste("y ~", bl, paste0("P", 1:gr, collapse = " + "))),
                     data = d))
  p_lin  <- an_pol["P1", "Pr(>F)"]
  p_cuad <- if (gr >= 2) an_pol["P2", "Pr(>F)"] else NA_real_
  
  m2b <- lm(as.formula(paste("y ~", bl, "NT + I(NT^2)")), data = d)
  mf  <- lm(as.formula(paste("y ~", bl, "NTf")), data = d)
  p_lof <- tryCatch(anova(m2b, mf)$`Pr(>F)`[2], error = function(e) NA_real_)
  
  # ---- Fisher LSD (α = 0.15) con el MEJOR tratamiento = "a" ---------------
  letras <- tryCatch({
    sm_fa  <- summary(fa)[[1]]
    df_err <- sm_fa["Residuals", "Df"]
    mse    <- sm_fa["Residuals", "Mean Sq"]
    g      <- factor(d$NT)
    mns    <- tapply(d$y, g, mean)
    ns     <- tapply(d$y, g, length)
    gn     <- levels(g)
    k      <- length(gn)
    
    gn_ord <- gn[order(mns, decreasing = TRUE)]
    
    pv_ord <- c(); nm_ord <- c()
    if (k >= 2) for (i in 1:(k - 1)) for (j in (i + 1):k) {
      gi <- gn_ord[i]; gj <- gn_ord[j]
      se   <- sqrt(mse * (1 / ns[gi] + 1 / ns[gj]))
      tval <- abs(mns[gi] - mns[gj]) / se
      p    <- 2 * pt(-tval, df_err)
      pv_ord <- c(pv_ord, p); nm_ord <- c(nm_ord, paste0(gi, "-", gj))
    }
    names(pv_ord) <- nm_ord
    
    lt <- multcompView::multcompLetters(pv_ord, threshold = alfa)$Letters
    
    mejor  <- names(mns)[which.max(mns)]
    l_mej  <- lt[mejor]
    if (!is.na(l_mej) && !startsWith(l_mej, "a")) {
      primera <- strsplit(l_mej, "")[[1]][1]
      lt <- sapply(lt, function(x) {
        ch <- strsplit(x, "")[[1]]
        out <- ch
        out[ch == "a"]      <- primera
        out[ch == primera]  <- "a"
        paste0(out, collapse = "")
      })
    }
    lt
  }, error = function(e) NULL)
  
  medias$letra <- if (!is.null(letras))
    unname(letras[as.character(medias$NT)]) else NA_character_
  
  # Vértice (óptimo cuadrático)
  x_vert   <- -b1 / (2 * b2)
  hay_max  <- is.finite(x_vert) && b2 < 0
  pred2    <- function(z) predict(m2, data.frame(NT = z))
  vert_interior <- hay_max && x_vert >= rango[1] && x_vert <= rango[2]
  x_opt    <- if (vert_interior) x_vert else rango[which.max(pred2(rango))]
  y_opt    <- unname(pred2(x_opt))
  y_ctrl   <- unname(pred2(rango[1]))
  
  g        <- data.frame(NT = seq(rango[1], x_opt, length.out = 1000))
  g$p      <- predict(m2, g)
  x_95     <- g$NT[which(g$p >= y_ctrl + 0.95 * (y_opt - y_ctrl))[1]]
  
  x_be     <- -b1 / b2 - rango[1]
  x_be     <- if (hay_max && x_be <= rango[2]) x_be else NA_real_
  
  x_eco <- NA_real_
  if (!is.na(ratio_econ[[v]]) && b2 < 0)
    x_eco <- min(max((ratio_econ[[v]] - b1) / (2 * b2), rango[1]), rango[2])
  
  set.seed(123)
  bs <- replicate(n_boot, {
    i  <- sample(nrow(d), replace = TRUE)
    cc <- tryCatch(coef(lm(y ~ NT + I(NT^2), data = d[i, ])),
                   error = function(e) c(NA, NA, NA))
    if (is.na(cc[3]) || cc[3] >= 0) NA_real_ else -cc[2] / (2 * cc[3])
  })
  bs_ok <- bs[!is.na(bs)]
  ic_boot <- if (length(bs_ok) > 30)
    quantile(pmin(pmax(bs_ok, rango[1]), rango[2]), c(0.025, 0.975)) else c(NA, NA)
  
  # ---- 2.2 Mitscherlich ---------------------------------------------------
  mit <- tryCatch(
    nls(y ~ A - B * exp(-k * NT), data = d,
        start = list(A = max(medias$m) * 1.02,
                     B = max(medias$m) * 1.02 - min(medias$m),
                     k = 3 / rango[2]),
        algorithm = "port", lower = c(-Inf, -Inf, 1e-6)),
    error = function(e) NULL)
  x_mit <- y_mit <- NA_real_
  if (!is.null(mit)) {
    k_m <- coef(mit)["k"]; B_m <- coef(mit)["B"]
    if (B_m != 0) {
      x_mit <- min(log(20) / k_m, rango[2])
      y_mit <- unname(predict(mit, data.frame(NT = x_mit)))
    }
  }
  
  # ---- 2.3 Lineal-plateau -------------------------------------------------
  lp <- tryCatch(
    nls(y ~ a + b * pmin(NT, xj), data = d,
        start = list(a = unname(coef(m1)[1]),
                     b = max(unname(coef(m1)[2]), 1e-3),
                     xj = median(x)),
        algorithm = "port",
        lower = c(-Inf, 0, rango[1]), upper = c(Inf, Inf, rango[2])),
    error = function(e) NULL)
  x_lp <- if (!is.null(lp)) unname(coef(lp)["xj"]) else NA_real_
  y_lp <- if (!is.null(lp)) unname(predict(lp, data.frame(NT = x_lp))) else NA_real_
  
  # ---- 2.4 Cuadrático-plateau --------------------------------------------
  qp <- tryCatch(
    nls(y ~ ifelse(NT < -b / (2 * cc), a + b * NT + cc * NT^2, a - b^2 / (4 * cc)),
        data = d,
        start = list(a = unname(coef(m2)[1]),
                     b = max(b1, 1e-3),
                     cc = min(b2, -1e-6)),
        algorithm = "port", lower = c(-Inf, 0, -Inf), upper = c(Inf, Inf, -1e-10)),
    error = function(e) NULL)
  x_qp <- y_qp <- NA_real_
  if (!is.null(qp)) {
    cq   <- coef(qp)
    x_qp <- min(unname(-cq["b"] / (2 * cq["cc"])), rango[2])
    y_qp <- unname(predict(qp, data.frame(NT = x_qp)))
  }
  
  # ---- 2.5 Regresión segmentada ------------------------------------------
  seg <- tryCatch(segmented(m1, seg.Z = ~NT, psi = mean(rango)),
                  error = function(e) NULL)
  x_qui <- pend1 <- pend2 <- se_qui <- p_dav <- NA_real_
  if (!is.null(seg)) {
    x_qui  <- unname(seg$psi[1, "Est."])
    se_qui <- unname(seg$psi[1, "St.Err"])
    sl     <- tryCatch(slope(seg)$NT[, "Est."], error = function(e) c(NA, NA))
    pend1  <- unname(sl[1]); pend2 <- unname(sl[2])
    p_dav  <- tryCatch(davies.test(m1, seg.Z = ~NT)$p.value,
                       error = function(e) NA_real_)
  }
  y_qui <- if (!is.na(x_qui)) unname(predict(seg, data.frame(NT = x_qui))) else NA_real_
  cae   <- !is.na(pend2) && pend2 < 0 && !is.na(p_dav) && p_dav < alfa
  x_perdida <- if (cae) x_qui else if (vert_interior) x_vert else NA_real_
  
  # ---- 2.6 Producto marginal ---------------------------------------------
  medias <- medias[order(medias$NT), ]
  inc <- data.frame(
    de = head(medias$NT, -1), a = tail(medias$NT, -1),
    dN = diff(medias$NT), dY = diff(medias$m))
  inc$prod_marginal <- inc$dY / inc$dN
  inc$Variable <- v
  rho <- p_rho <- NA_real_
  if (nrow(inc) >= 4) {
    ct <- suppressWarnings(cor.test((inc$de + inc$a) / 2, inc$prod_marginal,
                                    method = "spearman"))
    rho <- unname(ct$estimate); p_rho <- ct$p.value
  }
  
  # ---- 2.7 AIC ------------------------------------------------------------
  aic <- c(Lineal = AIC(m1), Cuadratico = AIC(m2),
           Mitscherlich = if (!is.null(mit)) AIC(mit) else NA,
           Lineal_plateau = if (!is.null(lp)) AIC(lp) else NA,
           Cuadratico_plateau = if (!is.null(qp)) AIC(qp) else NA,
           Segmentado = if (!is.null(seg)) AIC(seg) else NA)
  tabla_aic <- data.frame(Variable = v, Modelo = names(aic), AIC = unname(aic))
  
  # ---- 2.8 Tablas ---------------------------------------------------------
  fila <- function(metodo, dosis, resp)
    data.frame(Variable = v, Metodo = metodo, Dosis_NT = as.numeric(dosis),
               Respuesta = as.numeric(resp))
  tab_dosis <- rbind(
    fila("Cuadrático (vértice o límite del rango)", x_opt, y_opt),
    fila("Cuadrático-plateau", x_qp, y_qp),
    fila("Lineal-plateau", x_lp, y_lp),
    fila("Mitscherlich (95 % de respuesta máx.)", x_mit, y_mit),
    fila("Dosis eficiente cuadrática (95 % ganancia)", x_95, pred2(x_95)),
    fila("Dosis económica cuadrática", x_eco,
         if (is.na(x_eco)) NA else pred2(x_eco)),
    fila("Quiebre segmentado", x_qui, y_qui),
    fila("Dosis de pérdida total de la ganancia", x_be,
         if (is.na(x_be)) NA else pred2(x_be)))
  
  tab_pruebas <- data.frame(
    Variable = v, R2_cuadratico = r2, b1 = b1, b2 = b2,
    p_F_lineal_vs_cuad = p_F, p_b2_menor0 = p_b2,
    Spearman_rho_prod_marginal = rho, p_Spearman = p_rho,
    Pendiente1_segmentada = pend1, Pendiente2_segmentada = pend2,
    Quiebre = x_qui, EE_quiebre = se_qui, p_Davies = p_dav,
    Optimo_IC95_boot_inf = ic_boot[1], Optimo_IC95_boot_sup = ic_boot[2],
    Decrecientes = (b2 < 0 && p_b2 < alfa),
    p_ANOVA_factor = p_anova, p_contraste_lineal = p_lin,
    p_contraste_cuadratico = p_cuad, p_falta_ajuste_cuad = p_lof,
    Optimo_interior = vert_interior, Perdida_demostrada = cae)
  
  # ---- 3. Gráfico ---------------------------------------------------------
  pr <- predict(m2, grid, interval = "confidence")
  cur <- data.frame(NT = grid$NT, fit = pr[, "fit"], lwr = pr[, "lwr"], upr = pr[, "upr"])
  cur_seg <- if (!is.null(seg)) data.frame(NT = grid$NT, fit = predict(seg, grid)) else NULL
  
  yr   <- range(c(d$y, cur$lwr, cur$upr))
  pad  <- diff(yr) * 0.12
  ylim <- c(yr[1] - pad, yr[2] + pad)
  
  sub <- sprintf(paste0("R² = %.2f | ANOVA (NT factor) p = %s | contraste lineal p = %s | ",
                        "cuadrático p = %s | falta de ajuste cuad. p = %s\n%s | Fisher LSD (α = %.2f)"),
                 r2, format.pval(p_anova, digits = 2), format.pval(p_lin, digits = 2),
                 format.pval(p_cuad, digits = 2), format.pval(p_lof, digits = 2),
                 ifelse(b2 < 0 && p_b2 < alfa, "Rendimientos decrecientes: SÍ",
                        "Rendimientos decrecientes: no concluyente"),
                 alfa)
  
  col_o <- if (vert_interior) col_opt else "grey40"
  col_q <- if (cae) col_qui else "#E69F00"
  
  lab_opt <- if (vert_interior) {
    sprintf("Óptimo: NT = %.0f", x_opt)
  } else if (x_opt == rango[2]) {
    sprintf("Máx. alcanzado (límite: %.0f)", x_opt)
  } else {
    sprintf("Sin respuesta sig. (mín.: %.0f)", x_opt)
  }
  
  # ---- Rótulo del punto de quiebre + significado del color (lenguaje simple) ----
  lab_qui <- if (is.na(x_qui)) NA_character_ else
    sprintf("NT = %.0f", x_qui)
  
  caption_qui <- if (is.na(x_qui)) {
    NULL
  } else if (cae) {
    paste0("Punto rojo: a partir de esta dosis el cultivo empieza a bajar. ",
           "Ya no conviene seguir aplicando nitrógeno.")
  } else {
    paste0("Punto amarillo: a partir de esta dosis el cultivo sigue subiendo, ",
           "pero más despacio. Todavía responde al nitrógeno.")
  }
  
  p <- ggplot() +
    { if (!is.na(x_perdida))
      geom_rect(data = data.frame(x1 = x_perdida, x2 = rango[2]),
                aes(xmin = x1, xmax = x2, ymin = -Inf, ymax = Inf),
                fill = col_qui, alpha = 0.09) } +
    geom_ribbon(data = cur, aes(NT, ymin = lwr, ymax = upr),
                fill = col_cur, alpha = 0.15) +
    geom_jitter(data = d, aes(NT, y), colour = "grey55", alpha = 0.55,
                width = diff(rango) * 0.01, height = 0, size = 1.6) +
    geom_errorbar(data = medias, aes(NT, ymin = m - se, ymax = m + se),
                  width = diff(rango) * 0.015, colour = "grey20") +
    geom_point(data = medias, aes(NT, m), shape = 21, size = 3.2,
               fill = "white", colour = "grey20", stroke = 1) +
    geom_text(data = medias, aes(NT, m + se, label = letra), vjust = -0.8,
              fontface = "bold", size = 4) +
    { if (!is.null(cur_seg))
      geom_line(data = cur_seg, aes(NT, fit), colour = "grey35",
                linetype = "dashed", linewidth = 0.7) } +
    geom_line(data = cur, aes(NT, fit), colour = col_cur, linewidth = 1.5) +
    # Óptimo: línea + punto + valor
    geom_vline(xintercept = x_opt, colour = col_o, linewidth = 1.1,
               linetype = ifelse(vert_interior, "solid", "dotted")) +
    geom_point(aes(x_opt, y_opt), shape = 21, size = 6, fill = col_o,
               colour = "white", stroke = 1.5) +
    annotate("text", x = x_opt, y = y_opt, label = lab_opt,
             hjust = ifelse(x_opt > mean(rango), 1.1, -0.1),
             vjust = -1.2, fontface = "bold", size = 4, colour = col_o) +
    # Quiebre: línea + punto + valor junto al punto
    { if (!is.na(x_qui))
      geom_vline(xintercept = x_qui, colour = col_q, linewidth = 1.1,
                 linetype = "longdash") } +
    { if (!is.na(x_qui))
      geom_point(aes(x_qui, y_qui), shape = 23, size = 6, fill = col_q,
                 colour = "white", stroke = 1.5) } +
    { if (!is.na(x_qui))
      annotate("text", x = x_qui, y = y_qui, label = lab_qui,
               hjust = ifelse(x_qui > mean(rango), 1.15, -0.15),
               vjust = -1.3, fontface = "bold", size = 3.8, colour = col_q) } +
    scale_y_continuous(limits = ylim, expand = c(0, 0)) +
    scale_x_continuous(breaks = medias$NT) +
    labs(title = etiquetas[[v]], subtitle = sub,
         caption = caption_qui,
         x = etiqueta_N, y = etiquetas[[v]]) +
    theme_bw(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(size = 8.5, colour = "grey30"),
          plot.caption = element_text(size = 9, colour = "grey25",
                                      face = "italic", hjust = 0),
          panel.grid.minor = element_blank())
  
  ggsave(file.path(carpeta, paste0("curva_", v, ".png")), p,
         width = 8, height = 5.8, dpi = 300)
  
  list(tab_dosis = tab_dosis, tab_pruebas = tab_pruebas, inc = inc,
       aic = tabla_aic, plot = p, modelo = m2, seg = seg, letras = letras)
}

# ---- 3. Ejecutar -----------------------------------------------------------
res <- lapply(names(vars), analizar)
names(res) <- names(vars)

# ---- 4. Resultados consolidados -------------------------------------------
tabla_dosis   <- do.call(rbind, lapply(res, `[[`, "tab_dosis"))
tabla_pruebas <- do.call(rbind, lapply(res, `[[`, "tab_pruebas"))
tabla_inc     <- do.call(rbind, lapply(res, `[[`, "inc"))
tabla_aic     <- do.call(rbind, lapply(res, `[[`, "aic"))
rownames(tabla_dosis) <- rownames(tabla_pruebas) <- NULL
rownames(tabla_inc) <- rownames(tabla_aic) <- NULL

metodos_opt <- c("Cuadrático (vértice o límite del rango)", "Cuadrático-plateau",
                 "Lineal-plateau", "Quiebre segmentado")
consenso <- tabla_dosis |>
  filter(Metodo %in% metodos_opt) |>
  group_by(Variable) |>
  summarise(Dosis_min = min(Dosis_NT, na.rm = TRUE),
            Dosis_mediana = median(Dosis_NT, na.rm = TRUE),
            Dosis_max = max(Dosis_NT, na.rm = TRUE), .groups = "drop")

cat("\n================ DOSIS POR MÉTODO ================\n")
print(tabla_dosis |> mutate(across(where(is.numeric), ~ round(., 2))), row.names = FALSE)
cat("\n======= PRUEBAS DE RENDIMIENTOS DECRECIENTES Y QUIEBRE (alfa =", alfa, ") =======\n")
print(tabla_pruebas |> mutate(across(where(is.numeric), ~ signif(., 3))), row.names = FALSE)
cat("\n================ COMPARACIÓN DE MODELOS (AIC) ================\n")
print(tabla_aic |> mutate(AIC = round(AIC, 2)), row.names = FALSE)
cat("\n======== PRODUCTO MARGINAL ENTRE DOSIS (medias) ========\n")
print(tabla_inc |> mutate(across(where(is.numeric), ~ round(., 4))), row.names = FALSE)
cat("\n=========== DOSIS CONSENSO DE NT POR VARIABLE ===========\n")
print(consenso |> mutate(across(where(is.numeric), ~ round(., 1))), row.names = FALSE)
cat("\nDosis óptima global de NT (mediana entre variables): ",
    round(median(consenso$Dosis_mediana), 1), "\n")

cat("\n=== LETRAS FISHER LSD (mejor = 'a') POR VARIABLE ===\n")
for (v in names(res)) {
  cat("\n", v, ":\n", sep = "")
  print(res[[v]]$letras)
}

cat("\n=== ¿QUÉ PERMITEN AFIRMAR LOS DATOS? (alfa =", alfa, ") ===\n")
for (v in names(res)) {
  r <- tabla_pruebas[tabla_pruebas$Variable == v, ]
  cat(sprintf("%-12s óptimo interior: %-5s | pérdida demostrada: %-5s | óptimo NT = %.1f\n",
              v, r$Optimo_interior, r$Perdida_demostrada,
              tabla_dosis$Dosis_NT[tabla_dosis$Variable == v &
                                     tabla_dosis$Metodo == "Cuadrático (vértice o límite del rango)"]))
}

# ---- 5. Un gráfico por variable (se muestran de a uno) --------------------
for (v in names(res)) {
  cat("\n>> Mostrando gráfico:", v, "-", etiquetas[[v]], "\n")
  print(res[[v]]$plot)
  if (interactive()) Sys.sleep(0.4)
}

cat("\nListo. Gráficos y tablas guardados en:", normalizePath(carpeta), "\n")