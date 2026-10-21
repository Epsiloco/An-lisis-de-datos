# =========================================================================
# 1. CARGA DE LIBRERÍAS (ESTRICTAMENTE AL INICIO PARA EVITAR ERRORES)
# =========================================================================
paquetes <- c(
  "shiny", "shinythemes", "plotly", "DT", "patchwork", "dplyr", "tidyr",
  "agricolae", "purrr", "car", "openxlsx", "fBasics", "psych", "modeest", 
  "ggrepel", "GGally", "mice", "corrplot", "readxl", "doebioresearch", 
  "performance", "broom", "data.table", "emmeans", "ggplot2", "tidyverse", 
  "lattice", "nlme", "lme4", "lmerTest", "multcomp", "rstatix", "ggpubr", 
  "see", "MASS", "lsmeans", "scales", "lmtest", "multcompView", 
  "googlesheets4", "googledrive", "clipr", "FactoMineR", "factoextra", 
  "glmmTMB", "DHARMa", "MuMIn", "hnp", "effects", "sjstats", "ExpDes", 
  "sf", "tmap", "terra", "RVAideMemoire", "RColorBrewer", "DiagrammeR", 
  "esquisse", "dlookr", "VIM"
)

# Verifica cuáles no están instalados e instala
no_instalados <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]
if(length(no_instalados)) install.packages(no_instalados)

# Carga obligatoria de los paquetes clave para la App
suppressPackageStartupMessages({
  library(shiny)
  library(shinythemes)
  library(plotly)
  library(DT)
  library(patchwork)
  library(dplyr)
  library(tidyr)
  library(agricolae)
  library(purrr)
  library(ggplot2)
  library(clipr)
  library(readxl)
  library(openxlsx)
  library(mice)
  library(VIM)
})

# Función auxiliar para imprimir el ANDEVA al estilo InfoStat
print_infostat_anova <- function(modelo, y_name, data) {
  res_anova <- tryCatch(summary(modelo), error = function(e) NULL)
  if (is.null(res_anova)) return(paste("Error calculando ANDEVA para:", y_name, "\n"))
  
  if (inherits(modelo, "aovlist")) {
    out_txt <- paste0("\n======================================================\n",
                      "Variable: ", y_name, " (Modelo Anidado)\n",
                      "======================================================\n")
    out_txt <- paste0(out_txt, paste(capture.output(print(res_anova)), collapse = "\n"), "\n")
    return(out_txt)
  }
  
  tb <- res_anova[[1]]
  tb_df <- as.data.frame(tb)
  tb_df$FV <- rownames(tb)
  tb_df <- tb_df[, c("FV", "Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")]
  colnames(tb_df) <- c("F.V.", "SC", "gl", "CM", "F", "p-valor")
  
  cv <- (sqrt(tb_df[tb_df$F.V. == "Residuals ", "CM"]) / mean(data[[y_name]], na.rm=TRUE)) * 100
  r2 <- 1 - (tb_df[tb_df$F.V. == "Residuals ", "SC"] / sum(tb_df$SC, na.rm=TRUE))
  
  out_txt <- paste0("\n======================================================\n",
                    "Variable: ", y_name, "\n",
                    "R² = ", round(r2, 2), "    CV = ", round(cv, 2), "%\n",
                    "======================================================\n")
  
  out_txt <- paste0(out_txt, paste(capture.output(print(tb_df, row.names = FALSE, na.print = "")), collapse = "\n"), "\n")
  return(out_txt)
}

# =========================================================================
# 2. DEFINICIÓN DE LA INTERFAZ DE USUARIO (UI)
# =========================================================================
ui <- navbarPage(title = "Estadística para gente ociosa",
                 theme = shinytheme("flatly"),
                 
                 # CSS Personalizado
                 tags$head(
                   tags$style(HTML("
                     .instrucciones-box { background-color: #f8f9fa; border-left: 5px solid #2c3e50; padding: 15px; margin-bottom: 20px; border-radius: 5px; font-size: 14px; }
                     .btn-primary { background-color: #2c3e50; border-color: #2c3e50; }
                     .btn-success { background-color: #18bc9c; border-color: #18bc9c; }
                     .btn-warning { background-color: #f39c12; border-color: #f39c12; color: white;}
                     .btn-info { background-color: #3498db; border-color: #3498db; }
                     h4 { color: #2c3e50; font-weight: bold; }
                     .shiny-notification { position: fixed; top: 20px; right: 20px; font-size: 16px; }
                     .dataTables_wrapper { overflow-x: auto; }
                   "))
                 ),
                 
                 tabPanel("Carga de Datos y EDA",
                          sidebarLayout(
                            sidebarPanel(
                              h4("1. Importar Datos"),
                              radioButtons("import_method", "Método de Importación:", choices = c("Pegar desde Portapapeles" = "clip", "Cargar Archivo Excel" = "file")),
                              conditionalPanel(condition = "input.import_method == 'clip'", actionButton("paste_btn", "Pegar Datos", icon = icon("clipboard"), class = "btn-primary")),
                              conditionalPanel(condition = "input.import_method == 'file'", fileInput("file1", "Seleccionar Excel (.xlsx)", accept = c(".xlsx")), uiOutput("select_sheet")),
                              hr(),
                              h4("2. Imputación Faltantes"),
                              selectInput("imp_method", "Método:", choices = c("Media" = "mean", "Mediana" = "median", "PMM (MICE)" = "pmm", "KNN" = "knn", "Árboles (CART)" = "cart")),
                              actionButton("run_imputation", "Imputar Datos", class = "btn-warning"),
                              hr(),
                              h4("3. Auditoría de Atípicos (Global / Grupos)"),
                              uiOutput("eda_var_ui"),
                              uiOutput("eda_group_ui"),
                              selectInput("outlier_action", "Acción sobre Atípicos:", choices = c("Solo Reportar (No hacer nada)" = "report", "Reemplazar con NA" = "NA", "Eliminar fila completa" = "eliminar")),
                              actionButton("apply_outliers", "Auditar y Limpiar", class = "btn-danger"),
                              hr(),
                              downloadButton("download_data", "Descargar BD Limpia", class = "btn-success")
                            ),
                            mainPanel(
                              HTML("<div class='instrucciones-box'><b>INSTRUCCIONES:</b> Pega o carga tus datos. Asegúrate de auditar atípicos (por grupos) y corregir faltantes antes de pasar a la estadística.</div>"),
                              tabsetPanel(
                                tabPanel("Base de Datos", br(), verbatimTextOutput("data_status"), DTOutput("tabla_visual")),
                                tabPanel("Reporte NAs y Distribución", br(), fluidRow(column(6, plotlyOutput("plot_faltantes")), column(6, uiOutput("hist_var_ui"), radioButtons("hist_type", "Tipo", choices=c("Frecuencia"="freq", "Densidad"="density"), inline=T), sliderInput("hist_bins", "Bins", min=5, max=100, value=30), plotlyOutput("plot_hist")))),
                                tabPanel("Boxplot Interactivo", br(), colourpicker::colourInput("box_col", "Color", "#3498db"), plotlyOutput("plot_boxplot_eda", height="500px")),
                                tabPanel("Log de Atípicos Detectados", br(), DTOutput("tabla_iqr_detallada"))
                              )
                            )
                          )
                 ),
                 
                 tabPanel("Estadística Descriptiva",
                          sidebarLayout(
                            sidebarPanel(
                              h4("1. Selección de Datos"),
                              uiOutput("desc_var_ui"),
                              uiOutput("x_group_ui"),
                              radioButtons("stats_level", "Calcular Estadísticos:", choices = c("Global" = "overall", "Por Grupos" = "grouped")),
                              hr(),
                              h4("2. Gráficos Estadísticos"),
                              selectInput("plot_type", "Tipo de Gráfico:", choices = c("Boxplot Individual", "Boxplot por grupos", "Violin Plot", "Dot Plot", "Ridgeline Plot", "Barras Comparativas (Medias)", "Líneas y Puntos", "Líneas de Error (Media ± ...)")),
                              textInput("plot_title", "Título del Gráfico", "Distribución de Datos"),
                              colourpicker::colourInput("plot_color", "Color Principal", "#3498db"),
                              colourpicker::colourInput("border_color", "Color Borde", "black"),
                              sliderInput("line_width", "Grosor de Línea", min = 0.5, max = 3, value = 1.2),
                              conditionalPanel(condition = "input.plot_type == 'Líneas y Puntos' || input.plot_type == 'Líneas de Error (Media ± ...)'", selectInput("line_type", "Tipo de Línea", choices = c("solid", "dashed", "dotted", "dotdash"))),
                              conditionalPanel(condition = "input.plot_type == 'Boxplot Individual' || input.plot_type == 'Boxplot por grupos' || input.plot_type == 'Dot Plot' || input.plot_type == 'Líneas y Puntos' || input.plot_type == 'Líneas de Error (Media ± ...)'", selectInput("point_shape", "Forma del Punto (pch)", choices = 0:25, selected = 19)),
                              conditionalPanel(condition = "input.plot_type == 'Barras Comparativas (Medias)'", checkboxInput("show_error_bars", "Mostrar Error Estándar (SE)", value = TRUE)),
                              conditionalPanel(condition = "input.plot_type == 'Líneas de Error (Media ± ...)'", radioButtons("error_type", "Tipo de Error:", choices = c("Error Estándar (SE)" = "se", "Desviación Estándar (SD)" = "sd", "Intervalo de Confianza (95%)" = "ic95"), inline = TRUE))
                            ),
                            mainPanel(
                              HTML("<div class='instrucciones-box'><b>MÓDULO DESCRIPTIVO:</b> Genera tablas dinámicas con 21 métricas diferentes, gráficos personalizables listos para publicación y detecta atípicos extremos mediante Grubbs y Z-scores.</div>"),
                              tabsetPanel(
                                tabPanel("Tabla Dinámica", br(), DTOutput("tabla_estadisticos")),
                                tabPanel("Gráfico Personalizado", br(), plotOutput("plot_desc", height = "550px"), actionButton("btn_pop_desc", "Abrir Gráfico en Ventana NATIVA", icon = icon("external-link-alt"), class = "btn-info")),
                                tabPanel("Pruebas y Outliers", br(), h4("Normalidad (Shapiro-Wilk)"), verbatimTextOutput("normality_test"), hr(), h4("Atípico Extremo (Test de Grubbs)"), verbatimTextOutput("grubbs_test"), hr(), h4("Z-Scores (Valores > 3 SD)"), DTOutput("outliers_table"))
                              )
                            )
                          )
                 ),
                 
                 tabPanel("Inferencia Estadística",
                          sidebarLayout(
                            sidebarPanel(
                              h4("1. Configuración de Inferencia"),
                              selectInput("inf_pop", "Tipo de Análisis:", choices = c("1 Población" = "1pop", "2 Poblaciones" = "2pop", "Múltiples Poblaciones (IC Medias)" = "multipop")),
                              uiOutput("inf_param_ui"),
                              hr(),
                              h4("2. Selección de Variables"),
                              uiOutput("inf_vars_ui"),
                              hr(),
                              h4("3. Parámetros de la Prueba"),
                              conditionalPanel(condition = "input.inf_pop != 'multipop'", radioButtons("inf_alt", "Hipótesis Alternativa (H1):", choices = c("Bilateral (≠)" = "two.sided", "Mayor (>)" = "greater", "Menor (<)" = "less"))),
                              numericInput("inf_conf", "Nivel de Confianza (1 - α):", value = 0.95, min = 0.50, max = 0.99, step = 0.01),
                              uiOutput("inf_test_options_ui"),
                              actionButton("run_inf", "Ejecutar Prueba", class = "btn-primary", icon = icon("calculator"))
                            ),
                            mainPanel(
                              HTML("<div class='instrucciones-box'><b>MÓDULO DE INFERENCIA:</b> Aquí puedes realizar pruebas formales de hipótesis (T-test, Z-test, Varianzas, Proporciones) y estimar intervalos de confianza poblacionales.</div>"),
                              tabsetPanel(
                                tabPanel("Resultados de la Prueba", br(), verbatimTextOutput("inf_res_output")),
                                tabPanel("Intervalos de Confianza (Tabla)", br(), DTOutput("inf_ic_table"))
                              )
                            )
                          )
                 ),
                 
                 tabPanel("Correlación y Regresión",
                          tabsetPanel(
                            tabPanel("Análisis de Correlación",
                                     sidebarLayout(
                                       sidebarPanel(
                                         h4("Correlación y Dependencias"),
                                         uiOutput("cor_vars_ui"),
                                         selectInput("cor_method", "Método Clásico:", choices = c("pearson", "spearman", "kendall")),
                                         hr(),
                                         actionButton("run_cor", "Ejecutar Análisis", class="btn-primary", icon=icon("project-diagram"))
                                       ),
                                       mainPanel(
                                         HTML("<div class='instrucciones-box'><b>ANÁLISIS DE CORRELACIÓN:</b> Evalúa la relación entre múltiples variables numéricas. Se incluyen pruebas clásicas y métodos avanzados (MIC, dCor) para detectar <b>dependencias no lineales</b>.</div>"),
                                         tabsetPanel(
                                           tabPanel("Tabla de Dependencias Avanzada", 
                                                    br(), downloadButton("dl_cor_csv", "Descargar CSV", class="btn-success"), br(), br(),
                                                    DTOutput("tabla_dependencias")),
                                           tabPanel("Matrices (Clásica)", verbatimTextOutput("cor_matrices")),
                                           tabPanel("Correlograma", plotOutput("cor_plot", height="600px"), actionButton("btn_pop_cor", "Abrir Correlograma", icon=icon("external-link-alt"), class="btn-info")),
                                           tabPanel("Matriz de Dispersión", plotOutput("scatter_matrix", height="700px"), actionButton("btn_pop_scatter", "Abrir Matriz", icon=icon("external-link-alt"), class="btn-info")),
                                           tabPanel("Gráficos Individuales", br(), uiOutput("cor_individual_select_ui"), plotOutput("cor_indiv_plot", height="500px"))
                                         )
                                       )
                                     )
                            ),
                            tabPanel("Modelos de Regresión",
                                     sidebarLayout(
                                       sidebarPanel(
                                         h4("Ajuste de Regresión"),
                                         uiOutput("reg_y_ui"),
                                         uiOutput("reg_x_ui"),
                                         selectInput("reg_type", "Tipo de Modelo:", choices = c("Lineal Simple / Múltiple" = "lineal", "Polinomial (Grado 2)" = "poly2", "Polinomial (Grado 3)" = "poly3", "Exponencial (Y = a*e^(bX))" = "exp", "Logarítmica (Y = a + b*ln(X))" = "log")),
                                         checkboxInput("reg_no_int", "Forzar paso por el Origen (Sin Intercepto Y)", value = FALSE),
                                         hr(),
                                         actionButton("run_reg", "Ajustar Modelo", class="btn-primary", icon=icon("chart-line"))
                                       ),
                                       mainPanel(
                                         HTML("<div class='instrucciones-box'><b>MODELOS DE REGRESIÓN:</b> El resumen evalúa la calidad total del ajuste (VIF, AIC, BIC, R²) y muestra la ecuación resultante. Se incluye Breusch-Pagan para verificar la homocedasticidad.</div>"),
                                         tabsetPanel(
                                           tabPanel("Métricas y Resumen", 
                                                    h4("Ecuación del Modelo Ajustado"), verbatimTextOutput("reg_equation"), 
                                                    h4("Métricas de Calidad Global"), verbatimTextOutput("reg_metrics"), 
                                                    h4("Coeficientes y P-valores"), verbatimTextOutput("reg_summary")),
                                           tabPanel("Gráfico de Ajuste", plotOutput("reg_fit_plot", height="500px"), actionButton("btn_pop_reg_fit", "Abrir Gráfico Ajuste", icon=icon("external-link-alt"), class="btn-info")),
                                           tabPanel("Supuestos (Residuos)", fluidRow(column(6, plotOutput("reg_res_fit_plot")), column(6, plotOutput("reg_qq_plot"))), verbatimTextOutput("reg_supuestos_txt"))
                                         )
                                       )
                                     )
                            )
                          )
                 ),
                 
                 tabPanel("Diseños Experimentales",
                          tabsetPanel(
                            tabPanel("Experimentos Simples",
                                     sidebarLayout(
                                       sidebarPanel(
                                         h4("Diseño"), selectInput("sim_des", "Diseño Experimental:", choices = c("DCA", "DBCA", "DCL")), uiOutput("sim_vars_ui"), hr(), 
                                         h4("Prueba Múltiple (PMM)"), selectInput("sim_pmm", "Método:", choices = c("Tukey", "Fisher (LSD)", "Duncan", "SNK", "Scott-Knott", "Bonferroni")), numericInput("sim_alpha", "Alfa (α):", value=0.05, min=0.01, max=0.20, step=0.01), 
                                         conditionalPanel(condition = "input.sim_pmm == 'Tukey' || input.sim_pmm == 'Fisher (LSD)' || input.sim_pmm == 'Scott-Knott'", hr(), h4("Estética de Gráfico"), selectInput("sim_plot_err", "Error:", choices = c("SE"="se", "SD"="sd", "IC 95%"="ic")), checkboxInput("sim_facet", "Facetar Gráfico (DBCA/DCL)", value = FALSE), conditionalPanel("input.sim_facet == true", numericInput("sim_facet_ncol", "Columnas panel:", value=2, min=1))), 
                                         hr(), actionButton("run_sim", "Ejecutar Análisis", class="btn-primary", icon=icon("rocket"))
                                       ),
                                       mainPanel(
                                         HTML("<div class='instrucciones-box'><b>INSTRUCCIONES SIMPLES:</b> Selecciona tus variables. Las columnas de Valores Relativos se agruparán automáticamente a la izquierda en la Tabla Resumen.</div>"),
                                         tabsetPanel(
                                           tabPanel("Tabla Resumen", fluidRow(column(4, uiOutput("sim_resumen_vars_ui")), column(4, uiOutput("sim_resumen_testigo_ui")), column(4, br(), actionButton("btn_gen_sim_resumen", "Generar Tabla", class="btn-success", icon=icon("table")))), hr(), DTOutput("sim_tabla_resumen")),
                                           tabPanel("ANDEVA (InfoStat)", verbatimTextOutput("sim_anova")),
                                           tabPanel("Supuestos", verbatimTextOutput("sim_supuestos"), plotOutput("sim_plots", height="400px"), actionButton("btn_pop_sim_sup", "Abrir Supuestos", icon=icon("external-link-alt"), class="btn-info")),
                                           tabPanel("PMM y Gráficos", verbatimTextOutput("sim_pmm_out"), conditionalPanel("input.sim_pmm == 'Tukey' || input.sim_pmm == 'Fisher (LSD)' || input.sim_pmm == 'Scott-Knott'", h4("Gráfico de Medias Reales / Ajustadas"), plotOutput("sim_pmm_plot", height="500px"), actionButton("btn_pop_sim_pmm", "Abrir Gráfico NATIVO", icon=icon("external-link-alt"), class="btn-info"))),
                                           
                                           # LA NUEVA PESTAÑA DE CONTRASTES EN EL LUGAR CORRECTO
                                           tabPanel("Contrastes y Pruebas Especiales",
                                                    br(),
                                                    sidebarLayout(
                                                      sidebarPanel(
                                                        h4("Configuración del Contraste"),
                                                        selectInput("simp_cont_tipo", "Tipo de Prueba:", choices = c("Dunnett (vs Testigo)", "Contraste Ortogonal / Manual")),
                                                        conditionalPanel("input.simp_cont_tipo == 'Dunnett (vs Testigo)'", uiOutput("simp_dunnett_ui")),
                                                        conditionalPanel("input.simp_cont_tipo == 'Contraste Ortogonal / Manual'",
                                                                         HTML("<div style='background-color:#fff3cd; padding:10px; border-radius:5px; font-size:13px;'>
                                                                                <b>Asignación de Coeficientes:</b><br>
                                                                                Asigne los pesos (Ej: 1, 1, -2). Los tratamientos con 0 se ignoran. 
                                                                                La suma debe ser 0 para ser ortogonal.</div><br>"),
                                                                         uiOutput("simp_manual_coef_ui")
                                                        ),
                                                        hr(),
                                                        selectInput("simp_cont_adj", "Ajuste de P-valor:", choices = c("none (Fisher LSD)"="none", "tukey"="tukey", "bonferroni"="bonferroni")),
                                                        actionButton("run_simp_cont", "Calcular Contraste", class="btn-success", icon=icon("calculator"))
                                                      ),
                                                      mainPanel(
                                                        verbatimTextOutput("simp_cont_out")
                                                      )
                                                    )
                                           )
                                         )
                                       )
                                     )
                            ),
                            
                            tabPanel("Experimentos Factoriales",
                                     sidebarLayout(
                                       sidebarPanel(
                                         h4("Factorial"), selectInput("fac_des", "Diseño Base:", choices = c("DCA", "DBCA", "DCL")), selectInput("fac_arr", "Arreglo:", choices = c("Factorial Combinatorio", "Parcelas Divididas", "Parcelas Subdivididas", "Franjas", "Franjas Divididas")), uiOutput("fac_vars_ui"), hr(), 
                                         h4("Prueba Múltiple (PMM)"), selectInput("fac_pmm", "Método:", choices = c("Tukey", "Fisher (LSD)", "Duncan", "SNK", "Scott-Knott", "Bonferroni")), numericInput("fac_alpha", "Alfa (α):", value=0.05, min=0.01, max=0.20, step=0.01), uiOutput("fac_pmm_terms_ui"), uiOutput("fac_plot_options_ui"), hr(), actionButton("run_fac", "Ejecutar Análisis", class="btn-primary", icon=icon("rocket"))
                                       ),
                                       mainPanel(
                                         HTML("<div class='instrucciones-box'><b>INSTRUCCIONES FACTORIALES:</b> Selecciona el arreglo correcto. Revisa la pestaña <b>'Desdoblamientos'</b> para analizar interacciones complejas con letras de significancia anidadas.</div>"),
                                         tabsetPanel(
                                           tabPanel("Tabla Resumen", fluidRow(column(4, uiOutput("fac_resumen_vars_ui")), column(4, uiOutput("fac_resumen_term_ui")), column(4, uiOutput("fac_resumen_testigo_ui"))), actionButton("btn_gen_fac_resumen", "Generar Tabla", class="btn-success", icon=icon("table")), hr(), DTOutput("fac_tabla_resumen")),
                                           tabPanel("ANDEVA (InfoStat)", verbatimTextOutput("fac_anova")),
                                           tabPanel("Supuestos", verbatimTextOutput("fac_supuestos"), plotOutput("fac_plots", height="400px"), actionButton("btn_pop_fac_sup", "Abrir Supuestos", icon=icon("external-link-alt"), class="btn-info")),
                                           tabPanel("PMM Global", verbatimTextOutput("fac_pmm_out"), conditionalPanel("input.fac_pmm == 'Tukey' || input.fac_pmm == 'Fisher (LSD)' || input.fac_pmm == 'Scott-Knott'", h4("Gráfico de Medias"), plotOutput("fac_pmm_plot", height="450px"), actionButton("btn_pop_fac_pmm", "Abrir Gráfico Medias", icon=icon("external-link-alt"), class="btn-info"), h4("Gráfico de Interacción / Líneas"), plotOutput("fac_interaction_plot", height="400px"), actionButton("btn_pop_fac_int", "Abrir Gráfico Interacción", icon=icon("external-link-alt"), class="btn-info"))),
                                           tabPanel("Desdoblamientos", verbatimTextOutput("fac_desdob_txt"), plotOutput("fac_desdob_plot", height="800px"), actionButton("btn_pop_fac_desdob", "Abrir Panel de Desdoblamientos", icon=icon("external-link-alt"), class="btn-info"))
                                         )
                                       )
                                     )
                            ),
                            
                            tabPanel("Multi-Ambientales",
                                     sidebarLayout(
                                       sidebarPanel(
                                         h4("Configuración del Modelo"),
                                         uiOutput("ma_vars_ui"),
                                         hr(),
                                         h4("Opciones de Pruebas y Gráficos"),
                                         uiOutput("ma_pmm_terms_ui"),
                                         selectInput("ma_pmm", "Método PMM:", choices = c("Tukey", "Fisher (LSD)", "Scott-Knott")),
                                         numericInput("ma_alpha", "Significancia (α):", value=0.05, min=0.01, max=0.20, step=0.01),
                                         selectInput("ma_plot_err", "Error en barras:", choices = c("SE"="se", "SD"="sd", "IC 95%"="ic")),
                                         checkboxInput("ma_facet", "Facetar Gráficos", value = FALSE),
                                         uiOutput("ma_facet_ui"),
                                         conditionalPanel("input.ma_facet == true", numericInput("ma_facet_ncol", "Columnas panel:", value=2, min=1)),
                                         hr(),
                                         actionButton("run_ma", "Ejecutar Análisis", class="btn-primary", icon=icon("rocket"))
                                       ),
                                       mainPanel(
                                         HTML("<div class='instrucciones-box'><b>MÓDULO MULTI-AMBIENTAL:</b> Soporta múltiples factores de tratamiento. Detectará automáticamente si debe usar un ANDEVA clásico de Parcelas Divididas (1 Zafra) o un Modelo Mixto LMER (2+ Zafras).</div>"),
                                         tabsetPanel(
                                           tabPanel("Tabla Resumen", 
                                                    br(),
                                                    fluidRow(
                                                      column(6, uiOutput("ma_resumen_testigo_ui")),
                                                      column(6, br(), actionButton("btn_gen_ma_resumen", "Generar Tabla Resumen", class="btn-success", icon=icon("table")))
                                                    ),
                                                    hr(), 
                                                    DTOutput("ma_tabla_resumen")
                                           ),
                                           tabPanel("ANDEVA / Modelo Mixto", verbatimTextOutput("ma_anova")),
                                           tabPanel("Supuestos", verbatimTextOutput("ma_supuestos"), plotOutput("ma_plots", height="400px"), actionButton("btn_pop_ma_sup", "Abrir Supuestos", icon=icon("external-link-alt"), class="btn-info")),
                                           tabPanel("PMM y Gráficos", verbatimTextOutput("ma_pmm_out"), plotOutput("ma_pmm_plot", height="400px"), plotOutput("ma_int_plot", height="400px"), actionButton("btn_pop_ma_pmm", "Abrir Medias", icon=icon("external-link-alt"), class="btn-info"), actionButton("btn_pop_ma_int", "Abrir Interacción", icon=icon("external-link-alt"), class="btn-info"))
                                         )
                                       )
                                     )
                            ),
                            
                            tabPanel("Análisis de Consolidados",
                                     sidebarLayout(
                                       sidebarPanel(
                                         h4("Configuración del Consolidado"),
                                         radioButtons("cons_tipo", "Tipo de Experimento:", choices = c("Simple" = "simple", "Factorial" = "factorial"), inline = TRUE),
                                         uiOutput("cons_factores_ui"),
                                         uiOutput("cons_amb_ui"),
                                         uiOutput("cons_blk_ui"),
                                         hr(),
                                         h4("Variables y Post-Hoc"),
                                         uiOutput("cons_vars_ui"),
                                         selectInput("cons_pmm", "Prueba de Medias:", choices = c("Tukey", "Fisher (LSD)")),
                                         selectInput("cons_modo_letras", "Calcular letras para:", choices = c("Automático (Mejor Efecto)" = "auto", "Interacción Tratamiento:Ambiente" = "interaccion", "Solo Tratamiento Principal" = "principal")),
                                         uiOutput("cons_exclude_ui"),
                                         numericInput("cons_fincas_tabla", "Fincas por Tabla (Ambiente):", value = 1, min = 1),
                                         numericInput("cons_alpha", "Significancia (α):", value = 0.15, min = 0.01, max = 0.25, step = 0.01),
                                         hr(),
                                         actionButton("run_cons", "Generar Análisis Dual", class="btn-warning", icon=icon("layer-group"))
                                       ),
                                       mainPanel(
                                         HTML("<div class='instrucciones-box'><b>MÓDULO DE CONSOLIDADOS:</b> Motor original restaurado. Integra el análisis detallado <b>Por Finca</b> y el análisis <b>Global Maestro</b>. Generará automáticamente las columnas relativas y los P-valores anidados.</div>"),
                                         tabsetPanel(
                                           tabPanel("Tabla Resumen Global",
                                                    br(), h4("Cuadro de Resumen Global Maestro (Promedios, Relativos y P-valores)"),
                                                    hr(),
                                                    DTOutput("cons_tabla_maestra_out")),
                                           tabPanel("Resumen por Ambiente",
                                                    br(), h4("Medias y Letras desglosadas por Finca/Ambiente"),
                                                    uiOutput("cons_tablas_ambiente_ui")),
                                           tabPanel("Detalle ANDEVA y Supuestos",
                                                    br(), verbatimTextOutput("cons_anova_txt"), 
                                                    hr(), HTML("<h4>Prueba de Shapiro-Wilk (Global)</h4>"), verbatimTextOutput("cons_shapiro_txt"),
                                                    hr(), HTML("<h4>Gráficos de Diagnóstico (Global)</h4>"), uiOutput("cons_supuestos_plots"))
                                         )
                                       )
                                     )
                            )
                          )
                 ),
                 
                 tabPanel("Modelos Mixtos (LMM Avanzado)",
                          sidebarLayout(
                            sidebarPanel(
                              # UI Limpia (Los botones y cajas de texto ahora se generan desde el servidor)
                              uiOutput("lmm_vars_ui")
                            ),
                            mainPanel(
                              HTML("<div class='instrucciones-box'><b>USO DE MODELOS MIXTOS (lme4/lmerTest):</b> Libertad absoluta para crear modelos jerárquicos y cruzados.<br><b>1. Efectos Cruzados:</b> <code>Y ~ Trat + (1|Finca) + (1|Zafra)</code><br><b>2. Anidamiento:</b> <code>Y ~ Trat * Zafra + (1|Zafra/Finca)</code></div>"),
                              tabsetPanel(
                                tabPanel("Diagnóstico y Ajuste", verbatimTextOutput("lmm_diag")),
                                tabPanel("ANDEVA (Satterthwaite)", verbatimTextOutput("lmm_anova")),
                                tabPanel("Supuestos (Residuos)", verbatimTextOutput("lmm_supuestos"), plotOutput("lmm_plots_dharma", height="600px"), actionButton("btn_pop_lmm_sup", "Abrir Gráficos", icon=icon("external-link-alt"), class="btn-info")),
                                tabPanel("Efectos Aleatorios y BLUPs", verbatimTextOutput("lmm_blups_txt"), plotOutput("lmm_blups_plot", height="500px"), actionButton("btn_pop_lmm_blups", "Abrir BLUPs", icon=icon("external-link-alt"), class="btn-info"))
                              )
                            )
                          )
                 ),
                 
                 tabPanel("Análisis de Plagas (MLG y Gráficos)",
                          tabsetPanel(
                            tabPanel("Modelos (MLG / MLM)",
                                     sidebarLayout(
                                       sidebarPanel(
                                         h4("Análisis Generalizado y Mixto"), uiOutput("mlg_vars_ui"), hr(),
                                         h4("Familia / Distribución"), checkboxGroupInput("mlg_family", "Evaluar las siguientes:", choices = c("Poisson"="poisson", "Quasi-Poisson"="quasipoisson", "Binomial Negativa"="binomial_negativa", "Quasibinomial (Proporciones)"="quasibinomial", "Beta (Betareg)"="beta", "Beta Mixto (glmmTMB)"="beta_mixed"), selected = c("poisson", "quasipoisson", "binomial_negativa")), hr(),
                                         h4("Post-Hoc (emmeans)"), uiOutput("mlg_term_ui"), numericInput("mlg_alpha", "Significancia (α):", value = 0.05, min = 0.01, max = 0.20, step = 0.01), checkboxInput("mlg_ajuste_beta", "Ajuste automático de 0s y 1s (Transformación Smithson)", value = TRUE), hr(), actionButton("run_mlg", "Ejecutar Análisis Completo", class="btn-primary", icon=icon("bug"))
                                       ),
                                       mainPanel(
                                         HTML("<div class='instrucciones-box'><b>INSTRUCCIONES MLG DE PLAGAS:</b> Este módulo es para datos de <b>Conteos</b> (insectos, malezas) y <b>Proporciones</b> (daño, eficiencia). Toda la validación usa simulaciones de DHARMa.</div>"),
                                         tabsetPanel(
                                           tabPanel("Diagnóstico Inicial", verbatimTextOutput("mlg_diagnostico")),
                                           tabPanel("Devianza / ANDEVA", verbatimTextOutput("mlg_anova")),
                                           tabPanel("Supuestos DHARMa", verbatimTextOutput("mlg_supuestos"), plotOutput("mlg_plot_dharma", height="700px"), actionButton("btn_pop_mlg_sup", "Abrir DHARMa", icon=icon("external-link-alt"), class="btn-info")),
                                           tabPanel("Medias e IC", verbatimTextOutput("mlg_pmm_txt"), plotOutput("mlg_pmm_plot", height="500px"), actionButton("btn_pop_mlg_pmm", "Abrir Gráfico", icon=icon("external-link-alt"), class="btn-info"))
                                         )
                                       )
                                     )
                            ),
                            tabPanel("Dinámica Poblacional (UDE/NDE)",
                                     sidebarLayout(
                                       sidebarPanel(
                                         h4("Configuración de Curva"), uiOutput("dyn_vars_ui"),
                                         h4("Umbrales de Daño"),
                                         checkboxInput("show_ude", "Mostrar UDE", value = TRUE),
                                         conditionalPanel("input.show_ude == true", numericInput("val_ude", "Valor UDE:", value = 2, step = 0.1), textInput("label_ude", "Etiqueta UDE:", value = "UDE: 0.1%"), selectInput("col_ude", "Color UDE:", choices = c("firebrick", "red", "darkred", "orange", "blue"), selected = "firebrick")),
                                         checkboxInput("show_nde", "Mostrar NDE", value = FALSE),
                                         conditionalPanel("input.show_nde == true", numericInput("val_nde", "Valor NDE:", value = 1.5, step = 0.1), textInput("label_nde", "Etiqueta NDE:", value = "NDE: 1.5%"), selectInput("col_nde", "Color NDE:", choices = c("orange", "darkorange", "gold", "red"), selected = "orange")),
                                         hr(),
                                         h4("Estética del Gráfico"),
                                         textInput("dyn_title", "Título del Gráfico:", value = "Curva de densidad poblacional de Diatraea por tratamiento"),
                                         selectInput("dyn_palette", "Paleta de Colores:", choices = c("Set1", "Set2", "Dark2", "Paired", "Spectral")),
                                         numericInput("dyn_ymax", "Límite superior Y (Mínimo):", value = 0.5, step = 0.1),
                                         hr(), actionButton("run_dyn", "Generar Curva", class="btn-success", icon=icon("chart-area"))
                                       ),
                                       mainPanel(
                                         HTML("<div class='instrucciones-box'><b>CURVA POBLACIONAL:</b> Genera la gráfica de evolución temporal con la tabla de datos exacta adjunta abajo.</div>"),
                                         plotOutput("plot_dinamica", height="750px"), actionButton("btn_pop_dyn", "Abrir en Ventana NATIVA", icon=icon("external-link-alt"), class="btn-info")
                                       )
                                     )
                            ),
                            tabPanel("Comparación de Estadíos",
                                     sidebarLayout(
                                       sidebarPanel(
                                         h4("Configuración de Estadíos"),
                                         uiOutput("est_vars_ui"),
                                         textInput("est_plaga_name", "Nombre común de la Plaga:", value = "Diatraea spp."),
                                         selectInput("est_palette", "Paleta de Colores:", choices = c("mako", "viridis", "plasma", "cividis", "inferno")),
                                         hr(), actionButton("run_est", "Generar Gráfico Facetado", class="btn-success", icon=icon("chart-bar"))
                                       ),
                                       mainPanel(
                                         HTML("<div class='instrucciones-box'><b>ESTADÍOS:</b> Selecciona las variables correspondientes a los diferentes estadíos (ej. larva, adulto). El sistema las apilará, calculará medias y errores estándar, y las dividirá en facetas automáticamente.</div>"),
                                         plotOutput("plot_estadios", height="600px"), actionButton("btn_pop_est", "Abrir en Ventana NATIVA", icon=icon("external-link-alt"), class="btn-info")
                                       )
                                     )
                            )
                          )
                 ),
                 
                 tabPanel("Transformación y ANCOVA",
                          tabsetPanel(
                            tabPanel("Transformación", sidebarLayout(sidebarPanel(h4("Transformación de Datos"), uiOutput("trans_var_ui"), selectInput("trans_method", "Método:", choices=c("Log10"="log10", "Ln"="ln", "Raíz"="sqrt", "Arcoseno"="arcsin", "Box-Cox"="boxcox")), actionButton("run_trans", "Guardar en Base", class="btn-warning")), mainPanel(HTML("<div class='instrucciones-box'><b>INSTRUCCIONES:</b> Si los supuestos fallan, transforma la variable aquí. <b>Arcoseno</b> es para porcentajes. <b>Box-Cox</b> busca el lambda óptimo.</div>"), fluidRow(column(6, plotOutput("trans_plot_orig")), column(6, plotOutput("trans_plot_new"))), verbatimTextOutput("trans_shapiro")))),
                            tabPanel("ANCOVA Clásico", sidebarLayout(sidebarPanel(h4("ANCOVA Aislado"), selectInput("anc_des", "Diseño:", choices=c("DCA", "DBCA", "DCL")), uiOutput("anc_vars_ui"), hr(), selectInput("anc_pmm", "Ajuste emmeans:", choices=c("Tukey"="tukey", "Bonferroni"="bonferroni", "Sidak"="sidak", "Ninguno"="none")), actionButton("run_anc", "Ejecutar", class="btn-primary")), mainPanel(HTML("<div class='instrucciones-box'><b>INSTRUCCIONES ANCOVA:</b> Evalúa la relación entre Y y el Tratamiento ajustando por la Covariable X. Revisa la Prueba de Homogeneidad de Pendientes.</div>"), verbatimTextOutput("anc_anova"), verbatimTextOutput("anc_hp"), verbatimTextOutput("anc_sup"), verbatimTextOutput("anc_emmeans"), plotOutput("anc_plot"), actionButton("btn_pop_anc", "Abrir Gráfico", icon=icon("external-link-alt"), class="btn-info"))))
                          )
                 )
)

# =====================================================================
# --- SERVIDOR (SERVER) ---
# =====================================================================
server <- function(input, output, session) {
  
  # Contenedores reactivos globales para blindar la memoria de Shiny
  vals <- reactiveValues(datos = NULL, reporte = data.frame(), 
                         p_desc = NULL, p_sim_sup = NULL, p_sim_pmm = NULL, 
                         p_fac_sup = NULL, p_fac_pmm = NULL, p_fac_int = NULL, p_fac_desdob = NULL,
                         p_ma_sup = NULL, p_ma_pmm = NULL, p_ma_int = NULL, 
                         p_lmm_sup = NULL, p_lmm_blups = NULL,
                         p_mlg_sup = NULL, p_mlg_pmm = NULL, p_dyn = NULL, p_est = NULL, p_anc = NULL,
                         p_cor = NULL, p_scatter = NULL, p_cor_indiv = NULL, 
                         p_reg_fit = NULL, tabla_dependencias = NULL,
                         cons_tablas_ambiente = NULL, cons_tabla_maestra = NULL, 
                         cons_anova_txt = NULL, cons_shapiro_txt = NULL, cons_graficos_sup = NULL)
  
  refresh_trigger <- reactiveVal(0)
  
  # =====================================================================
  # --- CARGA POR ARCHIVO/PORTAPAPELES Y LIMPIEZA INICIAL ---
  # =====================================================================
  observeEvent(input$file1, { req(input$file1); output$select_sheet <- renderUI({ selectInput("sheet_selected", "Hoja:", choices = excel_sheets(input$file1$datapath)) }) })
  
  observe({
    req(input$import_method == "file", input$sheet_selected)
    tryCatch({ 
      df_in <- read_excel(input$file1$datapath, sheet = input$sheet_selected)
      # Forzar conversión a numérico de columnas que lo parezcan para evitar fallos
      for(col in names(df_in)) { if(all(!is.na(suppressWarnings(as.numeric(na.omit(as.character(df_in[[col]]))))))) { df_in[[col]] <- as.numeric(as.character(df_in[[col]])) } }
      vals$datos <- as.data.frame(df_in); vals$reporte <- data.frame(); refresh_trigger(refresh_trigger() + 1); showNotification("Archivo cargado con éxito", type="message") 
    }, error = function(e) { showNotification(e$message, type="error") })
  })
  
  observeEvent(input$paste_btn, {
    tryCatch({ 
      df_clip <- read_clip_tbl(); if (is.null(df_clip) || nrow(df_clip) == 0) { showNotification("Portapapeles vacío", type = "warning"); return(NULL) }
      df_clip <- df_clip[rowSums(is.na(df_clip) | df_clip == "") != ncol(df_clip), , drop = FALSE]; df_clip <- df_clip[, colSums(is.na(df_clip)) != nrow(df_clip), drop = FALSE]
      for(col in names(df_clip)) { if(all(!is.na(suppressWarnings(as.numeric(na.omit(as.character(df_clip[[col]]))))))) { df_clip[[col]] <- as.numeric(as.character(df_clip[[col]])) } }
      vals$datos <- as.data.frame(df_clip); vals$reporte <- data.frame(); refresh_trigger(refresh_trigger() + 1); showNotification("Datos pegados con éxito", type="message") 
    }, error = function(e) { showNotification(e$message, type="error") })
  })
  
  output$tabla_visual <- renderDataTable({ req(vals$datos); datatable(vals$datos, editable=TRUE, extensions = 'ColReorder', options=list(scrollX=TRUE, pageLength=15, dom = "Bfrtip", colReorder = TRUE), class = "compact stripe hover", rownames = FALSE) })
  observeEvent(input$tabla_visual_cell_edit, { info <- input$tabla_visual_cell_edit; vals$datos[info$row, info$col] <- info$value; refresh_trigger(refresh_trigger() + 1) })
  output$data_status <- renderPrint({ req(vals$datos); cat("Filas:", nrow(vals$datos), "| Columnas:", ncol(vals$datos)) })
  
  # =====================================================================
  # --- SELECTORES GLOBALES (EDA Y DESCRIPTIVA) ---
  # =====================================================================
  output$hist_var_ui <- renderUI({ req(vals$datos); selectInput("hist_v", "Variable:", choices = c("Seleccione..." = "", names(vals$datos))) })
  output$eda_var_ui <- renderUI({ req(vals$datos); selectizeInput("eda_v", "Variable(s) a evaluar:", choices = names(dplyr::select_if(vals$datos, is.numeric)), multiple = TRUE) })
  output$eda_group_ui <- renderUI({ req(vals$datos); selectInput("eda_g", "Agrupar por (Opcional):", choices = c("Sin agrupación" = "", names(vals$datos))) })
  
  # Selectores de Estadística Descriptiva
  output$desc_var_ui <- renderUI({ req(vals$datos); selectInput("desc_v", "Variable Numérica (eje Y):", choices = c("Seleccione..." = "", names(dplyr::select_if(vals$datos, is.numeric)))) })
  output$x_group_ui <- renderUI({ req(vals$datos); selectInput("x_group", "Variable de Agrupación (eje X / grupos):", choices = c("Seleccione..." = "", names(vals$datos))) })
  
  # =====================================================================
  # --- EDA Y ATÍPICOS ---
  # =====================================================================
  output$plot_hist <- renderPlotly({ req(vals$datos, input$hist_v); if(input$hist_v == "") return(NULL); df <- vals$datos; df[[input$hist_v]] <- as.numeric(as.character(df[[input$hist_v]])); p <- ggplot(df, aes_string(x = input$hist_v)); if (input$hist_type == "density") { p <- p + geom_histogram(aes(y = after_stat(density)), bins = input$hist_bins, fill = "#3498db", color = "white", alpha = 0.7) + geom_density(color = "#e74c3c", linewidth = 1.2) } else { p <- p + geom_histogram(bins = input$hist_bins, fill = "#3498db", color = "white", alpha = 0.7) }; p <- p + theme_light(base_size = 14) + labs(title = paste("Distribución de", input$hist_v), x = input$hist_v, y = if(input$hist_type == "density") "Densidad" else "Frecuencia"); ggplotly(p) %>% config(displayModeBar = TRUE) })
  
  output$plot_faltantes <- renderPlotly({ req(vals$datos); na_counts <- colSums(is.na(vals$datos)); if(sum(na_counts) == 0) { return(ggplotly(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "La base no tiene valores faltantes", size = 6, color = "#27ae60") + theme_void())) }; na_df <- data.frame(Variable = names(na_counts), Faltantes = na_counts) %>% filter(Faltantes > 0) %>% arrange(desc(Faltantes)) %>% mutate(Porcentaje = Faltantes / nrow(vals$datos) * 100); p <- ggplot(na_df, aes(x = reorder(Variable, -Faltantes), y = Faltantes, text = paste("Faltantes:", Faltantes, "<br>Porcentaje:", round(Porcentaje, 2), "%"))) + geom_bar(stat = "identity", fill = "#e74c3c", alpha = 0.8) + theme_light() + labs(title = "Pareto de NAs", x = "Variables", y = "Cantidad NAs") + theme(axis.text.x = element_text(angle = 45, hjust = 1)); ggplotly(p, tooltip = "text") %>% config(displayModeBar = TRUE) })
  
  observeEvent(input$run_imputation, { req(vals$datos, input$imp_method); df <- vals$datos; if(sum(is.na(df)) == 0) { showNotification("No hay faltantes.", type = "warning"); return() }; tryCatch({ if(input$imp_method == "mean") df <- df %>% mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) else if (input$imp_method == "median") df <- df %>% mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .))) else if (input$imp_method == "pmm") df <- complete(mice(df, m=1, method='pmm', printFlag=FALSE)) else if (input$imp_method == "knn") df <- kNN(df, imp_var=FALSE) else if (input$imp_method == "cart") df <- complete(mice(df, m=1, method='cart', printFlag=FALSE)); vals$datos <- df; refresh_trigger(refresh_trigger() + 1); showNotification("Imputación exitosa.", type="message") }, error = function(e) { showNotification(e$message, type="error") }) })
  
  output$plot_boxplot_eda <- renderPlotly({ req(vals$datos, input$eda_v); var_plot <- input$eda_v[1]; df <- vals$datos; df[[var_plot]] <- as.numeric(as.character(df[[var_plot]])); if(is.null(input$eda_g) || input$eda_g == "") { p <- ggplot(df, aes_string(x = "''", y = var_plot)) + geom_boxplot(fill = input$box_col, outlier.color = "#e74c3c", outlier.size = 2, alpha = 0.85) + theme_light(base_size = 14) + labs(title = paste("Boxplot de", var_plot), x = "Global", y = var_plot) } else { p <- ggplot(df, aes_string(x = input$eda_g, y = var_plot, fill = input$eda_g)) + geom_boxplot(outlier.color = "#e74c3c", outlier.size = 2, alpha = 0.85) + scale_fill_manual(values = rep(input$box_col, 100)) + theme_light(base_size = 14) + labs(title = paste("Boxplot de", var_plot, "por", input$eda_g), x = input$eda_g, y = var_plot) + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") }; ggplotly(p) %>% config(displayModeBar = TRUE) })
  
  output$tabla_iqr_detallada <- renderDT({ if(is.null(vals$reporte) || nrow(vals$reporte) == 0) { return(datatable(data.frame(Mensaje = "No atípicos."), options = list(dom='t'), rownames=FALSE)) }; datatable(vals$reporte, options = list(scrollX=TRUE, pageLength=15), rownames=FALSE, class = "cell-border stripe") })
  
  observeEvent(input$apply_outliers, { req(vals$datos, input$eda_v); df <- vals$datos; variables_num <- input$eda_v; grupo_col <- input$eda_g; accion <- input$outlier_action; if (is.null(grupo_col) || grupo_col == "") { df$ID_GRUPO <- "Todos_los_datos" } else { df$ID_GRUPO <- as.character(df[[grupo_col]]) }; niveles_grupo <- unique(df$ID_GRUPO); reporte_full <- data.frame(); filas_a_eliminar <- c()
  for (v in variables_num) { df[[v]] <- as.numeric(as.character(df[[v]])); hallazgos_var <- data.frame(); for (grupo in niveles_grupo) { mask_grupo <- df$ID_GRUPO == grupo & !is.na(df[[v]]); valores <- df[[v]][mask_grupo]; indices_reales <- which(mask_grupo); if (length(valores) >= 6) { qs <- quantile(valores, probs = c(0.25, 0.75), na.rm = TRUE, type = 6); Q1 <- qs[1]; Q3 <- qs[2]; IQR_val <- Q3 - Q1; lower <- Q1 - 1.5 * IQR_val; upper <- Q3 + 1.5 * IQR_val; outliers_mask_local <- (valores < (lower - 1e-9) | valores > (upper + 1e-9)); if (any(outliers_mask_local)) { idx_outliers <- indices_reales[outliers_mask_local]; hallazgos_var <- rbind(hallazgos_var, data.frame(Variable = v, Fila = idx_outliers, Grupo = grupo, Valor_Atipico = df[[v]][idx_outliers], Limite_Inf = round(lower, 4), Limite_Sup = round(upper, 4))); if (accion == "NA") { df[[v]][idx_outliers] <- NA }; if (accion == "eliminar") { filas_a_eliminar <- c(filas_a_eliminar, idx_outliers) } } } }; reporte_full <- rbind(reporte_full, hallazgos_var) }
  if (accion == "eliminar" && length(filas_a_eliminar) > 0) { df <- df[-unique(filas_a_eliminar), ] }; df$ID_GRUPO <- NULL; vals$datos <- df; if(nrow(vals$reporte) > 0) { vals$reporte <- rbind(vals$reporte, reporte_full) } else { vals$reporte <- reporte_full }; refresh_trigger(refresh_trigger() + 1); if(nrow(reporte_full) > 0) { showNotification(paste("Auditoría Finalizada. Atípicos:", nrow(reporte_full)), type="warning") } else { showNotification("No se detectaron atípicos.", type="message") }
  })
  
  # =====================================================================
  # --- ESTADÍSTICA DESCRIPTIVA ---
  # =====================================================================
  stats_reactive <- reactive({
    req(vals$datos, input$desc_v); if(input$desc_v == "") return(NULL)
    df <- vals$datos; x_val <- as.numeric(as.character(df[[input$desc_v]]))
    if(input$stats_level == "overall") {
      x <- x_val[!is.na(x_val) & is.finite(x_val)]; if(length(x) < 3) return(data.frame(Estadistico = "Datos insuficientes", Valor = NA_real_))
      n <- length(x); media <- mean(x); mediana <- median(x); moda_val <- tryCatch(modeest::mlv(x, method = "mfv")$M[1], error = function(e) NA_real_)
      de <- sd(x); varianza <- var(x); cv <- (de / media) * 100; ee <- de / sqrt(n); q <- quantile(x, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1), na.rm = TRUE)
      asim <- tryCatch(e1071::skewness(x), error=function(e) NA); kurt <- tryCatch(e1071::kurtosis(x), error=function(e) NA)
      geo <- tryCatch(exp(mean(log(x[x>0]))), error=function(e) NA); harm <- tryCatch(1 / mean(1/x[x>0]), error=function(e) NA)
      rms_val <- sqrt(mean(x^2)); shapiro <- tryCatch(shapiro.test(x)$p.value, error=function(e) NA)
      data.frame(Estadistico = c("N", "Media", "Mediana", "Moda", "Desv. Estándar", "Varianza", "Coef. de Variación (%)", "Error Estándar", "Mínimo", "Percentil 10%", "Q1", "Q2 (Mediana)", "Q3", "Percentil 90%", "Máximo", "Asimetría (Skewness)", "Curtosis", "Media Geométrica", "Media Armónica", "RMS", "Shapiro-Wilk p-value"), Valor = round(c(n, media, mediana, moda_val, de, varianza, cv, ee, q[1], q[2], q[3], q[4], q[5], q[6], q[7], asim, kurt, geo, harm, rms_val, shapiro), 4))
    } else {
      req(input$x_group); if(input$x_group == "") return(NULL)
      group_var <- input$x_group; temp_df <- data.frame(g = df[[group_var]], v = x_val)
      temp_df %>% group_by(g) %>% summarise(N = sum(!is.na(v)), Media = mean(v, na.rm = TRUE), Mediana = median(v, na.rm = TRUE), Moda = tryCatch(modeest::mlv(v[!is.na(v)], method = "mfv")$M[1], error = function(e) NA_real_), `Desv. Estándar` = sd(v, na.rm = TRUE), Varianza = var(v, na.rm = TRUE), `Coef. Variación (%)` = (`Desv. Estándar` / Media) * 100, `Error Estándar` = `Desv. Estándar` / sqrt(N), Mínimo = min(v, na.rm = TRUE), `Perc. 10%` = quantile(v, 0.1, na.rm = TRUE), Q1 = quantile(v, 0.25, na.rm = TRUE), `Q2 (Mediana)` = median(v, na.rm = TRUE), Q3 = quantile(v, 0.75, na.rm = TRUE), `Perc. 90%` = quantile(v, 0.9, na.rm = TRUE), Máximo = max(v, na.rm = TRUE), Asimetría = tryCatch(e1071::skewness(v[!is.na(v)]), error=function(e) NA), Curtosis = tryCatch(e1071::kurtosis(v[!is.na(v)]), error=function(e) NA), .groups = "drop") %>% rename(!!sym(group_var) := g)
    }
  })
  
  output$tabla_estadisticos <- renderDT({ datatable(stats_reactive(), extensions = 'Buttons', options=list(dom="Bfrtip", buttons = c('copy', 'excel'), scrollX=TRUE, pageLength=25), rownames=FALSE, class = "cell-border stripe") })
  
  ggplot_desc <- reactive({ 
    req(vals$datos, input$plot_type, input$plot_title, input$desc_v); if(input$desc_v == "") return(NULL)
    df <- vals$datos; df[[input$desc_v]] <- as.numeric(as.character(df[[input$desc_v]]))
    
    fill_col <- if(is.null(input$plot_color)) "#3498db" else input$plot_color
    border_col <- if(is.null(input$border_color)) "black" else input$border_color
    lw <- if(is.null(input$line_width)) 1.2 else input$line_width
    lty_val <- if(is.null(input$line_type)) 1 else switch(input$line_type, "solid" = 1, "dashed" = 2, "dotted" = 3, "dotdash" = 4, 1)
    pch_val <- if(is.null(input$point_shape)) 19 else as.numeric(input$point_shape)
    title <- input$plot_title
    
    x_var <- if(is.null(input$x_group) || input$x_group=="") { df$dummy<-"Todo"; "dummy" } else input$x_group
    
    if(input$plot_type == "Boxplot Individual") { p <- ggplot(df, aes_string(x="''", y=input$desc_v)) + geom_boxplot(fill=fill_col, color=border_col, linewidth=lw, alpha=0.85, width=0.4) + stat_summary(fun = mean, geom = "point", color = "red", size = 4, shape = pch_val) + theme_classic(base_size = 15) + labs(title=title, y=input$desc_v, x=NULL) + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    } else if(input$plot_type == "Boxplot por grupos") { p <- ggplot(df, aes_string(x=x_var, y=input$desc_v)) + geom_boxplot(fill=fill_col, color=border_col, linewidth=lw, alpha=0.85, outlier.color="red") + stat_summary(fun = mean, geom = "point", color = "red", size = 4, shape = pch_val) + theme_classic(base_size = 15) + labs(title=title, x=x_var, y=input$desc_v) + theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1))
    } else if(input$plot_type == "Violin Plot") { p <- ggplot(df, aes_string(x=x_var, y=input$desc_v)) + geom_violin(fill=fill_col, color=border_col, linewidth=lw, alpha=0.85) + geom_boxplot(width=0.1, fill="white", alpha=0.6, color=border_col) + theme_classic(base_size = 15) + labs(title=title, x=x_var, y=input$desc_v) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if(input$plot_type == "Dot Plot") { p <- ggplot(df, aes_string(x=x_var, y=input$desc_v)) + geom_jitter(width=0.2, height=0, color=fill_col, size=3, shape=pch_val, alpha=0.7) + theme_classic(base_size = 15) + labs(title=title, x=x_var, y=input$desc_v) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if(input$plot_type == "Ridgeline Plot") { p <- ggplot(df, aes_string(x=input$desc_v, y=x_var, fill=x_var)) + geom_density_ridges(alpha=0.6, scale=1.2, color=border_col, linewidth=lw) + theme_classic(base_size = 15) + labs(title=title, x=input$desc_v, y=x_var) + theme(legend.position="none")
    } else if(input$plot_type == "Barras Comparativas (Medias)") { means_df <- df %>% group_by(across(all_of(x_var))) %>% summarise(Media=mean(.data[[input$desc_v]],na.rm=T), SE=sd(.data[[input$desc_v]],na.rm=T)/sqrt(n()), .groups="drop"); p <- ggplot(means_df, aes_string(x=x_var, y="Media")) + geom_bar(stat="identity", fill=fill_col, color=border_col, linewidth=lw, alpha=0.85) + geom_text(aes(label=round(Media, 2), y=Media/2), color="black", fontface="bold", size=3.8) + theme_classic(base_size = 15) + labs(title=title, x=x_var, y="Media") + theme(axis.text.x = element_text(angle = 45, hjust = 1)); if(!is.null(input$show_error_bars) && input$show_error_bars) { p <- p + geom_errorbar(aes(ymin=Media-SE, ymax=Media+SE), width=0.2, color=border_col, linewidth=lw) }
    } else if(input$plot_type == "Líneas y Puntos") { sum_df <- df %>% group_by(across(all_of(x_var))) %>% summarise(Valor=mean(.data[[input$desc_v]],na.rm=T), .groups="drop"); p <- ggplot(sum_df, aes_string(x=x_var, y="Valor")) + geom_line(group=1, color=fill_col, linewidth=lw, linetype=lty_val) + geom_point(color=fill_col, size=4, shape=pch_val) + theme_classic(base_size = 15) + labs(title=title, x=x_var, y=input$desc_v) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if(input$plot_type == "Líneas de Error (Media ± ...)") { sum_df <- df %>% group_by(across(all_of(x_var))) %>% summarise(Media=mean(.data[[input$desc_v]],na.rm=T), SE=sd(.data[[input$desc_v]],na.rm=T)/sqrt(n()), SD=sd(.data[[input$desc_v]],na.rm=T), .groups="drop"); err_t <- if(is.null(input$error_type)) "se" else input$error_type; if(err_t == "ic95") { sum_df <- sum_df %>% mutate(lower=Media - 1.96*SE, upper=Media + 1.96*SE) } else if(err_t == "ee") { sum_df <- sum_df %>% mutate(lower=Media - SD, upper=Media + SD) } else { sum_df <- sum_df %>% mutate(lower=Media - SE, upper=Media + SE) }; p <- ggplot(sum_df, aes_string(x=x_var, y="Media")) + geom_point(color=fill_col, size=4, shape=pch_val) + geom_errorbar(aes(ymin=lower, ymax=upper), width=0.25, color=border_col, linewidth=lw) + geom_line(group=1, color=fill_col, linewidth=lw, linetype=lty_val) + theme_classic(base_size = 15) + labs(title=title, x=x_var, y=input$desc_v) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) }
    return(p) 
  })
  
  output$plot_desc <- renderPlot({ req(ggplot_desc()); vals$p_desc <- ggplot_desc(); vals$p_desc })
  observeEvent(input$btn_pop_desc, { req(vals$p_desc); if(interactive()){ dev.new(noRStudioGD=T); print(vals$p_desc) } })
  output$normality_test <- renderPrint({ req(vals$datos, input$desc_v); if(input$desc_v == "") return(); shapiro.test(as.numeric(as.character(vals$datos[[input$desc_v]]))) })
  output$grubbs_test <- renderPrint({ req(vals$datos, input$desc_v); if(input$desc_v == "") return(); outliers::grubbs.test(as.numeric(as.character(vals$datos[[input$desc_v]]))) })
  output$outliers_table <- renderDT({ req(vals$datos, input$desc_v); if(input$desc_v == "") return(NULL); x <- as.numeric(as.character(vals$datos[[input$desc_v]])); z <- scale(x); idx <- which(abs(z) > 3); if(length(idx)>0) data.frame(Fila=idx, Valor=x[idx]) else data.frame(Mensaje="No outliers extremos (z > 3)") })
  
  # =====================================================================
  # --- INFERENCIA ESTADÍSTICA E INTERVALOS DE CONFIANZA ---
  # =====================================================================
  output$inf_param_ui <- renderUI({ req(input$inf_pop); if(input$inf_pop == "1pop"){ selectInput("inf_param", "Parámetro a contrastar:", choices = c("Media", "Proporción", "Varianza", "Total")) } else if(input$inf_pop == "2pop") { selectInput("inf_param", "Parámetro a contrastar:", choices = c("Medias Independientes", "Medias Pareadas (Dependientes)", "Proporciones", "Varianzas")) } })
  output$inf_vars_ui <- renderUI({ req(vals$datos, input$inf_pop); df <- vals$datos; vars_all <- names(df); if(input$inf_pop == "multipop") { tagList(selectInput("inf_v1", "Variable Numérica (Y):", choices = c("Seleccione..."="", vars_all)), selectInput("inf_v2", "Factor / Tratamiento:", choices = c("Seleccione..."="", vars_all))) } else { req(input$inf_param); if (input$inf_param == "Media") { tagList(selectInput("inf_v1", "Variable (Numérica):", choices = c("Seleccione..."="", vars_all)), numericInput("inf_mu0", "Media Hipotética (H0):", value = 0)) } else if (input$inf_param == "Proporción") { tagList(selectInput("inf_v1", "Variable (Categórica/Binaria):", choices = c("Seleccione..."="", vars_all)), textInput("inf_exito", "Valor de 'Éxito':", value = "1"), numericInput("inf_p0", "Proporción Hipotética (H0):", value = 0.5, step = 0.05)) } else if (input$inf_param == "Varianza") { tagList(selectInput("inf_v1", "Variable (Numérica):", choices = c("Seleccione..."="", vars_all)), numericInput("inf_var0", "Varianza Hipotética (H0):", value = 1)) } else if (input$inf_param == "Total") { tagList(selectInput("inf_v1", "Variable (Numérica):", choices = c("Seleccione..."="", vars_all)), numericInput("inf_N", "Tamaño Población (N):", value = 1000), numericInput("inf_tot0", "Total Hipotético (H0):", value = 0)) } else if (input$inf_param %in% c("Medias Independientes", "Medias Pareadas (Dependientes)")) { tagList(selectInput("inf_v1", "Variable/Grupo 1:", choices = c("Seleccione..."="", vars_all)), selectInput("inf_v2", "Variable/Grupo 2:", choices = c("Seleccione..."="", vars_all)), numericInput("inf_mu0", "Diferencia de Medias (H0):", value = 0)) } else if (input$inf_param == "Proporciones") { tagList(selectInput("inf_v1", "Var 1 (Categórica/Binaria):", choices = c("Seleccione..."="", vars_all)), selectInput("inf_v2", "Var 2 (Categórica/Binaria):", choices = c("Seleccione..."="", vars_all)), textInput("inf_exito", "Valor de 'Éxito':", value = "1")) } else if (input$inf_param == "Varianzas") { tagList(selectInput("inf_v1", "Variable/Grupo 1:", choices = c("Seleccione..."="", vars_all)), selectInput("inf_v2", "Variable/Grupo 2:", choices = c("Seleccione..."="", vars_all)), numericInput("inf_ratio0", "Ratio Hipotético (H0):", value = 1)) } } })
  output$inf_test_options_ui <- renderUI({ 
    req(input$inf_param, input$inf_pop)
    
    # NUEVO: Selector de distribución para múltiples poblaciones
    if(input$inf_pop == "multipop") {
      return(tagList(
        radioButtons("inf_ic_dist", "Estadístico para IC:", 
                     choices = c("Automático (Z si n≥30, T si n<30)" = "auto", 
                                 "Siempre T-Student" = "t", 
                                 "Siempre Z-Normal" = "z"))
      ))
    }
    
    if(input$inf_param == "Media"){ 
      tagList(checkboxInput("inf_known_sd", "Conozco la Desv. Estándar Poblacional (Z-test)", value = FALSE), conditionalPanel("input.inf_known_sd", numericInput("inf_sigma", "Desviación (σ):", value = 1))) 
    } else if(input$inf_param == "Medias Independientes"){ 
      checkboxInput("inf_varequal", "Asumir varianzas poblacionales iguales (Homocedasticidad)", value = FALSE) 
    } else if(input$inf_param %in% c("Proporción", "Proporciones")){ 
      checkboxInput("inf_yates", "Aplicar corrección de Yates", value = TRUE) 
    } 
  })
  
  res_inferencia <- eventReactive(input$run_inf, {
    req(vals$datos, input$inf_v1, input$inf_pop)
    df <- vals$datos; conf <- input$inf_conf
    if(input$inf_pop == "multipop") {
      req(input$inf_v2, input$inf_ic_dist)
      df_clean <- df[complete.cases(df[, c(input$inf_v1, input$inf_v2)]), ]
      df_clean[[input$inf_v1]] <- as.numeric(as.character(df_clean[[input$inf_v1]]))
      
      # NUEVO: Cálculo adaptativo de IC (Z vs T)
      res_ic <- df_clean %>% group_by(.data[[input$inf_v2]]) %>% summarise(
        N = n(), 
        Media = mean(.data[[input$inf_v1]]), 
        SD = sd(.data[[input$inf_v1]]), 
        SE = SD/sqrt(N), 
        Valor_Critico = if(input$inf_ic_dist == "z" || (input$inf_ic_dist == "auto" && N >= 30)) qnorm(1 - (1 - conf)/2) else qt(1 - (1 - conf)/2, df = ifelse(N > 1, N - 1, 1)), 
        Limite_Inferior = Media - Valor_Critico * SE, 
        Limite_Superior = Media + Valor_Critico * SE, 
        .groups="drop"
      ) %>% mutate(across(where(is.numeric), ~round(., 4)))
      
      return(list(type="multipop", data=res_ic))
    } else {
      # ... (aquí se mantiene igual tu código de 1pop y 2pop, no lo borres, déjalo como estaba) ...
      x <- as.numeric(as.character(df[[input$inf_v1]])); x <- x[!is.na(x)]; alt <- input$inf_alt; param <- input$inf_param
      tryCatch({
        if (param == "Media") { if(input$inf_known_sd) { sigma <- input$inf_sigma; n <- length(x); z_stat <- (mean(x) - input$inf_mu0) / (sigma / sqrt(n)); p_val <- if(alt=="two.sided") 2*pnorm(-abs(z_stat)) else if(alt=="less") pnorm(z_stat) else pnorm(z_stat, lower.tail=F); z_crit <- if(alt=="two.sided") qnorm(1-(1-conf)/2) else qnorm(conf); margin <- z_crit * (sigma / sqrt(n)); c_l <- if(alt=="less") -Inf else mean(x) - margin; c_u <- if(alt=="greater") Inf else mean(x) + margin; if(alt=="two.sided"){ c_l <- mean(x) - margin; c_u <- mean(x) + margin }; res_txt <- paste0("\n\tOne Sample Z-test (Varianza Conocida)\n\ndata:  ", input$inf_v1, "\nZ = ", round(z_stat,4), ", p-value = ", signif(p_val,5), "\n", conf*100, " percent confidence interval:\n ", round(c_l,5), "  ", round(c_u,5), "\nsample estimates:\nmean of x \n ", round(mean(x),5), "\n"); return(list(type="test", data=res_txt)) } else { return(list(type="test", data=capture.output(print(t.test(x, mu = input$inf_mu0, alternative = alt, conf.level = conf))))) }
        } else if (param == "Proporción") return(list(type="test", data=capture.output(print(prop.test(sum(df[[input$inf_v1]] == input$inf_exito, na.rm=T), length(df[[input$inf_v1]][!is.na(df[[input$inf_v1]])]), p = input$inf_p0, alternative = alt, conf.level = conf, correct = input$inf_yates)))))
        else if (param == "Varianza") return(list(type="test", data=capture.output(print(EnvStats::varTest(x, sigma.sq = input$inf_var0, alternative = alt, conf.level = conf)))))
        else if (param == "Total") { N <- input$inf_N; n <- length(x); m_x <- mean(x); s_x <- sd(x); se_t <- N * (s_x / sqrt(n)); t_stat <- (N * m_x - input$inf_tot0) / se_t; df_t <- n - 1; p_val <- if(alt=="two.sided") 2*pt(-abs(t_stat), df_t) else if(alt=="less") pt(t_stat, df_t) else pt(t_stat, df_t, lower.tail=F); t_crit <- if(alt=="two.sided") qt(1-(1-conf)/2, df_t) else qt(conf, df_t); margin <- t_crit * se_t; c_l <- if(alt=="less") -Inf else (N*m_x) - margin; c_u <- if(alt=="greater") Inf else (N*m_x) + margin; if(alt=="two.sided"){ c_l <- (N*m_x) - margin; c_u <- (N*m_x) + margin }; res_txt <- paste0("\n\tOne Sample t-test for Population Total\n\ndata:  ", input$inf_v1, "\nt = ", round(t_stat,4), ", df = ", df_t, ", p-value = ", signif(p_val,5), "\n", conf*100, " percent confidence interval:\n ", round(c_l,5), "  ", round(c_u,5), "\nsample estimates:\nestimated total \n ", round(N*m_x,5), "\n"); return(list(type="test", data=res_txt)) }
        else if (param == "Medias Independientes") { req(input$inf_v2); y <- as.numeric(as.character(df[[input$inf_v2]])); y <- y[!is.na(y)]; return(list(type="test", data=capture.output(print(t.test(x, y, paired = F, var.equal = input$inf_varequal, mu = input$inf_mu0, alternative = alt, conf.level = conf))))) }
        else if (param == "Medias Pareadas (Dependientes)") { req(input$inf_v2); tmp <- na.omit(data.frame(x=as.numeric(as.character(df[[input$inf_v1]])), y=as.numeric(as.character(df[[input$inf_v2]])))); return(list(type="test", data=capture.output(print(t.test(tmp$x, tmp$y, paired = T, mu = input$inf_mu0, alternative = alt, conf.level = conf))))) }
        else if (param == "Proporciones") { req(input$inf_v2); return(list(type="test", data=capture.output(print(prop.test(c(sum(df[[input$inf_v1]]==input$inf_exito, na.rm=T), sum(df[[input$inf_v2]]==input$inf_exito, na.rm=T)), c(length(df[[input$inf_v1]][!is.na(df[[input$inf_v1]])]), length(df[[input$inf_v2]][!is.na(df[[input$inf_v2]])])), alternative = alt, conf.level = conf, correct = input$inf_yates))))) }
        else if (param == "Varianzas") { req(input$inf_v2); y <- as.numeric(as.character(df[[input$inf_v2]])); y <- y[!is.na(y)]; return(list(type="test", data=capture.output(print(var.test(x, y, ratio = input$inf_ratio0, alternative = alt, conf.level = conf))))) }
      }, error = function(e) { return(list(type="test", data=paste("Error en cálculo:", e$message))) })
    }
  })
  
  output$inf_res_output <- renderPrint({ req(res_inferencia()); if(res_inferencia()$type == "test") cat(paste(res_inferencia()$data, collapse = "\n")) })
  output$inf_ic_table <- renderDT({ req(res_inferencia()); if(res_inferencia()$type == "multipop") datatable(res_inferencia()$data, extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('copy', 'excel'), pageLength = 15), rownames = FALSE, class = "cell-border stripe") })
  
  # =====================================================================
  # --- ANÁLISIS DE CORRELACIÓN AVANZADA Y DEPENDENCIAS ---
  # =====================================================================
  output$cor_vars_ui <- renderUI({ req(vals$datos); selectizeInput("cor_v", "Variables Numéricas:", choices = names(dplyr::select_if(vals$datos, is.numeric)), multiple = TRUE) })
  
  observeEvent(input$run_cor, {
    req(vals$datos, input$cor_v)
    if(length(input$cor_v) < 2) { showNotification("Seleccione al menos 2 variables", type="warning"); return() }
    
    df_sub <- vals$datos %>% dplyr::select(dplyr::all_of(input$cor_v)) %>% drop_na()
    for(c in names(df_sub)) df_sub[[c]] <- as.numeric(as.character(df_sub[[c]]))
    if(nrow(df_sub) < 5) { showNotification("Datos insuficientes tras eliminar NAs", type="warning"); return() }
    
    metodo <- input$cor_method
    matriz_cor <- cor(df_sub, method = metodo)
    p_matriz <- cor.mtest(df_sub, method = metodo)
    
    output$cor_matrices <- renderPrint({ cat(paste("--- Matriz de Correlación (", toupper(metodo), ") ---\n")); print(matriz_cor); cat("\n--- Matriz de P-valores ---\n"); print(p_matriz) })
    
    output$cor_plot <- renderPlot({ par(mfrow=c(1,1)); corrplot(matriz_cor, method="color", type="upper", order="hclust", addCoef.col="red", tl.col="black", tl.srt=45, p.mat=p_matriz, sig.level=0.15, insig="pch", pch.col="black", col=COL2('RdBu', 10), title=paste("\nCorrelograma de", toupper(metodo)), mar=c(0,0,2,0)) })
    
    output$scatter_matrix <- renderPlot({ p <- ggpairs(df_sub, title=paste("Matriz de Dispersión -", toupper(metodo)), lower=list(continuous=GGally::wrap("smooth", alpha=0.4, color="blue", method="loess")), diag=list(continuous=GGally::wrap("densityDiag", fill="lightblue", alpha=0.5)), upper=list(continuous=GGally::wrap("cor", method=metodo, size=4))); print(p) })
    
    pares <- combn(names(df_sub), 2, simplify = FALSE)
    res_comp <- data.frame()
    lista_graficos_indiv <- list()
    opciones_pares <- c()
    
    withProgress(message = 'Calculando dependencias complejas...', value = 0, {
      for (i in seq_along(pares)) {
        local({
          par <- pares[[i]]; var_x <- par[1]; var_y <- par[2]; x_data <- df_sub[[var_x]]; y_data <- df_sub[[var_y]]
          nom_par <- paste(var_x, "vs", var_y)
          opciones_pares <<- c(opciones_pares, nom_par)
          
          p_pear <- cor.test(x_data, y_data, method="pearson")$p.value
          p_spear <- suppressWarnings(cor.test(x_data, y_data, method="spearman")$p.value)
          dcor_res <- energy::dcor.test(x_data, y_data, R=499); dcor_val <- dcor_res$estimate[1]; dcor_pval <- dcor_res$p.value
          mic_val <- minerva::mine(x_data, y_data)$MIC
          hoeff_pval <- Hmisc::hoeffd(x_data, y_data)$P[1,2]
          
          mejor_g <- NA; mejor_r2 <- NA; mejor_aic <- Inf; ec <- "-"
          for(g in 1:3) {
            mod_t <- lm(y_data ~ poly(x_data, g, raw=TRUE)); sum_t <- summary(mod_t); f_s <- sum_t$fstatistic
            if(!is.null(f_s)) {
              p_v <- pf(f_s[1], f_s[2], f_s[3], lower.tail=FALSE); aic_t <- AIC(mod_t)
              if(!is.na(p_v) && p_v < 0.15 && aic_t < mejor_aic) {
                mejor_aic <- aic_t; mejor_r2 <- sum_t$adj.r.squared; mejor_g <- g
                coefs <- round(coef(mod_t), 4); eq <- paste0("Y = ", coefs[1])
                if(g>=1 && !is.na(coefs[2])) eq <- paste0(eq, ifelse(coefs[2]>=0," + "," - "), abs(coefs[2]), "*X")
                if(g>=2 && !is.na(coefs[3])) eq <- paste0(eq, ifelse(coefs[3]>=0," + "," - "), abs(coefs[3]), "*X^2")
                if(g>=3 && !is.na(coefs[4])) eq <- paste0(eq, ifelse(coefs[4]>=0," + "," - "), abs(coefs[4]), "*X^3")
                ec <- eq
              }
            }
          }
          
          tipo_dep <- "Independiente"
          if(dcor_pval < 0.15 || hoeff_pval < 0.15 || mic_val > 0.4) {
            if(!is.na(mejor_g)) { if(mejor_g==1) tipo_dep<-"Lineal" else if(mejor_g==2) tipo_dep<-"Parábola/U" else if(mejor_g==3) tipo_dep<-"Compleja (S)" } else { tipo_dep <- "No paramétrica" }
          }
          
          res_comp <<- rbind(res_comp, data.frame(Par=nom_par, Relacion=tipo_dep, dCor=round(dcor_val,3), MIC=round(mic_val,3), R2_Adj=ifelse(is.na(mejor_r2), "-", as.character(round(mejor_r2,3))), Ecuacion=ec, p_dCor=round(dcor_pval,4), p_Pear=round(p_pear,4), p_Spear=round(p_spear,4), p_Hoeff=round(hoeff_pval,4)))
          
          lista_graficos_indiv[[nom_par]] <<- ggplot(df_sub, aes_string(x = var_x, y = var_y)) + geom_point(alpha = 0.6, size = 3, color = "black") + geom_smooth(method = "loess", color = "blue", se = FALSE, linewidth=1.2) + labs(title = nom_par, subtitle = paste("Dependencia:", tipo_dep, "| MIC:", round(mic_val, 2), "| dCor:", round(dcor_val, 2)), x = var_x, y = var_y) + theme_classic(base_size=15)
        })
        incProgress(1/length(pares))
      }
    })
    
    vals$tabla_dependencias <- res_comp
    vals$p_cor_indiv <- lista_graficos_indiv
    
    output$tabla_dependencias <- renderDT({ datatable(res_comp, extensions = 'Buttons', options=list(dom="Bfrtip", buttons = c('copy', 'csv', 'excel'), scrollX=TRUE, pageLength=15), rownames=FALSE, class="cell-border stripe") })
    output$dl_cor_csv <- downloadHandler(filename = function() { paste0("Dependencias_", Sys.Date(), ".csv") }, content = function(file) { write.csv(res_comp, file, row.names = FALSE) })
    output$cor_individual_select_ui <- renderUI({ selectInput("cor_indiv_sel", "Seleccione Par a Visualizar:", choices = opciones_pares) })
  })
  
  output$cor_indiv_plot <- renderPlot({ req(input$cor_indiv_sel, vals$p_cor_indiv); vals$p_cor_indiv[[input$cor_indiv_sel]] })
  
  # =====================================================================
  # --- MODELOS DE REGRESIÓN AVANZADOS ---
  # =====================================================================
  output$reg_y_ui <- renderUI({ req(vals$datos); selectInput("reg_y", "Variable Dependiente (Y):", choices = c("Seleccione..."="", names(dplyr::select_if(vals$datos, is.numeric)))) })
  output$reg_x_ui <- renderUI({ req(vals$datos); selectizeInput("reg_x", "Variable(s) Predictora(s) (X):", choices = names(dplyr::select_if(vals$datos, is.numeric)), multiple = TRUE) })
  
  observeEvent(input$run_reg, {
    req(vals$datos, input$reg_y, input$reg_x); if(length(input$reg_x) == 0 || input$reg_y == "") return()
    df <- vals$datos; y_var <- input$reg_y; x_vars <- input$reg_x; tipo <- input$reg_type
    df_clean <- df[complete.cases(df[, c(y_var, x_vars)]), ]
    for(v in c(y_var, x_vars)) df_clean[[v]] <- as.numeric(as.character(df_clean[[v]]))
    
    term_int <- if(input$reg_no_int) "- 1" else ""
    
    tryCatch({
      if(tipo == "lineal") { mod <- lm(as.formula(paste(y_var, "~", paste(x_vars, collapse="+"), term_int)), data=df_clean)
      } else if(tipo %in% c("poly2", "poly3")) { if(length(x_vars)>1) { showNotification("Polinomial solo admite 1 variable X.", type="warning"); return() }; g <- if(tipo=="poly2") 2 else 3; mod <- lm(as.formula(paste(y_var, "~ poly(", x_vars[1], ",", g, ", raw=TRUE)", term_int)), data=df_clean)
      } else if(tipo == "exp") { if(length(x_vars)>1) { showNotification("Exponencial solo admite 1 variable X.", type="warning"); return() }; df_clean$ln_y <- log(df_clean[[y_var]]); mod <- lm(as.formula(paste("ln_y ~", x_vars[1], term_int)), data=df_clean)
      } else if(tipo == "log") { if(length(x_vars)>1) { showNotification("Logarítmica solo admite 1 variable X.", type="warning"); return() }; df_clean$ln_x <- log(df_clean[[x_vars[1]]]); mod <- lm(as.formula(paste(y_var, "~ ln_x", term_int)), data=df_clean) }
      
      output$reg_equation <- renderPrint({
        coefs <- coef(mod)
        term_strings <- c()
        for(i in seq_along(coefs)) {
          c_name <- names(coefs)[i]; c_val <- round(coefs[i], 5); if(is.na(c_val)) next
          if(c_name == "(Intercept)") { term_strings <- c(term_strings, as.character(c_val)) } else { sign_str <- ifelse(c_val >= 0, "+", "-"); term_strings <- c(term_strings, paste(sign_str, abs(c_val), "*", c_name)) }
        }
        final_eq <- paste(term_strings, collapse = " ")
        final_eq <- sub("^ \\+ ", "", final_eq)
        if(tipo == "exp") { cat(paste0("ln(", y_var, ") = ", final_eq, "\n")) } else { cat(paste0(y_var, " = ", final_eq, "\n")) }
      })
      
      output$reg_metrics <- renderPrint({
        s <- summary(mod)
        cat(sprintf("R² Múltiple: %.4f\n", s$r.squared))
        cat(sprintf("R² Ajustado: %.4f\n", s$adj.r.squared))
        cat(sprintf("AIC: %.2f\n", AIC(mod)))
        cat(sprintf("BIC: %.2f\n", BIC(mod)))
        if(!is.null(s$fstatistic)) {
          pval_mod <- pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail=FALSE)
          cat(sprintf("P-valor del Modelo (F-test): %.4e %s\n", pval_mod, ifelse(pval_mod < 0.05, "(Significativo)", "(No Significativo)")))
        }
        if(length(x_vars) > 1 && tipo == "lineal") {
          cat("\n--- Factor de Inflación de la Varianza (VIF) ---\n")
          tryCatch({ print(car::vif(mod)) }, error = function(e) cat("No se pudo calcular VIF.\n"))
        }
      })
      
      output$reg_summary <- renderPrint({ print(summary(mod)); if(tipo == "exp") cat("\nNota: Modelo exponencial ajustado como ln(Y) ~ X.\n"); if(tipo == "log") cat("\nNota: Modelo logarítmico ajustado como Y ~ ln(X).\n") })
      
      output$reg_supuestos_txt <- renderPrint({ 
        cat("--- Pruebas Analíticas de los Residuos ---\n")
        p_shap <- tryCatch(shapiro.test(resid(mod))$p.value, error=function(e) NA)
        cat(sprintf("Normalidad (Shapiro-Wilk): p-value = %.4f %s\n", p_shap, ifelse(!is.na(p_shap) && p_shap<0.05, "[NO NORMAL]", "[NORMAL]")))
        p_bp <- tryCatch(lmtest::bptest(mod)$p.value, error=function(e) NA)
        cat(sprintf("Homocedasticidad (Breusch-Pagan): p-value = %.4f %s\n", p_bp, ifelse(!is.na(p_bp) && p_bp<0.05, "[HETEROCEDÁSTICO]", "[HOMOCEDÁSTICO]")))
        p_dw <- tryCatch(lmtest::dwtest(mod)$p.value, error=function(e) NA)
        cat(sprintf("Independencia (Durbin-Watson): p-value = %.4f\n", p_dw)) 
      })
      
      df_diag <- data.frame(Fitted=fitted(mod), Residuos=rstandard(mod))
      output$reg_res_fit_plot <- renderPlot({ ggplot(df_diag, aes(x=Fitted, y=Residuos)) + geom_point(color="blue", size=3, alpha=0.6) + geom_hline(yintercept=0, color="red", linetype="dashed", linewidth=1) + theme_classic(base_size=14) + labs(title="Homocedasticidad: Res vs Pred", x="Valores Predichos", y="Residuos Estandarizados") })
      output$reg_qq_plot <- renderPlot({ ggplot(df_diag, aes(sample=Residuos)) + stat_qq(color="blue", size=2) + stat_qq_line(color="red", linewidth=1) + theme_classic(base_size=14) + labs(title="Normalidad: QQ-Plot", x="Cuantiles Teóricos", y="Residuos Estandarizados") })
      
      output$reg_fit_plot <- renderPlot({
        if(length(x_vars) == 1) {
          form_plot <- if(input$reg_no_int) as.formula(y ~ x - 1) else as.formula(y ~ x)
          p <- ggplot(df_clean, aes_string(x=x_vars[1], y=y_var)) + geom_point(size=3, color="black", alpha=0.6) + theme_classic(base_size=15) + labs(title=paste("Ajuste de Regresión:", y_var, "vs", x_vars[1]))
          if(tipo=="lineal") p <- p + geom_smooth(method="lm", formula=form_plot, color="red", se=TRUE)
          else if(tipo=="poly2") p <- p + geom_smooth(method="lm", formula=if(input$reg_no_int) y~poly(x,2,raw=T)-1 else y~poly(x,2,raw=T), color="red", se=TRUE)
          else if(tipo=="poly3") p <- p + geom_smooth(method="lm", formula=if(input$reg_no_int) y~poly(x,3,raw=T)-1 else y~poly(x,3,raw=T), color="red", se=TRUE)
          else if(tipo=="exp") p <- p + geom_smooth(method="nls", formula=y~exp(a+b*x), method.args=list(start=c(a=0,b=0)), color="red", se=FALSE)
          else if(tipo=="log") p <- p + geom_smooth(method="lm", formula=if(input$reg_no_int) y~log(x)-1 else y~log(x), color="red", se=TRUE)
          vals$p_reg_fit <- p; print(p)
        } else {
          p <- ggplot(df_diag, aes(x=Fitted, y=df_clean[[y_var]])) + geom_point(size=3, color="black", alpha=0.6) + geom_abline(slope=1, intercept=0, color="red", linetype="dashed", linewidth=1) + theme_classic(base_size=15) + labs(title="Regresión Múltiple: Observados vs Predichos", x="Valores Predichos por el Modelo", y="Valores Observados Reales")
          vals$p_reg_fit <- p; print(p)
        }
      })
    }, error = function(e) { showNotification(paste("Error en modelo:", e$message), type="error") })
  })
  
  # =====================================================================
  # --- DISEÑOS EXPERIMENTALES: EXPERIMENTOS SIMPLES ---
  # =====================================================================
  output$sim_vars_ui <- renderUI({ req(vals$datos); vars <- names(vals$datos); tagList(selectizeInput("sim_y", "Variables Respuesta (Y):", choices = names(dplyr::select_if(vals$datos, is.numeric)), multiple = TRUE), selectInput("sim_trt", "Tratamiento (Factor):", choices = c("Seleccione..."="", vars)), selectizeInput("sim_cov", "Covariable(s) (Opcional):", choices = names(dplyr::select_if(vals$datos, is.numeric)), multiple = TRUE), conditionalPanel("input.sim_des == 'DBCA'", selectInput("sim_blk", "Bloque / Repetición:", choices = c("Seleccione..."="", vars))), conditionalPanel("input.sim_des == 'DCL'", selectInput("sim_row", "Fila:", choices = c("Seleccione..."="", vars)), selectInput("sim_col", "Columna:", choices = c("Seleccione..."="", vars))) ) })
  
  observeEvent(input$run_sim, {
    req(vals$datos, input$sim_y, input$sim_trt); if(length(input$sim_y) == 0 || input$sim_trt == "") { showNotification("Seleccione variables.", type="warning"); return() }
    df <- vals$datos; trt_var <- input$sim_trt; covs <- input$sim_cov; df[[trt_var]] <- as.factor(df[[trt_var]])
    if(input$sim_des == "DBCA") df[[input$sim_blk]] <- as.factor(df[[input$sim_blk]]); if(input$sim_des == "DCL") { df[[input$sim_row]] <- as.factor(df[[input$sim_row]]); df[[input$sim_col]] <- as.factor(df[[input$sim_col]]) }
    if(length(covs) > 0) { for(cv in covs) df[[cv]] <- as.numeric(as.character(df[[cv]])) }
    cov_str <- if(length(covs) > 0) paste(paste(covs, collapse=" + "), "+ ") else ""
    
    output$sim_anova <- renderPrint({ 
      txt_out <- ""
      for(y_var in input$sim_y) {
        vars_to_keep <- c(y_var, trt_var, covs); if(input$sim_des == "DBCA") vars_to_keep <- c(vars_to_keep, input$sim_blk); if(input$sim_des == "DCL") vars_to_keep <- c(vars_to_keep, input$sim_row, input$sim_col)
        df_clean <- df[complete.cases(df[, vars_to_keep]), ]; df_clean[[y_var]] <- as.numeric(as.character(df_clean[[y_var]])); df_clean <- droplevels(df_clean); if(nrow(df_clean) < 2) next
        form_str <- switch(input$sim_des, "DCA" = paste(y_var, "~", cov_str, trt_var), "DBCA" = paste(y_var, "~", cov_str, trt_var, "+", input$sim_blk), "DCL" = paste(y_var, "~", cov_str, trt_var, "+", input$sim_row, "+", input$sim_col))
        modelo <- aov(as.formula(form_str), data = df_clean)
        txt_out <- paste0(txt_out, print_infostat_anova(modelo, y_var, df_clean))
      }
      cat(txt_out)
    })
    
    output$sim_supuestos <- renderPrint({
      txt_sup <- ""
      for(y_var in input$sim_y) {
        vars_to_keep <- c(y_var, trt_var, covs); if(input$sim_des == "DBCA") vars_to_keep <- c(vars_to_keep, input$sim_blk); if(input$sim_des == "DCL") vars_to_keep <- c(vars_to_keep, input$sim_row, input$sim_col)
        df_clean <- df[complete.cases(df[, vars_to_keep]), ]; df_clean[[y_var]] <- as.numeric(as.character(df_clean[[y_var]])); df_clean <- droplevels(df_clean); if(nrow(df_clean) < 2) next
        form_str <- switch(input$sim_des, "DCA" = paste(y_var, "~", cov_str, trt_var), "DBCA" = paste(y_var, "~", cov_str, trt_var, "+", input$sim_blk), "DCL" = paste(y_var, "~", cov_str, trt_var, "+", input$sim_row, "+", input$sim_col))
        modelo <- aov(as.formula(form_str), data = df_clean)
        txt_sup <- paste0(txt_sup, "\n================ SUPUESTOS PARA: ", toupper(y_var), " ================\n")
        p_shap <- tryCatch(shapiro.test(resid(modelo))$p.value, error=function(e) NA)
        txt_sup <- paste0(txt_sup, sprintf("- Normalidad (Shapiro-Wilk): p-value = %.4f %s\n", p_shap, ifelse(!is.na(p_shap) && p_shap < 0.05, "(No Normal)", "(Normal)")))
        p_bart <- tryCatch(bartlett.test(as.formula(paste(y_var, "~", trt_var)), data = df_clean)$p.value, error=function(e) NA)
        txt_sup <- paste0(txt_sup, sprintf("- Homocedasticidad (Bartlett): p-value = %.4f %s\n", p_bart, ifelse(!is.na(p_bart) && p_bart < 0.05, "(Heterocedástico)", "(Homocedástico)")))
        p_lev <- tryCatch(car::leveneTest(as.formula(paste(y_var, "~", trt_var)), data = df_clean, center = "median")$`Pr(>F)`[1], error=function(e) NA)
        txt_sup <- paste0(txt_sup, sprintf("- Homocedasticidad Robusta (Levene): p-value = %.4f\n", p_lev))
        p_dw <- tryCatch(lmtest::dwtest(modelo)$p.value, error=function(e) NA)
        txt_sup <- paste0(txt_sup, sprintf("- Independencia (Durbin-Watson): p-value = %.4f\n", p_dw))
      }
      cat(txt_sup)
    })
    
    output$sim_pmm_out <- renderPrint({
      txt_pmm <- ""
      alpha_val <- input$sim_alpha; test_name <- input$sim_pmm
      for(y_var in input$sim_y) {
        vars_to_keep <- c(y_var, trt_var, covs); if(input$sim_des == "DBCA") vars_to_keep <- c(vars_to_keep, input$sim_blk); if(input$sim_des == "DCL") vars_to_keep <- c(vars_to_keep, input$sim_row, input$sim_col)
        df_clean <- df[complete.cases(df[, vars_to_keep]), ]; df_clean[[y_var]] <- as.numeric(as.character(df_clean[[y_var]])); df_clean <- droplevels(df_clean); if(nrow(df_clean) < 2) next
        form_str <- switch(input$sim_des, "DCA" = paste(y_var, "~", cov_str, trt_var), "DBCA" = paste(y_var, "~", cov_str, trt_var, "+", input$sim_blk), "DCL" = paste(y_var, "~", cov_str, trt_var, "+", input$sim_row, "+", input$sim_col))
        modelo <- aov(as.formula(form_str), data = df_clean)
        txt_pmm <- paste0(txt_pmm, "\n================ PRUEBA DE MEDIAS PARA: ", toupper(y_var), " ================\n")
        txt_pmm <- paste0(txt_pmm, "Prueba: ", test_name, " | Alpha: ", alpha_val, "\n\n")
        tryCatch({ if(test_name == "Tukey") { out <- capture.output(print(HSD.test(modelo, trt_var, alpha = alpha_val, console = TRUE))); txt_pmm <- paste0(txt_pmm, paste(out[-(1:10)], collapse="\n"), "\n") } else if(test_name == "Fisher (LSD)") { out <- capture.output(print(LSD.test(modelo, trt_var, p.adj = "none", alpha = alpha_val, console = TRUE))); txt_pmm <- paste0(txt_pmm, paste(out[-(1:10)], collapse="\n"), "\n") } else if(test_name == "Duncan") { out <- capture.output(print(duncan.test(modelo, trt_var, alpha = alpha_val, console = TRUE))); txt_pmm <- paste0(txt_pmm, paste(out[-(1:10)], collapse="\n"), "\n") } else if(test_name == "SNK") { out <- capture.output(print(SNK.test(modelo, trt_var, alpha = alpha_val, console = TRUE))); txt_pmm <- paste0(txt_pmm, paste(out[-(1:10)], collapse="\n"), "\n") } else if(test_name == "Bonferroni") { out <- capture.output(print(LSD.test(modelo, trt_var, p.adj = "bonferroni", alpha = alpha_val, console = TRUE))); txt_pmm <- paste0(txt_pmm, paste(out[-(1:10)], collapse="\n"), "\n") } else if(test_name == "Scott-Knott") { sk <- SK(modelo, which = trt_var, sig.level = alpha_val, dispersion="se"); txt_pmm <- paste0(txt_pmm, paste(capture.output(summary(sk)), collapse="\n"), "\n") } }, error = function(e) txt_pmm <- paste0(txt_pmm, "Fallo en PMM: ", e$message, "\n"))
      }
      cat(txt_pmm)
    })
    
    lista_supuestos <- list(); lista_graficos_pmm <- list()
    for(y_var in input$sim_y) {
      local({
        y_v <- y_var
        vars_to_keep <- c(y_v, trt_var, covs); if(input$sim_des == "DBCA") vars_to_keep <- c(vars_to_keep, input$sim_blk); if(input$sim_des == "DCL") vars_to_keep <- c(vars_to_keep, input$sim_row, input$sim_col)
        df_clean <- df[complete.cases(df[, vars_to_keep]), ]; df_clean[[y_v]] <- as.numeric(as.character(df_clean[[y_v]])); df_clean <- droplevels(df_clean); if(nrow(df_clean) < 2) return()
        form_str <- switch(input$sim_des, "DCA" = paste(y_v, "~", cov_str, trt_var), "DBCA" = paste(y_v, "~", cov_str, trt_var, "+", input$sim_blk), "DCL" = paste(y_v, "~", cov_str, trt_var, "+", input$sim_row, "+", input$sim_col))
        modelo <- aov(as.formula(form_str), data = df_clean)
        
        df_diag <- data.frame(Fitted = fitted(modelo), Residuos = rstandard(modelo))
        lista_supuestos[[paste0(y_v, "_res")]] <<- ggplot(df_diag, aes(x=Fitted, y=Residuos)) + geom_point(color="blue", size=2) + geom_hline(yintercept=0, color="red", linetype="dashed", linewidth=1) + theme_classic() + labs(title=paste("Residuos:", y_v))
        lista_supuestos[[paste0(y_v, "_qq")]] <<- ggplot(df_diag, aes(sample=Residuos)) + stat_qq(color="blue") + stat_qq_line(color="red", linewidth=1) + theme_classic() + labs(title=paste("QQ:", y_v))
        
        if(input$sim_pmm %in% c("Tukey", "Fisher (LSD)", "Scott-Knott")) {
          alpha_val <- input$sim_alpha
          if(input$sim_pmm == "Tukey") { pmm_res <- HSD.test(modelo, trt_var, alpha = alpha_val); tabla_letras <- data.frame(Tratamiento = as.character(rownames(pmm_res$groups)), Letra = as.character(pmm_res$groups$groups), stringsAsFactors = FALSE) } else if(input$sim_pmm == "Fisher (LSD)") { pmm_res <- LSD.test(modelo, trt_var, p.adj = "none", alpha = alpha_val); tabla_letras <- data.frame(Tratamiento = as.character(rownames(pmm_res$groups)), Letra = as.character(pmm_res$groups$groups), stringsAsFactors = FALSE) } else { sk_res <- SK(modelo, which = trt_var, sig.level = alpha_val, dispersion="se"); tabla_letras <- data.frame(Tratamiento = as.character(rownames(sk_res$groups)), Letra = as.character(sk_res$groups$Groups), stringsAsFactors = FALSE) }
          group_cols <- trt_var; facet_var <- NULL; if(input$sim_facet && input$sim_des %in% c("DBCA", "DCL")) { facet_var <- if(input$sim_des == "DBCA") input$sim_blk else input$sim_row; group_cols <- c(trt_var, facet_var) }
          df_stats <- df_clean %>% group_by(across(dplyr::all_of(group_cols))) %>% summarise(media_r = mean(.data[[y_v]], na.rm = TRUE), n_val = n(), sd_val = sd(.data[[y_v]], na.rm = TRUE), se_val = sd_val / sqrt(n_val), .groups = "drop") %>% mutate(disp = case_when(input$sim_plot_err == "se" ~ se_val, input$sim_plot_err == "sd" ~ sd_val, input$sim_plot_err == "ic" ~ se_val * qt(0.975, df = ifelse(n_val > 1, n_val - 1, 1))))
          df_stats[[trt_var]] <- as.character(df_stats[[trt_var]]); datos_plot <- df_stats %>% left_join(tabla_letras, by = setNames("Tratamiento", trt_var))
          gg <- ggplot(datos_plot, aes_string(y = paste0("reorder(", trt_var, ", media_r)"), x = "media_r")) + geom_errorbarh(aes(xmin = media_r - disp, xmax = media_r + disp), height = 0.2) + geom_point(size = 3, color = "black") + geom_text(aes(label = sprintf("%.2f %s", media_r, trimws(Letra))), hjust = -0.1, vjust = -1.1, size = 4.2) + scale_x_continuous(expand = expansion(mult = c(0.1, 0.5))) + labs(x = paste("Media Real ±", toupper(input$sim_plot_err)), y = "Tratamiento", title = paste("Medias:", y_v)) + theme_classic(base_size = 14) + theme(strip.text = element_text(size = 13, face = "bold"), strip.background = element_blank())
          if(!is.null(facet_var)) gg <- gg + facet_wrap(as.formula(paste("~", facet_var)), scales = "free_x", ncol = input$sim_facet_ncol)
          lista_graficos_pmm[[y_v]] <<- gg
        }
      })
    }
    if(length(lista_supuestos) > 0) vals$p_sim_sup <- patchwork::wrap_plots(lista_supuestos, ncol = 2)
    if(length(lista_graficos_pmm) > 0) vals$p_sim_pmm <- patchwork::wrap_plots(lista_graficos_pmm, ncol = if(length(lista_graficos_pmm)>1) 2 else 1)
  })
  
  output$sim_plots <- renderPlot({ req(vals$p_sim_sup); vals$p_sim_sup })
  output$sim_pmm_plot <- renderPlot({ req(vals$p_sim_pmm); vals$p_sim_pmm })
  observeEvent(input$btn_pop_sim_sup, { req(vals$p_sim_sup); if(interactive()){ dev.new(noRStudioGD=T); print(vals$p_sim_sup) } })
  observeEvent(input$btn_pop_sim_pmm, { req(vals$p_sim_pmm); if(interactive()){ dev.new(noRStudioGD=T); print(vals$p_sim_pmm) } })
  
  # --- TABLA RESUMEN SIMPLES (AGRUPACIÓN RELATIVOS IZQUIERDA) ---
  output$sim_resumen_vars_ui <- renderUI({ req(input$sim_y); selectizeInput("sim_resumen_vars", "Variables a incluir:", choices = input$sim_y, multiple = TRUE, selected = input$sim_y) })
  output$sim_resumen_testigo_ui <- renderUI({ req(vals$datos, input$sim_trt); if(input$sim_trt == "") return(NULL); selectInput("sim_resumen_testigo", "Testigo (Relativo = 1):", choices = unique(as.character(vals$datos[[input$sim_trt]]))) })
  
  sim_resumen_data <- eventReactive(input$btn_gen_sim_resumen, {
    req(vals$datos, input$sim_trt, input$sim_resumen_vars, input$sim_resumen_testigo)
    df <- vals$datos; trt_var <- input$sim_trt; vars_analizar <- input$sim_resumen_vars; testigo <- input$sim_resumen_testigo; alpha_val <- input$sim_alpha; covs <- input$sim_cov
    cov_str <- if(length(covs) > 0) paste(paste(covs, collapse=" + "), "+ ") else ""
    df_resumen <- data.frame(Tratamiento = unique(as.character(df[[trt_var]]))); p_valores <- c("P-valor (Tratamiento)")
    for(v in vars_analizar) {
      vars_to_keep <- c(v, trt_var, covs); if(input$sim_des == "DBCA") vars_to_keep <- c(vars_to_keep, input$sim_blk); if(input$sim_des == "DCL") vars_to_keep <- c(vars_to_keep, input$sim_row, input$sim_col)
      df_clean <- df[complete.cases(df[, vars_to_keep]), ]; df_clean[[trt_var]] <- as.factor(df_clean[[trt_var]])
      form_str <- switch(input$sim_des, "DCA" = paste(v, "~", cov_str, trt_var), "DBCA" = paste(v, "~", cov_str, trt_var, "+", input$sim_blk), "DCL" = paste(v, "~", cov_str, trt_var, "+", input$sim_row, "+", input$sim_col))
      mod_temp <- aov(as.formula(form_str), data = df_clean)
      res_anova <- summary(mod_temp)[[1]]; p_val <- res_anova[trt_var, "Pr(>F)"]; p_valores <- c(p_valores, ifelse(p_val < 0.001, "<0.001", sprintf("%.4f", p_val)))
      if(input$sim_pmm == "Tukey") { pmm_res <- HSD.test(mod_temp, trt_var, alpha = alpha_val); tabla_letras <- data.frame(Tratamiento = rownames(pmm_res$groups), Letra = pmm_res$groups$groups) } else if(input$sim_pmm == "Fisher (LSD)") { pmm_res <- LSD.test(mod_temp, trt_var, p.adj = "none", alpha = alpha_val); tabla_letras <- data.frame(Tratamiento = rownames(pmm_res$groups), Letra = pmm_res$groups$groups) } else { sk_res <- SK(mod_temp, which = trt_var, sig.level = alpha_val, dispersion="se"); tabla_letras <- data.frame(Tratamiento = rownames(sk_res$groups), Letra = sk_res$groups$Groups) }
      medias_var <- df_clean %>% group_by(.data[[trt_var]]) %>% summarise(Media = mean(.data[[v]], na.rm=TRUE), .groups="drop"); medias_var[[trt_var]] <- as.character(medias_var[[trt_var]])
      medias_var <- medias_var %>% left_join(tabla_letras, by = setNames("Tratamiento", trt_var))
      media_testigo <- medias_var$Media[medias_var[[trt_var]] == testigo]; if(length(media_testigo) == 0 || is.na(media_testigo) || media_testigo == 0) { media_testigo <- 1 }
      medias_var <- medias_var %>% mutate(Texto_Combinado = sprintf("%.2f %s", Media, trimws(Letra)), Relativo = Media / media_testigo)
      df_resumen <- df_resumen %>% left_join(medias_var %>% dplyr::select(dplyr::all_of(trt_var), Texto_Combinado, Relativo), by = setNames(trt_var, "Tratamiento"))
      colnames(df_resumen)[ncol(df_resumen)-1] <- v; colnames(df_resumen)[ncol(df_resumen)] <- paste(v, "Rel")
    }
    
    cols_relativas <- grep("Rel$", colnames(df_resumen), value = TRUE)
    cols_restantes <- setdiff(colnames(df_resumen), c("Tratamiento", cols_relativas))
    df_resumen <- df_resumen %>% dplyr::select(Tratamiento, dplyr::all_of(cols_relativas), dplyr::all_of(cols_restantes))
    df_resumen <- df_resumen[order(df_resumen$Tratamiento), ]; df_resumen <- data.frame(`No Trat` = paste0("T", 1:nrow(df_resumen)), df_resumen, check.names = FALSE)
    
    fila_p <- c("", "P-valor (Trt)")
    for (col in colnames(df_resumen)[-c(1,2)]) { if(col %in% vars_analizar) { fila_p <- c(fila_p, p_valores[which(vars_analizar == col) + 1]) } else { fila_p <- c(fila_p, "") } }
    
    df_resumen <- df_resumen %>% mutate(across(everything(), as.character)); df_final <- rbind(df_resumen, fila_p); return(df_final)
  })
  output$sim_tabla_resumen <- renderDT({ req(sim_resumen_data()); datatable(sim_resumen_data(), extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('copy', 'excel'), pageLength = 15), rownames = FALSE, class = "cell-border stripe") })
  
  # =====================================================================
  # --- DISEÑOS EXPERIMENTALES: FACTORIALES ---
  # =====================================================================
  output$fac_vars_ui <- renderUI({ 
    req(vals$datos); vars <- names(vals$datos)
    tagList(
      selectizeInput("fac_y", "Variables Respuesta (Y):", choices = names(dplyr::select_if(vals$datos, is.numeric)), multiple = TRUE), 
      selectInput("fac_A", "Factor A (F1):", choices = c("Seleccione..."="", vars)), 
      selectInput("fac_B", "Factor B (F2):", choices = c("Seleccione..."="", vars)), 
      conditionalPanel("input.fac_arr == 'Parcelas Subdivididas' || input.fac_arr == 'Franjas Divididas' || input.fac_arr == 'Factorial Combinatorio'", selectInput("fac_C", "Factor C (F3 - Opcional):", choices = c("Ninguno"="", vars))), 
      selectizeInput("fac_cov", "Covariable(s) (Opcional):", choices = names(dplyr::select_if(vals$datos, is.numeric)), multiple = TRUE), 
      # FIX: Mostrar bloque si es DBCA o si es CUALQUIER arreglo de parcelas divididas/franjas
      conditionalPanel("input.fac_des == 'DBCA' || input.fac_arr != 'Factorial Combinatorio'", selectInput("fac_blk", "Bloque / Repetición:", choices = c("Seleccione..."="", vars))), 
      conditionalPanel("input.fac_des == 'DCL'", selectInput("fac_row", "Fila:", choices = c("Seleccione..."="", vars)), selectInput("fac_col", "Columna:", choices = c("Seleccione..."="", vars))) 
    ) 
  })
  
  output$fac_pmm_terms_ui <- renderUI({ req(input$fac_A, input$fac_B); if(input$fac_A == "" || input$fac_B == "") return(NULL); req_C <- input$fac_arr %in% c("Parcelas Subdivididas", "Franjas Divididas", "Factorial Combinatorio"); if(req_C && !is.null(input$fac_C) && input$fac_C != "") { choices <- c(input$fac_A, input$fac_B, input$fac_C, paste(input$fac_A, input$fac_B, sep=":"), paste(input$fac_A, input$fac_C, sep=":"), paste(input$fac_B, input$fac_C, sep=":"), paste(input$fac_A, input$fac_B, input$fac_C, sep=":")) } else { choices <- c(input$fac_A, input$fac_B, paste(input$fac_A, input$fac_B, sep=":")) }; checkboxGroupInput("fac_pmm_terms", "Efectos a evaluar en PMM Global:", choices = choices, selected = choices[1]) })
  output$fac_plot_options_ui <- renderUI({ req(input$fac_pmm %in% c("Tukey", "Fisher (LSD)", "Scott-Knott"), input$fac_pmm_terms); fac_choices <- c(); if(input$fac_des == "DBCA") fac_choices <- input$fac_blk; if(input$fac_des == "DCL") fac_choices <- c(input$fac_row, input$fac_col); tagList(hr(), h4("Configuración de Gráficos PMM Global"), selectInput("fac_plot_term", "Efecto a Graficar:", choices = input$fac_pmm_terms), selectInput("fac_plot_err", "Error en barras:", choices = c("SE"="se", "SD"="sd", "IC 95%"="ic")), if(length(fac_choices) > 0) { tagList(checkboxInput("fac_facet", "Facetar por Bloque/Fila", value=FALSE), conditionalPanel("input.fac_facet == true", selectInput("fac_facet_var", "Faceta:", choices = fac_choices), numericInput("fac_facet_ncol", "Columnas:", value=2, min=1))) }) })
  
  observeEvent(input$run_fac, {
    req(vals$datos, input$fac_y, input$fac_A, input$fac_B); if(length(input$fac_y) == 0 || input$fac_A == "" || input$fac_B == "") { showNotification("Seleccione variables.", type="warning"); return() }
    df <- vals$datos; A <- input$fac_A; B <- input$fac_B; des <- input$fac_des; arr <- input$fac_arr; covs <- input$fac_cov
    df[[A]] <- as.factor(df[[A]]); df[[B]] <- as.factor(df[[B]])
    usar_C <- arr %in% c("Parcelas Subdivididas", "Franjas Divididas", "Factorial Combinatorio") && !is.null(input$fac_C) && input$fac_C != ""
    if(usar_C) df[[input$fac_C]] <- as.factor(df[[input$fac_C]])
    
    # FIX: Identificar si se usa bloque/repetición y convertirlo a factor
    usar_blk <- des == "DBCA" || arr != "Factorial Combinatorio"
    if(usar_blk && !is.null(input$fac_blk) && input$fac_blk != "") df[[input$fac_blk]] <- as.factor(df[[input$fac_blk]])
    if(des == "DCL") { df[[input$fac_row]] <- as.factor(df[[input$fac_row]]); df[[input$fac_col]] <- as.factor(df[[input$fac_col]]) }
    if(length(covs) > 0) { for(cv in covs) df[[cv]] <- as.numeric(as.character(df[[cv]])) }
    
    base_str <- if(des=="DBCA") paste0(input$fac_blk, " + ") else if(des=="DCL") paste0(input$fac_row, " + ", input$fac_col, " + ") else ""
    cov_str <- if(length(covs) > 0) paste(paste(covs, collapse=" + "), "+ ") else ""
    
    output$fac_anova <- renderPrint({ 
      txt_out <- ""
      for(y_var in input$fac_y) {
        # FIX: Incluir el bloque en la limpieza de NAs si se está usando
        vars_to_keep <- c(y_var, A, B, covs); if(usar_C) vars_to_keep <- c(vars_to_keep, input$fac_C); if(usar_blk) vars_to_keep <- c(vars_to_keep, input$fac_blk); if(des == "DCL") vars_to_keep <- c(vars_to_keep, input$fac_row, input$fac_col)
        df_clean <- df[complete.cases(df[, vars_to_keep]), ]; df_clean[[y_var]] <- as.numeric(as.character(df_clean[[y_var]])); df_clean <- droplevels(df_clean); if(nrow(df_clean) < 2) next
        
        # FIX: Fórmulas de Parcelas Divididas corregidas (Reemplazo de 'Rep' por input$fac_blk)
        if(arr == "Factorial Combinatorio") { if(usar_C) { form_anova <- paste(y_var, "~", cov_str, base_str, A, "*", B, "*", input$fac_C) } else { form_anova <- paste(y_var, "~", cov_str, base_str, A, "*", B) } 
        } else if (arr == "Parcelas Divididas") { form_anova <- paste(y_var, "~", cov_str, base_str, A, "+", if(des=="DBCA") paste0("Error(", input$fac_blk, "/", A, ")") else paste0("Error(", input$fac_blk, ":", A, ")"), "+", B, "+", A, ":", B) 
        } else if (arr == "Parcelas Subdivididas") { form_anova <- paste(y_var, "~", cov_str, base_str, A, "*", B, "*", input$fac_C, "+ Error(", input$fac_blk, "/", A, "/", B, ")") 
        } else if (arr == "Franjas") { form_anova <- paste(y_var, "~", cov_str, base_str, A, "*", B, "+ Error(", input$fac_blk, "/(", A, "+", B, "))") 
        } else if (arr == "Franjas Divididas") { form_anova <- paste(y_var, "~", cov_str, A, "*", B, "*", input$fac_C, "+ Error(", input$fac_blk, "/(", A, "*", B, "))") }
        
        modelo_anova <- aov(as.formula(form_anova), data = df_clean)
        txt_out <- paste0(txt_out, print_infostat_anova(modelo_anova, y_var, df_clean))
      }
      cat(txt_out)
    })
    
    # ================= SUPUESTOS FACTORIALES =================
    output$fac_supuestos <- renderPrint({
      for(y_var in input$fac_y) {
        vars_to_keep <- c(y_var, A, B, covs); if(usar_C) vars_to_keep <- c(vars_to_keep, input$fac_C); if(usar_blk) vars_to_keep <- c(vars_to_keep, input$fac_blk); if(des == "DCL") vars_to_keep <- c(vars_to_keep, input$fac_row, input$fac_col)
        df_clean <- df[complete.cases(df[, vars_to_keep]), ]; df_clean[[y_var]] <- as.numeric(as.character(df_clean[[y_var]])); df_clean <- droplevels(df_clean); if(nrow(df_clean) < 2) next
        form_fix <- if(usar_C) paste(y_var, "~", cov_str, base_str, A, "*", B, "*", input$fac_C) else paste(y_var, "~", cov_str, base_str, A, "*", B)
        modelo_fix <- aov(as.formula(form_fix), data = df_clean)
        
        cat("===================================================\n")
        cat("   PRUEBAS FORMALES DE SUPUESTOS PARA:", toupper(y_var), "\n")
        cat("===================================================\n\n")
        
        residuos <- tryCatch(resid(modelo_fix), error = function(e) NULL)
        if(!is.null(residuos)) {
          # 1. NORMALIDAD
          shap <- tryCatch(shapiro.test(residuos), error=function(e) NULL)
          if(!is.null(shap)) { cat(sprintf("1. NORMALIDAD (Shapiro-Wilk):\n   W = %.4f, P-valor = %.4f\n   --> %s\n\n", shap$statistic, shap$p.value, ifelse(shap$p.value < 0.05, "[ALERTA] No Normal.", "[OK] Distribución Normal."))) }
          # 2. HOMOCEDASTICIDAD (Levene)
          lev <- tryCatch(car::leveneTest(modelo_fix), error=function(e) NULL)
          if(!is.null(lev)) { cat(sprintf("2. HOMOCEDASTICIDAD ROBUSTA (Levene):\n   F = %.4f, P-valor = %.4f\n   --> %s\n\n", lev$`F value`[1], lev$`Pr(>F)`[1], ifelse(lev$`Pr(>F)`[1] < 0.05, "[ALERTA] Varianzas Heterogéneas.", "[OK] Varianzas Homogéneas."))) }
          # 3. HOMOCEDASTICIDAD (Bartlett)
          tryCatch({
            y_data <- modelo_fix$model[[1]]; factores_modelo <- modelo_fix$model[, -1, drop = FALSE]; grupos_comb <- apply(factores_modelo, 1, paste, collapse = "-")
            bart <- bartlett.test(y_data ~ as.factor(grupos_comb))
            cat(sprintf("3. HOMOCEDASTICIDAD PARAMÉTRICA (Bartlett):\n   K-cuadrado = %.4f, P-valor = %.4f\n   --> %s\n\n", bart$statistic, bart$p.value, ifelse(bart$p.value < 0.05, "[ALERTA] Varianzas Heterogéneas.", "[OK] Varianzas Homogéneas.")))
          }, error = function(e) { })
          # 4. INDEPENDENCIA (Durbin-Watson)
          dw <- tryCatch(car::durbinWatsonTest(modelo_fix), error=function(e) NULL)
          if(!is.null(dw)) { cat(sprintf("4. INDEPENDENCIA (Durbin-Watson):\n   DW = %.4f, P-valor = %.4f\n   --> %s\n\n", dw$dw, dw$p, ifelse(dw$p < 0.05, "[ALERTA] Autocorrelación detectada.", "[OK] Residuos Independientes."))) }
        }
      }
    })
    
    output$fac_pmm_out <- renderPrint({
      txt_pmm <- ""
      alpha_val <- input$fac_alpha; test_name <- input$fac_pmm
      for(y_var in input$fac_y) {
        vars_to_keep <- c(y_var, A, B, covs); if(usar_C) vars_to_keep <- c(vars_to_keep, input$fac_C); if(usar_blk) vars_to_keep <- c(vars_to_keep, input$fac_blk); if(des == "DCL") vars_to_keep <- c(vars_to_keep, input$fac_row, input$fac_col)
        df_clean <- df[complete.cases(df[, vars_to_keep]), ]; df_clean[[y_var]] <- as.numeric(as.character(df_clean[[y_var]])); df_clean <- droplevels(df_clean); if(nrow(df_clean) < 2) next
        form_fix <- if(usar_C) paste(y_var, "~", cov_str, base_str, A, "*", B, "*", input$fac_C) else paste(y_var, "~", cov_str, base_str, A, "*", B)
        modelo_fix <- aov(as.formula(form_fix), data = df_clean)
        txt_pmm <- paste0(txt_pmm, "\n================ PRUEBA DE MEDIAS GLOBAL: ", toupper(y_var), " ================\n")
        txt_pmm <- paste0(txt_pmm, "Prueba: ", test_name, " | Alpha: ", alpha_val, "\n\n")
        for(term in input$fac_pmm_terms) {
          txt_pmm <- paste0(txt_pmm, "\n => TÉRMINO: ", term, "\n")
          term_eval <- unlist(strsplit(term, ":"))
          tryCatch({
            if(test_name == "Scott-Knott") { if(length(term_eval) > 1) { df_clean$SK_INT <- interaction(df_clean[, term_eval], sep=":"); mod_sk <- aov(as.formula(paste(y_var, "~", base_str, "SK_INT")), data=df_clean); sk <- SK(mod_sk, which = "SK_INT", sig.level = alpha_val, dispersion = "se"); txt_pmm <- paste0(txt_pmm, paste(capture.output(summary(sk)), collapse="\n"), "\n") } else { sk <- SK(modelo_fix, which = term, sig.level = alpha_val, dispersion = "se"); txt_pmm <- paste0(txt_pmm, paste(capture.output(summary(sk)), collapse="\n"), "\n") }
            } else { if(test_name == "Tukey") { out<-capture.output(print(HSD.test(modelo_fix, term_eval, alpha=alpha_val, console=T))); txt_pmm <- paste0(txt_pmm, paste(out[-(1:10)], collapse="\n"), "\n") } else if(test_name == "Fisher (LSD)") { out<-capture.output(print(LSD.test(modelo_fix, term_eval, p.adj="none", alpha=alpha_val, console=T))); txt_pmm <- paste0(txt_pmm, paste(out[-(1:10)], collapse="\n"), "\n") } else if(test_name == "Duncan") { out <- capture.output(print(duncan.test(modelo_fix, term_eval, alpha=alpha_val, console=T))); txt_pmm <- paste0(txt_pmm, paste(out[-(1:10)], collapse="\n"), "\n") } else if(test_name == "SNK") { out <- capture.output(print(SNK.test(modelo_fix, term_eval, alpha=alpha_val, console=T))); txt_pmm <- paste0(txt_pmm, paste(out[-(1:10)], collapse="\n"), "\n") } else if(test_name == "Bonferroni") { out <- capture.output(print(LSD.test(modelo_fix, term_eval, p.adj="bonferroni", alpha=alpha_val, console=T))); txt_pmm <- paste0(txt_pmm, paste(out[-(1:10)], collapse="\n"), "\n") } }
          }, error = function(e) txt_pmm <- paste0(txt_pmm, "Fallo en PMM: ", e$message, "\n"))
        }
      }
      cat(txt_pmm)
    })
    
    # Desdoblamientos Factoriales
    output$fac_desdob_txt <- renderPrint({
      alpha_val <- input$fac_alpha; test_name <- input$fac_pmm
      factores <- if(usar_C) c(A, B, input$fac_C) else c(A, B)
      for(y_var in input$fac_y) {
        local({
          y_v <- y_var
          vars_to_keep <- c(y_v, factores, covs); if(usar_blk) vars_to_keep <- c(vars_to_keep, input$fac_blk); if(des == "DCL") vars_to_keep <- c(vars_to_keep, input$fac_row, input$fac_col)
          df_clean <- df[complete.cases(df[, vars_to_keep]), ]; df_clean[[y_v]] <- as.numeric(as.character(df_clean[[y_v]])); df_clean <- droplevels(df_clean); if(nrow(df_clean) < 2) return()
          cat(paste("\n======================================================\nANALIZANDO DESDOBLAMIENTOS:", toupper(y_v), "\n======================================================\n"))
          form_int <- as.formula(paste(y_v, "~", paste(factores, collapse = "*")))
          m0 <- do.call("lm", list(formula = form_int, data = quote(df_clean)))
          f1 <- factores[1]; otros_f <- factores[2:length(factores)]
          formula_emm <- as.formula(paste("~", f1, "|", paste(otros_f, collapse = "*")))
          cat(paste("\n--- Desdoblamiento:", f1, "anidado en", paste(otros_f, collapse = " y "), "---\n"))
          tryCatch({
            emm_res <- emmeans::emmeans(m0, formula_emm, data = df_clean)
            desdob_cld <- multcomp::cld(emm_res, alpha = alpha_val, Letters = letters, adjust = ifelse(test_name == "Tukey", "tukey", "none"), reversed = TRUE)
            print(desdob_cld)
            df_ani <- as.data.frame(desdob_cld)
            p_ani <- ggplot(df_ani, aes_string(y = paste0("reorder(", f1, ", emmean)"), x = "emmean")) + geom_errorbarh(aes(xmin = emmean - SE, xmax = emmean + SE), height = 0.2) + geom_point(size = 3, color = "darkred") + geom_text(aes(label = sprintf("%.2f %s", emmean, trimws(.group))), hjust = -0.2, vjust = -1, size = 3.5) + scale_x_continuous(expand = expansion(mult = c(0.1, 0.5))) + labs(title = paste("Desdoblamiento:", y_v), subtitle = paste(f1, "anidado en", paste(otros_f, collapse = " y ")), x = "Media Ajustada ± SE", y = f1) + theme_bw() + theme(strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 10))
            if(length(factores) == 3) { p_ani <- p_ani + facet_grid(as.formula(paste(factores[2], "~", factores[3])), labeller = label_both) } else { p_ani <- p_ani + facet_wrap(as.formula(paste("~", factores[2])), scales = "free_x") }
            plot_efecto <- function(f_nom) {
              res_tk <- HSD.test(aov(as.formula(paste(y_v, "~", f_nom)), data = df_clean), f_nom, alpha = alpha_val)$groups
              res_tk[[f_nom]] <- rownames(res_tk)
              df_p <- df_clean %>% group_by(.data[[f_nom]]) %>% summarise(m = mean(.data[[y_v]], na.rm = TRUE), se = sd(.data[[y_v]], na.rm = TRUE) / sqrt(n()), .groups = "drop") %>% left_join(res_tk, by = f_nom)
              ggplot(df_p, aes_string(y = paste0("reorder(", f_nom, ", m)"), x = "m")) + geom_errorbarh(aes(xmin = m - se, xmax = m + se), height = 0.2) + geom_point(size = 3, color = "steelblue") + geom_text(aes(label = sprintf("%.2f %s", m, trimws(groups))), hjust = -0.2, vjust = -1, size = 3) + labs(title = paste("Efecto Ppal:", f_nom), x = "Media ± SE", y = NULL) + theme_classic()
            }
            principales <- lapply(factores, plot_efecto)
            lista_graficos_desdob[[y_v]] <<- wrap_plots(principales, nrow = 1) / p_ani
          }, error = function(e) cat("Error calculando desdoblamiento (emmeans):", e$message, "\n"))
        })
      }
    })
    
    lista_supuestos <- list(); lista_graficos_pmm <- list(); lista_graficos_int <- list()
    for(y_var in input$fac_y) {
      local({
        y_v <- y_var
        vars_to_keep <- c(y_v, A, B, covs); if(usar_C) vars_to_keep <- c(vars_to_keep, input$fac_C); if(usar_blk) vars_to_keep <- c(vars_to_keep, input$fac_blk); if(des == "DCL") vars_to_keep <- c(vars_to_keep, input$fac_row, input$fac_col)
        df_clean <- df[complete.cases(df[, vars_to_keep]), ]; df_clean[[y_v]] <- as.numeric(as.character(df_clean[[y_v]])); df_clean <- droplevels(df_clean); if(nrow(df_clean) < 2) return()
        form_fix <- if(usar_C) paste(y_v, "~", cov_str, base_str, A, "*", B, "*", input$fac_C) else paste(y_v, "~", cov_str, base_str, A, "*", B)
        modelo_fix <- aov(as.formula(form_fix), data = df_clean)
        
        df_diag <- data.frame(Fitted = fitted(modelo_fix), Residuos = rstandard(modelo_fix))
        lista_supuestos[[paste0(y_v, "_res")]] <<- ggplot(df_diag, aes(x=Fitted, y=Residuos)) + geom_point(color="blue", size=2) + geom_hline(yintercept=0, color="red", linetype="dashed") + theme_classic() + labs(title=paste("Residuos:", y_v))
        lista_supuestos[[paste0(y_v, "_qq")]] <<- ggplot(df_diag, aes(sample=Residuos)) + stat_qq(color="blue") + stat_qq_line(color="red") + theme_classic() + labs(title=paste("QQ:", y_v))
        
        if(input$fac_pmm %in% c("Tukey", "Fisher (LSD)", "Scott-Knott") && !is.null(input$fac_plot_term)) {
          term_eval <- unlist(strsplit(input$fac_plot_term, ":")); alpha_val <- input$fac_alpha
          if(input$fac_pmm == "Tukey") { pmm_res <- HSD.test(modelo_fix, term_eval, alpha = alpha_val); tabla_letras <- data.frame(Tratamiento = as.character(rownames(pmm_res$groups)), Letra = as.character(pmm_res$groups$groups), stringsAsFactors = FALSE) } else if(input$fac_pmm == "Fisher (LSD)") { pmm_res <- LSD.test(modelo_fix, term_eval, p.adj = "none", alpha = alpha_val); tabla_letras <- data.frame(Tratamiento = as.character(rownames(pmm_res$groups)), Letra = as.character(pmm_res$groups$groups), stringsAsFactors = FALSE) } else { if(length(term_eval) > 1) { df_clean$SK_INT <- interaction(df_clean[, term_eval], sep=":"); mod_sk <- aov(as.formula(paste(y_v, "~ SK_INT")), data=df_clean); sk_res <- SK(mod_sk, which = "SK_INT", sig.level = alpha_val, dispersion = "se") } else { sk_res <- SK(modelo_fix, which = term_eval, sig.level = alpha_val, dispersion = "se") }; tabla_letras <- data.frame(Tratamiento = as.character(rownames(sk_res$groups)), Letra = as.character(sk_res$groups$Groups), stringsAsFactors = FALSE) }
          group_cols <- term_eval; facet_var <- NULL; if(!is.null(input$fac_facet) && input$fac_facet == TRUE && !is.null(input$fac_facet_var)) { facet_var <- input$fac_facet_var; group_cols <- unique(c(group_cols, facet_var)) }
          df_stats <- df_clean %>% group_by(across(dplyr::all_of(group_cols))) %>% summarise(media_r = mean(.data[[y_v]], na.rm = TRUE), n_val = n(), sd_val = sd(.data[[y_v]], na.rm = TRUE), se_val = sd_val / sqrt(n_val), .groups = "drop") %>% mutate(disp = case_when(input$fac_plot_err == "se" ~ se_val, input$fac_plot_err == "sd" ~ sd_val, input$fac_plot_err == "ic" ~ se_val * qt(0.975, df = ifelse(n_val > 1, n_val - 1, 1))))
          if(length(term_eval) > 1) { df_stats$Tratamiento <- apply(df_stats[, term_eval], 1, paste, collapse=":") } else { df_stats$Tratamiento <- as.character(df_stats[[term_eval]]) }
          datos_plot <- df_stats %>% left_join(tabla_letras, by = "Tratamiento")
          
          if(length(term_eval) > 1) { gg <- ggplot(datos_plot, aes_string(y = paste0("reorder(", term_eval[2], ", media_r)"), x = "media_r")) + geom_errorbarh(aes(xmin = media_r - disp, xmax = media_r + disp), height = 0.2) + geom_point(size = 3, color = "black") + geom_text(aes(label = sprintf("%.2f %s", media_r, trimws(Letra))), hjust = -0.1, vjust = -1.1, size = 4.2) + facet_wrap(as.formula(paste("~", term_eval[1])), scales = "free_x") + scale_x_continuous(expand = expansion(mult = c(0.1, 0.5))) + labs(x = paste("Media ±", toupper(input$fac_plot_err)), y = term_eval[2], title = paste(toupper(y_v))) + theme_classic(base_size = 14) + theme(strip.background = element_blank()) } else { gg <- ggplot(datos_plot, aes(y = reorder(Tratamiento, media_r), x = media_r)) + geom_errorbarh(aes(xmin = media_r - disp, xmax = media_r + disp), height = 0.2) + geom_point(size = 3, color = "black") + geom_text(aes(label = sprintf("%.2f %s", media_r, trimws(Letra))), hjust = -0.1, vjust = -1.1, size = 4.2) + scale_x_continuous(expand = expansion(mult = c(0.1, 0.5))) + labs(x = paste("Media ±", toupper(input$fac_plot_err)), y = input$fac_plot_term, title = paste(toupper(y_v))) + theme_classic(base_size = 14) + theme(strip.background = element_blank()); if(!is.null(facet_var)) { gg <- gg + facet_wrap(as.formula(paste("~", facet_var)), scales = "free_x", ncol = input$fac_facet_ncol) } }
          lista_graficos_pmm[[y_v]] <<- gg
          
          if(length(term_eval) >= 2) { 
            x_var <- term_eval[1]; color_var <- term_eval[2]; 
            if(length(term_eval) == 2) { 
              sum_df_int <- df_clean %>% group_by(across(dplyr::all_of(c(x_var, color_var)))) %>% summarise(Media = mean(.data[[y_v]], na.rm=TRUE), SE = sd(.data[[y_v]], na.rm=TRUE)/sqrt(n()), SD = sd(.data[[y_v]], na.rm=TRUE), .groups="drop") %>% mutate(disp = if(input$fac_plot_err=="se") SE else if(input$fac_plot_err=="sd") SD else SE*1.96); sum_df_int$disp[is.na(sum_df_int$disp)] <- 0; 
              gg_int <- ggplot(sum_df_int, aes_string(x = x_var, y = "Media", color = color_var, group = color_var)) + geom_line(linewidth = 1.2) + geom_point(size = 4) + geom_errorbar(aes(ymin = Media - disp, ymax = Media + disp), width = 0.2, linewidth=0.8) + theme_classic(base_size = 14) + labs(title = paste("Interacción:", y_v), y = "Media", x = x_var, color = color_var) 
            } else { 
              facet_var_3 <- term_eval[3]; sum_df_int <- df_clean %>% group_by(across(dplyr::all_of(c(x_var, color_var, facet_var_3)))) %>% summarise(Media = mean(.data[[y_v]], na.rm=TRUE), SE = sd(.data[[y_v]], na.rm=TRUE)/sqrt(n()), SD = sd(.data[[y_v]], na.rm=TRUE), .groups="drop") %>% mutate(disp = if(input$fac_plot_err=="se") SE else if(input$fac_plot_err=="sd") SD else SE*1.96); sum_df_int$disp[is.na(sum_df_int$disp)] <- 0; 
              gg_int <- ggplot(sum_df_int, aes_string(x = x_var, y = "Media", color = color_var, group = color_var)) + geom_line(linewidth = 1.2) + geom_point(size = 4) + geom_errorbar(aes(ymin = Media - disp, ymax = Media + disp), width = 0.2, linewidth=0.8) + facet_wrap(as.formula(paste("~", facet_var_3))) + theme_classic(base_size = 14) + labs(title = paste("Interacción Triple:", y_v), y = "Media", x = x_var, color = color_var) 
            }
            lista_graficos_int[[y_v]] <<- gg_int 
          }
        }
      })
    }
    if(length(lista_supuestos) > 0) vals$p_fac_sup <- patchwork::wrap_plots(lista_supuestos, ncol = 2)
    if(length(lista_graficos_pmm) > 0) vals$p_fac_pmm <- patchwork::wrap_plots(lista_graficos_pmm, ncol = if(length(lista_graficos_pmm)>1) 2 else 1)
    if(length(lista_graficos_int) > 0) vals$p_fac_int <- patchwork::wrap_plots(lista_graficos_int, ncol = if(length(lista_graficos_int)>1) 2 else 1)
  })
  
  output$fac_plots <- renderPlot({ req(vals$p_fac_sup); vals$p_fac_sup })
  output$fac_pmm_plot <- renderPlot({ req(vals$p_fac_pmm); vals$p_fac_pmm })
  output$fac_interaction_plot <- renderPlot({ req(vals$p_fac_int); vals$p_fac_int })
  output$fac_desdob_plot <- renderPlot({ req(vals$p_fac_desdob); vals$p_fac_desdob })
  observeEvent(input$btn_pop_fac_sup, { req(vals$p_fac_sup); if(interactive()){ dev.new(noRStudioGD=T); print(vals$p_fac_sup) } })
  observeEvent(input$btn_pop_fac_pmm, { req(vals$p_fac_pmm); if(interactive()){ dev.new(noRStudioGD=T); print(vals$p_fac_pmm) } })
  observeEvent(input$btn_pop_fac_int, { req(vals$p_fac_int); if(interactive()){ dev.new(noRStudioGD=T); print(vals$p_fac_int) } })
  observeEvent(input$btn_pop_fac_desdob, { req(vals$p_fac_desdob); if(interactive()){ dev.new(noRStudioGD=T, width=12, height=8); print(vals$p_fac_desdob) } })
  
  # --- TABLA RESUMEN FACTORIAL (AGRUPACIÓN RELATIVOS IZQUIERDA) ---
  output$fac_resumen_vars_ui <- renderUI({ req(input$fac_y); selectizeInput("fac_resumen_vars", "Variables a incluir:", choices = input$fac_y, multiple = TRUE, selected = input$fac_y) })
  output$fac_resumen_term_ui <- renderUI({ req(input$fac_pmm_terms); selectInput("fac_resumen_term", "Efecto a resumir:", choices = input$fac_pmm_terms, selected = tail(input$fac_pmm_terms, 1)) })
  output$fac_resumen_testigo_ui <- renderUI({ req(vals$datos, input$fac_resumen_term); df <- vals$datos; term_eval <- unlist(strsplit(input$fac_resumen_term, ":")); if(length(term_eval) == 1) { niveles <- unique(as.character(df[[term_eval]])) } else { niveles <- unique(apply(df[, term_eval], 1, paste, collapse=":")) }; selectInput("fac_resumen_testigo", "Testigo (Base Relativa = 1):", choices = niveles) })
  
  fac_resumen_data <- eventReactive(input$btn_gen_fac_resumen, {
    req(vals$datos, input$fac_resumen_vars, input$fac_resumen_term, input$fac_resumen_testigo)
    df <- vals$datos; vars_analizar <- input$fac_resumen_vars; term <- input$fac_resumen_term; term_eval <- unlist(strsplit(term, ":")); testigo <- input$fac_resumen_testigo; alpha_val <- input$fac_alpha; covs <- input$fac_cov
    if(length(term_eval) == 1) { df_resumen <- data.frame(Tratamiento = unique(as.character(df[[term_eval]]))) } else { df_resumen <- data.frame(Tratamiento = unique(apply(df[, term_eval], 1, paste, collapse=":"))) }
    lista_p_valores <- list(); todos_los_terminos_anova <- c(); cov_str <- if(length(covs) > 0) paste(paste(covs, collapse=" + "), "+ ") else ""
    
    for(v in vars_analizar) {
      usar_blk <- input$fac_des == "DBCA" || input$fac_arr != "Factorial Combinatorio"
      vars_to_keep <- c(v, input$fac_A, input$fac_B, covs); if(input$fac_arr %in% c("Parcelas Subdivididas", "Franjas Divididas", "Factorial Combinatorio") && !is.null(input$fac_C) && input$fac_C != "") vars_to_keep <- c(vars_to_keep, input$fac_C); if(usar_blk) vars_to_keep <- c(vars_to_keep, input$fac_blk); if(input$fac_des == "DCL") vars_to_keep <- c(vars_to_keep, input$fac_row, input$fac_col)
      df_clean <- df[complete.cases(df[, vars_to_keep]), ]; df_clean[[v]] <- as.numeric(as.character(df_clean[[v]])); df_clean[[input$fac_A]] <- as.factor(df_clean[[input$fac_A]]); df_clean[[input$fac_B]] <- as.factor(df_clean[[input$fac_B]])
      if(input$fac_arr %in% c("Parcelas Subdivididas", "Franjas Divididas", "Factorial Combinatorio") && !is.null(input$fac_C) && input$fac_C != "") df_clean[[input$fac_C]] <- as.factor(df_clean[[input$fac_C]]); if(usar_blk) df_clean[[input$fac_blk]] <- as.factor(df_clean[[input$fac_blk]]); if(input$fac_des == "DCL") { df_clean[[input$fac_row]] <- as.factor(df_clean[[input$fac_row]]); df_clean[[input$fac_col]] <- as.factor(df_clean[[input$fac_col]]) }
      
      A <- input$fac_A; B <- input$fac_B; des <- input$fac_des; arr <- input$fac_arr; base_str <- if(des=="DBCA") paste0(input$fac_blk, " + ") else if(des=="DCL") paste0(input$fac_row, " + ", input$fac_col, " + ") else ""
      
      if(arr == "Factorial Combinatorio") { if(!is.null(input$fac_C) && input$fac_C != "") { form_anova <- paste(v, "~", cov_str, base_str, A, "*", B, "*", input$fac_C); form_fix <- form_anova } else { form_anova <- paste(v, "~", cov_str, base_str, A, "*", B); form_fix <- form_anova } 
      } else if (arr == "Parcelas Divididas") { form_anova <- paste(v, "~", cov_str, base_str, A, "+", if(des=="DBCA") paste0("Error(", input$fac_blk, "/", A, ")") else paste0("Error(", input$fac_blk, ":", A, ")"), "+", B, "+", A, ":", B); form_fix <- paste(v, "~", cov_str, base_str, A, "*", B) 
      } else if (arr == "Parcelas Subdivididas") { form_anova <- paste(v, "~", cov_str, base_str, A, "*", B, "*", input$fac_C, "+ Error(", input$fac_blk, "/", A, "/", B, ")"); form_fix <- paste(v, "~", cov_str, base_str, A, "*", B, "*", input$fac_C) 
      } else if (arr == "Franjas") { form_anova <- paste(v, "~", cov_str, base_str, A, "*", B, "+ Error(", input$fac_blk, "/(", A, "+", B, "))"); form_fix <- paste(v, "~", cov_str, base_str, A, "*", B) 
      } else if (arr == "Franjas Divididas") { form_anova <- paste(v, "~", cov_str, A, "*", B, "*", input$fac_C, "+ Error(", input$fac_blk, "/(", A, "*", B, "))"); form_fix <- paste(v, "~", cov_str, base_str, A, "*", B, "*", input$fac_C) }
      
      mod_anova <- aov(as.formula(form_anova), data = df_clean); mod_fix <- aov(as.formula(form_fix), data = df_clean)
      pvs <- c(); nombres_pvs <- c()
      if(inherits(mod_anova, "aovlist")) { for(i in 2:length(summary(mod_anova))) { sm <- summary(mod_anova)[[i]][[1]]; rn <- rownames(sm); rn <- rn[rn != "Residuals" & trimws(rn) != ""]; for(r in rn) { p <- sm[r, "Pr(>F)"]; if(!is.na(p)) { nombres_pvs <- c(nombres_pvs, trimws(r)); pvs <- c(pvs, p) } } } } else { sm <- summary(mod_anova)[[1]]; rn <- rownames(sm); rn <- rn[rn != "Residuals" & trimws(rn) != ""]; for(r in rn) { p <- sm[r, "Pr(>F)"]; if(!is.na(p)) { nombres_pvs <- c(nombres_pvs, trimws(r)); pvs <- c(pvs, p) } } }
      pvs_fmt <- ifelse(pvs < 0.001, "<0.001", sprintf("%.4f", pvs)); names(pvs_fmt) <- nombres_pvs; lista_p_valores[[v]] <- pvs_fmt; todos_los_terminos_anova <- unique(c(todos_los_terminos_anova, nombres_pvs))
      
      if(input$fac_pmm == "Scott-Knott") { if(length(term_eval) > 1) { df_clean$SK_INT <- interaction(df_clean[, term_eval], sep=":"); mod_sk <- aov(as.formula(paste(v, "~", cov_str, base_str, "SK_INT")), data=df_clean); sk_res <- SK(mod_sk, which = "SK_INT", sig.level = alpha_val, dispersion = "se"); tabla_letras <- data.frame(Tratamiento = as.character(rownames(sk_res$groups)), Letra = as.character(sk_res$groups$Groups), stringsAsFactors = FALSE) } else { sk_res <- SK(mod_fix, which = term, sig.level = alpha_val, dispersion = "se"); tabla_letras <- data.frame(Tratamiento = as.character(rownames(sk_res$groups)), Letra = as.character(sk_res$groups$Groups), stringsAsFactors = FALSE) } } else { if(input$fac_pmm == "Tukey") pmm_res <- HSD.test(mod_fix, term_eval, alpha = alpha_val) else pmm_res <- LSD.test(mod_fix, term_eval, p.adj = "none", alpha = alpha_val); tabla_letras <- data.frame(Tratamiento = as.character(rownames(pmm_res$groups)), Letra = as.character(pmm_res$groups$groups), stringsAsFactors = FALSE) }
      if(length(term_eval) > 1) { df_clean$Tratamiento <- apply(df_clean[, term_eval], 1, paste, collapse=":") } else { df_clean$Tratamiento <- as.character(df_clean[[term_eval]]) }
      medias_var <- df_clean %>% group_by(Tratamiento) %>% summarise(Media = mean(.data[[v]], na.rm=TRUE), .groups="drop") %>% left_join(tabla_letras, by = "Tratamiento")
      media_testigo <- medias_var$Media[medias_var$Tratamiento == testigo]; if(length(media_testigo) == 0 || is.na(media_testigo) || media_testigo == 0) { media_testigo <- 1 }
      medias_var <- medias_var %>% mutate(Texto_Combinado = sprintf("%.2f %s", Media, trimws(Letra)), Relativo = Media / media_testigo)
      df_resumen <- df_resumen %>% left_join(medias_var %>% dplyr::select(Tratamiento, Texto_Combinado, Relativo), by = "Tratamiento")
      colnames(df_resumen)[ncol(df_resumen)-1] <- v; colnames(df_resumen)[ncol(df_resumen)] <- paste(v, "Rel")
    }
    
    cols_relativas <- grep("Rel$", colnames(df_resumen), value = TRUE)
    cols_restantes <- setdiff(colnames(df_resumen), c("Tratamiento", cols_relativas))
    df_resumen <- df_resumen %>% dplyr::select(Tratamiento, dplyr::all_of(cols_relativas), dplyr::all_of(cols_restantes))
    df_resumen <- df_resumen[order(df_resumen$Tratamiento), ]; df_resumen <- data.frame(`No Trat` = paste0("T", 1:nrow(df_resumen)), df_resumen, check.names = FALSE)
    df_resumen <- df_resumen %>% mutate(across(everything(), as.character)); todos_los_terminos_anova <- sort(todos_los_terminos_anova)
    
    for(termino in todos_los_terminos_anova) { 
      fila_p <- c("", paste("P-valor (", termino, ")", sep=""))
      for (col in colnames(df_resumen)[-c(1,2)]) { if(col %in% vars_analizar) { val <- lista_p_valores[[col]][termino]; if(is.na(val)) val <- "-"; fila_p <- c(fila_p, val) } else { fila_p <- c(fila_p, "") } }
      df_resumen <- rbind(df_resumen, fila_p) 
    }
    return(df_resumen)
  })
  output$fac_tabla_resumen <- renderDT({ req(fac_resumen_data()); datatable(fac_resumen_data(), extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('copy', 'excel'), pageLength = 25), rownames = FALSE, class = "cell-border stripe") })
  
  # =====================================================================
  # --- MULTI-AMBIENTALES (MÚLTIPLES FACTORES, AOV Y LMER FLEXIBLE) ---
  # =====================================================================
  output$ma_vars_ui <- renderUI({ 
    req(vals$datos); vars <- names(vals$datos)
    tagList(
      selectizeInput("ma_y", "Variables Respuesta (Y):", choices = names(dplyr::select_if(vals$datos, is.numeric)), multiple=TRUE), 
      selectizeInput("ma_gen", "Factores de Tratamiento (Permite múltiples):", choices = vars, multiple = TRUE), 
      selectInput("ma_amb", "Ambiente (Finca/Localidad):", choices = c("Seleccione..."="", vars)), 
      selectInput("ma_zafra", "Zafra/Año (Opcional):", choices = c("Solo tengo 1 Zafra"="", vars)),
      selectInput("ma_blk", "Bloque / Réplica:", choices = c("Seleccione..."="", vars)), 
      selectizeInput("ma_cov", "Covariable(s) (Opcional):", choices = names(dplyr::select_if(vals$datos, is.numeric)), multiple = TRUE),
      hr(),
      h4("Estructura Mixta (Solo aplica si usas Zafra)"),
      selectInput("ma_lmm_type", "Efectos Aleatorios (LMER):", 
                  choices = c(
                    "1. (1 | Zafra / Finca)" = "mod1",
                    "2. (1 | Finca / Replica)" = "mod2",
                    "3. (1 | Zafra / Finca) + (1 | Finca : Replica)" = "mod3"
                  ), selected = "mod3")
    ) 
  })
  
  output$ma_pmm_terms_ui <- renderUI({ 
    req(input$ma_gen, input$ma_amb)
    if(!is.null(input$ma_zafra) && input$ma_zafra != "") {
      choices <- c(input$ma_gen, paste(input$ma_gen, collapse=":"), paste(paste(input$ma_gen, collapse=":"), input$ma_zafra, sep=":"))
    } else {
      choices <- c(input$ma_gen, paste(input$ma_gen, collapse=":"), paste(paste(input$ma_gen, collapse=":"), input$ma_amb, sep=":"))
    }
    selectInput("ma_pmm_term_sel", "Efecto a evaluar para PMM y Tabla Resumen:", choices = choices, selected = paste(input$ma_gen, collapse=":")) 
  })
  
  output$ma_facet_ui <- renderUI({ req(input$ma_gen, input$ma_amb); selectInput("ma_facet_var", "Variable para Facetas:", choices = c(input$ma_amb, input$ma_gen)) })
  
  output$ma_resumen_testigo_ui <- renderUI({ 
    req(vals$datos, input$ma_pmm_term_sel); df <- vals$datos; 
    term_eval <- unlist(strsplit(input$ma_pmm_term_sel, ":"))
    if(length(term_eval) == 1) { niveles <- unique(as.character(df[[term_eval]])) } else { niveles <- unique(apply(df[, term_eval], 1, paste, collapse=":")) }
    selectInput("ma_resumen_testigo", "Testigo (Base Relativa = 1):", choices = niveles) 
  })
  
  observeEvent(input$run_ma, {
    req(vals$datos, input$ma_y, input$ma_gen, input$ma_amb, input$ma_blk); if(length(input$ma_y)==0) return()
    df <- vals$datos; G_vars <- input$ma_gen; E <- input$ma_amb; B <- input$ma_blk; Z <- input$ma_zafra; covs <- input$ma_cov
    
    for(g in G_vars) df[[g]] <- as.factor(df[[g]])
    df[[E]] <- as.factor(df[[E]]); df[[B]] <- as.factor(df[[B]])
    usar_zafra <- !is.null(Z) && Z != ""
    if(usar_zafra) df[[Z]] <- as.factor(df[[Z]])
    if(length(covs) > 0) { for(cv in covs) df[[cv]] <- as.numeric(as.character(df[[cv]])) }
    
    G_str <- paste(G_vars, collapse = "*")
    cov_str <- if(length(covs) > 0) paste(paste(covs, collapse=" + "), "+ ") else ""
    
    # 1. ANDEVA / LMER
    output$ma_anova <- renderPrint({
      txt_out <- ""
      for(y_var in input$ma_y) {
        cols_limpieza <- c(y_var, G_vars, E, B, covs); if(usar_zafra) cols_limpieza <- c(cols_limpieza, Z)
        df_c <- df[complete.cases(df[, cols_limpieza]), ]; df_c[[y_var]] <- as.numeric(as.character(df_c[[y_var]])); df_c <- droplevels(df_c); if(nrow(df_c)<2) next
        
        if(usar_zafra) {
          # Lógica dinámica de Efectos Aleatorios
          if(input$ma_lmm_type == "mod1") ran_eff <- paste0("(1|", Z, "/", E, ")")
          else if(input$ma_lmm_type == "mod2") ran_eff <- paste0("(1|", E, "/", B, ")")
          else ran_eff <- paste0("(1|", Z, "/", E, ") + (1|", E, ":", B, ")")
          
          form_lmer <- paste(y_var, "~", cov_str, G_str, "*", Z, "+", ran_eff)
          mod <- lmerTest::lmer(as.formula(form_lmer), data = df_c)
          txt_out <- paste0(txt_out, "\n========================================\n MODELO MIXTO LMER: ", toupper(y_var), "\n========================================\n")
          txt_out <- paste0(txt_out, "Formula: ", form_lmer, "\n\n")
          txt_out <- paste0(txt_out, paste(capture.output(anova(mod)), collapse="\n"), "\n")
        } else {
          # Modelo AOV 1 Zafra
          form_aov <- paste(y_var, "~", cov_str, G_str, "+", E, "+ Error(", B, "%in%", E, ") +", G_str, ":", E)
          mod <- aov(as.formula(form_aov), data = df_c)
          txt_out <- paste0(txt_out, "\n========================================\n ANDEVA AOV: ", toupper(y_var), "\n========================================\n")
          txt_out <- paste0(txt_out, "Formula: ", form_aov, "\n\n")
          txt_out <- paste0(txt_out, paste(capture.output(summary(mod)), collapse="\n"), "\n")
        }
      }
      cat(txt_out)
    })
    
    # 2. SUPUESTOS MULTI-AMBIENTALES
    output$ma_supuestos <- renderPrint({
      for(y_var in input$ma_y) {
        cols_limpieza <- c(y_var, G_vars, E, B, covs); if(usar_zafra) cols_limpieza <- c(cols_limpieza, Z)
        df_c <- df[complete.cases(df[, cols_limpieza]), ]; df_c[[y_var]] <- as.numeric(as.character(df_c[[y_var]])); df_c <- droplevels(df_c); if(nrow(df_c)<2) next
        form_fix <- if(usar_zafra) paste(y_var, "~", cov_str, G_str, "*", Z, "*", E) else paste(y_var, "~", cov_str, G_str, "*", E)
        mod_fix <- aov(as.formula(form_fix), data = df_c)
        
        cat("===================================================\n")
        cat("   PRUEBAS FORMALES DE SUPUESTOS PARA:", toupper(y_var), "\n")
        cat("===================================================\n\n")
        
        residuos <- tryCatch(resid(mod_fix), error = function(e) NULL)
        if(!is.null(residuos)) {
          shap <- tryCatch(shapiro.test(residuos), error=function(e) NULL)
          if(!is.null(shap)) { cat(sprintf("1. NORMALIDAD (Shapiro-Wilk):\n   W = %.4f, P-valor = %.4f\n   --> %s\n\n", shap$statistic, shap$p.value, ifelse(shap$p.value < 0.05, "[ALERTA] No Normal.", "[OK] Distribución Normal."))) }
          lev <- tryCatch(car::leveneTest(mod_fix), error=function(e) NULL)
          if(!is.null(lev)) { cat(sprintf("2. HOMOCEDASTICIDAD ROBUSTA (Levene):\n   F = %.4f, P-valor = %.4f\n   --> %s\n\n", lev$`F value`[1], lev$`Pr(>F)`[1], ifelse(lev$`Pr(>F)`[1] < 0.05, "[ALERTA] Varianzas Heterogéneas.", "[OK] Varianzas Homogéneas."))) }
          tryCatch({
            y_data <- mod_fix$model[[1]]; factores_modelo <- mod_fix$model[, -1, drop = FALSE]; grupos_comb <- apply(factores_modelo, 1, paste, collapse = "-")
            bart <- bartlett.test(y_data ~ as.factor(grupos_comb))
            cat(sprintf("3. HOMOCEDASTICIDAD PARAMÉTRICA (Bartlett):\n   K-cuadrado = %.4f, P-valor = %.4f\n   --> %s\n\n", bart$statistic, bart$p.value, ifelse(bart$p.value < 0.05, "[ALERTA] Varianzas Heterogéneas.", "[OK] Varianzas Homogéneas.")))
          }, error = function(e) { })
          dw <- tryCatch(car::durbinWatsonTest(mod_fix), error=function(e) NULL)
          if(!is.null(dw)) { cat(sprintf("4. INDEPENDENCIA (Durbin-Watson):\n   DW = %.4f, P-valor = %.4f\n   --> %s\n\n", dw$dw, dw$p, ifelse(dw$p < 0.05, "[ALERTA] Autocorrelación detectada.", "[OK] Residuos Independientes."))) }
        }
      }
    })
    
    output$ma_pmm_out <- renderPrint({ cat("PMM calculado en el gráfico y en la Tabla Resumen.\n") })
    
    lista_supuestos <- list(); lista_pmm <- list(); lista_int <- list()
    for(y_var in input$ma_y) {
      local({
        y_v <- y_var
        cols_limpieza <- c(y_v, G_vars, E, B, covs); if(usar_zafra) cols_limpieza <- c(cols_limpieza, Z)
        df_c <- df[complete.cases(df[, cols_limpieza]), ]; df_c[[y_v]] <- as.numeric(as.character(df_c[[y_v]])); df_c <- droplevels(df_c); if(nrow(df_c)<2) return()
        
        form_fix <- if(usar_zafra) paste(y_v, "~", cov_str, G_str, "*", Z, "*", E) else paste(y_v, "~", cov_str, G_str, "*", E)
        mod_fix <- aov(as.formula(form_fix), data = df_c)
        df_diag <- data.frame(Fitted = fitted(mod_fix), Residuos = rstandard(mod_fix)); p_res <- ggplot(df_diag, aes(x=Fitted, y=Residuos)) + geom_point(color="blue") + geom_hline(yintercept=0, color="red", linetype="dashed") + theme_classic() + labs(title=paste("Residuos:", y_v)); p_qq <- ggplot(df_diag, aes(sample=Residuos)) + stat_qq(color="blue") + stat_qq_line(color="red") + theme_classic() + labs(title=paste("QQ:", y_v))
        lista_supuestos[[paste0(y_v, "_res")]] <<- p_res; lista_supuestos[[paste0(y_v, "_qq")]] <<- p_qq
        
        term_eval <- unlist(strsplit(input$ma_pmm_term_sel, ":")); alpha_val <- input$ma_alpha
        
        if(usar_zafra) {
          if(input$ma_lmm_type == "mod1") ran_eff <- paste0("(1|", Z, "/", E, ")")
          else if(input$ma_lmm_type == "mod2") ran_eff <- paste0("(1|", E, "/", B, ")")
          else ran_eff <- paste0("(1|", Z, "/", E, ") + (1|", E, ":", B, ")")
          
          mod_temp <- lmerTest::lmer(as.formula(paste(y_v, "~", cov_str, G_str, "*", Z, "+", ran_eff)), data = df_c)
          em <- emmeans::emmeans(mod_temp, as.formula(paste("~", paste(term_eval, collapse="*"))), data = df_c)
          res_cld <- as.data.frame(multcomp::cld(em, adjust = ifelse(input$ma_pmm=="Tukey", "tukey", "none"), Letters = letters, alpha = alpha_val, reversed = TRUE))
          if(length(term_eval) > 1) { res_cld$Tratamiento <- apply(res_cld[, term_eval], 1, paste, collapse=":") } else { res_cld$Tratamiento <- as.character(res_cld[[term_eval]]) }
          tabla_letras <- data.frame(Tratamiento = res_cld$Tratamiento, Letra = as.character(res_cld$.group), stringsAsFactors = FALSE)
        } else {
          mod_temp <- aov(as.formula(paste(y_v, "~", cov_str, paste(term_eval, collapse="*"))), data=df_c)
          if(input$ma_pmm == "Tukey") { pmm_res <- HSD.test(mod_temp, term_eval, alpha = alpha_val); tabla_letras <- data.frame(Tratamiento = as.character(rownames(pmm_res$groups)), Letra = as.character(pmm_res$groups$groups), stringsAsFactors = FALSE) } else if(input$ma_pmm == "Fisher (LSD)") { pmm_res <- LSD.test(mod_temp, term_eval, p.adj = "none", alpha = alpha_val); tabla_letras <- data.frame(Tratamiento = as.character(rownames(pmm_res$groups)), Letra = as.character(pmm_res$groups$groups), stringsAsFactors = FALSE) } else { if(length(term_eval) > 1) { df_c$SK_INT <- interaction(df_c[, term_eval], sep=":"); mod_sk <- aov(as.formula(paste(y_v, "~", cov_str, "SK_INT")), data=df_c); sk_res <- SK(mod_sk, which = "SK_INT", sig.level = alpha_val, dispersion = "se") } else { sk_res <- SK(mod_temp, which = term_eval, sig.level = alpha_val, dispersion = "se") }; tabla_letras <- data.frame(Tratamiento = as.character(rownames(sk_res$groups)), Letra = as.character(sk_res$groups$Groups), stringsAsFactors = FALSE) }
        }
        
        group_cols <- term_eval; facet_var <- NULL; if(input$ma_facet && !is.null(input$ma_facet_var)) { facet_var <- input$ma_facet_var; group_cols <- unique(c(group_cols, facet_var)) }
        df_stats <- df_c %>% group_by(across(dplyr::all_of(group_cols))) %>% summarise(media_r = mean(.data[[y_v]], na.rm = TRUE), n_val = n(), sd_val = sd(.data[[y_v]], na.rm = TRUE), se_val = sd_val / sqrt(n_val), .groups = "drop") %>% mutate(disp = case_when(input$ma_plot_err == "se" ~ se_val, input$ma_plot_err == "sd" ~ sd_val, input$ma_plot_err == "ic" ~ se_val * qt(0.975, df = ifelse(n_val > 1, n_val - 1, 1))))
        if(length(term_eval) > 1) { df_stats$Tratamiento <- apply(df_stats[, term_eval], 1, paste, collapse=":") } else { df_stats$Tratamiento <- as.character(df_stats[[term_eval]]) }
        datos_plot <- df_stats %>% left_join(tabla_letras, by = "Tratamiento")
        
        eje_y <- if(length(term_eval) > 1) "Tratamiento" else term_eval[1]
        gg <- ggplot(datos_plot, aes_string(y = paste0("reorder(", eje_y, ", media_r)"), x = "media_r")) + geom_errorbarh(aes(xmin = media_r - disp, xmax = media_r + disp), height = 0.2) + geom_point(size = 3, color = "black") + geom_text(aes(label = sprintf("%.2f %s", media_r, trimws(Letra))), hjust = -0.1, vjust = -1.1, size = 4.2) + scale_x_continuous(expand = expansion(mult = c(0.1, 0.5))) + labs(x = paste("Media ±", toupper(input$ma_plot_err)), y = eje_y, title = paste("Medias:", y_v)) + theme_classic(base_size = 14) + theme(strip.text = element_text(size = 13, face = "bold"), strip.background = element_blank())
        if(!is.null(facet_var) && length(term_eval) == 1) { gg <- gg + facet_wrap(as.formula(paste("~", facet_var)), scales = "free_x", ncol = input$ma_facet_ncol) }
        lista_pmm[[y_v]] <<- gg
        
        G_primario <- G_vars[1]
        sum_df_int <- df_c %>% group_by(across(dplyr::all_of(c(G_primario, E)))) %>% summarise(Media = mean(.data[[y_v]], na.rm=TRUE), SE = sd(.data[[y_v]], na.rm=TRUE)/sqrt(n()), SD = sd(.data[[y_v]], na.rm=TRUE), .groups="drop") %>% mutate(disp = if(input$ma_plot_err=="se") SE else if(input$ma_plot_err=="sd") SD else SE*1.96); sum_df_int$disp[is.na(sum_df_int$disp)] <- 0
        gg_int <- ggplot(sum_df_int, aes_string(x = G_primario, y = "Media", color = E, group = E)) + geom_line(linewidth = 1.2) + geom_point(size = 4) + geom_errorbar(aes(ymin = Media - disp, ymax = Media + disp), width = 0.2, linewidth=0.8) + theme_classic(base_size = 14) + labs(title = paste("Interacción Trt x Ambiente:", y_v), y = "Media", x = G_primario, color = E)
        if(input$ma_facet && !is.null(input$ma_facet_var)) { gg_int <- gg_int + facet_wrap(as.formula(paste("~", input$ma_facet_var)), ncol = input$ma_facet_ncol) }
        lista_int[[y_v]] <<- gg_int
      })
    }
    if(length(lista_supuestos) > 0) vals$p_ma_sup <- patchwork::wrap_plots(lista_supuestos, ncol = 2)
    if(length(lista_pmm) > 0) vals$p_ma_pmm <- patchwork::wrap_plots(lista_pmm, ncol = if(length(lista_pmm)>1) 2 else 1)
    if(length(lista_int) > 0) vals$p_ma_int <- patchwork::wrap_plots(lista_int, ncol = if(length(lista_int)>1) 2 else 1)
  })
  
  # --- 3. GENERADOR DE LA TABLA RESUMEN MULTI-AMBIENTAL ---
  ma_resumen_data <- eventReactive(input$btn_gen_ma_resumen, {
    req(vals$datos, input$ma_y, input$ma_pmm_term_sel, input$ma_resumen_testigo)
    df <- vals$datos; vars_analizar <- input$ma_y; term_eval <- unlist(strsplit(input$ma_pmm_term_sel, ":")); testigo <- input$ma_resumen_testigo
    G_vars <- input$ma_gen; E <- input$ma_amb; Z <- input$ma_zafra; B <- input$ma_blk; covs <- input$ma_cov
    usar_zafra <- !is.null(Z) && Z != ""; G_str <- paste(G_vars, collapse = "*"); cov_str <- if(length(covs) > 0) paste(paste(covs, collapse=" + "), "+ ") else ""
    
    if(length(term_eval) == 1) { df_resumen <- data.frame(Tratamiento = unique(as.character(df[[term_eval]]))) } else { df_resumen <- data.frame(Tratamiento = unique(apply(df[, term_eval], 1, paste, collapse=":"))) }
    lista_p_valores <- list(); todos_los_terminos_anova <- c()
    
    for(v in vars_analizar) {
      cols_limpieza <- c(v, G_vars, E, B, covs); if(usar_zafra) cols_limpieza <- c(cols_limpieza, Z)
      df_clean <- df[complete.cases(df[, cols_limpieza]), ]; df_clean[[v]] <- as.numeric(as.character(df_clean[[v]])); df_clean <- droplevels(df_clean)
      
      pvs <- c(); nombres_pvs <- c()
      if(usar_zafra) {
        if(input$ma_lmm_type == "mod1") ran_eff <- paste0("(1|", Z, "/", E, ")")
        else if(input$ma_lmm_type == "mod2") ran_eff <- paste0("(1|", E, "/", B, ")")
        else ran_eff <- paste0("(1|", Z, "/", E, ") + (1|", E, ":", B, ")")
        
        mod <- lmerTest::lmer(as.formula(paste(v, "~", cov_str, G_str, "*", Z, "+", ran_eff)), data = df_clean)
        anv <- anova(mod); rn <- rownames(anv)
        for(r in rn) { p <- anv[r, "Pr(>F)"]; if(!is.na(p)) { nombres_pvs <- c(nombres_pvs, trimws(r)); pvs <- c(pvs, p) } }
        em <- emmeans::emmeans(mod, as.formula(paste("~", paste(term_eval, collapse="*"))))
      } else {
        mod <- aov(as.formula(paste(v, "~", cov_str, G_str, "+", E, "+ Error(", B, "%in%", E, ") +", G_str, ":", E)), data = df_clean)
        if(inherits(mod, "aovlist")) { for(i in 2:length(summary(mod))) { sm <- summary(mod)[[i]][[1]]; rn <- rownames(sm); rn <- rn[rn != "Residuals" & trimws(rn) != ""]; for(r in rn) { p <- sm[r, "Pr(>F)"]; if(!is.na(p)) { nombres_pvs <- c(nombres_pvs, trimws(r)); pvs <- c(pvs, p) } } } }
        mod_fix <- aov(as.formula(paste(v, "~", cov_str, paste(term_eval, collapse="*"))), data=df_clean)
        em <- emmeans::emmeans(mod_fix, as.formula(paste("~", paste(term_eval, collapse="*"))))
      }
      
      pvs_fmt <- ifelse(pvs < 0.01, "<0.01", sprintf("%.4f", pvs)); names(pvs_fmt) <- nombres_pvs; lista_p_valores[[v]] <- pvs_fmt; todos_los_terminos_anova <- unique(c(todos_los_terminos_anova, nombres_pvs))
      
      letras <- as.data.frame(multcomp::cld(em, adjust = ifelse(input$ma_pmm=="Tukey", "tukey", "none"), Letters = letters, alpha = input$ma_alpha, reversed = TRUE))
      if(length(term_eval) > 1) { letras$Tratamiento <- apply(letras[, term_eval], 1, paste, collapse=":") } else { letras$Tratamiento <- as.character(letras[[term_eval]]) }
      
      media_testigo <- letras$emmean[letras$Tratamiento == testigo]; if(length(media_testigo) == 0 || is.na(media_testigo) || media_testigo == 0) media_testigo <- 1
      letras <- letras %>% mutate(Texto_Combinado = sprintf("%.2f %s", emmean, toupper(trimws(.group))), Relativo = emmean / media_testigo)
      
      df_resumen <- df_resumen %>% left_join(letras %>% dplyr::select(Tratamiento, Texto_Combinado, Relativo), by = "Tratamiento")
      colnames(df_resumen)[ncol(df_resumen)-1] <- v; colnames(df_resumen)[ncol(df_resumen)] <- paste(v, "Rel")
    }
    
    cols_relativas <- grep("Rel$", colnames(df_resumen), value = TRUE)
    cols_restantes <- setdiff(colnames(df_resumen), c("Tratamiento", cols_relativas))
    df_resumen <- df_resumen %>% dplyr::select(Tratamiento, dplyr::all_of(cols_relativas), dplyr::all_of(cols_restantes))
    df_resumen <- df_resumen[order(df_resumen$Tratamiento), ]; df_resumen <- data.frame(`No Trat` = paste0("T", 1:nrow(df_resumen)), df_resumen, check.names = FALSE)
    df_resumen <- df_resumen %>% mutate(across(everything(), as.character))
    
    for(termino in todos_los_terminos_anova) { 
      fila_p <- c("", paste("P-valor (", termino, ")", sep=""))
      for (col in colnames(df_resumen)[-c(1,2)]) { if(col %in% vars_analizar) { val <- lista_p_valores[[col]][termino]; if(is.na(val)) val <- "-"; fila_p <- c(fila_p, val) } else { fila_p <- c(fila_p, "") } }
      df_resumen <- rbind(df_resumen, fila_p) 
    }
    return(df_resumen)
  })
  
  output$ma_tabla_resumen <- renderDT({ req(ma_resumen_data()); datatable(ma_resumen_data(), extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('copy', 'excel'), pageLength = 25), rownames = FALSE, class = "cell-border stripe") })
  
  output$ma_plots <- renderPlot({ req(vals$p_ma_sup); vals$p_ma_sup })
  output$ma_pmm_plot <- renderPlot({ req(vals$p_ma_pmm); vals$p_ma_pmm })
  output$ma_int_plot <- renderPlot({ req(vals$p_ma_int); vals$p_ma_int })
  observeEvent(input$btn_pop_ma_sup, { req(vals$p_ma_sup); if(interactive()){ dev.new(noRStudioGD=T); print(vals$p_ma_sup) } })
  observeEvent(input$btn_pop_ma_pmm, { req(vals$p_ma_pmm); if(interactive()){ dev.new(noRStudioGD=T); print(vals$p_ma_pmm) } })
  observeEvent(input$btn_pop_ma_int, { req(vals$p_ma_int); if(interactive()){ dev.new(noRStudioGD=T); print(vals$p_ma_int) } })
  
  # =====================================================================
  # --- UI CONSOLIDADOS ---
  # =====================================================================
  output$cons_trat_ui <- renderUI({ req(vals$datos); selectizeInput("cons_trat", "Factores de Tratamiento:", choices = names(vals$datos), multiple = TRUE) })
  output$cons_amb_ui <- renderUI({ req(vals$datos); selectInput("cons_amb", "Columna Finca/País:", choices = names(vals$datos)) })
  output$cons_blk_ui <- renderUI({ req(vals$datos); selectInput("cons_blk", "Bloque (Réplica):", choices = names(vals$datos)) })
  output$cons_vars_ui <- renderUI({ req(vals$datos); selectizeInput("cons_vars", "Variables (Y):", choices = names(dplyr::select_if(vals$datos, is.numeric)), multiple = TRUE) })
  output$cons_exclude_ui <- renderUI({ req(input$cons_vars); selectizeInput("cons_exclude", "Variables SIN letras:", choices = input$cons_vars, multiple = TRUE) })
  
  # =====================================================================
  # --- SERVER CONSOLIDADOS (MOTOR CLÁSICO ORIGINAL RESTAURADO) ---
  # =====================================================================
  output$cons_factores_ui <- renderUI({ 
    req(vals$datos)
    if(input$cons_tipo == "simple") { selectInput("cons_trat", "Tratamiento Principal:", choices = names(vals$datos)) } 
    else { selectizeInput("cons_trat", "Factores (Seleccione varios):", choices = names(vals$datos), multiple = TRUE) } 
  })
  output$cons_amb_ui <- renderUI({ req(vals$datos); selectInput("cons_amb", "Ambiente (Finca/Zafra):", choices = names(vals$datos)) })
  output$cons_blk_ui <- renderUI({ req(vals$datos); selectInput("cons_blk", "Bloque (Réplica):", choices = names(vals$datos)) })
  output$cons_vars_ui <- renderUI({ req(vals$datos); selectizeInput("cons_vars", "Variables (Y):", choices = names(dplyr::select_if(vals$datos, is.numeric)), multiple = TRUE) })
  output$cons_exclude_ui <- renderUI({ req(input$cons_vars); selectizeInput("cons_exclude", "Variables SIN letras:", choices = input$cons_vars, multiple = TRUE) })
  
  observeEvent(input$run_cons, {
    req(vals$datos, input$cons_trat, input$cons_amb, input$cons_blk, input$cons_vars)
    if(length(input$cons_trat) == 0) return()
    
    withProgress(message = 'Procesando Motor Clásico...', value = 0, {
      
      datos <- vals$datos
      tipo_experimento  <- input$cons_tipo
      factores          <- input$cons_trat
      bloque            <- input$cons_blk
      variables_resp    <- input$cons_vars
      variables_sin_letras <- input$cons_exclude
      prueba_medias     <- input$cons_pmm
      modo_letras       <- input$cons_modo_letras
      fincas_por_tabla  <- input$cons_fincas_tabla
      columna_finca     <- input$cons_amb
      nivel_sig         <- input$cons_alpha
      
      # Conversión a factor
      cols_a_factor <- c(factores, bloque, columna_finca)
      datos <- datos %>% mutate(across(all_of(cols_a_factor), as.factor))
      
      # =========================================================================
      # 1. PROCESAMIENTO POR AMBIENTE (TU SCRIPT ORIGINAL INTACTO)
      # =========================================================================
      fincas_unicas <- unique(datos[[columna_finca]])
      resultados_totales <- list()
      
      for (finca_actual in fincas_unicas) {
        df_finca <- datos %>% dplyr::filter(!!sym(columna_finca) == finca_actual) %>% droplevels()
        if (nrow(df_finca) == 0) next
        
        factores_validos <- purrr::keep(factores, ~ length(unique(na.omit(df_finca[[.x]]))) >= 2)
        if (length(unique(na.omit(df_finca[[bloque]]))) < 2 || length(factores_validos) == 0) next
        if (tipo_experimento != "factorial") factores_validos <- factores_validos[1]
        
        df_finca[[bloque]] <- as.factor(df_finca[[bloque]])
        for (fac in factores_validos) df_finca[[fac]] <- as.factor(df_finca[[fac]])
        
        grid_tratamientos <- df_finca %>% 
          dplyr::select(dplyr::all_of(factores_validos)) %>% 
          dplyr::distinct() %>% 
          dplyr::arrange(dplyr::across(dplyr::all_of(factores_validos)))
        
        tabla_finca <- grid_tratamientos
        p_valores_lista <- list()
        nombres_p_guardados <- c()
        
        for (var in variables_resp) {
          resultado_variable <- tryCatch({
            
            if (var(df_finca[[var]], na.rm = TRUE) == 0) stop("Varianza cero")
            
            termino_factores <- paste(factores_validos, collapse = " * ")
            formula_mod <- as.formula(paste(var, "~", bloque, "+", termino_factores))
            
            modelo <- aov(formula_mod, data = df_finca)
            anova_res <- anova(modelo)
            
            # P-valores
            terminos_anova <- rownames(anova_res)[!rownames(anova_res) %in% c(bloque, "Residuals")]
            p_vals_fmt <- c()
            for (term in terminos_anova) {
              p_val <- anova_res[term, "Pr(>F)"]
              nombre_p <- paste0("P-valor (", trimws(term), ")")
              if (!nombre_p %in% nombres_p_guardados) nombres_p_guardados <- c(nombres_p_guardados, nombre_p)
              p_vals_fmt[nombre_p] <- ifelse(!is.na(p_val) && p_val < 0.01, "<0.01*", 
                                             ifelse(!is.na(p_val) && p_val <= nivel_sig, paste0(sprintf("%.2f", p_val), "*"), 
                                                    ifelse(!is.na(p_val), sprintf("%.2f", p_val), "NA")))
            }
            
            # Lógica Original de Selección de Tratamiento
            interaccion_max <- paste(factores_validos, collapse = ":")
            if (modo_letras == "auto") {
              p_int_max <- if(interaccion_max %in% rownames(anova_res)) anova_res[interaccion_max, "Pr(>F)"] else 1
              if (!is.na(p_int_max) && p_int_max <= nivel_sig) {
                trt_prueba <- factores_validos
              } else {
                p_principales <- anova_res[factores_validos, "Pr(>F)"]
                trt_prueba <- if (any(p_principales <= nivel_sig, na.rm = TRUE)) factores_validos[which.min(p_principales)] else factores_validos
              }
            } else {
              trt_prueba <- if(modo_letras == "interaccion") factores_validos else unlist(strsplit(modo_letras, ":"))
            }
            
            # Prueba de Medias Original
            if (prueba_medias == "Tukey") {
              prueba <- HSD.test(modelo, trt_prueba, alpha = nivel_sig, group = TRUE)
            } else {
              prueba <- LSD.test(modelo, trt_prueba, alpha = nivel_sig, group = TRUE)
            }
            
            grupos <- prueba$groups
            grupos <- grupos[order(grupos[[var]], decreasing = TRUE), ]
            grupos$Letras <- toupper(trimws(as.character(grupos$groups)))
            
            incluir_letras <- !(var %in% variables_sin_letras)
            
            # Unión de Resultados Original
            if (length(trt_prueba) > 1) {
              grupos_sep <- grupos %>%
                tibble::rownames_to_column("Tratamiento") %>%
                tidyr::separate(Tratamiento, into = trt_prueba, sep = ":") %>%
                dplyr::mutate(dplyr::across(dplyr::all_of(trt_prueba), as.factor))
              grupos_sep$Media <- round(grupos_sep[[var]], 2)
              res_var <- grid_tratamientos %>% dplyr::left_join(grupos_sep, by = trt_prueba)
              val_final <- if (incluir_letras) paste(res_var$Media, res_var$Letras) else as.character(res_var$Media)
            } else {
              grupos_sep <- grupos %>%
                tibble::rownames_to_column(trt_prueba) %>%
                dplyr::mutate(!!sym(trt_prueba) := as.factor(!!sym(trt_prueba)))
              res_var <- aggregate(as.formula(paste(var, "~", paste(factores_validos, collapse = "+"))), data = df_finca, FUN = mean)
              res_var[[var]] <- round(res_var[[var]], 2)
              res_var <- res_var %>% dplyr::left_join(grupos_sep %>% dplyr::select(dplyr::all_of(trt_prueba), Letras), by = trt_prueba)
              val_final <- if (incluir_letras) paste(res_var[[var]], res_var$Letras) else as.character(res_var[[var]])
            }
            
            list(p_vals = p_vals_fmt, datos_col = val_final)
            
          }, error = function(e) {
            list(p_vals = setNames(rep("Error", length(factores_validos)), paste0("P-valor (", factores_validos, ")")), datos_col = rep("N.A", nrow(grid_tratamientos)))
          })
          
          p_valores_lista[[var]] <- resultado_variable$p_vals
          tabla_finca[[var]] <- resultado_variable$datos_col
        }
        
        # Filas P-valores
        if(length(nombres_p_guardados) > 0) {
          filas_p <- data.frame(matrix("", ncol = length(factores_validos), nrow = length(nombres_p_guardados)))
          colnames(filas_p) <- factores_validos
          filas_p[[factores_validos[1]]] <- nombres_p_guardados
          for (var in variables_resp) { 
            for(np in nombres_p_guardados) {
              val_p <- p_valores_lista[[var]][np]
              filas_p[filas_p[[factores_validos[1]]] == np, var] <- ifelse(is.null(val_p) || is.na(val_p), "NA", val_p)
            }
          }
          tabla_finca <- dplyr::bind_rows(tabla_finca %>% dplyr::mutate(across(everything(), as.character)), filas_p %>% dplyr::mutate(across(everything(), as.character)))
        }
        
        tabla_finca <- tabla_finca %>% dplyr::mutate(Finca = ifelse(dplyr::row_number() == 1, finca_actual, "")) %>% dplyr::select(Finca, dplyr::everything())
        resultados_totales[[finca_actual]] <- tabla_finca
      }
      
      # =================================================================
      # CHUNKING DE TABLAS (REPARADO: SIN USAR CUT)
      # =================================================================
      fincas_procesadas <- names(resultados_totales)
      tablas_chunked <- list()
      if (length(fincas_procesadas) > 0) {
        # División matemática segura (a prueba de 1 solo elemento)
        indices_chunks <- ceiling(seq_along(fincas_procesadas) / fincas_por_tabla)
        chunks_fincas <- split(fincas_procesadas, indices_chunks)
        
        for (i in seq_along(chunks_fincas)) { 
          tablas_chunked[[paste0("Tabla_Resultados_", i)]] <- as.data.frame(dplyr::bind_rows(resultados_totales[chunks_fincas[[i]]])) 
        }
      }
      vals$cons_tablas_ambiente <- tablas_chunked
      incProgress(0.5)
      
      # =========================================================================
      # 2. ANÁLISIS GLOBAL DETALLADO (Para la pestaña de Detalle)
      # =========================================================================
      txt_anova_glb <- ""
      txt_sup_glb <- ""
      lista_graficos_sup <- list()
      trat_form <- paste(factores, collapse = "*")
      
      for (var in variables_resp) {
        df_c <- datos[complete.cases(datos[, c(var, factores, columna_finca, bloque)]), ]
        if(nrow(df_c) < 2) next
        
        form_aov <- as.formula(paste(var, "~", trat_form, "+", columna_finca, "+ Error(", bloque, "%in%", columna_finca, ") +", trat_form, ":", columna_finca))
        mod_aov <- tryCatch(aov(form_aov, data = df_c), error = function(e) NULL)
        
        txt_anova_glb <- paste0(txt_anova_glb, "\n========================================\n ANDEVA GLOBAL (", var, ")\n========================================\n")
        if(!is.null(mod_aov)) { txt_anova_glb <- paste0(txt_anova_glb, paste(capture.output(summary(mod_aov)), collapse="\n"), "\n") }
        
        form_sup <- as.formula(paste(var, "~", trat_form, "*", columna_finca, "+", bloque, "/", columna_finca))
        mod_sup <- tryCatch(aov(form_sup, data = df_c), error = function(e) NULL)
        
        txt_sup_glb <- paste0(txt_sup_glb, "\n--- SHAPIRO-WILK: ", toupper(var), " ---\n")
        if(!is.null(mod_sup)) {
          shap <- tryCatch(shapiro.test(resid(mod_sup)), error=function(e) NULL)
          if(!is.null(shap)) { txt_sup_glb <- paste0(txt_sup_glb, "W = ", round(shap$statistic, 4), ", p-value = ", signif(shap$p.value, 4), "\n") }
          lista_graficos_sup[[var]] <- mod_sup
        }
      }
      
      vals$cons_anova_txt <- txt_anova_glb
      vals$cons_shapiro_txt <- txt_sup_glb
      vals$cons_graficos_sup <- lista_graficos_sup
    })
  })
  
  output$cons_tablas_ambiente_ui <- renderUI({
    req(vals$cons_tablas_ambiente); if(length(vals$cons_tablas_ambiente) == 0) return(h4("No hay datos que procesar."))
    ui_elems <- lapply(names(vals$cons_tablas_ambiente), function(nom) {
      id_name <- paste0("dt_amb_", gsub("[^A-Za-z0-9]", "", nom))
      output[[id_name]] <- renderDT({ datatable(vals$cons_tablas_ambiente[[nom]], extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('copy', 'excel'), scrollX = TRUE, pageLength = 50), rownames = FALSE, class = "cell-border stripe") })
      tagList(h5(strong(nom)), DTOutput(id_name), hr(), br())
    })
    do.call(tagList, ui_elems)
  })
  
  output$cons_anova_txt <- renderPrint({ req(vals$cons_anova_txt); cat(vals$cons_anova_txt) })
  output$cons_shapiro_txt <- renderPrint({ req(vals$cons_shapiro_txt); cat(vals$cons_shapiro_txt) })
  
  output$cons_supuestos_plots <- renderUI({
    req(vals$cons_graficos_sup); if(length(vals$cons_graficos_sup) == 0) return(NULL)
    plot_elems <- lapply(names(vals$cons_graficos_sup), function(var) {
      id_plot <- paste0("plot_sup_cons_", gsub("[^A-Za-z0-9]", "", var))
      output[[id_plot]] <- renderPlot({ mod <- vals$cons_graficos_sup[[var]]; par(mfrow=c(1,2)); plot(mod, 1, main=paste("Residuos vs Pred -", var)); plot(mod, 2, main=paste("QQ Plot -", var)) })
      tagList(h4(paste("Variable:", var)), plotOutput(id_plot, height="400px"), br())
    })
    do.call(tagList, plot_elems)
  })
  
  
  # =====================================================================
  # --- MODELOS MIXTOS (LMM AVANZADO CON NLME Y DHARMA COMPLETO) ---
  # =====================================================================
  output$lmm_vars_ui <- renderUI({ 
    req(vals$datos); vars <- names(vals$datos)
    tagList(
      selectInput("lmm_y", "Variable Respuesta (Y):", choices = c("Seleccione..."="", vars)), 
      textInput("lmm_fixed", "Efectos Fijos (Ej: Tratamiento * Zafra):", value = ""), 
      textInput("lmm_random", "Efectos Aleatorios (Ej: 1|Replica):", value = "1|Replica"),
      hr(),
      HTML("<div style='background-color:#e8f4f8; padding:10px; border-radius:5px; font-size:13px;'>
             <b>GUÍA DE INTERPRETACIÓN DE SUPUESTOS (DHARMa):</b><br>
             <b>• Uniformidad:</b> Evalúa si los residuos siguen la distribución esperada (Normalidad simulada).<br>
             <b>• Dispersión Global:</b> Compara la varianza total contra la media esperada del modelo.<br>
             <b>• Homocedasticidad (Boxplot):</b> Si el tercer gráfico se pinta de <b>ROJO</b>, la varianza es diferente entre los tratamientos. Usa los métodos de corrección (Ej. varPower o varConstPower) para homogeneizar.<br>
           </div>")
    ) 
  })
  
  output$lmm_varstrato_ui <- renderUI({ 
    req(vals$datos)
    tagList(
      selectInput("lmm_varcor", "Método de Homogeneización de Varianza:", choices=c("Ninguna (Asumir Homocedasticidad)"="none", "varIdent (Varianza distinta por cada grupo)"="varIdent", "varPower (Varianza aumenta con la media)"="varPower", "varExp (Aumento exponencial)"="varExp", "varConstPower (Constante + Potencia, ideal si hay valores cercanos a cero)"="varConstPower")),
      selectInput("lmm_strato", "Factor para agrupar Varianza y evaluar DHARMa (Ej: Tratamiento):", choices = c("Seleccione..."="", names(vals$datos)))
    )
  })
  
  observeEvent(input$run_lmm, {
    req(vals$datos, input$lmm_y, input$lmm_fixed, input$lmm_random)
    df <- vals$datos; y_var <- input$lmm_y; fix_form <- input$lmm_fixed; ran_form <- input$lmm_random
    df[[y_var]] <- as.numeric(as.character(df[[y_var]])); df <- drop_na(df, all_of(y_var))
    
    tryCatch({
      formula_fija <- as.formula(paste(y_var, "~", fix_form))
      formula_aleatoria <- as.formula(paste("~", ran_form))
      
      # Inyección de Pesos (Modelado de Varianza Residual)
      if(input$lmm_varcor == "none") {
        mod_lmm <- nlme::lme(formula_fija, random = formula_aleatoria, data = df, na.action = na.omit)
      } else {
        req(input$lmm_strato); estr <- input$lmm_strato
        df[[estr]] <- as.factor(df[[estr]])
        if(input$lmm_varcor == "varIdent") pesos <- nlme::varIdent(form = as.formula(paste("~1|", estr)))
        if(input$lmm_varcor == "varPower") pesos <- nlme::varPower(form = as.formula(paste("~1|", estr)))
        if(input$lmm_varcor == "varExp") pesos <- nlme::varExp(form = as.formula(paste("~1|", estr)))
        if(input$lmm_varcor == "varConstPower") pesos <- nlme::varConstPower(form = as.formula(paste("~1|", estr)))
        mod_lmm <- nlme::lme(formula_fija, random = formula_aleatoria, weights = pesos, data = df, na.action = na.omit)
      }
      
      output$lmm_diag <- renderPrint({ 
        cat("===================================================\n")
        cat("      RESUMEN DEL MODELO MIXTO (Efectos y Ajuste)\n")
        cat("===================================================\n\n")
        cat("--- 1. MÉTRICAS DE CALIDAD GLOBAL ---\n")
        cat(sprintf("AIC: %.2f  |  BIC: %.2f  |  Log-Verosimilitud: %.2f\n", AIC(mod_lmm), BIC(mod_lmm), logLik(mod_lmm)))
        cat("(Nota: Modelos con menor AIC/BIC indican un mejor ajuste)\n\n")
        
        cat("--- 2. VARIANZA EXPLICADA (Efectos Aleatorios) ---\n")
        print(nlme::VarCorr(mod_lmm))
        
        cat("\n--- 3. SIGNIFICANCIA DE LOS COEFICIENTES FIJOS ---\n")
        print(round(summary(mod_lmm)$tTable, 4))
      })
      
      output$lmm_anova <- renderPrint({ 
        cat("===================================================\n")
        cat("      ANDEVA (Prueba F de Wald para Efectos Fijos)\n")
        cat("===================================================\n\n")
        print(anova(mod_lmm)) 
      })
      
      output$lmm_supuestos <- renderPrint({ 
        cat("===================================================\n")
        cat("   PRUEBAS FORMALES DE SUPUESTOS (Análisis DHARMa)\n")
        cat("===================================================\n\n")
        tryCatch({
          sim_res <- DHARMa::simulateResiduals(fittedModel = mod_lmm, plot = FALSE)
          unif <- DHARMa::testUniformity(sim_res, plot = FALSE)
          disp <- DHARMa::testDispersion(sim_res, plot = FALSE)
          outl <- DHARMa::testOutliers(sim_res, plot = FALSE)
          
          cat("1. UNIFORMIDAD (Equivalente a Normalidad de los Residuos):\n")
          cat(sprintf("   P-valor = %.4f  -->  %s\n\n", unif$p.value, ifelse(unif$p.value < 0.05, "[ALERTA] Los residuos se desvían de la distribución teórica esperada.", "[OK] Distribución uniforme/normal adecuada.")))
          
          cat("2. DISPERSIÓN GLOBAL:\n")
          cat(sprintf("   P-valor = %.4f  -->  %s\n\n", disp$p.value, ifelse(disp$p.value < 0.05, "[ALERTA] Hay sobre/subdispersión global.", "[OK] La varianza global del modelo es estable.")))
          
          cat("3. VALORES ATÍPICOS EXTREMOS (Outliers):\n")
          cat(sprintf("   P-valor = %.4f  -->  %s\n\n", outl$p.value, ifelse(outl$p.value < 0.05, "[ALERTA] Existen outliers significativos que distorsionan el modelo.", "[OK] No hay outliers problemáticos.")))
          
          if(!is.null(input$lmm_strato) && input$lmm_strato != "") {
            cat("4. HOMOCEDASTICIDAD POR GRUPO (Prueba de Levene simulada):\n")
            cat_pred <- as.factor(df[[input$lmm_strato]])
            lev_test <- DHARMa::testCategorical(sim_res, catPred = cat_pred, plot = FALSE)
            cat(sprintf("   P-valor = %.4f  -->  %s\n", lev_test$p.value, ifelse(lev_test$p.value < 0.05, "[ALERTA] Heterocedasticidad. Las varianzas difieren entre los grupos (Usa métodos de corrección).", "[OK] Las varianzas son homogéneas entre los grupos.")))
          }
          
        }, error = function(e) cat("Aviso: La estructura del modelo es demasiado compleja para calcular las pruebas matemáticas de DHARMa automáticamente.\n"))
        
        p_shap <- tryCatch(shapiro.test(resid(mod_lmm, type="pearson"))$p.value, error=function(e) NA)
        cat(sprintf("\n--- PRUEBA CLÁSICA ADICIONAL ---\nNormalidad de Residuos de Pearson (Shapiro-Wilk): P-valor = %.4f %s\n", p_shap, ifelse(!is.na(p_shap) && p_shap < 0.05, "[NO NORMAL]", "[NORMAL]")))
      })
      
      # Gráficos Completos DHARMa (Los 3 paneles)
      output$lmm_plots_dharma <- renderPlot({ 
        tryCatch({
          sim_res <- DHARMa::simulateResiduals(fittedModel = mod_lmm, plot = FALSE)
          
          par(mfrow=c(1,3), mar=c(4,4,3,1), oma=c(0,0,2,0))
          
          # 1. QQ Plot (Normalidad/Uniformidad)
          DHARMa::plotQQunif(sim_res)
          
          # 2. Residuos vs Predichos (Dispersión General)
          DHARMa::plotResiduals(sim_res)
          
          # 3. Boxplot de Levene (Homocedasticidad por factor)
          if(!is.null(input$lmm_strato) && input$lmm_strato != "") {
            factor_grafico <- as.factor(df[[input$lmm_strato]])
            DHARMa::plotResiduals(sim_res, form = factor_grafico, main = paste("Boxplots vs", input$lmm_strato))
          } else {
            # Si no selecciona factor, dibujamos un histograma simple para llenar el panel
            hist(sim_res$scaledResiduals, breaks=20, col="steelblue", main="Histograma de Residuos", xlab="Residuos DHARMa escalados", border="white")
          }
          
          mtext("Diagnóstico de Supuestos DHARMa", outer=TRUE, cex=1.5, font=2)
          vals$p_lmm_sup <- recordPlot()
          
        }, error = function(e) {
          # PLAN B: Si DHARMa falla por pesos extremos, grafica Pearson
          df_diag <- drop_na(data.frame(Fitted = fitted(mod_lmm), Residuos = resid(mod_lmm, type="pearson")))
          par(mfrow=c(1,3), mar=c(4,4,3,1), oma=c(0,0,2,0))
          
          qqnorm(df_diag$Residuos, main="Normalidad (QQ-Plot Pearson)", col="darkblue"); qqline(df_diag$Residuos, col="red", lwd=2)
          plot(df_diag$Fitted, df_diag$Residuos, main="Dispersión Global (Pearson)", col="darkblue", pch=16, xlab="Valores Predichos", ylab="Residuos Estandarizados")
          abline(h=0, col="red", lty=2, lwd=2)
          
          if(!is.null(input$lmm_strato) && input$lmm_strato != "") {
            boxplot(df_diag$Residuos ~ df[[input$lmm_strato]], col="steelblue", main=paste("Boxplot vs", input$lmm_strato), ylab="Residuos Pearson", xlab=input$lmm_strato)
            abline(h=0, col="red", lty=2, lwd=2)
          }
          
          mtext("Diagnóstico Alternativo de Pearson", outer=TRUE, cex=1.5, font=2)
          vals$p_lmm_sup <- recordPlot()
        })
      })
      
      output$lmm_blups_txt <- renderPrint({ cat("--- Efectos Aleatorios (BLUPs) ---\n"); print(nlme::ranef(mod_lmm)) })
      output$lmm_blups_plot <- renderPlot({ 
        re <- nlme::ranef(mod_lmm); df_re <- data.frame(Nivel = rownames(re), Efecto = re[,1])
        p <- ggplot(df_re, aes(x=reorder(Nivel, Efecto), y=Efecto)) + geom_bar(stat="identity", fill="steelblue", alpha=0.8) + coord_flip() + theme_classic(base_size=14) + labs(title="BLUPs (Efectos Aleatorios)", x="Nivel", y="Desviación de la Media Global")
        vals$p_lmm_blups <- p; print(p)
      }, res = 110)
      
    }, error = function(e) { showNotification(paste("Error en LMM:", e$message), type="error") })
  })
  
  observeEvent(input$btn_pop_lmm_sup, { req(vals$p_lmm_sup); if(interactive()){ dev.new(noRStudioGD=T, width=15, height=5); replayPlot(vals$p_lmm_sup) } })
  observeEvent(input$btn_pop_lmm_blups, { req(vals$p_lmm_blups); if(interactive()){ dev.new(noRStudioGD=T); print(vals$p_lmm_blups) } })
  
  # =====================================================================
  # --- ANÁLISIS DE PLAGAS (MLG / MLM) ---
  # =====================================================================
  output$mlg_vars_ui <- renderUI({ 
    req(vals$datos); vars <- names(vals$datos); 
    tagList(
      selectInput("mlg_y", "Variable Respuesta (Conteo/Proporción):", choices = c("Seleccione..."="", vars)), 
      textInput("mlg_fixed", "Efectos Fijos Principales (Ej: Tratamiento):", value = ""),
      selectInput("mlg_blk", "Bloque / Réplica (Opcional):", choices = c("Ninguno"="", vars))
    ) 
  })
  
  output$mlg_term_ui <- renderUI({ req(input$mlg_fixed); textInput("mlg_term", "Término para Gráficos DHARMa y Post-Hoc:", value = input$mlg_fixed) })
  
  observeEvent(input$run_mlg, {
    req(vals$datos, input$mlg_y, input$mlg_fixed)
    df <- vals$datos; y_var <- input$mlg_y; fix_form <- input$mlg_fixed; blk <- input$mlg_blk
    df[[y_var]] <- as.numeric(as.character(df[[y_var]]))
    
    if(!is.null(blk) && blk != "") {
      df[[blk]] <- as.factor(df[[blk]])
      df <- drop_na(df, all_of(c(y_var, blk)))
    } else {
      df <- drop_na(df, all_of(y_var))
    }
    
    tryCatch({
      if(!is.null(blk) && blk != "") { formula_mlg <- as.formula(paste(y_var, "~", blk, "+", fix_form)) } 
      else { formula_mlg <- as.formula(paste(y_var, "~", fix_form)) }
      
      # DIAGNÓSTICO ESTADÍSTICO DE LA VARIABLE
      output$mlg_diagnostico <- renderPrint({
        y_vec <- df[[y_var]]
        media_val <- mean(y_vec, na.rm=TRUE)
        var_val <- var(y_vec, na.rm=TRUE)
        ratio_disp <- var_val / media_val
        ceros_pct <- (sum(y_vec == 0, na.rm=TRUE) / length(y_vec)) * 100
        
        cat("===================================================\n")
        cat("      DIAGNÓSTICO ESTADÍSTICO DE LA VARIABLE Y\n")
        cat("===================================================\n\n")
        cat(sprintf("Variable evaluada: %s\n", y_var))
        cat(sprintf("Media: %.4f\n", media_val))
        cat(sprintf("Varianza: %.4f\n", var_val))
        cat(sprintf("Ratio Varianza/Media (Dispersión): %.4f\n", ratio_disp))
        cat(sprintf("Porcentaje de Ceros: %.2f%%\n\n", ceros_pct))
        
        cat("--- RECOMENDACIÓN DE MODELADO ---\n")
        if (ratio_disp > 1.5) {
          cat("⚠️ ALERTA: Fuerte SOBREDISPERSIÓN detectada (Varianza >> Media).\n")
          cat("-> Sugerencia: Utiliza la familia 'Binomial Negativa' o 'Quasi-Poisson'.\n")
        } else if (ratio_disp < 0.8) {
          cat("⚠️ ALERTA: SUBDISPERSIÓN detectada (Varianza < Media).\n")
          cat("-> Sugerencia: Utiliza la familia 'Quasi-Poisson'.\n")
        } else {
          cat("✅ Dispersión equilibrada (Varianza ~ Media).\n")
          cat("-> Sugerencia: La familia 'Poisson' estándar debería ajustarse bien.\n")
        }
        if (ceros_pct > 20) cat("\n⚠️ ALTA CANTIDAD DE CEROS. Atento a la prueba DHARMa (Zero-Inflation).\n")
      })
      
      txt_diag <- ""; txt_anova <- ""; res_mods <- list()
      
      for(fam in input$mlg_family) {
        if(fam == "poisson") mod <- glm(formula_mlg, family = poisson(link="log"), data = df)
        if(fam == "quasipoisson") mod <- glm(formula_mlg, family = quasipoisson(link="log"), data = df)
        if(fam == "binomial_negativa") mod <- MASS::glm.nb(formula_mlg, data = df)
        if(fam == "quasibinomial") mod <- glm(formula_mlg, family = quasibinomial(link="logit"), data = df)
        
        txt_diag <- paste0(txt_diag, "\n=== Resumen de Coeficientes: ", toupper(fam), " ===\n", paste(capture.output(summary(mod)$coefficients), collapse="\n"), "\n")
        txt_anova <- paste0(txt_anova, "\n=== ANDEVA (Devianza): ", toupper(fam), " ===\n", paste(capture.output(anova(mod, test="Chisq")), collapse="\n"), "\n")
        res_mods[[fam]] <- mod
      }
      
      output$mlg_anova <- renderPrint({ cat(txt_diag); cat("\n"); cat(txt_anova) })
      
      if(length(res_mods) > 0) {
        mod_sel <- res_mods[[1]] 
        
        output$mlg_supuestos <- renderPrint({ 
          cat("===================================================\n")
          cat("    INTERPRETACIÓN DE SUPUESTOS DHARMa (1er Modelo)\n")
          cat("===================================================\n\n")
          sim_res <- DHARMa::simulateResiduals(fittedModel = mod_sel, plot = FALSE)
          disp <- DHARMa::testDispersion(sim_res, plot = FALSE)
          zin <- DHARMa::testZeroInflation(sim_res, plot = FALSE)
          out <- DHARMa::testOutliers(sim_res, plot = FALSE)
          
          cat(sprintf("1. Prueba de Dispersión: p-valor = %.4f %s\n", disp$p.value, ifelse(disp$p.value < 0.05, "[FALLA]", "[OK]")))
          cat(sprintf("2. Exceso de Ceros: p-valor = %.4f %s\n", zin$p.value, ifelse(zin$p.value < 0.05, "[FALLA]", "[OK]")))
          cat(sprintf("3. Valores Atípicos: p-valor = %.4f %s\n", out$p.value, ifelse(out$p.value < 0.05, "[FALLA]", "[OK]")))
        })
        
        # AQUÍ ESTÁN TUS BOXPLOTS DE LEVENE (DHARMa clásico)
        output$mlg_plot_dharma <- renderPlot({ 
          req(input$mlg_term)
          sim_res <- DHARMa::simulateResiduals(fittedModel = mod_sel, plot = FALSE)
          par(mfrow=c(1,3), mar=c(4,4,4,2), oma=c(0,0,2,0))
          
          DHARMa::plotQQunif(sim_res)
          DHARMa::plotResiduals(sim_res) # Dispersion general
          
          # Boxplots por Tratamiento (Si falla Levene, se pinta rojo)
          term_factor <- as.factor(df[[input$mlg_term]])
          DHARMa::plotResiduals(sim_res, form = term_factor, main = paste("Boxplots vs", input$mlg_term))
          
          mtext(paste("DHARMa: Familia", toupper(input$mlg_family[1])), outer=TRUE, cex=1.5, font=2)
          vals$p_mlg_sup <- recordPlot() 
        })
        
        output$mlg_pmm_txt <- renderPrint({
          req(input$mlg_term)
          tryCatch({
            em <- emmeans::emmeans(mod_sel, as.formula(paste("~", input$mlg_term)), type = "response")
            cld_res <- multcomp::cld(em, alpha = input$mlg_alpha, Letters = letters, reversed = TRUE)
            cat("--- Medias Estimadas (Back-transformed) ---\n"); print(cld_res)
            
            df_cld <- as.data.frame(cld_res); df_cld$Letras <- toupper(trimws(df_cld$.group))
            y_col <- if("response" %in% names(df_cld)) "response" else if("prob" %in% names(df_cld)) "prob" else "emmean"
            
            p <- ggplot(df_cld, aes_string(x = input$mlg_term, y = y_col)) + geom_bar(stat="identity", fill="#2c3e50", alpha=0.85) + geom_errorbar(aes(ymin=as.numeric(as.character(asymp.LCL)), ymax=as.numeric(as.character(asymp.UCL))), width=0.2) + geom_text(aes(label=Letras, y=as.numeric(as.character(asymp.UCL))), vjust=-0.5, size=5, fontface="bold") + theme_classic(base_size=15) + labs(title="Medias Estimadas y Significancia", y="Respuesta")
            vals$p_mlg_pmm <- p
            output$mlg_pmm_plot <- renderPlot({ p })
          }, error = function(e) cat("Error en cálculo de medias:", e$message))
        })
      }
    }, error = function(e) { showNotification(paste("Error en MLG:", e$message), type="error") })
  })
  
  observeEvent(input$btn_pop_mlg_sup, { req(vals$p_mlg_sup); if(interactive()){ dev.new(noRStudioGD=T, width=14, height=5); replayPlot(vals$p_mlg_sup) } })
  observeEvent(input$btn_pop_mlg_pmm, { req(vals$p_mlg_pmm); if(interactive()){ dev.new(noRStudioGD=T); print(vals$p_mlg_pmm) } })
  
  # =====================================================================
  # --- PLAGAS: DINÁMICA POBLACIONAL (GRÁFICO NÍTIDO Y CONTROLABLE) ---
  # =====================================================================
  output$dyn_vars_ui <- renderUI({ 
    req(vals$datos); vars <- names(vals$datos)
    tagList(
      selectInput("dyn_time", "Variable Temporal (Ej: Semanas, Días):", choices = c("Seleccione..."="", vars)), 
      selectInput("dyn_y", "Densidad/Conteo de Plaga (Y):", choices = c("Seleccione..."="", vars)), 
      selectInput("dyn_trt", "Agrupar por (Ej: Tratamiento):", choices = c("Seleccione..."="", vars)),
      hr(),
      h4("Control de Ejes Y"),
      fluidRow(
        column(6, numericInput("dyn_ymin", "Límite Inferior (Min):", value = 0)),
        column(6, numericInput("dyn_ymax", "Límite Superior (Max):", value = 100))
      )
    ) 
  })
  
  observeEvent(input$run_dyn, {
    req(vals$datos, input$dyn_time, input$dyn_y, input$dyn_trt)
    df <- vals$datos
    t_var <- input$dyn_time
    y_var <- input$dyn_y
    trt_var <- input$dyn_trt
    
    df[[y_var]] <- as.numeric(as.character(df[[y_var]]))
    
    # Manejo de variable tiempo (texto o número)
    if(is.character(df[[t_var]]) || is.factor(df[[t_var]])) {
      df[[t_var]] <- factor(df[[t_var]], levels = unique(df[[t_var]]))
    }
    
    # Calcular medias sin errores (como pediste, gráfico más limpio)
    df_sum <- df %>% group_by(across(all_of(c(t_var, trt_var)))) %>% summarise(Media = mean(.data[[y_var]], na.rm=TRUE), .groups="drop")
    df_sum <- drop_na(df_sum)
    
    # Gráfico limpio, sin barras de error, líneas más gruesas y nítidas
    p <- ggplot(df_sum, aes_string(x = t_var, y = "Media", color = trt_var, group = trt_var)) + 
      geom_line(linewidth = 1.5) + 
      geom_point(size = 4, shape = 21, fill = "white", stroke = 2) + 
      scale_color_brewer(palette = input$dyn_palette) + 
      theme_classic(base_size=16) + 
      labs(title=input$dyn_title, x=t_var, y=y_var) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, face="bold"),
            legend.position = "right", 
            legend.title = element_text(face="bold"))
    
    if(input$show_ude) p <- p + geom_hline(yintercept=input$val_ude, color=input$col_ude, linetype="dashed", linewidth=1.2) + annotate("text", x=1, y=input$val_ude, label=input$label_ude, vjust=-0.8, hjust=0, color=input$col_ude, fontface="bold", size=5)
    if(input$show_nde) p <- p + geom_hline(yintercept=input$val_nde, color=input$col_nde, linetype="dotted", linewidth=1.2) + annotate("text", x=1, y=input$val_nde, label=input$label_nde, vjust=-0.8, hjust=0, color=input$col_nde, fontface="bold", size=5)
    
    # Control de zoom en el eje Y
    p <- p + coord_cartesian(ylim=c(input$dyn_ymin, input$dyn_ymax))
    
    vals$p_dyn <- p
    # RENDERIZADO EN ALTA DEFINICIÓN (res = 120 evita el pixelado)
    output$plot_dinamica <- renderPlot({ p }, res = 120)
  })
  observeEvent(input$btn_pop_dyn, { req(vals$p_dyn); if(interactive()){ dev.new(noRStudioGD=T); print(vals$p_dyn) } })
  
  # --- PLAGAS: ESTADÍOS ---
  output$est_vars_ui <- renderUI({ req(vals$datos); vars <- names(vals$datos); tagList(selectizeInput("est_y_vars", "Variables de Estadíos (Huevos, Larvas, etc.):", choices = names(dplyr::select_if(vals$datos, is.numeric)), multiple=TRUE), selectInput("est_trt", "Tratamiento:", choices = c("Seleccione..."="", vars)), selectInput("est_time", "Variable Temporal (Opcional para facetar):", choices = c("Ninguno"="", vars))) })
  
  observeEvent(input$run_est, {
    req(vals$datos, input$est_y_vars, input$est_trt)
    df <- vals$datos; vars_est <- input$est_y_vars; trt <- input$est_trt; tm <- input$est_time
    for(v in vars_est) df[[v]] <- as.numeric(as.character(df[[v]]))
    
    grp_cols <- c(trt); if(!is.null(tm) && tm != "") grp_cols <- c(grp_cols, tm)
    df_long <- df %>% dplyr::select(all_of(c(grp_cols, vars_est))) %>% pivot_longer(cols = all_of(vars_est), names_to = "Estadio", values_to = "Conteo")
    df_sum <- df_long %>% group_by(across(all_of(c(grp_cols, "Estadio")))) %>% summarise(Media = mean(Conteo, na.rm=TRUE), .groups="drop")
    
    p <- ggplot(df_sum, aes_string(x = trt, y = "Media", fill = "Estadio")) + geom_bar(stat="identity", position="stack", color="black", width=0.7) + scale_fill_viridis_d(option = input$est_palette) + theme_classic(base_size=15) + labs(title=paste("Distribución de Estadíos -", input$est_plaga_name), x=trt, y="Población Promedio") + theme(axis.text.x = element_text(angle=45, hjust=1))
    if(!is.null(tm) && tm != "") p <- p + facet_wrap(as.formula(paste("~", tm)))
    vals$p_est <- p
    output$plot_estadios <- renderPlot({ p })
  })
  observeEvent(input$btn_pop_est, { req(vals$p_est); if(interactive()){ dev.new(noRStudioGD=T); print(vals$p_est) } })
  
  # =====================================================================
  # --- TRANSFORMACIÓN Y ANCOVA CLÁSICO ---
  # =====================================================================
  output$trans_var_ui <- renderUI({ req(vals$datos); selectInput("trans_y", "Variable a Transformar:", choices = names(dplyr::select_if(vals$datos, is.numeric))) })
  output$anc_vars_ui <- renderUI({ req(vals$datos); vars <- names(vals$datos); tagList(selectInput("anc_y", "Variable Respuesta (Y):", choices=names(dplyr::select_if(vals$datos, is.numeric))), selectInput("anc_cov", "Covariable (X):", choices=names(dplyr::select_if(vals$datos, is.numeric))), selectInput("anc_trt", "Tratamiento (Factor):", choices=vars), conditionalPanel("input.anc_des == 'DBCA'", selectInput("anc_blk", "Bloque:", choices=vars)), conditionalPanel("input.anc_des == 'DCL'", selectInput("anc_row", "Fila:", choices=vars), selectInput("anc_col", "Columna:", choices=vars)) ) })
  
  observeEvent(input$run_trans, {
    req(vals$datos, input$trans_y)
    df <- vals$datos; y_var <- input$trans_y; y_val <- as.numeric(as.character(df[[y_var]]))
    nombre_nuevo <- paste0(y_var, "_", input$trans_method)
    
    tryCatch({
      if(input$trans_method == "log10") y_new <- log10(y_val + 1)
      if(input$trans_method == "ln") y_new <- log(y_val + 1)
      if(input$trans_method == "sqrt") y_new <- sqrt(y_val + 0.5)
      if(input$trans_method == "arcsin") y_new <- asin(sqrt(y_val / 100))
      if(input$trans_method == "boxcox") {
        bc <- MASS::boxcox(lm(y_val + 0.0001 ~ 1), plotit=FALSE); lambda <- bc$x[which.max(bc$y)]
        y_new <- if(lambda == 0) log(y_val + 0.0001) else (((y_val + 0.0001)^lambda) - 1) / lambda
        showNotification(paste("Lambda óptimo:", round(lambda, 3)), type="message")
      }
      
      df[[nombre_nuevo]] <- y_new; vals$datos <- df; refresh_trigger(refresh_trigger() + 1)
      showNotification(paste("Guardado como:", nombre_nuevo), type="message")
      
      output$trans_plot_orig <- renderPlot({ ggplot(df, aes(x=y_val)) + geom_histogram(fill="gray", color="black", bins=30) + theme_classic() + labs(title="Original", x=y_var) })
      output$trans_plot_new <- renderPlot({ ggplot(df, aes(x=y_new)) + geom_histogram(fill="#18bc9c", color="black", bins=30) + theme_classic() + labs(title=paste("Transformado (", input$trans_method, ")"), x=nombre_nuevo) })
      output$trans_shapiro <- renderPrint({ cat("--- Shapiro-Wilk Original ---\n"); print(tryCatch(shapiro.test(y_val), error=function(e)"Error")); cat("\n--- Shapiro-Wilk Transformado ---\n"); print(tryCatch(shapiro.test(y_new), error=function(e)"Error")) })
    }, error = function(e) { showNotification(paste("Error en transformación:", e$message), type="error") })
  })
  
  observeEvent(input$run_anc, {
    req(vals$datos, input$anc_y, input$anc_cov, input$anc_trt)
    df <- vals$datos; Y <- input$anc_y; X <- input$anc_cov; T <- input$anc_trt; des <- input$anc_des
    df_c <- df[complete.cases(df[, c(Y, X, T)]), ]; df_c[[Y]] <- as.numeric(as.character(df_c[[Y]])); df_c[[X]] <- as.numeric(as.character(df_c[[X]])); df_c[[T]] <- as.factor(df_c[[T]])
    if(des=="DBCA") df_c[[input$anc_blk]] <- as.factor(df_c[[input$anc_blk]]); if(des=="DCL") { df_c[[input$anc_row]] <- as.factor(df_c[[input$anc_row]]); df_c[[input$anc_col]] <- as.factor(df_c[[input$anc_col]]) }
    
    base_str <- if(des=="DBCA") paste0("+", input$anc_blk) else if(des=="DCL") paste0("+", input$anc_row, "+", input$anc_col) else ""
    form_hp <- as.formula(paste(Y, "~", T, "*", X, base_str))
    form_anc <- as.formula(paste(Y, "~", X, "+", T, base_str))
    
    mod_hp <- aov(form_hp, data=df_c); mod_anc <- aov(form_anc, data=df_c)
    
    output$anc_hp <- renderPrint({ cat("--- 1. Homogeneidad de Pendientes (Interacción Trat:Cov) ---\n"); print(anova(mod_hp)) })
    output$anc_anova <- renderPrint({ cat("--- 2. ANCOVA Clásico ---\n"); print(anova(mod_anc)) })
    output$anc_sup <- renderPrint({ cat("--- Normalidad (Shapiro-Wilk) ---\n"); print(shapiro.test(resid(mod_anc))) })
    
    if(input$anc_pmm != "none") {
      output$anc_emmeans <- renderPrint({ cat("--- Medias Ajustadas por la Covariable ---\n"); em <- emmeans::emmeans(mod_anc, as.formula(paste("~", T))); print(multcomp::cld(em, adjust=input$anc_pmm, Letters=letters, reversed=TRUE)) })
    } else { output$anc_emmeans <- renderPrint({ cat("Prueba de medias desactivada.\n") }) }
    
    p <- ggplot(df_c, aes_string(x=X, y=Y, color=T)) + geom_point(size=3, alpha=0.7) + geom_smooth(method="lm", se=FALSE, linewidth=1.2) + theme_classic(base_size=15) + labs(title="ANCOVA: Dispersión y Pendientes", x=paste("Covariable:", X), y=Y, color=T)
    vals$p_anc <- p
    output$anc_plot <- renderPlot({ p })
  })
  observeEvent(input$btn_pop_anc, { req(vals$p_anc); if(interactive()){ dev.new(noRStudioGD=T); print(vals$p_anc) } })
  
} # Cierre de la función server

# =====================================================================
# --- LANZAMIENTO DE LA APLICACIÓN ---
# =====================================================================
shinyApp(ui = ui, server = server)

