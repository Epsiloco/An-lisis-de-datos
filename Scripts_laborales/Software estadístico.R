# --- LIBRERÍAS ---
install.packages("shinythemes")
library(shiny)
library(shinythemes)
library(DT)
library(readxl)
library(openxlsx)
library(clipr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(dlookr)
library(mice)
library(VIM)
library(e1071)
library(modeest)
library(outliers)
library(ggridges)
library(EnvStats)
library(agricolae)
library(ScottKnott)
library(car)          
library(MASS)         
library(emmeans)      
library(multcompView) 
library(multcomp)
library(lmtest)
library(lme4)         # Para Modelos Mixtos
library(lmerTest)     # Para p-valores en lmer

# Definir la lista de paquetes necesarios
paquetes <- c(
  "shiny", "shinythemes", "DT", "readxl", "openxlsx", "clipr", 
  "dplyr", "tidyr", "ggplot2", "plotly", "dlookr", "mice", 
  "VIM", "e1071", "modeest", "outliers", "ggridges", "EnvStats", 
  "agricolae", "ScottKnott", "car", "MASS", "emmeans", 
  "multcompView", "multcomp", "lmtest", "lme4", "lmerTest"
)

# Identificar cuáles no están instalados aún
nuevos_paquetes <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]

# Instalar los que faltan
if(length(nuevos_paquetes)) {
  message(paste("Instalando los siguientes paquetes:", paste(nuevos_paquetes, collapse = ", ")))
  install.packages(nuevos_paquetes, dependencies = TRUE)
} else {
  message("Todos los paquetes ya están instalados.")
}

# Confirmación de carga (opcional, para verificar que todos funcionen)
# lapply(paquetes, library, character.only = TRUE)

# --- INTERFAZ DE USUARIO (UI) ---
ui <- navbarPage(
  title = "Software Estadístico Falcon V1.0",
  theme = shinytheme("flatly"),
  
  header = tags$head(
    tags$style(HTML("
      body { font-family: system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; background-color: #f8f9fa; }
      .navbar { background-color: #2c3e50 !important; border-bottom: 3px solid #3498db; }
      .navbar-brand { color: #ffffff !important; font-weight: 700; font-size: 1.35em; }
      .well { background-color: #ffffff; border: 1px solid #dee2e6; border-radius: 12px; box-shadow: 0 8px 25px rgba(0,0,0,0.08); padding: 20px; }
      .control-label { font-weight: 700; color: #2c3e50; font-size: 1.05em; }
      .shiny-plot-output { border: none; border-radius: 12px; box-shadow: 0 10px 30px rgba(0,0,0,0.1); background: #ffffff; padding: 15px; }
      .btn { border-radius: 8px; font-weight: 600; transition: all 0.3s ease; }
      .btn:hover { transform: translateY(-2px); box-shadow: 0 6px 15px rgba(0,0,0,0.2); }
      .btn-primary { background-color: #3498db; border-color: #3498db; }
      .btn-success { background-color: #27ae60; border-color: #27ae60; }
      .btn-warning { background-color: #f39c12; border-color: #f39c12; color: white !important; }
      .btn-danger { background-color: #e74c3c; border-color: #e74c3c; }
      .dataTable { border-radius: 8px; overflow: hidden; box-shadow: 0 4px 12px rgba(0,0,0,0.05); }
      .form-control { border-radius: 8px; }
      .nav-tabs > li > a { font-weight: 600; }
      .interpretation-box { background-color: #e8f4f8; border-left: 5px solid #3498db; padding: 15px; border-radius: 5px; margin-bottom: 20px; }
      .resizable-plot { resize: both; overflow: auto; border: 2px dashed #bdc3c7; border-radius: 8px; padding: 10px; background: white; min-height: 300px; min-width: 400px; position: relative; margin-bottom: 20px;}
      .resizable-plot::after { content: '↘ Arrastra aquí para ajustar tamaño'; position: absolute; bottom: 2px; right: 15px; font-size: 11px; color: #7f8c8d; pointer-events: none; }
    "))
  ),
  
  tabPanel("Carga de Datos",
           sidebarLayout(
             sidebarPanel(
               h4("Importación de Datos"),
               radioButtons("import_method", "Seleccione método:", choices = c("Importar Excel (.xlsx)" = "file", "Pegar desde Excel (Portapapeles)" = "clip")),
               conditionalPanel(condition = "input.import_method == 'file'", fileInput("file1", "Elegir archivo Excel", accept = ".xlsx"), uiOutput("select_sheet")),
               conditionalPanel(condition = "input.import_method == 'clip'", actionButton("paste_btn", "Pegar Datos", icon = icon("paste"), class = "btn-primary")),
               hr(), h5("Estado de la Base:"), verbatimTextOutput("data_status"),
               downloadButton("download_data", "Descargar Excel (Datos + Reporte)", class = "btn-success")
             ), mainPanel(DTOutput("tabla_visual", height = "600px"))
           )
  ),
  
  tabPanel("EDA",
           tabsetPanel(
             tabPanel("Distribución e Imputación",
                      sidebarLayout(sidebarPanel(h4("Distribución"), uiOutput("hist_var_ui"), radioButtons("hist_type", "Tipo:", choices=c("Densidad"="density", "Frecuencia"="count")), sliderInput("hist_bins", "Bins:", min=10, max=100, value=30), hr(), h4("Imputación"), selectInput("imp_method", "Método:", choices=c("Media"="mean", "Mediana"="median", "MICE"="pmm", "KNN"="knn", "CART"="cart")), actionButton("run_imputation", "Imputar", class="btn-warning")), mainPanel(plotlyOutput("plot_hist", height="450px"), plotlyOutput("plot_faltantes", height="450px")))
             ),
             tabPanel("Tratamiento de Atípicos",
                      sidebarLayout(sidebarPanel(h4("Filtro IQR"), uiOutput("eda_var_ui"), uiOutput("eda_group_ui"), hr(), radioButtons("outlier_action", "Acción:", choices=c("Identificar"="none", "NA"="NA", "Eliminar"="eliminar")), actionButton("apply_outliers", "EJECUTAR", class="btn-danger"), hr(), selectInput("box_col", "Color:", choices=c("lightgreen", "lightblue", "white", "orange"))), mainPanel(plotlyOutput("plot_boxplot_eda", height="550px"), h4("Control por Grupo"), DTOutput("tabla_iqr_detallada")))
             )
           )
  ),
  
  tabPanel("Estadística Descriptiva",
           sidebarLayout(
             sidebarPanel(
               h4("Análisis Descriptivo"), uiOutput("desc_var_ui"), uiOutput("x_group_ui"), radioButtons("stats_level", "Nivel:", choices=c("Global"="overall", "Por grupos"="grouped")), hr(),
               selectInput("plot_type", "Gráfico:", choices=c("Boxplot Individual", "Boxplot por grupos", "Violin Plot", "Dot Plot (puntos individuales)", "Ridgeline Plot", "Barras Comparativas (Medias)", "Líneas y Puntos", "Líneas de Error (Media ± ...)")),
               conditionalPanel(condition="input.plot_type == 'Barras Comparativas (Medias)'", checkboxInput("show_error_bars", "Mostrar error (± SE)", value=FALSE)),
               conditionalPanel(condition="input.plot_type == 'Líneas de Error (Media ± ...)'", selectInput("error_type", "Intervalo:", choices=c("SE"="se", "SD"="ee", "IC 95%"="ic95"), selected="se")),
               conditionalPanel(condition="input.plot_type %in% c('Boxplot por grupos', 'Violin Plot', 'Dot Plot (puntos individuales)', 'Líneas de Error (Media ± ...)', 'Líneas y Puntos')", hr(), selectInput("line_type", "Línea:", choices=c("Sólida"="solid", "Punteada"="dashed", "Puntos"="dotted", "Guion-Punto"="dotdash"), selected="solid"), selectInput("point_shape", "Punto:", choices=c("Círculo"=19, "Triángulo"=17, "Cuadrado"=15, "Diamante"=18, "Estrella"=8), selected=19)),
               hr(), textInput("plot_title", "Título:", value="Gráfico Descriptivo"), selectInput("plot_color", "Color relleno:", choices=c("Azul"="#3498db", "Rojo"="#e74c3c", "Verde"="#2ecc71", "Naranja"="#f39c12", "Turquesa"="#1abc9c", "Blanco"="white"), selected="#3498db"), selectInput("border_color", "Color bordes:", choices=c("Negro"="black", "Gris"="#2c3e50", "Azul oscuro"="#2980b9", "Blanco"="white"), selected="black"), sliderInput("line_width", "Grosor:", min=0.5, max=3, value=1.2, step=0.1), hr(), downloadButton("descargar_estadisticos", "Descargar", class="btn-success")
             ), mainPanel(tabsetPanel(tabPanel("Estadísticos", DTOutput("tabla_estadisticos")), tabPanel("Gráfico", plotlyOutput("plot_desc", height="600px")), tabPanel("Supuestos", verbatimTextOutput("normality_test"), verbatimTextOutput("grubbs_test"), DTOutput("outliers_table"))))
           )
  ),
  
  tabPanel("Inferencia",
           sidebarLayout(
             sidebarPanel(
               h4("Análisis"), radioButtons("inf_pop", "Poblaciones:", choices=c("Una"="1pop", "Dos"="2pop")), uiOutput("inf_param_ui"), uiOutput("inf_vars_ui"), hr(),
               h4("Prueba"), uiOutput("inf_test_options_ui"), radioButtons("inf_alt", "Hipótesis Alternativa:", choices=c("Bilateral"="two.sided", "Menor <"="less", "Mayor >"="greater")), sliderInput("inf_conf", "Confianza:", min=0.80, max=0.99, value=0.95, step=0.01), hr(), actionButton("run_inf", "Ejecutar", class="btn-primary")
             ), mainPanel(verbatimTextOutput("inf_res_output"), uiOutput("inf_interpretation_ui"))
           )
  ),
  
  tabPanel("Diseños Experimentales",
           tabsetPanel(
             
             tabPanel("Experimentos Simples",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Diseño"), selectInput("sim_des", "Diseño Experimental:", choices = c("DCA", "DBCA", "DCL")), uiOutput("sim_vars_ui"), hr(), 
                          h4("Prueba Múltiple (PMM)"), selectInput("sim_pmm", "Método:", choices = c("Tukey", "Fisher (LSD)", "Duncan", "SNK", "Scott-Knott", "Bonferroni")), numericInput("sim_alpha", "Alfa (α):", value=0.05, min=0.01, max=0.20, step=0.01), 
                          conditionalPanel(condition = "input.sim_pmm == 'Tukey' || input.sim_pmm == 'Fisher (LSD)' || input.sim_pmm == 'Scott-Knott'", hr(), h4("Gráfico"), selectInput("sim_plot_err", "Error:", choices = c("SE"="se", "SD"="sd", "IC 95%"="ic")), checkboxInput("sim_facet", "Facetar Gráfico (DBCA/DCL)", value = FALSE), conditionalPanel("input.sim_facet == true", numericInput("sim_facet_ncol", "Columnas panel:", value=2, min=1))), hr(), actionButton("run_sim", "Ejecutar", class="btn-primary")
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Interpretación", uiOutput("sim_interpretacion_ui")),
                            tabPanel("ANDEVA", verbatimTextOutput("sim_anova")),
                            tabPanel("Supuestos", verbatimTextOutput("sim_supuestos"), plotOutput("sim_plots", height="500px")),
                            tabPanel("PMM y Gráficos", verbatimTextOutput("sim_pmm_out"), div(class="resizable-plot", style="width:100%; height:500px;", plotOutput("sim_pmm_plot", width="100%", height="100%")))
                          )
                        )
                      )
             ),
             
             tabPanel("Experimentos Factoriales",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Factorial"), selectInput("fac_des", "Diseño Base:", choices = c("DCA", "DBCA", "DCL")), selectInput("fac_arr", "Arreglo:", choices = c("Factorial Combinatorio", "Parcelas Divididas", "Parcelas Subdivididas", "Franjas", "Franjas Divididas")), uiOutput("fac_vars_ui"), hr(), 
                          h4("Prueba Múltiple (PMM)"), selectInput("fac_pmm", "Método:", choices = c("Tukey", "Fisher (LSD)", "Duncan", "SNK", "Scott-Knott", "Bonferroni")), numericInput("fac_alpha", "Alfa (α):", value=0.05, min=0.01, max=0.20, step=0.01), uiOutput("fac_pmm_terms_ui"), uiOutput("fac_plot_options_ui"), hr(), actionButton("run_fac", "Ejecutar", class="btn-primary")
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Interpretación", uiOutput("fac_interpretacion_ui")),
                            tabPanel("ANDEVA", verbatimTextOutput("fac_anova")),
                            tabPanel("Supuestos", verbatimTextOutput("fac_supuestos"), plotOutput("fac_plots", height="500px")),
                            tabPanel("PMM y Gráficos", verbatimTextOutput("fac_pmm_out"), div(class="resizable-plot", style="width:100%; height:500px;", plotOutput("fac_pmm_plot", width="100%", height="100%")), div(class="resizable-plot", style="width:100%; height:400px;", plotOutput("fac_interaction_plot", width="100%", height="100%")))
                          )
                        )
                      )
             ),
             
             tabPanel("Multi-Ambientales",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Configuración Multi-Ambiente"), uiOutput("ma_vars_ui"), radioButtons("ma_model_type", "Modelo Estadístico:", choices=c("Efectos Fijos (aov)"="fixed", "Modelo Mixto (lmer)"="mixed")), hr(),
                          h4("Prueba de Medias (PMM)"), selectInput("ma_pmm", "Método:", choices=c("Tukey", "Fisher (LSD)", "Scott-Knott")), numericInput("ma_alpha", "Alfa (α):", value=0.05, min=0.01, max=0.20, step=0.01), uiOutput("ma_pmm_terms_ui"), hr(),
                          h4("Opciones Gráficas"), selectInput("ma_plot_err", "Error:", choices = c("SE"="se", "SD"="sd", "IC 95%"="ic")), checkboxInput("ma_facet", "Facetar Gráfico", value=TRUE), conditionalPanel("input.ma_facet == true", uiOutput("ma_facet_ui"), numericInput("ma_facet_ncol", "Columnas panel:", value=2, min=1)), hr(), actionButton("run_ma", "Ejecutar Análisis", class="btn-primary")
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Interpretación", uiOutput("ma_interpretacion_ui")),
                            tabPanel("Modelo General", verbatimTextOutput("ma_anova")),
                            tabPanel("Supuestos", verbatimTextOutput("ma_supuestos"), plotOutput("ma_plots", height="500px")),
                            tabPanel("PMM y Gráficos", verbatimTextOutput("ma_pmm_out"), div(class="resizable-plot", style="width:100%; height:500px;", plotOutput("ma_pmm_plot", width="100%", height="100%")), div(class="resizable-plot", style="width:100%; height:400px;", plotOutput("ma_int_plot", width="100%", height="100%")))
                          )
                        )
                      )
             ),
             
             tabPanel("Transformación / ANCOVA",
                      tabsetPanel(
                        tabPanel("Transformación", sidebarLayout(sidebarPanel(h4("Transformación"), uiOutput("trans_var_ui"), selectInput("trans_method", "Método:", choices=c("Log10"="log10", "Ln"="ln", "Raíz"="sqrt", "Arcoseno"="arcsin", "Box-Cox"="boxcox")), actionButton("run_trans", "Guardar", class="btn-warning")), mainPanel(fluidRow(column(6, plotOutput("trans_plot_orig")), column(6, plotOutput("trans_plot_new"))), verbatimTextOutput("trans_shapiro")))),
                        tabPanel("ANCOVA", sidebarLayout(sidebarPanel(h4("ANCOVA"), selectInput("anc_des", "Diseño:", choices=c("DCA", "DBCA", "DCL")), uiOutput("anc_vars_ui"), hr(), selectInput("anc_pmm", "Ajuste:", choices=c("Tukey"="tukey", "Bonferroni"="bonferroni", "Sidak"="sidak", "Ninguno"="none")), actionButton("run_anc", "Ejecutar", class="btn-primary")), mainPanel(verbatimTextOutput("anc_anova"), verbatimTextOutput("anc_hp"), verbatimTextOutput("anc_sup"), verbatimTextOutput("anc_emmeans"), plotOutput("anc_plot"))))
                      )
             )
           )
  )
)

# --- SERVIDOR (SERVER) ---
server <- function(input, output, session) {
  
  vals <- reactiveValues(datos = NULL, reporte = data.frame())
  refresh_trigger <- reactiveVal(0)
  
  # --- CARGA POR ARCHIVO/PORTAPAPELES ---
  observeEvent(input$file1, {
    req(input$file1)
    output$select_sheet <- renderUI({ selectInput("sheet_selected", "Hoja:", choices = excel_sheets(input$file1$datapath)) })
  })
  
  observe({
    req(input$import_method == "file", input$sheet_selected)
    tryCatch({ 
      vals$datos <- read_excel(input$file1$datapath, sheet = input$sheet_selected)
      refresh_trigger(refresh_trigger() + 1)
      showNotification("Archivo cargado correctamente", type="message")
    }, error = function(e) { showNotification(e$message, type="error") })
  })
  
  observeEvent(input$paste_btn, {
    tryCatch({ 
      df_clip <- read_clip_tbl()
      if (is.null(df_clip) || nrow(df_clip) == 0) {
        showNotification("Portapapeles vacío o no válido", type = "warning")
        return(NULL)
      }
      df_clip <- df_clip[rowSums(is.na(df_clip) | df_clip == "") != ncol(df_clip), , drop = FALSE]
      df_clip <- df_clip[, colSums(is.na(df_clip)) != nrow(df_clip), drop = FALSE]
      vals$datos <- df_clip
      refresh_trigger(refresh_trigger() + 1)
      showNotification("Datos pegados correctamente", type="message")
    }, error = function(e) { showNotification(paste("Error:", e$message), type="error") })
  })
  
  output$tabla_visual <- renderDataTable({ 
    req(vals$datos)
    datatable(vals$datos, editable=TRUE, options=list(scrollX=TRUE, pageLength=15, dom = "Bfrtip"), class = "compact stripe hover", rownames = FALSE) 
  })
  
  observeEvent(input$tabla_visual_cell_edit, {
    info <- input$tabla_visual_cell_edit
    vals$datos[info$row, info$col] <- info$value
    refresh_trigger(refresh_trigger() + 1)
  })
  
  output$data_status <- renderPrint({ req(vals$datos); cat("Filas:", nrow(vals$datos), "| Columnas:", ncol(vals$datos)) })
  
  # --- SELECTORES GENERALES ---
  output$hist_var_ui <- renderUI({ req(vals$datos); selectInput("hist_v", "Variable:", choices = c("Seleccione..." = "", names(vals$datos))) })
  output$eda_var_ui <- renderUI({ req(vals$datos); selectInput("eda_v", "Variable a limpiar:", choices = c("Seleccione..." = "", names(vals$datos))) })
  output$eda_group_ui <- renderUI({ req(vals$datos); selectInput("eda_g", "Agrupar por:", choices = c("Seleccione..." = "", names(vals$datos))) })
  output$desc_var_ui <- renderUI({ req(vals$datos); selectInput("desc_v", "Variable Numérica (eje Y):", choices = c("Seleccione..." = "", names(vals$datos))) })
  output$x_group_ui <- renderUI({ req(vals$datos); selectInput("x_group", "Variable de Agrupación (eje X / grupos):", choices = c("Seleccione..." = "", names(vals$datos))) })
  
  # --- EDA: DISTRIBUCIÓN E IMPUTACIÓN ---
  output$plot_hist <- renderPlotly({
    req(vals$datos, input$hist_v)
    if(input$hist_v == "") return(NULL)
    df <- vals$datos
    df[[input$hist_v]] <- as.numeric(as.character(df[[input$hist_v]]))
    p <- ggplot(df, aes(x = .data[[input$hist_v]]))
    if (input$hist_type == "density") {
      p <- p + geom_histogram(aes(y = after_stat(density)), bins = input$hist_bins, fill = "#3498db", color = "white", alpha = 0.7) +
        geom_density(color = "#e74c3c", linewidth = 1.2)
    } else {
      p <- p + geom_histogram(bins = input$hist_bins, fill = "#3498db", color = "white", alpha = 0.7)
    }
    p <- p + theme_light(base_size = 14) + labs(title = paste("Distribución de", input$hist_v), x = input$hist_v, y = if(input$hist_type == "density") "Densidad" else "Frecuencia")
    ggplotly(p) %>% config(displayModeBar = TRUE)
  })
  
  output$plot_faltantes <- renderPlotly({
    req(vals$datos)
    na_counts <- colSums(is.na(vals$datos))
    if(sum(na_counts) == 0) {
      p <- ggplot() + annotate("text", x = 0.5, y = 0.5, label = "La base de datos no tiene valores faltantes", size = 6, color = "#27ae60") + theme_void()
      return(ggplotly(p))
    }
    na_df <- data.frame(Variable = names(na_counts), Faltantes = na_counts) %>% filter(Faltantes > 0) %>% arrange(desc(Faltantes)) %>% mutate(Porcentaje = Faltantes / nrow(vals$datos) * 100)
    p <- ggplot(na_df, aes(x = reorder(Variable, -Faltantes), y = Faltantes, text = paste("Faltantes:", Faltantes, "<br>Porcentaje:", round(Porcentaje, 2), "%"))) +
      geom_bar(stat = "identity", fill = "#e74c3c", alpha = 0.8) + theme_light() + labs(title = "Pareto de Valores Faltantes", x = "Variables", y = "Cantidad de NAs") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = TRUE)
  })
  
  observeEvent(input$run_imputation, {
    req(vals$datos, input$imp_method)
    df <- vals$datos
    if(sum(is.na(df)) == 0) { showNotification("No hay faltantes.", type = "warning"); return() }
    tryCatch({
      if(input$imp_method == "mean") df <- df %>% mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))
      else if (input$imp_method == "median") df <- df %>% mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
      else if (input$imp_method == "pmm") df <- complete(mice(df, m=1, method='pmm', printFlag=FALSE))
      else if (input$imp_method == "knn") df <- kNN(df, imp_var=FALSE)
      else if (input$imp_method == "cart") df <- complete(mice(df, m=1, method='cart', printFlag=FALSE))
      vals$datos <- df; refresh_trigger(refresh_trigger() + 1); showNotification("Imputación exitosa.", type="message")
    }, error = function(e) { showNotification(e$message, type="error") })
  })
  
  # --- EDA: TRATAMIENTO DE ATÍPICOS ---
  output$plot_boxplot_eda <- renderPlotly({
    req(vals$datos, input$eda_v, input$eda_g)
    if(input$eda_v == "" || input$eda_g == "") return(NULL)
    df <- vals$datos
    df[[input$eda_v]] <- as.numeric(as.character(df[[input$eda_v]]))
    p <- ggplot(df, aes(x = .data[[input$eda_g]], y = .data[[input$eda_v]], fill = .data[[input$eda_g]])) +
      geom_boxplot(fill = input$box_col, outlier.color = "#e74c3c", outlier.size = 2, alpha = 0.85) + theme_light(base_size = 14) +
      labs(title = paste("Boxplot de", input$eda_v, "por", input$eda_g), x = input$eda_g, y = input$eda_v) + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    ggplotly(p) %>% config(displayModeBar = TRUE)
  })
  
  output$tabla_iqr_detallada <- renderDT({
    req(vals$datos, input$eda_v, input$eda_g)
    if(input$eda_v == "" || input$eda_g == "") return(NULL)
    df <- vals$datos
    df[[input$eda_v]] <- as.numeric(as.character(df[[input$eda_v]]))
    res <- df %>% group_by(across(all_of(input$eda_g))) %>%
      summarise(Q1 = round(quantile(.data[[input$eda_v]], 0.25, na.rm=T),3), Mediana = round(median(.data[[input$eda_v]], na.rm=T),3),
                Q3 = round(quantile(.data[[input$eda_v]], 0.75, na.rm=T),3), IQR = round(IQR(.data[[input$eda_v]], na.rm=T),3),
                Limite_Inf = round(Q1 - 1.5*IQR,3), Limite_Sup = round(Q3 + 1.5*IQR,3),
                N_Outliers = sum(.data[[input$eda_v]] < Limite_Inf | .data[[input$eda_v]] > Limite_Sup, na.rm=T), .groups="drop")
    datatable(res, options = list(scrollX=TRUE, dom='t', pageLength=15), rownames=FALSE)
  })
  
  observeEvent(input$apply_outliers, {
    req(vals$datos, input$eda_v, input$eda_g)
    if(input$eda_v == "" || input$eda_g == "") return()
    if(input$outlier_action == "none") { showNotification("Selecciona una acción", type = "warning"); return() }
    
    df <- vals$datos; var_name <- input$eda_v; group_name <- input$eda_g
    df[[var_name]] <- as.numeric(as.character(df[[var_name]]))
    limits <- df %>% group_by(across(all_of(group_name))) %>% summarise(Q1=quantile(.data[[var_name]],0.25,na.rm=T), Q3=quantile(.data[[var_name]],0.75,na.rm=T), IQR=IQR(.data[[var_name]],na.rm=T), .groups="drop") %>% mutate(Lim_Inf=Q1-1.5*IQR, Lim_Sup=Q3+1.5*IQR)
    df_joined <- df %>% left_join(limits, by=group_name)
    es_outlier <- df_joined[[var_name]] < df_joined$Lim_Inf | df_joined[[var_name]] > df_joined$Lim_Sup; es_outlier[is.na(es_outlier)] <- FALSE
    if(input$outlier_action == "NA") { df[[var_name]][es_outlier] <- NA; showNotification("Convertidos a NA.", type="message") }
    else if(input$outlier_action == "eliminar") { df <- df[!es_outlier, ]; showNotification("Filas eliminadas.", type="message") }
    vals$datos <- df; refresh_trigger(refresh_trigger() + 1)
  })
  
  # --- ESTADÍSTICA DESCRIPTIVA COMPLETAS (21 Métricas) ---
  stats_reactive <- reactive({
    req(vals$datos, input$desc_v)
    if(input$desc_v == "") return(NULL)
    df <- vals$datos
    var_num <- input$desc_v
    x_val <- as.numeric(as.character(df[[var_num]]))
    
    if(input$stats_level == "overall") {
      x <- x_val[!is.na(x_val) & is.finite(x_val)]
      if(length(x) < 3) return(data.frame(Estadistico = "Datos insuficientes", Valor = NA_real_))
      
      n <- length(x); media <- mean(x); mediana <- median(x)
      moda_val <- tryCatch(modeest::mlv(x, method = "mfv")$M[1], error = function(e) NA_real_)
      de <- sd(x); varianza <- var(x); cv <- (de / media) * 100; ee <- de / sqrt(n)
      q <- quantile(x, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1), na.rm = TRUE)
      asim <- tryCatch(e1071::skewness(x), error=function(e) NA)
      kurt <- tryCatch(e1071::kurtosis(x), error=function(e) NA)
      geo <- tryCatch(exp(mean(log(x[x>0]))), error=function(e) NA)
      harm <- tryCatch(1 / mean(1/x[x>0]), error=function(e) NA)
      rms_val <- sqrt(mean(x^2))
      shapiro <- tryCatch(shapiro.test(x)$p.value, error=function(e) NA)
      
      data.frame(
        Estadistico = c("N", "Media", "Mediana", "Moda", "Desv. Estándar", "Varianza",
                        "Coef. de Variación (%)", "Error Estándar",
                        "Mínimo", "Percentil 10%", "Q1", "Q2 (Mediana)", "Q3",
                        "Percentil 90%", "Máximo",
                        "Asimetría (Skewness)", "Curtosis",
                        "Media Geométrica", "Media Armónica", "RMS (Media Cuadrática)",
                        "Shapiro-Wilk p-value"),
        Valor = round(c(n, media, mediana, moda_val, de, varianza, cv, ee,
                        q[1], q[2], q[3], q[4], q[5], q[6], q[7], asim, kurt,
                        geo, harm, rms_val, shapiro), 4)
      )
    } else {
      req(input$x_group)
      if(input$x_group == "") return(NULL)
      group_var <- input$x_group
      temp_df <- data.frame(g = df[[group_var]], v = x_val)
      
      temp_df %>%
        group_by(g) %>%
        summarise(
          N = sum(!is.na(v)),
          Media = mean(v, na.rm = TRUE),
          Mediana = median(v, na.rm = TRUE),
          Moda = tryCatch(modeest::mlv(v[!is.na(v)], method = "mfv")$M[1], error = function(e) NA_real_),
          `Desv. Estándar` = sd(v, na.rm = TRUE),
          Varianza = var(v, na.rm = TRUE),
          `Coef. Variación (%)` = (`Desv. Estándar` / Media) * 100,
          `Error Estándar` = `Desv. Estándar` / sqrt(N),
          Mínimo = min(v, na.rm = TRUE),
          `Perc. 10%` = quantile(v, 0.1, na.rm = TRUE),
          Q1 = quantile(v, 0.25, na.rm = TRUE),
          `Q2 (Mediana)` = median(v, na.rm = TRUE),
          Q3 = quantile(v, 0.75, na.rm = TRUE),
          `Perc. 90%` = quantile(v, 0.9, na.rm = TRUE),
          Máximo = max(v, na.rm = TRUE),
          Asimetría = tryCatch(e1071::skewness(v[!is.na(v)]), error=function(e) NA),
          Curtosis = tryCatch(e1071::kurtosis(v[!is.na(v)]), error=function(e) NA),
          .groups = "drop"
        ) %>%
        rename(!!sym(group_var) := g)
    }
  })
  output$tabla_estadisticos <- renderDT({ datatable(stats_reactive(), options=list(dom="t", scrollX=TRUE, pageLength=25), rownames=FALSE) })
  
  # --- GRÁFICOS DESCRIPTIVOS CON PERSONALIZACIÓN ---
  ggplot_desc <- reactive({
    req(vals$datos, input$plot_type, input$plot_title, input$desc_v)
    if(input$desc_v == "") return(NULL)
    df <- vals$datos
    df[[input$desc_v]] <- as.numeric(as.character(df[[input$desc_v]]))
    
    fill_col <- input$plot_color
    border_col <- input$border_color
    lw <- input$line_width
    lty <- switch(input$line_type, "solid" = 1, "dashed" = 2, "dotted" = 3, "dotdash" = 4)
    pch_val <- as.numeric(input$point_shape)
    title <- input$plot_title
    
    x_var <- if(is.null(input$x_group) || input$x_group=="") { df$dummy<-"Todo"; "dummy" } else input$x_group
    
    if(input$plot_type == "Boxplot Individual") {
      ggplot(df, aes(x="", y=.data[[input$desc_v]])) + 
        geom_boxplot(fill=fill_col, color=border_col, linewidth=lw, alpha=0.85, width=0.4) + 
        stat_summary(fun = mean, geom = "point", color = "red", size = 4, shape = pch_val) +
        theme_light(base_size = 15) + labs(title=title, y=input$desc_v, x=NULL, caption="Falcon V1.0") +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      
    } else if(input$plot_type == "Boxplot por grupos") {
      ggplot(df, aes(x=.data[[x_var]], y=.data[[input$desc_v]])) + 
        geom_boxplot(fill=fill_col, color=border_col, linewidth=lw, alpha=0.85, outlier.color="red") + 
        stat_summary(fun = mean, geom = "point", color = "red", size = 4, shape = pch_val) +
        theme_light(base_size = 15) + labs(title=title, x=x_var, y=input$desc_v, caption="Falcon V1.0") +
        theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if(input$plot_type == "Violin Plot") {
      ggplot(df, aes(x=.data[[x_var]], y=.data[[input$desc_v]])) + 
        geom_violin(fill=fill_col, color=border_col, linewidth=lw, alpha=0.85) + 
        geom_boxplot(width=0.1, fill="white", alpha=0.6, color=border_col) +
        theme_light(base_size = 15) + labs(title=title, x=x_var, y=input$desc_v, caption="Falcon V1.0") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if(input$plot_type == "Dot Plot (puntos individuales)") {
      ggplot(df, aes(x=.data[[x_var]], y=.data[[input$desc_v]])) + 
        geom_jitter(width=0.2, height=0, color=fill_col, size=3, shape=pch_val, alpha=0.7) + 
        theme_light(base_size = 15) + labs(title=title, x=x_var, y=input$desc_v, caption="Falcon V1.0") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if(input$plot_type == "Ridgeline Plot") {
      ggplot(df, aes(x=.data[[input$desc_v]], y=.data[[x_var]], fill=.data[[x_var]])) + 
        geom_density_ridges(alpha=0.6, scale=1.2, color=border_col, linewidth=lw) + 
        theme_light(base_size = 15) + labs(title=title, x=input$desc_v, y=x_var, caption="Falcon V1.0") +
        theme(legend.position="none")
      
    } else if(input$plot_type == "Barras Comparativas (Medias)") {
      means_df <- df %>% group_by(across(all_of(x_var))) %>% summarise(Media=mean(.data[[input$desc_v]],na.rm=T), SE=sd(.data[[input$desc_v]],na.rm=T)/sqrt(n()), .groups="drop")
      p <- ggplot(means_df, aes(x=.data[[x_var]], y=Media)) + 
        geom_bar(stat="identity", fill=fill_col, color=border_col, linewidth=lw, alpha=0.85) + 
        geom_text(aes(label=round(Media, 2), y=Media/2), color="black", fontface="bold", size=3.8) +
        theme_light(base_size = 15) + labs(title=title, x=x_var, y="Media", caption="Falcon V1.0") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      if(input$show_error_bars) { p <- p + geom_errorbar(aes(ymin=Media-SE, ymax=Media+SE), width=0.2, color=border_col, linewidth=lw) }
      p
      
    } else if(input$plot_type == "Líneas y Puntos") {
      sum_df <- df %>% group_by(across(all_of(x_var))) %>% summarise(Valor=mean(.data[[input$desc_v]],na.rm=T), .groups="drop")
      ggplot(sum_df, aes(x=.data[[x_var]], y=Valor)) + 
        geom_line(group=1, color=fill_col, linewidth=lw, linetype=lty) + 
        geom_point(color=fill_col, size=4, shape=pch_val) + 
        theme_light(base_size = 15) + labs(title=title, x=x_var, y=input$desc_v, caption="Falcon V1.0") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if(input$plot_type == "Líneas de Error (Media ± ...)") {
      sum_df <- df %>% group_by(across(all_of(x_var))) %>% 
        summarise(Media=mean(.data[[input$desc_v]],na.rm=T), SE=sd(.data[[input$desc_v]],na.rm=T)/sqrt(n()), SD=sd(.data[[input$desc_v]],na.rm=T), .groups="drop")
      if(input$error_type == "ic95") { sum_df <- sum_df %>% mutate(lower=Media - 1.96*SE, upper=Media + 1.96*SE) } 
      else if(input$error_type == "ee") { sum_df <- sum_df %>% mutate(lower=Media - SD, upper=Media + SD) } 
      else { sum_df <- sum_df %>% mutate(lower=Media - SE, upper=Media + SE) }
      
      ggplot(sum_df, aes(x=.data[[x_var]], y=Media)) + 
        geom_point(color=fill_col, size=4, shape=pch_val) + 
        geom_errorbar(aes(ymin=lower, ymax=upper), width=0.25, color=border_col, linewidth=lw) + 
        geom_line(group=1, color=fill_col, linewidth=lw, linetype=lty) + 
        theme_light(base_size = 15) + labs(title=title, x=x_var, y=input$desc_v, caption="Falcon V1.0") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  output$plot_desc <- renderPlotly({ req(ggplot_desc()); ggplotly(ggplot_desc()) %>% config(displayModeBar=TRUE) })
  
  output$normality_test <- renderPrint({ req(vals$datos, input$desc_v); if(input$desc_v == "") return(); shapiro.test(as.numeric(as.character(vals$datos[[input$desc_v]]))) })
  output$grubbs_test <- renderPrint({ req(vals$datos, input$desc_v); if(input$desc_v == "") return(); outliers::grubbs.test(as.numeric(as.character(vals$datos[[input$desc_v]]))) })
  output$outliers_table <- renderDT({
    req(vals$datos, input$desc_v); if(input$desc_v == "") return(NULL)
    x <- as.numeric(as.character(vals$datos[[input$desc_v]])); z <- scale(x); idx <- which(abs(z) > 3)
    if(length(idx)>0) data.frame(Fila=idx, Valor=x[idx]) else data.frame(Mensaje="No outliers extremos (z > 3)")
  })
  
  # --- INFERENCIA ESTADÍSTICA ---
  output$inf_param_ui <- renderUI({
    req(input$inf_pop)
    if(input$inf_pop == "1pop"){
      selectInput("inf_param", "Parámetro a contrastar:", choices = c("Media", "Proporción", "Varianza", "Total"))
    } else {
      selectInput("inf_param", "Parámetro a contrastar:", choices = c("Medias Independientes", "Medias Pareadas (Dependientes)", "Proporciones", "Varianzas"))
    }
  })
  
  output$inf_vars_ui <- renderUI({
    req(vals$datos, input$inf_param)
    df <- vals$datos; vars_all <- names(df)
    
    if (input$inf_param == "Media") {
      tagList(selectInput("inf_v1", "Seleccione Variable (Numérica):", choices = c("Seleccione..."="", vars_all)), numericInput("inf_mu0", "Media Hipotética (H0):", value = 0))
    } else if (input$inf_param == "Proporción") {
      tagList(selectInput("inf_v1", "Variable (Categórica/Binaria):", choices = c("Seleccione..."="", vars_all)), textInput("inf_exito", "Valor de 'Éxito':", value = "1"), numericInput("inf_p0", "Proporción Hipotética (H0):", value = 0.5, step = 0.05))
    } else if (input$inf_param == "Varianza") {
      tagList(selectInput("inf_v1", "Variable (Numérica):", choices = c("Seleccione..."="", vars_all)), numericInput("inf_var0", "Varianza Hipotética (H0):", value = 1))
    } else if (input$inf_param == "Total") {
      tagList(selectInput("inf_v1", "Variable (Numérica):", choices = c("Seleccione..."="", vars_all)), numericInput("inf_N", "Tamaño Población (N):", value = 1000), numericInput("inf_tot0", "Total Hipotético (H0):", value = 0))
    } else if (input$inf_param %in% c("Medias Independientes", "Medias Pareadas (Dependientes)")) {
      tagList(selectInput("inf_v1", "Variable/Grupo 1:", choices = c("Seleccione..."="", vars_all)), selectInput("inf_v2", "Variable/Grupo 2:", choices = c("Seleccione..."="", vars_all)), numericInput("inf_mu0", "Diferencia de Medias (H0):", value = 0))
    } else if (input$inf_param == "Proporciones") {
      tagList(selectInput("inf_v1", "Var 1 (Categórica/Binaria):", choices = c("Seleccione..."="", vars_all)), selectInput("inf_v2", "Var 2 (Categórica/Binaria):", choices = c("Seleccione..."="", vars_all)), textInput("inf_exito", "Valor de 'Éxito':", value = "1"))
    } else if (input$inf_param == "Varianzas") {
      tagList(selectInput("inf_v1", "Variable/Grupo 1:", choices = c("Seleccione..."="", vars_all)), selectInput("inf_v2", "Variable/Grupo 2:", choices = c("Seleccione..."="", vars_all)), numericInput("inf_ratio0", "Ratio Hipotético (H0):", value = 1))
    }
  })
  
  output$inf_test_options_ui <- renderUI({
    req(input$inf_param)
    if(input$inf_param == "Media"){
      tagList(checkboxInput("inf_known_sd", "Conozco la Desv. Estándar Poblacional (Z-test)", value = FALSE), conditionalPanel("input.inf_known_sd", numericInput("inf_sigma", "Desviación (σ):", value = 1)))
    } else if(input$inf_param == "Medias Independientes"){
      checkboxInput("inf_varequal", "Asumir varianzas poblacionales iguales (Homocedasticidad)", value = FALSE)
    } else if(input$inf_param %in% c("Proporción", "Proporciones")){
      checkboxInput("inf_yates", "Aplicar corrección de Yates", value = TRUE)
    }
  })
  
  res_inferencia <- eventReactive(input$run_inf, {
    req(vals$datos, input$inf_v1)
    if(input$inf_v1 == "") return("Por favor, seleccione las variables.")
    
    x <- as.numeric(as.character(vals$datos[[input$inf_v1]])); x <- x[!is.na(x)]; 
    alt <- input$inf_alt; conf <- input$inf_conf; param <- input$inf_param
    
    tryCatch({
      if (param == "Media") {
        if(input$inf_known_sd) {
          sigma <- input$inf_sigma; n <- length(x); z_stat <- (mean(x) - input$inf_mu0) / (sigma / sqrt(n))
          p_val <- if(alt=="two.sided") 2*pnorm(-abs(z_stat)) else if(alt=="less") pnorm(z_stat) else pnorm(z_stat, lower.tail=F)
          z_crit <- if(alt=="two.sided") qnorm(1-(1-conf)/2) else qnorm(conf); margin <- z_crit * (sigma / sqrt(n))
          c_l <- if(alt=="less") -Inf else mean(x) - margin; c_u <- if(alt=="greater") Inf else mean(x) + margin; if(alt=="two.sided"){ c_l <- mean(x) - margin; c_u <- mean(x) + margin }
          return(paste0("\n\tOne Sample Z-test (Varianza Conocida)\n\ndata:  ", input$inf_v1, "\nZ = ", round(z_stat,4), ", p-value = ", signif(p_val,5), "\n", conf*100, " percent confidence interval:\n ", round(c_l,5), "  ", round(c_u,5), "\nsample estimates:\nmean of x \n ", round(mean(x),5), "\n"))
        } else { return(capture.output(print(t.test(x, mu = input$inf_mu0, alternative = alt, conf.level = conf)))) }
      } else if (param == "Proporción") return(capture.output(print(prop.test(sum(vals$datos[[input$inf_v1]] == input$inf_exito, na.rm=T), length(vals$datos[[input$inf_v1]][!is.na(vals$datos[[input$inf_v1]])]), p = input$inf_p0, alternative = alt, conf.level = conf, correct = input$inf_yates))))
      else if (param == "Varianza") return(capture.output(print(EnvStats::varTest(x, sigma.sq = input$inf_var0, alternative = alt, conf.level = conf))))
      else if (param == "Total") {
        N <- input$inf_N; n <- length(x); m_x <- mean(x); s_x <- sd(x); se_t <- N * (s_x / sqrt(n)); t_stat <- (N * m_x - input$inf_tot0) / se_t; df_t <- n - 1
        p_val <- if(alt=="two.sided") 2*pt(-abs(t_stat), df_t) else if(alt=="less") pt(t_stat, df_t) else pt(t_stat, df_t, lower.tail=F)
        t_crit <- if(alt=="two.sided") qt(1-(1-conf)/2, df_t) else qt(conf, df_t); margin <- t_crit * se_t
        c_l <- if(alt=="less") -Inf else (N*m_x) - margin; c_u <- if(alt=="greater") Inf else (N*m_x) + margin; if(alt=="two.sided"){ c_l <- (N*m_x) - margin; c_u <- (N*m_x) + margin }
        return(paste0("\n\tOne Sample t-test for Population Total\n\ndata:  ", input$inf_v1, "\nt = ", round(t_stat,4), ", df = ", df_t, ", p-value = ", signif(p_val,5), "\n", conf*100, " percent confidence interval:\n ", round(c_l,5), "  ", round(c_u,5), "\nsample estimates:\nestimated total \n ", round(N*m_x,5), "\n"))
      } else if (param == "Medias Independientes") { req(input$inf_v2); y <- as.numeric(as.character(vals$datos[[input$inf_v2]])); y <- y[!is.na(y)]; return(capture.output(print(t.test(x, y, paired = F, var.equal = input$inf_varequal, mu = input$inf_mu0, alternative = alt, conf.level = conf))))
      } else if (param == "Medias Pareadas (Dependientes)") { req(input$inf_v2); tmp <- na.omit(data.frame(x=as.numeric(as.character(vals$datos[[input$inf_v1]])), y=as.numeric(as.character(vals$datos[[input$inf_v2]])))); return(capture.output(print(t.test(tmp$x, tmp$y, paired = T, mu = input$inf_mu0, alternative = alt, conf.level = conf))))
      } else if (param == "Proporciones") { req(input$inf_v2); return(capture.output(print(prop.test(c(sum(vals$datos[[input$inf_v1]]==input$inf_exito, na.rm=T), sum(vals$datos[[input$inf_v2]]==input$inf_exito, na.rm=T)), c(length(vals$datos[[input$inf_v1]][!is.na(vals$datos[[input$inf_v1]])]), length(vals$datos[[input$inf_v2]][!is.na(vals$datos[[input$inf_v2]])])), alternative = alt, conf.level = conf, correct = input$inf_yates))))
      } else if (param == "Varianzas") { req(input$inf_v2); y <- as.numeric(as.character(vals$datos[[input$inf_v2]])); y <- y[!is.na(y)]; return(capture.output(print(var.test(x, y, ratio = input$inf_ratio0, alternative = alt, conf.level = conf)))) }
    }, error = function(e) { return(paste("Error en cálculo:", e$message)) })
  })
  
  output$inf_res_output <- renderPrint({ req(res_inferencia()); cat(paste(res_inferencia(), collapse = "\n")) })
  
  output$inf_interpretation_ui <- renderUI({
    req(res_inferencia()); alt_t <- switch(input$inf_alt, "two.sided"="diferente a", "less"="menor a", "greater"="mayor a")
    p_t <- switch(input$inf_param, "Media"="la media poblacional", "Proporción"="la proporción poblacional", "Varianza"="la varianza poblacional", "Total"="el total poblacional", "Medias Independientes"="la diferencia de medias", "Medias Pareadas (Dependientes)"="la diferencia media", "Proporciones"="la diferencia de proporciones", "Varianzas"="el ratio de varianzas")
    HTML(paste0("<div class='interpretation-box'><h4>Interpretación</h4><p><b>H0:</b> ", p_t, " es igual al valor de referencia.</p><p><b>H1:</b> ", p_t, " es <b>", alt_t, "</b> al valor de referencia.</p><p><b>Decisión (α=", round(1-input$inf_conf,2), "):</b> Si p-value < α, se rechaza H0.</p></div>"))
  })
  
  
  # =====================================================================
  # --- DISEÑOS EXPERIMENTALES: EXPERIMENTOS SIMPLES ---
  # =====================================================================
  
  output$sim_vars_ui <- renderUI({
    req(vals$datos); vars <- names(vals$datos)
    tagList(
      selectInput("sim_y", "Variable Respuesta (Y):", choices = c("Seleccione..."="", vars)),
      selectInput("sim_trt", "Tratamiento (Factor):", choices = c("Seleccione..."="", vars)),
      conditionalPanel("input.sim_des == 'DBCA'", selectInput("sim_blk", "Bloque / Repetición:", choices = c("Seleccione..."="", vars))),
      conditionalPanel("input.sim_des == 'DCL'", selectInput("sim_row", "Fila:", choices = c("Seleccione..."="", vars)), selectInput("sim_col", "Columna:", choices = c("Seleccione..."="", vars)))
    )
  })
  
  output$sim_interpretacion_ui <- renderUI({
    req(input$sim_des)
    texto <- switch(input$sim_des,
                    "DCA" = "<p><b>Modelo Estadístico (DCA):</b> Y<sub>ij</sub> = &mu; + &tau;<sub>i</sub> + &epsilon;<sub>ij</sub></p><ul><li><b>&mu;:</b> Media general</li><li><b>&tau;<sub>i</sub>:</b> Efecto del i-ésimo tratamiento</li><li><b>&epsilon;<sub>ij</sub>:</b> Error experimental</li></ul>",
                    "DBCA" = "<p><b>Modelo Estadístico (DBCA):</b> Y<sub>ij</sub> = &mu; + &tau;<sub>i</sub> + &beta;<sub>j</sub> + &epsilon;<sub>ij</sub></p><ul><li><b>&tau;<sub>i</sub>:</b> Efecto del tratamiento</li><li><b>&beta;<sub>j</sub>:</b> Efecto del bloque</li></ul>",
                    "DCL" = "<p><b>Modelo Estadístico (DCL):</b> Y<sub>ijk</sub> = &mu; + &tau;<sub>i</sub> + &rho;<sub>j</sub> + &gamma;<sub>k</sub> + &epsilon;<sub>ijk</sub></p><ul><li><b>&tau;<sub>i</sub>:</b> Efecto del tratamiento</li><li><b>&rho;<sub>j</sub>:</b> Efecto de la fila</li><li><b>&gamma;<sub>k</sub>:</b> Efecto de la columna</li></ul>")
    HTML(paste0("<div class='interpretation-box'>", texto, "</div>"))
  })
  
  observeEvent(input$run_sim, {
    req(vals$datos, input$sim_y, input$sim_trt)
    if(input$sim_y == "" || input$sim_trt == "") { showNotification("Seleccione variables.", type="warning"); return() }
    
    df <- vals$datos; vars_to_keep <- c(input$sim_y, input$sim_trt)
    if(input$sim_des == "DBCA") vars_to_keep <- c(vars_to_keep, input$sim_blk)
    if(input$sim_des == "DCL") vars_to_keep <- c(vars_to_keep, input$sim_row, input$sim_col)
    if(any(vars_to_keep == "")) { showNotification("Faltan variables por seleccionar.", type="warning"); return() }
    
    df[[input$sim_y]] <- as.numeric(as.character(df[[input$sim_y]]))
    df <- df[complete.cases(df[, vars_to_keep]), ]
    if(nrow(df) < 2) { showNotification("Datos insuficientes tras omitir NA.", type="error"); return() }
    
    df[[input$sim_trt]] <- as.factor(df[[input$sim_trt]])
    if(input$sim_des == "DBCA") df[[input$sim_blk]] <- as.factor(df[[input$sim_blk]])
    if(input$sim_des == "DCL") { df[[input$sim_row]] <- as.factor(df[[input$sim_row]]); df[[input$sim_col]] <- as.factor(df[[input$sim_col]]) }
    df <- droplevels(df) 
    
    tryCatch({
      form_str <- switch(input$sim_des, "DCA" = paste(input$sim_y, "~", input$sim_trt), "DBCA" = paste(input$sim_y, "~", input$sim_trt, "+", input$sim_blk), "DCL" = paste(input$sim_y, "~", input$sim_trt, "+", input$sim_row, "+", input$sim_col))
      modelo <- aov(as.formula(form_str), data = df)
      
      output$sim_anova <- renderPrint({ cat("--- Tabla de Análisis de Varianza (ANOVA) ---\n"); print(summary(modelo)); cat("\n--- Coeficiente de Variación (CV) ---\n"); print(agricolae::cv.model(modelo)) })
      output$sim_supuestos <- renderPrint({
        cat("--- Normalidad de Residuales (Shapiro-Wilk) ---\n"); tryCatch({ print(shapiro.test(resid(modelo))) }, error=function(e) cat("Error:", e$message, "\n"))
        cat("\n--- Homocedasticidad (Bartlett) ---\n"); tryCatch({ print(bartlett.test(as.formula(paste(input$sim_y, "~", input$sim_trt)), data = df)) }, error=function(e) cat("Error:", e$message, "\n"))
        cat("\n--- Homocedasticidad Robusta (Levene) ---\n"); tryCatch({ print(car::leveneTest(as.formula(paste(input$sim_y, "~", input$sim_trt)), data = df, center = "median")) }, error=function(e) cat("Error:", e$message, "\n"))
        cat("\n--- Independencia de Residuales (Durbin-Watson) ---\n"); tryCatch({ print(lmtest::dwtest(modelo)) }, error=function(e) cat("Error:", e$message, "\n"))
      })
      output$sim_plots <- renderPlot({
        par(mfrow = c(1, 2), mar=c(5,4,4,2))
        plot(fitted(modelo), rstandard(modelo), xlab = "Predichos", ylab = "Residuos Estandarizados", main = "Residuos vs Predichos", pch = 16, col = "blue"); abline(h = 0, col = "red", lwd = 2, lty = 2)
        car::qqPlot(modelo$residuals, main = "QQ-Plot de Residuos", ylab = "Cuantiles", pch = 16, col = "blue")
      })
      output$sim_pmm_out <- renderPrint({
        alpha_val <- input$sim_alpha; test_name <- input$sim_pmm
        cat(paste("--- Prueba:", test_name, "| Alpha:", alpha_val, "---\n\n"))
        tryCatch({
          if(test_name == "Tukey") print(HSD.test(modelo, input$sim_trt, alpha = alpha_val, console = TRUE))
          else if(test_name == "Fisher (LSD)") print(LSD.test(modelo, input$sim_trt, p.adj = "none", alpha = alpha_val, console = TRUE))
          else if(test_name == "Duncan") print(duncan.test(modelo, input$sim_trt, alpha = alpha_val, console = TRUE))
          else if(test_name == "SNK") print(SNK.test(modelo, input$sim_trt, alpha = alpha_val, console = TRUE))
          else if(test_name == "Bonferroni") print(LSD.test(modelo, input$sim_trt, p.adj = "bonferroni", alpha = alpha_val, console = TRUE))
          else if(test_name == "Scott-Knott") { sk <- SK(modelo, which = input$sim_trt, sig.level = alpha_val, dispersion="se"); summary(sk) }
        }, error = function(e) cat("Fallo en PMM:", e$message, "\n"))
      })
    }, error = function(e) { showNotification(paste("Error en análisis:", e$message), type="error") })
  })
  
  output$sim_pmm_plot <- renderPlot({
    req(input$sim_pmm %in% c("Tukey", "Fisher (LSD)"), vals$datos, input$sim_y, input$sim_trt)
    df <- vals$datos; y_var <- input$sim_y; trt_var <- input$sim_trt
    vars_to_keep <- c(y_var, trt_var)
    if(input$sim_des == "DBCA") vars_to_keep <- c(vars_to_keep, input$sim_blk)
    if(input$sim_des == "DCL") vars_to_keep <- c(vars_to_keep, input$sim_row, input$sim_col)
    
    df[[y_var]] <- as.numeric(as.character(df[[y_var]])); df <- df[complete.cases(df[, vars_to_keep]), ]; df[[trt_var]] <- as.factor(df[[trt_var]])
    if(input$sim_des == "DBCA") df[[input$sim_blk]] <- as.factor(df[[input$sim_blk]])
    if(input$sim_des == "DCL") { df[[input$sim_row]] <- as.factor(df[[input$sim_row]]); df[[input$sim_col]] <- as.factor(df[[input$sim_col]]) }
    
    form_str <- switch(input$sim_des, "DCA" = paste(y_var, "~", trt_var), "DBCA" = paste(y_var, "~", trt_var, "+", input$sim_blk), "DCL" = paste(y_var, "~", trt_var, "+", input$sim_row, "+", input$sim_col))
    modelo <- aov(as.formula(form_str), data = df)
    
    alpha_val <- input$sim_alpha
    pmm_res <- if(input$sim_pmm == "Tukey") HSD.test(modelo, trt_var, alpha = alpha_val) else LSD.test(modelo, trt_var, p.adj = "none", alpha = alpha_val)
    tabla_letras <- data.frame(Tratamiento = as.character(rownames(pmm_res$groups)), Letra = as.character(pmm_res$groups$groups), stringsAsFactors = FALSE)
    
    group_cols <- trt_var; facet_var <- NULL
    if(input$sim_facet && input$sim_des %in% c("DBCA", "DCL")) { facet_var <- if(input$sim_des == "DBCA") input$sim_blk else input$sim_row; group_cols <- c(trt_var, facet_var) }
    
    df_stats <- df %>% group_by(across(all_of(group_cols))) %>% summarise(media_r = mean(.data[[y_var]], na.rm = TRUE), n_val = n(), sd_val = sd(.data[[y_var]], na.rm = TRUE), se_val = sd_val / sqrt(n_val), .groups = "drop")
    df_stats$sd_val[is.na(df_stats$sd_val)] <- 0; df_stats$se_val[is.na(df_stats$se_val)] <- 0
    df_stats <- df_stats %>% mutate(disp = case_when(input$sim_plot_err == "se" ~ se_val, input$sim_plot_err == "sd" ~ sd_val, input$sim_plot_err == "ic" ~ se_val * qt(0.975, df = ifelse(n_val > 1, n_val - 1, 1))))
    
    df_stats[[trt_var]] <- as.character(df_stats[[trt_var]]); datos_plot <- df_stats %>% left_join(tabla_letras, by = setNames("Tratamiento", trt_var))
    
    gg <- ggplot(datos_plot, aes(y = reorder(.data[[trt_var]], media_r), x = media_r)) +
      geom_errorbarh(aes(xmin = media_r - disp, xmax = media_r + disp), height = 0.2) + geom_point(size = 3, color = "black") +
      geom_text(aes(label = sprintf("%.2f %s", media_r, trimws(Letra))), hjust = -0.1, vjust = -1, size = 4.5, family="sans") +
      scale_x_continuous(expand = expansion(mult = c(0.1, 0.5))) + labs(x = paste("Media Real ±", toupper(input$sim_plot_err)), y = "Tratamiento", title = paste("Medias Reales -", input$sim_pmm)) +
      theme_classic(base_size = 14) + theme(strip.text = element_text(size = 13, face = "bold"), panel.spacing = unit(1, "lines"))
    
    if(!is.null(facet_var)) gg <- gg + facet_wrap(as.formula(paste("~", facet_var)), scales = "free_x", ncol = input$sim_facet_ncol)
    gg
  })
  
  # =====================================================================
  # --- DISEÑOS EXPERIMENTALES: FACTORIALES ---
  # =====================================================================
  
  output$fac_vars_ui <- renderUI({
    req(vals$datos); vars <- names(vals$datos)
    tagList(
      selectInput("fac_y", "Variable Respuesta (Y):", choices = c("Seleccione..."="", vars)), selectInput("fac_A", "Factor A:", choices = c("Seleccione..."="", vars)), selectInput("fac_B", "Factor B:", choices = c("Seleccione..."="", vars)), 
      conditionalPanel("input.fac_arr == 'Parcelas Subdivididas' || input.fac_arr == 'Franjas Divididas'", selectInput("fac_C", "Factor C:", choices = c("Seleccione..."="", vars))), 
      conditionalPanel("input.fac_des == 'DBCA'", selectInput("fac_blk", "Bloque:", choices = c("Seleccione..."="", vars))), 
      conditionalPanel("input.fac_des == 'DCL'", selectInput("fac_row", "Fila:", choices = c("Seleccione..."="", vars)), selectInput("fac_col", "Columna:", choices = c("Seleccione..."="", vars)))
    )
  })
  
  output$fac_interpretacion_ui <- renderUI({
    req(input$fac_arr)
    texto <- switch(input$fac_arr,
                    "Factorial Combinatorio" = "<p><b>Modelo Factorial:</b> Y<sub>ijk</sub> = &mu; + &tau;<sub>i</sub> + &beta;<sub>j</sub> + (&tau;&beta;)<sub>ij</sub> + &epsilon;<sub>ijk</sub></p><p>Evalúa el efecto individual de cada factor y el efecto conjunto (interacción).</p>",
                    "Parcelas Divididas" = "<p><b>Modelo Parcelas Divididas:</b> Diseño restrictivo donde un factor (Parcela Principal) se aleatoriza a un nivel, y el otro factor (Subparcela) se aleatoriza dentro del primero. Posee dos errores experimentales: Error(A) y Error(B).</p>",
                    "Parcelas Subdivididas" = "<p><b>Modelo Parcelas Subdivididas:</b> Extensión con un tercer factor (Sub-subparcela). Posee tres errores experimentales.</p>",
                    "Franjas" = "<p><b>Modelo en Franjas (Strip-Plot):</b> Los factores se cruzan en direcciones perpendiculares, creando un Error conjunto para la interacción y errores separados para las franjas.</p>",
                    "Franjas Divididas" = "<p><b>Modelo Franjas Divididas:</b> Combina Franjas para A y B, y Parcelas Divididas para el Factor C.</p>")
    HTML(paste0("<div class='interpretation-box'>", texto, "</div>"))
  })
  
  output$fac_pmm_terms_ui <- renderUI({
    req(input$fac_A, input$fac_B); if(input$fac_A == "" || input$fac_B == "") return(NULL)
    req_C <- input$fac_arr %in% c("Parcelas Subdivididas", "Franjas Divididas")
    if(req_C && !is.null(input$fac_C) && input$fac_C != "") { choices <- c(input$fac_A, input$fac_B, input$fac_C, paste(input$fac_A, input$fac_B, sep=":"), paste(input$fac_A, input$fac_C, sep=":"), paste(input$fac_B, input$fac_C, sep=":"), paste(input$fac_A, input$fac_B, input$fac_C, sep=":")) 
    } else { choices <- c(input$fac_A, input$fac_B, paste(input$fac_A, input$fac_B, sep=":")) }
    checkboxGroupInput("fac_pmm_terms", "Efectos a evaluar en PMM (Múltiple):", choices = choices, selected = choices[1])
  })
  
  output$fac_plot_options_ui <- renderUI({
    req(input$fac_pmm %in% c("Tukey", "Fisher (LSD)"), input$fac_pmm_terms)
    fac_choices <- c(); if(input$fac_des == "DBCA") fac_choices <- input$fac_blk; if(input$fac_des == "DCL") fac_choices <- c(input$fac_row, input$fac_col)
    tagList(
      hr(), h4("Configuración de Gráficos"),
      selectInput("fac_plot_term", "Efecto a Graficar (Barras/Líneas):", choices = input$fac_pmm_terms),
      selectInput("fac_plot_err", "Error en barras:", choices = c("SE"="se", "SD"="sd", "IC 95%"="ic")),
      if(length(fac_choices) > 0) { tagList(checkboxInput("fac_facet", "Facetar por Bloque/Fila", value=FALSE), conditionalPanel("input.fac_facet == true", selectInput("fac_facet_var", "Faceta:", choices = fac_choices), numericInput("fac_facet_ncol", "Columnas:", value=2, min=1))) }
    )
  })
  
  observeEvent(input$run_fac, {
    req(vals$datos, input$fac_y, input$fac_A, input$fac_B)
    if(input$fac_y == "" || input$fac_A == "" || input$fac_B == "") { showNotification("Seleccione variables.", type="warning"); return() }
    
    df <- vals$datos; vars_to_keep <- c(input$fac_y, input$fac_A, input$fac_B)
    if(input$fac_arr %in% c("Parcelas Subdivididas", "Franjas Divididas")) vars_to_keep <- c(vars_to_keep, input$fac_C)
    if(input$fac_des == "DBCA") vars_to_keep <- c(vars_to_keep, input$fac_blk)
    if(input$fac_des == "DCL") vars_to_keep <- c(vars_to_keep, input$fac_row, input$fac_col)
    
    if(any(vars_to_keep == "")) { showNotification("Faltan variables por definir.", type="warning"); return() }
    df[[input$fac_y]] <- as.numeric(as.character(df[[input$fac_y]])); df <- df[complete.cases(df[, vars_to_keep]), ]
    if(nrow(df) < 2) { showNotification("Datos insuficientes tras omitir NA.", type="error"); return() }
    
    df[[input$fac_A]] <- as.factor(df[[input$fac_A]]); df[[input$fac_B]] <- as.factor(df[[input$fac_B]])
    if(input$fac_arr %in% c("Parcelas Subdivididas", "Franjas Divididas")) df[[input$fac_C]] <- as.factor(df[[input$fac_C]])
    if(input$fac_des == "DBCA") df[[input$fac_blk]] <- as.factor(df[[input$fac_blk]])
    if(input$fac_des == "DCL") { df[[input$fac_row]] <- as.factor(df[[input$fac_row]]); df[[input$fac_col]] <- as.factor(df[[input$fac_col]]) }
    df <- droplevels(df)
    
    y <- input$fac_y; A <- input$fac_A; B <- input$fac_B; des <- input$fac_des; arr <- input$fac_arr
    base_str <- if(des=="DBCA") paste0(input$fac_blk, " + ") else if(des=="DCL") paste0(input$fac_row, " + ", input$fac_col, " + ") else ""
    
    tryCatch({
      if(arr == "Factorial Combinatorio") { 
        if(!is.null(input$fac_C) && input$fac_C != "") { form_anova <- paste(y, "~", base_str, A, "*", B, "*", input$fac_C); form_fix <- form_anova
        } else { form_anova <- paste(y, "~", base_str, A, "*", B); form_fix <- form_anova }
      } else if (arr == "Parcelas Divididas") { form_anova <- paste(y, "~", base_str, A, "+", if(des=="DBCA") paste0("Error(", input$fac_blk, "/", A, ")") else paste0("Error(Rep/", A, ")"), "+", B, "+", A, ":", B); form_fix <- paste(y, "~", base_str, A, "*", B) 
      } else if (arr == "Parcelas Subdivididas") { C <- input$fac_C; form_anova <- paste(y, "~", base_str, A, "*", B, "*", C, "+ Error(", input$fac_blk, "/", A, "/", B, ")"); form_fix <- paste(y, "~", base_str, A, "*", B, "*", C) 
      } else if (arr == "Franjas") { form_anova <- paste(y, "~", base_str, A, "*", B, "+ Error(", input$fac_blk, "/(", A, "+", B, "))"); form_fix <- paste(y, "~", base_str, A, "*", B) 
      } else if (arr == "Franjas Divididas") { C <- input$fac_C; form_anova <- paste(y, "~", A, "*", B, "*", C, "+ Error(", input$fac_blk, "/(", A, "*", B, "))"); form_fix <- paste(y, "~", base_str, A, "*", B, "*", C) }
      
      modelo_anova <- aov(as.formula(form_anova), data = df)
      modelo_fix <- aov(as.formula(form_fix), data = df) 
      
      output$fac_anova <- renderPrint({ cat("--- Tabla de ANDEVA (Errores Anidados) ---\n"); print(summary(modelo_anova)); cat("\n--- Coeficiente de Variación Global ---\n"); print(agricolae::cv.model(modelo_fix)) })
      output$fac_supuestos <- renderPrint({ 
        cat("--- Normalidad de Residuales (Shapiro-Wilk) ---\n"); tryCatch({ print(shapiro.test(resid(modelo_fix))) }, error=function(e) cat("Fallo:", e$message, "\n"))
        cat("\n--- Homocedasticidad Factor A (Levene) ---\n"); tryCatch({ print(car::leveneTest(as.formula(paste(y, "~", A)), data=df, center="median")) }, error=function(e) cat("Fallo:", e$message, "\n"))
        cat("\n--- Homocedasticidad Factor B (Levene) ---\n"); tryCatch({ print(car::leveneTest(as.formula(paste(y, "~", B)), data=df, center="median")) }, error=function(e) cat("Fallo:", e$message, "\n"))
        if(arr %in% c("Parcelas Subdivididas", "Franjas Divididas")){ cat("\n--- Homocedasticidad Factor C (Levene) ---\n"); tryCatch({ print(car::leveneTest(as.formula(paste(y, "~", input$fac_C)), data=df, center="median")) }, error=function(e) cat("Fallo:", e$message, "\n")) }
        cat("\n--- Prueba de Independencia (Durbin-Watson) ---\n"); tryCatch({ print(lmtest::dwtest(modelo_fix)) }, error=function(e) cat("Fallo:", e$message, "\n"))
      })
      output$fac_plots <- renderPlot({
        par(mfrow = c(1, 2), mar=c(5,4,4,2))
        plot(fitted(modelo_fix), rstandard(modelo_fix), xlab = "Predichos", ylab = "Residuos", main = "Residuos vs Predichos", pch = 16, col = "blue"); abline(h = 0, col = "red", lwd = 2, lty = 2)
        car::qqPlot(modelo_fix$residuals, main = "QQ-Plot Global", ylab = "Cuantiles", pch = 16, col = "blue")
      })
      
      output$fac_pmm_out <- renderPrint({ 
        req(input$fac_pmm_terms); alpha_val <- input$fac_alpha; test_name <- input$fac_pmm
        cat(paste("--- Prueba Principal:", test_name, "| Alpha:", alpha_val, "---\n"))
        for(term in input$fac_pmm_terms) {
          cat("\n======================================================\n => COMPARACIÓN DE MEDIAS PARA:", term, "\n======================================================\n")
          term_eval <- unlist(strsplit(term, ":"))
          tryCatch({
            if(test_name == "Scott-Knott") {
              if(length(term_eval) > 1) {
                df$SK_INT <- interaction(df[, term_eval], sep="_")
                mod_sk <- aov(as.formula(paste(y, "~", base_str, "SK_INT")), data=df)
                sk <- SK(mod_sk, which = "SK_INT", sig.level = alpha_val, dispersion = "se"); summary(sk)
              } else { sk <- SK(modelo_fix, which = term, sig.level = alpha_val, dispersion = "se"); summary(sk) }
            } else {
              if(test_name == "Tukey") print(HSD.test(modelo_fix, term_eval, alpha=alpha_val, console=T)) 
              else if(test_name == "Fisher (LSD)") print(LSD.test(modelo_fix, term_eval, p.adj="none", alpha=alpha_val, console=T)) 
              else if(test_name == "Duncan") print(duncan.test(modelo_fix, term_eval, alpha=alpha_val, console=T)) 
              else if(test_name == "SNK") print(SNK.test(modelo_fix, term_eval, alpha=alpha_val, console=T)) 
              else if(test_name == "Bonferroni") print(LSD.test(modelo_fix, term_eval, p.adj="bonferroni", alpha=alpha_val, console=T))
            }
          }, error = function(e) cat("No se pudo procesar la PMM para este término:", e$message, "\n"))
        }
      })
    }, error = function(e) { showNotification(paste("Error Factorial:", e$message), type="error") })
  })
  
  # Gráfico Combinado PMM Factorial
  output$fac_pmm_plot <- renderPlot({
    req(input$fac_pmm %in% c("Tukey", "Fisher (LSD)"), input$fac_plot_term, vals$datos)
    df <- vals$datos; y_var <- input$fac_y
    term_eval <- unlist(strsplit(input$fac_plot_term, ":"))
    
    # Crear modelo fijo interno para extraer letras
    form_str <- switch(input$fac_des, "DCA" = paste(y_var, "~", paste(term_eval, collapse="*")), "DBCA" = paste(y_var, "~", input$fac_blk, "+", paste(term_eval, collapse="*")), "DCL" = paste(y_var, "~", input$fac_row, "+", input$fac_col, "+", paste(term_eval, collapse="*")))
    mod_temp <- aov(as.formula(form_str), data=df)
    
    alpha_val <- input$fac_alpha
    pmm_res <- if(input$fac_pmm == "Tukey") HSD.test(mod_temp, term_eval, alpha = alpha_val) else LSD.test(mod_temp, term_eval, p.adj = "none", alpha = alpha_val)
    tabla_letras <- data.frame(Tratamiento = as.character(rownames(pmm_res$groups)), Letra = as.character(pmm_res$groups$groups), stringsAsFactors = FALSE)
    
    group_cols <- term_eval; facet_var <- NULL
    if(!is.null(input$fac_facet) && input$fac_facet == TRUE && !is.null(input$fac_facet_var)) { facet_var <- input$fac_facet_var; group_cols <- unique(c(group_cols, facet_var)) }
    
    df_stats <- df %>% group_by(across(all_of(group_cols))) %>% summarise(media_r = mean(.data[[y_var]], na.rm = TRUE), n_val = n(), sd_val = sd(.data[[y_var]], na.rm = TRUE), se_val = sd_val / sqrt(n_val), .groups = "drop")
    df_stats$sd_val[is.na(df_stats$sd_val)] <- 0; df_stats$se_val[is.na(df_stats$se_val)] <- 0
    df_stats <- df_stats %>% mutate(disp = case_when(input$fac_plot_err == "se" ~ se_val, input$fac_plot_err == "sd" ~ sd_val, input$fac_plot_err == "ic" ~ se_val * qt(0.975, df = ifelse(n_val > 1, n_val - 1, 1))))
    
    if(length(term_eval) > 1) { df_stats$Tratamiento <- apply(df_stats[, term_eval], 1, paste, collapse=":") } else { df_stats$Tratamiento <- as.character(df_stats[[term_eval]]) }
    datos_plot <- df_stats %>% left_join(tabla_letras, by = "Tratamiento")
    
    gg <- ggplot(datos_plot, aes(y = reorder(Tratamiento, media_r), x = media_r)) +
      geom_errorbarh(aes(xmin = media_r - disp, xmax = media_r + disp), height = 0.2) + geom_point(size = 3, color = "black") +
      geom_text(aes(label = sprintf("%.2f %s", media_r, trimws(Letra))), hjust = -0.1, vjust = -1, size = 4.5, family="sans") +
      scale_x_continuous(expand = expansion(mult = c(0.1, 0.5))) + labs(x = paste("Media ±", toupper(input$fac_plot_err)), y = input$fac_plot_term, title = paste("Comparación -", input$fac_plot_term)) +
      theme_classic(base_size = 14) + theme(strip.text = element_text(size = 13, face = "bold"), panel.spacing = unit(1, "lines"))
    if(!is.null(facet_var)) { gg <- gg + facet_wrap(as.formula(paste("~", facet_var)), scales = "free_x", ncol = input$fac_facet_ncol) }
    gg
  })
  
  # Gráfico de Líneas de Interacción (Solo se dibuja si se seleccionó una interacción)
  output$fac_interaction_plot <- renderPlot({
    req(input$fac_plot_term)
    term_eval <- unlist(strsplit(input$fac_plot_term, ":"))
    if(length(term_eval) < 2) return(NULL) 
    
    df <- vals$datos
    x_var <- term_eval[1]
    color_var <- term_eval[2]
    
    if(length(term_eval) == 2) {
      sum_df <- df %>% group_by(across(all_of(c(x_var, color_var)))) %>% summarise(Media = mean(.data[[input$fac_y]], na.rm=TRUE), .groups="drop")
      ggplot(sum_df, aes(x = as.factor(.data[[x_var]]), y = Media, color = as.factor(.data[[color_var]]), group = as.factor(.data[[color_var]]))) +
        geom_line(linewidth = 1.2) + geom_point(size = 4) + theme_classic(base_size = 14) + labs(title = paste("Interacción:", x_var, "y", color_var), y = "Media", x = x_var, color = color_var)
    } else { # Interacción triple
      facet_var_3 <- term_eval[3]
      sum_df <- df %>% group_by(across(all_of(c(x_var, color_var, facet_var_3)))) %>% summarise(Media = mean(.data[[input$fac_y]], na.rm=TRUE), .groups="drop")
      ggplot(sum_df, aes(x = as.factor(.data[[x_var]]), y = Media, color = as.factor(.data[[color_var]]), group = as.factor(.data[[color_var]]))) +
        geom_line(linewidth = 1.2) + geom_point(size = 4) + facet_wrap(as.formula(paste("~", facet_var_3))) + theme_classic(base_size = 14) + labs(title = paste("Interacción Triple:", input$fac_plot_term), y = "Media", x = x_var, color = color_var)
    }
  })
  
  # =====================================================================
  # --- DISEÑOS EXPERIMENTALES: MULTI-AMBIENTALES ---
  # =====================================================================
  
  output$ma_vars_ui <- renderUI({
    req(vals$datos); vars <- names(vals$datos)
    tagList(
      selectInput("ma_y", "Variable Respuesta (Y):", choices = c("Seleccione..."="", vars)),
      selectInput("ma_gen", "Factor 1 (Línea / Genotipo / Tratamiento):", choices = c("Seleccione..."="", vars)),
      selectInput("ma_amb", "Factor 2 (Ambiente / Localidad / Zafra):", choices = c("Seleccione..."="", vars)),
      selectInput("ma_blk", "Bloque / Réplica:", choices = c("Seleccione..."="", vars)),
      helpText("Nota: Si tu ambiente es una combinación (ej. Finca + Año), crea primero la combinación o elígela aquí.")
    )
  })
  
  output$ma_interpretacion_ui <- renderUI({
    req(input$ma_model_type)
    texto <- switch(input$ma_model_type,
                    "fixed" = "<p><b>Modelo de Efectos Fijos (ANDEVA Clásico):</b> Y<sub>ijk</sub> = &mu; + G<sub>i</sub> + E<sub>j</sub> + B<sub>k(j)</sub> + (GE)<sub>ij</sub> + &epsilon;<sub>ijk</sub></p><p>Evalúa el Genotipo (G), Ambiente (E) y su interacción (GE) como fijos. El Bloque (B) está anidado dentro del Ambiente.</p>",
                    "mixed" = "<p><b>Modelo Lineal Mixto (MLM):</b> Y<sub>ijk</sub> = &mu; + G<sub>i</sub> + E<sub>j</sub> + b<sub>k(j)</sub> + &epsilon;<sub>ijk</sub></p><p>Utiliza <code>lme4::lmer()</code>. Genotipo y Ambiente son efectos fijos, mientras que el Bloque anidado en el Ambiente <code>(1|Ambiente:Bloque)</code> se asume como efecto aleatorio. Las medias se ajustan mediante <code>emmeans</code>.</p>")
    HTML(paste0("<div class='interpretation-box'>", texto, "</div>"))
  })
  
  output$ma_pmm_terms_ui <- renderUI({
    req(input$ma_gen, input$ma_amb); if(input$ma_gen == "" || input$ma_amb == "") return(NULL)
    choices <- c(input$ma_gen, input$ma_amb, paste(input$ma_gen, input$ma_amb, sep=":"))
    checkboxGroupInput("ma_pmm_terms", "Evaluar PMM para:", choices = choices, selected = input$ma_gen)
  })
  
  output$ma_facet_ui <- renderUI({
    req(input$ma_gen, input$ma_amb); if(input$ma_gen == "" || input$ma_amb == "") return(NULL)
    selectInput("ma_facet_var", "Variable para Facetas (Paneles):", choices = c(input$ma_amb, input$ma_gen))
  })
  
  observeEvent(input$run_ma, {
    req(vals$datos, input$ma_y, input$ma_gen, input$ma_amb, input$ma_blk)
    if(any(c(input$ma_y, input$ma_gen, input$ma_amb, input$ma_blk) == "")) { showNotification("Faltan variables por seleccionar.", type="warning"); return() }
    
    df <- vals$datos
    vars_to_keep <- c(input$ma_y, input$ma_gen, input$ma_amb, input$ma_blk)
    df[[input$ma_y]] <- as.numeric(as.character(df[[input$ma_y]]))
    df <- df[complete.cases(df[, vars_to_keep]), ]
    if(nrow(df) < 2) { showNotification("Datos insuficientes.", type="error"); return() }
    
    df[[input$ma_gen]] <- as.factor(df[[input$ma_gen]])
    df[[input$ma_amb]] <- as.factor(df[[input$ma_amb]])
    df[[input$ma_blk]] <- as.factor(df[[input$ma_blk]])
    df <- droplevels(df)
    
    y <- input$ma_y; G <- input$ma_gen; E <- input$ma_amb; B <- input$ma_blk
    
    tryCatch({
      if(input$ma_model_type == "fixed") {
        form_anova <- paste(y, "~", G, "*", E, "+ Error(", B, "/", E, ")")
        form_fix <- paste(y, "~", G, "*", E, "+", B, "/", E)
        modelo_anova <- aov(as.formula(form_anova), data = df)
        modelo_fix <- aov(as.formula(form_fix), data = df)
        
        output$ma_anova <- renderPrint({ cat("--- ANDEVA (Efectos Fijos Multi-Ambiente) ---\n"); print(summary(modelo_anova)); cat("\n--- CV Global ---\n"); print(agricolae::cv.model(modelo_fix)) })
        
      } else {
        # Modelo Mixto con lme4
        form_mixed <- paste(y, "~", G, "+", E, "+ (1|", E, ":", B, ")")
        modelo_mix <- lmerTest::lmer(as.formula(form_mixed), data = df)
        modelo_fix <- aov(as.formula(paste(y, "~", G, "*", E)), data = df) # Para supuestos básicos
        
        output$ma_anova <- renderPrint({ cat("--- ANDEVA del Modelo Mixto (lmerTest) ---\n"); print(anova(modelo_mix)) })
      }
      
      output$ma_supuestos <- renderPrint({ 
        cat("--- Normalidad de Residuales (Shapiro-Wilk) ---\n")
        resids <- if(input$ma_model_type == "fixed") resid(modelo_fix) else resid(modelo_mix)
        tryCatch({ print(shapiro.test(resids)) }, error=function(e) cat("Fallo:", e$message, "\n"))
      })
      
      output$ma_plots <- renderPlot({
        mod_plot <- if(input$ma_model_type == "fixed") modelo_fix else modelo_mix
        par(mfrow = c(1, 2), mar=c(5,4,4,2))
        plot(fitted(mod_plot), resid(mod_plot), xlab = "Predichos", ylab = "Residuos", main = "Residuos vs Predichos", pch = 16, col = "blue"); abline(h = 0, col = "red", lwd = 2, lty = 2)
        car::qqPlot(resid(mod_plot), main = "QQ-Plot Global", ylab = "Cuantiles", pch = 16, col = "blue")
      })
      
      output$ma_pmm_out <- renderPrint({ 
        req(input$ma_pmm_terms); alpha_val <- input$ma_alpha
        cat(paste("--- Prueba:", input$ma_pmm, "| Alpha:", alpha_val, "---\n"))
        
        for(term in input$ma_pmm_terms) {
          cat("\n======================================================\n => COMPARACIÓN DE MEDIAS PARA:", term, "\n======================================================\n")
          term_eval <- unlist(strsplit(term, ":"))
          
          tryCatch({
            if(input$ma_model_type == "fixed") {
              if(input$ma_pmm == "Scott-Knott") {
                if(length(term_eval) > 1) { df$SK_INT <- interaction(df[, term_eval], sep="_"); mod_sk <- aov(as.formula(paste(y, "~ SK_INT")), data=df); sk <- SK(mod_sk, which = "SK_INT", sig.level = alpha_val, dispersion = "se"); summary(sk) } else { sk <- SK(modelo_fix, which = term, sig.level = alpha_val, dispersion = "se"); summary(sk) }
              } else {
                if(input$ma_pmm == "Tukey") print(HSD.test(modelo_fix, term_eval, alpha=alpha_val, console=T)) else print(LSD.test(modelo_fix, term_eval, p.adj="none", alpha=alpha_val, console=T))
              }
            } else {
              # PMM para Modelo Mixto usando emmeans
              cat("Calculando Medias Ajustadas (emmeans) para LMM...\n")
              em_formula <- as.formula(paste("~", paste(term_eval, collapse="*")))
              em <- emmeans::emmeans(modelo_mix, em_formula)
              adjust_meth <- if(input$ma_pmm == "Tukey") "tukey" else "none"
              res_cld <- multcomp::cld(em, adjust = adjust_meth, Letters = letters, alpha = alpha_val, reverse = TRUE)
              print(res_cld)
            }
          }, error = function(e) cat("Fallo en PMM:", e$message, "\n"))
        }
      })
    }, error = function(e) { showNotification(paste("Error Multi-Ambiente:", e$message), type="error") })
  })
  
  # Gráfico Combinado PMM Multi-Ambiente
  output$ma_pmm_plot <- renderPlot({
    req(vals$datos, input$ma_pmm_terms)
    df <- vals$datos; y_var <- input$ma_y
    term_eval <- unlist(strsplit(input$ma_pmm_terms[1], ":")) # Grafica el primer término seleccionado
    
    alpha_val <- input$ma_alpha
    if(input$ma_model_type == "fixed") {
      mod_temp <- aov(as.formula(paste(y_var, "~", paste(term_eval, collapse="*"))), data=df)
      pmm_res <- if(input$ma_pmm == "Tukey") HSD.test(mod_temp, term_eval, alpha = alpha_val) else LSD.test(mod_temp, term_eval, p.adj = "none", alpha = alpha_val)
      tabla_letras <- data.frame(Tratamiento = as.character(rownames(pmm_res$groups)), Letra = as.character(pmm_res$groups$groups), stringsAsFactors = FALSE)
    } else {
      form_mixed <- paste(y_var, "~", input$ma_gen, "+", input$ma_amb, "+ (1|", input$ma_amb, ":", input$ma_blk, ")")
      mod_temp <- lmerTest::lmer(as.formula(form_mixed), data = df)
      em <- emmeans::emmeans(mod_temp, as.formula(paste("~", paste(term_eval, collapse="*"))))
      res_cld <- as.data.frame(multcomp::cld(em, adjust = ifelse(input$ma_pmm=="Tukey", "tukey", "none"), Letters = letters, alpha = alpha_val))
      
      if(length(term_eval) > 1) { res_cld$Tratamiento <- apply(res_cld[, term_eval], 1, paste, collapse=":") } else { res_cld$Tratamiento <- as.character(res_cld[[term_eval]]) }
      tabla_letras <- data.frame(Tratamiento = res_cld$Tratamiento, Letra = as.character(res_cld$.group), stringsAsFactors = FALSE)
    }
    
    group_cols <- term_eval; facet_var <- NULL
    if(input$ma_facet && !is.null(input$ma_facet_var)) { facet_var <- input$ma_facet_var; group_cols <- unique(c(group_cols, facet_var)) }
    
    df_stats <- df %>% group_by(across(all_of(group_cols))) %>% summarise(media_r = mean(.data[[y_var]], na.rm = TRUE), n_val = n(), sd_val = sd(.data[[y_var]], na.rm = TRUE), se_val = sd_val / sqrt(n_val), .groups = "drop")
    df_stats$sd_val[is.na(df_stats$sd_val)] <- 0; df_stats$se_val[is.na(df_stats$se_val)] <- 0
    df_stats <- df_stats %>% mutate(disp = case_when(input$ma_plot_err == "se" ~ se_val, input$ma_plot_err == "sd" ~ sd_val, input$ma_plot_err == "ic" ~ se_val * qt(0.975, df = ifelse(n_val > 1, n_val - 1, 1))))
    
    if(length(term_eval) > 1) { df_stats$Tratamiento <- apply(df_stats[, term_eval], 1, paste, collapse=":") } else { df_stats$Tratamiento <- as.character(df_stats[[term_eval]]) }
    datos_plot <- df_stats %>% left_join(tabla_letras, by = "Tratamiento")
    
    gg <- ggplot(datos_plot, aes(y = reorder(Tratamiento, media_r), x = media_r)) +
      geom_errorbarh(aes(xmin = media_r - disp, xmax = media_r + disp), height = 0.2) + geom_point(size = 3, color = "black") +
      geom_text(aes(label = sprintf("%.2f %s", media_r, trimws(Letra))), hjust = -0.1, vjust = -1, size = 4.5, family="sans") +
      scale_x_continuous(expand = expansion(mult = c(0.1, 0.5))) + labs(x = paste("Media Reales ±", toupper(input$ma_plot_err)), y = "Factor Analizado", title = paste("Medias Multi-Ambiente -", input$ma_pmm)) +
      theme_classic(base_size = 14) + theme(strip.text = element_text(size = 13, face = "bold"), panel.spacing = unit(1, "lines"))
    
    if(!is.null(facet_var)) { gg <- gg + facet_wrap(as.formula(paste("~", facet_var)), scales = "free_x", ncol = input$ma_facet_ncol) }
    gg
  })
  
  # Gráfico de Interacción (Líneas de error facetadas)
  output$ma_int_plot <- renderPlot({
    req(vals$datos, input$ma_gen, input$ma_amb)
    df <- vals$datos; x_var <- input$ma_gen; color_var <- input$ma_amb
    
    sum_df <- df %>% group_by(across(all_of(c(x_var, color_var)))) %>% 
      summarise(Media = mean(.data[[input$ma_y]], na.rm=TRUE), SE = sd(.data[[input$ma_y]], na.rm=TRUE)/sqrt(n()), SD = sd(.data[[input$ma_y]], na.rm=TRUE), .groups="drop") %>%
      mutate(disp = if(input$ma_plot_err=="se") SE else if(input$ma_plot_err=="sd") SD else SE*1.96)
    
    sum_df$disp[is.na(sum_df$disp)] <- 0
    
    gg <- ggplot(sum_df, aes(x = as.factor(.data[[x_var]]), y = Media, color = as.factor(.data[[color_var]]), group = as.factor(.data[[color_var]]))) +
      geom_line(linewidth = 1.2) + geom_point(size = 4) +
      geom_errorbar(aes(ymin = Media - disp, ymax = Media + disp), width = 0.2, linewidth=0.8) +
      theme_classic(base_size = 14) + labs(title = "Interacción Genotipo x Ambiente", y = paste("Media de", input$ma_y), x = "Genotipo / Línea", color = "Ambiente") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    if(input$ma_facet && !is.null(input$ma_facet_var)) {
      # Facetar si se solicitó (útil si hay un 3er factor, o para separar por ambiente)
      gg <- gg + facet_wrap(as.formula(paste("~", input$ma_facet_var)), ncol = input$ma_facet_ncol)
    }
    gg
  })
  
  # =====================================================================
  # --- TRANSFORMACIONES Y ANCOVA ---
  # =====================================================================
  
  output$trans_var_ui <- renderUI({ req(vals$datos); selectInput("trans_var", "Variable a Transformar:", choices = c("Seleccione..."="", names(select_if(vals$datos, is.numeric)))) })
  trans_data <- reactive({
    req(vals$datos, input$trans_var, input$trans_method)
    if(input$trans_var == "") return(NULL)
    x <- as.numeric(as.character(vals$datos[[input$trans_var]])); x_clean <- x[!is.na(x)]
    if (input$trans_method == "log10") res <- log10(x + ifelse(min(x, na.rm=T)<=0, abs(min(x, na.rm=T))+1, 0))
    if (input$trans_method == "ln") res <- log(x + ifelse(min(x, na.rm=T)<=0, abs(min(x, na.rm=T))+1, 0))
    if (input$trans_method == "sqrt") res <- sqrt(x + ifelse(min(x, na.rm=T)<0, abs(min(x, na.rm=T)), 0))
    if (input$trans_method == "arcsin") res <- asin(sqrt(x/100))
    if (input$trans_method == "boxcox") { bc <- MASS::boxcox(x_clean ~ 1, plotit = FALSE); lambda <- bc$x[which.max(bc$y)]; res <- if(round(lambda, 2) == 0) log(x) else (x^lambda - 1)/lambda }
    list(orig = x, new = res)
  })
  output$trans_plot_orig <- renderPlot({ req(trans_data()); x <- trans_data()$orig; par(mfrow=c(2,1), mar=c(4,4,2,1)); hist(x, col="lightblue", main="Original"); car::qqPlot(x, main="QQ-Plot Original") })
  output$trans_plot_new <- renderPlot({ req(trans_data()); x <- trans_data()$new; par(mfrow=c(2,1), mar=c(4,4,2,1)); hist(x, col="lightgreen", main="Transformada"); car::qqPlot(x, main="QQ-Plot Transformada") })
  output$trans_shapiro <- renderPrint({ req(trans_data()); cat("--- Original ---\n"); print(shapiro.test(trans_data()$orig)); cat("\n--- Transformada ---\n"); print(shapiro.test(trans_data()$new)) })
  observeEvent(input$run_trans, { req(trans_data(), input$trans_var); new_col <- paste0(input$trans_var, "_", input$trans_method); df <- vals$datos; df[[new_col]] <- trans_data()$new; vals$datos <- df; refresh_trigger(refresh_trigger() + 1); showNotification("Guardado en base de datos.", type="message") })
  
  output$anc_vars_ui <- renderUI({ 
    req(vals$datos); vars <- names(vals$datos)
    tagList(
      selectInput("anc_y", "Variable Respuesta (Y):", choices = c("Seleccione..."="", vars)), 
      selectInput("anc_trt", "Tratamiento (Factor):", choices = c("Seleccione..."="", vars)), 
      selectInput("anc_cov", "Covariable (X):", choices = c("Seleccione..."="", vars)), 
      conditionalPanel("input.anc_des == 'DBCA'", selectInput("anc_blk", "Bloque:", choices = c("Seleccione..."="", vars))), 
      conditionalPanel("input.anc_des == 'DCL'", selectInput("anc_row", "Fila:", choices = c("Seleccione..."="", vars)), selectInput("anc_col", "Columna:", choices = c("Seleccione..."="", vars)))
    ) 
  })
  
  observeEvent(input$run_anc, {
    req(vals$datos, input$anc_y, input$anc_trt, input$anc_cov)
    if(input$anc_y == "" || input$anc_trt == "" || input$anc_cov == "") return()
    df <- vals$datos; df[[input$anc_y]] <- as.numeric(as.character(df[[input$anc_y]])); df[[input$anc_cov]] <- as.numeric(as.character(df[[input$anc_cov]])); df[[input$anc_trt]] <- as.factor(df[[input$anc_trt]])
    if(input$anc_des == "DBCA") df[[input$anc_blk]] <- as.factor(df[[input$anc_blk]]); if(input$anc_des == "DCL") { df[[input$anc_row]] <- as.factor(df[[input$anc_row]]); df[[input$anc_col]] <- as.factor(df[[input$anc_col]]) }
    df <- df[complete.cases(df[, c(input$anc_y, input$anc_trt, input$anc_cov)]), ]; df <- droplevels(df)
    
    y <- input$anc_y; trt <- input$anc_trt; covx <- input$anc_cov; des <- input$anc_des
    base_str <- if (des == "DBCA") paste0(input$anc_blk, " + ") else if (des == "DCL") paste0(input$anc_row, " + ", input$anc_col, " + ") else ""
    
    tryCatch({
      mod_anc <- lm(as.formula(paste(y, "~", base_str, trt, "+", covx)), data = df)
      mod_hp <- lm(as.formula(paste(y, "~", base_str, trt, "*", covx)), data = df)
      mod_ind <- aov(as.formula(paste(covx, "~", base_str, trt)), data = df)
      
      output$anc_anova <- renderPrint({ car::Anova(mod_anc, type="III") })
      output$anc_hp <- renderPrint({ cat("--- Homogeneidad de Pendientes ---\n"); print(anova(mod_anc, mod_hp)) })
      output$anc_sup <- renderPrint({ cat("--- Normalidad ---\n"); tryCatch({print(shapiro.test(resid(mod_anc)))}, error=function(e) cat("Error:", e$message, "\n")); cat("\n--- Independencia Trat-Covariable ---\n"); print(summary(mod_ind)) })
      output$anc_emmeans <- renderPrint({ em <- emmeans::emmeans(mod_anc, as.formula(paste("~", trt))); print(em); cat("\n"); print(multcomp::cld(em, adjust = input$anc_pmm, Letters = letters)) })
      output$anc_plot <- renderPlot({ ggplot(df, aes_string(x = covx, y = y, color = trt)) + geom_point(size = 3) + geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) + theme_light(base_size = 14) + labs(title = paste("Relación", covx, "y", y), x = covx, y = y) })
    }, error = function(e) { showNotification(paste("Error en ANCOVA:", e$message), type="error") })
  })
  
  output$download_data <- downloadHandler(
    filename = function(){ paste0("Falcon_Procesado_", Sys.Date(), ".xlsx") },
    content = function(file){ wb <- createWorkbook(); addWorksheet(wb, "Data_Final"); addWorksheet(wb, "Reporte_Atípicos"); writeData(wb, "Data_Final", vals$datos); writeData(wb, "Reporte_Atípicos", vals$reporte); saveWorkbook(wb, file, overwrite = TRUE) }
  )
}

shinyApp(ui, server)

