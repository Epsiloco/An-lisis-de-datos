########### Software Estadístico ###########################
########## Scripts unificados ##########################

# Estos son los paquetes que voy a cargar
paquetes <- c(
  "openxlsx", "fBasics", "psych", "modeest", "ggrepel", "GGally", 
  "mice", "corrplot", "readxl", "doebioresearch", "performance", "dplyr", 
  "ScottKnott", "agricolae", "car", "broom", "data.table", "emmeans", 
  "ggplot2", "tidyverse", "lattice", "nlme", "lme4", "lmerTest", "multcomp", 
  "rstatix", "ggpubr", "see", "MASS", "lsmeans", "scales", "lmtest", "multcompView", 
  "googlesheets4", "googledrive", "clipr", "FactoMineR", "factoextra", 
  "glmmTMB", "DHARMa", "MuMIn", "hnp", "effects", "sjstats", "ExpDes", "sf", "tmap", "terra",
  "RVAideMemoire", "RColorBrewer", "DiagrammeR", "esquisse", "dlookr")

# Verifica cuáles no están instalados
no_instalados <- paquetes[!(paquetes %in% installed.packages()[,"Package"])];no_instalados

# Instala los que faltan
if(length(no_instalados)) {
  install.packages(no_instalados)
} else {
  message("Todos los paquetes ya están instalados.")
}

# Carga todos los paquetes (opcional)
lapply(paquetes, library, character.only = TRUE)


# Esta es la primera opcion para copiar y pegar datos de EXCEL:

excel_sheets("Velocidades de cosechadoras - Obraje 10114-213.xlsx")
datos<- read_excel("Velocidades de cosechadoras - Obraje 10114-213.xlsx", sheet = "R")

# Esta es la segunda opcion para pegar los datos de EXCEL (copiar y pegar)
datos<- read_clip_tbl()
# Cualquier base de datos se almacenara en el objeto datos


# ESTADISTICA DESCRIPTIVA
# Medidas de tendencia central y dispersion
# En esta parte quiero que presentes los resultados en tablas, pero que cambies las mediciones segun 
# tratamientos, replicas y una medida a nivel general 

# Limpieza de datos y analisis exploratorio de datos (EDA)
# Grafico para identificar datos faltantes
plot_na_pareto(datos)

#------------------------------------------
# Análisis de datos atípicos
#------------------------------------------

names(datos)
# Errores de digitación

# Variables:
variables_revisar <- c("Altura", "Diametro", "Peso", "Poblacion", "Tallo.Ha", 
                       "TCH.Est")   


for (var in variables_revisar) {
  if (var %in% names(datos)) {
    print(sort(datos[[var]], na.last = TRUE))
  } else {
    warning(paste("La variable", var, "no existe en 'datos'."))
  }
}

# Identificación con gráficos
colnames(datos)
vars_num <- c("TCH","Rendimiento.Kg.Ton", "TAH", "Pureza") 
var_group <- "Tratamiento"                        

# Histogramas
# Según frecuencia
for (v in vars_num) {
  hist(
    datos[[v]],
    main = paste("Distribución de", v),
    xlab = v,
    col = "lightblue",
    breaks = 20
  )
}

# Según densidad
for (v in vars_num) {
  
  dens <- density(datos[[v]], na.rm = TRUE)
  
  hist(
    datos[[v]],
    main = paste("Distribución de", v),
    xlab = v,
    col = "lightblue",
    breaks = 20,
    freq = FALSE   
  )
  
  lines(dens, lwd = 2, col = "red")
  
  abline(v = mean(datos[[v]], na.rm = TRUE), col = "darkgreen", lwd = 2, lty = 2)
}

# Boxplots
for (v in vars_num) {
  boxplot(
    datos[[v]] ~ datos[[var_group]],
    main = paste("Boxplot de", v, "por", var_group),
    xlab = var_group,
    ylab = v,
    col = "lightgreen"
  )
}


# Boxplots elegantes y ligeramente vivaces
for (v in vars_num) {
  
  
  colores <- c("white", "white", "white", "white", "white", "white")
  n_grupos <- length(unique(datos[[var_group]]))
  colores <- colores[1:n_grupos]
  
  
  boxplot(
    datos[[v]] ~ datos[[var_group]],
    main = paste("Boxplot de", v, "por", var_group),
    xlab = var_group,
    ylab = v,
    col = colores,
    border = "black",
    notch = FALSE,
    las = 1,
    cex.main = 1.8,
    cex.lab = 1.5,
    cex.axis = 1.3,
    outline = TRUE
  )
  
  medias <- tapply(datos[[v]], datos[[var_group]], mean, na.rm = TRUE)
  points(1:length(medias), medias, col = "red", pch = 19, cex = 1.3)
}


# Boxplots con réplicas
vars_num <- c("Rendimiento.Kg.Ton", "Pureza")
var_group <- "Tratamiento"
var_anidada <- "Replica" 

for (v in vars_num) {
  
  grafico_boxplot_final <- datos %>%
    ggplot(aes(x = as.factor(.data[[var_anidada]]), 
               y = .data[[v]], 
               fill = as.factor(.data[[var_anidada]]))) + 
    
    
    geom_boxplot(
      width = 0.9, 
      outlier.shape = 1, 
      outlier.size = 3,
      
      linewidth = 0.3 
    ) +
    
    
    facet_wrap(~ .data[[var_group]], 
               scales = "free_x", 
               ncol = 4) + 
    
    
    labs(
      title = paste("Boxplot de", v, "por Réplica dentro de cada Tratamiento"),
      x = "Réplica",
      y = v,
      fill = "Réplica"
    ) +
    
    
    theme_bw(base_size = 18) + 
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 22), 
      legend.position = "right",
      axis.title.x = element_blank(), 
      
      strip.text = element_text(size = 16, face = "bold"), 
      
      panel.spacing = unit(0.5, "lines") 
    ) +
    
    scale_fill_brewer(palette = "Dark2") 
  
  print(grafico_boxplot_final)
}

# Captura de valores de boxplot
outliers_list <- list()

for (v in vars_num) {
  bp <- boxplot(
    datos[[v]] ~ datos[[var_group]],
    plot = FALSE
  )
  outliers_list[[v]] <- bp$stats
}

outliers_list

# Datos atípicos por tratamiento
outliers_by_group <- list()

for (v in vars_num) {
  outliers_by_group[[v]] <- tapply(datos[[v]], datos[[var_group]], boxplot.stats)
}

outliers_by_group



# Eliminar datos atípicos
# Método 2: con el IQR (Reporte Detallado y Consola)
names(datos)

accion <- "NA" # Opciones: "NA" para reemplazar por NA, "eliminar" para eliminar filas completas
numericas <- c("Rendimiento.Kg.Ton")
columnas_agrupacion <- c("Tratamiento") 
reporte_eliminados <- list()

datos$ID_GRUPO <- apply(datos[, columnas_agrupacion, drop = FALSE], 1, paste, collapse = "_")

# Bucle para eliminar

for (v in numericas) {
  if (v %in% names(datos)) {
    hallazgos_variable <- data.frame()
    niveles_grupo <- unique(datos$ID_GRUPO)
    
    cat("\n--- Auditoría de Límites para:", v, "---\n")
    
    for (grupo in niveles_grupo) {
      mask_grupo <- datos$ID_GRUPO == grupo & !is.na(datos[[v]])
      valores <- datos[[v]][mask_grupo]
      
      if (length(valores) >= 6) { 
        # type=2 para coincidir con Excel
        qs <- quantile(valores, probs = c(0.25, 0.75), na.rm = TRUE, type = 6)
        Q1 <- qs[1]; Q3 <- qs[2]
        IQR_val <- Q3 - Q1
        lower <- Q1 - 1.5 * IQR_val
        upper <- Q3 + 1.5 * IQR_val
        
        # Impresión en consola para comparar sin descargar archivos
        cat(sprintf("Grupo: %s | Q1: %.3f | Q3: %.3f | LI: %.3f | LS: %.3f\n", 
                    grupo, Q1, Q3, lower, upper))
        
        # Detección con margen decimal
        outliers_mask_local <- (valores < (lower - 1e-9) | valores > (upper + 1e-9))
        
        if (any(outliers_mask_local)) {
          indices_reales <- which(mask_grupo)[outliers_mask_local]
          
          hallazgos_variable <- rbind(hallazgos_variable, data.frame(
            Fila_Original = indices_reales,
            Grupo_ID = grupo,
            Valor_Detectado = datos[[v]][indices_reales],
            LI = lower,
            LS = upper
          ))
          
          if (accion == "NA") datos[[v]][indices_reales] <- NA
        }
      } else {
        cat("Grupo:", grupo, "| Datos insuficientes (<6)\n")
      }
    }
    
    if (accion == "eliminar" && nrow(hallazgos_variable) > 0) {
      datos <- datos[-sort(hallazgos_variable$Fila_Original, decreasing = TRUE), ]
    }
    
    reporte_eliminados[[v]] <- hallazgos_variable
    cat("\nTotal atípicos en", v, ":", nrow(hallazgos_variable), "\n")
  }
}

datos$ID_GRUPO <- NULL
print(reporte_eliminados)


plot_na_pareto(datos)


#------------------------------------------
# Datos faltantes
#------------------------------------------

# Imputación con media
colnames(datos)
numericas <- c("TCH")
colSums(is.na(datos[numericas]))

for (v in numericas) {
  datos[[v]][is.na(datos[[v]])] <- mean(datos[[v]], na.rm = TRUE)
}

colSums(is.na(datos[numericas]))
attach(datos)
view(datos)


##############################
##################Otros métodos de imputación más potentes

if (!require("pacman")) install.packages("pacman")
pacman::p_load(mice, missForest, VIM, dplyr, tidyr, ggplot2)

# MICE con PMM (Predictive Mean Matching)
imp_mice_obj <- mice(datos, m=5, method='pmm', seed=123, printFlag=FALSE)
datos_mice <- complete(imp_mice_obj, 1)
view(datos_mice)

datos<- datos_mice
view(datos)

# MissForest (Random Forest) - El más potente en Machine Learning
set.seed(123)
imp_forest_obj <- missForest(datos)
datos_forest <- imp_forest_obj$ximp
view(datos_forest)

# KNN Imputation (k-Vecinos más cercanos)
datos_knn <- VIM::kNN(datos, k=5, imp_var = FALSE) 
view(datos_knn)

# CART (Classification and Regression Trees) - Alta Precisión
imp_cart_obj <- mice(datos, m=5, method='cart', seed=123, printFlag=FALSE)
datos_cart <- complete(imp_cart_obj, 1)
view(datos_cart)

# Diagnostico
imp_forest_obj$OOBerror[1] # Error NRMSE
VIM::marginplot(datos[, c("tch", "rendimiento")]) # < 0.3 es imputación confiable

# Comparación de métodos
# Identificación de negativos por método
negativos_resumen <- data.frame(
  PMM    = sum(datos_mice[sapply(datos_mice, is.numeric)] < 0),
  Forest = sum(datos_forest[sapply(datos_forest, is.numeric)] < 0),
  KNN    = sum(datos_knn[sapply(datos_knn, is.numeric)] < 0),
  CART   = sum(datos_cart[sapply(datos_cart, is.numeric)] < 0)
)
View(negativos_resumen)

# Comparativo de Medias 
target_var <- "tch" 
resumen_medias <- data.frame(
  Metodo = c("Original", "PMM", "Forest", "KNN", "CART"),
  Media  = c(mean(datos[[target_var]], na.rm = TRUE),
             mean(datos_mice[[target_var]]),
             mean(datos_forest[[target_var]]),
             mean(datos_knn[[target_var]]),
             mean(datos_cart[[target_var]]))
)
View(resumen_medias)

# Gráfico de Fidelidad de Distribución
df_comp <- data.frame(
  Original = datos[[target_var]],
  PMM = datos_mice[[target_var]],
  Forest = datos_forest[[target_var]],
  KNN = datos_knn[[target_var]],
  CART = datos_cart[[target_var]]
) %>% pivot_longer(cols = everything(), names_to = "Metodo", values_to = "Valor")

ggplot(df_comp, aes(x = Valor, color = Metodo)) +
  geom_density(linewidth = 1) +
  theme_minimal() +
  labs(title = paste("Distribución Imputada vs Original:", target_var),
       x = "Valor", y = "Densidad")

# Matriz de Correlación 
cor_original <- cor(datos$tch, datos$rendimiento, use="complete.obs")
cor_forest   <- cor(datos_forest$tch, datos_forest$rendimiento)
cor_cart     <- cor(datos_cart$tch, datos_cart$rendimiento)
print(c(cor_original, cor_forest, cor_cart))

# Medias por tratamiento
medias_por_tratamiento <- df_comp %>%
  mutate(Tratamiento = rep(datos$Tratamiento, 5)) %>%
  group_by(Metodo, Tratamiento) %>%
  summarise(Media = mean(Valor, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Metodo, values_from = Media)

View(medias_por_tratamiento)

# Gráfico de consistencia por replica (Jitter plot)

ggplot(df_comp %>% mutate(Tratamiento = rep(datos$Tratamiento, 5)), 
       aes(x = Tratamiento, y = Valor, color = Metodo)) +
  geom_jitter(position = position_jitter(0.2), size = 2, alpha = 0.6) +
  facet_wrap(~Metodo) +
  theme_bw() +
  labs(title = paste("Dispersión de Datos por Tratamiento:", target_var),
       subtitle = "Compare si la nube de puntos imputada imita la dispersión original",
       x = "Tratamiento", y = "Valor") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# -------------------------------------------------------------------------
# GUARDADO EN EXCEL: guardar los datos limpios en un nuevo excel
# -------------------------------------------------------------------------


nombre_archivo <- "datos_limpios_Falcon.xlsx"

wb <- createWorkbook()
addWorksheet(wb, "Datos_Limpios")
addWorksheet(wb, "Reporte_Outliers")

writeData(wb, "Datos_Limpios", datos)

df_reporte <- do.call(rbind, reporte_eliminados)
writeData(wb, "Reporte_Outliers", df_reporte)

saveWorkbook(wb, nombre_archivo, overwrite = TRUE)

cat("\nProceso finalizado. Archivo guardado como:", nombre_archivo, "\n")
cat("Dimensiones finales:", dim(datos)[1], "filas y", dim(datos)[2], "columnas\n")




# Cálculos
ED1<- round(basicStats(Precipitación, ci = 0.95), 2)
print(ED1)

media<- mean(Precipitación)
print(media)
mediana<- median(Precipitación)
moda<- mlv(Precipitación, method = "mfv")
print(moda)
moda1<- NA
n<- length(Precipitación)
DE<- sd(Precipitación)
varianza<- var(Precipitación)
CV<- (DE/media)*100
EE<- DE/sqrt(n)
quantile(Precipitación)
summary(Precipitación)
Q1<- quantile(Precipitación, 0.25)
Q2<- quantile(Precipitación, 0.5)
Q3<- quantile(Precipitación, 0.75)
quantiles<- quantile(Precipitación, c(0.1, 0.25, 0.5, 0.75))
print(quantiles)

as<- skewness(Precipitación)
kurtosis<- kurtosis(Precipitación)
sw<- shapiro.test(Precipitación)
print(sw)

MG<- geometric.mean(Precipitación)
geometric<- function(Precipitación) exp(sum(log(Precipitación))/n)
geometric(Precipitación)

p<- Temperatura/sum(Temperatura)
MP<- weighted.mean(Precipitación, p)

MC<- sqrt(mean((Precipitación)^2))
MA<- 1/mean(1/Precipitación)


sapply(list(as, CV, DE, EE, kurtosis, MA, MC, media, mediana, MG, moda1, MP, n, Q1, Q2, Q3, varianza), length)
ED2<- data.frame(Estadistica = c("as", "CV", "DE", "EE", "kurtosis", "MA", "MC", "media", "mediana", "MG", "moda1", "MP", "n", "Q1", "Q2", "Q3", "varianza"),
                 Valor= c(as, CV, DE, EE, kurtosis, MA, MC, media, mediana, MG, moda1, MP, n, Q1, Q2, Q3, varianza))

ED1_df<- as.data.frame(ED1)
d<- loadWorkbook("Broca_ADC (1).xlsx")
addWorksheet(d, "Lluvia")
writeData(d, sheet = "Lluvia", x = ED1_df, colNames = T)
saveWorkbook(d, "Broca_ADC (1).xlsx", overwrite = T)
excel_sheets("Broca_ADC (1).xlsx")
datos<- read_excel("Broca_ADC.xlsx", sheet=5)

addWorksheet(d, "ED")
writeData(d, sheet = "ED", x = ED2, rowNames = T)
saveWorkbook(d, "Broca_ADC (1).xlsx", overwrite = T)
excel_sheets("Broca_ADC (1).xlsx")
datos<- read_excel("Broca_ADC (1).xlsx", sheet = 6)
print(datos)
# Quiero que des la opcion de guardar los resultados en un nuevo excel, que se pueda descargar desde la aplicacion

#Gráficos:-------------------------------------------------------------

# En el apartado de graficos quiero que sean dinamicos y modificables (APLICABLE PARA TODOS), me refiero a cambios de color, cambios en el grosor de las lineas, modificacion de titulos y etiquetas 

#Box plot para una variable------------------------------------------------------------------------------
li<- Q1-1.5*IQR
ls<- Q3+1.5*IQR


boxplot(Precipitación, width = 0.3, main = "Box plot de Precipitación", 
        ylab = "Precipitación (mm)",
        col = "lightgray",
        border = "darkgray",
        ylim= range(c(Precipitación, ls, li)))
points(mean(Precipitación), col = "red", pch = 19)
abline(h = li, col = "red", lwd = 2)
abline(h = ls, col = "red", lwd = 2)

#Identificar valores extremos

outliers<- boxplot(Precipitación, plot = FALSE)$out #para saber qué dato es el atípico
which(Precipitación %in% outliers) #Indica la fila en la que se encuentra el valor atípico según la columna analizada
datos$outlier<- ifelse(Precipitación < li| Precipitación>ls, "si","no") #crea una columna para indicar qué datos son atípicos y cuáles no

#Prueba de grubbs
resultado_grubbs <- grubbs.test(Precipitación)
print(resultado_grubbs)
z_scores <- scale(Precipitación)  # Estandariza los datos
outlier <- Precipitación[which.max(abs(z_scores))] 
print(paste("El outlier más extremo es:", outlier))



ggplot(datos, aes(x = "", y = Precipitación)) +
  geom_boxplot(
    outlier.shape = T,  # Ocultar outliers
    outlier.color = "darkgreen",
    outlier.size = 3,
    outlier.alpha = 1,
    outlier.fill = "green",
    fill = "lightpink",
    color = "red",
    size = 0.5,
    alpha = 0.8  # Transparencia
  ) +
  # Resaltar la media
  stat_summary(
    fun = mean,
    geom = "point",
    color = "red",
    size = 3,
    shape = 23,
    fill = "black"
  ) +
  # Agregar límites superior e inferior
  geom_hline(yintercept = li, color = "red", linetype = "dashed", size = 1) +
  geom_hline(yintercept = ls, color = "red", linetype = "dashed", size = 1) +
  # Etiquetas y título
  labs(
    title = "Box plot para Precipitación",
    y = "Precipitación (mm)"
  ) +
  # Personalizar el tema
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 12),
    panel.grid.major.y = element_line(color = "gray", linetype = "dotted", size = 0.2)
  )
ggsave("box plot.png", bg = "white", width = 12, height = 7, units = "in", dpi = 300)  #solo funciona con ggplot



#box plot para varias variables--------------------------------------------------------------------------------------------
str(datos)

boxplot(list(Precipitación, `No.broca/trampa`), 
        main = "box plot de variables",
        ylab = "Medidas",
        col = c("lightpink", "lightblue"),
        names = c("Precipitación", "Densidad pobl"))
ggsave("box plot3.png", bg = "white", width = 12, height = 7, units = "in", dpi = 300)


#ggplot
str(datos)

ggplot() +
  geom_boxplot(data = datos, aes(x = "Temperatura", y = Temperatura), fill = "skyblue") +
  geom_boxplot(data = datos, aes(x = "Densidad", y = `No.broca/trampa`), fill = "orange") +
  geom_boxplot(data = datos, aes(x = "Precipitación", y = Precipitación), fill = "violet") +
  
  
  stat_summary(data = datos, aes(x = "Temperatura", y = Temperatura),
               fun = mean, geom = "point", shape = 23, size = 3, fill = "yellow") +
  stat_summary(data = datos, aes(x = "Densidad", y = `No.broca/trampa`),
               fun = mean, geom = "point", shape = 23, size = 3, fill = "yellow") +
  stat_summary(data = datos, aes(x = "Precipitación", y = Precipitación),
               fun = mean, geom = "point", shape = 23, size = 3, fill = "yellow") +
  
  
  geom_segment(aes(x = 0.7, xend = 1.3, y = 22, yend = 22), color = "red", linetype = "dashed") +
  geom_segment(aes(x = 0.7, xend = 1.3, y = 30, yend = 30), color = "red", linetype = "dashed") +
  
  geom_segment(aes(x = 1.7, xend = 2.3, y = 100, yend = 100), color = "blue", linetype = "dashed") +
  geom_segment(aes(x = 1.7, xend = 2.3, y = 1500, yend = 1500), color = "blue", linetype = "dashed") +
  
  geom_segment(aes(x = 2.7, xend = 3.3, y = 50, yend = 50), color = "darkgreen", linetype = "dashed") +
  geom_segment(aes(x = 2.7, xend = 3.3, y = 200, yend = 200), color = "darkgreen", linetype = "dashed") +
  
  labs(title = "Boxplots de Temperatura, densidad y Precipitación",
       x = "Variable", y = "Valor") +
  theme_minimal()
ggsave("box plot4.png", bg = "white", width = 12, height = 7, units = "in", dpi = 300)  #solo funciona con ggplot





#Gráfico de barras------------------------------

#Según la frecuencia absoluta:

frecuencia <- c(sum(!is.na(datos$Precipitación)),     # Contar cuántos valores válidos tiene cada variable
                sum(!is.na(datos$`No.broca/trampa`)),
                sum(!is.na(datos$Temperatura)))

variables <- c("Precipitación", "No.broca/trampa", "Temperatura")

tabla <- data.frame(variables, ni = frecuencia)

n <- sum(frecuencia)          #Frecuencia relativa
tabla$fi <- tabla$ni / n
print(tabla)

barplot(tabla$ni,
        names.arg = tabla$variables,
        col = c("skyblue", "orange", "violet"),
        ylab = "Frecuencia absoluta (ni)",
        main = "Frecuencia de datos por variable")


#Según la media:

medias <- c(mean(datos$Precipitación, na.rm = TRUE),
            mean(datos$`No.broca/trampa`, na.rm = TRUE),
            mean(datos$Temperatura, na.rm = TRUE))

tabla_medias <- data.frame(variables, media = medias)  #crear tabla
print(tabla_medias)

barplot(tabla_medias$media,
        names.arg = tabla_medias$variables,
        col = c("skyblue", "orange", "violet"),
        ylab = "Media",
        main = "Media de cada variable")


#Para representar todos los datos según una variable categórica
var<- c("Día", "Densidad")
barplot(`No.broca/trampa`, col = "blue", border = "black",
        names.arg = Día,
        xlab = "Tiempo (Días)",
        ylab = "Densidad poblacional")




#Gráfico de pastel------------------------------------

datos<- read_excel("Broca_ADC (1).xlsx", sheet = 1)   #para datos diarios
str(datos)

par(mar = c(2, 2, 4, 2))         
par(cex.main = 1)              
par(cex = 0.7)

porcentajes <- round(datos$`No.broca/trampa` / sum(datos$`No.broca/trampa`) * 100, 1)
etiquetas_porcentaje <- paste(datos$Día, " (", porcentajes, "%)", sep = "")

pie(datos$`No.broca/trampa`,
    labels = etiquetas_porcentaje,
    col    = rainbow(length(datos$`No.broca/trampa`)),
    main   = "Distribución de No.broca/trampa por Día",
    cex = 0.75,
    radius = 1.2)


datos<- read_excel("Broca_ADC (1).xlsx", sheet = 2) #para datos semanales
str(datos)

par(mar = c(2, 2, 4, 2))         
par(cex.main = 1)              
par(cex = 0.8)

porcentajes <- round(datos$`No.broca/trampa`/ sum(datos$`No.broca/trampa`) * 100, 1)
etiquetas_porcentaje <- paste(datos$Semana, " (", porcentajes, "%)", sep = "")

pie(datos$`No.broca/trampa`,
    labels = etiquetas_porcentaje,
    col    = rainbow(length(datos$Semana)),
    main   = "Gráfico de pastel (Densidad semanal de broca del café)",
    cex = 1,
    radius = 1.29)



#ggplot

datos <- datos %>%
  mutate(Semana     = as.factor(Semana),
         Porcentaje = round(`No.broca/trampa` / sum(`No.broca/trampa`) * 100, 1)) %>%
  arrange(desc(Semana)) %>%
  mutate(ypos = cumsum(Porcentaje) - 0.5 * Porcentaje)

# Gráfico con etiquetas afuera
ggplot(datos, aes(x = "", y = Porcentaje, fill = Semana)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(y = ypos, label = paste0(Porcentaje, "%")),
            size = 4, nudge_x = 0.7) +  # nudge_x empuja la etiqueta hacia afuera
  labs(title = "Distribución de No.broca/trampa por semana",
       fill = "Semana") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))




datos <- datos %>%
  mutate(Semana     = as.factor(Semana),
         Porcentaje = round(`No.broca/trampa` / sum(`No.broca/trampa`) * 100, 1)) %>%
  arrange(desc(Semana)) %>%
  mutate(ypos = cumsum(Porcentaje) - 0.5 * Porcentaje)

# Crear gráfico tipo dona
ggplot(datos, aes(x = 2, y = Porcentaje, fill = Semana)) +  # x = numérico (no factor)
  geom_bar(stat = "identity", width = 1, color = "white") +  # Grosor del anillo
  coord_polar(theta = "y") +                                 # Coordenadas polares
  xlim(0.5, 2.5) +                                            # Espacio para hueco
  geom_text(aes(y = ypos, label = paste0(Porcentaje, "%")),
            size = 3.3, x = 2.2) +                              # Etiquetas fuera del anillo
  labs(title = "Gráfico de pastel de densidad semanal de broca del café",
       fill = "Semana") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("pastel_broca.png", bg = "white", width = 12, height = 7, units = "in", dpi = 300) 


###################### Pruebas de hipotesis ########################
# Para una media
# --- Media (Z-test, varianza conocida o muestra grande) ---
z_stat <- (mean(datos) - mu_0) / (sd_poblacional / sqrt(length(datos)))
p_valor_z <- 2 * pnorm(-abs(z_stat))

# --- Media (T-test, varianza desconocida) ---
t.test(datos, mu = mu_0, alternative = "two.sided")

# Para una proporcion

# --- Proporción (Exacta y Aproximación) ---
prop.test(x = exitos, n = total_n, p = p_0, alternative = "two.sided", correct = TRUE)

# --- Proporción (Sin corrección de continuidad) ---
z_prop <- (prop_obs - p_0) / sqrt((p_0 * (1 - p_0)) / total_n)
p_valor_prop <- 2 * pnorm(-abs(z_prop))

# Para un total

# --- Total Poblacional ---
N <- 1000 
media_obs <- mean(datos)
error_est_total <- N * (sd(datos) / sqrt(length(datos)))
t_total <- (N * media_obs - Total_0) / error_est_total
p_valor_total <- 2 * pt(-abs(t_total), df = length(datos) - 1)

## Incluye la opcion de elegir entre prueba de una cola (unilateral izquierda o derecha) y bilateral o de dos colas 
# Dame la opcion de ajustar los niveles de significancia y confianza

#### Para dos medias (dependientes o pareadas e independientes)

# --- Medias Pareadas ---
t.test(grupo_A, grupo_B, paired = TRUE, alternative = "two.sided")

# --- Método manual (Diferencias) ---
diferencias <- grupo_A - grupo_B
t.test(diferencias, mu = 0)


# --- Medias Independientes (Varianzas iguales - Homocedasticidad) ---
t.test(grupo_1, grupo_2, paired = FALSE, var.equal = TRUE)

# --- Medias Independientes (Varianzas distintas - Welch) ---
t.test(grupo_1, grupo_2, paired = FALSE, var.equal = FALSE)

# --- Una Varianza (Prueba Chi-cuadrado) ---
# Requiere paquete 'EnvStats'
# install.packages("EnvStats")
EnvStats::varTest(datos, sigma.sq = varianza_0, alternative = "two.sided")

# --- Dos Varianzas (Prueba F de Fisher) ---
var.test(grupo_1, grupo_2, alternative = "two.sided")

## En la aplicacion incluye la opcion de elegir entre el tipo de prueba

########################### Diseños experimentales #######################################

# Conversion a factores
datos$Tratamiento<- as.factor(datos$Tratamiento)
datos$Replica<- as.factor(datos$Tratmiento)
datos$Respuesta<- as.numeric(datos$Respuesta)
## En Respusta van cualquier grupo de variables de respuesta

#### Experimentos simples
# Diseño completamente al azar (DCA)

#ANDEVA
resultado<-aov(Y2~TR)
anova(resultado)
cv.model(resultado)

mod <- lm(Y2~TR)
anova(mod , test="F")

#Verificación de supuestos
shapiro.test(mod$res)
qqPlot(resultado)

fit <- fitted(resultado) #predichos
res <- residuals(resultado)  #residuos
res_standard <- rstandard(resultado) #residuos estandarizados
res_student <- rstudent(resultado) #residuos estudentizados
plot(fit,res_standard,xlab="Valores predichos", ylab="Residuos estandarizados",abline(h=0))

bartlett.test(Y2~TR) #Prueba de Barlett
leveneTest(Y2~TR, center = "median") #Prueba de Levene

# Prueba multiple de medias (dame la opcion en la app de elegir cualquiera de estas pruebas, y la opcion de elegir el nivel de significancia)
outLSD <-LSD.test(resultado, "TR",console=TRUE) #prueba LSD
outHSD<-HSD.test(resultado, "TR",console=TRUE) #prueba Tukey
SNK.test(resultado, "TR",console=TRUE) #prueba de Newman Keuls (SNK)
scheffe.test(resultado, "TR",console=TRUE) #prueba de Scheffé
duncan.test(resultado, "TR",console=TRUE) #Duncan
LSD.test(resultado, "TR", p.adj= "bon",console=TRUE) #Bonferroni
sk <- SK(resultado, which= "TR",  dispersion="se", sig.level=0.05) #ScottKnott
summary(sk)

# Diseño de bloques completos al azar (DBCA)
resaov<-aov(ALT1 ~ BLOQ + TRB)
anova(resaov)
cv.model(resaov) 

euc.lm <- lm(ALT1 ~ BLOQ + TRB)
anova(euc.lm , test="F")

#Verificación de supuestos
shapiro.test(euc.lm$res)
qqPlot(resaov)
fitb <- fitted(resaov)  
res_stb <- rstandard(resaov)  
plot(fitb,res_stb,xlab="Valores predichos", ylab="Residuos estandarizados",abline(h=0))
bartlett.test(ALT1 ~ TRB) #Barlett
leveneTest(ALT1~TRB, center = "median") #levene

#PMM
outLSD <-LSD.test(resaov, "TRB",console=TRUE) #LSD
outHSD<-HSD.test(resaov, "TRB",console=TRUE) #Tukey
SNK.test(resaov, "TRB",console=TRUE) #SNK
scheffe.test(resaov, "TRB",console=TRUE) #Scheffe
duncan.test(resaov, "TRB",console=TRUE) #Duncan
LSD.test(resaov, "TRB", p.adj= "bon",console=TRUE) #Bonferroni
sk <- SK(resaov, which= "TRB",  dispersion="se", sig.level=0.05) #SkottKnott
summary(sk)

# Otra forma
andeva<- lm(y~x+bloq)
anova(andeva)
cv.model(andeva)

qqPlot(andeva$residuals)
sw<- shapiro.test(andeva$residuals)
print(sw)

plot(andeva$fitted.values, andeva$residuals, col="red", pch=1, pty=3, abline(h=0, col="red", lwd=3))
leveneTest(y~x, center="mean")
bartlett.test(y~x)

sk<- SK(andeva, which = "x", sig.level = 0.05, dispersion="se")
summary(sk)



par(mfrow=c(2,2))
plot(andeva)
check_normality(andeva)
win.graph(11,11)
check_model(andeva)

# Diseño cuadrado latino (DCL)

DCL.aov<-aov(Resp1 ~ FILA + COL + TRC)
anova(DCL.aov)
cv.model(DCL.aov)

DCL.lm <- lm(Resp1 ~ FILA + COL + TRC)
anova(DCL.lm)

#Supuestos
shapiro.test(DCL.lm$res)

qqPlot(DCL.aov)
fitc <- fitted(DCL.aov)
res_stc <- rstandard(DCL.aov)  
plot(fitc,res_stc,xlab="Valores predichos", ylab="Residuos estandarizados",abline(h=0))

bartlett.test(Resp1 ~ TRC)
leveneTest(Resp1 ~ TRC, center = "median")

#PMM
outLSD <-LSD.test(DCL.aov, "TRC",console=TRUE)
outHSD<-HSD.test(DCL.aov, "TRC",console=TRUE) #Tukey
SNK.test(DCL.aov, "TRC",console=TRUE)
scheffe.test(DCL.aov, "TRC",console=TRUE)
duncan.test(DCL.aov, "TRC",console=TRUE)
LSD.test(DCL.aov, "TRC", p.adj= "bon",console=TRUE)
sk <- SK(DCL.aov, which= "TRC",  dispersion="se", sig.level=0.05)
summary(sk)


andeva<- lm(y1~x+f+c)
anova(andeva)

qqPlot(andeva$residuals)
sw<- shapiro.test(andeva$residuals)
print(sw)

plot(andeva$fitted.values, andeva$residuals, col="red", 
     pch=18, xlab="pred", ylab="res", cex=1.5, abline(h=0, col="red", lwd=2))
leveneTest(y1~x, center="mean")
bartlett.test(y1~x)

par(mfrow=c(2,2))
plot(andeva)
check_normality(andeva)
win.graph(11,11)
check_model(andeva)

sk<- SK(andeva, which="x", sig.level = 0.05, dispersion="se")
summary(sk)

# Transformacion de datos: necesario para transformar los datos que no cumplen los supuestos y poder aplicar el ANDEVA

TRt<- factor(trat)
pul<-as.vector(pulgones)
pul1<-as.numeric(pul)
plot(pul1~TRt, xlab="Productos", ylab="Cantidad de Pulgones") #Box plot

m0<-aov(pul1~TRt)    
anova(m0)
cv.model(m0)

par(mfrow = c(2,2)) #Análisis gráfico de los residuos
plot(m0)


#Verificación de supuestos
shapiro.test(m0$res)
par(mfrow = c(1,1))
qqPlot(m0)

fit <- fitted(m0)
res <- residuals(m0)
res_standard <- rstandard(m0)
res_student <- rstudent(m0)
plot(fit,res_standard,xlab="Valores preditos", ylab="Residuos estandarizados",abline(h=0))

bartlett.test(pul1~TRt)
bartlett.test(m0$res, TRt)
leveneTest(pul1~TRt)


#TRANSFORMACIÓN-----------------------------------------------------------------
#Transformación logarítmica: Es utilizada cuando consta de cierta proporcionalidad entre las medias y las desviaciones estándar de los tratamientos.
m1 <- lm (log(pul1) ~ TRt)
anova(m1)
cv.model(m1)

#Transformación box cox
boxcox(m0)
#Ampliando la gráfica
boxcox(m0, lam = seq(-1, 1, 1/10))
boxcox(m0)
abline(boxcox(m0),v=0.2)
#Para encontrar el valor de lambda
bc<-boxcox(m0)
(lambda <- bc$x[which.max(bc$y)])

summary(powerTransform(pul1)) #Para obtener lambda y la prueba de hipotesis para ver si es necesario o no transformar

# Incluye otros metodos de transformacion (logaritmico, arcoseno para datos en porcentaje, raiz cuadrada, logaritmo base 10, logaritmo natural, incluye todos los metodos de transformacion que hay en INFOSTAT)
# Enfocate en los metodos de transformacion para datos de conteo, porcentaje y otros metodos como box-cox

##################### Experimentos factoriales ###################################
##### Arreglo factorial combinatorio

# Diseño completamente al azar (DCA)

par(mar=c(5,4,5,7))
interaction.plot(FA,FB,y, col=c("red", "black", "brown"),
                 lwd=1.4, lty=1, xlab = "Raza", ylab = "peso", 
                 trace.label = "Ambiente")

interaction.plot(FA, FC, y, col=c("blue", "green", "black"), 
                 lwd=2, lty=2,
                 xlab = "Raza", ylab="peso", trace.label = "Alimentación")

#ANDEVA
# Yijk = u + Ti + Bj + pk + (TB)ij + (Tp)ik + (Bp)jk + (TBp)ijk + Eijk

andeva<- lm(y~FA+FB+FC+FA:FB+FA:FC+FB:FC+FA:FB:FC) #las 3 formas dan el mismo resultado
andeva<- aov(y~FA*FB*FC)
andeva<- lm(y~FA*FB*FC)
anova(andeva)

qqPlot(andeva$residuals)
sw<- shapiro.test(andeva$residuals)
print(sw)

plot(andeva$fitted.values, andeva$residuals, col="red", cex=1.1,
     pch=16, xlab="Predichos", ylab="Residuos",
     abline(h=0, col="red", lwd=2))

leveneTest(y~interaction(FA,FB,FC), center="mean")
bartlett.test(y~interaction(FA,FB,FC))
bartlett.test(y~FA:FB:FC)
leveneTest(y~FA:FB:FC, center="mean")
dwtest(andeva) #Prueba de independencia de los residuos

#Prueba de scott-Knott: la fx SK no funciona para interacciones. Se tiene que crear una nueva columna que contenga la interacción

datos$FA_FB<- interaction(datos$Raza, datos$ambiente, sep = "_") #se crea la nueva columna con la interacción
andeva2<- lm(y~FA_FB) #nuevo ANDEVA (solo para la interacción)
anova(andeva2)

sk<- SK(andeva2, which = "FA_FB", dispersion="se", sig.level = 0.05)
summary(sk)

#Otras pruebas:
tukey<- HSD.test(andeva2, "FA_FB", console = T)
SNK<- SNK.test(andeva2, "FA_FB", console=T)
Duncan<- duncan.test(andeva2, "FA_FB", console = T)

# Diseño de bloques completos al azar (DBCA)

par(mar=c(3,3,2,3))
interaction.plot(FA, FB, y1, col= c("red", "blue", "black"), 
                 xlab = "N", ylab="rend", fixed = T, lwd=1.6, lty = 10)

andeva<- aov(y1~bloq+FA+FB+FA:FB)
anova(andeva)
cv.model(andeva)

qqPlot(andeva$residuals, col="black", pch = 1, cex = 1)
sw<- shapiro.test(andeva$residuals)
print(sw)

par(mar=c(6,6,2,4))
plot(andeva$fitted.values, andeva$residuals, col="red", pch=16, cex=1,
     xlab="pred", ylab="res", abline(h=0, col="blue", lwd=3, lty=3))

leveneTest(y1, FA:FB, center = "mean")
leveneTest(y1~interaction(FA,FB), center="mean")
bartlett.test(y1~interaction(FA,FB))

par(mfrow=c(2,2))
plot(andeva)
check_normality(andeva)
win.graph(11,11)
check_model(andeva)


#PMM para FA
sk1<- SK(andeva, which = "FA", dispersion="se", sig.level = 0.05)
summary(sk1) #hay diferencias significativas
#PMM para FB
sk2<- SK(andeva, which = "FB", dispersion = "se", sig.level = 0.05)
summary(sk2) #todas las medias son iguales, por eso la misma letra

#PMM para interacción: colocar en la función tal como aparece en el objeto andeva (which="FA:FB") en este caso 


# Diseño cuadrado latino (DCL): es lo mismo, solo hay que incluir las filas y columnas en vez del bloque


###### Arreglo en parcelas divididas
# Diseño completamente al azar (DCA)

par(mar=c(5,4,3,9))
interaction.plot(FA,FB,y, fixed = F, col=c("red", "blue", "black", "orange", "lightblue", "brown"), 
                 lwd=1.4, lty=1, trace.label = "E_surcos",
                 xlab="D_sem", ylab="Rend", cex=1)

#ANDEVA
# Yijk = u + Ti + (p)j(i) + ak + (Ta)ik + Eijk
andeva<- aov(y~FA+FB+Error(rep/FA)+FA:FB) #Error() no funciona en lm() pq solo acepta modelos lineales con 1 error
summary(andeva) #ya no usa anova q solo acepta 1 error
model.tables(andeva, type = "means") #tabla de medias para cada tratamiento e interacción

#Verificación de supuestos
#crear un modelo con un único error, ya que las pruebas de supuestos solo aceptan uno.
modelo<- aov(y~FA+FB+FA*FB+rep/FA) #se quita Error() y se deja solo como rep/FA para dejar un solo error (el residual)
modelo<- aov(y~FA*FB+rep/FA) #otra forma
summary(modelo)

sw<- shapiro.test(modelo$residuals)
print(sw)
qqPlot(modelo$residuals)
plot(modelo, 2) #otra forma de qqplot

plot(modelo$fitted.values, modelo$residuals, col="red", cex=1, pch=16, 
     abline(h=0, col="red", lwd=2))
plot(modelo, 1) #otra forma de hacerlo
plot(modelo, 3) #grafico de predichos vs residuos estandarizados

#Para verificar los supuestos de una vez:
par(mfrow=c(2,2), mar=c(2,3,4,5))
plot(modelo)
check_normality(modelo)
win.graph(11,11)
check_model(modelo)

leveneTest(modelo$residuals, FA, center = "mean") #para FA
leveneTest(modelo$residuals~FA, center = "mean")
bartlett.test(modelo$residuals, FB) #para FB
bartlett.test(modelo$residuals, FA:FB)

check_heteroscedasticity(modelo) #otra forma de verificar

#PMM:se hace con el ANDEVA original
sk1<- SK(andeva, which = "FA", dispersion="se", sig.level = 0.05) #para FA
summary(sk1)
sk2<- SK(andeva, which = "FB", dispersion="se", sig.level = 0.05) #para FB
summary(sk2)

#Prueba de Scott Knott para la interacción:
colnames(datos)
datos$FA_FB<- interaction(datos$D_sem, datos$E_Surcos, sep = "_") #unir ambos factores como FA*FB en la tabla
attach(datos)
sk3<- SK(andeva, which = "FA_FB", dispersion="se", sig.level = 0.05)
summary(sk3)

Tukey1<- HSD.test(y, FA, DFerror =12, MSerror = 3.21)  #colocar los valors de forma manual
print(Tukey1) 
Tukey2<- HSD.test(y, FB, DFerror = 60, MSerror = 4.55) 
print(Tukey2)
Tukey3<- HSD.test(y, FA:FB, DFerror = 60, MSerror = 4.55) #Ambas formas son validas 
Tukey3<- HSD.test(y, FA_FB, DFerror = 60, MSerror = 4.55) 
print(Tukey3)


# Diseño de bloques completos al azar (DBCA)

par(mar=c(3,4,2,11))
interaction.plot(FA,FB,y, col=c("red","black","blue","green"), lwd=1.4, cex=0.7, 
                 trace.label = "Híbrido", xlab = "Densidad", ylab = "Frutos frescos")

#ANDEVA: la diferencia con el DCA es que se incorpora el efecto bloque
#Yijk= u+ Ti + Bj + pk + (TB)ij + (Tp)ik + Eijk

andeva<- aov(y~FA+FB+bloq+Error(bloq/FA)+FA*FB)
summary(andeva)
andeva2<- aov(y~FA+FB+bloq+Error(bloq*FA)+FA:FB) #es igual al primer modelo
summary(andeva)

andeva3<- splitplot(data=datos2[5],
                    main.plot = FA,
                    sub.plot = FB,
                    block = bloq,
                    mean.comparison.test = 1)
print(andeva3)

#Verificar los supuestos: hacer un ANDEVA para un solo error
colnames(datos)
modelo<- aov(y~FA+FB+FA:FB+rep/FA) #también se puede colocar como rep*FA o rep:FA
summary(modelo)
modelo<- aov(y~FA*FB+rep+rep:FA)
summary(modelo)

#Normalidad
sw<- shapiro.test(modelo$residuals)
print(sw)
qqPlot(modelo$residuals, col = "blue", cex = 1.3, pch = 16)
plot(modelo, 2, col="black", cex=1.3, pch=16) #igual que QQplot

#Homocedasticidad
leveneTest(modelo$residuals, FA, center = "mean")
leveneTest(modelo$residuals, FB, center = "mean")
leveneTest(modelo$residuals, interaction(FA,FB), center = "mean")
bartlett.test(modelo$residuals, FA)
bartlett.test(modelo$residuals, FB)
bartlett.test(modelo$residuals, interaction(FA, FB))

plot(modelo$fitted.values, modelo$residuals, col="blue", pch=16, cex=1.3, 
     xlab="predichos", ylab="residuos", abline(h=0, col="red", lwd=2))

plot(modelo, 1, col="red", pch=16)

check_heteroscedasticity(modelo) #otra forma de verificar


#Independencia
dwtest(modelo) 

#PMM:
sk1<- SK(andeva, which = "FA", sig.level = 0.05, dispersion = "se")
summary(sk1)
sk2<- SK(andeva, which = "FB", sig.level = 0.05, dispersion = "se")
summary(sk2)

datos2$FA_FB<- interaction(FA, FB, sep = "_") 
attach(datos2)
sk3<- SK(andeva, which = "FA_FB", sig.level = 0.05, dispersion="se")
summary(sk3)

tukey1<- HSD.test(y, FA, DFerror = 3, MSerror = 93)
print(tukey1)
tukey2<- HSD.test(y, FB, DFerror = 18, MSerror = 146.28)
print(tukey2)
tukey3<- HSD.test(y, FA:FB, DFerror = 18, MSerror = 146.28)
print(tukey3)

#Para eliminar todo lo trabajado:
cat("\014") #en vez del ctrl+L
graphics.off() #limpia el panel grafico
rm(list=ls()) #limpia la consola


# Diseño en cuadrado latino (DCL)

par(mar=c(5,4,3,9))
interaction.plot(FA, FB, y, cex=0.2, col=c("red","blue","black"), lty = 1, lwd=2, 
                 trace.label="Dens", xlab="Var", ylab = "Rend")

with(datos, xyplot(y ~ FA | FB))
with(datos, xyplot(y ~ FA | FB, groups = fila))
with(datos, xyplot(y ~ FA | FB, groups = fila, aspect = "xy"))
with(datos, xyplot(y ~ FA | FB, groups = fila, aspect = "xy", type ="o"))
with(datos, xyplot(y ~ FA | FB, groups = fila, aspect = "xy", type ="a"))

#ANDEVA:#ANDEVA:fila
# Yijkl = u + Ti + Fj + Ck + pl + (FCT)ijk + (Tp)il + Eijkl

parcela_principal<- with(datos, interaction(fila,col))
andeva<- aov(y~FA+FB+FA:FB+fila+col+Error(parcela_principal))
summary(andeva)

andeva1 <- aov(y ~ FA + FB + FA:FB + fila + col + Error(interaction(fila,col))) #Ambas soluciones dan lo mismo
summary(andeva1)

#Verificación de supuestos: andeva con 1 error
#Normalidad
andeva2<- aov(y~FA*FB+fila+col+interaction(fila, col))
summary(andeva2)
sw<- shapiro.test(andeva2$residuals)
print(sw)
check_normality(andeva2)
qqPlot(andeva2$residuals, col="red", pch=16)
plot(andeva2, 2)

#Homocedasticidad
leveneTest(andeva2$residuals~FA, center="mean")
leveneTest(andeva2$residuals~FB, center="mean")
leveneTest(andeva2$residuals~interaction(FA,FB), center="mean")

bartlett.test(andeva2$residuals~FA)
bartlett.test(andeva2$residuals~FB)
bartlett.test(andeva2$residuals~interaction(FA,FB))

#Independencia
dwtest(andeva2)

#PMM
sk<- SK(andeva, which = "FA", sig.level = 0.05, dispersion="se")
summary(sk)
sk2<- SK(andeva, which = "FB", sig.level = 0.05, dispersion="se")
summary(sk2)

datos$FA_FB<- interaction(FA,FB,sep = "_")
attach(datos)
sk3<- SK(andeva, which = "FA_FB", sig.level = 0.05, dispersion="se")
summary(sk3)
tukey<- HSD.test(y, FA, DFerror = 2, MSerror = 0.0144)
print(tukey)

tukey2<- HSD.test(y, FB, DFerror = 2, MSerror = 0.0144)
print(tukey2)

tukey3<- HSD.test(y, FA_FB, DFerror = 2, MSerror = 0.0144)
print(tukey3)

#Limpiar consola, gráficos y ambiente de trabajo
cat("\014")
rm(list = ls())
graphics.off()

##################### Arreglo en Parcelas subdivididas
# Diseño completamente al azar (DCA)

interaction.plot(FA,FB,y, col= c("red","blue"), lty=1, lwd=2, 
                 trace.label = "Var", xlab="Fecha", ylab="Rend",
                 cex=0.4, main="Gráfico de interacción")

interaction.plot(FA,FC,y, col= c("red","black"), lty=1, lwd=2, 
                 trace.label = "Var", xlab="Fecha", ylab="Rend",
                 cex=0.4, main="Gráfico de interacción")


with(datos, xyplot(y ~ FA | FB, groups = FC, pch = 21, cex=1.2))
with(datos, xyplot(y ~ FA | FB, groups = FC, type="a", lwd=2, col=c("blue", "red"))) #en type se puede poner "o"

#ANDEVA
# Yijkl = u + Ti + pj + Bk + (D)l(i) + (S)lj(i) + (Tp)ij + (TB)ik + (pB)jk + Eijkl

andeva<- aov(y~FA+FB+FC+FA:FB+FA:FC+FB:FC+FA:FB:FC+Error(bloq/FA/FB))
summary(andeva)
andeva1<- aov(y~FA*FB*FC+Error(bloq/FA/FB)) #ambas formas son equivalentes
summary(andeva1)

#Verificación de los supuestos: obtener ANDEVA con 1 error
andeva2<- aov(y~FA*FB*FC+bloq/FA/FB)
summary(andeva2)

#Normalidad:
sw<- shapiro.test(andeva2$residuals)
print(sw)
check_normality(andeva2)
plot(andeva2,2)
qqPlot(andeva2$residuals, col = "red", pch=16, cex = 1.2)

#Homocedasticidad:
leveneTest(andeva2$residuals, FA, center = "mean")
leveneTest(andeva2$residuals, FB, center = "mean")
leveneTest(andeva2$residuals, FC, center = "mean")
leveneTest(andeva2$residuals, FA:FB, center = "mean")
leveneTest(andeva2$residuals, FA:FC, center = "mean")
leveneTest(andeva2$residuals, FB:FC, center = "mean")
leveneTest(andeva2$residuals, FA:FB:FC, center = "mean")

bartlett.test(andeva2$residuals~FA)
bartlett.test(andeva2$residuals~FB)
bartlett.test(andeva2$residuals~interaction(FA,FB))
bartlett.test(andeva2$residuals~interaction(FA,FC))
bartlett.test(andeva2$residuals~interaction(FB,FC))              
bartlett.test(andeva2$residuals~interaction(FA,FB,FC))

check_heteroscedasticity(andeva2)

#Independencia
dwtest(andeva2)

#PMM:
sk<- SK(andeva, which="FA", sig.level = 0.05, dispersion="se")
summary(sk)
sk2<- SK(andeva, which="FB", sig.level = 0.05, dispersion="se")
summary(sk2)
sk3<- SK(andeva, which="FC", sig.level = 0.05, dispersion="se")
summary(sk3)

attach(datos)
datos$FA_FB<- interaction(FA,FB,sep="_")
sk4<- SK(andeva, which="FA_FB", sig.level = 0.05, dispersion="se")
summary(sk4)

datos$FA_FC<- interaction(FA,FC,sep="_")
sk5<- SK(andeva, which="FA_FC", sig.level = 0.05, dispersion="se")
summary(sk5)

datos$FB_FC<- interaction(FB,FC,sep="_")
sk6<- SK(andeva, which="FB_FC", sig.level = 0.05, dispersion="se")
summary(sk6)

datos$FA_FB_FC<- interaction(FA,FB,FC,sep="_")
sk7<- SK(andeva, which="FA_FB", sig.level = 0.05, dispersion="se")
summary(sk7)

tukey<- HSD.test(y, FA, DFerror = 6, MSerror = 0.56, console = T)
tukey<- HSD.test(y, FB, DFerror = 6, MSerror = 0.018, console = T)
tukey<- HSD.test(y, FC, DFerror = 12, MSerror = 0.06, console = T)
tukey<- HSD.test(y, FA:FB, DFerror = 6, MSerror = 0.018, console = T)
tukey<- HSD.test(y, FA:FC, DFerror = 12, MSerror = 0.06, console = T)
tukey<- HSD.test(y, FB:FC, DFerror = 12, MSerror = 0.06, console = T)
tukey<- HSD.test(y, FA:FB:FC, DFerror = 12, MSerror = 0.06, console = T)

graphics.off()
cat("\014")
rm(list=ls())


# Diseño de bloques completos al azar (DBCA)

interaction.plot(FA,FB,y, col= c("red","blue"), lty=1, lwd=2, 
                 trace.label = "Var", xlab="Fecha", ylab="Rend",
                 cex=0.4, main="Gráfico de interacción")

interaction.plot(FA,FC,y, col= c("red","black"), lty=1, lwd=2, 
                 trace.label = "Var", xlab="Fecha", ylab="Rend",
                 cex=0.4, main="Gráfico de interacción")


with(datos, xyplot(y ~ FA | FB, groups = FC, pch = 21, cex=1.2))
with(datos, xyplot(y ~ FA | FB, groups = FC, type="a", lwd=2, col=c("blue", "red"))) #en type se puede poner "o"




# Calcular medias y error estándar (o desviación estándar)

medias <- datos2 %>%      
  group_by(FA) %>%
  summarise(Media = mean(y, na.rm = TRUE), SE = sd(y, na.rm = TRUE) / sqrt(n())) # Error estándar

#Gráfico para comparar las medias por tratamiento:

ggplot(medias, aes(x = FA, y = Media, fill = FA)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = Media - SE, ymax = Media + SE),  # Intervalos de error
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Media de var/trat", x = "var", y = "Rend") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotar etiquetas
  scale_fill_manual(values = c("#D9D9D9", "#FF9999")) 



#ANDEVA
# Yijkl = u + Ti + Bj + pk + Ml + (TB)ij + (y)jk(i) + (Tp)ik + (TM)il + (pM)kl + Eijkl

andeva<- aov(y~bloq+FA+FB+FC+FA:FB+FA:FC+FB:FC+FA:FB:FC+Error(bloq/FA/FB))
summary(andeva)
andeva1<- aov(y~bloq+FA*FB*FC+Error(bloq/FA/FB)) #ambas formas son equivalentes
summary(andeva1)
model.tables(andeva, type = "means")

#Verificación de los supuestos: obtener ANDEVA con 1 error
andeva2<- aov(y~bloq+FA*FB*FC+bloq/FA/FB)
summary(andeva2)

#Normalidad:
sw<- shapiro.test(andeva2$residuals)
print(sw)
check_normality(andeva2)
plot(andeva2,2)
qqPlot(andeva2$residuals, col = "red", pch=16, cex = 1.2)

#Homocedasticidad:
leveneTest(andeva2$residuals, FA, center = "mean")
leveneTest(andeva2$residuals, FB, center = "mean")
leveneTest(andeva2$residuals, FC, center = "mean")
leveneTest(andeva2$residuals, FA:FB, center = "mean")
leveneTest(andeva2$residuals, FA:FC, center = "mean")
leveneTest(andeva2$residuals, FB:FC, center = "mean")
leveneTest(andeva2$residuals, FA:FB:FC, center = "mean")

bartlett.test(andeva2$residuals~FA)
bartlett.test(andeva2$residuals~FB)
bartlett.test(andeva2$residuals~interaction(FA,FB))
bartlett.test(andeva2$residuals~interaction(FA,FC))
bartlett.test(andeva2$residuals~interaction(FB,FC))              
bartlett.test(andeva2$residuals~interaction(FA,FB,FC))

check_heteroscedasticity(andeva2)

#Independencia
dwtest(andeva2)

#PMM:
sk<- SK(andeva, which="FA", sig.level = 0.05, dispersion="se")
summary(sk)
sk2<- SK(andeva, which="FB", sig.level = 0.05, dispersion="se")
summary(sk2)
sk3<- SK(andeva, which="FC", sig.level = 0.05, dispersion="se")
summary(sk3)

attach(datos)
datos$FA_FB<- interaction(FA,FB,sep="_")
sk4<- SK(andeva, which="FA_FB", sig.level = 0.05, dispersion="se")
summary(sk4)

datos$FA_FC<- interaction(FA,FC,sep="_")
sk5<- SK(andeva, which="FA_FC", sig.level = 0.05, dispersion="se")
summary(sk5)

datos$FB_FC<- interaction(FB,FC,sep="_")
sk6<- SK(andeva, which="FB_FC", sig.level = 0.05, dispersion="se")
summary(sk6)

datos$FA_FB_FC<- interaction(FA,FB,FC,sep="_")
sk7<- SK(andeva, which="FA_FB", sig.level = 0.05, dispersion="se")
summary(sk7)

tukey<- HSD.test(y, FA, DFerror = 6, MSerror = 0.56, console = T)
tukey<- HSD.test(y, FB, DFerror = 6, MSerror = 0.018, console = T)
tukey<- HSD.test(y, FC, DFerror = 12, MSerror = 0.06, console = T)
tukey<- HSD.test(y, FA:FB, DFerror = 6, MSerror = 0.018, console = T)
tukey<- HSD.test(y, FA:FC, DFerror = 12, MSerror = 0.06, console = T)
tukey<- HSD.test(y, FB:FC, DFerror = 12, MSerror = 0.06, console = T)
tukey<- HSD.test(y, FA:FB:FC, DFerror = 12, MSerror = 0.06, console = T)


# Diseño en cuadrado latino (DCL)

interaction.plot(FA,FB,y, col=c("red","blue"), lwd=2, lty=1, trace.label = "Fos", xlab="Var",
                 ylab="Forraje")

interaction.plot(FA,FC,y, col=c("red","blue"), lwd=2, lty=1, trace.label = "Nitro", xlab="Var",
                 ylab="Forraje")

with(datos2, xyplot(y~FA | FB, groups = FC, pch=16, cex=1.4))
with(datos, xyplot(y ~ FA | FB, groups = FC, type="a", lwd=2, col=c("blue", "red")))


# Calcular medias y error estándar (o desviación estándar)

medias <- datos2 %>%      
  group_by(FA) %>%
  summarise(Media = mean(y, na.rm = TRUE), SE = sd(y, na.rm = TRUE) / sqrt(n())) # Error estándar

#Gráfico para comparar las medias por tratamiento:

ggplot(medias, aes(x = FA, y = Media, fill = FA)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = Media - SE, ymax = Media + SE),  # Intervalos de error
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Media de var/trat", x = "var", y = "Rend") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotar etiquetas
  scale_fill_manual(values = c("#D9D9D9", "#FF9999", "lightblue", "lightpink")) 


#ANDEVA
# Yijklm = u + Ti + Fj + Ck + Pl + Zm (TFC)ijk + Sjkl(i) + (Tp)il + (TZ)im + (PZ)lm + (TPZ)ilm + Eijklm 

andeva<- aov(y~FA+FB+FC+FA:FB+FA:FC+FB:FC+FA:FB:FC+fila+col+Error(interaction(fila,col)/FB))
summary(andeva)
andeva1<- aov(y~fila+col+FA*FB*FC+Error(interaction(fila,col)/FB)) #Ambas formas son equivalentes
summary(andeva1)

#Verificación de los supuestos: ANDEVA con un error
andeva2<- aov(y~FA*FB*FC+fila+col+interaction(fila, col)/FB)
summary(andeva2)
sw<- shapiro.test(andeva2$residuals)
print(sw)
check_normality(andeva2)
qqPlot(andeva2$residuals, col="red", pch=16)
plot(andeva2,2)

#Homocedasticidad
leveneTest(andeva2$residuals, FA, center="mean")
leveneTest(andeva2$residuals, FB, center="mean")
leveneTest(andeva2$residuals, FC, center="mean")
leveneTest(andeva2$residuals, FA:FB, center="mean")
leveneTest(andeva2$residuals, FA:FC, center="mean")
leveneTest(andeva2$residuals, FB:FC, center="mean")
leveneTest(andeva2$residuals, FA:FB:FC, center="mean")

bartlett.test(andeva2$residuals, FA)
bartlett.test(andeva2$residuals, FB)
bartlett.test(andeva2$residuals, FC)
bartlett.test(andeva2$residuals, FA:FB)
bartlett.test(andeva2$residuals, FA:FC)
bartlett.test(andeva2$residuals, FB:FC)
bartlett.test(andeva2$residuals, FA:FB:FC)

check_heteroscedasticity(andeva2)
plot(andeva2$fitted.values, andeva2$residuals, col="blue", pch=16,
     abline(h=0, col="red", lwd=3, lty=3))

plot(andeva2,1)

par(mar=c(2,4,5,6))
plot(andeva2)
check_model(andeva2)
win.graph(11,11)
check_normality(andeva2) #dar click varias veces en la ventana gráfica para que aparezcan todos los graficos

#Independencia
dwtest(andeva2)


#PMM:
sk<- SK(andeva, which="FA", sig.level = 0.05, dispersion="se")
summary(sk)
sk2<- SK(andeva, which="FB", sig.level = 0.05, dispersion="se")
summary(sk2)
sk3<- SK(andeva, which="FC", sig.level = 0.05, dispersion="se")
summary(sk3)


datos2$FA_FB<- interaction(FA,FB, sep="_")
datos2$FA_FC<- interaction(FA,FC,sep = "_")
datos2$FB_FC<- interaction(FB,FC, sep = "_")
datos2$FA_FB_FC<- interaction(FA,FB,FC,sep = "_")
attach(datos2)
sk4<- SK(andeva, which="FA_FB", sig.level = 0.05, dispersion="se")
summary(sk4)
sk5<- SK(andeva, which="FA_FC", sig.level = 0.05, dispersion="se")
summary(sk5)
sk6<- SK(andeva, which="FB_FC", sig.level = 0.05, dispersion="se")
summary(sk6)
sk7<- SK(andeva, which="FA_FB_FC", sig.level = 0.05, dispersion="se")
summary(sk7)

tukey<- HSD.test(y, FA, DFerror = 6, MSerror = 0.0808, console = T)
print(tukey)
tukey1<- HSD.test(y, FB, DFerror = 12, MSerror = 0.00479, console = T)
print(tukey1)
tukey2<- HSD.test(y, FC, DFerror = 48, MSerror = 0.0065, console = T)
print(tukey2)


################################# Arreglo en franjas
# Diseño de bloques completos al azar (DBCA)

par(mar=c(4,4,3,9), cex=0.8)
interaction.plot(FA,FB,y, col= c("red", "blue"), lwd=2, 
                 xlab="N", ylab="Rend", trace.label = "Riego")

with(datos, xyplot(y~FA | FB, groups = bloq, pch = 21, cex=1.2))
with(datos, xyplot(y~FA | FB, type="a", groups = FB, lwd=2, col=c("red", "blue")))





# Calcular medias y error estándar (o desviación estándar)

medias <- datos2 %>%      
  group_by(FA) %>%
  summarise(Media = mean(y, na.rm = TRUE), SE = sd(y, na.rm = TRUE) / sqrt(n())) # Error estándar

#Gráfico para comparar las medias por tratamiento:

ggplot(medias, aes(x = FA, y = Media, fill = FA)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = Media - SE, ymax = Media + SE),  # Intervalos de error
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Media de var/trat", x = "var", y = "Rend") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotar etiquetas
  scale_fill_manual(values = c("#D9D9D9", "#FF9999", "lightblue")) 


#ANDEVA
# Yijk = u + Ti + Bj + pk + (TB)ij + (Tp)ik + (Tp)ik + Eijk

andeva<- strip.plot(bloq, FA, FB, y) #con la fx strip.plot(), introducirlo en ese orden
andeva1<- aov(y~FA+FB+FA:FB+bloq+Error(bloq/(FA+FB)))
summary(andeva1)


#Verificación de los supuestos: ANDEVA con un error
andeva2<- aov(y~FA*FB+bloq+bloq/(FA+FB))
summary(andeva2)
sw<- shapiro.test(andeva2$residuals)
print(sw)
check_normality(andeva2)
qqPlot(andeva2$residuals, col="red", pch=16)
plot(andeva2,2)

#Homocedasticidad
leveneTest(andeva2$residuals, FA, center="mean")
leveneTest(andeva2$residuals, FB, center="mean")
leveneTest(andeva2$residuals, FA:FB, center="mean")

bartlett.test(andeva2$residuals, FA)
bartlett.test(andeva2$residuals, FB)
bartlett.test(andeva2$residuals, FA:FB)

check_heteroscedasticity(andeva2)
plot(andeva2$fitted.values, andeva2$residuals, col="blue", pch=16,
     abline(h=0, col="red", lwd=3, lty=3))

plot(andeva2,1)

par(mar=c(2,4,5,6))
plot(andeva2)
check_model(andeva2)
win.graph(11,11)
check_normality(andeva2) #dar click varias veces en la ventana gráfica para que aparezcan todos los graficos

#Independencia
dwtest(andeva2)


#PMM:
sk<- SK(andeva1, which="FA", sig.level = 0.05, dispersion="se")
summary(sk)
sk2<- SK(andeva1, which="FB", sig.level = 0.05, dispersion="se")
summary(sk2)
datos2$FA_FB<- interaction(FA,FB, sep="_")
attach(datos2)

sk3<- SK(andeva1, which="FA_FB", sig.level = 0.05, dispersion="se")
summary(sk3)


tukey<- HSD.test(y, FA, DFerror = 6, MSerror = 2.82, console = T)
print(tukey)
tukey1<- HSD.test(y, FB, DFerror = 3, MSerror = 10.9, console = T)
print(tukey1)
tukey2<- HSD.test(y, FA:FB, DFerror = 6, MSerror = 1.43, console = T)
print(tukey2)


# Experimentos multi-ambiente o por localidad
# Diseño de bloques completos al azar

interaction.plot(Lin, Loc, Rend, fixed=T, xlab="Línea de frijol", ylab="localidad", 
                 main="Gráfico de interacción",
                 legend = T, 
                 trace.label = "Localidad")

#ANDEVA/Localidad
Jicaro<-subset(datos, Loc=="Jicaro")  #Para extraer la información solo de la localidad Jicaro
ANDEVA_jic<-aov(Rend ~ Lin + Bloq, data = Jicaro)
summary(ANDEVA_jic)

Rancho<-subset(datos, Loc=="Rancho")
ANDEVA_Ran<-aov(Rend ~ Lin + Bloq, data = Rancho)
summary(ANDEVA_Ran)

Estancia<- subset(datos, Loc=="Estancia")
ANDEVA_Est<-aov(Rend ~ Lin + Bloq, data=Estancia)
summary(ANDEVA_Est)


#ANDEVA GENERAL

#Conversión de datos a factores categóricos
datos<- datos %>%
  mutate(across(c(Lin, Loc, Bloq, Loc_Bloq), .fns=factor))  #Primer método para convertir los datos a factores

datos$Loc<- as.factor(datos$Loc)
datos$Lin<- as.factor(datos$Lin)
datos$Bloq<- as.factor(datos$Bloq)
datos$Loc_Bloq<- as.factor(datos$Loc_Bloq) #Segundo método para convertir los datos a factores

mod <- aov(Rend ~ Lin + Loc + Error(Bloq %in% Loc) + Lin:Loc, data=datos)
summary(mod)

mod1 <- aov(Rend ~ datos$Lin + datos$Loc + Error(datos$Bloq/datos$Loc) + datos$Loc:datos$Lin )
summary(mod1)

#Verificación de supuestos
#Residuos vs predichos
resp1<-aov(Rend~ Lin*Loc+ Bloq/Loc)
summary(resp1)
plot(resp1,1)
#QQ-plot
plot(resp1,2)
#Prueba de Shapiro Wilks
shapiro.test(resp1$residuals)


#Prueba múltiple de medias (Scott Knott)
sk1<- SK(mod,
         data=datos,
         which="Lin",
         sig.level = 0.05)
summary(sk1)

sk2<- SK(mod,
         data=datos,
         which="Loc",
         sig.level = 0.05)
summary(sk2)



# Experimentos multiambiente con modelo lineal de efectos fijos #################

names(datos)

mod <- aov(pureza ~ volumen + zafra + Error(Replica %in% zafra) + volumen:zafra, data=datos)
summary(mod)

mod1 <- aov(pureza ~ datos$volumen + datos$zafra + Error(datos$Replica/datos$zafra) + datos$zafra:datos$volumen)
summary(mod1)

#Verificación de supuestos
#Residuos vs predichos
resp1<-aov(pureza ~ volumen*zafra+ Replica/zafra)
summary(resp1)
plot(resp1,1)
#QQ-plot
plot(resp1,2)
#Prueba de Shapiro Wilks
shapiro.test(resp1$residuals)


#Prueba múltiple de medias (Scott Knott)
sk1<- SK(mod,
         data=datos,
         which="volumen",
         sig.level = 0.15)
summary(sk1)

sk2<- SK(mod,
         data=datos,
         which="zafra",
         sig.level = 0.15)
summary(sk2)




# Modelo mixto para multiambientales: diferente finca y diferente año 
names(datos)

datos$Ambiente <- as.factor(paste(datos$ingenio, datos$zafra, sep = "_"))

modelo_tch <- lmer(pureza ~ volumen + (1|Ambiente) + (1|Ambiente:Replica), data = datos)
anova(modelo_tch)

medias_ajustadas <- emmeans(modelo_tch, ~ volumen)


cld(medias_ajustadas, Letters = letters, reverse = TRUE)



modelo_para_ambientes <- lmer(rendimiento ~ volumen + Ambiente + (1|Ambiente:Replica), data = datos)

tukey_ambientes <- emmeans(modelo_para_ambientes, pairwise ~ Ambiente)

summary(tukey_ambientes$contrasts)

tukey_letras <- cld(tukey_ambientes$emmeans, Letters = letters, reverse = TRUE, alpha = 0.15)
print(tukey_letras)


# ANCOVA: para cada diseño (experimentos simples y factoriales): incluye la opción de agregar una o varias covariables

#ANCOVA: diseño completamente al azar (DCA)
# Yij = u + Ti + B(xij-Xm) + Eij

#Supuestos del ANCOVA:

# Homogeneidad de pendientes de regresión (B) entre X e Y para cada trat
# Ho: las pendientes son iguales entre cada trat
# Ha: las pendientes son distintas entre cada trat

# Relación lineal entre X e Y (linealidad)
# Diagrama de dispersión entre X e Y

# Independencia entre FA y X (debe cumplirse la Ho para verificar independencia)
# Ho: Ti = T
# Ha: Ti distinto de T

# Independencia de los residuos
# Homocedasticidad de los res
# Normalidad

ancova<- aov(y~FA+x) #Modelo sin la interacción
summary(ancova)
cv.model(ancova) # %CV



ancova2<- aov(y~FA*x) #Modelo con interaccion para verificar el cumplimiento de la homogeneidad de pendientes
summary(ancova2)
print(anova(ancova, ancova2)) # Para comparar los modelos y verificar el cumplimiento del supuesto de HP

ancova3<- aov(y~FA+x+FA:x) # Otra forma de verificar el supuesto de HP
summary(ancova3)
# En un grafico de dispersión las líneas de cada tratamiento deben ser paralelas


#Verificar supuesto de linealidad de pendientes
plot(x, y, col = FA, pch=16, cex=1.3, xlab="Covariable", ylab = "VR")


# Gráfico de dispersión con líneas de regresión por tratamiento

ggplot(datos2, aes(x = x, y = y, color = FA)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "covariable", 
       y = "variable de respuesta", title = "Relación entre x e y")

# Análisis de correlación por tratamiento
correlaciones<- datos2 %>%
  group_by(FA) %>%
  summarise(correlacion = cor(x, y, method = "pearson"),
            p_valor = cor.test(x, y, method = "pearson")$p.value)

print(correlaciones)


#Gráfico para verificar que los puntos se ajusten a líneas curvas (en ese caso, no hay linealidad)
ggplot(datos2, aes(x = x, y = y, color = FA)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "cov", y = "Resp", title = "Relación no lineal entre X e Y")


# ANCOVA CON lm y cálculo del coeficiente angular de regresión (B)

ancova5<- lm(y~FA+x) # Otra forma de realizar el ANCOVA
Anova(ancova5, type = "III")

sumRes<- summary(ancova5)
print(sumRes)

# Media general de la VR
mean(datos2[,3])
mean(y) # Otra forma

# Normalidad:
shapiro.test(ancova$residuals)
check_normality(ancova)
qqPlot(ancova$residuals, col="red", pch=16, cex=1.3)
plot(ancova,2)

# Homocedasticidad
leveneTest(y~FA, center = "mean")
check_heteroscedasticity(ancova)
plot(ancova$fitted.values, ancova$residuals, col = "red", pch = 16, cex = 1.3,
     xlab="predichos", ylab="residuos", abline(h=0, col="black", lwd= 2))

# Independencia
dwtest(ancova)



# PMM:
lsmeans(ancova,~FA) # Medias de Y ajustadas por el efecto de la covariable x

medias_ajustadas <- emmeans(ancova, ~ FA); print(medias_ajustadas) #Otra forma de hacerlo


# Hacer las comparaciones o contrastes de dos en dos (pares)
tukey<- lsmeans(ancova, pairwise~FA, glhargs=list());tukey
duncan<- lsmeans(ancova, pairwise~FA, glhargs=list("duncan"));duncan

tukey1<- pairs(medias_ajustadas, adjust = "tukey") #otra forma (con emmeans)
summary(tukey1)


#Prueba de Scott-Knott
sk<- SK(ancova, which = "FA", dispersion = "se", sig.level = 0.05)
summary(sk)

tukey1<- HSD.test(ancova, "FA", console = T)

#Prueba de Tukey original
tukey_cld <- cld(medias_ajustadas, adjust = "sidak", Letters = letters)
print(tukey_cld)


#Gráfico de barras con medias ajustadas:
medias_aj<- as.data.frame(summary(medias_ajustadas))

ggplot(medias_aj, aes(x = FA, y = emmean, fill = FA)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.4) +
  labs(x = "Tratamiento (FA)", y = "Media ajustada de Y", 
       title = "Medias ajustadas por nivel de FA",
       fill="Trat") +
  theme_minimal() +
  theme(legend.position = "right") 

## Diseño de bloques completos al azar

#ANCOVA: diseño completamente al azar (DCA)
# Yij = u + Ti + Bj + B(xij-Xm) + Eij

ancova<- aov(y~FA+bloq+x)
summary(ancova)
ancova1<- aov(y~FA*x+bloq) #Supuesto de homogeneidad de pendientes
summary(ancova1)
print(anova(ancova, ancova1)) #comparación de los dos modelos para comprobar HP
cv.model(ancova)

plot(x,y, col=FA, pch=16, cex=1.2)

ggplot(datos2, aes(x=x, y=y, color = FA)) +
  geom_point() +
  geom_smooth(method = "lm", se=FALSE) +
  labs(x="cov", y="VR", title="Gráfico de pendientes", color = "Trat") +
  theme_minimal() +
  theme(legend.position = "right") 

correlaciones<- datos2 %>%
  group_by(FA) %>%
  summarise(coeficiente = cor(x,y, method = "pearson"),
            p_valor = cor.test(x, y, method = "pearson")$p.value)

print(correlaciones)

#Gráfico para ver si los datos se adaptan a curvas

ggplot(datos2, aes(x=x, y=y, color=FA)) +
  geom_point() +
  geom_smooth(method = "loess", se=F) +
  labs(x = "cov", y = "Resp", title = "Relación no lineal entre X e Y", color="trat")


ancova2<- aov(x~FA+bloq) #No diferencias significativas indica HP e independencia de x en cada trat
summary(ancova2)

#Cálculo de coeficiente angular Beta
ancova3<- lm(y~FA+bloq+x) #el coeficiente solo se obtiene con lm
summary(ancova3)

#Media general de la VR
mean(datos2[,5])
mean(y)

#Normalidad
shapiro.test(ancova$residuals)
check_normality(ancova)
plot(ancova,2)
qqPlot(ancova$residuals, col="red", pch=16, cex=1.2)

#Homocedasticidad
leveneTest(y~FA, center="mean")
bartlett.test(y~FA)
check_heteroscedasticity(ancova)

#Independencia
dwtest(ancova)

#PMM:
sk<- SK(ancova, which = "FA", dispersion="se", sig.level = 0.05)
summary(sk)

#Medias ajustadas
medias_ajustadas<- lsmeans(ancova,~FA)
print(medias_ajustadas)

medias_aj2 <- emmeans(ancova, ~ FA); print(medias_aj2) #Otra forma de hacerlo


# Hacer las comparaciones o contrastes de dos en dos (pares)
tukey<- lsmeans(ancova, pairwise~FA, glhargs=list());tukey
duncan<- lsmeans(ancova, pairwise~FA, glhargs=list("duncan"));duncan

tukey1<- pairs(medias_ajustadas, adjust = "tukey") #otra forma (con emmeans)
summary(tukey1)


#Prueba de Tukey original
tukey_cld <- cld(medias_aj2, adjust = "sidak", Letters = letters)
print(tukey_cld)


#Gráfico de barras con medias ajustadas:
medias_aj3<- as.data.frame(summary(medias_aj2))

ggplot(medias_aj3, aes(x = FA, y = emmean, fill = FA)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.4) +
  labs(x = "Tratamiento (FA)", y = "Media ajustada de Y", 
       title = "Medias ajustadas por nivel de FA",
       fill="Trat") +
  theme_minimal() +
  theme(legend.position = "right") 

# Analisis de correlacion:

correlacion<-cor(ca, ce) #para obtener el coeficiente de correlación de Pearson (p)   
summary(correlacion)
print(correlacion)

plot(ca, ce, col="blue", pch=12, main="Gráfico de dispersión", xlab="CE", ylab="Ca")
modelo<-lm(ce ~ ca, data=salinidad)
summary(modelo)
abline(modelo, col="red", lwd=2)

#Otra forma:-------------------------------------------------------------------------------------------------------
datos<- read_excel("salinidad.xlsx", sheet = 1)
attach(datos)
view(datos)
head(datos)
colnames(datos)

par(mfrow = c(1,2), cex.main=1) #Sirve para ver ambos gráficos en un mismo plano (1 fila, 2 columnas)

plot(CE,`Ca2+`, xlab="Concentracion de Ca", ylab="CE", main = "diagrama de dispersion", col= "red")
plot(`Ca2+`, CE, xlab = "CE", ylab = "Concentración de calcio", main = "diagrama de dispersion", col = "blue")

correlacion<- cor.test(CE, `Ca2+`, method = "pearson", alternative = "two.sided", conf.level = 0.95 )
print(correlacion)

correlacion2<- cor(CE, `Ca2+`, method = "pearson")
print(correlacion2)

################################# Matriz de correlación #####################################


# Matriz de correlación
variables_correlacion <- datos %>%
  select(Altura, Diametro, Tallos_macolla, Poblacion)

matriz_cor <- cor(variables_correlacion, use = "complete.obs", method = "pearson")

cat("MATRIZ DE CORRELACIÓN DE PEARSON\n")
print(round(matriz_cor, 3))
# Incluye el grafico correlograma


# Análisis de regresion: incluye el analisis para regresion lineal simple, logaritmico, exponencial y multiple

#Regresion lineal simple: DAP(x) vs H(y)--------------------------
names(datos)
modelo1<-lm(volumen ~ diametro, data=datos)
summary(modelo1)
print(modelo1)
#Modelo: y=Bo+B1x-----y=7.357+0.489x
plot(diametro, volumen, main="Gráfico de dispersion (vol vs DAP)", xlab="DAP (cm)", ylab="Altura (m)", pch=19, col="blue")
abline(modelo1, col="red", linetype="solid", lwd=2)
plot(modelo1, 1:2)
Shapiro<- shapiro.test(modelo1$residuals)
print(Shapiro)

#Modelo logarítmico:DAP (X) y Volumen (Y)----------------------
names(datos)
modelo2<-lm(log(volumen)~log(diametro), data=datos)
summary(modelo2)
print(modelo2)
plot(log(diametro), log(volumen))
abline(modelo2, col="red", lwd=2 )
#Modelo resultante: LnY=B0+B1LnX------     LnY=-9.603+2.652*Ln(X)
plot(modelo2, 1:2)
Shapiro2<- shapiro.test(modelo2$residuals)
print(Shapiro2)


#Modelo potencial: lo ajustamos a partir del modelo logaritmico--------------
B1<-exp(-7.690);B1
#Modelo: e^(LnY)=e^(-9.603+2.652*LnX)=(e^-9.603)*(e^2.652*LnX)= y=0.00006752585*x^2.652: y=B0*X^B1

#Modelo final: y=0.00006752585*x^2.652: y=B0*X^B1
names(datos)
Y<- 0.0004573782 * diametro^2.062 

plot(diametro, Y)



#Ejemplo para modelo logaritmico y potencial--------------------------------
#Modelo logarítmico: H(X) y Volumen(Y)
names(datos)
modelo_ejemplo<-lm(log(VOLUMEN) ~ log(H))
summary(modelo_ejemplo)
print(modelo_ejemplo)
plot(log(H), log(VOLUMEN))
abline(modelo_ejemplo, col="red", lwd=2)
# LnY=-11.3722*x^3.459
B0<-exp(-11.3722); B0
# Y=1.151109e-05*x^3.459

#Regresión múltiple--------------------------------
#DAP (X1) y ALTURA (X2) para predecir el Volumen (Y): modelo lineal múltiple
names(datos)
modelo3<-lm(VOLUMEN ~ DAP+H)
summary(modelo3)
print(modelo3)
# Y=-0.65313+0.02874(X1)+0.01620(X2)  

# Regresion polinomial

#Ajuste del modelo polinomial de grado 2: y=B0+B1X+B2X^2
names(datos)
modelo2<-lm(tension~madera+I(madera^2), data=datos)
print(modelo2)
summary(modelo2)
check_model(modelo2)
vif(modelo2)
AIC(modelo2)
#Quitamos B0 porque no es significativo
modelo3<-lm(tension~-1+madera+I(madera^2), data=datos)
summary(modelo3)
vif(modelo3)
print(vif(modelo3))
AIC(modelo3)

residuos3<-residuals(modelo3)
predichos3<-fitted(modelo3)
qqnorm(residuos3)
qqline(residuos3, col="red")
SW3<-shapiro.test(residuos3)
summary(SW3)
print(SW3)
plot(predichos3, residuos3)
abline(h=0, col="red", lwd=3)
check_model(modelo3)
AIC(modelo3)

ggplot(datos, aes(x=madera, y=tension)) +
  geom_point()+
  geom_smooth(method="lm", se=TRUE, col="red", linetype="solid")+
  geom_smooth(method="lm", se=TRUE, col="blue", linetype="solid")

# Es regresión polinomial de varios grados


names(datos)
modelo1<-lm(tch ~ velocidad, data=datos)
summary(modelo1)
print(modelo1)
#Modelo: y=Bo+B1x-----y=7.357+0.489x
plot(velocidad, tch, main="Gráfico de dispersion (TCH vs velocidad)", xlab="Velocidad (km/h)", ylab="TCH", pch=19, col="blue")
abline(modelo1, col="red", linetype="solid", lwd=2)




# En bucle

respuestas_reg <- c("tch")
f_reg_x <- "fertilidad"
x_lims <- c(40, 110) 

for (var in respuestas_reg) {
  if (!var %in% colnames(datos)) next
  
  # 2. LIMPIEZA DE DATOS
  df_temp <- datos %>%
    mutate(
      x_num = as.numeric(gsub("[^0-9.]", "", as.character(get(f_reg_x)))),
      y_clean = gsub(",", ".", as.character(.data[[var]])),
      y_num = as.numeric(gsub("[^0-9.]", "", y_clean))
    ) %>%
    filter(!is.na(x_num), !is.na(y_num))
  
  # 3. MODELO Y ESTADÍSTICOS
  modelo <- lm(y_num ~ x_num, data = df_temp)
  aic_val <- round(AIC(modelo), 2)
  bic_val <- round(BIC(modelo), 2)
  r2_val  <- round(summary(modelo)$r.squared, 3)
  p_val   <- summary(modelo)$coefficients[2,4]
  
  # Impresión en consola para diagnóstico
  cat("\nVariable:", toupper(var), "| AIC:", aic_val, "| BIC:", bic_val, "| R2:", r2_val, "\n")
  
  # 4. CÁLCULO DE BANDAS (Suavizado)
  x_range <- seq(x_lims[1], x_lims[2], length.out = 200)
  preds <- data.frame(x_num = x_range)
  
  conf <- predict(modelo, newdata = preds, interval = "confidence")
  pred <- predict(modelo, newdata = preds, interval = "prediction")
  df_preds <- cbind(preds, as.data.frame(conf), as.data.frame(pred[,-1]))
  colnames(df_preds) <- c("x_num", "fit", "lwr_conf", "upr_conf", "lwr_pred", "upr_pred")
  
  # 5. GRÁFICO PROFESIONAL
  g <- ggplot(df_temp, aes(x = x_num, y = y_num)) +
    # Banda de Predicción (Área de incertidumbre de datos individuales)
    geom_ribbon(data = df_preds, aes(y = fit, ymin = lwr_pred, ymax = upr_pred), 
                fill = "grey92", alpha = 0.5) +
    # Banda de Confianza (Precisión de la recta de regresión)
    geom_ribbon(data = df_preds, aes(y = fit, ymin = lwr_conf, ymax = upr_conf), 
                fill = "steelblue", alpha = 0.2) +
    # Línea de tendencia elegante
    geom_line(data = df_preds, aes(y = fit), color = "#2C3E50", linewidth = 1.1) +
    # Puntos con borde (Estética clásica)
    geom_point(size = 3, color = "white", fill = "#2C3E50", shape = 21, stroke = 1) +
    
    # Anotaciones matemáticas con mejor formato
    annotate("label", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2,
             label = paste0("Model: y = ", round(coef(modelo)[1],2), " + ", round(coef(modelo)[2],2), "x",
                            "\nR² = ", r2_val, " (p < ", format.pval(p_val, digits = 2), ")",
                            "\nAIC = ", aic_val),
             family = "Arial", size = 3.5, fill = "white", label.size = NA, alpha = 0.8) +
    
    # Escalas y visualización
    scale_x_continuous(breaks = c(50, 75, 100)) +
    scale_y_continuous(n.breaks = 6) +
    coord_cartesian(xlim = x_lims) +
    
    labs(title = paste("Análisis de Tendencia Lineal:", toupper(var)),
         subtitle = "Diseño con intervalos de confianza (95%) y predicción",
         x = "Dosis (%)", 
         y = paste("Respuesta:", var)) +
    
    # Tema profesional
    theme_pubr() + # Basado en ggpubr para estilo de revista científica
    theme(
      aspect.ratio = 3/4, # Estira el gráfico para que la pendiente sea más clara
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, color = "grey30"),
      axis.title = element_text(face = "bold"),
      panel.grid.major.y = element_line(color = "grey95"), # Líneas horizontales sutiles
      axis.line = element_line(color = "black")
    )
  
  print(g)
}


############################--Regresión lineal múltiple--#######################################
datos$Replica<- as.numeric(datos$Replica)
class(datos$Replica)

names(datos)
modelo3<-lm(tch ~ velocidad + Replica)
summary(modelo3)
print(modelo3)

names(datos)
modelo4<-lm(rendimiento_kg_ton ~ velocidad + Replica)
summary(modelo3)
print(modelo3)

names(datos)
modelo5<-lm(tah ~ velocidad + Replica)
summary(modelo3)
print(modelo3)

names(datos)
modelo6<-lm(pureza ~ velocidad + Replica)
summary(modelo3)
print(modelo3)


############# En bucle

datos$Replica <- as.numeric(datos$Replica)

resp_multiple <- "tch"
modelos_a_probar <- list(
  modelo1 = c("velocidad"),
  modelo2 = c("velocidad", "Replica") 
)


for (i in seq_along(modelos_a_probar)) {
  vars_x <- modelos_a_probar[[i]]
  
  formula_m <- as.formula(paste(resp_multiple, "~", paste(vars_x, collapse = " + ")))
  modelo_m <- lm(formula_m, data = datos, na.action = na.exclude)
  
  
  coefs <- round(coef(modelo_m), 3)
  intercepto <- coefs[1]
  
  
  ecuacion_texto <- paste0("italic(Y) == ", intercepto)
  
  for (j in 2:length(coefs)) {
    valor <- coefs[j]
    nombre_var <- names(coefs)[j]
    
    signo <- if(valor >= 0) " + " else " " 
    ecuacion_texto <- paste0(ecuacion_texto, signo, valor, "*italic(", nombre_var, ")")
  }
  
  datos_grafico <- data.frame(
    Observado = datos[[resp_multiple]],
    Predicho = predict(modelo_m, datos)
  )
  
  r2_val <- round(summary(modelo_m)$adj.r.squared, 3)
  p_val <- format.pval(summary(modelo_m)$coefficients[2,4], digits = 2)
  
  p_m <- ggplot(datos_grafico, aes(x = Predicho, y = Observado)) +
    geom_point(color = "black", fill = "gray70", shape = 21, size = 3, alpha = 0.8) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", size = 0.7) +
    geom_smooth(method = "lm", color = "firebrick", fill = "firebrick", alpha = 0.1, size = 1) +
    labs(
      title = paste("Validación del Modelo:", names(modelos_a_probar)[i]),
      subtitle = bquote(R[adj]^2 == .(r2_val) ~~~ p < .(p_val)),
      x = "Predichos",
      y = "Valores observados"
    ) +
    theme_bw(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      axis.title = element_text(face = "bold")
    ) +
    
    annotate("text", 
             x = -Inf, y = Inf, # Esquina superior izquierda
             label = ecuacion_texto, 
             parse = TRUE, 
             hjust = -0.1, vjust = 1.5, 
             size = 4, color = "darkblue")
  
  print(p_m)
}


# Gráficos de dispersión por variable regresora

for (i in seq_along(modelos_a_probar)) {
  vars_x <- modelos_a_probar[[i]]
  
  for (x_var in vars_x) {
    
    fit <- lm(as.formula(paste(resp_multiple, "~", x_var)), data = datos)
    r2_simple <- round(summary(fit)$r.squared, 3)
    
    p_disp <- ggplot(datos, aes_string(x = x_var, y = resp_multiple)) +
      geom_point(color = "black", fill = "steelblue", shape = 21, size = 3, alpha = 0.6) +
      geom_smooth(method = "lm", color = "darkred", fill = "gray80", size = 1) +
      labs(
        title = paste("Relación Individual:", x_var, "vs", resp_multiple),
        subtitle = bquote(R^2 == .(r2_simple)),
        x = paste(x_var, "(Km/h)"),
        y = paste(resp_multiple)
      ) +
      theme_bw(base_size = 12) +
      theme(panel.grid.minor = element_blank())
    
    print(p_disp)
  }
}






library(ggplot2)

# Asegurar que Replica sea numérica
datos$Replica <- as.numeric(as.character(datos$Replica))

resp_multiple <- "tch"
modelos_a_probar <- list(
  modelo1 = c("velocidad"),
  modelo2 = c("velocidad", "Replica") 
)

# 1. BUCLE DE VALIDACIÓN (Observado vs Predicho)
# ---------------------------------------------------------
for (i in seq_along(modelos_a_probar)) {
  vars_x <- modelos_a_probar[[i]]
  formula_m <- as.formula(paste(resp_multiple, "~", paste(vars_x, collapse = " + ")))
  modelo_m <- lm(formula_m, data = datos, na.action = na.exclude)
  
  coefs <- round(coef(modelo_m), 3)
  ecuacion_texto <- paste0("italic(Y) == ", coefs[1])
  for (j in 2:length(coefs)) {
    valor <- coefs[j]; nombre_var <- names(coefs)[j]
    signo <- if(valor >= 0) " + " else " " 
    ecuacion_texto <- paste0(ecuacion_texto, signo, valor, "*italic(", nombre_var, ")")
  }
  
  datos_grafico <- data.frame(
    Observado = datos[[resp_multiple]],
    Predicho = predict(modelo_m, datos)
  )
  
  r2_val <- round(summary(modelo_m)$adj.r.squared, 3)
  p_val <- format.pval(summary(modelo_m)$coefficients[2,4], digits = 2)
  
  p_m <- ggplot(datos_grafico, aes(x = Predicho, y = Observado)) +
    geom_point(color = "black", fill = "gray70", shape = 21, size = 3, alpha = 0.8) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
    geom_smooth(method = "lm", color = "firebrick", fill = "firebrick", alpha = 0.1) +
    # ASPECT.RATIO = 1 hace que el gráfico sea cuadrado sin importar las unidades
    theme_bw(base_size = 14) +
    theme(aspect.ratio = 1, plot.title = element_text(face = "bold")) +
    labs(
      title = paste("Validación del Modelo:", names(modelos_a_probar)[i]),
      subtitle = bquote(R[adj]^2 == .(r2_val) ~~~ p < .(p_val)),
      x = "Valores Predichos", y = "Valores Observados"
    ) +
    annotate("text", x = -Inf, y = Inf, label = ecuacion_texto, parse = TRUE, 
             hjust = -0.05, vjust = 1.5, size = 4, color = "darkblue")
  
  print(p_m)
}

# 2. BUCLE DE DISPERSIÓN INDIVIDUAL (X vs Y)
# ---------------------------------------------------------
for (i in seq_along(modelos_a_probar)) {
  vars_x <- modelos_a_probar[[i]]
  
  for (x_var in vars_x) {
    fit_s <- lm(as.formula(paste(resp_multiple, "~", x_var)), data = datos, na.action = na.exclude)
    cf <- round(coef(fit_s), 3); sgn <- if(cf[2] >= 0) " + " else " "
    ecuacion_s <- paste0("italic(Y) == ", cf[1], sgn, cf[2], "*italic(", x_var, ")")
    r2_simple <- round(summary(fit_s)$r.squared, 3)
    
    p_disp <- ggplot(datos, aes_string(x = x_var, y = resp_multiple)) +
      geom_jitter(width = 0.05, color = "black", fill = "steelblue", shape = 21, size = 3, alpha = 0.6) +
      geom_smooth(method = "lm", color = "darkred", fill = "gray80") +
      # Aquí también forzamos un marco cuadrado para que la línea se vea inclinada
      theme_bw(base_size = 12) +
      theme(aspect.ratio = 1, panel.grid.minor = element_blank()) +
      labs(
        title = paste("Relación:", x_var, "vs", resp_multiple),
        subtitle = bquote(R^2 == .(r2_simple)),
        x = paste(x_var, "(km/h)"), y = resp_multiple
      ) +
      annotate("text", x = -Inf, y = Inf, label = ecuacion_s, parse = TRUE, 
               hjust = -0.05, vjust = 1.5, size = 4, color = "darkred")
    
    print(p_disp)
  }
}

###########################################################################################
# Estadística multivariada
######## MANOVA ##########

library(car)
library(biotools)
library(emmeans)
library(ggplot2)
library(multcomp)
library(multcompView)
library(dplyr)
library(ggpubr)


deps <- c("TCH", "Rendimiento.Kg.Ton", "TAH", "Pureza")
f_principal <- "Tratamiento"
f_bloque <- "Replica" 

datos[[f_principal]] <- as.factor(datos[[f_principal]])
datos[[f_bloque]] <- as.factor(datos[[f_bloque]])

# manova global
form_m <- as.formula(paste("cbind(", paste(deps, collapse = ","), ") ~", f_bloque, "+", f_principal))
mod_m <- manova(form_m, data = datos)

cat("\n--- resultados globales (wilks) ---\n")
print(summary(mod_m, test = "Wilks")) # Wilks,Pillai, Hotelling-Lawley, Roy 

# --- supuestos ---
cat("\n--- verificación de supuestos ---\n")
print(boxM(datos[, deps], datos[[f_principal]])) # Homocedasticidad

datos_final_grafico <- data.frame()

for (var in deps) {
  cat("\nanálisis para:", var, "\n")
  
  f_uni <- as.formula(paste(var, "~", f_bloque, "+", f_principal))
  fit <- aov(f_uni, data = datos)
  
  res_em <- emmeans(fit, specs = f_principal)
  res_cld <- multcomp::cld(res_em, Letters = letters, adjust = "tukey")
  
  print(summary(fit))
  
  df_temp <- as.data.frame(res_cld) %>%
    rename(media_r = emmean, se_r = SE) %>%
    mutate(Variable = var)
  
  datos_final_grafico <- rbind(datos_final_grafico, df_temp)
}

# Gráfico
grupos_graficos <- split(deps, ceiling(seq_along(deps) / 4))

for (i in seq_along(grupos_graficos)) {
  datos_plot <- datos_final_grafico %>%
    filter(Variable %in% grupos_graficos[[i]]) %>%
    mutate(Variable = factor(Variable, levels = grupos_graficos[[i]]))
  
  if(nrow(datos_plot) == 0) next
  
  gg <- ggplot(datos_plot, aes(y = reorder(!!sym(f_principal), media_r), x = media_r)) +
    geom_errorbarh(aes(xmin = media_r - se_r, xmax = media_r + se_r), height = 0.2) +
    geom_point(size = 3, color = "black") +
    geom_text(aes(label = sprintf("%.2f %s", media_r, trimws(.group))),
              hjust = -0.2, vjust = -1, size = 4) +
    facet_wrap(~ Variable, scales = "free_x") + 
    scale_x_continuous(expand = expansion(mult = c(0.1, 0.5))) +
    labs(x = "Media Ajustada ± SE", y = f_principal,
         title = paste("comparación de medias - panel", i),
         subtitle = "modelo ajustado por factor réplica") +
    theme_classic2() +
    theme(strip.text = element_text(face = "bold"))
  
  print(gg)
}


########## Análisis discriminante ###############

# librerías
library(MASS)
library(ggplot2)
library(dplyr)
library(tidyr)

# configuración
deps <- c("TCH", "Rendimiento.Kg.Ton", "TAH", "Pureza")
f_principal <- "Tratamiento"

# --- análisis discriminante lineal (lda) ---
form_lda <- as.formula(paste(f_principal, "~", paste(deps, collapse = "+")))
mod_lda <- lda(form_lda, data = datos)

# 1. salida de consola ultra-limpia
cat("\n========================================================")
cat("\n       RESUMEN DEL ANÁLISIS DISCRIMINANTE (LDA)         ")
cat("\n========================================================\n")

# Proporción de traza (importancia de cada dimensión)
prop_lda <- mod_lda$svd^2 / sum(mod_lda$svd^2)
cat("proporción de varianza explicada por dimensión:\n")
names(prop_lda) <- paste0("LD", 1:length(prop_lda))
print(round(prop_lda, 4))

cat("\n--------------------------------------------------------")
cat("\n contribución de variables (coeficientes estandarizados)")
cat("\n--------------------------------------------------------\n")

sd_v <- apply(datos[, deps], 2, sd)
coef_est <- as.data.frame(mod_lda$scaling * sd_v)
print(round(coef_est, 4))

# 2. bucle de gráficos de importancia
for (d in 1:ncol(coef_est)) {
  dim_name <- colnames(coef_est)[d]
  df_bar <- data.frame(var = rownames(coef_est), val = coef_est[,d])
  
  p1 <- ggplot(df_bar, aes(x = reorder(var, abs(val)), y = val, fill = val > 0)) +
    geom_col(width = 0.7) +
    coord_flip() +
    scale_fill_manual(values = c("#d9534f", "#5bc0de"), labels = c("Negativa", "Positiva")) +
    labs(title = paste("importancia relativa en", dim_name),
         subtitle = paste0("explica el ", round(prop_lda[d]*100, 1), "% de la separación"),
         x = NULL, y = "coeficiente estandarizado", fill = "influencia") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p1)
}

# 3. biplot de separación de grupos
lda_dat <- data.frame(Tratamiento = datos[[f_principal]], predict(mod_lda)$x)

p2 <- ggplot(lda_dat, aes(x = LD1, y = LD2, color = Tratamiento, shape = Tratamiento)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(level = 0.95, linetype = 2) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "biplot discriminante: separación de tratamientos",
       subtitle = "espacio definido por las dos funciones principales (LD1 y LD2)",
       x = paste0("LD1 (", round(prop_lda[1]*100, 1), "%)"),
       y = paste0("LD2 (", round(prop_lda[2]*100, 1), "%)")) +
  theme_classic()

print(p2)



######### Detectar el mejor trtamiento a nivel global ####################

# Distancia de Mahalanobis

# Cálculo de centroides (medias multivariadas)
centroides <- aggregate(cbind(TCH, Rendimiento.Kg.Ton, TAH, Pureza) ~ Tratamiento, 
                        data = datos, mean)

# Calcular la distancia de Mahalanobis respecto al Testigo
matriz_cov <- cov(datos[, deps]) # Matriz de covarianza global
testigo_media <- as.numeric(centroides[centroides$Tratamiento == "Testigo", -1])

centroides$distancia_al_testigo <- apply(centroides[, -1], 1, function(x) {
  mahalanobis(x, testigo_media, matriz_cov)
})

cat("\n--- distancia de mahalanobis al testigo ---\n")
print(centroides[, c("Tratamiento", "distancia_al_testigo")])



# Análisis de conglomerados

datos_scale <- scale(centroides[, -1])
rownames(datos_scale) <- centroides$Tratamiento

# Crear el dendrograma
dist_euc <- dist(datos_scale)
hc <- hclust(dist_euc, method = "ward.D2")

# Gráfico profesional
plot(hc, main = "agrupamiento jerárquico de tratamientos", 
     sub = "basado en todas las variables dependientes",
     xlab = "tratamientos", ylab = "distancia euclidiana")



### Análisis de perfiles multivariados

# 1. Preparar datos: Medias escaladas (Z-scores) para que sean comparables
perfiles <- datos %>%
  group_by(Tratamiento) %>%
  summarise(across(all_of(deps), mean)) %>%
  pivot_longer(cols = -Tratamiento, names_to = "Variable", values_to = "Media") %>%
  group_by(Variable) %>%
  mutate(Media_Z = (Media - mean(Media)) / sd(Media)) # Estandarización

# 2. Gráfico de Perfiles (Huella Dactilar)
ggplot(perfiles, aes(x = Variable, y = Media_Z, group = Tratamiento, color = Tratamiento)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "perfiles multivariados por tratamiento",
       subtitle = "valores por encima de 0 son superiores al promedio general",
       y = "desviaciones estándar (z-score)",
       x = "variables analizadas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


######## Prueba post-hoc multivariante

# --- librerías ---
library(Hotelling)
library(dplyr)
library(ggplot2)
library(ggpubr)

alpha_nivel <- 0.15

# --- 1. comparaciones multivariantes (pillai) ---
cat("\n=== comparaciones post-hoc (pillai) alpha: 0.15 ===\n")

pares <- combn(levels(datos[[f_principal]]), 2, simplify = FALSE)
res_pillai <- do.call(rbind, lapply(pares, function(p) {
  sub_df <- datos[datos[[f_principal]] %in% p, ]
  f_sub  <- as.formula(paste("cbind(", paste(deps, collapse = ","), ") ~", f_principal))
  p_val  <- summary(manova(f_sub, data = sub_df), test = "Pillai")$stats[1, 6]
  data.frame(comparacion = paste(p[1], "vs", p[2]), p_val = p_val)
})) %>% mutate(sig = ifelse(p_val <= alpha_nivel, "*", "ns"))

print(res_pillai)

# --- 2. ranking por distancia de mahalanobis ---
cat("\n=== ranking global (distancia de mahalanobis) ===\n")

ranking <- centroides %>%
  select(Tratamiento, distancia_al_testigo) %>%
  arrange(desc(distancia_al_testigo)) %>%
  mutate(diferencia_sig = ifelse(Tratamiento == "Testigo", "-", 
                                 ifelse(distancia_al_testigo > 1.5, "alta", "baja")))

print(ranking)

# --- 3. gráfico de ranking definitivo ---
# este gráfico combina la distancia (cuánto mejoró) con el p-valor (confianza)
gg_ranking <- ggplot(ranking, aes(x = reorder(Tratamiento, distancia_al_testigo), 
                                  y = distancia_al_testigo, fill = distancia_al_testigo)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_gradient(low = "gray80", high = "steelblue") +
  labs(title = "ranking de superioridad multivariante",
       subtitle = paste("distancia de mahalanobis al testigo (alfa =", alpha_nivel, ")"),
       x = "tratamientos", y = "distancia estadística (magnitud del efecto)") +
  theme_classic2() +
  geom_text(aes(label = round(distancia_al_testigo, 2)), hjust = 1.5, color = "white", fontface = "bold")

print(gg_ranking)


# --- 3. Gráfico de Ranking Definitivo ---
# Limpiamos nombres para el gráfico
ranking_plot <- ranking %>%
  mutate(Etiqueta = ifelse(distancia_al_testigo > 0, 
                           paste0(round(distancia_al_testigo, 2), " (Sig)"), "Control"))

gg_final <- ggplot(ranking_plot, aes(x = reorder(Tratamiento, distancia_al_testigo), 
                                     y = distancia_al_testigo, 
                                     fill = distancia_al_testigo)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  geom_text(aes(label = Etiqueta), hjust = -0.2, size = 4.5, fontface = "bold") +
  coord_flip() +
  scale_fill_gradient(low = "#edf8b1", high = "#2c7fb8") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  labs(
    title = "Jerarquía de Superioridad Multivariante",
    subtitle = "Basado en Distancia de Mahalanobis (Significancia 15%)",
    x = "Tratamiento",
    y = "Magnitud de la Diferencia respecto al Testigo"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text = element_text(color = "black", face = "bold")
  )

print(gg_final)



####################### Análisis de componentes principales -ACP-#################


vars_acp <- c("TCH", "Rendimiento", "TAH", "Pureza", "Población", "Tallos.con.Flor", "Porcentaje", "Altura.Total", 
              "Peso.Total", "Altura.sin.Corcho", "Peso.sin.Corcho", "Entrenudos.totales.por.tallo", "Entrenudos.con.corcho.por.tallo",
              "Grado.de.corcho")

factor_grupo <- "Tratamiento" 
mostrar_puntos_fondo <- TRUE  # TRUE: muestra observaciones | FALSE: solo centroides y flechas


library(FactoMineR)
library(factoextra)
library(dplyr)
library(stringr)

# --- PREPARACIÓN DE DATOS ---
df_acp <- datos %>%
  select(all_of(factor_grupo), all_of(vars_acp)) %>%
  na.omit()

# --- EJECUCIÓN DEL ACP ---
# Usamos el factor como variable suplementaria (index 1)
res.pca <- PCA(df_acp, quali.sup = 1, graph = FALSE)

# --- GENERACIÓN DEL BIPLOT POR CUADRANTES ---
fviz_pca_biplot(res.pca, 
                # 1. Configuración de Individuos (Fondo)
                geom.ind = if(mostrar_puntos_fondo) "point" else "none",
                col.ind = "gray85",      # Color neutro para no distraer
                alpha.ind = 0.4,         # Transparencia
                pointshape = 19, 
                
                # 2. Configuración de Tratamientos (Centroides)
                habillage = 1,           # Colorear por Tratamiento
                addEllipses = TRUE,      # Elipses para ver agrupamiento por cuadrante
                ellipse.level = 0.95,
                mean.point = TRUE,       # Resalta el punto medio del tratamiento
                mean.point.size = 5,
                
                # 3. Configuración de Variables (Flechas)
                col.var = "firebrick3",  # Color contrastante para las flechas
                arrowsize = 0.7,
                
                # 4. Ajustes de Texto y Repelado
                repel = TRUE,            # CRUCIAL: evita que nombres de variables y tratamientos choquen
                labelsize = 4) + 
  
  # Estética de Cuadrantes
  geom_vline(xintercept = 0, linetype = "dotted", color = "black", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", alpha = 0.5) +
  
  theme_minimal() +
  labs(title = "Análisis Biplot: Distribución de Tratamientos y Variables",
       subtitle = "Ubicación por cuadrantes según componentes principales",
       x = paste0("PC1 (", round(res.pca$eig[1,2], 1), "%)"),
       y = paste0("PC2 (", round(res.pca$eig[2,2], 1), "%)"),
       colour = "Tratamiento", fill = "Tratamiento") +
  theme(legend.position = "bottom",
        text = element_text(size = 12)) +
  # Opcional: Acorta nombres de tratamientos en la leyenda si son muy largos
  guides(colour = guide_legend(ncol = 3))



###################################### MLM (Poblacion como factor de efecto aleatorio, la pendiente se considerará fija y el intercepto aleatorio)
modelo_mixto <- lmer(y ~ trat + rep + (1 | Pob))
anova(modelo_mixto)
VarCorr(modelo_mixto)
summary(modelo_mixto)

#Ajuste de medias para hacer PMM
medias <- emmeans(modelo_mixto,~trat);medias
tukey <- pairs(medias, adjust = "tukey"); tukey #Comparación por pares
medias_data.frame <- as.data.frame(medias); attach(medias_data.frame); medias_data.frame


#Verificación de los supuestos
# Normalidad
qqnorm(resid(modelo_mixto), col="red", pch=16, cex=1.3)
qqline(resid(modelo_mixto), lwd=2); par(mar=c(5,6,6,5), cex=0.7)
shapiro.test(resid(modelo_mixto))

# Homocedasticidad
plot(fitted(modelo_mixto), resid(modelo_mixto),
     xlab = "Valores ajustados", ylab = "Residuos")
abline(h = 0, col = "red", lwd=2.5)

leveneTest(resid(modelo_mixto) ~ datos2$trat, center="mean")


################################################################################################################################################
# Otra forma:


# Ejemplo
efectos_fijos <- c("Tratamiento")
efectos_aleatorios <- c("(1|Replica)")

factores <- c("Tratamiento", "Replica")
for(f in factores){
  if(f %in% colnames(datos)) datos[[f]] <- factor(datos[[f]])}

library(lme4)

# Fórmula manual
formula <- TCH ~ Tratamiento + (1|Replica)

# Ajustar modelo
modelo <- lmer(formula, data = datos, REML = TRUE)

# Resumen y ANOVA
summary(modelo)
anova(modelo)


library(DHARMa)

sim_res <- simulateResiduals(modelo)
plotResiduals(sim_res, form = datos$Tratamiento)  # Residuos por tratamiento
plot(sim_res)  # Gráficos generales

# Shapiro-Wilk
shapiro.test(residuals(sim_res))

# Residuos vs ajustados
library(ggplot2)
fitted_vals <- fitted(modelo)
resids <- resid(modelo)
ggplot(data.frame(Fitted=fitted_vals, Resid=resids), aes(x=Fitted, y=Resid)) +
  geom_point() +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  labs(title="Residuos vs Ajustados")


library(emmeans)
library(multcomp)

# Medias ajustadas
emmeans_var <- emmeans(modelo, ~ Tratamiento)

# Comparaciones Tukey
tukey <- glht(modelo, linfct = mcp(Tratamiento = "Tukey"))
summary(tukey)

# Agrupamiento con letras
letras <- cld(emmeans_var, Letters=letters, adjust="tukey")
print(letras)




resumen <- as.data.frame(letras)

ggplot(resumen, aes(x=Tratamiento, y=emmean, fill=Tratamiento)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=.group, y=emmean + 0.05*max(emmean)), vjust=0) +
  theme_minimal() +
  labs(title="Medias ajustadas con letras Tukey", y="Media ajustada")



ggplot(resumen, aes(x=emmean, y=reorder(Tratamiento, emmean))) +
  geom_point(size=3) +
  geom_errorbarh(aes(xmin=emmean-SE, xmax=emmean+SE), height=0, size=1.2) +
  theme_minimal() +
  labs(title="Medias ± SE", x="Media", y="Tratamiento")


# Boxplot
ggplot(datos, aes(x=Tratamiento, y=TCH, fill=Tratamiento)) +
  geom_boxplot() +
  theme_minimal()

# Histograma
ggplot(datos, aes(x=TCH)) +
  geom_histogram(bins=15, fill="steelblue", color="black", alpha=0.7) +
  theme_minimal()

# En la parte de modelos lineales mixtos, quiero que incluyas la opcion de colocar anidamientos, interacciones
# Quiero que tambien incluyas la opcion de corregir la heterocedasticidad por varldent y cualquier otro metodo


#############################################################
############# MLG para analisis de datos de conteo y porcentajes

# 1. MODELO BINOMIAL NEGATIVA
attach(datos)
modelo_nb <- glm.nb(datos$ind_m2 ~ datos$Tratamiento + datos$Replica, data = datos)
summary(modelo_nb)
Anova(modelo_nb)

sim_res_nb <- simulateResiduals(fittedModel = modelo_nb, n = 1000)
plot(sim_res_nb)
testDispersion(sim_res_nb)

ems_nb_trat <- emmeans(modelo_nb, ~ Tratamiento, type = "response")
pairs_nb_trat <- pairs(ems_nb_trat, adjust = "tukey")
print(ems_nb_trat)
print(pairs_nb_trat)




# Cargar librerías necesarias
library(emmeans)
library(multcomp)
library(ggplot2)
library(dplyr)

# 1. Generar el Compact Letter Display
cld_nb_trat <- cld(ems_nb_trat,
                   alpha = 0.15,
                   Letters = LETTERS,
                   adjust = "tukey")

# 2. Convertir el resultado a un data frame y preparar la columna de etiquetas
cld_df <- as.data.frame(cld_nb_trat)

# NOTA: Imprime los nombres de las columnas para verificar, si el error persiste:
# print(names(cld_df)) 

# Crear la columna de etiquetas
cld_df <- cld_df %>%
  mutate(etiqueta = .group)

# 3. Creación del Gráfico de Líneas de Error, USANDO NOMBRES CORRECTOS:
grafico_tukey_nb <- ggplot(cld_df, aes(x = Tratamiento, y = response)) + # <--- ¡CAMBIO CLAVE AQUÍ!
  
  # Bigotes (Intervalos de Confianza)
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.2,
                linewidth = 0.8,
                color = "gray40") +
  
  # Puntos (Medias Estimadas)
  geom_point(size = 4, color = "red") +
  
  # Etiquetas de Letras de Tukey (colocadas por encima del bigote superior)
  geom_text(aes(label = etiqueta, y = asymp.UCL),
            vjust = -0.5,
            size = 5,
            color = "black",
            fontface = "bold") +
  
  # Ajustes estéticos y títulos
  labs(title = "Medias Estimadas por Tratamiento con Letras de Tukey",
       subtitle = "Basado en el Modelo GLM Negativo Binomial",
       x = "Tratamiento",
       y = "Media Estimada de la Respuesta (con IC al 95%)") +
  
  # Tema
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# 4. Imprimir el gráfico
print(grafico_tukey_nb)


# 2. MODELO QUASI-POISSON
modelo_qp <- glm(conteo ~ Tratamiento + Replica, 
                 family = quasipoisson(link = "log"), 
                 data = datos)
summary(modelo_qp)

sim_res_qp <- simulateResiduals(fittedModel = modelo_qp, n = 1000)
plot(sim_res_qp)

ems_qp_trat <- emmeans(modelo_qp, ~ Tratamiento, type = "response")
pairs_qp_trat <- pairs(ems_qp_trat, adjust = "tukey")
print(ems_qp_trat)
print(pairs_qp_trat)


# 3. MODELO QUASI-BINOMIAL (para porcentajes 0-100%)
# Convertir porcentaje a proporción
datos$prop_porcentaje <- datos$porcentaje / 100

modelo_qb <- glm(prop_porcentaje ~ Tratamiento + Replica,
                 family = quasibinomial(link = "logit"),
                 data = datos,
                 weights = rep(100, nrow(datos)))
summary(modelo_qb)

sim_res_qb <- simulateResiduals(fittedModel = modelo_qb, n = 1000)
plot(sim_res_qb)
testDispersion(sim_res_qb)

ems_qb_trat <- emmeans(modelo_qb, ~ Tratamiento, type = "response")
ems_qb_porcentaje <- ems_qb_trat
ems_qb_porcentaje@grid$emmean <- ems_qb_porcentaje@grid$emmean * 100
pairs_qb_trat <- pairs(ems_qb_trat, adjust = "tukey")

print(ems_qb_porcentaje)
print(pairs_qb_trat)

# 4. MODELO BETA REGRESSION (para porcentajes 0-100%)
# Convertir y ajustar límites
datos$prop_beta <- datos$porcentaje / 100
epsilon <- 0.001
datos$prop_beta[datos$prop_beta == 0] <- epsilon
datos$prop_beta[datos$prop_beta == 1] <- 1 - epsilon

modelo_beta <- betareg(prop_beta ~ Tratamiento + Replica, data = datos)
summary(modelo_beta)

residuos_beta <- residuals(modelo_beta, type = "deviance")
fitted_beta <- fitted(modelo_beta)

par(mfrow = c(2, 2))
plot(fitted_beta, residuos_beta)
abline(h = 0, col = "red")
qqnorm(residuos_beta)
qqline(residuos_beta)
hist(residuos_beta)
plot(density(residuos_beta))
par(mfrow = c(1, 1))

ems_beta_trat <- emmeans(modelo_beta, ~ Tratamiento, type = "response")
ems_beta_porcentaje <- ems_beta_trat
ems_beta_porcentaje@grid$emmean <- ems_beta_porcentaje@grid$emmean * 100
pairs_beta_trat <- pairs(ems_beta_trat, adjust = "tukey")

print(ems_beta_porcentaje)
print(pairs_beta_trat)

# RESUMEN COMPARATIVO
cat("BINOMIAL NEGATIVA - Medias:\n")
print(ems_nb_trat)
cat("Comparaciones:\n")
print(pairs_nb_trat)

cat("QUASI-POISSON - Medias:\n")
print(ems_qp_trat)
cat("Comparaciones:\n")
print(pairs_qp_trat)

cat("QUASI-BINOMIAL - Medias (%):\n")
print(ems_qb_porcentaje)
cat("Comparaciones:\n")
print(pairs_qb_trat)

cat("BETA REGRESSION - Medias (%):\n")
print(ems_beta_porcentaje)
cat("Comparaciones:\n")
print(pairs_beta_trat)

# GRÁFICO COMPARATIVO
resultados_grafico <- data.frame(
  Modelo = rep(c("Binomial Neg", "Quasi-Poisson", "Quasi-Binomial", "Beta"), 
               each = nrow(summary(ems_nb_trat))),
  Tratamiento = rep(summary(ems_nb_trat)$Tratamiento, 4),
  Media = c(
    summary(ems_nb_trat)$emmean,
    summary(ems_qp_trat)$emmean,
    summary(ems_qb_porcentaje)$emmean,
    summary(ems_beta_porcentaje)$emmean
  )
)

ggplot(resultados_grafico, aes(x = Tratamiento, y = Media, fill = Modelo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparación de medias ajustadas por modelo", y = "Media ajustada") +
  theme_minimal()

# 5. Modelo Poisson 

library(ggplot2)
library(emmeans)
library(car)

# 1. Ajuste del Modelo Poisson
# Reemplaza 'variable_conteo' por tu variable dependiente (debe ser entera >= 0)
modelo_poisson <- glm(tallos_ha ~ Tratamiento, 
                      data = datos, 
                      family = poisson(link = "log"))

# 2. Resumen del modelo
cat("\n--- Resumen del Modelo Poisson ---\n")
print(summary(modelo_poisson))

# 3. Prueba de Significación Global (Análisis de Desviación)
cat("\n--- Análisis de Desviación (ANOVA para GLM) ---\n")
print(Anova(modelo_poisson, type = "III"))

# 4. Verificación de Sobredispersión
# Si el cociente (Residual deviance / df) es mucho mayor a 1, hay sobredispersión
phi <- summary(modelo_poisson)$deviance / summary(modelo_poisson)$df.residual
cat("\nFactor de Dispersión Estimado:", phi, "\n")

if(phi > 1.5) {
  cat("Aviso: Hay sobredispersión detectada. Se recomienda usar family = quasipoisson.\n")
}

# 5. Comparaciones Múltiples (Medias Estimadas)
# Los resultados se devuelven en la escala original (type = 'response')
emm_poisson <- emmeans(modelo_poisson, ~ Tratamiento, type = "response")
cld_poisson <- as.data.frame(pairs(emm_poisson))

cat("\n--- Comparaciones de Medias (Ratios) ---\n")
print(emm_poisson)

# 6. Gráfico de Diagnóstico de Residuos
par(mfrow = c(2, 2))
plot(modelo_poisson)
par(mfrow = c(1, 1))

# 7. Gráfico de Resultados
ggplot(as.data.frame(emm_poisson), aes(x = Tratamiento, y = rate)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  geom_point(size = 3, color = "darkblue") +
  labs(title = "Medias Estimadas - Distribución Poisson",
       subtitle = "Intervalos de confianza al 95%",
       y = "Conteo Estimado (Rate)",
       x = "Tratamiento") +
  theme_minimal()







datos_prop <- datos
for (v in config$variables) {
  if (max(datos_prop[[v]], na.rm = TRUE) > 1) {
    datos_prop[[v]] <- datos_prop[[v]] / 100
  }
}

resultados_modelos <- list()

for (variable in config$variables) {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat(" ANALIZANDO VARIABLE:", variable, "\n")
  
  for (familia in config$familias) {
    cat("\n>>> Modelo:", familia, "\n")
    
    formula_f <- as.formula(paste(variable, "~", paste(config$terminos, collapse = " + ")))
    
    modelo <- tryCatch({
      if (familia == "beta") {
        y <- datos_prop[[variable]]
        if (config$ajuste_beta) {
          n <- length(y)
          # Transformación a proporción
          y_mod <- (y * (n - 1) + 0.5) / n
          d_temp <- datos_prop; d_temp[[variable]] <- y_mod
          betareg(formula_f, data = d_temp)
        } else {
          betareg(formula_f, data = datos_prop)
        }
      } else {
        glm(formula_f, data = datos_prop, family = quasibinomial(link = "logit"))
      }
    }, error = function(e) {
      cat("  Error al ajustar el modelo:", e$message, "\n")
      return(NULL)
    })
    
    if (is.null(modelo)) next
    
    nombre_mod <- paste0(variable, "_", familia)
    resultados_modelos[[nombre_mod]] <- modelo
    
    # Diagnósticos
    ratio <- if(familia == "quasibinomial") summary(modelo)$dispersion else (1/mean(predict(modelo, type="precision")))
    em_res <- as.data.frame(emmeans(modelo, ~ Tratamiento))
    
    cat("  Ratio Dispersión:", round(ratio, 5), "\n")
    cat("  SE promedio:", round(mean(em_res$SE), 6), "\n")
    
    if (familia == "quasibinomial") {
      print(Anova(modelo, type = "II", test = config$test_quasi))
    } else {
      print(Anova(modelo, type = "II", test = "Chisq"))
    }
  }
}

# Post-ANDEVA

for (nombre in names(resultados_modelos)) {
  m <- resultados_modelos[[nombre]]
  info <- unlist(strsplit(nombre, "_"))
  v_name <- info[1]; f_name <- info[length(info)]
  
  em <- emmeans(m, ~ Tratamiento, type = "response")
  
  letras_df <- tryCatch({
    cld(em, Letters = letters, adjust = "tukey", reversed = TRUE) %>% as.data.frame()
  }, error = function(e) return(NULL))
  
  if (is.null(letras_df)) next
  
  col_y   <- intersect(names(letras_df), c("response", "prob", "emmean"))[1]
  col_inf <- intersect(names(letras_df), c("asymp.LCL", "lower.CL"))[1]
  col_sup <- intersect(names(letras_df), c("asymp.UCL", "upper.CL"))[1]
  
  df_plot <- letras_df %>%
    rename(media = !!col_y, lcl = !!col_inf, ucl = !!col_sup, letras = .group) %>%
    mutate(letras = trimws(letras))
  
  p <- ggplot(df_plot, aes(x = Tratamiento, y = media, group = 1)) +
    geom_line(color = "grey70", linetype = "dashed") +
    geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.1) +
    geom_point(size = 4, color = ifelse(f_name == "beta", "blue4", "red4")) +
    geom_text(aes(label = letras, y = ucl), vjust = -1, fontface = "bold", size = 5) +
    scale_y_continuous(labels = scales::percent_format(), 
                       limits = c(0, max(df_plot$ucl) * 1.3)) +
    labs(title = paste("Variable:", v_name),
         subtitle = paste("Modelo:", f_name, "| Test Quasi:", 
                          if(f_name=="quasibinomial") config$test_quasi else "Chisq"),
         y = "Cobertura Estimada (%)", x = "Tratamiento") +
    theme_classic(base_size = 12)
  
  print(p)
}




################### con MLM con distribución beta regresión ###################

library(glmmTMB)
library(car)
library(emmeans)
library(ggplot2)
library(dplyr)
library(multcomp)
library(multcompView)
library(DHARMa)


config <- list(
  variables = c("Eficiencia.Hoja.Ancha", "Eficiencia.Graminea", "Eficiencia.Ciperaceas"),
  fijo      = "Tratamiento",
  aleatorio = "(1|Replica)", 
  epsilon   = 0.0001 
)


datos_mlm <- datos
resultados_mlm <- list()

for (v in config$variables) {
  cat("\n", paste(rep("=", 70), collapse = ""), "\n")
  cat(" ANALIZANDO VARIABLE (MLM):", v, "\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  
  y <- datos_mlm[[v]]
  if(max(y, na.rm=T) > 1) y <- y / 100
  
  y[y >= 1] <- 1 - config$epsilon
  y[y <= 0] <- 0 + config$epsilon
  v_prop_name <- paste0(v, "_prop")
  datos_mlm[[v_prop_name]] <- y
  
  formula_mlm <- as.formula(paste0(v_prop_name, " ~ ", config$fijo, " + ", config$aleatorio))
  
  modelo <- tryCatch({
    glmmTMB(formula_mlm, data = datos_mlm, family = beta_family(link = "logit"))
  }, error = function(e) {
    cat("  Error al ajustar MLM:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(modelo)) next
  resultados_mlm[[v]] <- modelo
  
  cat("\n>>> DIAGNÓSTICO DE RESIDUOS (DHARMa):\n")
  residuos <- simulateResiduals(fittedModel = modelo, n = 250)
  
  print(testResiduals(residuos))
  
  
  plot(residuos)
  
  cat("\n>>> ANOVA (Type II Wald Chi-square):\n")
  print(Anova(modelo, type = "II"))
  
  em_res <- emmeans(modelo, ~ Tratamiento, type = "response")
  letras_df <- cld(em_res, Letters = letters, adjust = "tukey", reversed = TRUE) %>% 
    as.data.frame()
  
  col_media <- intersect(names(letras_df), c("response", "prob", "emmean"))[1]
  col_inf   <- intersect(names(letras_df), c("lower.CL", "asymp.LCL", "lower"))[1]
  col_sup   <- intersect(names(letras_df), c("upper.CL", "asymp.UCL", "upper"))[1]
  
  letras_df <- letras_df %>%
    rename(media = !!col_media, lcl = !!col_inf, ucl = !!col_sup) %>%
    mutate(.group = trimws(.group))
  
  cat("\n>>> ESTIMACIONES Y PRECISIÓN:\n")
  print(letras_df %>% select(Tratamiento, media, SE, lcl, ucl))
  cat("\n  * SE Promedio:", round(mean(letras_df$SE), 6), "\n")
  
  p <- ggplot(letras_df, aes(x = Tratamiento, y = media)) +
    geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.1, color = "red", linewidth = 0.7) +
    geom_point(size = 4, color = "red") +
    geom_text(aes(label = .group, y = ucl), vjust = -1.2, fontface = "bold", size = 5) +
    scale_y_continuous(labels = scales::percent_format(), 
                       limits = c(0, min(1.1, max(letras_df$ucl) * 1.3))) +
    labs(title = paste("Efecto en:", v),
         subtitle = "Modelo Mixto (GLMM Beta) | Réplica como Efecto Aleatorio",
         caption = "Diferencias según prueba de Tukey (p < 0.05)",
         y = "Control Estimado (%)", x = "Tratamiento") +
    theme_minimal(base_size = 14)
  
  print(p)
}

############################ Análisis de datos porcentuales con transformación arcoseno y box-cox 
# DBCA

library(MASS)
library(agricolae)
library(ggplot2)
library(dplyr)

# --- 1. CONFIGURACIÓN INICIAL ---
var_dep      <- "TCH"  
f_trat       <- "Resiembra" 
f_bloque     <- "Replica"     

metodo_trans <- "boxcox"    
prueba_post  <- "LSD"         # "Tukey" o "LSD"
letras_altas <- T          # TRUE: 'a' al mayor | FALSE: 'a' al menor

vars_analisis <- c(var_dep) 

# --- 2. TRANSFORMACIÓN DE DATOS ---
datos[[f_trat]]   <- as.factor(datos[[f_trat]])
datos[[f_bloque]] <- as.factor(datos[[f_bloque]])

if (metodo_trans == "arcoseno") {
  datos$y_trans <- asin(sqrt(datos[[var_dep]] / 100))
  message("Transformación Arcoseno aplicada.")
} else {
  y_temp <- datos[[var_dep]]
  if(any(y_temp <= 0)) y_temp <- y_temp + 0.5
  bc <- boxcox(y_temp ~ datos[[f_bloque]] + datos[[f_trat]], plotit = FALSE)
  lambda <- bc$x[which.max(bc$y)]
  datos$y_trans <- if(lambda == 0) log(y_temp) else (y_temp^lambda - 1) / lambda
  message(paste("Transformación Box-Cox aplicada. Lambda:", round(lambda, 4)))
}

# --- 3. ANDEVA Y PRUEBA DE MEDIAS (MOSTRAR EN CONSOLA) ---
formula_mod   <- as.formula(paste("y_trans ~", f_bloque, "+", f_trat))
modelo        <- aov(formula_mod, data = datos)
titulo_prueba <- ifelse(prueba_post == "Tukey", "Prueba de Tukey (HSD)", "Prueba de Fisher (LSD)")

cat("\n==============================================================\n")
cat("                RESULTADOS DEL ANÁLISIS (ANDEVA)              \n")
cat("==============================================================\n")
print(summary(modelo)) # <--- MUESTRA EL ANDEVA EN CONSOLA

cat("\n==============================================================\n")
cat("             ", titulo_prueba, "             \n")
cat("==============================================================\n")

if (prueba_post == "Tukey") {
  test_post <- HSD.test(modelo, f_trat, group = TRUE, console = TRUE, decreasing = letras_altas)
} else {
  # Para LSD, el orden de letras se maneja internamente en agricolae
  # Si letras_altas es FALSE, invertimos el orden de las medias para que 'a' sea el menor
  test_post <- LSD.test(modelo, f_trat, group = TRUE, console = TRUE)
  
  if (!letras_altas) {
    # Re-ejecutamos con lógica de orden inverso si se solicita
    test_post <- LSD.test(modelo, f_trat, group = TRUE, console = FALSE)
    # Reordenamos manualmente para que 'a' sea el valor más bajo
    test_post$groups <- test_post$groups[order(test_post$groups$y_trans), ]
  }
}

# --- 4. DIAGNÓSTICO DE SUPUESTOS ---
cat("\n--- DIAGNÓSTICO DE RESIDUOS ---\n")
print(shapiro.test(residuals(modelo)))
par(mfrow = c(1, 2))
plot(modelo, which = 1) # Homocedasticidad
plot(modelo, which = 2) # Normalidad

# --- 5. PREPARACIÓN PARA EL GRÁFICO (DATOS REALES) ---
df_resumen <- datos %>%
  group_by(!!sym(f_trat)) %>%
  summarise(
    media_r = mean(!!sym(var_dep), na.rm = TRUE),
    sd_r    = sd(!!sym(var_dep), na.rm = TRUE),
    n       = n()
  ) %>%
  mutate(se_r = sd_r / sqrt(n), Variable = var_dep)

labels_df <- test_post$groups
labels_df[[f_trat]] <- rownames(labels_df)
datos_final_grafico <- df_resumen %>% 
  left_join(labels_df[, c(f_trat, "groups")], by = f_trat)

# --- 6. GRÁFICO CON ESTÉTICA UNIFICADA ---



grupos_graficos <- split(vars_analisis, ceiling(seq_along(vars_analisis) / 4))

for (i in seq_along(grupos_graficos)) {
  datos_plot <- datos_final_grafico %>%
    filter(Variable %in% grupos_graficos[[i]]) %>%
    mutate(Variable = factor(Variable, levels = grupos_graficos[[i]]))
  
  if(nrow(datos_plot) == 0) next
  
  gg <- ggplot(datos_plot, aes(y = reorder(!!sym(f_trat), media_r), x = media_r)) +
    geom_errorbarh(aes(xmin = media_r - se_r, xmax = media_r + se_r), height = 0.2) +
    geom_point(size = 3, color = "black") +
    geom_text(aes(label = sprintf("%.2f %s", media_r, trimws(groups))),
              hjust = -0.2, vjust = -1, size = 4.5) +
    
    facet_wrap(~ Variable, scales = "free_x") + 
    scale_x_continuous(expand = expansion(mult = c(0.1, 0.5))) +
    labs(x = "Media Aritmética Real ± SE", 
         y = "Tratamiento",
         title = paste("Comparación de Medias (", titulo_prueba, ") - Panel", i)) +
    theme_classic() +
    theme(strip.text = element_text(size = 13, face = "bold"),
          text = element_text(size = 12))
  
  print(gg)
}


# Da la posibilidad de seleccionar el tipo de distribucion segun sea el caso (para conteo o regresion), da la opcion de elegir si los porcentajes están como % o decimal para hacer los ajustes respectivos





