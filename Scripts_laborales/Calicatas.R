library(sp)
library(gstat)
library(raster)
library(ggplot2)
library(viridis)

datos<- read_clip_tbl()
names(datos)

# Sin dir_salida: fijamos el directorio de trabajo una vez
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Carpetas trabajos/Pantaleon/Proyectos/Calicatas_raices")

# Limpieza
datos_limpio <- datos[!is.na(datos$Den_raiz), ]
tratamientos <- unique(datos_limpio$Tratamiento)
tratamientos <- tratamientos[!is.na(tratamientos)]

for (tr in tratamientos) {
  cat("\n=== Procesando:", tr, "===\n")
  
  dat <- datos_limpio[datos_limpio$Tratamiento == tr, ]
  if (nrow(dat) < 10) { cat("  Muy pocos puntos, se omite.\n"); next }
  
  coordinates(dat) <- ~X+Y
  
  # Grilla a 2 cm (más suave que 1 cm, menos "pixelado")
  grd <- expand.grid(
    X = seq(-50, 50, by = 2),
    Y = seq(20, 100, by = 2)
  )
  coordinates(grd) <- ~X+Y
  gridded(grd) <- TRUE
  
  # IDW
  idw_res <- idw(Den_raiz ~ 1, dat, newdata = grd, idp = 2.0)
  df_r <- as.data.frame(raster(idw_res), xy = TRUE)
  
  # Puntos muestreados como data.frame (para superponerlos)
  pts <- as.data.frame(dat)[, c("X", "Y", "Den_raiz")]
  
  p <- ggplot() +
    # interpolate = TRUE suaviza visualmente las celdas al renderizar
    geom_raster(data = df_r, aes(x = x, y = y, fill = var1.pred),
                interpolate = TRUE) +
    # Escala CONTINUA con transformación log1p (comprime la cola derecha)
    scale_fill_viridis_c(
      option = "D",
      name = "Densidad de raíces\n(raíces por muestra)",
      trans  = "log1p",
      breaks = c(0, 1, 2, 5, 10, 19),
      labels = c("0", "1", "2", "5", "10", "19"),
      na.value = "grey90"
    ) +
    # Puntos muestreados encima (blancos, sin leyenda propia)
    geom_point(data = pts, aes(x = X, y = Y),
               color = "white", size = 1, alpha = 0.7) +
    coord_fixed() +
    scale_y_reverse(breaks = seq(20, 100, by = 20)) +
    scale_x_continuous(breaks = seq(-50, 50, by = 10)) +
    geom_hline(yintercept = 50, color = "white", linetype = "dashed", alpha = 0.6) +
    geom_vline(xintercept = 0,  color = "white", linetype = "dashed", alpha = 0.6) +
    labs(
      title = paste("Densidad de raíces —", tr),
      x = "Distancia horizontal (cm)",
      y = "Profundidad (cm)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title   = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title   = element_text(size = 14),
      axis.text    = element_text(size = 12),
      legend.title = element_text(size = 12, hjust = 0),
      legend.text  = element_text(size = 11),
      panel.grid   = element_blank()   # sin cuadrícula gris detrás del raster
    )
  
  print(p)
  ggsave(paste0("Mapa_densidad_raices_", gsub(" ", "_", tr), ".png"),
         plot = p, width = 8, height = 6, dpi = 300)
  cat("  Guardado.\n")
}


# =====================================================================
# 2. PANEL A: HISTOGRAMA DE DISTRIBUCIÓN
# =====================================================================
p_hist <- ggplot(datos_limpio, aes(x = Den_raiz)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "white") +
  scale_x_continuous(trans = "log1p", breaks = c(0, 1, 2, 5, 10, 19)) +
  facet_wrap(~ Tratamiento, scales = "free_y", ncol = 3) +
  labs(title = "A. Distribución global de densidades",
       x = "Densidad de raíces (escala logarítmica)", y = "Frecuencia") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16))


ggsave("Panel_A_Histogramas.png", plot = p_hist, width = 12, height = 7, dpi = 300)


# =====================================================================
# 3. PANEL B: PERFIL VERTICAL (Profundidad)
# =====================================================================
perfil_vert <- datos_limpio %>%
  mutate(Profundidad_bin = cut(Y, breaks = seq(0, 100, by = 10), include.lowest = TRUE)) %>%
  group_by(Tratamiento, Profundidad_bin) %>%
  summarise(Media = mean(Den_raiz), SE = sd(Den_raiz)/sqrt(n()), .groups = 'drop')

p_vert <- ggplot(perfil_vert, aes(x = Media, y = Profundidad_bin)) +
  geom_col(fill = "darkcyan", width = 0.7) +
  geom_errorbarh(aes(xmin = pmax(0, Media - SE), xmax = Media + SE), height = 0.2) +
  scale_y_discrete(limits = rev) +
  facet_wrap(~ Tratamiento, ncol = 3) +
  labs(title = "B. Perfil vertical de raíces",
       x = "Densidad media (raíces)", y = "Profundidad (cm)") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16))

ggsave("Panel_B_Perfil_Vertical.png", plot = p_vert, width = 12, height = 7, dpi = 300)


# =====================================================================
# 4. PANEL C: DISTRIBUCIÓN LATERAL (Distancia X)
# =====================================================================
perfil_lat <- datos_limpio %>%
  mutate(Distancia_bin = cut(X, breaks = seq(-60, 60, by = 20), include.lowest = TRUE)) %>%
  group_by(Tratamiento, Distancia_bin) %>%
  summarise(Media = mean(Den_raiz), SE = sd(Den_raiz)/sqrt(n()), .groups = 'drop')

p_lat <- ggplot(perfil_lat, aes(x = Distancia_bin, y = Media)) +
  geom_col(fill = "coral", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = pmax(0, Media - SE), ymax = Media + SE), width = 0.2) +
  facet_wrap(~ Tratamiento, ncol = 3) +
  labs(title = "C. Distribución lateral de raíces",
       x = "Distancia horizontal X (cm)", y = "Densidad media") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16))

ggsave("Panel_C_Perfil_Lateral.png", plot = p_lat, width = 12, height = 7, dpi = 300)



# Con curvas 


# =====================================================================
# PAQUETES NECESARIOS
# =====================================================================
library(sp)
library(gstat)
library(raster)
library(ggplot2)
library(viridis)
library(dplyr)
library(tidyr)
library(metR) # Para las etiquetas en las curvas a nivel

# Fijamos el directorio de trabajo
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Carpetas trabajos/Pantaleon/Proyectos/Calicatas_raices")

# Limpieza
datos_limpio <- datos[!is.na(datos$Den_raiz), ]
tratamientos <- unique(datos_limpio$Tratamiento)
tratamientos <- tratamientos[!is.na(tratamientos)]

# =====================================================================
# 1. MAPAS DE CURVAS A NIVEL (Estilo Superficie de Respuesta)
# =====================================================================
for (tr in tratamientos) {
  cat("\n=== Procesando:", tr, "===\n")
  
  dat <- datos_limpio[datos_limpio$Tratamiento == tr, ]
  if (nrow(dat) < 10) { cat("  Muy pocos puntos, se omite.\n"); next }
  
  coordinates(dat) <- ~X+Y
  
  # Grilla a 2 cm
  grd <- expand.grid(
    X = seq(-50, 50, by = 2),
    Y = seq(20, 100, by = 2)
  )
  coordinates(grd) <- ~X+Y
  gridded(grd) <- TRUE
  
  # IDW
  idw_res <- idw(Den_raiz ~ 1, dat, newdata = grd, idp = 2.0)
  df_r <- as.data.frame(raster(idw_res), xy = TRUE)
  
  # Definimos los cortes (niveles) para las curvas y rellenos
  # Al usar log1p, es mejor definir cortes que tengan sentido biológico
  cortes_nivel <- c(0, 1, 2, 3, 5, 8, 12, 20)
  
  p <- ggplot() +
    # 1. Relleno de las zonas con contornos (Superficie de respuesta)
    geom_contour_filled(data = df_r, 
                        aes(x = x, y = y, z = var1.pred), 
                        breaks = cortes_nivel) +
    
    # 2. Líneas de contorno negras para delimitar cada sector
    geom_contour(data = df_r, 
                 aes(x = x, y = y, z = var1.pred), 
                 breaks = cortes_nivel, 
                 color = "black", 
                 linewidth = 0.4, 
                 alpha = 0.6) +
    
    # 3. Etiquetas numéricas sobre las líneas (metR)
    geom_text_contour(data = df_r, 
                      aes(x = x, y = y, z = var1.pred), 
                      breaks = cortes_nivel, 
                      stroke = 0.2,      # Halo blanco alrededor del texto para que se lea
                      color = "black", 
                      size = 3.5, 
                      check_overlap = TRUE) +
    
    # Escala de colores discretos (porque usamos contornos rellenos)
    scale_fill_viridis_d(
      option = "D", 
      name = "Densidad de raíces\n(raíces)",
      labels = c("0 - 1", "1 - 2", "2 - 3", "3 - 5", "5 - 8", "8 - 12", "12 - 20")
    ) +
    
    coord_fixed() +
    scale_y_reverse(breaks = seq(20, 100, by = 20)) +
    scale_x_continuous(breaks = seq(-50, 50, by = 10)) +
    
    # Líneas de referencia base
    geom_hline(yintercept = 50, color = "black", linetype = "dashed", alpha = 0.4) +
    geom_vline(xintercept = 0,  color = "black", linetype = "dashed", alpha = 0.4) +
    
    labs(
      title = paste("Sectores de densidad de raíces —", tr),
      x = "Distancia horizontal (cm)",
      y = "Profundidad (cm)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title   = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title   = element_text(size = 14),
      axis.text    = element_text(size = 12),
      legend.title = element_text(size = 12, hjust = 0),
      legend.text  = element_text(size = 11),
      panel.grid   = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1) # Borde del gráfico
    )
  
  print(p)
  ggsave(paste0("Mapa_curvas_nivel_", gsub(" ", "_", tr), ".png"),
         plot = p, width = 8, height = 6, dpi = 300)
  cat("  Guardado.\n")
}




# GAM

# =====================================================================
# PAQUETES NECESARIOS
# =====================================================================
library(sp)
library(raster)
library(ggplot2)
library(viridis)
library(dplyr)
library(tidyr)
library(metR) 
library(mgcv) # Para los Splines Espaciales (Superficies suaves)

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Carpetas trabajos/Pantaleon/Proyectos/Calicatas_raices")

datos_limpio <- datos[!is.na(datos$Den_raiz), ]
tratamientos <- unique(datos_limpio$Tratamiento)
tratamientos <- tratamientos[!is.na(tratamientos)]

# =====================================================================
# 1. MAPAS DE CURVAS A NIVEL USANDO GAM (Splines Espaciales)
# =====================================================================
for (tr in tratamientos) {
  cat("\n=== Procesando:", tr, "===\n")
  
  dat <- datos_limpio[datos_limpio$Tratamiento == tr, ]
  if (nrow(dat) < 10) { cat("  Muy pocos puntos, se omite.\n"); next }
  
  # 1. Ajustamos el modelo GAM (Spline 2D)
  # La función s(X, Y) crea una superficie suave. 
  # Usamos k=5 o k=8 para controlar la suavidad (a menor k, más suave).
  modelo_gam <- gam(Den_raiz ~ s(X, Y, k = 8), data = dat)
  
  # 2. Creamos la grilla para predecir
  grd <- expand.grid(
    X = seq(-50, 50, by = 1),
    Y = seq(20, 100, by = 1)
  )
  
  # 3. Predecimos la densidad en toda la grilla
  grd$Den_pred <- predict(modelo_gam, newdata = grd, type = "response")
  
  # Evitamos predicciones negativas (el GAM a veces extrapola bajo cero)
  grd$Den_pred <- pmax(grd$Den_pred, 0)
  
  # 4. Definimos los cortes para las curvas (ajustados a tu escala)
  cortes_nivel <- c(0, 0.5, 1, 2, 3, 5, 8, 12, 20)
  
  p <- ggplot() +
    # Relleno de regiones
    geom_contour_filled(data = grd, 
                        aes(x = X, y = Y, z = Den_pred), 
                        breaks = cortes_nivel) +
    
    # Líneas de contorno negras
    geom_contour(data = grd, 
                 aes(x = X, y = Y, z = Den_pred), 
                 breaks = cortes_nivel, 
                 color = "black", 
                 linewidth = 0.4, 
                 alpha = 0.7) +
    
    # Números sobre las curvas
    geom_text_contour(data = grd, 
                      aes(x = X, y = Y, z = Den_pred), 
                      breaks = cortes_nivel, 
                      stroke = 0.2, 
                      color = "black", 
                      size = 3.5, 
                      check_overlap = TRUE) +
    
    # Escala de colores
    scale_fill_viridis_d(
      option = "D", 
      name = "Densidad de raíces\n(raíces / muestra)",
      labels = c("0 - 0.5", "0.5 - 1", "1 - 2", "2 - 3", "3 - 5", "5 - 8", "8 - 12", "12 - 20")
    ) +
    
    coord_fixed() +
    scale_y_reverse(breaks = seq(20, 100, by = 20)) +
    scale_x_continuous(breaks = seq(-50, 50, by = 10)) +
    geom_hline(yintercept = 50, color = "black", linetype = "dashed", alpha = 0.4) +
    geom_vline(xintercept = 0,  color = "black", linetype = "dashed", alpha = 0.4) +
    
    labs(
      title = paste("Sectores de densidad de raíces (GAM) —", tr),
      x = "Distancia horizontal (cm)",
      y = "Profundidad (cm)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title   = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title   = element_text(size = 14),
      axis.text    = element_text(size = 12),
      legend.title = element_text(size = 12, hjust = 0),
      legend.text  = element_text(size = 11),
      panel.grid   = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )
  
  print(p)
  ggsave(paste0("Mapa_GAM_", gsub(" ", "_", tr), ".png"),
         plot = p, width = 8, height = 6, dpi = 300)
  cat("  Guardado.\n")
}

