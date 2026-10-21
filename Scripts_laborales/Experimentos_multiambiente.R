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



datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")

names(datos)
View(datos)
attach(datos)

# Conversión de variables

cols_a_numerica <- intersect(names(datos), c("TCH", "Rendimiento", "TAH", "Pureza"))
vr<- c("TCH", "Rendimiento", "TAH", "Pureza")
cols_a_factor <- intersect(names(datos), c("Tratamiento", "Replica"))


datos <- datos %>%
  mutate(across(all_of(cols_a_numerica), ~as.numeric(as.character(.)))) %>%
  mutate(across(all_of(cols_a_factor), as.factor))

sapply(datos, class)

attach(datos)
####################### Experimentos multiambiente con modelo lineal de efectos fijos #################

names(datos)

mod <- aov(TCH ~ Biochar + Finca + Error(Replica %in% Finca) + Biochar:Finca, data=datos)
summary(mod)

mod <- aov(Pureza ~ Tratamiento + Finca + Replica + Error(Replica %in% Finca) + Tratamiento:Finca, data=datos)

summary(mod)


#Verificación de supuestos
#Residuos vs predichos
resp1<-aov(TCH ~ Biochar*Zafra+ Replica/Zafra)
summary(resp1)
plot(resp1,1)
#QQ-plot
plot(resp1,2)
#Prueba de Shapiro Wilks
shapiro.test(resp1$residuals)


#Prueba múltiple de medias (Scott Knott)
sk1<- SK(mod,
         data=datos,
         which="Tratamiento",
         sig.level = 0.15)

summary(sk1)

sk2<- SK(mod,
         data=datos,
         which="Zafra",
         sig.level = 0.15)
summary(sk2)



sk2<- SK(mod,
         data=datos,
         which="Biochar",
         sig.level = 0.15)
summary(sk2)



# Modelo mixto para multiambientales: diferente finca y diferente año 
names(datos)

datos$Ambiente <- as.factor(paste(datos$Finca, datos$Zafra, sep = "_"))

modelo_tch <- lmer(TCH ~ Biosustrato + (1|Ambiente) + (1|Ambiente:Replica), data = datos)
anova(modelo_tch)

medias_ajustadas <- emmeans(modelo_tch, ~ Biosustrato)


cld(medias_ajustadas, Letters = letters, reverse = TRUE)



 library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
library(multcompView)

modelo_para_ambientes <- lmer(TCH ~ Biosustrato + Ambiente + (1|Ambiente:Replica), data = datos)

tukey_ambientes <- emmeans(modelo_para_ambientes, pairwise ~ Ambiente)

summary(tukey_ambientes$contrasts)

tukey_letras <- cld(tukey_ambientes$emmeans, Letters = letters, reverse = TRUE, alpha = 0.15)
print(tukey_letras)

#######################################################################################################
names(datos)
# =========================================================================
# CONFIGURACIÓN (Variables y Factores)
# =========================================================================
variables_resp <- c("TCH")
f_tratamiento  <- "Biochar"
f_ambiente     <- "Zafra"
bloque         <- "Replica"
nivel_sig      <- 0.15

# Contenedores para la Tabla Maestra Consolidada
tabla_maestra_anova <- datos %>%
  group_by(across(all_of(f_tratamiento))) %>%
  summarise(N = n(), .groups = 'drop') %>%
  dplyr::select(all_of(f_tratamiento)) %>%
  mutate(across(everything(), as.character))

p_valores_maestra <- data.frame(Termino = character())

# =========================================================================
# BUCLE DE ANÁLISIS: ANDEVA + SK1 (Trat) + SK2 (Zafra)
# =========================================================================

for (var in variables_resp) {
  if (!var %in% names(datos)) next
  
  # 1. MODELO AOV ORIGINAL
  form_aov <- as.formula(paste(var, "~", f_tratamiento, "+", f_ambiente, 
                               "+ Error(", bloque, "%in%", f_ambiente, ") +", 
                               f_tratamiento, ":", f_ambiente))
  mod <- tryCatch({ aov(form_aov, data = datos) }, error = function(e) NULL)
  
  if (is.null(mod)) next
  
  # 2. PRUEBA SK1: SCOTT-KNOTT PARA TRATAMIENTO
  try({
    sk1 <- SK(mod, data = datos, which = f_tratamiento, sig.level = nivel_sig)
    summary(sk1) # Muestra resumen en consola
    
    # Crear Tabla para View
    tab_sk1 <- as.data.frame(sk1$groups)
    tab_sk1$Tratamiento <- rownames(tab_sk1)
    nombre_v1 <- paste0("TABLA_SK_TRAT_", var)
    assign(nombre_v1, tab_sk1[, c("Tratamiento", "groups")])
    View(get(nombre_v1), title = paste("SK Tratamiento", var))
  }, silent = TRUE)
  
  # 3. PRUEBA SK2: SCOTT-KNOTT PARA ZAFRA
  try({
    sk2 <- SK(mod, data = datos, which = f_ambiente, sig.level = nivel_sig)
    summary(sk2) # Muestra resumen en consola
    
    # Crear Tabla para View
    tab_sk2 <- as.data.frame(sk2$groups)
    tab_sk2$Zafra <- rownames(tab_sk2)
    nombre_v2 <- paste0("TABLA_SK_ZAFRA_", var)
    assign(nombre_v2, tab_sk2[, c("Zafra", "groups")])
    View(get(nombre_v2), title = paste("SK Zafra", var))
  }, silent = TRUE)
  
  # 4. CONSOLIDACIÓN DE TABLA MAESTRA DE ANDEVA
  media_var <- datos %>%
    group_by(across(all_of(f_tratamiento))) %>%
    summarise(!!var := round(mean(!!sym(var), na.rm = TRUE), 2), .groups = 'drop') %>%
    mutate(across(everything(), as.character))
  
  tabla_maestra_anova <- tabla_maestra_anova %>% left_join(media_var, by = f_tratamiento)
  
  # Extraer P-valores
  try({
    s_mod <- summary(mod)
    for(i in seq_along(s_mod)){
      tp <- s_mod[[i]][[1]]
      if(!is.null(tp[["Pr(>F)"]])){
        for(term in rownames(tp)[!is.na(tp[["Pr(>F)"]])] ){
          lab <- paste0("P-val (", term, ")")
          if(!(lab %in% p_valores_maestra$Termino)) p_valores_maestra <- bind_rows(p_valores_maestra, data.frame(Termino = lab))
          p_valores_maestra[p_valores_maestra$Termino == lab, var] <- sprintf("%.4f", tp[term, "Pr(>F)"])
        }
      }
    }
  }, silent = TRUE)
}

# =========================================================================
# VISTA FINAL DE LA TABLA MAESTRA
# =========================================================================
colnames(p_valores_maestra)[1] <- f_tratamiento
TABLA_MAESTRA_TOTAL <- rbind(
  tabla_maestra_anova %>% mutate(across(everything(), as.character)),
  p_valores_maestra %>% mutate(across(everything(), as.character))
)

View(TABLA_MAESTRA_TOTAL, title = "RESUMEN ANDEVA Y MEDIAS")



###############################################################################################################################


# Multiambientales considerando la zafra y finca

# 1. Cargar librerías necesarias
library(lme4)      # Para el modelo mixto
library(lmerTest)  # Para obtener p-valores (ANOVA)
library(emmeans)   # Para la prueba de medias
library(multcomp)  # Para las letras de significancia

# 2. Ajuste del Modelo Lineal Mixto
# NOTA: Cambia 'Variable_Respuesta' y 'Tratamiento' por tus nombres de columna
modelo <- lmer(TCH ~ Biochar + (1|Finca) + (1|Zafra), data = datos)

# 3. Análisis de Varianza (ANOVA)
# Esto te dirá si el Tratamiento tiene un efecto significativo global
anova_res <- anova(modelo)
print(anova_res)

# 4. Prueba de Medias (DMS / LSD)
# 'adjust = "none"' solicita la prueba de Diferencia Mínima Significativa
medias <- emmeans(modelo, ~ Biochar)

# Obtener las comparaciones de a pares
comparaciones <- contrast(medias, method = "pairwise", adjust = "none")
print(comparaciones)

# 5. Ranking y Letras (Para ver cuál es mejor)
# Los tratamientos que comparten letras NO son diferentes entre sí
grupos <- cld(medias, alpha = 0.15, Letters = letters, adjust = "none", decreasing = TRUE)
print(grupos)



# Medias reales:

# 1. Calcular las medias reales (observadas) desde el dataframe 'datos'
library(dplyr)
medias_reales <- datos %>%
  group_by(Biosustrato) %>%
  summarise(Media_Real = mean(TCH, na.rm = TRUE))

# 2. Obtener el ranking de letras del modelo (usando alpha = 0.15 como pediste)
# Nota: 'medias' viene del objeto emmeans definido anteriormente
grupos <- cld(medias, alpha = 0.15, Letters = letters, adjust = "none", decreasing = TRUE)

# 3. Unir ambos resultados en una sola tabla para comparar
# Convertimos 'grupos' a dataframe para poder unirlo
tabla_final <- as.data.frame(grupos) %>%
  inner_join(medias_reales, by = "Biosustrato") %>%
  arrange(desc(Media_Real)) # Ordenar de mayor a menor

# 4. Mostrar el resultado final
print(tabla_final)



# 6. Diagnóstico rápido del modelo
# Es importante verificar que los residuos sigan una distribución normal
plot(modelo) # Heterocedasticidad
qqnorm(resid(modelo)); qqline(resid(modelo)) # Normalidad



########################################################################################################

library(lme4)
library(lmerTest)
library(emmeans)
names(datos)

# 1. Ajuste del modelo correcto (Efectos fijos: Trat x Zafra | Efecto aleatorio: Finca en Zafra)
# Nota: Si tienes réplicas físicas dentro de cada finca, usa (1 | Zafra:Finca:Replica)
modelo <- lmer(TCH ~ Biosustrato * Zafra + (1 | Zafra/Finca), data = datos)

# 2. ANOVA para validar si la interacción es significativa
print(anova(modelo))

# 3. Medias por Zafra (DMS / LSD)
# Aquí es donde comparamos los tratamientos DENTRO de cada año
medias_agrupadas <- emmeans(modelo, ~ Biosustrato | Zafra)


# 4. Obtener las letras de significancia (Alpha 0.15 como pediste)
# 'adjust = "none"' es lo que convierte la prueba en una DMS de Fisher
tab_letras <- cld(medias_agrupadas, alpha = 0.15, Letters = letters, adjust = "none", decreasing = TRUE)

# 5. Agregar las Medias Reales (Observadas)
medias_reales <- datos %>%
  group_by(Zafra, Biosustrato) %>%
  summarise(Media_Real = mean(TCH, na.rm = TRUE), .groups = 'drop')

# Resultado final comparativo
resultado_final <- as.data.frame(tab_letras) %>%
  inner_join(medias_reales, by = c("Zafra", "Biosustrato")) %>%
  arrange(Zafra, desc(Media_Real))

print(resultado_final)


##########################################################################################


library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)

# 1. Ajuste del Modelo con Anidamiento Correcto
# Zafra se incluye en el efecto fijo para comparar tratamientos entre años
# (1 | Finca/Replica) indica que las réplicas están anidadas en las fincas
modelo_tch <- lmer(TCH ~ Biosustrato * Biochar * Microorganismos * Zafra + 
                     (1 | Finca/Replica), 
                   data = datos)

# 2. ANOVA para verificar interacciones
print(anova(modelo_tch))

# 3. Medias ajustadas (EMMeans) por Zafra
# Esto genera el grid para comparar tus 3 factores dentro de cada año
medias_aj <- emmeans(modelo_tch, ~ Biosustrato * Biochar * Microorganismos | Zafra)

# 4. Prueba DMS (LSD) con Alpha 0.15 y Letras
# 'adjust = "none"' es lo que define legalmente la prueba DMS de Fisher
ranking <- cld(medias_aj, alpha = 0.15, Letters = letters, adjust = "none", decreasing = TRUE)

# 5. Integración con Medias Reales para el reporte
library(dplyr)
medias_reales <- datos %>%
  group_by(Zafra, Biosustrato, Biochar, Microorganismos) %>%
  summarise(TCH_Real = mean(TCH, na.rm = TRUE), .groups = 'drop')

tabla_final <- as.data.frame(ranking) %>%
  inner_join(medias_reales, by = c("Zafra", "Biosustrato", "Biochar", "Microorganismos")) %>%
  arrange(Zafra, desc(TCH_Real))

# Resultado listo para exportar o analizar
print(tabla_final)














library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)

# 1. Ajuste del Modelo con Anidamiento Correcto
# Zafra se incluye en el efecto fijo para comparar tratamientos entre años
# (1 | Finca/Replica) indica que las réplicas están anidadas en las fincas
modelo_tch <- lmer(TCH ~ Biosustrato * Coctel + 
                     (1 | Finca/Replica), 
                   data = datos)

# 2. ANOVA para verificar interacciones
print(anova(modelo_tch))

# 3. Medias ajustadas (EMMeans) por Zafra
# Esto genera el grid para comparar tus 3 factores dentro de cada año
medias_aj <- emmeans(modelo_tch, ~ Biosustrato * Coctel | Finca)

# 4. Prueba DMS (LSD) con Alpha 0.15 y Letras
# 'adjust = "none"' es lo que define legalmente la prueba DMS de Fisher
ranking <- cld(medias_aj, alpha = 0.15, Letters = letters, adjust = "none", decreasing = TRUE)

# 5. Integración con Medias Reales para el reporte
library(dplyr)
medias_reales <- datos %>%
  group_by(Zafra, Biosustrato, Biochar, Microorganismos) %>%
  summarise(TCH_Real = mean(TCH, na.rm = TRUE), .groups = 'drop')

tabla_final <- as.data.frame(ranking) %>%
  inner_join(medias_reales, by = c("Zafra", "Biosustrato", "Biochar", "Microorganismos")) %>%
  arrange(Zafra, desc(TCH_Real))

# Resultado listo para exportar o analizar
print(tabla_final)
