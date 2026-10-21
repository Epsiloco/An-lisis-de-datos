paquetes <- c("readxl", "dplyr", "ScottKnott", "tidyr", "factoextra",
              "cluster", "data.table", "dendextend", "sqldf", "dtw", "pastecs",
              "ggtext", "ggh4x")

for (p in paquetes) {
  if (!(p %in% rownames(installed.packages()))) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
}


library(readxl)
data <- read_excel("C:/Users/105216/Downloads/Formato R (1).xlsx", 
                   sheet = "EIV")
View(data)


library(readxl)
data <- read_excel("C:/Users/105216/Downloads/Book4.xlsx", 
                   sheet = "EIV")
View(data)

data <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")




library(sqldf)

result<-sqldf('SELECT  DISTINCT CORRELATIVO, TIPO, FINCA, LOTE
      FROM data
      WHERE CORRELATIVO ==24
      ORDER BY TIPO ASC')
print(result)

flor19<-data%>%filter(CORRELATIVO==24,
                      TIPO=="NF",
                      FINCA=="Juan Pablo II")




flor19<-flor19%>%
  ungroup()%>%
  select(VARIEDAD, ZAFRA, TCH, TAH, KgAzTc, Pureza)
flor19$TAH<-(flor19$KgAzTc*(flor19$Pureza/100)*flor19$TCH)/1000

#write.csv(flor19, "c:/users/147379/OneDrive - Pantaleon. S.A/ESTADO IV (PR)/formato_pr.csv")

resumen<-flor19%>%
  group_by(VARIEDAD)%>%
  summarise(registros=length(VARIEDAD))

resumen
F19<-dcast.data.table(setDT(flor19), VARIEDAD~ZAFRA, value.var = c("TCH", "TAH", "KgAzTc", "Pureza"),
                      fun.aggregate = mean)#data.table

df<-data.frame(F19[,-1])

df<-scale(df)

df <- na.omit(df)
res<-fviz_nbclust(df, kmeans, method = "silhouette", k.max = 5)+#factoextra
  theme_minimal()+
  geom_line(aes(group=1),size=0.9, color="steelblue")+
  geom_point(aes(group=1),size=5, color="steelblue")+
  theme(text = element_text(size = 20),
        axis.text.x = element_text(size = 20))

res
rownames(df)<-F19$VARIEDAD
g<-2 #<-----aquí asignar el número de grups

#con distancias ward
#d<-dist(df)
#hc<-hclust(d, method = "ward.D")

#con distancias  de series de tiempo
library(dtw)

d<-dist(df, method = "dtw") #euclidean ,  dtw 
hc<-hclust(d)
cut_avg <- cutree(hc,g)#se asigna la cantidad de grupos
cut_avg<-as.data.frame(cut_avg)

F19<-cbind(F19, data.frame(cluster=cut_avg$cut_avg))#se asigna el cluster  a cada variedad

n<-ncol(F19)
n
flor19<-merge(flor19, F19[,.SD, .SDcols = c(1,n)], by=c("VARIEDAD"))#En F19 solo se elige el campode variedad y cluster
flor19<-flor19%>%
  gather(key = "VARIABLE", value = "VALOR", 3:6)#se seleccionan las columanas incluyendo pureza

#F19[,c(1,11)]
#par(mar=c(2,0.1,0.1,6)) 




flor19$VARIABLE<-factor(flor19$VARIABLE, levels = c("TAH","Pureza", "KgAzTc","TCH"))
medias<-flor19%>%
  filter(VARIABLE=="TAH")%>%
  group_by(cluster)%>%
  summarise(VALOR=mean(VALOR))

medias<-medias%>%arrange(desc(VALOR))%>%
  mutate(cluster2=1:n())
flor19<-merge(flor19, medias[,c(1,3)], by=c("cluster"))

library(dendextend)
testigo<-c("CP72-2086", "CG02-163", "CGMEX10-26315", "SP80-1842", "CG09-133119")
# Crear dendrograma
dend <- as.dendrogram(hc)
k <- g

# Cortar y colorear ramas
#dend <- color_branches(dend, k = k)
dend <- set(dend, "labels_cex", 0.8)

# Reiniciar todos los colores de etiquetas a negro
labels_colors(dend) <- rep("black", length(labels(dend)))

# Asignar rojo solo a los testigos
for (t in testigo) {
  idx <- which(labels(dend) == t)
  if (length(idx) > 0) {
    labels_colors(dend)[idx] <- "red"
  }
}

# Graficar dendrograma
par(mar = c(2, 0.1, 0.1, 6), cex = 0.8)
plot(dend, horiz = TRUE)
rect.dendrogram(dend, k = k, horiz = TRUE, border = 2:11)

# Obtener los grupos originales
clusters <- cutree(hc, k = k)

# Asociar cluster original con cluster2 (reclasificado)
cluster_map <- unique(flor19[, c("cluster", "cluster2")])

# Calcular posición media de cada grupo y mostrar cluster2
ordered_labels <- labels(dend)
for (i in 1:k) {
  miembros <- names(clusters[clusters == i])
  y_pos <- mean(which(ordered_labels %in% miembros))
  x_pos <- max(get_branches_heights(dend)) * 1.02
  
  # Obtener el nuevo número de clúster (cluster2)
  nuevo_cluster <- cluster_map$cluster2[cluster_map$cluster == i]
  
  text(x = x_pos, y = y_pos, labels = paste("Grupo", nuevo_cluster), col = "black", cex = 1.2, pos = 4)
}



#Dibujar flecha

# Obtener etiquetas en orden
ordered_labels <- labels(dend)

# Dibujar flechas para cada testigo
for (t in testigo) {
  if (t %in% ordered_labels) {
    y_pos <- which(ordered_labels == t)
    
    # Coordenadas de la flecha
    x_start <- 1.5
    x_end <- -0.2
    
    arrows(x0 = x_start, y0 = y_pos, 
           x1 = x_end, y1 = y_pos, 
           col = "red", lwd = 1.5, length = 0.1)
  }
}





mejores<-flor19%>%
  filter(cluster2==1,
         VARIABLE=="TAH")%>%
  group_by(VARIEDAD)%>%
  summarise(VALOR=mean(VALOR))


flor19%>%
  ggplot(aes(x=as.factor(cluster2), y=VALOR))+
  geom_boxplot()+
  stat_summary(fun=mean, colour="red", geom="point",
               size=2, show.legend=FALSE)+
  facet_wrap(.~VARIABLE, scales = "free_y")+
  labs(x="Grupo",y="Productividad")+
  theme(text = element_text(size=15))


length(unique(flor19$VARIEDAD))
flor19%>%group_by(cluster2, VARIEDAD)%>%summarise(Canta_Var=length(unique(flor19$VARIEDAD)))%>%
  group_by(cluster2)%>%summarise(variedades=length(VARIEDAD))

library(pastecs)#PARA VER RESEUMENES
by(flor19$VALOR[flor19$VARIABLE=="TAH"], flor19$cluster[flor19$VARIABLE=="TAH"], stat.desc)
by(flor19$VALOR[flor19$VARIABLE=="TCH"], flor19$cluster[flor19$VARIABLE=="TCH"], stat.desc)

library(dplyr)

varstop<-flor19%>%#solo cuando se exploran del grupo2
  filter(cluster2==1,
         VARIABLE=="TAH")%>%
  group_by(VARIEDAD)%>%
  summarise(TAH=mean(VALOR))%>%
  top_n(10,TAH)

#write.csv(F19,"c:/users/147379/OneDrive - Pantaleon. S.A/VARIEDADES/ENSAYOS/ZAFRA_23_24/GIRA/OCTUBRE_2024/22PRNF_PLAYAG.csv", row.names = F)


var_tah<-flor19%>%
  filter(VARIABLE=="TAH")%>%
  group_by(VARIEDAD)%>%
  summarise(Means=mean(VALOR))%>%
  arrange(desc(Means))


seleccionadas_f<-c("CG16-43", 
                   "CG16-66", 
                   "CG16-67", 
                   "CG16-85",
                   "CG16-94",
                   "CG16-179")

seleccionadas_nf<-c("CG16-206", 
                    "CG16-303", 
                    "CG16-414")

flor19$tipo<-ifelse(flor19$VARIEDAD %in% seleccionadas_nf, "Seleccionada", "No")
cols<-c("coral")


library(dplyr)
library(ggplot2)
library(ggtext)

testigo <- c("CP72-2086", "CG02-163", "CGMEX10-26315", "SP80-1842")

# Paso 1: Agregar columna 'etiqueta' con HTML para testigos
flor19 <- flor19 %>%
  mutate(
    tipo = ifelse(VARIEDAD %in% seleccionadas_nf, "Seleccionada", "No"),
    etiqueta = ifelse(VARIEDAD %in% testigo,
                      paste0("<span style='color:red;'>", VARIEDAD, "</span>"),
                      VARIEDAD)
  )

# Paso 2: Crear tabla de resumen
resumen <- flor19 %>%
  filter(cluster2 == 1) %>% #<---Aquí ir cambiando los grupos
  group_by(VARIEDAD, VARIABLE, tipo, etiqueta) %>%
  summarise(VALOR = mean(VALOR), .groups = "drop")




# Paso 2: Crear tabla de resumen blindada (Máximo 5 variedades en total)

# 2.1 Extraer estrictamente las 2 mejores del Grupo 1 (basado en TAH)
top2_grupo1 <- flor19 %>%
  filter(cluster2 == 1, VARIABLE == "TAH") %>%
  group_by(VARIEDAD) %>%
  summarise(Media_TAH = mean(VALOR), .groups = "drop") %>%
  arrange(desc(Media_TAH)) %>%
  slice_head(n = 2) %>% # <--- Fija un tope de 2 variedades
  pull(VARIEDAD)

# 2.2 Extraer estrictamente las 3 mejores del Grupo 2 (basado en TAH)
top3_grupo2 <- flor19 %>%
  filter(cluster2 == 1, VARIABLE == "TAH") %>%
  group_by(VARIEDAD) %>%
  summarise(Media_TAH = mean(VALOR), .groups = "drop") %>%
  arrange(desc(Media_TAH)) %>%
  slice_head(n = 3) %>% # <--- Fija un tope de 3 variedades
  pull(VARIEDAD)

# 2.3 Filtrar la base usando únicamente las 5 variedades seleccionadas arriba
resumen <- flor19 %>%
  filter(VARIEDAD %in% c(top2_grupo1, top3_grupo2)) %>% 
  group_by(VARIEDAD, VARIABLE, tipo, etiqueta) %>%
  summarise(VALOR = mean(VALOR), .groups = "drop")



# Paso 3: Ordenar etiquetas según el orden de VARIEDAD en var_tah
orden_etiquetas <- resumen %>%
  distinct(VARIEDAD, etiqueta) %>%
  mutate(VARIEDAD = factor(VARIEDAD, levels = var_tah$VARIEDAD)) %>%
  arrange(VARIEDAD) %>%
  pull(etiqueta)

resumen <- resumen %>%
  mutate(etiqueta = factor(etiqueta, levels = orden_etiquetas))

# Paso 4: Graficar
ggplot(resumen, aes(x = etiqueta, y = VALOR, fill = "red", color = tipo)) +
  geom_bar(stat = "identity", width = 0.8, size = 1) +
  scale_fill_manual(values = c("red" = "coral")) +
  scale_color_manual(values = c("NA", "#26B300")) +
  facet_wrap(. ~ VARIABLE, scales = "free_y", ncol = 1) +
  geom_text(aes(label = round(VALOR, 1)), vjust = 2, size = 6, color = "black") +#<--cambiar tamaño de letra de la etiqueta de valores
  labs(x = NULL, y = NULL) +
  theme(
    text = element_text(size = 15),
    legend.position = "none",
    axis.text.x = element_markdown(size = 12, angle = 0),#<----Cambiar orientación de las etiquetas eje x
    axis.text.x.top = element_markdown(),
    axis.text.x.bottom = element_markdown(),
  ) +
  scale_x_discrete(guide = guide_axis(n.dodge = 1))#<----Cambiar cuando sea necesario más de una fila etiquetas eje x



