paquetes <- c("readxl", "dplyr", "ScottKnott", "tidyr", "factoextra",
              "ggplot2", "ggh4x", "viridis", "scales", "ggh4x", "ggrepel")


for (p in paquetes) {
  if (!(p %in% rownames(installed.packages()))) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
}



setwd("c:/users/147379/OneDrive - Pantaleon. S.A/ESTADO IV (PR)")
getwd()
data_orig<-read_excel("formato2_PR.xlsx",
                 sheet = "PR")


library(readxl)
data_orig <- read_excel("C:/Users/105216/Downloads/Formato R (1) (version 1).xlsx", 
                        sheet = "EV")
View(data_orig)




data<-data_orig%>%
  filter(TAH>0)%>%
  group_by(FINCA, EE,VARIEDAD, ZAFRA, CORRELATIVO, CORTE,ENSAYO, REPETICION)%>%
  summarise(TCH=mean(TCH, na.rm=TRUE), 
            KgAzTc=mean(KgAzTc, na.rm=TRUE), 
            TAH=(TCH*KgAzTc)/1000, 
            Pureza=mean(Pureza, na.rm = TRUE))



ensayos<-unique(data$ENSAYO)
data$ID<-paste(data$EE, data$ZAFRA,sep = "_")
lotes<-unique(data$ID)

data<-data%>%
  mutate(factor=Pureza/100,
         KgAzTc_Aj=KgAzTc*factor,
         TAH_Aj=(TCH*KgAzTc_Aj)/1000)

ID2<-data.frame(ID2=paste(data$EE, data$ZAFRA, data$VARIEDAD, data$REPETICION, sep = "_"))

data<-cbind(ID2, data)



##############################AOV REAL
columnas<-colnames(data)[c(10,11,12,13,16,17)]#son las columnas que se les hará anova
df_lotes<-data.frame(lotes)

data_aov<-NULL
aovbyvar<-NULL

for (i in 1:length(lotes)) {#un ciclo es una variable y sus tres zafras
  

  for (j in 1:length(columnas)) {
    data2<-data%>%filter(
      TAH>0,
      ID==lotes[i])#i
    
    if (length(unique(data2$REPETICION))>1){
      base<-data%>%
        filter(ID==lotes[i],#i
               TAH>0)%>%#lote en realidad es el lote y su zafra
        group_by(VARIEDAD,ZAFRA, FINCA,EE,CORRELATIVO, CORTE,ENSAYO)%>%
        summarise(error=sd(get(columnas[j]))/sqrt(length(get(columnas[j]))))#j CAMBIAR LA VARIABLE
      
      
      formula_text <- paste(columnas[j], "~ VARIEDAD + REPETICION") #j
      formula <- as.formula(formula_text)
      
      andeva<-aov(formula, data=data2)#CAMBIAR LA VARIABLE
      post_andeva<-SK(andeva, which = "VARIEDAD", sig.level = 0.15)
      
      
      ver<-summary(post_andeva)
      ver<-cbind(VARIEDAD=rownames(ver), ver)
      ver<-ver%>%
        unite("literal", 3:ncol(ver), remove = F, sep = "")
      
      ver<-ver[1:3]
      fin<-merge(base, ver, by = c("VARIEDAD"))
      fin$variable<-colnames(andeva$mode[1])
      fin$pvalor_aov<-summary(andeva)[[1]][["Pr(>F)"]][1]
      data_aov<-rbind(data_aov,fin)
      print(fin[1,2:7])
    }else
    {
      fin<-NULL
      data_aov<-rbind(data_aov,fin)
    }
    
  }
}



aux<-data%>%#AOV del promedio de cortes
  filter(TAH>0)%>%
  group_by(FINCA, EE,VARIEDAD, REPETICION, ZAFRA, CORRELATIVO,ENSAYO)%>%
  summarise(TCH=mean(TCH), TAH=mean(TAH), KgAzTc=mean(KgAzTc),
            Pureza=mean(Pureza), KgAzTc_Aj=mean(KgAzTc_Aj), TAH_Aj=mean(TAH_Aj))

aux$ID<-paste(aux$EE,aux$ENSAYO)

ids<-unique(aux$ID)

for (i in 1:length(ids)) {
  aux2<-aux%>%
    filter(ID==ids[i])
  
  if(length(unique(aux2$ZAFRA))>1 & length(unique(aux2$REPETICION))>1){
    
    for (j in 1:length(columnas)) {
      aux2$ZAFRA<-"Promedio"
      aux2$CORTE<-"todos"
      aux2<-aux2%>%
        group_by(FINCA, EE,VARIEDAD, REPETICION, ZAFRA, CORRELATIVO, CORTE,ENSAYO)%>%
        summarise(TCH=mean(TCH), TAH=mean(TAH), KgAzTc=mean(KgAzTc),
                  Pureza=mean(Pureza), KgAzTc_Aj=mean(KgAzTc_Aj), TAH_Aj=mean(TAH_Aj))
      
      
      base2<-aux2%>%
        group_by(VARIEDAD,ZAFRA, FINCA,EE,CORRELATIVO, CORTE,ENSAYO)%>%
        summarise(error=sd(get(columnas[j]))/sqrt(length(get(columnas[j]))))#cambiar variable
      
      formula_text <- paste(columnas[j], "~ VARIEDAD + REPETICION") 
      formula <- as.formula(formula_text)
      
      
      andeva2<-aov(formula, data=aux2)#CAMBIAR LA VARIABLE
      
      post_andeva2<-SK(andeva2, which = "VARIEDAD", sig.level = 0.1)
      
      ver2<-summary(post_andeva2)
      ver2<-cbind(VARIEDAD=rownames(ver2), ver2)
      ver2<-ver2%>%
        unite("literal", 3:ncol(ver2), remove = F, sep = "")
      
      ver2<-ver2[1:3]
      fin2<-merge(base2, ver2, by = c("VARIEDAD"))
      fin2$variable<-colnames(andeva2$mode[1])
      fin2$pvalor_aov<-summary(andeva2)[[1]][["Pr(>F)"]][1]
      data_aov<-rbind(data_aov,fin2)
      
    }
    
  }else
  {
    fin2<-NULL
    data_aov<-rbind(data_aov,fin2)
  }
  
  
  print(print(fin2[1,2:7]))
  
}


data_aov$Means<-as.numeric(data_aov$Means)
data_aov$error<-as.numeric(data_aov$error)
data_aov2<-data_aov
data_aov2$variable<-factor(data_aov2$variable, 
                           levels = c("TAH", "KgAzTc", "TCH", "Pureza", "KgAzTc_Aj", "TAH_Aj"))
library(ggh4x)

data_aov2$VARIEDAD<-reorder(data_aov2$VARIEDAD, data_aov2$Means, decreasing = T)

data_aov2$Calculo<-ifelse(data_aov2$variable%in% c("TAH_Aj", "KgAzTc_Aj"), "Ajuste", "Sin ajuste")

data_aov2 <- data_aov2 %>% 
  mutate(variable = case_when(
    variable == "KgAzTc_Aj" ~ "KgAzTc",
    variable == "TAH_Aj" ~ "TAH",
    TRUE ~ variable))

data_aov2$variable<-factor(data_aov2$variable, 
                           levels = c("TAH", "Pureza","KgAzTc", "TCH" ))

library(viridis)
library(scales)
library(ggh4x)
library(ggrepel)


var_tah<-data_aov2%>% #PARA ORDENAR LAS VERIEDAD/TAH FINCA
  filter(CORRELATIVO== 25,
         ZAFRA=="25-26",
         ENSAYO=="25 PF F",
         FINCA=="Guanacaste",
         EE=="NI01-0020684-0001",
         Calculo=="Ajuste",
         variable=="TAH")%>%
  group_by(VARIEDAD)%>%
  summarise(Means=mean(Means))%>%
  arrange(desc(Means))


data_aov2$reclasvar<-paste(data_aov2$variable, data_aov2$Calculo)

  
data_aov2%>%
  filter(CORRELATIVO==25,
         ENSAYO=="25 PF F",
         EE=="NI01-0020684-0001",
         FINCA=="Guanacaste",
         !reclasvar%in% c("KgAzTc Ajuste", "TAH Sin ajuste"),
  )%>%
  mutate(VARIEDAD=factor(VARIEDAD, levels=c(var_tah$VARIEDAD)))%>%
  ggplot(aes(x=VARIEDAD, y=as.numeric(Means), fill="red"))+
  geom_bar(stat = "identity", position=position_dodge(width = 1),width = 0.9)+
  geom_errorbar(aes(ymax=Means+error, ymin=Means-error), position = position_dodge(1), width=0.1)+
  geom_text(aes(y=Means+error,label = paste(literal)), position = position_dodge(1), hjust=0.5, vjust=-0.2,  size=4)+
  geom_text(aes(y=Means-error,label = paste(round(Means,1))), 
            position = position_dodge(0.9), hjust=0.5, vjust=2.5,  size=4)+
  geom_blank(aes(y = Means+10))+
  facet_grid2(ZAFRA~variable, scales = "free_y", independent = "y",switch = "y")+
  theme(text = element_text(size = 28),
        legend.position = "none",
        strip.text.y.left = element_text(angle = 90),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(face = "bold", size = 12),
        axis.text.x = element_text(angle = 0, size = 12))+
  #axis.text.x = element_blank())+
  labs(y="Productividad", x=NULL)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))


#####################################################################################


# Verificar cuántos datos tiene después del filtrado
test_data <- data_aov2%>%
  filter(CORRELATIVO==23,
         ENSAYO=="23 PF NF",
         EE=="NI01-0020477-0001",
         FINCA=="Olomega",
         !reclasvar%in% c("KgAzTc Ajuste", "TAH Sin ajuste"))

print(dim(test_data))
print(unique(test_data$ZAFRA))
print(unique(test_data$variable))




# 1. Verificar qué ENSAYOS tienes disponibles
print("ENSAYOS disponibles:")
print(unique(data_aov2$ENSAYO))

# 2. Verificar qué EE tienes disponibles
print("EE disponibles:")
print(unique(data_aov2$EE))

# 3. Verificar qué FINCAS tienes disponibles
print("FINCAS disponibles:")
print(unique(data_aov2$FINCA))

# 4. Verificar qué CORRELATIVO tienes
print("CORRELATIVOS disponibles:")
print(unique(data_aov2$CORRELATIVO))

# 5. Verificar qué ZAFRAS tienes
print("ZAFRAS disponibles:")
print(unique(data_aov2$ZAFRA))

# 6. Verificar qué reclasvar tienes
print("reclasvar disponibles:")
print(unique(data_aov2$reclasvar))
