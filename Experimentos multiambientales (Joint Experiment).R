
####################### Experimentos multiambiente con modelo lineal de efectos fijos #################


datos <- read_clip_tbl()  

attach(datos)
view(datos)
head(datos)
tail(datos)
names(datos)


mod <- aov(Pureza ~ Tratamiento + Zafra + Error(Replica %in% Zafra) + Tratamiento:Zafra, data=datos)
summary(mod)

mod1 <- aov(TAH ~ datos$Tratamiento + datos$Zafra + Error(datos$Replica/datos$Zafra) + datos$Zafra:datos$Tratamiento)
summary(mod1)

#Verificación de supuestos
#Residuos vs predichos
resp1<-aov(TCH ~ Tratamiento*Zafra+ Replica/Zafra)
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




# Modelo mixto para multiambientales: diferente finca y diferente año 
names(datos)

datos$Ambiente <- as.factor(paste(datos$ingenio, datos$zafra, sep = "_")) # Variable combinada zafra*ingenio

modelo_tch <- lmer(TCH ~ Dosis + (1|Zafra) + (1|Zafra:Replica), data = datos)
anova(modelo_tch)

medias_ajustadas <- emmeans(modelo_tch, ~ Tratamiento)


cld(medias_ajustadas, Letters = letters, reverse = TRUE)



library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
library(multcompView)
# TCH
names(datos)

modelo_para_ambientes <- lmer(TCH ~ Tratamiento + Zafra + (1|Zafra:Replica), data = datos)

tukey_ambientes <- emmeans(modelo_para_ambientes, pairwise ~ Zafra)

summary(tukey_ambientes$contrasts)

tukey_letras <- cld(tukey_ambientes$emmeans, Letters = letters, reverse = TRUE, alpha = 0.15)
print(tukey_letras)


# Rendimiento

modelo_para_ambientes <- lmer(Rendimiento ~ Tratamiento + Zafra + (1|Zafra:Replica), data = datos)

tukey_ambientes <- emmeans(modelo_para_ambientes, pairwise ~ Zafra)

summary(tukey_ambientes$contrasts)

tukey_letras <- cld(tukey_ambientes$emmeans, Letters = letters, reverse = TRUE, alpha = 0.15)
print(tukey_letras)

# TAH

modelo_para_ambientes <- lmer(TAH ~ Tratamiento + Zafra + (1|Zafra:Replica), data = datos)

tukey_ambientes <- emmeans(modelo_para_ambientes, pairwise ~ Zafra)

summary(tukey_ambientes$contrasts)

tukey_letras <- cld(tukey_ambientes$emmeans, Letters = letters, reverse = TRUE, alpha = 0.15)
print(tukey_letras)

# Pureza

modelo_para_ambientes <- lmer(Pureza ~ Tratamiento + Zafra + (1|Zafra:Replica), data = datos)

tukey_ambientes <- emmeans(modelo_para_ambientes, pairwise ~ Zafra)

summary(tukey_ambientes$contrasts)

tukey_letras <- cld(tukey_ambientes$emmeans, Letters = letters, reverse = TRUE, alpha = 0.15)
print(tukey_letras)

