##### PROYECTO DE LICyT ###
#Diego Emilio Dávila Navarro#####

##Determinación de tasa de crecimiento###

install.packages("multcompView")
install.packages("dplyr")
install.packages("tidyr")
install.packages("Hmisc")

#Limpiar el ambiente de trabajo 

rm(list=ls())

#Cargar librerías
library(dplyr)
library(tidyr)
library(multcompView)
library(stringr)
library(Hmisc)
library(lsmeans)
#1.- Exploración de datos
#Med 1. 03/09/18
#Med 2. 16/09/18
#Med 3. 25/09/18
#Param. fisio. 02/10/18

Med1<-read.csv("TasaCrec_M1.csv")
Med2<-read.csv("TasaCrec_M2.csv")
Med3<-read.csv("TasaCrec_M3.csv")


str(Med1)
class(Med1)
summary(Med1)

#Eliminar Nas
Med3.j<-na.omit(Med3)
Med2.j<-na.omit(Med2)
Med1.j<-na.omit(Med1)

#Crear DataFrame combinada para todos, ayudará a verificar supuestos
MedTodas<-rbind(Med1.j, Med2.j, Med3.j)

#Separar el Bloque del Tratamiento ##Para MED 1##

head(Med1.j)
Med1.j$Bloque<-str_split_fixed(Med1.j$Bloque, 
                  "(?<=[A-Za-z][0-9]).(?=[0-9])", 2)
str(Med1)

head(Med1.j)
tail(Med1.j)

write.csv(Med1.j, "Med1_split.csv")

#Hacer lo mismo con el resto
#Separar Med2

head(Med2.j)
Med2.j$Bloque<-str_split_fixed(Med2.j$Bloque, 
                             "(?<=[A-Za-z][0-9]).(?=[0-9])", 2)
str(Med2)

head(Med2.j)
tail(Med2.j)

write.csv(Med2.j, "Med2_split.csv")

#Separar Med3

head(Med3.j)
Med3.j$Bloque<-str_split_fixed(Med3.j$Bloque, 
                             "(?<=[A-Za-z][0-9]).(?=[0-9])", 2)
str(Med3)

head(Med3.j)
tail(Med3.j)

write.csv(Med3.j, "Med3_split.csv")

#Invocar los nuevos DataFrames sobreescribiendo
Med1<-read.csv("Med1_split.csv")
Med2<-read.csv("Med2_split.csv")
Med3<-read.csv("Med3_split.csv")

#verificar
head(Med1)
head(Med2)
head(Med3)

#Cambiar nombres
colnames(Med1)[colnames(Med1)=="Bloque.1"] <- "Bloque"
colnames(Med1)[colnames(Med1)=="Bloque.2"] <- "Tratamiento"
head(Med1)

colnames(Med2)[colnames(Med2)=="Bloque.1"] <- "Bloque"
colnames(Med2)[colnames(Med2)=="Bloque.2"] <- "Tratamiento"
head(Med2)

colnames(Med3)[colnames(Med3)=="Bloque.1"] <- "Bloque"
colnames(Med3)[colnames(Med3)=="Bloque.2"] <- "Tratamiento"
head(Med3)

#Hacer subset y sobreescribir archivos una vez más
head(Med1)
Med1<-subset(Med1, select=c("Bloque", "Tratamiento", "Individuo",
                            "AlturaTallo", "DiamTallo", "CobVeg_anc", 
                            "CobVeg_lar"))
head(Med1)

head(Med2)
Med2<-subset(Med2, select=c("Bloque", "Tratamiento", "Individuo",
                            "AlturaTallo", "DiamTallo", "CobVeg_anc", 
                            "CobVeg_lar"))

head(Med2)
Med3<-subset(Med3, select=c("Bloque", "Tratamiento", "Individuo",
                            "AlturaTallo", "DiamTallo", "CobVeg_anc", 
                            "CobVeg_lar"))
head(Med3)

summary(Med1)
summary(Med2)
summary(Med3)#Ya no hay Nas

##Ahora visualicemos los datos para Diámetro del Tallo 

boxplot(Med3$DiamTallo~Med3$Tratamiento, main="Medición 3",
        ylab="Diámetro del tallo (mm)", 
        xlab="Tratamiento", notch = T)

boxplot(Med2$DiamTallo~Med2$Tratamiento, main="Medición 2",
        ylab="Diámetro del tallo (mm)",
        xlab="Tratamiento", notch=T)

boxplot(Med1$DiamTallo~Med1$Tratamiento, main="Medición 1",
        ylab="Diámetro del tallo (mm)",
        xlab="Tratamiento", notch=T)

##Visualizar datos para Altura

boxplot(Med3$AlturaTallo~Med3$Tratamiento, main="Medición 3",
        xlab="Tratamiento", ylab="Altura de la planta (cm)", notch=T)

boxplot(Med2$AlturaTallo~Med2$Tratamiento, main="Medición 2",
        xlab="Tratamiento", ylab="Altura de la planta (cm)", notch=T)

boxplot(Med1$AlturaTallo~Med1$Tratamiento, main="Medición 1",
        xlab="Tratamiento", ylab="Altura de la planta (cm)", notch=T)

#Ahora calcular una sola medida de cobertura para cada data.frame
#por separado. 

head(Med1)
Rad.Prom<-function(x){
  (((Med1$CobVeg_anc+Med1$CobVeg_lar)/2)/2)}

Med1$Rad.prom<-Rad.Prom(x)
head(Med1)

area.cob<-function(x){
  ((Med1$Rad.prom^2)*pi)
}

Med1$Area.cob<-area.cob(x)
head(Med1)

write.csv(Med1, "Med1_Cob.csv")

#Ahora aplicar funciones a Meds 2 y 3.

#Med 2

head(Med2)
Rad.Prom<-function(x){
  (((Med2$CobVeg_anc+Med2$CobVeg_lar)/2)/2)}

Med2$Rad.prom<-Rad.Prom(x)
head(Med2)

area.cob<-function(x){
  ((Med2$Rad.prom^2)*pi)
}

Med2$Area.cob<-area.cob(x)
head(Med2)

write.csv(Med2, "Med2_Cob.csv")

# Med 3

head(Med3)
Rad.Prom<-function(x){
  (((Med3$CobVeg_anc+Med3$CobVeg_lar)/2)/2)}

Med3$Rad.prom<-Rad.Prom(x)
head(Med3)

area.cob<-function(x){
  ((Med3$Rad.prom^2)*pi)
}

Med3$Area.cob<-area.cob(x)
head(Med3)

write.csv(Med3, "Med3_Cob.csv")

#¡Bien! Ahora tenemos una nueva variable!!

#Visualicemos

boxplot(Med3$Area.cob~Med3$Tratamiento, main="Medición 3",
        xlab="Tratamiento", ylab="Cobertura (cm2)", notch=T)

boxplot(Med2$Area.cob~Med2$Tratamiento, main="Medición 2",
        xlab="Tratamiento", ylab="Cobertura (cm2)", notch=T)

boxplot(Med1$Area.cob~Med1$Tratamiento, main="Medición 1",
        xlab="Tratamiento", ylab="Cobertura(cm2)", notch=T)


summary(Med1$Area.cob)
summary(Med2$Area.cob)
summary(Med3$Area.cob)

##Tasa de crecimiento relativa
#Hacer las 3 combinaciones posibles

#1con2

merge.1y2<-merge(Med1, Med2, by="Individuo")
head(merge.1y2)

merge.1y2$dif.tallo<-merge.1y2$DiamTallo.y-merge.1y2$DiamTallo.x
head(merge.1y2)

merge.1y2$dif.alt<-merge.1y2$AlturaTallo.y-merge.1y2$AlturaTallo.x

merge.1y2$dif.cob<-merge.1y2$Area.cob.y-merge.1y2$Area.cob.x

str(merge.1y2)
write.csv(merge.1y2, "Merge1y2.csv")

#2con3

merge.2y3<-merge(Med2, Med3, by="Individuo")
head(merge.2y3)

merge.2y3$dif.tallo<-merge.2y3$DiamTallo.y-merge.2y3$DiamTallo.x
head(merge.2y3)

merge.2y3$dif.alt<-merge.2y3$AlturaTallo.y-merge.2y3$AlturaTallo.x

merge.2y3$dif.cob<-merge.2y3$Area.cob.y-merge.2y3$Area.cob.x
head(merge.2y3)

str(merge.2y3)
write.csv(merge.2y3, "Merge2y3.csv")

#1con3

merge.1y3<-merge(Med1, Med3, by="Individuo")
head(merge.1y3)

merge.1y3$dif.tallo<-merge.1y3$DiamTallo.y-merge.1y3$DiamTallo.x
head(merge.1y3)


merge.1y3$dif.alt<-merge.1y3$AlturaTallo.y-merge.1y3$AlturaTallo.x

merge.1y3$dif.cob<-merge.1y3$Area.cob.y-merge.1y3$Area.cob.x

str(merge.1y3)
write.csv(merge.1y3, "Merge1y3.csv")

#USAR COMANDO PARA QUE TODAS LAS DIFERENCIAS NEGATIVAS 
#RESULTADO DE ERRORES EN EL MUESTREO, PASEN A SER CERO

merge.1y2$dif.tallo[merge.1y2$dif.tallo<0]<-0

merge.1y2$dif.alt[merge.1y2$dif.alt<0]<-0

merge.1y2$dif.cob[merge.1y2$dif.cob<0]<-0

head(merge.1y2)

write.csv(merge.1y2, "Merge1y2.csv")

#

merge.2y3$dif.tallo[merge.2y3$dif.tallo<0]<-0

merge.2y3$dif.alt[merge.2y3$dif.alt<0]<-0

merge.2y3$dif.cob[merge.2y3$dif.cob<0]<-0

head(merge.2y3)

write.csv(merge.2y3, "Merge2y3.csv")

#

merge.1y3$dif.tallo[merge.1y3$dif.tallo<0]<-0

merge.1y3$dif.alt[merge.1y3$dif.alt<0]<-0

merge.1y3$dif.cob[merge.1y3$dif.cob<0]<-0

head(merge.1y3)

write.csv(merge.1y3, "Merge1y3.csv")

#

ls()

#

#Plot a través del tiempo

series.tiempo<-merge(merge.1y2, Med3, by="Individuo")
head(series.tiempo)
series.1<-subset(series.tiempo, select=c("Bloque.x","Tratamiento.x",
                                        "Individuo", "AlturaTallo.x", 
                                         "DiamTallo.x", "Area.cob.x",
                                        "AlturaTallo.y", "DiamTallo.y", 
                                        "Area.cob.y", "AlturaTallo",
                                        "DiamTallo", "Area.cob"))
head(series.1)

colnames(series.1)[colnames(series.1)=="AlturaTallo.x"] <- "hTallo.1"
colnames(series.1)[colnames(series.1)=="DiamTallo.x"] <- "DTallo.1"
colnames(series.1)[colnames(series.1)=="Area.cob.x"] <- "ACob.1"
head(series.1)

colnames(series.1)[colnames(series.1)=="AlturaTallo.y"] <- "hTallo.2"
colnames(series.1)[colnames(series.1)=="DiamTallo.y"] <- "DTallo.2"
colnames(series.1)[colnames(series.1)=="Area.cob.y"] <- "ACob.2"
head(series.1)

colnames(series.1)[colnames(series.1)=="AlturaTallo"] <- "hTallo.3"
colnames(series.1)[colnames(series.1)=="DiamTallo"] <- "DTallo.3"
colnames(series.1)[colnames(series.1)=="Area.cob"] <- "ACob.3"
head(series.1)

write.csv(series.1, "Series.tiempo.csv")

#if(series.1$hTallo.3-series.1$DTallo.2-series.1<=0){
#  plot.ts(series.1$hTallo.3, series.1$))
#}#
##Tengo que acomodar mis datos de manera diferente para esto.

##Subset para trabajar solo con las diferencias


head(merge.1y2)

dat.dif.1y2<-subset(merge.1y2, select=c("Bloque.x","Tratamiento.x",
                    "Individuo", "dif.tallo", "dif.alt", "dif.cob"))

colnames(dat.dif.1y2)[colnames(dat.dif.1y2)=="dif.tallo"] <- "df.tallo.12"
colnames(dat.dif.1y2)[colnames(dat.dif.1y2)=="dif.alt"] <- "df.alt.12"
colnames(dat.dif.1y2)[colnames(dat.dif.1y2)=="dif.cob"] <- "df.cob.12"
head(dat.dif.1y2)

#

dat.dif.2y3<-subset(merge.2y3, select=c("Bloque.x","Tratamiento.x",
                                        "Individuo", "dif.tallo", "dif.alt", "dif.cob"))
colnames(dat.dif.2y3)[colnames(dat.dif.2y3)=="dif.tallo"] <- "df.tallo.23"
colnames(dat.dif.2y3)[colnames(dat.dif.2y3)=="dif.alt"] <- "df.alt.23"
colnames(dat.dif.2y3)[colnames(dat.dif.2y3)=="dif.cob"] <- "df.cob.23"
head(dat.dif.2y3)

#

dat.dif.1y3<-subset(merge.1y3, select=c("Bloque.x","Tratamiento.x",
                                        "Individuo", "dif.tallo", "dif.alt", "dif.cob"))
colnames(dat.dif.1y3)[colnames(dat.dif.1y3)=="dif.tallo"] <- "df.tallo.13"
colnames(dat.dif.1y3)[colnames(dat.dif.1y3)=="dif.alt"] <- "df.alt.13"
colnames(dat.dif.1y3)[colnames(dat.dif.1y3)=="dif.cob"] <- "df.cob.13"
head(dat.dif.1y3)

#
#Unir todas las tasas de cambio relativas en un dataframe

dif.merge.1<-merge(dat.dif.1y2, dat.dif.2y3, by="Individuo")
head(dif.merge.1)

dif.merge.tot<-merge(dif.merge.1, dat.dif.1y3, by="Individuo")
head(dif.merge.tot)

write.csv(dif.merge.tot, "TasasDeCambio1.csv")
dif.merge.wk<-read.csv("TasasDeCambio1.csv")

#Ahora hacer subset para limpiarlo

head(dif.merge.wk)
dif.merge.fin<-subset(dif.merge.wk, select = c("Individuo", "Bloque.x.x", 
                                    "Tratamiento.x.x", "df.tallo.12",
                                    "df.alt.12", "df.cob.12",
                                    "df.tallo.13", "df.alt.13", "df.cob.13",
                                    "df.tallo.23", "df.alt.23", "df.cob.23"))
head(dif.merge.fin)

colnames(dif.merge.fin)[colnames(dif.merge.fin)=="Bloque.x.x"] <- "Bloque"
colnames(dif.merge.fin)[colnames(dif.merge.fin)=="Tratamiento.x.x"] <- "Trat"
head(dif.merge.fin)

#Escribir nuevo csv ##OJO NO SON TASAS, SON INCREMENTOS

write.csv(dif.merge.fin, "Incrementos_variables.csv")
inc.trats<-read.csv("Incrementos_variables.csv", header=T)

head(inc.trats)

summary(inc.trats$df.tallo.12~inc.trats$Trat)
summary(inc.trats$df.tallo.23~inc.trats$Trat)
summary(inc.trats$df.tallo.13~inc.trats$Trat)

boxplot(inc.trats$df.tallo.13~inc.trats$Trat, notch=T, xlab="Tratamientos",
        ylab="Cambio en el diámetro (mm)")

boxplot(inc.trats$df.alt.13~inc.trats$Trat, notch=T, xlab="Tratamientos",
        ylab="Cambio en la altura (cm)")

boxplot(t.cambio$df.cob.13~t.cambio$Trat, notch=T, xlab="Tratamientos",
        ylab="Cambio en cobertura (cm2)")

#El cambio en la altura y el cambió en diámetro del tallo son interesantes. 

##PRUEBAS ESTADÍSTICAS PARA DETERMINAR DIFERENCIAS SIGNIFICATIVAS##
##

#Diferencias entre variables de los tratamientos a dos meses desde la siembra
ls()

#Cobertura
boxplot((Med3$Area.cob)~Med3$Tratamiento, notch=T)
bartlett.test(Med3$Area.cob~Med3$Tratamiento)
#Ho se rechaza. No hay homocedasticidad.

#Altura
boxplot(Med3$AlturaTallo~Med3$Tratamiento, notch=T)
bartlett.test(Med3$AlturaTallo~Med3$Tratamiento)
#Ho se rechaza. No hay homocedasticidad.

#Diámetro
boxplot(Med3$DiamTallo~Med3$Tratamiento, notch=T)
bartlett.test(Med3$Area.cob~Med3$Tratamiento)

#Verificar supuestos en los datos
#Hacer del tratamiento un factor
str(Med3)
Med3$Tratamiento<-as.factor(Med3$Tratamiento)
str(Med3)
##El tratamiento ya es un factor.

boxplot(Area.cob~Tratamiento:Bloque, data = Med3)

anova.cob<-aov(Med3$Area.cob~Med3$Tratamiento)
hist(residuals(anova.cob))
shapiro.test(residuals(anova.cob))
summary(anova.cob)
#oJO, PRUEBA shapiro-wilk sugiere que no hay normalidad, pero el histograma
#de los reiduales podría ajustarse

#Prueba post-Hoc

Tukey.test.1<-TukeyHSD(anova.cob)

lsma<-lsmeans(anova.cob, pairwise~Tratamiento, adjust="Tukey")
CLD(lsma, alpha=0.05, Letters=letters)

plot(Tukey.test.1 , las=1 , col="brown" )

multcompBoxplot(Area.cob~Tratamiento, Med3, horizontal=T,
                compFn = "TukeyHSD", 
                sortFn = "mean",
                decreasing = T,
                plotList = list(boxplot=list(fig=c(0,0.75,0,1)),
                                multcompTs=list(fig=c(0.7,0.85,0,1)),
                                multcompLetters=list(fig=c(0.87,0.97,0.03,0.98),
                                                     fontsize = 20,
                                                     fontface = "bold")))
title(ylab = 'Tratamiento', xlab="Cobertura(cm2)")

##ANOVA 2
#DiamTallo

bartlett.test(Med3$DiamTallo~Med3$Tratamiento)
#Hay homocedasticidad

anova.tallo<-aov(Med3$DiamTallo~Med3$Tratamiento)
hist(residuals(anova.tallo))
shapiro.test(residuals(anova.tallo))
#Aparentemente no hay normalidad, ojo

summary(anova.tallo)

TukeyHSD(anova.tallo)
lsm2<-lsmeans(anova.tallo, pairwise~Tratamiento, adjust="Tukey")
CLD(lsm2, alpha=0.05, Letters=letters) 

multcompBoxplot(DiamTallo~Tratamiento, Med3, horizontal=T,
                compFn = "TukeyHSD", 
                sortFn = "mean",
                decreasing = T,
                plotList = list(boxplot=list(fig=c(0,0.75,0,1)),
                                multcompTs=list(fig=c(0.7,0.85,0,1)),
                                multcompLetters=list(fig=c(0.87,0.97,0.03,0.98),
                                                     fontsize = 20,
                                                     fontface = "bold")))

title(ylab = 'Tratamiento', xlab="Diámetro del tallo (mm)")

#Otro Anova para Altura

bartlett.test(Med3$AlturaTallo~Med3$Tratamiento)
#Ho se rechaza, no hay homocedasticidad. Pero ANOVA es robusto.

anova.alt<-aov(Med3$AlturaTallo~Med3$Tratamiento)
hist(residuals(anova.alt))
shapiro.test(residuals(anova.alt))
summary(anova.alt)

TukeyHSD(anova.alt)

lsm3<-lsmeans(anova.alt, pairwise~Tratamiento, adjust="Tukey")
CLD(lsm2, alpha=0.05, Letters=letters) 

multcompBoxplot(AlturaTallo~Tratamiento, Med3, horizontal=T,
                compFn = "TukeyHSD", 
                sortFn = "mean",
                decreasing = T,
                plotList = list(boxplot=list(fig=c(0,0.75,0,1)),
                                multcompTs=list(fig=c(0.7,0.85,0,1)),
                                multcompLetters=list(fig=c(0.87,0.97,0.03,0.98),
                                                     fontsize = 20,
                                                     fontface = "bold")))

title(ylab = 'Tratamiento', xlab="Altura del tallo (cm)")

#Ahora un ANOVA para los incrementos 1y3 
head(inc.trats)

bartlett.test(sqrt(inc.trats$df.tallo.13)~inc.trats$Trat)
#Ho se rechaza, no hay homocedasticidad.
anova.1y3<-aov(sqrt(inc.trats$df.tallo.13)~inc.trats$Trat)
hist(residuals(anova.1y3))
shapiro.test(residuals(anova.1y3))

summary(anova.1y3)

#No hay normalidad en los residuales-no se procede con ANOVA.

summary(inc.trats$df.tallo.13~inc.trats$Trat)
boxplot(inc.trats$df.tallo.13~inc.trats$Trat, xlab="Tratamiento",
        ylab="Incremento de diámetro (mm)", main="Diámetro del tallo")


summary(Med3$AlturaTallo~Med3$Tratamiento)

summary(Med3$DiamTallo~Med3$Tratamiento)

summary(Med3$Area.cob~Med3$Tratamiento)

summary(inc.trats$df.tallo.13~inc.trats$Trat)

summary(inc.trats$df.alt.13~inc.trats$Trat)

summary(inc.trats$df.cob.13~inc.trats$Trat)

#NOTA## A continuación se muestra ANOVA DE BLOQUES 

#ANOVA DE BLOQUES PARA ALTURA

bartlett.test(AlturaTallo~interaction(Tratamiento,Bloque), 
              data = Med3)
#NO SE CUMPLE HOMOCEDASTICIDAD
##REGLA DEL DEDO##
## First, there's a rule of thumb that the ANOVA is robust to heterogeneity of variance so 
#long as the largest variance is not more than 4 times the smallest variance. 
#Furthermore, the general effect of heterogeneity of variance is to make the 
#ANOVA less efficient. That is, you would have lower power. Since you have a 
#significant effect anyway, there is less reason to be concerned here.

anova2v.alt<-aov((AlturaTallo)~Tratamiento*Bloque, data=Med3)
hist(residuals(anova2v.alt))
shapiro.test(residuals(anova2v.alt))
#oJO. NO HAY NORMALIDAD. PROCEDER CON ANOVA????

summary(anova2v.alt)
#EXISTENCIA DE UNA INTERACCIÓN. PARECE QUE LOS BLOQUES TIENEN UN EFECTO 
#IMPORTANTE. #NO INTERPRETAR VARIABLES AISLADAS EN PRESENCIA DE UNA INTE-
#RACCIÓN

interaction.plot(Med3$Tratamiento, Med3$Bloque,
                 Med3$AlturaTallo, type="b", col=c(1:3),
                 leg.bty = "o", leg.bg="beige", lwd=2,
                 pch=c(18,24,22), xlab = "Tratamiento",
                 ylab="Altura tallo (cm)", main = "Interacciones")

#Gráficos

boxplot(Med3$AlturaTallo~Med3$Tratamiento*Med3$Bloque)

#### Parámetros fisiológicos##

PamFis<-read.csv("Fisiol.csv", header = T)
head(PamFis)

PamFis2<-subset(PamFis, select=c("Bloque", "Individuo", "Clorofila_Mmol_m2"))
head(PamFis2)

str(PamFis2)
hist(PamFis2$Clorofila_Mmol_m2)
shapiro.test(PamFis2$Clorofila_Mmol_m2)

boxplot(PamFis2$Clorofila_Mmol_m2~PamFis2$Bloque)

head(PamFis2)
PamFis2$Bloque<-str_split_fixed(PamFis2$Bloque, 
                               "(?<=[A-Za-z][0-9]).(?=[0-9])", 2)
head(PamFis2)
str(PamFis2)

write.csv(PamFis2, "PamFis2_split.csv")

PamFis3<-read.csv("PamFis2_split.csv")
tail(PamFis3)
na.omit(PamFis3)

summary(PamFis3$Clorofila_Mmol_m2~PamFis3$Bloque.1)
boxplot(PamFis3$Clorofila_Mmol_m2~PamFis3$Bloque.1, notch=T, xlab="Bloque",
        ylab="[Clorofila]")
