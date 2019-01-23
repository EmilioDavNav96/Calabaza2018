#Análisis de Germinación"

#Limpiar el ambiente de trabajo 

rm(list=ls())

#Cargar librerias
library(dplyr)
library(tidyr)
library(multcompView)
library(stringr)
library(Hmisc)
library(lsmeans)

#Empezar
dat.germin<-read.csv("Germin.csv")

#Separar la variable x

head(dat.germin)
dat.germin$sep<-str_split_fixed(dat.germin$x, 
                               "(?<=[A-Za-z][0-9]).(?=[0-9])", 2)
head(dat.germin)

write.csv(dat.germin, "DatGerminSep.csv")
dat.germin.sep<-read.csv("DatGerminSep.csv")
dat.germin.sep<-subset(dat.germin.sep, select=c("sep.1", "sep.2", "n_germin"))
dat.germin.sep

#renombrar

colnames(dat.germin.sep)[colnames(dat.germin.sep)=="sep.1"] <- "Trat"
colnames(dat.germin.sep)[colnames(dat.germin.sep)=="sep.2"] <- "Bloque"
head(dat.germin.sep)

write.csv(dat.germin.sep, "DatGerminFinal.csv")
germin<-read.csv("DatGerminFinal.csv")

summary(germin)

ptj<-function(x){
  ((germin$n_germin)*100)/25
}

germin$porcentaje<-ptj(x)
head(germin)

#porcentaje

boxplot(porcentaje~Trat, data=germin, xlab="Tratamiento", 
        ylab="% de Germinación")
hist(log10((germin$porcentaje)/100))

trans1<-function(x){
  asin(sqrt(x/100))
}

germin$trans<-trans1(germin$porcentaje)
head(germin)

hist(germin$trans)
shapiro.test(germin$trans)
#Ho mo se rechaza

bartlett.test(germin$trans~germin$Trat)
#Varianza es homogénea

anova.germ<-aov(germin$trans~germin$Trat)
hist(residuals(anova.germ))
shapiro.test(residuals(anova.germ))
#Se cumple normalidad
summary(anova.germ)
#No existen diferencias significativas entre los tratamientos

anova.germ.2<-aov(trans~Trat:Bloque, data=germin)
hist(residuals(anova.germ.2))
shapiro.test(residuals(anova.germ.2))

summary(anova.germ.2)
#No existen diferencias significativas

summary(germin$porcentaje~germin$Trat)
