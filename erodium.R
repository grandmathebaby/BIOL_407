## Sacha Medjo - Spring 25
#Erodium Project
library(tidyverse)
library(car)
library(psych)
library(moments)
library(lmodel2)
library(lme4)
library(pscl)
library(factoextra)
library("pwr")
library(MASS)
library(ggsignif)
library(MoMAColors)
library(lme4)
display.all.moma()

#--Question 1
rm(list=ls())
plant <- read_csv("erodium.csv")
View(plant)
#
Shade <- plant %>%
  filter(treatment == "shade")
Shade
Sun  <- plant %>%
  filter(treatment == "sun")
Sun
#Nb Leaves
meanshade<- mean(Shade$nbleaves)
meansun <- mean(Sun$nbleaves)
SEshade <-sd(Shade$nbleaves) / sqrt(length(Shade$plantnb))
SEsun <-sd(Sun$nbleaves) / sqrt(length(Sun$plantnb))
#Small leaf height
meanshade2<- mean(Shade$heightcm)
meansun2 <- mean(Sun$heightcm)
SEshade2 <-sd(Shade$heightcm) / sqrt(length(Shade$plantnb))
SEsun2 <-sd(Sun$heightcm) / sqrt(length(Sun$plantnb))
#Leaflets large 
meanshade3<- mean(Shade$leaflet)
meansun3 <- mean(Sun$leaflet)
SEshade3 <-sd(Shade$leaflet) / sqrt(length(Shade$plantnb))
SEsun3 <-sd(Sun$leaflet) / sqrt(length(Sun$plantnb))
#Ttest 
mytest1 <- t.test(heightcm~treatment, var.equal=FALSE, data=plant, na.rm=TRUE)
mytest1
#
mytest2 <- t.test(leaflet~treatment, var.equal=FALSE, data=plant, na.rm=TRUE)
mytest2