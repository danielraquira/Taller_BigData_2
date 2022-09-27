################################################
#                 Taller 2                     #
#                                              #
# Colaboradores                                 #
# Karol Gutierrez - 201816712                  #
# Daniel Raquira - 201914059                   #
# Santiago Becerra - 201911587                 #
################################################


## Punto 1 ##
#a)
rm(list=ls())
setwd("C:/Users/Santiago Becerra/Desktop/Santiago/Andes/Materias y Trabajos/Octavo Semestre/Big Data/Problem set 2/Taller_BigData_2/Stores")
require(pacman)
p_load(tidyverse, caret, rio, 
       modelsummary, 
       gamlr,        
       class, skimr,dplyr, glmnet)
library(readxl)
train_hogares <- read_rds("train_hogares.rds")
train_personas <- read_rds("train_personas.rds")
test_hogares <- read_rds("test_hogares.rds")
test_personas <- read_rds("test_personas.rds")

colnames(train_hogares)
colnames(train_personas)

## Creo la variable sum ingresos de las personas del hogar
sum_ingresos<-train_personas %>% group_by(id) %>% summarize(Ingtot_hogar=sum(Ingtot,na.rm = TRUE)) 
summary(sum_ingresos)
## Merge por id para agregarle a la base de train hogares la variable de sum ingresos
train_hogares<-left_join(train_hogares,sum_ingresos)
colnames(train_hogares)