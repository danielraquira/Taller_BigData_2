################################################
#                 Taller 2                     #
#                                              #
# Colabradores                                 #
# Karol Gutierrez - 201816712                  #
# Daniel Raquira - 201914059                   #
# Santiago Becerra - 201911587                 #
################################################


######### Aspectos relacionados con datos ######
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
train_personas <- read_rds("train_hogares.rds")
test_hogares <- read_rds("test_hogares.rds")
test_personas <- read_rds("test_personas.rds")


