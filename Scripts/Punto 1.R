################################################
#                 Taller 2                     #
#                                              #
# Colaboradores                                #
# Karol Gutierrez - 201816712                  #
# Daniel Raquira - 201914059                   #
# Santiago Becerra - 201911587                 #
################################################

## read csv para tener la guia de como se deben entregar las predicciones
rm(list=ls())
require(pacman)
p_load(readr)
file.choose()
ruta_template <- "C:\\Users\\Santiago Becerra\\Desktop\\Santiago\\Andes\\Materias y Trabajos\\Octavo Semestre\\Big Data\\Problem set 2\\Taller_BigData_2\\Stores\\submission_template.csv"
template <- read.csv(ruta_template)


##### Aspectos relacionados con datos ######

rm(list=ls())
setwd("C:/Users/Santiago Becerra/Desktop/Santiago/Andes/Materias y Trabajos/Octavo Semestre/Big Data/Problem set 2/Taller_BigData_2/Stores")
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

#MODIFICAMOS LAS BASES ANTES DE UNIRLAS:

# Omitimos las variables redundantes
vrep <- intersect(colnames(train_personas), colnames(train_hogares))[-1]
train_personas <- train_personas %>% select(-all_of(vrep))

## Merge por id para agregarle a la base de train hogares la variable de sum ingresos
train_hogares<-left_join(train_hogares,sum_ingresos)
colnames(train_hogares)

##estadisticas descriptivas
p_load(ggcorrplot, skimr, stargazer, boot)
skim(train_hogares)
glimpse(train_hogares)

sum(is.na(train_hogares$Ingtotugarr))
sum(is.na(train_hogares$Ingtot_hogar))

# variables X´s <- c("P6430 (POSICION OCUPACIONAL PRIMERA ACTIVIDAD)", "edad (P6040)", "educación (P6210)", "vivivienda propia (P5090)","Nper", "P6800 (horas trabajadas semanalmente)", "Arri", "P5000 (CUARTOS HOGAR")))

#Volvemos 0 los NA de hogares
sum(is.na(train_hogares$P5090))
sum(is.na(train_hogares$P5130))
train_hogares$P5130 [is.na(train_hogares$P5130)] <- 0
sum(is.na(train_hogares$P5140))
train_hogares$P5140 [is.na(train_hogares$P5140 )] <- 0
sum(is.na(train_hogares$Nper))
sum(is.na(train_hogares$P5000))

#Volvemos 0 los NA de personas
sum(is.na(train_personas$P6430))
#Hacemos un "tab" para entender la variable
train_personas %>%
  group_by(P6430) %>%
  summarise(n = n()) %>%
  mutate(
    totalN = (cumsum(n)),
    percent = round((n / sum(n)), 3),
    cumuPer = round(cumsum(freq = n / sum(n)), 3))
train_personas$P6430 [is.na(train_personas$P6430 )] <- 0
sum(is.na(train_personas$P6040))
sum(is.na(train_personas$P6210))
train_personas %>%
  group_by(P6210) %>%
  summarise(n = n()) %>%
  mutate(
    totalN = (cumsum(n)),
    percent = round((n / sum(n)), 3),
    cumuPer = round(cumsum(freq = n / sum(n)), 3))
train_personas$P6210 [is.na(train_personas$P6210 )] <- 0
sum(is.na(train_personas$P6800))
train_personas$P6800 [is.na(train_personas$P6800 )] <- 0

train_hogares <- train_hogares %>% mutate_at(.vars = "P5090", .funs = factor)
train_personas <- train_personas %>% mutate_at(.vars = c("Estrato1", "P6430", "P6210"), .funs = factor)

#Ahora las de test

sum(is.na(test_hogares$P5090))
sum(is.na(test_hogares$P5130))
test_hogares$P5130 [is.na(test_hogares$P5130)] <- 0
sum(is.na(test_hogares$P5140))
test_hogares$P5140 [is.na(test_hogares$P5140 )] <- 0
sum(is.na(test_hogares$Nper))
sum(is.na(test_hogares$P5000))



sum(is.na(test_personas$P6430))
test_personas$P6430 [is.na(test_personas$P6430 )] <- 0
sum(is.na(test_personas$P6040))
sum(is.na(test_personas$P6210))
test_personas$P6210 [is.na(test_personas$P6210 )] <- 0
sum(is.na(test_personas$P6800))
test_personas$P6800 [is.na(test_personas$P6800 )] <- 0

test_hogares <- test_hogares %>% mutate_at(.vars = "P5090", .funs = factor)
test_personas <- test_personas %>% mutate_at(.vars = c("P6430", "P6210"), .funs = factor)

#Finalmente creamos variable Arri
train_hogares <- train_hogares %>%  mutate(Arri = P5130+P5140)
test_hogares <- test_hogares %>%  mutate(Arri = P5130+P5140)


#LUEGO DE LIMPIAR LAS BASES, SE HACE LAS DESCRIPTIVAS


data_plot <- train_hogares %>% select(-c("estrato1", "oficio", "P6020", "P2010", "regSalud", "cotPension"))
corr <- round(cor(data_plot), 1)

ggcorrplot(corr, method = "circle", type = "lower", lab = TRUE) +
  ggtitle("Correlograma de base de ingresos") +
  theme_minimal() +
  theme(legend.position = "none")


train_personas <- train_personas %>%
  filter(Ingtot != "")
train_personas <- train_personas %>%
  filter(P6800 != "")

sum(is.na(db_trainper$Oficio))


#Modelo 1 Tradicional Personas**************

modelo1 <- lm(Ingtot_hogar ~  estrato1 +P6430 + p6040 + P6210 + p5090 + p6800 + p5130 + p5140 + Nper  , data = train_hogares)
summary(modelo1)

#Copiar codigo!

       



