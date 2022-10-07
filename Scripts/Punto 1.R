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


#Modelo 1 con train_Personas

modelo1 <- lm(Ingtotugarr ~ P6040 + P6430 + P6210 + P6800, data = train_personas)
summary(modelo1)

predicho_test<-predict(modelo1,newdata=test_personas)

model1 <- train(Ingtotugarr ~ p6430 + P6040 + P6210 +P6800 ,
                # model to fit
                data = train_personas,
                trControl = trainControl(method = "cv", number = 4), method = "lm")

df_coeficientesm1 <- modelo1$coefficients %>%
  enframe(name = "predictor", value = "coeficiente")

df_coeficientesm1 %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes modelo tradicional") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 5, angle = 45))

#Modelo 2 es hacer un ridge con las personas

x_train <- model.matrix(Ingtotugarr ~ P6040 + P6430 + P6210 + P6800, data = train_personas)[, -1]
y_train <- train_personas$Ingtotugarr

#scale(x_train)
model_2_ridge <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 0,
  nlambda     = 100,
  standardize = T
)

regularizacion <- model_2_ridge$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = model_2_ridge$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes en función de la regularización para m2") +
  theme_bw() +
  theme(legend.position = "none")

cv_error <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 0,
  nfolds = 10,
  type.measure = "mse",
  standardize  = T
)

plot(cv_error)
paste("Mejor valor de lambda:", cv_error$lambda.min)
paste("Mejor valor de lambda + 1 desviación estandar:", cv_error$lambda.1se)

modelo2 <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 0,
  lambda      = cv_error$lambda.min,
  standardize = TRUE
)

df_coeficientesm2 <- coef(modelo2) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

df_coeficientes %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes modelo Ridge") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))

#Modelo 3 con lasso y Personas

x_train <- model.matrix(~ P6040 + P6430 + P6210 + P6800, data = train_personas)
y_train <- db_trainper$Ingtot

x_test<- model.matrix(~ P6040 + P6430 + P6210 + P6800, data = test_personas)


modelo_3_lasso <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  nlambda     = 100,
  standardize = TRUE
)

regularizacion <- modelo_3_lasso$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo_3_lasso$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes modelo regularizaci?n") +
  theme_bw() +
  theme(legend.position = "none")


cv_error <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

plot(cv_error)
paste("Mejor valor de lambda:", cv_error$lambda.min)
paste("Mejor valor de lambda + 1 desviación estandar:", cv_error$lambda.1se)


modelo3 <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  lambda      = cv_error$lambda.min,
  standardize = TRUE
)

df_coeficientesm3 <- coef(modelo3) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

df_coeficientes %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes modelo Lasso") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))

ing_predicho<-data.frame()
ing_predicho<-predict(modelo3,newx=xtest)
ingtot_pred<- cbind(ing_predicho, test_personas$id)
ingtot_pred<-as.data.frame(ingtot_pred)
ingtot_pred$s0 <- as.numeric(ingtot_pred$s0)
aggregate<-aggregate(s0 ~ V2, data=ingtot_pred, FUN=sum)

aggregate<-rename(aggregate, ingtot = s0)
aggregate<-rename(aggregate, id = V2)


##Nuevo merge
test_hogares2<-as.data.frame(test_hogares)
Final <- merge(aggregate,test_hogares2,by="id")
Final <- Final %>% mutate(ingtotper=ingtot/Npersug)
Final <- Final[c("id","ingtot","ingtotper","Npersug","Lp")]
total$Pobre_ingtot<-ifelse(total$ingtotper<total$Lp,1,0)

#Modelo 4  tradicional Hogares

modelo4 <- lm(Ingtotugarr  ~ p5090 + Nper + p5000 + Arri, data = train_hogares)
summary(modelo4)

model4 <- train(Ingtotugarr  ~ p5090 + Nper + p5000 + Arri
                # model to fit
                data = train_hogares,
                trControl = trainControl(method = "cv", number = 5), method = "lm")


df_coeficientesm4 <- model4$coefficients %>%
  enframe(name = "predictor", value = "coeficiente")

df_coeficientesm4 %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes modelo tradicional") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 5, angle = 45))

#Modelo 5 hacer ridge con hogares

x_train <- model.matrix(Ingtotugarr  ~ p5090 + Nper + p5000 + Arri, data = train_hogares)[, -1]
y_train <- train_hogares$Ingtotugarr

#scale(x_train)
model_5_ridge <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 0,
  nlambda     = 100,
  standardize = T
)

regularizacion <- model_5_ridge$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = model_5_ridge$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes modelo según regularización") +
  theme_bw() +
  theme(legend.position = "none")

cv_error <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 0,
  nfolds = 10,
  type.measure = "mse",
  standardize  = T
)

plot(cv_error)
paste("Mejor valor de lambda:", cv_error$lambda.min)
paste("Mejor valor de lambda + 1 desviación estandar:", cv_error$lambda.1se)

modelo5 <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 0,
  lambda      = cv_error$lambda.1se,
  standardize = TRUE
)

df_coeficientesm5 <- coef(modelo5) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

df_coeficientesm5 %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo Ridge") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))

#Modelo 6 hacer un lasso con Hogares****************************************

x_train <- model.matrix(Ingtotugarr  ~ p5090 + Nper + p5000 + Arri, data = train_hogares)[, -1]
y_train <- train_hogares$Ingtotugarr

modelo_6_lasso <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  nlambda     = 100,
  standardize = TRUE
)

regularizacion <- modelo_6_lasso$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo_6_lasso$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en funci?n de la regularizaci?n") +
  theme_bw() +
  theme(legend.position = "none")


cv_error <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

plot(cv_error)
paste("Mejor valor de lambda:", cv_error$lambda.min)
paste("Mejor valor de lambda + 1 desviación estandar:", cv_error$lambda.1se)


modelo6 <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  lambda      = cv_error$lambda.1se,
  standardize = TRUE
)

df_coeficientesm6 <- coef(modelo6) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

df_coeficientesm6 %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo Lasso") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))

#Modelo 7: Se realiza una mezcla entre base de personas y de hogares para escoger el mejor modelo posible 
## Esto toca analizarlo mayor profundidad cuando corramos los 6 modelos anteriores.
##Ultimo merge
train_general <- merge(train_hogares,train_personas,by="id")
test_general <- merge(test_hogares,test_personas,by="id")

##Comienzo del modelo
x_train <- model.matrix(~ p6430 + p6040 + p6210 + p5090 + Nper + p6800 + Arri + p5000, train_general)
y_train <- train_general$Ingtotugarr
x_test<- model.matrix(~ p6430 + p6040 + p6210 + p5090 + Nper + p6800 + Arri + p5000, test_general)

modelo_7_lasso <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  nlambda     = 100,
  standardize = TRUE
)


regularizacion <- modelo_7_lasso$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo_7_lasso$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes modelo según regularización") +
  theme_bw() +
  theme(legend.position = "none")


cv_error <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

plot(cv_error)
paste("Mejor valor de lambda:", cv_error$lambda.min)
paste("Mejor valor de lambda + 1 desviaci?n est?ndar:", cv_error$lambda.1se)


modelo7 <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  lambda      = cv_error$lambda.min,
  standardize = TRUE
)


# Coeficientes del modelo
df_coeficientesm7 <- coef(modelo7) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

df_coeficientesm7 %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo Lasso") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))

#Predicciones del Modelo 7

ing_predicho<-data.frame()
ing_predicho<-predict(modelo7,newx=x_test)
length(ing_predicho)
prediccion_ingt<- cbind(ing_predicho, test_general$id)
prediccion_ingt<-as.data.frame(prediccion_ingt)
prediccion_ingt$s0 <- as.numeric(prediccion_ingt$s0)
prediccion_ingt<-aggregate(s0 ~ V2, data=ingtot_pred, FUN=sum)
prediccion_ingt<-rename(prediccion_ingt, ingtot = s0)
prediccion_ingt<-rename(prediccion_ingt, id = V2)
testhogares_prediccion<-as.data.frame(test_hogares)
Definitivo <- merge(prediccion_ingt,testhogares_prediccion,by="id")
Definitivo<- Definitivo %>% mutate(ingtotper=ingtot/Npersug)
Definitivo <- Definitivo[c("id","ingtot","ingtotper","Npersug","Lp")] ####### variables elegidas al final, solo se mantiene id]
Definitivo$Pobre_ingtot<-ifelse(Definitivo$ingtotper<total$Lp,1,0)
write.csv(total,"colocar direccion del nuevo archivo", row.names = FALSE)


#¿encontrar FN y FP a partir del m7

train_general <- merge(train_hogares,train_personas,by="id")

set.seed(476)
split1 <- createDataPartition(train_general$Pobre, p = .7)[[1]]
length(split1)

testing <- train_general[-split1,]
training <- train_general[split1,]

#Modelo 7 Lasso


x_train <- model.matrix(~ p6430 + p6040 + p6210 + p5090 + Nper + p6800 + Arri + p5000, training)
y_train <- training$Ingtotugarr

x_test<- model.matrix(~ p6430 + p6040 + p6210 + p5090 + Nper + p6800 + Arri + p5000, test)


modelo_7_lasso <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  nlambda     = 100,
  standardize = TRUE
)

regularizacion <- modelo_7_lasso$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo_7_lasso$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes modelo según regularización") +
  theme_bw() +
  theme(legend.position = "none")


cv_error <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

plot(cv_error)
paste("Mejor valor de lambda:", cv_error$lambda.min)
paste("Mejor valor de lambda + 1 desviación estandar:", cv_error$lambda.1se)


modelo7 <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  lambda      = cv_error$lambda.min,
  standardize = TRUE
)

# Coeficientes del modelo

df_coeficientesm7 <- coef(modelo7) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

df_coeficientesm7 %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes modelo 7 Lasso") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))


ing_predicho<-data.frame()
ing_predicho<-predict(modelo7,newx=x_test)
length(ing_predicho)

prediccion_test<- cbind(ing_predicho, testing$id, testing$Pobre, testing$Lp, testing$Npersug)
prediccion_test<-as.data.frame(prediccion_test)
prediccion_test$s0 <- as.numeric(prediccion_test$s0)
prediccion_test<-aggregate(s0 ~ V2 + V3 + V4 + V5, data=prediccion_test, FUN=sum)
prediccion_test<-rename(prediccion_test, ingtot = s0)
prediccion_test<-rename(prediccion_test, id = V2)
prediccion_test<-rename(prediccion_test, Pobre = V3)
prediccion_test<-rename(prediccion_test, Lp = V4)
prediccion_test<-rename(prediccion_test, Npersug = V5)
prediccion_test$Npersug <- as.numeric(prediccion_test$Npersug)
prediccion_test<- prediccion_test %>% mutate(ingtotper=ingtot/Npersug)
prediccion_test$Pobre_pred<-ifelse(prediccion_test$ingtotper<prediccion_test$Lp,1,0)

with(prediccion_test,table(Pobre,Pobre_pred))






       



