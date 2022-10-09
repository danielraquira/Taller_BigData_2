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
require(pacman)
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
train_personas$P6430 [is.na(train_personas$P6430)] <- 0
sum(is.na(train_personas$P6040))
sum(is.na(train_personas$P6210))
train_personas$P6210 [is.na(train_personas$P6210 )] <- 0
sum(is.na(train_personas$P6800))
train_personas$P6800 [is.na(train_personas$P6800 )] <- 0

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

#Creamos variable Arri
train_hogares <- train_hogares %>%  mutate(Arri = P5130+P5140)
test_hogares <- test_hogares %>%  mutate(Arri = P5130+P5140)

#Cambiamos nombres de variables por simplicidad

train_hogares <- rename(.data = train_hogares, vivienda = P5090)
test_hogares <- rename(.data = test_hogares, vivienda = P5090)
train_hogares <- rename(.data = train_hogares, cuartos = P5000)
test_hogares <- rename(.data = test_hogares, cuartos = P5000)

train_personas <- rename(.data = train_personas, posicion = P6430)
test_personas <- rename(.data = test_personas, posicion = P6430)
train_personas <- rename(.data = train_personas, edad = P6040)
test_personas <- rename(.data = test_personas, edad = P6040)
train_personas <- rename(.data = train_personas, educ = P6210)
test_personas <- rename(.data = test_personas, educ = P6210)
train_personas <- rename(.data = train_personas, horastr = P6800)
test_personas <- rename(.data = test_personas, horastr = P6800)

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



#### PREDICCIÓN INGRESO ####

#Modelo 1 con train_Personas

modelo1 <- lm(Ingtot ~ edad + posicion + educ + horastr, data = train_personas)
summary(modelo1)

predicho_test<-predict(modelo1,newdata=test_personas)

model1 <- train(Ingtot ~ edad + posicion + educ + horastr ,
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

x_train <- model.matrix(Ingtot ~ edad + posicion + educ + horastr, data = train_personas)[, -1]
y_train <- train_personas$Ingtot
y_train = na.omit(y_train) ##igualar dimensiones

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
  labs(title = "Coefs regularización m2") +
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
paste("Mejor valor de lambda + una desviación estandar:", cv_error$lambda.1se)

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

df_coeficientesm2 %>%
  filter(predictor != "(Intercepto)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes modelo2 Ridge") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))

#Modelo 3 con lasso y Personas

x_train <- model.matrix(Ingtot~ edad + posicion + educ + horastr, data = train_personas)
y_train <- train_personas$Ingtot
y_train = na.omit(y_train) ##igualar dimensiones

x_test<- model.matrix( ~ edad + posicion + educ + horastr, data = test_personas)


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
  labs(title = "Coeficientes modelo3 regularización") +
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
paste("Mejor valor de lambda + una desviación estandar:", cv_error$lambda.1se)


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

df_coeficientesm3 %>%
  filter(predictor != "(Intercepto)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes modelo3 Lasso") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))
 #######################################################

ing_predicho<-data.frame()
ing_predicho<-predict(modelo3,newx=x_test)
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
Final$Pobre_ingtot<-ifelse(Final$ingtotper<Final$Lp,1,0)

#Modelo 4  tradicional Hogares

modelo4 <- lm(Ingtotugarr  ~ vivienda + Nper + cuartos + Arri, data = train_hogares)
summary(modelo4)

modelo4 <- train(Ingtotugarr  ~ vivienda + Nper + cuartos + Arri,
                data = train_hogares,
                trControl = trainControl(method = "cv", number = 5), method = "lm")


df_coeficientesm4 <- modelo4$coefficients %>%
  enframe(name = "predictor", value = "coeficiente")

df_coeficientesm4 %>%
  filter(predictor != "(Intercepto)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes modelo tradicional") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 5, angle = 45))

#Modelo 5 hacer ridge con hogares

x_train <- model.matrix(Ingtotugarr  ~ vivienda + Nper + cuartos + Arri, data = train_hogares)[, -1]
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
  filter(predictor != "(Intercepto)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo Ridge") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))

#Modelo 6 hacer un lasso con Hogares****************************************

x_train <- model.matrix(Ingtotugarr  ~ vivienda + Nper + cuartos + Arri, data = train_hogares)[, -1]
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
  labs(title = "Coefs del m6 regularización") +
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
paste("Mejor valor de lambda + una desviación estandar:", cv_error$lambda.1se)


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
  labs(title = "Coefs modelo 6 Lasso") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))

#Modelo 7: Se realiza una mezcla entre base de personas y de hogares para escoger el mejor modelo posible 
## Esto toca analizarlo mayor profundidad cuando corramos los 6 modelos anteriores.
##Ultimo merge
train_general <- merge(train_hogares,train_personas,by="id")
test_general <- merge(test_hogares,test_personas,by="id")

##Comienzo del modelo
x_train <- model.matrix(~ cuartos + Nper + vivienda + educ + posicion, train_general)
y_train <- train_general$Ingtotugarr
x_test<- model.matrix(~  cuartos + Nper + vivienda + educ + posicion, test_general)

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
  labs(title = "Coeficientes m7 regularización") +
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
paste("Mejor valor de lambda + una desviación estandar:", cv_error$lambda.1se)


modelo7 <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  lambda      = cv_error$lambda.min,
  standardize = TRUE
)


df_coeficientesm7 <- coef(modelo7) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

df_coeficientesm7 %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes m7 Lasso") +
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
Definitivo$Pobre_ingtot<-ifelse(Definitivo$ingtotper<Final$Lp,1,0)
write.csv(Final,"C:\\Users\\dani_\\OneDrive\\Escritorio\\Universidad\\Octavo semestre\\Big data\\Taller_BigData_2\\Views\\Resultados.csv", row.names = FALSE)


#¿encontrar FN y FP a partir del m7

train_general <- merge(train_hogares,train_personas,by="id")

set.seed(476)
split1 <- createDataPartition(train_general$Pobre, p = .7)[[1]]
length(split1)

testing <- train_general[-split1,]
training <- train_general[split1,]


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





#### CLASIFICACIÓN ####
# Primero hogares, recordamos que utilizamos variables vivivienda propia (P5090), Arri, P5000 (CUARTOS HOGAR), Nper

######## Lasso-logit ########
X <- model.matrix(Pobre ~ vivienda + cuartos + Arri + Nper, data = training)
Y <- training$Pobre

#lamda por cv 
set.seed(3456)
modelo.cv<- cv.glmnet(X, Y, alpha=1, family = "binomial")  
modelo.cv
plot(modelo.cv)

minimo.l <- modelo.cv$lambda.min
minimo.l

#lasso con lamda mínimo
lasso.modelo <- glmnet(X,Y, 
                      family = "binomial", 
                      alpha=1, 
                      lambda = minimo.l,
                      preProcess= c("center","scale"))
#coeficientes de lasso-reg
lasso.modelo$beta 

#predicciones
x.test <- model.matrix(Pobre ~ vivienda + cuartos + Arri + Nper, data = testing)
lasso.predecido <- predict(lasso.modelo, newx =x.test, type="response")
lasso.predecido
predecir.lasso <- ifelse(lasso.predecido > 0.5, 1, 0)
predecir.lasso

#Performance del modelo
library(ROCR)
lasso_testing<-data.frame(testing,predecir.lasso)
lasso_testing<-rename(lasso_testing, prediccion_lasso =s0)
with(lasso_testing,prop.table(table(Pobre,prediccion_lasso))) #17.6% son falsos negativos

pred_lasso <- prediction(predecir.lasso, lasso_testing$Pobre)
roc_lasso <- performance(pred_lasso,"tpr","fpr")
plot(roc_lasso, main = "ROC curve", colorize = T)
abline(a = 0, b = 1)
auc_lasso <- performance(pred_lasso, measure = "auc")
auc_lasso@y.values[[1]] #AUC=0.621792

confusionMatrix(as.factor(lasso_testing$prediccion_lasso),as.factor(lasso_testing$Pobre)) #Accuracy: 0.7842, sensitivity: 0.2963, specificity: 0.9472

######## Ridge-reg ########
ridge.modelo <- glmnet(x = X, y = Y, alpha = 0, nlambda = 100, standardize = TRUE)
set.seed(321)
ridge.error <- cv.glmnet(x = X, y = Y,alpha = 0, nfolds = 10, type.measure = "mse", standardize  = TRUE)
plot(ridge.error)
ridge_minimo.l<-ridge.error$lambda.min
ridge_minimo.l

#ridge con lambda óptimo
ridge.modelo <- glmnet(x = X, y = Y, alpha = 0,lambda  = ridge_minimo.l, standardize = TRUE)

#Coeficientes ridge-reg
ridge.modelo$beta

#Predicciones
x.test <- model.matrix(Pobre ~ vivienda + cuartos + Arri + Nper, data = testing)
ridge.predecido <- predict(ridge.modelo, newx =x.test,type="response")
ridge.predecido
predecir.ridge <- ifelse(ridge.predecido > 0.5, 1, 0)
predecir.ridge

#Performance del modelo
ridge_testing <- data.frame(testing,predecir.ridge)

ridge_testing <- rename(ridge_testing, prediccion_ridge =s0)
with(ridge_testing,prop.table(table(Pobre,prediccion_ridge))) #20.9% son falsos negativos

pred_ridge <- prediction(as.numeric(predecir.ridge), as.numeric(ridge_testing$Pobre))
roc_ridge <- performance(pred_ridge,"tpr","fpr")
plot(roc_ridge, main = "ROC curve", colorize = T)
abline(a = 0, b = 1)
auc_ridge <- performance(pred_ridge, measure = "auc")
auc_ridge@y.values[[1]] #AUC=0.5664

confusionMatrix(as.factor(ridge_testing$prediccion_ridge),as.factor(ridge_testing$Pobre)) #Accuracy: 0.767, sensitivity: 0.969, specificity: 0.163


######## Logit-reg con lambda = 0 ########
set.seed(4000)
logit.modelo <- glmnet(X,Y, family = "binomial", alpha=1, lambda = 0, preProcess=c("center","scale"))

#Coeficientes
logit.modelo$beta 

#Predicciones
x.test <- model.matrix(Pobre ~ vivienda + cuartos + Arri + Nper, data = testing)
logit.predecido <- predict(logit.modelo, newx =x.test,type="response")
logit.predecido
predecir.logit <- ifelse(logit.predecido > 0.5, 1, 0)
predecir.logit

#Performance del modelo
logit_testing<-data.frame(testing,predecir.logit)

logit_testing<-rename(logit_testing, prediccion_logit =s0)
with(logit_testing,prop.table(table(Pobre, prediccion_logit))) #17.6% son falsos negativos

pred_logit<-prediction(as.numeric(predecir.logit), as.numeric(logit_testing$Pobre))
roc_logit <- performance(pred_logit,"tpr","fpr")
plot(roc_logit, main = "ROC curve", colorize = T)
abline(a = 0, b = 1)
auc_logit <- performance(pred_logit, measure = "auc")
auc_logit@y.values[[1]] ##AUC=0.6217
confusionMatrix(as.factor(logit_testing$prediccion_logit),as.factor(logit_testing$Pobre)) #Accuracy: 0.78, sensitivity: 0.947, specificity: 0.296

##Remuestreo upsampling para lasso##

training$Pobre <- factor(training$Pobre)
set.seed(867)
uptraining <- upSample(x = training, y = training$Pobre, yname = "Pobre")

dim(training)
dim(uptraining)
table(uptraining$Pobre)

up_X<- model.matrix(Pobre ~ vivienda + cuartos + Arri + Nper, data = uptraining)
up_Y<- uptraining$Pobre

#lambda por cv
set.seed(2001)
modelo.cv.up<- cv.glmnet(up_X, up_Y, alpha=1, family = "binomial")  
modelo.cv.up
plot(modelo.cv.up)

minimo.l.up <- modelo.cv.up$lambda.min
minimo.l.up

#lasso con lambda minimo
lasso.modelo.up <- glmnet(up_X,up_Y, 
                       family = "binomial", 
                       alpha=1, 
                       lambda = minimo.l.up,
                       preProcess= c("center","scale"))
#coeficientes
lasso.modelo.up$beta 

#Predicciones
x.test.up <- model.matrix(Pobre ~ vivienda + cuartos + Arri + Nper, data = testing)
lasso.up.predecido <- predict(lasso.modelo.up, newx =x.test.up, type="response")
lasso.up.predecido
predecir.lasso.up <- ifelse(lasso.up.predecido > 0.5, 1, 0)
predecir.lasso.up

#Performance del modelo
lasso.up_testing <- data.frame(testing,predecir.lasso.up)

lasso.up_testing <- rename(lasso.up_testing, prediccion_lasso.up =s0)
with(lasso.up_testing,prop.table(table(Pobre, prediccion_lasso.up))) #Un 6.8% son falsos negativos

pred_lasso.up<-prediction(as.numeric(predecir.lasso.up), as.numeric(lasso.up_testing$Pobre))
roc_lasso.up <- performance(pred_lasso.up,"tpr","fpr")
plot(roc_lasso.up, main = "ROC curve", colorize = T)
abline(a = 0, b = 1)
auc_lasso.up <- performance(pred_lasso.up, measure = "auc")
auc_lasso.up@y.values[[1]] #AUC=0.7196

confusionMatrix(as.factor(lasso.up_testing$prediccion_lasso.up),as.factor(lasso.up_testing$Pobre)) #Accuracy: 0.715, sensitivity: 0.711, specificity: 0.727

##Remuestreo upsampling para ridge##
set.seed(222)
modelo.cv.ridgeup<- cv.glmnet(up_X, up_Y, alpha=0, family = "binomial")  
modelo.cv.ridgeup
plot(modelo.cv.ridgeup)

minimo.l.ridgeup <- modelo.cv.ridgeup$lambda.min
minimo.l.ridgeup 

#ridge con lambda minimo
ridge.modelo.up <- glmnet(up_X,up_Y, family = "binomial", alpha=0, lambda = minimo.l.ridgeup, preProcess= c("center","scale"))
#coeficientes 
ridge.modelo.up$beta 

#predicciones
x.test.ridgeup <- model.matrix(Pobre ~ vivienda + cuartos + Arri + Nper, data = testing)
ridge.up.predecido <- predict(ridge.modelo.up, newx =x.test.ridgeup, type="response")
ridge.up.predecido
predecir.ridge.up <- ifelse(ridge.up.predecido > 0.5, 1, 0)
predecir.ridge.up

#Performance del modelo
ridge.up_testing<-data.frame(testing,predecir.ridge.up)

ridge.up_testing<-rename(ridge.up_testing, prediccion_ridge.up =s0)
with(ridge.up_testing,prop.table(table(Pobre,prediccion_ridge.up))) ##Un 7.8% son falsos negativos

pred_ridge.up<-prediction(as.numeric(predecir.ridge.up), as.numeric(ridge.up_testing$Pobre))
roc_ridge.up <- performance(pred_ridge.up,"tpr","fpr")
plot(roc_ridge.up, main = "ROC curve", colorize = T)
abline(a = 0, b = 1)
auc_ridge.up <- performance(pred_ridge.up, measure = "auc")
auc_ridge.up@y.values[[1]] #AUC=0.6906

confusionMatrix(as.factor(ridge.up_testing$prediccion_ridge.up),as.factor(ridge.up_testing$Pobre)) #Accuracy: 0.693, sensitivity: 0.695, specificity: 0.685

##Remuestreo downsampling para lasso##
set.seed(3720)
downtraining <- downSample(x = training, y = training$Pobre, yname = "Pobre")

dim(training)
dim(downtraining)
table(downtraining$Pobre)

down_X<- model.matrix(Pobre ~ vivienda + cuartos + Arri + Nper, data = downtraining)
down_Y<- downtraining$Pobre

#lambda por cv
set.seed(1111)
modelo.cv.down<- cv.glmnet(down_X, down_Y, alpha=1, family = "binomial")  
modelo.cv.down
plot(modelo.cv.down)

minimo.l.down <- modelo.cv.down$lambda.min
minimo.l.down 

#lasso con lambda mínimo
lasso.model.down <- glmnet(down_X,down_Y, family = "binomial", alpha=1, lambda = minimo.l.down, preProcess= c("center","scale"))
#coeficientes
lasso.model.down$beta 

#Predicciones
x.test.down <- model.matrix(Pobre ~ vivienda + cuartos + Arri + Nper, data = testing)
lasso.down.predecido <- predict(lasso.model.down, newx =x.test.down, type="response")
lasso.down.predecido
predecir.lasso.down <- ifelse(lasso.down.predecido > 0.5, 1, 0)
predecir.lasso.down

#Performance del modelo
lasso.down_testing<-data.frame(testing,predecir.lasso.down)

lasso.down_testing<-rename(lasso.down_testing, prediccion_lasso.down =s0)
with(lasso.down_testing,prop.table(table(Pobre,prediccion_lasso.down))) # 6.81% son falsos negativos

pred_lasso.down<-prediction(as.numeric(predecir.lasso.down), as.numeric(lasso.down_testing$Pobre))
roc_down <- performance(pred_lasso.down,"tpr","fpr")
plot(roc_down, main = "ROC curve", colorize = T)
abline(a = 0, b = 1)
auc_down <- performance(pred_lasso.down, measure = "auc")
auc_down@y.values[[1]] #AUC=0.7193

confusionMatrix(as.factor(lasso.down_testing$prediccion_lasso.down),as.factor(lasso.down_testing$Pobre)) #Accuracy: 0.715, sensitivity: 0.71, specificity: 0.727

##Remuestreo downsampling para ridge##
#Lambda por cv
set.seed(2109)
modelo.cv.ridgedown<- cv.glmnet(down_X, down_Y, alpha=0, family = "binomial")  
modelo.cv.ridgedown
plot(modelo.cv.ridgedown)

minimo.l.ridgedown <- modelo.cv.ridgedown$lambda.min
minimo.l.ridgedown

#ridge con lambda mínimo
ridge.modelo.down <- glmnet(down_X,down_Y, family = "binomial", alpha=0, lambda = minimo.l.ridgedown, preProcess= c("center","scale"))
#coeficientes
ridge.modelo.down$beta

#predicciones
x.test.ridge.down <- model.matrix(Pobre ~ vivienda + cuartos + Arri + Nper, data = testing)
ridge.down.predecido <- predict(ridge.modelo.down, newx =x.test.ridge.down, type="response")
ridge.down.predecido
predecir.ridge.down <- ifelse(ridge.down.predecido > 0.5, 1, 0)
predecir.ridge.down

#Performance del modelo
ridge.down_testing<-data.frame(testing,predecir.ridge.down)

ridge.down_testing<-rename(ridge.down_testing, prediccion_ridge.down =s0)
with(ridge.down_testing,prop.table(table(Pobre,prediccion_ridge.down))) #7.9% son falsos negativos

pred_ridge.down<-prediction(as.numeric(predecir.ridge.down), as.numeric(ridge.down_testing$Pobre))
roc_ridge.down <- performance(pred_ridge.down,"tpr","fpr")
plot(roc_ridge.down, main = "ROC curve", colorize = T)
abline(a = 0, b = 1)
auc_ridge.down <- performance(pred_ridge.down, measure = "auc")
auc_ridge.down@y.values[[1]] #AUC=0.6898

confusionMatrix(as.factor(ridge.down_testing$prediccion_ridge.down),as.factor(ridge.down_testing$Pobre)) #Accuracy: 0.692, sensitivity: 0.695, specificity: 0.684

#### Según los AUC de todos los modelos, el que mejor se desempeña es el modelo upsampling lasso con un AUC de 0.7196, una tasa de falsos positivos de 6.8%, un accuracy de 0.715, sensitivity de 0.711 y specificity de 0.727

## Ahora predecimos con testing hogares##
x.test.final <- model.matrix( ~ vivienda + cuartos + Arri + Nper, data = test_hogares)
lasso.predecido.final <- predict(lasso.modelo.up, newx =x.test.final, type="response")
lasso.predecido.final
predecir.lasso.final <- ifelse(lasso.predecido.final > 0.5, 1, 0)
predecir.lasso.final

#Performance del modelo en base final
lasso.final_testing<-data.frame(test_hogares,predecir.lasso.final)
lasso.final_testing<-rename(lasso.final_testing, prediccion_lasso.final =s0)
#se hace para cada id
pred_mejor.mod<-data.frame(lasso.final_testing$id,lasso.final_testing$prediccion_lasso.final)
pred_mejor.mod<-rename(pred_mejor.mod, Prediccion_pobre = lasso.final_testing.prediccion_lasso.final)
pred_mejor.mod<-rename(pred_mejor.mod, Hogar =lasso.final_testing.id)

#Se vuelven a mostrar resultados del modelo escogido (upsampling lasso)
with(lasso.up_testing,prop.table(table(Pobre,prediccion_lasso.up))) #6.8% son falsos negativos
auc_lasso.up <- performance(pred_lasso.up, measure = "auc")
auc_lasso.up@y.values[[1]] #AUC=0.7196
confusionMatrix(as.factor(lasso.up_testing$prediccion_lasso.up),as.factor(lasso.up_testing$Pobre)) #Accuracy: 0.715, sensitivity: 0.711, specificity: 0.727

#Coeficientes del modelo final
lasso.final_coefs <- coef(lasso.modelo.up) %>% as.matrix() %>% as_tibble(rownames = "predictor") %>% rename(coeficiente = s0) 
lasso.final_coefs

#Gráfica coeficientes
lasso.final_coefs %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) + geom_col() + labs(title = "Coeficientes del modelo Lasso upsampling") + theme_bw() + theme(axis.text.x = element_text(size = 6, angle = 45))

#Grafica predicción con modelo final
library(scales)
ggplot(pred_mejor.mod, aes(x = as.factor(Prediccion_pobre))) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs(title = "Predicción pobreza", y = "Percent", x = "Pobreza")

