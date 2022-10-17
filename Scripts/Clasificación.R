################################################
#                 Taller 2                     #
#                CLASIFICACIÓN                 #
# Colaboradores                                #
# Karol Gutierrez - 201816712                  #
# Daniel Raquira - 201914059                   #
# Santiago Becerra - 201911587                 #
################################################

require(pacman)
p_load(readr)
file.choose()
ruta_template <- "C:\\Users\\Santiago Becerra\\Desktop\\Santiago\\Andes\\Materias y Trabajos\\Octavo Semestre\\Big Data\\Problem set 2\\Taller_BigData_2\\Stores\\submission_template.csv"
template <- read.csv(ruta_template)

rm(list=ls())
require(pacman)
setwd("C:/Users/Santiago Becerra/Desktop/Santiago/Andes/Materias y Trabajos/Octavo Semestre/Big Data/Problem set 2/Taller_BigData_2/Stores")
p_load(tidyverse, caret, rio, 
       modelsummary, 
       gamlr,        
       class, skimr,dplyr, glmnet)



train_general <- merge(train_hogares,train_personas,by="id")

set.seed(476)
split1 <- createDataPartition(train_general$Pobre, p = .7)[[1]]
length(split1)

testing <- train_general[-split1,]
training <- train_general[split1,]



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

