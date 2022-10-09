
##ESTADISTICAS DESCRIPTIVAS
library(ggplot2)
summary(train_hogares$Ingtotugarr)
summary(train_hogares$Arri)

#Hitograma Ingtotugarr

Ingtotugarr <- ggplot(train_hogares, aes(x = Ingtotugarr)) +
  geom_histogram(bins = 50, fill = "blue") +
  ggtitle("Ingreso del hogar") +
  labs(x = "Ingreso total)", y = "Cantidad") +
  theme_bw()
ggsave("C:/Users/Valeria Gut Borbon/Desktop/Ingtotugarr.png", Ingtotugarr)



#Tablas descriptivas

resumen_p<-summary.data.frame(object = train_personas)
print(resumen_p)
setwd(dir = "C:/Users/Valeria Gut Borbon/Desktop")
write.table(resumen_p, file = "tabla descriptivap.txt, sep = ",", quote = FALSE, row.names = F)

resumen_h<-summary.data.frame(object = train_hogares)
print(resumen_h)
setwd(dir = "C:/Users/Valeria Gut Borbon/Desktop")
write.table(resumen_h, file = "tabla descriptivah.txt", sep = ",", quote = FALSE, row.names = F)

##Graficos de dispersión

#Relación entre la cantidad de personas y el ingreso

ggplot(data = train_hogares,aes(Nper,Ingtotugarr)) + geom_point() + labs(title = "Gráfico de disperción",subtitle = "Número de personas vs Ingreso ", y="Ingreso ", x="Número de personas")




##Relación entre la cantidad de horas trabajadas y el ingreso 

grafico1 <- ggplot(train_personas, aes(x=edad, y=horastr)) +         # capa1: Datos
  geom_bar(stat="identity") +                               # capa2: Forma
  xlab("edad") + ylab(" horas trabajadas") +  # capas 3 y 4: títulos
  ggtitle("Relación entre la cantidad de horas trabajadas y la edad") +
  theme_bw()                                                 # Tema: blanco y negro
  
grafico1



##Relación entre la cantidad de horas trabajadas y la edad. 

grafico1 <- ggplot(train_personas, aes(x=edad, y=horastr)) +         # capa1: Datos
  geom_bar(stat="identity") +                               # capa2: Forma
  xlab("edad") + ylab(" horas trabajadas") +  # capas 3 y 4: títulos
  ggtitle("Relación entre la cantidad de horas trabajadas y la edad") +
  theme_bw()                                                 # Tema: blanco y negro
  
grafico1
