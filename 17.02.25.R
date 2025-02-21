library(insuranceData)
library(dplyr)
library(skimr)
library(visdat) #visualize
library(ggplot2)
library(shiny)

#activar el data frame de datacar
data(dataCar)

#para conocer la estructura del data frame
str(dataCar)
summary(dataCar) #vista preliminar de las medidas de tendencia basicas de la base de datos
skim(dataCar) #resumen mas detallado (histograma, missing values)
head(dataCar) #encabezado, primeras 6 filas
tail(dataCar) #ultimos valores
dim(dataCar) #dimension del data frame

glimpse(dataCar) #como esta definida cada variable, valores que puede obtener. Conocer la definicion de las variables. (opcion a str) esta en dplyr

#nombres de variables
names(dataCar)
colnames(dataCar)

#valores faltantes
any(is.na(dataCar))
vis_dat(dataCar)
vis_miss(dataCar)

#suma de las polizas con al menos una reclamacion
sum(dataCar$numclaims != 0)

#porcentaje de polizas con al menos una reclamacion
((4624)/(67856))*100

#top 5 de vehiculos con mayor monto de reclamaciones
order(-dataCar$numclaims)

decr<-sort(dataCar$numclaims, decreasing = TRUE)
head(decr)

top5<- dataCar %>%
  group_by(veh_body) %>%
  summarise(totclaims=sum(numclaims, na.rm = TRUE)) %>%
  arrange(desc(totclaims)) %>%
  head(5)

print(top5)

#Gráfica de barras para vehículos y clms
ggplot(top5, aes(x=reorder(veh_body, -totclaims), y=totclaims))+
  geom_bar(stat = "identity", fill="red", color="black")+
  labs(title="Número de Reclamaciones por Tipo de Vehículo",
       x="Tipo de Vehículo",
       y="Total de Reclamaciones")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1) )

#top 10 montos con mayor reclamaciones

top10<- dataCar %>%
  group_by(veh_body) %>%
  summarise(totmont=sum(claimcst0, na.rm = TRUE)) %>%
  arrange(desc(totmont)) %>%
  head(10)

print(top10)


#Gráfica de barras para vehículos y montos
ggplot(top10, aes(x=reorder(veh_body, -totmont), y=totmont))+
  geom_bar(stat = "identity", fill="blue", color="black")+
  labs(title="Monto de Reclamaciones por Tipo de Vehículo",
       x="Tipo de Vehículo",
       y="Monto de Reclamaciones")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1) )


#Análisis de acuerdo al sexo

claims_veh_gen <- dataCar %>%
  group_by(gender, veh_body) %>%
  summarise(totclaims=sum(numclaims)) %>%
  arrange(desc(totclaims))

#Mostrar los resultados
print(claims_veh_gen)

#Crear un gráfico de barras
ggplot(claims_veh_gen, aes(x=reorder(veh_body, -totclaims), y=totclaims, fill=gender)) +
  geom_bar(stat = "identity", position = "dodge")+
  labs(title="Tipos de Vehículos reclamados por Género",
       x="Tipo de Vehículo",
       y="Número de Reclamaciones")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1) )



