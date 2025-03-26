###Proyecto 
##Cargamos las bases de datos desde el 2020 
Año2020<-read.csv(file.choose())
Año2021<-read.csv(file.choose())
Año2022<-read.csv(file.choose())
Año2023<-read.csv(file.choose())
Año2425<-read.csv(file.choose())
##Juntamos las bases de datos para formar una sola con todos los años 

BaseInicial <- rbind(Año2020,Año2021,Año2022,Año2023,Año2425)

##Vemos que ahora ya contamos con una base con un total de 4414309 datos en 29 variables. 
##Ponemos todas las liberias que ocuparemos 
library(insuranceData)
library(dplyr) 
library(skimr)
library(visdat)
library(janitor)
##Necesitamos ver si hay datos faltantes existentes
is.na(BaseInicial) ##Vemos que este metodo no nos conviene por lo que tenemos que sumar los datos faltantes para saber si sí existen 
sum(is.na(BaseInicial))
###Se puede apreciar que dentro de nuestra base de datos no existe ningun dato faltante,  

##Ahora se visualizaran si existen datos duplicados 
dim(BaseInicial)
BaseInicial%>% 
  janitor::get_dupes() ##Para ver cuantos duplicados hay 
duplicados <- duplicated(BaseInicial)
sum(duplicados)  # Cantidad de filas duplicadas
##Se puede apreciar que no existen datos dupkicados en la base de datos. 

##Duplicados
BaseInicial %>%
  janitor:: get_dupes()


############## EXPLORACIÓN #######################

#Para conocer la estructura del data frame
str(BaseInicial)
summary(BaseInicial) #vista preliminar de las medidas de tendencia basicas de la base de datos
skim(BaseInicial) #resumen mas detallado (histograma, missing values)
head(BaseInicial) #encabezado, primeras 6 filas
tail(BaseInicial) #ultimos valores
dim(BaseInicial) #dimension del data frame

glimpse(BaseInicial) #como esta definida cada variable, valores que puede obtener. Conocer la definicion de las variables. (opcion a str) esta en dplyr

#Nombres de variables
names(BaseInicial)
colnames(BaseInicial)

#Visualizar una muestra de los datos

datos_muestra <- BaseInicial %>% slice_sample(n = 2000) 
vis_dat(datos_muestra)


#Fechas de mayor contagio
#comportamiento del virus
#que tanto influye el sexo, la edad, enfermedades, municipio de residencia, obesidad, tabaquismo
#fecha de defuncion, momento de contagio hasta la muerte
#estuvo intubado antes de morir // intubado y sobrevivió



#############CREACIÓN DE VARIABLE ###########

###Tenemos que crear una nueva variable dicotomica para poder hacer nuestro debido analisis 
##Primero cambiamos la fechqa que viene por default a vacios
BaseInicial$FECHA_DEF <- ifelse(BaseInicial$FECHA_DEF == "9999-99-99", NA, BaseInicial$FECHA_DEF)

# Crear la variable Fallecimiento con "Sí" si hay fecha y "No" si es NA
BaseInicial$FALLECIMIENTO <- ifelse(is.na(BaseInicial$FECHA_DEF), "2", "1")

str(BaseInicial)



###################### CLUSTER ###############


# Cargar paquetes
library(tidyverse)
library(cluster)

# Filtrar y seleccionar variables (incluyendo FALLECIMIENTO)
BaseInicialf <- BaseInicial %>%
  select(SECTOR, ENTIDAD_UM, SEXO, TIPO_PACIENTE, INTUBADO, NEUMONIA, EDAD, EMBARAZO, DIABETES, EPOC, ASMA, INMUSUPR, HIPERTENSION, 
         OTRA_COM, CARDIOVASCULAR, OBESIDAD, RENAL_CRONICA, TABAQUISMO, OTRO_CASO, TOMA_MUESTRA_LAB, TOMA_MUESTRA_ANTIGENO, 
         RESULTADO_ANTIGENO, UCI, FALLECIMIENTO)  # Incluir FALLECIMIENTO

# Convertir variables categóricas a numéricas (si no lo has hecho)
BaseInicialf <- BaseInicialf %>%
  mutate(across(c(SECTOR, ENTIDAD_UM, SEXO, TIPO_PACIENTE, INTUBADO, NEUMONIA, EMBARAZO, DIABETES, EPOC, ASMA, INMUSUPR, HIPERTENSION, 
                  OTRA_COM, CARDIOVASCULAR, OBESIDAD, RENAL_CRONICA, TABAQUISMO, OTRO_CASO, TOMA_MUESTRA_LAB, TOMA_MUESTRA_ANTIGENO, RESULTADO_ANTIGENO, UCI, FALLECIMIENTO), 
                as.numeric))

# Normalizar solo las variables numéricas
BI_scaled <- BaseInicialf %>%
  select(-SECTOR, -ENTIDAD_UM) %>%
  scale()

# Número de clusters
num_clusters <- 50  #

# Aplicar K-Means Clustering
set.seed(123)  # Para reproducibilidad
km_result <- kmeans(BI_scaled, centers = num_clusters, nstart = 10)

# Asignar clústeres a las observaciones originales
BaseInicialf <- BaseInicialf %>%
  mutate(cluster = km_result$cluster)

# Calcular la cantidad de observaciones por cada clúster (según la proporción original)
cluster_counts <- table(BaseInicialf$cluster)
total_observaciones <- 80000

# Calcular la proporción de cada clúster en la base original
cluster_proportions <- cluster_counts / sum(cluster_counts)

# Calcular la cantidad de observaciones que necesitamos de cada clúster
observaciones_por_cluster <- round(cluster_proportions * total_observaciones)

# Crear una nueva base de datos con la muestra representativa de 80,000 observaciones
set.seed(123)  # Para reproducibilidad
expanded_data <- do.call(rbind, lapply(1:num_clusters, function(i) {
  # Filtrar las observaciones del clúster i
  cluster_data <- BaseInicialf %>% filter(cluster == i)
  
  # Muestrear las observaciones de este clúster según la cantidad necesaria
  sampled_data <- cluster_data %>%
    sample_n(observaciones_por_cluster[i])
  
  return(sampled_data)
}))

# Ahora, asegurémonos de que la proporción de "FALLECIMIENTO" en la muestra final se mantenga representativa

# Calcular la proporción de fallecidos en la base original
proporcion_fallecidos <- mean(BaseInicialf$FALLECIMIENTO, na.rm = TRUE)

# Filtrar fallecidos y no fallecidos en la base final
fallecidos <- expanded_data %>% filter(FALLECIMIENTO == 1)
no_fallecidos <- expanded_data %>% filter(FALLECIMIENTO == 2)

# Calcular cuántos fallecidos necesitamos en la muestra final
num_fallecidos <- round(proporcion_fallecidos * total_observaciones)

# Si la cantidad de fallecidos en la muestra es menor o mayor que el número necesario, ajustamos
if (nrow(fallecidos) < num_fallecidos) {
  faltantes_fallecidos <- num_fallecidos - nrow(fallecidos)
  adicionales_fallecidos <- expanded_data %>% filter(FALLECIMIENTO == 1) %>% sample_n(faltantes_fallecidos, replace = TRUE)
  fallecidos <- bind_rows(fallecidos, adicionales_fallecidos)
} else if (nrow(fallecidos) > num_fallecidos) {
  fallecidos <- fallecidos %>% sample_n(num_fallecidos)
}

# Combinar fallecidos y no fallecidos
final_data <- bind_rows(fallecidos, no_fallecidos) %>%
  sample_n(total_observaciones)  # Aleatorizar la muestra final

# Visualizar la base reducida con 80,000 observaciones y proporción de fallecidos representativa
print(head(final_data))

#Una vez teniendo la base reducida, comenzamos a trabajar con ella.

############ Análisis Exploratorio de Datos ############

# Descripción de las variables y su tipo (categóricas, numéricas, etc.)

## Para conocer la estructura del data frame
str(final_data)
summary(final_data) #vista preliminar de las medidas de tendencia basicas de la base de datos
skim(final_data) #resumen mas detallado (histograma, missing values)
head(final_data) #encabezado, primeras 6 filas
tail(final_data) #ultimos valores
dim(final_data) #dimension del data frame

glimpse(final_data) #como esta definida cada variable, valores que puede obtener. Conocer la definicion de las variables. (opcion a str) esta en dplyr

## Nombres de variables
names(final_data)
colnames(final_data)

# Estadísticas descriptivas (media, mediana, moda, desviación estándar, etc.).

#### Moda
get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]  # El valor más frecuente
}

##### Desviación estándar
get_sd <- function(x) {
  sd(x, na.rm = TRUE)  # Calcula la desviación estándar, ignorando los NA
}

###### Calcular la moda y desviación estándar para todas las variables
resultados <- final_data %>%
  summarise(across(everything(), 
                   list(moda = ~get_mode(.), sd = ~get_sd(.)),
                   .names = "{col}_moda_{.fn}"))  # Calcula moda y sd para cada variable

# Mostrar los resultados
print(resultados)



# Visualización de datos (gráficos de barras, histogramas, diagramas de dispersión, etc.).

## TOP 5 ENFERMEDADES MÁS COMUNES EN PACIENTES FALLECIDOS POR COVID

# Seleccionar las variables relacionadas con enfermedades y la variable de fallecimiento
enfermedades <- c("DIABETES", "EPOC", "ASMA", "INMUSUPR", "HIPERTENSION", 
                  "CARDIOVASCULAR", "OBESIDAD", "RENAL_CRONICA", "TABAQUISMO")

# Filtrar los pacientes fallecidos (FALLECIMIENTO == 1)
fallecidos <- final_data %>%
  filter(FALLECIMIENTO == 1)

# Contar la frecuencia de cada enfermedad entre los fallecidos
frecuencias_enfermedades <- fallecidos %>%
  select(all_of(enfermedades)) %>%
  summarise(across(everything(), ~sum(. == 1, na.rm = TRUE)))  # Contar cuántos pacientes tienen cada enfermedad

# Convertir a formato largo para facilitar la ordenación
frecuencias_enfermedades_long <- frecuencias_enfermedades %>%
  pivot_longer(cols = everything(), names_to = "Enfermedad", values_to = "Frecuencia")

# Ordenar por frecuencia descendente y obtener el top 5
top_5_enfermedades <- frecuencias_enfermedades_long %>%
  arrange(desc(Frecuencia)) %>%
  head(5)

# Mostrar el top 5
print(top_5_enfermedades)


#Graficamos

# Instalar y cargar ggplot2 si no lo tienes instalado
if (!require("ggplot2")) install.packages("ggplot2", dependencies=TRUE)
library(ggplot2)

# Crear la gráfica de barras horizontales para el Top 5 de enfermedades
ggplot(top_5_enfermedades, aes(x = reorder(Enfermedad, Frecuencia), y = Frecuencia, fill = Enfermedad)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +  # Para que las barras sean horizontales
  labs(title = "Top 5 Enfermedades en Pacientes Fallecidos por COVID",
       x = "Enfermedad", 
       y = "Frecuencia de Pacientes Fallecidos") +
  theme_minimal() +  # Tema minimalista para hacer la gráfica más limpia
  scale_fill_brewer(palette = "Set3") +  # Colores agradables para las barras
  theme(axis.text = element_text(size = 12, color = "black"),  # Ajustar tamaño y color de texto
        axis.title = element_text(size = 14, color = "black"),  # Ajustar tamaño y color de los títulos de los ejes
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))  # Ajustar título




## NO. DE PERSONAS CON HIPERTENSIÓN RESPECTO A SU EDAD 


# Filtrar los pacientes con hipertensión
pacientes_con_hipertension <- final_data %>%
  filter(HIPERTENSION == 1)



# Crear un boxplot para la distribución de edad en pacientes con hipertensión
ggplot(pacientes_con_hipertension, aes(x = factor(HIPERTENSION), y = EDAD, fill = factor(HIPERTENSION))) +
  geom_boxplot() +
  labs(title = "Distribución de Edad en Pacientes con Hipertensión",
       x = "Hipertensión (1 = Sí)",
       y = "Edad") +
  scale_fill_manual(values = c("blue")) +  # Color personalizado para las cajas
  theme_minimal() +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))


# Fallecimiento con respecto a edad


# Crear un boxplot para la distribución de edad según el fallecimiento
ggplot(final_data, aes(x = factor(FALLECIMIENTO), y = EDAD, fill = factor(FALLECIMIENTO))) +
  geom_boxplot() +
  labs(title = "Distribución de Edad en Pacientes Fallecidos por COVID",
       x = "Fallecimiento (1 = Fallecido)",
       y = "Edad") +
  scale_fill_manual(values = c("skyblue", "salmon")) +  # Colores personalizados
  theme_minimal() +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))


# Instalar y cargar dplyr si no lo tienes instalado
if (!require("dplyr")) install.packages("dplyr", dependencies=TRUE)
library(dplyr)

# Crear una tabla con el conteo de fallecidos por cada edad
fallecidos_por_edad <- final_data %>%
  filter(FALLECIMIENTO == 1) %>%  # Filtrar solo las personas fallecidas
  group_by(EDAD) %>%  # Agrupar por la variable EDAD
  summarise(Cantidad_Fallecidos = n()) %>%  # Contar la cantidad de fallecidos por cada edad
  arrange(EDAD)  # Ordenar por edad

# Mostrar la tabla
print(fallecidos_por_edad)


#Fallecidos por sector

fallecidos_por_sector <- final_data %>%
  filter(FALLECIMIENTO == 1)  # Filtrar solo los fallecidos

# Crear el histograma de fallecidos por sector
ggplot(fallecidos_por_sector, aes(x = factor(SECTOR), fill = factor(SECTOR))) +
  geom_bar() +
  labs(title = "Cantidad de Fallecidos por Sector",
       x = "Sector",
       y = "Cantidad de Fallecidos") +
  scale_fill_brewer(palette = "Set3") +  # Paleta de colores más visual
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Rotar etiquetas del eje X
        axis.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))


#Hombres y mujeres fallecidos por covid

# Filtrar solo los fallecidos
fallecidos_por_sexo <- final_data %>%
  filter(FALLECIMIENTO == 1)  # Filtrar solo fallecidos

# Crear el gráfico de barras
ggplot(fallecidos_por_sexo, aes(x = factor(SEXO), fill = factor(SEXO))) +
  geom_bar() +
  labs(title = "Cantidad de Fallecidos por Sexo",
       x = "Sexo",
       y = "Cantidad de Fallecidos",
       fill = "Sexo") +
  scale_fill_manual(values = c("pink", "blue"), labels = c("Mujeres", "Hombres")) +  # Colores diferenciados
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

# Identificación de valores atípicos y datos faltantes.
#Datos faltantes
is.na(final_data) ##Vemos que este metodo no nos conviene por lo que tenemos que sumar los datos faltantes para saber si sí existen 
sum(is.na(final_data))

#Valores atípicos


# Función para identificar valores atípicos con IQR
detectar_atipicos_IQR <- function(df) {
  df %>%
    summarise(across(everything(), 
                     ~ sum(. < (quantile(., 0.25, na.rm = TRUE) - 1.5 * IQR(., na.rm = TRUE)) | 
                             . > (quantile(., 0.75, na.rm = TRUE) + 1.5 * IQR(., na.rm = TRUE)), na.rm = TRUE), 
                     .names = "Atipicos_{.col}"))
}

# Función para identificar valores atípicos con Z-score
detectar_atipicos_Zscore <- function(df) {
  df %>%
    summarise(across(everything(), 
                     ~ sum((. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE) > 3 | 
                             (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE) < -3, na.rm = TRUE),
                     .names = "Atipicos_Z_{.col}"))
}

# Aplicar ambas funciones a la base de datos
atipicos_IQR <- detectar_atipicos_IQR(final_data)
atipicos_Zscore <- detectar_atipicos_Zscore(final_data)

# Unir los resultados en una tabla final
tabla_atipicos <- bind_rows(atipicos_IQR, atipicos_Zscore)

# Mostrar la tabla con valores atípicos por cada variable numérica
print(tabla_atipicos)

#Solo se encontró un valor atípico con falta de información


#### Regresión
library(dplyr)
library(car)
library(ggplot2)

# Filtrar solo las variables relevantes
Base_regresion <- final_data %>%
  select(FALLECIMIENTO, EDAD, SEXO, INTUBADO, UCI, NEUMONIA, DIABETES, HIPERTENSION, 
         OBESIDAD, EPOC, ASMA, CARDIOVASCULAR, RENAL_CRONICA, TABAQUISMO) %>%
  mutate(
    FALLECIMIENTO = as.factor(FALLECIMIENTO),  # Convertir a factor (0/1)
    SEXO = as.factor(SEXO),
    INTUBADO = as.factor(INTUBADO),
    UCI = as.factor(UCI),
    NEUMONIA = as.factor(NEUMONIA),
    DIABETES = as.factor(DIABETES),
    HIPERTENSION = as.factor(HIPERTENSION),
    OBESIDAD = as.factor(OBESIDAD),
    EPOC = as.factor(EPOC),
    ASMA = as.factor(ASMA),
    CARDIOVASCULAR = as.factor(CARDIOVASCULAR),
    RENAL_CRONICA = as.factor(RENAL_CRONICA),
    TABAQUISMO = as.factor(TABAQUISMO)
  )

# Ajustar el modelo de regresión logística
modelo_logit <- glm(FALLECIMIENTO ~ ., data = Base_regresion, family = binomial(link = "logit"))

# Mostrar resumen del modelo
summary(modelo_logit)

# Calcular y mostrar los Odds Ratios (OR) con intervalos de confianza
exp(cbind(OR = coef(modelo_logit), confint(modelo_logit)))

# Ordenar variables según su impacto en la probabilidad de fallecimiento
OR_values <- exp(coef(modelo_logit))  # Convertir coeficientes a OR
OR_sorted <- sort(OR_values, decreasing = TRUE)

# Crear gráfico de barras con los Odds Ratios más significativos
OR_df <- data.frame(Variable = names(OR_sorted), OR = OR_sorted)
















