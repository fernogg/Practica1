library(pacman)
p_load(haven, dplyr, factoextra, FactoMineR, readr, rgl, fpc, psych)
require(psych)
library(haven)
library(readr)
data_pca <- read.csv2("8vo/Computo Cientifico/data_pca.csv")
View(data_pca)


# Normalizar datos
data_pcan <- scale(data_pca[,-16]) #[filas,columnas]

#datos normalizados
View(data_pcan)

#Realizar pca


pca <- princomp(data_pcan)
pca

#diagnóstico
summary(data_pcan)
#hasta el momento se observa que las principales componentes que aportaron mayor varianza son la de la 1 a la 15
#considerar los componentes que tengan proporcion de varianza mayor al 15%
#lo ideal es dejar dos componentes


#revisar varianza y eigenvalores
fviz_eig(pca, choice = "variance")

#Si el determinante de la correlacion tiende a 0 los datos son adecuados para el PCA


fviz_eig(pca, choice = "eigenvalue")
#seis componentes tienen un eigenvalor mayor a la unidad
#lo adecuado es extraer unicamente seis factores

#Análisis gráfico
#El coseno cuadrado se utiliza para medir la calidad de la representación de las variables originales en el espacio de los componentes prncipales.
#Específicamente, el coseno cuadrado de una variable en un componente principal
#es el cuadrado del coseno del angulo entre la variable original y el componente
#principal.

#gráfico de las puntuaciones factoriales y su representación
fviz_pca_ind(pca,
             col.ind = "cos2",
             gradient.cols=c("red", "yellow", "green"),
             repel = FALSE)
#Entre mas verde sea quiere decir que esta mejor representada, mientras que mientras mas rojo sea esta peor representada en esas dos dimensiones.


#Gráfico de las cargas
#¿cuanto contribuye cada variable a las diferentes componentes principales?
#Los componentes contribuyen en diferente medida a los cuadrantes dentro de la representación bidimensional.
fviz_pca_var(pca,
             col.var = "contrib",
             gradient.cols=c("red", "yellow","green"),
             repel = FALSE)
#Son vectores: la magnitud puede cambiar pero el sentido y la dirección se mantienen

#Para visualizar puntuaciones se emplea un biplot
#La flecha indica la direccion en la que contribuyen las variables y donde se ubican los sujetos estudiados
fviz_pca_biplot(pca,
                col.var="red",
                col.ind = "black")

summary(pca)

x11()
psych::cor.plot(data_pcan)

#análisis como lo proporciona spss
#Se debe notar que todas las variables en un pca deben estar altamente
#correlacionadas para que tenga sentido realizarlo
det(cor(data_pcan))

#Resultado del pca rotando los factores
#(más faciles de interpretar los componentes)
#La rotación más común es varimax
#En un pca, los componentes principales iniciales pueden ser dificiles de
#interpretar porque cada variable puede tener cargas significativas en varios componentes. Al aplicar la rotación 
#varimax, se ajustan las cargas de manera que cada variable tenga una carga alta en un solo componente, haciendo que
#la estructura sea más simple y clara

pca2<- psych::principal(data1, nfactors=6, residuals= FALSE, rotate="varimax",
                        scores=TRUE, oblique.scores= FALSE, method="regression",
                        use="pairwise", cor="cor", weight= NULL)
pca2



#Las variables son las siguientes:
pca2$scores
