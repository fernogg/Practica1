library(pacman)
p_load(haven, dplyr, factoextra, FactoMineR, readr, rgl, fpc, psych)
require(psych)
library(haven)
library(readxl)
pob_usa <- read_excel("8vo/Computo Cientifico/PoblacionUSA.xlsm.xlsx")
View(pob_usa)

#Separamos datos
datos_2000 <- pob_usa[c("State","Census Resident Total Population - AB:Qr-1-2000","Resident Total Population Estimate - Jul-1-2000","Net Domestic Migration - Jul-1-2000","Federal/Civilian Movement from Abroad - Jul-1-2000","Net International Migration - Jul-1-2000","Period Births - Jul-1-2000","Period Deaths - Jul-1-2000","Resident Under 65 Population Estimate - Jul-1-2000","Resident 65 Plus Population Estimate - Jul-1-2000","Residual - Jul-1-2000" )]

datos_2001 <- pob_usa[c("State","Resident Total Population Estimate - Jul-1-2001","Net Domestic Migration - Jul-1-2001","Federal/Civilian Movement from Abroad - Jul-1-2001","Net International Migration - Jul-1-2001","Period Births - Jul-1-2001","Period Deaths - Jul-1-2001","Resident Under 65 Population Estimate - Jul-1-2001","Resident 65 Plus Population Estimate - Jul-1-2001","Residual - Jul-1-2001" )]

datos_2000
datos_2001

# Normalizar datos
datos_2000n <- scale(datos_2000[,-1]) #[filas,columnas]
datos_2001n <- scale(datos_2001[,-1])

#datos normalizados
View(datos_2000n)
View(datos_2001n)



#Realizar pca

#Diagnóstico para el PCA
#Calcular factor de adecuación muestral de kaiser-Meyer
psych::KMO(datos_2000n) #KMO > 0.5
psych::KMO(datos_2001n)

#todas las variables poseen una msa mayor a 0.5, por lo que es pertinente el pca
pca01 <- princomp(datos_2000n)
pca02 <- princomp(datos_2001n)

#diagnóstico
summary(pca01)
summary(pca02)

#hasta el momento se observa que las principales componentes que aportaron mayor varianza son la 1 y la 2
#considerar los componentes que tengan proporcion de varianza mayor al 15%
#lo ideal es dejar dos componentes

#varianza son 1 y 2
#2000
#revisar varianza y eigenvalores
fviz_eig(pca01, choice = "variance")

#Si el determinante de la correlacion tiende a 0 los datos son adecuados para el PCA
#Prueba KMO si el MSA >= 0.5 (mediocre pero util), lo ideal es que sea >= 0.6

fviz_eig(pca01, choice = "eigenvalue")
#solo dos componentes tienen un eigenvalor mayor a la unidad
#lo adecuado es extraer unicamente dos factores

#2001
fviz_eig(pca02, choice = "variance")
fviz_eig(pca02, choice = "eigenvalue")


#Análisis gráfico
#El coseno cuadrado se utiliza para medir la calidad de la representación de las variables originales en el espacio de los componentes prncipales.
#Específicamente, el coseno cuadrado de una variable en un componente principal
#es el cuadrado del coseno del angulo entre la variable original y el componente
#principal.

#2000
#gráfico de las puntuaciones factoriales y su representación
fviz_pca_ind(pca01,
             col.ind = "cos2",
             gradient.cols=c("red", "yellow", "green"),
             repel = FALSE)
#Entre mas verde sea quiere decir que esta mejor representada, mientras que mientras mas rojo sea esta peor representada en esas dos dimensiones.
#Las observaciones 28,23,25 y 27 no son tan bien representadas, péro son la minoría.

#Gráfico de las cargas
#¿cuanto contribuye cada variable a las diferentes componentes principales?
#Los componentes contribuyen en diferente medida a los cuadrantes dentro de la representación bidimensional.
fviz_pca_var(pca01,
             col.var = "contrib",
             gradient.cols=c("red", "yellow","green"),
             repel = FALSE)
#Son vectores: la magnitud puede cambiar pero el sentido y la dirección se mantienen

#Para visualizar puntuaciones se emplea un biplot
#La flecha indica la direccion en la que contribuyen las variables y donde se ubican los sujetos estudiados
fviz_pca_biplot(pca01,
                col.var="red",
                col.ind = "black")
pca01$loadings

#2001
fviz_pca_ind(pca02,
             col.ind = "cos2",
             gradient.cols=c("red", "yellow", "green"),
             repel = FALSE)
#Gráfico de cargas
fviz_pca_var(pca02,
             col.var = "contrib",
             gradient.cols=c("red", "yellow","green"),
             repel = FALSE)
#biplot
fviz_pca_biplot(pca02,
                col.var="red",
                col.ind = "black")

pca02$loadings


x11()
psych::cor.plot(datos_2000n)
psych::cor.plot(datos_2001n)

#análisis como lo proporciona spss
#Se debe notar que todas las variables en un pca deben estar altamente
#correlacionadas para que tenga sentido realizarlo
det(cor(datos_2000n))
det(cor(datos_2001n))

#Resultado del pca rotando los factores
#(más faciles de interpretar los componentes)
#La rotación más común es varimax
#En un pca, los componentes principales iniciales pueden ser dificiles de
#interpretar porque cada variable puede tener cargas significativas en varios componentes. Al aplicar la rotación 
#varimax, se ajustan las cargas de manera que cada variable tenga una carga alta en un solo componente, haciendo que
#la estructura sea más simple y clara

pca2000<- psych::principal(datos_2000n, nfactors=2, residuals= FALSE, rotate="varimax",
                        scores=TRUE, oblique.scores= FALSE, method="regression",
                        use="pairwise", cor="cor", weight= NULL)
pca2000


pca2001<- psych::principal(datos_2001n, nfactors=2, residuals= FALSE, rotate="varimax",
                           scores=TRUE, oblique.scores= FALSE, method="regression",
                           use="pairwise", cor="cor", weight= NULL)
pca2001
#Nuevas variables obtenidas, cuya principal característica es que son ortogonales
#es decir, linealmente independientes

#Por lo anterior, un conjunto de 6 variables altamente relacionadas
#se redujo a unicamente dos variables cuya característica es que son
#ortogonales.
#Las variables son las siguientes:
pca2000$scores
pca2001$scores
