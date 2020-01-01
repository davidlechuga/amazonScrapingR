data <- resultadolimpio
data <- data [,-1]
data <- data [,-3]
View(data)
# Una vez solo nos quedamos con variables numericas , 
# Hacemos el escalado de las variables para que todas tengan media de 0 y varianza de 1 
data <- scale(data)
# Realizamos la funcion Kmeans a nuestro modelo pasandole 3 parametros.
myCluster <- kmeans(data, 3, nstart = 5, iter.max = 30) 
myCluster
# para calcular la suma de distacias de un cluster usamos la suma de aplicar la varianza al conjunto de datos.
wss <- (nrow(data)-1)*sum(apply(data,2,var))
wss
# buqule. Para cada cada numero de cluster calcular el withinss y guardarlo en la variable wss
for (i in 2:20  ) wss[i] <- sum(kmeans(data,centers = i)$withinss)
wss
#plotear elbow graph PARA IDENTIFICAR NUMERO DE CLUSTER CON EL QUE VAMOS A PLOTEAR.
plot(1:20 , wss , type = "b" , xlab = "Numero de clusters", ylab = "Withinss groups")

#########################################################  ##########################
#########YA SABEMOS QUE TRABAJAREMSO CON 8 CLUSTERS VAMOS A CALCULARLOS#############
myCluster <- kmeans(data,8, nstart = 5, iter.max = 30)
myCluster
# Cargar libreria donde estan las  graficos de Radar
library(fmsb)
#partir la visualizacion 2 filas y en cada fila 4 graphos
par(mfrow=c(2,4))
# sacar cada grapho de radar asociada a cada conjunto de datos ($Clustear means:)
# para cada graphica creamos un subconjutno que tiene el valor de la primera fila el : a) valor min. b) valor max. c) valor de los centros para cada una de la variables que teniamos.
dat<- as.data.frame(t(myCluster$centers[1, ]))
dat
# colocamos en el dataFrame los valores maximos , minimos , y el valor de Cluster means de cada fila (8)
dat <- rbind(rep(5,10), rep(-1.5, 10), dat)
dat
radarchart(dat)
# ploteando para cada uno de los cluster sustituyendo los Centros
dat<- as.data.frame(t(myCluster$centers[2, ]))
dat
dat <- rbind(rep(5,10), rep(-1.5, 10), dat)
dat
radarchart(dat)

### 3
dat<- as.data.frame(t(myCluster$centers[3, ]))
dat
dat <- rbind(rep(5,10), rep(-1.5, 10), dat)
dat
radarchart(dat)

### 4
dat<- as.data.frame(t(myCluster$centers[4, ]))
dat
dat <- rbind(rep(5,10), rep(-1.5, 10), dat)
dat
radarchart(dat)

## 5 
dat<- as.data.frame(t(myCluster$centers[5, ]))
dat
dat <- rbind(rep(5,10), rep(-1.5, 10), dat)
dat
radarchart(dat)

## 6
dat<- as.data.frame(t(myCluster$centers[6, ]))
dat
dat <- rbind(rep(5,10), rep(-1.5, 10), dat)
dat
radarchart(dat)

## 7
dat<- as.data.frame(t(myCluster$centers[7, ]))
dat
dat <- rbind(rep(5,10), rep(-1.5, 10), dat)
dat
radarchart(dat)

## 8
dat<- as.data.frame(t(myCluster$centers[8, ]))
dat
dat <- rbind(rep(5,10), rep(-1.5, 10), dat)
dat
radarchart(dat)
## Podemos observar en la grafica de radar para cada una de los cluster,
## el como se caracterizan cada una de las variables ( ejemplo. numero de opiniones en el cluster 5 es mayor que en todos  )
## cada cluster tiene caracteristicas diferentes , de esta forma de agrupan diferentes articulos en diferentes clusters que podemos indentificar o perfilar.
##Kmeans nos ayuda a clusterizar y segmentar para conocer:
## tipo de compras que hacen los clientes,
## tipos de clientes tenemos,
## que tipo de consumo hacer.





