#########################  INICIA PREPROCESAMIENTO DE LA RECOPILACION  ##########################
#########################  INICIA PREPROCESAMIENTO DE LA RECOPILACION  ##########################
#########################  INICIA PREPROCESAMIENTO DE LA RECOPILACION  ##########################
##########################                                  ###########################
#############################                              ############################
#################################        PROCESO I      ###############################

aspiradoras <- as.data.frame(res)
View(res)



##############################Procesado de MARCA y funcion ###################################
aspiradoras$Marca <- as.character(aspiradoras$Marca)
aspiradoras$Marca
aspiradoras$Marca <- gsub("-1", "otras", aspiradoras$Marca)
aspiradoras$Marca
class(aspiradoras$Marca)

##############################Procesado de Potencia###################################

aspiradoras$Potencia <- as.character(aspiradoras$Potencia)
aspiradoras$Potencia <- gsub("vatios", "", aspiradoras$Potencia)
aspiradoras$Potencia <- gsub("kW", "", aspiradoras$Potencia)
aspiradoras$Potencia <- gsub("voltios", "", aspiradoras$Potencia)
aspiradoras$Potencia <- gsub("watt_hours", "", aspiradoras$Potencia)
aspiradoras$Potencia <- gsub("watt_hours", "", aspiradoras$Potencia)
aspiradoras$Potencia <- gsub("-1", NA, aspiradoras$Potencia)
aspiradoras$Potencia <- as.numeric(aspiradoras$Potencia)
aspiradoras$Potencia
# Una vez el data frame con clase numerica , se hace un promedio de todas las observaciones de la variable potencia
potenciaMedia <- mean(aspiradoras$Potencia, na.rm = TRUE)
potenciaMedia
#remplazamos los NA por el promedio en potencia de las muestras
aspiradoras$Potencia[is.na(aspiradoras$Potencia)] <- potenciaMedia
aspiradoras$Potencia
class(aspiradoras$Potencia)
hist(aspiradoras$Potencia, breaks = 10, col="grey")
summary(aspiradoras$Potencia)


#############################Procesado de Precio######################################

aspiradoras$Precio <- as.character(aspiradoras$Precio)
aspiradoras$Precio
aspiradoras$Precio <- gsub("€", "", aspiradoras$Precio)
aspiradoras$Precio <- gsub(",", ".", aspiradoras$Precio)
aspiradoras$Precio
class(aspiradoras$Precio)
aspiradoras$Precio <- as.numeric(aspiradoras$Precio)
aspiradoras$Precio
class(aspiradoras$Precio)

# Una vez el data frame con clase numerica , se hace un promedio de todas las observaciones de la variable potencia
precioMedia <- mean(aspiradoras$Precio, na.rm = TRUE)
precioMedia
#remplazamos los NA por el promedio en potencia de las muestras
aspiradoras$Precio[is.na(aspiradoras$Precio)] <- precioMedia
aspiradoras$Precio
hist(aspiradoras$Precio, breaks = 10, col="grey")
summary(aspiradoras$Potencia)


#############################Procesado de Opiniones######################################

aspiradoras$Opiniones <- as.character(aspiradoras$Opiniones)
aspiradoras$Opiniones
aspiradoras$Opiniones <- gsub("valoraciones", "", aspiradoras$Opiniones)
aspiradoras$Opiniones
class(aspiradoras$Opiniones)
aspiradoras$Opiniones <- as.numeric(aspiradoras$Opiniones)
aspiradoras$Opiniones
class(aspiradoras$Opiniones)

# Una vez el data frame con clase numerica , se hace un promedio de todas las observaciones de la variable potencia
opinionesMedia <- mean(aspiradoras$Opiniones, na.rm = TRUE)
opinionesMedia
#remplazamos los NA por el promedio en potencia de las muestras
aspiradoras$Opiniones[is.na(aspiradoras$Opiniones)] <- opinionesMedia
aspiradoras$Opiniones
hist(aspiradoras$Opiniones, breaks = 10, col="grey")
summary(aspiradoras$Opiniones)

#############################Procesado de Dimensiones del Producto######################################
library(stringr)
aspiradoras$`Dimensiones del producto` <- as.character(aspiradoras$`Dimensiones del producto`)
aspiradoras$`Dimensiones del producto` <- gsub("cm", "", aspiradoras$`Dimensiones del producto`)
aspiradoras$`Dimensiones del producto` <- gsub(",", ".", aspiradoras$`Dimensiones del producto`)
aspiradoras$`Dimensiones del producto` <- gsub("-1", NA, aspiradoras$`Dimensiones del producto`)
aspiradoras$`Dimensiones del producto`
#funcion para cortar  donde esta la variabel
dimen <- str_split_fixed(aspiradoras$`Dimensiones del producto`, "x",3)
# Primero Es importante pasarlo como dataFrame para sacar media
dimen<-as.data.frame(dimen)
colnames(dimen)<-c("ancho","alto","profundidad")
dimen
class(dimen)
# Segundo Cambiarlo de tipo dataFrame a Character 
dimen$ancho<-as.character(dimen$ancho)
dimen$alto<-as.character(dimen$alto)
dimen$profundidad<-as.character(dimen$profundidad)
dimen
class(dimen)
#Tercero Cambiarlo de character a numeric
dimen$ancho<-as.numeric(dimen$ancho)
dimen$alto<-as.numeric(dimen$alto)
dimen$profundidad<-as.numeric(dimen$profundidad)
dimen
class(dimen)
#Ahora si calculamos las medias
mediaancho<-mean(dimen$ancho,na.rm = TRUE)
mediaancho
mediaalto<-mean(dimen$alto, na.rm = TRUE)
mediaalto
mediaprofundidad<-mean(dimen$profundidad, na.rm = TRUE)
mediaprofundidad
#y reemplazamos los NA por las medias
dimen$ancho[is.na(dimen$ancho)]<-mediaancho
dimen$alto[is.na(dimen$alto)]<-mediaalto
dimen$profundidad[is.na(dimen$profundidad)]<-mediaprofundidad
#View(dimen)
class(dimen)
View(aspiradoras)
#unimos dataFrames
resultadolimpio<-cbind(aspiradoras,dimen)
#View(resultadolimpio)
#eliminamos variable procesada
resultadolimpio<-resultadolimpio[,-4]
View(resultadolimpio)
names(resultadolimpio)
names (resultadolimpio) = c("Nombre", "Precio", "Opiniones", "Marca", "Potencia", "Ancho", "Alto", "Profundidad")
View(resultadolimpio)
#########################  FIN EL DATA SET ESTA LIMPIO  ##########################
#########################  FIN EL DATA SET ESTA LIMPIO  ##########################
#########################  FIN EL DATA SET ESTA LIMPIO  ##########################
##########################                                  ###########################
##########################                                  ###########################