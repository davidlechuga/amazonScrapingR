#########################  INICIA PROCESO DE RECOPILACION  ##########################
#########################  INICIA PROCESO DE RECOPILACION  ##########################
#########################  INICIA PROCESO DE RECOPILACION  ##########################
##########################                                ###########################
#############################                             ###########################
################################# PROCESO I #######################################

############## EXTRAER LA URL DEL PRIMER OBJETO EN LISTA ###########################

library(rvest)

url<-"https://www.amazon.es/s?k=aspiradora&dc&__mk_es_ES=%C3%85M%C3%85%C5%BD%C3%95%C3%91&qid=1577397985&ref=sr_ex_p_89_0"
selector<-"#search > div.s-desktop-width-max.s-desktop-content.sg-row > div.sg-col-20-of-24.sg-col-28-of-32.sg-col-16-of-20.sg-col.s-right-column.sg-col-32-of-36.sg-col-8-of-12.sg-col-12-of-16.sg-col-24-of-28 > div > span:nth-child(5) > div.s-result-list.s-search-results.sg-row > div:nth-child(1) > div > span > div > div > div > div > div:nth-child(2) > div.sg-col-4-of-12.sg-col-8-of-16.sg-col-16-of-24.sg-col-12-of-20.sg-col-24-of-32.sg-col.sg-col-28-of-36.sg-col-20-of-28 > div > div:nth-child(1) > div > div > div:nth-child(1) > h2 > a"
#leer la pagina html
pagina<-read_html(url)
# generar nodo 
nodo<- html_node(pagina, selector)
#Nodo como texto
nodo_texto<-html_text(nodo)
nodo_texto
# Obtener la url del objeto mediante html_attr
nodo_links <- html_attr(nodo, "href")
nodo_links
# URL parseada (concatenada)
urlCompleta <- paste0("www.amazon.es",nodo_links)
urlCompleta



##### MODIFICAR LA URL DE LOS PAGINADORES, CONCATENANDO VECTOR Y SUSTITUYENDO  ######


library(stringr)
#paginacion2 URL
pag <- "s?k=aspiradora&dc&page=2&__mk_es_ES=%C3%85M%C3%85%C5%BD%C3%95%C3%91&qid=1577401880&ref=sr_pg_2"
#vector para usarlo con funciones de stringr
lista_paginas <- c(1:10)
#remplazar un texto especifico para obtener la paginacion.
pag<- str_replace(pag, "page=2", paste0("page=",lista_paginas))
pag
pag<- str_replace(pag, "sr_pg_2", paste0("sr_pg_",lista_paginas))
pag
#Concatenar la siguiente liga a pag.
paginas <- paste0("http://www.amazon.es/", pag)
paginas



#### SE CREA FUNCION PARA EXTRAER URL DE TODOS LOS OBJETOS EN UNA PAGINA (SIN APGINACION)   ######


## la funcion se crea para acceder a todos los objetos de la pagina.
dameLinksPagina <- function (url){
  selector<-"div > div:nth-child(1) > div > div > div:nth-child(1) > h2 > a"
  #leer la pagina html
  pagina<-read_html(url)
  # generar nodo 
  nodo<- html_nodes(pagina, selector)
  #Nodo como texto
  nodo_texto<-html_text(nodo)
  nodo_texto
  # Obtener la url del objeto mediante html_attr
  nodo_links <- html_attr(nodo, "href")
  nodo_links
}
dameLinksPagina(url)
# Obtener las SUB URLS asociadas a Cada Una de las Computadoras.
test <- dameLinksPagina(paginas[1])
test
## VOLVIMOS UNA MATRIZ 
linksAsp <- sapply(paginas,dameLinksPagina)
class(linksAsp)
# 20 entradas correspondiente a las 10 columnas con cada pagina de paginador
dim(linksAsp)
vlink <- as.vector(linksAsp)
length(vlink)
#concatenar los las 20 urls de las 10 paginaciones con https://www.amazon.es/
vlinkAspiradora <- paste0("https://www.amazon.es/", vlink)
vlinkAspiradora


################ EXTRAER INFORMACION DE LOS OBJETOS DE LAS SUB-URL'S ################ 
######################### EXTRAER Titulo del Producto ############################### 

url <- ("https://www.amazon.es/Muzili-Aspiradora-Aspirador-Recargable-Filtraci%C3%B3n/dp/B07R5HCHR1/ref=sr_1_164_sspa?__mk_es_ES=%C3%85M%C3%85%C5%BD%C3%95%C3%91&keywords=aspiradora&qid=1577409616&sr=8-164-spons&psc=1&spLa=ZW5jcnlwdGVkUXVhbGlmaWVyPUExU0tOODZLTzNDSk5IJmVuY3J5cHRlZElkPUEwMDQxNjE3MVlXTUpONjFZV0c4ViZlbmNyeXB0ZWRBZElkPUEwNDIzNTM1MUtWODRXU1ozT1k2MiZ3aWRnZXROYW1lPXNwX2J0ZiZhY3Rpb249Y2xpY2tSZWRpcmVjdCZkb05vdExvZ0NsaWNrPXRydWU=")
nombre <- "#productTitle"
pagina_web <- read_html(url)
nombre_nodo<- html_node(pagina_web, nombre)
nombre_texto <- html_text(nombre_nodo)
nombre_texto
######################### EXTRAER Opiniones del Producto ############################### 

opiniones <- "#acrCustomerReviewText"
opiniones_nodo<- html_node(pagina_web, opiniones)
opiniones_texto <- html_text(opiniones_nodo)
opiniones_texto


######################### EXTRAER Precio del Producto ############################### 

precio <- "#priceblock_ourprice"
precio_nodo<- html_node(pagina_web, precio)
precio_texto <- html_text(precio_nodo)
precio_texto

######################### EXTRAER Tabla de Descripcion del Producto ############################### 

tabla <- "#prodDetails > div > div.column.col1 > div > div.content.pdClearfix > div > div > table"
tabla_nodo<- html_node(pagina_web, tabla)
tabla_tab <- html_table(tabla_nodo)
tabla_tab
class(tabla_tab)
### Modificar el dataFrame var1 col var2 row en un vector
val <- tabla_tab$X2
val
##tranformada de val 
res_tabla <- data.frame(t(val))
res_tabla
tabla_name <- tabla_tab$X1
tabla_name
#colocar los nombres de columnas 
colnames(res_tabla) <- tabla_name
res_tabla
class(res_tabla)
str(res_tabla)
# convertir a character nuestro data frame
resultado_aspiradoras <- c(nombre_texto,precio_texto,opiniones_texto, as.character(res_tabla$`Dimensiones del producto`), as.character(res_tabla$Marca), as.character(res_tabla$Potencia))
resultado_aspiradoras
class(resultado_aspiradoras)
####################################################################################
##########################                          ################################
#############################                   ####################################
################################# PROCESO II #######################################

getArticulo <- function (url){
  #print(url)
  ######################### EXTRAER titulo del Producto ############################### 
  
  nombre <- "#productTitle"
  pagina_web <- read_html(url)
  nombre_nodo<- html_node(pagina_web, nombre)
  nombre_texto <- html_text(nombre_nodo)
  nombre_texto
  ######################### EXTRAER Opiniones del Producto ############################### 
  
  opiniones <- "#acrCustomerReviewText"
  opiniones_nodo<- html_node(pagina_web, opiniones)
  opiniones_texto <- html_text(opiniones_nodo)
  opiniones_texto
  
  
  ######################### EXTRAER Precio del Producto ############################### 
  
  precio <- "#priceblock_ourprice"
  precio_nodo<- html_node(pagina_web, precio)
  precio_texto <- html_text(precio_nodo)
  precio_texto
  
  ######################### EXTRAER Tabla de Descripcion del Producto ############################### 
  
  tabla <- "#prodDetails > div > div.column.col1 > div > div.content.pdClearfix > div > div > table"
  tabla_nodo<- html_node(pagina_web, tabla)
  if (!is.na(tabla_nodo)) {
    tabla_tab <- html_table(tabla_nodo)
    tabla_tab
    class(tabla_tab)
    ### Modificar el dataFrame var1 col var2 row en un vector
    val <- tabla_tab$X2
    val
    ##tranformada de val 
    res_tabla <- data.frame(t(val))
    res_tabla
  
    tabla_name <- tabla_tab$X1
    tabla_name
    #colocar los nombres de columnas 
    colnames(res_tabla) <- tabla_name
    res_tabla
    class(res_tabla)
    str(res_tabla)
  }
  
  col <- c( "Dimensiones del producto", "Marca", "Potencia")
  
  #esta condicional es por si el html no contiene lo que pedimos en su estructura.
  if(length(res_tabla) == 0 ) {
    #NO hay detalles todo a menos -1
    mitab<- data.frame(colnames(col))
    mitab<-rbind(mitab, c("-1","-1","-1","-1"))
    colnames(mitab)<- col
    col
    mitab
  } else {
    #Evaluar cada uno de los atributos (INTERESA O NO NOS INTERESA).
    zero<- matrix("-1", ncol = 4, nrow = 1)
    dfzero<- as.data.frame(zero)
    # poniendole los nombres de col al DATAFRAME
    colnames(dfzero)<- col
    dfzero
    #el Data Frame lo convertimos en character para amnipular la matriz
    
   
    
    dimen<- as.character(res_tabla$`Dimensiones del producto`)
    #condicionales para DIMENSIONES
    if (length(dimen)== 0) dimen <- "-1"
    
    marca<- as.character(res_tabla$`Marca`)
    #condicionales MARCA
    if (length(marca)== 0) marca <- "-1"
    
    potencia<- as.character(res_tabla$`Potencia`)
    #condicionales para POTENCIA
    if (length(potencia)== 0) potencia <- "-1"
    
    ##Asignamos los valores de nuestras variables a nuestro dataFrame
    dfzero
    dfzero$`Dimensiones del producto` <- dimen
    dfzero$`Marca` <- marca
    dfzero$`Potencia`<- potencia
    mitab <- dfzero
    colnames(mitab) <- col
  }
  articulo <- c(nombre_texto,precio_texto,opiniones_texto, as.character(mitab$`Dimensiones del producto`), as.character(mitab$Marca), as.character(mitab$Potencia))
  articulo
}

res<- getArticulo(vlinkAspiradora[4])
res
#volvemos una matrix con saplly para unir las variables con las sub-URL'S
resultado_datos<- sapply(vlinkAspiradora, getArticulo)

class(resultado_datos)
dim(resultado_datos)
View(resultado_datos)
#vista de transformada inversa ( COLUMNAS VARIABLES / FILAS OBSERVACONES)
res<- t(resultado_datos)
View(res)
mis_aspiradoras<- as.data.frame(res)
colnames(res)<- c("Nombre", "Precio", "Opiniones", "Dimensiones del producto", "Marca", "Potencia")
row.names(res)<- c(1:200)
View(res)

#########################  ACABÓ PROCESO DE RECOPILACION  ##########################
#########################  ACABA PROCESO DE RECOPILACION  ##########################
#########################  ACABA PROCESO DE RECOPILACION  ##########################