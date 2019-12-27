############## EXTRAER LA URL DEL PRIMERO OBJETO EN LISTA ###########################

library(rvest)
url<- "https://www.amazon.es/s?k=aspiradora&dc&__mk_es_ES=%C3%85M%C3%85%C5%BD%C3%95%C3%91&qid=1577397985&ref=sr_ex_p_89_0"
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

url <- ("https://www.amazon.es/Muzili-Aspiradora-Aspirador-Recargable-Filtraci%C3%B3n/dp/B07R5HCHR1/ref=sr_1_164_sspa?__mk_es_ES=%C3%85M%C3%85%C5%BD%C3%95%C3%91&keywords=aspiradora&qid=1577409616&sr=8-164-spons&psc=1&spLa=ZW5jcnlwdGVkUXVhbGlmaWVyPUExU0tOODZLTzNDSk5IJmVuY3J5cHRlZElkPUEwMDQxNjE3MVlXTUpONjFZV0c4ViZlbmNyeXB0ZWRBZElkPUEwNDIzNTM1MUtWODRXU1ozT1k2MiZ3aWRnZXROYW1lPXNwX2J0ZiZhY3Rpb249Y2xpY2tSZWRpcmVjdCZkb05vdExvZ0NsaWNrPXRydWU=")
nombre <- "#productTitle"
pagina_web <- read_html(url)
nombre_nodo<- html_node(pagina_web, nombre)
nombre_texto <- html_text(nombre_nodo)
nombre_texto
