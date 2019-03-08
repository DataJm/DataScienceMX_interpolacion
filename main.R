# Establezco directorio de trabajo (sólo con Rstudio)
# install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(sjlabelled)
# https://strengejacke.github.io/sjlabelled/

library(dplyr)
# https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html

library(ggpubr)
# https://rpkgs.datanovia.com/ggpubr/index.html

library(readxl)
# https://readxl.tidyverse.org/

library(sf)
# https://r-spatial.github.io/sf/articles/sf1.html
# https://bookdown.org/robinlovelace/geocompr/

library(leaflet)
# https://leafletjs.com/
# https://rstudio.github.io/leaflet/

library(gstat)

# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 
# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 
# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 
# 
# 2019 - 03 - 07 
# 
# FUENTES DE DATOS Y ANÁLISIS EXPLORATORIO CON DPLYR ----
# Vamos a descargar nuestras fuentes de información, y quedarán en la carpeta "./00data/"
# 
# Base de datos de enigh (Concentrado del hogar):
# 
#   Descarga directa: https://www.inegi.org.mx/contenidos/programas/enigh/nc/2016/microdatos/enigh2016_ns_concentradohogar_sav.zip
#   Página para consulta: https://www.inegi.org.mx/programas/enigh/nc/2016/default.html#Microdatos
#   Descripción de archivos: https://www.inegi.org.mx/contenidos/programas/enigh/nc/2016/doc/702825091996.pdf
#   Reporte para validación:  http://www.beta.inegi.org.mx/contenidos/programas/enigh/nc/2016/doc/presentacion_resultados_enigh2016.pdf
# 
# Censo 2010, manzanas y agebs CDXM
#   Descarga directa: https://www.inegi.org.mx/contenidos/programas/ccpv/2010/microdatos/iter/ageb_manzana/09_distrito_federal_2010_ageb_manzana_urbana_xls.zip
#   Página para consulta: https://www.inegi.org.mx/programas/ccpv/2010/default.html#Microdatos
#   Reporte para validación: https://apps1.semarnat.gob.mx:445/dgeia/compendio_2016/archivos/04_demografia/D1_DEMOGRAF01_02_D.pdf
# 
# Cartografía CDMX:
#   Descarga directa: http://internet.contenidos.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/889463674658/09_ciudaddemexico.zip
#   Página para consulta: https://www.inegi.org.mx/app/biblioteca/ficha.html?upc=889463674658
#   Reporte de consulta: http://www.beta.inegi.org.mx/contenidos/temas/mapas/mg/metadatos/manual_cartografia_censal.pdf
# 
# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 

# Base de datos enigh:
enigh2016 <- sjlabelled::read_spss("./00data/enigh2016_ns_concentradohogar_sav/concentradohogar.sav")

# Usamos el paquete sjlablled para no perder los meta datos asociados a SPSS, por ejemplo, el label...
# No lo usaremos ni será necesario, sin embargo es bueno saber que podemos rescatar esa información y tener
# datos más robustos con información completa.

str(enigh2016$factor)
str(enigh2016$ubica_geo)

head(enigh2016[,1:13])

# Antes que nada, reviso que puedo llegar a los mismos resultados publicados...

# Mi "factor" se lee como  "cuántos hogares representa este registro..."
# Por esta razón, al sumar el total de factor, tengo el total de hogares nacional
# De igual manera, al multiplicar mi "tot_integ" por el "factor" , obtengo el total de integrantes del hogar.

sum(enigh2016$factor)
sum(enigh2016$tot_integ * enigh2016$factor)

# Promedio de integrantes en el hogar...
round(sum(enigh2016$tot_integ * enigh2016$factor) / sum(enigh2016$factor),2)

# Ingreso corriente trimestral
round(sum(enigh2016$ing_cor * enigh2016$factor)/1000,0)

# Ingreso corriente trimestral promedio por hogar
round(sum(enigh2016$ing_cor * enigh2016$factor)/ sum(enigh2016$factor),0) 

# Selecciono las variables que me interesan... convenientemente, las dejo en un vector, así puedo volver y modificarlo
aProyectar <- c('tot_integ','ing_cor')

enigh2016 %>% select_(.dots= c(aProyectar, "folioviv","foliohog","ubica_geo","ageb","factor","tot_integ")) -> enigh2016

head(enigh2016)

# Me quedo con los que pertenecen a CDMX (código de entidad 09). Más adelante veremos como obtener esta información del marco
# geoestadístico:
# "Contiene la ubicación geográfica de la vivienda. 
#  Los dos primeros dígitos representan la clave de la entidad, 
#  los siguientes tres la clave del municipio y los últimos cuatro 
#  la clave de la localidad. 
#  Éstas corresponden al Catálogo de claves de entidades federativas, municipios y localidades, que está disponible en el sitio del INEGI"

head(substr(enigh2016$ubica_geo,1,2))
tail(substr(enigh2016$ubica_geo,1,2))

unique(substr(enigh2016$ubica_geo,1,2))

# Sólo para CDMX

enigh2016df <- enigh2016 %>% 
  mutate(entidad=substr(ubica_geo,1,2)) %>%
  filter(entidad=="09")

# Voy a revisar consistencia de mis resultados...
enigh2016df %>% 
  select_(.dots = c('entidad','factor', aProyectar)) %>% 
  mutate_at(.vars = aProyectar,function(val, x){val*x}, .$factor) %>%
  group_by(entidad) %>%
  summarise_all(funs(sum)) %>% 
  mutate_at(.vars = aProyectar,function(val, x){val/x}, .$factor)

# Por diversión... consigo lo mismo pero para todos los estados... 
enigh2016 %>% 
  mutate(entidad=substr(ubica_geo,1,2)) %>%
  select_(.dots = c('entidad','factor', aProyectar)) %>% 
  mutate_at(.vars = aProyectar,function(val, x){val*x}, .$factor) %>%
  group_by(entidad) %>%
  summarise_all(funs(sum)) %>% 
  mutate_at(.vars = aProyectar,function(val, x){val/x}, .$factor) %>% 
  arrange(desc(ing_cor))

# También por diversión... puedo conseguir una gráfica parecida a la de INEGI?
# Usaré la librería ggpubr

enigh2016 %>% 
  mutate(entidad=substr(ubica_geo,1,2)) %>%
  select_(.dots = c('entidad','factor', aProyectar)) %>% 
  mutate_at(.vars = aProyectar,function(val, x){val*x}, .$factor) %>%
  group_by(entidad) %>%
  summarise_all(funs(sum)) %>% 
  mutate_at(.vars = aProyectar,function(val, x){val/x}, .$factor) %>% 
  arrange(ing_cor) %>% 
  mutate(ing_cor = round(ing_cor/1000,1)) %>% 
  # Aquí está el truco...
  ggbarplot(data = ., x = 'entidad', y = 'ing_cor', orientation = "horiz",fill = 'entidad',label = TRUE, lab.pos = 'in') + rremove('legend')

# Limpio, y elimino para liberar espacio en memoria
enigh2016 <- enigh2016df
rm(enigh2016df)

# Antes de continuar, es buena idea guardar mis resultados:
poblacion2016df <- sum(enigh2016$tot_integ * enigh2016$factor)
viviendas2016df <- sum(enigh2016$factor)

# Por ahora nos quedamos en que está validado mi data frame para seguir trabajando.
# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <-




# Ahora vamos a validar los resultados del CENSO 2010
censo2010 <- "./00data/09_distrito_federal_2010_ageb_manzana_urbana_xls/RESAGEBURB_09XLS10.xls"

# Aqui sólo leo la primera hoja... y veo que efectivamente sólo llego hasta el municipio 014
# censo2010 <- read_xls(censo2010)
# unique(censo2010$MUN)

# Cuantas hojas tengo, y como se llaman
readxl::excel_sheets(censo2010)

censo2010 <- rbind( readxl::read_xls(censo2010, sheet = readxl::excel_sheets(censo2010)[1]),
       readxl::read_xls(censo2010, sheet = readxl::excel_sheets(censo2010)[2]))

# Listo, ya tengo todos mis municipios
unique(censo2010$MUN)

str(censo2010$POBTOT)

censo2010$POBTOT <- as.numeric(censo2010$POBTOT)

sum(censo2010$POBTOT)

# Tomemos un minuto para entender la geografía de INEGI: página 12
# http://www.beta.inegi.org.mx/contenidos/temas/mapas/mg/metadatos/manual_cartografia_censal.pdf

# Los totales son 0000.
# Por ejemplo: Este es mi total de población
# Además inician con la palabra "Total"

# EJERCICIO 1 : ---- 
# Sabiendo esto...cómo puedo limpiar mi tabla? Guardar la nueva tabla como censo2010_b

# FIN EJERCICIO 1 : ---- 
sum(censo2010_b$POBTOT)

# Ahora si, tengo todo lo urbano de la CDMX. La pregunta es ¿dónde quedó lo rural? No está en este archivo...

# Población Rural:
censo2010 %>% 
  filter(MUN=="000") %>% 
  select(POBTOT) - sum(censo2010_b$POBTOT)

# Listo. Tengo validado mi data frame de censo2010. Limpio mis nombres y elimino para liberar memoria.
censo2010 <- censo2010_b
rm(censo2010_b)

# EJERCICIO2 : Construir la CVEGEO completa a nivel AGEB ( ENTIDAD, MUNICIPIO, LOCALIDAD, AGEB) del CENSO2010, ----
# seleccionar solo las columnas de CVEGEO y POBTOT, y agrupo por mi clave AGEB (para tener la población de 2010 por cada AGEB)
# y sumar mi total de población


# FIN EJERCICIO2 ----
# Ahora...tenemos un problema. 
# https://www.inegi.org.mx/programas/enigh/nc/2016/default.html#Microdatos

# "El 25 de septiembre de 2018 se remplazaron los archivos de base de datos, 
# debido a que se suprimen los valores de las variables AGEB y UBICA_GEO de 
# las tablas de VIVIENDAS y CONCENTRADOHOGAR, lo anterior debido a que esta 
# encuesta permite generar estimaciones hasta por entidad federativa. 
# Las solicitudes adicionales de información serán atendidas a través del Laboratorio de Microdatos."

# EJERCICIO 3 ----
# Usar el archivo RDS de "enigh2016_AGEBS" para conseguir la CVEGEO de la enigh2016, y agregarla a nuestro data frame
# Tip, buscar la diferencia entre vivienda y hogar
# http://cuentame.inegi.org.mx/poblacion/hogares.aspx?tema=P
# Una vez realizado, comparar si tenemos los mismos agebs en censo que en enigh

# FIN EJERCICIO 3 ----
# oh oh...
table(enigh2016$CVEGEO %in% censo2010$CVEGEO)
table(censo2010$CVEGEO %in% enigh2016$CVEGEO)

# Claramente tenemos un problema... pensemos...
# La enigh es una muestra. Por tanto, tiene sentido que no todos los agebs del censo estén en la enigh... ok
table(censo2010$CVEGEO %in% enigh2016$CVEGEO)

# Esta tabla significa que hay agebs en la enigh que no pertenecen al censo...
table(enigh2016$CVEGEO %in% censo2010$CVEGEO)

# Vamos a verlos...
enigh2016[!enigh2016$CVEGEO %in% censo2010$CVEGEO,] %>% head()

# En realidad son 4... 
unique(enigh2016$CVEGEO[!enigh2016$CVEGEO %in% censo2010$CVEGEO])
# Tomemos un minuto para recordar cómo se hace la enigh...
enigh2016 %>% group_by(CVEGEO,folioviv) %>% summarise()

# Tomemos un minuto para recordar nuestro objetivo...
# Para el modelo de interpolación, usaremos los valores "declarados" (en lugar de los valores "expandidos") 
# para así tener la proyección (de cada variable de interés) a nivel promedio de cada hogar. Esto significa lo siguiente:
# Si utulizo los valores "expandidos", donde un hogar que gane 40,000 mil pesos lo multiplico por un factor
# de 300, entonces el resultado es el ingreso "expandido" (40000 * 300 ). Hacer interpolación sobre este valor nos llevará a resultados "inflados".
# Por lo tanto usaremos los valores DECLARADOS y su promedio.
# Es decir, de tal manera que en un ageb determinado, yo tendré el ingreso promedio trimestral de 1 hogar sin expandir (~40mil pesos)

# Antes de calcular los promedios por ageb, voy a obtener mis sumas. Ya que estos son mis valores reales "reportados"
enigh2016_sums <- enigh2016 %>% 
  select_(.dots = c('factor', aProyectar)) %>% 
  mutate_at(.vars = aProyectar,function(val, x){val*x}, .$factor) %>%
  summarise_all(funs(sum))

# Mi ingreso promedio trimestral, por ejemplo, sigue igual
enigh2016_sums$ing_cor / enigh2016_sums$factor
enigh2016_sums$tot_integ / enigh2016_sums$factor


enigh2016 <- enigh2016 %>% 
  select_(.dots = c('CVEGEO','factor', aProyectar)) %>% 
  group_by(CVEGEO) %>%
  summarise_all(funs(mean))

# Vale, ahora veo que tengo 4 agebs que no están en el censo. Lo dejaremos así por ahora.
table(enigh2016$CVEGEO %in% censo2010$CVEGEO)

# Es buen momento para guardar el total de población de la cdmx en 2010
poblacion2010df <- sum(censo2010$POBTOT)

# Por diversión, cuántas personas nuevas hay?
poblacion2016df - poblacion2010df

# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 
# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 
# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 
# PAQUETE SF ----
# https://r-spatial.github.io/sf/articles/sf1.html
agebs <- sf::st_read("./00data/09_ciudaddemexico/conjunto de datos/09a.shp")

class(agebs)
# Es un objeto sf

# Hablemos de coordenadas...
# https://epsg.io/4485
# https://epsg.io/4326
head(agebs)
st_crs(agebs)

agebs <- agebs %>% st_transform(4326)
head(agebs)
# Como si tuvieramos un Data Frame, que además se encadena con pipes
nrow(agebs)

# Además plotear es super fácil
# so awesome...
agebs %>% select(CVE_ENT) %>% plot()
agebs %>% select(CVE_ENT,CVE_MUN) %>% plot()
agebs %>% select(CVE_AGEB) %>% plot()

# Mapas de calor by default... noice
agebs %>% mutate(newVar= rnorm(nrow(agebs))) %>% select(newVar) %>% plot()

# La gran mayoría (o todas) de las operaciones geoespaciales que conocemos y usamos, se pueden hacer con sf:

# Podemos hacer el estatal...es importante siempre usar el "summarise" para relaizar el spatial dissolve
estado <- agebs %>% group_by(CVE_ENT) %>% summarise()
plot(estado)

# Crear un mapa de municipios con la cuenta de los agebs
municipios <- agebs %>% group_by(CVE_MUN) %>% summarise(n=n())
municipios %>% plot() 

# Es compatible con los otros paquetes... por ejemplo raster...

  # Vamos a hacer un extent, para el estado de CDMX
  # Lo puedo converir en un spatial
  estado <- estado %>% sf::as_Spatial(from = estado,cast = T)
  sp::plot(estado)
  class(estado)

  # Para esto, primero debo encontrar mis "box boundaries"
  limitesBox <- sp::bbox(estado)
  limitesBox <- raster::extent(x = limitesBox)
  
  limitesBox <- as(limitesBox,"SpatialPolygons")
  sp::proj4string(limitesBox) <- sp::proj4string(estado)
  sp::plot(estado)
  sp::plot(limitesBox,add=T)
  
  # Puedo regresar a sf en cualquier momento...
  estado <- st_as_sf(estado)
  class(estado)
  limitesBox <- st_as_sf(limitesBox)

  plot(limitesBox)
  plot(estado,add=T)
  
  # plot(estado)
  # plot(limitesBox,add=T)
  
# Centroide
estadoCentroide <- estado %>% st_centroid()
plot(limitesBox)
plot(estadoCentroide,add=T)

# Filtrar y centroide...
municipioCentroide <- municipios %>% select(CVE_MUN) %>% filter(CVE_MUN=="003") %>% st_centroid()

# Calcular la distancia entre dos puntos (depende del sistema de coordenadas)
st_distance(estadoCentroide, municipioCentroide)

punto <- st_sfc(st_point(x = c(-99.142027, 19.357418)),crs = st_crs(estadoCentroide))
st_distance(estadoCentroide, punto)

# Convertir en data frame
estado <- estado %>% st_drop_geometry()
municipios <- municipios %>% st_set_geometry(NULL)

class(estado)
class(municipios)

# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 
# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 
# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 
# LEAFLET CON R  ----

# Puntos
leaflet() %>% addTiles()

leaflet() %>% addTiles() %>% addMarkers(data = punto) 
leaflet() %>% addTiles() %>% addMarkers(data = punto) %>% addMarkers(data = estadoCentroide) %>% addMarkers(data = municipioCentroide)

leaflet() %>% addTiles() %>% addMarkers(data = estadoCentroide) %>% addProviderTiles(providers$Stamen.Toner)

leaflet(punto) %>% addTiles() %>% addMarkers()


# Poligonos
leaflet(agebs) %>% addTiles() %>% addPolygons()

municipios <- agebs %>% group_by(CVE_MUN) %>% summarise(n=n())

leaflet() %>% addTiles() %>% addPolygons(data = municipios)

agebs <- agebs %>% mutate(newVar=rnorm(nrow(agebs)))

# Colores

# leaflet::colorBin()
# leaflet::colorFactor()
# leaflet::colorNumeric()
# leaflet::colorQuantile()
# https://rdrr.io/cran/leaflet/man/colorNumeric.html

colores <- colorFactor(palette = "viridis", domain = municipios$CVE_MUN)
leaflet() %>% 
  addTiles() %>%
  addPolygons(data = municipios, stroke = T,smoothFactor = 0.2,fillOpacity = 0.5,color = ~colores(CVE_MUN))



colores <- colorFactor(palette = grDevices::rainbow(16), domain = municipios$CVE_MUN)
leaflet() %>% 
  addTiles() %>%
  addPolygons(data = municipios, stroke = T,smoothFactor = 0.2,fillOpacity = 0.5,color = ~colores(CVE_MUN))


# Leyenda. Notar que pasé los agebs al inicio de mi funcion...
colores <- colorBin(palette = "RdYlBu", domain = agebs$newVar,reverse = T)
leaflet(agebs) %>% 
  addTiles() %>%
  addPolygons(stroke = F,smoothFactor = 0.2,fillOpacity = 1,color = ~colores(newVar)) %>% 
  addLegend("bottomright", pal = colores, values = ~newVar,
          title = "Mi nueva super variable",
          opacity = 1)


# Etiquetas.
etiquetas <- sprintf("<h3>Valor: %s </h3><br> CVEGEO: %s", agebs$newVar, agebs$CVEGEO) %>% lapply(htmltools::HTML)

colores <- colorBin(palette = "viridis", domain = agebs$newVar,reverse = T)
leaflet(agebs) %>% 
  addTiles() %>%
  addPolygons(label = etiquetas, stroke = F,smoothFactor = 0.2,fillOpacity = 0.8,color = ~colores(newVar)) %>% 
  addLegend("bottomright", pal = colores, values = ~newVar,
          title = "Mi nueva super variable",
          opacity = 1)

# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 
# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 
# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 
# DE VUELTA AL PROYECTO ----
rm(punto,municipios,municipioCentroide,limitesBox,etiquetas,estadoCentroide,estado,enigh2016_AGEBS)

# Vale, ya tenemos el mapa de agebs
table(censo2010$CVEGEO %in% agebs$CVEGEO)
table(agebs$CVEGEO %in% censo2010$CVEGEO)

# El problema real de esto es que necesitamos ambas partes de la información.
# Tanto tener los agebs en poligono, como tener la información completa del censo

agebs$newVar <- NULL

# EJERCICIO 4 ----
# Pues hagamos un mapita, con la población de 2010....
# Esto significa, al mapa de agebs, agregarle la información de 2010. Renombar pobtot como pob2010

# FIN EJERCICIO 4 ----

# Tengo el siguiente problema...
poblacion2010df - sum(agebs$pob2010,na.rm=T)

# Me faltan personas :/ 

# Lo correcto es hacer una interpolación, como método para imputar. 
# Para propósitos de este ejercicio, omitiremos esto y asignaremos directamte el valor que falta.
# En mi mapa, sólo es 1 ageb la que no tiene población. ESTO ES ALGO QUE YO DECIDO
summary(agebs$pob2010)
table(is.na(agebs$pob2010))

agebs$pob2010[is.na(agebs$pob2010)] <- poblacion2010df - sum(agebs$pob2010,na.rm=T)

sum(agebs$pob2010)
poblacion2010df

# GENERAMOS LOS NUEVOS INDICADORES ---- 

# Total de personas en 2016, y viviendas 2016...
# 
poblacion2016df
viviendas2016df

# Mi objetivo es, para cada AGEB, darle el valor correspondiente de poblacion y viviendas 2016.
# De qué formas puedo hacer esto?
# 

head(agebs)


# Hay muchos métodos sofisticados... regresión espacial, promedios, bosques? etc...
# Ninguno pega mejor como lo siguiente...

agebs$pob2010dits <- agebs$pob2010 / sum(agebs$pob2010)
sum(agebs$pob2010dits)

agebs$pob2016 <- agebs$pob2010dits * poblacion2016df
agebs$viviendas2016 <- agebs$pob2010dits * viviendas2016df

agebsView <- agebs %>% st_drop_geometry()
View(agebsView)

# Nuestro mayor reto, es que en ningún ageb perdamos población. Si, puede pasar. Pero genera más dudas de las que resuelve...
# El segundo reto son los espacios que no tienen población (parques, cementerios, etc.).
# Por estas razones lo mejor es usar la distribución de población 2010.
# De hecho, pondremos a prueba esto, a través de interpolar el total de integrantes (promedio) de los hogares

agebs$crecimientoPob <- agebs$pob2016-agebs$pob2010

boxplot(agebs$crecimientoPob)
summary(agebs$crecimientoPob)

etiquetas <- sprintf("<h3>Poblacíón: %s </h3><br> CVEGEO: %s", prettyNum(agebs$pob2016,big.mark = ","), agebs$CVEGEO) %>% lapply(htmltools::HTML)
colores <- colorBin(palette = "viridis", domain = agebs$pob2016,reverse = F)
leaflet(agebs) %>% 
  addTiles() %>%
  addPolygons(label = etiquetas, stroke = F,smoothFactor = 0.2,fillOpacity = 0.8,color = ~colores(pob2016)) %>% 
  addLegend("bottomright", pal = colores, values = ~pob2016,
            title = "2016",
            opacity = 1)

agebs$pob2010dits <- NULL
agebs$crecimientoPob <- NULL

# Bien. Pero ahora queremos el ingreso corriente trimestral. Para esto sí necesito algún método de interpolación.
# https://gisgeography.com/inverse-distance-weighting-idw-interpolation/
# distancia inversa ponderada


# poblacion2016df <- sum(enigh2016$tot_integ * enigh2016$factor)
# mean(agebs$pob2016 / agebs$viviendas2016,na.rm = T)

# Vamos a calcular los centroides...
miniAgebs <- agebs

miniAgebs <- miniAgebs %>% left_join(.,enigh2016[,c('CVEGEO',aProyectar)], by=c("CVEGEO"))

# Estos son los agebs donde tengo información. 
plot(miniAgebs[,"ing_cor"],main = "Ingreso Corriente promedio trimestral")

miniAgebs <- miniAgebs %>% st_centroid()

# Separo 
conDatos <- miniAgebs %>% filter(!is.na(tot_integ)) %>% as_Spatial()
sinDatos <- miniAgebs %>% filter(is.na(tot_integ)) %>% as_Spatial()

gstat::idw(formula("ing_cor ~ 1"), conDatos, sinDatos)
gstat::idw(formula("ing_cor ~ 1"), conDatos, sinDatos)$var1.pred

# El default es 2 para la potencia
sinDatos@data[,aProyectar] <- sapply(aProyectar,function(x) idw(eval(parse(text = paste0(x,'~1'))), conDatos, sinDatos)$var1.pred)

miniAgebs <- rbind(conDatos@data,sinDatos@data)

plot(
  left_join(agebs, miniAgebs[,c("CVEGEO","ing_cor")],by=c("CVEGEO"))[,"ing_cor"],
  main = "Ingreso Corriente promedio trimestral"
)

# Lo que tenemos ahora es, para cada ageb, cual es el promedio de "x" por hogar.
# Por ejemplo, en un ageb determinado tenemos 45,175.65 en ingreso corriente, esto significa que en promedio, en ese ageb, cada hogar tiene ese ingreso por trimestre.
# Para quitar el promedio, multiplicamos por el total de hogares. Además, obtenemos su distribución:
miniAgebs <- miniAgebs %>% 
  mutate_at(.vars = aProyectar,function(val, x){val*x}, .$viviendas2016) %>% 
  mutate_at(.vars = aProyectar,function(x){x/sum(x)})

head(miniAgebs)

# Finalmente, calculamos los valores expandidos para el distrito federal y los redistribuiomos para todos los agebs:
for(i in  aProyectar){
  miniAgebs[,i] <- miniAgebs[,i] * enigh2016_sums[,i]
}

#Revisamos consistencia con los resultados oficiales:
sum(miniAgebs$ing_cor) / sum(miniAgebs$viviendas2016)
sum(miniAgebs$tot_integ) / sum(miniAgebs$viviendas2016)

#Guardamos los resultados en el objeto espacial:
agebs <- agebs %>% left_join(.,miniAgebs %>% select_(.dots = c("CVEGEO",aProyectar)), by=c("CVEGEO"))

agebs <- agebs %>% rename(pob2016_estimacion=tot_integ)
head(agebs)
agebsView <- agebs %>% st_drop_geometry()


# Podemos revisar qué tan buena es nuestra estimación...estos no son métodos estadísticos de validación, esto es una exploración visual.
View(agebsView)

plot(agebsView$pob2016,agebsView$pob2016_estimacion)

summary(agebsView$pob2016)
summary(agebsView$pob2016_estimacion)

hist(agebsView$pob2016)
hist(agebsView$pob2016_estimacion)

# Aquí es donde truena...
summary(agebsView$pob2016 - agebsView$pob2010)
summary(agebsView$pob2016_estimacion - agebsView$pob2010)


# EJERCICIO 5 ----
# Vale. Ya pasó lo peor. Ahora preparemos la entrega para el cliente.
# Hay que redondear todos los valores, y dejar la variable de ingreso corriente como promedio por hogar.
# Recordar que debemos usar la variable "aProyectar", es decir, no vale hacer sum(agebs$ing_cor) / sum(agebs$viviendas2016)

# Después, crearemos un mapa de calor que vaya de azul "menor ingreso" a rojo "mayor ingreso"

# FIN EJERCICIO 5 ----

# Ahora si, exportamos el resultado a shapefile
st_write(agebs,dsn = "./01resultados/agebsEstimacion.shp")










