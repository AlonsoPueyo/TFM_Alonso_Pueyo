library(readr)
momo <-read_csv('momo.csv')
View(momo)

#Hay datos del 2025. Como estamos estudiando de 2015-2024, borramos esas filas:
momo<-subset(momo, fecha_defuncion<as.Date('2025-01-01'))

library(jsonlite)


###------------

api_key <- "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJhbG9uc29wdWV5bzIzQGdtYWlsLmNvbSIsImp0aSI6ImUwYTYxZjQ1LTJlMTQtNDc4MS1iNTU1LTg5N2RhYTY0ZjBmMSIsImlzcyI6IkFFTUVUIiwiaWF0IjoxNzQ4MjgwNTY4LCJ1c2VySWQiOiJlMGE2MWY0NS0yZTE0LTQ3ODEtYjU1NS04OTdkYWE2NGYwZjEiLCJyb2xlIjoiIn0.3ijCfx5a07iNcnTLvaYC-VKId0h0yCysWBAOE42xi1g"

fecha_inicio <- as.Date("2015-01-01")
fecha_fin <- as.Date("2024-12-31")

  #Creamos secuencia cada 15 días
fechas <- seq(fecha_inicio, fecha_fin, by='15 days')

  #Inicializamos la base de datos vacía
datos_totales <- data.frame()

  #Bucle de descarga cada dos semanas:
for (i in 1:(length(fechas) - 1)) {
  
  ini <- fechas[i]
  fin <- fechas[i + 1] - 1  #restamos 1 día para evitar solapamiento
  
  cat("Descargando del", ini, "al", fin, "...\n")
  
  url_api <- paste0(
    "https://opendata.aemet.es/opendata/api/valores/climatologicos/diarios/datos/fechaini/",
    ini, "T00:00:00UTC/fechafin/", fin, "T23:59:59UTC/todasestaciones/?api_key=", api_key
  )
  
  res <- GET(url_api)
  if (status_code(res) != 200) next
  enlace <- fromJSON(content(res, "text", encoding = "UTF-8"))$datos

  datos_txt <- tryCatch({
    readLines(enlace, encoding = "latin1")
  }, error = function(e) {
    warning(paste("Fallo al leer datos de", ini, "a", fin))
    return(NULL)
  })
  
  if (is.null(datos_txt)) next
  
  datos_json <- fromJSON(paste(datos_txt, collapse = ""))
  
  datos_totales <- rbind(datos_totales, datos_json)
  
  Sys.sleep(3)
}

# Guardamos los datos
saveRDS(datos_totales, "datos_aemet.rds")


#Leemos los datos guardados. Debido a la inestabilidad de los API, al ejecutar
# el bucle, dio error varias veces. Se tuvo que volver a ejecutar para distintas
# fechas, lo que dio lugar a varias bases de datos disjuntas que hay que unir en una.

datos1 <- readRDS("datos_aemet_2016_2025.rds")
datos2 <- readRDS("datos_aemet_2016_2025_1.rds")
datos2.1 <- readRDS("datos_aemet_2016_2025_11.rds")
datos3 <- readRDS("datos_aemet_2016_2025_2.rds")
datos4 <- readRDS("datos_aemet_2016_2025_3.rds")
datos5 <- readRDS("datos_aemet_2016_2025_4.rds")
datos6 <- readRDS("datos_aemet_2016_2025_5.rds")


datosFinales<-rbind(datos1, datos2, datos2.1,  datos3, datos4, datos5, datos6)

#Ya tenemos los datos almacenados. Ahora hay que filtrar las estaciones y las 
#variables que queremos:

datos_filtrados <- datosFinales[, c("fecha", "nombre", "provincia", "tmed", "tmin", "tmax")]
data_base<-datos_filtrados[datos_filtrados$nombre %in% 
    c("VITORIA-GASTEIZ AEROPUERTO", "ALBACETE", "ALICANTE-ELCHE AEROPUERTO", 
      "ALMERÍA AEROPUERTO", "ASTURIAS AEROPUERTO", "ÁVILA", "BADAJOZ AEROPUERTO",
      "PALMA DE MALLORCA, AEROPUERTO", "BARCELONA AEROPUERTO", "BILBAO AEROPUERTO",
      "BURGOS AEROPUERTO", "CÁCERES", "CÁDIZ", "SANTANDER AEROPUERTO", "CASTELLÓ - ALMASSORA",
      "CEUTA", "CIUDAD REAL", "CÓRDOBA AEROPUERTO", "A CORUÑA AEROPUERTO", "CUENCA",
      "GIRONA AEROPUERTO", "GRANADA AEROPUERTO", "GUADALAJARA", "DONOSTIA / SAN SEBASTIÁN AEROPUERTO",
      "HUELVA, RONDA ESTE", "HUESCA, AEROPUERTO", "JAÉN", "LEÓN AEROPUERTO", "LLEIDA",
      "LUGO AEROPUERTO", "MADRID AEROPUERTO", "MÁLAGA AEROPUERTO", "MELILLA",
      "MURCIA AEROPUERTO", "PAMPLONA, AEROPUERTO", "OURENSE", "PALENCIA", "GRAN CANARIA AEROPUERTO",
      "PONTEVEDRA", "LOGROÑO, AEROPUERTO", "SALAMANCA AEROPUERTO", "STA.CRUZ DE TENERIFE",
      "SEGOVIA", "SEVILLA AEROPUERTO", "SORIA", "REUS AEROPUERTO", "TERUEL", "TOLEDO",
      "VALENCIA AEROPUERTO", "VALLADOLID AEROPUERTO", "ZAMORA", "ZARAGOZA, AEROPUERTO"), ]

#-------

#Juntamos la base de datos junto a la de momo:
library(dplyr)
momo$nombre_ambito<-as.factor(momo$nombre_ambito)
data_base$provincia<-as.factor(data_base$provincia)
levels(momo$nombre_ambito)
levels(data_base$provincia)


momo$fecha_defuncion <- as.Date(momo$fecha_defuncion, format = "%Y-%m-%d")
data_base$fecha <- as.Date(data_base$fecha, format = "%Y-%m-%d")

library(stringi)  #Esto es para que los nombres de las provincias sean iguales
                  # en ambas bases de datos. Reescribimos los que no estén igual:
momo$nombre_ambito <- toupper(stri_trans_general(momo$nombre_ambito, "Latin-ASCII"))
data_base$provincia <- toupper(stri_trans_general(data_base$provincia, "Latin-ASCII"))
momo$nombre_ambito<-recode(momo$nombre_ambito, 'ALICANTE/ALACANT'='ALICANTE',
                           'BALEARS, ILLES'='BALEARES', 'CASTELLON/CASTELLO'='CASTELLON',
                           'PALMAS, LAS'='LAS PALMAS', 'VALENCIA/VALENCIA'='VALENCIA',
                           'CORUNA, A'='A CORUNA', 'RIOJA, LA'='LA RIOJA')


momo_provincial<-filter(momo, ambito=='provincia')
momo_provincial_temp<-merge(momo_provincial, data_base, by.x=c('fecha_defuncion', 'nombre_ambito'), by.y=c('fecha', 'provincia'), all.x=TRUE)

saveRDS(momo_provincial_temp, "momo_provincial_temp.rds")
momo_provincial_temp <- readRDS("momo_provincial_temp.rds")
  #Ya tenemos guardados los datos de momo junto a las temperaturas que hizo
  #cada día en cada sitio.
