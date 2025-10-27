install.packages("INLA", repos="https://inla.r-inla-download.org/R/stable")
library(INLA)

###########################################
#PARTE 1: PREPARACIÓN DE LOS DATOS
###########################################

library(dlnm)
library(dplyr)

momo_provincial_temp <- readRDS("momo_provincial_temp.rds")

#Para quitar los NA:
momo_provincial_temp <- momo_provincial_temp[!is.na(momo_provincial_temp$tmed), ]

momo_provincial_temp$tmed <- as.numeric(gsub(",", ".", momo_provincial_temp$tmed))
momo_provincial_temp$tmax <- as.numeric(gsub(",", ".", momo_provincial_temp$tmax))
momo_provincial_temp$tmin <- as.numeric(gsub(",", ".", momo_provincial_temp$tmin))

momo_provincial_temp$cod_sexo<-as.factor(momo_provincial_temp$cod_sexo)
momo_provincial_temp$nombre_sexo<-as.factor(momo_provincial_temp$nombre_sexo)
momo_provincial_temp$cod_gedad<-as.factor(momo_provincial_temp$cod_gedad)
momo_provincial_temp$nombre_gedad<-as.factor(momo_provincial_temp$nombre_gedad)
momo_provincial_temp$nombre_ambito<-as.factor(momo_provincial_temp$nombre_ambito)

#---

momo_provincial_temp$mes<-format(momo_provincial_temp$fecha_defuncion, "%m")
momo_provincial_temp$año<-format(momo_provincial_temp$fecha_defuncion, "%y")
momo_verano24<-subset(momo_provincial_temp, (mes=='07' & año=='24') | (mes=='08' & año=='24'))


load("C:/Users/alons/OneDrive/Escritorio/Máster Modelización Alonso Pueyo/TFM/Trabajo/Revisión/Mortalidad por Provincias/Carto_SpainPROV.Rdata")
Carto_SpainPROV <- as.data.frame(Carto_SpainPROV)
filas<-data.frame(ID.area=c(7,35,38), NAME=c("BALEARES", "LAS PALMAS", "TENERIFE"))
Carto_SpainPROV_Entero <- rbind(Carto_SpainPROV, filas)
Carto_SpainPROV_Entero <- Carto_SpainPROV_Entero[Carto_SpainPROV_Entero$ID.area != 24, ]


setwd("C:/Users/alons/OneDrive/Escritorio/Máster Modelización Alonso Pueyo/TFM/Trabajo/Revisión/Mortalidad por Provincias/SpanishProv/SpanishProv")

library(sf)
library(tidyverse)
library(ggthemes)
library(ggspatial)


mapa <- read_sf("esp_prov.shp")

#En 'mapa$NAME' no hay datos de Ceuta ni de Melilla. Para que salga bien tenemos
# que quitar también las filas correspondientes en la base de datos

momo_verano24<-momo_verano24[momo_verano24$nombre_ambito != 'CEUTA', ]
momo_verano24<-momo_verano24[momo_verano24$nombre_ambito != 'MELILLA', ]


#---

library(spdep)
library(lubridate)

#Generamos una matriz de provincias vecinas:
lista_vecinos <- nb2mat(poly2nb(mapa), style = "B", zero.policy=TRUE)

#Creamos base cruzada para cada provincia:

#Ordenamos por provincias

momo_verano24<-momo_verano24[order(momo_verano24$cod_ine_ambito), ]

for (i in c(1:length(momo_verano24[,1]))) {
  if(momo_verano24$cod_ine_ambito[i]>=24) {
    momo_verano24$cod_ine_ambito[i]<-momo_verano24$cod_ine_ambito[i]-1
  }
}     #Lo anterior lo hacemos porque en ese tiempo León no tiene observaciones.
      # Más tarde, a la hora de dibujar el mapa, esto daba errores fatales.


#Lo cambiamos para que el tiempo y coste computacional no sea tan grande y poder
# ejecutarlo en un periodo asequible:
lista_bases<-lapply(formatC(1:49, width=2, flag="0"), 
                    function(provincia_i) {
                      tmed<-subset(momo_verano24, as.character(cod_ine_ambito)==provincia_i)
                      
                      tmed_valid <- tmed[!is.na(tmed$tmed), ]
                      if (nrow(tmed_valid) == 0) return(NULL)
                      
                      base<-crossbasis(tmed$tmed, lag=30, argvar=list(fun="bs", df=4),
                                       arglag=list(fun="bs", df=4))
                      base
                    })

base<- do.call(rbind, lista_bases)
nrow(base)

saveRDS(base, "base.rds")
base<-readRDS("base.rds") #Lo guardamos.

#---

    #Hacemos el diseño case-crossover:
datos_cco <- momo_verano24
base_cco <- base
rm(base)

#CASE-CROSSOVER DESIGN:

#Crear strata para el case-crossover (provincia-año-mes-día de la semana):
datos_cco$strata<-paste(datos_cco$cod_ine_ambito, 
                        year(datos_cco$fecha_defuncion), 
                        formatC(month(datos_cco$fecha_defuncion), width = 2, flag = "0"),
                        wday(datos_cco$fecha_defuncion, week_start = 1),
                        sep = ":")
#En vez de modelar la serie temporal entera, lo que hace este diseño es
# emparejar cada día con otros parecidos (similar estacionalidad y tendencia)
# pero distinta exposición.

#Lo de strata se usa para definir los conjuntos de "control". Se asegura que
# sea de la misma provincia, dentro del mismo año y mes y el mismo día de la semana

#---

#Se excluyen estratos sin casos ya que no contribuyen al vecindario (provincia).

datos_cco$defunciones_obs_redondeadas <- round(datos_cco$defunciones_observadas)

mantener<-sapply(split(datos_cco$defunciones_obs_redondeadas, datos_cco$strata), sum)
mantener<-datos_cco$strata %in% names(mantener[mantener != 0])
length(mantener)

datos_cco<-datos_cco[mantener,]
base_cco <- as.data.frame(base_cco)
base_cco<-base_cco[mantener,]

#--------

###########################################
#PARTE 2: HACER EL R-INLA CON DLNM
###########################################


#Para la implementación INLA, hay que añadir variables identificando diferentes
# categorías:

colnames(base_cco)<-paste0("base", 1:ncol(base_cco))

datos_cco<-cbind(datos_cco, base_cco) #Se añade la base cruzada a los datos
#datos_cco y base_cco. Tienen que tener la misma longitud.

  #Notar que ahora tenemos 16 parámetros, 4*4
for (i in 1:16) { 
  col_name <- paste0("id_base", i)
  datos_cco[[col_name]]<-as.numeric(datos_cco$cod_ine_ambito)
}
rm(i, col_name)


#Hacemos el MODELO 3, un SB-DLNM con cross-over design:

inla_formula <- defunciones_obs_redondeadas ~ -1 + 
  base1 + base2 + base3 + base4 + base5 + base6 + base7 + base8 + base9 + base10 + 
  base11 + base12 + base13 + base14 + base15 + base16 +
  f(strata, model = "iid", hyper = list(prec = list(initial = log(1e-04), fixed = TRUE))) + 
  f(id_base1, base1, model = "bym2", graph = lista_vecinos) + 
  f(id_base2, base2, model = "bym2", graph = lista_vecinos) +
  f(id_base3, base3, model = "bym2", graph = lista_vecinos) + 
  f(id_base4, base4, model = "bym2", graph = lista_vecinos) + 
  f(id_base5, base5, model = "bym2", graph = lista_vecinos) + 
  f(id_base6, base6, model = "bym2", graph = lista_vecinos) + 
  f(id_base7, base7, model = "bym2", graph = lista_vecinos) + 
  f(id_base8, base8, model = "bym2", graph = lista_vecinos) + 
  f(id_base9, base9, model = "bym2", graph = lista_vecinos) + 
  f(id_base10, base10, model = "bym2", graph = lista_vecinos) + 
  f(id_base11, base11, model = "bym2", graph = lista_vecinos) + 
  f(id_base12, base12, model = "bym2", graph = lista_vecinos) + 
  f(id_base13, base13, model = "bym2", graph = lista_vecinos) + 
  f(id_base14, base14, model = "bym2", graph = lista_vecinos) + 
  f(id_base15, base15, model = "bym2", graph = lista_vecinos) + 
  f(id_base16, base16, model = "bym2", graph = lista_vecinos) 

#---

modelo_inla <- inla(inla_formula,
                    data = datos_cco,
                    family = "poisson",
                    control.compute = list(config = TRUE),
                    control.inla = list(strategy = "laplace",
                                        int.strategy = "grid"), verbose=TRUE)

#Se probó a ejecutar el modelo con los datos de todo el año 2024 pero no se pudo
# ni con 12 ni con 16 parámetros. Luego se probó solo con los datos de los meses
# de julio y agosto del 2024 empleando 16 parámetros. 

#La ejecución ha empezado a las 12:28 y ha terminado a las 14:08. Ha tardado
# 1 hora y 40 minutos pero al final se pudo ejecutar. 

library(qs)
qsave(modelo_inla, "modelo_inla.qs")
modelo_inla<-qread("modelo_inla.qs")


#---

inla_res <- inla.posterior.sample(1000, modelo_inla,
                                  selection = list(base1 = 1, base2 = 1, base3 = 1,
                                                   base4 = 1, base5 = 1, base6 = 1,
                                                   base7 = 1, base8 = 1, base9 = 1,
                                                   base10 = 1, base11 = 1, base12 = 1,
                                                   base13 = 1, base14 = 1, base15 = 1,
                                                   base16 = 1,
                                                   "id_base1" = 1:50, "id_base2" = 1:50,
                                                   "id_base3" = 1:50, "id_base4" = 1:50,
                                                   "id_base5" = 1:50, "id_base6" = 1:50,
                                                   "id_base7" = 1:50, "id_base8" = 1:50,
                                                   "id_base9" = 1:50, "id_base10" = 1:50,
                                                   "id_base11" = 1:50, "id_base12" = 1:50,
                                                   "id_base13" = 1:50, "id_base14" = 1:50,
                                                   "id_base15" = 1:50, "id_base16" = 1:50))

qsave(inla_res, "inla_res.qs")
inla_res<-qread("inla_res.qs")

#---

base_res <- lapply(1:49, function(provincia_i) {
  beta_reg <- sapply(inla_res, function(x) {
    sapply(1:16, function(i) {
      x$latent[paste0("base", i, ":1"),] + 
        x$latent[paste0("id_base", i, ":", provincia_i),]
    })
  })
  t(beta_reg)
})


qsave(base_res, "base_res.qs")
base_res<-qread("base_res.qs")


#-------------


###########################################
#PARTE 3: DIBUJAR EL SB-DLNM
###########################################


#1) Preparación de los datos:

momo_verano24$defunciones_obs_redondeadas <- round(momo_verano24$defunciones_observadas)

#Definimos los percentiles de temperatura a calcular:
percentiles<-c(seq(0, 1, by = 0.1), 
               seq(2, 98, by = 1), 
               seq(99, 100, by = 0.1)) /100

#Crear los valores de las temperaturas usados en los modelos dlnm:
lista_temperaturas <- lapply(1:49, function(provincia_i) {
  
  tmed <- subset(momo_verano24, cod_ine_ambito == provincia_i)   
  
  tmed_valid <- tmed$tmed[!is.na(tmed$tmed)]
  if (length(tmed_valid) == 0) return(NULL)
  
  nudos_temperatura<-quantile(tmed_valid, c(0.10,0.75,0.90), na.rm = TRUE)
  limites_temp<-range(tmed_valid, na.rm = TRUE)
  x_tmed<- quantile(tmed_valid, percentiles, na.rm = TRUE)
  
  return(list(temp_knots = nudos_temperatura, #nudos de la función exposición-respuesta  
              temp_boundary = limites_temp, #rango de temperaturas
              x_temp = x_tmed)) #temperaturas en las que se calcula el RR
  
})

qsave(lista_temperaturas, "lista_temperaturas.qs")
lista_temperaturas<-qread("lista_temperaturas.qs")


nudos_temperatura <- lapply(lista_temperaturas, function(x) if (!is.null(x)) x[["temp_knots"]] else NULL)
limites_temp      <- lapply(lista_temperaturas, function(x) if (!is.null(x)) x[["temp_boundary"]] else NULL)
x_tmed            <- lapply(lista_temperaturas, function(x) if (!is.null(x)) x[["x_temp"]] else NULL)


#---

#Crear una lista con las bases para la temp y para los retardos en cada vecindario:

base_all <- lapply(1:49, function(provincia_i) {
  
  f.nudos_temperatura <- nudos_temperatura[[provincia_i]]
  f.limites_temp <- limites_temp[[provincia_i]]
  f.x_tmed <- x_tmed[[provincia_i]]
  
  if (is.null(f.nudos_temperatura) || 
      is.null(f.limites_temp) || 
      is.null(f.x_tmed)) return(NULL)
  
  #base para la temperatura:
  Q <- onebasis(f.x_tmed, fun = 'ns', knots = f.nudos_temperatura, 
                Boundary.knots = f.limites_temp)
  
  #base para el retardo:
  C <- onebasis(0:30, fun = "ns", 
                knots = logknots(30, 2), 
                intercept = TRUE)
  
  return(list(base_temp = Q, base_retardo = C))
  
})


#---

#Crear una lista con la base cruzada para cada provincia:

base_cruzada<-lapply(1:49, function(provincia_i) {
  
  f.nudos_temperatura <- nudos_temperatura[[provincia_i]]
  f.limites_temp <- limites_temp[[provincia_i]]
  f.x_tmed <- x_tmed[[provincia_i]]
  
  if (is.null(f.nudos_temperatura) || is.null(f.limites_temp) || is.null(f.x_tmed)) return(NULL)
  
  base_cruzada <- crossbasis(matrix(rep(f.x_tmed, 31), 
                                    ncol = 31),
                             argvar = list(fun = 'ns',
                                           knots = f.nudos_temperatura,
                                           Boundary.knots = f.limites_temp),
                             arglag = list(fun = "ns",
                                           knots = logknots(30, 2),
                                           intercept = TRUE))
  
  return(base_cruzada)
  
})


################################
################################
################################



################################
################################
################################


#4) MAPA COLOREADO:

#Calculamos los RR de cada provincia:
rr <- lapply(1:49, function(provincia_i) {
  
  beta_reg <- base_res[[provincia_i]]
  
  rr <- apply(beta_reg, 1, function(x) {
    sapply(1:length(x_tmed[[provincia_i]]), function(i) base_cruzada[[provincia_i]][i] %*% x)
  })            
  
  rr
  
})



#---

#Creamos función para centrar el RR en cada provincia:

centrar_RR <- function(f.rr, f.cen, f.temp){
  cen <- f.temp[which.min(abs(f.temp - f.cen))]
  rr <- apply(f.rr, 2, function(x) x - x[f.temp == cen])
  rr <- as.matrix(rr)  # <-- fuerza siempre a matriz
  return(rr)
}



#---

#Centramos los RR de las provincias y extraemos el punto de estimación en el percentil 99:

rr_plot <- sapply(1:49, function(provincia_i) {
  x_plot <- x_tmed[[provincia_i]]
  cen_plot <- x_plot[percentiles == 0.5]
  rr_plot <- centrar_RR(f.rr = rr[[provincia_i]], 
                        f.cen = cen_plot,
                        f.temp = x_plot)
  
  # Point estimate as the median of the values at percentile 99
  median(exp(rr_plot[percentiles == 0.99,]))
  
  
})
rr_plot


#---

# Pallete of colours for the maps

library(leaflet)

pal <- colorNumeric(
  palette = rev(c("#8B0000", "#B22222", "#DC143C", "#F08080", "#FFFFFF", 
                  "#87CEFA", "#4682B4", "#27408B")),
  domain = range(log(rr_plot), na.rm = TRUE),
  reverse = FALSE)

#---

#Dibujar el mapa:

peninsula <- mapa %>% filter(!NAME %in% c("PALMAS (LAS)", "SANTA CRUZ DE TENERIFE"))
canarias  <- mapa %>% filter(NAME %in% c("PALMAS (LAS)", "SANTA CRUZ DE TENERIFE"))

#Acercamos Canarias a la península.
canarias_shifted <- st_geometry(canarias) + c(5, 8)
canarias <- st_set_geometry(canarias, canarias_shifted)

# Unir todo de nuevo
mapa_mod <- rbind(peninsula, canarias)

par(mar = c(1, 1, 1, 1), omi = c(0, 0, 0.4, 0))
plot(mapa_mod$geometry, col = pal(log(rr_plot)))
legend("topleft",
       fill = pal(seq(min(log(rr_plot), na.rm=TRUE),
                      max(log(rr_plot), na.rm=TRUE), length.out=7)),
       legend = round(exp(seq(min(log(rr_plot), na.rm=TRUE),
                              max(log(rr_plot), na.rm=TRUE), length.out=7)), 2),
       title = "RR")

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

  #INVIERNO 2024: (seleccionamos enero y febrero)

install.packages("INLA", repos="https://inla.r-inla-download.org/R/stable")
library(INLA)

###########################################
#PARTE 1: PREPARACIÓN DE LOS DATOS
###########################################

library(dlnm)
library(dplyr)

momo_provincial_temp <- readRDS("momo_provincial_temp.rds")

#Para quitar los NA:
momo_provincial_temp <- momo_provincial_temp[!is.na(momo_provincial_temp$tmed), ]

momo_provincial_temp$tmed <- as.numeric(gsub(",", ".", momo_provincial_temp$tmed))
momo_provincial_temp$tmax <- as.numeric(gsub(",", ".", momo_provincial_temp$tmax))
momo_provincial_temp$tmin <- as.numeric(gsub(",", ".", momo_provincial_temp$tmin))

momo_provincial_temp$cod_sexo<-as.factor(momo_provincial_temp$cod_sexo)
momo_provincial_temp$nombre_sexo<-as.factor(momo_provincial_temp$nombre_sexo)
momo_provincial_temp$cod_gedad<-as.factor(momo_provincial_temp$cod_gedad)
momo_provincial_temp$nombre_gedad<-as.factor(momo_provincial_temp$nombre_gedad)
momo_provincial_temp$nombre_ambito<-as.factor(momo_provincial_temp$nombre_ambito)

#---

momo_provincial_temp$mes<-format(momo_provincial_temp$fecha_defuncion, "%m")
momo_provincial_temp$año<-format(momo_provincial_temp$fecha_defuncion, "%y")
momo_invierno24<-subset(momo_provincial_temp, (mes=='01' & año=='24') | (mes=='02' & año=='24'))


load("C:/Users/alons/OneDrive/Escritorio/Máster Modelización Alonso Pueyo/TFM/Trabajo/Revisión/Mortalidad por Provincias/Carto_SpainPROV.Rdata")
Carto_SpainPROV <- as.data.frame(Carto_SpainPROV)
filas<-data.frame(ID.area=c(7,35,38), NAME=c("BALEARES", "LAS PALMAS", "TENERIFE"))
Carto_SpainPROV_Entero <- rbind(Carto_SpainPROV, filas)
Carto_SpainPROV_Entero <- Carto_SpainPROV_Entero[Carto_SpainPROV_Entero$ID.area != 24, ]


setwd("C:/Users/alons/OneDrive/Escritorio/Máster Modelización Alonso Pueyo/TFM/Trabajo/Revisión/Mortalidad por Provincias/SpanishProv/SpanishProv")

library(sf)
library(tidyverse)
library(ggthemes)
library(ggspatial)

mapa <- read_sf("esp_prov.shp")

#En 'mapa$NAME' no hay datos de Ceuta ni de Melilla. Para que salga bien tenemos
# que quitar también las filas correspondientes en la base de datos

momo_invierno24<-momo_invierno24[momo_invierno24$nombre_ambito != 'CEUTA', ]
momo_invierno24<-momo_invierno24[momo_invierno24$nombre_ambito != 'MELILLA', ]

#---

library(spdep)
library(lubridate)

#Generamos una matriz de provincias vecinas:
lista_vecinos <- nb2mat(poly2nb(mapa), style = "B", zero.policy=TRUE)

#Creamos base cruzada para cada provincia:

#Ordenamos por provincias

momo_invierno24<-momo_invierno24[order(momo_invierno24$cod_ine_ambito), ]

for (i in c(1:length(momo_invierno24[,1]))) {
  if(momo_invierno24$cod_ine_ambito[i]>=24) {
    momo_invierno24$cod_ine_ambito[i]<-momo_invierno24$cod_ine_ambito[i]-1
  }
}     #Lo anterior lo hacemos porque en ese tiempo León no tiene observaciones.
# Más tarde, a la hora de dibujar el mapa, esto daba errores fatales.


#Lo cambiamos para que el tiempo y coste computacional no sea tan grande y poder
# ejecutarlo en un periodo asequible:
lista_bases<-lapply(formatC(1:49, width=2, flag="0"), 
                    function(provincia_i) {
                      tmed<-subset(momo_invierno24, as.character(cod_ine_ambito)==provincia_i)
                      
                      tmed_valid <- tmed[!is.na(tmed$tmed), ]
                      if (nrow(tmed_valid) == 0) return(NULL)
                      
                      base<-crossbasis(tmed$tmed, lag=30, argvar=list(fun="bs", df=4),
                                       arglag=list(fun="bs", df=4))
                      base
                    })

base<- do.call(rbind, lista_bases)
nrow(base)

saveRDS(base, "base.rds")
base<-readRDS("base.rds") #Lo guardamos.

#---

#Hacemos el diseño case-crossover:
datos_cco <- momo_invierno24
base_cco <- base
rm(base)

#CASE-CROSSOVER DESIGN:

#Crear strata para el case-crossover (provincia-año-mes-día de la semana):
datos_cco$strata<-paste(datos_cco$cod_ine_ambito, 
                        year(datos_cco$fecha_defuncion), 
                        formatC(month(datos_cco$fecha_defuncion), width = 2, flag = "0"),
                        wday(datos_cco$fecha_defuncion, week_start = 1),
                        sep = ":")
#En vez de modelar la serie temporal entera, lo que hace este diseño es
# emparejar cada día con otros parecidos (similar estacionalidad y tendencia)
# pero distinta exposición.

#Lo de strata se usa para definir los conjuntos de "control". Se asegura que
# sea de la misma provincia, dentro del mismo año y mes y el mismo día de la semana

#---

#Se excluyen estratos sin casos ya que no contribuyen al vecindario (provincia).

datos_cco$defunciones_obs_redondeadas <- round(datos_cco$defunciones_observadas)

mantener<-sapply(split(datos_cco$defunciones_obs_redondeadas, datos_cco$strata), sum)
mantener<-datos_cco$strata %in% names(mantener[mantener != 0])
length(mantener)

datos_cco<-datos_cco[mantener,]
base_cco <- as.data.frame(base_cco)
base_cco<-base_cco[mantener,]

#--------

###########################################
#PARTE 2: HACER EL R-INLA CON DLNM
###########################################


#Para la implementación INLA, hay que añadir variables identificando diferentes
# categorías:

colnames(base_cco)<-paste0("base", 1:ncol(base_cco))

datos_cco<-cbind(datos_cco, base_cco) #Se añade la base cruzada a los datos
#datos_cco y base_cco. Tienen que tener la misma longitud.

#Notar que ahora tenemos 16 parámetros, 4*4
for (i in 1:16) { 
  col_name <- paste0("id_base", i)
  datos_cco[[col_name]]<-as.numeric(datos_cco$cod_ine_ambito)
}
rm(i, col_name)


#Hacemos el MODELO 3, un SB-DLNM con cross-over design:

inla_formula <- defunciones_obs_redondeadas ~ -1 + 
  base1 + base2 + base3 + base4 + base5 + base6 + base7 + base8 + base9 + base10 + 
  base11 + base12 + base13 + base14 + base15 + base16 +
  f(strata, model = "iid", hyper = list(prec = list(initial = log(1e-04), fixed = TRUE))) + 
  f(id_base1, base1, model = "bym2", graph = lista_vecinos) + 
  f(id_base2, base2, model = "bym2", graph = lista_vecinos) +
  f(id_base3, base3, model = "bym2", graph = lista_vecinos) + 
  f(id_base4, base4, model = "bym2", graph = lista_vecinos) + 
  f(id_base5, base5, model = "bym2", graph = lista_vecinos) + 
  f(id_base6, base6, model = "bym2", graph = lista_vecinos) + 
  f(id_base7, base7, model = "bym2", graph = lista_vecinos) + 
  f(id_base8, base8, model = "bym2", graph = lista_vecinos) + 
  f(id_base9, base9, model = "bym2", graph = lista_vecinos) + 
  f(id_base10, base10, model = "bym2", graph = lista_vecinos) + 
  f(id_base11, base11, model = "bym2", graph = lista_vecinos) + 
  f(id_base12, base12, model = "bym2", graph = lista_vecinos) + 
  f(id_base13, base13, model = "bym2", graph = lista_vecinos) + 
  f(id_base14, base14, model = "bym2", graph = lista_vecinos) + 
  f(id_base15, base15, model = "bym2", graph = lista_vecinos) + 
  f(id_base16, base16, model = "bym2", graph = lista_vecinos) 

#---

modelo_inla <- inla(inla_formula,
                    data = datos_cco,
                    family = "poisson",
                    control.compute = list(config = TRUE),
                    control.inla = list(strategy = "laplace",
                                        int.strategy = "grid"), verbose=TRUE)
print('ya')


#Ha tardado 5995 segundos en completarse el modelo.


library(qs)
qsave(modelo_inla, "modelo_inla.qs")
modelo_inla<-qread("modelo_inla.qs")


#---

inla_res <- inla.posterior.sample(1000, modelo_inla,
                                  selection = list(base1 = 1, base2 = 1, base3 = 1,
                                                   base4 = 1, base5 = 1, base6 = 1,
                                                   base7 = 1, base8 = 1, base9 = 1,
                                                   base10 = 1, base11 = 1, base12 = 1,
                                                   base13 = 1, base14 = 1, base15 = 1,
                                                   base16 = 1,
                                                   "id_base1" = 1:50, "id_base2" = 1:50,
                                                   "id_base3" = 1:50, "id_base4" = 1:50,
                                                   "id_base5" = 1:50, "id_base6" = 1:50,
                                                   "id_base7" = 1:50, "id_base8" = 1:50,
                                                   "id_base9" = 1:50, "id_base10" = 1:50,
                                                   "id_base11" = 1:50, "id_base12" = 1:50,
                                                   "id_base13" = 1:50, "id_base14" = 1:50,
                                                   "id_base15" = 1:50, "id_base16" = 1:50))

qsave(inla_res, "inla_res.qs")
inla_res<-qread("inla_res.qs")

#---

base_res <- lapply(1:49, function(provincia_i) {
  beta_reg <- sapply(inla_res, function(x) {
    sapply(1:16, function(i) {
      x$latent[paste0("base", i, ":1"),] + 
        x$latent[paste0("id_base", i, ":", provincia_i),]
    })
  })
  t(beta_reg)
})


qsave(base_res, "base_res.qs")
base_res<-qread("base_res.qs")


#-------------


###########################################
#PARTE 3: DIBUJAR EL SB-DLNM
###########################################

#1) Preparación de los datos:

momo_invierno24$defunciones_obs_redondeadas <- round(momo_invierno24$defunciones_observadas)

#Definimos los percentiles de temperatura a calcular:
percentiles<-c(seq(0, 1, by = 0.1), 
               seq(2, 98, by = 1), 
               seq(99, 100, by = 0.1)) /100

#Crear los valores de las temperaturas usados en los modelos dlnm:
lista_temperaturas <- lapply(1:49, function(provincia_i) {
  
  tmed <- subset(momo_invierno24, cod_ine_ambito == provincia_i)   
  
  tmed_valid <- tmed$tmed[!is.na(tmed$tmed)]
  if (length(tmed_valid) == 0) return(NULL)
  
  nudos_temperatura<-quantile(tmed_valid, c(0.10,0.75,0.90), na.rm = TRUE)
  limites_temp<-range(tmed_valid, na.rm = TRUE)
  x_tmed<- quantile(tmed_valid, percentiles, na.rm = TRUE)
  
  return(list(temp_knots = nudos_temperatura, #nudos de la función exposición-respuesta  
              temp_boundary = limites_temp, #rango de temperaturas
              x_temp = x_tmed)) #temperaturas en las que se calcula el RR
  
})

qsave(lista_temperaturas, "lista_temperaturas.qs")
lista_temperaturas<-qread("lista_temperaturas.qs")


nudos_temperatura <- lapply(lista_temperaturas, function(x) if (!is.null(x)) x[["temp_knots"]] else NULL)
limites_temp      <- lapply(lista_temperaturas, function(x) if (!is.null(x)) x[["temp_boundary"]] else NULL)
x_tmed            <- lapply(lista_temperaturas, function(x) if (!is.null(x)) x[["x_temp"]] else NULL)


#---

#Crear una lista con las bases para la temp y para los retardos en cada vecindario:

base_all <- lapply(1:49, function(provincia_i) {
  
  f.nudos_temperatura <- nudos_temperatura[[provincia_i]]
  f.limites_temp <- limites_temp[[provincia_i]]
  f.x_tmed <- x_tmed[[provincia_i]]
  
  if (is.null(f.nudos_temperatura) || 
      is.null(f.limites_temp) || 
      is.null(f.x_tmed)) return(NULL)
  
  #base para la temperatura:
  Q <- onebasis(f.x_tmed, fun = 'ns', knots = f.nudos_temperatura, 
                Boundary.knots = f.limites_temp)
  
  #base para el retardo:
  C <- onebasis(0:30, fun = "ns", 
                knots = logknots(30, 2), 
                intercept = TRUE)
  
  return(list(base_temp = Q, base_retardo = C))
  
})


#---

#Crear una lista con la base cruzada para cada provincia:

base_cruzada<-lapply(1:49, function(provincia_i) {
  
  f.nudos_temperatura <- nudos_temperatura[[provincia_i]]
  f.limites_temp <- limites_temp[[provincia_i]]
  f.x_tmed <- x_tmed[[provincia_i]]
  
  if (is.null(f.nudos_temperatura) || is.null(f.limites_temp) || is.null(f.x_tmed)) return(NULL)
  
  base_cruzada <- crossbasis(matrix(rep(f.x_tmed, 31), 
                                    ncol = 31),
                             argvar = list(fun = 'ns',
                                           knots = f.nudos_temperatura,
                                           Boundary.knots = f.limites_temp),
                             arglag = list(fun = "ns",
                                           knots = logknots(30, 2),
                                           intercept = TRUE))
  
  return(base_cruzada)
  
})


################################
################################
################################


#4) MAPA COLOREADO:

#Calculamos los RR de cada provincia:
rr <- lapply(1:49, function(provincia_i) {
  
  beta_reg <- base_res[[provincia_i]]
  
  rr <- apply(beta_reg, 1, function(x) {
    sapply(1:length(x_tmed[[provincia_i]]), function(i) base_cruzada[[provincia_i]][i] %*% x)
  })            
  
  rr
  
})



#---

#Creamos función para centrar el RR en cada provincia:

centrar_RR <- function(f.rr, f.cen, f.temp){
  cen <- f.temp[which.min(abs(f.temp - f.cen))]
  rr <- apply(f.rr, 2, function(x) x - x[f.temp == cen])
  rr <- as.matrix(rr)  # <-- fuerza siempre a matriz
  return(rr)
}



#---

#Centramos los RR de las provincias y extraemos el punto de estimación en el percentil 99:

rr_plot <- sapply(1:49, function(provincia_i) {
  x_plot <- x_tmed[[provincia_i]]
  cen_plot <- x_plot[percentiles == 0.5]
  rr_plot <- centrar_RR(f.rr = rr[[provincia_i]], 
                        f.cen = cen_plot,
                        f.temp = x_plot)
  
  # Point estimate as the median of the values at percentile 99
  median(exp(rr_plot[percentiles == 0.99,]))
  
  
})
rr_plot


#---

# Pallete of colours for the maps

library(leaflet)

pal <- colorNumeric(
  palette = rev(c("#8B0000", "#B22222", "#DC143C", "#F08080", "#FFFFFF", 
                  "#87CEFA", "#4682B4", "#27408B")),
  domain = range(log(rr_plot), na.rm = TRUE),
  reverse = FALSE)

#---

#Dibujar el mapa:

peninsula <- mapa %>% filter(!NAME %in% c("PALMAS (LAS)", "SANTA CRUZ DE TENERIFE"))
canarias  <- mapa %>% filter(NAME %in% c("PALMAS (LAS)", "SANTA CRUZ DE TENERIFE"))

#Acercamos Canarias a la península.
canarias_shifted <- st_geometry(canarias) + c(5, 8)
canarias <- st_set_geometry(canarias, canarias_shifted)

# Unir todo de nuevo
mapa_mod <- rbind(peninsula, canarias)

par(mar = c(1, 1, 1, 1), omi = c(0, 0, 0.4, 0))
plot(mapa_mod$geometry, col = pal(log(rr_plot)))
legend("topleft",
       fill = pal(seq(min(log(rr_plot), na.rm=TRUE),
                      max(log(rr_plot), na.rm=TRUE), length.out=7)),
       legend = round(exp(seq(min(log(rr_plot), na.rm=TRUE),
                              max(log(rr_plot), na.rm=TRUE), length.out=7)), 2),
       title = "RR")

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################


