#######
# 3.2) Análisis de la probabilidad de exceso de mortalidad atribuible a la 
#       temperatura por provincias
#######


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

load("C:/Users/alons/OneDrive/Escritorio/Máster Modelización Alonso Pueyo/TFM/Trabajo/Revisión/Mortalidad por Provincias/Carto_SpainPROV.Rdata")
Carto_SpainPROV <- as.data.frame(Carto_SpainPROV)
filas<-data.frame(ID.area=c(7,35,38), NAME=c("BALEARES", "LAS PALMAS", "TENERIFE"))
Carto_SpainPROV_Entero <- rbind(Carto_SpainPROV, filas)

setwd("C:/Users/alons/OneDrive/Escritorio/Máster Modelización Alonso Pueyo/TFM/Trabajo/Revisión/Mortalidad por Provincias/SpanishProv/SpanishProv")

library(sf)
library(tidyverse)
library(ggthemes)
library(ggspatial)

  
mapa <- read_sf("esp_prov.shp")


#En 'mapa$NAME' no hay datos de Ceuta ni de Melilla. Para que salga bien tenemos
# que quitar también las filas correspondientes en la base de datos

momo_provincial_temp<-momo_provincial_temp[momo_provincial_temp$nombre_ambito != 'CEUTA', ]
momo_provincial_temp<-momo_provincial_temp[momo_provincial_temp$nombre_ambito != 'MELILLA', ]



#---

library(spdep)
library(lubridate)

  #Generamos una matriz de provincias vecinas:
lista_vecinos <- nb2mat(poly2nb(mapa), style = "B", zero.policy=TRUE)

  #Creamos base cruzada para cada provincia:

#Ordenamos por provincias
momo_provincial_temp<-momo_provincial_temp[order(momo_provincial_temp$cod_ine_ambito), ]

  #Vamos a cambiar la base cruzada. Nuestro modelo tenía 'bs' para temp. y para
  # retardo con 4 df para cada uno. Para reducir el nº de bases vamos a hacer 'ns'
  # para ambas dimensiones con 2 df.

  #Lo cambiamos para que el tiempo y coste computacional no sea tan grande y poder
  # ejecutarlo en un periodo asequible:
lista_bases<-lapply(formatC(1:50, width=2, flag="0"), 
                function(provincia_i) {
                    tmed<-subset(momo_provincial_temp, as.character(cod_ine_ambito)==provincia_i)
                    
                    tmed_valid <- tmed[!is.na(tmed$tmed), ]
                    if (nrow(tmed_valid) == 0) return(NULL)
                    
                    base<-crossbasis(tmed$tmed, lag=30, argvar=list(fun="ns", df=2),
                                     arglag=list(fun="ns", df=2))
                    base
                  })

base<- do.call(rbind, lista_bases)
nrow(base)

saveRDS(base, "base.rds")
base<-readRDS("base.rds") #Lo guardamos.

#---

  #Haremos el diseño case-crossover. No haremos el diseño de series temporales
  # porque vamos a hacer SB-DLNM, y con las series temporales el tiempo de ejecución
  # se disparaba a varios días. Con case-crossover son solo minutos.

datos_cco <- momo_provincial_temp
base_cco <- base
datos_ts <- momo_provincial_temp
base_ts <- base
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



###########################################
#PARTE 2: HACER EL R-INLA CON DLNM
###########################################


#Para la implementación INLA, hay que añadir variables identificando diferentes
# categorías:

colnames(base_cco)<-paste0("base", 1:ncol(base_cco))

datos_cco<-cbind(datos_cco, base_cco) #Se añade la base cruzada a los datos
    #datos_cco y base_cco. Tienen que tener la misma longitud.

  #4 es el número de parámetros (2*2)
for (i in 1:4) { 
  col_name <- paste0("id_base", i)
  datos_cco[[col_name]]<-as.numeric(datos_cco$cod_ine_ambito)
}
rm(i, col_name)

  #Hacemos el MODELO 3, un SB-DLNM con cross-over design:
  

    inla_formula <- defunciones_obs_redondeadas ~ -1 + 
  base1 + base2 + base3 + base4 +
  f(strata, model = "iid", hyper = list(prec = list(initial = log(1e-04), fixed = TRUE))) + 
  f(id_base1, base1, model = "bym2", graph = lista_vecinos) + 
  f(id_base2, base2, model = "bym2", graph = lista_vecinos) +
  f(id_base3, base3, model = "bym2", graph = lista_vecinos) + 
  f(id_base4, base4, model = "bym2", graph = lista_vecinos) 
    
#---

modelo_inla <- inla(inla_formula,
                    data = datos_cco,
                    family = "poisson",
                    control.compute = list(config = TRUE),
                    control.inla = list(strategy = "laplace",
                                        int.strategy = "grid"), verbose=TRUE)

      #Este modelo de 4 parámetros tarda aproximadamente unos 27 minutos en ejecutarse.

    #Usaremos qs para guardar y leer, ya que es más rápido.
library(qs)
qsave(modelo_inla, "modelo_inla.qs")
modelo_inla<-qread("modelo_inla.qs")



#---

inla_res <- inla.posterior.sample(1000, modelo_inla,
              selection = list(base1 = 1, base2 = 1, base3 = 1, base4 = 1,
                               "id_base1" = 1:50, "id_base2" = 1:50,
                               "id_base3" = 1:50, "id_base4" = 1:50))

qsave(inla_res, "inla_res.qs")
inla_res<-qread("inla_res.qs")



#---

base_res <- lapply(1:50, function(provincia_i) {
  beta_reg <- sapply(inla_res, function(x) {
    sapply(1:4, function(i) {
      x$latent[paste0("base", i, ":1"),] + 
        x$latent[paste0("id_base", i, ":", provincia_i),]
    })
  })
  t(beta_reg)
})


qsave(base_res, "base_res.qs")
base_res<-qread("base_res.qs")


###########################################
#PARTE 3: DIBUJAR EL SB-DLNM
###########################################


  #1) Preparación de los datos:

momo_provincial_temp$defunciones_obs_redondeadas <- round(momo_provincial_temp$defunciones_observadas)

#Definimos los percentiles de temperatura a calcular:
percentiles<-c(seq(0, 1, by = 0.1), 
                seq(2, 98, by = 1), 
                seq(99, 100, by = 0.1)) /100


#Crear los valores de las temperaturas usados en los modelos dlnm:
lista_temperaturas <- lapply(1:50, function(provincia_i) {
  
  tmed <- subset(momo_provincial_temp, cod_ine_ambito == provincia_i)   
               
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

base_all <- lapply(1:50, function(provincia_i) {
  
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

base_cruzada<-lapply(1:50, function(provincia_i) {
  
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


  #2) Gráfico tridimensional:

#for (provincia_i in c(1:50)) {}
  #primero probamos para una sola provincia...

provincia_i <- 1
  base_temp <- base_all[[provincia_i]]$base_temp
  base_retardo <- base_all[[provincia_i]]$base_retardo
  f.x_tmed <- x_tmed[[provincia_i]]

beta_reg <- base_res[[provincia_i]]

#Inicializamos la matriz para almacenar los RR para cada combinación de temp y retardo:
matriz<-array(NA, dim=c(nrow(beta_reg), length(f.x_tmed), 31))

for(iteracion_i in 1:nrow(beta_reg)) {
  for(temp_i in 1:length(f.x_tmed)) {
    for(retardo_i in 1:31) {
      matriz[iteracion_i, temp_i, retardo_i] <- sum(
        (t(base_temp[temp_i,,drop = FALSE]) %*% base_retardo[retardo_i,, drop = FALSE]) *
          matrix(beta_reg[iteracion_i,], ncol = ncol(base_retardo), byrow = TRUE))
    }
  }
}
print('ya')


#Percentil de la temperatura sobre la que centramos:
centro_i <- 0.5

#Nuevo array para almacenar las asociaciones centradas:
matriz_centro <- array(NA, dim = c(nrow(beta_reg), length(f.x_tmed), 31))

for(i in 1:length(f.x_tmed)){
  matriz_centro[,i,] <- matriz[,i,]-matriz[,centro_i,]
}

#Se elige una iteración para dibujar:
dibujo <- exp(matriz_centro[1,,])

    #terminar...


################################
################################
################################


#3) 



################################
################################
################################


#4) MAPA COLOREADO:

#Calculamos los RR de cada provincia:
rr <- lapply(1:50, function(provincia_i) {
  
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

rr_plot <- sapply(1:50, function(provincia_i) {
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

pal <- colorNumeric(palette = rev(
  c("#A90C38", "#C52A40", "#E24848", "#F16B61", "#F89183", "#FEB6A8", "#FEDAD3",
    "#FFFFFF", "#D3E5F2", "#A8CCE5", "#88B4D5", "#6D9CC3", "#5585B1", "#416F9C", 
    "#2E5A87")), domain = range(log(rr_plot), na.rm=TRUE), reverse = FALSE)

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

