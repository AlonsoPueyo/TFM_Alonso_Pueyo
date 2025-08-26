library(dlnm)
library(dplyr)

momo_provincial_temp <- readRDS("momo_provincial_temp.rds")
Navarra<-filter(momo_provincial_temp, nombre_ambito=='NAVARRA')
  #Ahora mismo solo tenemos datos del aeropuerto de Pamplona. Podríamos conseguir 
  # los de temperaturas de otras estaciones meteorológicas, pero las defunciones
  # observadas no cambiarían. No podremos hacer el mapa de colores, pero sí el 
  # análisis por grupos...

which(is.na(Navarra$tmed)) #Algunos días sueltos no tiene info de la temperatura, 
                            # como el 26 de marzo del 2015

  #para borrar las filas con tmed=NA:
Navarra <- Navarra[!is.na(Navarra$tmed), ]

Navarra$tmed <- as.numeric(gsub(",", ".", Navarra$tmed))
Navarra$tmax <- as.numeric(gsub(",", ".", Navarra$tmax))
Navarra$tmin <- as.numeric(gsub(",", ".", Navarra$tmin))

Navarra$cod_sexo<-as.factor(Navarra$cod_sexo)
Navarra$nombre_sexo<-as.factor(Navarra$nombre_sexo)
Navarra$cod_gedad<-as.factor(Navarra$cod_gedad)
Navarra$nombre_gedad<-as.factor(Navarra$nombre_gedad)
Navarra$nombre_ambito<-as.factor(Navarra$nombre_ambito)

summary(Navarra)
    #de aqui se desprende el mismo análisis que con el conjunto entero de los datos...
    #explicar todo en el documento...

  #Se pueden incluir gráficos:


#-----

########
#MODELIZACIÓN:
########

library(splines)
Navarra$defunciones_obs_redondeadas <- round(Navarra$defunciones_observadas)
Navarra$dow <- factor(weekdays(Navarra$fecha_defuncion))


base<-crossbasis((Navarra$tmed), lag=30,
                 argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modelo<-glm(Navarra$defunciones_obs_redondeadas ~ base+Navarra$dow+
              ns(Navarra$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion<-crosspred(base, model=modelo, cen=20)


par(mfrow=c(1,2))
plot(prediccion, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))
  #este primer modelo ha sido hecho según lo obtenido en la parte 3.1. Como ahora
  #no son datos de tantos sitios diferentes,  sino que de uno solo, se observan cosas
  #un poco raras. Por ello parece que tendremos que elegir otro modelo. Probaremos
  #a hacer otro análisis de sensibilidad.


#---

#Análisis de sensibilidad:

#1) 'ns' para todos:

resultados_ns<-data.frame(df_temp=integer(), df_retardo=integer(), QAIC=numeric())

for (df_var in c(3:7)) {
  for (df_lag in c(3:7)) {
    base<-crossbasis((Navarra$tmed), lag=30, argvar=list(fun="ns", df=df_var), arglag=list(fun="ns", df=df_lag))
    modelo<-glm(Navarra$defunciones_obs_redondeadas ~ base+Navarra$dow+ns(Navarra$fecha_defuncion, df=10*7), family=quasipoisson())
    
    k <- length(coef(modelo))
    phi <- summary(modelo)$dispersion
    ll <- logLik(update(modelo, family=poisson))
    qaic <- -2 * as.numeric(ll) + 2 *phi * k
    
    resultados_ns <- rbind(resultados_ns, data.frame(df_temp=df_var, df_retardo=df_lag, QAIC = qaic))
  }
}

print(resultados_ns)
saveRDS(resultados_ns, "resultados_ns.rds")
resultados_ns <- readRDS("resultados_ns.rds")


#2) 'bs' para todos:

resultados_bs<-data.frame(df_temp=integer(), df_retardo=integer(), QAIC=numeric())

for (df_var in c(3:7)) {
  for (df_lag in c(4:7)) {
    base<-crossbasis((Navarra$tmed), lag=30, argvar=list(fun="bs", df=df_var), arglag=list(fun="bs", df=df_lag))
    modelo<-glm(Navarra$defunciones_obs_redondeadas ~ base+Navarra$dow+ns(Navarra$fecha_defuncion, df=10*7), family=quasipoisson())
    
    k <- length(coef(modelo))
    phi <- summary(modelo)$dispersion
    ll <- logLik(update(modelo, family=poisson))
    qaic <- -2 * as.numeric(ll) + 2 *phi * k
    
    resultados_bs <- rbind(resultados_bs, data.frame(df_temp=df_var, df_retardo=df_lag, QAIC = qaic))
  }
}

print(resultados_bs)
saveRDS(resultados_bs, "resultados_bs.rds")
resultados_bs <- readRDS("resultados_bs.rds")


#3) 'ns' para temp y 'bs' para lag:

resultados_ns_bs<-data.frame(df_temp=integer(), df_retardo=integer(), QAIC=numeric())

for (df_var in c(3:7)) {
  for (df_lag in c(4:7)) {
    base<-crossbasis((Navarra$tmed), lag=30, argvar=list(fun="ns", df=df_var), arglag=list(fun="bs", df=df_lag))
    modelo<-glm(Navarra$defunciones_obs_redondeadas ~ base+Navarra$dow+ns(Navarra$fecha_defuncion, df=10*7), family=quasipoisson())
    
    k <- length(coef(modelo))
    phi <- summary(modelo)$dispersion
    ll <- logLik(update(modelo, family=poisson))
    qaic <- -2 * as.numeric(ll) + 2 *phi * k
    
    resultados_ns_bs <- rbind(resultados_ns_bs, data.frame(df_temp=df_var, df_retardo=df_lag, QAIC = qaic))
  }
}

print(resultados_ns_bs)
saveRDS(resultados_ns_bs, "resultados_ns_bs.rds")
resultados_ns_bs <- readRDS("resultados_ns_bs.rds")


#4) 'bs' para temp y 'ns' para lag:

resultados_bs_ns<-data.frame(df_temp=integer(), df_retardo=integer(), QAIC=numeric())

for (df_var in c(3:7)) {
  for (df_lag in c(3:7)) {
    base<-crossbasis((Navarra$tmed), lag=30, argvar=list(fun="bs", df=df_var), arglag=list(fun="ns", df=df_lag))
    modelo<-glm(Navarra$defunciones_obs_redondeadas ~ base+Navarra$dow+ns(Navarra$fecha_defuncion, df=10*7), family=quasipoisson())
    
    k <- length(coef(modelo))
    phi <- summary(modelo)$dispersion
    ll <- logLik(update(modelo, family=poisson))
    qaic <- -2 * as.numeric(ll) + 2 *phi * k
    
    resultados_bs_ns <- rbind(resultados_bs_ns, data.frame(df_temp=df_var, df_retardo=df_lag, QAIC = qaic))
  }
}

print(resultados_bs_ns)
saveRDS(resultados_bs_ns, "resultados_bs_ns.rds")
resultados_bs_ns <- readRDS("resultados_bs_ns.rds")

#---

#Seleccionamos el mejor de todos:
resultados_ns[which(resultados_ns$QAIC==min(resultados_ns$QAIC)),]
resultados_bs[which(resultados_bs$QAIC==min(resultados_bs$QAIC)),]
resultados_ns_bs[which(resultados_ns_bs$QAIC==min(resultados_ns_bs$QAIC)),]
resultados_bs_ns[which(resultados_bs_ns$QAIC==min(resultados_bs_ns$QAIC)),]

  #El mejor modelo es 'bs' para ambas dimensiones, con 3 df para temp y 4 df
  # para el retardo. También es buen modelo el de 'bs' para la temp y 'ns' para
  # el retardo, ambos con 3 df.

  #NOTA: Ocurre al contrario que con todos los datos de España. Ahora tienen un
  # menor QAIC aquellos modelos más sencillos, con menos df. Cuando teníamos todos
  # los datos de momo eran mejores los modelos con más de 5 df.
      

#---

#Hacemos los gráficos de prediccion de los modelos:

base1<-crossbasis((Navarra$tmed), lag=30, argvar=list(fun="bs", df=3), arglag=list(fun="bs", df=4))
modelo1<-glm(Navarra$defunciones_obs_redondeadas ~ base1+Navarra$dow+ns(Navarra$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion1<-crosspred(base1, model=modelo1, cen=20)

par(mfrow=c(1,2))
plot(prediccion1, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion1, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))

#

base2<-crossbasis((Navarra$tmed), lag=30, argvar=list(fun="bs", df=3), arglag=list(fun="ns", df=3))
modelo2<-glm(Navarra$defunciones_obs_redondeadas ~ base2+Navarra$dow+ns(Navarra$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion2<-crosspred(base2, model=modelo2, cen=20)

par(mfrow=c(1,2))
plot(prediccion2, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion2, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))

  #El primero tiene buena pinta, pero veamos qué ocurre si
  # se suben los df a 4:

base1.1<-crossbasis((Navarra$tmed), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modelo1.1<-glm(Navarra$defunciones_obs_redondeadas ~ base1.1+Navarra$dow+ns(Navarra$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion1.1<-crosspred(base1.1, model=modelo1.1, cen=20)

par(mfrow=c(1,2))
plot(prediccion1.1, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion1.1, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))
    #Este modelo sale peor

#

base2.1<-crossbasis((Navarra$tmed), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="ns", df=4))
modelo2.1<-glm(Navarra$defunciones_obs_redondeadas ~ base2.1+Navarra$dow+ns(Navarra$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion2.1<-crosspred(base2.1, model=modelo2.1, cen=20)

par(mfrow=c(1,2))
plot(prediccion2.1, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion2.1, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))
    #Este también parece peor. Nos vamos a quedar con el modelo1...

#---

  #A continuación hacer gráficos según distintos valores de la temp y del retardo,
  # y a ver qué ocurre en cada caso. Después, análisis por subgrupos de población...



