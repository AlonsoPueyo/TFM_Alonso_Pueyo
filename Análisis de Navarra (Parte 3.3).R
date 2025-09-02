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
    #De aqui se desprende el mismo análisis que con el conjunto entero de los datos


#---

#Como vamos a estudiar según los grupos de edad, veamos las defunciones para
#   cada grupo:

tapply(Navarra$defunciones_observadas,
       Navarra$cod_gedad,
       sum, na.rm = FALSE)
#Los grupos más jovenes apenas tienen muertes, mientras que conforme se va
#    aumentando la edad, se mueren más. Notar que hay un grupo +65 y otro
#    65-74.

#---


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

  #El mejor modelo es 'bs' para ambas dimensiones, con 4 df para temp y 4 df
  # para el retardo. También es buen modelo el de 'bs' para la temp y 'ns' para
  # el retardo, ambos con 4 df. (En otras ejecuciones son 3/4 grados de libertad...)

  #NOTA: Ocurre al contrario que con todos los datos de España. Ahora tienen un
  # menor QAIC aquellos modelos más sencillos, con menos df. Cuando teníamos todos
  # los datos de momo eran mejores los modelos con más de 5 df.
      

#---

#Hacemos diferentes gráficos de prediccion de los modelos (alternamos entre bs y ns
# y entre 3 y 4 df):

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

#

base1.1<-crossbasis((Navarra$tmed), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modelo1.1<-glm(Navarra$defunciones_obs_redondeadas ~ base1.1+Navarra$dow+ns(Navarra$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion1.1<-crosspred(base1.1, model=modelo1.1, cen=20)

par(mfrow=c(1,2))
plot(prediccion1.1, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion1.1, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))

#

base2.1<-crossbasis((Navarra$tmed), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="ns", df=4))
modelo2.1<-glm(Navarra$defunciones_obs_redondeadas ~ base2.1+Navarra$dow+ns(Navarra$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion2.1<-crosspred(base2.1, model=modelo2.1, cen=20)

par(mfrow=c(1,2))
plot(prediccion2.1, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion2.1, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))

    #Nos quedamos con el modelo del análisis a nivel estatal. 'bs' para ambos con
    # 4 df para cada uno.

#---

  #A continuación hacer gráficos según distintos valores de la temp y del retardo,
  # y a ver qué ocurre en cada caso. 

plot(prediccion1.1, 'slices', lag=c(0,3,5,11), var=c(0,5,28,31), ylab='RR', ylim=c(0.90,1.08))

#---

  #Análisis por subgrupos de edad:

levels(Navarra$cod_gedad)

grupo_niños_Navarra<-subset(Navarra, cod_gedad=='0-14')
grupo_jovenes_Navarra<-subset(Navarra, cod_gedad=='15-44')
grupo_adultos_Navarra<-subset(Navarra, cod_gedad=='45-64')
grupo_mayores_Navarra<-subset(Navarra, cod_gedad=='65-74')
grupo_mas_mayores_Navarra<-subset(Navarra, cod_gedad=='75-84')
grupo_abuelos_Navarra<-subset(Navarra, cod_gedad=='+85')
grupo_mas65_Navarra<-subset(Navarra, cod_gedad=='+65')

####################################

#1) Niños: (0-14)
base_niños_Navarra<-crossbasis((grupo_niños_Navarra$tmed), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modelo_niños_Navarra<-glm(grupo_niños_Navarra$defunciones_obs_redondeadas ~ base_niños_Navarra+grupo_niños_Navarra$dow+ns(grupo_niños_Navarra$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion_niños_Navarra<-crosspred(base_niños_Navarra, model=modelo_niños_Navarra, cen=20)

par(mfrow=c(1,2))
plot(prediccion_niños_Navarra, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion_niños_Navarra, 'overall', xlab='Temperatura', ylab='RR', ylim=c(0,1.5))
par(mfrow=c(1,1))     
          #Estos gráficos no dicen nada. Hay pocos datos y mala aproximación.


#2) Jovenes: (15-44)
base_jovenes_Navarra<-crossbasis((grupo_jovenes_Navarra$tmed), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modelo_jovenes_Navarra<-glm(grupo_jovenes_Navarra$defunciones_obs_redondeadas ~ base_jovenes_Navarra+grupo_jovenes_Navarra$dow+ns(grupo_jovenes_Navarra$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion_jovenes_Navarra<-crosspred(base_jovenes_Navarra, model=modelo_jovenes_Navarra, cen=20)

par(mfrow=c(1,2))
plot(prediccion_jovenes_Navarra, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion_jovenes_Navarra, 'overall', xlab='Temperatura', ylab='RR', ylim=c(0,3))
par(mfrow=c(1,1))



#3) Adultos: (45-64)
base_adultos_Navarra<-crossbasis((grupo_adultos_Navarra$tmed), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modelo_adultos_Navarra<-glm(grupo_adultos_Navarra$defunciones_obs_redondeadas ~ base_adultos_Navarra+grupo_adultos_Navarra$dow+ns(grupo_adultos_Navarra$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion_adultos_Navarra<-crosspred(base_adultos_Navarra, model=modelo_adultos_Navarra, cen=20)

par(mfrow=c(1,2))
plot(prediccion_adultos_Navarra, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion_adultos_Navarra, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))



#4) Mayores: (65-74)
base_mayores_Navarra<-crossbasis((grupo_mayores_Navarra$tmed), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modelo_mayores_Navarra<-glm(grupo_mayores_Navarra$defunciones_obs_redondeadas ~ base_mayores_Navarra+grupo_mayores_Navarra$dow+ns(grupo_mayores_Navarra$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion_mayores_Navarra<-crosspred(base_mayores_Navarra, model=modelo_mayores_Navarra, cen=20)

par(mfrow=c(1,2))
plot(prediccion_mayores_Navarra, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion_mayores_Navarra, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))



#5) Más mayores: (75-84)
base_mas_mayores_Navarra<-crossbasis((grupo_mas_mayores_Navarra$tmed), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modelo_mas_mayores_Navarra<-glm(grupo_mas_mayores_Navarra$defunciones_obs_redondeadas ~ base_mas_mayores_Navarra+grupo_mas_mayores_Navarra$dow+ns(grupo_mas_mayores_Navarra$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion_mas_mayores_Navarra<-crosspred(base_mas_mayores_Navarra, model=modelo_mas_mayores_Navarra, cen=20)

par(mfrow=c(1,2))
plot(prediccion_mas_mayores_Navarra, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion_mas_mayores_Navarra, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))



#6) Abuelos: (+85)
base_abuelos_Navarra<-crossbasis((grupo_abuelos_Navarra$tmed), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modelo_abuelos_Navarra<-glm(grupo_abuelos_Navarra$defunciones_obs_redondeadas ~ base_abuelos_Navarra+grupo_abuelos_Navarra$dow+ns(grupo_abuelos_Navarra$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion_abuelos_Navarra<-crosspred(base_abuelos_Navarra, model=modelo_abuelos_Navarra, cen=20)

par(mfrow=c(1,2))
plot(prediccion_abuelos_Navarra, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion_abuelos_Navarra, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))



#7) Grupo +65:
base_mas65_Navarra<-crossbasis((grupo_mas65_Navarra$tmed), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modelo_mas65_Navarra<-glm(grupo_mas65_Navarra$defunciones_obs_redondeadas ~ base_mas65_Navarra+grupo_mas65_Navarra$dow+ns(grupo_mas65_Navarra$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion_mas65_Navarra<-crosspred(base_mas65_Navarra, model=modelo_mas65_Navarra, cen=20)

par(mfrow=c(1,2))
plot(prediccion_mas65_Navarra, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion_mas65_Navarra, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))

  #Para los grupos de menor edad se nota que faltan datos. Salen gráficos raros
  # e incoherentes.
