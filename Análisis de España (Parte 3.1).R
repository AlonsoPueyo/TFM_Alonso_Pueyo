#######
# 3.1) Análisis de la probabilidad de exceso de mortalidad atribuible a la 
#       temperatura en España periodo 2015-2024
#######

library(dlnm)
library(dplyr)

momo_provincial_temp <- readRDS("momo_provincial_temp.rds")

  #Hacemos primero un análisis descriptivo.
  #Como estamos con España en general, para esta primera parte seleccionamos todas
  # las filas.


which(is.na(momo_provincial_temp$tmed))
  #Hay muchos datos faltantes en las columnas de las temperaturas. Esto se debe
  # a que algunas estaciones no empezaron desde el 2015 a recoger datos, sino que
  # comenzaron después. La estación del aeropuerto de Murcia, por ejemplo, tiene
  # datos a partir de mayo del 2021.
  
  

#La siguiente línea es para quitar los valores faltantes si hiciese falta
momo_provincial_temp <- momo_provincial_temp[!is.na(momo_provincial_temp$tmed), ]

#---

  #Notamos que se usan comas como separador decimal, y R se espera que sean puntos.
  # Da error al querer convertir las columnas a numéricas con as.numeric().
  # Cambiamos las comas por puntos:
momo_provincial_temp$tmed <- as.numeric(gsub(",", ".", momo_provincial_temp$tmed))
momo_provincial_temp$tmax <- as.numeric(gsub(",", ".", momo_provincial_temp$tmax))
momo_provincial_temp$tmin <- as.numeric(gsub(",", ".", momo_provincial_temp$tmin))


#---
momo_provincial_temp$cod_sexo<-as.factor(momo_provincial_temp$cod_sexo)
momo_provincial_temp$nombre_sexo<-as.factor(momo_provincial_temp$nombre_sexo)
momo_provincial_temp$cod_gedad<-as.factor(momo_provincial_temp$cod_gedad)
momo_provincial_temp$nombre_gedad<-as.factor(momo_provincial_temp$nombre_gedad)

summary(momo_provincial_temp)
  #Se desprende de esto el siguiente análisis. Hay el
#   mismo número de observaciones para cada grupo porque cada fila es un día por
#   grupo de edad, sexo y lugar.
  #La media y la mediana de las temperaturas son semejantes, lo que indica que las
#   temperaturas se distribuyen equitativamente, sin muchos 'outliers'.
  #Con las defunciones, la media es mucho mayor que la mediana ya que hay muchos
#   días sin muertes, y los datos de mortalidad se concentran solo en ciertos días.


  #Se hacen gráficos que lo ilustren:
#1)
par(mfrow=c(1,3))
hist(momo_provincial_temp$tmin, xlab='Temperatura mínima', ylab='Frecuencia', main='')
hist(momo_provincial_temp$tmed, xlab='Temperatura media', ylab='Frecuencia', main='')
hist(momo_provincial_temp$tmax, xlab='Temperatura máxima', ylab='Frecuencia', main='')
par(mfrow=c(1,1))

#2)
hist(momo_provincial_temp$defunciones_observadas, xlab='Defunciones observadas', ylab='Frecuencia', main='')
    #Solo se ve la primera barra debido a la cantidad de datos que hay, pero
  #   muestra la idea.


#------

#MODELIZACIÓN:

library(dlnm)
library(splines)
momo_provincial_temp$defunciones_obs_redondeadas <- round(momo_provincial_temp$defunciones_observadas)
momo_provincial_temp$dow <- factor(weekdays(momo_provincial_temp$fecha_defuncion))


  #Cogemos una submuestra aleatoria estratificada:
set.seed(1234)
momo_provincial_temp$mes<-format(momo_provincial_temp$fecha_defuncion, "%m")
submuestra<-ungroup(sample_frac(group_by(momo_provincial_temp, mes),0.033))
  #Comprobamos que hemos seleccionado bien la submuestra:
summary(submuestra)
par(mfrow=c(1,3))
hist(submuestra$tmin, xlab='Temperatura mínima', ylab='Frecuencia', main='')
hist(submuestra$tmed, xlab='Temperatura media', ylab='Frecuencia', main='')
hist(submuestra$tmax, xlab='Temperatura máxima', ylab='Frecuencia', main='')
par(mfrow=c(1,1))
    
#---

base<-crossbasis((submuestra$tmed), lag=30,
                  argvar=list(fun="bs", df=3), arglag=list(fun="ns", df=3))
modelo<-glm(submuestra$defunciones_obs_redondeadas ~ base+submuestra$dow+
                    ns(submuestra$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion<-crosspred(base, model=modelo, cen=20)


par(mfrow=c(1,2))
plot(prediccion, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))
  

  #Para el calor parece que se ajusta bien, pero para el frío parece raro que el
  # RR sea menor que 1. Veamos cuántas observaciones se tienen.
sum(submuestra$tmin<5, na.rm=TRUE)
sum(submuestra$tmax>35, na.rm=TRUE)

#---
#---

#Hacemos el análisis de sensibilidad para elegir el mejor modelo

#1) 'ns' para ambos:

resultados_ns<-data.frame(df_temp=integer(), df_retardo=integer(), QAIC=numeric())

for (df_var in c(3:7)) {
  for (df_lag in c(3:7)) {
    base<-crossbasis((submuestra$tmed), lag=30, argvar=list(fun="ns", df=df_var), arglag=list(fun="ns", df=df_lag))
    modelo<-glm(submuestra$defunciones_obs_redondeadas ~ base+submuestra$dow+ns(submuestra$fecha_defuncion, df=10*7), family=quasipoisson())
    
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

#2) 'bs' para ambos:

resultados_bs<-data.frame(df_temp=integer(), df_retardo=integer(), QAIC=numeric())

for (df_var in c(3:7)) {
  for (df_lag in c(3:7)) {
    base<-crossbasis((submuestra$tmed), lag=30, argvar=list(fun="bs", df=df_var), arglag=list(fun="bs", df=df_lag))
    modelo<-glm(submuestra$defunciones_obs_redondeadas ~ base+submuestra$dow+ns(submuestra$fecha_defuncion, df=10*7), family=quasipoisson())
    
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


#3) 'ns' para temp. y 'bs' para lag:

resultados_ns_bs<-data.frame(df_temp=integer(), df_retardo=integer(), QAIC=numeric())

for (df_var in c(3:7)) {
  for (df_lag in c(3:7)) {
    base<-crossbasis((submuestra$tmed), lag=30, argvar=list(fun="ns", df=df_var), arglag=list(fun="bs", df=df_lag))
    modelo<-glm(submuestra$defunciones_obs_redondeadas ~ base+submuestra$dow+ns(submuestra$fecha_defuncion, df=10*7), family=quasipoisson())
    
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


#4) 'bs' para temp. y 'ns' para lag:

resultados_bs_ns<-data.frame(df_temp=integer(), df_retardo=integer(), QAIC=numeric())

for (df_var in c(3:7)) {
  for (df_lag in c(3:7)) {
    base<-crossbasis((submuestra$tmed), lag=30, argvar=list(fun="bs", df=df_var), arglag=list(fun="ns", df=df_lag))
    modelo<-glm(submuestra$defunciones_obs_redondeadas ~ base+submuestra$dow+ns(submuestra$fecha_defuncion, df=10*7), family=quasipoisson())
    
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

  #Se han guardado todos los resultados para no tener que volver a ejecutar
#         los bucles de nuevo. Solo hay que leerlos.

#---

#Seleccionamos el mejor de todos:
resultados_ns[which(resultados_ns$QAIC==min(resultados_ns$QAIC)),]
resultados_bs[which(resultados_bs$QAIC==min(resultados_bs$QAIC)),]
resultados_ns_bs[which(resultados_ns_bs$QAIC==min(resultados_ns_bs$QAIC)),]
resultados_bs_ns[which(resultados_bs_ns$QAIC==min(resultados_bs_ns$QAIC)),]
    
    #Parece que los dos mejores son:
#     1) B-splines para ambas dimensiones con 5 df para la temperatura y 7 para retardo
#     2) Spline natural para la temperatura y B-spline para el retardo con 3 df
#         para la temp. y 7 para el retardo.

#Hacemos los gráficos de predicción en ambos casos y evaluamos:
base1<-crossbasis((submuestra$tmed), lag=30, argvar=list(fun="bs", df=5), arglag=list(fun="bs", df=7))
modelo1<-glm(submuestra$defunciones_obs_redondeadas ~ base1+submuestra$dow+ns(submuestra$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion1<-crosspred(base1, model=modelo1, cen=20)

par(mfrow=c(1,2))
plot(prediccion1, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion1, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))

#####

base2<-crossbasis((submuestra$tmed), lag=30, argvar=list(fun="ns", df=3), arglag=list(fun="bs", df=7))
modelo2<-glm(submuestra$defunciones_obs_redondeadas ~ base2+submuestra$dow+ns(submuestra$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion2<-crosspred(base2, model=modelo2, cen=20)

par(mfrow=c(1,2))
plot(prediccion2, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion2, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))

    #En ambos casos parece que hay un sobreajuste de los datos. Los intervalos de
#     confianza tampoco cuadran.

  #Probaremos a reducir los grados de libertad del retardo pero con los mismos modelos:

base1.1<-crossbasis((submuestra$tmed), lag=30, argvar=list(fun="bs", df=5), arglag=list(fun="bs", df=5))
modelo1.1<-glm(submuestra$defunciones_obs_redondeadas ~ base1.1+submuestra$dow+ns(submuestra$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion1.1<-crosspred(base1.1, model=modelo1.1, cen=20)

par(mfrow=c(1,2))
plot(prediccion1.1, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion1.1, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))

####

base2.1<-crossbasis((submuestra$tmed), lag=30, argvar=list(fun="ns", df=3), arglag=list(fun="bs", df=5))
modelo2.1<-glm(submuestra$defunciones_obs_redondeadas ~ base2.1+submuestra$dow+ns(submuestra$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion2.1<-crosspred(base2.1, model=modelo2.1, cen=20)

par(mfrow=c(1,2))
plot(prediccion2.1, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion2.1, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))

    #El modelo 1.1 parece mejor. Tratamos de mejorar a partir de ahí.

base1.2<-crossbasis((submuestra$tmed), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modelo1.2<-glm(submuestra$defunciones_obs_redondeadas ~ base1.2+submuestra$dow+ns(submuestra$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion1.2<-crosspred(base1.2, model=modelo1.2, cen=20)

par(mfrow=c(1,2))
plot(prediccion1.2, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion1.2, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))

    #No podemos bajar más los df. Este modelo parece correcto. Nos quedaremos
#     con él.

####

  #Hacemos ahora el mismo modelo con todos los datos, no solo con los de la submuestra:

baseFinal<-crossbasis((momo_provincial_temp$tmed), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modeloFinal<-glm(momo_provincial_temp$defunciones_obs_redondeadas ~ baseFinal+momo_provincial_temp$dow+ns(momo_provincial_temp$fecha_defuncion, df=10*7), family=quasipoisson())
prediccionFinal<-crosspred(baseFinal, model=modeloFinal, cen=20)

par(mfrow=c(1,2))
plot(prediccionFinal, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccionFinal, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))


  #Hacemos 'slices' en ciertos puntos importantes:
plot(prediccionFinal, 'slices', lag=c(0,3,5,11), var=c(0,5,30,35), ylab='RR')

    #A partir del retardo 8, el RR del frío es >1. El RR del calor es mayor
    #los 5 primeros días de retardo.

####

  #Comparamos entre subgrupos de población:

levels(momo_provincial_temp$cod_gedad)
    #la población se divide en los grupos: 0-14, 15-44, 45-64, 65-74, 75-84, +85 (y +65)
    #No tendremos en cuenta el grupo 'all'

grupo_niños<-subset(momo_provincial_temp, cod_gedad=='0-14')
grupo_jovenes<-subset(momo_provincial_temp, cod_gedad=='15-44')
grupo_adultos<-subset(momo_provincial_temp, cod_gedad=='45-64')
grupo_mayores<-subset(momo_provincial_temp, cod_gedad=='65-74')
grupo_mas_mayores<-subset(momo_provincial_temp, cod_gedad=='75-84')
grupo_abuelos<-subset(momo_provincial_temp, cod_gedad=='+85')
grupo_mas65<-subset(momo_provincial_temp, cod_gedad=='+65')

###############################

#1) Niños: (0-14)
base_niños<-crossbasis((grupo_niños$tmed), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modelo_niños<-glm(grupo_niños$defunciones_obs_redondeadas ~ base_niños+grupo_niños$dow+ns(grupo_niños$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion_niños<-crosspred(base_niños, model=modelo_niños, cen=20)

par(mfrow=c(1,2))
plot(prediccion_niños, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion_niños, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))



#2) Jóvenes: (15-44)
base_jovenes<-crossbasis((grupo_jovenes$tmed), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modelo_jovenes<-glm(grupo_jovenes$defunciones_obs_redondeadas ~ base_jovenes+grupo_jovenes$dow+ns(grupo_jovenes$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion_jovenes<-crosspred(base_jovenes, model=modelo_jovenes, cen=20)

par(mfrow=c(1,2))
plot(prediccion_jovenes, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion_jovenes, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))



#3) Adultos: (45-64)
base_adultos<-crossbasis((grupo_adultos$tmed), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modelo_adultos<-glm(grupo_adultos$defunciones_obs_redondeadas ~ base_adultos+grupo_adultos$dow+ns(grupo_adultos$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion_adultos<-crosspred(base_adultos, model=modelo_adultos, cen=20)

par(mfrow=c(1,2))
plot(prediccion_adultos, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion_adultos, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))



#4) Mayores: (65-74)
base_mayores<-crossbasis((grupo_mayores$tmed), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modelo_mayores<-glm(grupo_mayores$defunciones_obs_redondeadas ~ base_mayores+grupo_mayores$dow+ns(grupo_mayores$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion_mayores<-crosspred(base_mayores, model=modelo_mayores, cen=20)

par(mfrow=c(1,2))
plot(prediccion_mayores, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion_mayores, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))



#5) Más mayores: (75-84)
base_mas_mayores<-crossbasis((grupo_mas_mayores$tmed), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modelo_mas_mayores<-glm(grupo_mas_mayores$defunciones_obs_redondeadas ~ base_mas_mayores+grupo_mas_mayores$dow+ns(grupo_mas_mayores$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion_mas_mayores<-crosspred(base_mas_mayores, model=modelo_mas_mayores, cen=20)

par(mfrow=c(1,2))
plot(prediccion_mas_mayores, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion_mas_mayores, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))



#6) Abuelos: (+85)
base_abuelos<-crossbasis((grupo_abuelos$tmed), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modelo_abuelos<-glm(grupo_abuelos$defunciones_obs_redondeadas ~ base_abuelos+grupo_abuelos$dow+ns(grupo_abuelos$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion_abuelos<-crosspred(base_abuelos, model=modelo_abuelos, cen=20)

par(mfrow=c(1,2))
plot(prediccion_abuelos, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion_abuelos, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))



#7) Más de 65 (+65)
base_mas65<-crossbasis((grupo_mas65$tmed), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modelo_mas65<-glm(grupo_mas65$defunciones_obs_redondeadas ~ base_mas65+grupo_mas65$dow+ns(grupo_mas65$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion_mas65<-crosspred(base_mas65, model=modelo_mas65, cen=20)

par(mfrow=c(1,2))
plot(prediccion_mas65, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion_mas65, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))




