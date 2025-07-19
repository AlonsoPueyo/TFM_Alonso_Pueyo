library(readr)
momo <-read_csv("momo.csv") 
View(momo)


  #Primero leemos las temperaturas de Navarra
library(Rcmdr)
TempNavarra <- 
  readXL("C:/Users/alons/OneDrive/Escritorio/Máster Modelización Alonso Pueyo/TFM/Trabajo/Datos/TempNavarra.xlsx",
         rownames=FALSE, header=TRUE, na="", sheet="Hoja1", stringsAsFactors=TRUE)

      #Hay algunos datos faltantes. En especial la temperatura media del 2020, aunque
      #podemos hacer la media de la temp máx y la mín, y faltan los datos del 2021, 
      #un par de días del 2024 y el 2025

  #Los datos son sacados de Meteo Navarra, de la estación automática Pamplona GN
  #Para los datos del 2021 hemos cogido los de la estación manual de Pamplona

is.na(TempNavarra$T_media)
which(is.na(TempNavarra$T_media))

  #Como las columnas las ha interpretado como factores, lo cambiamos para que sean numéricas:
TempNavarra$T_max <- as.numeric(as.character(TempNavarra$T_max))
TempNavarra$T_min <- as.numeric(as.character(TempNavarra$T_min))
TempNavarra$T_media <- as.numeric(as.character(TempNavarra$T_media))


TempNavarra$T_media[is.na(TempNavarra$T_media)] <- (
  TempNavarra$T_max[is.na(TempNavarra$T_media)] + 
  TempNavarra$T_min[is.na(TempNavarra$T_media)]
) / 2


which(is.na(TempNavarra$T_max))
which(is.na(TempNavarra$T_media))
which(is.na(TempNavarra$T_min))
    #Hay 7 datos que no tienen ni Temp. máx, ni mín ni media


#Juntamos todo en una misma base de datos:

library(dlnm)
library(dplyr)


help(crossbasis)

help(filter)
Navarra<-filter(momo, cod_ine_ambito==31)   #Con esto hacemos data frame solo con los datos de Navarara

   #Ponemos las fechas tipo Date para que no haya errores futuros
Navarra$fecha_defuncion <- as.Date(Navarra$fecha_defuncion, format = "%Y-%m-%d")
TempNavarra$Fecha <- as.Date(TempNavarra$Fecha, format = "%Y-%m-%d")


help(merge)
Navarra_con_temp<-merge(Navarra, TempNavarra, by.x='fecha_defuncion', by.y='Fecha', all.x=TRUE)
#Hemos juntado los dos data frames. Ahora tenemos la temperatura que hubo en cada día
    # y todos los datos

#-----

#Mejoramos el análisis descriptivo:

  #Ponemos todas las variables pertinentes en factor:
names(Navarra_con_temp)

Navarra_con_temp$cod_sexo<-as.factor(Navarra_con_temp$cod_sexo)
Navarra_con_temp$nombre_sexo<-as.factor(Navarra_con_temp$nombre_sexo)
Navarra_con_temp$cod_gedad<-as.factor(Navarra_con_temp$cod_gedad)
Navarra_con_temp$nombre_gedad<-as.factor(Navarra_con_temp$nombre_gedad)

summary(Navarra_con_temp)

  #Las 89040 observaciones se dividen de igual manera entre los grupos (tanto de edad
#   como por sexo). Esto pasa porque cada fila es un día por grupo de edad y de sexo.
#   Pasa lo mismo con la base de datos MoMo.

  #Notar que los valores de la media y la mediana de las temperaturas son parecidos,
#   lo que indica que los datos están distribuidos, y no hay casos excepcionales.

  #Al contrario ocurre con las defunciones_observadas (y las demás lo mismo). La
#   media vale casi el doble que la mediana. Esto indica que hay datos extremos que
#   suben la media. Esto puede deberse a que algunos días, después de mucho calor,
#   por ejemplo, ha habido muchos más fallecidos que en días normales.

#---

  #Como vamos a estudiar según los grupos de edad, veamos las defunciones para
#   cada grupo:

tapply(Navarra_con_temp$defunciones_observadas,
       Navarra_con_temp$cod_gedad,
       sum, na.rm = FALSE)
    #Los grupos más jovenes apenas tienen muertes, mientras que conforme se va
#    aumentando la edad, se mueren más. Notar que hay un grupo +65 y otro
#    65-74.


#Hacemos algunos gráficos para ilustrar lo que ocurre:

#1)
par(mfrow=c(1,3))
hist(Navarra_con_temp$T_min, xlab='Temperatura mínima', ylab='Frecuencia', main='')
hist(Navarra_con_temp$T_media, xlab='Temperatura media', ylab='Frecuencia', main='')
hist(Navarra_con_temp$T_max, xlab='Temperatura máxima', ylab='Frecuencia', main='')
par(mfrow=c(1,1))
    #Esto acompaña a la distribución de las temperaturas. Tiene sentido. No
#     hay muchos días con demasiado calor o demasiado frío

#2)
hist(Navarra_con_temp$defunciones_observadas, xlab='Defunciones observadas', ylab='Frecuencia', main='')
    #Como ya habíamos dicho. Muchos días no hay defunciones, o muy pocas. Más
#     defunciones=Menos frecuencia

#3)
plot(Navarra_con_temp$fecha_defuncion, Navarra_con_temp$T_min, type='l', ylab='')
plot(Navarra_con_temp$fecha_defuncion, Navarra_con_temp$T_media, type='l', ylab='')
plot(Navarra_con_temp$fecha_defuncion, Navarra_con_temp$T_max, type='l', ylab='')
    #Serie temporal de la temperatura media. En invierno baja. En verano sube.

#4)
boxplot(defunciones_observadas~cod_gedad, data=Navarra_con_temp)
    #Aquí ya se ve que cuanto más edad, más defunciones. Sobretodo a partir de los
#     75 años.

#5)
plot(Navarra_con_temp$T_media, Navarra_con_temp$defunciones_observadas)
plot(Navarra_con_temp$defunciones_observadas, Navarra_con_temp$T_media)


#------------------------------

#Una vez se tiene todo explicado, hacemos los primeros modelos.

Navarra_con_temp$defunciones_obs_redondeadas <- round(Navarra_con_temp$defunciones_observadas)
    #Hay que redondear porque si no a la hora de calcular la verosimilitud no puede
    #con números reales. Tienen que ser enteros.


#Hacemos el análisis de sensibilidad para elegir el mejor modelo:

  
#1) 'ns' para ambos:
  
resultados_ns<-data.frame(df_temp=integer(), df_retardo=integer(), QAIC=numeric())
  
for (df_var in c(3:7)) {
  for (df_lag in c(3:7)) {
    base<-crossbasis((Navarra_con_temp$T_media), lag=30, argvar=list(fun="ns", df=df_var), arglag=list(fun="ns", df=df_lag))
    modelo<-glm(Navarra_con_temp$defunciones_obs_redondeadas ~ base, family=quasipoisson())

    k <- length(coef(modelo))
    phi <- summary(modelo)$dispersion
    ll <- logLik(update(modelo, family=poisson))
    qaic <- -2 * as.numeric(ll) + 2 *phi * k
      
    resultados_ns <- rbind(resultados_ns, data.frame(df_temp=df_var, df_retardo=df_lag, QAIC = qaic))
  }
}
  
print(resultados_ns)

#2) 'bs' para ambos: (ponemos los df a partir de 4 porque salen avisos de que 3 es demasiado pequeño)

resultados_bs<-data.frame(df_temp=integer(), df_retardo=integer(), QAIC=numeric())

for (df_var in c(4:7)) {
  for (df_lag in c(4:7)) {
    base<-crossbasis((Navarra_con_temp$T_media), lag=30, argvar=list(fun="bs", df=df_var), arglag=list(fun="bs", df=df_lag))
    modelo<-glm(Navarra_con_temp$defunciones_obs_redondeadas ~ base, family=quasipoisson())
    
    k <- length(coef(modelo))
    phi <- summary(modelo)$dispersion
    ll <- logLik(update(modelo, family=poisson))
    qaic <- -2 * as.numeric(ll) + 2 *phi * k
    
    resultados_bs <- rbind(resultados_bs, data.frame(df_temp=df_var, df_retardo=df_lag, QAIC = qaic))
  }
}

print(resultados_bs)

#3) 'ns' para temp. y 'bs' para lag:

resultados_ns_bs<-data.frame(df_temp=integer(), df_retardo=integer(), QAIC=numeric())

for (df_var in c(3:7)) {
  for (df_lag in c(4:7)) {
    base<-crossbasis((Navarra_con_temp$T_media), lag=30, argvar=list(fun="ns", df=df_var), arglag=list(fun="bs", df=df_lag))
    modelo<-glm(Navarra_con_temp$defunciones_obs_redondeadas ~ base, family=quasipoisson())
    
    k <- length(coef(modelo))
    phi <- summary(modelo)$dispersion
    ll <- logLik(update(modelo, family=poisson))
    qaic <- -2 * as.numeric(ll) + 2 *phi * k
    
    resultados_ns_bs <- rbind(resultados_ns_bs, data.frame(df_temp=df_var, df_retardo=df_lag, QAIC = qaic))
  }
}

print(resultados_ns_bs)

#4) 'bs' para temp. y 'ns' para lag:

resultados_bs_ns<-data.frame(df_temp=integer(), df_retardo=integer(), QAIC=numeric())

for (df_var in c(3:7)) {
  for (df_lag in c(3:7)) {
    base<-crossbasis((Navarra_con_temp$T_media), lag=30, argvar=list(fun="bs", df=df_var), arglag=list(fun="ns", df=df_lag))
    modelo<-glm(Navarra_con_temp$defunciones_obs_redondeadas ~ base, family=quasipoisson())
    
    k <- length(coef(modelo))
    phi <- summary(modelo)$dispersion
    ll <- logLik(update(modelo, family=poisson))
    qaic <- -2 * as.numeric(ll) + 2 *phi * k
    
    resultados_bs_ns <- rbind(resultados_bs_ns, data.frame(df_temp=df_var, df_retardo=df_lag, QAIC = qaic))
  }
}

print(resultados_bs_ns)

#Seleccionamos el mejor de todos:
resultados_ns[which(resultados_ns$QAIC==min(resultados_ns$QAIC)),]
resultados_bs[which(resultados_bs$QAIC==min(resultados_bs$QAIC)),]
resultados_ns_bs[which(resultados_ns_bs$QAIC==min(resultados_ns_bs$QAIC)),]
resultados_bs_ns[which(resultados_bs_ns$QAIC==min(resultados_bs_ns$QAIC)),]

  #El mejor es el QAIC donde se usa un 'bs' para la temperatura y 'ns' para retardo.
  #Toma un valor de 690782.2, y df_temp=df_lag=3


base_mejor<-crossbasis((Navarra_con_temp$T_media), lag=30, argvar=list(fun="bs", df=3), arglag=list(fun="ns", df=3))
modelo_mejor<-glm(Navarra_con_temp$defunciones_obs_redondeadas ~ base_mejor, family=quasipoisson())
prediccion_mejor<-crosspred(base_mejor, model=modelo_mejor)

par(mfrow=c(1,2))
plot(prediccion_mejor, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion_mejor, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))


#-------------

#A continuación estudiamos el modelo con alguna variable más:

resultados_ns.1<-data.frame(df_temp=integer(), df_retardo=integer(), QAIC=numeric())
for (df_var in c(3:5)) {
  for (df_lag in c(3:5)) {
    base<-crossbasis((Navarra_con_temp$T_media), lag=30, argvar=list(fun="ns", df=df_var), arglag=list(fun="ns", df=df_lag))
    modelo<-glm(Navarra_con_temp$defunciones_obs_redondeadas ~ base+ns(Navarra_con_temp$fecha_defuncion, df=10*7), family=quasipoisson())
    
    k <- length(coef(modelo))
    phi <- summary(modelo)$dispersion
    ll <- logLik(update(modelo, family=poisson))
    qaic <- -2 * as.numeric(ll) + 2 *phi * k
    
    resultados_ns.1 <- rbind(resultados_ns.1, data.frame(df_temp=df_var, df_retardo=df_lag, QAIC = qaic))
  }
}
print(resultados_ns.1)


resultados_bs.1<-data.frame(df_temp=integer(), df_retardo=integer(), QAIC=numeric())
for (df_var in c(3:5)) {
  for (df_lag in c(3:5)) {
    base<-crossbasis((Navarra_con_temp$T_media), lag=30, argvar=list(fun="bs", df=df_var), arglag=list(fun="bs", df=df_lag))
    modelo<-glm(Navarra_con_temp$defunciones_obs_redondeadas ~ base+ns(Navarra_con_temp$fecha_defuncion, df=10*7), family=quasipoisson())
    
    k <- length(coef(modelo))
    phi <- summary(modelo)$dispersion
    ll <- logLik(update(modelo, family=poisson))
    qaic <- -2 * as.numeric(ll) + 2 *phi * k
    
    resultados_bs.1 <- rbind(resultados_bs.1, data.frame(df_temp=df_var, df_retardo=df_lag, QAIC = qaic))
  }
}
print(resultados_bs.1)


resultados_ns_bs.1<-data.frame(df_temp=integer(), df_retardo=integer(), QAIC=numeric())
for (df_var in c(3:5)) {
  for (df_lag in c(3:5)) {
    base<-crossbasis((Navarra_con_temp$T_media), lag=30, argvar=list(fun="ns", df=df_var), arglag=list(fun="bs", df=df_lag))
    modelo<-glm(Navarra_con_temp$defunciones_obs_redondeadas ~ base+ns(Navarra_con_temp$fecha_defuncion, df=10*7), family=quasipoisson())
    
    k <- length(coef(modelo))
    phi <- summary(modelo)$dispersion
    ll <- logLik(update(modelo, family=poisson))
    qaic <- -2 * as.numeric(ll) + 2 *phi * k
    
    resultados_ns_bs.1 <- rbind(resultados_ns_bs.1, data.frame(df_temp=df_var, df_retardo=df_lag, QAIC = qaic))
  }
}
print(resultados_ns_bs.1)


resultados_bs_ns.1<-data.frame(df_temp=integer(), df_retardo=integer(), QAIC=numeric())
for (df_var in c(3:5)) {
  for (df_lag in c(3:5)) {
    base<-crossbasis((Navarra_con_temp$T_media), lag=30, argvar=list(fun="bs", df=df_var), arglag=list(fun="ns", df=df_lag))
    modelo<-glm(Navarra_con_temp$defunciones_obs_redondeadas ~ base+ns(Navarra_con_temp$fecha_defuncion, df=10*7), family=quasipoisson())
    
    k <- length(coef(modelo))
    phi <- summary(modelo)$dispersion
    ll <- logLik(update(modelo, family=poisson))
    qaic <- -2 * as.numeric(ll) + 2 *phi * k
    
    resultados_bs_ns.1 <- rbind(resultados_bs_ns.1, data.frame(df_temp=df_var, df_retardo=df_lag, QAIC = qaic))
  }
}
print(resultados_bs_ns.1)


#Seleccionamos el mejor de todos:
resultados_ns.1[which(resultados_ns.1$QAIC==min(resultados_ns.1$QAIC)),]
resultados_bs.1[which(resultados_bs.1$QAIC==min(resultados_bs.1$QAIC)),]
resultados_ns_bs.1[which(resultados_ns_bs.1$QAIC==min(resultados_ns_bs.1$QAIC)),]
resultados_bs_ns.1[which(resultados_bs_ns.1$QAIC==min(resultados_bs_ns.1$QAIC)),]
  #La opcion para el menor QAIC sigue siendo 'bs' para la temp. y 'ns' para
  #el retardo, con 3 df para cada uno. QAIC=686365.2. Estos modelos son mejores
  #que los anteriores.

base_mejor.1<-crossbasis((Navarra_con_temp$T_media), lag=30, argvar=list(fun="bs", df=3), arglag=list(fun="ns", df=3))
modelo_mejor.1<-glm(Navarra_con_temp$defunciones_obs_redondeadas ~ base_mejor.1+ns(Navarra_con_temp$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion_mejor.1<-crosspred(base_mejor.1, model=modelo_mejor.1)

par(mfrow=c(1,2))
plot(prediccion_mejor.1, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion_mejor.1, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))

#---

#Podemos hacer 'slices' para ver lo que ocurre en algún valor concreto:
plot(prediccion_mejor, 'slices', var=c(0,25,30))
plot(prediccion_mejor, 'slices', lag=c(0,3,5))



#------------------------------

#Hacemos el análisis por subgrupos de edad:

names(Navarra_con_temp)
Navarra_con_temp$cod_gedad<-as.factor(Navarra_con_temp$cod_gedad)
levels(Navarra_con_temp$cod_gedad)
    #la población se divide en grupos: 0-14, 15-44, 45-64, 65-74, 75-84, +85
    #Después, tambien tenemos el grupo all y el +65. Nos fijamos en los de arriba

#Ponemos a los de +65 en el grupo de 65-74:

Navarra_con_temp$cod_gedad[Navarra_con_temp$cod_gedad == '+65'] <- '65-74'


#Hacemos el análisis:
grupo_niños<-subset(Navarra_con_temp, cod_gedad=='0-14')
grupo_jovenes<-subset(Navarra_con_temp, cod_gedad=='15-44')
grupo_adultos<-subset(Navarra_con_temp, cod_gedad=='45-64')
grupo_mayores<-subset(Navarra_con_temp, cod_gedad=='65-74')
grupo_mas_mayores<-subset(Navarra_con_temp, cod_gedad=='75-84')
grupo_abuelos<-subset(Navarra_con_temp, cod_gedad=='+85')




