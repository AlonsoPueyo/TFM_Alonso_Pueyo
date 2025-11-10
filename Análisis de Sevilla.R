library(dlnm)
library(dplyr)

momo_provincial_temp <- readRDS("momo_provincial_temp.rds")
Sevilla<-filter(momo_provincial_temp, nombre_ambito=='SEVILLA')

#para borrar las filas con tmed=NA:
Sevilla <- Sevilla[!is.na(Sevilla$tmed), ]

Sevilla$tmed <- as.numeric(gsub(",", ".", Sevilla$tmed))
Sevilla$tmax <- as.numeric(gsub(",", ".", Sevilla$tmax))
Sevilla$tmin <- as.numeric(gsub(",", ".", Sevilla$tmin))

Sevilla$cod_sexo<-as.factor(Sevilla$cod_sexo)
Sevilla$nombre_sexo<-as.factor(Sevilla$nombre_sexo)
Sevilla$cod_gedad<-as.factor(Sevilla$cod_gedad)
Sevilla$nombre_gedad<-as.factor(Sevilla$nombre_gedad)
Sevilla$nombre_ambito<-as.factor(Sevilla$nombre_ambito)

summary(Sevilla)

#---

#Defunciones por cada grupo de edad:

tapply(Sevilla$defunciones_observadas,
       Sevilla$cod_gedad,
       sum, na.rm = FALSE)

#---

#Hay que mejorar el análisis descriptivo:

Sevilla$mes<-format(Sevilla$fecha_defuncion, "%m")
Sevilla$año<-format(Sevilla$fecha_defuncion, "%y")

#Lo haremos también con los datos del 2023 y 2024.
Sevilla_23<-subset(Sevilla, año=='23')
Sevilla_24<-subset(Sevilla, año=='24')


#1) Media de temperatura y suma de defunciones por mes:
Sevilla_tmed_por_mes23<-aggregate(tmed~mes, data=Sevilla_23, FUN=mean)
Sevilla_tmed_por_mes24<-aggregate(tmed~mes, data=Sevilla_24, FUN=mean)
Sevilla_tmed_por_mes<-cbind(Sevilla_tmed_por_mes23, Sevilla_tmed_por_mes24)
Sevilla_tmed_por_mes


Sevilla_defunciones_diarias23<-subset(Sevilla_23, nombre_sexo=='todos' & nombre_gedad=='todos')
Sevilla_defunciones_diarias24<-subset(Sevilla_24, nombre_sexo=='todos' & nombre_gedad=='todos')

Sevilla_defunciones_por_mes23<-aggregate(defunciones_observadas~mes, data=Sevilla_defunciones_diarias23, FUN=sum)
Sevilla_defunciones_por_mes24<-aggregate(defunciones_observadas~mes, data=Sevilla_defunciones_diarias24, FUN=sum)
Sevilla_defunciones_por_mes<-cbind(Sevilla_defunciones_por_mes23, Sevilla_defunciones_por_mes24)
Sevilla_defunciones_por_mes

#Calculamos las tasas por 100000 habitantes:

#Trimestre 1 del 2023: (1.959.394)
(Sevilla_defunciones_por_mes23[1,2]/1959394)*100000
(Sevilla_defunciones_por_mes23[2,2]/1959394)*100000
(Sevilla_defunciones_por_mes23[3,2]/1959394)*100000

#Trimestre 2 del 2023: (1.960.628)
(Sevilla_defunciones_por_mes23[4,2]/1960628)*100000
(Sevilla_defunciones_por_mes23[5,2]/1960628)*100000
(Sevilla_defunciones_por_mes23[6,2]/1960628)*100000

#Trimestre 3 del 2023: (1.963.007)
(Sevilla_defunciones_por_mes23[7,2]/1963007)*100000
(Sevilla_defunciones_por_mes23[8,2]/1963007)*100000
(Sevilla_defunciones_por_mes23[9,2]/1963007)*100000

#Trimestre 4 del 2023: (1.966.439)
(Sevilla_defunciones_por_mes23[10,2]/1966439)*100000
(Sevilla_defunciones_por_mes23[11,2]/1966439)*100000
(Sevilla_defunciones_por_mes23[12,2]/1966439)*100000

###

#Trimestre 1 del 2024: (1.968.624)
(Sevilla_defunciones_por_mes24[1,2]/1968624)*100000
(Sevilla_defunciones_por_mes24[2,2]/1968624)*100000
(Sevilla_defunciones_por_mes24[3,2]/1968624)*100000

#Trimestre 2 del 2024: (1.970.354)
(Sevilla_defunciones_por_mes24[4,2]/1970354)*100000
(Sevilla_defunciones_por_mes24[5,2]/1970354)*100000
(Sevilla_defunciones_por_mes24[6,2]/1970354)*100000

#Trimestre 3 del 2024: (1.970.469)
(Sevilla_defunciones_por_mes24[7,2]/1970469)*100000
(Sevilla_defunciones_por_mes24[8,2]/1970469)*100000
(Sevilla_defunciones_por_mes24[9,2]/1970469)*100000

#Trimestre 4 del 2024: (1.973.366)
(Sevilla_defunciones_por_mes24[10,2]/1973366)*100000
(Sevilla_defunciones_por_mes24[11,2]/1973366)*100000
(Sevilla_defunciones_por_mes24[12,2]/1973366)*100000


tasas_Sevilla23<-c((Sevilla_defunciones_por_mes23[1,2]/1959394)*100000, (Sevilla_defunciones_por_mes23[2,2]/1959394)*100000, (Sevilla_defunciones_por_mes23[3,2]/1959394)*100000,
                   (Sevilla_defunciones_por_mes23[4,2]/1960628)*100000, (Sevilla_defunciones_por_mes23[5,2]/1960628)*100000, (Sevilla_defunciones_por_mes23[6,2]/1960628)*100000,
                   (Sevilla_defunciones_por_mes23[7,2]/1963007)*100000, (Sevilla_defunciones_por_mes23[8,2]/1963007)*100000, (Sevilla_defunciones_por_mes23[9,2]/1963007)*100000,
                   (Sevilla_defunciones_por_mes23[10,2]/1966439)*100000, (Sevilla_defunciones_por_mes23[11,2]/1966439)*100000, (Sevilla_defunciones_por_mes23[12,2]/1966439)*100000)

tasas_Sevilla24<-c((Sevilla_defunciones_por_mes24[1,2]/1968624)*100000, (Sevilla_defunciones_por_mes24[2,2]/1968624)*100000, (Sevilla_defunciones_por_mes24[3,2]/1968624)*100000,
                   (Sevilla_defunciones_por_mes24[4,2]/1970354)*100000, (Sevilla_defunciones_por_mes24[5,2]/1970354)*100000, (Sevilla_defunciones_por_mes24[6,2]/1970354)*100000,
                   (Sevilla_defunciones_por_mes24[7,2]/1970469)*100000, (Sevilla_defunciones_por_mes24[8,2]/1970469)*100000, (Sevilla_defunciones_por_mes24[9,2]/1970469)*100000,
                   (Sevilla_defunciones_por_mes24[10,2]/1973366)*100000, (Sevilla_defunciones_por_mes24[11,2]/1973366)*100000, (Sevilla_defunciones_por_mes24[12,2]/1973366)*100000)

tasas<-cbind(tasas_Sevilla23, tasas_Sevilla24)
tasas


#En gráfico:
meses=c('Enero', 'Feb.', 'Marzo', 'Abril', 'Mayo', 'Junio', 'Julio', 'Agosto', 'Sept.', 'Octubre', 'Noviembre', 'Diciembre')

par(mfrow=c(2,2))
plot(Sevilla_tmed_por_mes23$mes, Sevilla_tmed_por_mes23$tmed, type='b', xaxt='n', xlab='Mes (2023)', ylab='Temperatura media (Sevilla)', pch=16)
axis(1, at=1:12, labels=meses)
plot(Sevilla_defunciones_por_mes23$mes, tasas_Sevilla23, type='b', xaxt='n', xlab='Mes (2023)', ylab='Tasa de mortalidad (Sevilla)', pch=16)
axis(1, at=1:12, labels=meses)

plot(Sevilla_tmed_por_mes24$mes, Sevilla_tmed_por_mes24$tmed, type='b', xaxt='n', xlab='Mes (2024)', ylab='Temperatura media (Sevilla)', pch=16)
axis(1, at=1:12, labels=meses)
plot(Sevilla_defunciones_por_mes24$mes, tasas_Sevilla24, type='b', xaxt='n', xlab='Mes (2024)', ylab='Tasa de mortalidad (Sevilla)', pch=16)
axis(1, at=1:12, labels=meses)
par(mfrow=c(1,1))

#---

#2) Suma de defunciones por rangos de temperatura

Sevilla_temp_menor10_23<-filter(Sevilla_defunciones_diarias23, tmed<10)
(sum(Sevilla_temp_menor10_23$defunciones_observadas, na.rm=TRUE)/1966439)*100000
Sevilla_temp_menor10_24<-filter(Sevilla_defunciones_diarias24, tmed<10)
(sum(Sevilla_temp_menor10_24$defunciones_observadas, na.rm=TRUE)/1973366)*100000

Sevilla_temp_entre_10_20_23<-filter(Sevilla_defunciones_diarias23, tmed>=10 & tmed<20)
(sum(Sevilla_temp_entre_10_20_23$defunciones_observadas, na.rm=TRUE)/1966439)*100000
Sevilla_temp_entre_10_20_24<-filter(Sevilla_defunciones_diarias24, tmed>=10 & tmed<20)
(sum(Sevilla_temp_entre_10_20_24$defunciones_observadas, na.rm=TRUE)/1973366)*100000

Sevilla_temp_entre_20_30_23<-filter(Sevilla_defunciones_diarias23, tmed>=20 & tmed<30)
(sum(Sevilla_temp_entre_20_30_23$defunciones_observadas, na.rm=TRUE)/1966439)*100000
Sevilla_temp_entre_20_30_24<-filter(Sevilla_defunciones_diarias24, tmed>=20 & tmed<30)
(sum(Sevilla_temp_entre_20_30_24$defunciones_observadas, na.rm=TRUE)/1973366)*100000

Sevilla_temp_mas30_23<-filter(Sevilla_defunciones_diarias23, tmed>=30)
(sum(Sevilla_temp_mas30_23$defunciones_observadas, na.rm=TRUE)/1966439)*100000
Sevilla_temp_mas30_24<-filter(Sevilla_defunciones_diarias24, tmed>=30)
(sum(Sevilla_temp_mas30_24$defunciones_observadas, na.rm=TRUE)/1973366)*100000

#---

#3) Resumen

mean(c(Sevilla_tmed_por_mes23[7,2], Sevilla_tmed_por_mes23[8,2]))
mean(c(Sevilla_tmed_por_mes24[7,2], Sevilla_tmed_por_mes24[8,2]))
mean(c(Sevilla_tmed_por_mes23[1,2], Sevilla_tmed_por_mes23[2,2]))
mean(c(Sevilla_tmed_por_mes24[1,2], Sevilla_tmed_por_mes24[2,2]))

sum(Sevilla_defunciones_por_mes23[7,2], Sevilla_defunciones_por_mes23[8,2])
sum(Sevilla_defunciones_por_mes24[7,2], Sevilla_defunciones_por_mes24[8,2])
sum(Sevilla_defunciones_por_mes23[1,2], Sevilla_defunciones_por_mes23[2,2])
sum(Sevilla_defunciones_por_mes24[1,2], Sevilla_defunciones_por_mes24[2,2])

(sum(Sevilla_defunciones_por_mes23[7,2], Sevilla_defunciones_por_mes23[8,2])/1963007)*100000
(sum(Sevilla_defunciones_por_mes24[7,2], Sevilla_defunciones_por_mes24[8,2])/1970469)*100000
(sum(Sevilla_defunciones_por_mes23[1,2], Sevilla_defunciones_por_mes23[2,2])/1959394)*100000
(sum(Sevilla_defunciones_por_mes24[1,2], Sevilla_defunciones_por_mes24[2,2])/1968624)*100000

#---

#Los apartados 3) y 4) no tiene sentido ponerlos aquí porque hablan de un estudio
# por provincias.

#---

########
#MODELIZACIÓN:
########

library(splines)
Sevilla$defunciones_obs_redondeadas <- round(Sevilla$defunciones_observadas)
Sevilla$dow <- factor(weekdays(Sevilla$fecha_defuncion))

#Hacemos primero análisis con datos del 2023. Luego, con los del 2024:

Sevilla_23<-subset(Sevilla, año=='23')
Sevilla_24<-subset(Sevilla, año=='24')

#Análisis de sensibilidad:

#1) 'ns' para ambos:

resultados_Sevilla_23_ns<-data.frame(df_var=integer(), df_lag=integer(), QAIC=numeric())

for (df_var in c(3:7)) {
  for (df_lag in c(3:7)) {
    base<-crossbasis((Sevilla_23$tmed), lag=30, argvar=list(fun="ns", df=df_var), arglag=list(fun="ns", df=df_lag))
    modelo<-glm(Sevilla_23$defunciones_obs_redondeadas ~ base+Sevilla_23$dow+ns(Sevilla_23$fecha_defuncion, df=7), family=quasipoisson())
    
    k <- length(coef(modelo))
    phi <- summary(modelo)$dispersion
    ll <- logLik(update(modelo, family=poisson))
    qaic <- -2 * as.numeric(ll) + 2 *phi * k
    
    resultados_Sevilla_23_ns <- rbind(resultados_Sevilla_23_ns, data.frame(df_var=df_var, df_lag=df_lag, QAIC = qaic))
  }
}

print(resultados_Sevilla_23_ns)
saveRDS(resultados_Sevilla_23_ns, "resultados_Sevilla_23_ns.rds")
resultados_Sevilla_23_ns <- readRDS("resultados_Sevilla_23_ns.rds")



#2) 'bs' para ambos:

resultados_Sevilla_23_bs<-data.frame(df_var=integer(), df_lag=integer(), QAIC=numeric())

for (df_var in c(3:7)) {
  for (df_lag in c(4:7)) {
    base<-crossbasis((Sevilla_23$tmed), lag=30, argvar=list(fun="bs", df=df_var), arglag=list(fun="bs", df=df_lag))
    modelo<-glm(Sevilla_23$defunciones_obs_redondeadas ~ base+Sevilla_23$dow+ns(Sevilla_23$fecha_defuncion, df=7), family=quasipoisson())
    
    k <- length(coef(modelo))
    phi <- summary(modelo)$dispersion
    ll <- logLik(update(modelo, family=poisson))
    qaic <- -2 * as.numeric(ll) + 2 *phi * k
    
    resultados_Sevilla_23_bs <- rbind(resultados_Sevilla_23_bs, data.frame(df_var=df_var, df_lag=df_lag, QAIC = qaic))
  }
}

print(resultados_Sevilla_23_bs)
saveRDS(resultados_Sevilla_23_bs, "resultados_Sevilla_23_bs.rds")
resultados_Sevilla_23_bs <- readRDS("resultados_Sevilla_23_bs.rds")



#3) 'ns' para temp y 'bs' para lag:

resultados_Sevilla_23_ns_bs<-data.frame(df_var=integer(), df_lag=integer(), QAIC=numeric())

for (df_var in c(3:7)) {
  for (df_lag in c(4:7)) {
    base<-crossbasis((Sevilla_23$tmed), lag=30, argvar=list(fun="ns", df=df_var), arglag=list(fun="bs", df=df_lag))
    modelo<-glm(Sevilla_23$defunciones_obs_redondeadas ~ base+Sevilla_23$dow+ns(Sevilla_23$fecha_defuncion, df=7), family=quasipoisson())
    
    k <- length(coef(modelo))
    phi <- summary(modelo)$dispersion
    ll <- logLik(update(modelo, family=poisson))
    qaic <- -2 * as.numeric(ll) + 2 *phi * k
    
    resultados_Sevilla_23_ns_bs <- rbind(resultados_Sevilla_23_ns_bs, data.frame(df_var=df_var, df_lag=df_lag, QAIC = qaic))
  }
}

print(resultados_Sevilla_23_ns_bs)
saveRDS(resultados_Sevilla_23_ns_bs, "resultados_Sevilla_23_ns_bs.rds")
resultados_Sevilla_23_ns_bs <- readRDS("resultados_Sevilla_23_ns_bs.rds")


#4) 'bs' para temp y 'ns' para lag:

resultados_Sevilla_23_bs_ns<-data.frame(df_var=integer(), df_lag=integer(), QAIC=numeric())

for (df_var in c(3:7)) {
  for (df_lag in c(3:7)) {
    base<-crossbasis((Sevilla_23$tmed), lag=30, argvar=list(fun="bs", df=df_var), arglag=list(fun="ns", df=df_lag))
    modelo<-glm(Sevilla_23$defunciones_obs_redondeadas ~ base+Sevilla_23$dow+ns(Sevilla_23$fecha_defuncion, df=7), family=quasipoisson())
    
    k <- length(coef(modelo))
    phi <- summary(modelo)$dispersion
    ll <- logLik(update(modelo, family=poisson))
    qaic <- -2 * as.numeric(ll) + 2 *phi * k
    
    resultados_Sevilla_23_bs_ns <- rbind(resultados_Sevilla_23_bs_ns, data.frame(df_var=df_var, df_lag=df_lag, QAIC = qaic))
  }
}

print(resultados_Sevilla_23_bs_ns)
saveRDS(resultados_Sevilla_23_bs_ns, "resultados_Sevilla_23_bs_ns.rds")
resultados_Sevilla_23_bs_ns <- readRDS("resultados_Sevilla_23_bs_ns.rds")


#Seleccionamos el mejor de todos:
resultados_Sevilla_23_ns[which(resultados_Sevilla_23_ns$QAIC==min(resultados_Sevilla_23_ns$QAIC)),]
resultados_Sevilla_23_bs[which(resultados_Sevilla_23_bs$QAIC==min(resultados_Sevilla_23_bs$QAIC)),]
resultados_Sevilla_23_ns_bs[which(resultados_Sevilla_23_ns_bs$QAIC==min(resultados_Sevilla_23_ns_bs$QAIC)),]
resultados_Sevilla_23_bs_ns[which(resultados_Sevilla_23_bs_ns$QAIC==min(resultados_Sevilla_23_bs_ns$QAIC)),]

#---------------

#Hacemos el mismo análisis de sensibilidad para Navarra en el año 2024:

#1) 'ns' para ambos:

resultados_Sevilla_24_ns<-data.frame(df_var=integer(), df_lag=integer(), QAIC=numeric())

for (df_var in c(3:7)) {
  for (df_lag in c(3:7)) {
    base<-crossbasis((Sevilla_24$tmed), lag=30, argvar=list(fun="ns", df=df_var), arglag=list(fun="ns", df=df_lag))
    modelo<-glm(Sevilla_24$defunciones_obs_redondeadas ~ base+Sevilla_24$dow+ns(Sevilla_24$fecha_defuncion, df=7), family=quasipoisson())
    
    k <- length(coef(modelo))
    phi <- summary(modelo)$dispersion
    ll <- logLik(update(modelo, family=poisson))
    qaic <- -2 * as.numeric(ll) + 2 *phi * k
    
    resultados_Sevilla_24_ns <- rbind(resultados_Sevilla_24_ns, data.frame(df_var=df_var, df_lag=df_lag, QAIC = qaic))
  }
}

print(resultados_Sevilla_24_ns)
saveRDS(resultados_Sevilla_24_ns, "resultados_Sevilla_24_ns.rds")
resultados_Sevilla_24_ns <- readRDS("resultados_Sevilla_24_ns.rds")



#2) 'bs' para ambos:

resultados_Sevilla_24_bs<-data.frame(df_var=integer(), df_lag=integer(), QAIC=numeric())

for (df_var in c(3:7)) {
  for (df_lag in c(4:7)) {
    base<-crossbasis((Sevilla_24$tmed), lag=30, argvar=list(fun="bs", df=df_var), arglag=list(fun="bs", df=df_lag))
    modelo<-glm(Sevilla_24$defunciones_obs_redondeadas ~ base+Sevilla_24$dow+ns(Sevilla_24$fecha_defuncion, df=7), family=quasipoisson())
    
    k <- length(coef(modelo))
    phi <- summary(modelo)$dispersion
    ll <- logLik(update(modelo, family=poisson))
    qaic <- -2 * as.numeric(ll) + 2 *phi * k
    
    resultados_Sevilla_24_bs <- rbind(resultados_Sevilla_24_bs, data.frame(df_var=df_var, df_lag=df_lag, QAIC = qaic))
  }
}

print(resultados_Sevilla_24_bs)
saveRDS(resultados_Sevilla_24_bs, "resultados_Sevilla_24_bs.rds")
resultados_Sevilla_24_bs <- readRDS("resultados_Sevilla_24_bs.rds")



#3) 'ns' para temp y 'bs' para lag:

resultados_Sevilla_24_ns_bs<-data.frame(df_var=integer(), df_lag=integer(), QAIC=numeric())

for (df_var in c(3:7)) {
  for (df_lag in c(4:7)) {
    base<-crossbasis((Sevilla_24$tmed), lag=30, argvar=list(fun="ns", df=df_var), arglag=list(fun="bs", df=df_lag))
    modelo<-glm(Sevilla_24$defunciones_obs_redondeadas ~ base+Sevilla_24$dow+ns(Sevilla_24$fecha_defuncion, df=7), family=quasipoisson())
    
    k <- length(coef(modelo))
    phi <- summary(modelo)$dispersion
    ll <- logLik(update(modelo, family=poisson))
    qaic <- -2 * as.numeric(ll) + 2 *phi * k
    
    resultados_Sevilla_24_ns_bs <- rbind(resultados_Sevilla_24_ns_bs, data.frame(df_var=df_var, df_lag=df_lag, QAIC = qaic))
  }
}

print(resultados_Sevilla_24_ns_bs)
saveRDS(resultados_Sevilla_24_ns_bs, "resultados_Sevilla_24_ns_bs.rds")
resultados_Sevilla_24_ns_bs <- readRDS("resultados_Sevilla_24_ns_bs.rds")


#4) 'bs' para temp y 'ns' para lag:

resultados_Sevilla_24_bs_ns<-data.frame(df_var=integer(), df_lag=integer(), QAIC=numeric())

for (df_var in c(3:7)) {
  for (df_lag in c(3:7)) {
    base<-crossbasis((Sevilla_24$tmed), lag=30, argvar=list(fun="bs", df=df_var), arglag=list(fun="ns", df=df_lag))
    modelo<-glm(Sevilla_24$defunciones_obs_redondeadas ~ base+Sevilla_24$dow+ns(Sevilla_24$fecha_defuncion, df=7), family=quasipoisson())
    
    k <- length(coef(modelo))
    phi <- summary(modelo)$dispersion
    ll <- logLik(update(modelo, family=poisson))
    qaic <- -2 * as.numeric(ll) + 2 *phi * k
    
    resultados_Sevilla_24_bs_ns <- rbind(resultados_Sevilla_24_bs_ns, data.frame(df_var=df_var, df_lag=df_lag, QAIC = qaic))
  }
}

print(resultados_Sevilla_24_bs_ns)
saveRDS(resultados_Sevilla_24_bs_ns, "resultados_Sevilla_24_bs_ns.rds")
resultados_Sevilla_24_bs_ns <- readRDS("resultados_Sevilla_24_bs_ns.rds")


#Seleccionamos el mejor de todos:
resultados_Sevilla_24_ns[which(resultados_Sevilla_24_ns$QAIC==min(resultados_Sevilla_24_ns$QAIC)),]
resultados_Sevilla_24_bs[which(resultados_Sevilla_24_bs$QAIC==min(resultados_Sevilla_24_bs$QAIC)),]
resultados_Sevilla_24_ns_bs[which(resultados_Sevilla_24_ns_bs$QAIC==min(resultados_Sevilla_24_ns_bs$QAIC)),]
resultados_Sevilla_24_bs_ns[which(resultados_Sevilla_24_bs_ns$QAIC==min(resultados_Sevilla_24_bs_ns$QAIC)),]

#---------------

#Se eligen los mejores modelos para ambos años y se dibujan las predicciones
# para comparar.

base_Sevilla_23<-crossbasis((Sevilla_23$tmed), lag=30, argvar=list(fun="bs", df=3), arglag=list(fun="ns", df=3))
modelo_Sevilla_23<-glm(Sevilla_23$defunciones_obs_redondeadas ~ base_Sevilla_23+Sevilla_23$dow+ns(Sevilla_23$fecha_defuncion, df=7), family=quasipoisson())
prediccion_Sevilla_23<-crosspred(base_Sevilla_23, model=modelo_Sevilla_23, cen=20)

par(mfrow=c(1,2))
plot(prediccion_Sevilla_23, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion_Sevilla_23, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))

#

base_Sevilla_24<-crossbasis((Sevilla_24$tmed), lag=30, argvar=list(fun="bs", df=3), arglag=list(fun="ns", df=3))
modelo_Sevilla_24<-glm(Sevilla_24$defunciones_obs_redondeadas ~ base_Sevilla_24+Sevilla_24$dow+ns(Sevilla_24$fecha_defuncion, df=7), family=quasipoisson())
prediccion_Sevilla_24<-crosspred(base_Sevilla_24, model=modelo_Sevilla_24, cen=20)

par(mfrow=c(1,2))
plot(prediccion_Sevilla_24, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion_Sevilla_24, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))



############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
