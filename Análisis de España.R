#######
# 3.1) Análisis de la probabilidad de exceso de mortalidad atribuible a la 
#       temperatura en España periodo 2015-2024
#######

library(dlnm)
library(dplyr)

momo_provincial_temp <- readRDS("momo_provincial_temp.rds")

  #Hacemos primero un análisis descriptivo.

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
hist(momo_provincial_temp$tmin, xlab='Temperatura mínima', ylab='Frecuencia', main='')
abline(v=mean(momo_provincial_temp$tmin), col='red')
abline(v=median(momo_provincial_temp$tmin), col='blue')

hist(momo_provincial_temp$tmed, xlab='Temperatura media', ylab='Frecuencia', main='')
abline(v=mean(momo_provincial_temp$tmed), col='red')
abline(v=median(momo_provincial_temp$tmed), col='blue')

hist(momo_provincial_temp$tmax, xlab='Temperatura máxima', ylab='Frecuencia', main='')
abline(v=mean(momo_provincial_temp$tmax), col='red')
abline(v=median(momo_provincial_temp$tmax), col='blue')


#2)
hist(momo_provincial_temp$defunciones_observadas, xlab='Defunciones observadas', ylab='Frecuencia', main='')
abline(v=mean(momo_provincial_temp$defunciones_observadas, na.rm=TRUE), col='red')
abline(v=median(momo_provincial_temp$defunciones_observadas, na.rm=TRUE), col='blue')
    #Solo se ve la primera barra debido a la cantidad de datos que hay, pero
  #   muestra la idea.


#-----
#Mejoramos el análisis descriptivo. Vamos a ver más cosas que nos digan algo más.

  #Lo haremos con los datos de 2023 y 2024, únicamente...
momo_provincial_temp$mes<-format(momo_provincial_temp$fecha_defuncion, "%m")
momo_provincial_temp$año<-format(momo_provincial_temp$fecha_defuncion, "%y")

momo_23<-subset(momo_provincial_temp, año=='23')
momo_24<-subset(momo_provincial_temp, año=='24')
momo_23_24<-subset(momo_provincial_temp, año=='23' | año=='24')


#1) Media de temperatura y defunciones por mes:
tmed_por_mes23<-aggregate(tmed~mes, data=momo_23, FUN=mean)
tmed_por_mes24<-aggregate(tmed~mes, data=momo_24, FUN=mean)
tmed_por_mes<-cbind(tmed_por_mes23, tmed_por_mes24)
tmed_por_mes  #Primera columna es la de 2023 y la segunda la de 2024

  #Nota: la temperatura media en todas las filas correspondientes a un mismo día
  # tiene el mismo valor. Por eso lo anterior sirve para calcular la temp media por
  # mes. Para las defunciones observadas, este valor cambia para cada grupo. Por eso
  # hay que tomar solo las filas que indiquen las defunciones observadas ESE día,
  # y no todas las filas. Dichas filas son las que pertenecen al grupo 'todos' en
  # nombre_sexo y nombre_gedad.

defunciones_diarias23<-subset(momo_23, nombre_sexo=='todos' & nombre_gedad=='todos')
defunciones_diarias24<-subset(momo_24, nombre_sexo=='todos' & nombre_gedad=='todos')

defunciones_por_mes23<-aggregate(defunciones_observadas~mes, data=defunciones_diarias23, FUN=sum)
defunciones_por_mes24<-aggregate(defunciones_observadas~mes, data=defunciones_diarias24, FUN=sum)
defunciones_por_mes<-cbind(defunciones_por_mes23, defunciones_por_mes24)
defunciones_por_mes  #Esto indica la suma del número de defunciones diarias observadas en
                    # toda España cada mes

  #Nota: Las tablas anteriores no se incluyen en el documento. Se pone todo en los
  # gráficos siguientes. Además, no ponemos las defunciones absolutas por mes, sino que
  # nos decantamos por las tasas por 100.000 habitantes.

#Trimestre 1 del 2023: (48.085.361)
(defunciones_por_mes23[1,2]/48085361)*100000
(defunciones_por_mes23[2,2]/48085361)*100000
(defunciones_por_mes23[3,2]/48085361)*100000

#Trimestre 2 del 2023: (48.205.962)
(defunciones_por_mes23[4,2]/48205962)*100000
(defunciones_por_mes23[5,2]/48205962)*100000
(defunciones_por_mes23[6,2]/48205962)*100000

#Trimestre 3 del 2023: (48.320.520)
(defunciones_por_mes23[7,2]/48320520)*100000
(defunciones_por_mes23[8,2]/48320520)*100000
(defunciones_por_mes23[9,2]/48320520)*100000

#Trimestre 4 del 2023: (48.486.865)
(defunciones_por_mes23[10,2]/48486865)*100000
(defunciones_por_mes23[11,2]/48486865)*100000
(defunciones_por_mes23[12,2]/48486865)*100000

####

#Trimestre 1 del 2024: (48.619.695)
(defunciones_por_mes24[1,2]/48619695)*100000
(defunciones_por_mes24[2,2]/48619695)*100000
(defunciones_por_mes24[3,2]/48619695)*100000

#Trimestre 2 del 2024: (48.723.394)
(defunciones_por_mes24[4,2]/48723394)*100000
(defunciones_por_mes24[5,2]/48723394)*100000
(defunciones_por_mes24[6,2]/48723394)*100000

#Trimestre 3 del 2024: (48.807.474)
(defunciones_por_mes24[7,2]/48807474)*100000
(defunciones_por_mes24[8,2]/48807474)*100000
(defunciones_por_mes24[9,2]/48807474)*100000

#Trimestre 4 del 2024: (48.966.300)
(defunciones_por_mes24[10,2]/48966300)*100000
(defunciones_por_mes24[11,2]/48966300)*100000
(defunciones_por_mes24[12,2]/48966300)*100000

tasas23<-c((defunciones_por_mes23[1,2]/48085361)*100000, (defunciones_por_mes23[2,2]/48085361)*100000, (defunciones_por_mes23[3,2]/48085361)*100000,
         (defunciones_por_mes23[4,2]/48205962)*100000, (defunciones_por_mes23[5,2]/48205962)*100000, (defunciones_por_mes23[6,2]/48205962)*100000,
         (defunciones_por_mes23[7,2]/48320520)*100000, (defunciones_por_mes23[8,2]/48320520)*100000, (defunciones_por_mes23[9,2]/48320520)*100000,
         (defunciones_por_mes23[10,2]/48486865)*100000, (defunciones_por_mes23[11,2]/48486865)*100000, (defunciones_por_mes23[12,2]/48486865)*100000)

tasas24<-c((defunciones_por_mes24[1,2]/48619695)*100000, (defunciones_por_mes24[2,2]/48619695)*100000, (defunciones_por_mes24[3,2]/48619695)*100000,
           (defunciones_por_mes24[4,2]/48723394)*100000, (defunciones_por_mes24[5,2]/48723394)*100000, (defunciones_por_mes24[6,2]/48723394)*100000,
           (defunciones_por_mes24[7,2]/48807474)*100000, (defunciones_por_mes24[8,2]/48807474)*100000, (defunciones_por_mes24[9,2]/48807474)*100000,
           (defunciones_por_mes24[10,2]/48966300)*100000, (defunciones_por_mes24[11,2]/48966300)*100000, (defunciones_por_mes24[12,2]/48966300)*100000)

tasas<-cbind(tasas23, tasas24)
tasas



    #Realizamos los gráficos:
meses=c('Enero', 'Feb.', 'Marzo', 'Abril', 'Mayo', 'Junio', 'Julio', 'Agosto', 'Sept.', 'Octubre', 'Noviembre', 'Diciembre')

par(mfrow=c(2,2))
plot(tmed_por_mes23$mes, tmed_por_mes23$tmed, type='b', xaxt='n', xlab='Mes (2023)', ylab='Temperatura media', pch=16)
  axis(1, at=1:12, labels=meses)
plot(defunciones_por_mes23$mes, tasas23, type='b', xaxt='n', xlab='Mes(2023)', ylab='Tasa de mortalidad por 100000 habitantes', pch=16)  
  axis(1, at=1:12, labels=meses)

plot(tmed_por_mes24$mes, tmed_por_mes24$tmed, type='b', xaxt='n', xlab='Mes (2024)', ylab='Temperatura media', pch=16)
  axis(1, at=1:12, labels=meses)
plot(defunciones_por_mes24$mes, tasas24, type='b', xaxt='n', xlab='Mes(2024)', ylab='Tasa de mortalidad por 100000 habitantes', pch=16)  
  axis(1, at=1:12, labels=meses)
par(mfrow=c(1,1))
      #El hecho de que haya más defunciones en los meses 
      # de invierno es debido a que la gente muere por enfermedades relativas
      # al frío, como la gripe.

  #NOTA: Hacer estos mismos gráficos para el análisis de Navarra.


#---

#2) Suma de defunciones por rangos de temperatura

temp_menor10_23<-filter(defunciones_diarias23, tmed<10)
(sum(temp_menor10_23$defunciones_observadas, na.rm=TRUE)/48486865)*100000
temp_menor10_24<-filter(defunciones_diarias24, tmed<10)
(sum(temp_menor10_24$defunciones_observadas, na.rm=TRUE)/48966300)*100000

temp_entre_10_20_23<-filter(defunciones_diarias23, tmed>=10 & tmed<20)
(sum(temp_entre_10_20_23$defunciones_observadas, na.rm=TRUE)/48486865)*100000
temp_entre_10_20_24<-filter(defunciones_diarias24, tmed>=10 & tmed<20)
(sum(temp_entre_10_20_24$defunciones_observadas, na.rm=TRUE)/48966300)*100000

temp_entre_20_30_23<-filter(defunciones_diarias23, tmed>=20 & tmed<30)
(sum(temp_entre_20_30_23$defunciones_observadas, na.rm=TRUE)/48486865)*100000
temp_entre_20_30_24<-filter(defunciones_diarias24, tmed>=20 & tmed<30)
(sum(temp_entre_20_30_24$defunciones_observadas, na.rm=TRUE)/48966300)*100000

temp_mas30_23<-filter(defunciones_diarias23, tmed>=30)
(sum(temp_mas30_23$defunciones_observadas, na.rm=TRUE)/48486865)*100000
temp_mas30_24<-filter(defunciones_diarias24, tmed>=30)
(sum(temp_mas30_24$defunciones_observadas, na.rm=TRUE)/48966300)*100000
  #En documento decir también el número de observaciones que tiene cada rango
  # de temperatura y que eso influye en que apenas haya muertes los días de más
  # de 30 grados y que haya muchas entre los 10 y los 30...


#---

#3) Medias por provincias:

  #Vamos a seleccionar solo los meses de verano (julio y agosto) de los años 2023 y 2024
  # y los de invierno (enero y febrero) para ver la media de la tmed y la suma de 
  # defunciones en cada sitio:

momo_verano23<-subset(momo_23, mes=='07' | mes=='08')
momo_verano24<-subset(momo_24, mes=='07' | mes=='08')
momo_invierno23<-subset(momo_23, mes=='01' | mes=='02')
momo_invierno24<-subset(momo_24, mes=='01' | mes=='02')

tmed_verano23<-aggregate(tmed~nombre_ambito, data=momo_verano23, FUN=mean)
tmed_verano24<-aggregate(tmed~nombre_ambito, data=momo_verano24, FUN=mean)
tmed_invierno23<-aggregate(tmed~nombre_ambito, data=momo_invierno23, FUN=mean)
tmed_invierno24<-aggregate(tmed~nombre_ambito, data=momo_invierno24, FUN=mean)
tmed_por_provincia<-cbind(tmed_verano23, tmed_verano24, tmed_invierno23, tmed_invierno24)
tmed_por_provincia  

  #Seleccionamos solo Madrid, Barcelona, Zaragoza, Navarra, Sevilla, Asturias:
tmed_por_provincia[c(6,10,32,36,43,51),]


def_diarias_verano23<-subset(momo_verano23, nombre_sexo=='todos' & nombre_gedad=='todos')
def_diarias_verano24<-subset(momo_verano24, nombre_sexo=='todos' & nombre_gedad=='todos')
def_diarias_invierno23<-subset(momo_invierno23, nombre_sexo=='todos' & nombre_gedad=='todos')
def_diarias_invierno24<-subset(momo_invierno24, nombre_sexo=='todos' & nombre_gedad=='todos')

def_provincia_verano23<-aggregate(defunciones_observadas~nombre_ambito, data=def_diarias_verano23, FUN=sum)
def_provincia_verano24<-aggregate(defunciones_observadas~nombre_ambito, data=def_diarias_verano24, FUN=sum)
def_provincia_invierno23<-aggregate(defunciones_observadas~nombre_ambito, data=def_diarias_invierno23, FUN=sum)
def_provincia_invierno24<-aggregate(defunciones_observadas~nombre_ambito, data=def_diarias_invierno24, FUN=sum)
defunciones_por_provincia<-cbind(def_provincia_verano23, def_provincia_verano24, def_provincia_invierno23, def_provincia_invierno24)
defunciones_por_provincia

  #Seleccionamos solo Madrid, Barcelona, Zaragoza, Navarra, Sevilla, Asturias:
defunciones_por_provincia[c(6,10,32,36,43,51),]


    #Estudiar el número de defunciones observadas en cada sitio. Dado que el
    # número de habitantes cambia, hay que estudiar la tasa por cada 100.000 habitantes

#---

#4) Tasa de mortalidad por cada 100.000 habitantes:

  #Asturias: 1.009.599
  #Barcelona: 5.877.672
  #Madrid: 7.009.268
  #Navarra: 678.333
  #Sevilla: 1.968.624
  #Zaragoza: 987.763

#Los datos de población según la provincia fueron sacados del INE a fecha del 2024

(defunciones_por_provincia[6,c(2,4,6,8)]/1009599)*100000  #Asturias
(defunciones_por_provincia[10,c(2,4,6,8)]/5877672)*100000 #Barcelona
(defunciones_por_provincia[32,c(2,4,6,8)]/7009268)*100000 #Madrid
(defunciones_por_provincia[36,c(2,4,6,8)]/678333)*100000  #Navarra
(defunciones_por_provincia[43,c(2,4,6,8)]/1968624)*100000 #Sevilla
(defunciones_por_provincia[51,c(2,4,6,8)]/987763)*100000  #Zaragoza

    #NOTA: Cuando ponemos [,], el primer número indica la provincia (Barcelona es
    # el 10, Zgz es el 51...) y el segundo indica el periodo de tiempo (2=verano 23, 4=verano 24,
    # 6=invierno 23, 8=invierno 24)

  #Ahora sí que tiene sentido. Antes calculábamos la tasa con la media de muertes
  # en cada sitio y por eso salían 2. Ahora calculamos la suma total de muertes en
  #cada lugar durante un periodo de tiempo dado.

#------

#MODELIZACIÓN:

library(dlnm)
library(splines)
momo_provincial_temp$defunciones_obs_redondeadas <- round(momo_provincial_temp$defunciones_observadas)
momo_provincial_temp$dow <- factor(weekdays(momo_provincial_temp$fecha_defuncion))


  #Cogemos solo el año 2024. Así, reducimos la carga computacional. Lo hemos
  # intentado con el 2023 y el 2024 pero el análisis de sensibilidad es eterno...
  #Luego, comparamos con año 2023...
momo_24<-subset(momo_provincial_temp, año=='24')


#Primer modelo:
base_24<-crossbasis((momo_24$tmed), lag=30,
                  argvar=list(fun="bs", df=3), arglag=list(fun="ns", df=3))
modelo_24<-glm(momo_24$defunciones_obs_redondeadas ~ base_24+momo_24$dow+
                    ns(momo_24$fecha_defuncion, df=7), family=quasipoisson())
prediccion_24<-crosspred(base_24, model=modelo_24, cen=20)


par(mfrow=c(1,2))
plot(prediccion_24, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion_24, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))
  
#---
#---

#Hacemos el análisis de sensibilidad para elegir el mejor modelo (hay que quitar
#   los NA de tmed antes de tomar la submuestra para que no dé error)

#1) 'ns' para ambos:

resultados_ns<-data.frame(df_var=integer(), df_lag=integer(), QAIC=numeric())

for (df_var in c(3:7)) {
  for (df_lag in c(3:7)) {
    base<-crossbasis((momo_24$tmed), lag=30, argvar=list(fun="ns", df=df_var), arglag=list(fun="ns", df=df_lag))
    modelo<-glm(momo_24$defunciones_obs_redondeadas ~ base+momo_24$dow+ns(momo_24$fecha_defuncion, df=7), family=quasipoisson())
    
    k <- length(coef(modelo))
    phi <- summary(modelo)$dispersion
    ll <- logLik(update(modelo, family=poisson))
    qaic <- -2 * as.numeric(ll) + 2 *phi * k
    
    resultados_ns <- rbind(resultados_ns, data.frame(df_var=df_var, df_lag=df_lag, QAIC = qaic))
  }
}

print(resultados_ns)
saveRDS(resultados_ns, "resultados_ns.rds")
resultados_ns <- readRDS("resultados_ns.rds")

#2) 'bs' para ambos:

resultados_bs<-data.frame(df_temp=integer(), df_retardo=integer(), QAIC=numeric())

for (df_var in c(3:7)) {
  for (df_lag in c(4:7)) {
    base<-crossbasis((momo_24$tmed), lag=30, argvar=list(fun="bs", df=df_var), arglag=list(fun="bs", df=df_lag))
    modelo<-glm(momo_24$defunciones_obs_redondeadas ~ base+momo_24$dow+ns(momo_24$fecha_defuncion, df=7), family=quasipoisson())
    
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
  for (df_lag in c(4:7)) {
    base<-crossbasis((momo_24$tmed), lag=30, argvar=list(fun="ns", df=df_var), arglag=list(fun="bs", df=df_lag))
    modelo<-glm(momo_24$defunciones_obs_redondeadas ~ base+momo_24$dow+ns(momo_24$fecha_defuncion, df=7), family=quasipoisson())
    
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
    base<-crossbasis((momo_24$tmed), lag=30, argvar=list(fun="bs", df=df_var), arglag=list(fun="ns", df=df_lag))
    modelo<-glm(momo_24$defunciones_obs_redondeadas ~ base+momo_24$dow+ns(momo_24$fecha_defuncion, df=7), family=quasipoisson())
    
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

    #En todos sale que el mejor es el que tiene 7 grados de libertad para la
    # temperatura y el retardo. Me suena muy raro ¿Mal ajuste?

    #Si hacemos las predicciones con tantos grados de libertad se nota un sobreajuste
    # de los datos. Nos fijamos en otros valores del QAIC y elegimos un mejor modelo:


##############################################
##############################################
##############################################

  #AÑO 2023: Hacemos lo mismo pero con el 2023 a ver si se observa algo diferente.
  # Después, centrarse en meses de verano o de invierno...

momo_23<-subset(momo_provincial_temp, año=='23')


#Análisis de sensibilidad:

#1) 'ns' para ambos:

resultados_ns_23<-data.frame(df_var=integer(), df_lag=integer(), QAIC=numeric())

for (df_var in c(3:7)) {
  for (df_lag in c(3:7)) {
    base<-crossbasis((momo_23$tmed), lag=30, argvar=list(fun="ns", df=df_var), arglag=list(fun="ns", df=df_lag))
    modelo<-glm(momo_23$defunciones_obs_redondeadas ~ base+momo_23$dow+ns(momo_23$fecha_defuncion, df=7), family=quasipoisson())
    
    k <- length(coef(modelo))
    phi <- summary(modelo)$dispersion
    ll <- logLik(update(modelo, family=poisson))
    qaic <- -2 * as.numeric(ll) + 2 *phi * k
    
    resultados_ns_23 <- rbind(resultados_ns_23, data.frame(df_var=df_var, df_lag=df_lag, QAIC = qaic))
  }
}

print(resultados_ns_23)
saveRDS(resultados_ns_23, "resultados_ns_23.rds")
resultados_ns_23 <- readRDS("resultados_ns_23.rds")

#2) 'bs' para ambos:

resultados_bs_23<-data.frame(df_temp=integer(), df_retardo=integer(), QAIC=numeric())

for (df_var in c(3:7)) {
  for (df_lag in c(4:7)) {
    base<-crossbasis((momo_23$tmed), lag=30, argvar=list(fun="bs", df=df_var), arglag=list(fun="bs", df=df_lag))
    modelo<-glm(momo_23$defunciones_obs_redondeadas ~ base+momo_23$dow+ns(momo_23$fecha_defuncion, df=7), family=quasipoisson())
    
    k <- length(coef(modelo))
    phi <- summary(modelo)$dispersion
    ll <- logLik(update(modelo, family=poisson))
    qaic <- -2 * as.numeric(ll) + 2 *phi * k
    
    resultados_bs_23 <- rbind(resultados_bs_23, data.frame(df_temp=df_var, df_retardo=df_lag, QAIC = qaic))
  }
}

print(resultados_bs_23)
saveRDS(resultados_bs_23, "resultados_bs_23.rds")
resultados_bs_23 <- readRDS("resultados_bs_23.rds")


#3) 'ns' para temp. y 'bs' para lag:

resultados_ns_bs_23<-data.frame(df_temp=integer(), df_retardo=integer(), QAIC=numeric())

for (df_var in c(3:7)) {
  for (df_lag in c(4:7)) {
    base<-crossbasis((momo_23$tmed), lag=30, argvar=list(fun="ns", df=df_var), arglag=list(fun="bs", df=df_lag))
    modelo<-glm(momo_23$defunciones_obs_redondeadas ~ base+momo_23$dow+ns(momo_23$fecha_defuncion, df=7), family=quasipoisson())
    
    k <- length(coef(modelo))
    phi <- summary(modelo)$dispersion
    ll <- logLik(update(modelo, family=poisson))
    qaic <- -2 * as.numeric(ll) + 2 *phi * k
    
    resultados_ns_bs_23 <- rbind(resultados_ns_bs_23, data.frame(df_temp=df_var, df_retardo=df_lag, QAIC = qaic))
  }
}

print(resultados_ns_bs_23)
saveRDS(resultados_ns_bs_23, "resultados_ns_bs_23.rds")
resultados_ns_bs_23 <- readRDS("resultados_ns_bs_23.rds")


#4) 'bs' para temp. y 'ns' para lag:

resultados_bs_ns_23<-data.frame(df_temp=integer(), df_retardo=integer(), QAIC=numeric())

for (df_var in c(3:7)) {
  for (df_lag in c(3:7)) {
    base<-crossbasis((momo_23$tmed), lag=30, argvar=list(fun="bs", df=df_var), arglag=list(fun="ns", df=df_lag))
    modelo<-glm(momo_23$defunciones_obs_redondeadas ~ base+momo_23$dow+ns(momo_23$fecha_defuncion, df=7), family=quasipoisson())
    
    k <- length(coef(modelo))
    phi <- summary(modelo)$dispersion
    ll <- logLik(update(modelo, family=poisson))
    qaic <- -2 * as.numeric(ll) + 2 *phi * k
    
    resultados_bs_ns_23 <- rbind(resultados_bs_ns_23, data.frame(df_temp=df_var, df_retardo=df_lag, QAIC = qaic))
  }
}

print(resultados_bs_ns_23)
saveRDS(resultados_bs_ns_23, "resultados_bs_ns_23.rds")
resultados_bs_ns_23 <- readRDS("resultados_bs_ns_23.rds")

#---

resultados_ns_23[which(resultados_ns_23$QAIC==min(resultados_ns_23$QAIC)),]
resultados_bs_23[which(resultados_bs_23$QAIC==min(resultados_bs_23$QAIC)),]
resultados_ns_bs_23[which(resultados_ns_bs_23$QAIC==min(resultados_ns_bs_23$QAIC)),]
resultados_bs_ns_23[which(resultados_bs_ns_23$QAIC==min(resultados_bs_ns_23$QAIC)),]

  #Los mejores modelos que salen son los de 7 df pero esos sobreajustan. Finalmente
  # se optó por el modelo de 'bs' con 4 df para ambas dimensiones

#---------------------------
#---------------------------

  #Creamos los modelos y hacemos los gráficos en ambos años para comparar:

base23<-crossbasis((momo_23$tmed), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modelo23<-glm(momo_23$defunciones_obs_redondeadas ~ base23+momo_23$dow+ns(momo_23$fecha_defuncion, df=7), family=quasipoisson())
prediccion23<-crosspred(base23, model=modelo23, cen=20)

par(mfrow=c(1,2))
plot(prediccion23, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion23, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))

#

base24<-crossbasis((momo_24$tmed), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modelo24<-glm(momo_24$defunciones_obs_redondeadas ~ base24+momo_24$dow+ns(momo_24$fecha_defuncion, df=7), family=quasipoisson())
prediccion24<-crosspred(base24, model=modelo24, cen=20)

par(mfrow=c(1,2))
plot(prediccion24, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion24, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))


#------------------------------------------------------------------

#Análisis por subgrupos de edad:

momo_23$dow <- factor(weekdays(momo_23$fecha_defuncion))
momo_24$dow <- factor(weekdays(momo_24$fecha_defuncion))
momo_23_24$dow <- factor(weekdays(momo_23_24$fecha_defuncion))


levels(momo_23_24$cod_gedad)
tapply(momo_23_24$defunciones_observadas,
       momo_23_24$cod_gedad,
       sum, na.rm = TRUE)

grupo_niños<-subset(momo_23_24, cod_gedad=='0-14')
grupo_jovenes<-subset(momo_23_24, cod_gedad=='15-44')
grupo_adultos<-subset(momo_23_24, cod_gedad=='45-64')
grupo_mayores<-subset(momo_23_24, cod_gedad=='65-74')
grupo_mas_mayores<-subset(momo_23_24, cod_gedad=='75-84')
grupo_abuelos<-subset(momo_23_24, cod_gedad=='+85')


#1) Niños: (0-14)
base_niños<-crossbasis((grupo_niños$tmed), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modelo_niños<-glm(grupo_niños$defunciones_observadas ~ base_niños+grupo_niños$dow+ns(grupo_niños$fecha_defuncion, df=7), family=quasipoisson())
prediccion_niños<-crosspred(base_niños, model=modelo_niños, cen=20)

par(mfrow=c(1,2))
plot(prediccion_niños, xlab='Temperatura', ylab='Retardo', zlab='RR', main='Niños (0-14 años)')
plot(prediccion_niños, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))



#2) Jóvenes: (15-44)
base_jovenes<-crossbasis((grupo_jovenes$tmed), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modelo_jovenes<-glm(grupo_jovenes$defunciones_observadas ~ base_jovenes+grupo_jovenes$dow+ns(grupo_jovenes$fecha_defuncion, df=7), family=quasipoisson())
prediccion_jovenes<-crosspred(base_jovenes, model=modelo_jovenes, cen=20)

par(mfrow=c(1,2))
plot(prediccion_jovenes, xlab='Temperatura', ylab='Retardo', zlab='RR', main='Jóvenes (15-44 años)')
plot(prediccion_jovenes, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))



#3) Adultos: (45-64)
base_adultos<-crossbasis((grupo_adultos$tmed), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modelo_adultos<-glm(grupo_adultos$defunciones_observadas ~ base_adultos+grupo_adultos$dow+ns(grupo_adultos$fecha_defuncion, df=7), family=quasipoisson())
prediccion_adultos<-crosspred(base_adultos, model=modelo_adultos, cen=20)

par(mfrow=c(1,2))
plot(prediccion_adultos, xlab='Temperatura', ylab='Retardo', zlab='RR', main='Adultos (45-64 años)')
plot(prediccion_adultos, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))



#4) Mayores: (65-74)
base_mayores<-crossbasis((grupo_mayores$tmed), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modelo_mayores<-glm(grupo_mayores$defunciones_observadas ~ base_mayores+grupo_mayores$dow+ns(grupo_mayores$fecha_defuncion, df=7), family=quasipoisson())
prediccion_mayores<-crosspred(base_mayores, model=modelo_mayores, cen=20)

par(mfrow=c(1,2))
plot(prediccion_mayores, xlab='Temperatura', ylab='Retardo', zlab='RR', main='Mayores (65-74 años)')
plot(prediccion_mayores, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))



#5) Más mayores: (75-84)
base_mas_mayores<-crossbasis((grupo_mas_mayores$tmed), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modelo_mas_mayores<-glm(grupo_mas_mayores$defunciones_observadas ~ base_mas_mayores+grupo_mas_mayores$dow+ns(grupo_mas_mayores$fecha_defuncion, df=7), family=quasipoisson())
prediccion_mas_mayores<-crosspred(base_mas_mayores, model=modelo_mas_mayores, cen=20)

par(mfrow=c(1,2))
plot(prediccion_mas_mayores, xlab='Temperatura', ylab='Retardo', zlab='RR', main='Más mayores (75-84 años)')
plot(prediccion_mas_mayores, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))



#6) Abuelos: (+85)
base_abuelos<-crossbasis((grupo_abuelos$tmed), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modelo_abuelos<-glm(grupo_abuelos$defunciones_observadas ~ base_abuelos+grupo_abuelos$dow+ns(grupo_abuelos$fecha_defuncion, df=7), family=quasipoisson())
prediccion_abuelos<-crosspred(base_abuelos, model=modelo_abuelos, cen=20)

par(mfrow=c(1,2))
plot(prediccion_abuelos, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion_abuelos, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))




#No se observan grandes diferencias entre los grupos. En los grupos más jóvenes
# se nota que el número de fallecimientos es muy bajo. Para los grupos de edad
# más mayores, ya se ven gráficos mucho más parecidos.

#Se observa lo de siempre. Con temperaturas bajas, el pico del RR se alcanza
# tras los primeros días de retardo. Para temperaturas altas, el RR más alto
# se alcanza al principio.






############################################################################
############################################################################
############################################################################
############################################################################
############################################################################

  #A partir de aquí ya no está incluido en el documento. ¿¿¿¿PONER????....

#Hacemos los 'slices':
plot(prediccion23, 'slices', lag=c(0,3,5,11), var=c(0, 10, 30, 35), ylab='RR')
plot(prediccion24, 'slices', lag=c(0,3,5,11), var=c(0, 10, 30, 35), ylab='RR')
    
    #Lo anterior no está incluido en el documento.

