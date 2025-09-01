library(dlnm)
library(dplyr)

momo_provincial_temp <- readRDS("momo_provincial_temp.rds")
Lugo<-filter(momo_provincial_temp, nombre_ambito=='LUGO')
Huelva<-filter(momo_provincial_temp, nombre_ambito=='HUELVA')
  #Como en la base de datos se tiene una fila por grupo de edad y de sexo, todas las
  # provincias tienen el mismo número de observaciones. Nos fijamos en el mapa coloreado
  # y decidimos estudiar lo que ocurre en Lugo y en Huelva. El primero porque es el más
  # azul y el segundo porque es de los más rojos.

#---------------------------------------

#ANÁLISIS DESCRIPTIVO:

which(is.na(Lugo$tmed))
which(is.na(Huelva$tmed))

Lugo <- Lugo[!is.na(Lugo$tmed), ]
Huelva <- Huelva[!is.na(Huelva$tmed), ]

Lugo$tmed <- as.numeric(gsub(",", ".", Lugo$tmed))
Lugo$tmax <- as.numeric(gsub(",", ".", Lugo$tmax))
Lugo$tmin <- as.numeric(gsub(",", ".", Lugo$tmin))
#
Huelva$tmed <- as.numeric(gsub(",", ".", Huelva$tmed))
Huelva$tmax <- as.numeric(gsub(",", ".", Huelva$tmax))
Huelva$tmin <- as.numeric(gsub(",", ".", Huelva$tmin))


Lugo$cod_sexo<-as.factor(Lugo$cod_sexo)
Lugo$nombre_sexo<-as.factor(Lugo$nombre_sexo)
Lugo$cod_gedad<-as.factor(Lugo$cod_gedad)
Lugo$nombre_gedad<-as.factor(Lugo$nombre_gedad)
Lugo$nombre_ambito<-as.factor(Lugo$nombre_ambito)
#
Huelva$cod_sexo<-as.factor(Huelva$cod_sexo)
Huelva$nombre_sexo<-as.factor(Huelva$nombre_sexo)
Huelva$cod_gedad<-as.factor(Huelva$cod_gedad)
Huelva$nombre_gedad<-as.factor(Huelva$nombre_gedad)
Huelva$nombre_ambito<-as.factor(Huelva$nombre_ambito)

summary(Lugo) #media de tmed: 12.94, tmin: 7.363, tmax: 18.51
summary(Huelva) #media de tmed: 19.11, tmin: 13.11, tmax: 25.11

sum(Lugo$defunciones_observadas, na.rm=TRUE)  
sum(Huelva$defunciones_observadas, na.rm=TRUE)  

  #Se ve la diferencia, tanto en la dispersión de los datos de defunciones
  # observadas como en las temperaturas diarias.


#---

tapply(Lugo$defunciones_observadas,
       Lugo$cod_gedad,
       sum, na.rm = TRUE)
tapply(Huelva$defunciones_observadas,
       Huelva$cod_gedad,
       sum, na.rm = TRUE)

#----------------------------------------------------------------

#MODELIZACIÓN:

library(splines)

Lugo$defunciones_obs_redondeadas <- round(Lugo$defunciones_observadas)
Lugo$dow <- factor(weekdays(Lugo$fecha_defuncion))

Huelva$defunciones_obs_redondeadas <- round(Huelva$defunciones_observadas)
Huelva$dow <- factor(weekdays(Huelva$fecha_defuncion))

#---

  #Lugo:
base_Lugo<-crossbasis((Lugo$tmed), lag=30,
                 argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modelo_Lugo<-glm(Lugo$defunciones_obs_redondeadas ~ base_Lugo+Lugo$dow+
              ns(Lugo$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion_Lugo<-crosspred(base_Lugo, model=modelo_Lugo, cen=15)
                              #La centramos en 15 grados porque en Lugo la media
                              # de las temperaturas es menor
par(mfrow=c(1,2))
plot(prediccion_Lugo, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion_Lugo, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))


  #Huelva:
base_Huelva<-crossbasis((Huelva$tmed), lag=30,
                 argvar=list(fun="bs", df=4), arglag=list(fun="bs", df=4))
modelo_Huelva<-glm(Huelva$defunciones_obs_redondeadas ~ base_Huelva+Huelva$dow+
              ns(Huelva$fecha_defuncion, df=10*7), family=quasipoisson())
prediccion_Huelva<-crosspred(base_Huelva, model=modelo_Huelva, cen=20)


par(mfrow=c(1,2))
plot(prediccion_Huelva, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion_Huelva, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))

#---

par(mfrow=c(2,2))
  plot(prediccion_Lugo, xlab='Temperatura', ylab='Retardo', zlab='RR')
  plot(prediccion_Lugo, 'overall', xlab='Temperatura', ylab='RR')
  plot(prediccion_Huelva, xlab='Temperatura', ylab='Retardo', zlab='RR')
  plot(prediccion_Huelva, 'overall', xlab='Temperatura', ylab='RR')
par(mfrow=c(1,1))

#---

plot(prediccion_Lugo, 'slices', var=c(4,26), ylab='RR')
plot(prediccion_Huelva, 'slices', lag=c(0,5,10,15), var=c(6,10,28,35), ylab='RR')
    #se pueden poner estos graficos pero no me convencen. No me dicen nada...


