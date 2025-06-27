library(Rcmdr)
library(dlnm)

library(readr)
momo <-read_csv("momo.csv")
View(momo)


help(mkbasis)
length(momo$defunciones_atrib_exc_temp)
  
#La función mkbasis ya no se usa en las versiones recientes. Ahora se usa onebasis
#1)
mkbasis <- onebasis(momo$defunciones_atrib_exc_temp, fun='ns')
summary(mkbasis)

#mklagbasis tampoco existe ya.
#2)
mklagbasis <- onebasis(0:30, fun='ns')
summary(mklagbasis)

#Se puede reducir los dos primeros pasos a uno solo mediante el empleo de crossbasis:
#3)
help(crossbasis)
base_cruzada <- crossbasis(momo$defunciones_atrib_exc_temp, lag=30, argvar=list(fun='ns', df=1),
           arglag=list(fun='ns', df=1))
summary(base_cruzada)

#4)
help(crosspred)
  #Primero hay que ajustar un modelo para luego ponerlo en el argumento model de la
  #función crosspred
modelo <- lm(defunciones_atrib_exc_temp ~ base_cruzada, data=momo)
summary(modelo)   #el modelo se suele ajustar según un modelo de Poisson


prediccion_cruzada <- crosspred(base_cruzada, model=modelo)
names(prediccion_cruzada)

plot(prediccion_cruzada)
plot(prediccion_cruzada, "slices", var=c(5,10,30))
plot(prediccion_cruzada, "overall")
plot(prediccion_cruzada, "contour")




  #EL CÓDIGO DE ARRIBA ES PARA VER CÓMO FUNCIONA ESTA LIBRERÍA


#--------------------------------------------------------------------------

#--------------------------------------------------------------------------

#--------------------------------------------------------------------------

#--------------------------------------------------------------------------

#--------------------------------------------------------------------------

library(readr)
momo <-read_csv("momo.csv") 
View(momo)


  #Primero leemos las temperaturas de Navarra
library(Rcmdr)
TempNavarra <- 
  readXL("C:/Users/alons/OneDrive/Escritorio/Máster Modelización Alonso Pueyo/TFM/Trabajo/Datos Temperatura Navarra/TempNavarra.xlsx",
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
    #Hay 7 datos que no tienen ni Temp. máx, ni mín ni media..




#Ahora ya tenemos datos sobre las temperaturas en distintas fechas, y datos
#sobre la mortalidad. Pasamos a hacer modelos y a estudiar la relación:

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

#------------------------------

#MODELO 1:

base1<-crossbasis((Navarra_con_temp$T_media), lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="ns", df=4))
modelo1<-glm(Navarra_con_temp$defunciones_observadas ~ base1, family=quasipoisson())
    #En el modelo me salen muchos avisos con family=poisson, pero no da error con quasipoisson()
prediccion1<-crosspred(base1, model=modelo1)
    #Me salía el eror: 'coef/vcov not consistent with basis matrix' porque en modelo
    # hay que incluir la base como variable explicativa


 #Ya tenemos una primera base, modelo y prediccion. Hacemos gráficos para ver qué está
# ocurriendo, cómo evoluciona... Después, podemos probar con otras bases y modelos para
# ver cuál es mejor, y una vez encontrado, dividir la población en distintos grupos

par(mfrow=c(1,2))
plot(prediccion1, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion1, 'overall', xlab='Temperatura', ylab='RR', main='Gráfico de efecto acumulado')
par(mfrow=c(1,1))

plot(prediccion1, 'slices', lag=c(0,3,7))  
  plot(prediccion1, 'slices', var=30) #los 'slices' son en realidad cortes de la superficie de antes
plot(prediccion1, 'contour')

#------------------------------

#MODELOS 2:

base2<-crossbasis((Navarra_con_temp$T_media), lag=30, argvar=list(fun="bs", df=3), arglag=list(fun="ns", df=2))
modelo2<-glm(Navarra_con_temp$defunciones_observadas ~ base2, family=quasipoisson())
prediccion2<-crosspred(base2, model=modelo2)

par(mfrow=c(1,2))
plot(prediccion2, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion2, 'overall', xlab='Temperatura', ylab='RR', main='Gráfico de efecto acumulado')
par(mfrow=c(1,1))


#------------------------------

#MODELO 3:

base3<-crossbasis((Navarra_con_temp$T_media), lag=30, argvar=list(fun="bs", df=10), arglag=list(fun="ns", df=10))
modelo3<-glm(Navarra_con_temp$defunciones_observadas ~ base3, family=quasipoisson())
prediccion3<-crosspred(base3, model=modelo3)

par(mfrow=c(1,2))
plot(prediccion3, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion3, 'overall', xlab='Temperatura', ylab='RR', main='Gráfico de efecto acumulado')
par(mfrow=c(1,1))


#------------------------------

#Hacemos análisis de sensibilidad para estudiar cuántos grados de libertad se han de tener:

resultados<-data.frame(df_temp=integer(), df_retardo=integer(), RR_max=numeric())

for (df_var in c(3:12)) {
  for (df_lag in c(3:12)) {
    base<-crossbasis((Navarra_con_temp$T_media), lag=30, argvar=list(fun="bs", df=df_var), arglag=list(fun="ns", df=df_lag))
    modelo<-glm(Navarra_con_temp$defunciones_observadas ~ base, family=quasipoisson())
    prediccion<-crosspred(base, model=modelo)
    
    rr_max <- max(prediccion$allRRfit)
    resultados <- rbind(resultados, data.frame(df_temp=df_var, df_retardo=df_lag, RR_max = rr_max))
  }
}

print(resultados)

library(ggplot2)
help(ggplot)
ggplot(resultados, aes(x = df_temp, y = df_retardo, fill = RR_max)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
  labs(
    title = "Riesgo relativo acumulado máximo según grados de libertad",
    x = "Grados de libertad (temperatura)",
    y = "Grados de libertad (retardo)",
    fill = "RR máximo"
  ) +
  theme_minimal()


base_final<-crossbasis((Navarra_con_temp$T_media), lag=30, argvar=list(fun="bs", df=9), arglag=list(fun="ns", df=8))
modelo_final<-glm(Navarra_con_temp$defunciones_observadas ~ base_final, family=quasipoisson())
prediccion_final<-crosspred(base_final, model=modelo_final)

par(mfrow=c(1,2))
plot(prediccion_final, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion_final, 'overall', xlab='Temperatura', ylab='RR', main='Gráfico de efecto acumulado')
par(mfrow=c(1,1))


#------------------------------

#Veamos qué pasa cuando usamos splines naturales o beta-splines para todo:

#solo beta-splines:
base_bs<-crossbasis((Navarra_con_temp$T_media), lag=30, argvar=list(fun="bs", df=9), arglag=list(fun="bs", df=8))
modelo_bs<-glm(Navarra_con_temp$defunciones_observadas ~ base_bs, family=quasipoisson())
prediccion_bs<-crosspred(base_bs, model=modelo_bs)

par(mfrow=c(1,2))
plot(prediccion_bs, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion_bs, 'overall', xlab='Temperatura', ylab='RR', main='Gráfico de efecto acumulado')
par(mfrow=c(1,1))


#solo splines naturales:
base_ns<-crossbasis((Navarra_con_temp$T_media), lag=30, argvar=list(fun="ns", df=9), arglag=list(fun="ns", df=8))
modelo_ns<-glm(Navarra_con_temp$defunciones_observadas ~ base_ns, family=quasipoisson())
prediccion_ns<-crosspred(base_ns, model=modelo_ns)

par(mfrow=c(1,2))
plot(prediccion_ns, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion_ns, 'overall', xlab='Temperatura', ylab='RR', main='Gráfico de efecto acumulado')
par(mfrow=c(1,1))


#spline natural para temperatura y beta-spline para retardo:
base_ns_bs<-crossbasis((Navarra_con_temp$T_media), lag=30, argvar=list(fun="ns", df=9), arglag=list(fun="bs", df=8))
modelo_ns_bs<-glm(Navarra_con_temp$defunciones_observadas ~ base_ns_bs, family=quasipoisson())
prediccion_ns_bs<-crosspred(base_ns_bs, model=modelo_ns_bs)

par(mfrow=c(1,2))
plot(prediccion_ns_bs, xlab='Temperatura', ylab='Retardo', zlab='RR')
plot(prediccion_ns_bs, 'overall', xlab='Temperatura', ylab='RR', main='Gráfico de efecto acumulado')
par(mfrow=c(1,1))
    
        #¿Incluir en el trabajo?

#------------------------------

#Hacer el análisis por grupos de edad o de población:

names(Navarra_con_temp)
Navarra_con_temp$cod_gedad<-as.factor(Navarra_con_temp$cod_gedad)
levels(Navarra_con_temp$cod_gedad)
    #la población se divide en grupos: 0-14, 15-44, 45-64, 65-74, 75-84, +85
    #Después, tambien tenemos el grupo all y el +65. Nos fijamos en los de arriba

#Hay que poner a los de +65 en el grupo de 65-74:

Navarra_con_temp$cod_gedad[Navarra_con_temp$cod_gedad == '+65'] <- '65-74'


    #Omitimos los datos que pone 'all'

#Hacemos el análisis:
grupo_niños<-subset(Navarra_con_temp, cod_gedad=='0-14')
grupo_jovenes<-subset(Navarra_con_temp, cod_gedad=='15-44')
grupo_adultos<-subset(Navarra_con_temp, cod_gedad=='45-64')
grupo_mayores<-subset(Navarra_con_temp, cod_gedad=='65-74')
grupo_mas_mayores<-subset(Navarra_con_temp, cod_gedad=='75-84')
grupo_abuelos<-subset(Navarra_con_temp, cod_gedad=='+85')


#Grupo 0-14:
base_niños<-crossbasis((grupo_niños$T_media), lag=30, argvar=list(fun="bs", df=9), arglag=list(fun="ns", df=8))
modelo_niños<-glm(grupo_niños$defunciones_observadas ~ base_niños, family=quasipoisson())
prediccion_niños<-crosspred(base_niños, model=modelo_niños)

par(mfrow=c(1,2))
plot(prediccion_niños, xlab='Temperatura', ylab='Retardo', zlab='RR', main='Grupo 0-14 años')
plot(prediccion_niños, 'overall', xlab='Temperatura', ylab='RR', main='Gráfico de efecto acumulado (0-14 años)')
par(mfrow=c(1,1))
  #el grafico de efecto acumulado no tiene sentido. Sube y baja
  #constantemente. Puede ser por falta de datos.

tapply(Navarra_con_temp$defunciones_observadas,
       Navarra_con_temp$cod_gedad,
       sum, na.rm = FALSE)
  #contamos cuántas defunciones se han observado para cada grupo de edad, 
  # y solo hay casi 404 para 0-14 años. No son suficientes

#---
      
#Grupo 15-44:
base_jovenes<-crossbasis((grupo_jovenes$T_media), lag=30, argvar=list(fun="bs", df=9), arglag=list(fun="ns", df=8))
modelo_jovenes<-glm(grupo_jovenes$defunciones_observadas ~ base_jovenes, family=quasipoisson())
prediccion_jovenes<-crosspred(base_jovenes, model=modelo_jovenes)

par(mfrow=c(1,2))
plot(prediccion_jovenes, xlab='Temperatura', ylab='Retardo', zlab='RR', main='Grupo 15-44 años')
plot(prediccion_jovenes, 'overall', xlab='Temperatura', ylab='RR', main='Gráfico de efecto acumulado (15-44 años)')
par(mfrow=c(1,1))
      #parece que hay algo raro. el RR es muy grande, muchos altibajos.
      #hacemos zoom a la curva
plot(prediccion_jovenes, 'overall', ylim=c(-0.1,3), xlab='Temperatura', ylab='RR', main='Gráfico de efecto acumulado (15-44 años)')

#---

#Grupo 45-64:
base_adultos<-crossbasis((grupo_adultos$T_media), lag=30, argvar=list(fun="bs", df=9), arglag=list(fun="ns", df=8))
modelo_adultos<-glm(grupo_adultos$defunciones_observadas ~ base_adultos, family=quasipoisson())
prediccion_adultos<-crosspred(base_adultos, model=modelo_adultos)

par(mfrow=c(1,2))
plot(prediccion_adultos, xlab='Temperatura', ylab='Retardo', zlab='RR', main='Grupo 45-64 años')
plot(prediccion_adultos, 'overall', xlab='Temperatura', ylab='RR', main='Gráfico de efecto acumulado (45-64 años)')
par(mfrow=c(1,1))
      #esto no quiere decir que haya algo mal. No hay suficientes
      #muertes atribuibles al exceso de calor para este grupo.

#---

#Grupo 65-74:
base_mayores<-crossbasis((grupo_mayores$T_media), lag=30, argvar=list(fun="bs", df=9), arglag=list(fun="ns", df=8))
modelo_mayores<-glm(grupo_mayores$defunciones_observadas ~ base_mayores, family=quasipoisson())
prediccion_mayores<-crosspred(base_mayores, model=modelo_mayores)

par(mfrow=c(1,2))
plot(prediccion_mayores, xlab='Temperatura', ylab='Retardo', zlab='RR', main='Grupo 65-74 años')
plot(prediccion_mayores, 'overall', xlab='Temperatura', ylab='RR', main='Gráfico de efecto acumulado (65-74 años)')
par(mfrow=c(1,1))

#---

#Grupo 75-84:
base_mas_mayores<-crossbasis((grupo_mas_mayores$T_media), lag=30, argvar=list(fun="bs", df=9), arglag=list(fun="ns", df=8))
modelo_mas_mayores<-glm(grupo_mas_mayores$defunciones_observadas ~ base_mas_mayores, family=quasipoisson())
prediccion_mas_mayores<-crosspred(base_mas_mayores, model=modelo_mas_mayores)

par(mfrow=c(1,2))
plot(prediccion_mas_mayores, xlab='Temperatura', ylab='Retardo', zlab='RR', main='Grupo 75-84 años')
plot(prediccion_mas_mayores, 'overall', xlab='Temperatura', ylab='RR', main='Gráfico de efecto acumulado (75-84 años)')
par(mfrow=c(1,1))

#---

#Grupo +85:
base_abuelos<-crossbasis((grupo_abuelos$T_media), lag=30, argvar=list(fun="bs", df=9), arglag=list(fun="ns", df=8))
modelo_abuelos<-glm(grupo_abuelos$defunciones_observadas ~ base_abuelos, family=quasipoisson())
prediccion_abuelos<-crosspred(base_abuelos, model=modelo_abuelos)

par(mfrow=c(1,2))
plot(prediccion_abuelos, xlab='Temperatura', ylab='Retardo', zlab='RR', main='Grupo +85 años')
plot(prediccion_abuelos, 'overall', xlab='Temperatura', ylab='RR', main='Gráfico de efecto acumulado (+85 años)')
par(mfrow=c(1,1))

#---


