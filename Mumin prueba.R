
# SCRIPT PARA PROBAR MANEJO DE INFERENCIA MULTIMODELO

# rm(list=ls())
# ls()

# Datos tomados de 
# https://sites.google.com/site/rforfishandwildlifegrads/home/mumin_usage_examples

# Establezcan el directorio de trabajo con setwd()
# setwd("D:/Bibliotecas/Documentos/Institucional/Univ TdF/Estadistica Avanzada")
dater<-read.csv("Example data.csv",header=TRUE)
# Los datos incluyen 3 variables de respuesta: 
# conteo, densidad y presencia 
# y 5 variables explicativas: elevación, pendiente (slope), 
# área, distancia (a la población más cercana) y pct.cover (% de cobertura).

head(dater)

# Asegurarse de que no hay valores numéricos faltantes 
summary(dater)

# Cargar el paquete MUMIn 

if(!require(MuMIn)){
  install.packages("MuMIn", repos="http://R-Forge.R-project.org")
  library(MuMIn)
}

# Una cosa muy importante que se debe hacer a continuación es 
# cambiar las opciones globales de cómo las funciones R manejan
# los datos que faltan (NA). Al realizar este cambio, una función 
# no funcionará si faltan datos. Esto es necesario si utiliza la
# función "dredge" para el análisis exploratorio de datos. 
# change na. action 
op <- options(na.action = "na.fail") 

# Estamos listos para comenzar. Postulemos cuatro modelos candidatos 
# que explican la variación en la densidad animal. Lo ideal sería
# que estos modelos representaran hipótesis. Dada la naturaleza
# de la respuesta, usaremos la regresión lineal ordinaria con la
# función "lm".

#Primero, ajustamos   4 modelos candidatos para explicar la variacón en la densidad 

mod1<-lm(density~distance+elev, data = dater) 
mod2<-lm(density~slope+pct.cover, data = dater) 
mod3<-lm(density~slope+distance, data = dater) 
mod4<-lm(density~slope+distance+elev, data = dater) 

# usar la funcion model.sel para hacer la seleccion de modelos
# y colocar la salida en el objeto  out.put 
out.put<-model.sel(mod1,mod2,mod3,mod4) 

# Asi podemos obtener de un vistazo los valores de AICvpara muestras pequeñas  AICc 
# los delta AICc, y el peso de los modelos 
out.put 

# Mirar los pesos de los modelos!


# crear un conjunto de modelos usando la función subset 
# seleccionar modelos con  delta AICc menor a 5 
# IMPORTANTE: Los pesos han sido renormalizados!! 
subset(out.put, delta <5) 


# ______________________________________
# ESTO ES ACCESORIO AL TEMA DE INFERENCIA MULTIMODELO
# Armamos una salida linda para publicar con coerce (obligar)

# obligamos al objeto out.put a un data frame 
# los elements 6-10 en out.put tienen lo que queremos  
sel.table<-as.data.frame(out.put)[6:10] 
sel.table


# Está un poco desordenado.....Limpiemos un poco, primero redondeando. 
# 
sel.table[,2:3]<- round(sel.table[,2:3],2) 
sel.table[,4:5]<- round(sel.table[,4:5],3) 
# Ahora está un poco mejor
sel.table

# Ahora cambiamos el nombre a las columnas para seguir convenciones
# el número de parámetros (df) debería ser  K 
names(sel.table)[1] = "K" 

## poner los nombres de modelos en columnas
sel.table$Modelo<-rownames(sel.table) 

# podemos reemplazar el nombre de los modelos con fórmulas
# Ojo que es un poco dificultoso 
for(i in 1:nrow(sel.table)) sel.table$Modelo[i]<- as.character(formula(paste(sel.table$Modelo[i])))[3] 

# veamos como queda 
sel.table 

# reordenamos columnas 
sel.table<-sel.table[,c(6,1,2,3,4,5)] 
sel.table 
# Ahora podemos escribirlo en un archivo de texo
# Ojo al directorio donde lo escriben 
# con getwd() lo se 
# Aunque lo definí el inicio con setwd()

write.csv(sel.table,"Mi tabla de seleccion de modelos.csv", row.names = F) 


# El método de selección de modelo por defecto es AICc
# Puedo usar BIC
# model.sel(mod1,mod2,mod3,mod4, rank = BIC)
#
# -----------------------------------------------------------------------



# Importancia en pesos para variables predictoras individuales 
# se calcula usando la función  importance 
importance(out.put)

# Mirando la salida de arriba, hay mas evidencia para considerar a
#  la distancia y la elevación (pesos cercanos a uno), pero mucho
#  menos evidencia para cobertura (pct.cover). 
# El número de modelos candidatos donde un parámetro aparece puede
#  tener un gran efecto en la importancia del peso.

# Por ejemplo, el Intercepto está incluido en todos los modelos,
#  por lo que el peso de su importancia es 1 (no se muestra).

# Ojo, que en la salida anterior, pct.cover está en un solo
#  modelo, así que interpretar los pesos con cuidado,
# dado que es mas robusta la decisión si las variables están mas repartidas en los modelos.


# El promediado del modelo es un medio para incorporar la
#  incertidumbre de la selección del modelo. 
# Aquí, las estimaciones de los parámetros para cada modelo
#  candidato se ponderan usando los correspondientes pesos del
# modelo y se suman. 
# Hay dos métodos para calcular el promediado de modelos definidos
#  por Burnham y Anderson.
# Uno donde las estimaciones de parámetros se promedian en todos los modelos
# en los que se produce el predictor xj
# y otro donde se evalúan los parámetros en todos los
#  modelos, no sólo aquellos en los que se produce el predictor
#  xj. 
# La función MuMIn model.avg lleva a cabo ambos tipos de
#  promediado de modelo
# el primer tipo de promediado es "subset" 
# y el segundo tipo como "full".

# Promediado de modelo usando todos los modelos candidatos, siempre usar  revised.var = TRUE 
MA.ests<-model.avg(out.put, revised.var = TRUE) 

MA.ests

# Noten como se indican los modelos 

#Component models: 
#  '12'  '124' '14'  '34' 


# De este modo presento los estimados, los ES condicionales a los modelos 
# y los IC  
summary(MA.ests)
# El "Full average" incluye todos los modelos. Equivale al "shrinkage"
# El "Conditional average" está condicionado a los modelos seleccionados
# Al final figura la mportancia relativa

# Con esta orden me presenta los coeficientes promediados
MA.ests$coefficients 


# Con esta orden veo la tabla de grados de libertad, LogLik, AICc, delta AICc y peso
MA.ests$msTable

# IC condicionales a los modelos ajustados
confint(MA.ests,level=0.95)


# Puedo promediar de entre modelos elegidos por algun criterop en partocula

MA.ests2<-model.avg(out.put, subset= delta < 5, revised.var = TRUE) 
summary(MA.ests2)


#extraigo parámetros y pesos del conjunto de modelos 
# uso funcion get.models
pred.parms<-get.models(out.put, subset= delta < 5)
pred.parms

# PREDICCION
# predigo valores de variable respuesta para cada modelo  
# usamos los datos que tenemos pero podemos traer nuevos
model.preds = sapply(pred.parms, predict, newdata = dater)
model.preds

# ponderar las predicciones de cada modelo por su peso  AIC
# La funcion Weights extrae los pesos del conjunto de modelos elegidos
# Tambien usamos multplicacion de matrices  %*%
mod.ave.preds<-model.preds %*% Weights(AICc(mod1,mod4))
mod.ave.preds


#
#__________________________________________
# AHORA HAGAMOS UNA EXPEDICION DE PESCA 
#__________________________________________
## SOLO PARA PROPOSTOS EXPLORATORIOS....
## NUNCA USAR EN LA VIDA REAL

# Ajustar modelo con todos los parámetros
all.parms<-lm(density~slope+distance+elev+ pct.cover, data = dater)

# La funcion dredge ajusta todas las combinaciones
# del modelo de  all.parms ajustado arriba
results<-dredge(all.parms)
results
# Comparar con la salida original out.put
out.put


# Algunos ejemplos de seleccion
# Tomar los modelos mas fundamentados de este set (delta AIC menor a 5)
subset(results, delta <5)

#Tomar el MEJOR modelo
subset(results, delta == 0)

# calcular las importancias en peso}
importance(results)

# use another model selection criteria
#results<-dredge(all.parms, rank = BIC)
#results

# permitir un maximo de 3 parametros y un minimo de 1 parametro en cada modelo 
results_1<-dredge(all.parms,m.lim=c(1,3))
results_1

# ajustar todos los modelos pero EXCLUYENDO slope y elevation
results_2<-dredge(all.parms, subset= !(slope && elev))
results_2

# forzar incluir  elevation en TODOS los modelos 
results_3<-dredge(all.parms,fixed =c("elev"))
results_3

# los objetos creados con   dredge pueden usarse para  
# crear parametros de promediado de modelos
MA.estsDR<-model.avg(results, subset= delta < 2, revised.var = TRUE)

summary(MA.estsDR)
MA.estsDR$coefficients 

MA.estsDR$msTable

# IC condicionales a los modelos ajustados
confint(MA.estsDR,level=0.95)


options(op)
