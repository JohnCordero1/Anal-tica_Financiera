##Analítica Financiera. Tutorial Momentos Estadísticos Series Financiaras
##Instalaciones necesarias
library(fBasics)
library(PerformanceAnalytics)
library(xts)
library(quantmod)
library(ggplot2)
library(tseries)
library(dygraphs)
options(warn = - 1) 

##--------Obtención Datos (Recordar primer tutorial que empleamos una función muy similar):
start<-format(as.Date("2018-02-01"),"%Y-%m-%d")
end<-format(as.Date("2020-12-31"),"%Y-%m-%d")

#--------- Función para bajar precios y generar rendimientos:
rend<-function(simbolo,start,end) {
  ##---------Obtener precios de yahoo finance:
  datos<-getSymbols(simbolo, src = "yahoo", auto.assign = FALSE)
  ##---------eliminar datos faltantes:
  datos<-na.omit(datos)
  ##--------Mantener el precio de interés:
  datos<-datos[,4]
  ##--------Rendimientos simples:
  rend<-periodReturn(datos, period="daily", subset=paste(c(start, end), "::", sep=""), type='arithmetic')
  #--------Para hacer dtos accesibles  GLobal ENv:
  assign(simbolo, rend, envir = .GlobalEnv)
}

#--------Llamar la función para cada activo particular:
rend("META", start, end)
str(META)

rend("FORD", start, end)
str(FORD)
## Gráfico:
rends<-merge.xts(META, FORD)
colnames(rends)<-c("META", "FORD")
dygraph(rends, main = "META & FORD Rendimientos") %>%
  dyAxis("y", label = "Rend %") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"))

###########################
#----------Estadisticas básicas 
basicStats(META) ## Resumen estadísticos (exceso de kurtosis 18.75 y sesgo(skewness) negativo)
mean(META)
var(META)
stdev(META) # Desv Std
t.test(META)  # Prueba que H0: mean return = 0 (P-value=0.5761>5% N RH0)
s3=skewness(META)  #Sesgo
T=length(META) # tamaño muestra
t3=s3/sqrt(6/T) # Prueba de sesgo
t3
pp=2*pt(abs(t3), T-1, lower=FALSE) # Calcula p-valor, si p valor > alfa, no se rechaza nula y por tanto sesgo de cero 
pp #(P-value=0.000<5% R H0 por tanto sesgo distinto de 0)
s4=kurtosis(META)
s4
t4=s4/sqrt(24/T) # Prueba de curtosis, en exceso
t4
pv=2*(1-pnorm(t4)) # p-valor,  si p valor > alfa, no se rechaza nula y por tanto exceso de curtosis de cero 
pv #(P-valor=0<5% R H0 kurtosis significativo)
normalTest(META,method='jb') # Prueba Jaque Bera, H0: Normal
#(P-VALOR=000<5% RH0 No normal)

###################################################################################################
##---------- Segundo Activo
###Estadisticas básicas 
basicStats(FORD) ## Resumen estadísticos (sesgo positivo y kurtosias mayor)
mean(FORD)
var(FORD)
stdev(FORD) # Desv Std
t.test(FORD)  # Prueba que H0: mean return = 0 (p-value=0.83 N RH0)
s3=skewness(FORD)  #Sesgo
T=length(FORD) # tamaño muestra
t3=s3/sqrt(6/T) # Prueba de sesgo
t3
pp=2*(1-pnorm(abs(t3))) 
pp #(p-valor=0<5% R H0 sesgo distinto de 0)
s4=kurtosis(FORD)
s4
t4=s4/sqrt(24/T) 
t4
pv=2*(1-pnorm(t4)) 
pv #(p-valor=0<5% R H0 kurtosis significativo)
normalTest(FORD,method='jb')#(P-VALOR=000<5% RH0 No normal)

##----------Gráfica Densidad ambos activos

library(PerformanceAnalytics)
par(mfrow=c(1,2))
chart.Histogram(META, methods = c("add.normal", "add.density"), colorset = c("gray", "blue", "red"))
legend("topright", legend = c("Hist-META" ,"META dist","dnorm META"), col=c("gray", "blue", "red"), lty=1, cex = 0.7)
chart.Histogram(FORD, methods = c("add.normal", "add.density"), colorset = c("gray", "blue", "red"))
legend("topright", legend = c("Hist-FORD" ,"FORD dist","dnorm FORD"), col=c("gray", "blue", "red"), lty=1, cex = 0.7)
