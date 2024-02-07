library(dygraphs) ##graficas
library(xts) ##para trabajar con series de tiempo
library(quantmod) ##acceder datos de fuentes (yahoo finance)
library(dplyr) ##operaciones mas faciles en las lineas de codigo
library(RColorBrewer)

#---Primero, generamos una funcion que ayude a simplificar los
#---tipo de datos que deseamos de la fuente de informacion financiera
#---En este ejemplo los datos de cierre y volumen, que dependera
#---del simbolo o ticket
#Obtencion de datos:

start<-format(as.Date("2014-01-01"), "%Y-%m-%d")
end<-format(as.Date("2020-07-01"), "%Y-%m-%d")

precios_volumenes <-function(simbolo) #declaramos funcion
{
  #obtener precios stocks de Yahoo finance
  datos <-getSymbols(simbolo, from=start, to=end, auto.assign = FALSE)
  #eliminar datos faltantes
  datos <-na.omit(datos)
  #mantener datos con precios cierre y volumen: columna 4 y 5
  datos <- datos[,4:5]
  #para hacerlo datos accesibles en el global enviroment
  assign(simbolo, datos, envir = .GlobalEnv)
}
#---Llamamos la funcion para cada stock
precios_volumenes("AMZN")
precios_volumenes("NFLX")
precios_volumenes("IBM")
precios_volumenes("SPY")
#---juntamos los datos y renombramos las columnas
PyV <- merge.xts(AMZN, NFLX, IBM, SPY)
colnames(PyV) <- c("Amazon P.Cierre", "Amazon Vol", "Netflix P.Cierre", 
                   "Netflix Vol", "IBM P.Cierre", "IBM Vol", "SPY P.Cierre", "SPY Vol")
#....Serie de tiempo
#podemos generar una grafica interactiva de las variables, en este caso los precios:
Precios<- dygraph(PyV[,c(1,3,5,7)], main = "Precios de Amazon, Netflix, IBM, SP&500") %>%
  dyAxis("y", label = "Precios") %>%
  dyRangeSelector(dateWindow = c("2014-01-01", "2020-07-01")) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"))
Precios

#podemos ver los ultimos 5 datos redondeados hasta 3 decimales:
round(tail(PyV, n =5), 3)

#Ejemplo de datos de panel, generamos una lista de objetos dygraphs y para impreimirlos usamos hmtltools:
library(htmltools)
dy_graficos <-list(
  dygraphs::dygraph(PyV[,c(1,3,5,7)], main = "Precios de Amazon, Netflix, IBM, SP&500"),
  dygraphs::dygraph(PyV[,c(2,4,6,8)], main = "Volumenes de Amazon, Netflix, IBM, SP&500"))

#Representamos los objetos dygraphs usando htmltools
htmltools::browsable(htmltools::tagList(dy_graficos))

#---Datos transversales
#Seleccionamos datos de AMZN del 2014 y 2020
#empezemos seleccionando los datos de 2014 de AMZN que es la 1ra columna

AMZN_2014<-subset(PyV[,1], index(PyV)>="2014-01-01" & index(PyV)<="2014-12-31")
AMZN_2014[c(1:5, nrow(AMZN_2014))]
#para el 2020
AMZN_2020<-subset(PyV[,1], index(PyV)>="2020-01-01" & index(PyV)<="2020-12-31")
AMZN_2020[c(1:5, nrow(AMZN_2020))]

#Ahora podemos visualizarlos
par(mfrow=c(2,1))

hist(AMZN_2014, freq = FALSE, col = "yellow", border = "blue", main = "Densidades de los precios de AMZN 2014", xlab = "Precio Cierre")
lines(density(AMZN_2014), lwd =2, col = "red")
hist(AMZN_2020, freq = FALSE, col = "blue", border = "blue", main = "Densidades de los precios de AMZN 2020", xlab = "Precio Cierre")
lines(density(AMZN_2020), lwd =2, col = "red")
