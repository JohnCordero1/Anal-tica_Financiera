#### Ejercicio Analítica Financiera: Tipos de Datos
library(dygraphs)
library(xts)
library(quantmod)
library(dplyr)

options(warn = - 1)  
######################################################
###Primero, generemos una función que ayude a simplificar los tipos de datos que deseamos de la fuente de 
#información financiera.
#En este ejemplo, los datos de Cierre y Volúmenes, que dependerán del simbolo o ticker 
#del activo y a partir de qué año se consultan:

##Datos:

precios_volumenes <- function(simbolo, year)
{
  # Obtener precios stocks de Yahoo Finance
  datos <- getSymbols(simbolo, src = "yahoo", auto.assign = FALSE) 
  # Eliminando valores faltantes
  datos <- na.omit(datos)
  # Mantenemos columnas con Precios de Cierre y Volúmenes, columnas 4 y 5 de cada stock:
  datos <- datos[, 5:6]
  # Para hacer los datos accesibles, asignamos a Global Environment:
  assign(simbolo, datos, envir = .GlobalEnv)
}

# Llamamos la función para cada stock desde el 2014:
precios_volumenes("IBM", 2015)
precios_volumenes("ORCL", 2015)
precios_volumenes("INTC", 2015)
precios_volumenes("MSFT", 2015)

# Juntamos los datos y renombramos las columnas:
PyV <- merge.xts(IBM, ORCL, INTC, MSFT)
colnames(PyV) <- c("IBM Vol","IBM P. Ajust", "Oracle Vol","Oracle P. Ajust", 
                   "Intel Vol","Intel P. Ajust", "Microsoft Vol","Microsoft P. Ajust")

##Serie De Tiempo:
# Podemos generar una gráfica interactiva las variables, en este caso de los precios:
Precios<-  dygraph(PyV[,c(2,4,6,8)], main = "Precios de IBM, Oracle, Intel y Microsoft") %>%
  dyAxis("y", label = "Precios") %>%
  dyRangeSelector(dateWindow = c("2015-01-01", "2020-12-31")) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1")) 
Precios

# Podemos ver los 5 ultimos datos redondeando hasta 3 decimales:
round(tail(PyV, n = 5), 3)

#########################################################################################################
# Ejemplo de Panel Data, generemos una list de objetos dygraphs, y para imprimirlos usamos htmltools:
library(dygraphs)
library(htmltools)
dy_graficos <- list(
  dygraphs::dygraph(PyV[,c(1,3,5,7)], main = "Volumenes de IBM, Volumenes de Oracle, Volumenes de Intel y Volumenes de Microsoft"), 
  dygraphs::dygraph(PyV[,c(2,4,6,8)], main = "P.Ajust de IBM, P.Ajust de Oracle, P.Ajust de Intel y P.Ajust de Microsoft"))


# Representemos los objetos dygraphs usando htmltools
htmltools::browsable(htmltools::tagList(dy_graficos))


###############################################################################################
### Datos tipo Transversales o Cross Sectional
### Seleccionaremos los datos de AMZN del 2014 y del 2020. 
#Empecemos seleccionando los años 2014 de AMZN que es la 1ra columna.

MSFT_2015<-subset(PyV[,8], index(PyV)>="2015-01-01"& index(PyV)<="2015-12-31")
MSFT_2015[c(1:5, nrow(MSFT_2015))]
#Para el año 2020:
MSFT_2020<-subset(PyV[,8], index(PyV)>="2020-01-01"& index(PyV)<="2020-12-31")
MSFT_2020[c(1:5, nrow(MSFT_2020))]

## Ahora, podemos tambien visualizarlo, elegimos un histograma  
par(mfrow=c(2,1))

hist(MSFT_2015, freq = FALSE, col="yellow", border="blue",main= "Densidades de los Precios Ajust MCST en 2015", xlab = "Precios Ajustados")
lines(density(MSFT_2015), lwd = 2, col = 'red')
hist(MSFT_2020, freq = FALSE, col="blue", border="blue",main= "Densidades de los Precios Ajust MCSFT en 2020", xlab = "Precios Ajustados")
lines(density(MSFT_2020), lwd = 2, col = 'red')