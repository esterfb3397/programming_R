#Pregunta 1 (20 puntos): Un boleto del sorteo de la ONCE consta de dos partes, la primera es
#un número de 4 dígitos y la segunda es un número de tres dígitos que forman la serie del boleto.
#Aquí consideramos sólo el número, por ejemplo,
#0 2 0 9
#Se pide:
#a) Genera todos los números que entran en el sorteo de la ONCE y mostrarlos con los
#cuatro dígitos.
#b) ¿Cuál es la suma de los números de un boleto que más se repite?

#a) 
#Primero de todo creamos la matriz y sus dimensión 10000x4
X = matrix(0, 10000, 4)
#Para que salgan todos las posibles combinaciones de la lotería hay que crear un
#bucle
for (i in 0:10000) {
  # digito 1
  X[i,4] = i%%10
  # digito 2
  X[i,3] = trunc(i/10)%%10 # 3542 / 10 = 354.2 -> trunc(354.2) = 354 -> 354 %% 10 = 4
  # digito 3
  X[i,2] = trunc(i/100)%%10 # 3542 / 100 = 35.42 -> trunc(35.42) = 35 -> 35 %% 10 = 5
  # digito 4
  X[i,1] = trunc(i/1000)%%10
}

#Aquí se encuentra el ejemplo con las 10 primeras filas
#       [,1] [,2] [,3] [,4]
#[1,]    0    0    0    1
#[2,]    0    0    0    2
#[3,]    0    0    0    3
#[4,]    0    0    0    4
#[5,]    0    0    0    5
#[6,]    0    0    0    6
#[7,]    0    0    0    7
#[8,]    0    0    0    8
#[9,]    0    0    0    9
#[10,]   0    0    1    0

#b)

b = (table(rowSums(X)))
b
rowSums(X)
#0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22 
#1   4  10  20  35  56  84 120 165 220 282 348 415 480 540 592 633 660 670 660 633 592 540 
#23  24  25  26  27  28  29  30  31  32  33  34  35  36 
#480 415 348 282 220 165 120  84  56  35  20  10   4   1

max(table(rowSums(X)))
#[1] 670 Por lo que la suma que más sale es el 18

#Pregunta 2 (20 puntos): En la carpeta covid_19 hay una serie de archivos sobre el covid-19 en
#España (Fuente:
          #https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-
          #China/situacionActual.htm).
#Se pide:
  #a) Leer los archivos “datos_provincias.csv”, “CodProv.txt* y “CodCCAA.dat “. Añade el
#código de la comunidad autónoma al fichero “datos_provincias.csv” (no manualmente).
#b) Selecciona los datos de la comunidad autónoma que te corresponda. Para saber cuál es
#tu comunidad autónoma realiza la siguiente operación
#DNI o Pasaporte mod 17 por ejemplo (12345678 %% 17 = 6 → Castilla y León)
#hay que seleccionar las provincias de Castilla y León
#Gráficos
#c) Realizar un gráfico que muestre adecuadamente la evolución de los casos nuevos.
#Justifica el gráfico elegido.
#d) Presenta en único gráfico la evolución de las distintas variables (columnas) por medio
#de un gráfico de líneas múltiples. Utiliza diferentes colores y añade una leyenda muestre
#el origen de cada línea.

#a)
library(readr)
datosbrutos_provincias <- read_csv("~/Escritorio/MASTER/4.R/Datos/Cod_19/datos_provincias.csv")
codigo_CCAA <- read_delim("~/Escritorio/MASTER/4.R/Datos/Cod_19/CodCCAA.csv", 
                                   delim = "\t", escape_double = FALSE, 
                                   trim_ws = TRUE)
codigo_provincia <- read_csv("~/Escritorio/MASTER/4.R/Datos/Cod_19/CodProv.txt")
#Hemos comprobado que en datos_provincias hay varios NA por lo que hay que eliminarlos
library(tidyr)
datos_provincias<-datosbrutos_provincias[!is.na(datosbrutos_provincias$provincia_iso),]
#Antes de unir las tablas datos_provincias y codigo_CCAA, hay que hacer una serie de pasos
library(plyr)
#Separamos el codigo ES y las iniciales del código de provincia
pro1 <-subset(codigo_provincia, select= "Código")
pro2<-pro1%>%separate("Código",c('ES','provincia_iso'),"-",remove = TRUE)
pro3 <- subset(pro2,select = "provincia_iso")
#Ahora crearemos un dataset aparte para poder adjuntarlo con datosbrutos_provincia
codigo_provincia <- cbind(codigo_provincia, provincia_iso = c(pro3))
codigo_provincia2 <- subset(codigo_provincia,select = c("provincia_iso","Comunidad autónoma"))
#Creamos un nuevo dataframe a partir del merge
datos <- merge(datosbrutos_provincias,codigo_provincia2,by = "provincia_iso")
#Reordenamos el dataset para que el codigo provincia este al lado del codigo de la CCAA
datos1 <-  datos [ , c(1,8,2,3,4,5,6,7)]

#b) Para saber la comunidad autonoma tendremos que usar el data set codigo_CCAA

#Primero de todo hacemos la operacion con el DNI
DNI <- 48231944

indice_ca<- DNI %% 17

indice_ca
codigo_ca<- codigo_CCAA[indice_ca,1]

ca <- codigo_CCAA[indice_ca,2]

cat("Mi comunidad autonoma es",ca[[c(1)]],"con codigo",codigo_ca[[c(1)]])

datos_asturias <- datos1[datos1$`Comunidad autónoma` == "AS",]

#c)
library(ggplot2)
library(lubridate)
#Ordenamos los datos
datos_asturias <-datos_asturias[ order ( as.Date (datos_asturias$ fecha, format = " % m /% d /% Y ")),]
#Hacemos un attach para acceder mejor a los datos
attach(datos_asturias)
x <- fecha
y <- num_casos

plot(x,y, type = "h",col = "red", lwd = 5,main = "Evolución de los casos de covid-19 en Asturias", xlab = "fecha", 
     ylab = "casos de covid")
#d)
#Podemos reutilizar el gráfico del apartado anterior ya que tenemos que añadir más variables.
#Las cuales las definiremos a continuacion


y1 <- num_casos_prueba_pcr
y2 <- num_casos_prueba_test_ac
y3 <- num_casos_prueba_otras
y4 <- num_casos_prueba_desconocida



plot(x,y1, type = "h",col = "red", lwd = 2,main = "Evolución de los casos de Covid-19 en Asturias", 
     xlab = "fecha",ylab = "casos de covid")
#Añadimos la segunda variable
lines(x,y2, type = "h", col = "blue", lwd = 2)
#La tercera variable
lines(x,y3, type = "h", col = "green", lwd = 4)
#La cuarta variable
lines(x,y4, type = "h", col = "yellow", lwd = 2)

#Dibujamos la leyenda
windows(width = 3, height = 3)
# Cambiamos los márgenes del gráfico (el primero es el margen inferior)
par(mar = c(1, 1.5, 1.5, 1.5))


legend(x = "bottom",
       inset = c(0, -0.5), # Tendrás que ajustar el segundo valor
       # dependiendo del tamaño de tu gráfico
       legend = c("num_casos_prueba_pcr","num_casos_prueba_test_ac",
                  "num_casos_prueba_otras","num_casos_prueba_desconocida"),
       lty = c(1, 1,1,1,1),
       col = c("red","blue","green","yellow"),
       xpd = TRUE)# Tienes que especificar este parámetro gráfico
       # para poner la leyenda fuera del gráfico
 
# Volvemos a los parámetros gráficos por defecto
on.exit(par(opar))

#Pregunta 3 (30 puntos): Una tienda vende una serie de artículos cuyo precio y volumen de
#ventas están recogidos en el archivo “articulo.xlsx”. Al llegar el periodo de rebajas se plantea
#aplicar un descuento para lo cual decide clasificar los artículos en tres tipos A, B y C. Los artículos
#se clasifican en función del ingreso bruto que proporcionan (𝑖𝑛𝑔𝑟𝑒𝑠𝑜𝑏𝑟𝑢𝑡𝑜 = 𝑃𝑉𝑃 ×
#𝑐𝑎𝑛𝑡𝑖𝑑𝑎𝑑), de forma que el 20% de los que más aportan se clasifican de tipo A, el 30% siguiente
#de Clase B y el resto de clase C.
#El porcentaje de rebaja que aplicará en función de esta clasificación se encuentra en el archivo
#“descuento_aplicar.txt”.
#Se pide:
#a)Crear dos data frame de nombres, uno de nombre articulo leyendo la información de
#“articulo.xlsx” y el otro de nombre descuento que guarde la información de
#“descuento_aplicar.txt”. (5 ptos)
#b)Crear una variable llamada tipo que clasifique los artículos en los tipos A, B y C en la
#forma indicada y añadirlo al data frame artículo y calcula, en una única sentencia, el
#total de ingresos brutos por cada tipo de artículo.
#c)Unir los dos data frame, articulo y descuento, de forma adecuada, para crear el data
#frame clientes. Calcular la variable nuevo_pvp = pvp – cantidad a descontar. Suponiendo
#que el volumen de ventas se mantiene constante dar una estimación del porcentaje de
#decremento en los ingresos brutos al aplicar los descuentos.

#a
library(readr)

descuento <- read_delim("~/Escritorio/MASTER/4.R/Datos/descuento_aplicar.txt", 
                                             delim = "\t", escape_double = FALSE, 
                                             trim_ws = TRUE, show_col_types = FALSE)
library(readxl)
articulo <- read_excel("~/Escritorio/MASTER/4.R/Datos/articulo.xlsx")

#b)
#Primero de todo añadimos una nueva columna con el ingreso bruto por articulo
articulo$ingresobruto <- articulo$PVP*articulo$CANTIDAD
# Ordenando por la columna ingresobruto
df_ordenado <-articulo[order(-articulo$ingresobruto), ]
#Ahora creamos una nueva columna llamada tipo y vamos agrupando los valores
#Calculamos los quantiles agrupandolos por ingresobruto acumulado
deciles <- quantile(articulo$ingresobruto,probs = seq(.1,.9,by = .1))
deciles
#Para el 80% acumulado sería 6108000, para el 70% y 50%, 3020952 y 1797000 consecutivamente
#y el resto el 50%
check <- function(ingresobruto) {
  if (ingresobruto >6108000) {
    result <- "A"
  }
  else if (ingresobruto >= 1797000 & ingresobruto < 6108000) {
    result <- "B"
  }
  else {
    result <- "C"
  }
  return(result)
}


new_colum = sapply(df_ordenado$ingresobruto,check)

articulo <- cbind(df_ordenado, tipo = c(new_colum))

#c)
#Primero de todo unimos los dos dataframes
clientes <- merge(articulo,descuento,by = "tipo")
#creamos la nueva columna con el nuevo precio
clientes$nuevoPVP <- clientes$PVP-clientes$descuento
#calculamos el nuevo ingreso
clientes$ingresobruto2 <- clientes$nuevoPVP*clientes$CANTIDAD

#Pregunta 4 (30 puntos): La siguiente tabla representa puntuaciones de sensación de ardor
#para 16 sujetos en un estudio para probar un nuevo hidrogel. La primera columna da el número
#del sujeto. Las siguientes columnas dan la puntuación de sensación de ardor (en una escala de
#Propondremos una forma interesante de mostrar estos datos.
#1.- Para la semana 𝑆 7 , calcule el vector (𝑓 1 , 1 − 𝑓 1 , 𝑓 2 , 1 − 𝑓 2 , 𝑓 3 , 1 − 𝑓 3 , 𝑓 4 , 1 − 𝑓 4 ) donde
#𝑓 𝑖 es la frecuencia (relativa) de la modalidad 𝑖 ∈ {1,2,3,4} observada en la semana 𝑆 7
#sobre los 16 sujetos. (Sugerencia: use las funciones tabulate() , cbind() , t() y as.vector() ).
#Ahora, use la función apply() para hacer el mismo cálculo para todas las demás semanas.
#Almacene el resultado en una matriz.
#2.- Utilice la función barplot() y el argumento col = c ("black", "white") en esta matriz. El
#gráfico que se obtiene ofrece una descripción general de la evolución de la Sensación de
#ardor con el tiempo.
#3.- Cambie el gráfico anterior para que las barras que representan las frecuencias estén
#en rojo. Los números de las semanas deben estar en azul y en la parte superior del
#gráfico en lugar del fondo. Los números de modalidad deben estar a la izquierda, en
#azul. Agrega un título al gráfico 1 a 4) para semanas 1 (S1) a 7 (S7). (La matriz de datos se encuentra en “matriz.R” , no se puede 
#abrir el fichero y copiar su contenido en el script)
#1
#Lo primero de todo cargamos la matriz de datos del fichero de R a traves de source

matriz <- source("~/Escritorio/MASTER/4.R/Datos/matriz.R")

#Para ver que tipo de objeto es matriz
str(matriz)


matriz$value

f <- function(x) {
  fi = tabulate(x, nbins = 4) / length(x)
  return(c(
    fi[1],
    1 - fi[1],
    fi[2],
    1 - fi[2],
    fi[3],
    1 - fi[3],
    fi[4],
    1 - fi[4]
  ))
}

# Cálculo del vector S7
S7 <- as.vector(f(matriz$value[,8]))
S7

# Cálculo completo de la matriz de frecuencias utilizando la función apply()
Z = apply(matriz$value[,2:8],2,f)
nombres_columnas <- c("S1", "S2", "S3", "S4","S5","S6","S7")
colnames(Z) <- nombres_columnas

#2.
barplot(Z,col = c ("black", "white"))

#3.
par(bty = "u",adj=0)
barplot(Z, col = c ("red", "white"),axes = FALSE,xaxt = "n") 
axis(3,col.axis = "blue",at = c(0.8,1.9,3.1,4.3,5.5,6.7,7.9), labels=c("S1", "S2", "S3", "S4","S5","S6","S7"),
     tck = 0,lwd = 0)
axis(2,col.axis = "blue")
title(main = "Frecuencia sensación de ardor",line = 3)

