install.packages("readr") #Lo instalo para leer archivos CSV
library(readr)

file.choose() #Me sirve para buscar la ruta del archivo con el que quiero trabajar

ruta <- "C:\\Users\\Usuario\\Desktop\\Politecnico\\insurance.csv"

insurance <- read_csv(ruta) #cargo el csv en la variable insurance

head(insurance) #head me sirve para visualizar los datos

insurance.scale <- as.data.frame(scale(insurance[,5:9])) #Escalo el dataframe que en este caso se llama "insurance" para que todas las variables tengan el mismo peso

#------------------------------------#
       #CREACION DE CLUSTER#
#------------------------------------#

set.seed(80) #Fijo semilla para que despues al repetir el codigo no de distinto

insurance.km <- kmeans(insurance.scale, centers = 4) #Hago los cluster y elijo hacerlo con 4

names(insurance.km) #names me sirve para ver que contiene el objeto kmeans creado, es decir de que se compone

insurance.km$cluster #Con eso asigno cada observación a un cluster
insurance.km$totss #Con esto hago la evaluacion de la inercia total de los grupos con respecto al centroide de todas las observaciones
insurance.km$betweenss #Con esto hago la evaluacion de la inercia inter grupos que se busca maximizarla ya que asegura herogeneidad entre los grupos
insurance.km$withinss#Con esto hago la evaluacion de la inercia dentro de los grupos (me tira uno para cada grupo) llamada inercia intragrupos la cual debe ser la menor posible
insurance.km$tot.withinss#Esto me permite ver una suma de las inercias intragrupo de recien, esta tiene que ser baja.

#----------------------------------------------#
  #SELECCIONAR LA CANTIDAD DE CLUSTER OPTIMO#
#----------------------------------------------#

#Si bien no hay una medida ideal de cluster porqeu depende de lo que estemos buscando, podemos apoyarnos para calcularlo en la inercia intergrupos
#Para ver como es la inercia intercluster vamos a provar como seria con 1 cluster con 2 y asi hasta el numero de cluster que querramos probar

suma_betweenss <- kmeans(insurance.scale, centers = 1)$betweenss #Armo una variable donde voy guardar la insercia inter grupos, de una kmeans con los datos escalados y un solo cluster, así armo un vector
for(i in 2:10) suma_betweenss[i] <-kmeans(insurance.scale, centers = i)$betweenss #hago un for que va desde el 2 al 10 y lo que va a ir haciendo es ir incluyendo en el vector los betweenss para cada uno de los cluster

#vamos a graficar
plot(1:10,suma_betweenss, type = "b", xlab = "Nº de cluster", ylab = "suma de cuadrados inter grupos") #Grafica un vector con un circulo para cada punto, introduce etiqueta para la x y para las y

#----------------------------------------------#
            #ANALISIS DE LOS DATOS#
#----------------------------------------------#

plot(insurance$ant_comp, insurance$ant_perm, col=insurance.km$cluster ,xlab= "fidelidad", ylab = "experiencia") #Aca lo que hago es elegir dos variables de mi dataset (antiguedad en la compania y antiguedad del permiso de conducir) y que pinte las observaciones en base al cluster al que fueron asignadas, finalmente introduzco una etiqueta para las variables elegidas.

aggregate(insurance[,5:9], by= list(insurance.km$cluster), mean) #En este analisis lo que hacemos es tomar todas las varibles numericas con las que trabajamos en (por eso asigno el rango) las enlisto en base al cluster al que se asignaron y pido la media
