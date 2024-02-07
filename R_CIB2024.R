##### R como calculadora #####
#### Operadores lógicos ####
3+2
3-2
3*2
3/2 # División con decimales
3%/%2 # División entera
3**2
3^2

#### Operadores lógicos ####
1==2 ## 1 es igual a 2?
1!=2 ## 1 es diferente de 2?
1<0 ## 1 es menor que 0?
1>0 ## 1 es mayor que 0?

##### Objetos #####
#### Variables ####
a=2 
b<-3 
b < -3  # OJO!
c <- "Hola"
e <- "Adios"


#### Vectores ####
numeros <- c(1,2,3,4,5)
saludos<-c("Hola","Buenos días", "Buenas tardes")
saludos[2]
saludos[c(3,1,2)]

#### Conjuntos de datos ####
calendario<-data.frame(meses=c("enero","febrero","marzo","abril","mayo","junio",
                               "julio","agosto","septiembre","octubre",
                               "noviembre","diciembre"),
                       dias=c(31,28,31,30,31,30,31,31,30,31,30,31))
calendario$meses

#### Funciones ####
log(10)
sum(a,b)
help(sum)

sum(meses_dias$dias) # Sumatorio
mean(meses_dias$dias) # Media
median(meses_dias$dias) # Mediana
sd(meses_dias$dias) # Desviación estandar


media.dos<-function(x,y){
  (x+y)/2
}
media.dos(3,5)
mean(c(3,5))
