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
e<-3 # sobreescribir una variable

#### Vectores ####
numeros <- c(1,2,3,4,5)
saludos<-c("Hola","Buenos días", "Buenas tardes")
saludos[2]
saludos[c(3,1,2)]

#### Conjuntos de datos ####
meses_dias<-data.frame(meses=c("enero","febrero","marzo","abril","mayo","junio",
                               "julio","agosto","septiembre","octubre",
                               "noviembre","diciembre"),
                       dias=c(31,28,31,30,31,30,31,31,30,31,30,31))
meses_dias$meses

#### Funciones ####
log(10)
sum(a,b)
help(sum)

sum(meses_dias$dias) # Sumatorio
mean(meses_dias$dias) # Media
median(meses_dias$dias) # Mediana
sd(meses_dias$dias) # Desviación estandar

# Sintaxis de una función
media.dos<-function(x,y){
  (x+y)/2
}
media.dos(3,5)
mean(c(3,5))


##### Clases de objetos #####
c="4"
class(c)
d<-a+c # No permite sumar nº y letras
d<-c+e # No permite sumar caracteres

## 4.1 Importar datos de Excel (.xlsx)
# install.packages("readxl") # Descomentar esta línea para instalarlo
library(readxl)

bca<-read_xlsx("BCA_assay.xlsx",sheet=1,skip = 1)
str(bca)

# 1. Calculamos la media de absorbancia de cada muestra
bca$media<-rowMeans(bca[,c(2,3)])
# 2. Restamos a cada media la señal del blanco
bca$media_sin_blanco<-bca$media-bca$media[bca$nombre=="I"]
# 3. Necesitamos que la concentración R la lea como un número
bca$concentracion<-as.numeric(bca$concentracion)
# 4. Construimos la recta patrón con una regresión lineal (y= a +bx)
plot(bca$concentracion,bca$media_sin_blanco)
linear.model<-lm(media_sin_blanco ~ concentracion, data=bca[1:9,])
a<-linear.model$coefficients[1]
b<-linear.model$coefficients[2]
# 5. Calculamos la concentración de cada muestra
bca$concentracion[c(10,11)]<-(bca$media_sin_blanco[c(10,11)]-a)/b

## 4.2 Importar datos de un archivo tabulado (.tsv)
# Dos opciones igual de válidas:
tmb<-read.delim("tmb_mskcc_2018_clinical_data.tsv")
# Con read.table obligatorio poner el separador
tmb<-read.table("tmb_mskcc_2018_clinical_data.tsv",header=TRUE,sep="\t")


#### Análisis descriptivo ####
class(tmb) # vemos que R lo reconoce como data.frame
str(tmb) # estructura del conjunto de datos

# PREGUNTA DIRIGIDA 1. Sabiendo cómo se accede a las variables y recordando que 
# la media se puede calcular con la función mean(), ¿sabríais decirme cuál es la
# media de la variable Age.at.Which.Sequencing.was.Reported..Days.?


#### Filtrar una variable ####
mean(tmb$Overall.Survival..Months.)
mean(tmb$Overall.Survival..Months.[tmb$Overall.Survival.Status=="1:DECEASED"])
mean(tmb$Overall.Survival..Months.[tmb$Overall.Survival.Status=="0:LIVING"])

# PREGUNTA DIRIGIDA 2.Ahora que sabemos cómo filtrar una variable, ¿cuál es la
# media de la variable Overall.Survival..Months. en mujeres?

##### Función summary() #####
summary(tmb$Overall.Survival..Months.)
summary(tmb$Overall.Survival..Months.[tmb$Overall.Survival.Status=="1:DECEASED"])
summary(tmb$Overall.Survival..Months.[tmb$Overall.Survival.Status=="0:LIVING"])

range(tmb$Overall.Survival..Months.[tmb$Overall.Survival.Status=="0:LIVING"])

par(mfrow=c(1,2))

#### Graficos Rbase

#### Gráfico de cajas y bigotes: función boxplot() ####
boxplot(tmb$TMB..nonsynonymous.~tmb$Overall.Survival.Status)

# Sexo
sex.freq<-table(tmb$Sex)
barplot(sex.freq) # Lo más sencillo
sex.bar<-barplot(sex.freq,
                 col=c("blue","red"), # 1 color por cada grupo que se quiere representar
                 xlab="Sexo",
                 ylab="Número de pacientes",
                 ylim = c(0,max(sex.freq)+200), # Limite eje Y
) # Un poco más bonito
text(x=sex.bar, y=sex.freq+20,
     labels=paste0(round(sex.freq/sum(sex.freq)*100),"%") ,cex=1) # Etiqueta de porcentaje

#### El color en los gráficos ####
# Edad
age.freq<-table(tmb$Age.Group.at.Diagnosis.in.Years)[c(1,3,4,5,2)]
age.bar<-barplot(age.freq,
                 col=c("#FF99FF","#0066FF","#00FF4D","#FF9900", "#AA4371"), # 1 color por grupo, 5 grupos
                 xlab="Edad al diagnóstico (años)",
                 ylab="Número de pacientes",
                 ylim = c(0,max(age.freq)+30), 
                 cex.names=0.6
)
text(age.bar, age.freq+20, paste0(round(age.freq/sum(age.freq)*100), "%") ,cex=1)

##### Colores predeterminados ####
# heat.colors(), topo.colors(), terrain.colors(),rainbow(),
age.bar<-barplot(age.freq,
                 col=heat.colors(5), ## Indicamos el nº de colores que necesitamos
                 xlab="Edad al diagnóstico (años)",
                 ylab="Número de pacientes",
                 ylim = c(0,max(age.freq)+30),
                 cex.names=0.6
)

##### Paletas predefinidas ####
# install.packages("RColorBrewer") # descomentar para instalar
library(RColorBrewer) # R color brewer cheat sheet

# R color brewer cheat sheet (nombres de las paletas y opciones de colores)
display.brewer.all(colorblindFriendly = TRUE)

# Edad
age.freq<-table(tmb$Age.Group.at.Diagnosis.in.Years)[c(1,3,4,5,2)]
age.bar<-barplot(age.freq,
                 col=brewer.pal(5, "Blues"), # brewer.pal(5, "RdBu"); brewer.pal(5, "Set3"); brewer.pal(5, "Pastel2")
                 xlab="Edad al diagnóstico (años)",
                 ylab="Número de pacientes",
                 ylim = c(0,max(age.freq)+30),
                 cex.names=0.6
)



#### Gráficos con ggplot2 ####
# install.packages("ggplot2") # Descomentar esta línea para instalarla
library(ggplot2) 

# lineplot | boxplot | dotplot | barplot

##### ggplot2: lineplot ####
# capa base
ggplot(data = tmb[25:100,], aes(x = Mutation.Count, y = TMB..nonsynonymous.)) +
geom_line() 

# Personalizar etiquetas de los ejes
ggplot(data = tmb[25:100,], aes(x = Mutation.Count, y = TMB..nonsynonymous.)) +
  geom_line() +
  labs(title = "Tumor Mutational Burden (Non-Synonymous) vs. Mutation Count", 
       x = "Mutation Count", 
       y = "Tumor Mutational Burden (Non-Synonymous)")

##### ggplot2: boxplot ####
# Capa base
ggplot(data=tmb,
       aes(x=Overall.Survival.Status,y=Overall.Survival..Months.)) + 
  geom_boxplot() # Agregar capa de caja

# Personalizar etiquetas de los ejes
ggplot(data=tmb,
       aes(x=Overall.Survival.Status,y=Overall.Survival..Months.))+
  geom_boxplot() +
  labs(y="Supervivencia global (meses)",x=" ") +
  theme(text=element_text(size=10))

# Eliminar los outliers
ggplot(data=tmb,
       aes(x=Overall.Survival.Status,y=Overall.Survival..Months.))+
  geom_boxplot(outlier.shape = NA) +
  labs(y="Supervivencia global (meses)",x=" ") +
  theme(text=element_text(size=10))

# Añadir color en función de una tercera variable
ggplot(data = tmb, aes(x = Overall.Survival.Status, y = Overall.Survival..Months., fill = factor(Overall.Survival.Status))) + 
  geom_boxplot()

# Editar colores manualmente
ggplot(data = tmb, aes(x = Overall.Survival.Status, y = Overall.Survival..Months., fill = factor(Overall.Survival.Status))) + 
  geom_boxplot() +
  scale_fill_manual(values = c("0:LIVING" = "lightblue", "1:DECEASED" = "grey")) +
  labs(title = "Overall Survival by Status", 
       x = "Survival Status", 
       y = "Overall Survival (Months)")

# Editar nombre de la leyenda:
ggplot(data = tmb, aes(x = Overall.Survival.Status, y = Overall.Survival..Months., fill = factor(Overall.Survival.Status))) + 
  geom_boxplot() +
  scale_fill_manual(values = c("0:LIVING" = "lightblue", "1:DECEASED" = "grey")) +
  labs(title = "Overall Survival by Status", 
       x = "Survival Status", 
       y = "Overall Survival (Months)",
       fill = "Survival Status" )

## R ggplot sheet ##
       
# Superponer plots:
ggplot(data=tmb,
       aes(x=Overall.Survival.Status,y=Overall.Survival..Months.)) +
  geom_jitter(aes(colour=Overall.Survival.Status))+ # datos puntuales categoricos
  theme(text=element_text(size=10))+
  labs(y="Supervivencia global (meses)",x=" ")
       
ggplot(data=tmb,
       aes(x=Overall.Survival.Status,y=Overall.Survival..Months.),
       fill=factor(Overall.Survival.Status))+
  geom_boxplot()+
  geom_jitter(aes(colour=Overall.Survival.Status))+ # datos puntuales categoricos
  theme(text=element_text(size=10))+
  labs(y="Supervivencia global (meses)",x=" ")

ggplot(data=tmb,
       aes(x=Overall.Survival.Status,y=Overall.Survival..Months.),
       fill=factor(Overall.Survival.Status))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(colour=Overall.Survival.Status))+
  theme(text=element_text(size=10))+
  labs(y="Supervivencia global (meses)",x=" ")


##### ggplot2: dotplot ####
## Mostrar los datos filtrados: Dibujar un subconjunto de datos

# Añadir leyenda con color: por defecto a la derecha
ggplot(data=tmb[1:20,], 
       aes(y=Sample.coverage, x=Tumor.Purity, color=Sample.Type)) +
  geom_point(size=3) + # capa de puntos tamaño 3
  theme(text=element_text(size=10))+
  labs(y="Cobertura",x="Pureza Tumoral")

# Posiciones de la leyenda 
ggplot(data=tmb[1:20,],
       aes(y=Sample.coverage, x=Tumor.Purity, color=Sample.Type)) +
  geom_point(size=3) +
  theme(text=element_text(size=10), legend.position = "bottom")+
  labs(y="Cobertura",x="Pureza Tumoral")

ggplot(data=tmb[1:20,],
       aes(y=Sample.coverage, x=Tumor.Purity, color=Sample.Type)) +
  geom_point(size=3) +
  theme(text=element_text(size=10), legend.position = "top")+
  labs(y="Cobertura",x="Pureza Tumoral")

cols <- c("#55AD89", "#EF6F6A") ## cambiamos el color
ggplot(data=tmb[1:20,],
       aes(y=Sample.coverage, x=Tumor.Purity, color=Sample.Type)) +
  geom_point(size=3) +
  scale_color_manual(values = cols) +
  theme(text=element_text(size=10), legend.position = c(0.1, 0.9))+ # coordenadas de leyenda
  labs(y="Cobertura",x="Pureza Tumoral")


##### ggplot2: barplot ####
# Capa base
ggplot(data=tmb,
       aes(x=Cancer.Type, fill=Cancer.Type)) +
  geom_bar() 

# The value of hjust (horizontal) and vjust (vertical) are only defined between 0 and 1:

# 0 : justificado izquierda
# 1 : justificado derecha

# Orientación etiquetas eje X
ggplot(data=tmb,
       aes(x=Cancer.Type, fill=Cancer.Type)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data=tmb,
       aes(x=Cancer.Type, fill=Cancer.Type)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 50, hjust=1)) +
  scale_fill_manual(values = topo.colors(11)) 

# Barras Horizontales
ggplot(data=tmb,
       aes(x=Cancer.Type, fill=Cancer.Type)) +
  geom_bar() +
  coord_flip() +
  scale_fill_brewer(palette = "Paired") +
  xlab("Tipo de Cancer") +
  ylab("Nº de Muestras") +
  ggtitle("Grafico de Barras", subtitle= "Taller de R CIB 2024")

#### Guardar gráficos: ggsave() ####
# No ejecutar
# ruta_file_name = ""
# ggsave(ruta_file_name,
#        plot=last_plot(),
#        device = "png")
# help(ggsave)


# PREGUNTA DIRIGIDA 3. Os dejamos un rato para que hagáis un gráfico de barras 
# para la variable Drug.Type. Queremos enseñar de forma gráfica cuál fue la 
# inmunoterapia de elección más frecuente en esta cohorte.



# PREGUNTA DIRIGIDA 4. Intentad hacer un gráfico de cajas y bigotes para la 
# variable TMB..nonsynonymous. según el tipo de tumor. Primero hacedlo sencillo 
# ¿Veis alguna tendencia?


#### Variables para cada tipo de tumor ####
# Sexo
sex.freq.tumor<-table(tmb$Sex,tmb$Cancer.Type)
sex.tumor.bar<-barplot(sex.freq.tumor,
                       beside=TRUE,
                       legend=rownames(sex.freq.tumor),
                       col=c("blue","red"),
                       xlab="Sexo por tipo de tumor",
                       ylab="Número de pacientes",
                       ylim = c(0,max(sex.freq.tumor)+20),
                       xaxt="n",
                       cex.names=0.5)
text(sex.tumor.bar, sex.freq.tumor+5 ,
     paste0(apply(sex.freq.tumor,2,function(x){round(x/sum(x)*100)}), "%") ,cex=0.6) 
tumor.labels <- c("Bladder","Breast","Unknown\nprimary","Colorectal",
                  "Esophagogastric","Glioma","Head and Neck","Melanoma",
                  "Non-Small\nCell Lung","Renal","Skin")
text(cex=0.8, x=colMeans(sex.tumor.bar)-.25, y=-20,
     tumor.labels,xpd=TRUE, srt=20)

# Edad
age.freq.tumor<-table(tmb$Age.Group.at.Diagnosis.in.Years,
                      tmb$Cancer.Type)[c(1,3,4,5,2),]
age.tumor.bar<-barplot(age.freq.tumor,
                       beside=TRUE,
                       legend=rownames(age.freq.tumor),
                       col=brewer.pal(5, "Set3"),
                       xlab="Edad al diagnóstico (años)",
                       ylim = c(0,max(age.freq.tumor)+20),
                       xaxt="n",
                       cex.names=0.5)
text(age.tumor.bar, age.freq.tumor+5,
     paste0(apply(age.freq.tumor,2,function(x){round(x/sum(x)*100)}), "%"),
     cex=0.6) 

text(cex=0.8, x=colMeans(age.tumor.bar)-.25, y=-15,
     tumor.labels,
     xpd=TRUE, srt=10)

#### Correlación entre variables ####
cor(tmb$TMB,tmb$Overall.Survival..Months.)
cor.tmb.cancer<-sapply(unique(tmb$Cancer.Type),function(x){
  cor(tmb$TMB..nonsynonymous.[tmb$Cancer.Type==x],
      tmb$Overall.Survival..Months.[tmb$Cancer.Type==x])
})
plot(tmb$TMB..nonsynonymous.,tmb$Overall.Survival..Months.)
plot(tmb$Age.at.Which.Sequencing.was.Reported..Days.,tmb$TMB..nonsynonymous.)
