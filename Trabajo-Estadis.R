#                 TRABAJO ESTADÍSTICA
#  COMPARATIVA DE LAS SITUACIONES LABORALES TRAS TERMINAR DIFERENTES CARRERAS

#______________________________________________________________________________
#
#                 PREPARACIÓN DE LOS DATOS
#______________________________________________________________________________


rownames(BBDD.Estadis)<-c("Total","Educacion", "ArtesHumanidades", "Sociales", "EconomiaDerecho", "Ciencias", "Informatica", "Ingenieria", "AgriculturaAnimales", "Salud", "Servicios")
colnames(BBDD.Estadis)<-c("Campo","Total", "Trabajando", "Desempleo", "Inactivo", "Hombres", "Mujeres")

tabla<-data.frame(BBDD.Estadis)
#Eliminamos información redundante
tabla$Campo<-NULL
tabla
datosM= c(32939, 12970, 13507, 27691, 6811, 1082, 11535, 2028, 24853, 3353)
datosH= c(8169, 7246, 7734, 20658, 5422, 5777, 27151, 2005, 8328, 4367 )

#______________________________________________________________________________
#
#                           PROPORCIONES
#______________________________________________________________________________

#Numero total de encuestados
tabla$Total[1]

#Numero de hombres encuestados
tabla$Hombres[1]
#porcentaje sobre el total
(tabla$Hombres[1]/tabla$Total[1])*100

#Numero de mujeres encuestadas
tabla$Mujeres[1]
#porcentaje sobre el total
(tabla$Mujeres[1]/tabla$Total[1])*100

#Numero de personas encuestadas que hayan estudiado Educación
tabla$Total[2]
#proporción sobre el total
tabla$Total[2]/tabla$Total[1]*100

#Numero de personas encuestadas que hayan estudiado Artes o Humanidades
tabla$Total[3]
#proporción sobre el total
tabla$Total[3]/tabla$Total[1]*100

#Numero de personas encuestadas que hayan estudiado Sociales
tabla$Total[4]
#proporción sobre el total
tabla$Total[4]/tabla$Total[1]*100

#Numero de personas encuestadas que hayan estudiado Economía o Derecho
tabla$Total[5]
#proporción sobre el total
tabla$Total[5]/tabla$Total[1]*100

#Numero de personas encuestadas que hayan estudiado Ciencias
tabla$Total[6]
#proporción sobre el total
tabla$Total[6]/tabla$Total[1]*100

#Numero de personas encuestadas que hayan estudiado Informática
tabla$Total[7]
#proporción sobre el total
tabla$Total[7]/tabla$Total[1]*100

#Numero de personas encuestadas que hayan estudiado Ingeniería
tabla$Total[8]
#proporción sobre el total
tabla$Total[8]/tabla$Total[1]*100

#Numero de personas encuestadas que hayan estudiado Agricultura o Veterinaria
tabla$Total[9]
#proporción sobre el total
tabla$Total[9]/tabla$Total[1]*100

#Numero de personas encuestadas que hayan estudiado Salud
tabla$Total[10]
#proporción sobre el total
tabla$Total[10]/tabla$Total[1]*100

#Numero de personas encuestadas que hayan estudiado Servicios
tabla$Total[11]
#proporción sobre el total
tabla$Total[11]/tabla$Total[1]*100

#______________________________________________________________________
#
#                     ESTADÍSTICOS
#______________________________________________________________________

#       1. MEDIAS

#media poblacional de Personas que terminan trabajando
mean(tabla$Trabajando)

#media poblacional de Personas que terminan sin empleo
mean(tabla$Desempleo)

#media poblacional de Personas que se encuentran Inactivas
mean(tabla$Inactivo)

#media poblacional de Hombres en los principales sectores de estudio
mean(datosH)

#media poblacional de Mujeres en los principales sectores de estudio
mean(datosM)


#       2. VARIANZAS

#Varianza MUJERES

numM=length(datosM);numM
varianzaMujeres= var(datosM)* (numM-1)/numM; varianzaMujeres

#varianza HOMBRES

numH=length(datosH);numH
varianzaHombres= var(datosH)* (numH-1)/numH; varianzaHombres




#______________________________________________________________________
#
#                     TODAS LAS CARRERAS
#______________________________________________________________________

#Todas las carreras Sin Distinción de sexo

carrerasN= c(111, 20216, 21241, 48349, 12233, 6859, 38686, 4033, 33181, 7720)
carreras <- factor(c(rep("1", 41108), rep("2", 20216), rep("3", 21241), rep("4", 48349), rep("5", 12233), rep("6", 6859), rep("7", 38686), rep("8", 4033), rep("9", 33181), rep("10", 7720)))#Hay que hacerlo porque son variables categóricas

tabla_carreras=table(carreras) 
plot(tabla_carreras,xlim= c(0,10),col=1:10,ylab = "Numero de Estudiantes",xlab = "Carreras", main = "Carreras solicitadas por hombres y mujeres")
legend("topright", legend = c("Educación", "ArtesHumanidades","Sociales","EconomiaDerecho","Ciencias","Informática","Ingenieria","AgriculturaAnimales","Salud","Servicios"), fill =  c("black","green","blue","lightblue","purple","yellow","grey","black","red","pink"),title = "Carreras ")


#Las mas solicitadas por hombres


carrerasH <- factor(c(rep("1", 8169), rep("2", 7246), rep("3", 7734), rep("4", 20658), rep("5", 5422), rep("6", 5777), rep("7", 27151), rep("8", 2005), rep("9", 8328), rep("10", 4367 )))#Hay que hacerlo porque son variables categóricas
tabla_carrerasH=table(carrerasH) 
plot(tabla_carrerasH,xlim= c(0,10),col=1:10,ylab = "Numero de Estudiantes",xlab = "Carreras", main= "Carreras solicitadas por hombres")
legend("topright", legend = c("Educación", "ArtesHumanidades","Sociales","EconomiaDerecho","Ciencias","Informática","Ingenieria","AgriculturaAnimales","Salud","Servicios"), fill =  c("black","green","blue","lightblue","purple","yellow","grey","black","red","pink"),title = "Carreras ")






#Las mas solicitudes por mujeres

carrerasM <- factor(c(rep("1", 32939), rep("2", 12970), rep("3", 13507), rep("4", 27691), rep("5", 6811), rep("6", 1082), rep("7", 11535), rep("8", 2028), rep("9", 24853), rep("10", 3353 )))#Hay que hacerlo porque son variables categóricas
tabla_carrerasM=table(carrerasM) 
plot(tabla_carrerasM,xlim= c(0,10),col=1:10,ylab = "Numero de Estudiantes",xlab = "Carreras", main= "Carreras solicitadas por mujeres")
#legend("topright", legend = c("Educación", "ArtesHumanidades","Sociales","EconomiaDerecho","Ciencias","Informática","Ingenieria","AgriculturaAnimales","Salud","Servicios"), fill =  c("black","green","blue","lightblue","purple","yellow","grey","black","red","pink"),title = "Carreras ")


#Ambas en el mismo grafico
plot(tabla_carrerasH,tabla_carrerasM)



#______________________________________________________________________
#
#                     INFORMÁTICA
#______________________________________________________________________

#########Grafico queso de num de mujeres y num de Hombres#########
attach(tabla)
numMInfor= Mujeres[7];
numHInfor= Hombres[7];

genero <- factor(c(rep("Hombres", numHInfor), rep("Mujeres", numMInfor)))#Hay que hacerlo porque son variables categóricas
tabla_genero <- table(genero)

#El tanto por ciento (A %) de cualquier cantidad (C) se calcula multiplicando esa cantidad C por el número A del porcentaje, y dividiendo el resultado por 100. (A% de C = C*A/100

pie(tabla_genero, main="Informática")
legend("topleft", legend = c("Hombres  84%", "Mujeres  16%"), fill =  c("white", "lightblue"))



#########Grafico queso Tasa de Empleo en general#########

nTrabajando = Trabajando[7]
nSinTrabajo =Desempleo[7]+Inactivo[7];nSinTrabajo

empleo=factor(c(rep("Empleados",nTrabajando),rep("Desempleados", nSinTrabajo)))
tabla_empleo= table(empleo)

pie(tabla_empleo, main = "Tasa de empleo en Informática")
legend("topleft", legend = c("Empleados  96%","Desempleados  4%"), fill =  c( "lightblue", "white"))


#______________________________________________________________________
#
#                     Salud
#______________________________________________________________________

#########Grafico queso de num de mujeres y num de Hombres#########
numMsalud= Mujeres[10];
numHsalud= Hombres[10];

genero <- factor(c(rep("Hombres", numHsalud), rep("Mujeres", numMsalud)))#Hay que hacerlo porque son variables categóricas
tabla_genero <- table(genero)

#El tanto por ciento (A %) de cualquier cantidad (C) se calcula multiplicando esa cantidad C por el número A del porcentaje, y dividiendo el resultado por 100. (A% de C = C*A/100



barplot(tabla_genero, main = "Salud", col = c("lightblue","pink"),ylim=c(0,25500),xlim=c(0,5.3))

#########Grafico queso Tasa de Empleo en general#########

nTrabajando = Trabajando[10]
nSinTrabajo =Desempleo[10]+Inactivo[10];nSinTrabajo

empleo=factor(c(rep("Empleados",nTrabajando),rep("Desempleados", nSinTrabajo)))
tabla_empleo= table(empleo)

pie(tabla_empleo, main = "Tasa de empleo en Salud")
legend("topleft", legend = c("Empleados  92%","Desempleados  8%"), fill =  c( "lightblue", "white"))

#______________________________________________________________________
#
#                     Educación
#______________________________________________________________________

#########Grafico queso de num de mujeres y num de Hombres#########
numMEdu= Mujeres[2];
numHEdu= Hombres[2];

genero <- factor(c(rep("Hombres", numHEdu), rep("Mujeres", numMEdu)))#Hay que hacerlo porque son variables categóricas
tabla_genero <- table(genero)

#El tanto por ciento (A %) de cualquier cantidad (C) se calcula multiplicando esa cantidad C por el número A del porcentaje, y dividiendo el resultado por 100. (A% de C = C*A/100

pie(tabla_genero,main = "Educación")
legend("topleft", legend = c("Hombres  20%", "Mujeres  80%"), fill =  c("white", "lightblue"))



#########Grafico queso Tasa de Empleo en general#########

nTrabajando = Trabajando[2]
nSinTrabajo =Desempleo[2]+Inactivo[2];nSinTrabajo

empleo=factor(c(rep("Empleados",nTrabajando),rep("Desempleados", nSinTrabajo)))
tabla_empleo= table(empleo)

pie(tabla_empleo, main = "Tasa de empleo en Salud")
legend("topleft", legend = c("Empleados  82%","Desempleados  18%"), fill =  c( "lightblue", "white"))


#______________________________________________________________________
#
#                     AgriculturaAnimales
#______________________________________________________________________

#########Grafico queso de num de mujeres y num de Hombres#########
numMagri= Mujeres[9];
numHagri= Hombres[9];

genero <- factor(c(rep("Hombres", numHagri), rep("Mujeres", numMagri)))#Hay que hacerlo porque son variables categóricas
tabla_genero <- table(genero)

#El tanto por ciento (A %) de cualquier cantidad (C) se calcula multiplicando esa cantidad C por el número A del porcentaje, y dividiendo el resultado por 100. (A% de C = C*A/100

pie(tabla_genero, main = "Agricultura y Animales")
legend("topleft", legend = c("Hombres  49.7%", "Mujeres  50.3%"), fill =  c("white", "lightblue") )



#########Grafico queso Tasa de Empleo en general#########

nTrabajando = Trabajando[9]
nSinTrabajo =Desempleo[9]+Inactivo[9];nSinTrabajo

empleo=factor(c(rep("Empleados",nTrabajando),rep("Desempleados", nSinTrabajo)))
tabla_empleo= table(empleo)

pie(tabla_empleo, main = "Tasa de empleo en Agricultura y Animales")
legend("topleft", legend = c("Empleados  87%","Desempleados  13%"), fill =  c( "lightblue", "white"))

