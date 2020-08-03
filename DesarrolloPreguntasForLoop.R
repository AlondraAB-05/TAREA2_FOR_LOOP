
##En el poder judicial tienen un sistema que clasifica los documentos por categorías, 
##con el objetivo de ordenarlos de manera eficiente, las categorías más frecuentes son las medidas de protección 
##y oficios con solicitudes.En cuanto a los oficios, estos van asociados a un caso, el cual se puede repetir más de una vez.  

##A continuación se le entrega una lista con medidas de protección (mp) y oficios (of): 

##Lista <- list(c("mp","Juan","Christofer"),
                        #c("of","av01","ampr"),
                        #c("of","av01","ante"),                      
                        #c("of","av08","arme"),
                        #c("of","av02","ante"),
                        #c("of","av07","ampr"),                      
                        #c("of","av03","dape"),
                        #c("of","av01","meca"),
                        #c("of","av02","dape"),                      
                        #c("mp","Antonia"),
                        #c("mp","Christian","Mario"), 
                        #c("mp","Jose","Pedro","Antonela"),
                        #c("of","av05","meca"), 
                        #c("of","av04","dape"),
                        #c("of","av02","arme")) 

## Dentro de los oficios podemos encontrar temáticas como: Antecedentes (ante), Datos Personales (dape), 
##Medidas Cautelares (meca), Audiencia de Revisión de Medidas (arme) y Ampliación Medidas de Protección (ampr). 
##Estas pueden ser aceptadas o rechazadas por el tribunal. 



##EJERCICIO 1. Las mp tienen los nombres de las personas a las cuales están asociadas, 
##realice una función que cuente cuántos niños hay por cada una, y entregue una estadística de cuántos niños hay por mp. 

##Ejemplo: 
  ##Se cuentan con 5 mp de 2 niños Se cuentan con 4 mp de 1 niños Se cuentan con 2 mp de 3 niños 

Lista <- list(c("mp","Juan","Christofer"),
                        c("of","av01","ampr"),
                        c("of","av01","ante"),                      
                        c("of","av08","arme"),
                        c("of","av02","ante"),
                        c("of","av07","ampr"),                      
                        c("of","av03","dape"),
                        c("of","av01","meca"),
                        c("of","av02","dape"),                      
                        c("mp","Antonia"),
                        c("mp","Christian","Mario"), 
                        c("mp","Jose","Pedro","Antonela"),
                        c("of","av05","meca"), 
                        c("of","av04","dape"),
                        c("of","av02","arme"))

TotalMP<- 0
TotalNiños<-0

for (Totales in Lista) {
  if (Totales[1]== "mp"){
    TotalMP <- TotalMP+1
    Niños <- length(Totales)-1
    Informacion <- c("MP", TotalMP, "tiene", Niños, "niños") 
    print(Informacion)
    TotalNiños <- TotalNiños + length (Totales) -1
  }
}

print(paste("Hay", TotalMP, "MP con", TotalNiños, "Niños en total"))

Promedio <- print(paste("Hay en promedio", TotalNiños/TotalMP, "niños por cada MP"))





##EJERCICIO 2. Los oficios están compuestos por el código al cual pertenecen, construya una función que almacene los códigos y las temáticas a las que están asociadas. 

##Ejemplo: 
  ##av01, ante, arme, ampr av02, arme av03, dape, ampr 



##EJERCICIO 3. Construya una función que actúe de juez y retorne aprobada o reprobada para los diferentes oficios, y entregue la cantidad que hay de cada una. 

##Ejemplo: 
  ##Llegaron 10 oficios de los cuales: 7 son aprobados y 3 reprobados 


List <- list() #Es en donde se va a almacenar la información del ejercicio 2
contador2<-0 # variable para controlar el crecimiento de la matriz List
bandera<-0 #variable para la igualdad
BanderaBandera<-0 #variable para el control de la matriz List

for(i in Lista) { 
  if (i[1] == "of") { 
    if(contador2==0){  #Se almacena el primer dato
      contador2<-contador2+1  #Inicio del contador 
      List[contador2] <- list(c(i[2], i[3])) #Se almacena la primera variable 
      if (1==rbinom(1,1,0.5)){ #Rbinom genera 1 o 0 de forma aleatoria, 1 aprobado, 2 reprobado
        Dictado<-"Aprobado" 
      }else{
        Dictado<-"Reprobado"
      }
      Juez <-(c("El Juez dicta como ", Dictado, " el oficio:", i[2] )) 
      print(Juez)
    }else{
      for (j in List){ 
        BanderaBandera<-BanderaBandera+1 #Se empieza a contar 
        if (i[2] == j[1]){ #Se pregunta si av01... es igual a lo ya almacenado, si no, se almacena todo
          bandera<-BanderaBandera #si es igual, se guarda el contador anterior
          variable<-j #tambien se almacena la variable completa
        }
      }
      if(bandera==0){
        contador2<-contador2+1 
        List[contador2] <- list(c(i[2], i[3])) #Se almacenan nuevas variables
        if (1==rbinom(1,1,0.5)){
          Dictado <-"Aprobado" 
        }else{
          Dictado <-"Reprobado" 
        }
        Juez <-(c("El Juez dicta como ", Dictado, " el oficio:", i[2] ))
        print(Juez)
      }else{ #Si las comparaciones son iguales se guarda la temática
        List[bandera] <- list(c(variable, i[3]))
        
      }
      bandera<-0 #Se reinician las variables
      BanderaBandera<-0 
    }
  }
  
}

print(List) 









##Matriz de Crecimiento: Almacena toda la información que se le vaya asignando, no tiene límites, 
##el algoritmo se encarga de revisar la información ahí contenida para no repetirla

##Rbinom: Se encarga de generar 2 números, 1 y 0.

##Las variables se reinician  para mantener el orden y el crecimiento en la matriz.