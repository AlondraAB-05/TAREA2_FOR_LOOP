
##En el poder judicial tienen un sistema que clasifica los documentos por categor�as, 
##con el objetivo de ordenarlos de manera eficiente, las categor�as m�s frecuentes son las medidas de protecci�n 
##y oficios con solicitudes.En cuanto a los oficios, estos van asociados a un caso, el cual se puede repetir m�s de una vez.  

##A continuaci�n se le entrega una lista con medidas de protecci�n (mp) y oficios (of): 

##ListaDocumentos <- list(c("mp","Juan","Christofer"),
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

## Dentro de los oficios podemos encontrar tem�ticas como: Antecedentes (ante), Datos Personales (dape), 
##Medidas Cautelares (meca), Audiencia de Revisi�n de Medidas (arme) y Ampliaci�n Medidas de Protecci�n (ampr). 
##Estas pueden ser aceptadas o rechazadas por el tribunal. 



##EJERCICIO 1. Las mp tienen los nombres de las personas a las cuales est�n asociadas, 
##realice una funci�n que cuente cu�ntos ni�os hay por cada una, y entregue una estad�stica de cu�ntos ni�os hay por mp. 

##Ejemplo: 
  ##Se cuentan con 5 mp de 2 ni�os Se cuentan con 4 mp de 1 ni�os Se cuentan con 2 mp de 3 ni�os 

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
TotalNi�os<-0

for (Totales in Lista) {
  if (Totales[1]== "mp"){
    TotalMP <- TotalMP+1
    Ni�os <- length(Totales)-1
    Informacion <- c("MP", TotalMP, "tiene", Ni�os, "ni�os") 
    print(Informacion)
    TotalNi�os <- TotalNi�os + length (Totales) -1
  }
}

print(paste("Hay", TotalMP, "MP con", TotalNi�os, "Ni�os en total"))

Promedio <- print(paste("Hay en promedio", TotalNi�os/TotalMP, "ni�os por cada MP"))



##EJERCICIO 2. Los oficios est�n compuestos por el c�digo al cual pertenecen, construya una funci�n que almacene los c�digos y las tem�ticas a las que est�n asociadas. 

##Ejemplo: 
  ##av01, ante, arme, ampr av02, arme av03, dape, ampr 

  
##EJERCICIO 3. Construya una funci�n que act�e de juez y retorne aprobada o reprobada para los diferentes oficios, y entregue la cantidad que hay de cada una. 

##Ejemplo: 
  ##Llegaron 10 oficios de los cuales: 7 son aprobados y 3 reprobados 


List <- list() #Es en donde se va a almacenar la informaci�n del ejercicio 2
Contador<-0 # variable para controlar el crecimiento de la matriz List
Bandera<-0 #variable para la igualdad
Bandera2<-0 #variable para el control de la matriz List

for(i in Lista) { 
  if (i[1] == "of") { 
    if(Contador==0){  #Se almacena el primer dato
      Contador<-Contador+1  #Inicio del contador 
      List[Contador] <- list(c(i[2], i[3])) #Se almacena la primera variable 
      if (1==rbinom(1,1,0.5)){ #Rbinom genera 1 o 0 de forma aleatoria, 1 aprobado, 2 reprobado
        Dictado<-"Aprobado" 
      }else{
        Dictado<-"Reprobado"
      }
      Juez <-(c("El Juez dicta como ", Dictado, " el oficio:", i[2] )) 
      print(Juez)
    }else{
      for (j in List){ 
        Bandera2<-Bandera2+1 #Se empieza a contar 
        if (i[2] == j[1]){ #Se pregunta si av01, etc. es igual a lo ya almacenado, si no, se almacena todo
          Bandera<-Bandera2 #Se guarda el contador anterior
          variable<-j #Se almacena la variable completa
        }
      }
      if(Bandera==0){
        Contador<-Contador+1 
        List[Contador] <- list(c(i[2], i[3])) #Se almacenan nuevas variables
        if (1==rbinom(1,1,0.5)){
          Dictado <-"Aprobado" 
        }else{
          Dictado <-"Reprobado" 
        }
        Juez <-(c("El Juez dicta como ", Dictado, " el oficio:", i[2] ))
        print(Juez)
      }else{ #Si las comparaciones son iguales se guarda la tem�tica
        List[Bandera] <- list(c(variable, i[3]))
        
      }
      Bandera<-0 #Se reinician las variables
      Bandera2<-0 
    }
  }
  
}

print(List) 












##Matriz de Crecimiento: Almacena toda la informaci�n que se le vaya asignando, no tiene l�mites, 
##el algoritmo se encarga de revisar la informaci�n ah� contenida para no repetirla

##Rbinom: Se encarga de generar 2 n�meros, 1 o 0.

##Las variables se reinician  para mantener el orden y el crecimiento en la matriz.