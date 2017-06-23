
#Funcion que compara varios numeros trapezoidales segun el metodo seleccionado
ComparaNumeros=function(){
  cat(paste("Introduce la cantidad de numeros difusos trapezoidales para hacer ranking"))
  cantidadNumeros<-scan(n=1)
  #si hay una cantidad positiva
  if(cantidadNumeros>0){
    #inicialimos los vectores que usaremos para los parametros  a null
    a=NULL
    b=NULL
    c=NULL
    d=NULL

    cat(paste("\nCada numero difuso trapezoidal se introduce como (a,b,c,d), si es triangular, b=c\n"))

    #introduce los numeros difusos
    for(i in 1:cantidadNumeros){
      cat(paste("\nEl numero difuso trapezoidal",i,"es :\n"))#cada valor del numero difusos lo guardamos en un vector
      num=scan(nmax=4)
      #Comprobamos que los valores son correctos
      if(num[1]<num[2] && num[2]<= num[3] && num[3] < num[4]){
        a=c(a,num[1])#contiene todas las a de los numeros difusos
        b=c(b,num[2])
        c=c(c,num[3])
        d=c(d,num[4])
      }else{#si algun valor es incorrecto que pare de introducir numeros
        cat(paste("valores incorrectos"))
        return(0)
      }

    }
    #Siempre este activo hasta que el usuario lo detenga
    while(TRUE){
      indice=NULL
      vectorConjunto=NULL
      vectorCoincidencias=NULL
      #elegir un metodo
      cat(paste("\nElige el metodo para realizar ranking:
                1- metodo centroide Chu y Tsao
                2- metodo centroide Wang
                3- metodo coeficiente variacional proporcional
                4- metodo coeficiente variacional uniforme
                5- metodo distancia muestra p
                6- metodo de la distancia Wang
                7- metodo de la distancia Yaou y Wu
                8- metodo de la magnitud
                Para salir pulse cualquier otra tecla "))
      metodo<-scan(n=1)

      #booleano usado por si usamos el metodo de los coeficientes de variacion
      OrdenacionNormal=TRUE

      #para el metodo seleccionado obtener el vector con los indices de los numeros trapezoidales
      if(metodo ==1){#metodo del centroide Chu y Tsao
        for(i in 1:cantidadNumeros){
          indice=c(indice,CentroideChuYTsaoTrapezoidal(a[i],b[i],c[i],d[i]))
        }

      }else if(metodo == 2){#metodo del centroide Wang
        for(i in 1:cantidadNumeros){
          indice=c(indice,CentroideWangTrapezoidal(a[i],b[i],c[i],d[i]))
        }

      }else if(metodo == 3){#metodo coeficiente proporcional
        for(i in 1:cantidadNumeros){
          indice=c(indice,CVProporcionalTrapezoidal(a[i],b[i],c[i],d[i]))
          #activamos el booleano
          OrdenacionNormal=FALSE
        }

      }else if(metodo == 4){#metodo coeficiente uniforme
        for(i in 1:cantidadNumeros){
          indice=c(indice,CVUniformeTrapezoidal(a[i],b[i],c[i],d[i]))
          #activamos el booleano
          OrdenacionNormal=FALSE
        }

      }else if(metodo ==5){#metodo muestra p
        cat(paste("Introduce el valor de p positivo"))
        p<-scan(n=1)
        if(p>0){
          for(i in 1:cantidadNumeros){
            indice=c(indice,DistanciaMuestraPTrapezoidal(a[i],b[i],c[i],d[i],p))
          }

        }else{#valor incorrecto de p
          cat(paste("valor de p incorrecto"))
        }
      }else if(metodo == 6){#metodo de distancia de Wang
        for(i in 1:cantidadNumeros){
          indice=c(indice,DistanciaWangTrapezoidal(a[i],b[i],c[i],d[i]))
        }

      }else if(metodo == 7){#metodo distancia Yao y Wu
        for(i in 1:cantidadNumeros){
          indice=c(indice,DistanciaYaoYWuTrapezoidal(a[i],b[i],c[i],d[i]))
        }

      }else if(metodo == 8){#metodo magnitud
        for(i in 1:cantidadNumeros){
          indice=c(indice,MagnitudTrapezoidal(a[i],b[i],c[i],d[i]))
        }
      }else {
        cat(paste("Saliendo......"))
        return(0)
      }

      cat(paste("\n El valor del indice para cada numero\n"))
      for(i in 1:cantidadNumeros){
        cat(paste(i," = ",indice[i],"\n"))
      }

      #ordenamos el vector de indices de menor a mayor los indices
      indiceOrdenado = sort(indice)

      #Obtengo el vector conjunto con los numeros difusos ordenados, teniendo en cuenta repeticiones
      for(i in 1:cantidadNumeros){
        #si los indice actual y anterior no coinciden busca la posicion
        if(i>=2 && indiceOrdenado[i-1] != indiceOrdenado[i]){
          for(t in 1:cantidadNumeros){
            #busco la posicion que coincide y lo pongo en el vector la posicion
            if(indiceOrdenado[i]==indice[t]){
              vectorConjunto =c(vectorConjunto,t)

            }
          }

        }else if(i==1){#para el primer caso, solo busco la posicion que coincide
          for(t in 1:cantidadNumeros){
            if(indiceOrdenado[i]==indice[t]){
              vectorConjunto =c(vectorConjunto,t)

            }
          }

        }else{#las posicion actual y anterior en el vector coinciden guardamos las posicion en el vector de coincidencias
          vectorCoincidencias=c(vectorCoincidencias,i)
        }
      }#for



      #Para imprimir, obtengo el orden y miramos el tipo de ordenacion
      #Busco para cada posicion si está en el vector de coincidencias, de ser así esa posicion es con igual. Si no hay repetidos, directamente sacar el orden
      if(OrdenacionNormal){
        cat(paste("\nLos numeros difusos ordenados son \n"))
        for(i in 1:cantidadNumeros){
          if(i==1){
            cat(paste(vectorConjunto[i]))#imprimo el primero
          }else{#para todas las posiciones menos la primera
            aux=FALSE
            #si hay coincidencias
            if(length(vectorCoincidencias)!=0){
              for(j in 1:length(vectorCoincidencias)){#busco si la posicion esta en coincidencias, por tanto el indice era el mismo
                if(i == vectorCoincidencias[j]){
                  aux=TRUE
                }
              }
            }

            if(aux){#si esa posicion era coincidencia imprimo con
              cat(paste(" = ",vectorConjunto[i]))
            }else{
              cat(paste(" < ",vectorConjunto[i]))
            }

          }#else

        }#for
      }else{
        cat(paste("\nLos numeros difusos ordenados son \n"))
        for(i in 1:cantidadNumeros){
          if(i==1){
            cat(paste(vectorConjunto[i]))#imprimo el primero
          }else{#para todas las posiciones menos la primera
            aux=FALSE
            if(length(vectorCoincidencias)!=0){
              for(j in 1:length(vectorCoincidencias)){#busco si la posicion esta en coincidencias, por tanto el indice era el mismo
                if(i == vectorCoincidencias[j]){
                  aux=TRUE
                }
              }
            }
            if(aux){#si esa posicion era coincidencia imprimo con =
              cat(paste(" = ",vectorConjunto[i]))
            }else{
              cat(paste(" > ",vectorConjunto[i]))
            }

          }#else

        }#for
      }
      cat(paste(" \n"))

    }



  }else{#tiene que introducir al menos un numero
    cat(paste("Tiene que introducir al menos un numero"))
  }

}


