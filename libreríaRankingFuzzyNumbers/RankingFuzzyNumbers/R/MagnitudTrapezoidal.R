
#metodo para calcular el indice de la magnitud de un numero difuso
MagnitudTrapezoidal=function(a,b,c,d,w=NULL){
  #no hay omega
  if(is.null(w)){
    #si los valores son correctos
    if(a<b && b<=c && c<d){

      #calculamos la funcion para integrar desarrollada
      integral=function(x)((a+(b-a)*x + d-(d-c)*x+b+c)*x);
      valor=integrate(integral,0,1);

      #valor de la magnitud
      magnitud<- (valor$value/2);

      return(magnitud)
    }else{
      cat(paste("Error: Los valores a,b,c,d no estan en orden"))
    }

  }else{#Tenemos valor de w
    #Si los parametros son correctos
    if(a<b && b<=c && c<d && w>0 && w<=1){
      #funcion desarrollada
      integral=function(x)((a*w+(b-a)*x + d*w-(d-c)*x+w*b+w*c)*x);
      #obtenemos el valor
      valor=integrate(integral,0,1);
      magnitud<- (valor$value/(2*w));
      #Devuelve el valor de la magnitud
      return(magnitud)

    }else{#Parametros incorrectos
      cat(paste("Error: parametros incorrectos"))
    }
  }
}


