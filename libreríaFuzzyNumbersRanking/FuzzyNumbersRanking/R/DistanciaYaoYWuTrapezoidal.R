
#Metodo para calcular el indice de la distancia de Yao y Wu para numeros difuso trapezoidales
DistanciaYaoYWuTrapezoidal=function(a,b,c,d,w=NULL){
  #no hay omega
  if(is.null(w)){
    #valores correctos
    if(a<b && b<=c && c<d){

      #Funciones L y R inversas
      Linversa=function(x)(a+((b-a)*x));
      Rinversa=function(x)(d-(d-c)*x);

      #integrales
      n1=integrate(Linversa,0,1);
      n2=integrate(Rinversa,0,1);

      #indice
      valor<- 0.5*(n1$value + n2$value);
      #devuelve el indice
      return(valor)

    }else{#valores incorrectos
      cat(paste("Error: Los valores a,b,c,d no estan en orden"))
    }

  }else{#hay omega
    #parametros correctos
    if(w >0 && w <=1 && a<b && b<=c && c<d){

      #funciones L y R inversas
      Linversa=function(x)(a+((b-a)*x/w));
      Rinversa=function(x)(d-(d-c)*x/w);

      #integral
      n1=integrate(Linversa,0,1);
      n2=integrate(Rinversa,0,1);

      #indice
      valor<- 0.5*(n1$value + n2$value)
      #devuelve el indice
      return(valor)

    }else{#parametros incorrectos
      cat(paste("Error: parametros incorrectos"))
    }

  }

}

