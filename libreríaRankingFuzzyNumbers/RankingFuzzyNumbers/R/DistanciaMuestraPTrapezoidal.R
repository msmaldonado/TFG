
#Metodo de la distancia a la muestra p para un numero difuso trapezoidal
DistanciaMuestraPTrapezoidal=function(a,b,c,d,p,w=NULL){

  #Si no hay omega
  if(is.null(w)){
    #comprobamos que todos los parametros sean correctos
    if(a<=b && b<=c && c<=d ){
      #valor de p correcto
      if(p>0){

        #Definimos las funciones inversas en valor absoluto
        LinversaA=function(x)(abs(a+(b-a)*x))^p;
        RinversaA=function(x)(abs(d-(d-c)*x))^p;
        #funcion interna de la integral
        fsumaA = function(x)LinversaA(x)+RinversaA(x);

        #calculamos la integral y obtenemos el valor
        ra=integrate(fsumaA,0,1);
        vintegral<-ra$value;

        #Calculamos el signo
        Linversa=function(x)(a+(b-a)*x);
        Rinversa=function(x)(d-(d-c)*x);
        fsuma = function(x)Linversa(x)+Rinversa(x);
        signo =integrate(fsuma,0,1);
        vsigno<-signo$value;

        if(vsigno>=0){
          #devolvemos el valor del indice
          return(vintegral^(1/p))
        }else{
          #devolvemos el valor del indice cambiado de signo
          return(-(vintegral^(1/p)))
        }




      }else{#error en p
        cat(paste("Error: p debe ser positivo"))
      }

    }else{#error en el orden de numeros
      cat(paste("Error: los valores a,b,c,d no son correctos"))
    }


  }else{#tenemos omega
    #parametros correctos
    if(w >0 && w <=1 && a<=b && b<=c && c<=d){
      #Si p es correcto
      if(p>0){

        #Definimos las funciones inversas en valor absoluto
        LinversaA=function(x)(abs(a+(b-a)*x/w))^p;
        RinversaA=function(x)(abs(d-(d-c)*x/w))^p;

        #funcion interna de la integral
        fsumaA = function(x)LinversaA(x)+RinversaA(x);

        #calculamos la integral y obtenemos el valor
        ra=integrate(fsumaA,0,1);
        vintegral<-ra$value;

        #Calculamos el signo
        Linversa=function(x)(a+(b-a)*x/w);
        Rinversa=function(x)(d-(d-c)*x/w);
        fsuma = function(x)Linversa(x)+Rinversa(x);
        signo =integrate(fsuma,0,1);
        vsigno<-signo$value;

        if(vsigno>=0){
          #devolvemos el valor del indice
          return(vintegral^(1/p))
        }else{
          #devolvemos el valor del indice cambiado de signo
          return(-(vintegral^(1/p)))
        }

      }else{#P es incorrecto
        cat(paste("Error:  valor de p incorrecto "))
      }

    }else{#parametros incorrectos
      cat(paste("Error:  parametros incorrectos "))
    }

  }

}







