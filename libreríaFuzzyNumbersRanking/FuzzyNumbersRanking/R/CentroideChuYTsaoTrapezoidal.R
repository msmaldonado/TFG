
CentroideChuYTsaoTrapezoidal=function(a,b,c,d, w=NULL){
  #no se introduce w
  if(is.null(w)){
    #Comprobamos que los numeros son correctos
    if(a<b && b<=c && c<d){

      #Definimos funciones del x0
      #Funciones L y R
      L=function(x)((x-a)/(b-a));
      R=function(x)((d-x)/(d-c));
      Lx=function(x)(x*(x-a)/(b-a));
      Rx=function(x)(x*(d-x)/(d-c));
      Linear=function(x)(x);
      #Funciones L y R inversas
      Linversa=function(x)(a+((b-a)*x));
      Rinversa=function(x)(d-(d-c)*x);
      Lxinversa=function(x)(x*(a+(b-a)*x))
      Rxinversa=function(x)(x*(d-(d-c)*x));

      #integrales
      #numerador
      n1=integrate(Lx,a,b);
      n2=integrate(Linear,b,c);
      n3=integrate(Rx,c,d);
      #denominador
      d1=integrate(L,a,b);
      d2=c-b;
      d3=integrate(R,c,d);

      #valor del x0
      x0<-(n1$value + n2$value + n3$value)/(d1$value + d2 + d3$value)


      #calcular y0

      #integrrales
      #numerador
      m1=integrate(Rxinversa,0,1);
      m2=integrate(Lxinversa,0,1);
      #denominador
      f1=integrate(Rinversa,0,1);
      f2=integrate(Linversa,0,1);

      #valor del y0
      y0<-(m1$value + m2$value)/(f1$value + f2$value)

      #valor del indice indice
      return(x0*y0)

    }else{#numeros incorrectos
      cat(paste("Error:  los valores a,b,c,d no estan en orden"))
    }


  }else{# Si introducimos omega
    #Comprobamos que los parametros sean correctos
    if(w >0 && w <=1 && a<b && b<=c && c<d){

      #Definimos funciones del x0
      #Funciones L y R
      L=function(x)(w*(x-a)/(b-a));
      R=function(x)(w*(d-x)/(d-c));
      Lx=function(x)(x*w*(x-a)/(b-a));
      Rx=function(x)(x*w*(d-x)/(d-c));
      Linear=function(x)(x);
      #Funciones L y R inversas
      Linversa=function(x)(a+((b-a)*x/w));
      Rinversa=function(x)(d-(d-c)*x/w);
      Lxinversa=function(x)(x*(a+(b-a)*x/w))
      Rxinversa=function(x)(x*(d-(d-c)*x/w));

      #integrales

      #numerador
      n1=integrate(Lx,a,b);
      n2=integrate(Linear,b,c);
      n3=integrate(Rx,c,d);
      #denominador
      d1=integrate(L,a,b);
      d2=c-b;
      d3=integrate(R,c,d);

      #valor del x0
      x0<-(n1$value + n2$value + n3$value)/(d1$value + d2 + d3$value)


      #calcular y0

      #integrales
      #numerador
      m1=integrate(Rxinversa,0,w);
      m2=integrate(Lxinversa,0,w);
      #denominador
      f1=integrate(Rinversa,0,w);
      f2=integrate(Linversa,0,w);

      #valor del y0
      y0<-(m1$value + m2$value)/(f1$value + f2$value)

      #valor del indice
      return(x0*y0)

    }else{#parametros incorrectos
      cat(paste("Error:  parametros incorrectos "))
    }
  }
}
