
#Coeficiente de Variacion para numeros trapezoidales
CVProporcionalTrapezoidal=function(a,b,c,d,w=NULL){

  #Si no hay omega
  if(is.null(w)){
    #parametros correctos
    if(a<b && b<=c && c<d){

      #funciones para usar en las integrales
      L=function(x)(((x-a)/(b-a))^2);
      R=function(x)(((d-x)/(d-c))^2);
      Lx=function(x)(x*((x-a)/(b-a))^2);
      Rx=function(x)(x*((d-x)/(d-c))^2);
      Linear=function(x)(x);

      #calculamos la integrales

      #numeradores
      n1=integrate(Lx,a,b);
      n2=integrate(Linear,b,c);
      n3=integrate(Rx,c,d);

      #denominadores
      d1=integrate(L,a,b);
      d2=c-b;
      d3=integrate(R,c,d);

      #valor de media
      m<-(n1$value + n2$value + n3$value)/(d1$value + d2 + d3$value)

      #calcular G
      #definimos funciones
      Lx=function(x)(x^2*((x-a)/(b-a))^2);
      Rx=function(x)(x^2*((d-x)/(d-c))^2);
      Linearc=function(x)(x*x);

      #numerador
      n1=integrate(Lx,a,b);
      n2=integrate(Linearc,b,c);
      n3=integrate(Rx,c,d);

      #denominador
      d1=integrate(L,a,b);
      d2=c-b;
      d3=integrate(R,c,d);

      G1<-(n1$value + n2$value + n3$value)/(d1$value + d2 + d3$value)

      #desviacion
      G<-(sqrt(G1 - m^2))

      #coeficiente
      return(G/m)

    }else{#parametros incorrectos
      cat(paste("Error:  los valores a,b,c,d no estan en orden"))
    }

  }else{#hay omega

    #Comprobamos si los parametros son correctos
    if(w >0 && w <=1 && a<b && b<=c && c<d){

      #funciones para usar en las integrales
      L=function(x)(((x-a)*w/(b-a))^2);
      R=function(x)(((d-x)*w/(d-c))^2);
      Lx=function(x)(x*((x-a)*w/(b-a))^2);
      Rx=function(x)(x*((d-x)*w/(d-c))^2);
      Linear=function(x)(x);

      #calculamos la integrales

      #numeradores
      n1=integrate(Lx,a,b);
      n2=integrate(Linear,b,c);
      n3=integrate(Rx,c,d);

      #denominadores
      d1=integrate(L,a,b);
      d2=c-b;
      d3=integrate(R,c,d);

      #valor de media
      m<-(n1$value + n2$value + n3$value)/(d1$value + d2 + d3$value)

      #calcular G
      #definimos funciones
      Lx=function(x)(x^2*((x-a)*w/(b-a))^2);
      Rx=function(x)(x^2*((d-x)*w/(d-c))^2);
      Linearc=function(x)(x*x);

      #numerador
      n1=integrate(Lx,a,b);
      n2=integrate(Linearc,b,c);
      n3=integrate(Rx,c,d);

      #denominador
      d1=integrate(L,a,b);
      d2=c-b;
      d3=integrate(R,c,d);

      G1<-(n1$value + n2$value + n3$value)/(d1$value + d2 + d3$value)

      #desviacion
      G<-(sqrt(G1 - m^2))

      #coeficiente
      return(G/m)


    }else{#parametros incorrectos
      cat(paste("Error:  parametros incorrectos "))

    }
  }

}
