
#Coeficiente de variacion uniforme para numeros difusos trapezoidales
CVUniformeTrapezoidal=function(a,b,c,d,w=NULL){
  #no hay omega
  if(is.null(w)){
    #parametros correctos
    if(a<b && b<=c && c<d){

      #funciones para usar en las integrales
      L=function(x)((x-a)/(b-a));
      R=function(x)((d-x)/(d-c));
      Lx=function(x)(x*(x-a)/(b-a));
      Rx=function(x)(x*(d-x)/(d-c));
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
      L=function(x)(((x-a)/(b-a)));
      R=function(x)(((d-x)/(d-c)));
      Lx=function(x)(x^2*((x-a)/(b-a)));
      Rx=function(x)(x^2*((d-x)/(d-c)));
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

      G<-(sqrt(G1 - m^2)) # desviacion
      #coeficiente
      return(G/m)

    }else{#parametros incorrectos
      cat(paste("Error:  los valores a,b,c,d no estan en orden"))
    }

  }else{#hay omega
    #parametros correctos
    if(w >0 && w <=1 && a<b && b<=c && c<d){

      #funciones para usar en las integrales
      L=function(x)((x-a)/(b-a));
      R=function(x)((d-x)/(d-c));
      Lx=function(x)(x*(x-a)/(b-a));
      Rx=function(x)(x*(d-x)/(d-c));
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
      L=function(x)(((x-a)*w/(b-a)));
      R=function(x)(((d-x)*w/(d-c)));
      Lx=function(x)(x^2*((x-a)*w/(b-a)));
      Rx=function(x)(x^2*((d-x)*w/(d-c)));
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

      G<-(sqrt(G1 - m^2)) #desviacion
      #coeficiente
      return(G/m)

    }else{#parametros incorrectos
      cat(paste("Error:  parametros incorrectos "))

    }
  }

}
