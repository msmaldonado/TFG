
#Metodo de la distancia muestra p para cualquier numero difuso
DistanciaMuestraP=function(a,b,c,d,p,w=NULL,L=NULL,R=NULL,LI=NULL,RI=NULL){
  sympyStart()
  #Si es un numero difuso trapezoidal
  if(is.null(w) && is.null(L) && is.null(R)&& is.null(LI) && is.null(RI)){
    DistanciaMuestraPTrapezoidal(a,b,c,d,p)

  }else if(!is.null(w) && !is.null(L) && !is.null(R)&& !is.null(LI) && !is.null(RI)){#si se introducen todos los parametros
    if(a<b && b<=c && c<d && w>0 && w<=1 && p>0){
      #Variables y funciones para Sympy
      sympy("x = Symbol('x',positive = True, real= True)")
      sympy(paste("L =",L))
      sympy(paste("R =",R))
      sympy(paste("LI =",LI))
      sympy(paste("RI =",RI))

      #comprobamos que L y R estan bien definidos
      if(sympy(paste("L.subs(x,",a,")"))==0 && sympy(paste("R.subs(x,",d,")"))==0  && sympy(paste("L.subs(x,",b,")"))==w && sympy(paste("R.subs(x,",c,")"))==w ) {
        #Comprobamos que las funciones LI y RI son las inversas
        if(sympy("LI.subs({'x':L}) == L.subs({'x':LI})")=="True" && sympy("RI.subs({'x':R}) == R.subs({'x':RI})")=="True"){#Composicion

          #Calculamos la integral
          sympy(paste("valorI=integrate(abs(LI**",p,") + abs(RI**",p,"), (x, ",0,",",1,"))"))


          #Realizamos la divison con Rational para que salga exacto
          sympy("R=Rational")
          #obtenemos la potencia
          sympy(paste("potencia = float(R(1,",p,"))"))

          sympy("indice=valorI**potencia")
          #miramos el signo
          if(sympy("integrate(LI + RI,(x,0,1))")){
            #indice con float
            sympy("float(indice)")
          }else{
            #indice con float
            sympy("float(-indice)")
          }



        }else{
          cat(paste("Error: Las inversas no corresponden con las funciones"))
        }
      }else{
        cat(paste("Error: Las funciones L y R no estan bien definidas"))
      }
    }else{
      cat(paste("Error: parametros"))
    }

  }else if(!is.null(w) && is.null(L) && is.null(R)&& is.null(LI) && is.null(RI)){#Numero difuso trapezoidal con omega
    DistanciaMuestraPTrapezoidal(a,b,c,d,p,w)
  }else{#error al introducir variables
    cat(paste("Error: Lectura incorrecta "))
  }
}

