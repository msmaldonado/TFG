
#Metodo que calcula el indice de la magnitud para cualquier numero difuso
Magnitud=function(a,b,c,d,w=NULL,L=NULL,R=NULL,LI=NULL,RI=NULL){
  #Comprobamos que SymPy este instalado y lo cargamos en memoria
  if (!require('rSymPy'))
    stop("Debe instalar el paquete 'rSympy' para continuar")
  sympyStart()
  #Si introducimos un numero trapezoidal
  if(is.null(w) && is.null(L) && is.null(R)&& is.null(LI) && is.null(RI)){
    MagnitudTrapezoidal(a,b,c,d)

  }else if(!is.null(w) && !is.null(L) && !is.null(R)&& !is.null(LI) && !is.null(RI)){
    #parametros correctos
    if(a<b && b<=c && c<d && w>0 && w<=1){

      #Funciones y variables de Sympy
      sympy("x = Symbol('x',positive = True, real= True)")
      sympy(paste("L =",L))
      sympy(paste("R =",R))
      sympy(paste("LI =",LI))
      sympy(paste("RI =",RI))

      #comprobamos que las funciones L y R estan bien definidas
      if(sympy(paste("L.subs(x,",a,")"))==0 && sympy(paste("R.subs(x,",d,")"))==0  && sympy(paste("L.subs(x,",b,")"))==w && sympy(paste("R.subs(x,",c,")"))==w ) {
        #Comprobamos que las funciones LI y RI son las inversas
        if(sympy("LI.subs({'x':L}) == L.subs({'x':LI})")=="True" && sympy("RI.subs({'x':R}) == R.subs({'x':RI})")=="True"){#la composicion es correcta

          #calculamos la integral
          sympy(paste("valorI=integrate((LI+RI+",b,"+",c,")*x, (x, ",0,",",1,"))"))
          sympy("indice=0.5*valorI")
          #valor del indice con float
          sympy("float(indice)")

        }else{#las inversas no se corresponden
          cat(paste("Error: Las inversas no corresponden con las funciones"))
        }
      }else{#Si las funciones no estan bien definidas
        cat(paste("Error: Funciones mal definidas"))
      }
    }

  }else if(!is.null(w) && is.null(L) && is.null(R)&& is.null(LI) && is.null(RI)){#Si es un numero trapezoidal con w
    MagnitudTrapezoidal(a,b,c,d,w)
  }else{#hemos introducido mal los parametros
    cat(paste("Error: Lectura incorrecta "))
  }
}
