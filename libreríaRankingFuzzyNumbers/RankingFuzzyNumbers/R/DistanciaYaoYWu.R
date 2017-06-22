
#metodo para calcular el indice de la distancia de Yao y Wu
DistanciaYaoYWu=function(a,b,c,d,w=NULL,L=NULL,R=NULL,LI=NULL,RI=NULL){
  sympyStart()
  #Introducimos un numero trapezoidal
  if(is.null(w) && is.null(L) && is.null(R)&& is.null(LI) && is.null(RI)){
    DistanciaYaoYWuTrapezoidal(a,b,c,d)

  }else if(!is.null(w) && !is.null(L) && !is.null(R)&& !is.null(LI) && !is.null(RI)){#Ponemos todas las funciones
    if(a<b && b<=c && c<d && w>0 && w<=1){

      #variables y funciones de Sympy
      sympy("x = Symbol('x',positive = True, real= True)")#X como variable
      #Ponemos las funciones
      sympy(paste("L =",L))
      sympy(paste("R =",R))
      sympy(paste("LI =",LI))
      sympy(paste("RI =",RI))

      #comprobamos que las funciones L y R estan bien definidas
      if(sympy(paste("L.subs(x,",a,")"))==0 && sympy(paste("R.subs(x,",d,")"))==0  && sympy(paste("L.subs(x,",b,")"))==w && sympy(paste("R.subs(x,",c,")"))==w ) {
        #Comprobamos que las funciones LI y RI son las inversas
        if(sympy("LI.subs({'x':L}) == L.subs({'x':LI})")=="True" && sympy("RI.subs({'x':R}) == R.subs({'x':RI})")=="True"){#la composicion es correcta

          #calculamos la integral
          sympy(paste("valorI=integrate(LI+RI, (x, ",0,",",1,"))"))
          sympy("indice=0.5*valorI")
          #devolvemos el indice con float
          sympy("float(indice)")

        }else{#las inversas no se corresponden
          cat(paste("Error: Las inversas no corresponden con las funciones"))
        }
      }else{#Si las funciones no estan bien definidas
        cat(paste("Error: Funciones mal definidas"))
      }
    }

  }else if(!is.null(w) && is.null(L) && is.null(R)&& is.null(LI) && is.null(RI)){#Si es un numero trapezoidal con omega
    DistanciaYaoYWuTrapezoidal(a,b,c,d,w)
  }else{#hemos introducido mal los parametros
    cat(paste("Error: Lectura incorrecta "))
  }
}
