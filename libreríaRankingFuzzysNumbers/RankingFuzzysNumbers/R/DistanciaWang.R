#Metodo que calcula indice de la distancia de Wang para cualquier numero difuso
DistanciaWang=function(a,b,c,d,w=NULL,L=NULL,R=NULL,LI=NULL,RI=NULL){
  #Comprobamos que SymPy este instalado y lo cargamos en memoria
  if (!require('rSymPy'))
    stop("Debe instalar el paquete 'rSympy' para continuar")
  sympyStart()
  #Si es un numero difuso trapezoidal
  if(is.null(w) && is.null(L) && is.null(R)&& is.null(LI) && is.null(RI)){
    DistanciaWangTrapezoidal(a,b,c,d)

  }else if(!is.null(w) && !is.null(L) && !is.null(R)&& !is.null(LI) && !is.null(RI)){
    if(a<b && b<=c && c<d && w>0 && w<=1){
      #Funciones y variables para Rsympy
      sympy("x = Symbol('x',positive = True, real= True)")
      sympy(paste("L =",L))
      sympy(paste("R =",R))
      sympy(paste("LI =",LI))
      sympy(paste("RI =",RI))

      #comprobamos que L y R esten bien definidas
      if(sympy(paste("L.subs(x,",a,")"))==0 && sympy(paste("R.subs(x,",d,")"))==0  && sympy(paste("L.subs(x,",b,")"))==w && sympy(paste("R.subs(x,",c,")"))==w ) {
        #Comprobamos que las funciones LI y RI son las inversas
        if(sympy("LI.subs({'x':L}) == L.subs({'x':LI})")=="True" && sympy("RI.subs({'x':R}) == R.subs({'x':RI})")=="True"){#composicion

          #Calculamos las integrales para  x0
          sympy(paste("primeraI=integrate(L*x, (x, ",a,",",b,"))"))
          sympy(paste("segundaI=integrate(x, (x, ",b,",",c,"))"))
          sympy(paste("terceraI=integrate(R*x, (x, ",c,",",d,"))"))
          sympy(paste("fprimeraI=integrate(L, (x, ",a,",",b,"))"))
          sympy(paste("fsegundaI=integrate(1, (x, ",b,",",c,"))"))
          sympy(paste("fterceraI=integrate(R, (x, ",c,",",d,"))"))

          #valor del x0
          sympy("x0 = ((primeraI + segundaI + terceraI)/(fprimeraI + fsegundaI + fterceraI))")

          #Claculamos las integrales para y0
          sympy(paste("yprimeraI=integrate(x*RI, (x, ",0,",",w,"))"))
          sympy(paste("ysegundaI=integrate(x*LI, (x, ",0,",",w,"))"))
          sympy(paste("yfprimeraI=integrate(RI, (x, ",0,",",w,"))"))
          sympy(paste("yfsegundaI=integrate(LI, (x, ",0,",",w,"))"))

          #valor del y0
          sympy("y0 = ((yprimeraI - ysegundaI )/(yfprimeraI - yfsegundaI ))")

          #indice
          sympy("float(sqrt(x0*x0 + y0*y0))")

        }else{
          cat(paste("Error: Las inversas no corresponden con las funciones"))
        }
      }else{
        cat(paste("Error: valores iniciales no corresponden con valores iniciales de las funciones"))
      }
    }else{
      cat(paste("Error: parametros"))
    }

  }else if(!is.null(w) && is.null(L) && is.null(R)&& is.null(LI) && is.null(RI)){#si es un numero trapezoidal con omega
    DistanciaWangTrapezoidal(a,b,c,d,w)
  }else{
    cat(paste("Error: Lectura incorrecta "))
  }
}

