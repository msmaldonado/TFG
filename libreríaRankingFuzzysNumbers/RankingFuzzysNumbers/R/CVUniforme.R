
#Coeficiente de variacion uniforme para cualquier tipo de numero difuso
CVUniforme=function(a,b,c,d,w=NULL,L=NULL,R=NULL,LI=NULL,RI=NULL){
  #Comprobamos que SymPy este instalado y lo cargamos en memoria
  if (!require('rSymPy'))
    stop("Debe instalar el paquete 'rSympy' para continuar")
  sympyStart()
  #Si el numero difuso es trapezoidal
  if(is.null(w) && is.null(L) && is.null(R)&& is.null(LI) && is.null(RI)){
    CVUniformeTrapezoidal(a,b,c,d)

  }else if(!is.null(w) && !is.null(L) && !is.null(R)&& !is.null(LI) && !is.null(RI)){#Si introducimos todos los parametros
    #Comprobramos parametros correctos
    if(a<b && b<=c && c<d && w>0 && w<=1){
      sympy("x = Symbol('x',positive = True, real= True)")
      sympy(paste("L =",L))
      sympy(paste("R =",R))
      sympy(paste("LI =",LI))
      sympy(paste("RI =",RI))

      #Comprobamos que L y R esten bien definidas
      if(sympy(paste("L.subs(x,",a,")"))==0 && sympy(paste("R.subs(x,",d,")"))==0  && sympy(paste("L.subs(x,",b,")"))==w && sympy(paste("R.subs(x,",c,")"))==w ) {
        #Comprobamos que las funciones LI y RI son las inversas
        if(sympy("LI.subs({'x':L}) == L.subs({'x':LI})")=="True" && sympy("RI.subs({'x':R}) == R.subs({'x':RI})")=="True"){#Composicion

          #Calculamos las integrales para  la media
          sympy(paste("primeraI=integrate(L*x, (x, ",a,",",b,"))"))
          sympy(paste("segundaI=integrate(x, (x, ",b,",",c,"))"))
          sympy(paste("terceraI=integrate(R*x, (x, ",c,",",d,"))"))
          sympy(paste("fprimeraI=integrate(L, (x, ",a,",",b,"))"))
          sympy(paste("fsegundaI=integrate(1, (x, ",b,",",c,"))"))
          sympy(paste("fterceraI=integrate(R, (x, ",c,",",d,"))"))

          #valor de la media
          sympy("media = ((primeraI + segundaI + terceraI)/(fprimeraI + fsegundaI + fterceraI))")

          #Claculamos las integrales para desviacion
          sympy(paste("primeraI=integrate(L*x*x, (x, ",a,",",b,"))"))
          sympy(paste("segundaI=integrate(x**2, (x, ",b,",",c,"))"))
          sympy(paste("terceraI=integrate(R*x*x, (x, ",c,",",d,"))"))

          sympy("G1 = (((primeraI + segundaI + terceraI)/(fprimeraI + fsegundaI + fterceraI))-(media*media))")
          sympy("desviacion=float(sqrt(G1))")

          #indice
          sympy("float(desviacion/media)")


        }else{
          cat(paste("Error: Las inversas no corresponden con las funciones"))
        }
      }else{
        cat(paste("Error: Las funciones L y R no estan bien definidas"))
      }
    }

  }else if(!is.null(w) && is.null(L) && is.null(R)&& is.null(LI) && is.null(RI)){#numero trapezoidal con omega
    CVUniformeTrapezoidal(a,b,c,d,w)
  }else{
    cat(paste("Error: Lectura incorrecta "))
  }
}
