
#Coeficienta variacion proporcional para cualquier numero difuso
CVProporcional=function(a,b,c,d,w=NULL,L=NULL,R=NULL,LI=NULL,RI=NULL){
  #iniciamos Sympy
  sympyStart()

  #si introducimos un numero difuso trapezoidal
  if(is.null(w) && is.null(L) && is.null(R)&& is.null(LI) && is.null(RI)){
    CVProporcionalTrapezoidal(a,b,c,d)

  }else if(!is.null(w) && !is.null(L) && !is.null(R)&& !is.null(LI) && !is.null(RI)){#si es un numero con funciones L y R
    if(a<b && b<=c && c<d && w>0 && w<=1){
      #Definimos las funciones para usar en Sympy
      sympy("x = Symbol('x',positive = True, real= True)")
      sympy(paste("L =",L))
      sympy(paste("R =",R))
      sympy(paste("LI =",LI))
      sympy(paste("RI =",RI))

      #comprobamos que las funcioens L y R esten bien definidas
      if(sympy(paste("L.subs(x,",a,")"))==0 && sympy(paste("R.subs(x,",d,")"))==0  && sympy(paste("L.subs(x,",b,")"))==w && sympy(paste("R.subs(x,",c,")"))==w ) {
        #Comprobamos que las funciones LI y RI son las inversas
        if(sympy("LI.subs({'x':L}) == L.subs({'x':LI})")=="True" && sympy("RI.subs({'x':R}) == R.subs({'x':RI})")=="True"){#hacemos composicion

          #Calculamos las integrales para  la media
          sympy(paste("primeraI=integrate(L*L*x, (x, ",a,",",b,"))"))
          sympy(paste("segundaI=integrate(x, (x, ",b,",",c,"))"))
          sympy(paste("terceraI=integrate(R*R*x, (x, ",c,",",d,"))"))
          sympy(paste("fprimeraI=integrate(L*L, (x, ",a,",",b,"))"))
          sympy(paste("fsegundaI=integrate(1, (x, ",b,",",c,"))"))
          sympy(paste("fterceraI=integrate(R*R, (x, ",c,",",d,"))"))

          sympy("media = ((primeraI + segundaI + terceraI)/(fprimeraI + fsegundaI + fterceraI))")

          #Claculamos las integrales para desviacion
          sympy(paste("primeraI=integrate(L*L*x*x, (x, ",a,",",b,"))"))
          sympy(paste("segundaI=integrate(x**2, (x, ",b,",",c,"))"))
          sympy(paste("terceraI=integrate(R*R*x*x, (x, ",c,",",d,"))"))

          sympy("G1 = (((primeraI + segundaI + terceraI)/(fprimeraI + fsegundaI + fterceraI))-(media*media))")
          sympy("desviacion=float(sqrt(G1))")
          #valor del indice de variacion
          sympy("float(desviacion/media)")


        }else{#error en las inversas
          cat(paste("Error: Las inversas no corresponden con las funciones"))
        }
      }else{
        cat(paste("Error: Las funciones L y R estan mal definidas"))
      }
    }



  }else if(!is.null(w) && is.null(L) && is.null(R)&& is.null(LI) && is.null(RI)){#numero trapezoidal con omega
    CVProporcionalTrapezoidal(a,b,c,d,w)
  }else{
    cat(paste("Error: Lectura incorrecta "))
  }
}
