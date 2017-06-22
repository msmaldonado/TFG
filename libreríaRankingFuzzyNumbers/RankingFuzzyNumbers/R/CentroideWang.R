
#Funcion para calcular el indice del centroide de Wang
CentroideWang=function(a,b,c,d,w=NULL,L=NULL,R=NULL,LI=NULL,RI=NULL){
  #iniciamos Sympy
  sympyStart()
  #Si hemos introducido un numero trapezoidal, llamamos al metodo del trapezoidal
  if(is.null(w) && is.null(L) && is.null(R)&& is.null(LI) && is.null(RI)){
    CentroideWangTrapezoidal(a,b,c,d)

  }else if(!is.null(w) && !is.null(L) && !is.null(R)&& !is.null(LI) && !is.null(RI)){#si hemos introducido todos los parametros
    #comprobamos que los numeros sean correctos
    if(a<b && b<=c && c<d && w>0 && w<=1){

      #declaramos las funciones en Sympy
      sympy("x = Symbol('x',positive = True, real= True)")#variable x
      sympy(paste("L =",L))
      sympy(paste("R =",R))
      sympy(paste("LI =",LI))
      sympy(paste("RI =",RI))

      #comprobamos que las funciones L y R estan bien definidas
      if(sympy(paste("L.subs(x,",a,")"))==0 && sympy(paste("R.subs(x,",d,")"))==0  && sympy(paste("L.subs(x,",b,")"))==w && sympy(paste("R.subs(x,",c,")"))==w ) {

        #Comprobamos que las funciones LI y RI son las inversas
        if(sympy("LI.subs({'x':L}) == L.subs({'x':LI})")=="True" && sympy("RI.subs({'x':R}) == R.subs({'x':RI})")=="True"){

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

          #valor del indice
          sympy("float(x0*y0)")

        }else{#si las funciones no eran las inversas
          cat(paste("Error: Las inversas no corresponden con las funciones"))
        }
      }else{#Si las funciones no estan bien definidas
        cat(paste("Error: Las funciones L y R no estan bien definidas"))
      }
    }

  }else if(!is.null(w) && is.null(L) && is.null(R)&& is.null(LI) && is.null(RI)){#Si introducimos un numero trapezoidal con w
    CentroideWangTrapezoidal(a,b,c,d,w)
  }else{#Parametros incorrectos o incompletos
    cat(paste("Error: Lectura incorrecta "))
  }
}
