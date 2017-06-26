
#Metodo de Chen  para un numero difuso trapezoidal
ChenTrapezoidal=function(a,b,c,d,xmin,xmax){
    #comprobamos que todos los parametros sean correctos
    if(a<=b && b<=c && c<=d && xmin<=a && xmax>=d){
      primero= (d-xmin)/(xmax-xmin-c+d)
      ultimo= (xmax-a)/(xmax-xmin+b-a)
      indice = 0.5*(primero +1-ultimo)
      return(indice)

    }else{#error 
      cat(paste("Error: los argumentos no son correctos"))
    }

}







