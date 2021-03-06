\name{DistanciaMuestraPTrapezoidal}
\alias{DistanciaMuestraPTrapezoidal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Funcion que calcula el indice del metodo de la distancia de signo para numeros difusos trapezoidales .}
\description{
Funcion que calcula el indice del metodo de la distancia de signo para numeros difusos trapezoidales .
}
\usage{
DistanciaMuestraPTrapezoidal(a,b,c,d,p,w=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{a}{Extremo inferior del soporte }
\item{b}{Extremo inferior del nucleo }
\item{c}{Extremo superior del nucleo }
\item{d}{Extremo superior del soporte }
\item{p}{valor sobre el que se calcula el metodo \code{p} mayor que 0 }
\item{w}{altura en el eje y del numero difuso. \code{w} por defecto vale 1 }
}
\details{
Los valores a,b,c,d tienen que estar ordenados en forma creciente y p ser positivo.w mayor que cero y menor que 1. Por defecto vale 1
}
\value{
Devuelve el valor del indice para el p elegido
}
\references{}
\author{msmaldonado}
\note{}
\seealso{}
\examples{
DistanciaMuestraPTrapezoidal(0.4,0.5,0.5,1,2);
DistanciaMuestraPTrapezoidal(0.4,0.5,0.5,1,2,1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{NumerosDifusosTrapezoidales,DistanciaMuestraP}


