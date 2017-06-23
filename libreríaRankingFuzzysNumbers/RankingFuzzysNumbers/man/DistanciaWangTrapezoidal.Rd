\name{DistanciaWangTrapezoidal}
\alias{DistanciaWangTrapezoidal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{indice del metodo de Wang}
\description{
Funcion que calcula el indice del metodo de la distancia de Wang para un numero difuso trapezoidal
}
\usage{
DistanciaWangTrapezoidal(a,b,c,d,w=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{a}{Extremo inferior del soporte }
\item{b}{Extremo inferior del nucleo }
\item{c}{Extremo superior del nucleo }
\item{d}{Extremo superior del soporte }
\item{w}{altura en el eje y del numero difuso. \code{w} por defecto vale 1 }
}
\details{
Los valores a,b,c,d tienen que estar ordenados en forma creciente, w mayor que cero y menor que 1. Por defecto vale 1
}
\value{
Devuelve el valor del indice
}
\references{}
\author{msmaldonado}
\note{}
\seealso{}
\examples{
DistanciaWangTrapezoidal(0.4,0.5,0.5,1,1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{NumerosDifusosTrapezoidales,DistanciaCheng}
