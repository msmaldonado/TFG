\name{CentroideChuYTsaoTrapezoidal}
\alias{CentroideChuYTsaoTrapezoidal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{funcion que calcula el indice del metodo del centroide de Chu y Tsao}
\description{
Funcion que calcula el indice del metodo del centroide de Chu y Tsao para numero difusos trapezoidales
}
\usage{
CentroideChuYTsaoTrapezoidal(a,b,c,d,w=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{a}{Primer valor de la funcion L }
\item{b}{Ultimo valor de la funcion L  }
\item{c}{primer valor de la funcion R }
\item{d}{ultimo valor de la funcion R }
\item{w}{altura en el eje y del numero difuso \code{d} por defecto vale 1 }
}
\details{
Los valores a,b,c,d tienen que estar ordenados en forma creciente y w estar entre 0 y 1
}
\value{
Devuelve el valor del indice para el metodo Chu y Tsao
}
\references{}
\author{msmaldonado}
\note{}
\seealso{}
\examples{
CentroideChuYTsaoTrapezoidal(0.4,0.5,0.5,1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{NumerosDifusosTrapezoidales,Centroide}

