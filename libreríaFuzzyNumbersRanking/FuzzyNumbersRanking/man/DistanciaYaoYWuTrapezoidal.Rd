\name{DistanciaYaoYWuTrapezoidal}
\alias{DistanciaYaoYWuTrapezoidal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{funcion que calcula el indice del metodo Yao y Wu}
\description{
Funcion que calcula el valor del indice del metodo de Yao y Wu para numeros difusos trapezoidales
}
\usage{
YaoYWuTrapezoidal(a,b,c,d,w=F)
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
Los valores a,b,c,d tienen que estar ordenados en forma creciente. w pertene al 0,1
}
\value{
Devuelve el valor del indice para metodo Yao y Wu
}
\references{}
\author{msmaldonado}
\note{}
\seealso{}
\examples{
DistanciaYaoYWuTrapezoidal(0.4,0.5,0.5,1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{NumerosDifusosTrapezoidales,DistanciaYaoYWu}
