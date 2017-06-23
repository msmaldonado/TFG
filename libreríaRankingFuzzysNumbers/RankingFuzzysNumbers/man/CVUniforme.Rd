\name{CVUniforme}
\alias{CVUniforme}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{indice del metodo de coeficiente de variacion uniforme}
\description{
Funcion que calcula el indice del metodo del coeficiente de variacion uniforme para cualquier tipo de numero difuso pasandole la funcion L y R
}
\usage{
CVUniforme(a,b,c,d,w=NULL,L=NULL,R=NULL,LI=NULL,RI=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{a}{Extremo inferior del soporte }
\item{b}{Extremo inferior del nucleo }
\item{c}{Extremo superior del nucleo }
\item{d}{Extremo superior del soporte }
\item{w}{altura en el eje y del numero difuso. \code{w} por defecto vale 1 }
\item{L}{Funcion L }
\item{R}{Funcion R }
\item{LI}{Funcion inversa de L }
\item{RI}{Funcion inversa de R }
}
\details{
Los valores a,b,c,d tienen que estar ordenados en forma creciente, w mayor que cero y menor que 1. Por defecto vale 1. Las funcion L y R tienen que ser las inversas y estar bien definidas
}
\value{
Devuelve el valor del indice
}
\references{}
\author{msmaldonado}
\note{}
\seealso{}
\examples{
CVUniforme(0.4,0.5,0.5,1,1,'x','-x+2','x','x-x+2')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{NumerosDifusos,CVUniforme}
