\name{CVUniformeTrapezoidal}
\alias{CVUniformeTrapezoidal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Funcion que calcula el indice del metodo de variacion uniforme de Cheng para numeros difusos trapezoidales.}
\description{
Funcion que calcula el indice del metodo de variacion uniforme de Cheng para numeros difusos trapezoidales.
}
\usage{
CVUniformeTrapezoidal(a,b,c,d,w=NULL)
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
Los valores a,b,c,d tienen que estar ordenados en forma creciente y w estar entre 0 y 1
}
\value{
Devuelve el valor del indice de variacion uniforme
}
\references{}
\author{msmaldonado}
\note{}
\seealso{}
\examples{
CVUniformeTrapezoidal(0.4,0.5,0.5,1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{NumerosDifusosTrapezoidales,CoeficienteVariacion}
